-- |
-- Module      : Hanalyze.MCMC.SMC
-- Description : Tempered target による Sequential Monte Carlo (SMC) サンプラー
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Sequential Monte Carlo (SMC) sampler with tempered targets.
--
-- Implements a particle-based sampler that bridges from a broad initial
-- distribution to the full posterior @π(θ) ∝ p(θ) · L(θ)@ via a sequence
-- of intermediate targets @π_t(θ) ∝ p(θ) · L(θ)^β_t@, where
-- @β_0 = 0 → β_T = 1@.
--
-- Reference: Del Moral, Doucet, Jasra (2006) "Sequential Monte Carlo
-- samplers". JRSSB 68:411-436.
--
-- ## アルゴリズム概要 (Phase 29-A1)
--
-- 1. **Init**: N 個の粒子を @init_@ を中心とする広い Gaussian cloud から
--    サンプル (= 近似 prior)
-- 2. **Tempering loop** (t = 1..T):
--    a. **Weight**: 重み更新 @w_i ∝ exp((β_t − β_{t-1}) · logL(θ_i))@
--    b. **log marginal contribution**: @log(mean w_i)@ を累積
--    c. **Resample**: ESS = @(Σw)² / Σw²@ が閾値以下なら systematic resampling
--    d. **Move**: 各粒子に対し K 回の MH 移動 (target = π_t、 random walk
--       proposal)
-- 3. **Output**: 最終粒子集合を 'Chain' として返す + log marginal likelihood
--    の推定値
--
-- ## NUTS / MH との位置付け
--
-- SMC の advantage:
--
--   * 並列性が高い (粒子間は独立、 移動が並列化可能)
--   * 多峰分布で chain がはまりにくい (= temperature annealing)
--   * **log marginal likelihood の副産物推定**: Bridge Sampling より
--     軽量で取れる (= Bayes Factor / BMA の前処理に使える)
--
-- SMC の disadvantage:
--
--   * 単峰分布なら NUTS の方が effective sample size / 時間 で有利
--   * temperature schedule の選択が結果に影響
--
-- Phase 29-A2 Bridge Sampling は本 SMC の log marginal 推定の **独立な
-- 検証手段** として使う (両者で 5% 以内一致なら確からしい)。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE BangPatterns      #-}
module Hanalyze.MCMC.SMC
  ( SMCConfig (..)
  , defaultSMCConfig
  , SMCResult (..)
  , smc
  , smcPure
  ) where

import           Control.Monad             (forM, replicateM, foldM)
import           Control.Monad.Primitive   (PrimMonad, PrimState)
import           Control.Monad.ST          (runST)
import qualified Data.Map.Strict           as Map
import           Data.List                 (sort)
import           Data.Text                 (Text)
import           Data.Word                 (Word32)
import qualified Data.Vector               as V
import qualified Data.Vector.Unboxed       as VU
import           System.Random.MWC         (Gen, uniform, initialize)
import           System.Random.MWC.Distributions (normal)

import           Hanalyze.Model.HBM        (ModelP, Params, logPrior, logLikelihood, sampleNames)
import           Hanalyze.MCMC.Core        (Chain (..))

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | SMC configuration.
data SMCConfig = SMCConfig
  { smcNParticles   :: !Int     -- ^ N: 粒子数 (典型 500-2000)
  , smcNSteps       :: !Int     -- ^ T: temperature step 数 (典型 10-50)
  , smcMHIterations :: !Int     -- ^ K: 各 temperature 内の MH 移動 回数 (典型 5-20)
  , smcMHStepSize   :: !(Map.Map Text Double)  -- ^ Random walk MH の per-param std
  , smcInitJitter   :: !Double  -- ^ 初期粒子を init_ から散らす Gaussian σ (typical 2-5)
  , smcESSThreshold :: !Double  -- ^ 0..1、 ESS < N · threshold で resample (typical 0.5)
  } deriving (Show)

-- | Default: N=500、 T=20、 K=10、 step=0.5、 jitter σ=3、 ESS threshold=0.5。
defaultSMCConfig :: [Text] -> SMCConfig
defaultSMCConfig names = SMCConfig
  { smcNParticles   = 500
  , smcNSteps       = 20
  , smcMHIterations = 10
  , smcMHStepSize   = Map.fromList [(n, 0.5) | n <- names]
  , smcInitJitter   = 3.0
  , smcESSThreshold = 0.5
  }

-- | SMC の結果。 粒子を Chain 形に詰めた posterior 推定 + log marginal +
-- temperature step ごとの ESS 履歴。
--
-- **重要 (Phase 29-A1)**: 'smcLogMarginal' は **初期粒子が prior から
-- サンプルされていることを仮定** した推定値。 本実装は init_ を中心とする
-- jittered Gaussian から初期粒子を作るため、 prior が広いと bias する。
-- 厳密な log marginal が必要な場合は Phase 29-A2 'Hanalyze.Stat.BridgeSampling.bridgeSampling'
-- を使用すること (SMC chain を入力に独立に推定する)。 SMC の primary 用途は
-- **多峰 posterior の効率的なサンプリング**。
data SMCResult = SMCResult
  { smcChain        :: !Chain
  , smcLogMarginal  :: !Double
  , smcESSHistory   :: ![Double]
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- 公開 API
-- ---------------------------------------------------------------------------

-- | SMC を実行。 'init_' を中心に initial particles を散らし、 linear
-- temperature schedule (β_t = t/T) で posterior に温めていく。
smc :: forall r m. PrimMonad m => ModelP r -> SMCConfig -> Params -> Gen (PrimState m) -> m SMCResult
smc model cfg init_ gen = do
  let n      = smcNParticles cfg
      tT     = smcNSteps cfg
      names  = sampleNames model
      steps  = smcMHStepSize cfg
      jitter = smcInitJitter cfg
  -- 1. Init particles: init_ + N(0, jitter · stepSizes_i)
  particles0 <- replicateM n (jitterInit names jitter steps init_ gen)
  let betas = [ fromIntegral t / fromIntegral tT | t <- [0 .. tT] ]   -- [0, 1/T, .., 1]
      betaSteps = zip betas (tail betas)                              -- [(β_{t-1}, β_t)]

  -- 2. Tempering loop
  (finalParticles, logMarg, essHist) <-
    foldM (stepTemper model steps (smcMHIterations cfg) (smcESSThreshold cfg) gen n)
          (particles0, 0.0 :: Double, [])
          betaSteps

  let accepted = chainAcceptedAcc (length finalParticles * tT * smcMHIterations cfg)
      total    = length finalParticles * tT * smcMHIterations cfg
  pure SMCResult
    { smcChain = Chain
        { chainSamples     = finalParticles
        , chainAccepted    = accepted
        , chainTotal       = total
        , chainEnergy      = []
        , chainDivergences = []
        , chainTreeDepths  = []
        }
    , smcLogMarginal = logMarg
    , smcESSHistory  = reverse essHist
    }
  where
    -- 受理数は本実装では追跡しない (= 0 を入れて acceptanceRate は意味なし)
    chainAcceptedAcc _ = 0

-- | Phase 50: 純粋・決定的な SMC (seed → 確定 SMCResult・IO 不要)。 'smc' の ST/seed 版。
smcPure :: ModelP r -> SMCConfig -> Params -> Word32 -> SMCResult
smcPure model cfg initP seed =
  runST (initialize (V.singleton seed) >>= smc model cfg initP)

-- | 1 ステップの tempering:
--   * 重み計算 + log marginal 累積
--   * ESS 判定して resample
--   * K 回の MH 移動 (target = π_t = p(θ) · L(θ)^β_t)
stepTemper
  :: forall r m. PrimMonad m => ModelP r
  -> Map.Map Text Double         -- ^ step sizes
  -> Int                         -- ^ K
  -> Double                      -- ^ ESS threshold
  -> Gen (PrimState m)
  -> Int                         -- ^ N (元の粒子数、 resample で N keep)
  -> ([Params], Double, [Double]) -- ^ (粒子、 累積 log marginal、 ESS 履歴)
  -> (Double, Double)            -- ^ (β_{t-1}, β_t)
  -> m ([Params], Double, [Double])
stepTemper model steps k essThr gen n (particles, logMarg, essHist) (b0, b1) = do
  let dbeta   = b1 - b0
      logLs   = map (logLikelihood model) particles
      logWs   = map (dbeta *) logLs               -- log incremental weights
      logSumW = logSumExp logWs
      logMean = logSumW - log (fromIntegral (length particles))
      ws      = map (\lw -> exp (lw - logSumW)) logWs   -- normalized weights
      ess     = if sum (map (** 2) ws) == 0 then 0
                  else 1 / sum (map (** 2) ws)
      logMarg' = logMarg + logMean

  -- Resample if ESS < threshold · N
  resampled <-
    if ess < essThr * fromIntegral n
      then systematicResample particles ws n gen
      else pure particles

  -- Move with K MH iterations
  moved <- moveK model steps b1 k resampled gen
  pure (moved, logMarg', ess : essHist)

-- | systematic resampling (= particle filter standard)。
systematicResample
  :: forall m. PrimMonad m => [Params] -> [Double] -> Int -> Gen (PrimState m) -> m [Params]
systematicResample particles ws n gen = do
  u0 <- uniform gen :: m Double
  let total = sum ws
      ws' = map (/ total) ws  -- normalize
      cdf = scanl1 (+) ws'
      ps  = [ (fromIntegral i + u0) / fromIntegral n | i <- [0 .. n - 1] ]
      pick p = pickAt p cdf particles
  pure (map pick ps)
  where
    pickAt p (c : cs) (x : xs)
      | p <= c    = x
      | otherwise = pickAt p cs xs
    pickAt _ _ (x : _) = x  -- fallback (numeric edge)
    pickAt _ _ []      = error "systematicResample: empty particle list"

-- | K 回の Random Walk MH 移動。 target は @log π_t = logPrior + β · logLik@。
moveK
  :: forall r m. PrimMonad m => ModelP r
  -> Map.Map Text Double
  -> Double          -- ^ β
  -> Int
  -> [Params]
  -> Gen (PrimState m)
  -> m [Params]
moveK model steps beta k particles gen =
  mapM (mhKSteps model steps beta k gen) particles

mhKSteps
  :: forall r m. PrimMonad m => ModelP r
  -> Map.Map Text Double
  -> Double
  -> Int
  -> Gen (PrimState m)
  -> Params
  -> m Params
mhKSteps model steps beta k gen p0 = go k p0
  where
    target p = logPrior model p + beta * logLikelihood model p
    go 0 p = pure p
    go i p = do
      let names = Map.keys p
      proposed <- fmap Map.fromList $ forM names $ \n -> do
        let s   = Map.findWithDefault 1.0 n steps
            cur = Map.findWithDefault 0.0 n p
        eps <- normal 0 s gen
        pure (n, cur + eps)
      let logA = target proposed - target p
      u <- uniform gen :: m Double
      let !next = if log u < logA then proposed else p
      go (i - 1) next

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

jitterInit
  :: forall m. PrimMonad m => [Text] -> Double -> Map.Map Text Double -> Params -> Gen (PrimState m) -> m Params
jitterInit names jitter steps init_ gen =
  fmap Map.fromList $ forM names $ \n -> do
    let s   = jitter * Map.findWithDefault 1.0 n steps
        cur = Map.findWithDefault 0.0 n init_
    eps <- normal 0 s gen
    pure (n, cur + eps)

-- | Numerically stable log-sum-exp.
logSumExp :: [Double] -> Double
logSumExp [] = -1 / 0
logSumExp xs =
  let m = maximum xs
  in m + log (sum [ exp (x - m) | x <- xs ])
