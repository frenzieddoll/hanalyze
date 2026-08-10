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
-- [日本語]:
--
-- ## アルゴリズム概要
--
-- 1. __Init__: N 個の粒子を @init_@ を中心とする広い Gaussian cloud から
--    サンプル (= 近似 prior)
-- 2. __Tempering loop__ (t = 1..T):
--    a. __Weight__: 重み更新 @w_i ∝ exp((β_t − β_{t-1}) · logL(θ_i))@
--    b. __log marginal contribution__: @log(mean w_i)@ を累積
--    c. __Resample__: ESS = @(Σw)² / Σw²@ が閾値以下なら systematic resampling
--    d. __Move__: 各粒子に対し K 回の MH 移動 (target = π_t、 random walk
--       proposal)
-- 3. __Output__: 最終粒子集合を 'Chain' として返す + log marginal likelihood
--    の推定値
--
-- ## NUTS / MH との位置付け
--
-- SMC の advantage:
--
--   - 並列性が高い (粒子間は独立、 移動が並列化可能)
--   - 多峰分布で chain がはまりにくい (= temperature annealing)
--   - __log marginal likelihood の副産物推定__: Bridge Sampling より
--     軽量で取れる (= Bayes Factor / BMA の前処理に使える)
--
-- SMC の disadvantage:
--
--   - 単峰分布なら NUTS の方が effective sample size / 時間 で有利
--   - temperature schedule の選択が結果に影響
--
-- Bridge Sampling は本 SMC の log marginal 推定の __独立な検証手段__ として
-- 使う (両者で 5% 以内一致なら確からしい)。
--
-- [English]:
--
-- ## Algorithm overview
--
-- 1. __Init__: sample N particles from a broad Gaussian cloud centered on
--    @init_@ (= an approximate prior)
-- 2. __Tempering loop__ (t = 1..T):
--    a. __Weight__: update weights @w_i ∝ exp((β_t − β_{t-1}) · logL(θ_i))@
--    b. __log marginal contribution__: accumulate @log(mean w_i)@
--    c. __Resample__: if ESS = @(Σw)² / Σw²@ falls below the threshold,
--       systematic resampling
--    d. __Move__: K MH moves per particle (target = π_t, random walk
--       proposal)
-- 3. __Output__: returns the final particle set as a 'Chain' plus an
--    estimate of the log marginal likelihood
--
-- ## Positioning relative to NUTS / MH
--
-- SMC's advantages:
--
--   - Highly parallel (particles are independent; moves parallelize)
--   - Chains are less likely to get stuck in a multimodal distribution
--     (= temperature annealing)
--   - __A by-product estimate of the log marginal likelihood__: obtained
--     more cheaply than Bridge Sampling (= usable as a preprocessing step
--     for a Bayes Factor / model averaging)
--
-- SMC's disadvantages:
--
--   - For unimodal distributions, NUTS is favorable in effective sample
--     size per unit time
--   - The choice of temperature schedule affects the result
--
-- Bridge Sampling is used as an __independent verification method__ for
-- this SMC's log marginal estimate (agreement within 5% between the two
-- is taken as reassurance).
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
  { smcNParticles   :: !Int     -- ^ [日本語]: N: 粒子数 (典型 500-2000) [English]: N: the number of particles (typically 500-2000)
  , smcNSteps       :: !Int     -- ^ [日本語]: T: temperature step 数 (典型 10-50) [English]: T: the number of temperature steps (typically 10-50)
  , smcMHIterations :: !Int     -- ^ [日本語]: K: 各 temperature 内の MH 移動 回数 (典型 5-20) [English]: K: the number of MH moves within each temperature (typically 5-20)
  , smcMHStepSize   :: !(Map.Map Text Double)  -- ^ [日本語]: Random walk MH の per-param std [English]: The per-parameter std for the random-walk MH proposal
  , smcInitJitter   :: !Double  -- ^ [日本語]: 初期粒子を init_ から散らす Gaussian σ (typical 2-5) [English]: The Gaussian σ used to scatter initial particles around init_ (typically 2-5)
  , smcESSThreshold :: !Double  -- ^ [日本語]: 0..1、 ESS < N · threshold で resample (typical 0.5) [English]: In 0..1; resamples when ESS < N · threshold (typically 0.5)
  } deriving (Show)

-- | [日本語]: Default: N=500、 T=20、 K=10、 step=0.5、 jitter σ=3、 ESS threshold=0.5。
--   [English]: Default: N=500, T=20, K=10, step=0.5, jitter σ=3, ESS threshold=0.5.
defaultSMCConfig :: [Text] -> SMCConfig
defaultSMCConfig names = SMCConfig
  { smcNParticles   = 500
  , smcNSteps       = 20
  , smcMHIterations = 10
  , smcMHStepSize   = Map.fromList [(n, 0.5) | n <- names]
  , smcInitJitter   = 3.0
  , smcESSThreshold = 0.5
  }

-- | [日本語]: SMC の結果。 粒子を Chain 形に詰めた posterior 推定 + log marginal +
--   temperature step ごとの ESS 履歴。
--
--   __重要__: 'smcLogMarginal' は「初期粒子が prior からサンプルされている」
--   __ことを仮定した推定値__。 本実装は init_ を中心とする jittered Gaussian
--   から初期粒子を作るため、 prior が広いと bias する。 厳密な log marginal が
--   必要な場合は 'Hanalyze.Stat.BridgeSampling.bridgeSampling' を
--   使用すること (SMC chain を入力に独立に推定する)。 SMC の primary 用途は
--   __多峰 posterior の効率的なサンプリング__。
--   [English]: The result of SMC. A posterior estimate packed into a Chain
--   shape, plus the log marginal, plus a per-temperature-step ESS history.
--
--   __Important__: 'smcLogMarginal' is an estimate that assumes the
--   __initial particles were sampled from the prior__. This implementation
--   creates initial particles from a jittered Gaussian centered on init_,
--   so it is biased when the prior is broad. If a rigorous log marginal is
--   needed, use
--   'Hanalyze.Stat.BridgeSampling.bridgeSampling' instead (it
--   estimates independently, taking the SMC chain as input). SMC's primary
--   use case is __efficient sampling of multimodal posteriors__.
data SMCResult = SMCResult
  { smcChain        :: !Chain
  , smcLogMarginal  :: !Double
  , smcESSHistory   :: ![Double]
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- 公開 API
-- ---------------------------------------------------------------------------

-- | [日本語]: SMC を実行。 @init_@ を中心に initial particles を散らし、 linear
--   temperature schedule (β_t = t/T) で posterior に温めていく。
--   [English]: Runs SMC. Scatters initial particles around @init_@ and
--   tempers toward the posterior along a linear temperature schedule
--   (β_t = t/T).
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

-- | [日本語]: 純粋・決定的な SMC (seed → 確定 SMCResult・IO 不要)。 'smc' の ST/seed 版。
--   [English]: A pure, deterministic SMC (seed → a fixed SMCResult, no IO
--   needed). The ST/seed counterpart of 'smc'.
smcPure :: ModelP r -> SMCConfig -> Params -> Word32 -> SMCResult
smcPure model cfg initP seed =
  runST (initialize (V.singleton seed) >>= smc model cfg initP)

-- | [日本語]: 1 ステップの tempering:
--   - 重み計算 + log marginal 累積
--   - ESS 判定して resample
--   - K 回の MH 移動 (target = π_t = p(θ) · L(θ)^β_t)
--   [English]: One tempering step:
--   - Compute weights + accumulate the log marginal
--   - Check ESS and resample
--   - K MH moves (target = π_t = p(θ) · L(θ)^β_t)
stepTemper
  :: forall r m. PrimMonad m => ModelP r
  -> Map.Map Text Double         -- ^ step sizes
  -> Int                         -- ^ K
  -> Double                      -- ^ ESS threshold
  -> Gen (PrimState m)
  -> Int                         -- ^ [日本語]: N (元の粒子数、 resample で N keep) [English]: N (the original particle count; resampling keeps N)
  -> ([Params], Double, [Double]) -- ^ [日本語]: (粒子、 累積 log marginal、 ESS 履歴) [English]: (particles, cumulative log marginal, ESS history)
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

-- | [日本語]: systematic resampling (= particle filter standard)。
--   [English]: Systematic resampling (the standard particle-filter method).
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

-- | [日本語]: K 回の Random Walk MH 移動。 target は @log π_t = logPrior + β · logLik@。
--   [English]: K random-walk MH moves. The target is
--   @log π_t = logPrior + β · logLik@.
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
