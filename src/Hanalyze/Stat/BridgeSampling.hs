{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE BangPatterns      #-}
-- |
-- Module      : Hanalyze.Stat.BridgeSampling
-- Description : Bridge Sampling による周辺尤度 log p(y) 推定 (Meng & Wong 1996)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Bridge Sampling estimator of the marginal likelihood @log p(y)@
-- (Meng & Wong 1996).
--
-- Reference:
--
--   * Meng & Wong (1996) "Simulating ratios of normalising constants
--     via a simple identity: a theoretical exploration". Statistica
--     Sinica 6:831-860.
--   * Gronau, Sarafoglou, Matzke, Ly, Boehm, Marsman, Leslie, Forster,
--     Wagenmakers, Steingroever (2017) "A tutorial on bridge sampling".
--     Journal of Mathematical Psychology 81:80-97.
--
-- ## アルゴリズム (Phase 29-A2)
--
-- 目的: 周辺尤度 @log p(y) = log ∫ p(y|θ) p(θ) dθ@ を、 既存 MCMC chain
-- (posterior samples) と diagonal Gaussian proposal @g(θ)@ から推定する。
--
-- Bridge identity (Meng-Wong):
--
-- @
--   p(y) = E_g[α(θ) q(θ)] / E_p[α(θ) g(θ)]
-- @
--
-- 最適 bridge function @α*(θ) = 1 / (s_1 q(θ) + s_2 r g(θ))@ を使った
-- iterative scheme で @r̂@ を求める:
--
-- @
--   r̂_{t+1} = [(1/N_2) Σ_i q(θ̃_2,i) / (s_1 q(θ̃_2,i) + s_2 r̂_t g(θ̃_2,i))]
--           / [(1/N_1) Σ_j g(θ̃_1,j) / (s_1 q(θ̃_1,j) + s_2 r̂_t g(θ̃_1,j))]
-- @
--
-- ここで:
--   * @θ̃_1@ は proposal @g@ から (本実装では Gaussian fit-to-chain)
--   * @θ̃_2@ は posterior chain サンプル
--   * @s_1 = N_1/(N_1+N_2)@、 @s_2 = N_2/(N_1+N_2)@
--   * @q(θ) = p(y|θ)·p(θ)@ = 'logJoint' の exp 化
--
-- 全計算は **log space** で行い (log-sum-exp 安定化)、 浮動小数 underflow を回避。
--
-- ## SMC との関係 (Phase 29-A1/A2 統合)
--
-- SMC は副産物として log marginal を推定する (= temperature schedule の
-- incremental log-mean-weight 累積)。 Bridge Sampling は MCMC chain + proposal
-- から **独立な推定経路** で求めるので、 両者が 5% 以内で一致すれば妥当性が裏付け。
-- 不一致なら chain の収束不足 / SMC schedule 粗さ / proposal 不適切のサイン。
module Hanalyze.Stat.BridgeSampling
  ( BridgeConfig (..)
  , defaultBridgeConfig
  , BridgeResult (..)
  , bridgeSampling
  ) where

import           Control.Monad             (replicateM, forM)
import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)
import           System.Random.MWC         (GenIO)
import           System.Random.MWC.Distributions (normal)

import           Hanalyze.Model.HBM        (ModelP, Params, logJoint, sampleNames)
import           Hanalyze.MCMC.Core        (Chain (..), chainVals)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Bridge Sampling 設定。
data BridgeConfig = BridgeConfig
  { bcNProposal :: !Int     -- ^ N_1: proposal samples 数 (典型 chain サンプル数と同等)
  , bcMaxIter   :: !Int     -- ^ 反復解の最大回数 (典型 100、 通常 < 20 で収束)
  , bcTolerance :: !Double  -- ^ 反復収束判定 |Δ log r̂| < tol (典型 1e-6)
  } deriving (Show)

defaultBridgeConfig :: BridgeConfig
defaultBridgeConfig = BridgeConfig
  { bcNProposal = 500
  , bcMaxIter   = 100
  , bcTolerance = 1e-6
  }

-- | Bridge Sampling 結果。
data BridgeResult = BridgeResult
  { brLogMarginal :: !Double   -- ^ 推定 @log p(y)@
  , brIterations  :: !Int      -- ^ 収束に要した反復数
  , brConverged   :: !Bool     -- ^ tol 以内で収束したか
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- 公開 API
-- ---------------------------------------------------------------------------

-- | Bridge Sampling で @log p(y)@ を推定。
--
-- 入力:
--   * モデル (logJoint = log q(θ) = log p(y|θ) + log p(θ))
--   * posterior chain (既存 NUTS / MH / SMC 等の結果)
--   * proposal は **diagonal Gaussian fit** to chain (各パラメータの sample
--     mean / SD から構築)
--
-- 出力: log marginal likelihood 推定値 + 収束情報。
bridgeSampling
  :: forall r. ModelP r
  -> BridgeConfig
  -> Chain                     -- ^ posterior chain
  -> GenIO
  -> IO BridgeResult
bridgeSampling model cfg chain gen = do
  let names      = sampleNames model
      posterior  = chainSamples chain
      n2         = length posterior
      (mus, sds) = fitDiagGaussian names chain
  -- 1. Sample N_1 from proposal g (diagonal Gaussian)
  proposal <- replicateM (bcNProposal cfg) (sampleProposal names mus sds gen)
  let n1   = length proposal
      s1   = fromIntegral n1 / fromIntegral (n1 + n2)
      s2   = fromIntegral n2 / fromIntegral (n1 + n2)
      -- 2. Precompute log q (logJoint) and log g (proposal log-density)
      logq2 = map (logJoint model) posterior
      logq1 = map (logJoint model) proposal
      logg2 = map (logProposal names mus sds) posterior
      logg1 = map (logProposal names mus sds) proposal
  -- 3. Iterative solve for log r̂
  let (logR, niter, converged) =
        iterateBridge cfg logq1 logg1 logq2 logg2 s1 s2 0.0
  pure BridgeResult
    { brLogMarginal = logR
    , brIterations  = niter
    , brConverged   = converged
    }

-- | Meng-Wong iterative formula in log space.
iterateBridge
  :: BridgeConfig
  -> [Double] -> [Double]   -- ^ logq1, logg1 (proposal samples)
  -> [Double] -> [Double]   -- ^ logq2, logg2 (posterior samples)
  -> Double                 -- ^ s_1
  -> Double                 -- ^ s_2
  -> Double                 -- ^ 初期 log r̂
  -> (Double, Int, Bool)
iterateBridge cfg logq1 logg1 logq2 logg2 s1 s2 logR0 = go 0 logR0
  where
    ls1 = log s1
    ls2 = log s2
    go !it !logR
      | it >= bcMaxIter cfg = (logR, it, False)
      | otherwise =
          let -- Numerator: posterior 側の logq2 - logSumExp(s1·q2, s2·r·g2)
              numTerms =
                [ lq - logSumExp2 (ls1 + lq) (ls2 + logR + lg)
                | (lq, lg) <- zip logq2 logg2 ]
              -- Denominator: proposal 側の logg1 - logSumExp(s1·q1, s2·r·g1)
              denTerms =
                [ lg - logSumExp2 (ls1 + lq) (ls2 + logR + lg)
                | (lq, lg) <- zip logq1 logg1 ]
              num = logMeanExp numTerms
              den = logMeanExp denTerms
              logR' = num - den
              diff  = abs (logR' - logR)
          in if diff < bcTolerance cfg
               then (logR', it + 1, True)
               else go (it + 1) logR'

-- ---------------------------------------------------------------------------
-- Diagonal Gaussian proposal (fit-to-chain)
-- ---------------------------------------------------------------------------

-- | chain から各パラメータの sample mean / SD を抽出。 SD = 0 になりうる
-- (定数推定) 場合は 1e-6 で下駄を履かせる (g(θ) 評価で除算 0 を避ける safety)。
fitDiagGaussian
  :: [Text] -> Chain -> (Map.Map Text Double, Map.Map Text Double)
fitDiagGaussian names chain =
  let mus = Map.fromList
        [ (n, mean (chainVals n chain)) | n <- names ]
      sds = Map.fromList
        [ (n, max 1e-6 (stddev (chainVals n chain))) | n <- names ]
  in (mus, sds)
  where
    mean xs = sum xs / fromIntegral (length xs)
    stddev xs =
      let mu = mean xs
          n  = fromIntegral (length xs) :: Double
      in if n <= 1 then 0
                   else sqrt (sum [(x - mu) ^ (2 :: Int) | x <- xs] / (n - 1))

-- | Diagonal Gaussian proposal からサンプル抽出。
sampleProposal
  :: [Text] -> Map.Map Text Double -> Map.Map Text Double -> GenIO
  -> IO Params
sampleProposal names mus sds gen =
  fmap Map.fromList $ forM names $ \n -> do
    let mu = Map.findWithDefault 0 n mus
        sd = Map.findWithDefault 1 n sds
    x <- normal mu sd gen
    pure (n, x)

-- | log density of diagonal Gaussian proposal at θ。
logProposal
  :: [Text] -> Map.Map Text Double -> Map.Map Text Double -> Params
  -> Double
logProposal names mus sds theta =
  sum
    [ let mu = Map.findWithDefault 0 n mus
          sd = Map.findWithDefault 1 n sds
          x  = Map.findWithDefault 0 n theta
          z  = (x - mu) / sd
      in -0.5 * log (2 * pi) - log sd - 0.5 * z * z
    | n <- names ]

-- ---------------------------------------------------------------------------
-- log-sum-exp helpers
-- ---------------------------------------------------------------------------

logSumExp2 :: Double -> Double -> Double
logSumExp2 a b
  | a > b     = a + log (1 + exp (b - a))
  | otherwise = b + log (1 + exp (a - b))

logMeanExp :: [Double] -> Double
logMeanExp xs
  | null xs   = -1 / 0
  | otherwise =
      let m  = maximum xs
          s  = sum [ exp (x - m) | x <- xs ]
          n  = fromIntegral (length xs) :: Double
      in m + log (s / n)
