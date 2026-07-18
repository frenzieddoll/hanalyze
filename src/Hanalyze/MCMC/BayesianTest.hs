-- |
-- Module      : Hanalyze.MCMC.BayesianTest
-- Description : Bayesian A/B test — 2 群間の平均差を NUTS でサンプルし ROPE/HDI で判定
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Bayesian A/B test helper — 2 群間の平均差を NUTS でサンプル、
-- ROPE / HDI に基づき決定。
--
-- Spotfire 風 "Good vs Bad" の Bayesian 版。 既存の頻度論版
-- ('Hanalyze.Stat.GroupComparison.goodVsBad') が Welch t + Cohen's d で
-- 並列比較するのに対し、 本モジュールは **2 群の平均差の posterior** を
-- 得て、 HDI (highest density interval) + ROPE (region of practical
-- equivalence) で意思決定する。
--
-- モデル:
--
-- @
-- μ_A    ~ Normal(0, priorScale)
-- μ_B    ~ Normal(0, priorScale)
-- σ_A    ~ HalfNormal(sigmaScale)
-- σ_B    ~ HalfNormal(sigmaScale)
-- y_A    ~ Normal(μ_A, σ_A)
-- y_B    ~ Normal(μ_B, σ_B)
-- diff   = μ_B - μ_A
-- @
--
-- 決定ルール (`ROPEDecision lo hi`):
--
--   * HDI が ROPE [lo, hi] と **重ならず HDI 全体が ROPE の外** → 'RejectH0'
--   * HDI が ROPE 内に **完全に含まれる** → 'AcceptH0'
--   * それ以外 → 'Inconclusive'
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Hanalyze.MCMC.BayesianTest
  ( -- * 入力
    BayesianABConfig (..)
  , DecisionRule (..)
  , defaultBayesianABConfig
    -- * 出力
  , BayesianABResult (..)
  , ABDecision (..)
    -- * 実行
  , bayesianAB
    -- * 補助
  , highestDensityInterval
  ) where

import qualified Data.Map.Strict       as Map
import           Data.List             (sort)
import qualified System.Random.MWC     as MWC

import qualified Hanalyze.MCMC.Core    as MC
import qualified Hanalyze.MCMC.NUTS    as NUTS
import qualified Hanalyze.Model.HBM    as HBM

-- ===========================================================================
-- 型
-- ===========================================================================

-- | 意思決定ルール。
data DecisionRule
  = HDIOnly                     -- ^ HDI を計算するのみ、 自動判定しない
  | ROPEDecision !Double !Double
    -- ^ @ROPEDecision lo hi@ で「実用上 0 と区別不能な区間 @[lo, hi]@」 を指定
  deriving (Show, Eq)

-- | A/B 試験の入力設定。
data BayesianABConfig = BayesianABConfig
  { babCredible   :: !Double         -- ^ HDI の信頼水準 (例 0.95)
  , babRule       :: !DecisionRule
  , babPriorScale :: !Double         -- ^ μ_A, μ_B の prior σ (default 10)
  , babSigmaScale :: !Double         -- ^ HalfNormal σ の scale (default 5)
  , babNUTS       :: !NUTS.NUTSConfig
  } deriving (Show)

defaultBayesianABConfig :: BayesianABConfig
defaultBayesianABConfig = BayesianABConfig
  { babCredible   = 0.95
  , babRule       = HDIOnly
  , babPriorScale = 10.0
  , babSigmaScale = 5.0
  , babNUTS       = NUTS.defaultNUTSConfig
                      { NUTS.nutsIterations = 1000
                      , NUTS.nutsBurnIn     = 500
                      }
  }

-- | 自動判定の結果。
data ABDecision
  = AcceptH0       -- ^ HDI が ROPE 内 → 「実用上 0」 と判定
  | RejectH0       -- ^ HDI が ROPE の外 → 「明確に差がある」 と判定
  | Inconclusive   -- ^ HDI が ROPE と部分的に重なる → 「データ不足」
  | NoRuleApplied  -- ^ 'HDIOnly' 指定で判定なし
  deriving (Show, Eq)

-- | A/B 試験の出力。
data BayesianABResult = BayesianABResult
  { babPosteriorDiff :: ![Double]
    -- ^ 平均差 (μ_B − μ_A) の post-burn-in サンプル
  , babMeanDiff      :: !Double
    -- ^ posterior mean (μ_B − μ_A)
  , babHDI           :: !(Double, Double)
    -- ^ @babCredible@ 信頼水準の HDI
  , babDecision      :: !ABDecision
  , babProbDiffPos   :: !Double
    -- ^ @P(μ_B > μ_A)@ の posterior 確率
  , babChain         :: !MC.Chain
    -- ^ 生 chain (μ_A / μ_B / σ_A / σ_B / diff の post-burn-in サンプル)
  } deriving (Show)

-- ===========================================================================
-- 公開関数
-- ===========================================================================

-- | 2 群のデータから Bayesian A/B 試験を実行。
--
-- 内部で HBM モデルを組み立て、 NUTS で posterior をサンプル、
-- 平均差の HDI と決定を返す。
--
-- 失敗条件: いずれかの群が空 → @error@ (canvas backend では事前に検査)。
bayesianAB
  :: BayesianABConfig
  -> [Double]         -- ^ 群 A の観測値
  -> [Double]         -- ^ 群 B の観測値
  -> MWC.GenIO
  -> IO BayesianABResult
bayesianAB cfg ysA ysB gen
  | null ysA || null ysB =
      error "Hanalyze.MCMC.BayesianTest.bayesianAB: both groups must be non-empty"
  | otherwise = do
      let priorScale_ = babPriorScale cfg
          sigmaScale_ = babSigmaScale cfg
          model :: HBM.ModelP ()
          model = do
            muA <- HBM.sample "mu_a"    (HBM.Normal 0 (realToFrac priorScale_))
            muB <- HBM.sample "mu_b"    (HBM.Normal 0 (realToFrac priorScale_))
            sA  <- HBM.sample "sigma_a" (HBM.HalfNormal (realToFrac sigmaScale_))
            sB  <- HBM.sample "sigma_b" (HBM.HalfNormal (realToFrac sigmaScale_))
            HBM.observe "ya" (HBM.Normal muA sA) ysA
            HBM.observe "yb" (HBM.Normal muB sB) ysB
            _   <- HBM.deterministic "diff" (muB - muA)
            pure ()
          initParams = Map.fromList
            [ ("mu_a", mean ysA)
            , ("mu_b", mean ysB)
            , ("sigma_a", max 0.1 (stddev ysA))
            , ("sigma_b", max 0.1 (stddev ysB))
            ]
      rawChain <- NUTS.nuts model (babNUTS cfg) initParams gen
      -- deterministic 値 "diff" は raw chain に入っていないため augment で注入
      let chain = HBM.augmentChainWithDeterministic model rawChain
          diffs = MC.chainVals "diff" chain
          n     = length diffs
          mu    = if n == 0 then 0 else sum diffs / fromIntegral n
          hdi   = highestDensityInterval (babCredible cfg) diffs
          probP = if n == 0
                    then 0
                    else fromIntegral (length (filter (> 0) diffs))
                       / fromIntegral n
          decision = case babRule cfg of
            HDIOnly -> NoRuleApplied
            ROPEDecision lo hi -> classifyROPE hdi lo hi
      pure BayesianABResult
        { babPosteriorDiff = diffs
        , babMeanDiff      = mu
        , babHDI           = hdi
        , babDecision      = decision
        , babProbDiffPos   = probP
        , babChain         = chain
        }

-- ===========================================================================
-- 補助
-- ===========================================================================

-- | サンプル列の **highest density interval (HDI)**。
--
-- ソート後、 窓幅 @floor(n · level)@ で全 sliding window を試し、
-- 最も狭い窓を返す。 unimodal な posterior では HDI = 最短連続区間。
--
-- @level ∈ (0, 1)@、 例: 0.95 で 95% HDI。
highestDensityInterval :: Double -> [Double] -> (Double, Double)
highestDensityInterval level xs
  | null xs = (0, 0)
  | level <= 0 || level >= 1 = error "HDI: level must be in (0, 1)"
  | otherwise =
      let sorted = sort xs
          n      = length sorted
          k      = max 1 (floor (fromIntegral n * level :: Double))
          -- 全 sliding windows (start = 0 .. n-k)
          arr    = case sorted of
                     [] -> []
                     _  -> sorted
          windows = [ (arr !! i, arr !! (i + k - 1))
                    | i <- [0 .. n - k] ]
          -- 最も狭い窓
          best   = head $ foldr keepNarrower [head windows] (tail windows)
      in best
  where
    keepNarrower w (b:_) =
      if (snd w - fst w) < (snd b - fst b) then [w] else [b]
    keepNarrower w []    = [w]

-- | HDI と ROPE [lo, hi] から ABDecision を分類。
classifyROPE :: (Double, Double) -> Double -> Double -> ABDecision
classifyROPE (hdiLo, hdiHi) ropeLo ropeHi
  | hdiHi < ropeLo || hdiLo > ropeHi = RejectH0       -- HDI 全体が ROPE 外
  | hdiLo >= ropeLo && hdiHi <= ropeHi = AcceptH0     -- HDI 全体が ROPE 内
  | otherwise = Inconclusive                          -- 部分重複

-- ===========================================================================
-- 統計 helper
-- ===========================================================================

mean :: [Double] -> Double
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

stddev :: [Double] -> Double
stddev xs
  | length xs < 2 = 1
  | otherwise =
      let n = fromIntegral (length xs) :: Double
          m = mean xs
      in sqrt (sum [ (x - m) ** 2 | x <- xs ] / (n - 1))
