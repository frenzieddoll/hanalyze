{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- |
-- Module      : Hanalyze.Stat.BayesFactor
-- Description : Bridge Sampling による Bayes Factor (Kass & Raftery 1995) 計算
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Bayes Factor (Kass & Raftery 1995) via Bridge Sampling.
--
-- @
--   BF_{10} = p(y | M_1) / p(y | M_0)
-- @
--
-- 計算は 2 モデルそれぞれに対し
-- 'Hanalyze.Stat.BridgeSampling.bridgeSampling' を呼び、 log marginal
-- 同士の差を取る。 解釈表は Kass-Raftery (1995) Table 1。
--
-- Reference: Kass & Raftery (1995) "Bayes factors". JASA 90:773-795.
module Hanalyze.Stat.BayesFactor
  ( BayesFactorResult (..)
  , bayesFactor
  , BFInterpretation (..)
  , interpretBF
  ) where

import           System.Random.MWC         (GenIO)

import           Hanalyze.Model.HBM        (ModelP)
import           Hanalyze.MCMC.Core        (Chain)
import           Hanalyze.Stat.BridgeSampling
                   (BridgeConfig, BridgeResult (..), bridgeSampling)

-- ---------------------------------------------------------------------------
-- Bayes Factor
-- ---------------------------------------------------------------------------

data BayesFactorResult = BayesFactorResult
  { bfLog10        :: !Double  -- ^ log_10 BF_{10}
  , bfLogE         :: !Double  -- ^ log_e BF_{10} = log p(y|M_1) - log p(y|M_0)
  , bfLogMarginal0 :: !Double  -- ^ log p(y | M_0)
  , bfLogMarginal1 :: !Double  -- ^ log p(y | M_1)
  , bfConverged0   :: !Bool
  , bfConverged1   :: !Bool
  } deriving (Show)

-- | 2 モデル間の Bayes Factor BF_{10} = p(y|M_1) / p(y|M_0)。
-- 各モデルに対し Bridge Sampling で log marginal を推定、 差を取る。
bayesFactor
  :: forall r0 r1.
     ModelP r0 -> Chain     -- ^ M_0 + posterior chain
  -> ModelP r1 -> Chain     -- ^ M_1 + posterior chain
  -> BridgeConfig
  -> GenIO
  -> IO BayesFactorResult
bayesFactor m0 ch0 m1 ch1 cfg gen = do
  r0 <- bridgeSampling m0 cfg ch0 gen
  r1 <- bridgeSampling m1 cfg ch1 gen
  let logE   = brLogMarginal r1 - brLogMarginal r0
      log10v = logE / log 10
  pure BayesFactorResult
    { bfLog10        = log10v
    , bfLogE         = logE
    , bfLogMarginal0 = brLogMarginal r0
    , bfLogMarginal1 = brLogMarginal r1
    , bfConverged0   = brConverged r0
    , bfConverged1   = brConverged r1
    }

-- ---------------------------------------------------------------------------
-- Kass-Raftery 解釈表
-- ---------------------------------------------------------------------------

-- | Bayes Factor の強度区分 (Kass & Raftery 1995 Table 1)。
-- 区分の境界は @log_e BF@ で定義 (= log_10 ≈ /2.303):
--
-- @
--   0 < log_e BF < 1   (1 < BF < 2.7)    : Negligible
--   1 ≤ log_e BF < 3   (2.7 ≤ BF < 20)   : Positive (substantial)
--   3 ≤ log_e BF < 5   (20 ≤ BF < 150)   : Strong
--   5 ≤ log_e BF       (BF ≥ 150)        : Very strong (decisive)
-- @
--
-- 負側は対称 (M_0 寄り)。
data BFInterpretation
  = BFNegligible
  | BFPositive          -- substantial evidence
  | BFStrong
  | BFVeryStrong
  deriving (Show, Eq)

-- | log_e BF 値から強度区分を返す。 符号で方向 (M_0 / M_1 どちらに寄与) は
-- 呼び出し側が判定する想定 (@abs logE@ を渡しても OK)。
interpretBF :: Double -> BFInterpretation
interpretBF logE
  | a < 1     = BFNegligible
  | a < 3     = BFPositive
  | a < 5     = BFStrong
  | otherwise = BFVeryStrong
  where
    a = abs logE
