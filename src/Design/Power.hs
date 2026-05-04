{-# LANGUAGE OverloadedStrings #-}
-- | Power analysis: sample-size determination and power computation.
--
-- Main functions:
--
--   * 'powerTTest'        — power of a two-sample t-test.
--   * 'sampleSizeTTest'   — @n@ required to attain a given power.
--   * 'powerOneWayAnova'  — power of an F-test (one-way ANOVA).
--   * 'powerProportion'   — power of a two-sample proportion test.
module Design.Power
  ( -- * t 検定
    powerTTest
  , sampleSizeTTest
    -- * F 検定 (ANOVA)
  , powerOneWayAnova
  , sampleSizeOneWayAnova
    -- * 比率検定
  , powerProportion
    -- * 効果量の指標
  , cohensD
  , cohensF
  ) where

import qualified Statistics.Distribution as SD
import qualified Statistics.Distribution.StudentT as ST
import qualified Statistics.Distribution.Normal as NormalD
import qualified Statistics.Distribution.FDistribution as FD

-- ---------------------------------------------------------------------------
-- 効果量
-- ---------------------------------------------------------------------------

-- | Cohen's @d@: standardized two-sample mean difference.
-- @d = (μ_1 − μ_2) / σ_pooled@.
-- Interpretation: 0.2 = small, 0.5 = medium, 0.8 = large.
cohensD :: Double -> Double -> Double -> Double
cohensD mu1 mu2 sigma = (mu1 - mu2) / sigma

-- | Cohen's @f@: effect size for one-way ANOVA.
-- @f = σ_means / σ_within@.
-- Interpretation: 0.10 = small, 0.25 = medium, 0.40 = large.
cohensF :: [Double]    -- ^ Per-group means.
        -> Double      -- ^ Within-group SD (@= √MSE@).
        -> Double
cohensF means sigma =
  let k    = length means
      gm   = sum means / fromIntegral k
      var  = sum [(m - gm)^(2::Int) | m <- means] / fromIntegral k
  in sqrt var / sigma

-- ---------------------------------------------------------------------------
-- t 検定
-- ---------------------------------------------------------------------------

-- | Two-sample two-sided t-test power, equal-variance assumption.
powerTTest :: Double  -- ^ Cohen's @d@ (effect size).
           -> Int     -- ^ Sample size of group 1, @n_1@.
           -> Int     -- ^ Sample size of group 2, @n_2@.
           -> Double  -- ^ Significance level @α@ (e.g. 0.05).
           -> Double
powerTTest d n1 n2 alpha =
  let df  = n1 + n2 - 2
      ncp = d * sqrt (fromIntegral n1 * fromIntegral n2
                      / fromIntegral (n1 + n2))
      tCrit = SD.quantile (ST.studentT (fromIntegral df))
                          (1 - alpha / 2)
      -- 非心 t 分布の代わりに正規近似 (df 大なら良好)
      sigma = 1.0  -- t 分布近似なら sd ≈ 1
      pUpper = 1 - SD.cumulative (NormalD.normalDistr ncp sigma) tCrit
      pLower = SD.cumulative (NormalD.normalDistr ncp sigma) (-tCrit)
  in pUpper + pLower

-- | Smallest balanced sample size that attains the requested power.
-- (Both groups assumed equal in size.)
sampleSizeTTest :: Double  -- ^ Effect size @d@.
                -> Double  -- ^ Target power.
                -> Double  -- ^ Significance level @α@.
                -> Int
sampleSizeTTest d targetPow alpha = search 2 1000
  where
    search lo hi
      | lo >= hi = hi
      | otherwise =
          let mid = (lo + hi) `div` 2
              p   = powerTTest d mid mid alpha
          in if p >= targetPow then search lo mid else search (mid + 1) hi

-- ---------------------------------------------------------------------------
-- 一元配置 ANOVA の F 検定
-- ---------------------------------------------------------------------------

-- | One-way ANOVA F-test power.
powerOneWayAnova :: Double   -- ^ Cohen's @f@ (effect size).
                 -> Int      -- ^ Number of groups @k@.
                 -> Int      -- ^ Per-group sample size @n@.
                 -> Double   -- ^ Significance level @α@.
                 -> Double
powerOneWayAnova f k n alpha =
  let dfBetween = k - 1
      dfWithin  = k * (n - 1)
      ncp       = f * f * fromIntegral (k * n)
      fCrit     = SD.quantile (FD.fDistribution dfBetween dfWithin)
                              (1 - alpha)
      -- 非心 F 分布 ≈ scaled F で近似
      mean1     = fromIntegral dfBetween + ncp
      var1      = 2 * mean1   -- chi² 近似
      -- 標準正規近似:
      z         = (fCrit * fromIntegral dfBetween - mean1) / sqrt var1
  in 1 - SD.cumulative (NormalD.normalDistr 0 1) z

-- | Smallest per-group sample size that attains the requested ANOVA power.
sampleSizeOneWayAnova :: Double -> Int -> Double -> Double -> Int
sampleSizeOneWayAnova f k targetPow alpha = search 2 1000
  where
    search lo hi
      | lo >= hi = hi
      | otherwise =
          let mid = (lo + hi) `div` 2
              p   = powerOneWayAnova f k mid alpha
          in if p >= targetPow then search lo mid else search (mid + 1) hi

-- ---------------------------------------------------------------------------
-- 比率検定 (二群)
-- ---------------------------------------------------------------------------

-- | Two-sample two-sided proportion z-test power.
--
-- Arguments: true proportions @p_1@, @p_2@, group sample sizes @n_1@,
-- @n_2@, and significance level @α@.
powerProportion :: Double -> Double -> Int -> Int -> Double -> Double
powerProportion p1 p2 n1 n2 alpha =
  let n1d = fromIntegral n1; n2d = fromIntegral n2
      pP  = (n1d * p1 + n2d * p2) / (n1d + n2d)
      seH0 = sqrt (pP * (1 - pP) * (1/n1d + 1/n2d))
      seH1 = sqrt (p1 * (1 - p1) / n1d + p2 * (1 - p2) / n2d)
      delta = abs (p1 - p2)
      zAlpha = SD.quantile (NormalD.normalDistr 0 1) (1 - alpha / 2)
      crit = zAlpha * seH0
      z = (delta - crit) / seH1
  in SD.cumulative (NormalD.normalDistr 0 1) z
