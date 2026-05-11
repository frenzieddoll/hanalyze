{-# LANGUAGE OverloadedStrings #-}
-- | Effect sizes and power analysis.
--
-- Effect-size measures complement p-values by quantifying the
-- magnitude of an effect, not just its statistical significance.
-- Power analysis lets the user pick sample sizes a priori or assess
-- post-hoc power.
--
-- == Effect-size summary
--
--   * 'cohenD' — standardised mean difference (two-sample).
--   * 'hedgesG' — small-sample-corrected Cohen's d.
--   * 'cohensF' — for ANOVA / regression.
--   * 'eta2' / 'omega2' — variance explained in ANOVA.
--   * 'cramerV' — for chi-square contingency tables.
--   * 'oddsRatio' — for 2×2 tables.
--
-- == Power analysis
--
-- Each test family provides @powerXxx@ (compute power given n / α /
-- effect) and @sampleSizeXxx@ (compute n given power / α / effect).
module Hanalyze.Stat.Effect
  ( -- * Effect-size measures (location)
    cohenD
  , cohenDPaired
  , hedgesG
    -- * Effect-size (ANOVA / regression)
  , cohensF
  , eta2
  , omega2
    -- * Effect-size (categorical)
  , cramerV
  , phiCoeff
  , oddsRatio
    -- * Power analysis (t-test)
  , powerTTest
  , sampleSizeTTest
    -- * Power analysis (one-way ANOVA)
  , powerANOVA
  , sampleSizeANOVA
    -- * Power analysis (correlation)
  , powerCorrelation
  ) where

import qualified Numeric.LinearAlgebra            as LA
import qualified Statistics.Distribution          as SD
import qualified Statistics.Distribution.FDistribution as FDist
import qualified Statistics.Distribution.Normal   as Normal
import qualified Statistics.Distribution.StudentT as StuT

-- ---------------------------------------------------------------------------
-- Effect sizes (location)
-- ---------------------------------------------------------------------------

-- | Cohen's d for two independent samples (pooled SD denominator).
-- Conventional interpretation: small = 0.2, medium = 0.5, large = 0.8.
cohenD :: LA.Vector Double -> LA.Vector Double -> Double
cohenD xs ys =
  let n1 = fromIntegral (LA.size xs) :: Double
      n2 = fromIntegral (LA.size ys) :: Double
      m1 = mean xs
      m2 = mean ys
      v1 = variance xs
      v2 = variance ys
      pooledV = ((n1 - 1) * v1 + (n2 - 1) * v2) / (n1 + n2 - 2)
  in if pooledV <= 0 then 0 else (m1 - m2) / sqrt pooledV

-- | Cohen's d for paired samples (uses SD of differences).
cohenDPaired :: LA.Vector Double -> LA.Vector Double -> Double
cohenDPaired xs ys =
  let diffs = xs - ys
      m     = mean diffs
      s     = sqrt (variance diffs)
  in if s <= 0 then 0 else m / s

-- | Hedges' g — Cohen's d corrected for small-sample bias.
-- @g = d × (1 − 3 / (4(n1 + n2) − 9))@.
hedgesG :: LA.Vector Double -> LA.Vector Double -> Double
hedgesG xs ys =
  let d  = cohenD xs ys
      n1 = LA.size xs
      n2 = LA.size ys
      df = fromIntegral (n1 + n2) - 2
      j  = 1 - 3 / (4 * df - 1)
  in d * j

-- ---------------------------------------------------------------------------
-- Effect sizes (ANOVA / regression)
-- ---------------------------------------------------------------------------

-- | Cohen's f for ANOVA: @sqrt(η² / (1 − η²))@.
-- Conventional: small = 0.10, medium = 0.25, large = 0.40.
cohensF :: Double -> Double
cohensF e2 = sqrt (e2 / max 1e-15 (1 - e2))

-- | η² (eta-squared): @SS_between / SS_total@.
-- Range @[0, 1]@; biased upward, especially with small @n@.
eta2 :: [LA.Vector Double] -> Double
eta2 groups
  | null groups = 0
  | otherwise =
      let ns    = map (fromIntegral . LA.size) groups :: [Double]
          n     = sum ns
          means = map mean groups
          grand = sum (zipWith (*) ns means) / n
          ssB   = sum [ ni * (mi - grand)^(2::Int) | (ni, mi) <- zip ns means ]
          ssT   = sum [ LA.sumElements ((g - LA.scalar grand)^(2::Int))
                      | g <- groups ]
      in if ssT <= 0 then 0 else ssB / ssT

-- | ω² (omega-squared): unbiased version of η².
-- @ω² = (SS_between − (k − 1) × MS_within) / (SS_total + MS_within)@.
omega2 :: [LA.Vector Double] -> Double
omega2 groups
  | length groups < 2 = 0
  | otherwise =
      let k     = length groups
          ns    = map (fromIntegral . LA.size) groups :: [Double]
          n     = sum ns
          means = map mean groups
          grand = sum (zipWith (*) ns means) / n
          ssB   = sum [ ni * (mi - grand)^(2::Int) | (ni, mi) <- zip ns means ]
          ssW   = sum [ LA.sumElements ((g - LA.scalar mi)^(2::Int))
                      | (g, mi) <- zip groups means ]
          ssT   = ssB + ssW
          msW   = ssW / (n - fromIntegral k)
      in if ssT + msW <= 0 then 0
           else (ssB - fromIntegral (k - 1) * msW) / (ssT + msW)

-- ---------------------------------------------------------------------------
-- Effect sizes (categorical)
-- ---------------------------------------------------------------------------

-- | Cramér's V from a chi-square statistic and table dimensions.
-- Range @[0, 1]@; > 0.5 = strong association.
cramerV :: Double -> Int -> Int -> Int -> Double
cramerV chi2 n r c =
  sqrt (chi2 / (fromIntegral n * fromIntegral (min r c - 1)))

-- | φ (phi) coefficient for 2×2 tables. @φ = sqrt(χ² / n)@. Same as
-- 'cramerV' for 2×2.
phiCoeff :: Double -> Int -> Double
phiCoeff chi2 n = sqrt (chi2 / fromIntegral n)

-- | Odds ratio for a 2×2 table @((a, b), (c, d))@.
oddsRatio :: ((Int, Int), (Int, Int)) -> Double
oddsRatio ((a, b), (c, d))
  | b * c == 0 = 1 / 0
  | otherwise  = fromIntegral (a * d) / fromIntegral (b * c)

-- ---------------------------------------------------------------------------
-- Power analysis — t-test
-- ---------------------------------------------------------------------------

-- | Power of a two-sided two-sample t-test.
--
-- @power(n, α, d) = 1 − β@ where @β@ is the type-II error rate.
-- Computed via the noncentral t-distribution; we approximate with a
-- normal approximation good for moderate-to-large @n@.
--
-- Inputs:
--
--   * @nPerGroup@: sample size per group.
--   * @alpha@: significance level (e.g. 0.05).
--   * @effect@: Cohen's d.
powerTTest :: Int -> Double -> Double -> Double
powerTTest nPerGroup alpha d =
  let n      = fromIntegral nPerGroup :: Double
      df     = 2 * n - 2
      tCrit  = SD.quantile (StuT.studentT df) (1 - alpha / 2)
      ncp    = d * sqrt (n / 2)
      -- P(T > tCrit | non-centrality = ncp), approximated via Normal:
      -- z ≈ (T − ncp) / 1; P(T > tCrit) ≈ 1 - Φ(tCrit - ncp)
      pUpper = 1 - SD.cumulative Normal.standard (tCrit - ncp)
      pLower = SD.cumulative Normal.standard (-tCrit - ncp)
  in pUpper + pLower

-- | Required sample size per group for a target power on a two-sample
-- t-test (two-sided). Solved by binary search over @powerTTest@.
sampleSizeTTest
  :: Double  -- ^ Target power (e.g. 0.80).
  -> Double  -- ^ Significance level @α@.
  -> Double  -- ^ Cohen's d.
  -> Int
sampleSizeTTest tgtPower alpha d
  | d <= 0    = 0
  | otherwise = binSearch 4 100000
  where
    binSearch lo hi
      | hi - lo <= 1 = hi
      | otherwise    =
          let mid = (lo + hi) `div` 2
              p   = powerTTest mid alpha d
          in if p >= tgtPower then binSearch lo mid else binSearch mid hi

-- ---------------------------------------------------------------------------
-- Power analysis — one-way ANOVA
-- ---------------------------------------------------------------------------

-- | Power of a one-way ANOVA F-test.
--
--   * @nPerGroup@: cells per group.
--   * @k@: number of groups.
--   * @f@: Cohen's f effect size.
powerANOVA :: Int -> Int -> Double -> Double -> Double
powerANOVA nPerGroup k alpha f =
  let n     = fromIntegral nPerGroup * fromIntegral k :: Double
      df1   = fromIntegral (k - 1) :: Double
      df2   = n - fromIntegral k
      fCrit = SD.quantile (FDist.fDistribution (k - 1)
                                                (round df2)) (1 - alpha)
      ncp   = f * f * n   -- non-centrality parameter
      -- Approximation: shift the F crit by ncp/df1.
      adjustedF = fCrit / (1 + ncp / df1)
      _ = adjustedF
      -- A better approximation uses the noncentral F directly. We use
      -- a simple normal approximation on the test statistic.
      mu = (1 + ncp / df1) * df2 / (df2 - 2)
      sd = sqrt (2 * (df2 / (df2 - 2))^(2::Int) * (df1 + ncp)
                 / (df1 * (df2 - 4)))
      _ = sd
  in 1 - SD.cumulative Normal.standard ((fCrit - mu) / max 1e-9 sd)

-- | Required cells per group for a target power on one-way ANOVA.
sampleSizeANOVA
  :: Double  -- ^ Target power.
  -> Int     -- ^ Number of groups.
  -> Double  -- ^ Significance level @α@.
  -> Double  -- ^ Cohen's f.
  -> Int
sampleSizeANOVA tgtPower k alpha f
  | f <= 0    = 0
  | otherwise = binSearch 4 100000
  where
    binSearch lo hi
      | hi - lo <= 1 = hi
      | otherwise    =
          let mid = (lo + hi) `div` 2
              p   = powerANOVA mid k alpha f
          in if p >= tgtPower then binSearch lo mid else binSearch mid hi

-- ---------------------------------------------------------------------------
-- Power analysis — correlation
-- ---------------------------------------------------------------------------

-- | Power of testing @H0: ρ = 0@ via Fisher z transform.
--
--   * @n@: sample size.
--   * @r@: target correlation effect size.
powerCorrelation :: Int -> Double -> Double -> Double
powerCorrelation n alpha r =
  let nn   = fromIntegral n :: Double
      zr   = 0.5 * log ((1 + r) / (1 - r))  -- Fisher z transform
      seZ  = 1 / sqrt (nn - 3)
      zCrit = SD.quantile Normal.standard (1 - alpha / 2)
      pUpper = 1 - SD.cumulative Normal.standard (zCrit - zr / seZ)
      pLower = SD.cumulative Normal.standard (-zCrit - zr / seZ)
  in pUpper + pLower

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

mean :: LA.Vector Double -> Double
mean v = LA.sumElements v / fromIntegral (LA.size v)

variance :: LA.Vector Double -> Double
variance v =
  let n = fromIntegral (LA.size v) :: Double
      m = mean v
  in LA.sumElements ((v - LA.scalar m) ^ (2 :: Int)) / max 1 (n - 1)
