{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | Hypothesis tests with a unified result format.
--
-- Most tests delegate to the @statistics@ package internals
-- (@Statistics.Test.*@) and add hanalyze-specific niceties: a single
-- 'TestResult' record, effect sizes, confidence intervals, and a
-- consistent two-sided / one-sided @Alternative@ parameter.
--
-- == Test categories
--
--   * __Parametric (location)__: 'tTest1Sample', 'tTestPaired',
--     'tTestWelch', 'tTestStudent', 'anovaOneWay'
--   * __Non-parametric (location / rank)__: 'mannWhitneyU',
--     'wilcoxonSignedRank', 'kruskalWallis'
--   * __Goodness-of-fit / independence__: 'chiSquareGOF',
--     'chiSquareIndep', 'fisherExact2x2'
--   * __Normality__: 'shapiroWilk', 'kolmogorovSmirnovNormal'
--   * __Variance equality__: 'leveneTest', 'bartlettTest', 'fTestVariance'
module Hanalyze.Stat.Test
  ( -- * Common types
    TestResult (..)
  , Alternative (..)
    -- * Parametric (location)
  , tTest1Sample
  , tTestPaired
  , tTestWelch
  , tTestStudent
  , anovaOneWay
    -- * Non-parametric (location / rank)
  , mannWhitneyU
  , wilcoxonSignedRank
  , kruskalWallis
    -- * Goodness-of-fit / independence
  , chiSquareGOF
  , chiSquareIndep
  , fisherExact2x2
    -- * Normality
  , shapiroWilk
  , kolmogorovSmirnovNormal
    -- * Variance equality
  , leveneTest
  , bartlettTest
  , fTestVariance
  ) where

import qualified Data.List                      as L
import           Data.Ord                       (comparing)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Vector.Storable           as VS
import qualified Data.Vector.Unboxed            as VU
import qualified Numeric.LinearAlgebra          as LA
import qualified Statistics.Distribution        as SD
import qualified Statistics.Distribution.ChiSquared as ChiSq
import qualified Statistics.Distribution.FDistribution as FDist
import qualified Statistics.Distribution.Normal as Normal
import qualified Statistics.Distribution.StudentT as StuT
import qualified Statistics.Test.KolmogorovSmirnov as TKS
import qualified Statistics.Test.KruskalWallis  as TKW
import qualified Statistics.Test.MannWhitneyU   as TMW
import qualified Statistics.Test.StudentT       as TST
import qualified Statistics.Test.Types          as TT
import qualified Statistics.Types               as STy

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Tail / sidedness of a test.
data Alternative
  = TwoSided    -- ^ default; @H1: parameter ≠ value@
  | Less        -- ^ @H1: parameter < value@
  | Greater     -- ^ @H1: parameter > value@
  deriving (Show, Eq)

-- | Unified result of a hypothesis test.
data TestResult = TestResult
  { trMethod       :: !Text
    -- ^ Human-readable name of the test.
  , trStatistic    :: !Double
    -- ^ Test statistic (t, F, chi², U, W, ...).
  , trDf           :: !(Maybe (Double, Maybe Double))
    -- ^ Degrees of freedom: @Just (df1, Just df2)@ for F-tests
    --   (numerator & denominator), @Just (df, Nothing)@ for one-DF
    --   tests, @Nothing@ when not applicable.
  , trPValue       :: !Double
    -- ^ Two-sided / one-sided p-value depending on 'trAlternative'.
  , trEffect       :: !(Maybe (Text, Double))
    -- ^ Optional effect size as @(name, value)@ — Cohen's d, η², φ, …
  , trCI           :: !(Maybe (Double, Double))
    -- ^ Optional 95% CI for the test parameter (mean diff, etc.).
  , trAlternative  :: !Alternative
  , trNote         :: !(Maybe Text)
    -- ^ Free-form caveat (e.g. "small-sample asymptotic; consider exact").
  } deriving (Show)

-- | Convert a @statistics@ package @Test@ result into our 'TestResult'.
fromStatTest
  :: Text              -- ^ method label
  -> Alternative       -- ^ alternative used
  -> Maybe (Double, Maybe Double)  -- ^ degrees of freedom
  -> Maybe (Text, Double)          -- ^ effect size
  -> Maybe (Double, Double)        -- ^ confidence interval
  -> Maybe Text                    -- ^ note
  -> TT.Test d
  -> TestResult
fromStatTest method alt df eff ci note t =
  TestResult
    { trMethod      = method
    , trStatistic   = TT.testStatistics t
    , trDf          = df
    , trPValue      = STy.pValue (TT.testSignificance t)
    , trEffect      = eff
    , trCI          = ci
    , trAlternative = alt
    , trNote        = note
    }

-- | Convert hanalyze @Alternative@ to @statistics@ @PositionTest@ for
-- the location-shift family of tests.
posTest :: Alternative -> TT.PositionTest
posTest TwoSided = TT.SamplesDiffer
posTest Greater  = TT.AGreater
posTest Less     = TT.BGreater

-- | Conversion helpers between Storable vectors and Vector.Unboxed
-- (the @statistics@ package family uses Unboxed).
toU :: LA.Vector Double -> VU.Vector Double
toU = VU.fromList . LA.toList

-- ---------------------------------------------------------------------------
-- Parametric (location)
-- ---------------------------------------------------------------------------

-- | One-sample t-test against a hypothesised population mean @μ₀@.
tTest1Sample
  :: LA.Vector Double  -- ^ Sample.
  -> Double            -- ^ μ₀ (hypothesised mean).
  -> Alternative
  -> TestResult
tTest1Sample xs mu0 alt =
  let n     = LA.size xs
      xMean = LA.sumElements xs / fromIntegral n
      xVar  = LA.sumElements ((xs - LA.scalar xMean) ^ (2 :: Int))
              / fromIntegral (n - 1)
      seM   = sqrt (xVar / fromIntegral n)
      tStat = (xMean - mu0) / seM
      df    = fromIntegral (n - 1) :: Double
      tDist = StuT.studentT df
      tail_ = altTail alt
      p     = pFromT tail_ tStat tDist
      cohenD = (xMean - mu0) / sqrt xVar
      tCrit  = SD.quantile tDist 0.975
      ci     = (xMean - tCrit * seM, xMean + tCrit * seM)
  in TestResult
       { trMethod      = "One-sample t-test"
       , trStatistic   = tStat
       , trDf          = Just (df, Nothing)
       , trPValue      = p
       , trEffect      = Just ("Cohen's d", cohenD)
       , trCI          = Just ci
       , trAlternative = alt
       , trNote        = Nothing
       }

-- | Paired t-test on @(x, y)@ pairs, testing @H0: mean(x − y) = 0@.
tTestPaired
  :: LA.Vector Double
  -> LA.Vector Double
  -> Alternative
  -> TestResult
tTestPaired xs ys alt =
  let diffs = xs - ys
  in (tTest1Sample diffs 0 alt) { trMethod = "Paired t-test" }

-- | Welch's two-sample t-test (does not assume equal variance).
tTestWelch
  :: LA.Vector Double
  -> LA.Vector Double
  -> Alternative
  -> TestResult
tTestWelch xs ys alt =
  let pt = posTest alt
      tx = TST.welchTTest pt (toU xs) (toU ys)
      n1 = fromIntegral (LA.size xs) :: Double
      n2 = fromIntegral (LA.size ys) :: Double
      m1 = LA.sumElements xs / n1
      m2 = LA.sumElements ys / n2
      v1 = LA.sumElements ((xs - LA.scalar m1) ^ (2 :: Int)) / (n1 - 1)
      v2 = LA.sumElements ((ys - LA.scalar m2) ^ (2 :: Int)) / (n2 - 1)
      pooledSd = sqrt ((v1 + v2) / 2)
      cohenD   = if pooledSd > 0 then (m1 - m2) / pooledSd else 0
      df = (v1/n1 + v2/n2) ^ (2 :: Int)
           / ((v1/n1)^(2::Int)/(n1-1) + (v2/n2)^(2::Int)/(n2-1))
  in case tx of
       Nothing -> noResultTRR "Welch's t-test" alt "insufficient samples"
       Just t  -> fromStatTest "Welch's t-test" alt
                    (Just (df, Nothing))
                    (Just ("Cohen's d", cohenD))
                    Nothing
                    Nothing
                    t

-- | Student's two-sample t-test (assumes equal variance).
tTestStudent
  :: LA.Vector Double
  -> LA.Vector Double
  -> Alternative
  -> TestResult
tTestStudent xs ys alt =
  let pt = posTest alt
      tx = TST.studentTTest pt (toU xs) (toU ys)
      n1 = fromIntegral (LA.size xs) :: Double
      n2 = fromIntegral (LA.size ys) :: Double
      m1 = LA.sumElements xs / n1
      m2 = LA.sumElements ys / n2
      v1 = LA.sumElements ((xs - LA.scalar m1) ^ (2 :: Int)) / (n1 - 1)
      v2 = LA.sumElements ((ys - LA.scalar m2) ^ (2 :: Int)) / (n2 - 1)
      pooledV = ((n1-1)*v1 + (n2-1)*v2) / (n1 + n2 - 2)
      cohenD  = if pooledV > 0 then (m1 - m2) / sqrt pooledV else 0
      df      = n1 + n2 - 2
  in case tx of
       Nothing -> noResultTRR "Student's t-test" alt "insufficient samples"
       Just t  -> fromStatTest "Student's t-test" alt
                    (Just (df, Nothing))
                    (Just ("Cohen's d", cohenD))
                    Nothing
                    Nothing
                    t

-- | One-way ANOVA across @k@ groups (F-test on between- vs
-- within-group variance). Returns η² as effect size.
anovaOneWay :: [LA.Vector Double] -> TestResult
anovaOneWay groups
  | length groups < 2 =
      noResultTRR "One-way ANOVA" TwoSided "need ≥ 2 groups"
  | otherwise =
      let k     = length groups
          ns    = map (fromIntegral . LA.size) groups :: [Double]
          n     = sum ns
          means = [ LA.sumElements g / fromIntegral (LA.size g)
                  | g <- groups ]
          grand = sum (zipWith (*) ns means) / n
          ssB   = sum [ ni * (mi - grand)^(2::Int)
                      | (ni, mi) <- zip ns means ]
          ssW   = sum [ LA.sumElements ((g - LA.scalar mi)^(2::Int))
                      | (g, mi) <- zip groups means ]
          dfB   = fromIntegral (k - 1) :: Double
          dfW   = n - fromIntegral k
          msB   = ssB / dfB
          msW   = ssW / dfW
          fStat = msB / msW
          pVal  = SD.complCumulative (FDist.fDistribution (round dfB) (round dfW)) fStat
          eta2  = ssB / (ssB + ssW)
      in TestResult
           { trMethod      = "One-way ANOVA"
           , trStatistic   = fStat
           , trDf          = Just (dfB, Just dfW)
           , trPValue      = pVal
           , trEffect      = Just ("η²", eta2)
           , trCI          = Nothing
           , trAlternative = TwoSided
           , trNote        = Nothing
           }

-- ---------------------------------------------------------------------------
-- Non-parametric
-- ---------------------------------------------------------------------------

-- | Mann–Whitney U test (Wilcoxon rank-sum).
mannWhitneyU
  :: LA.Vector Double
  -> LA.Vector Double
  -> Alternative
  -> TestResult
mannWhitneyU xs ys alt =
  let pt    = posTest alt
      pVal  = STy.mkPValue 0.05  -- threshold; actual p inside Test
      r     = TMW.mannWhitneyUtest pt pVal (toU xs) (toU ys)
      m     = fromIntegral (LA.size xs) :: Double
      n     = fromIntegral (LA.size ys) :: Double
  in case r of
       Nothing -> noResultTRR "Mann-Whitney U" alt "samples too small"
       Just _testRes ->
         -- statistics' API returns TestResult (Significant/NotSignificant)
         -- without statistic. We compute U manually for richer output.
         let (u1, u2, p) = mannWhitneyManual (toU xs) (toU ys) alt
         in TestResult
              { trMethod      = "Mann-Whitney U"
              , trStatistic   = min u1 u2
              , trDf          = Nothing
              , trPValue      = p
              , trEffect      = Just ("rank-biserial r", rankBiserial u1 m n)
              , trCI          = Nothing
              , trAlternative = alt
              , trNote        = Just "normal-approximation p-value"
              }

-- | Wilcoxon signed-rank test (paired, non-parametric).
wilcoxonSignedRank
  :: LA.Vector Double
  -> LA.Vector Double
  -> Alternative
  -> TestResult
wilcoxonSignedRank xs ys alt =
  let (wPlus, wMinus, p) = wilcoxonManual xs ys alt
  in TestResult
       { trMethod      = "Wilcoxon signed-rank"
       , trStatistic   = min wPlus wMinus
       , trDf          = Nothing
       , trPValue      = p
       , trEffect      = Nothing
       , trCI          = Nothing
       , trAlternative = alt
       , trNote        = Just "normal-approximation p-value"
       }

-- | Kruskal-Wallis H test (k-group non-parametric ANOVA).
kruskalWallis :: [LA.Vector Double] -> TestResult
kruskalWallis groups
  | length groups < 2 =
      noResultTRR "Kruskal-Wallis" TwoSided "need ≥ 2 groups"
  | otherwise =
      let groupsU = map toU groups
          h = TKW.kruskalWallis groupsU :: Double
          k = length groups
          dfH = fromIntegral (k - 1) :: Double
          p = SD.complCumulative (ChiSq.chiSquared (k - 1)) h
      in TestResult
           { trMethod      = "Kruskal-Wallis"
           , trStatistic   = h
           , trDf          = Just (dfH, Nothing)
           , trPValue      = p
           , trEffect      = Nothing
           , trCI          = Nothing
           , trAlternative = TwoSided
           , trNote        = Just "chi-square approximation"
           }

-- ---------------------------------------------------------------------------
-- Goodness-of-fit / independence
-- ---------------------------------------------------------------------------

-- | Chi-square goodness-of-fit test.
-- @observed@ and @expected@ must have the same length and @sum expected
-- = sum observed@.
chiSquareGOF :: LA.Vector Double -> LA.Vector Double -> TestResult
chiSquareGOF observed expected =
  let chi2 = LA.sumElements
              (((observed - expected) ^ (2 :: Int)) / expected)
      df   = fromIntegral (LA.size observed - 1) :: Double
      p    = SD.complCumulative (ChiSq.chiSquared (round df)) chi2
  in TestResult
       { trMethod      = "Chi-square goodness-of-fit"
       , trStatistic   = chi2
       , trDf          = Just (df, Nothing)
       , trPValue      = p
       , trEffect      = Nothing
       , trCI          = Nothing
       , trAlternative = TwoSided
       , trNote        = Nothing
       }

-- | Chi-square independence test on a contingency table (rows × cols).
-- Returns Cramér's V as effect size.
chiSquareIndep :: LA.Matrix Double -> TestResult
chiSquareIndep tbl =
  let r        = LA.rows tbl
      c        = LA.cols tbl
      rowSums  = tbl LA.#> LA.konst 1 c
      colSums  = LA.konst 1 r LA.<# tbl
      total    = LA.sumElements tbl
      expected = LA.outer rowSums colSums / LA.scalar total
      diff2    = (tbl - expected) ^ (2 :: Int)
      contrib  = LA.sumElements (diff2 / expected)
      df       = fromIntegral ((r - 1) * (c - 1)) :: Double
      p        = SD.complCumulative (ChiSq.chiSquared (round df)) contrib
      cramerV  = sqrt (contrib / (total * fromIntegral (min r c - 1)))
  in TestResult
       { trMethod      = "Chi-square independence"
       , trStatistic   = contrib
       , trDf          = Just (df, Nothing)
       , trPValue      = p
       , trEffect      = Just ("Cramér's V", cramerV)
       , trCI          = Nothing
       , trAlternative = TwoSided
       , trNote        = Nothing
       }

-- | Fisher's exact test on a 2×2 contingency table.
-- @[[a, b], [c, d]]@. Returns the (one-sided or two-sided) exact
-- p-value from the hypergeometric distribution.
fisherExact2x2 :: ((Int, Int), (Int, Int)) -> Alternative -> TestResult
fisherExact2x2 ((a, b), (c, d)) alt =
  let n       = a + b + c + d
      r1      = a + b   -- row 1 marginal
      c1      = a + c   -- col 1 marginal
      -- Hypergeometric: drawing r1 items from n where c1 are "success".
      pmf k   = fromIntegral (choose c1 k * choose (n - c1) (r1 - k))
              / fromIntegral (choose n r1)
      kMin    = max 0 (r1 - (n - c1))
      kMax    = min r1 c1
      pAt     = pmf a
      p       = case alt of
        Less     -> sum [pmf k | k <- [kMin .. a]]
        Greater  -> sum [pmf k | k <- [a .. kMax]]
        TwoSided ->
          -- Sum of pmf at all k with pmf k <= pmf a (standard def).
          sum [pmf k | k <- [kMin .. kMax], pmf k <= pAt + 1e-15]
      oddsRatio | b * c == 0 = 1 / 0
                | otherwise  = fromIntegral (a * d) / fromIntegral (b * c)
  in TestResult
       { trMethod      = "Fisher's exact (2×2)"
       , trStatistic   = oddsRatio
       , trDf          = Nothing
       , trPValue      = p
       , trEffect      = Just ("odds ratio", oddsRatio)
       , trCI          = Nothing
       , trAlternative = alt
       , trNote        = Nothing
       }

-- ---------------------------------------------------------------------------
-- Normality
-- ---------------------------------------------------------------------------

-- | Shapiro-Wilk test (@n@ ≤ 5000). Implements Royston's 1992
-- approximation. Returns the W statistic and asymptotic p-value.
shapiroWilk :: LA.Vector Double -> TestResult
shapiroWilk xs0 =
  let n      = LA.size xs0
      xs     = LA.toList (sortVec xs0)  :: [Double]
      mean   = sum xs / fromIntegral n
      ss     = sum [ (x - mean) ^ (2 :: Int) | x <- xs ]
      -- Royston coefficients via Bloom's expected normal order stats.
      -- Approximate m_i = Φ⁻¹((i − 3/8) / (n + 1/4)).
      mIs    = [ SD.quantile Normal.standard
                   ((fromIntegral i - 3 / 8) / (fromIntegral n + 1 / 4))
               | i <- [1 .. n] ]
      mTm    = sum [m^(2::Int) | m <- mIs]
      aIs    = [ m / sqrt mTm | m <- mIs ]
      wNum   = sum (zipWith (*) aIs xs) ^ (2 :: Int)
      w      = wNum / ss
      -- Royston 1992 approximation for n ∈ [4, 11]
      -- For larger n use the lognormal-of-(1-W) approximation.
      pApprox
        | n < 4     = 1
        | n <= 11   =
            let g  = -2.273 + 0.459 * fromIntegral n
                mu = 0.5440 - 0.39978 * fromIntegral n
                     + 0.025054 * fromIntegral n^(2::Int)
                     - 0.0006714 * fromIntegral n^(3::Int)
                sigma = exp (1.30405 - 0.04213 * fromIntegral n
                            - 0.0005006 * fromIntegral n^(2::Int))
                z = (g + log (1 - w) - mu) / sigma
            in 1 - SD.cumulative Normal.standard z
        | otherwise =
            let mu    = -1.5861 - 0.31082 * log (fromIntegral n)
                        - 0.083751 * (log (fromIntegral n))^(2::Int)
                        + 0.0038915 * (log (fromIntegral n))^(3::Int)
                sigma = exp (-0.4803 - 0.082676 * log (fromIntegral n)
                            + 0.0030302 * (log (fromIntegral n))^(2::Int))
                z = (log (1 - w) - mu) / sigma
            in 1 - SD.cumulative Normal.standard z
  in TestResult
       { trMethod      = "Shapiro-Wilk"
       , trStatistic   = w
       , trDf          = Nothing
       , trPValue      = pApprox
       , trEffect      = Nothing
       , trCI          = Nothing
       , trAlternative = TwoSided
       , trNote        = Just "Royston 1992 approximation; n ≤ 5000"
       }

-- | Kolmogorov-Smirnov goodness-of-fit test against the standard
-- Normal distribution (one-sample).
kolmogorovSmirnovNormal :: LA.Vector Double -> TestResult
kolmogorovSmirnovNormal xs =
  let xsU = toU xs
      d   = TKS.kolmogorovSmirnovD Normal.standard xsU
      n   = LA.size xs
      p   = TKS.kolmogorovSmirnovProbability n d
  in TestResult
       { trMethod      = "Kolmogorov-Smirnov (vs Normal(0,1))"
       , trStatistic   = d
       , trDf          = Nothing
       , trPValue      = p
       , trEffect      = Nothing
       , trCI          = Nothing
       , trAlternative = TwoSided
       , trNote        = Nothing
       }

-- ---------------------------------------------------------------------------
-- Variance equality
-- ---------------------------------------------------------------------------

-- | Levene's test for equality of variances across k groups.
-- Uses median-based formulation (Brown-Forsythe variant) which is
-- more robust than mean-based to non-normal data.
leveneTest :: [LA.Vector Double] -> TestResult
leveneTest groups
  | length groups < 2 =
      noResultTRR "Levene's test" TwoSided "need ≥ 2 groups"
  | otherwise =
      let k       = length groups
          ns      = map LA.size groups
          n       = sum ns
          medians = map sampleMedian groups
          -- Z_ij = |x_ij - median_i|
          zs      = [ LA.cmap (\x -> abs (x - med)) g
                    | (g, med) <- zip groups medians ]
          zMeans  = [ LA.sumElements z / fromIntegral (LA.size z) | z <- zs ]
          zGrand  = sum [ LA.sumElements z | z <- zs ] / fromIntegral n
          ssB     = sum [ fromIntegral ni * (zi - zGrand) ^ (2 :: Int)
                        | (ni, zi) <- zip ns zMeans ]
          ssW     = sum [ LA.sumElements ((z - LA.scalar zi)^(2::Int))
                        | (z, zi) <- zip zs zMeans ]
          dfB     = fromIntegral (k - 1) :: Double
          dfW     = fromIntegral (n - k) :: Double
          fStat   = (ssB / dfB) / (ssW / dfW)
          p       = SD.complCumulative
                      (FDist.fDistribution (k - 1) (n - k)) fStat
      in TestResult
           { trMethod      = "Levene's test (Brown-Forsythe)"
           , trStatistic   = fStat
           , trDf          = Just (dfB, Just dfW)
           , trPValue      = p
           , trEffect      = Nothing
           , trCI          = Nothing
           , trAlternative = TwoSided
           , trNote        = Nothing
           }

-- | Bartlett's test for equality of variances (assumes normality,
-- more powerful than Levene when normality holds).
bartlettTest :: [LA.Vector Double] -> TestResult
bartlettTest groups
  | length groups < 2 =
      noResultTRR "Bartlett's test" TwoSided "need ≥ 2 groups"
  | otherwise =
      let k    = length groups
          ns   = map (fromIntegral . LA.size) groups :: [Double]
          n    = sum ns
          vars = map sampleVariance groups
          spv  = sum [ (ni - 1) * vi | (ni, vi) <- zip ns vars ]
                 / (n - fromIntegral k)
          numer = (n - fromIntegral k) * log spv
                  - sum [ (ni - 1) * log vi | (ni, vi) <- zip ns vars ]
          c    = 1 + (1 / (3 * fromIntegral (k - 1)))
                   * (sum [1 / (ni - 1) | ni <- ns] - 1 / (n - fromIntegral k))
          chi2 = numer / c
          dfB  = fromIntegral (k - 1) :: Double
          p    = SD.complCumulative (ChiSq.chiSquared (k - 1)) chi2
      in TestResult
           { trMethod      = "Bartlett's test"
           , trStatistic   = chi2
           , trDf          = Just (dfB, Nothing)
           , trPValue      = p
           , trEffect      = Nothing
           , trCI          = Nothing
           , trAlternative = TwoSided
           , trNote        = Just "assumes normality"
           }

-- | F-test for variance ratio between two samples (parametric).
fTestVariance :: LA.Vector Double -> LA.Vector Double -> Alternative
              -> TestResult
fTestVariance xs ys alt =
  let n1 = fromIntegral (LA.size xs) :: Double
      n2 = fromIntegral (LA.size ys) :: Double
      m1 = LA.sumElements xs / n1
      m2 = LA.sumElements ys / n2
      v1 = LA.sumElements ((xs - LA.scalar m1)^(2::Int)) / (n1 - 1)
      v2 = LA.sumElements ((ys - LA.scalar m2)^(2::Int)) / (n2 - 1)
      f  = v1 / v2
      df1 = n1 - 1
      df2 = n2 - 1
      fd  = FDist.fDistribution (round df1) (round df2)
      p  = case alt of
        TwoSided -> 2 * min (SD.cumulative fd f) (SD.complCumulative fd f)
        Greater  -> SD.complCumulative fd f
        Less     -> SD.cumulative fd f
  in TestResult
       { trMethod      = "F-test for equal variances"
       , trStatistic   = f
       , trDf          = Just (df1, Just df2)
       , trPValue      = p
       , trEffect      = Just ("variance ratio", f)
       , trCI          = Nothing
       , trAlternative = alt
       , trNote        = Just "assumes normality"
       }

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Sentinel result when test inputs are insufficient.
noResultTRR :: Text -> Alternative -> Text -> TestResult
noResultTRR method alt msg = TestResult
  { trMethod      = method
  , trStatistic   = 0
  , trDf          = Nothing
  , trPValue      = 1 / 0
  , trEffect      = Nothing
  , trCI          = Nothing
  , trAlternative = alt
  , trNote        = Just msg
  }

-- | Side / tail used for p-value computation.
data Tail = TLeft | TRight | TBoth

altTail :: Alternative -> Tail
altTail Less     = TLeft
altTail Greater  = TRight
altTail TwoSided = TBoth

pFromT :: Tail -> Double -> StuT.StudentT -> Double
pFromT TLeft  t d = SD.cumulative d t
pFromT TRight t d = SD.complCumulative d t
pFromT TBoth  t d = 2 * min (SD.cumulative d t) (SD.complCumulative d t)

-- | Sample median.
sampleMedian :: LA.Vector Double -> Double
sampleMedian v =
  let xs = sortDoubles (LA.toList v)
      n  = length xs
  in if even n
       then (xs !! (n `div` 2 - 1) + xs !! (n `div` 2)) / 2
       else xs !! (n `div` 2)
  where
    sortDoubles :: [Double] -> [Double]
    sortDoubles []     = []
    sortDoubles (x:xs) = sortDoubles [y | y <- xs, y < x]
                      ++ [x]
                      ++ sortDoubles [y | y <- xs, y >= x]

-- | Unbiased sample variance.
sampleVariance :: LA.Vector Double -> Double
sampleVariance v =
  let n = fromIntegral (LA.size v) :: Double
      m = LA.sumElements v / n
  in LA.sumElements ((v - LA.scalar m) ^ (2 :: Int)) / (n - 1)

-- | n choose k (Int).
choose :: Int -> Int -> Integer
choose n k
  | k < 0 || k > n = 0
  | k == 0 || k == n = 1
  | otherwise = product [fromIntegral (n - i + 1) | i <- [1 .. k]]
                `div` product [fromIntegral i | i <- [1 .. k]]

-- | Sort an LA vector (ascending) via 'Data.List.sort' (mergesort,
-- O(n log n) / O(n) space). Phase 11b (2026-05-14): replaced naive list
-- quicksort to avoid pivot-bias O(n²) blowup on large inputs.
sortVec :: LA.Vector Double -> LA.Vector Double
sortVec v = LA.fromList (L.sort (LA.toList v))

-- | Manual Mann-Whitney U with normal approximation (handles ties).
mannWhitneyManual
  :: VU.Vector Double
  -> VU.Vector Double
  -> Alternative
  -> (Double, Double, Double)
mannWhitneyManual xs ys alt =
  let n1 = fromIntegral (VU.length xs) :: Double
      n2 = fromIntegral (VU.length ys) :: Double
      tagged = [(x, 1::Int) | x <- VU.toList xs]
            ++ [(y, 2::Int) | y <- VU.toList ys]
      sorted = L.sortBy (comparing fst) tagged
      ranks  = assignRanks (map fst sorted)
      r1     = sum [ rk | (rk, (_, g)) <- zip ranks sorted, g == 1 ]
      u1     = r1 - n1 * (n1 + 1) / 2
      u2     = n1 * n2 - u1
      u      = min u1 u2
      meanU  = n1 * n2 / 2
      varU   = n1 * n2 * (n1 + n2 + 1) / 12
      z      = (u - meanU) / sqrt varU
      p      = case alt of
        TwoSided -> 2 * SD.cumulative Normal.standard z
        Less     -> SD.cumulative Normal.standard z
        Greater  -> SD.complCumulative Normal.standard z
  in (u1, u2, p)

-- | Average ranks (handles ties via mid-rank).
assignRanks :: [Double] -> [Double]
assignRanks vs =
  let n = length vs
      pairs = zip [1 :: Int ..] vs
      go [] = []
      go ((i, v):rest) =
        let same = takeWhile ((== v) . snd) ((i, v):rest)
            others = drop (length same) ((i, v):rest)
            ranks = map fromIntegral (map fst same)
            avg = sum ranks / fromIntegral (length ranks)
        in replicate (length same) avg ++ go others
  in go pairs ++ [] ++ replicate 0 (fromIntegral n)

-- | Rank-biserial correlation effect size for Mann-Whitney.
rankBiserial :: Double -> Double -> Double -> Double
rankBiserial u1 m n = 1 - 2 * u1 / (m * n)

-- | Manual Wilcoxon signed-rank with normal approximation.
wilcoxonManual
  :: LA.Vector Double
  -> LA.Vector Double
  -> Alternative
  -> (Double, Double, Double)
wilcoxonManual xs ys alt =
  let diffs   = LA.toList (xs - ys)
      nonZero = filter (/= 0) diffs
      absD    = map abs nonZero
      ranks   = assignRanks absD
      paired  = zip nonZero ranks
      wPlus   = sum [ rk | (d, rk) <- paired, d > 0 ]
      wMinus  = sum [ rk | (d, rk) <- paired, d < 0 ]
      n       = fromIntegral (length nonZero) :: Double
      meanW   = n * (n + 1) / 4
      varW    = n * (n + 1) * (2 * n + 1) / 24
      w       = min wPlus wMinus
      z       = (w - meanW) / sqrt varW
      p       = case alt of
        TwoSided -> 2 * SD.cumulative Normal.standard z
        Less     -> SD.cumulative Normal.standard z
        Greater  -> SD.complCumulative Normal.standard z
  in (wPlus, wMinus, p)

