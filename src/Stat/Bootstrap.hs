{-# LANGUAGE OverloadedStrings #-}
-- | Bootstrap resampling and permutation tests.
--
-- @
-- import Stat.Bootstrap
-- import qualified System.Random.MWC as MWC
--
-- gen <- MWC.createSystemRandom
-- mean_ci <- bootstrapCI 10000 0.95 sampleMean xs gen
-- @
--
-- Provides:
--
--   * 'bootstrap' — generic resampling, returns a list of statistics.
--   * 'bootstrapCI' — percentile interval.
--   * 'bootstrapBcaCI' — bias-corrected & accelerated (BCa) interval.
--   * 'permutationTest' — permutation test for two-sample location.
module Stat.Bootstrap
  ( -- * Generic resampling
    bootstrap
  , bootstrapCI
  , bootstrapBcaCI
    -- * Permutation tests
  , permutationTest
    -- * Statistics
  , sampleMean
  , sampleVar
  , sampleMedian
  ) where

import qualified Numeric.LinearAlgebra            as LA
import qualified Statistics.Distribution          as SD
import qualified Statistics.Distribution.Normal   as Normal
import qualified System.Random.MWC                as MWC
import qualified Data.Vector                      as V
import qualified Data.Vector.Mutable              as VM
import qualified Data.Vector.Storable             as VS
import qualified Data.Vector.Storable.Mutable     as MVS
import           Control.Monad                    (replicateM, forM)
import           Data.List                        (sort)

-- ---------------------------------------------------------------------------
-- Bootstrap
-- ---------------------------------------------------------------------------

-- | Bootstrap @n@ resamples and apply the statistic. Returns the list
-- of @n@ statistic values.
bootstrap
  :: Int                              -- ^ Number of resamples.
  -> (LA.Vector Double -> Double)     -- ^ Statistic.
  -> LA.Vector Double                 -- ^ Sample.
  -> MWC.GenIO
  -> IO [Double]
bootstrap nReps stat xs gen = do
  -- LA.Vector Double = Storable.Vector Double under the hood, so we can
  -- fill a Storable.Mutable buffer and freeze it directly to an
  -- LA.Vector. The previous implementation used [Double] + (!!), giving
  -- O(n) per index → O(n²·B) total; this is O(n·B).
  let n = LA.size xs
  forM [1 .. nReps] $ \_ -> do
    mv <- MVS.unsafeNew n
    let go i
          | i >= n    = pure ()
          | otherwise = do
              j <- MWC.uniformR (0, n - 1) gen
              MVS.unsafeWrite mv i (xs `LA.atIndex` j)
              go (i + 1)
    go 0
    frozen <- VS.unsafeFreeze mv
    pure (stat frozen)

-- | Percentile bootstrap CI: @[(α/2)-quantile, (1-α/2)-quantile]@ of
-- the resampled statistic distribution.
bootstrapCI
  :: Int                              -- ^ Number of resamples.
  -> Double                           -- ^ Confidence level (0 < c < 1).
  -> (LA.Vector Double -> Double)     -- ^ Statistic.
  -> LA.Vector Double                 -- ^ Sample.
  -> MWC.GenIO
  -> IO (Double, Double)
bootstrapCI nReps conf stat xs gen = do
  bs <- bootstrap nReps stat xs gen
  let alpha = 1 - conf
      sorted = sort bs
      lo = quantile (alpha / 2) sorted
      hi = quantile (1 - alpha / 2) sorted
  pure (lo, hi)

-- | Bias-corrected & accelerated (BCa) bootstrap CI (Efron 1987).
-- Improves on percentile CI when the bootstrap distribution is biased
-- or skewed.
bootstrapBcaCI
  :: Int
  -> Double
  -> (LA.Vector Double -> Double)
  -> LA.Vector Double
  -> MWC.GenIO
  -> IO (Double, Double)
bootstrapBcaCI nReps conf stat xs gen = do
  bs <- bootstrap nReps stat xs gen
  let alpha   = 1 - conf
      theta0  = stat xs
      sorted  = sort bs
      -- z0: bias correction.
      pBelow  = fromIntegral (length [b | b <- bs, b < theta0])
                / fromIntegral nReps
      z0      = SD.quantile Normal.standard (clip pBelow)
      clip p  = max 1e-10 (min (1 - 1e-10) p)
      -- a: acceleration via jackknife.
      n       = LA.size xs
      xsList  = LA.toList xs
      jackVals = [ stat (LA.fromList (omit i xsList))
                 | i <- [0 .. n - 1] ]
      jMean   = sum jackVals / fromIntegral n
      jDiffs  = [(jMean - jv) | jv <- jackVals]
      num     = sum [d^(3::Int) | d <- jDiffs]
      den     = 6 * (sum [d^(2::Int) | d <- jDiffs] ** 1.5)
      a       = if den == 0 then 0 else num / den
      -- Adjusted alphas.
      zL      = SD.quantile Normal.standard (alpha / 2)
      zU      = SD.quantile Normal.standard (1 - alpha / 2)
      alphaLo = SD.cumulative Normal.standard
                  (z0 + (z0 + zL) / (1 - a * (z0 + zL)))
      alphaHi = SD.cumulative Normal.standard
                  (z0 + (z0 + zU) / (1 - a * (z0 + zU)))
      lo      = quantile alphaLo sorted
      hi      = quantile alphaHi sorted
  pure (lo, hi)

-- | Permutation test for difference in means between two samples.
-- Returns @(observed diff, p-value)@.
permutationTest
  :: Int                              -- ^ Number of permutations.
  -> LA.Vector Double                 -- ^ Sample 1.
  -> LA.Vector Double                 -- ^ Sample 2.
  -> MWC.GenIO
  -> IO (Double, Double)
permutationTest nPerms xs ys gen = do
  let xsL = LA.toList xs
      ysL = LA.toList ys
      n1  = length xsL
      _n2 = length ysL
      pooled = xsL ++ ysL
      meanOf vs = sum vs / fromIntegral (length vs)
      observedDiff = meanOf xsL - meanOf ysL
  permDiffs <- forM [1 .. nPerms] $ \_ -> do
    shuffled <- shuffleList pooled gen
    let g1 = take n1 shuffled
        g2 = drop n1 shuffled
    pure (meanOf g1 - meanOf g2)
  let p = fromIntegral (length [d | d <- permDiffs, abs d >= abs observedDiff])
          / fromIntegral nPerms
  pure (observedDiff, p)

-- ---------------------------------------------------------------------------
-- Statistics
-- ---------------------------------------------------------------------------

-- | Sample mean.
sampleMean :: LA.Vector Double -> Double
sampleMean v = LA.sumElements v / fromIntegral (LA.size v)

-- | Unbiased sample variance.
sampleVar :: LA.Vector Double -> Double
sampleVar v =
  let n = fromIntegral (LA.size v) :: Double
      m = sampleMean v
  in LA.sumElements ((v - LA.scalar m) ^ (2 :: Int)) / (n - 1)

-- | Sample median.
sampleMedian :: LA.Vector Double -> Double
sampleMedian v =
  let xs = sort (LA.toList v)
      n  = length xs
  in if even n
       then (xs !! (n `div` 2 - 1) + xs !! (n `div` 2)) / 2
       else xs !! (n `div` 2)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Linear-interpolation quantile from a sorted list.
quantile :: Double -> [Double] -> Double
quantile q sorted
  | null sorted = 0
  | q <= 0      = head sorted
  | q >= 1      = last sorted
  | otherwise   =
      let n  = length sorted
          h  = q * fromIntegral (n - 1)
          lo = floor h
          hi = ceiling h
          fr = h - fromIntegral lo
      in if lo == hi
           then sorted !! lo
           else sorted !! lo * (1 - fr) + sorted !! hi * fr

-- | Omit element at index i.
omit :: Int -> [a] -> [a]
omit i xs = take i xs ++ drop (i + 1) xs

-- | Shuffle a list (Fisher-Yates) via mutable Vector.
shuffleList :: [a] -> MWC.GenIO -> IO [a]
shuffleList xs gen = do
  let n = length xs
  v <- V.thaw (V.fromList xs)
  let loop i
        | i <= 0 = pure ()
        | otherwise = do
            j <- MWC.uniformR (0, i) gen
            a <- VM.read v i
            b <- VM.read v j
            VM.write v i b
            VM.write v j a
            loop (i - 1)
  loop (n - 1)
  V.toList <$> V.freeze v
