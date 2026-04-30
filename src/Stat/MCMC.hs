{-# LANGUAGE OverloadedStrings #-}
module Stat.MCMC
  ( autocorr
  , hdi
  , ess
  ) where

import Data.List (minimumBy, sort)
import Data.Ord  (comparing)
import qualified Data.Vector as V

-- | Autocorrelation at lags 0 .. min(maxLag, n-1).
-- Uses O(n × maxLag) time with Vector indexing.
autocorr :: Int -> [Double] -> [(Int, Double)]
autocorr maxLag xs =
  let v   = V.fromList xs
      n   = V.length v
      mu  = V.sum v / fromIntegral n
      var = V.sum (V.map (\x -> (x - mu) ^ (2 :: Int)) v) / fromIntegral n
      acf k
        | var == 0 || k >= n = 0
        | otherwise =
            V.sum (V.zipWith (\a b -> (a - mu) * (b - mu))
                             (V.take (n - k) v)
                             (V.drop k      v))
            / (fromIntegral (n - k) * var)
  in [(k, acf k) | k <- [0 .. min maxLag (n - 1)]]

-- | Highest density interval: shortest contiguous interval that covers
-- `level` fraction of the (sorted) samples. Returns (lower, upper).
hdi :: Double -> [Double] -> (Double, Double)
hdi level xs
  | null xs   = (0, 0)
  | otherwise =
      let sorted  = V.fromList (sort xs)
          n       = V.length sorted
          window  = max 1 (min (n - 1) (floor (level * fromIntegral n) :: Int))
          (_, i)  = minimumBy (comparing fst)
                      [ (sorted V.! (i' + window) - sorted V.! i', i')
                      | i' <- [0 .. n - window - 1] ]
      in (sorted V.! i, sorted V.! (i + window))

-- | Effective sample size via Geyer's initial monotone sequence estimator.
-- Returns n when the chain is too short to estimate.
ess :: [Double] -> Double
ess xs
  | n < 4     = fromIntegral n
  | otherwise =
      let acs    = map snd (autocorr (n `div` 2) xs)
          -- Gamma(k) = rho(2k) + rho(2k+1)
          gammas = pairSums acs
          -- Monotone non-increasing sequence of Gamma
          monoG  = scanl1 min gammas
          posG   = takeWhile (> 0) monoG
          tau    = max 1 (-1 + 2 * sum posG)
      in fromIntegral n / tau
  where
    n = length xs
    pairSums (a : b : rest) = (a + b) : pairSums rest
    pairSums _              = []
