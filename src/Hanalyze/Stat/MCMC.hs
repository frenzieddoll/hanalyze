{-# LANGUAGE OverloadedStrings #-}
-- | Pure post-processing utilities for MCMC chains.
--
-- Provides autocorrelation, highest-density intervals (HDI), effective
-- sample size (Geyer's initial monotone sequence estimator), split-R-hat
-- (Vehtari et al. 2021), kernel density estimation (Silverman bandwidth)
-- and BFMI. Operates on raw @Vector@ samples or on the 'Hanalyze.MCMC.Core.Chain'
-- type from the sampler layer.
module Hanalyze.Stat.MCMC
  ( autocorr
  , hdi
  , ess
  , rhat
  , kde
  , bfmi
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
-- @level@ fraction of the (sorted) samples. Returns (lower, upper).
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

-- | Split-R-hat convergence diagnostic (Vehtari et al. 2021).
--
-- Splits each chain in half to obtain @2M@ sub-chains, then computes
-- R-hat from the between-chain variance @B@ and within-chain variance
-- @W@. The conventional convergence threshold is @R-hat < 1.01@.
-- The argument is the per-chain sample list for a single parameter.
-- Returns 'Nothing' when there are fewer than 2 chains or fewer than 4
-- samples per chain.
rhat :: [[Double]] -> Maybe Double
rhat chains
  | m < 2 || n < 4 = Nothing
  | w == 0         = Nothing
  | otherwise      = Just (sqrt (varPlus / w))
  where
    allVals   = filter (not . null) chains
    splitOne vs = let half = length vs `div` 2
                  in [take half vs, drop half vs]
    subchains = concatMap splitOne allVals
    m         = length subchains
    n         = minimum (map length subchains)
    trimmed   = map (take n) subchains
    mean_ vs  = sum vs / fromIntegral (length vs)
    chainMeans = map mean_ trimmed
    grandMean  = mean_ chainMeans
    b = fromIntegral n / fromIntegral (m - 1)
        * sum (map (\mu -> (mu - grandMean) ^ (2 :: Int)) chainMeans)
    chainVars = map (\vs -> let mu = mean_ vs
                            in sum (map (\x -> (x - mu) ^ (2 :: Int)) vs)
                               / fromIntegral (n - 1)) trimmed
    w       = mean_ chainVars
    varPlus = fromIntegral (n - 1) / fromIntegral n * w + b / fromIntegral n

-- | Kernel density estimation (Gaussian kernel, Silverman bandwidth).
--
-- Returns @nPoints@ pairs of @(x, density)@. With fewer than two samples
-- the returned list is empty. The grid spans @[min - 3σ, max + 3σ]@.
kde :: Int -> [Double] -> [(Double, Double)]
kde nPoints xs
  | length xs < 2 = []
  | sig <= 0      = []
  | otherwise     = [(x, density x) | x <- grid]
  where
    n    = length xs
    mu   = sum xs / fromIntegral n
    var  = sum (map (\x -> (x - mu) ^ (2 :: Int)) xs) / fromIntegral (n - 1)
    sig  = sqrt var
    h    = 1.06 * sig * fromIntegral n ** (-0.2)   -- Silverman's rule
    lo   = minimum xs - 3 * sig
    hi   = maximum xs + 3 * sig
    step = (hi - lo) / fromIntegral (nPoints - 1)
    grid = [lo + fromIntegral i * step | i <- [0 .. nPoints - 1 :: Int]]
    kernel u = exp (-0.5 * u * u) / sqrt (2 * pi)
    density x = sum [kernel ((x - xi) / h) | xi <- xs]
                / (fromIntegral n * h)

-- | Bayesian Fraction of Missing Information (Betancourt 2016).
--
-- @
-- BFMI = E[(E_n − E_{n−1})²] / Var(E)
-- @
--
-- Computed from the energy sequence (Hamiltonian per iteration) of an
-- HMC/NUTS run. Values below 0.3 indicate that momentum resampling is
-- not exploring the posterior tails (consider reparameterization — the
-- canonical example is Neal's funnel). Values above 0.3 are healthy;
-- PyMC commonly uses 0.5 as a reference threshold.
bfmi :: [Double] -> Maybe Double
bfmi es
  | length es < 4 = Nothing
  | varE == 0     = Nothing
  | otherwise     = Just (numer / varE)
  where
    n        = length es
    mu       = sum es / fromIntegral n
    varE     = sum (map (\x -> (x - mu) ^ (2 :: Int)) es)
               / fromIntegral (n - 1)
    diffs    = zipWith (-) (drop 1 es) es
    numer    = sum (map (\d -> d * d) diffs)
               / fromIntegral (length diffs)
