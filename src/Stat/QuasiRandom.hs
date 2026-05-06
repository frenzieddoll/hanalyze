-- | Quasi-random number sequences with low discrepancy.
--
-- These sequences cover a multi-dimensional unit hyper-cube more
-- evenly than independent uniform-random samples and are the
-- recommended way to seed Bayesian-optimization initial designs and
-- multi-start global optimizers.
--
-- The 'haltonSequence' implementation uses the first @d@ prime numbers
-- as bases. For @d ≤ 6@ (Branin, Hartmann6, etc.) it is essentially
-- as good as Sobol; for @d ≥ 10@ correlation between dimensions can
-- become visible and Sobol with scrambling is preferred (not
-- implemented here).
module Stat.QuasiRandom
  ( haltonPoint
  , haltonSequence
  , haltonSequenceIn
  , primes
    -- * Latin Hypercube Sampling
  , lhsSamples
  , lhsSamplesIn
  ) where

import           Control.Monad         (forM)
import qualified Data.Vector.Mutable   as MV
import qualified Data.Vector           as V
import           System.Random.MWC     (GenIO, uniformR)

-- | Infinite list of prime numbers via a simple Sieve.
primes :: [Int]
primes = sieve [2 ..]
  where
    sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    sieve []       = []

-- | Radical-inverse function in base @b@. Maps an integer @i@ into
-- @[0, 1)@.
radicalInverse :: Int -> Int -> Double
radicalInverse base i = go i (1.0 / fromIntegral base) 0
  where
    go 0 _   acc = acc
    go n f   acc =
      let (q, r) = n `divMod` base
      in go q (f / fromIntegral base) (acc + fromIntegral r * f)

-- | Single Halton point in @d@ dimensions: applies 'radicalInverse'
-- with the first @d@ primes.
haltonPoint :: Int          -- ^ Dimension @d@.
            -> Int          -- ^ Index @i@ (1-based; @i = 0@ would yield the origin).
            -> [Double]
haltonPoint d i = take d [ radicalInverse p i | p <- primes ]

-- | First @n@ Halton points in @d@ dimensions, each in @[0, 1)^d@.
-- Indexed from 1 (skipping @i = 0@, which would be at the origin).
haltonSequence :: Int        -- ^ Number of points @n@.
               -> Int        -- ^ Dimension @d@.
               -> [[Double]]
haltonSequence n d =
  -- Pre-extract the @d@ basis primes once instead of re-traversing
  -- the lazy 'primes' generator on every point. Per-point body is a
  -- direct @map@ over this strict list, no further list overhead.
  let bases = take d primes
  in [ map (\b -> radicalInverse b i) bases | i <- [1 .. n] ]

-- | Halton sequence rescaled into a per-dimension box
-- @[lo_k, hi_k)@. @bounds@ must have length @d@.
haltonSequenceIn :: Int                       -- ^ @n@.
                 -> [(Double, Double)]        -- ^ @bounds@ (length @d@).
                 -> [[Double]]
haltonSequenceIn n bs =
  let d   = length bs
      pts = haltonSequence n d
  in [ zipWith (\u (lo, hi) -> lo + u * (hi - lo)) p bs | p <- pts ]

-- ---------------------------------------------------------------------------
-- Latin Hypercube Sampling
-- ---------------------------------------------------------------------------

-- | Generate @n@ Latin-Hypercube samples in @[0, 1)^d@.
--
-- Algorithm (McKay-Beckman-Conover 1979):
--
--   1. For each dimension @k@, partition @[0, 1)@ into @n@ equal cells
--      @[i/n, (i+1)/n)@ and pick one stratified-random point per cell:
--      @u_{i,k} = (i + r_{i,k}) / n@ where @r ~ U(0, 1)@.
--   2. Independently for each dimension, randomly permute the @n@ cells.
--   3. Stack the per-dim permutations into @n@ points of @d@ coords.
--
-- The result fills every per-dimension marginal cell exactly once,
-- giving much better coverage than @n@ iid uniform draws while still
-- being random.
lhsSamples :: Int -> Int -> GenIO -> IO [[Double]]
lhsSamples n d gen = do
  -- per-dim stratified samples (length n each)
  perDim <- forM [1 .. d] $ \_ -> do
    -- 1) one stratified sample per cell
    base <- forM [0 .. n - 1] $ \i -> do
      r <- uniformR (0, 1) gen :: IO Double
      pure ((fromIntegral i + r) / fromIntegral n)
    -- 2) random permutation (Fisher-Yates)
    mv <- V.thaw (V.fromList base)
    let nLast = n - 1
    mapM_ (\i -> do
              j <- uniformR (i, nLast) gen
              MV.swap mv i j) [0 .. nLast - 1]
    V.toList <$> V.unsafeFreeze mv
  -- transpose: perDim is d × n, want n × d
  pure [ [ (perDim !! k) !! i | k <- [0 .. d - 1] ] | i <- [0 .. n - 1] ]

-- | LHS samples rescaled into the per-dimension box @[lo_k, hi_k)@.
-- @bounds@ must have length @d@.
lhsSamplesIn :: Int -> [(Double, Double)] -> GenIO -> IO [[Double]]
lhsSamplesIn n bs gen = do
  let d = length bs
  pts <- lhsSamples n d gen
  pure [ zipWith (\u (lo, hi) -> lo + u * (hi - lo)) p bs | p <- pts ]
