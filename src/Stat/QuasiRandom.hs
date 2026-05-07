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
  , haltonMatrix
  , primes
    -- * Latin Hypercube Sampling
  , lhsSamples
  , lhsSamplesIn
  ) where

import           Control.Monad         (forM)
import qualified Data.Vector.Mutable   as MV
import qualified Data.Vector           as V
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as MVS
import qualified Numeric.LinearAlgebra        as LA
import           System.Random.MWC     (GenIO, uniformR)

-- | Infinite list of prime numbers via a simple Sieve.
primes :: [Int]
primes = sieve [2 ..]
  where
    sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    sieve []       = []

-- | Radical-inverse function in base @b@. Maps an integer @i@ into
-- @[0, 1)@.
--
-- P41 inner-loop tweaks:
--
--   * @1 / fromIntegral base@ is computed once; subsequent iterations
--     multiply by @invB@ instead of dividing by @base@ each step.
--     Halton at n=10000 d=5 spends ~500K loop iterations here, each
--     previously paying a Double division.
--   * @divMod@ → @quot@ + @r = n - q*base@: avoids the @(q,r)@ tuple
--     pattern-match alloc, replaces a IDIV with an IMUL+SUB on x86.
radicalInverse :: Int -> Int -> Double
radicalInverse base i = go i invB 0
  where
    !invB = 1.0 / fromIntegral base
    go !n !f !acc
      | n == 0    = acc
      | otherwise =
          let !q = n `quot` base
              !r = n - q * base
          in go q (f * invB) (acc + fromIntegral r * f)
{-# INLINE radicalInverse #-}

-- | Single Halton point in @d@ dimensions: applies 'radicalInverse'
-- with the first @d@ primes.
haltonPoint :: Int          -- ^ Dimension @d@.
            -> Int          -- ^ Index @i@ (1-based; @i = 0@ would yield the origin).
            -> [Double]
haltonPoint d i = take d [ radicalInverse p i | p <- primes ]

-- | First @n@ Halton points in @d@ dimensions, each in @[0, 1)^d@.
-- Indexed from 1 (skipping @i = 0@, which would be at the origin).
--
-- We tried @runST@ + flat Storable Vector + final list-comp slicing,
-- but the cost is dominated by the @n × d@ cons-cell allocations of
-- the @[[Double]]@ boundary representation, not by the kernel of
-- 'radicalInverse'. The flat-vector path benchmarked the same as or
-- slightly slower than the direct list comprehension below — the
-- structural ceiling here is the @[[Double]]@ API. Internal-only
-- callers that want the table as a flat Storable can use a future
-- 'haltonMatrix' (TODO).
haltonSequence :: Int        -- ^ Number of points @n@.
               -> Int        -- ^ Dimension @d@.
               -> [[Double]]
haltonSequence n d =
  let bases = take d primes
  in [ map (\b -> radicalInverse b i) bases | i <- [1 .. n] ]

-- | First @n@ Halton points returned as a flat @n × d@ matrix
-- (row-major: row @i@ = the @i@-th Halton point in @[0, 1)^d@).
--
-- This is the same numerical sequence as 'haltonSequence', but
-- written into a Storable buffer with no @[[Double]]@ boxing — the
-- scipy.stats.qmc.Halton API returns an @ndarray@ of the same shape,
-- and the @[[Double]]@ form was a 2× allocation tax purely from the
-- API boundary (P41).
--
-- Internal-loop optimisations:
--
--   * Bases are loaded into an unboxed @VS.Vector Int@ once.
--   * Per-cell write goes through a hand-rolled ST loop ('outer'/
--     'inner') so no @forM_ [0..k]@ list cells are allocated.
--   * 'radicalInverse' is the same kernel as before; the saving is
--     entirely in the boundary representation.
haltonMatrix :: Int        -- ^ Number of points @n@.
             -> Int        -- ^ Dimension @d@.
             -> LA.Matrix Double
haltonMatrix n d
  | n <= 0 || d <= 0 = LA.fromLists []
  | otherwise =
      let basesV = VS.fromList (take d primes) :: VS.Vector Int
          total  = n * d
          flat = VS.create $ do
            v <- MVS.unsafeNew total
            let outer !i
                  | i >= n    = pure ()
                  | otherwise = do
                      let !iOne   = i + 1   -- skip i=0 (origin)
                          !rowBeg = i * d
                          inner !k
                            | k >= d    = pure ()
                            | otherwise = do
                                let !b   = VS.unsafeIndex basesV k
                                    !val = radicalInverse b iOne
                                MVS.unsafeWrite v (rowBeg + k) val
                                inner (k + 1)
                      inner 0
                      outer (i + 1)
            outer 0
            pure v
      in LA.reshape d flat

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
