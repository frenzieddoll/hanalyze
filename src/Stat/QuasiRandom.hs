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
  ) where

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
haltonSequence n d = [haltonPoint d i | i <- [1 .. n]]

-- | Halton sequence rescaled into a per-dimension box
-- @[lo_k, hi_k)@. @bounds@ must have length @d@.
haltonSequenceIn :: Int                       -- ^ @n@.
                 -> [(Double, Double)]        -- ^ @bounds@ (length @d@).
                 -> [[Double]]
haltonSequenceIn n bs =
  let d   = length bs
      pts = haltonSequence n d
  in [ zipWith (\u (lo, hi) -> lo + u * (hi - lo)) p bs | p <- pts ]
