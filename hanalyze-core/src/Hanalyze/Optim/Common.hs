-- |
-- Module      : Hanalyze.Optim.Common
-- Description : 単一目的最適化アルゴリズム群が共有する基盤型・既定値
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Common foundation for the single-objective optimization algorithms.
--
-- Provides the shared types and defaults used by every single-objective
-- optimizer (@Hanalyze.Optim.NelderMead@, @Hanalyze.Optim.LBFGS@, @Hanalyze.Optim.LineSearch@,
-- @Hanalyze.Optim.DifferentialEvolution@, @Hanalyze.Optim.CMAES@, @Hanalyze.Optim.CMAESFull@,
-- @Hanalyze.Optim.SimulatedAnnealing@, @Hanalyze.Optim.ParticleSwarm@), plus the unified
-- 'Bounds' type for box constraints.
--
-- Each optimizer's runner has the same shape:
--
-- @
-- runX :: XConfig -> ([Double] -> Double) -> [Double] -> IO OptimResult
-- @
--
-- (Deterministic algorithms also return @IO@ for uniformity. A pure-only
-- variant can be exported separately when needed.)
{-# LANGUAGE StrictData #-}
module Hanalyze.Optim.Common
  ( OptimResult (..)
  , StopCriteria (..)
  , defaultStopCriteria
  , Direction (..)
  , flipFor
    -- * Box constraints (search range)
  , Bounds
  , clipToBounds
  , projectToBounds
  , sampleUniformIn
  , boundsPenalty
  , inBounds
  ) where

import Control.Monad (forM)
import qualified System.Random.MWC as MWC

-- | Optimization direction.
data Direction = Minimize | Maximize deriving (Show, Eq)

-- | Stopping criteria shared by every optimizer.
data StopCriteria = StopCriteria
  { stMaxIter :: !Int     -- ^ Maximum number of iterations.
  , stTolFun  :: !Double  -- ^ Convergence on @|Δf| < tol@.
  , stTolX    :: !Double  -- ^ Convergence on @‖Δx‖∞ < tol@ (or simplex
                          --   size for Nelder-Mead).
  } deriving (Show, Eq)

-- | Standard generic stopping criteria. Sufficient for the bundled
-- benchmarks.
defaultStopCriteria :: StopCriteria
defaultStopCriteria = StopCriteria
  { stMaxIter = 1000
  , stTolFun  = 1e-8
  , stTolX    = 1e-10
  }

-- | Optimization result.
data OptimResult = OptimResult
  { orBest      :: ![Double]   -- ^ Best point @x*@.
  , orValue     :: !Double     -- ^ Best value @f(x*)@ (internally minimized).
  , orHistory   :: ![Double]   -- ^ Per-iteration best-value trace (up to
                               --   @stMaxIter + 1@ entries).
  , orIters     :: !Int        -- ^ Actual number of iterations executed.
  , orConverged :: !Bool       -- ^ True if stopped on tolerance criteria.
  } deriving (Show, Eq)

-- | Toggle between the user's 'Direction' and the internal-always-minimize
-- representation. Each optimizer applies this at entry and reverses the
-- value sign at exit.
--
-- > flipFor Maximize f x = -(f x)
-- > flipFor Minimize f x =   f x
flipFor :: Direction -> ([Double] -> Double) -> ([Double] -> Double)
flipFor Minimize f = f
flipFor Maximize f = negate . f
{-# INLINE flipFor #-}

-- ---------------------------------------------------------------------------
-- Box constraints (各次元の上下限)
-- ---------------------------------------------------------------------------

-- | Per-dimension @(lower, upper)@ list.
type Bounds = [(Double, Double)]

-- | Reflect each coordinate back into its range when outside. Excessive
-- excursions are clamped to the range width.
clipToBounds :: Bounds -> [Double] -> [Double]
clipToBounds bs xs = zipWith reflect bs xs
  where
    reflect (lo, hi) x
      | x < lo    = let d = lo - x in lo + min d (hi - lo)
      | x > hi    = let d = x - hi in hi - min d (hi - lo)
      | otherwise = x

-- | Plain clipping: pin out-of-range coordinates to the boundary value.
--
-- >>> projectToBounds [(0,1),(0,1)] [-0.5, 1.5]
-- [0.0,1.0]
projectToBounds :: Bounds -> [Double] -> [Double]
projectToBounds bs xs =
  zipWith (\(lo, hi) x -> max lo (min hi x)) bs xs

-- | Sample a single point uniformly within the bounds (shared
-- initialization for DE / PSO / SA / NSGA).
sampleUniformIn :: Bounds -> MWC.GenIO -> IO [Double]
sampleUniformIn bs gen = forM bs $ \(lo, hi) -> MWC.uniformR (lo, hi) gen

-- | Soft penalty for out-of-range coordinates, intended to be added to
-- the objective in L-BFGS / Nelder-Mead. Returns @0@ inside the bounds
-- and @k Σ_i d_i²@ outside (with @k = 10^6@).
--
-- @
-- objWithPenalty xs = f xs + boundsPenalty (Just bs) xs
-- @
boundsPenalty :: Maybe Bounds -> [Double] -> Double
boundsPenalty Nothing   _  = 0
boundsPenalty (Just bs) xs =
  let k = 1e6 :: Double
      dists = zipWith dist bs xs
  in k * sum [d * d | d <- dists]
  where
    dist (lo, hi) x
      | x < lo    = lo - x
      | x > hi    = x - hi
      | otherwise = 0

-- | True when every coordinate lies inside the bounds.
--
-- >>> inBounds [(0,1),(0,1)] [0.5, 0.5]
-- True
-- >>> inBounds [(0,1),(0,1)] [0.5, 1.5]
-- False
inBounds :: Bounds -> [Double] -> Bool
inBounds bs xs = all (\((lo, hi), x) -> x >= lo && x <= hi) (zip bs xs)
