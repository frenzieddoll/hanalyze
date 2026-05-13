{-# LANGUAGE OverloadedStrings #-}
-- | Adam first-order optimizer (Kingma & Ba 2014).
--
-- A general-purpose gradient-based optimizer used for ELBO maximization,
-- neural-network training, acquisition-function optimization, and similar
-- tasks. Originally embedded in @Hanalyze.Stat.VI@; extracted here as a shared
-- foundation.
--
-- 使い方:
--
-- @
-- let cfg = defaultAdamConfig { adamLearningRate = 0.01, adamIterations = 1000 }
--     gradFn x = ...                            -- 勾配 (上昇方向)
--     (xFinal, history) = runAdam cfg gradFn x0
-- @
--
-- 'adamStep' 単体は 1 ステップだけ進める低レベル API で、`Hanalyze.Stat.VI` などが
-- 内部で利用する。
module Hanalyze.Optim.Adam
  ( -- * 設定
    AdamConfig (..)
  , defaultAdamConfig
    -- * Single-step update (low-level)
  , adamStep
    -- * High-level loop
  , runAdam
  , runAdamMaximize
  , runAdamMinimize
  ) where

import Control.DeepSeq (force)
import Data.IORef
import Control.Monad (forM_)
import System.IO.Unsafe (unsafePerformIO)

-- | Adam configuration.
data AdamConfig = AdamConfig
  { adamIterations   :: Int     -- ^ Number of iterations.
  , adamLearningRate :: Double  -- ^ Learning rate @α@.
  , adamBeta1        :: Double  -- ^ First-moment decay (default 0.9).
  , adamBeta2        :: Double  -- ^ Second-moment decay (default 0.999).
  , adamEpsilon      :: Double  -- ^ Numerical stabilizer (default 1e-8).
  } deriving (Show)

-- | Default Adam configuration: 1000 iterations, @α = 0.01@,
-- @β₁ = 0.9@, @β₂ = 0.999@, @ε = 1e-8@.
defaultAdamConfig :: AdamConfig
defaultAdamConfig = AdamConfig
  { adamIterations   = 1000
  , adamLearningRate = 0.01
  , adamBeta1        = 0.9
  , adamBeta2        = 0.999
  , adamEpsilon      = 1e-8
  }

-- | Single Adam update.
--
-- Arguments:
--
--   * @β1@, @β2@, @ε@, @α@ — Adam hyperparameters.
--   * @t@ — iteration count (1-based; needed for bias correction).
--   * @m1@, @m2@ — previous first- and second-moment estimates.
--   * @g@ — current gradient.
--
-- Returns @(m1', m2', dx)@: the updated moments and the step direction
-- (in the @+gradient@ direction). Callers do @x ← x + dx@ for ascent or
-- @x ← x − dx@ for descent.
adamStep
  :: Double -> Double -> Double -> Double -> Int
  -> [Double] -> [Double] -> [Double]
  -> ([Double], [Double], [Double])
adamStep b1 b2 eps alpha t m1 m2 g =
  let m1' = zipWith (\m gi -> b1 * m + (1 - b1) * gi)      m1 g
      m2' = zipWith (\v gi -> b2 * v + (1 - b2) * gi * gi)  m2 g
      mH  = map (/ (1 - b1 ^ t)) m1'
      vH  = map (/ (1 - b2 ^ t)) m2'
      dx  = zipWith (\m_ v -> alpha * m_ / (sqrt v + eps))   mH vH
  in (m1', m2', dx)

-- | Gradient-ascent loop. @gradFn@ returns the gradient of the objective.
-- The update @x ← x + Δx@ moves in the @+gradient@ direction, so pass the
-- gradient of the quantity to maximize.
--
-- Returns @(x_final, x_history)@; the per-iteration trajectory is kept
-- for debugging and visualization.
runAdamMaximize :: AdamConfig
                -> ([Double] -> [Double])  -- ^ Gradient function.
                -> [Double]                -- ^ Initial point.
                -> ([Double], [[Double]])
runAdamMaximize cfg gradFn x0 = unsafePerformIO $ do
  let n = length x0
  xRef  <- newIORef x0
  m1Ref <- newIORef (replicate n 0.0)
  m2Ref <- newIORef (replicate n 0.0)
  histRef <- newIORef []
  forM_ [1 .. adamIterations cfg] $ \t -> do
    x  <- readIORef xRef
    m1 <- readIORef m1Ref
    m2 <- readIORef m2Ref
    let g            = gradFn x
        (m1', m2', dx) = adamStep
                          (adamBeta1 cfg) (adamBeta2 cfg) (adamEpsilon cfg)
                          (adamLearningRate cfg) t m1 m2 g
        x'           = zipWith (+) x dx
    -- Phase Q3 (2026-05-14): force lists before storing in IORef. Without
    -- this each iter writes a thunk that reads the previous IORef contents
    -- and chains a fresh @zipWith@ on top — after T iters the chain holds
    -- O(T) closures. See Stat.VI for the same fix and BenchMemVI numbers.
    let !x''  = force x'
        !m1'' = force m1'
        !m2'' = force m2'
    writeIORef xRef x''
    writeIORef m1Ref m1''
    writeIORef m2Ref m2''
    modifyIORef' histRef (x'' :)
  xF   <- readIORef xRef
  hist <- fmap reverse (readIORef histRef)
  return (xF, hist)

-- | Gradient-descent variant: negates @gradFn@ and delegates to
-- 'runAdamMaximize'.
runAdamMinimize :: AdamConfig -> ([Double] -> [Double]) -> [Double]
                -> ([Double], [[Double]])
runAdamMinimize cfg gradFn x0 =
  runAdamMaximize cfg (map negate . gradFn) x0

-- | Alias for 'runAdamMaximize' (the default convention is ascent).
runAdam :: AdamConfig -> ([Double] -> [Double]) -> [Double]
        -> ([Double], [[Double]])
runAdam = runAdamMaximize
