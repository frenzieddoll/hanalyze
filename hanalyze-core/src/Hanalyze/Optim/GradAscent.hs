-- |
-- Module      : Hanalyze.Optim.GradAscent
-- Description : 素朴な勾配上昇 / 下降法
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Vanilla gradient ascent / descent.
--
-- The numeric-gradient implementation that used to live in
-- @Hanalyze.Model.GP.optimizeGP@, extracted as a shared foundation. The learning
-- rate is shrunk by 0.5 % per iteration; iteration stops early when the
-- gradient norm drops below the configured tolerance.
--
-- When to use which:
--
--   * 'Hanalyze.Optim.Adam.runAdam' — momentum-based, robust, recommended default.
-- - 'Hanalyze.Optim.GradAscent.gradientAscent' — シンプル、軽量、デバッグ容易
-- - 'Hanalyze.Optim.GradAscent.gradientDescent' — 上の符号反転版
{-# LANGUAGE OverloadedStrings #-}
module Hanalyze.Optim.GradAscent
  ( GradConfig (..)
  , defaultGradConfig
  , gradientAscent
  , gradientDescent
  ) where

-- | Configuration for gradient ascent / descent.
data GradConfig = GradConfig
  { gradIterations   :: Int     -- ^ Maximum number of iterations.
  , gradLearningRate :: Double  -- ^ Initial learning rate.
  , gradDecay        :: Double  -- ^ Per-iteration learning-rate decay (e.g. 0.995).
  , gradTolerance    :: Double  -- ^ Early-stop threshold on gradient norm.
  } deriving (Show)

-- | Default configuration: 400 iterations, lr 0.1, decay 0.995, tol 1e-8.
defaultGradConfig :: GradConfig
defaultGradConfig = GradConfig
  { gradIterations  = 400
  , gradLearningRate = 0.1
  , gradDecay       = 0.995
  , gradTolerance   = 1e-8
  }

-- | Gradient ascent. Pass the gradient of the objective to maximize it.
--
-- @gradFn x@ returns the gradient at the current point. Each iteration:
--
--   1. Compute the gradient @g@.
--   2. Stop when @|g| < tol@.
--   3. @x ← x + lr × g/|g|@ (normalized for stability).
--   4. @lr ← lr × decay@.
gradientAscent :: GradConfig -> ([Double] -> [Double]) -> [Double] -> [Double]
gradientAscent cfg gradFn = go (gradIterations cfg) (gradLearningRate cfg)
  where
    go 0   _  x = x
    go itr lr x =
      let g     = gradFn x
          gnorm = sqrt (sum (map (\v -> v * v) g))
      in if gnorm < gradTolerance cfg
           then x
           else
             let x' = zipWith (\xi gi -> xi + lr * gi / gnorm) x g
             in go (itr - 1) (lr * gradDecay cfg) x'

-- | Gradient descent. Negates the gradient and delegates to
-- 'gradientAscent'.
gradientDescent :: GradConfig -> ([Double] -> [Double]) -> [Double] -> [Double]
gradientDescent cfg gradFn = gradientAscent cfg (map negate . gradFn)
