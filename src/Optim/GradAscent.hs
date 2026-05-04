{-# LANGUAGE OverloadedStrings #-}
-- | Vanilla gradient ascent / descent.
--
-- The numeric-gradient implementation that used to live in
-- @Model.GP.optimizeGP@, extracted as a shared foundation. The learning
-- rate is shrunk by 0.5 % per iteration; iteration stops early when the
-- gradient norm drops below the configured tolerance.
--
-- When to use which:
--
--   * 'Optim.Adam.runAdam' — momentum-based, robust, recommended default.
-- - 'Optim.GradAscent.gradientAscent' — シンプル、軽量、デバッグ容易
-- - 'Optim.GradAscent.gradientDescent' — 上の符号反転版
module Optim.GradAscent
  ( GradConfig (..)
  , defaultGradConfig
  , gradientAscent
  , gradientDescent
  ) where

data GradConfig = GradConfig
  { gradIterations  :: Int     -- ^ 最大反復数
  , gradLearningRate :: Double  -- ^ 初期学習率
  , gradDecay       :: Double  -- ^ 反復ごとの学習率減衰率 (例: 0.995)
  , gradTolerance   :: Double  -- ^ 早期終了の勾配ノルム閾値
  } deriving (Show)

defaultGradConfig :: GradConfig
defaultGradConfig = GradConfig
  { gradIterations  = 400
  , gradLearningRate = 0.1
  , gradDecay       = 0.995
  , gradTolerance   = 1e-8
  }

-- | 勾配上昇法 (目的関数の勾配を渡す → 最大化)。
--
-- @gradFn x@ が現在地での勾配を返す。各反復で:
--
-- 1. 勾配 g を計算
-- 2. \|g\| < tol なら終了
-- 3. x ← x + lr × g/|g|   (正規化勾配で安定化)
-- 4. lr ← lr × decay
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

-- | 勾配下降法。'gradientAscent' で勾配の符号を反転して使う。
gradientDescent :: GradConfig -> ([Double] -> [Double]) -> [Double] -> [Double]
gradientDescent cfg gradFn = gradientAscent cfg (map negate . gradFn)
