{-# LANGUAGE OverloadedStrings #-}
-- | Adam 一次最適化アルゴリズム (Kingma & Ba 2014)。
--
-- ELBO 最大化、ニューラルネット学習、acquisition function の最適化など
-- 一般的な勾配ベース最適化に使う。`Stat.VI` から共通基盤として切り出した。
--
-- 使い方:
--
-- @
-- let cfg = defaultAdamConfig { adamLearningRate = 0.01, adamIterations = 1000 }
--     gradFn x = ...                            -- 勾配 (上昇方向)
--     (xFinal, history) = runAdam cfg gradFn x0
-- @
--
-- 'adamStep' 単体は 1 ステップだけ進める低レベル API で、`Stat.VI` などが
-- 内部で利用する。
module Optim.Adam
  ( -- * 設定
    AdamConfig (..)
  , defaultAdamConfig
    -- * 1 ステップ更新 (低レベル)
  , adamStep
    -- * 高レベルループ
  , runAdam
  , runAdamMaximize
  , runAdamMinimize
  ) where

import Data.IORef
import Control.Monad (forM_)
import System.IO.Unsafe (unsafePerformIO)

-- | Adam の設定。
data AdamConfig = AdamConfig
  { adamIterations   :: Int     -- ^ 反復数
  , adamLearningRate :: Double  -- ^ 学習率 α
  , adamBeta1        :: Double  -- ^ 1 次モーメント減衰率 (default 0.9)
  , adamBeta2        :: Double  -- ^ 2 次モーメント減衰率 (default 0.999)
  , adamEpsilon      :: Double  -- ^ 数値安定化定数 (default 1e-8)
  } deriving (Show)

defaultAdamConfig :: AdamConfig
defaultAdamConfig = AdamConfig
  { adamIterations   = 1000
  , adamLearningRate = 0.01
  , adamBeta1        = 0.9
  , adamBeta2        = 0.999
  , adamEpsilon      = 1e-8
  }

-- | Adam の 1 ステップ更新。
--
-- 引数:
--   * @β1, β2, ε, α@ — Adam パラメタ
--   * @t@ — 反復回数 (1-origin、bias correction 用)
--   * @m1, m2@ — 直前の 1 次/2 次モーメント
--   * @g@ — 現在の勾配
--
-- 戻り値: @(m1', m2', dx)@ — 更新後モーメントと進む方向 (= +勾配方向)。
-- 呼び出し側は @x ← x + dx@ で**勾配上昇**または @x ← x − dx@ で**勾配下降**。
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

-- | 勾配上昇ループ。`gradFn` は **目的関数の勾配** を返す。
-- @x ← x + Δx@ で更新するので、最大化したい量の勾配を渡す。
--
-- 戻り値: @(x_final, x_history)@。各反復後の x を保存 (デバッグ/可視化用)。
runAdamMaximize :: AdamConfig
                -> ([Double] -> [Double])  -- ^ 勾配関数
                -> [Double]                -- ^ 初期値
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
    writeIORef xRef x'
    writeIORef m1Ref m1'
    writeIORef m2Ref m2'
    modifyIORef' histRef (x' :)
  xF   <- readIORef xRef
  hist <- fmap reverse (readIORef histRef)
  return (xF, hist)

-- | 勾配下降版。
-- @gradFn@ を反転させて 'runAdamMaximize' を呼ぶ。
runAdamMinimize :: AdamConfig -> ([Double] -> [Double]) -> [Double]
                -> ([Double], [[Double]])
runAdamMinimize cfg gradFn x0 =
  runAdamMaximize cfg (map negate . gradFn) x0

-- | エイリアス: 'runAdamMaximize' (デフォルトは最大化として使う)。
runAdam :: AdamConfig -> ([Double] -> [Double]) -> [Double]
        -> ([Double], [[Double]])
runAdam = runAdamMaximize
