{-# LANGUAGE OverloadedStrings #-}
-- | Hamiltonian Monte Carlo (HMC) サンプラー。
--
-- 勾配は中心差分で数値計算します（O(2n) 回の logJoint 評価）。
-- 'leapfrog' と 'gradU' は "Model.NUTS" からも使用します。
--
-- 使い方:
--
-- @
-- cfg   = defaultHMCConfig { hmcStepSize = 0.05, hmcLeapfrogSteps = 20 }
-- chain <- hmc myModel cfg initParams gen
-- @
module Model.HMC
  ( -- * Configuration
    HMCConfig (..)
  , defaultHMCConfig
    -- * Gradient / leapfrog (NUTS から利用)
  , gradU
  , leapfrog
  , kinetic
  , paramsToVec
  , vecToParams
    -- * Sampler
  , hmc
  ) where

import Control.Monad (forM)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.Random.MWC (GenIO, uniform)
import System.Random.MWC.Distributions (standard)

import Model.HBM (Model, Params, logJoint, sampleNames)
import Model.MCMC (Chain (..))

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data HMCConfig = HMCConfig
  { hmcIterations    :: Int
    -- ^ バーンイン後に保存するサンプル数
  , hmcBurnIn        :: Int
    -- ^ 破棄するバーンインステップ数
  , hmcStepSize      :: Double
    -- ^ リープフロッグのステップサイズ ε。受容率が 0.6〜0.9 になるよう調整する。
  , hmcLeapfrogSteps :: Int
    -- ^ リープフロッグのステップ数 L。大きいほど相関が小さくなるが計算コストが増える。
  } deriving (Show)

defaultHMCConfig :: HMCConfig
defaultHMCConfig = HMCConfig
  { hmcIterations    = 2000
  , hmcBurnIn        = 500
  , hmcStepSize      = 0.1
  , hmcLeapfrogSteps = 10
  }

-- ---------------------------------------------------------------------------
-- パラメータ変換ユーティリティ
-- ---------------------------------------------------------------------------

-- | Params マップを宣言順の Double リストに変換する。
paramsToVec :: [Text] -> Params -> [Double]
paramsToVec names params = map (\n -> Map.findWithDefault 0.0 n params) names

-- | Double リストを Params マップに変換する。
vecToParams :: [Text] -> [Double] -> Params
vecToParams names vals = Map.fromList (zip names vals)

-- ---------------------------------------------------------------------------
-- 数値勾配 (中心差分)
-- ---------------------------------------------------------------------------

-- | ポテンシャルエネルギー U(θ) = -log p(θ|data) の勾配。
-- 中心差分 h = 1e-5 で計算する (O(2n) 回の logJoint 評価)。
gradU :: Model a -> [Text] -> Params -> [Double]
gradU model names params = map df [0 .. length names - 1]
  where
    h = 1e-5
    df i =
      let nm = names !! i
          v  = Map.findWithDefault 0.0 nm params
          -- ∂U/∂θ_i = -∂logJoint/∂θ_i ≈ -(logJoint(+h) - logJoint(-h))/(2h)
          p1 = Map.insert nm (v + h) params
          p2 = Map.insert nm (v - h) params
      in (logJoint model p2 - logJoint model p1) / (2 * h)

-- ---------------------------------------------------------------------------
-- リープフロッグ積分
-- ---------------------------------------------------------------------------

-- | 運動エネルギー: K(r) = 0.5 * ||r||^2 (単位質量行列).
kinetic :: [Double] -> Double
kinetic r = 0.5 * sum (map (^ (2 :: Int)) r)

-- | L ステップのリープフロッグ積分。
--
-- ハミルトン方程式:
--   r_{t+ε/2} = r_t   - (ε/2) * ∇U(θ_t)
--   θ_{t+ε}   = θ_t   + ε * r_{t+ε/2}
--   r_{t+ε}   = r_{t+ε/2} - (ε/2) * ∇U(θ_{t+ε})
--
-- ε < 0 にすると逆方向 (NUTS の後退ステップで使用).
leapfrog :: Model a -> [Text] -> Double -> Int -> Params -> [Double] -> (Params, [Double])
leapfrog model names eps steps theta0 r0 = go steps theta0 r0
  where
    go 0 theta r = (theta, r)
    go n theta r =
      let g      = gradU model names theta
          rHalf  = zipWith (\ri gi -> ri - (eps / 2) * gi) r g
          tVec'  = zipWith (\ti ri -> ti + eps * ri) (paramsToVec names theta) rHalf
          theta' = vecToParams names tVec'
          g'     = gradU model names theta'
          r'     = zipWith (\ri gi -> ri - (eps / 2) * gi) rHalf g'
      in go (n - 1) theta' r'

-- ---------------------------------------------------------------------------
-- HMC サンプラー
-- ---------------------------------------------------------------------------

-- | HMC を実行する。
--
-- 1 ステップの手順:
--   1. 運動量 r ~ N(0, I) をサンプリング
--   2. L ステップのリープフロッグで候補点 (θ', r') を生成
--   3. メトロポリス基準で採否判定: log α = -H(θ',r') + H(θ,r)
hmc :: Model a -> HMCConfig -> Params -> GenIO -> IO Chain
hmc model cfg init_ gen = do
  let names = sampleNames model
      total = hmcBurnIn cfg + hmcIterations cfg

  samplesRef  <- newIORef []
  acceptedRef <- newIORef (0 :: Int)

  let step current = do
        r <- forM names (\_ -> standard gen)
        let (proposed, rFinal) =
              leapfrog model names (hmcStepSize cfg) (hmcLeapfrogSteps cfg) current r
            logAlpha =
              (logJoint model proposed - kinetic rFinal)
              - (logJoint model current  - kinetic r)
        u <- uniform gen
        if log (u :: Double) < logAlpha
          then do modifyIORef' acceptedRef (+1); return proposed
          else return current

  let loop 0 current = return current
      loop i current = do
        next <- step current
        if i <= hmcIterations cfg
          then modifyIORef' samplesRef (next :)
          else return ()
        loop (i - 1) next

  _ <- loop total init_
  samples  <- fmap reverse (readIORef samplesRef)
  accepted <- readIORef acceptedRef
  return Chain
    { chainSamples  = samples
    , chainAccepted = accepted
    , chainTotal    = total
    }
