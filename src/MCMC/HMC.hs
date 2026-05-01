{-# LANGUAGE OverloadedStrings #-}
-- | Hamiltonian Monte Carlo (HMC) サンプラー。
--
-- 制約付きパラメータ（正値・単位区間）は unconstrained 空間に変換してから
-- リープフロッグを行い、サンプルを constrained 空間に戻します。
-- 変換は事前分布の型から自動検出されるため、初期値は通常のパラメータ値で渡せます。
module MCMC.HMC
  ( -- * Configuration
    HMCConfig (..)
  , defaultHMCConfig
    -- * 制約変換ユーティリティ
  , toUnconstrainedParams
  , fromUnconstrainedParams
  , logJointU
  , gradUU
  , leapfrogWith
    -- * 基本ユーティリティ
  , gradU
  , leapfrog
  , kinetic
  , paramsToVec
  , vecToParams
    -- * Sampler
  , hmc
  , hmcChains
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM, replicateM)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.Random.MWC (GenIO, uniform)
import System.Random.MWC.Distributions (standard)

import Model.HBM (Model, Params, logJoint, sampleNames, getTransforms)
import MCMC.Core (Chain (..), spawnGen)
import Stat.Distribution (Transform, toUnconstrained, fromUnconstrained, logJacobianAdj)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data HMCConfig = HMCConfig
  { hmcIterations    :: Int
  , hmcBurnIn        :: Int
  , hmcStepSize      :: Double
  , hmcLeapfrogSteps :: Int
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

paramsToVec :: [Text] -> Params -> [Double]
paramsToVec names params = map (\n -> Map.findWithDefault 0.0 n params) names

vecToParams :: [Text] -> [Double] -> Params
vecToParams names vals = Map.fromList (zip names vals)

toUnconstrainedParams :: Map.Map Text Transform -> Params -> Params
toUnconstrainedParams transforms =
  Map.mapWithKey (\k v -> maybe v (`toUnconstrained` v) (Map.lookup k transforms))

fromUnconstrainedParams :: Map.Map Text Transform -> Params -> Params
fromUnconstrainedParams transforms =
  Map.mapWithKey (\k u -> maybe u (`fromUnconstrained` u) (Map.lookup k transforms))

-- ---------------------------------------------------------------------------
-- unconstrained 空間での log-joint (Jacobian 補正付き)
-- ---------------------------------------------------------------------------

logJointU :: Model a -> Map.Map Text Transform -> Params -> Double
logJointU model transforms paramsU =
  let paramsC = fromUnconstrainedParams transforms paramsU
      logJ    = sum [ logJacobianAdj t u
                    | (nm, t) <- Map.toList transforms
                    , Just u  <- [Map.lookup nm paramsU] ]
  in logJoint model paramsC + logJ

-- ---------------------------------------------------------------------------
-- 数値勾配 (中心差分)
-- ---------------------------------------------------------------------------

gradU :: Model a -> [Text] -> Params -> [Double]
gradU model names params = map df [0 .. length names - 1]
  where
    h = 1e-5
    df i =
      let nm = names !! i
          v  = Map.findWithDefault 0.0 nm params
          p1 = Map.insert nm (v + h) params
          p2 = Map.insert nm (v - h) params
      in (logJoint model p2 - logJoint model p1) / (2 * h)

gradUU :: Model a -> Map.Map Text Transform -> [Text] -> Params -> [Double]
gradUU model transforms names paramsU = map df [0 .. length names - 1]
  where
    h = 1e-5
    df i =
      let nm = names !! i
          v  = Map.findWithDefault 0.0 nm paramsU
          p1 = Map.insert nm (v + h) paramsU
          p2 = Map.insert nm (v - h) paramsU
      in (logJointU model transforms p2 - logJointU model transforms p1) / (2 * h)

-- ---------------------------------------------------------------------------
-- リープフロッグ積分
-- ---------------------------------------------------------------------------

kinetic :: [Double] -> Double
kinetic r = 0.5 * sum (map (^ (2 :: Int)) r)

leapfrogWith
  :: ([Text] -> Params -> [Double])
  -> [Text]
  -> Double
  -> Int
  -> Params
  -> [Double]
  -> (Params, [Double])
leapfrogWith gradFn names eps steps theta0 r0 = go steps theta0 r0
  where
    go 0 theta r = (theta, r)
    go n theta r =
      let g      = gradFn names theta
          rHalf  = zipWith (\ri gi -> ri - (eps / 2) * gi) r g
          tVec'  = zipWith (\ti ri -> ti + eps * ri) (paramsToVec names theta) rHalf
          theta' = vecToParams names tVec'
          g'     = gradFn names theta'
          r'     = zipWith (\ri gi -> ri - (eps / 2) * gi) rHalf g'
      in go (n - 1) theta' r'

leapfrog :: Model a -> [Text] -> Double -> Int -> Params -> [Double] -> (Params, [Double])
leapfrog model names = leapfrogWith (gradU model) names

-- ---------------------------------------------------------------------------
-- HMC サンプラー
-- ---------------------------------------------------------------------------

hmc :: Model a -> HMCConfig -> Params -> GenIO -> IO Chain
hmc model cfg initC gen = do
  let names      = sampleNames model
      transforms = getTransforms model
      initU      = toUnconstrainedParams transforms initC
      total      = hmcBurnIn cfg + hmcIterations cfg
      gradFn     = gradUU model transforms

  samplesRef  <- newIORef []
  acceptedRef <- newIORef (0 :: Int)

  let step currentU = do
        r <- forM names (\_ -> standard gen)
        let (proposedU, rFinal) =
              leapfrogWith gradFn names (hmcStepSize cfg) (hmcLeapfrogSteps cfg) currentU r
            logAlpha =
              (logJointU model transforms proposedU - kinetic rFinal)
              - (logJointU model transforms currentU  - kinetic r)
        u <- uniform gen
        if log (u :: Double) < logAlpha
          then do modifyIORef' acceptedRef (+1); return proposedU
          else return currentU

  let loop 0 currentU = return currentU
      loop i currentU = do
        nextU <- step currentU
        if i <= hmcIterations cfg
          then modifyIORef' samplesRef (fromUnconstrainedParams transforms nextU :)
          else return ()
        loop (i - 1) nextU

  _ <- loop total initU
  samples  <- fmap reverse (readIORef samplesRef)
  accepted <- readIORef acceptedRef
  return Chain
    { chainSamples  = samples
    , chainAccepted = accepted
    , chainTotal    = total
    }

-- | HMC を numChains 本並列実行する (+RTS -N で CPU 並列)。
hmcChains :: Model a -> HMCConfig -> Int -> Params -> GenIO -> IO [Chain]
hmcChains model cfg numChains initC baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> hmc model cfg initC g) gens
