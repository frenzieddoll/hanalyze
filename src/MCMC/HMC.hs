{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Hamiltonian Monte Carlo (HMC) サンプラー。
--
-- 'Model.HBM' の多相モデル ('ModelP') に対して 'Numeric.AD.Mode.Forward' で
-- 正確な勾配を計算します。制約付きパラメータ (PositiveT, UnitIntervalT) は
-- 事前分布から自動検出します。
--
-- @
-- import Model.HBM
-- import MCMC.HMC
--
-- myModel :: ModelP ()
-- myModel = do
--   mu    <- sample "mu"    (Normal 0 10)
--   sigma <- sample "sigma" (Exponential 1)
--   observe "y" (Normal mu sigma) [1.5, 2.0, 1.8]
--
-- chain <- hmc myModel defaultHMCConfig (Map.fromList [("mu",0),("sigma",1)]) gen
-- @
module MCMC.HMC
  ( -- * Configuration
    HMCConfig (..)
  , defaultHMCConfig
    -- * 制約変換ユーティリティ
  , toUnconstrainedParams
  , fromUnconstrainedParams
  , logJointU
  , leapfrogWith
    -- * 基本ユーティリティ
  , kinetic
  , paramsToVec
  , vecToParams
    -- * Sampler
  , hmc
  , hmcChains
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM, replicateM, when)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import System.Random.MWC (GenIO, uniform)
import System.Random.MWC.Distributions (standard)

import Model.HBM (ModelP, Params, sampleNames, getTransforms,
                  logJointUnconstrained, gradADU)
import MCMC.Core (Chain (..), spawnGen)
import Stat.Distribution (Transform, toUnconstrained, fromUnconstrained)

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

toUnconstrainedParams :: Map Text Transform -> Params -> Params
toUnconstrainedParams transforms =
  Map.mapWithKey (\k v -> maybe v (`toUnconstrained` v) (Map.lookup k transforms))

fromUnconstrainedParams :: Map Text Transform -> Params -> Params
fromUnconstrainedParams transforms =
  Map.mapWithKey (\k u -> maybe u (`fromUnconstrained` u) (Map.lookup k transforms))

-- ---------------------------------------------------------------------------
-- unconstrained 空間での log-joint (Jacobian 補正付き)
-- ---------------------------------------------------------------------------

-- | 多相モデルの unconstrained 空間における log-joint (VI / NUTS と共有)。
logJointU :: ModelP r -> Map Text Transform -> Params -> Double
logJointU model transforms paramsU =
  let names     = sampleNames model
      transList = [Map.findWithDefault errT n transforms | n <- names]
      errT      = error "logJointU: transform missing"
  in logJointUnconstrained model names transList paramsU

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

-- ---------------------------------------------------------------------------
-- HMC サンプラー (AD 勾配版)
-- ---------------------------------------------------------------------------

-- | 多相 HBM モデル ('ModelP') に対する HMC サンプラー。
-- AD 勾配 ('Numeric.AD.Mode.Forward') を使うため数値微分より正確で速い。
-- 制約変換は 'getTransforms' で事前分布から自動検出する。
hmc :: ModelP r -> HMCConfig -> Params -> GenIO -> IO Chain
hmc m cfg initC gen = do
  let names      = sampleNames m
      trMap      = getTransforms m
      transList  = [Map.findWithDefault errT n trMap | n <- names]
      errT       = error "hmc: missing transform (should not happen)"

      initU = Map.fromList
        [ (n, toUnconstrained t v)
        | (n, t) <- zip names transList
        , Just v <- [Map.lookup n initC] ]

      total = hmcBurnIn cfg + hmcIterations cfg

      logJU :: Params -> Double
      logJU paramsU = logJointUnconstrained m names transList paramsU

      gradFn :: [Text] -> Params -> [Double]
      gradFn ns paramsU =
        let xs = [Map.findWithDefault 0 n paramsU | n <- ns]
        in map negate (gradADU m names transList xs)

  samplesRef  <- newIORef []
  energyRef   <- newIORef ([] :: [Double])
  acceptedRef <- newIORef (0 :: Int)

  let step currentU = do
        r <- forM names (\_ -> standard gen)
        let h0 = -(logJU currentU) + kinetic r
            (proposedU, rFinal) =
              leapfrogWith gradFn names
                           (hmcStepSize cfg) (hmcLeapfrogSteps cfg)
                           currentU r
            logAlpha = (logJU proposedU - kinetic rFinal)
                     - (logJU currentU  - kinetic r)
        u <- uniform gen
        nextU <- if log (u :: Double) < logAlpha
          then do modifyIORef' acceptedRef (+1); return proposedU
          else return currentU
        return (nextU, h0)

  let toConstrained pu = Map.fromList
        [ (n, fromUnconstrained t (Map.findWithDefault 0 n pu))
        | (n, t) <- zip names transList ]

  let loop 0 currentU = return currentU
      loop i currentU = do
        (nextU, h0) <- step currentU
        when (i <= hmcIterations cfg) $ do
          modifyIORef' samplesRef (toConstrained nextU :)
          modifyIORef' energyRef  (h0 :)
        loop (i - 1) nextU

  _ <- loop total initU
  samples  <- fmap reverse (readIORef samplesRef)
  energies <- fmap reverse (readIORef energyRef)
  accepted <- readIORef acceptedRef
  return Chain
    { chainSamples  = samples
    , chainAccepted = accepted
    , chainTotal    = total
    , chainEnergy   = energies
    , chainDivergences = []
    }

-- | hmc を numChains 本並列実行する (+RTS -N で CPU 並列)。
hmcChains :: ModelP r -> HMCConfig -> Int -> Params -> GenIO -> IO [Chain]
hmcChains m cfg numChains initC baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> hmc m cfg initC g) gens
