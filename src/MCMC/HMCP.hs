{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | 多相 HBM DSL ('Model.HBMP') 用の HMC / NUTS サンプラー。
--
-- 内部で 'Numeric.AD.Mode.Forward.grad' を使って勾配を正確に計算する。
-- 制約付きパラメータ (PositiveT, UnitIntervalT) は事前分布から自動検出。
--
-- @
-- import Model.HBMP
-- import MCMC.HMCP
--
-- myModel :: ModelP ()
-- myModel = do
--   mu    <- sample "mu"    (Normal 0 10)
--   sigma <- sample "sigma" (Exponential 1)
--   observe "y" (Normal mu sigma) [1.5, 2.0, 1.8]
--
-- chain <- hmcP myModel defaultHMCConfig (Map.fromList [("mu",0),("sigma",1)]) gen
-- @
module MCMC.HMCP
  ( hmcP
  , hmcPChains
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM, replicateM, when)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import System.Random.MWC (GenIO, uniform)
import System.Random.MWC.Distributions (standard)

import MCMC.Core (Chain (..), spawnGen)
import MCMC.HMC  (HMCConfig (..), leapfrogWith, kinetic)
import Model.HBMP (ModelP, sampleNames, getTransforms,
                   logJointUnconstrained, gradADU)
import Stat.Distribution (Transform, toUnconstrained, fromUnconstrained)

type Params = Map Text Double

-- | 多相 HBM モデル ('ModelP') に対する HMC サンプラー。
--
-- AD 勾配を使うため数値微分版 'MCMC.HMC.hmc' より正確で速い (パラメータ数 < ~50)。
-- 制約変換は 'getTransforms' で事前分布から自動検出する。
hmcP :: ModelP r -> HMCConfig -> Params -> GenIO -> IO Chain
hmcP m cfg initC gen = do
  let names      = sampleNames m
      trMap      = getTransforms m
      transList  = [Map.findWithDefault errT n trMap | n <- names]
      errT       = error "hmcP: missing transform (should not happen)"

      initU = Map.fromList
        [ (n, toUnconstrained t v)
        | (n, t) <- zip names transList
        , Just v <- [Map.lookup n initC] ]

      total = hmcBurnIn cfg + hmcIterations cfg

      -- log p*(u) = log p(θ(u), y) + log|∂θ/∂u|
      logJU :: Params -> Double
      logJU paramsU = logJointUnconstrained m names transList paramsU

      -- ∇U = -∇log p*(u)  (leapfrogWith は U 勾配を期待)
      gradFn :: [Text] -> Params -> [Double]
      gradFn ns paramsU =
        let xs = [Map.findWithDefault 0 n paramsU | n <- ns]
        in map negate (gradADU m names transList xs)

  samplesRef  <- newIORef []
  acceptedRef <- newIORef (0 :: Int)

  let step currentU = do
        r <- forM names (\_ -> standard gen)
        let (proposedU, rFinal) =
              leapfrogWith gradFn names
                           (hmcStepSize cfg) (hmcLeapfrogSteps cfg)
                           currentU r
            logAlpha = (logJU proposedU - kinetic rFinal)
                     - (logJU currentU  - kinetic r)
        u <- uniform gen
        if log (u :: Double) < logAlpha
          then do modifyIORef' acceptedRef (+1); return proposedU
          else return currentU

  let toConstrained pu = Map.fromList
        [ (n, fromUnconstrained t (Map.findWithDefault 0 n pu))
        | (n, t) <- zip names transList ]

  let loop 0 currentU = return currentU
      loop i currentU = do
        nextU <- step currentU
        when (i <= hmcIterations cfg) $
          modifyIORef' samplesRef (toConstrained nextU :)
        loop (i - 1) nextU

  _ <- loop total initU
  samples  <- fmap reverse (readIORef samplesRef)
  accepted <- readIORef acceptedRef
  return Chain
    { chainSamples  = samples
    , chainAccepted = accepted
    , chainTotal    = total
    }

-- | hmcP を numChains 本並列実行する。
hmcPChains :: ModelP r -> HMCConfig -> Int -> Params -> GenIO -> IO [Chain]
hmcPChains m cfg numChains initC baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> hmcP m cfg initC g) gens
