{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | No-U-Turn Sampler (NUTS)。
--
-- Hoffman & Gelman (2014) Algorithm 3 + Nesterov dual averaging を実装。
-- 勾配は 'Numeric.AD.Mode.Forward' による正確な値を使用。
--
-- 制約付きパラメータ (PositiveT, UnitIntervalT) は事前分布から自動検出。
--
-- @
-- import Model.HBM
-- import MCMC.NUTS
--
-- chain <- nuts myModel defaultNUTSConfig
--                (Map.fromList [("mu",0),("sigma",1)]) gen
-- @
module MCMC.NUTS
  ( NUTSConfig (..)
  , defaultNUTSConfig
  , nuts
  , nutsChains
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (foldM, forM, replicateM, when)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import System.Random.MWC (GenIO, uniform)
import System.Random.MWC.Distributions (standard)

import MCMC.Core (Chain (..), spawnGen)
import MCMC.HMC  (kinetic, leapfrogWith, paramsToVec)
import Model.HBM (ModelP, Params, sampleNames, getTransforms,
                  logJointUnconstrained, gradADU)
import Stat.Distribution (toUnconstrained, fromUnconstrained)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data NUTSConfig = NUTSConfig
  { nutsIterations    :: Int
  , nutsBurnIn        :: Int
  , nutsStepSize      :: Double
  , nutsMaxDepth      :: Int
  , nutsAdaptStepSize :: Bool
  , nutsTargetAccept  :: Double
  } deriving (Show)

defaultNUTSConfig :: NUTSConfig
defaultNUTSConfig = NUTSConfig
  { nutsIterations    = 2000
  , nutsBurnIn        = 500
  , nutsStepSize      = 0.1
  , nutsMaxDepth      = 10
  , nutsAdaptStepSize = True
  , nutsTargetAccept  = 0.8
  }

-- ---------------------------------------------------------------------------
-- Dual averaging
-- ---------------------------------------------------------------------------

data DualAvgState = DualAvgState
  { daLogEps     :: Double
  , daLogEpsBar  :: Double
  , daH          :: Double
  , daMu         :: Double
  , daM          :: Int
  }

initDualAvg :: Double -> DualAvgState
initDualAvg eps0 = DualAvgState
  { daLogEps    = log eps0
  , daLogEpsBar = log eps0
  , daH         = 0.0
  , daMu        = log (10 * eps0)
  , daM         = 0
  }

updateDualAvg :: Double -> Double -> DualAvgState -> DualAvgState
updateDualAvg delta alpha da =
  let m      = daM da + 1
      gamma  = 0.05
      t0     = 10.0
      kappa  = 0.75
      hNew   = (1 - 1 / (fromIntegral m + t0)) * daH da
             + (1 / (fromIntegral m + t0)) * (delta - alpha)
      logEps = daMu da - sqrt (fromIntegral m) / gamma * hNew
      logEpsClip = max (-7) (min 5 logEps)
      logEpsBar = (fromIntegral m ** (-kappa)) * logEpsClip
                + (1 - fromIntegral m ** (-kappa)) * daLogEpsBar da
  in da { daLogEps = logEpsClip, daLogEpsBar = logEpsBar, daH = hNew, daM = m }

-- ---------------------------------------------------------------------------
-- 内部ツリー
-- ---------------------------------------------------------------------------

data NUTSTree = NUTSTree
  { ntThMinus :: Params
  , ntRMinus  :: [Double]
  , ntThPlus  :: Params
  , ntRPlus   :: [Double]
  , ntThPrime :: Params
  , ntN       :: Int
  , ntS       :: Bool
  }

deltaMax :: Double
deltaMax = 1000.0

uTurn :: [Text] -> Params -> [Double] -> Params -> [Double] -> Bool
uTurn names thMinus rMinus thPlus rPlus =
  let delta     = zipWith (-) (paramsToVec names thPlus) (paramsToVec names thMinus)
      dot xs ys = sum (zipWith (*) xs ys)
  in dot delta rMinus < 0 || dot delta rPlus < 0

-- ---------------------------------------------------------------------------
-- ツリービルダー
-- ---------------------------------------------------------------------------

buildTree
  :: ([Text] -> Params -> [Double])
  -> (Params -> Double)
  -> [Text]
  -> Double
  -> Params
  -> [Double]
  -> Double
  -> Int
  -> Int
  -> GenIO
  -> IO NUTSTree
buildTree gradFn logPiFn names eps theta r logU dir depth gen
  | depth == 0 = do
      let (theta', r') = leapfrogWith gradFn names (fromIntegral dir * eps) 1 theta r
          h'  = -(logPiFn theta') + kinetic r'
          n'  = if logU <= -h' then 1 else 0
          s'  = logU < deltaMax - h'
      return NUTSTree
        { ntThMinus = theta', ntRMinus = r'
        , ntThPlus  = theta', ntRPlus  = r'
        , ntThPrime = theta', ntN = n', ntS = s'
        }
  | otherwise = do
      t1 <- buildTree gradFn logPiFn names eps theta r logU dir (depth - 1) gen
      if not (ntS t1) then return t1
      else do
        let (th0, r0) = if dir == -1
              then (ntThMinus t1, ntRMinus t1)
              else (ntThPlus  t1, ntRPlus  t1)
        t2 <- buildTree gradFn logPiFn names eps th0 r0 logU dir (depth - 1) gen
        let n1 = ntN t1; n2 = ntN t2
        thPrime' <-
          if n1 == 0 then return (ntThPrime t2)
          else if n2 == 0 then return (ntThPrime t1)
          else do
            u <- uniform gen :: IO Double
            return $ if u < min 1.0 (fromIntegral n2 / fromIntegral n1)
                     then ntThPrime t2
                     else ntThPrime t1
        let (minus', rMinus', plus', rPlus') = if dir == -1
              then (ntThMinus t2, ntRMinus t2, ntThPlus t1, ntRPlus t1)
              else (ntThMinus t1, ntRMinus t1, ntThPlus t2, ntRPlus t2)
            s' = ntS t2 && not (uTurn names minus' rMinus' plus' rPlus')
        return NUTSTree
          { ntThMinus = minus', ntRMinus = rMinus'
          , ntThPlus  = plus',  ntRPlus  = rPlus'
          , ntThPrime = thPrime', ntN = n1 + n2, ntS = s'
          }

-- ---------------------------------------------------------------------------
-- NUTS サンプラー
-- ---------------------------------------------------------------------------

-- | 多相 HBM モデル ('ModelP') に対する NUTS サンプラー。
-- 軌道長は U-Turn 判定で自動決定。
nuts :: ModelP r -> NUTSConfig -> Params -> GenIO -> IO Chain
nuts m cfg initC gen = do
  let names      = sampleNames m
      trMap      = getTransforms m
      transList  = [Map.findWithDefault errT n trMap | n <- names]
      errT       = error "nuts: missing transform"

      initU = Map.fromList
        [ (n, toUnconstrained t v)
        | (n, t) <- zip names transList
        , Just v <- [Map.lookup n initC] ]

      total   = nutsBurnIn cfg + nutsIterations cfg
      doAdapt = nutsAdaptStepSize cfg && nutsBurnIn cfg > 0

      logPiFn :: Params -> Double
      logPiFn paramsU = logJointUnconstrained m names transList paramsU

      gradFn :: [Text] -> Params -> [Double]
      gradFn ns paramsU =
        let xs = [Map.findWithDefault 0 n paramsU | n <- ns]
        in map negate (gradADU m names transList xs)

      toConstrained pu = Map.fromList
        [ (n, fromUnconstrained t (Map.findWithDefault 0 n pu))
        | (n, t) <- zip names transList ]

  samplesRef  <- newIORef []
  acceptedRef <- newIORef (0 :: Int)
  daRef       <- newIORef (initDualAvg (nutsStepSize cfg))

  let step eps currentU = do
        r0 <- forM names (\_ -> standard gen)
        u0 <- uniform gen :: IO Double
        let h0   = -(logPiFn currentU) + kinetic r0
            logU = log u0 - h0
        let tree0 = NUTSTree
              { ntThMinus = currentU, ntRMinus = r0
              , ntThPlus  = currentU, ntRPlus  = r0
              , ntThPrime = currentU, ntN = 1, ntS = True
              }
        let doubleTree tree j =
              if not (ntS tree) then return tree
              else do
                u <- uniform gen :: IO Double
                let dir = if u < 0.5 then -1 else 1 :: Int
                    (th0, r0') = if dir == -1
                      then (ntThMinus tree, ntRMinus tree)
                      else (ntThPlus  tree, ntRPlus  tree)
                subtree <- buildTree gradFn logPiFn names eps th0 r0' logU dir j gen
                let n1 = ntN tree; n2 = ntN subtree
                thPrime' <-
                  if not (ntS subtree) || n2 == 0
                  then return (ntThPrime tree)
                  else do
                    u2 <- uniform gen :: IO Double
                    return $ if u2 < min 1.0 (fromIntegral n2 / fromIntegral n1)
                             then ntThPrime subtree
                             else ntThPrime tree
                let (minus', rMinus', plus', rPlus') = if dir == -1
                      then (ntThMinus subtree, ntRMinus subtree,
                            ntThPlus  tree,    ntRPlus  tree)
                      else (ntThMinus tree,    ntRMinus tree,
                            ntThPlus  subtree, ntRPlus  subtree)
                    s' = ntS subtree && not (uTurn names minus' rMinus' plus' rPlus')
                return NUTSTree
                  { ntThMinus = minus', ntRMinus = rMinus'
                  , ntThPlus  = plus',  ntRPlus  = rPlus'
                  , ntThPrime = thPrime', ntN = n1 + n2, ntS = s'
                  }
        finalTree <- foldM doubleTree tree0 [0 .. nutsMaxDepth cfg - 1]
        let proposedU = ntThPrime finalTree
            (thetaOne, rOne) = leapfrogWith gradFn names eps 1 currentU r0
            hOne   = -(logPiFn thetaOne) + kinetic rOne
            alpha  = min 1.0 (exp (h0 - hOne))
        when (proposedU /= currentU) $ modifyIORef' acceptedRef (+1)
        return (proposedU, alpha)

  let loop 0 currentU _eps = return currentU
      loop i currentU eps = do
        (nextU, alpha) <- step eps currentU
        let isBurnIn = i > nutsIterations cfg
        eps' <- if doAdapt && isBurnIn
          then do
            da <- readIORef daRef
            let da' = updateDualAvg (nutsTargetAccept cfg) alpha da
            writeIORef daRef da'
            return (exp (daLogEps da'))
          else do
            da <- readIORef daRef
            let epsBar = if doAdapt && not isBurnIn && i == nutsIterations cfg
                         then exp (daLogEpsBar da)
                         else eps
            return epsBar
        if not isBurnIn
          then modifyIORef' samplesRef (toConstrained nextU :)
          else return ()
        loop (i - 1) nextU eps'

  _ <- loop total initU (nutsStepSize cfg)
  samples  <- fmap reverse (readIORef samplesRef)
  accepted <- readIORef acceptedRef
  return Chain
    { chainSamples  = samples
    , chainAccepted = accepted
    , chainTotal    = total
    }

-- | NUTS を numChains 本並列実行する。
nutsChains :: ModelP r -> NUTSConfig -> Int -> Params -> GenIO -> IO [Chain]
nutsChains m cfg numChains initC baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> nuts m cfg initC g) gens
