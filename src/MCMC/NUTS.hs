{-# LANGUAGE OverloadedStrings #-}
-- | No-U-Turn Sampler (NUTS)。
--
-- Hoffman & Gelman (2014) Algorithm 3 を実装。
-- 制約付きパラメータは unconstrained 空間で自動変換されます（HMC と同様）。
-- 自動的に最適な軌道長を決定するため、HMC のステップ数チューニングが不要。
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
import Data.Text (Text)
import System.Random.MWC (GenIO, uniform)
import System.Random.MWC.Distributions (standard)

import Model.HBM (Model, Params, sampleNames, getTransforms)
import MCMC.Core (Chain (..), spawnGen)
import MCMC.HMC
  ( kinetic, leapfrogWith, gradUU, logJointU
  , paramsToVec, toUnconstrainedParams, fromUnconstrainedParams
  )
import Stat.Distribution (Transform)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data NUTSConfig = NUTSConfig
  { nutsIterations :: Int
  , nutsBurnIn     :: Int
  , nutsStepSize   :: Double
  , nutsMaxDepth   :: Int
  } deriving (Show)

defaultNUTSConfig :: NUTSConfig
defaultNUTSConfig = NUTSConfig
  { nutsIterations = 2000
  , nutsBurnIn     = 500
  , nutsStepSize   = 0.1
  , nutsMaxDepth   = 10
  }

-- ---------------------------------------------------------------------------
-- 内部: バイナリツリー
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
-- 再帰的ツリービルダー
-- ---------------------------------------------------------------------------

buildTree
  :: Model a
  -> Map.Map Text Transform
  -> [Text]
  -> Double
  -> Params
  -> [Double]
  -> Double
  -> Int
  -> Int
  -> GenIO
  -> IO NUTSTree
buildTree model transforms names eps theta r logU dir depth gen
  | depth == 0 = do
      let gradFn  = gradUU model transforms
          (theta', r') = leapfrogWith gradFn names (fromIntegral dir * eps) 1 theta r
          h'  = -(logJointU model transforms theta') + kinetic r'
          n'  = if logU <= -h' then 1 else 0
          s'  = logU < deltaMax - h'
      return NUTSTree
        { ntThMinus = theta', ntRMinus = r'
        , ntThPlus  = theta', ntRPlus  = r'
        , ntThPrime = theta', ntN = n', ntS = s'
        }
  | otherwise = do
      t1 <- buildTree model transforms names eps theta r logU dir (depth - 1) gen
      if not (ntS t1) then return t1
      else do
        let (th0, r0) = if dir == -1
              then (ntThMinus t1, ntRMinus t1)
              else (ntThPlus  t1, ntRPlus  t1)
        t2 <- buildTree model transforms names eps th0 r0 logU dir (depth - 1) gen
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

nuts :: Model a -> NUTSConfig -> Params -> GenIO -> IO Chain
nuts model cfg initC gen = do
  let names      = sampleNames model
      transforms = getTransforms model
      initU      = toUnconstrainedParams transforms initC
      total      = nutsBurnIn cfg + nutsIterations cfg

  samplesRef  <- newIORef []
  acceptedRef <- newIORef (0 :: Int)

  let step currentU = do
        r0 <- forM names (\_ -> standard gen)
        u0 <- uniform gen :: IO Double
        let h0   = -(logJointU model transforms currentU) + kinetic r0
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
                subtree <- buildTree model transforms names (nutsStepSize cfg) th0 r0' logU dir j gen
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
        when (proposedU /= currentU) $ modifyIORef' acceptedRef (+1)
        return proposedU

  let loop 0 currentU = return currentU
      loop i currentU = do
        nextU <- step currentU
        if i <= nutsIterations cfg
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

-- | NUTS を numChains 本並列実行する (+RTS -N で CPU 並列)。
nutsChains :: Model a -> NUTSConfig -> Int -> Params -> GenIO -> IO [Chain]
nutsChains model cfg numChains initC baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> nuts model cfg initC g) gens
