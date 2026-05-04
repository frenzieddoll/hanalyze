{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Random-Walk Metropolis-Hastings sampler.
--
-- Tune the per-parameter step sizes ('mcmcStepSizes') so the acceptance rate
-- lands in the 20-50% range. Pair 'MCMC.Core.Chain' with
-- 'Viz.Report.renderReport' to produce diagnostic plots.
module MCMC.MH
  ( MCMCConfig (..)
  , defaultMCMCConfig
  , metropolis
  , metropolisChains
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM, replicateM)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.Random.MWC (GenIO, uniform)
import System.Random.MWC.Distributions (normal)

import Model.HBM (ModelP, Params, logJoint, sampleNames)
import MCMC.Core (Chain (..), spawnGen)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data MCMCConfig = MCMCConfig
  { mcmcIterations :: Int
  , mcmcBurnIn     :: Int
  , mcmcStepSizes  :: Map.Map Text Double
  } deriving (Show)

defaultMCMCConfig :: [Text] -> MCMCConfig
defaultMCMCConfig names = MCMCConfig
  { mcmcIterations = 2000
  , mcmcBurnIn     = 500
  , mcmcStepSizes  = Map.fromList [(n, 1.0) | n <- names]
  }

-- ---------------------------------------------------------------------------
-- Random Walk Metropolis
-- ---------------------------------------------------------------------------

-- | Random Walk Metropolis を実行する。
-- 全潜在変数を同時に提案する joint proposal。
metropolis :: ModelP r -> MCMCConfig -> Params -> GenIO -> IO Chain
metropolis model cfg init_ gen = do
  let names = sampleNames model
      total = mcmcBurnIn cfg + mcmcIterations cfg
      steps = mcmcStepSizes cfg

  samplesRef  <- newIORef []
  acceptedRef <- newIORef (0 :: Int)

  let step current = do
        proposed <- fmap Map.fromList $ forM names $ \n -> do
          let s   = Map.findWithDefault 1.0 n steps
              cur = Map.findWithDefault 0.0 n current
          eps <- normal 0 s gen
          return (n, cur + eps)
        let logA = logJoint model proposed - logJoint model current
        u <- uniform gen
        if log (u :: Double) < logA
          then do modifyIORef' acceptedRef (+1)
                  return proposed
          else return current

  let loop 0 current = return current
      loop i current = do
        next <- step current
        if i <= mcmcIterations cfg
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
    , chainEnergy   = []
    , chainDivergences = []
    }

-- | Metropolis を numChains 本並列実行する。
-- 各チェーンは独立した乱数列を使う (+RTS -N で CPU 並列)。
metropolisChains :: ModelP r -> MCMCConfig -> Int -> Params -> GenIO -> IO [Chain]
metropolisChains model cfg numChains initP baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> metropolis model cfg initP g) gens
