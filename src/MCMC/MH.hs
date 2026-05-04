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

-- | Random-Walk Metropolis configuration.
data MCMCConfig = MCMCConfig
  { mcmcIterations :: Int                   -- ^ Total iterations (burn-in included).
  , mcmcBurnIn     :: Int                   -- ^ Burn-in iterations to discard.
  , mcmcStepSizes  :: Map.Map Text Double   -- ^ Per-parameter proposal step.
  } deriving (Show)

-- | Default configuration: 2000 iterations, 500 burn-in, step size 1.0
-- for every parameter.
defaultMCMCConfig :: [Text] -> MCMCConfig
defaultMCMCConfig names = MCMCConfig
  { mcmcIterations = 2000
  , mcmcBurnIn     = 500
  , mcmcStepSizes  = Map.fromList [(n, 1.0) | n <- names]
  }

-- ---------------------------------------------------------------------------
-- Random Walk Metropolis
-- ---------------------------------------------------------------------------

-- | Run Random-Walk Metropolis. Uses a joint proposal that updates all
-- latent variables simultaneously.
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

-- | Run 'metropolis' on @numChains@ parallel chains, each with an
-- independent RNG (use @+RTS -N@ to run on multiple cores).
metropolisChains :: ModelP r -> MCMCConfig -> Int -> Params -> GenIO -> IO [Chain]
metropolisChains model cfg numChains initP baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> metropolis model cfg initP g) gens
