-- |
-- Module      : Hanalyze.MCMC.MH
-- Description : Random-Walk Metropolis-Hastings サンプラー
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Random-Walk Metropolis-Hastings sampler.
--
-- Tune the per-parameter step sizes ('mcmcStepSizes') so the acceptance rate
-- lands in the 20-50% range. Pair 'Hanalyze.MCMC.Core.Chain' with
-- 'Hanalyze.Viz.Report.renderReport' to produce diagnostic plots.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Hanalyze.MCMC.MH
  ( MCMCConfig (..)
  , defaultMCMCConfig
  , metropolis
  , metropolisChains
  , metropolisPure
  , metropolisChainsPure
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM, replicateM)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.Primitive.MutVar
import Data.Word (Word32)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.Text (Text)
import System.Random.MWC (Gen, GenIO, uniform, initialize)
import System.Random.MWC.Distributions (normal)

import Hanalyze.Model.HBM (ModelP, Params, logJoint, sampleNames)
import Hanalyze.MCMC.Core (Chain (..), spawnGen)

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
metropolis :: PrimMonad m => ModelP r -> MCMCConfig -> Params -> Gen (PrimState m) -> m Chain
metropolis model cfg init_ gen = do
  let names = sampleNames model
      total = mcmcBurnIn cfg + mcmcIterations cfg
      steps = mcmcStepSizes cfg

  samplesRef  <- newMutVar []
  acceptedRef <- newMutVar (0 :: Int)

  let step current = do
        proposed <- fmap Map.fromList $ forM names $ \n -> do
          let s   = Map.findWithDefault 1.0 n steps
              cur = Map.findWithDefault 0.0 n current
          eps <- normal 0 s gen
          return (n, cur + eps)
        let logA = logJoint model proposed - logJoint model current
        u <- uniform gen
        if log (u :: Double) < logA
          then do modifyMutVar' acceptedRef (+1)
                  return proposed
          else return current

  let loop 0 current = return current
      loop i current = do
        next <- step current
        if i <= mcmcIterations cfg
          then modifyMutVar' samplesRef (next :)
          else return ()
        loop (i - 1) next

  _ <- loop total init_
  samples  <- fmap reverse (readMutVar samplesRef)
  accepted <- readMutVar acceptedRef
  return Chain
    { chainSamples  = samples
    , chainAccepted = accepted
    , chainTotal    = total
    , chainEnergy   = []
    , chainDivergences = []
    , chainTreeDepths  = []
    }

-- | Run 'metropolis' on @numChains@ parallel chains, each with an
-- independent RNG (use @+RTS -N@ to run on multiple cores).
metropolisChains :: ModelP r -> MCMCConfig -> Int -> Params -> GenIO -> IO [Chain]
metropolisChains model cfg numChains initP baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> metropolis model cfg initP g) gens

-- | Phase 50: 純粋・決定的な Metropolis (seed → 確定 Chain・IO 不要)。
metropolisPure :: ModelP r -> MCMCConfig -> Params -> Word32 -> Chain
metropolisPure model cfg initP seed =
  runST (initialize (V.singleton seed) >>= metropolis model cfg initP)

-- | Phase 50: 純粋・決定的な multi-chain Metropolis。 親 seed から子 seed を純粋導出し
-- 各 chain 別 'runST' → @parList rdeepseq@ で chain 横断を並列評価 (決定性は seed 由来)。
metropolisChainsPure :: ModelP r -> MCMCConfig -> Int -> Params -> Word32 -> [Chain]
metropolisChainsPure model cfg numChains initP seed =
  let childSeeds :: [Word32]
      childSeeds = runST $ do
        g <- initialize (V.singleton seed)
        replicateM numChains (uniform g)
      chains = [ metropolisPure model cfg initP s | s <- childSeeds ]
  in chains `using` parList rdeepseq
