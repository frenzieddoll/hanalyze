-- |
-- Module      : Hanalyze.MCMC.Core
-- Description : MCMC 共通の Chain 型と事後統計量 (mean/SD/分位点)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Common MCMC types and posterior statistics.
--
-- Sampler-agnostic: this is the foundation when @MCMC.*@ is used as a
-- standalone sampling library.
{-# LANGUAGE OverloadedStrings #-}
module Hanalyze.MCMC.Core
  ( -- * チェーン型
    Chain (..)
    -- * Posterior statistics
  , acceptanceRate
  , posteriorMean
  , posteriorSD
  , posteriorQuantile
  , chainVals
    -- * Utilities
  , spawnGen
  ) where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Word (Word32)
import qualified Data.Vector as V
import System.Random.MWC (Gen, GenIO, uniform, initialize)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.DeepSeq (NFData (..))

-- ---------------------------------------------------------------------------
-- Chain
-- ---------------------------------------------------------------------------

-- | MCMC chain. Holds post-burn-in samples only.
data Chain = Chain
  { chainSamples  :: [Map.Map Text Double]  -- ^ Post-burn-in samples in draw order.
  , chainAccepted :: Int                    -- ^ Accepted proposals (burn-in included).
  , chainTotal    :: Int                    -- ^ Total proposals (burn-in included).
  , chainEnergy   :: [Double]
    -- ^ Hamiltonian energy @H = −log p(θ) + 0.5|p|²@ per post-burn-in
    --   iteration. Only meaningful for HMC / NUTS; samplers like MH /
    --   Gibbs leave it empty. Used by BFMI and the energy plot.
  , chainDivergences :: [Int]
    -- ^ Zero-origin iteration indices where NUTS reported a divergent
    --   transition (post-burn-in). Following Stan, the criterion is
    --   @|H_proposal − H_initial| > 1000@. Many divergences signal a
    --   pathological posterior that needs reparameterization.
  , chainTreeDepths :: [Int]
    -- ^ Phase 85.3: NUTS の per-draw tree depth (実行された doubling 回数・
    --   post-burn-in・draw 順)。 leapfrog 数 ≈ 2^depth ゆえ per-draw コストの
    --   診断に使う (PyMC の tree_depth 相当)。 NUTS 以外のサンプラは []。
  } deriving (Show)

-- | Phase 50: 純粋 multi-chain (`nutsChainsPure`) で @parList rdeepseq@ により
-- chain 横断を spark 並列評価するため、 'Chain' を完全評価できるようにする。
instance NFData Chain where
  rnf (Chain s a t e d td) =
    rnf s `seq` rnf a `seq` rnf t `seq` rnf e `seq` rnf d `seq` rnf td

-- ---------------------------------------------------------------------------
-- Summary statistics
-- ---------------------------------------------------------------------------

-- | Overall acceptance rate (burn-in included).
acceptanceRate :: Chain -> Double
acceptanceRate ch =
  fromIntegral (chainAccepted ch) / fromIntegral (chainTotal ch)

-- | Posterior mean for a given parameter, or 'Nothing' if absent.
posteriorMean :: Text -> Chain -> Maybe Double
posteriorMean name ch =
  let vals = chainVals name ch
  in if null vals then Nothing
     else Just (sum vals / fromIntegral (length vals))

-- | Posterior standard deviation for a given parameter.
posteriorSD :: Text -> Chain -> Maybe Double
posteriorSD name ch =
  case posteriorMean name ch of
    Nothing -> Nothing
    Just mu ->
      let vals = chainVals name ch
      in if null vals then Nothing
         else Just (sqrt (sum (map (\x -> (x - mu) ^ (2 :: Int)) vals)
                         / fromIntegral (length vals)))

-- | Empirical quantile of a parameter (@0 ≤ p ≤ 1@).
posteriorQuantile :: Double -> Text -> Chain -> Maybe Double
posteriorQuantile p name ch =
  let vals = sort (chainVals name ch)
      n    = length vals
  in if null vals then Nothing
     else
       let idx = min (n - 1) (floor (p * fromIntegral n) :: Int)
       in Just (vals !! idx)

-- | Extract the sample sequence for one parameter from a chain. Useful
-- when feeding 'Hanalyze.Stat.MCMC.rhat' and friends.
chainVals :: Text -> Chain -> [Double]
chainVals name ch = [v | Just v <- map (Map.lookup name) (chainSamples ch)]

-- ---------------------------------------------------------------------------
-- Utility
-- ---------------------------------------------------------------------------

-- | Spawn an independent child generator seeded from a parent generator.
-- Used to give each parallel chain a different seed.
--
-- Phase 50: 'PrimMonad' に一般化 (既存 IO 呼出は @m=IO@ で不変)。 これにより
-- @ST s@ でも同じ種まきができ、 純粋な multi-chain (runST + seed) に使える。
spawnGen :: PrimMonad m => Gen (PrimState m) -> m (Gen (PrimState m))
spawnGen base = do
  seed <- uniform base
  initialize (V.singleton (seed :: Word32))
