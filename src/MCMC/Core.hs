{-# LANGUAGE OverloadedStrings #-}
-- | Common MCMC types and posterior statistics.
--
-- Sampler-agnostic: this is the foundation when @MCMC.*@ is used as a
-- standalone sampling library.
module MCMC.Core
  ( -- * チェーン型
    Chain (..)
    -- * 事後統計量
  , acceptanceRate
  , posteriorMean
  , posteriorSD
  , posteriorQuantile
  , chainVals
    -- * ユーティリティ
  , spawnGen
  ) where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Word (Word32)
import qualified Data.Vector as V
import System.Random.MWC (GenIO, uniform, initialize)

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
  } deriving (Show)

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
-- when feeding 'Stat.MCMC.rhat' and friends.
chainVals :: Text -> Chain -> [Double]
chainVals name ch = [v | Just v <- map (Map.lookup name) (chainSamples ch)]

-- ---------------------------------------------------------------------------
-- Utility
-- ---------------------------------------------------------------------------

-- | Spawn an independent child 'GenIO' seeded from a parent generator.
-- Used to give each parallel chain a different seed.
spawnGen :: GenIO -> IO GenIO
spawnGen base = do
  seed <- uniform base :: IO Word32
  initialize (V.singleton seed)
