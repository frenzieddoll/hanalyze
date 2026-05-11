{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Prior- and posterior-predictive sampling (analogous to PyMC's
-- @sample_prior_predictive@ / @sample_posterior_predictive@).
--
-- @
-- import Hanalyze.Stat.PosteriorPredictive
--
-- chain <- nuts model cfg initP gen
-- ppc   <- posteriorPredictive model chain gen
-- -- ppc :: [Map Text [Double]]   -- predicted observations per sample
-- @
module Hanalyze.Stat.PosteriorPredictive
  ( -- * 事後予測サンプリング (chain ベース)
    posteriorPredictive
  , posteriorPredictiveSummary
    -- * Prior predictive sampling (chain not required)
  , priorPredictive
    -- * Prior sampling (including latents)
  , samplePrior
  ) where

import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.List (sort)
import System.Random.MWC (GenIO)

import Hanalyze.MCMC.Core (Chain (..))
import Hanalyze.Model.HBM
  ( ModelP, sampleDist, runObserveDists, priorList )

-- ---------------------------------------------------------------------------
-- 事後予測サンプリング
-- ---------------------------------------------------------------------------

-- | Posterior-predictive samples for every observe node in the model.
--
-- Algorithm:
--
--   1. Walk the chain's latent samples.
--   2. At each sample, evaluate 'runObserveDists' to obtain the
--      conditional distribution at every observe node.
--   3. Draw as many fresh @y@ values from that distribution as the
--      original observation count.
--
-- The returned list has the same length as @chainSamples@; each element
-- is a @Map@ from observe-node name to a fresh predicted-value list of
-- the original length.
posteriorPredictive
  :: forall r. ModelP r
  -> Chain
  -> GenIO
  -> IO [Map Text [Double]]
posteriorPredictive m chain gen =
  mapM (\ps -> genFromObserves m ps gen) (chainSamples chain)

-- | Per-observation posterior-predictive summary statistics
-- (mean and 95 % credible interval).
--
-- Returns: observation name ↦ a list of @(mean, 2.5%, 97.5%)@ triples,
-- one per original observation index.
posteriorPredictiveSummary
  :: [Map Text [Double]]                           -- posteriorPredictive の出力
  -> Map Text [(Double, Double, Double)]
posteriorPredictiveSummary preds =
  let names = case preds of
                []    -> []
                (m:_) -> Map.keys m
  in Map.fromList
       [ (n, summarizePerObs (perSamplePerObs n preds)) | n <- names ]
  where
    -- 観測 n: 各サンプルの観測 i 番目を集めて [[Double]] (列ごと)
    perSamplePerObs :: Text -> [Map Text [Double]] -> [[Double]]
    perSamplePerObs nm samples =
      transpose (map (Map.findWithDefault [] nm) samples)

    summarizePerObs :: [[Double]] -> [(Double, Double, Double)]
    summarizePerObs cols = map oneObs cols
      where
        oneObs xs =
          let s   = sort xs
              n   = length s
              mu  = if n == 0 then 0 else sum xs / fromIntegral n
              q p = if n == 0 then 0
                              else s !! min (n - 1) (max 0 (floor (p * fromIntegral n) :: Int))
          in (mu, q 0.025, q 0.975)

    transpose :: [[a]] -> [[a]]
    transpose [] = []
    transpose xss
      | all null xss = []
      | otherwise =
          let heads = [h | (h:_) <- xss]
              tails = [t | (_:t) <- xss]
          in heads : transpose tails

-- ---------------------------------------------------------------------------
-- 事前予測サンプリング (チェーン不要)
-- ---------------------------------------------------------------------------

-- | Generate @N@ predictive samples from the prior alone (without any
-- observed data). Useful for sanity-checking what the model predicts
-- /before/ conditioning on observations.
priorPredictive
  :: forall r. ModelP r
  -> Int        -- ^ Number of samples @N@.
  -> GenIO
  -> IO [Map Text [Double]]
priorPredictive m n gen = replicateM n $ do
  ps <- samplePrior m gen
  genFromObserves m ps gen

-- | Draw one sample of every latent variable from its prior.
--
-- Note: 'priorList' walks the model with placeholder zeros to extract its
-- structure. This function then samples each latent independently from
-- its individual prior. For hierarchical models this does not match
-- PyMC's @sample_prior_predictive@ (which threads downstream dependencies),
-- but it is enough for quick prior sanity checks.
samplePrior :: forall r. ModelP r -> GenIO -> IO (Map Text Double)
samplePrior m gen = do
  let priors = priorList m   -- [(name, Distribution Double)] (placeholder=0 走査)
  vals <- mapM (\(_, d) -> sampleDist d gen) priors
  return (Map.fromList (zip (map fst priors) vals))

-- ---------------------------------------------------------------------------
-- 内部: 与えられた latent 値で観測を生成
-- ---------------------------------------------------------------------------

-- 各 observe ノードについて、元データの個数だけ新しいサンプルを生成。
genFromObserves
  :: forall r. ModelP r
  -> Map Text Double
  -> GenIO
  -> IO (Map Text [Double])
genFromObserves m ps gen = do
  let observes = runObserveDists m ps   -- [(name, Distribution Double, [Double])]
  newGroups <- mapM
    (\(nm, d, ys) -> do
        let nObs = length ys
        newYs <- replicateM nObs (sampleDist d gen)
        return (nm, newYs))
    observes
  -- 同名 observe が複数ある場合はリスト連結
  return $ Map.fromListWith (++) newGroups
