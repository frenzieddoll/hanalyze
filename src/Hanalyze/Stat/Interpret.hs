{-# LANGUAGE OverloadedStrings #-}
-- | Model interpretability tools.
--
-- Model-agnostic explanations of predictions:
--
--   * 'permutationImportance' — feature importance by random shuffling
--     (Breiman 2001).
--   * 'partialDependence' — marginal effect of a feature on predictions
--     (Friedman 2001).
--   * 'icePlot' — individual conditional expectation curves (Goldstein
--     et al. 2015).
--
-- These work on any black-box model exposed as a function
-- @predict :: [Double] -> Double@ or @[[Double]] -> [Double]@; the
-- caller is responsible for plumbing in their fitted model.
module Hanalyze.Stat.Interpret
  ( -- * Permutation feature importance
    PermutationConfig (..)
  , defaultPermutationConfig
  , PermutationImportance (..)
  , permutationImportance
    -- * Partial dependence
  , PDPResult (..)
  , partialDependence
    -- * Individual conditional expectation
  , ICEResult (..)
  , icePlot
  ) where

import qualified System.Random.MWC     as MWC
import qualified Data.Vector           as V
import qualified Data.Vector.Mutable   as VM
import           Control.Monad         (forM, forM_)

-- ---------------------------------------------------------------------------
-- Permutation feature importance
-- ---------------------------------------------------------------------------

-- | Configuration for permutation importance.
data PermutationConfig = PermutationConfig
  { pcNRepeats :: !Int
    -- ^ Number of times to shuffle each feature (Breiman recommends 10-30).
  } deriving (Show, Eq)

-- | Default: 30 repeats.
defaultPermutationConfig :: PermutationConfig
defaultPermutationConfig = PermutationConfig { pcNRepeats = 30 }

-- | Result of permutation importance.
data PermutationImportance = PermutationImportance
  { piMeanImportance :: ![Double]   -- ^ Per-feature mean drop in score.
  , piStdImportance  :: ![Double]   -- ^ Per-feature std dev across repeats.
  , piBaselineScore  :: !Double     -- ^ Score on un-shuffled data.
  } deriving (Show)

-- | Compute permutation importance for each feature.
--
-- For each feature @j@:
--
--   1. Shuffle column @j@ across rows.
--   2. Predict and compute score.
--   3. Importance @= baseline_score − shuffled_score@.
--
-- A higher score means the feature was more important.
--
-- The user supplies:
--
--   * a predict function @[[Double]] -> [Double]@,
--   * a score function comparing true vs predicted (e.g. accuracy,
--     R²; higher is better).
permutationImportance
  :: PermutationConfig
  -> ([[Double]] -> [Double])     -- ^ Predict.
  -> ([Double] -> [Double] -> Double)  -- ^ Score (true, pred -> Double).
  -> [[Double]]                   -- ^ Test X.
  -> [Double]                     -- ^ True y.
  -> MWC.GenIO
  -> IO PermutationImportance
permutationImportance cfg predict score xs ys gen =
  let nFeat = if null xs then 0 else length (head xs)
      nReps = pcNRepeats cfg
      baseline = score ys (predict xs)
  in do
    perFeat <- forM [0 .. nFeat - 1] $ \j -> do
      drops <- forM [1 .. nReps] $ \_ -> do
        xsShuffled <- shuffleColumn j xs gen
        let predShuf = predict xsShuffled
            scoreShuf = score ys predShuf
        pure (baseline - scoreShuf)
      let n     = fromIntegral nReps :: Double
          mean  = sum drops / n
          var   = sum [(d - mean) ^ (2 :: Int) | d <- drops]
                  / max 1 (n - 1)
      pure (mean, sqrt var)
    pure PermutationImportance
      { piMeanImportance = map fst perFeat
      , piStdImportance  = map snd perFeat
      , piBaselineScore  = baseline
      }

-- | Shuffle column @j@ of a 2D feature matrix.
shuffleColumn :: Int -> [[Double]] -> MWC.GenIO -> IO [[Double]]
shuffleColumn j xs gen = do
  let column = [row !! j | row <- xs]
  shuffled <- shuffleList column gen
  pure [ [if k == j then shuffled !! i else row !! k
         | k <- [0 .. length row - 1]]
       | (i, row) <- zip [0 ..] xs ]

-- ---------------------------------------------------------------------------
-- Partial dependence
-- ---------------------------------------------------------------------------

-- | Partial dependence plot result.
data PDPResult = PDPResult
  { pdpFeatureValues :: ![Double]     -- ^ Grid points for the chosen feature.
  , pdpMeanPredict   :: ![Double]     -- ^ Mean prediction at each grid point.
  } deriving (Show)

-- | Partial dependence: marginal effect of feature @j@ on prediction.
--
-- For each value @v@ on the grid:
--
--   1. Replace column @j@ with @v@ in every row of the dataset.
--   2. Predict on the modified dataset.
--   3. Average predictions to get @PD(v)@.
--
-- @
-- PD_j(v) = (1/n) Σ_i predict(replaceCol(x_i, j, v))
-- @
partialDependence
  :: ([[Double]] -> [Double])    -- ^ Predict.
  -> [[Double]]                  -- ^ Background X.
  -> Int                         -- ^ Feature index j.
  -> [Double]                    -- ^ Grid of values for feature j.
  -> PDPResult
partialDependence predict xs j grid =
  let pdAt v =
        let xsModified = [replaceAt j v row | row <- xs]
            preds = predict xsModified
        in sum preds / fromIntegral (length preds)
      means = [pdAt v | v <- grid]
  in PDPResult
       { pdpFeatureValues = grid
       , pdpMeanPredict   = means
       }

-- ---------------------------------------------------------------------------
-- Individual conditional expectation (ICE)
-- ---------------------------------------------------------------------------

-- | ICE plot result: one curve per row in the input, plus the average
-- (= partial dependence).
data ICEResult = ICEResult
  { iceFeatureValues :: ![Double]
  , iceCurves        :: ![[Double]]   -- ^ Per-sample prediction curves.
  , iceMean          :: ![Double]     -- ^ Average curve (= partial dep).
  } deriving (Show)

-- | Compute ICE curves: per-sample partial-dependence-style plots.
--
-- Same as partial dependence, but instead of averaging across samples
-- we keep each sample's curve. Useful for detecting heterogeneous
-- effects (interactions).
icePlot
  :: ([[Double]] -> [Double])    -- ^ Predict.
  -> [[Double]]                  -- ^ Samples (each gets its own curve).
  -> Int                         -- ^ Feature index j.
  -> [Double]                    -- ^ Grid of values.
  -> ICEResult
icePlot predict xs j grid =
  let -- For each grid value, predict for ALL samples (with feature j replaced).
      predsByGrid =
        [ predict [replaceAt j v row | row <- xs]
        | v <- grid ]
      -- Reshape: predsByGrid[g][i] → curves[i] is [predsByGrid[g][i] for g].
      curves =
        [ [ predsByGrid !! g !! i | g <- [0 .. length grid - 1] ]
        | i <- [0 .. length xs - 1] ]
      meanCurve =
        [ sum [predsByGrid !! g !! i | i <- [0 .. length xs - 1]]
          / fromIntegral (length xs)
        | g <- [0 .. length grid - 1] ]
  in ICEResult
       { iceFeatureValues = grid
       , iceCurves        = curves
       , iceMean          = meanCurve
       }

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Replace element at position @i@ in a list.
replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ []     = []
replaceAt 0 v (_:xs) = v : xs
replaceAt i v (x:xs) = x : replaceAt (i - 1) v xs

-- | Shuffle a list (Fisher-Yates).
shuffleList :: [a] -> MWC.GenIO -> IO [a]
shuffleList xs gen = do
  let n = length xs
  v <- V.thaw (V.fromList xs)
  forM_ [n - 1, n - 2 .. 1] $ \i -> do
    j <- MWC.uniformR (0, i) gen
    a <- VM.read v i
    b <- VM.read v j
    VM.write v i b
    VM.write v j a
  V.toList <$> V.freeze v
