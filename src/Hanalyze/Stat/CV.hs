{-# LANGUAGE OverloadedStrings #-}
-- | Cross-validation framework.
--
-- Provides train/validation splits and a generic 'crossValidate'
-- function that runs a user-supplied @fit@ + @score@ on each fold.
--
-- @
-- import Hanalyze.Stat.CV
-- import qualified System.Random.MWC as MWC
--
-- gen <- MWC.createSystemRandom
-- folds <- kFold 5 (LA.rows x) gen
-- scores <- crossValidate folds fitFn scoreFn (x, y)
-- let mean = sum scores / fromIntegral (length scores)
-- @
--
-- == Available split strategies
--
--   * 'kFold' (random k-fold)
--   * 'stratifiedKFold' (preserves class balance for classification)
--   * 'leaveOneOut'
--   * 'shuffleSplit' (random repeated train/test)
--   * 'timeSeriesSplit' (forward-chaining for time series)
--
-- All return @[Fold]@ where each 'Fold' is a pair @(trainIdx, testIdx)@.
module Hanalyze.Stat.CV
  ( -- * Fold types
    Fold
    -- * Split strategies
  , kFold
  , stratifiedKFold
  , leaveOneOut
  , shuffleSplit
  , timeSeriesSplit
    -- * Cross-validation
  , crossValidate
  , crossValidateScores
    -- * Hyperparameter search
  , gridSearchCV
  , GridSearchResult (..)
  ) where

import qualified Data.Map.Strict       as Map
import qualified Data.Vector           as V
import qualified Data.Vector.Mutable   as VM
import           Control.Monad         (forM, forM_)
import           Data.List             (sortBy)
import           Data.Ord              (comparing)
import qualified System.Random.MWC     as MWC

-- ---------------------------------------------------------------------------
-- Fold types
-- ---------------------------------------------------------------------------

-- | A single train / test split: @(trainIdx, testIdx)@. Indices are
-- 0-based row numbers into the original data.
type Fold = ([Int], [Int])

-- ---------------------------------------------------------------------------
-- Split strategies
-- ---------------------------------------------------------------------------

-- | Random k-fold split.
kFold
  :: Int            -- ^ Number of folds @k@.
  -> Int            -- ^ Total sample count @n@.
  -> MWC.GenIO
  -> IO [Fold]
kFold k n gen
  | k < 2     = pure [(allIdx n, [])]
  | k > n     = leaveOneOut n
  | otherwise = do
      perm <- shuffleIndices n gen
      let foldSize  = n `div` k
          remainder = n `mod` k
          -- Fold sizes: first 'remainder' folds get 1 extra.
          sizes = [foldSize + (if i < remainder then 1 else 0) | i <- [0..k-1]]
          starts = scanl (+) 0 sizes
          ranges = [(s, s + sz) | (s, sz) <- zip starts sizes]
          allRows = take n perm
      pure [ let testIdx  = take (e - s) (drop s allRows)
                 trainIdx = take s allRows ++ drop e allRows
             in (trainIdx, testIdx)
           | (s, e) <- ranges ]

-- | Stratified k-fold: preserves class proportions in each fold.
stratifiedKFold
  :: Int            -- ^ Number of folds @k@.
  -> [Int]          -- ^ Class labels (length @n@).
  -> MWC.GenIO
  -> IO [Fold]
stratifiedKFold k labels gen
  | k < 2 = pure [(allIdx (length labels), [])]
  | otherwise = do
      let n         = length labels
          byClass   = Map.fromListWith (++)
                        [(l, [i]) | (i, l) <- zip [0..] labels]
      -- For each class, shuffle its indices and split into k folds.
      classFolds <- forM (Map.toList byClass) $ \(_, idxs) -> do
        shuffled <- shuffleList idxs gen
        let m         = length shuffled
            foldSize  = m `div` k
            remainder = m `mod` k
            sizes     = [foldSize + (if i < remainder then 1 else 0)
                        | i <- [0..k-1]]
            starts    = scanl (+) 0 sizes
            ranges    = [(s, s + sz) | (s, sz) <- zip starts sizes]
        pure [take (e - s) (drop s shuffled) | (s, e) <- ranges]
      -- Combine: fold i = concat of i-th sub-fold from each class.
      let testIdxByFold =
            [ concat [classFolds !! ci !! fi | ci <- [0 .. length classFolds - 1]]
            | fi <- [0 .. k - 1] ]
          allI = [0 .. n - 1]
      pure [ let testIdx  = sortBy compare ti
                 trainIdx = filter (`notElem` testIdx) allI
             in (trainIdx, testIdx)
           | ti <- testIdxByFold ]

-- | Leave-one-out cross-validation: @n@ folds, each test set is a
-- single row.
leaveOneOut :: Int -> IO [Fold]
leaveOneOut n =
  pure [ ([j | j <- [0 .. n - 1], j /= i], [i]) | i <- [0 .. n - 1] ]

-- | Repeated random train/test split (Monte-Carlo CV).
shuffleSplit
  :: Int            -- ^ Number of repetitions.
  -> Double         -- ^ Test fraction (0 < t < 1).
  -> Int            -- ^ Total samples @n@.
  -> MWC.GenIO
  -> IO [Fold]
shuffleSplit nReps testFrac n gen = do
  let testN = max 1 (round (fromIntegral n * testFrac))
  forM [1 .. nReps] $ \_ -> do
    perm <- shuffleIndices n gen
    let testIdx  = take testN perm
        trainIdx = drop testN perm
    pure (trainIdx, testIdx)

-- | Time-series forward-chaining split. Fold @i@ uses the first
-- @initial + i × step@ samples for train and the next @step@ for test.
-- Useful for evaluating models on time-ordered data.
timeSeriesSplit
  :: Int            -- ^ Initial training set size.
  -> Int            -- ^ Step size (samples per test fold).
  -> Int            -- ^ Total samples.
  -> [Fold]
timeSeriesSplit initial step n =
  [ ([0 .. initial + (i - 1) * step - 1],
     [initial + (i - 1) * step .. initial + i * step - 1])
  | i <- [1 .. (n - initial) `div` step]
  ]

-- ---------------------------------------------------------------------------
-- Cross-validation
-- ---------------------------------------------------------------------------

-- | Run a fit / score loop over folds. Returns a score per fold.
--
-- The user provides:
--
--   * a function that takes (trainIdx, testIdx) and the dataset, fits
--     a model on the train indices, and returns predictions on the
--     test indices,
--   * a score function that compares true and predicted values.
--
-- For type generality the dataset and predictions are user-defined.
crossValidate
  :: [Fold]
  -> (([Int], [Int]) -> data_ -> IO pred_)  -- ^ fit + predict
  -> (data_ -> [Int] -> pred_ -> IO Double) -- ^ scoring fn (true vs pred)
  -> data_
  -> IO [Double]
crossValidate folds fitPredict scoreFn d =
  forM folds $ \fold@(_train, testIdx) -> do
    pred_ <- fitPredict fold d
    scoreFn d testIdx pred_

-- | Convenience: returns @(mean, std)@ of fold scores.
crossValidateScores
  :: [Fold]
  -> (([Int], [Int]) -> data_ -> IO pred_)
  -> (data_ -> [Int] -> pred_ -> IO Double)
  -> data_
  -> IO (Double, Double)
crossValidateScores folds fp sf d = do
  scores <- crossValidate folds fp sf d
  let n     = fromIntegral (length scores) :: Double
      mean  = sum scores / n
      var   = sum [(s - mean) ^ (2 :: Int) | s <- scores]
              / max 1 (n - 1)
  pure (mean, sqrt var)

-- ---------------------------------------------------------------------------
-- Grid search
-- ---------------------------------------------------------------------------

-- | Result of a grid search.
data GridSearchResult hp = GridSearchResult
  { gsBestParams :: hp
  , gsBestScore  :: !Double
  , gsAllResults :: ![(hp, Double, Double)]
    -- ^ (params, mean score, std of fold scores) for each grid point.
  } deriving (Show)

-- | Grid search over hyperparameters with k-fold CV. The user
-- provides:
--
--   * the list of HP values to try
--   * a function to fit/predict given an HP and a fold
--   * a scoring function (higher = better)
--
-- Returns the best HP plus full grid results.
gridSearchCV
  :: [Fold]
  -> [hp]                                              -- ^ HP grid
  -> (hp -> ([Int], [Int]) -> data_ -> IO pred_)       -- ^ fit/predict
  -> (data_ -> [Int] -> pred_ -> IO Double)            -- ^ score
  -> data_
  -> IO (GridSearchResult hp)
gridSearchCV folds grid fp sf d = do
  results <- forM grid $ \hp -> do
    (mean, std) <- crossValidateScores folds (fp hp) sf d
    pure (hp, mean, std)
  let (bestHp, bestScore, _) = head (sortBy (comparing (\(_, s, _) -> negate s)) results)
  pure GridSearchResult
    { gsBestParams = bestHp
    , gsBestScore  = bestScore
    , gsAllResults = results
    }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

allIdx :: Int -> [Int]
allIdx n = [0 .. n - 1]

-- | Fisher-Yates shuffle producing a list of indices.
shuffleIndices :: Int -> MWC.GenIO -> IO [Int]
shuffleIndices n gen = do
  v <- V.thaw (V.fromList [0 .. n - 1])
  forM_ [n - 1, n - 2 .. 1] $ \i -> do
    j <- MWC.uniformR (0, i) gen
    a <- VM.read v i
    b <- VM.read v j
    VM.write v i b
    VM.write v j a
  V.toList <$> V.freeze v

-- | Shuffle an arbitrary list.
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

