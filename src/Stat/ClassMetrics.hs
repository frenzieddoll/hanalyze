{-# LANGUAGE OverloadedStrings #-}
-- | Classification model evaluation metrics.
--
-- Two families:
--
--   * __Hard predictions__ (predicted class labels): 'confusionMatrix',
--     'accuracy', 'precision', 'recall', 'f1Score', 'fBetaScore'.
--   * __Soft predictions__ (predicted probabilities): 'rocCurve',
--     'auc', 'prCurve', 'averagePrecision', 'logLoss',
--     'brierScore'.
--
-- Multi-class extensions: 'macroAvg', 'weightedAvg'. Binary helpers
-- assume class labels @0@ / @1@ (negative / positive).
module Stat.ClassMetrics
  ( -- * Confusion matrix (binary)
    Confusion (..)
  , confusionMatrix
    -- * Hard-prediction metrics
  , accuracy
  , precision
  , recall
  , specificity
  , f1Score
  , fBetaScore
  , balancedAccuracy
  , matthewsCorr
    -- * Soft-prediction metrics
  , rocCurve
  , auc
  , prCurve
  , averagePrecision
  , logLoss
  , brierScore
    -- * Multi-class confusion
  , ConfusionMulti (..)
  , confusionMulti
  , accuracyMulti
  , macroF1
  , weightedF1
  ) where

import qualified Data.Map.Strict       as Map
import           Data.List             (sort, sortBy)
import           Data.Ord              (comparing, Down (..))

-- ---------------------------------------------------------------------------
-- Binary confusion matrix
-- ---------------------------------------------------------------------------

-- | 2×2 confusion matrix for binary classification (labels @0@/@1@).
--
-- @
--                Predicted
--               ┌─────┬─────┐
--               │  0  │  1  │
--      ┌────┬───┼─────┼─────┤
-- True │  0 │   │ TN  │ FP  │
--      │  1 │   │ FN  │ TP  │
--      └────┴───┴─────┴─────┘
-- @
data Confusion = Confusion
  { confTP :: !Int
  , confFP :: !Int
  , confFN :: !Int
  , confTN :: !Int
  } deriving (Show, Eq)

-- | Build a binary confusion matrix from true / predicted label vectors
-- (both 0/1).
confusionMatrix
  :: [Int]   -- ^ True labels.
  -> [Int]   -- ^ Predicted labels.
  -> Confusion
confusionMatrix ys yhats =
  let pairs = zip ys yhats
      tp    = length [() | (1, 1) <- pairs]
      fp    = length [() | (0, 1) <- pairs]
      fn    = length [() | (1, 0) <- pairs]
      tn    = length [() | (0, 0) <- pairs]
  in Confusion tp fp fn tn

-- ---------------------------------------------------------------------------
-- Hard-prediction metrics (binary)
-- ---------------------------------------------------------------------------

-- | Overall accuracy: @(TP + TN) / total@.
accuracy :: Confusion -> Double
accuracy c =
  let n = confTP c + confFP c + confFN c + confTN c
  in if n == 0 then 0
       else fromIntegral (confTP c + confTN c) / fromIntegral n

-- | Precision: @TP / (TP + FP)@. The "purity" of positive predictions.
precision :: Confusion -> Double
precision c =
  let denom = confTP c + confFP c
  in if denom == 0 then 0 else fromIntegral (confTP c) / fromIntegral denom

-- | Recall (sensitivity, TPR): @TP / (TP + FN)@.
recall :: Confusion -> Double
recall c =
  let denom = confTP c + confFN c
  in if denom == 0 then 0 else fromIntegral (confTP c) / fromIntegral denom

-- | Specificity (TNR): @TN / (TN + FP)@.
specificity :: Confusion -> Double
specificity c =
  let denom = confTN c + confFP c
  in if denom == 0 then 0 else fromIntegral (confTN c) / fromIntegral denom

-- | F1: harmonic mean of precision and recall.
f1Score :: Confusion -> Double
f1Score c =
  let p = precision c
      r = recall c
  in if p + r == 0 then 0 else 2 * p * r / (p + r)

-- | F-beta: weighted harmonic mean. @β > 1@ favours recall, @β < 1@
-- favours precision.
fBetaScore :: Double -> Confusion -> Double
fBetaScore beta c =
  let p   = precision c
      r   = recall c
      b2  = beta * beta
      num = (1 + b2) * p * r
      den = b2 * p + r
  in if den == 0 then 0 else num / den

-- | Balanced accuracy: @(sensitivity + specificity) / 2@. Robust to
-- class imbalance.
balancedAccuracy :: Confusion -> Double
balancedAccuracy c = (recall c + specificity c) / 2

-- | Matthews correlation coefficient (MCC) — robust binary metric in
-- @[-1, 1]@.
matthewsCorr :: Confusion -> Double
matthewsCorr c =
  let tp = fromIntegral (confTP c) :: Double
      fp = fromIntegral (confFP c) :: Double
      fn = fromIntegral (confFN c) :: Double
      tn = fromIntegral (confTN c) :: Double
      num = tp * tn - fp * fn
      den = sqrt ((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  in if den == 0 then 0 else num / den

-- ---------------------------------------------------------------------------
-- Soft-prediction metrics
-- ---------------------------------------------------------------------------

-- | ROC curve: list of @(FPR, TPR)@ points. Sorted by descending
-- score threshold; starts at @(0, 0)@ and ends at @(1, 1)@.
rocCurve
  :: [Int]      -- ^ True labels (0/1).
  -> [Double]   -- ^ Predicted scores (higher = more positive).
  -> [(Double, Double)]
rocCurve ys scores =
  let pairs   = sortBy (comparing (Down . snd)) (zip ys scores)
      pos     = length [y | (y, _) <- pairs, y == 1]
      neg     = length [y | (y, _) <- pairs, y == 0]
      go _ _ tp fp [] = [(fromIntegral fp / fromIntegral (max 1 neg),
                          fromIntegral tp / fromIntegral (max 1 pos))]
      go prev acc tp fp ((y, s):rest)
        | s == prev =
            go prev acc (if y == 1 then tp + 1 else tp)
                       (if y == 0 then fp + 1 else fp) rest
        | otherwise =
            let pt = (fromIntegral fp / fromIntegral (max 1 neg),
                      fromIntegral tp / fromIntegral (max 1 pos))
            in pt : go s acc (if y == 1 then tp + 1 else tp)
                              (if y == 0 then fp + 1 else fp) rest
      curve = (0, 0) : go (1/0) [] 0 0 pairs
  in curve

-- | Area under ROC curve (trapezoidal integration).
auc :: [Int] -> [Double] -> Double
auc ys scores =
  let pts = rocCurve ys scores
      ps  = sort pts  -- sort by FPR ascending
      go []       _      acc = acc
      go [_]      _      acc = acc
      go ((x1, y1):(x2, y2):rest) prev acc =
        go ((x2, y2):rest) (x2, y2)
          (acc + (x2 - x1) * (y1 + y2) / 2)
      _ = prev
      prev = (0, 0)
  in go ps (0, 0) 0

-- | Precision–recall curve as @(recall, precision)@ pairs, sorted by
-- recall ascending.
prCurve :: [Int] -> [Double] -> [(Double, Double)]
prCurve ys scores =
  let pairs = sortBy (comparing (Down . snd)) (zip ys scores)
      pos   = length [y | (y, _) <- pairs, y == 1]
      go tp fp [] = [(fromIntegral tp / fromIntegral (max 1 pos),
                      if tp + fp == 0 then 1
                        else fromIntegral tp / fromIntegral (tp + fp))]
      go tp fp ((y, _):rest) =
        let tp' = if y == 1 then tp + 1 else tp
            fp' = if y == 0 then fp + 1 else fp
            r   = fromIntegral tp' / fromIntegral (max 1 pos)
            p   = if tp' + fp' == 0 then 1
                    else fromIntegral tp' / fromIntegral (tp' + fp')
        in (r, p) : go tp' fp' rest
  in (0, 1) : go 0 0 pairs

-- | Average precision (area under PR curve via step-wise integration).
averagePrecision :: [Int] -> [Double] -> Double
averagePrecision ys scores =
  let pairs = sortBy (comparing (Down . snd)) (zip ys scores)
      pos   = length [y | (y, _) <- pairs, y == 1]
      go _ _ _ [] = 0
      go tp _fp prevR ((y, _):rest) =
        let tp' = if y == 1 then tp + 1 else tp
            fp' = if y == 0 then 0 else 0  -- fp not used in formula
            _ = fp'
            r   = fromIntegral tp' / fromIntegral (max 1 pos)
            p   = fromIntegral tp' / fromIntegral (max 1 (length pairs
                                                          - length rest))
            inc = if y == 1 then (r - prevR) * p else 0
        in inc + go tp' 0 r rest
  in go 0 0 0 pairs

-- | Logarithmic loss (cross-entropy).
-- Clipped to @[1e-15, 1 − 1e-15]@ to avoid log(0).
logLoss :: [Int] -> [Double] -> Double
logLoss ys probs =
  let n     = fromIntegral (length ys) :: Double
      clip x = max 1e-15 (min (1 - 1e-15) x)
      lossOf y p =
        let p' = clip p
        in fromIntegral y * log p' + (1 - fromIntegral y) * log (1 - p')
      total = sum (zipWith lossOf ys probs)
  in - total / n

-- | Brier score: mean squared error between predicted probabilities
-- and true labels.
brierScore :: [Int] -> [Double] -> Double
brierScore ys probs =
  let n     = fromIntegral (length ys) :: Double
      total = sum [ (p - fromIntegral y) ^ (2 :: Int)
                  | (y, p) <- zip ys probs ]
  in total / n

-- ---------------------------------------------------------------------------
-- Multi-class
-- ---------------------------------------------------------------------------

-- | Multi-class confusion matrix as a Map (true, pred) -> count.
data ConfusionMulti = ConfusionMulti
  { cmCounts :: !(Map.Map (Int, Int) Int)
  , cmLabels :: ![Int]
  } deriving (Show)

-- | Build a multi-class confusion matrix from labels.
confusionMulti :: [Int] -> [Int] -> ConfusionMulti
confusionMulti ys yhats =
  let labels = sort (Map.keys (Map.fromList [(y, ()) | y <- ys ++ yhats]))
      pairs  = zip ys yhats
      countOf k = Map.fromListWith (+) [(p, 1::Int) | p <- pairs, p == k]
      _ = countOf
      counts = Map.fromListWith (+) [(p, 1::Int) | p <- pairs]
  in ConfusionMulti counts labels

-- | Multi-class overall accuracy.
accuracyMulti :: ConfusionMulti -> Double
accuracyMulti cm =
  let total    = sum (Map.elems (cmCounts cm))
      diagonal = sum [ Map.findWithDefault 0 (l, l) (cmCounts cm)
                     | l <- cmLabels cm ]
  in if total == 0 then 0
       else fromIntegral diagonal / fromIntegral total

-- | Per-class precision / recall as a binary one-vs-rest task.
classBinary :: ConfusionMulti -> Int -> Confusion
classBinary cm c =
  let counts = cmCounts cm
      tp = Map.findWithDefault 0 (c, c) counts
      fp = sum [ Map.findWithDefault 0 (t, c) counts
               | t <- cmLabels cm, t /= c ]
      fn = sum [ Map.findWithDefault 0 (c, p) counts
               | p <- cmLabels cm, p /= c ]
      tn = sum (Map.elems counts) - tp - fp - fn
  in Confusion tp fp fn tn

-- | Macro-averaged F1 (mean of per-class F1s, equal weight).
macroF1 :: ConfusionMulti -> Double
macroF1 cm =
  let f1s = [ f1Score (classBinary cm c) | c <- cmLabels cm ]
      n   = fromIntegral (length f1s) :: Double
  in if n == 0 then 0 else sum f1s / n

-- | Weighted-averaged F1 (weighted by class support).
weightedF1 :: ConfusionMulti -> Double
weightedF1 cm =
  let counts = cmCounts cm
      total  = fromIntegral (sum (Map.elems counts)) :: Double
      perClass = [ let cb = classBinary cm c
                       sup = fromIntegral (sum [ Map.findWithDefault 0 (c, p) counts
                                                | p <- cmLabels cm ]) :: Double
                   in sup * f1Score cb
                 | c <- cmLabels cm ]
  in if total == 0 then 0 else sum perClass / total

-- ---------------------------------------------------------------------------
-- Helpers (suppress unused warnings from internal stuff)
-- ---------------------------------------------------------------------------

