{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | Decision tree classifier (CART, classification).
--
-- Pairs with the existing regression-oriented 'Model.RandomForest';
-- this module focuses on classification. Splits use Gini impurity as
-- the criterion (matches sklearn default).
--
-- @
-- import Model.DecisionTree
--
-- let cfg  = defaultDTConfig
--     tree = fitDT cfg xs ys           -- xs :: [[Double]], ys :: [Int]
--     yhat = map (predictDT tree) xs
-- @
module Model.DecisionTree
  ( -- * Tree types
    DTree (..)
  , DTConfig (..)
  , defaultDTConfig
    -- * Fit / predict
  , fitDT
  , predictDT
  , predictDTProbs
    -- * Helpers
  , giniImpurity
  ) where

import qualified Data.Map.Strict as Map
import           Data.List       (sortBy, group, foldl')
import           Data.Ord        (comparing)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Classification decision tree node.
data DTree
  = DLeaf
      { dlClassProbs :: !(Map.Map Int Double)
        -- ^ Predicted class probabilities at this leaf.
      , dlMajority   :: !Int
        -- ^ Majority class (argmax of probs).
      }
  | DNode
      { dnFeature :: !Int     -- ^ Feature index for the split.
      , dnThr     :: !Double  -- ^ Threshold (left = ≤, right = >).
      , dnLeft    :: !DTree
      , dnRight   :: !DTree
      }
  deriving (Show)

-- | Decision tree configuration.
data DTConfig = DTConfig
  { dtMaxDepth     :: !(Maybe Int)
    -- ^ Maximum tree depth. 'Nothing' = unlimited.
  , dtMinSamplesSplit :: !Int
    -- ^ Minimum samples in a node to split (≥ 2).
  , dtMinSamplesLeaf  :: !Int
    -- ^ Minimum samples allowed in any leaf.
  , dtMinImpurity     :: !Double
    -- ^ Minimum Gini impurity to consider splitting (else leaf).
  } deriving (Show, Eq)

-- | Defaults (sklearn-compatible): unlimited depth, min split 2,
-- min leaf 1, min impurity 0.
defaultDTConfig :: DTConfig
defaultDTConfig = DTConfig
  { dtMaxDepth        = Nothing
  , dtMinSamplesSplit = 2
  , dtMinSamplesLeaf  = 1
  , dtMinImpurity     = 0
  }

-- ---------------------------------------------------------------------------
-- Fit
-- ---------------------------------------------------------------------------

-- | Fit a decision tree classifier.
fitDT
  :: DTConfig
  -> [[Double]]   -- ^ Features (n samples, p features).
  -> [Int]        -- ^ Class labels.
  -> DTree
fitDT cfg xs ys = buildNode cfg xs ys 0

-- | Build a node recursively, applying split criteria.
buildNode :: DTConfig -> [[Double]] -> [Int] -> Int -> DTree
buildNode cfg xs ys depth =
  let n      = length ys
      probs  = classProbs ys
      majority = case sortBy (comparing (negate . snd)) (Map.toList probs) of
                   ((c, _):_) -> c
                   []         -> 0
      leaf = DLeaf probs majority
      stop = case dtMaxDepth cfg of
               Just d  -> depth >= d
               Nothing -> False
            || n < dtMinSamplesSplit cfg
            || giniImpurity ys < dtMinImpurity cfg
            || allSame ys
  in if stop then leaf
       else case bestSplit cfg xs ys of
         Nothing -> leaf
         Just (fIdx, thr, _gainImp) ->
           let (lXs, lYs, rXs, rYs) = partitionAt fIdx thr xs ys
           in if length lYs < dtMinSamplesLeaf cfg
                || length rYs < dtMinSamplesLeaf cfg
                then leaf
                else DNode
                       { dnFeature = fIdx
                       , dnThr     = thr
                       , dnLeft    = buildNode cfg lXs lYs (depth + 1)
                       , dnRight   = buildNode cfg rXs rYs (depth + 1)
                       }

-- | Class probability map (class → fraction).
classProbs :: [Int] -> Map.Map Int Double
classProbs ys =
  let n      = fromIntegral (length ys) :: Double
      counts = Map.fromListWith (+) [(y, 1 :: Double) | y <- ys]
  in Map.map (/ n) counts

-- | All samples in same class?
allSame :: [Int] -> Bool
allSame []     = True
allSame (y:ys) = all (== y) ys

-- | Find best split across all features and thresholds.
bestSplit :: DTConfig -> [[Double]] -> [Int] -> Maybe (Int, Double, Double)
bestSplit _cfg xs ys
  | null xs       = Nothing
  | length ys < 2 = Nothing
  | otherwise =
      let p           = length (head xs)
          parentImp   = giniImpurity ys
          n           = fromIntegral (length ys) :: Double
          tryFeature i =
            let -- Sort samples by feature i.
                sorted = sortBy (comparing fst)
                                [(x !! i, y) | (x, y) <- zip xs ys]
                vals = map fst sorted
                -- Candidate thresholds: midpoints of consecutive
                -- distinct values.
                distinct = uniq vals
                thrs     = [ (a + b) / 2
                           | (a, b) <- zip distinct (drop 1 distinct) ]
                evalThr thr =
                  let (lYs, rYs) = (
                        [y | (v, y) <- sorted, v <= thr],
                        [y | (v, y) <- sorted, v >  thr])
                      lN = fromIntegral (length lYs) :: Double
                      rN = fromIntegral (length rYs) :: Double
                      childImp = (lN * giniImpurity lYs
                                + rN * giniImpurity rYs) / n
                      gain = parentImp - childImp
                  in (thr, gain)
            in [(i, thr, gain) | thr <- thrs, let (_, gain) = evalThr thr,
                                  gain >= -1e-10]
          candidates = concat [tryFeature i | i <- [0 .. p - 1]]
      in if null candidates
           then Nothing
           else
             let best = foldl1Max candidates
                 -- XOR-like: gain may be 0 yet deeper splits help.
                 -- Allow tie-breaking via feature index but require
                 -- the split actually partitions both sides non-empty.
             in Just best
  where
    foldl1Max = foldr1 (\(i1, t1, g1) (i2, t2, g2) ->
                          if g1 >= g2 then (i1, t1, g1) else (i2, t2, g2))

uniq :: Eq a => [a] -> [a]
uniq = map head . group

-- | Partition (xs, ys) into (left ≤ thr, right > thr) on feature i.
partitionAt :: Int -> Double -> [[Double]] -> [Int]
            -> ([[Double]], [Int], [[Double]], [Int])
partitionAt fIdx thr xs ys =
  let pairs = zip xs ys
      (lp, rp) = (
        [(x, y) | (x, y) <- pairs, x !! fIdx <= thr],
        [(x, y) | (x, y) <- pairs, x !! fIdx >  thr])
  in (map fst lp, map snd lp, map fst rp, map snd rp)

-- ---------------------------------------------------------------------------
-- Predict
-- ---------------------------------------------------------------------------

-- | Predict the majority class label for one sample.
predictDT :: DTree -> [Double] -> Int
predictDT (DLeaf _ m) _ = m
predictDT (DNode i thr l r) x
  | x !! i <= thr = predictDT l x
  | otherwise     = predictDT r x

-- | Predict class probabilities for one sample.
predictDTProbs :: DTree -> [Double] -> Map.Map Int Double
predictDTProbs (DLeaf p _) _ = p
predictDTProbs (DNode i thr l r) x
  | x !! i <= thr = predictDTProbs l x
  | otherwise     = predictDTProbs r x

-- ---------------------------------------------------------------------------
-- Impurity
-- ---------------------------------------------------------------------------

-- | Gini impurity: @1 − Σ p_i²@.
giniImpurity :: [Int] -> Double
giniImpurity []  = 0
giniImpurity ys  =
  let n      = fromIntegral (length ys) :: Double
      counts = foldl' (\m y -> Map.insertWith (+) y (1 :: Double) m)
                      Map.empty ys
      ps     = map (/ n) (Map.elems counts)
  in 1 - sum [p * p | p <- ps]

