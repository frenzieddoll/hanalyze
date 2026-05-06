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
--
-- /Performance/: the primary fit API is now 'fitDTV', which takes a
-- contiguous 'LA.Matrix' of features and an unboxed 'VU.Vector' of
-- labels. The classic 'fitDT' over @[[Double]]@ / @[Int]@ is preserved
-- as a backwards-compatible wrapper that converts at the boundary.
-- The internal representation keeps a single shared feature matrix
-- and recurses on row-index permutations, so building a tree is
-- @O(p · n log n · depth)@ rather than the old @O(p · n² · depth)@.
module Model.DecisionTree
  ( -- * Tree types
    DTree (..)
  , DTConfig (..)
  , defaultDTConfig
    -- * Fit / predict
  , fitDT
  , fitDTV
  , predictDT
  , predictDTProbs
    -- * Helpers
  , giniImpurity
  ) where

import qualified Data.Map.Strict             as Map
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Numeric.LinearAlgebra       as LA
import           Control.Monad.ST            (runST)
import           Data.List                   (foldl')

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Classification decision tree node.
data DTree
  = DLeaf
      { dlClassProbs :: !(Map.Map Int Double)
      , dlMajority   :: !Int
      }
  | DNode
      { dnFeature :: !Int
      , dnThr     :: !Double
      , dnLeft    :: !DTree
      , dnRight   :: !DTree
      }
  deriving (Show)

-- | Decision tree configuration.
data DTConfig = DTConfig
  { dtMaxDepth        :: !(Maybe Int)
  , dtMinSamplesSplit :: !Int
  , dtMinSamplesLeaf  :: !Int
  , dtMinImpurity     :: !Double
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
-- Fit (Vector-based primary API)
-- ---------------------------------------------------------------------------

-- | Fit a decision tree from a row-major feature matrix and unboxed
-- label vector. This is the high-performance path; 'fitDT' is a
-- list-based backwards-compatibility wrapper.
fitDTV :: DTConfig -> LA.Matrix Double -> VU.Vector Int -> DTree
fitDTV cfg x y =
  let !n   = VU.length y
      !idx = VU.enumFromN 0 n
  in buildNodeV cfg x y idx 0

-- | Backwards-compatible list-based fit.
fitDT :: DTConfig -> [[Double]] -> [Int] -> DTree
fitDT cfg xs ys
  | null xs   = DLeaf Map.empty 0
  | otherwise = fitDTV cfg (LA.fromLists xs) (VU.fromList ys)

-- ---------------------------------------------------------------------------
-- Recursive build over row-index permutations
-- ---------------------------------------------------------------------------

buildNodeV
  :: DTConfig
  -> LA.Matrix Double      -- ^ Shared feature matrix (n × p).
  -> VU.Vector Int         -- ^ Shared label vector (length n).
  -> VU.Vector Int         -- ^ Row indices in this subtree.
  -> Int                   -- ^ Current depth.
  -> DTree
buildNodeV cfg x y idx depth =
  let !nIdx     = VU.length idx
      !sublabs  = VU.map (y VU.!) idx
      !probs    = classProbsV sublabs
      !majority = case sortByValDescV probs of
                    ((c, _) : _) -> c
                    []           -> 0
      leaf      = DLeaf probs majority

      depthLimit = case dtMaxDepth cfg of
                     Just d  -> depth >= d
                     Nothing -> False
      stop = depthLimit
          || nIdx < dtMinSamplesSplit cfg
          || giniFromCounts probs < dtMinImpurity cfg
          || allSameV sublabs
  in if stop
       then leaf
       else case bestSplitV cfg x y idx of
         Nothing -> leaf
         Just (fIdx, thr, _gain) ->
           let (lIdx, rIdx) = partitionVIdx x idx fIdx thr
           in if VU.length lIdx < dtMinSamplesLeaf cfg
                || VU.length rIdx < dtMinSamplesLeaf cfg
                then leaf
                else DNode
                       { dnFeature = fIdx
                       , dnThr     = thr
                       , dnLeft    = buildNodeV cfg x y lIdx (depth + 1)
                       , dnRight   = buildNodeV cfg x y rIdx (depth + 1)
                       }

-- | Partition row indices by a feature threshold.
partitionVIdx
  :: LA.Matrix Double
  -> VU.Vector Int
  -> Int
  -> Double
  -> (VU.Vector Int, VU.Vector Int)
partitionVIdx x idx feat thr =
  let pred_ i = LA.atIndex x (i, feat) <= thr
  in VU.partition pred_ idx

-- ---------------------------------------------------------------------------
-- Class probabilities and Gini on subsets
-- ---------------------------------------------------------------------------

-- | Class probability map (class → fraction).
classProbsV :: VU.Vector Int -> Map.Map Int Double
classProbsV ys =
  let !n     = fromIntegral (VU.length ys) :: Double
      counts = VU.foldl'
                 (\m c -> Map.insertWith (+) c (1 :: Double) m)
                 Map.empty ys
  in Map.map (/ n) counts

allSameV :: VU.Vector Int -> Bool
allSameV ys
  | VU.null ys = True
  | otherwise  =
      let !y0 = VU.unsafeHead ys
      in VU.all (== y0) (VU.unsafeTail ys)

giniFromCounts :: Map.Map Int Double -> Double
giniFromCounts ps = 1 - foldl' (\acc p -> acc + p * p) 0 (Map.elems ps)

-- | Backwards-compatible Gini on @[Int]@.
giniImpurity :: [Int] -> Double
giniImpurity []  = 0
giniImpurity ys  =
  let !n     = fromIntegral (length ys) :: Double
      counts = foldl' (\m c -> Map.insertWith (+) c (1 :: Double) m)
                      Map.empty ys
  in 1 - foldl' (\acc c -> acc + (c / n) ^ (2 :: Int)) 0 (Map.elems counts)

sortByValDescV :: Map.Map Int Double -> [(Int, Double)]
sortByValDescV =
  -- Map.toList is ascending key; we want descending by value.
  reverse . sortByVal . Map.toList
  where
    sortByVal = foldr ins []
    ins p []     = [p]
    ins p (q:qs) = if snd p > snd q then p : q : qs else q : ins p qs

-- ---------------------------------------------------------------------------
-- Best split: per-feature O(n log n) sweep with running counts
-- ---------------------------------------------------------------------------

bestSplitV
  :: DTConfig
  -> LA.Matrix Double
  -> VU.Vector Int
  -> VU.Vector Int
  -> Maybe (Int, Double, Double)
bestSplitV _cfg x y idx
  | VU.length idx < 2 = Nothing
  | otherwise =
      let !p = LA.cols x
          best = foldr step Nothing [0 .. p - 1]
          step i acc =
            case bestSplitFeature x y idx i of
              Nothing       -> acc
              Just (thr, g) ->
                case acc of
                  Nothing                          -> Just (i, thr, g)
                  Just (_, _, gPrev) | g > gPrev   -> Just (i, thr, g)
                                     | otherwise   -> acc
      in best

-- | Per-feature best split on the index subset. Returns @Just (thr,
-- gain)@ where @gain@ is the impurity reduction (parent − weighted
-- children); negative or zero means no useful split was found.
bestSplitFeature
  :: LA.Matrix Double
  -> VU.Vector Int
  -> VU.Vector Int
  -> Int
  -> Maybe (Double, Double)
bestSplitFeature x y idx feat = runST $ do
  let !n = VU.length idx
  -- Build (value, label) pairs for this subset and sort by value.
  let valOf i = LA.atIndex x (i, feat)
      lab i   = y VU.! i
  pairs <- VUM.new n
  let fill !k
        | k == n = pure ()
        | otherwise = do
            let !i = VU.unsafeIndex idx k
            VUM.unsafeWrite pairs k (valOf i, lab i)
            fill (k + 1)
  fill 0
  Intro.sortBy (\a b -> compare (fst a) (fst b)) pairs
  pairsF <- VU.unsafeFreeze pairs

  -- Determine the number of distinct classes within this subset.
  let labels = VU.map snd pairsF
  let !numClasses = 1 + VU.maximum labels  -- labels are non-negative

  -- Right counts start with all labels.
  rightCounts <- VUM.replicate numClasses (0 :: Int)
  let initRight !k
        | k == n = pure ()
        | otherwise = do
            let !c = VU.unsafeIndex labels k
            old <- VUM.unsafeRead rightCounts c
            VUM.unsafeWrite rightCounts c (old + 1)
            initRight (k + 1)
  initRight 0
  leftCounts <- VUM.replicate numClasses (0 :: Int)

  let parentImp = giniFromIntCountsRO numClasses (VU.toList (VU.map snd pairsF))

  -- Sweep through sorted pairs, moving sample i to the left side and
  -- evaluating split between i and i+1 only when value changes.
  let sweep !k !bestThr !bestGain
        | k >= n - 1 = pure (bestThr, bestGain)
        | otherwise = do
            let (v_k, c_k)  = VU.unsafeIndex pairsF k
                (v_k1, _)   = VU.unsafeIndex pairsF (k + 1)
            -- Move sample k to left.
            lOld <- VUM.unsafeRead leftCounts c_k
            VUM.unsafeWrite leftCounts c_k (lOld + 1)
            rOld <- VUM.unsafeRead rightCounts c_k
            VUM.unsafeWrite rightCounts c_k (rOld - 1)
            -- Skip threshold if values equal — splitting equal
            -- samples is meaningless.
            if v_k == v_k1
              then sweep (k + 1) bestThr bestGain
              else do
                let !thr = (v_k + v_k1) / 2
                    !nL  = k + 1
                    !nR  = n - nL
                gL <- giniMutable leftCounts  numClasses nL
                gR <- giniMutable rightCounts numClasses nR
                let !nD    = fromIntegral n :: Double
                    !child = (fromIntegral nL * gL + fromIntegral nR * gR) / nD
                    !gain  = parentImp - child
                if gain > bestGain
                  then sweep (k + 1) thr  gain
                  else sweep (k + 1) bestThr bestGain
  (thr, gain) <- sweep 0 0 (negate (1.0 / 0.0))
  pure $ if gain == negate (1.0 / 0.0)
           then Nothing
           else Just (thr, gain)
  where
    -- Compute Gini from a mutable Int counts vector + total n.
    giniMutable counts numClasses nTot
      | nTot == 0 = pure 0
      | otherwise = do
          let !nD = fromIntegral nTot :: Double
              loop !i !acc
                | i == numClasses = pure (1 - acc)
                | otherwise = do
                    c <- VUM.unsafeRead counts i
                    let !p = fromIntegral c / nD
                    loop (i + 1) (acc + p * p)
          loop 0 0

-- | Read-only Gini from a list of class labels (used once per node
-- for the parent impurity baseline).
giniFromIntCountsRO :: Int -> [Int] -> Double
giniFromIntCountsRO numClasses labels =
  let !n = fromIntegral (length labels) :: Double
      counts = foldl' (\m c -> Map.insertWith (+) c (1 :: Double) m)
                      Map.empty labels
      _ = numClasses  -- silence unused
  in 1 - sum [ (c / n) ^ (2 :: Int) | c <- Map.elems counts ]

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

-- Silence unused-import warning for V (keeps import slot for future
-- variants without re-touching imports).
_unused :: V.Vector Int -> Int
_unused = V.length
