{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | Random forest for regression (CART + bagging + random feature subset).
--
-- /Performance/: this module was ported in B9b from a list-based
-- implementation to a row-index permutation scheme, mirroring the
-- 'Hanalyze.Model.DecisionTree' refactor:
--
--   * Single shared @LA.Matrix Double@ feature matrix.
--   * @VU.Vector Int@ row indices recurse through subtrees.
--   * Per-feature best split via 'Data.Vector.Algorithms.Intro' sort
--     and incremental sum / sum-of-squares sweep.
--   * Bootstrap = random index Vector (no row data copied).
--
-- The classic 'fitRF' over @[[Double]] / [Double]@ is preserved as a
-- backwards-compatibility wrapper that calls 'fitRFV'.
module Hanalyze.Model.RandomForest
  ( -- * Single regression tree
    Tree (..)
  , RFConfig (..)
  , defaultRFConfig
  , buildTree
  , predictTree
    -- * Forest
  , RandomForest (..)
  , fitRF
  , fitRFV
  , predictRF
  , featureImportance
  ) where

import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as VU
import qualified Data.Vector.Unboxed.Mutable  as VUM
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Numeric.LinearAlgebra        as LA
import qualified System.Random.MWC            as MWC
import           Control.Monad                (replicateM)
import           Control.Monad.ST             (runST)
import           Data.IORef                   (IORef, newIORef, readIORef,
                                               modifyIORef')

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A regression tree node.
data Tree
  = Leaf !Double
  | Node !Int !Double !Tree !Tree
  deriving (Show)

-- | Random-forest configuration.
data RFConfig = RFConfig
  { rfTrees      :: !Int
  , rfMaxDepth   :: !Int
  , rfMinSamples :: !Int
  , rfMtry       :: !(Maybe Int)
  , rfBootstrap  :: !Bool
  } deriving (Show)

defaultRFConfig :: RFConfig
defaultRFConfig = RFConfig
  { rfTrees      = 100
  , rfMaxDepth   = 12
  , rfMinSamples = 3
  , rfMtry       = Nothing
  , rfBootstrap  = True
  }

data RandomForest = RandomForest
  { rfTreesV     :: ![Tree]
  , rfNFeatures  :: !Int
  , rfImportance :: !(V.Vector Double)
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Vector-based fit (primary)
-- ---------------------------------------------------------------------------

fitRFV :: RFConfig
       -> LA.Matrix Double
       -> VU.Vector Double
       -> MWC.GenIO
       -> IO RandomForest
fitRFV cfg x y gen = do
  let !n = VU.length y
      !d = LA.cols x
  impRef <- newIORef (V.replicate d 0.0)
  trees <- replicateM (rfTrees cfg) $ do
    !idx <- if rfBootstrap cfg
              then bootstrapIdx n gen
              else pure (VU.enumFromN 0 n)
    let !t = buildTreeV cfg x y idx 0
    accumulateImportance impRef t
    pure t
  imp <- readIORef impRef
  pure RandomForest
    { rfTreesV     = trees
    , rfNFeatures  = d
    , rfImportance = imp
    }

-- | Backwards-compatible list-based fit.
fitRF :: RFConfig -> [[Double]] -> [Double] -> MWC.GenIO -> IO RandomForest
fitRF cfg xs ys gen
  | null xs   = pure (RandomForest [] 0 V.empty)
  | otherwise = fitRFV cfg (LA.fromLists xs) (VU.fromList ys) gen

-- | Single-tree builder kept for the symmetry of the old API. Most
-- callers should use 'fitRFV'.
buildTree :: RFConfig -> [[Double]] -> [Double] -> MWC.GenIO -> IO Tree
buildTree cfg rows ys gen
  | null rows = pure (Leaf 0)
  | otherwise = do
      let !x = LA.fromLists rows
          !y = VU.fromList ys
          !n = VU.length y
      idx <- if rfBootstrap cfg
               then bootstrapIdx n gen
               else pure (VU.enumFromN 0 n)
      pure (buildTreeV cfg x y idx 0)

bootstrapIdx :: Int -> MWC.GenIO -> IO (VU.Vector Int)
bootstrapIdx n gen =
  VU.replicateM n (MWC.uniformR (0, n - 1) gen)

-- ---------------------------------------------------------------------------
-- Recursive build
-- ---------------------------------------------------------------------------

buildTreeV :: RFConfig
           -> LA.Matrix Double
           -> VU.Vector Double
           -> VU.Vector Int
           -> Int
           -> Tree
buildTreeV cfg x y idx depth =
  let !n      = VU.length idx
      !subY   = VU.map (y VU.!) idx
      !meanY  = if n == 0 then 0
                          else VU.sum subY / fromIntegral n
      !varY   = varianceUS subY
  in if n <= rfMinSamples cfg
       || depth >= rfMaxDepth cfg
       || varY < 1e-12
       then Leaf meanY
       else
         let !d    = LA.cols x
             !mtry = case rfMtry cfg of
                       Just m  -> max 1 (min d m)
                       Nothing -> max 1 (d `div` 3)
             !featIxs = pickFeats d mtry depth n
             !mBest   = bestSplitVRF featIxs x y idx
         in case mBest of
              Nothing             -> Leaf meanY
              Just (j, thr, _)    ->
                let (lIdx, rIdx) = partitionByFeat x idx j thr
                in if VU.null lIdx || VU.null rIdx
                     then Leaf meanY
                     else Node j thr
                            (buildTreeV cfg x y lIdx (depth + 1))
                            (buildTreeV cfg x y rIdx (depth + 1))

-- | Deterministic pseudo-random feature subset using an LCG seeded by
-- @(depth, n)@. Different nodes typically see different subsets,
-- which is the decorrelation that random forests need at split time.
-- Tree-level randomness comes from 'bootstrapIdx', which threads
-- through 'MWC.GenIO'.
pickFeats :: Int -> Int -> Int -> Int -> VU.Vector Int
pickFeats d mtry depth n
  | mtry >= d = VU.enumFromN 0 d
  | otherwise =
      let seed0 = depth * 1009 + n * 31 + 1
          step !s = (s * 1103515245 + 12345) `mod` (2 ^ (31 :: Int))
          go !s !chosen !left
            | left == 0 = chosen
            | otherwise =
                let !s' = step s
                    !i  = s' `mod` d
                in if i `VU.elem` chosen
                     then go s' chosen left
                     else go s' (chosen `VU.snoc` i) (left - 1)
      in go seed0 VU.empty mtry

partitionByFeat :: LA.Matrix Double
                -> VU.Vector Int
                -> Int
                -> Double
                -> (VU.Vector Int, VU.Vector Int)
partitionByFeat x idx feat thr =
  let pred_ i = LA.atIndex x (i, feat) <= thr
  in VU.partition pred_ idx

-- ---------------------------------------------------------------------------
-- Best split
-- ---------------------------------------------------------------------------

bestSplitVRF :: VU.Vector Int
             -> LA.Matrix Double
             -> VU.Vector Double
             -> VU.Vector Int
             -> Maybe (Int, Double, Double)
bestSplitVRF featIxs x y idx
  | VU.length idx < 2 = Nothing
  | otherwise =
      let go best j =
            case bestSplitFeatureRF x y idx j of
              Nothing       -> best
              Just (thr, g) ->
                case best of
                  Nothing                       -> Just (j, thr, g)
                  Just (_, _, gPrev) | g > gPrev -> Just (j, thr, g)
                                    | otherwise -> best
      in VU.foldl' go Nothing featIxs

-- | Per-feature best split for regression: maximise variance reduction
-- via single sort + linear sweep with running sum / sum-of-squares.
bestSplitFeatureRF :: LA.Matrix Double
                   -> VU.Vector Double
                   -> VU.Vector Int
                   -> Int
                   -> Maybe (Double, Double)
bestSplitFeatureRF x y idx feat = runST $ do
  let !n = VU.length idx
  pairs <- VUM.new n
  let valOf i = LA.atIndex x (i, feat)
      yOf  i = y VU.! i
      fill !k
        | k == n = pure ()
        | otherwise = do
            let !i = VU.unsafeIndex idx k
            VUM.unsafeWrite pairs k (valOf i, yOf i)
            fill (k + 1)
  fill 0
  Intro.sortBy (\a b -> compare (fst a) (fst b)) pairs
  pairsF <- VU.unsafeFreeze pairs

  let !sumY     = VU.sum (VU.map snd pairsF)
      !sumY2    = VU.sum (VU.map (\(_, v) -> v * v) pairsF)
      !nD       = fromIntegral n :: Double
      !parentSS = sumY2 - sumY * sumY / nD

  let sweep !k !sumYL !sumY2L !bestThr !bestGain
        | k >= n - 1 = pure (bestThr, bestGain)
        | otherwise = do
            let (v_k,  yk) = VU.unsafeIndex pairsF k
                (v_k1, _)  = VU.unsafeIndex pairsF (k + 1)
                !sumYL'  = sumYL  + yk
                !sumY2L' = sumY2L + yk * yk
            if v_k == v_k1
              then sweep (k + 1) sumYL' sumY2L' bestThr bestGain
              else do
                let !nL  = fromIntegral (k + 1) :: Double
                    !nR  = nD - nL
                    !sumYR  = sumY  - sumYL'
                    !sumY2R = sumY2 - sumY2L'
                    !ssL    = sumY2L' - sumYL' * sumYL' / nL
                    !ssR    = sumY2R  - sumYR  * sumYR  / nR
                    !gain   = parentSS - ssL - ssR
                    !thr    = (v_k + v_k1) / 2
                if gain > bestGain
                  then sweep (k + 1) sumYL' sumY2L' thr  gain
                  else sweep (k + 1) sumYL' sumY2L' bestThr bestGain
  (thr, gain) <- sweep 0 0 0 0 (negate (1.0 / 0.0))
  pure $ if gain == negate (1.0 / 0.0)
           then Nothing
           else Just (thr, gain)

-- ---------------------------------------------------------------------------
-- Variance helper
-- ---------------------------------------------------------------------------

varianceUS :: VU.Vector Double -> Double
varianceUS v
  | VU.length v <= 1 = 0
  | otherwise =
      let !n  = fromIntegral (VU.length v) :: Double
          !mu = VU.sum v / n
      in VU.foldl' (\acc x -> acc + (x - mu) ^ (2 :: Int)) 0 v / n

-- ---------------------------------------------------------------------------
-- Predict
-- ---------------------------------------------------------------------------

predictTree :: Tree -> [Double] -> Double
predictTree (Leaf v)         _  = v
predictTree (Node j thr l r) xs =
  if (xs !! j) <= thr then predictTree l xs else predictTree r xs

predictRF :: RandomForest -> [Double] -> Double
predictRF rf xs =
  let preds = map (`predictTree` xs) (rfTreesV rf)
      n     = length preds
  in if n == 0 then 0 else sum preds / fromIntegral n

featureImportance :: RandomForest -> V.Vector Double
featureImportance rf =
  let raw = rfImportance rf
      tot = V.sum raw
  in if tot <= 0 then raw else V.map (/ tot) raw

-- ---------------------------------------------------------------------------
-- Importance accumulation (per split, simple count)
-- ---------------------------------------------------------------------------

accumulateImportance :: IORef (V.Vector Double) -> Tree -> IO ()
accumulateImportance ref = walk
  where
    walk (Leaf _)       = pure ()
    walk (Node j _ l r) = do
      modifyIORef' ref (\v ->
        let !cur = v V.! j
        in v V.// [(j, cur + 1.0)])
      walk l
      walk r
