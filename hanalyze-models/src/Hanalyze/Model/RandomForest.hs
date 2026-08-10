{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Model.RandomForest
-- Description : 回帰用 Random Forest (CART + bagging + random feature subset、行インデックス置換方式)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Random forest for regression (CART + bagging + random feature subset).
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
  , defaultRandomForest
  , buildTree
  , buildTreeV
  , predictTree
    -- * Forest
  , RandomForest (..)
  , fitRF
  , fitRFV
  , fitRFPure
  , fitRFVPure
  , predictRF
  , featureImportance
  , rfPermutationImportance
  , defaultFeatureNames
  ) where

import qualified Data.Vector                  as V
import qualified Data.Vector.Mutable          as VM
import qualified Data.Vector.Unboxed          as VU
import qualified Data.Vector.Unboxed.Mutable  as VUM
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Numeric.LinearAlgebra        as LA
import qualified System.Random.MWC            as MWC
import           Control.Monad                (replicateM)
import           Control.Monad.Primitive      (PrimMonad, PrimState)
import           Control.Monad.ST             (runST)
import           Data.Word                    (Word32)
import           Data.Text                    (Text)
import qualified Data.Text                    as T

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

defaultRandomForest :: RFConfig
defaultRandomForest = RFConfig
  { rfTrees      = 100
  , rfMaxDepth   = 12
  , rfMinSamples = 3
  , rfMtry       = Nothing
  , rfBootstrap  = True
  }

data RandomForest = RandomForest
  { rfTreesV         :: ![Tree]
  , rfNFeatures      :: !Int
  , rfImportance     :: !(V.Vector Double)  -- ^ impurity/split ベース (MDI 相当・R IncNodePurity)。
  , rfPermImportance :: !(V.Vector Double)  -- ^ permutation ベース (MSE 増加・R %IncMSE・sklearn permutation_importance)。
  , rfFeatureNames   :: ![Text]             -- ^ 特徴列名。 df|-> 経路が実列名を設定、 低レベル行列 fit は 'defaultFeatureNames' ("f1"..)。
  } deriving (Show)

-- | 名前を持たない行列入力の既定特徴名 ("f1", "f2", …・1 始まり = R/sklearn 慣例)。
defaultFeatureNames :: Int -> [Text]
defaultFeatureNames d = [ "f" <> T.pack (show k) | k <- [1 .. d] ]

-- ---------------------------------------------------------------------------
-- Vector-based fit (primary)
-- ---------------------------------------------------------------------------

-- | IO ラッパ。 ロジックは 'PrimMonad' 汎用の 'fitRFVM' を共有
-- (mwc は 'PrimMonad' 汎用ゆえ ST/IO 両経路で同コード)。
fitRFV :: RFConfig
       -> LA.Matrix Double
       -> VU.Vector Double
       -> MWC.GenIO
       -> IO RandomForest
fitRFV = fitRFVM

-- | 'PrimMonad' 汎用の forest 本体。 'fitRFV' (IO) / 'fitRFVPure' (ST) が共有。
-- 乱数 (gen) は bootstrap index のみで使う。 木構築 'buildTreeV' と feature
-- importance は純粋ゆえ ST/IO でビット同一。
fitRFVM :: PrimMonad m
        => RFConfig
        -> LA.Matrix Double
        -> VU.Vector Double
        -> MWC.Gen (PrimState m)
        -> m RandomForest
fitRFVM cfg x y gen = do
  let !n = VU.length y
      !d = LA.cols x
  trees <- replicateM (rfTrees cfg) $ do
    !idx <- if rfBootstrap cfg
              then bootstrapIdxM n gen
              else pure (VU.enumFromN 0 n)
    pure $! buildTreeV cfg x y idx 0
  -- permutation importance は列シャッフルに gen を使う (bootstrap の後・seed 決定的)。
  !perm <- permImportanceRegM x y trees gen
  pure RandomForest
    { rfTreesV         = trees
    , rfNFeatures      = d
    , rfImportance     = importanceOf d trees
    , rfPermImportance = perm
    , rfFeatureNames   = defaultFeatureNames d
    }

-- | Backwards-compatible list-based fit.
fitRF :: RFConfig -> [[Double]] -> [Double] -> MWC.GenIO -> IO RandomForest
fitRF cfg xs ys gen
  | null xs   = pure emptyForest
  | otherwise = fitRFV cfg (LA.fromLists xs) (VU.fromList ys) gen

-- | 純粋・決定的な行列入力 forest。 同じ @seed@ なら必ず同じ 'RandomForest'。
-- 'fitRFVM' を 'ST' で走らせ 'runST' で閉じる
-- ([[phase-50-mcmc-purification-status]] の 'nutsPure' と同方針)。
fitRFVPure :: RFConfig
           -> LA.Matrix Double
           -> VU.Vector Double
           -> Word32
           -> RandomForest
fitRFVPure cfg x y seed =
  runST (MWC.initialize (V.singleton seed) >>= fitRFVM cfg x y)

-- | 純粋・決定的な list 入力 forest (list 版 'fitRF' の seed 純粋版)。
fitRFPure :: RFConfig -> [[Double]] -> [Double] -> Word32 -> RandomForest
fitRFPure cfg xs ys seed
  | null xs   = emptyForest
  | otherwise = fitRFVPure cfg (LA.fromLists xs) (VU.fromList ys) seed

-- | 空データ時の forest (全フィールド空)。
emptyForest :: RandomForest
emptyForest = RandomForest [] 0 V.empty V.empty []

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
               then bootstrapIdxM n gen
               else pure (VU.enumFromN 0 n)
      pure (buildTreeV cfg x y idx 0)

bootstrapIdxM :: PrimMonad m => Int -> MWC.Gen (PrimState m) -> m (VU.Vector Int)
bootstrapIdxM n gen =
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

-- | Permutation importance (= 列を無作為置換したときの MSE 増加) を正の総和で
-- 正規化して返す。 全て非正なら raw のまま (負 = その特徴が予測に無寄与)。
-- R @randomForest %IncMSE@ / sklearn @permutation_importance@ 同方式。
rfPermutationImportance :: RandomForest -> V.Vector Double
rfPermutationImportance rf =
  let raw = rfPermImportance rf
      tot = V.sum (V.filter (> 0) raw)
  in if tot <= 0 then raw else V.map (/ tot) raw

-- ---------------------------------------------------------------------------
-- Permutation importance (MSE 増加ベース)
-- ---------------------------------------------------------------------------

-- | 各特徴列を無作為置換し、 forest の MSE 増加量を測る (純粋・'PrimMonad')。
-- gen は列シャッフルにのみ使う。 同 seed → ビット同一。
permImportanceRegM :: PrimMonad m
                   => LA.Matrix Double -> VU.Vector Double -> [Tree]
                   -> MWC.Gen (PrimState m) -> m (V.Vector Double)
permImportanceRegM x y trees gen
  | LA.rows x == 0 || null trees = pure (V.replicate (LA.cols x) 0)
  | otherwise = do
      let !base = forestMSE x y trees
      scores <- mapM (\j -> do
                         xp <- permuteColM j x gen
                         pure $! forestMSE xp y trees - base)
                     [0 .. LA.cols x - 1]
      pure (V.fromList scores)

-- | forest の平均二乗誤差 (行毎に木予測を平均)。
forestMSE :: LA.Matrix Double -> VU.Vector Double -> [Tree] -> Double
forestMSE x y trees =
  let !n = LA.rows x
      !k = length trees
      rowPred i =
        let row   = LA.toList (LA.flatten (x LA.? [i]))
            preds = map (`predictTree` row) trees
        in if k == 0 then 0 else sum preds / fromIntegral k
      sse = sum [ (rowPred i - y VU.! i) ^ (2 :: Int) | i <- [0 .. n - 1] ]
  in if n == 0 then 0 else sse / fromIntegral n

-- | 列 j を Fisher-Yates で置換した行列を返す (他列は不変)。
permuteColM :: PrimMonad m
            => Int -> LA.Matrix Double -> MWC.Gen (PrimState m) -> m (LA.Matrix Double)
permuteColM j x gen = do
  let cols0 = LA.toColumns x
      colj  = VU.fromList (LA.toList (cols0 !! j))
  shuf <- fisherYatesM gen colj
  let newCols = [ if kk == j then LA.fromList (VU.toList shuf) else cols0 !! kk
                | kk <- [0 .. length cols0 - 1] ]
  pure (LA.fromColumns newCols)

-- | 可変ベクトル上の Fisher-Yates シャッフル ('PrimMonad'・gen 決定的)。
fisherYatesM :: PrimMonad m
             => MWC.Gen (PrimState m) -> VU.Vector Double -> m (VU.Vector Double)
fisherYatesM gen v0 = do
  mv <- VU.thaw v0
  let go i | i <= 0    = pure ()
           | otherwise = do
               j <- MWC.uniformR (0, i) gen
               VUM.swap mv i j
               go (i - 1)
  go (VUM.length mv - 1)
  VU.freeze mv

-- ---------------------------------------------------------------------------
-- Importance accumulation (per split, simple count)
-- ---------------------------------------------------------------------------

-- | 全木の split 特徴を 1 回の可変ベクトル走査で集計 (純粋)。 旧 'IORef'
-- 版を 'runST' + 可変ベクトルへ置換 (count の和は可換ゆえ木順不問で同値)。
importanceOf :: Int -> [Tree] -> V.Vector Double
importanceOf d trees = runST $ do
  v <- VM.replicate d 0.0
  let walk (Leaf _)       = pure ()
      walk (Node j _ l r) = do
        VM.modify v (+ 1.0) j
        walk l
        walk r
  mapM_ walk trees
  V.freeze v
