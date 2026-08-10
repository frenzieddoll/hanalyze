{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Model.RandomForestClassifier
-- Description : Random Forest 分類版 — DecisionTree の bootstrap aggregation + OOB error + permutation importance
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Random Forest **分類版**。
--
-- bootstrap aggregation of 'Hanalyze.Model.DecisionTree' (CART 分類)。
-- OOB (Out-of-Bag) error と permutation importance を併せて返す。
module Hanalyze.Model.RandomForestClassifier
  ( RFCConfig (..)
  , defaultRFCConfig
  , RFClassifierFit (..)
  , fitRFClassifier
  , fitRFClassifierPure
  , predictRFClassifier
  ) where

import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as VU
import qualified Numeric.LinearAlgebra       as LA
import qualified Data.Map.Strict             as Map
import           Data.List                   (nub, sort, group, sortBy, foldl')
import           Data.Ord                    (comparing, Down (..))
import           Data.Text                   (Text)
import           Data.Word                   (Word32)
import qualified System.Random.MWC           as MWC
import           Control.Monad               (forM, replicateM)
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Control.Monad.ST            (runST)

import qualified Hanalyze.Model.DecisionTree as DT
import           Hanalyze.Model.RandomForest (defaultFeatureNames)

-- ===========================================================================
-- 型
-- ===========================================================================

data RFCConfig = RFCConfig
  { rfcNTrees   :: !Int
  , rfcMaxDepth :: !(Maybe Int)
  , rfcMinSplit :: !Int
  } deriving (Show)

defaultRFCConfig :: RFCConfig
defaultRFCConfig = RFCConfig
  { rfcNTrees   = 100
  , rfcMaxDepth = Just 10
  , rfcMinSplit = 2
  }

data RFClassifierFit = RFClassifierFit
  { rfcTrees          :: ![DT.DTree]
  , rfcOOBSamples     :: ![[Int]]
  , rfcClasses        :: ![Int]
  , rfcOOBError       :: !Double
  , rfcImportance     :: !(LA.Vector Double)  -- ^ permutation importance (OOB accuracy 低下)。
  , rfcGiniImportance :: !(LA.Vector Double)  -- ^ MDI (mean decrease in gini・木構造から純粋計算・sklearn feature_importances_ 同方式)。
  , rfcFeatureNames   :: ![Text]              -- ^ 特徴列名。 行列 fit は 'defaultFeatureNames' ("f1"..)。 実列名は df|-> 化 (後続) で。
  , rfcConfig         :: !RFCConfig
  } deriving (Show)

-- ===========================================================================
-- fit
-- ===========================================================================

-- | IO ラッパ。 ロジックは 'PrimMonad' 汎用の 'fitRFClassifierM' を共有。
fitRFClassifier
  :: RFCConfig
  -> LA.Matrix Double
  -> VU.Vector Int
  -> MWC.GenIO
  -> IO RFClassifierFit
fitRFClassifier = fitRFClassifierM

-- | 純粋・決定的な forest 分類器 (同 @seed@ → ビット同一)。 回帰の 'fitRFVPure' と同方針
-- ([[phase-50-mcmc-purification-status]])。 df|-> ('Fit RFCSpec') 経路が使う。
fitRFClassifierPure
  :: RFCConfig -> LA.Matrix Double -> VU.Vector Int -> Word32 -> RFClassifierFit
fitRFClassifierPure cfg x y seed =
  runST (MWC.initialize (V.singleton seed) >>= fitRFClassifierM cfg x y)

-- | 'PrimMonad' 汎用の forest 分類器本体。 gen は bootstrap index と permutation の
-- 列シャッフルにのみ使う (木構築・OOB・gini は純粋ゆえ ST/IO でビット同一)。
fitRFClassifierM
  :: PrimMonad m
  => RFCConfig
  -> LA.Matrix Double
  -> VU.Vector Int
  -> MWC.Gen (PrimState m)
  -> m RFClassifierFit
fitRFClassifierM cfg x y gen = do
  let n = LA.rows x
      p = LA.cols x
      classes = sort (nub (VU.toList y))
      dtCfg = DT.defaultDecisionTree
        { DT.dtMaxDepth        = rfcMaxDepth cfg
        , DT.dtMinSamplesSplit = rfcMinSplit cfg
        }
  results <- forM [1 .. rfcNTrees cfg] $ \_ -> do
    idxs <- replicateM n (MWC.uniformR (0, n - 1) gen)
    let x'  = x LA.? idxs
        y'  = VU.fromList [ y VU.! i | i <- idxs ]
        tree = DT.fitDTV dtCfg x' y'
        oob  = filter (`notElem` idxs) [0 .. n - 1]
    pure (tree, oob)
  let trees    = [ t | (t, _) <- results ]
      oobLists = [ o | (_, o) <- results ]
      oobErr   = computeOOB x y trees oobLists
  -- permutation importance: fixed-seed gen for reproducibility per feature
  imp <- permImportance gen x y trees
  pure RFClassifierFit
    { rfcTrees          = trees
    , rfcOOBSamples     = oobLists
    , rfcClasses        = classes
    , rfcOOBError       = oobErr
    , rfcImportance     = imp
    , rfcGiniImportance = giniImportance p trees
    , rfcFeatureNames   = defaultFeatureNames p
    , rfcConfig         = cfg
    }

-- | 各サンプルを多数決で予測。
predictRFClassifier :: RFClassifierFit -> LA.Matrix Double -> V.Vector Int
predictRFClassifier fit xNew =
  V.generate (LA.rows xNew) $ \i ->
    let row = LA.toList (LA.flatten (xNew LA.? [i]))
    in majority [ DT.predictDT t row | t <- rfcTrees fit ]

-- ===========================================================================
-- 内部
-- ===========================================================================

computeOOB
  :: LA.Matrix Double -> VU.Vector Int -> [DT.DTree] -> [[Int]] -> Double
computeOOB x y trees oobLists =
  let n = LA.rows x
      voteFor s =
        let voters = [ t | (t, oob) <- zip trees oobLists, s `elem` oob ]
        in if null voters then Nothing
           else
             let row = LA.toList (LA.flatten (x LA.? [s]))
             in Just (majority [ DT.predictDT t row | t <- voters ])
      voted = [ (s, p) | s <- [0 .. n - 1]
                       , Just p <- [voteFor s] ]
      nTotal = length voted
      nErr   = length [ () | (s, p) <- voted, p /= (y VU.! s) ]
  in if nTotal == 0 then 0 else fromIntegral nErr / fromIntegral nTotal

majority :: [Int] -> Int
majority xs =
  let grouped = map (\g -> (head g, length g)) (group (sort xs))
  in case sortBy (comparing (Down . snd)) grouped of
       ((c, _) : _) -> c
       []           -> 0

-- | Mean Decrease in Impurity (gini) per feature, summed over all trees
-- (sklearn @feature_importances_@ 同方式・木構造から純粋計算)。 各内部ノードの
-- 重み付き gini 減少 @n·(imp − (nL/n)·impL − (nR/n)·impR)@ を分割特徴に加算し、
-- 全木ぶん合計 → 合計 1 に正規化。 'DT.DTree' の 75.23 拡張 (dnN/dnImpurity) を使う。
giniImportance :: Int -> [DT.DTree] -> LA.Vector Double
giniImportance p trees =
  let m0 = Map.fromList [ (j, 0 :: Double) | j <- [0 .. p - 1] ]
      go m (DT.DLeaf{}) = m
      go m (DT.DNode { DT.dnFeature = j, DT.dnLeft = l, DT.dnRight = r
                     , DT.dnN = nn, DT.dnImpurity = imp }) =
        let n    = fromIntegral nn :: Double
            nL   = fromIntegral (nodeN l)
            nR   = fromIntegral (nodeN r)
            dec  = if n <= 0 then 0
                   else n * (imp - (nL / n) * nodeImp l - (nR / n) * nodeImp r)
            m'   = Map.insertWith (+) j dec m
        in go (go m' l) r
      accM = foldl' go m0 trees
      raw  = [ Map.findWithDefault 0 j accM | j <- [0 .. p - 1] ]
      tot  = sum raw
  in LA.fromList (if tot <= 0 then raw else map (/ tot) raw)

-- | ノードのサンプル数 / gini 不純度 (葉・内部で共通アクセス)。
nodeN :: DT.DTree -> Int
nodeN (DT.DLeaf{ DT.dlN = n }) = n
nodeN (DT.DNode{ DT.dnN = n }) = n

nodeImp :: DT.DTree -> Double
nodeImp (DT.DLeaf{ DT.dlImpurity = i }) = i
nodeImp (DT.DNode{ DT.dnImpurity = i }) = i

permImportance
  :: PrimMonad m
  => MWC.Gen (PrimState m) -> LA.Matrix Double -> VU.Vector Int -> [DT.DTree]
  -> m (LA.Vector Double)
permImportance gen x y trees = do
  let p = LA.cols x
      baseAcc = forestAccuracy x y trees
  scores <- forM [0 .. p - 1] $ \j -> do
    xPerm <- permuteColumn j gen x
    let acc = forestAccuracy xPerm y trees
    pure (baseAcc - acc)
  pure (LA.fromList scores)

forestAccuracy :: LA.Matrix Double -> VU.Vector Int -> [DT.DTree] -> Double
forestAccuracy x y trees =
  let n = LA.rows x
      preds =
        [ let row = LA.toList (LA.flatten (x LA.? [i]))
          in majority [ DT.predictDT t row | t <- trees ]
        | i <- [0 .. n - 1] ]
      correct = length [ () | (p_, i) <- zip preds [0 ..]
                            , p_ == (y VU.! i) ]
  in fromIntegral correct / fromIntegral n

permuteColumn :: PrimMonad m
              => Int -> MWC.Gen (PrimState m) -> LA.Matrix Double -> m (LA.Matrix Double)
permuteColumn j gen x = do
  let col = LA.toList (LA.flatten (x LA.¿ [j]))
  shuf <- fisherYates gen col
  let newCol = LA.fromList shuf
      cols = [ if k == j then newCol else LA.flatten (x LA.¿ [k])
             | k <- [0 .. LA.cols x - 1] ]
  pure (LA.fromColumns cols)

fisherYates :: PrimMonad m => MWC.Gen (PrimState m) -> [a] -> m [a]
fisherYates gen xs =
  let v0 = V.fromList xs
  in go v0 (V.length v0 - 1)
  where
    go v 0 = pure (V.toList v)
    go v i = do
      j <- MWC.uniformR (0, i) gen
      let vi = v V.! i
          vj = v V.! j
          v' = v V.// [(i, vj), (j, vi)]
      go v' (i - 1)
