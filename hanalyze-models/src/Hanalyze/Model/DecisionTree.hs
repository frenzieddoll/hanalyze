{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Model.DecisionTree
-- Description : 決定木分類器 (CART, classification)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Decision tree classifier (CART, classification).
--
-- Pairs with the existing regression-oriented 'Hanalyze.Model.RandomForest';
-- this module focuses on classification. Splits use Gini impurity as
-- the criterion (matches sklearn default).
--
-- @
-- import Hanalyze.Model.DecisionTree
--
-- let cfg  = defaultDecisionTree
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
module Hanalyze.Model.DecisionTree
  ( -- * Tree types
    DTree (..)
  , DTFit (..)
  , DTConfig (..)
  , defaultDecisionTree
    -- * Fit / predict
  , fitDT
  , fitDTV
  , predictDT
  , predictDTProbs
    -- * Text export (R @print.rpart@ 相当)
  , printRpart
  , printRpartRaw
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
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Numeric                     (showFFloat)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Classification decision tree node.
-- | 決定木。 Phase 75.23 で各ノードに **サンプル数 n / gini 不純度 / クラス分布 /
-- 多数決クラス** を保持するよう拡張 (rpart.plot / sklearn plot_tree 水準の樹形図・
-- ルールテキスト出力のため)。 予測 (predict) の数値は不変。
data DTree
  = DLeaf
      { dlClassProbs :: !(Map.Map Int Double)  -- ^ クラス割合。
      , dlMajority   :: !Int                   -- ^ 多数決クラス (予測)。
      , dlN          :: !Int                   -- ^ このノードのサンプル数。
      , dlImpurity   :: !Double                -- ^ gini 不純度。
      }
  | DNode
      { dnFeature :: !Int
      , dnThr     :: !Double
      , dnLeft    :: !DTree
      , dnRight   :: !DTree
      , dnN        :: !Int                     -- ^ このノードのサンプル数。
      , dnImpurity :: !Double                  -- ^ 分割前の gini 不純度。
      , dnProbs    :: !(Map.Map Int Double)    -- ^ 分割前のクラス割合。
      , dnMajority :: !Int                     -- ^ 分割前の多数決クラス。
      }
  deriving (Show)

-- | 学習済み決定木 + 表示メタ (特徴量名・クラス名)。 高レベル @df |-> decisionTree@
--   ('Hanalyze.Fit') が fit 時に手元の実列名とクラス列の levels を載せて返す
--   ('RandomForestClassifier.RFClassifierFit' と同型のラッパ)。 これにより 'treePlot' /
--   'printRpart' は名前を手渡しせず @DTFit@ 一つで済む。 クラス番号 (0..K-1) は
--   @dtClassNames !! k@ で名前が引ける。
data DTFit = DTFit
  { dtTree         :: !DTree    -- ^ 学習済み木。
  , dtFeatureNames :: ![Text]   -- ^ 特徴量名 (fit に使った列順)。
  , dtClassNames   :: ![Text]   -- ^ クラス名 (label 0..K-1 に対応する levels)。
  } deriving (Show)

-- | Decision tree configuration.
data DTConfig = DTConfig
  { dtMaxDepth        :: !(Maybe Int)
  , dtMinSamplesSplit :: !Int
  , dtMinSamplesLeaf  :: !Int
  , dtMinImpurity     :: !Double
  } deriving (Show, Eq)

-- | Defaults (sklearn-compatible): unlimited depth, min split 2,
-- min leaf 1, min impurity 0.
defaultDecisionTree :: DTConfig
defaultDecisionTree = DTConfig
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
  | null xs   = DLeaf Map.empty 0 0 0
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
      !gini     = giniFromCounts probs
      !majority = argMaxClass probs
      leaf      = DLeaf probs majority nIdx gini

      depthLimit = case dtMaxDepth cfg of
                     Just d  -> depth >= d
                     Nothing -> False
      stop = depthLimit
          || nIdx < dtMinSamplesSplit cfg
          || gini < dtMinImpurity cfg
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
                       , dnN        = nIdx
                       , dnImpurity = gini
                       , dnProbs    = probs
                       , dnMajority = majority
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

-- | 多数決 (予測) クラス = 確率最大のクラス。 同点は **最小クラス index** を選ぶ
--   (rpart / sklearn 慣例)。 'Map.toList' は昇順 key なので、 @foldl'@ で「厳密に
--   大きい確率でだけ更新」すれば先勝ち = 最小 index の同点タイブレークになる。
--
--   ⚠ 旧 @sortByValDescV@ は名前に反して昇順を返し (@reverse . 降順ソート@)、
--   @head@ が **最小確率クラス (argmin)** を拾っていた。 深さ無制限で葉が純粋な間は
--   露見しないが、 depth/min_samples で止まった混在葉で予測が少数派に化ける実バグ
--   だった (Phase 75.26 で樹形図を目視して発覚・修正)。
argMaxClass :: Map.Map Int Double -> Int
argMaxClass m = case Map.toList m of
  []       -> 0
  (x : xs) -> fst (foldl' better x xs)
  where
    better acc@(_, av) cur@(_, cv)
      | cv > av   = cur   -- 厳密に大きい確率のときだけ更新。
      | otherwise = acc   -- 同点は据置き = 昇順 key で先に来た小さい index が勝つ。

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
predictDT DLeaf{dlMajority = m} _ = m
predictDT DNode{dnFeature = i, dnThr = thr, dnLeft = l, dnRight = r} x
  | x !! i <= thr = predictDT l x
  | otherwise     = predictDT r x

-- | Predict class probabilities for one sample.
predictDTProbs :: DTree -> [Double] -> Map.Map Int Double
predictDTProbs DLeaf{dlClassProbs = p} _ = p
predictDTProbs DNode{dnFeature = i, dnThr = thr, dnLeft = l, dnRight = r} x
  | x !! i <= thr = predictDTProbs l x
  | otherwise     = predictDTProbs r x

-- ---------------------------------------------------------------------------
-- Text export (R print.rpart 相当)
-- ---------------------------------------------------------------------------

-- | 決定木のルールを R @print.rpart@ 形式のテキストで出力する。
--
-- R の rpart オブジェクトを @print@ したときと同じ体裁:
--
-- @
-- n= &lt;total&gt;
--
-- node), split, n, loss, yval, (yprob)
--       * denotes terminal node
--
-- 1) root 12 8 setosa (0.3333 0.3333 0.3333)
--   2) petal_width< 0.80 4 0 setosa (1.0000 0.0000 0.0000) *
--   3) petal_width>=0.80 8 4 versicolor (0.0000 0.5000 0.5000)
--     6) petal_width< 1.65 4 0 versicolor (0.0000 1.0000 0.0000) *
--     7) petal_width>=1.65 4 0 virginica (0.0000 0.0000 1.0000) *
-- @
--
-- 各行 = @&lt;node#&gt;) &lt;split&gt; &lt;n&gt; &lt;loss&gt; &lt;yval&gt; (&lt;yprob…&gt;) [*]@。
-- ノード番号は R 同様 root=1・子は @2k@/@2k+1@。 @loss@ = 誤分類数
-- (n − 多数決クラス件数)、 @yval@ = 予測クラス、 @yprob@ = 木に現れる全クラスの
-- 確率 (クラス index 昇順)、 @*@ = 終端 (葉)。 分岐は R の固定幅表記に忠実に
-- 左 = @name&lt; thr@ (≤・条件成立)、 右 = @name&gt;=thr@ とする (dtreeToDag と同じ
-- 左 ≤ / 右 > 慣例)。
--
-- 第 1 引数 = 特徴量名、 第 2 引数 = クラス名 (yval に使う factor 水準)。
-- いずれも index に対し長さ不足・空文字なら @f{i}@ / 生の整数へフォールバックする
-- (行列 fit で名無しの木でも動く)。 75.23 で各ノードに載せた n / gini / クラス分布
-- から純粋計算し、 予測 (predict) の数値には非依存。
-- | 高レベル版 — 'DTFit' からノード規則テキストを出す ('df |-> decisionTree' の返り値
--   をそのまま渡せる)。 名前を手渡ししたい低レベルは 'printRpartRaw'。
printRpart :: DTFit -> Text
printRpart (DTFit tree feats classes) = printRpartRaw feats classes tree

-- | 行列 fit 用の低レベル版 — 特徴量名・クラス名を明示的に渡す。
printRpartRaw :: [Text] -> [Text] -> DTree -> Text
printRpartRaw featNames classNames tree =
  T.intercalate "\n" (header ++ go 1 0 "root" tree)
  where
    classes = Map.keys (labelSet tree)          -- 木に現れる全クラス (昇順)。
    header =
      [ "n= " <> tShow (nodeN tree)
      , ""
      , "node), split, n, loss, yval, (yprob)"
      , "      * denotes terminal node"
      , "" ]

    go :: Int -> Int -> Text -> DTree -> [Text]
    go num d split node =
      let n     = nodeN node
          probs = nodeProbs node
          maj   = nodeMajority node
          loss  = n - round (Map.findWithDefault 0 maj probs * fromIntegral n) :: Int
          yprob = "(" <> T.intercalate " "
                    [ fmt4 (Map.findWithDefault 0 c probs) | c <- classes ] <> ")"
          term  = case node of DLeaf{} -> " *"; _ -> ""
          line  = T.concat (replicate d "  ") <> tShow num <> ") " <> split
                    <> " " <> tShow n <> " " <> tShow loss <> " " <> classLabel maj
                    <> " " <> yprob <> term
      in case node of
           DLeaf{}                -> [line]
           DNode f thr l r _ _ _ _ ->
             let fn = featName f
                 lb = fn <> "< "  <> fmt2 thr
                 rb = fn <> ">="  <> fmt2 thr
             in line : go (2 * num) (d + 1) lb l ++ go (2 * num + 1) (d + 1) rb r

    featName i  = pick i featNames  ("f" <> tShow i)
    classLabel i = pick i classNames (tShow i)
    pick i xs dflt = case drop i xs of
      (nm : _) | not (T.null nm) -> nm
      _                          -> dflt

    tShow  = T.pack . show
    fmt2 x = T.pack (showFFloat (Just 2) x "")
    fmt4 x = T.pack (showFFloat (Just 4) x "")

-- | 木に現れる全クラス label を集めた集合 (値は () のダミー)。 'Map.keys' で昇順。
labelSet :: DTree -> Map.Map Int ()
labelSet (DLeaf p m _ _)          = Map.insert m () (() <$ p)
labelSet (DNode _ _ l r _ _ p m)  =
  Map.unions [Map.insert m () (() <$ p), labelSet l, labelSet r]

-- ノードアクセサ (葉 / 分岐 共通)。
nodeN :: DTree -> Int
nodeN (DLeaf _ _ n _)         = n
nodeN (DNode _ _ _ _ n _ _ _) = n

nodeProbs :: DTree -> Map.Map Int Double
nodeProbs (DLeaf p _ _ _)         = p
nodeProbs (DNode _ _ _ _ _ _ p _) = p

nodeMajority :: DTree -> Int
nodeMajority (DLeaf _ m _ _)         = m
nodeMajority (DNode _ _ _ _ _ _ _ m) = m

-- Silence unused-import warning for V (keeps import slot for future
-- variants without re-touching imports).
_unused :: V.Vector Int -> Int
_unused = V.length
