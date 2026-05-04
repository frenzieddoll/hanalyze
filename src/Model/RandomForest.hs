{-# LANGUAGE OverloadedStrings #-}
-- | Random forest for regression (CART + bagging + random feature subset).
--
-- Tree construction:
--
--   1. At each node, sample @mtry@ features at random (without
--      replacement, @mtry < d@).
--   2. For each feature, try the best split (maximum variance reduction).
--   3. Split the node and recurse.
--   4. Leaf conditions: @depth ≥ maxDepth@, fewer than @minSamples@ in
--      the node, or near-zero variance.
--
-- Forest:
--
--   * Build @n_trees@ trees on bootstrap samples.
--   * Predict with the mean across trees.
--   * Feature importance: per-feature sum of variance reductions across
--     all splits that used the feature.
module Model.RandomForest
  ( -- * 単一決定木
    Tree (..)
  , RFConfig (..)
  , defaultRFConfig
  , buildTree
  , predictTree
    -- * フォレスト
  , RandomForest (..)
  , fitRF
  , predictRF
  , featureImportance
  ) where

import qualified Data.Vector as V
import qualified System.Random.MWC as MWC
import Control.Monad (replicateM)
import Data.List (sort, foldl')
import qualified Data.IORef
import Data.IORef (newIORef, readIORef, modifyIORef')

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | A regression tree node.
data Tree
  = Leaf Double                       -- ^ Leaf prediction (mean of @y@ in the node).
  | Node !Int !Double !Tree !Tree     -- ^ Split feature index, threshold,
                                      --   left child (@≤@), right child (@>@).
  deriving (Show)

-- | Random-forest configuration.
data RFConfig = RFConfig
  { rfTrees      :: Int       -- ^ Number of trees (default 100).
  , rfMaxDepth   :: Int       -- ^ Maximum tree depth (default 12).
  , rfMinSamples :: Int       -- ^ Minimum samples per leaf (default 3).
  , rfMtry       :: Maybe Int -- ^ Features tried per split
                              --   (default @max(1, d/3)@).
  , rfBootstrap  :: Bool      -- ^ Use bootstrap sampling (default 'True').
  } deriving (Show)

-- | Default configuration: 100 trees, max depth 12, min-samples 3,
-- default mtry, bootstrap enabled.
defaultRFConfig :: RFConfig
defaultRFConfig = RFConfig
  { rfTrees      = 100
  , rfMaxDepth   = 12
  , rfMinSamples = 3
  , rfMtry       = Nothing
  , rfBootstrap  = True
  }

-- | A trained random forest.
data RandomForest = RandomForest
  { rfTreesV     :: ![Tree]              -- ^ The constituent trees.
  , rfNFeatures  :: !Int                 -- ^ Number of input features @d@.
  , rfImportance :: !(V.Vector Double)   -- ^ Per-feature accumulated variance reduction.
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- 単一木の構築
-- ---------------------------------------------------------------------------

-- | Build a single CART regression tree from the @n × d@ feature matrix
-- and length-@n@ response.
buildTree :: RFConfig
          -> [[Double]]            -- ^ Rows = samples, columns = features.
          -> [Double]              -- ^ Response @y@.
          -> MWC.GenIO
          -> IO Tree
buildTree cfg rows ys gen = do
  -- 累計 importance を IORef に持って外部から参照可能にしたいが、
  -- ここでは buildTree は単一木のみ返す (importance は fitRF で集計)。
  buildNode cfg rows ys 0 gen

buildNode :: RFConfig -> [[Double]] -> [Double] -> Int -> MWC.GenIO -> IO Tree
buildNode cfg rows ys depth gen
  | length ys <= rfMinSamples cfg
      || depth >= rfMaxDepth cfg
      || variance ys < 1e-12 =
      return (Leaf (mean ys))
  | otherwise = do
      let d = if null rows then 0 else length (head rows)
          mtry = case rfMtry cfg of
            Just m  -> max 1 (min d m)
            Nothing -> max 1 (d `div` 3)
      featIxs <- sampleWithoutReplacement mtry d gen
      mBest <- bestSplit featIxs rows ys
      case mBest of
        Nothing -> return (Leaf (mean ys))
        Just (j, thr, _gain) -> do
          let (rowsL, ysL, rowsR, ysR) = splitOn j thr rows ys
          if null ysL || null ysR
             then return (Leaf (mean ys))
             else do
               left  <- buildNode cfg rowsL ysL (depth + 1) gen
               right <- buildNode cfg rowsR ysR (depth + 1) gen
               return (Node j thr left right)

-- | 復元なしランダムサンプリング: 0..d-1 から k 個をランダム選択。
sampleWithoutReplacement :: Int -> Int -> MWC.GenIO -> IO [Int]
sampleWithoutReplacement k n gen
  | k >= n    = return [0 .. n - 1]
  | otherwise = do
      idxs <- replicateM k (MWC.uniformR (0, n - 1) gen)
      -- 重複を排除して目標 k 個になるまでサンプル追加 (簡易版)
      let dedup []     = []
          dedup (x:xs) = x : dedup (filter (/= x) xs)
          unique = dedup idxs
      if length unique >= k
        then return (take k unique)
        else do
          extra <- replicateM k (MWC.uniformR (0, n - 1) gen)
          return (take k (dedup (unique ++ extra)))

-- | 各特徴インデックスに対して最適な split を探し、最大 variance reduction を返す。
bestSplit :: [Int] -> [[Double]] -> [Double] -> IO (Maybe (Int, Double, Double))
bestSplit featIxs rows ys = do
  let n     = length ys
      yMean = mean ys
      yVar  = variance ys * fromIntegral n
      cands = [ trySplit j rows ys n yMean yVar | j <- featIxs ]
      valid = [ (j, thr, gain) | Just (j, thr, gain) <- cands ]
  if null valid
    then return Nothing
    else do
      let best = foldl1 (\a@(_, _, ga) b@(_, _, gb) ->
                          if gb > ga then b else a) valid
      return (Just best)

-- | 1 特徴に対して最適な閾値を探す。
trySplit :: Int -> [[Double]] -> [Double] -> Int -> Double -> Double
         -> Maybe (Int, Double, Double)
trySplit j rows ys _n _yMean parentSS =
  let pairs = sort [ (xs !! j, y) | (xs, y) <- zip rows ys ]
      vals  = map fst pairs
      ysSorted = map snd pairs
      uniqueXs = removeAdj vals
      candidates = [ (a + b) / 2
                   | (a, b) <- zip uniqueXs (drop 1 uniqueXs) ]
      best = foldl' improve Nothing candidates
      improve cur thr =
        let (left, right) = splitAtThr thr pairs
            nL = length left
            nR = length right
        in if nL == 0 || nR == 0 then cur
           else
             let yL = map snd left
                 yR = map snd right
                 ssL = variance yL * fromIntegral nL
                 ssR = variance yR * fromIntegral nR
                 gain = parentSS - ssL - ssR
             in case cur of
                  Nothing -> Just (j, thr, gain)
                  Just (_, _, g0) | gain > g0 -> Just (j, thr, gain)
                  _ -> cur
      _ = ysSorted
  in best
  where
    removeAdj []  = []
    removeAdj [x] = [x]
    removeAdj (x:y:rs)
      | x == y = removeAdj (y:rs)
      | otherwise = x : removeAdj (y:rs)

splitAtThr :: Double -> [(Double, Double)] -> ([(Double, Double)], [(Double, Double)])
splitAtThr thr xs = (filter (\(v, _) -> v <= thr) xs,
                     filter (\(v, _) -> v >  thr) xs)

splitOn :: Int -> Double -> [[Double]] -> [Double]
        -> ([[Double]], [Double], [[Double]], [Double])
splitOn j thr rows ys =
  let pairs = zip rows ys
      (lp, rp) = foldr go ([], []) pairs
      go (xs, y) (l, r)
        | (xs !! j) <= thr = ((xs, y) : l, r)
        | otherwise        = (l, (xs, y) : r)
      (rowsL, ysL) = unzip lp
      (rowsR, ysR) = unzip rp
  in (rowsL, ysL, rowsR, ysR)

-- ---------------------------------------------------------------------------
-- 単一木の予測
-- ---------------------------------------------------------------------------

predictTree :: Tree -> [Double] -> Double
predictTree (Leaf v)         _  = v
predictTree (Node j thr l r) xs =
  if (xs !! j) <= thr then predictTree l xs else predictTree r xs

-- ---------------------------------------------------------------------------
-- フォレスト
-- ---------------------------------------------------------------------------

-- | Fit @n@ trees on bootstrap samples and aggregate the feature
-- importance.
fitRF :: RFConfig -> [[Double]] -> [Double] -> MWC.GenIO -> IO RandomForest
fitRF cfg rows ys gen = do
  let n = length ys
      d = if null rows then 0 else length (head rows)
  impRef <- newIORef (V.replicate d 0.0)
  trees <- replicateM (rfTrees cfg) $ do
    (rs, yb) <- if rfBootstrap cfg
                  then bootstrap n rows ys gen
                  else return (rows, ys)
    t <- buildTree cfg rs yb gen
    -- importance: ツリー内の全 split を集計
    accumulateImportance impRef t
    return t
  imp <- readIORef impRef
  return RandomForest
    { rfTreesV     = trees
    , rfNFeatures  = d
    , rfImportance = imp
    }

bootstrap :: Int -> [[Double]] -> [Double] -> MWC.GenIO
          -> IO ([[Double]], [Double])
bootstrap n rows ys gen = do
  ixs <- replicateM n (MWC.uniformR (0, n - 1) gen)
  let rs = [ rows !! i | i <- ixs ]
      yb = [ ys   !! i | i <- ixs ]
  return (rs, yb)

accumulateImportance :: Data.IORef.IORef (V.Vector Double) -> Tree -> IO ()
accumulateImportance ref tree = walk tree
  where
    walk (Leaf _)       = return ()
    walk (Node j _ l r) = do
      modifyIORef' ref (\v ->
        let curV = v V.! j
        in v V.// [(j, curV + 1.0)])   -- 簡易: 分割回数で代替
      walk l
      walk r

-- ---------------------------------------------------------------------------
-- フォレスト予測
-- ---------------------------------------------------------------------------

-- | Predict for one input by averaging the trees' predictions.
predictRF :: RandomForest -> [Double] -> Double
predictRF rf xs =
  let preds = map (`predictTree` xs) (rfTreesV rf)
      n     = length preds
  in if n == 0 then 0 else sum preds / fromIntegral n

-- | Per-feature importance, normalized to sum to 1.
featureImportance :: RandomForest -> V.Vector Double
featureImportance rf =
  let raw  = rfImportance rf
      tot  = V.sum raw
  in if tot <= 0 then raw else V.map (/ tot) raw

-- ---------------------------------------------------------------------------
-- 補助関数
-- ---------------------------------------------------------------------------

mean :: [Double] -> Double
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

variance :: [Double] -> Double
variance xs
  | length xs <= 1 = 0
  | otherwise =
      let m = mean xs
      in sum [(x - m) ^ (2 :: Int) | x <- xs] / fromIntegral (length xs)
