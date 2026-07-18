{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Model.HierarchicalCluster
-- Description : 凝集型階層クラスタリング (Agglomerative Hierarchical Clustering)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 凝集型階層クラスタリング (Agglomerative Hierarchical Clustering)。
--
-- Lance-Williams update formula による O(n²) アルゴリズム。
-- 各ステップで最近接クラスタ対をマージし、 新クラスタへの距離を再計算する。
--
-- 対応 linkage:
--
--   * 'Single'   : d(i∪j, k) = min(d(i,k), d(j,k))
--   * 'Complete' : d(i∪j, k) = max(d(i,k), d(j,k))
--   * 'Average'  : (|i|·d(i,k) + |j|·d(j,k)) / (|i|+|j|)
--   * 'Ward'     : Lance-Williams 係数で分散最小化
--
-- 距離は Euclidean のみサポート (X の各行をサンプルとして二乗ユークリッド距離)。
module Hanalyze.Model.HierarchicalCluster
  ( Linkage (..)
  , HClusterFit (..)
  , fitHierarchical
  , cutTree
  ) where

import qualified Data.Vector                  as V
import qualified Data.Vector.Mutable          as MV
import qualified Data.Vector.Unboxed.Mutable  as MU
import qualified Numeric.LinearAlgebra        as LA
import           Control.Monad                (forM_, when)
import           Control.Monad.ST             (runST)
import           Data.STRef                   (newSTRef, readSTRef, writeSTRef,
                                               modifySTRef')
import           Data.List                    (foldl')

-- ===========================================================================
-- 型
-- ===========================================================================

data Linkage = Single | Complete | Average | Ward
             deriving (Show, Eq)

data HClusterFit = HClusterFit
  { hcMerges       :: ![(Int, Int)]  -- ^ マージ列 (n-1 個)。 ID は 0..n-1 が元サンプル、
                                     --   以降 n, n+1, ... が新クラスタ
  , hcHeights      :: ![Double]      -- ^ マージ時点での距離 (linkage に応じた値)
  , hcLinkage      :: !Linkage
  , hcNumOriginals :: !Int           -- ^ n_samples
  } deriving (Show)

-- ===========================================================================
-- fit
-- ===========================================================================

-- | 階層クラスタリングを fit する。 X は n × p 行列、 各行が 1 サンプル。
fitHierarchical :: Linkage -> LA.Matrix Double -> HClusterFit
fitHierarchical link xs =
  let n = LA.rows xs
      d0 = initialDistance link xs
  in agglomerate link n d0

-- | 樹形図を K クラスタに切り、 各サンプルのクラスタ ID を返す。
--   K = 1 → 全サンプル ID 0; K = n → 全サンプル別 ID。
cutTree :: HClusterFit -> Int -> V.Vector Int
cutTree fit k
  | k <= 0 = V.replicate (hcNumOriginals fit) 0
  | k >= n = V.generate n id
  | otherwise =
      let nMerges = n - k     -- K クラスタにするには n-K 回マージを適用
          mergesUsed = take nMerges (hcMerges fit)
          -- union-find 風: parent[i] = root cluster representative
          parents = runST $ do
            arr <- MV.replicate (2 * n) (-1 :: Int)
            forM_ [0 .. n - 1] $ \i -> MV.write arr i i
            forM_ (zip [n ..] mergesUsed) $ \(newId, (a, b)) -> do
              ra <- findRoot arr a
              rb <- findRoot arr b
              MV.write arr ra newId
              MV.write arr rb newId
              MV.write arr newId newId
            V.generateM n (findRoot arr)
          uniqRoots = foldr (\r acc -> if r `elem` acc then acc else r:acc) [] (V.toList parents)
          roots = zip uniqRoots [0 ..]
          lookupId r = case lookup r roots of
            Just i  -> i
            Nothing -> 0
      in V.map lookupId parents
  where
    n = hcNumOriginals fit
    findRoot arr i = do
      p <- MV.read arr i
      if p == i then pure i else findRoot arr p

-- ===========================================================================
-- 内部: 距離行列の構築
-- ===========================================================================

-- | 初期距離行列 (n × n)。 二乗ユークリッド距離。
--   Ward は二乗距離を使うのが定義どおり。 他 linkage は √ を取って通常距離にする。
initialDistance :: Linkage -> LA.Matrix Double -> LA.Matrix Double
initialDistance link xs =
  let n = LA.rows xs
      sqDist i j =
        let r = LA.flatten (xs LA.? [i]) - LA.flatten (xs LA.? [j])
        in LA.sumElements (r * r)
      raw = LA.build (n, n)
              (\i j -> sqDist (round i) (round j) :: Double)
  in case link of
       Ward -> raw           -- squared
       _    -> LA.cmap sqrt raw

-- ===========================================================================
-- 内部: 凝集アルゴリズム
-- ===========================================================================

agglomerate :: Linkage -> Int -> LA.Matrix Double -> HClusterFit
agglomerate link n d0 = runST $ do
  -- Phase 17.2 改善:
  --   * 距離行列を MU (Unboxed Mutable Vector Double) で flat 配列に
  --   * active set を Unboxed Mutable Vector Int でコンパクトに保持
  --     (毎ステップ tail 切詰めの代わりに、 in-place で a,b 位置を最後と入替え)
  --   * unsafeRead / unsafeWrite で境界チェック排除
  --   * inner loop の STRef 更新を local accumulator (Int * 2 + Double) で減らす
  let !totalIds = 2 * n - 1
  dist  <- MU.unsafeNew (totalIds * totalIds)
  -- 初期化: ∞
  forM_ [0 .. totalIds * totalIds - 1] $ \k -> MU.unsafeWrite dist k (1/0 :: Double)
  sizes <- MU.replicate totalIds (1 :: Int)
  forM_ [0 .. n - 1] $ \i ->
    forM_ [0 .. n - 1] $ \j ->
      when (i /= j) $
        MU.unsafeWrite dist (i * totalIds + j) (LA.atIndex d0 (i, j))
  -- active: 先頭 `activeLen` 要素が active な ID
  active <- MU.unsafeNew totalIds
  forM_ [0 .. n - 1] $ \i -> MU.unsafeWrite active i i
  activeLenRef <- newSTRef n
  mergesRef    <- newSTRef ([] :: [(Int, Int)])
  heightsRef   <- newSTRef ([] :: [Double])
  forM_ [0 .. n - 2] $ \step -> do
    let !nextId = n + step
    !alen <- readSTRef activeLenRef
    -- find argmin。 active[0 .. alen-1] のペアを直接走査
    bestRef <- newSTRef ((-1) :: Int, (-1) :: Int, 1/0 :: Double, (-1) :: Int, (-1) :: Int)
    -- (a, b, bestDist, posA, posB)  posA/posB は active 内の位置
    forM_ [0 .. alen - 2] $ \pi_ -> do
      !i <- MU.unsafeRead active pi_
      forM_ [pi_ + 1 .. alen - 1] $ \pj -> do
        !j <- MU.unsafeRead active pj
        !d <- MU.unsafeRead dist (i * totalIds + j)
        (_, _, !best, _, _) <- readSTRef bestRef
        when (d < best) $ writeSTRef bestRef (i, j, d, pi_, pj)
    (!a, !b, !h, !pa, !pb) <- readSTRef bestRef
    modifySTRef' mergesRef  ((a, b) :)
    modifySTRef' heightsRef ((reportHeight link h) :)
    !na <- MU.unsafeRead sizes a
    !nb <- MU.unsafeRead sizes b
    MU.unsafeWrite sizes nextId (na + nb)
    -- active から a, b を削除し nextId を追加: pb を末尾と swap で除去、
    -- 同様に pa を新末尾と swap、 alen 減 2、 末尾に nextId を入れて alen 増 1
    -- ※ pa < pb 不変 (内側 loop が pj > pi)
    !lastPos <- pure (alen - 1)
    !valLast <- MU.unsafeRead active lastPos
    MU.unsafeWrite active pb valLast
    !secondLast <- pure (alen - 2)
    !valSecond <- MU.unsafeRead active secondLast
    -- pa の位置は pb と入替えで動いていない (pa < pb なので)
    MU.unsafeWrite active pa valSecond
    MU.unsafeWrite active secondLast nextId
    writeSTRef activeLenRef (alen - 1)  -- 2 削除 + 1 追加 = -1
    !alenNew <- readSTRef activeLenRef
    -- Lance-Williams update: active[0 .. alenNew - 1] (末尾は nextId)
    let !nextRow = nextId * totalIds
    forM_ [0 .. alenNew - 2] $ \pk -> do
      !k <- MU.unsafeRead active pk
      !dak <- MU.unsafeRead dist (a * totalIds + k)
      !dbk <- MU.unsafeRead dist (b * totalIds + k)
      !nk  <- MU.unsafeRead sizes k
      let !dNew = lanceWilliams link (na, nb, nk) dak dbk h
      MU.unsafeWrite dist (nextRow + k) dNew
      MU.unsafeWrite dist (k * totalIds + nextId) dNew
  merges  <- reverse <$> readSTRef mergesRef
  heights <- reverse <$> readSTRef heightsRef
  pure HClusterFit
    { hcMerges       = merges
    , hcHeights      = heights
    , hcLinkage      = link
    , hcNumOriginals = n
    }
  where
    reportHeight Ward h = sqrt (max 0 h)
    reportHeight _    h = h

-- | Lance-Williams recurrence:
--   d(i∪j, k) = α_i d(i,k) + α_j d(j,k) + β d(i,j) + γ |d(i,k) − d(j,k)|
lanceWilliams :: Linkage
              -> (Int, Int, Int)   -- sizes (n_a, n_b, n_k)
              -> Double            -- d(a, k)
              -> Double            -- d(b, k)
              -> Double            -- d(a, b)
              -> Double
lanceWilliams link (na, nb, nk) dak dbk dab =
  case link of
    Single   -> min dak dbk
    Complete -> max dak dbk
    Average  ->
      let naD = fromIntegral na; nbD = fromIntegral nb
      in (naD * dak + nbD * dbk) / (naD + nbD)
    Ward ->
      let naD = fromIntegral na; nbD = fromIntegral nb
          nkD = fromIntegral nk
          tot = naD + nbD + nkD
      in ((naD + nkD) * dak + (nbD + nkD) * dbk - nkD * dab) / tot
