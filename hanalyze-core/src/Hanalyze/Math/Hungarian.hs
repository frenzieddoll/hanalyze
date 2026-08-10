{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Math.Hungarian
-- Description : Hungarian (Kuhn-Munkres) 法による正方割当問題の最小コスト解 (O(n³))
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Hungarian (Kuhn-Munkres) アルゴリズムによる正方割当問題の最小コスト解。
--
-- ## 入出力
--
-- 入力: コスト行列 C (n × n、 各成分は実数、 inf 不可)。
-- 出力: 行 i に割当てる列 j からなる長さ n のベクトル @assignment[i] = j@。
-- 目的: Σᵢ C[i, assignment[i]] を最小化、 かつ assignment が **全単射**。
--
-- ## 実装
--
-- e-maxx の "Hungarian algorithm in O(V³)" 系統 (Jonker-Volgenant の
-- shortest augmenting path 方式)。 双対変数 u, v と potential を保持して
-- 1 行ずつ augment する。 ST + mutable Vector で内部状態を管理し、 純関数
-- 'hungarianMin' として API 公開する。
--
-- ## 用途
--
-- ICA-LiNGAM (Shimizu 2006) の行/列順列下三角化で、 W 行列の対角成分絶対値
-- を最大化する割当を求めるのに使う。 コスト C[i, j] = 1 / (|W[i, j]| + ε)
-- で 'hungarianMin' を呼ぶと、 グリーディと違って大域最適解が得られる。
-- p > 10 でグリーディが劣化するケースを救う。
--
-- ## 計算量
--
-- O(n³)。 n ≤ 200 程度では実用上問題なし (測定: n=100 で数十 ms オーダー、
-- 計測値ではなく目安)。
module Hanalyze.Math.Hungarian
  ( hungarianMin
  ) where

import           Control.Monad               (forM_, unless, when)
import           Control.Monad.ST            (ST, runST)
import           Data.STRef
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Numeric.LinearAlgebra       as LA

-- ===========================================================================
-- 公開 API
-- ===========================================================================

-- | 正方コスト行列 C (n × n) に対する最小コスト割当。
--   戻り値 @v@ は @v VU.! i = j@ で「行 i が列 j に割当てられる」 意味。
hungarianMin :: LA.Matrix Double -> VU.Vector Int
hungarianMin cost
  | n == 0    = VU.empty
  | otherwise = runST (runHungarian n cost)
  where
    n = LA.rows cost

-- ===========================================================================
-- 内部実装 (ST monad、 1-indexed の慣例で size n+1 配列を確保)
-- ===========================================================================

runHungarian :: Int -> LA.Matrix Double -> ST s (VU.Vector Int)
runHungarian n cost = do
  let !inf = 1.0e300 :: Double
  u   <- MV.replicate (n + 1) (0 :: Double)
  v   <- MV.replicate (n + 1) (0 :: Double)
  p   <- MV.replicate (n + 1) (0 :: Int)     -- p[j] = 列 j に割当てた行
  way <- MV.replicate (n + 1) (0 :: Int)

  forM_ [1 .. n] $ \i -> do
    MV.write p 0 i
    j0Ref <- newSTRef (0 :: Int)
    minv  <- MV.replicate (n + 1) inf
    used  <- MV.replicate (n + 1) False

    let -- shortest-path-tree 拡張 1 ステップ
        step = do
          j0 <- readSTRef j0Ref
          MV.write used j0 True
          i0 <- MV.read p j0
          deltaRef <- newSTRef inf
          j1Ref    <- newSTRef (0 :: Int)
          forM_ [1 .. n] $ \j -> do
            isU <- MV.read used j
            unless isU $ do
              ui0 <- MV.read u i0
              vj  <- MV.read v j
              let !cur = LA.atIndex cost (i0 - 1, j - 1) - ui0 - vj
              mj <- MV.read minv j
              when (cur < mj) $ do
                MV.write minv j cur
                MV.write way  j j0
              mj' <- MV.read minv j
              d   <- readSTRef deltaRef
              when (mj' < d) $ do
                writeSTRef deltaRef mj'
                writeSTRef j1Ref j
          delta <- readSTRef deltaRef
          forM_ [0 .. n] $ \j -> do
            isU <- MV.read used j
            if isU
              then do
                pj <- MV.read p j
                upj <- MV.read u pj
                MV.write u pj (upj + delta)
                vj <- MV.read v j
                MV.write v j (vj - delta)
              else do
                mj <- MV.read minv j
                MV.write minv j (mj - delta)
          j1 <- readSTRef j1Ref
          writeSTRef j0Ref j1
          pj1 <- MV.read p j1
          when (pj1 /= 0) step
    step

    -- augmenting path に沿って割当を更新
    let aug = do
          j0 <- readSTRef j0Ref
          j1 <- MV.read way j0
          pj1 <- MV.read p j1
          MV.write p j0 pj1
          writeSTRef j0Ref j1
          when (j1 /= 0) aug
    aug

  -- 結果ベクトルを構築: assignment[i-1] = j-1 (p[j] = i ⇒ row i → col j)
  result <- MV.replicate n (0 :: Int)
  forM_ [1 .. n] $ \j -> do
    pj <- MV.read p j
    when (pj >= 1) $ MV.write result (pj - 1) (j - 1)
  VU.freeze result
