{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Design.Mixture
-- Description : 配合計画 (Mixture Design) — 成分比合計 = 1 制約下の Simplex Lattice / Centroid
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 配合計画 (Mixture Design) — 成分比の合計が常に 1 となる制約下の DoE。
--
-- 材料 / 化学プロセス向け。 各実験点は @[x_1, ..., x_m]@ で
-- @x_i ≥ 0@、 @Σ x_i = 1@ を満たす。
--
-- 提供方式:
--
--   - 'SimplexLattice' @d@ — 各成分が @{0, 1/d, ..., d/d}@ から値を取り、 合計が
--     1 になる全組合せ。 点数 = @C(m+d−1, d)@
--   - 'SimplexCentroid' — 1 ≤ k ≤ m について、 任意 k 成分を均等に @1/k@、 他は 0。
--     点数 = @2^m − 1@
--
-- 制約付き Extreme Vertices design は将来対応予定。
--
-- [English]: Mixture Design — DoE under the constraint that component
-- proportions always sum to 1.
--
-- For material \/ chemical-process use. Each experimental point is
-- @[x_1, ..., x_m]@ satisfying @x_i ≥ 0@, @Σ x_i = 1@.
--
-- Provided schemes:
--
--   - 'SimplexLattice' @d@ — every combination where each component takes a
--     value from @{0, 1/d, ..., d/d}@ and they sum to 1. Number of points =
--     @C(m+d−1, d)@
--   - 'SimplexCentroid' — for 1 ≤ k ≤ m, any k components are set evenly to
--     @1/k@, the rest to 0. Number of points = @2^m − 1@
--
-- A constrained Extreme Vertices design is planned for future support.
module Hanalyze.Design.Mixture
  ( MixtureDesignType (..)
  , MixtureResult (..)
  , mixtureDesign
  ) where

import qualified Numeric.LinearAlgebra as LA
import           Data.Text             (Text)
import qualified Data.Text             as T

-- ===========================================================================
-- 型
-- ===========================================================================

-- | [日本語]: Mixture design の種別。
--   [English]: The kind of mixture design.
data MixtureDesignType
  = SimplexLattice !Int  -- ^ [日本語]: 次数 d。 各成分は @{0, 1/d, ..., 1}@ のいずれかの値 [English]: Degree d. Each component takes a value from @{0, 1/d, ..., 1}@
  | SimplexCentroid      -- ^ [日本語]: 2^m - 1 点 (頂点 + 辺中点 + ... + 全体重心) [English]: 2^m - 1 points (vertices + edge midpoints + ... + overall centroid)
  deriving (Show, Eq)

-- | [日本語]: Mixture design の結果。
--   [English]: The result of a mixture design.
data MixtureResult = MixtureResult
  { mdMatrix      :: !(LA.Matrix Double)
    -- ^ [日本語]: @nRuns × m@ 行列。 各行の合計 = 1、 各要素 ∈ @[0, 1]@
    --   [English]: An @nRuns x m@ matrix. Each row sums to 1, each element
    --   is in @[0, 1]@.
  , mdNComponents :: !Int               -- ^ [日本語]: m (成分数) [English]: m (the number of components)
  , mdNRuns       :: !Int               -- ^ [日本語]: 実験数 [English]: The number of runs
  , mdType        :: !MixtureDesignType -- ^ [日本語]: 入力の種別を保持 [English]: Retains the input's kind
  } deriving (Show)

-- ===========================================================================
-- 公開関数
-- ===========================================================================

-- | [日本語]: Mixture design を生成。
--
--   失敗条件:
--
--     - 成分数 m < 2 → 'Left'
--     - SimplexLattice の次数 d < 1 → 'Left'
--   [English]: Generates a mixture design.
--
--   Failure conditions:
--
--     - Number of components m < 2 -> 'Left'
--     - SimplexLattice degree d < 1 -> 'Left'
mixtureDesign :: MixtureDesignType -> Int -> Either Text MixtureResult
mixtureDesign typ m
  | m < 2 = Left (T.pack ("mixtureDesign: need m >= 2 components, got m=" <> show m))
  | otherwise = case typ of
      SimplexLattice d
        | d < 1 -> Left (T.pack ("mixtureDesign SimplexLattice: need d >= 1, got d=" <> show d))
        | otherwise ->
            let pts = simplexLatticePoints m d
                mat = LA.fromLists pts
            in Right MixtureResult
                 { mdMatrix      = mat
                 , mdNComponents = m
                 , mdNRuns       = length pts
                 , mdType        = typ
                 }
      SimplexCentroid ->
        let pts = simplexCentroidPoints m
            mat = LA.fromLists pts
        in Right MixtureResult
             { mdMatrix      = mat
             , mdNComponents = m
             , mdNRuns       = length pts
             , mdType        = typ
             }

-- ===========================================================================
-- 内部: Simplex Lattice
-- ===========================================================================

-- | [日本語]: Simplex Lattice {m, d} の全点。
--
--   非負整数 m-tuple (n_1, ..., n_m) で sum = d を満たす全組合せを列挙し、
--   各点を (n_1/d, ..., n_m/d) に正規化。
--   [English]: All points of the Simplex Lattice {m, d}.
--
--   Enumerates all non-negative integer m-tuples (n_1, ..., n_m) with
--   sum = d, then normalizes each point to (n_1/d, ..., n_m/d).
simplexLatticePoints :: Int -> Int -> [[Double]]
simplexLatticePoints m d =
  let intTuples = compositions m d
      scale = 1 / fromIntegral d :: Double
  in [ map ((scale *) . fromIntegral) t | t <- intTuples ]

-- | [日本語]: 非負整数 m-tuple (n_1, ..., n_m) で sum = total を満たすもの全列挙。
--   [English]: Enumerates all non-negative integer m-tuples
--   (n_1, ..., n_m) with sum = total.
compositions :: Int -> Int -> [[Int]]
compositions 0 0     = [[]]
compositions 0 _     = []
compositions m total =
  [ k : rest
  | k <- [0 .. total]
  , rest <- compositions (m - 1) (total - k)
  ]

-- ===========================================================================
-- 内部: Simplex Centroid
-- ===========================================================================

-- | [日本語]: Simplex Centroid (m components) の全点。
--
--   1 ≤ k ≤ m について、 m 成分から任意の k 個を選び、 その k 成分だけ @1/k@、
--   他は 0 とする点を作る。 合計 @2^m - 1@ 点。
--   [English]: All points of the Simplex Centroid (m components).
--
--   For 1 ≤ k ≤ m, chooses any k of the m components and builds a point
--   where only those k components are @1/k@ and the rest are 0. Total
--   @2^m - 1@ points.
simplexCentroidPoints :: Int -> [[Double]]
simplexCentroidPoints m =
  [ centroidPointFromSubset m subset
  | k <- [1 .. m]
  , subset <- choose [0 .. m - 1] k
  ]

-- | [日本語]: サブセット (= component の index list、 size = k) から centroid 点を構築。
--   各位置 i が subset に含まれていれば @1/k@、 含まれていなければ 0。
--   [English]: Builds the centroid point from a subset (a component index
--   list of size k). Each position i is @1/k@ if it is in the subset, 0
--   otherwise.
centroidPointFromSubset :: Int -> [Int] -> [Double]
centroidPointFromSubset m subset =
  let k    = length subset
      val  = 1 / fromIntegral k :: Double
  in [ if i `elem` subset then val else 0 | i <- [0 .. m - 1] ]

-- | [日本語]: 長さ k の組合せを全列挙。 順序は lexicographic。
--   [English]: Enumerates all combinations of length k, in lexicographic
--   order.
choose :: [a] -> Int -> [[a]]
choose _      0 = [[]]
choose []     _ = []
choose (x:xs) k =
  map (x :) (choose xs (k - 1)) ++ choose xs k
