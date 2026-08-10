{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Design.Block
-- Description : ブロック計画 (ラテン方格・グレコラテン方格・乱塊法) の生成
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Block designs: Latin squares and randomized complete block designs.
--
--   * 'latinSquare'        — @n × n@ Latin square (efficient arrangement
--     of @n@ treatments).
--   * 'graecoLatinSquare'  — pair of orthogonal Latin squares.
--   * 'randomizedBlock'    — randomized block design (@b@ blocks × @t@
--     treatments).
--   * 'shuffleSeq'         — pseudo-random sequence shuffler (seed-driven
--     for reproducibility).
module Hanalyze.Design.Block
  ( latinSquare
  , graecoLatinSquare
  , randomizedBlock
  , shuffleSeq
  ) where

import Data.List (foldl')

-- | Build an @n × n@ Latin square. Cell values are @1..n@.
--
-- 標準形 (cyclic shift):
--   row i, col j → ((i + j) mod n) + 1
latinSquare :: Int -> [[Int]]
latinSquare n
  | n < 1     = []
  | otherwise =
      [ [((i + j) `mod` n) + 1 | j <- [0 .. n - 1]]
      | i <- [0 .. n - 1] ]

-- | Graeco-Latin square (a pair of orthogonal Latin squares).
-- n が素数のとき構成可能 (n=6 は不可能)。
-- 戻り値は (n × n) のセルごとに (a, b) のペア (両方とも 1..n)。
--
-- 構成: (i + j) mod n と (i + 2j) mod n
graecoLatinSquare :: Int -> Maybe [[(Int, Int)]]
graecoLatinSquare n
  | n < 3 || n == 6 = Nothing
  | otherwise = Just
      [ [ (((i + j)     `mod` n) + 1
          , ((i + 2 * j) `mod` n) + 1)
        | j <- [0 .. n - 1] ]
      | i <- [0 .. n - 1] ]

-- | Randomized complete block design: @b@ blocks of @t@ treatments.
--
-- Within each block, treatments @1..t@ are placed in a randomized order.
-- The result @[[Int]]@ has one row per block; values inside a row are
-- the application order of treatment IDs.
randomizedBlock :: Int             -- ^ Number of blocks @b@.
                -> Int             -- ^ Number of treatments @t@.
                -> Int             -- ^ Random seed.
                -> [[Int]]
randomizedBlock b t seed =
  [ shuffleSeq (seed + i * 1000) [1 .. t] | i <- [0 .. b - 1] ]

-- | Fisher-Yates pseudo-random shuffle (seeded for reproducibility).
-- Uses a simple internal LCG (test-quality only, not cryptographically
-- strong).
shuffleSeq :: Int -> [a] -> [a]
shuffleSeq seed xs =
  let n   = length xs
      lcg s = (s * 1103515245 + 12345) `mod` (2 ^ (31 :: Int))
      seeds = take n (drop 1 (iterate lcg seed))
      -- (rand, original_index) でソート → 擬似シャッフル
      paired = zip seeds xs
      sorted = foldl' insert [] paired
      insert acc p = mergeOne p acc
      mergeOne (k, x) ((k', y) : rest)
        | k < k' = (k, x) : (k', y) : rest
        | otherwise = (k', y) : mergeOne (k, x) rest
      mergeOne p [] = [p]
  in map snd sorted
