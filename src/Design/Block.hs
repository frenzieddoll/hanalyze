{-# LANGUAGE OverloadedStrings #-}
-- | ブロック計画 (block designs): ラテン方格、乱塊法。
--
-- - 'latinSquare':         n × n のラテン方格 (n 因子の効率的配置)
-- - 'graecoLatinSquare':   2 つの直交ラテン方格の組
-- - 'randomizedBlock':     乱塊法 (b ブロック × t 処理)
-- - 'shuffleSeq':          擬似乱数で配列をシャッフル (再現性のため seed 指定)
module Design.Block
  ( latinSquare
  , graecoLatinSquare
  , randomizedBlock
  , shuffleSeq
  ) where

import Data.List (foldl')

-- | n × n ラテン方格を返す。値は 1..n。
--
-- 標準形 (cyclic shift):
--   row i, col j → ((i + j) mod n) + 1
latinSquare :: Int -> [[Int]]
latinSquare n
  | n < 1     = []
  | otherwise =
      [ [((i + j) `mod` n) + 1 | j <- [0 .. n - 1]]
      | i <- [0 .. n - 1] ]

-- | グレコラテン方格 (二つの直交ラテン方格のペア)。
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

-- | 乱塊法: b ブロック × t 処理。
--
-- 各ブロック内で処理 1..t をランダム順に並べる。
-- 結果は @[[Int]]@ で、行 = ブロック、列内 = 適用順、値 = 処理 ID。
randomizedBlock :: Int             -- b ブロック数
                -> Int             -- t 処理数
                -> Int             -- ランダムシード
                -> [[Int]]
randomizedBlock b t seed =
  [ shuffleSeq (seed + i * 1000) [1 .. t] | i <- [0 .. b - 1] ]

-- | 配列を Fisher-Yates 法で擬似乱数シャッフル (再現性のため seed 指定)。
-- 単純な線形合同法 RNG を内部使用 (テスト用、暗号学的強度なし)。
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
