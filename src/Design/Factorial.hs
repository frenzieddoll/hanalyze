{-# LANGUAGE OverloadedStrings #-}
-- | 要因計画 (factorial designs)。
--
-- - 'fullFactorial':      k 因子それぞれが levels[i] 水準を持つ全組合せ
-- - 'twoLevelFactorial':  2^k 計画 (各因子 ±1)
-- - 'threeLevelFactorial': 3^k 計画 (各因子 -1, 0, +1)
-- - 'fractionalFactorial': 2^(k-p) 部分要因計画 (defining relation 指定)
-- - 'mixedFactorial':     混合水準計画 (e.g. 2² × 3¹)
--
-- 設計行列を `[[Double]]` で返し、別途 `Design.Quality` で直交性等を評価。
module Design.Factorial
  ( fullFactorial
  , twoLevelFactorial
  , threeLevelFactorial
  , fractionalFactorial
  , mixedFactorial
  , factorialColumnNames
  ) where

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- 完全要因計画
-- ---------------------------------------------------------------------------

-- | 各因子の水準値 [lvl_1, lvl_2, ...] のリストから、全組合せ
-- (デカルト積) を行列として生成する。
--
-- 例: @fullFactorial [[1,2,3], [10,20]]@ →
--     @[[1,10],[1,20],[2,10],[2,20],[3,10],[3,20]]@
fullFactorial :: [[Double]] -> [[Double]]
fullFactorial = foldl' addCol [[]]
  where
    addCol acc levels =
      [ row ++ [v] | row <- acc, v <- levels ]

-- | 2^k 計画: 各因子は -1, +1 の 2 水準。
--
-- @twoLevelFactorial 3@ → 8 行 × 3 列
twoLevelFactorial :: Int -> [[Double]]
twoLevelFactorial k = fullFactorial (replicate k [-1, 1])

-- | 3^k 計画: 各因子は -1, 0, +1 の 3 水準。
threeLevelFactorial :: Int -> [[Double]]
threeLevelFactorial k = fullFactorial (replicate k [-1, 0, 1])

-- ---------------------------------------------------------------------------
-- 部分要因計画 (Fractional factorial)
-- ---------------------------------------------------------------------------

-- | 2^(k-p) 部分要因計画。
--
-- @fractionalFactorial k generators@:
--   * @k@ — 全因子数
--   * @generators@ — 追加因子 (k-p+1, ..., k) の定義関係。
--     各 generator は基本因子 (1..k-p) のインデックス集合 (1-based)
--     で「これらの積」を取る。
--
-- 例: 2^(4-1) (4 因子、1 個の generator) で D = ABC とする:
--   @fractionalFactorial 4 [[1,2,3]]@
--   → 2^3 = 8 行、4 列 (D 列は A*B*C)
--
-- 注: generator は (k-p+1) 個必要 (= 追加因子の数)。
fractionalFactorial :: Int -> [[Int]] -> [[Double]]
fractionalFactorial k generators =
  let p     = length generators
      kBase = k - p
      base  = twoLevelFactorial kBase
      -- 各 generator から追加列を計算
      extraCol gen row = foldl' (*) 1.0 [row !! (i - 1) | i <- gen]
      addExtras row = row ++ [extraCol gen row | gen <- generators]
  in map addExtras base

-- ---------------------------------------------------------------------------
-- 混合水準計画
-- ---------------------------------------------------------------------------

-- | 混合水準計画。各因子の水準数が異なる場合に使う。
--
-- 例: 2² × 3¹ → @mixedFactorial [2, 2, 3]@
--   各因子は等間隔の水準 (-1, +1) または (-1, 0, +1)
mixedFactorial :: [Int] -> [[Double]]
mixedFactorial levelCounts =
  fullFactorial (map standardLevels levelCounts)
  where
    standardLevels n
      | n <= 1    = [0]
      | n == 2    = [-1, 1]
      | otherwise =
          let step = 2 / fromIntegral (n - 1)
          in [-1 + fromIntegral i * step | i <- [0 .. n - 1] :: [Int]]

-- ---------------------------------------------------------------------------
-- 列名生成
-- ---------------------------------------------------------------------------

-- | 因子名 ["A", "B", "C", ...] を生成する。
factorialColumnNames :: Int -> [Text]
factorialColumnNames k
  | k <= 26   = [T.singleton c | c <- take k ['A' ..]]
  | otherwise =
      [T.pack ("X" ++ show i) | i <- [1 .. k] :: [Int]]
