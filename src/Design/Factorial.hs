{-# LANGUAGE OverloadedStrings #-}
-- | Factorial designs.
--
--   * 'fullFactorial'        — full factorial with @k@ factors each at
--     @levels[i]@ levels.
--   * 'twoLevelFactorial'    — @2^k@ design (each factor at @±1@).
--   * 'threeLevelFactorial'  — @3^k@ design (each factor at @-1, 0, +1@).
--   * 'fractionalFactorial'  — @2^(k-p)@ fractional design (specified
--     defining relation).
--   * 'mixedFactorial'       — mixed-level design (e.g. @2² × 3¹@).
--
-- All designs are returned as @[[Double]]@. Use 'Design.Quality' to
-- evaluate orthogonality and other criteria.
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

-- | Full factorial design: take a list of per-factor level vectors
-- @[lvl_1, lvl_2, …]@ and emit every combination (Cartesian product).
--
-- Example: @fullFactorial [[1,2,3], [10,20]]@ →
-- @[[1,10],[1,20],[2,10],[2,20],[3,10],[3,20]]@.
fullFactorial :: [[Double]] -> [[Double]]
fullFactorial = foldl' addCol [[]]
  where
    addCol acc levels =
      [ row ++ [v] | row <- acc, v <- levels ]

-- | @2^k@ design — each factor takes the levels @-1, +1@.
-- @twoLevelFactorial 3@ has 8 rows × 3 columns.
twoLevelFactorial :: Int -> [[Double]]
twoLevelFactorial k = fullFactorial (replicate k [-1, 1])

-- | @3^k@ design — each factor takes the levels @-1, 0, +1@.
threeLevelFactorial :: Int -> [[Double]]
threeLevelFactorial k = fullFactorial (replicate k [-1, 0, 1])

-- ---------------------------------------------------------------------------
-- 部分要因計画 (Fractional factorial)
-- ---------------------------------------------------------------------------

-- | @2^(k-p)@ fractional factorial design.
--
-- @fractionalFactorial k generators@:
--
--   * @k@         — total number of factors.
--   * @generators@ — defining relations for the added factors
--     @k-p+1, …, k@. Each generator is a set of base-factor indices
--     (1-based, in @1..k-p@); the corresponding column is their product.
--
-- Example: @2^(4-1)@ design (4 factors, one generator) with
-- @D = ABC@: @fractionalFactorial 4 [[1,2,3]]@ → @2^3 = 8@ rows × 4
-- columns (the @D@ column is @A·B·C@). The number of generators equals
-- the number of added factors @p@.
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

-- | Mixed-level design (factors with different numbers of levels).
--
-- Example: @2² × 3¹@ → @mixedFactorial [2, 2, 3]@. Each factor uses
-- evenly-spaced levels (@-1, +1@ or @-1, 0, +1@).
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

-- | Generate factor labels @[\"A\", \"B\", \"C\", …]@.
factorialColumnNames :: Int -> [Text]
factorialColumnNames k
  | k <= 26   = [T.singleton c | c <- take k ['A' ..]]
  | otherwise =
      [T.pack ("X" ++ show i) | i <- [1 .. k] :: [Int]]
