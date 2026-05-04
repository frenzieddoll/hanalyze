{-# LANGUAGE OverloadedStrings #-}
-- | Mixed-level designs.
--
-- Designs in which factors have different numbers of levels. An extension
-- of @Design.Factorial.mixedFactorial@ that accepts an explicit list of
-- level values per factor.
module Design.Mixed
  ( mixedLevelDesign
  , crossDesign
  ) where

import Design.Factorial (fullFactorial)

-- | 各因子に異なる水準値リストを与える混合水準計画。
--
-- 例: 因子 A は (10, 20, 30)、因子 B は (-1, +1)
--   @mixedLevelDesign [[10, 20, 30], [-1, 1]]@
mixedLevelDesign :: [[Double]] -> [[Double]]
mixedLevelDesign = fullFactorial

-- | 2 つの計画行列の直積 (cross design)。
--
-- 例: 全要因設計 A と B を横に結合した複合設計を作る。
crossDesign :: [[Double]] -> [[Double]] -> [[Double]]
crossDesign d1 d2 = [r1 ++ r2 | r1 <- d1, r2 <- d2]
