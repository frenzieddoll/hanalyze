{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Design.Mixed
-- Description : 因子ごとに異なる水準数を持つ混合水準計画の生成
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Mixed-level designs.
--
-- Designs in which factors have different numbers of levels. An extension
-- of @Hanalyze.Design.Factorial.mixedFactorial@ that accepts an explicit list of
-- level values per factor.
module Hanalyze.Design.Mixed
  ( mixedLevelDesign
  , crossDesign
  ) where

import Hanalyze.Design.Factorial (fullFactorial)

-- | Mixed-level design where the user supplies an explicit list of
-- level values per factor.
--
-- Example: factor A on @(10, 20, 30)@ and factor B on @(-1, +1)@:
-- @mixedLevelDesign [[10, 20, 30], [-1, 1]]@.
mixedLevelDesign :: [[Double]] -> [[Double]]
mixedLevelDesign = fullFactorial

-- | Cross product of two design matrices (cross design).
--
-- Useful e.g. for combining two full factorial designs side-by-side.
crossDesign :: [[Double]] -> [[Double]] -> [[Double]]
crossDesign d1 d2 = [r1 ++ r2 | r1 <- d1, r2 <- d2]
