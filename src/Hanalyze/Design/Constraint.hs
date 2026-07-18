{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Design.Constraint
-- Description : DoE 古典側の設計制約 (線形不等式・禁止行の組合せ) によるフィルタ / 検証
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- DoE 古典側の設計制約 (Phase 23-b)。
--
-- 候補集合ベースの 'Hanalyze.Design.Optimal' に渡す前のフィルタ用途、
-- および手動構築した設計行列の事後検証用途を想定。
--
-- ADT は最小 2 種:
--
--   * 'LinearConstraint' coeffs rel rhs — 線形不等式 / 等式
--     @sum_i (coeffs[i] * x[i]) `rel` rhs@
--   * 'ForbiddenCombination' values — 厳密に一致する row を禁止
--     (浮動小数比較は 'forbiddenTolerance' = 1e-9 で許容)
--
-- 条件付 (If-then) 制約は本モジュールでは扱わない (Custom Design spec 専有、
-- spec/hanalyze-doe-custom-design-spec.md §2.3 / §9 参照)。
--
-- spec: doe-spec v0.2 §2.8 / §3.12。
module Hanalyze.Design.Constraint
  ( ConstraintRel (..)
  , DesignConstraint (..)
  , checkRow
  , checkDesign
  , filterCandidates
  , forbiddenTolerance
  ) where

import qualified Numeric.LinearAlgebra as LA

-- ===========================================================================
-- 型
-- ===========================================================================

-- | 線形制約の関係子。
data ConstraintRel = CLeq | CEq | CGeq
  deriving (Eq, Show)

-- | 設計行列に対する制約。
data DesignConstraint
  = LinearConstraint     ![Double] !ConstraintRel !Double
    -- ^ @sum_i (coeffs[i] * x[i]) `rel` rhs@。 coeffs の長さは row の
    --   次元と一致する必要 ('checkRow' は不一致を即 False として弾く)
  | ForbiddenCombination ![Double]
    -- ^ row がこの値と (許容誤差 'forbiddenTolerance' で) 一致したら違反。
  deriving (Eq, Show)

-- | 'ForbiddenCombination' の浮動小数比較に用いる許容誤差。
forbiddenTolerance :: Double
forbiddenTolerance = 1e-9

-- ===========================================================================
-- 公開 API
-- ===========================================================================

-- | 1 row が全制約を満たすか。 制約違反 (= 不可) なら 'False'。
checkRow :: [DesignConstraint] -> [Double] -> Bool
checkRow cs row = all (rowSatisfies row) cs

-- | 設計行列 (= 各 row が 1 試行) の制約違反 row index を返す。
-- row 数 0 のときは空 list。
checkDesign :: [DesignConstraint] -> LA.Matrix Double -> [Int]
checkDesign cs m =
  let rows = LA.toLists m
  in [ i | (i, r) <- zip [0 ..] rows, not (checkRow cs r) ]

-- | 候補集合から制約違反 row を除去する helper。 'Hanalyze.Design.Optimal'
-- の入力候補を作る前に挟む想定。 順序は保持。
filterCandidates :: [DesignConstraint] -> [[Double]] -> [[Double]]
filterCandidates cs = filter (checkRow cs)

-- ===========================================================================
-- 内部
-- ===========================================================================

-- | 1 row が単一制約を満たすか判定。
rowSatisfies :: [Double] -> DesignConstraint -> Bool
rowSatisfies row (LinearConstraint coeffs rel rhs)
  | length coeffs /= length row = False
  | otherwise =
      let lhs = sum (zipWith (*) coeffs row)
      in case rel of
           CLeq -> lhs <= rhs + forbiddenTolerance
           CEq  -> abs (lhs - rhs) <= forbiddenTolerance
           CGeq -> lhs >= rhs - forbiddenTolerance
rowSatisfies row (ForbiddenCombination vals)
  | length vals /= length row = True   -- 次元不一致 = forbidden ではない
  | otherwise =
      not (and (zipWith (\a b -> abs (a - b) <= forbiddenTolerance) row vals))
