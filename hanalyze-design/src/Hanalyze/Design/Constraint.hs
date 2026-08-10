{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Design.Constraint
-- Description : DoE 古典側の設計制約 (線形不等式・禁止行の組合せ) によるフィルタ / 検証
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: DoE 古典側の設計制約。
--
-- 候補集合ベースの 'Hanalyze.Design.Optimal' に渡す前のフィルタ用途、
-- および手動構築した設計行列の事後検証用途を想定。
--
-- ADT は最小 2 種:
--
--   - 'LinearConstraint' coeffs rel rhs — 線形不等式 / 等式
--     @sum_i (coeffs[i] * x[i]) @rel@ rhs@
--   - 'ForbiddenCombination' values — 厳密に一致する row を禁止
--     (浮動小数比較は 'forbiddenTolerance' = 1e-9 で許容)
--
-- 条件付 (If-then) 制約は本モジュールでは扱わない (Custom Design spec 専有、
-- spec/hanalyze-doe-custom-design-spec.md §2.3 / §9 参照)。
--
-- spec: doe-spec v0.2 §2.8 / §3.12。
--
-- [English]: Design constraints on the classical DoE side.
--
-- Intended for use as a filter before candidate sets are passed to
-- 'Hanalyze.Design.Optimal', and for post-hoc verification of
-- manually constructed design matrices.
--
-- The ADT has a minimal 2 variants:
--
--   - 'LinearConstraint' coeffs rel rhs — linear inequality / equality
--     @sum_i (coeffs[i] * x[i]) @rel@ rhs@
--   - 'ForbiddenCombination' values — forbids a row that matches exactly
--     (floating-point comparison is tolerated via 'forbiddenTolerance' = 1e-9)
--
-- Conditional (if-then) constraints are not handled by this module (they
-- belong to the Custom Design spec; see
-- spec/hanalyze-doe-custom-design-spec.md §2.3 / §9).
--
-- spec: doe-spec v0.2 §2.8 / §3.12.
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

-- | [日本語]: 線形制約の関係子。 [English]: The relational operator for a
--   linear constraint.
data ConstraintRel = CLeq | CEq | CGeq
  deriving (Eq, Show)

-- | [日本語]: 設計行列に対する制約。 [English]: A constraint on a design matrix.
data DesignConstraint
  = LinearConstraint     ![Double] !ConstraintRel !Double
    -- ^ [日本語]: @sum_i (coeffs[i] * x[i]) \`rel\` rhs@。 coeffs の長さは row の
    --   次元と一致する必要 ('checkRow' は不一致を即 False として弾く)
    --   [English]: @sum_i (coeffs[i] * x[i]) \`rel\` rhs@. The length of
    --   coeffs must match the row's dimension ('checkRow' rejects a
    --   mismatch immediately as False).
  | ForbiddenCombination ![Double]
    -- ^ [日本語]: row がこの値と (許容誤差 'forbiddenTolerance' で) 一致したら違反。
    --   [English]: Violated when the row matches these values (within the
    --   tolerance 'forbiddenTolerance').
  deriving (Eq, Show)

-- | [日本語]: 'ForbiddenCombination' の浮動小数比較に用いる許容誤差。
--   [English]: The tolerance used for the floating-point comparison in
--   'ForbiddenCombination'.
forbiddenTolerance :: Double
forbiddenTolerance = 1e-9

-- ===========================================================================
-- 公開 API
-- ===========================================================================

-- | [日本語]: 1 row が全制約を満たすか。 制約違反 (= 不可) なら 'False'。
--   [English]: Whether a row satisfies all constraints. 'False' if any
--   constraint is violated (= infeasible).
checkRow :: [DesignConstraint] -> [Double] -> Bool
checkRow cs row = all (rowSatisfies row) cs

-- | [日本語]: 設計行列 (= 各 row が 1 試行) の制約違反 row index を返す。
--   row 数 0 のときは空 list。
--   [English]: Returns the row indices of a design matrix (each row = one
--   trial) that violate a constraint. An empty list when there are 0 rows.
checkDesign :: [DesignConstraint] -> LA.Matrix Double -> [Int]
checkDesign cs m =
  let rows = LA.toLists m
  in [ i | (i, r) <- zip [0 ..] rows, not (checkRow cs r) ]

-- | [日本語]: 候補集合から制約違反 row を除去する helper。 'Hanalyze.Design.Optimal'
--   の入力候補を作る前に挟む想定。 順序は保持。
--   [English]: A helper that removes constraint-violating rows from a
--   candidate set. Intended to be inserted before building the input
--   candidates for 'Hanalyze.Design.Optimal'. Order is preserved.
filterCandidates :: [DesignConstraint] -> [[Double]] -> [[Double]]
filterCandidates cs = filter (checkRow cs)

-- ===========================================================================
-- 内部
-- ===========================================================================

-- | [日本語]: 1 row が単一制約を満たすか判定。
--   [English]: Determines whether a single row satisfies a single constraint.
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
