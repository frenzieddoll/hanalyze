{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Design.Custom.Constraint
-- Description : Custom Design の Constraint 内部正規化形 (Coordinate Exchange 用の候補行フィルタ ADT)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Custom Design の Constraint 内部正規化形 (skeleton)。
--
-- spec: doe-custom-design-spec v0.1.1 §2.3 / §9.3。
--
-- __本 skeleton では「内部 ADT (正規化済形)」 のみを定義する__。
-- ∀LIC∃Code 表面構文 (= @RawConstraint@ newtype around @ExprRep@) と
-- @normalize :: RawConstraint -> Constraint@ は、 DSL frontend パッケージ
-- (現状 @CanvasApp/backend/@ にある) との依存関係を整理してから着手する
-- (未着手・後続コミットの候補)。
--
-- 現在の利用想定: Coordinate Exchange アルゴリズム (後続コミットで実装予定)
-- が ADT を inspect して候補 grid を事前 filter するための内部表現。
--
-- [English]: The internal normalized form of a Custom Design Constraint
-- (skeleton).
--
-- spec: doe-custom-design-spec v0.1.1 §2.3 / §9.3.
--
-- __This skeleton defines only the "internal ADT (normalized form)"__.
-- The ∀LIC∃Code surface syntax (= a @RawConstraint@ newtype around
-- @ExprRep@) and @normalize :: RawConstraint -> Constraint@ will be
-- tackled once the dependency on the DSL frontend package (currently in
-- @CanvasApp/backend/@) is sorted out (not yet started; a candidate
-- for a follow-up commit).
--
-- Intended current use: an internal representation the Coordinate
-- Exchange algorithm (to be implemented in a follow-up commit) can
-- inspect the ADT with, to pre-filter the candidate grid.
module Hanalyze.Design.Custom.Constraint
  ( ConstraintRel (..)
  , FactorValue (..)
  , ConstraintGuard (..)
  , Constraint (..)
  , checkRowAgainst
  , compileRowFromFactors
  ) where

import           Data.Text (Text)
import qualified Data.Map.Strict as M

-- | [日本語]: 線形制約の関係子 (Custom 側、 古典 Hanalyze.Design.Constraint と
--   表現は同じだが名前空間を分けて使う)。
--   [English]: The relational operator for a linear constraint (on the
--   Custom side; the same representation as the classical
--   Hanalyze.Design.Constraint, but kept in a separate namespace).
data ConstraintRel = CLeq | CEq | CGeq
  deriving (Eq, Show)

-- | [日本語]: カテゴリ / 数値の混在値 (Forbidden に使う)。
--   [English]: A mixed category/numeric value (used by Forbidden).
data FactorValue
  = FVDouble !Double
  | FVText   !Text
  deriving (Eq, Show)

-- | [日本語]: 条件付制約のガード (AND/OR/単項、 NOT は v0.2 検討)。
--   [English]: The guard of a conditional constraint (AND/OR/unary; NOT is
--   under consideration for v0.2).
data ConstraintGuard
  = GuardEq  !Text !FactorValue
  | GuardLeq !Text !Double
  | GuardGeq !Text !Double
  | GuardAnd ![ConstraintGuard]
  | GuardOr  ![ConstraintGuard]
  deriving (Eq, Show)

-- | [日本語]: Custom Design 内部の正規化済 Constraint。
--
--   連続因子 (因子名で参照) の半空間 / 等式 / カテゴリ列の forbidden /
--   条件付 / 範囲上書きを覆う。 表面 ∀LIC∃Code Expr からの正規化失敗時の
--   @Generic@ (= ExprRep 抱え込み) は本 skeleton では未対応 (DSL frontend
--   依存解決後に追加)。
--   [English]: The normalized Constraint used internally by Custom Design.
--
--   Covers half-spaces / equalities on continuous factors (referenced by
--   name), forbidden combinations on category columns, conditionals, and
--   range overrides. The @Generic@ fallback (= carrying an ExprRep) for
--   when normalization from a surface ∀LIC∃Code Expr fails is not yet
--   supported by this skeleton (to be added once the DSL frontend
--   dependency is resolved).
data Constraint
  = LinearIneq  ![(Text, Double)] !ConstraintRel !Double
    -- ^ [日本語]: @sum_i (coef_i * x_{name_i}) \`rel\` rhs@ 連続因子のみ参照可
    --   [English]: @sum_i (coef_i * x_{name_i}) \`rel\` rhs@; may only
    --   reference continuous factors.
  | Forbidden   ![(Text, FactorValue)]
    -- ^ [日本語]: 全項が一致する row を禁止 (AND)
    --   [English]: Forbids a row where every term matches (AND).
  | Conditional !ConstraintGuard ![Constraint]
    -- ^ [日本語]: ガード成立時のみ inner 制約を活性化
    --   [English]: Activates the inner constraints only when the guard holds.
  | RangeBound  !Text !Double !Double
    -- ^ [日本語]: 範囲上書き (低、 高)
    --   [English]: A range override (low, high).
  deriving (Eq, Show)

-- | [日本語]: 1 row (= 因子名 → 値の Map) に対する制約評価。
--   skeleton では Categorical 因子は Text 値で照合、 連続因子は Double で照合。
--   値が見つからない / 型不一致は __その制約を 'False' (= 違反) と判定__。
--   [English]: Evaluates a constraint against a single row (= a Map from
--   factor name to value). In this skeleton, Categorical factors are
--   matched by Text value and continuous factors by Double. If the value
--   is missing or the type mismatches,
--   __the constraint is judged 'False' (= violated)__.
checkRowAgainst :: M.Map Text FactorValue -> Constraint -> Bool
checkRowAgainst row (LinearIneq coefs rel rhs) =
  let lookupNum k = case M.lookup k row of
                      Just (FVDouble x) -> Just x
                      _                 -> Nothing
      ms = traverse (\(n, c) -> fmap (c *) (lookupNum n)) coefs
  in case ms of
       Nothing -> False
       Just xs ->
         let lhs = sum xs
         in case rel of
              CLeq -> lhs <= rhs + 1e-9
              CEq  -> abs (lhs - rhs) <= 1e-9
              CGeq -> lhs >= rhs - 1e-9
checkRowAgainst row (Forbidden vs) =
  not (all (\(n, v) -> M.lookup n row == Just v) vs)
checkRowAgainst row (Conditional guard cs) =
  if evalGuard row guard
    then all (checkRowAgainst row) cs
    else True
checkRowAgainst row (RangeBound n lo hi) =
  case M.lookup n row of
    Just (FVDouble x) -> lo - 1e-9 <= x && x <= hi + 1e-9
    _                  -> False

-- | [日本語]: ガード評価。 [English]: Evaluates a guard.
evalGuard :: M.Map Text FactorValue -> ConstraintGuard -> Bool
evalGuard row (GuardEq  n v)   = M.lookup n row == Just v
evalGuard row (GuardLeq n c)   = case M.lookup n row of
                                   Just (FVDouble x) -> x <= c + 1e-9
                                   _                 -> False
evalGuard row (GuardGeq n c)   = case M.lookup n row of
                                   Just (FVDouble x) -> x >= c - 1e-9
                                   _                 -> False
evalGuard row (GuardAnd gs)    = all (evalGuard row) gs
evalGuard row (GuardOr  gs)    = any (evalGuard row) gs

-- | [日本語]: ヘルパ: 因子名リストと 1 row 値リスト (= Double のみの場合) から Map に変換。
--   Custom Design Core が coordinate exchange の inner loop で使う想定。
--   [English]: Helper: converts a list of factor names and a single row's
--   values (the all-Double case) into a Map. Intended for use by Custom
--   Design Core in the inner loop of coordinate exchange.
compileRowFromFactors :: [Text] -> [Double] -> M.Map Text FactorValue
compileRowFromFactors names values =
  M.fromList (zip names (map FVDouble values))
