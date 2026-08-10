{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Design.Custom.Constraint
-- Description : Custom Design の Constraint 内部正規化形 (Coordinate Exchange 用の候補行フィルタ ADT)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Custom Design の Constraint 内部正規化形 (Phase 24-1 skeleton)。
--
-- spec: doe-custom-design-spec v0.1.1 §2.3 / §9.3。
--
-- **本 skeleton では「内部 ADT (正規化済形)」 のみを定義する**。
-- ∀LIC∃Code 表面構文 (= @RawConstraint@ newtype around @ExprRep@) と
-- @normalize :: RawConstraint -> Constraint@ は、 DSL frontend パッケージ
-- (現状 @フロントエンド app のバックエンド/@ にある) との依存関係を整理してから着手する
-- (= Phase 24 後続 commit 候補)。
--
-- 現在の利用想定: Coordinate Exchange アルゴリズム (本 Phase 後続 commit)
-- が ADT を inspect して候補 grid を事前 filter するための内部表現。
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

-- | 線形制約の関係子 (Custom 側、 古典 Hanalyze.Design.Constraint と
-- 表現は同じだが名前空間を分けて使う)。
data ConstraintRel = CLeq | CEq | CGeq
  deriving (Eq, Show)

-- | カテゴリ / 数値の混在値 (Forbidden に使う)。
data FactorValue
  = FVDouble !Double
  | FVText   !Text
  deriving (Eq, Show)

-- | 条件付制約のガード (AND/OR/単項、 NOT は v0.2 検討)。
data ConstraintGuard
  = GuardEq  !Text !FactorValue
  | GuardLeq !Text !Double
  | GuardGeq !Text !Double
  | GuardAnd ![ConstraintGuard]
  | GuardOr  ![ConstraintGuard]
  deriving (Eq, Show)

-- | Custom Design 内部の正規化済 Constraint。
--
-- 連続因子 (因子名で参照) の半空間 / 等式 / カテゴリ列の forbidden /
-- 条件付 / 範囲上書きを覆う。 表面 ∀LIC∃Code Expr からの正規化失敗時の
-- @Generic@ (= ExprRep 抱え込み) は本 skeleton では未対応 (DSL frontend
-- 依存解決後に追加)。
data Constraint
  = LinearIneq  ![(Text, Double)] !ConstraintRel !Double
    -- ^ @sum_i (coef_i * x_{name_i}) `rel` rhs@ 連続因子のみ参照可
  | Forbidden   ![(Text, FactorValue)]
    -- ^ 全項が一致する row を禁止 (AND)
  | Conditional !ConstraintGuard ![Constraint]
    -- ^ ガード成立時のみ inner 制約を活性化
  | RangeBound  !Text !Double !Double
    -- ^ 範囲上書き (低、 高)
  deriving (Eq, Show)

-- | 1 row (= 因子名 → 値の Map) に対する制約評価。
-- skeleton では Categorical 因子は Text 値で照合、 連続因子は Double で照合。
-- 値が見つからない / 型不一致は **その制約を 'False' (= 違反) と判定**。
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

-- | ガード評価。
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

-- | ヘルパ: 因子名リストと 1 row 値リスト (= Double のみの場合) から Map に変換。
-- Custom Design Core が coordinate exchange の inner loop で使う想定。
compileRowFromFactors :: [Text] -> [Double] -> M.Map Text FactorValue
compileRowFromFactors names values =
  M.fromList (zip names (map FVDouble values))
