{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Design.Custom.Factor
-- Description : Custom Design の Factor 定義 (Role × Kind の直交軸による因子型)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Custom Design の Factor 定義 (Phase 24-1 skeleton)。
--
-- spec: doe-custom-design-spec v0.1.1 §2.1 / §3.1。
--
-- 「コントロール性 (Role)」 × 「水準型 (Kind)」 の直交軸で 1 型に集約。
-- HardToChange フラグが split-plot を駆動 (Phase 25 で実装)。
module Hanalyze.Design.Custom.Factor
  ( FactorRole (..)
  , FactorKind (..)
  , Factor (..)
  , factorIsContinuous
  , factorDimension
  ) where

import Data.Text (Text)

-- | 因子の運用上の役割。
data FactorRole
  = Controllable      -- ^ 通常因子
  | HardToChange      -- ^ Whole-plot 因子 (split-plot 駆動)
  | VeryHardToChange  -- ^ Strip-plot 駆動
  | Blocking          -- ^ 既知ブロック
  | Covariate         -- ^ 共変量 (測定可だが操作不可)
  | Constant          -- ^ 固定 (設計には現れず記録のみ)
  | Uncontrolled      -- ^ ノイズ (Taguchi outer array 由来)
  deriving (Eq, Show)

-- | 因子の水準型。
data FactorKind
  = Continuous   !Double !Double         -- ^ (low, high)、 coded ±1 への正規化対象
  | DiscreteNum  ![Double]               -- ^ 離散水準 (順序あり)
  | Categorical  ![Text]                 -- ^ 順序なしカテゴリ
  | Ordinal      ![Text]                 -- ^ 順序ありカテゴリ
  | Mixture      !Double !Double         -- ^ 混合比制約下の (lower, upper)
  deriving (Eq, Show)

-- | Factor = 名前 + 水準型 + 役割。
data Factor = Factor
  { fName :: !Text
  , fKind :: !FactorKind
  , fRole :: !FactorRole
  } deriving (Eq, Show)

-- | 連続系 (Continuous / DiscreteNum / Mixture) かどうか。
-- 設計行列の展開時に、 categorical 因子の treatment coding 分岐に使う。
factorIsContinuous :: Factor -> Bool
factorIsContinuous f = case fKind f of
  Continuous  _ _ -> True
  DiscreteNum _   -> True
  Mixture     _ _ -> True
  Categorical _   -> False
  Ordinal     _   -> False

-- | Factor の「設計行列に占める列数」 概算 (skeleton 段階の単純実装)。
-- - 連続系: 1
-- - Categorical / Ordinal: (水準数 − 1)  ※reference coding
-- 0 水準 (空 Categorical) は 0 列 (実装側で warn 推奨)。
factorDimension :: Factor -> Int
factorDimension f = case fKind f of
  Continuous  _ _   -> 1
  DiscreteNum _     -> 1
  Mixture     _ _   -> 1
  Categorical xs    -> max 0 (length xs - 1)
  Ordinal     xs    -> max 0 (length xs - 1)
