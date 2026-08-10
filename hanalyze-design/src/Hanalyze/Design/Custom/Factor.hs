{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Design.Custom.Factor
-- Description : Custom Design の Factor 定義 (Role × Kind の直交軸による因子型)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Custom Design の Factor 定義 (skeleton)。
--
-- spec: doe-custom-design-spec v0.1.1 §2.1 / §3.1。
--
-- 「コントロール性 (Role)」 × 「水準型 (Kind)」 の直交軸で 1 型に集約。
-- HardToChange フラグが split-plot を駆動する (未実装・将来のフェーズで対応予定)。
--
-- [English]: Factor definition for Custom Design (skeleton).
--
-- spec: doe-custom-design-spec v0.1.1 §2.1 / §3.1.
--
-- Consolidated into a single type via the orthogonal axes of
-- "controllability (Role)" × "level type (Kind)". The HardToChange flag
-- drives split-plot behavior (not yet implemented; planned for a future
-- phase).
module Hanalyze.Design.Custom.Factor
  ( FactorRole (..)
  , FactorKind (..)
  , Factor (..)
  , factorIsContinuous
  , factorDimension
  ) where

import Data.Text (Text)

-- | [日本語]: 因子の運用上の役割。 [English]: The operational role of a factor.
data FactorRole
  = Controllable
    -- ^ [日本語]: 通常因子 [English]: An ordinary factor.
  | HardToChange
    -- ^ [日本語]: Whole-plot 因子 (split-plot 駆動)
    --   [English]: A whole-plot factor (drives split-plot behavior).
  | VeryHardToChange
    -- ^ [日本語]: Strip-plot 駆動 [English]: Drives strip-plot behavior.
  | Blocking
    -- ^ [日本語]: 既知ブロック [English]: A known block.
  | Covariate
    -- ^ [日本語]: 共変量 (測定可だが操作不可)
    --   [English]: A covariate (measurable but not manipulable).
  | Constant
    -- ^ [日本語]: 固定 (設計には現れず記録のみ)
    --   [English]: A constant (doesn't appear in the design; recorded only).
  | Uncontrolled
    -- ^ [日本語]: ノイズ (Taguchi outer array 由来)
    --   [English]: Noise (originating from a Taguchi outer array).
  deriving (Eq, Show)

-- | [日本語]: 因子の水準型。 [English]: The level type of a factor.
data FactorKind
  = Continuous   !Double !Double
    -- ^ [日本語]: (low, high)、 coded ±1 への正規化対象
    --   [English]: (low, high); normalized to coded ±1.
  | DiscreteNum  ![Double]
    -- ^ [日本語]: 離散水準 (順序あり) [English]: Discrete levels (ordered).
  | Categorical  ![Text]
    -- ^ [日本語]: 順序なしカテゴリ [English]: An unordered category.
  | Ordinal      ![Text]
    -- ^ [日本語]: 順序ありカテゴリ [English]: An ordered category.
  | Mixture      !Double !Double
    -- ^ [日本語]: 混合比制約下の (lower, upper)
    --   [English]: (lower, upper) under a mixture-ratio constraint.
  deriving (Eq, Show)

-- | [日本語]: Factor = 名前 + 水準型 + 役割。
--   [English]: Factor = name + level type + role.
data Factor = Factor
  { fName :: !Text
  , fKind :: !FactorKind
  , fRole :: !FactorRole
  } deriving (Eq, Show)

-- | [日本語]: 連続系 (Continuous / DiscreteNum / Mixture) かどうか。
--   設計行列の展開時に、 categorical 因子の treatment coding 分岐に使う。
--   [English]: Whether the factor is continuous-like (Continuous /
--   DiscreteNum / Mixture). Used when expanding the design matrix, to
--   branch on treatment coding for categorical factors.
factorIsContinuous :: Factor -> Bool
factorIsContinuous f = case fKind f of
  Continuous  _ _ -> True
  DiscreteNum _   -> True
  Mixture     _ _ -> True
  Categorical _   -> False
  Ordinal     _   -> False

-- | [日本語]: Factor の「設計行列に占める列数」 概算 (skeleton 段階の単純実装)。
--
--   - 連続系: 1
--   - Categorical / Ordinal: (水準数 − 1)  ※reference coding
--
--   0 水準 (空 Categorical) は 0 列 (実装側で warn 推奨)。
--   [English]: An estimate of how many columns a Factor occupies in the
--   design matrix (a simple implementation at the skeleton stage).
--
--   - Continuous-like: 1
--   - Categorical / Ordinal: (level count − 1)  (reference coding)
--
--   0 levels (an empty Categorical) yields 0 columns (a warning is
--   recommended on the implementation side).
factorDimension :: Factor -> Int
factorDimension f = case fKind f of
  Continuous  _ _   -> 1
  DiscreteNum _     -> 1
  Mixture     _ _   -> 1
  Categorical xs    -> max 0 (length xs - 1)
  Ordinal     xs    -> max 0 (length xs - 1)
