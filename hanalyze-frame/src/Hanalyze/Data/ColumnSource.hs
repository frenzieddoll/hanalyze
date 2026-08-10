{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Data.ColumnSource
-- Description : 列名 → 数値列を引ける「データ源」の最小抽象型クラス (plot 非依存)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 列名 → 数値列 を引ける「データ源」 の最小抽象。
--
-- モデル学習の入口 (@df |-> spec@) を、 データ表現
-- (@[(Text,[Double])]@ / @Map Text [Double]@ / Hackage @DataFrame@ /
-- plot @ColData@) から疎結合にするための型クラス。 数値列の取得と
-- 列名列挙の 2 メソッドのみを持ち、 factor/NA の解釈は上位
-- (formula 経路) に委ねる。
--
-- このモジュールは __plot 非依存 (portable)__。 plot 専用の
-- @[(Text, ColData)]@ instance は別パッケージ @hanalyze-plot@ の
-- @Hanalyze.Plot@ (@cabal build --project-file=cabal.project.plot@
-- で build) に隔離する。
--
-- [English]: A minimal abstraction for a "data source" from which a numeric
-- column can be looked up by column name.
--
-- This type class decouples the entry point of model fitting (@df |->
-- spec@) from the underlying data representation (@[(Text,[Double])]@ \/
-- @Map Text [Double]@ \/ the Hackage @DataFrame@ \/ plot's @ColData@). It
-- has only two methods — fetching a numeric column and enumerating column
-- names — and leaves the interpretation of factors\/NA to the upper layer
-- (the formula path).
--
-- This module is __plot-independent (portable)__. The plot-specific
-- @[(Text, ColData)]@ instance is isolated in the separate
-- @hanalyze-plot@ package's @Hanalyze.Plot@ (built via
-- @cabal build --project-file=cabal.project.plot@).
module Hanalyze.Data.ColumnSource
  ( ColumnSource (..)
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Vector     as V
import qualified DataFrame.Internal.Column    as DX
import qualified DataFrame.Internal.DataFrame  as DX

import           Hanalyze.DataIO.Convert (getDoubleVec)

-- ===========================================================================
-- 型クラス
-- ===========================================================================

-- | [日本語]: 列名で数値列を引けるデータ源。
--
--   * 'lookupCol' は __数値列__のみを返す (factor 列は formula 経路が
--     contrast 展開するため、 ここでは数値列の素取得に限る)。
--   * 'columnNames' は欠落検出 (要求列が無い) のための全列名列挙。
--   [English]: A data source from which a numeric column can be looked up
--   by column name.
--
--   * 'lookupCol' returns only __numeric columns__ (factor columns are
--     expanded via contrasts by the formula path, so this is limited to a
--     plain fetch of numeric columns).
--   * 'columnNames' enumerates all column names, used to detect missing
--     columns (a requested column that doesn't exist).
class ColumnSource d where
  -- | [日本語]: 列名 → 数値列 (無ければ 'Nothing')。
  --   [English]: Column name → numeric column (or 'Nothing' if absent).
  lookupCol   :: Text -> d -> Maybe [Double]
  -- | [日本語]: 全列名。
  --   [English]: All column names.
  columnNames :: d -> [Text]
  -- | [日本語]: データ源全体を Hackage @DataFrame@ に変換 (formula 経路が
  --   @MissingPolicy@\/contrast\/応答列判定で ModelFrame に変換するため)。
  --
  --   既定は __数値列のみから再構築__ (assoc\/Map など数値源で正しい)。
  --   @DX.DataFrame@ instance は 'id' で上書きし factor\/NA を温存する
  --   (formula 多変量の canonical 経路)。
  --   [English]: Converts the entire data source into a Hackage
  --   @DataFrame@ (so the formula path can convert it to a ModelFrame via
  --   @MissingPolicy@ \/ contrasts \/ response-column detection).
  --
  --   The default __reconstructs from numeric columns only__ (correct for
  --   numeric sources such as assoc lists \/ Map). The @DX.DataFrame@
  --   instance overrides this with 'id' to preserve factors\/NA (the
  --   canonical path for multivariate formulas).
  toFrame :: d -> DX.DataFrame
  toFrame d = DX.fromNamedColumns
    [ (n, DX.fromList vs)
    | n <- columnNames d, Just vs <- [lookupCol n d] ]

-- ===========================================================================
-- core instance (portable)
-- ===========================================================================

-- | [日本語]: HBM の既存入力 (列名 assoc) と同型。
--   [English]: Isomorphic to HBM's existing input (a column-name assoc list).
instance ColumnSource [(Text, [Double])] where
  lookupCol n = lookup n
  columnNames = map fst

-- | [日本語]: 'Map' 版。
--   [English]: The 'Map' variant.
instance ColumnSource (Map Text [Double]) where
  lookupCol   = Map.lookup
  columnNames = Map.keys

-- | [日本語]: Hackage @dataframe@ (analyze formula 経路と同じ df)。
--   数値変換は 'getDoubleVec' に委譲 (formula 経路と同じ判定)。
--   [English]: The Hackage @dataframe@ (the same df used by analyze's
--   formula path). Numeric conversion is delegated to 'getDoubleVec' (the
--   same judgment as the formula path).
instance ColumnSource DX.DataFrame where
  lookupCol n df = V.toList <$> getDoubleVec n df
  columnNames    = DX.columnNames
  toFrame        = id   -- factor/NA を温存 (formula 経路の canonical)
