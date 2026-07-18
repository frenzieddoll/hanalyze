{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Data.ColumnSource
-- Description : 列名 → 数値列を引ける「データ源」の最小抽象型クラス (plot 非依存)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 列名 → 数値列 を引ける「データ源」 の最小抽象。
--
-- モデル学習の入口 (Phase 51 の @df |-> spec@) を、 データ表現
-- (@[(Text,[Double])]@ / @Map Text [Double]@ / Hackage @DataFrame@ /
-- plot @ColData@) から疎結合にするための型クラス。 数値列の取得と
-- 列名列挙の 2 メソッドのみを持ち、 factor/NA の解釈は上位
-- (Phase 47 formula 経路) に委ねる。
--
-- このモジュールは **plot 非依存 (portable)**。 plot 専用の
-- @[(Text, ColData)]@ instance は flag @plot-integration@ 配下
-- (@Hanalyze.Plot@) に隔離する。
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

-- | 列名で数値列を引けるデータ源。
--
-- * 'lookupCol' は **数値列**のみを返す (factor 列は formula 経路が
--   contrast 展開するため、 ここでは数値列の素取得に限る)。
-- * 'columnNames' は欠落検出 (要求列が無い) のための全列名列挙。
class ColumnSource d where
  -- | 列名 → 数値列 (無ければ 'Nothing')。
  lookupCol   :: Text -> d -> Maybe [Double]
  -- | 全列名。
  columnNames :: d -> [Text]
  -- | データ源全体を Hackage @DataFrame@ に変換 (formula 経路 = Phase 47 の
  --   @MissingPolicy@\/contrast\/応答列判定で ModelFrame に変換するため)。
  --
  --   既定は **数値列のみから再構築** (assoc\/Map など数値源で正しい)。
  --   'DX.DataFrame' instance は 'id' で上書きし factor\/NA を温存する
  --   (formula 多変量の canonical 経路)。
  toFrame :: d -> DX.DataFrame
  toFrame d = DX.fromNamedColumns
    [ (n, DX.fromList vs)
    | n <- columnNames d, Just vs <- [lookupCol n d] ]

-- ===========================================================================
-- core instance (portable)
-- ===========================================================================

-- | HBM の既存入力 (列名 assoc) と同型。
instance ColumnSource [(Text, [Double])] where
  lookupCol n = lookup n
  columnNames = map fst

-- | 'Map' 版。
instance ColumnSource (Map Text [Double]) where
  lookupCol   = Map.lookup
  columnNames = Map.keys

-- | Hackage @dataframe@ (analyze formula 経路と同じ df)。
--   数値変換は 'getDoubleVec' に委譲 (formula 経路と同じ判定)。
instance ColumnSource DX.DataFrame where
  lookupCol n df = V.toList <$> getDoubleVec n df
  columnNames    = DX.columnNames
  toFrame        = id   -- factor/NA を温存 (formula 経路の canonical)
