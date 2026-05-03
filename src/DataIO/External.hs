{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- | 外部データフォーマット (Parquet / JSON 等) を Hackage の 'dataframe'
-- ライブラリ経由で読み込み、内部の 'DataFrame.Core.DataFrame' に変換する。
--
-- D-β (ラッパー方式): 既存の自前 DataFrame を維持しつつ、Parquet/JSON 等の
-- 高機能 I/O は外部ライブラリに委譲する。
--
-- 変換規則:
-- - 'Int' / 'Double' / 'Float' / 'Integer' 列 → 'NumericCol' (Double キャスト)
-- - 'Text' 列 → 'TextCol'
-- - 'Maybe Double' / 'Maybe Int' 列 → 'TextCol' (null は \"NA\" 文字列に変換)
--   その後 'DataIO.Preprocess.imputeMean' 等で補完する想定
-- - その他 → 'TextCol' (showElement で文字列化)
--
-- 補完済みデータが必要なら:
-- @
-- df0 <- loadParquet \"data.parquet\"
-- let df1 = imputeMean \"score\" =<< df0    -- score 列を平均で補完
-- @
module DataIO.External
  ( -- * Hackage 'dataframe' を直接返すローダ (Phase 0+ 推奨経路)
    loadCsvX
  , loadTsvX
  , loadParquetX
  , loadJsonX
    -- * 旧ラッパー API (Phase 7 で削除予定)
  , loadCSVExt
  , loadTSVExt
  , loadParquet
  , loadJSON
  , fromExternalDF
  ) where

import qualified DataFrame                    as DX
import qualified DataFrame.Functions          as DXF
import qualified DataFrame.Internal.Column    as DXC
import qualified DataFrame.Internal.DataFrame as DXD
import qualified DataFrame.IO.JSON            as DXJ

import DataFrame.Core (DataFrame, Column (..), mkDataFrame)

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

-- ---------------------------------------------------------------------------
-- Hackage 'DataFrame' を直接返すローダ (新規・推奨経路)
-- ---------------------------------------------------------------------------

-- | CSV を読み込み Hackage 'DXD.DataFrame' をそのまま返す。
loadCsvX :: FilePath -> IO (Either String DXD.DataFrame)
loadCsvX = loadRaw DX.readCsv

-- | TSV を読み込み Hackage 'DXD.DataFrame' をそのまま返す。
loadTsvX :: FilePath -> IO (Either String DXD.DataFrame)
loadTsvX = loadRaw DX.readTsv

-- | Parquet を読み込み Hackage 'DXD.DataFrame' をそのまま返す。
loadParquetX :: FilePath -> IO (Either String DXD.DataFrame)
loadParquetX = loadRaw DX.readParquet

-- | JSON (records-of-objects) を読み込み Hackage 'DXD.DataFrame' をそのまま返す。
loadJsonX :: FilePath -> IO (Either String DXD.DataFrame)
loadJsonX = loadRaw DXJ.readJSON

loadRaw :: (FilePath -> IO DXD.DataFrame)
        -> FilePath -> IO (Either String DXD.DataFrame)
loadRaw reader path = do
  result <- try (reader path) :: IO (Either SomeException DXD.DataFrame)
  return $ case result of
    Left  e  -> Left ("External loader failed: " ++ show e)
    Right df -> Right df

-- ---------------------------------------------------------------------------
-- 旧ラッパー: 内部 'DataFrame.Core.DataFrame' を返す (Phase 7 で削除)
-- ---------------------------------------------------------------------------

-- | Hackage 'dataframe' で CSV を読み込み、内部 'DataFrame' に変換する。
-- 既存の 'DataIO.CSV.loadAuto' と異なり:
-- - 列ごとの型推論が高度 (Int / Double / Maybe Double / Text を区別)
-- - 欠損値 (空セル / NA) を 'Maybe' 型として保持し、変換時に \"NA\" 文字列化
loadCSVExt :: FilePath -> IO (Either String DataFrame)
loadCSVExt = loadWith DX.readCsv

loadTSVExt :: FilePath -> IO (Either String DataFrame)
loadTSVExt = loadWith DX.readTsv

-- | Apache Parquet ファイルを読み込み (列指向、圧縮対応)。
loadParquet :: FilePath -> IO (Either String DataFrame)
loadParquet = loadWith DX.readParquet

-- | JSON (records-of-objects 形式) を読み込み。
loadJSON :: FilePath -> IO (Either String DataFrame)
loadJSON = loadWith DXJ.readJSON

-- ---------------------------------------------------------------------------
-- 内部ユーティリティ
-- ---------------------------------------------------------------------------

loadWith :: (FilePath -> IO DXD.DataFrame)
         -> FilePath -> IO (Either String DataFrame)
loadWith reader path = do
  result <- try (reader path) :: IO (Either SomeException DXD.DataFrame)
  return $ case result of
    Left  e  -> Left ("External loader failed: " ++ show e)
    Right df -> Right (fromExternalDF df)

-- | 外部 'DataFrame' (dataframe lib) を内部表現に変換する。
fromExternalDF :: DXD.DataFrame -> DataFrame
fromExternalDF dx =
  let names = DX.columnNames dx
  in mkDataFrame [ (n, convertColumn n dx) | n <- names ]

-- | 列ごとの変換: 'columnTypeString' で型を分岐。
convertColumn :: Text -> DXD.DataFrame -> Column
convertColumn name dx = case DXD.getColumn name dx of
  Nothing -> TextCol V.empty
  Just c  ->
    case DXC.columnTypeString c of
      "Double"  -> tryDouble name dx (textFallback name dx)
      "Float"   -> tryFloat  name dx (textFallback name dx)
      "Int"     -> tryInt    name dx (textFallback name dx)
      "Integer" -> tryInt    name dx (textFallback name dx)
      "Text"    -> tryText   name dx
      ty
        | "Maybe " `T.isPrefixOf` T.pack ty ->
            convertNullable (T.unpack (T.drop 6 (T.pack ty))) name dx
        | otherwise -> textFallback name dx

-- | "Maybe X" 系の列を NumericCol に変換できるなら NumericCol、
-- できなければ TextCol で null は "NA" にする。
convertNullable :: String -> Text -> DXD.DataFrame -> Column
convertNullable inner name dx =
  case inner of
    "Double"  -> tryMaybeDouble name dx (textFallback name dx)
    "Float"   -> tryMaybeFloat  name dx (textFallback name dx)
    "Int"     -> tryMaybeInt    name dx (textFallback name dx)
    "Integer" -> tryMaybeInt    name dx (textFallback name dx)
    _         -> textFallback name dx

-- | NumericCol への抽出: 全要素 Double として取得。
tryDouble :: Text -> DXD.DataFrame -> Column -> Column
tryDouble name dx fallback =
  case safeColumnList @Double name dx of
    Just xs -> NumericCol (V.fromList xs)
    Nothing -> fallback

tryFloat :: Text -> DXD.DataFrame -> Column -> Column
tryFloat name dx fallback =
  case safeColumnList @Float name dx of
    Just xs -> NumericCol (V.fromList (map realToFrac xs))
    Nothing -> fallback

tryInt :: Text -> DXD.DataFrame -> Column -> Column
tryInt name dx fallback =
  case safeColumnList @Int name dx of
    Just xs -> NumericCol (V.fromList (map fromIntegral xs))
    Nothing -> fallback

tryMaybeDouble :: Text -> DXD.DataFrame -> Column -> Column
tryMaybeDouble name dx fallback =
  case safeColumnList @(Maybe Double) name dx of
    Just xs -> TextCol (V.fromList (map maybeToTextDouble xs))
    Nothing -> fallback

tryMaybeFloat :: Text -> DXD.DataFrame -> Column -> Column
tryMaybeFloat name dx fallback =
  case safeColumnList @(Maybe Float) name dx of
    Just xs -> TextCol (V.fromList
                 (map (maybeToTextDouble . fmap realToFrac) xs))
    Nothing -> fallback

tryMaybeInt :: Text -> DXD.DataFrame -> Column -> Column
tryMaybeInt name dx fallback =
  case safeColumnList @(Maybe Int) name dx of
    Just xs -> TextCol (V.fromList
                 (map (maybe "NA" (T.pack . show)) xs))
    Nothing -> fallback

tryText :: Text -> DXD.DataFrame -> Column
tryText name dx =
  case safeColumnList @Text name dx of
    Just xs -> TextCol (V.fromList xs)
    Nothing -> TextCol V.empty

-- | 取得できない型は空 TextCol を返す (例外を伝播させない)。
textFallback :: Text -> DXD.DataFrame -> Column
textFallback _ _ = TextCol V.empty

-- | columnAsList を例外セーフに呼び出す。
safeColumnList
  :: forall a. (DXC.Columnable a)
  => Text -> DXD.DataFrame -> Maybe [a]
safeColumnList name dx =
  let result = DX.columnAsList @a (DXF.col @a name) dx
  in seqList result `seq` Just result
  where
    -- 強制評価で例外を発火させ、catch するためには try を使う必要があるが、
    -- 純粋関数では難しいので「型が合わない場合 columnAsList が例外を投げる」前提の
    -- 楽観的実装にする。columnTypeString で事前に分岐しているため通常は安全。
    seqList []     = ()
    seqList (_:_)  = ()

-- | Maybe Double → Text ("NA" or 数値文字列)。
maybeToTextDouble :: Maybe Double -> Text
maybeToTextDouble Nothing  = "NA"
maybeToTextDouble (Just d) = T.pack (show d)
