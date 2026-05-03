{-# LANGUAGE OverloadedStrings #-}
-- | 外部データフォーマット (Parquet / JSON) を Hackage の @dataframe@
-- ライブラリ経由で読み込む。
--
-- Phase 2 で旧ラッパー API (loadCSVExt / loadTSVExt / loadCsvX / loadTsvX /
-- loadParquetX / loadJsonX / fromExternalDF / 内部 'DataFrame.Core' 変換)
-- は撤廃し、Hackage 'DataFrame.Internal.DataFrame.DataFrame' を直接返す経路
-- に統一した。CSV / TSV は 'DataIO.CSV.loadCSV' / 'loadTSV' を利用する。
module DataIO.External
  ( loadParquet
  , loadJSON
  ) where

import qualified DataFrame                    as DX
import qualified DataFrame.Internal.DataFrame as DXD
import qualified DataFrame.IO.JSON            as DXJ

import Control.Exception (SomeException, try)

-- | Apache Parquet ファイルを読み込み (列指向、圧縮対応)。
loadParquet :: FilePath -> IO (Either String DXD.DataFrame)
loadParquet = loadRaw DX.readParquet

-- | JSON (records-of-objects 形式) を読み込み。
loadJSON :: FilePath -> IO (Either String DXD.DataFrame)
loadJSON = loadRaw DXJ.readJSON

loadRaw :: (FilePath -> IO DXD.DataFrame)
        -> FilePath -> IO (Either String DXD.DataFrame)
loadRaw reader path = do
  result <- try (reader path) :: IO (Either SomeException DXD.DataFrame)
  return $ case result of
    Left  e  -> Left ("External loader failed: " ++ show e)
    Right df -> Right df
