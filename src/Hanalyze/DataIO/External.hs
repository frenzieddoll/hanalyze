{-# LANGUAGE OverloadedStrings #-}
-- | External data-format loaders (Parquet / JSON) via the Hackage
-- @dataframe@ library.
--
-- Returns Hackage's 'DataFrame.Internal.DataFrame.DataFrame' directly.
-- For CSV and TSV use 'Hanalyze.DataIO.CSV.loadCSV' / @loadTSV@ instead.
module Hanalyze.DataIO.External
  ( loadParquet
  , loadJSON
  ) where

import qualified DataFrame.IO.Parquet          as DX
import qualified DataFrame.Internal.DataFrame as DXD
import qualified DataFrame.IO.JSON            as DXJ

import Control.Exception (SomeException, try)

-- | Load an Apache Parquet file (columnar, supports compression).
loadParquet :: FilePath -> IO (Either String DXD.DataFrame)
loadParquet = loadRaw DX.readParquet

-- | Load a JSON file in records-of-objects format.
loadJSON :: FilePath -> IO (Either String DXD.DataFrame)
loadJSON = loadRaw DXJ.readJSON

loadRaw :: (FilePath -> IO DXD.DataFrame)
        -> FilePath -> IO (Either String DXD.DataFrame)
loadRaw reader path = do
  result <- try (reader path) :: IO (Either SomeException DXD.DataFrame)
  return $ case result of
    Left  e  -> Left ("External loader failed: " ++ show e)
    Right df -> Right df
