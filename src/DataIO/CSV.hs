{-# LANGUAGE OverloadedStrings #-}
module DataIO.CSV
  ( loadCSV
  , loadTSV
  , loadSSV
  , ParseError
  ) where

import DataFrame.Core

import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.Csv (NamedRecord, Header, defaultDecodeOptions, DecodeOptions(..), decodeByNameWith)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Text.Read (readMaybe)

type ParseError = String

data FileType = CSV | TSV | SSV

decodeOpts :: FileType -> DecodeOptions
decodeOpts fileType = defaultDecodeOptions { decDelimiter = fromIntegral (ord delim)}
  where
    delim = case fileType of
      CSV -> ','
      TSV -> '\t'
      SSV -> ' '

loadFileType :: FileType -> FilePath -> IO (Either ParseError DataFrame)
loadFileType fileType path = do
  content <- BL.readFile path
  let opts = decodeOpts fileType
  case decodeByNameWith opts content of
    Left err          -> return (Left err)
    Right (hdr, rows) -> return (Right (toDataFrame hdr rows))

loadCSV, loadTSV, loadSSV :: FilePath -> IO (Either ParseError DataFrame)
loadCSV = loadFileType CSV
loadTSV = loadFileType TSV
loadSSV = loadFileType SSV

toDataFrame :: Header -> V.Vector NamedRecord -> DataFrame
toDataFrame hdr rows =
  mkDataFrame
    [ (colName, classifyColumn colName rows)
    | key <- V.toList hdr
    , let colName = TE.decodeUtf8 key
    ]

classifyColumn :: Text -> V.Vector NamedRecord -> Column
classifyColumn col rows =
  let bsKey = TE.encodeUtf8 col
      vals   = V.map (TE.decodeUtf8 . HM.lookupDefault "" bsKey) rows
  in case V.mapM (readMaybe . T.unpack) vals of
       Just nums -> NumericCol nums
       Nothing   -> TextCol vals
