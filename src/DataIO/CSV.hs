{-# LANGUAGE OverloadedStrings #-}
module DataIO.CSV
  ( loadCSV
  , ParseError
  ) where

import DataFrame.Core

import Data.Csv (decodeByName, NamedRecord, Header)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Text.Read (readMaybe)

type ParseError = String

loadCSV :: FilePath -> IO (Either ParseError DataFrame)
loadCSV path = do
  content <- BL.readFile path
  case decodeByName content of
    Left err          -> return (Left err)
    Right (hdr, rows) -> return (Right (toDataFrame hdr rows))

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
