{-# LANGUAGE OverloadedStrings #-}
module DataFrame.Core
  ( DataFrame
  , Column (..)
  , mkDataFrame
  , getColumn
  , getNumeric
  , getText
  , columnNames
  , numRows
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as V

data Column
  = NumericCol (V.Vector Double)
  | TextCol    (V.Vector Text)
  deriving (Show, Eq)

newtype DataFrame = DataFrame
  { unDataFrame :: Map Text Column }
  deriving (Show, Eq)

mkDataFrame :: [(Text, Column)] -> DataFrame
mkDataFrame = DataFrame . Map.fromList

getColumn :: Text -> DataFrame -> Maybe Column
getColumn name = Map.lookup name . unDataFrame

getNumeric :: Text -> DataFrame -> Maybe (V.Vector Double)
getNumeric name df = case getColumn name df of
  Just (NumericCol v) -> Just v
  _                   -> Nothing

getText :: Text -> DataFrame -> Maybe (V.Vector Text)
getText name df = case getColumn name df of
  Just (TextCol v) -> Just v
  _                -> Nothing

columnNames :: DataFrame -> [Text]
columnNames = Map.keys . unDataFrame

numRows :: DataFrame -> Int
numRows (DataFrame cols) =
  case Map.elems cols of
    []                 -> 0
    (NumericCol v : _) -> V.length v
    (TextCol v    : _) -> V.length v
