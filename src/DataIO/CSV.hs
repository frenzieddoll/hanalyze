{-# LANGUAGE OverloadedStrings #-}
-- | CSV / TSV / SSV ローダ。Phase 2 以降は Hackage @dataframe@ の
-- 'DataFrame.Internal.DataFrame.DataFrame' を直接返す。
--
-- - CSV / TSV: Hackage の 'DX.readCsv' / 'DX.readTsv' に委譲 (型推論強化、
--   欠損ビットマップ対応)。
-- - SSV: Hackage に専用ローダが無いため cassava で読みつつ、各列を
--   'DX.fromList' で 'DX.Column' 化、'DX.insertColumn' で組み立てる。
module DataIO.CSV
  ( loadCSV
  , loadTSV
  , loadSSV
  , loadAuto
  , ParseError
  ) where

import qualified DataFrame                    as DX
import qualified DataFrame.Internal.DataFrame as DXD

import Control.Exception (SomeException, try)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.List (isSuffixOf)
import Data.Csv (NamedRecord, Header, defaultDecodeOptions, DecodeOptions(..), decodeByNameWith)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Text.Read (readMaybe)

type ParseError = String

-- ---------------------------------------------------------------------------
-- CSV / TSV: Hackage に直接委譲
-- ---------------------------------------------------------------------------

loadCSV :: FilePath -> IO (Either ParseError DXD.DataFrame)
loadCSV = loadHackage DX.readCsv

loadTSV :: FilePath -> IO (Either ParseError DXD.DataFrame)
loadTSV = loadHackage DX.readTsv

loadHackage :: (FilePath -> IO DXD.DataFrame)
            -> FilePath -> IO (Either ParseError DXD.DataFrame)
loadHackage reader path = do
  r <- try (reader path) :: IO (Either SomeException DXD.DataFrame)
  return $ case r of
    Left  e  -> Left ("CSV/TSV loader failed: " ++ show e)
    Right df -> Right df

-- ---------------------------------------------------------------------------
-- SSV: cassava で読み、Hackage 'DataFrame' に詰め替える
-- ---------------------------------------------------------------------------

loadSSV :: FilePath -> IO (Either ParseError DXD.DataFrame)
loadSSV path = do
  content <- BL.readFile path
  let opts = defaultDecodeOptions { decDelimiter = fromIntegral (ord ' ') }
  case decodeByNameWith opts content of
    Left err          -> return (Left err)
    Right (hdr, rows) -> return (Right (toHackageDF hdr rows))

toHackageDF :: Header -> V.Vector NamedRecord -> DXD.DataFrame
toHackageDF hdr rows =
  foldl insert DX.empty
    [ (TE.decodeUtf8 key, classifyCells key rows) | key <- V.toList hdr ]
  where
    insert df (name, col) = DX.insertColumn name col df

-- | 列の値を全て読み 'Double' として parse できれば数値列、そうでなければ Text 列。
classifyCells :: BS.ByteString -> V.Vector NamedRecord -> DX.Column
classifyCells key rows =
  let cells = V.map (TE.decodeUtf8 . HM.lookupDefault "" key) rows
      texts = V.toList cells
  in case mapM (readMaybe . T.unpack) texts of
       Just nums -> DX.fromList (nums :: [Double])
       Nothing   -> DX.fromList (texts :: [Text])

-- ---------------------------------------------------------------------------
-- 拡張子による自動振り分け
-- ---------------------------------------------------------------------------

loadAuto :: FilePath -> IO (Either ParseError DXD.DataFrame)
loadAuto path
  | ".tsv" `isSuffixOf` path = loadTSV path
  | ".ssv" `isSuffixOf` path = loadSSV path
  | otherwise                = loadCSV path
