{-# LANGUAGE OverloadedStrings #-}
-- | Core visualization helpers shared by every @Viz.*@ module.
--
-- Provides 'PlotConfig' / 'defaultConfig' (title, dimensions, output
-- format), 'writeSpec' for emitting HTML / PNG / SVG (PNG and SVG go
-- through the @vl-convert@ subprocess; HTML is the always-available
-- fallback) and the @openInBrowser@ convenience helper.
module Viz.Core
  ( PlotConfig (..)
  , defaultConfig
  , openInBrowser
  , OutputFormat (..)
  , parseFormat
  , writeSpec
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (encode)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO
import Graphics.Vega.VegaLite (VegaLite, toHtmlFile, fromVL)
import System.FilePath (replaceExtension)
import System.Info (os)
import System.IO (hFlush, hClose, hPutStrLn, stderr)
import System.IO.Temp (withSystemTempFile)
import System.Process (callCommand, callProcess)

data PlotConfig = PlotConfig
  { plotTitle  :: Text
  , plotWidth  :: Double
  , plotHeight :: Double
  } deriving (Show)

defaultConfig :: Text -> PlotConfig
defaultConfig t = PlotConfig
  { plotTitle  = t
  , plotWidth  = 600
  , plotHeight = 400
  }

-- | Output format for generated plots.
data OutputFormat = HTML | PNG | SVG deriving (Show, Eq)

parseFormat :: String -> Either String OutputFormat
parseFormat "html" = Right HTML
parseFormat "png"  = Right PNG
parseFormat "svg"  = Right SVG
parseFormat s      = Left ("Unknown format '" ++ s ++ "'. Use: html | png | svg")

-- | Write a Vega-Lite spec in the requested format.
-- PNG/SVG は vl-convert CLI に JSON を渡して変換する。
writeSpec :: OutputFormat -> FilePath -> VegaLite -> IO ()
writeSpec HTML path spec = toHtmlFile path spec
writeSpec fmt  path spec = do
  result <- try (writeViaVlConvert fmt path spec) :: IO (Either SomeException ())
  case result of
    Right _ -> return ()
    Left err -> do
      hPutStrLn stderr $ "Warning: vl-convert failed (" ++ show err ++ "). Writing HTML instead."
      toHtmlFile (replaceExtension path "html") spec

-- | vl-convert を使って Vega-Lite spec を PNG/SVG に変換する。
-- spec を一時ファイルに書き出し → vl-convert → 一時ファイル削除。
writeViaVlConvert :: OutputFormat -> FilePath -> VegaLite -> IO ()
writeViaVlConvert fmt outPath spec = do
  let json   = decodeUtf8 . toStrict . encode . fromVL $ spec
      subcmd = case fmt of
        PNG -> "vl2png"
        SVG -> "vl2svg"
        HTML -> "vl2html"
  withSystemTempFile "vl-spec-.json" $ \tmpPath tmpH -> do
    TIO.hPutStr tmpH json
    hFlush tmpH
    hClose tmpH
    callProcess "vl-convert" [subcmd, "-i", tmpPath, "-o", outPath]

openInBrowser :: FilePath -> IO ()
openInBrowser path = do
  result <- try (callCommand cmd) :: IO (Either SomeException ())
  case result of
    Right _  -> return ()
    Left err -> putStrLn $ "Note: could not open browser (" ++ show err ++ ")"
  where
    cmd = case os of
      "darwin"  -> "open "     ++ path
      "mingw32" -> "start "    ++ path
      _         -> "xdg-open " ++ path
