-- |
-- Module      : Hanalyze.Viz.Core
-- Description : 全 Viz.* モジュール共有の I/O ヘルパ (writeSpec / openInBrowser / vlJson 等)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
{-# LANGUAGE OverloadedStrings #-}
-- | Core visualization I/O helpers shared by every @Viz.*@ module.
--
-- Owns 'OutputFormat', 'writeSpec' (HTML / PNG / SVG via @vl-convert@
-- subprocess; HTML is the always-available fallback), 'openInBrowser',
-- and the JSON serialiser 'vlJson' used by downstream consumers
-- (HPotfire) to ship Vega-Lite specs over the wire.
--
-- Plot configuration ('PlotConfig' / 'defaultConfig') lives in
-- 'Hanalyze.Viz.PlotConfig' since 2026-05-14 and is re-exported here for
-- backwards compatibility.
module Hanalyze.Viz.Core
  ( -- * Plot configuration (re-exported from "Hanalyze.Viz.PlotConfig")
    PlotConfig (..)
  , defaultConfig
    -- * Spec I/O
  , openInBrowser
  , OutputFormat (..)
  , parseFormat
  , writeSpec
    -- * Spec serialisation
  , vlJson
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

import Hanalyze.Viz.PlotConfig (PlotConfig (..), defaultConfig)

-- | Serialise a 'VegaLite' spec to its canonical JSON 'Text'. Convenient
-- for downstream consumers (e.g. HPotfire's @/api/viz@) that need to
-- ship the spec over the wire instead of writing to disk.
--
-- Equivalent to @decodeUtf8 . toStrict . encode . fromVL@; provided here
-- so every @Viz.*@ module can re-export a single canonical spelling.
vlJson :: VegaLite -> Text
vlJson = decodeUtf8 . toStrict . encode . fromVL

-- | Output format for generated plots.
data OutputFormat = HTML | PNG | SVG deriving (Show, Eq)

-- | Parse an 'OutputFormat' name (@\"html\"@ / @\"png\"@ / @\"svg\"@).
parseFormat :: String -> Either String OutputFormat
parseFormat "html" = Right HTML
parseFormat "png"  = Right PNG
parseFormat "svg"  = Right SVG
parseFormat s      = Left ("Unknown format '" ++ s ++ "'. Use: html | png | svg")

-- | Write a Vega-Lite spec in the requested format. PNG and SVG are
-- produced by piping the JSON through the @vl-convert@ CLI.
writeSpec :: OutputFormat -> FilePath -> VegaLite -> IO ()
writeSpec HTML path spec = toHtmlFile path spec
writeSpec fmt  path spec = do
  result <- try (writeViaVlConvert fmt path spec) :: IO (Either SomeException ())
  case result of
    Right _ -> return ()
    Left err -> do
      hPutStrLn stderr $ "Warning: vl-convert failed (" ++ show err ++ "). Writing HTML instead."
      toHtmlFile (replaceExtension path "html") spec

-- | Convert a Vega-Lite spec to PNG / SVG via @vl-convert@.
-- Writes the spec to a temporary JSON file, invokes @vl-convert@, and
-- removes the temporary file.
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

-- | Open a file in the platform's default browser.
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
