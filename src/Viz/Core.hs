{-# LANGUAGE OverloadedStrings #-}
module Viz.Core
  ( PlotConfig (..)
  , defaultConfig
  , openInBrowser
  , OutputFormat (..)
  , parseFormat
  , writeSpec
  ) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Graphics.Vega.VegaLite (VegaLite, toHtmlFile)
import System.FilePath (replaceExtension)
import System.Info (os)
import System.IO (hPutStrLn, stderr)
import System.Process (callCommand)

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
-- PNG/SVG require vl-convert (not yet implemented); falls back to HTML with a warning.
writeSpec :: OutputFormat -> FilePath -> VegaLite -> IO ()
writeSpec HTML path spec = toHtmlFile path spec
writeSpec fmt  path spec = do
  hPutStrLn stderr $ "Warning: " ++ show fmt
    ++ " output requires vl-convert which is not installed."
    ++ " Writing HTML instead."
  toHtmlFile (replaceExtension path "html") spec

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
