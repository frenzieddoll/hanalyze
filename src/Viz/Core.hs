{-# LANGUAGE OverloadedStrings #-}
module Viz.Core
  ( PlotConfig (..)
  , defaultConfig
  , openInBrowser
  ) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import System.Info (os)
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
