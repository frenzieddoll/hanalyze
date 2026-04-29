{-# LANGUAGE OverloadedStrings #-}
module Main where

import DataIO.CSV     (loadCSV)
import DataFrame.Core (columnNames, numRows, getNumeric)
import Model.Core     (rSquared, coeffList, fittedList)
import Model.GLM      (Family (..), parseFamily, fitGLMWithSmooth)
import Viz.Core       (defaultConfig, openInBrowser)
import Viz.Scatter    (scatterWithSmoothFile, predictedVsActualFile)

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector  as V
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [csv, x, y]           -> run Gaussian 1   csv (T.pack x) (T.pack y)
    [csv, x, y, deg]      -> runDeg deg Gaussian csv (T.pack x) (T.pack y)
    [csv, x, y, deg, fam] -> case parseFamily fam of
      Left  err    -> putStrLn err
      Right family -> runDeg deg family csv (T.pack x) (T.pack y)
    _ -> putStrLn "Usage: hanalyze <csv> <x> <y> [degree] [gaussian|binomial|poisson]"

runDeg :: String -> Family -> FilePath -> T.Text -> T.Text -> IO ()
runDeg degStr family csv x y =
  case reads degStr of
    [(d, "")] -> run family d csv x y
    _         -> putStrLn "Error: degree must be a positive integer"

run :: Family -> Int -> FilePath -> T.Text -> T.Text -> IO ()
run family degree csvPath xCol yCol = do
  result <- loadCSV csvPath
  case result of
    Left err -> putStrLn ("Parse error: " ++ err)
    Right df -> do
      putStrLn $ "Loaded " ++ show (numRows df) ++ " rows"
      putStrLn "Columns:"
      mapM_ (TIO.putStrLn . ("  - " <>)) (columnNames df)

      case fitGLMWithSmooth family degree 0.95 200 df xCol yCol of
        Nothing        -> putStrLn "\nError: columns not found or not numeric"
        Just (res, sf) -> do
          let cs = coeffList res

          putStrLn $ "\nModel: " ++ T.unpack yCol ++ " ~ " ++ polyLabel degree xCol
                  ++ "  [" ++ familyLabel family ++ "]"
          mapM_ (\(lbl, v) -> printf "  %-22s = %9.4f\n" lbl v)
                (zip (coeffLabels degree) cs)
          printf "  %-22s = %9.4f\n" (r2Label family) (rSquared res)

          let scatterPath = "scatter.html"
              pvsaPath    = "pvsa.html"
              titleSuffix = "  [" <> T.pack (familyLabel family)
                         <> ", deg=" <> T.pack (show degree) <> ", 95% CI]"
              scatterCfg  = defaultConfig (xCol <> " vs " <> yCol <> titleSuffix)
              pvsaCfg     = defaultConfig ("Predicted vs Actual  " <> titleSuffix)

          scatterWithSmoothFile scatterPath scatterCfg df xCol yCol sf
          putStrLn $ "\nScatter plot:        " ++ scatterPath
          openInBrowser scatterPath

          case getNumeric yCol df of
            Nothing   -> return ()
            Just yVec -> do
              predictedVsActualFile pvsaPath pvsaCfg (V.toList yVec) (fittedList res)
              putStrLn $ "Predicted vs Actual: " ++ pvsaPath
              openInBrowser pvsaPath

-- ---------------------------------------------------------------------------
-- Formatting helpers
-- ---------------------------------------------------------------------------

familyLabel :: Family -> String
familyLabel Gaussian = "Gaussian/identity"
familyLabel Binomial = "Binomial/logit"
familyLabel Poisson  = "Poisson/log"

r2Label :: Family -> String
r2Label Gaussian = "R²"
r2Label _        = "McFadden R²"

polyLabel :: Int -> T.Text -> String
polyLabel degree xCol =
  T.unpack xCol
  ++ concatMap (\k -> " + " ++ T.unpack xCol ++ "^" ++ show k) [2 .. degree]

coeffLabels :: Int -> [String]
coeffLabels degree =
  "β₀ (intercept)"
  : [ "β" ++ show k ++ " (x" ++ (if k == 1 then "" else "^" ++ show k) ++ ")"
    | k <- [1 .. degree]
    ]
