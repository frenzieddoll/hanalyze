{-# LANGUAGE OverloadedStrings #-}
module Main where

import DataIO.CSV     (loadAuto)
import DataFrame.Core (DataFrame, columnNames, numRows, getNumeric)
import Model.Core     (Band (..), rSquared, coeffList, fittedList)
import Model.GLM      (Family (..), parseFamily, LinkFn (..), parseLink, canonicalLink,
                       fitGLMWithSmooth)
import Viz.Core       (defaultConfig, openInBrowser, OutputFormat (..), parseFormat)
import Viz.Scatter    (scatterWithSmoothFile, scatterMultiYFile, scatterPlotFile,
                       predictedVsActualFile)

import Data.Char          (isDigit)
import Data.List          (intercalate)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector  as V
import System.Environment (getArgs)
import System.IO          (hPutStrLn, stderr)
import Text.Printf        (printf)

-- ---------------------------------------------------------------------------
-- CLI types
-- ---------------------------------------------------------------------------

data ModelType = LM | GLM | NoReg deriving (Show, Eq)

data DegreeSpec
  = AllDegree Int
  | PerDegree [(Int, Int)]
  deriving (Show)

data Config = Config
  { cfgFile    :: FilePath
  , cfgXCols   :: [T.Text]
  , cfgYCols   :: [T.Text]   -- one or more y columns
  , cfgModel   :: ModelType
  , cfgDist    :: Family
  , cfgLink    :: LinkFn
  , cfgDegree  :: DegreeSpec
  , cfgBand    :: Band
  , cfgFormat  :: OutputFormat
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Argument parsing
-- ---------------------------------------------------------------------------

usageMsg :: String
usageMsg = unlines
  [ "Usage: hanalyze <file> <xcols> <ycols> [LM|GLM|NoReg] [options]"
  , ""
  , "  <file>    CSV/TSV/SSV file (auto-detected from extension)"
  , "  <xcols>   x column name(s); quote multiple: \"x1 x2\""
  , "  <ycols>   y column name(s); quote multiple: \"y1 y2\" (multi-y → scatter only)"
  , "  LM|GLM|NoReg  model type (default: LM)"
  , ""
  , "Options:"
  , "  -d, --dist DIST    distribution: gaussian|binomial|poisson  (default: gaussian)"
  , "  -l, --link LINK    link function: identity|log|logit|sqrt   (default: canonical)"
  , "  --degree SPEC      degree specification (default: 1)"
  , "  --ci [LEVEL]       show confidence interval (default level: 0.95)"
  , "  --pi [LEVEL]       show prediction interval (Gaussian only; default level: 0.95)"
  , "  --format FORMAT    output format: html|png|svg               (default: html)"
  , ""
  , "Degree specification:"
  , "  N                  all columns get degree N"
  , "  -i1 N1 [-i2 N2…]  column at 1-based position i1 gets degree N1; others: 1"
  , ""
  , "Examples:"
  , "  hanalyze data.csv x y"
  , "  hanalyze data.tsv \"x1 x2\" y LM --degree -1 2 -2 3 --ci 0.90"
  , "  hanalyze data.csv x y GLM -d poisson -l log --pi"
  , "  hanalyze data.csv x \"y1 y2\" NoReg"
  ]

parseArgs :: [String] -> Either String Config
parseArgs (file : xColsStr : yColsStr : rest) = do
  let xCols = map T.pack (words xColsStr)
      yCols = map T.pack (words yColsStr)
  if null xCols
    then Left "Error: xcols must not be empty"
    else if null yCols
    then Left "Error: ycols must not be empty"
    else do
      (model, rest1)                           <- parseModelType rest
      (mDist, mLink, degSpec, band, fmt, rest2) <- parseOptions rest1
      if not (null rest2)
        then Left ("Unexpected argument(s): " ++ unwords rest2)
        else do
          let dist = maybe Gaussian id mDist
              lnk  = maybe (canonicalLink dist) id mLink
          Right Config
            { cfgFile   = file
            , cfgXCols  = xCols
            , cfgYCols  = yCols
            , cfgModel  = model
            , cfgDist   = dist
            , cfgLink   = lnk
            , cfgDegree = degSpec
            , cfgBand   = band
            , cfgFormat = fmt
            }
parseArgs _ = Left usageMsg

parseModelType :: [String] -> Either String (ModelType, [String])
parseModelType ("LM"    : rest) = Right (LM,    rest)
parseModelType ("GLM"   : rest) = Right (GLM,   rest)
parseModelType ("NoReg" : rest) = Right (NoReg, rest)
parseModelType rest              = Right (LM,    rest)

parseOptions :: [String]
             -> Either String (Maybe Family, Maybe LinkFn, DegreeSpec, Band, OutputFormat, [String])
parseOptions = go Nothing Nothing (AllDegree 1) NoBand HTML
  where
    go mDist mLink deg band fmt [] = Right (mDist, mLink, deg, band, fmt, [])

    go mDist mLink deg band fmt (flag : rest)
      | flag `elem` ["-d", "--dist"] = case rest of
          (v:rest') -> do fam <- parseFamily v
                          go (Just fam) mLink deg band fmt rest'
          []        -> Left "Error: -d/--dist requires an argument"

      | flag `elem` ["-l", "--link"] = case rest of
          (v:rest') -> do lnk <- parseLink v
                          go mDist (Just lnk) deg band fmt rest'
          []        -> Left "Error: -l/--link requires an argument"

      | flag == "--degree" = do
          let (degTokens, remaining) = span isDegreeToken rest
          if null degTokens
            then Left "--degree requires a specification (e.g., 2 or -1 2 -2 3)"
            else do degSpec <- parseDegreeSpec degTokens
                    go mDist mLink degSpec band fmt remaining

      | flag == "--ci" =
          let (level, rest') = consumeLevel 0.95 rest
          in go mDist mLink deg (CI level) fmt rest'

      | flag == "--pi" =
          let (level, rest') = consumeLevel 0.95 rest
          in go mDist mLink deg (PI level) fmt rest'

      | flag `elem` ["-f", "--format"] = case rest of
          (v:rest') -> do f <- parseFormat v
                          go mDist mLink deg band f rest'
          []        -> Left "Error: -f/--format requires an argument"

      | otherwise = Right (mDist, mLink, deg, band, fmt, flag : rest)

-- Consume an optional level (0 < v < 1) after a band flag.
consumeLevel :: Double -> [String] -> (Double, [String])
consumeLevel _   (t:ts) | isLevelToken t = (read t, ts)
consumeLevel def rest                    = (def, rest)

isLevelToken :: String -> Bool
isLevelToken s = case (reads s :: [(Double, String)]) of
  [(v, "")] | v > 0, v < 1 -> True
  _                          -> False

isDegreeToken :: String -> Bool
isDegreeToken ('-' : ds) = not (null ds) && all isDigit ds
isDegreeToken s          = not (null s)  && all isDigit s

parseDegreeSpec :: [String] -> Either String DegreeSpec
parseDegreeSpec [n] =
  case (reads n :: [(Int, String)]) of
    [(v, "")] | v >= 0 -> Right (AllDegree v)
    _                   -> Left ("Invalid degree: " ++ n)
parseDegreeSpec tokens = fmap PerDegree (parsePairs tokens)
  where
    parsePairs [] = Right []
    parsePairs (pos : deg : rest) =
      case (reads pos :: [(Int,String)], reads deg :: [(Int,String)]) of
        ([(p,"")], [(d,"")]) | p < 0, d >= 0 ->
          fmap ((abs p, d) :) (parsePairs rest)
        _ -> Left ("Invalid degree pair near: " ++ pos ++ " " ++ deg)
    parsePairs [t] = Left ("Odd number of tokens in --degree near: " ++ t)

applyDegreeSpec :: DegreeSpec -> [T.Text] -> [(T.Text, Int)]
applyDegreeSpec (AllDegree d) cols  = [(c, d) | c <- cols]
applyDegreeSpec (PerDegree ps) cols =
  [ (c, maybe 1 id (lookup i ps)) | (i, c) <- zip [1..] cols ]

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err  -> hPutStrLn stderr err
    Right cfg -> runConfig cfg

runConfig :: Config -> IO ()
runConfig cfg = do
  -- Warn: PI with non-Gaussian falls back to CI
  case (cfgBand cfg, cfgDist cfg, cfgModel cfg) of
    (PI _, fam, GLM) | fam /= Gaussian ->
      hPutStrLn stderr "Warning: PI is only exact for Gaussian. Using CI with same level."
    _ -> return ()

  result <- loadAuto (cfgFile cfg)
  case result of
    Left err -> putStrLn ("Parse error: " ++ err)
    Right df -> do
      putStrLn $ "Loaded " ++ show (numRows df) ++ " rows from " ++ cfgFile cfg
      putStrLn "Columns:"
      mapM_ (TIO.putStrLn . ("  - " <>)) (columnNames df)

      let fmt       = cfgFormat cfg
          xCol1     = head (cfgXCols cfg)
          yCols     = cfgYCols cfg
          -- Force NoReg when multiple y columns are given
          effModel  = if length yCols > 1 then NoReg else cfgModel cfg

      case effModel of
        -- ── No regression: scatter plot only ──────────────────────────────
        NoReg ->
          case yCols of
            [yCol] -> do
              let scatterPath = "scatter.html"
                  scatterCfg  = defaultConfig (xCol1 <> " vs " <> yCol)
              scatterPlotFile fmt scatterPath scatterCfg df xCol1 yCol
              putStrLn $ "\nScatter plot:        " ++ scatterPath
              openInBrowser scatterPath

            _ -> do
              let scatterPath = "scatter.html"
                  scatterCfg  = defaultConfig (xCol1 <> " vs " <> T.intercalate ", " yCols)
              scatterMultiYFile fmt scatterPath scatterCfg df xCol1 yCols
              putStrLn $ "\nScatter plot (multi-y): " ++ scatterPath
              openInBrowser scatterPath

        -- ── Regression: LM or GLM ─────────────────────────────────────────
        _ -> case yCols of
          [yCol] -> runRegression cfg df fmt xCol1 yCol
          _      -> do
            -- Multiple y with regression: scatter only (regression not yet supported)
            putStrLn "\nNote: regression with multiple y columns not yet supported. Plotting scatter only."
            let scatterPath = "scatter.html"
                scatterCfg  = defaultConfig (xCol1 <> " vs " <> T.intercalate ", " yCols)
            scatterMultiYFile fmt scatterPath scatterCfg df xCol1 yCols
            putStrLn $ "Scatter plot (multi-y): " ++ scatterPath
            openInBrowser scatterPath

runRegression :: Config -> DataFrame -> OutputFormat -> T.Text -> T.Text -> IO ()
runRegression cfg df fmt xCol1 yCol = do
  let colDegs = applyDegreeSpec (cfgDegree cfg) (cfgXCols cfg)
      (dist, lnk) = case cfgModel cfg of
        LM  -> (Gaussian, Identity)
        GLM -> (cfgDist cfg, cfgLink cfg)
        NoReg -> (Gaussian, Identity)  -- unreachable

  case fitGLMWithSmooth dist lnk colDegs (cfgBand cfg) 200 df yCol of
    Nothing -> putStrLn "\nError: column(s) not found or not numeric"
    Just (res, mSmooth) -> do
      let cs  = coeffList res
          eq  = equationLabel dist lnk colDegs cs

      putStrLn $ "\nModel: " ++ T.unpack yCol ++ " ~ "
              ++ modelFormula colDegs
              ++ "  [" ++ modelLabel dist lnk ++ "]"
      mapM_ (\(lbl, v) -> printf "  %-30s = %9.4f\n" lbl v)
            (zip (multiCoeffLabels colDegs) cs)
      printf "  %-30s = %9.4f\n" (r2Label dist) (rSquared res)

      let bandLabel = case cfgBand cfg of
            NoBand   -> ""
            CI level -> ", " ++ show (round (level*100) :: Int) ++ "% CI"
            PI level -> ", " ++ show (round (level*100) :: Int) ++ "% PI"
          titleSuffix = "  [" <> T.pack (modelLabel dist lnk) <> T.pack bandLabel <> "]"

      case mSmooth of
        Just sf -> do
          let scatterPath = "scatter.html"
              scatterCfg  = defaultConfig (xCol1 <> " vs " <> yCol <> titleSuffix)
          scatterWithSmoothFile fmt scatterPath scatterCfg eq df xCol1 yCol sf
          putStrLn $ "\nScatter plot:        " ++ scatterPath
          openInBrowser scatterPath
        Nothing ->
          putStrLn "\n(Scatter plot skipped for multiple x columns)"

      case getNumeric yCol df of
        Nothing   -> return ()
        Just yVec -> do
          let pvsaPath = "pvsa.html"
              pvsaCfg  = defaultConfig ("Predicted vs Actual  " <> titleSuffix)
          predictedVsActualFile fmt pvsaPath pvsaCfg (V.toList yVec) (fittedList res)
          putStrLn $ "Predicted vs Actual: " ++ pvsaPath
          openInBrowser pvsaPath

-- ---------------------------------------------------------------------------
-- Formatting helpers
-- ---------------------------------------------------------------------------

modelLabel :: Family -> LinkFn -> String
modelLabel dist lnk = show dist ++ "/" ++ show lnk

r2Label :: Family -> String
r2Label Gaussian = "R²"
r2Label _        = "McFadden R²"

modelFormula :: [(T.Text, Int)] -> String
modelFormula colDegs = intercalate " + " (concatMap terms colDegs)
  where
    terms (col, deg) =
      [ T.unpack col ++ if k == 1 then "" else "^" ++ show k
      | k <- [1 .. deg]
      ]

multiCoeffLabels :: [(T.Text, Int)] -> [String]
multiCoeffLabels colDegs = "β₀ (intercept)" : zipWith fmt [1..] rest
  where
    rest          = concatMap expand colDegs
    expand (col, deg) = [(col, k) | k <- [1 .. deg]]
    fmt i (col, k) =
      "β" ++ show (i :: Int) ++ " ("
      ++ T.unpack col
      ++ (if k == 1 then "" else "^" ++ show k)
      ++ ")"

-- | Generate a human-readable regression equation for single x-column models.
equationLabel :: Family -> LinkFn -> [(T.Text, Int)] -> [Double] -> Maybe T.Text
equationLabel _ _ colDegs _ | length colDegs /= 1 = Nothing
equationLabel _ _ _ coeffs  | null coeffs          = Nothing
equationLabel fam lnk [(col, deg)] coeffs = Just (T.pack label)
  where
    lhs = case (fam, lnk) of
      (Gaussian, Identity) -> "y"
      (_, Identity)        -> "E[y]"
      _                    -> show lnk ++ "(y)"

    b0    = head coeffs
    betas = tail coeffs

    termStr b k =
      let sign = if b >= 0 then " + " else " - "
          xStr = T.unpack col ++ if k == 1 then "" else "^" ++ show k
      in sign ++ printf "%.4f" (abs b :: Double) ++ xStr

    label = lhs ++ " = " ++ printf "%.4f" b0
          ++ concat (zipWith termStr betas [1 .. deg])
equationLabel _ _ _ _ = Nothing
