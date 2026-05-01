{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import DataIO.CSV        (loadAuto)
import DataFrame.Core    (DataFrame, columnNames, numRows, getNumeric, getText)
import Model.Core        (Band (..), rSquared, coeffList, fittedList)
import Model.GLM         (Family (..), parseFamily, LinkFn (..), parseLink, canonicalLink,
                          fitGLMWithSmooth, fitGLMFull)
import Model.GLMM        (GLMMResult (..), fitLMEDataFrame, fitGLMMDataFrame)
import Model.LM          (SmoothFit (..), multiPolyDesignMatrix)
import Stat.Distribution (Distribution, parseDistribution)
import Viz.Core          (defaultConfig, openInBrowser, OutputFormat (..), parseFormat)
import Viz.Scatter       (scatterWithSmoothFile, scatterMultiYFile, scatterPlotFile,
                          scatterWithGroupsFile, predictedVsActualFile,
                          predictedVsActual, scatterWithGroups)
import Viz.Histogram     (histogramPlotFile, histogramWithDensityFile)
import Viz.AnalysisReport (AnalysisReportConfig (..), ModelFit (..), NamedPlot (..),
                           SmoothData (..), GPKernelFit (..), GPFitSummary (..), FitSummary (..),
                           GLMMSummary (..), HBMRegSummary (..),
                           mkFitSummary, mkGLMMSummary,
                           writeAnalysisReport, writeAnalysisReportPlots)
import qualified Model.HBM as HBMod
import qualified MCMC.NUTS as HBMnuts
import qualified MCMC.Core as MCMCcore
import qualified Data.Map.Strict as Map
import Viz.MCMC (mcmcDiagnostics, autocorrPlot)
import Viz.Core (PlotConfig (..))
import Model.GP           (Kernel (..), GPModel (..), GPParams, GPPredData,
                           initParamsFromData, optimizeGP, fitGP, logMarginalLikelihood,
                           gpPredData)

import Stat.ModelSelect  (lmPosteriorLogLiks, glmPosteriorLogLiks,
                          lmePosteriorLogLiks, waic, loo,
                          WAICResult (..), LOOResult (..))

import Data.Char          (isDigit)
import Data.List          (intercalate)
import System.FilePath    (dropExtension)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector  as V
import qualified Numeric.LinearAlgebra as LA
import System.Environment (getArgs)
import System.IO          (hPutStrLn, stderr)
import System.Random.MWC  (createSystemRandom)
import Text.Printf        (printf)

-- ---------------------------------------------------------------------------
-- CLI types
-- ---------------------------------------------------------------------------

data ModelType = LM | GLM | NoReg | GP | HBM deriving (Show, Eq)

data DegreeSpec
  = AllDegree Int
  | PerDegree [(Int, Int)]
  deriving (Show)

data Config = Config
  { cfgFile     :: FilePath
  , cfgXCols    :: [T.Text]
  , cfgYCols    :: [T.Text]   -- one or more y columns
  , cfgModel    :: ModelType
  , cfgDist     :: Family
  , cfgLink     :: LinkFn
  , cfgDegree   :: DegreeSpec
  , cfgBand     :: Band
  , cfgFormat   :: OutputFormat
  , cfgGroup    :: Maybe T.Text      -- grouping column → LME / GLMM
  , cfgHistMode :: Bool              -- --hist: draw histogram of x column
  , cfgFitDist  :: Maybe Distribution  -- --fit DIST PARAMS
  , cfgReport   :: Maybe FilePath    -- --report [FILE]: generate HTML report
  , cfgWAIC     :: Bool              -- --waic: compute WAIC/LOO-CV
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Argument parsing
-- ---------------------------------------------------------------------------

usageMsg :: String
usageMsg = unlines
  [ "Usage: hanalyze <file> <xcols> <ycols> [LM|GLM|NoReg|GP|HBM] [options]"
  , ""
  , "  <file>    CSV/TSV/SSV file (auto-detected from extension)"
  , "  <xcols>   x column name(s); quote multiple: \"x1 x2\""
  , "  <ycols>   y column name(s); quote multiple: \"y1 y2\" (multi-y → scatter only)"
  , "  LM|GLM|NoReg|GP|HBM  model type (default: LM)"
  , "    GP: Gaussian Process regression (single x/y only); compares RBF, Matérn5/2, Periodic"
  , "    HBM: Bayesian linear regression via NUTS (single x/y only); --report で AnalysisReport 生成"
  , ""
  , "Options:"
  , "  -d, --dist DIST    distribution: gaussian|binomial|poisson  (default: gaussian)"
  , "  -l, --link LINK    link function: identity|log|logit|sqrt   (default: canonical)"
  , "  --degree SPEC      degree specification (default: 1)"
  , "  --ci [LEVEL]       show confidence interval (default level: 0.95)"
  , "  --pi [LEVEL]       show prediction interval (Gaussian only; default level: 0.95)"
  , "  --format FORMAT    output format: html|png|svg               (default: html)"
  , "  --group COL        grouping column → LM+group: LME, GLM+group: GLMM"
  , "  --report [FILE]    generate HTML analysis report (default: report.html)"
  , "                     --format png|svg と組み合わせるとプロット部分を画像にも出力"
  , "  --waic             compute WAIC and LOO-CV and show in report (requires --report)"
  , ""
  , "Degree specification:"
  , "  N                  all columns get degree N"
  , "  -i1 N1 [-i2 N2…]  column at 1-based position i1 gets degree N1; others: 1"
  , ""
  , "Examples:"
  , "  hanalyze data.csv x y"
  , "  hanalyze data.tsv \"x1 x2\" y LM --degree -1 2 -2 3 --ci 0.90"
  , "  hanalyze data.csv x y GLM -d poisson -l log"
  , "  hanalyze data.csv x y LM --group school"
  , "  hanalyze data.csv x y GLM -d binomial -l logit --group hospital"
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
      (model, rest1)                                              <- parseModelType rest
      (mDist, mLink, degSpec, band, fmt, mGrp, hist, mFit, mRpt, waicF, rest2) <- parseOptions rest1
      if not (null rest2)
        then Left ("Unexpected argument(s): " ++ unwords rest2)
        else do
          let dist = maybe Gaussian id mDist
              lnk  = maybe (canonicalLink dist) id mLink
          Right Config
            { cfgFile     = file
            , cfgXCols    = xCols
            , cfgYCols    = yCols
            , cfgModel    = model
            , cfgDist     = dist
            , cfgLink     = lnk
            , cfgDegree   = degSpec
            , cfgBand     = band
            , cfgFormat   = fmt
            , cfgGroup    = mGrp
            , cfgHistMode = hist
            , cfgFitDist  = mFit
            , cfgReport   = mRpt
            , cfgWAIC     = waicF
            }
parseArgs _ = Left usageMsg

parseModelType :: [String] -> Either String (ModelType, [String])
parseModelType ("LM"    : rest) = Right (LM,    rest)
parseModelType ("GLM"   : rest) = Right (GLM,   rest)
parseModelType ("NoReg" : rest) = Right (NoReg, rest)
parseModelType ("GP"    : rest) = Right (GP,    rest)
parseModelType ("HBM"   : rest) = Right (HBM,   rest)
parseModelType rest              = Right (LM,    rest)

parseOptions :: [String]
             -> Either String (Maybe Family, Maybe LinkFn, DegreeSpec, Band, OutputFormat,
                               Maybe T.Text, Bool, Maybe Distribution, Maybe FilePath, Bool, [String])
parseOptions = go Nothing Nothing (AllDegree 1) NoBand HTML Nothing False Nothing Nothing False
  where
    go mDist mLink deg band fmt mGrp hist mFit mRpt waicF [] =
      Right (mDist, mLink, deg, band, fmt, mGrp, hist, mFit, mRpt, waicF, [])

    go mDist mLink deg band fmt mGrp hist mFit mRpt waicF (flag : rest)
      | flag `elem` ["-d", "--dist"] = case rest of
          (v:rest') -> do fam <- parseFamily v
                          go (Just fam) mLink deg band fmt mGrp hist mFit mRpt waicF rest'
          []        -> Left "Error: -d/--dist requires an argument"

      | flag `elem` ["-l", "--link"] = case rest of
          (v:rest') -> do lnk <- parseLink v
                          go mDist (Just lnk) deg band fmt mGrp hist mFit mRpt waicF rest'
          []        -> Left "Error: -l/--link requires an argument"

      | flag == "--degree" = do
          let (degTokens, remaining) = span isDegreeToken rest
          if null degTokens
            then Left "--degree requires a specification (e.g., 2 or -1 2 -2 3)"
            else do degSpec <- parseDegreeSpec degTokens
                    go mDist mLink degSpec band fmt mGrp hist mFit mRpt waicF remaining

      | flag == "--ci" =
          let (level, rest') = consumeLevel 0.95 rest
          in go mDist mLink deg (CI level) fmt mGrp hist mFit mRpt waicF rest'

      | flag == "--pi" =
          let (level, rest') = consumeLevel 0.95 rest
          in go mDist mLink deg (PI level) fmt mGrp hist mFit mRpt waicF rest'

      | flag `elem` ["-f", "--format"] = case rest of
          (v:rest') -> do f <- parseFormat v
                          go mDist mLink deg band f mGrp hist mFit mRpt waicF rest'
          []        -> Left "Error: -f/--format requires an argument"

      | flag == "--group" = case rest of
          (v:rest') -> go mDist mLink deg band fmt (Just (T.pack v)) hist mFit mRpt waicF rest'
          []        -> Left "Error: --group requires a column name"

      | flag == "--hist" =
          go mDist mLink deg band fmt mGrp True mFit mRpt waicF rest

      | flag == "--fit" = case rest of
          (name:rest') ->
            let (paramStrs, rest'') = span isNumericToken rest'
                params = map read paramStrs :: [Double]
            in case parseDistribution name params of
                 Left err -> Left ("--fit: " ++ err)
                 Right d  -> go mDist mLink deg band fmt mGrp hist (Just d) mRpt waicF rest''
          [] -> Left "--fit requires a distribution name (e.g. --fit normal 0 1)"

      | flag == "--report" = case rest of
          (v:rest') | not (null v) && head v /= '-' ->
                        go mDist mLink deg band fmt mGrp hist mFit (Just v) waicF rest'
          _           -> go mDist mLink deg band fmt mGrp hist mFit (Just "report.html") waicF rest

      | flag == "--waic" =
          go mDist mLink deg band fmt mGrp hist mFit mRpt True rest

      | otherwise = Right (mDist, mLink, deg, band, fmt, mGrp, hist, mFit, mRpt, waicF, flag : rest)

isNumericToken :: String -> Bool
isNumericToken s = case (reads s :: [(Double, String)]) of
  [(_, "")] -> True
  _         -> False

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

-- | --format PNG/SVG が指定されていれば、AnalysisReport のプロットを
--   個別画像として書き出す (HTML 本体に加えて補助出力)。
maybeExportReportPlots :: Config -> FilePath -> [NamedPlot] -> IO ()
maybeExportReportPlots cfg htmlPath plots =
  case cfgFormat cfg of
    HTML -> return ()
    fmt  -> do
      let prefix = dropExtension htmlPath
      paths <- writeAnalysisReportPlots prefix fmt plots
      mapM_ (\p -> putStrLn $ "Plot image:          " ++ p) paths

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
  -- Warn: GP only supports single x/y
  case cfgModel cfg of
    GP | length (cfgXCols cfg) /= 1 || length (cfgYCols cfg) /= 1 ->
      hPutStrLn stderr "Warning: GP requires exactly one x column and one y column."
    _ -> return ()

  result <- loadAuto (cfgFile cfg)
  case result of
    Left err -> putStrLn ("Parse error: " ++ err)
    Right df -> do
      putStrLn $ "Loaded " ++ show (numRows df) ++ " rows from " ++ cfgFile cfg
      putStrLn "Columns:"
      mapM_ (TIO.putStrLn . ("  - " <>)) (columnNames df)

      let fmt   = cfgFormat cfg
          xCol1 = head (cfgXCols cfg)

      -- ── Histogram mode ────────────────────────────────────────────────────
      if cfgHistMode cfg
        then runHistogram cfg df fmt xCol1
        else case cfgModel cfg of
               GP  -> runGP cfg df xCol1
               HBM -> runHBM cfg df xCol1
               _   -> runAnalysis cfg df fmt xCol1

-- ---------------------------------------------------------------------------
-- Mixed model (LME / GLMM)
-- ---------------------------------------------------------------------------

runMixedModel :: Config -> DataFrame -> OutputFormat -> T.Text -> T.Text -> T.Text -> IO ()
runMixedModel cfg df fmt xCol1 yCol grpCol = do
  let colDegs = applyDegreeSpec (cfgDegree cfg) (cfgXCols cfg)
      (dist, lnk) = case cfgModel cfg of
        LM    -> (Gaussian, Identity)
        GLM   -> (cfgDist cfg, cfgLink cfg)
        _     -> (Gaussian, Identity)  -- unreachable (NoReg/GP)

  let mResult = case cfgModel cfg of
        LM    -> fitLMEDataFrame  colDegs grpCol yCol df
        GLM   -> fitGLMMDataFrame dist lnk colDegs grpCol yCol df
        _     -> Nothing

  case mResult of
    Nothing -> putStrLn "\nError: column(s) not found or not numeric/text"
    Just gr -> do
      let modelKind = case cfgModel cfg of
            LM  -> "LME (Gaussian, exact EM)"
            GLM -> "GLMM (" ++ modelLabel dist lnk ++ ", Laplace)"
            _   -> ""
          cs = coeffList (glmmFixed gr)

      putStrLn $ "\nModel: " ++ T.unpack yCol ++ " ~ "
              ++ modelFormula colDegs
              ++ "  [" ++ modelKind ++ " | group: " ++ T.unpack grpCol ++ "]"

      putStrLn "Fixed effects:"
      mapM_ (\(lbl, v) -> printf "  %-30s = %9.4f\n" lbl v)
            (zip (multiCoeffLabels colDegs) cs)

      putStrLn "Variance components:"
      printf "  %-30s = %9.4f\n" ("σ²_u (" ++ T.unpack grpCol ++ ")") (glmmRandVar gr)
      case cfgModel cfg of
        LM  -> printf "  %-30s = %9.4f\n" ("σ² (residual)" :: String) (glmmResidVar gr)
        _   -> printf "  %-30s   (fixed by family)\n" ("σ² (residual)" :: String)
      printf "  %-30s = %9.4f  (%d%% between-group)\n"
             ("ICC" :: String) (glmmICC gr)
             (round (glmmICC gr * 100) :: Int)

      putStrLn $ "BLUPs (" ++ T.unpack grpCol ++ "):"
      mapM_ (\(g, u) -> printf "  %-12s = %+9.4f\n" g u)
            (zip (map T.unpack (V.toList (glmmGroups gr))) (V.toList (glmmBLUPs gr)))

      let suffix = "  [" <> T.pack modelKind <> " | group: " <> grpCol <> "]"

      -- Scatter with group-level fitted lines (single x only)
      case (length (cfgXCols cfg), getNumeric xCol1 df, getNumeric yCol df, getText grpCol df) of
        (1, Just xVec, Just yVec, Just gVec) -> do
          let ptData = zip3 (V.toList gVec) (V.toList xVec) (V.toList yVec)
              lnData = computeGroupLines lnk cs colDegs
                         (glmmGroups gr) (glmmBLUPs gr) xVec
              scatterPath = "scatter.html"
              scatterCfg  = defaultConfig (xCol1 <> " vs " <> yCol <> suffix)
          scatterWithGroupsFile fmt scatterPath scatterCfg xCol1 yCol ptData lnData
          putStrLn $ "\nScatter plot:        " ++ scatterPath
          openInBrowser scatterPath
        _ ->
          putStrLn "\n(Scatter plot skipped for multiple x columns)"

      -- Predicted vs Actual
      case getNumeric yCol df of
        Nothing   -> return ()
        Just yVec -> do
          let pvsaPath = "pvsa.html"
              pvsaCfg  = defaultConfig ("Predicted vs Actual" <> suffix)
          predictedVsActualFile fmt pvsaPath pvsaCfg (V.toList yVec) (fittedList (glmmFixed gr))
          putStrLn $ "Predicted vs Actual: " ++ pvsaPath
          openInBrowser pvsaPath

      -- ── HTML レポート生成 ──────────────────────────────────────────────────
      case cfgReport cfg of
        Nothing   -> return ()
        Just path -> do
          -- WAIC/LOO 計算 (--waic 指定時、Gaussian/Identity の LME のみ)
          mModelSel <-
            if cfgWAIC cfg && dist == Gaussian && lnk == Identity
            then case (getNumeric yCol df, getText grpCol df) of
                   (Just yVec, Just gVec) -> do
                     let xVecPairs = [ (xv, deg) | (xc, deg) <- colDegs
                                     , Just xv <- [getNumeric xc df] ]
                     case xVecPairs of
                       [] -> return Nothing
                       _  -> do
                         let dm = multiPolyDesignMatrix xVecPairs
                             y  = LA.fromList (V.toList yVec)
                             groupLabels = V.toList (glmmGroups gr)
                             blupsList   = V.toList (glmmBLUPs gr)
                             blupMap     = zip groupLabels blupsList
                             offsets     = [ maybe 0 id (lookup g blupMap)
                                           | g <- V.toList gVec ]
                             nSamples    = 1000
                         gen <- createSystemRandom
                         llMat <- lmePosteriorLogLiks
                                    dm y offsets (glmmFixed gr) nSamples gen
                         let w = waic llMat
                             l = loo  llMat
                         printf "  WAIC=%.2f  LOO=%.2f  p_WAIC=%.2f  k̂>0.7: %d件 (条件付き)\n"
                                (waicValue w) (looValue l) (waicPwaic w) (looKHatBad l)
                         return (Just (w, l))
                   _ -> return Nothing
            else return Nothing

          let baseSummary = mkGLMMSummary dist lnk colDegs grpCol Nothing gr
              summary     = baseSummary { gsModelSelect = mModelSel }
              rptCfg  = AnalysisReportConfig
                          { arcTitle = T.pack modelKind
                                     <> ": " <> yCol <> " | " <> grpCol }
              -- Vega-Lite specs
              scatterPlots =
                case (length (cfgXCols cfg), getNumeric xCol1 df, getNumeric yCol df, getText grpCol df) of
                  (1, Just xVec, Just yVec, Just gVec) ->
                    let ptData  = zip3 (V.toList gVec) (V.toList xVec) (V.toList yVec)
                        lnData  = computeGroupLines lnk cs colDegs (glmmGroups gr) (glmmBLUPs gr) xVec
                        scCfg   = defaultConfig (xCol1 <> " vs " <> yCol <> suffix)
                    in [NamedPlot "vl-scatter" "グループ別散布図"
                         (scatterWithGroups scCfg xCol1 yCol ptData lnData)]
                  _ -> []
              pvsaPlots =
                case getNumeric yCol df of
                  Just yVec ->
                    let pvCfg = defaultConfig ("Predicted vs Actual" <> suffix)
                    in [NamedPlot "vl-pvsa" "Predicted vs Actual"
                         (predictedVsActual pvCfg (V.toList yVec) (fittedList (glmmFixed gr)))]
                  Nothing -> []
              plots = scatterPlots ++ pvsaPlots
          writeAnalysisReport path rptCfg df (cfgXCols cfg) yCol (MixFit summary) plots
          putStrLn $ "Report:              " ++ path
          maybeExportReportPlots cfg path plots
          openInBrowser path

-- ---------------------------------------------------------------------------
-- GLM regression (no random effects)
-- ---------------------------------------------------------------------------

runRegression :: Config -> DataFrame -> OutputFormat -> T.Text -> T.Text -> IO ()
runRegression cfg df fmt xCol1 yCol = do
  let colDegs = applyDegreeSpec (cfgDegree cfg) (cfgXCols cfg)
      (dist, lnk) = case cfgModel cfg of
        LM  -> (Gaussian, Identity)
        GLM -> (cfgDist cfg, cfgLink cfg)
        _   -> (Gaussian, Identity)  -- unreachable (NoReg/GP)

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

      -- ── HTML レポート生成 ──────────────────────────────────────────────────
      case cfgReport cfg of
        Nothing   -> return ()
        Just path -> do
          let -- SmoothFit → SmoothData 変換 (単回帰のみ)
              mSmoothData = case (mSmooth, cfgXCols cfg) of
                (Just sf, [xc]) -> Just (xc, SmoothData
                  { sdXs      = sfX sf
                  , sdYs      = sfFit sf
                  , sdLower   = sfLower sf
                  , sdUpper   = sfUpper sf
                  , sdHasBand = sfHasBand sf })
                _               -> Nothing
              baseSummary = mkFitSummary dist lnk colDegs mSmoothData res
              rptCfg  = AnalysisReportConfig
                          { arcTitle = T.pack (modelLabel dist lnk)
                                     <> ": " <> yCol <> " ~ "
                                     <> T.pack (modelFormula colDegs) }
              pvsaPlots = case getNumeric yCol df of
                Just yVec ->
                  let pvCfg = defaultConfig ("Predicted vs Actual" <> titleSuffix)
                  in [NamedPlot "vl-pvsa" "Predicted vs Actual"
                       (predictedVsActual pvCfg (V.toList yVec) (fittedList res))]
                Nothing -> []

          -- ── WAIC/LOO-CV 計算 (--waic が指定された場合) ────────────────────
          mModelSelect <-
            if not (cfgWAIC cfg)
            then return Nothing
            else case getNumeric yCol df of
              Nothing   -> return Nothing
              Just yVec -> do
                let xVecPairs = [ (xv, deg)
                                | (xc, deg) <- colDegs
                                , Just xv   <- [getNumeric xc df] ]
                case xVecPairs of
                  [] -> return Nothing
                  _  -> do
                    let dm = multiPolyDesignMatrix xVecPairs
                        y  = LA.fromList (V.toList yVec)
                        nSamples = 1000 :: Int
                    gen <- createSystemRandom
                    llMat <- case dist of
                      Gaussian -> lmPosteriorLogLiks dm y res nSamples gen
                      _        -> do
                        let (_, fisherInv) = fitGLMFull dist lnk dm y
                        glmPosteriorLogLiks dist lnk dm y fisherInv res nSamples gen
                    let w = waic llMat
                        l = loo  llMat
                    printf "  WAIC=%.2f  LOO=%.2f  p_WAIC=%.2f  k̂>0.7: %d件\n"
                           (waicValue w) (looValue l) (waicPwaic w) (looKHatBad l)
                    return (Just (w, l))

          let summary = baseSummary { fsModelSelect = mModelSelect }
          writeAnalysisReport path rptCfg df (cfgXCols cfg) yCol (RegFit summary) pvsaPlots
          putStrLn $ "Report:              " ++ path
          maybeExportReportPlots cfg path pvsaPlots
          openInBrowser path

-- ---------------------------------------------------------------------------
-- Regression / scatter dispatch (non-histogram path)
-- ---------------------------------------------------------------------------

runAnalysis :: Config -> DataFrame -> OutputFormat -> T.Text -> IO ()
runAnalysis cfg df fmt xCol1 = do
  let yCols    = cfgYCols cfg
      effModel = if length yCols > 1 then NoReg
                 else if cfgModel cfg == GP then NoReg  -- GP はここに来ない
                 else cfgModel cfg

  case effModel of
    -- ── No regression: scatter plot only ──────────────────────────────────
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

    -- ── Regression (LM / GLM) ─────────────────────────────────────────────
    _ -> case yCols of
      [yCol] ->
        case cfgGroup cfg of
          Just grpCol -> runMixedModel cfg df fmt xCol1 yCol grpCol
          Nothing     -> runRegression cfg df fmt xCol1 yCol
      _ -> do
        putStrLn "\nNote: regression with multiple y columns not supported. Plotting scatter only."
        let scatterPath = "scatter.html"
            scatterCfg  = defaultConfig (xCol1 <> " vs " <> T.intercalate ", " yCols)
        scatterMultiYFile fmt scatterPath scatterCfg df xCol1 yCols
        putStrLn $ "Scatter plot (multi-y): " ++ scatterPath
        openInBrowser scatterPath

-- ---------------------------------------------------------------------------
-- GP regression
-- ---------------------------------------------------------------------------

runGP :: Config -> DataFrame -> T.Text -> IO ()
runGP cfg df xCol1 = do
  let yCol = head (cfgYCols cfg)
  case (getNumeric xCol1 df, getNumeric yCol df) of
    (Just xVec, Just yVec) -> do
      let xs = V.toList xVec
          ys = V.toList yVec
          p0 = initParamsFromData xs ys

      putStrLn "\nFitting GP kernels (this may take a moment)..."

      let kernelDefs = [(RBF, "RBF"), (Matern52, "Mat\xe9rn5/2"), (Periodic, "Periodic")]
          xMin = V.minimum xVec
          xMax = V.maximum xVec
          span' = max 1e-8 (xMax - xMin)
          testXs = [ xMin + fromIntegral i * span' / 199 | i <- [0 .. 199 :: Int] ]

      kfits <- mapM (\(ker, lbl) -> do
        putStrLn $ "  Optimizing " ++ lbl ++ " ..."
        let params = optimizeGP ker xs ys p0
            model  = GPModel ker params
            res    = fitGP model xs ys testXs
            lml    = logMarginalLikelihood xs ys ker params
            pd     = gpPredData model xs ys
        return GPKernelFit
          { gkLabel    = T.pack lbl
          , gkKernel   = ker
          , gkParams   = params
          , gkResult   = res
          , gkLML      = lml
          , gkPredData = pd
          }
        ) kernelDefs

      -- LML 降順にソート
      let sorted = foldr insertByLML [] kfits
          insertByLML x [] = [x]
          insertByLML x (y:ys') = if gkLML x >= gkLML y then x:y:ys'
                                  else y : insertByLML x ys'
          gfSummary = GPFitSummary
            { gfKernelFits = sorted
            , gfXCol       = xCol1
            , gfYCol       = yCol
            , gfTrainXs    = xs
            , gfTrainYs    = ys
            }
          path   = maybe "report.html" id (cfgReport cfg)
          rptCfg = AnalysisReportConfig
            { arcTitle = "GP Regression: " <> xCol1 <> " \x2192 " <> yCol }

      writeAnalysisReport path rptCfg df [xCol1] yCol (GPFit gfSummary) []
      putStrLn $ "Report: " ++ path
      maybeExportReportPlots cfg path []
      openInBrowser path

    _ -> putStrLn "\nError: column(s) not found or not numeric"

-- ---------------------------------------------------------------------------
-- HBM (Bayesian linear regression via NUTS)
-- ---------------------------------------------------------------------------

runHBM :: Config -> DataFrame -> T.Text -> IO ()
runHBM cfg df xCol = do
  let yCols = cfgYCols cfg
      xCols = cfgXCols cfg
  case (yCols, xCols, getNumeric xCol df) of
    ([yCol], [_], Just xVec) ->
      case getNumeric yCol df of
        Nothing -> putStrLn $ "Error: y column '" ++ T.unpack yCol ++ "' not numeric"
        Just yVec -> do
          let xs = V.toList xVec
              ys = V.toList yVec
          putStrLn ""
          putStrLn "=== HBM Bayesian Linear Regression ==="
          printf "  y = α + β·x + ε,  α,β ~ Normal(0,10),  ε ~ Normal(0,σ),  σ ~ Exp(1)\n"
          printf "  サンプリング: NUTS (AD 勾配 + dual averaging)\n"
          printf "  N = %d 観測, x = %s, y = %s\n\n"
                 (length xs) (T.unpack xCol) (T.unpack yCol)
          runHBMRegression xs ys xCol yCol df cfg
    _ ->
      putStrLn "Error: HBM requires exactly one x and one y column (numeric)"

runHBMRegression
  :: [Double] -> [Double] -> T.Text -> T.Text -> DataFrame -> Config -> IO ()
runHBMRegression xs ys xCol yCol df cfg = do
  let nutsCfg = HBMnuts.defaultNUTSConfig
                  { HBMnuts.nutsIterations = 1500
                  , HBMnuts.nutsBurnIn     = 500
                  , HBMnuts.nutsStepSize   = 0.05
                  }
      initP   = Map.fromList
                  [ ("alpha", 0.0), ("beta", 0.0), ("sigma", 1.0) ]
      hbmModel :: HBMod.ModelP ()
      hbmModel = do
        a <- HBMod.sample "alpha" (HBMod.Normal 0 10)
        b <- HBMod.sample "beta"  (HBMod.Normal 0 10)
        s <- HBMod.sample "sigma" (HBMod.Exponential 1)
        mapM_ (\(x, y) ->
                 let xC = realToFrac x
                 in HBMod.observe "y" (HBMod.Normal (a + b * xC) s) [y])
              (zip xs ys)

  gen <- createSystemRandom
  chain <- HBMnuts.nuts hbmModel nutsCfg initP gen
  let acc = MCMCcore.acceptanceRate chain
      n   = length (MCMCcore.chainSamples chain)
  printf "  受容率: %.1f%%, サンプル数: %d\n" (acc * 100 :: Double) n

  let aMean = maybe 0 id (MCMCcore.posteriorMean "alpha" chain)
      aSD   = maybe 0 id (MCMCcore.posteriorSD   "alpha" chain)
      bMean = maybe 0 id (MCMCcore.posteriorMean "beta"  chain)
      bSD   = maybe 0 id (MCMCcore.posteriorSD   "beta"  chain)
      sMean = maybe 0 id (MCMCcore.posteriorMean "sigma" chain)
      sSD   = maybe 0 id (MCMCcore.posteriorSD   "sigma" chain)
  printf "  α = %+.4f ± %.4f\n" aMean aSD
  printf "  β = %+.4f ± %.4f\n" bMean bSD
  printf "  σ = %+.4f ± %.4f\n" sMean sSD

  case cfgReport cfg of
    Nothing   -> return ()
    Just path -> do
      let smooth = makeSmooth xs chain
          fitted = [aMean + bMean * x | x <- xs]
          resid  = zipWith (-) ys fitted
          yBar   = sum ys / fromIntegral (length ys)
          tss    = sum [(y - yBar) ^ (2::Int) | y <- ys]
          rss    = sum [r ^ (2::Int) | r <- resid]
          r2     = if tss < 1e-12 then 0 else 1 - rss / tss

      mWaicLoo <-
        if cfgWAIC cfg
          then do
            let llMat = [ HBMod.perObsLogLiks hbmModel ps
                        | ps <- MCMCcore.chainSamples chain ]
                w = waic llMat
                l = loo  llMat
            printf "  WAIC=%.2f  LOO=%.2f  p_WAIC=%.2f  k̂>0.7: %d件\n"
                   (waicValue w) (looValue l) (waicPwaic w) (looKHatBad l)
            return (Just (w, l))
          else return Nothing

      let fs = FitSummary
                 { fsModelType    = "Bayesian Linear Regression (HBM, NUTS)"
                 , fsFormula      = "y ~ α + β · " <> xCol
                 , fsCoeffs       = [("α (Intercept)", aMean), ("β (" <> xCol <> ")", bMean)]
                 , fsR2           = r2
                 , fsR2Label      = "R²"
                 , fsFitted       = fitted
                 , fsResiduals    = resid
                 , fsLinkName     = "Normal (identity link)"
                 , fsXColDegs     = [(xCol, 1)]
                 , fsSmoothData   = Just (xCol, smooth)
                 , fsModelSelect  = mWaicLoo
                 }
          hs = HBMRegSummary
                 { hbmsFit           = fs
                 , hbmsModelGraph    = HBMod.buildModelGraph hbmModel
                 , hbmsChain         = chain
                 , hbmsParams        = ["alpha", "beta", "sigma"]
                 , hbmsPosteriorRows =
                     [ ("alpha", aMean, aSD
                       , maybe 0 id (MCMCcore.posteriorQuantile 0.025 "alpha" chain)
                       , maybe 0 id (MCMCcore.posteriorQuantile 0.975 "alpha" chain))
                     , ("beta",  bMean, bSD
                       , maybe 0 id (MCMCcore.posteriorQuantile 0.025 "beta"  chain)
                       , maybe 0 id (MCMCcore.posteriorQuantile 0.975 "beta"  chain))
                     , ("sigma", sMean, sSD
                       , maybe 0 id (MCMCcore.posteriorQuantile 0.025 "sigma" chain)
                       , maybe 0 id (MCMCcore.posteriorQuantile 0.975 "sigma" chain))
                     ]
                 }
          diagCfg = PlotConfig "MCMC 診断 (KDE + トレース)" 760 320
          acfCfg  = PlotConfig "自己相関 (lag 0..40)" 760 220
          diagPlot = NamedPlot "vl-hbm-diag" "MCMC 診断"
                       (mcmcDiagnostics diagCfg ["alpha","beta","sigma"] chain)
          acfPlot  = NamedPlot "vl-hbm-acf"  "自己相関"
                       (autocorrPlot acfCfg 40 ["alpha","beta","sigma"] chain)
          rptCfg = AnalysisReportConfig
                     { arcTitle = "HBM Linear Regression: " <> yCol <> " ~ " <> xCol }
      writeAnalysisReport path rptCfg df [xCol] yCol (HBMFit hs) [diagPlot, acfPlot]
      putStrLn $ "Report:              " ++ path
      maybeExportReportPlots cfg path [diagPlot, acfPlot]
      openInBrowser path
  where
    -- 信用区間付き予測曲線: 各事後サンプルから μ* = α + β·x* を計算 → 分位点
    makeSmooth :: [Double] -> MCMCcore.Chain -> SmoothData
    makeSmooth xs0 ch =
      let alphas = MCMCcore.chainVals "alpha" ch
          betas  = MCMCcore.chainVals "beta"  ch
          xMin   = minimum xs0
          xMax   = maximum xs0
          ext    = (xMax - xMin) * 0.5
          grid   = [xMin - ext + i * (xMax - xMin + 2 * ext) / 99 | i <- [0..99]]
          atX x  = let ss     = sortListAsc (zipWith (\a b -> a + b * x) alphas betas)
                       sn     = length ss
                       qAt p  = ss !! min (sn-1) (max 0 (floor (p * fromIntegral sn) :: Int))
                   in (qAt 0.5, qAt 0.025, qAt 0.975)
          (yMid, yLo, yHi) = unzip3 (map atX grid)
      in SmoothData
           { sdXs = grid, sdYs = yMid, sdLower = yLo, sdUpper = yHi
           , sdHasBand = True
           }

    sortListAsc :: [Double] -> [Double]
    sortListAsc = qs
      where
        qs []     = []
        qs (p:rs) = qs [x | x <- rs, x <= p] ++ [p] ++ qs [x | x <- rs, x > p]

-- ---------------------------------------------------------------------------
-- Histogram mode
-- ---------------------------------------------------------------------------

runHistogram :: Config -> DataFrame -> OutputFormat -> T.Text -> IO ()
runHistogram cfg df fmt xCol =
  case getNumeric xCol df of
    Nothing ->
      putStrLn $ "Error: column '" ++ T.unpack xCol ++ "' not found or not numeric"
    Just xVec -> do
      let vals     = V.toList xVec
          histPath = "histogram.html"
          histCfg  = defaultConfig ("Histogram: " <> xCol)
      case cfgFitDist cfg of
        Nothing   -> do
          histogramPlotFile fmt histPath histCfg xCol vals Nothing
          putStrLn $ "\nHistogram: " ++ histPath
        Just dist -> do
          histogramWithDensityFile fmt histPath histCfg xCol vals Nothing dist
          putStrLn $ "\nHistogram + density: " ++ histPath
      openInBrowser histPath

-- ---------------------------------------------------------------------------
-- Group-level prediction helpers
-- ---------------------------------------------------------------------------

invLink :: LinkFn -> Double -> Double
invLink Identity eta = eta
invLink Log      eta = exp eta
invLink Logit    eta = 1.0 / (1.0 + exp (negate eta))
invLink Sqrt     eta = eta * eta

-- | Generate per-group conditional fitted lines for visualization.
-- Only produces data when there is exactly one x column (scatter plot is 2D).
-- Returns [(group, xGrid, ŷ)] evaluated on a 100-point grid over [min(x), max(x)].
computeGroupLines
  :: LinkFn
  -> [Double]           -- fixed coefficients [β₀, β₁, ..., βd]
  -> [(T.Text, Int)]    -- x column / degree specs (length 1 → draw lines)
  -> V.Vector T.Text    -- group labels (sorted)
  -> V.Vector Double    -- BLUPs (same order as group labels)
  -> V.Vector Double    -- observed x values (used to determine grid range)
  -> [(T.Text, Double, Double)]
computeGroupLines lnk coeffs colDegs groups blups xVec =
  case colDegs of
    [(_, deg)] | not (V.null xVec) ->
      let xMin  = V.minimum xVec
          xMax  = V.maximum xVec
          nGrid = 100 :: Int
          grid  = [ xMin + fromIntegral i * (xMax - xMin) / fromIntegral (nGrid - 1)
                  | i <- [0 .. nGrid - 1] ]
          b0    = head coeffs
          bs    = tail coeffs
          etaAt x = b0 + sum (zipWith (*) bs [x ^ k | k <- [1 .. deg :: Int]])
      in [ (grp, x, invLink lnk (etaAt x + u))
         | (grp, u) <- zip (V.toList groups) (V.toList blups)
         , x <- grid ]
    _ -> []

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
