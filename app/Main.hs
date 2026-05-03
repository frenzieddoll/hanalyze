{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import DataIO.CSV        (loadAutoSafeWith, LoadOpts (..), defaultLoadOpts)
import qualified DataIO.Log     as Log
import qualified DataIO.Clean   as Clean
import qualified Stat.Standardize as Std
import qualified DataIO.Preprocess as Pp
import qualified DataFrame                    as DX
import qualified DataFrame.Internal.Column    as DXC
import qualified DataFrame.Internal.DataFrame as DXD
import DataIO.Convert     (getDoubleVec, getTextVec, getMaybeTextVec)
import Model.Core        (Band (..), FitResult, rSquared1, coeffList, fittedList, residualsV)
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
import qualified Design.Orthogonal as OA
import qualified Design.Taguchi as TG
import qualified Viz.Taguchi as VTG
import qualified Viz.ReportBuilder as RB
import qualified Viz.ReportInstances as RI
import qualified Viz.ModelGraph
import qualified Graphics.Vega.VegaLite as VL
import Graphics.Vega.VegaLite (VegaLite, VLProperty, VLSpec)
import qualified Model.Kernel as Kern
import qualified Model.Regularized as Reg
import qualified Model.GAM as GAM
import qualified Model.Quantile as QR
import qualified Model.RandomForest as RF
import qualified Model.RFF as RFF
import qualified Model.Spline as Spl
import Model.LM (SmoothFit (..))
import qualified Model.HBM as HBMod
import qualified MCMC.NUTS as HBMnuts
import qualified MCMC.Core as MCMCcore
import qualified Data.Map.Strict as Map
import Viz.MCMC (mcmcDiagnostics, autocorrPlot)
import Viz.Core (PlotConfig (..))
import Model.GP           (Kernel (..), GPModel (..), GPParams, GPPredData,
                           GPResult, gpMean,
                           initParamsFromData, optimizeGP, fitGP, logMarginalLikelihood,
                           gpPredData)

import Stat.ModelSelect  (lmPosteriorLogLiks, glmPosteriorLogLiks,
                          lmePosteriorLogLiks, waic, loo,
                          WAICResult (..), LOOResult (..))

import Control.Monad      (when)
import Data.Char          (isDigit)
import Data.List          (intercalate, sort)
import qualified Data.Set as Set
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
  , cfgLoadOpts :: LoadOpts           -- --no-header / --skip / --comment / --strict
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
parseArgs args0 =
  let (lopts, args) = parseLoadOpts args0
  in case args of
    (file : xColsStr : yColsStr : rest) -> do
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
                , cfgLoadOpts = lopts
                }
    _ -> Left usageMsg

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
-- CLI report builders (Phase 2: regress --report → ReportBuilder 経路)
-- ---------------------------------------------------------------------------

-- | NamedPlot を ReportSection に変換 (タイトル付き secVega)。
namedPlotsToSecs :: [NamedPlot] -> [RB.ReportSection]
namedPlotsToSecs nps =
  [ RB.secVega title vega | NamedPlot _ title vega <- nps ]

-- | WAIC/LOO 結果 (オプション) を 1 セクションに整形。
waicSection :: Maybe (WAICResult, LOOResult) -> [RB.ReportSection]
waicSection Nothing = []
waicSection (Just (w, l)) =
  [ RB.secKeyValue "モデル選択 (WAIC / LOO-CV)"
      [ ("WAIC",     T.pack (printf "%.2f" (waicValue w)))
      , ("LOO",      T.pack (printf "%.2f" (looValue l)))
      , ("p_WAIC",   T.pack (printf "%.2f" (waicPwaic w)))
      , ("k\x0302 > 0.7", T.pack (show (looKHatBad l) ++ " 件"))
      ]
  ]

-- | 残差の (σ_hat, RMSE, max|r|)。p は推定パラメータ数 (intercept 含む)。
cliResidStats :: [Double] -> Int -> (Double, Double, Double)
cliResidStats resid p =
  let n     = length resid
      sumSq = sum [ r * r | r <- resid ]
      sH    = sqrt (sumSq / fromIntegral (max 1 (n - p)))
      rmse  = sqrt (sumSq / fromIntegral (max 1 n))
      mAbs  = maximum (0 : map abs resid)
  in (sH, rmse, mAbs)

-- | LM / GLM 用 CLI レポートセクション群。多項式次数と WAIC/LOO に対応。
cliRegressSections
  :: Config -> DXD.DataFrame -> Family -> LinkFn
  -> [(T.Text, Int)] -> FitResult -> Maybe SmoothFit
  -> Maybe (WAICResult, LOOResult)
  -> [NamedPlot]
  -> [RB.ReportSection]
cliRegressSections cfg df dist lnk colDegs res mSmooth mModelSel pvsaPlots =
  let xCols   = cfgXCols cfg
      yCol    = case cfgYCols cfg of (y:_) -> y; _ -> "y"
      beta    = coeffList res
      coefLbls = map T.pack (multiCoeffLabels colDegs)
      coeffs  = zip coefLbls beta
      fitted  = fittedList res
      resid   = LA.toList (residualsV res)
      p       = length beta
      (sigmaH, rmse, maxAbs) = cliResidStats resid p
      r2      = rSquared1 res
      r2Lbl   = T.pack (r2Label dist)
      isLM    = dist == Gaussian
      isPoly  = any (\(_, d) -> d > 1) colDegs
      modelType
        | isLM      = if isPoly then "LM (polynomial)" else "LM"
        | otherwise = "GLM(" <> T.pack (show dist) <> ")"

      formulaTex
        | isLM = "$" <> yCol <> "_i = "
                 <> T.intercalate " + "
                     ("\\beta_0" :
                       [ "\\beta_" <> T.pack (show (i :: Int)) <> " " <> trm
                       | (i, trm) <- zip [1 ..] (polyTerms colDegs) ])
                 <> " + \\varepsilon_i$<br>"
                 <> "$\\varepsilon_i \\sim \\text{Normal}(0, \\sigma^2)$"
        | otherwise =
            "$g(\\mu_i) = "
            <> T.intercalate " + "
                ("\\beta_0" :
                  [ "\\beta_" <> T.pack (show (i :: Int)) <> " " <> trm
                  | (i, trm) <- zip [1 ..] (polyTerms colDegs) ])
            <> "$<br>"
            <> "$" <> yCol <> "_i \\sim \\text{" <> T.pack (show dist) <> "}(\\mu_i)$"

      smoothC = case mSmooth of
        Just sf -> RB.SmoothCurve (sfX sf) (sfFit sf) (sfLower sf) (sfUpper sf)
        Nothing -> RB.SmoothCurve [] [] [] []

      scatterCard = case (xCols, mSmooth) of
        ([xc], Just _) -> case (getDoubleVec xc df, getDoubleVec yCol df) of
          (Just xv, Just yv) ->
            [ RB.secCard "散布図 + 回帰線"
                [ RB.secFitScatter xc yCol (V.toList xv) (V.toList yv)
                    (Just smoothC) ] ]
          _ -> []
        _ -> []

      -- 対話的予測: 多項式拡張の場合は係数数と x 列数が合わないので省略。
      interactiveSecs = case (isPoly, traverse (`getDoubleVec` df) xCols, getDoubleVec yCol df) of
        (False, Just xVs, Just yV) | not (null xVs) ->
          let xRows = [ [ xv V.! i | xv <- xVs ]
                      | i <- [0 .. V.length yV - 1] ]
              mkSlider xv =
                let lo = V.minimum xv
                    hi = V.maximum xv
                    ext = (hi - lo) * 0.5
                in (lo - ext, (lo + hi) / 2, hi + ext)
              im = RB.InteractiveModel
                     { RB.imXCols     = xCols
                     , RB.imYCol      = yCol
                     , RB.imXValues   = xRows
                     , RB.imYValues   = V.toList yV
                     , RB.imIntercept = head beta
                     , RB.imBetas     = drop 1 beta
                     , RB.imLink      = T.pack (linkLabelLower lnk)
                     , RB.imSlider    = map mkSlider xVs
                     , RB.imCISigma   = if isLM then Just sigmaH else Nothing
                     }
          in [RB.secInteractiveMulti "対話的予測" im]
        _ -> []

      statRow =
        RB.secStatRow
          [ (r2Lbl,         T.pack (printf "%.4f" r2))
          , ("方法",        if isLM then "OLS (QR)" else "IRLS")
          , ("σ_hat",      T.pack (printf "%.4f" sigmaH))
          , ("RMSE",        T.pack (printf "%.4f" rmse))
          , ("最大絶対残差", T.pack (printf "%.4f" maxAbs))
          ]

      resultSec =
        RB.secCollapsible "<span class=\"sec-icon\">&#128200;</span> 回帰結果" True
          ([ statRow
           , RB.secCard "係数"
               [RB.secCoefficients coeffs (Just (r2Lbl, r2))]
           ]
           ++ scatterCard
           ++ [RB.secCard "残差プロット" [RB.secResiduals fitted resid]])

      modelSec
        | isLM      = RB.secModelOverview modelType formulaTex Nothing
        | otherwise = RB.secModelOverviewLink modelType formulaTex
                        (T.pack (linkLabelLower lnk)) Nothing

      extraPlotSecs = namedPlotsToSecs pvsaPlots

  in [ RB.secDataOverview df xCols yCol
     , modelSec
     , resultSec
     ] ++ interactiveSecs ++ extraPlotSecs ++ waicSection mModelSel

-- | colDegs を polynomial 項の文字列に展開: [(x, 2), (z, 1)] → ["x", "x^2", "z"]
polyTerms :: [(T.Text, Int)] -> [T.Text]
polyTerms = concatMap (\(c, d) ->
  [ if k == 1 then c else c <> "^" <> T.pack (show k) | k <- [1 .. d] ])

-- | リンク関数を JS 側のリンク名に対応させる (identity / log / logit / sqrt)。
linkLabelLower :: LinkFn -> String
linkLabelLower Identity = "identity"
linkLabelLower Log      = "log"
linkLabelLower Logit    = "logit"
linkLabelLower Sqrt     = "sqrt"

-- | GLMM (LME) 用 CLI レポートセクション群。
cliMixedSections
  :: Config -> DXD.DataFrame -> Family -> LinkFn
  -> [(T.Text, Int)] -> T.Text -> GLMMResult -> Maybe (WAICResult, LOOResult)
  -> [NamedPlot]
  -> [RB.ReportSection]
cliMixedSections cfg df dist lnk colDegs grpCol gr mModelSel extraPlots =
  let xCols    = cfgXCols cfg
      yCol     = case cfgYCols cfg of (y:_) -> y; _ -> "y"
      base     = RB.toReport (RB.defaultReportConfig "") df xCols yCol
                   (RI.GLMMReport gr dist lnk grpCol)
      colDegInfo =
        [ RB.secKeyValue "Polynomial degrees"
            [ (c, T.pack (show d)) | (c, d) <- colDegs, d > 1 ]
        | any (\(_, d) -> d > 1) colDegs
        ]
  in base ++ colDegInfo ++ namedPlotsToSecs extraPlots ++ waicSection mModelSel

-- | GP 用 CLI レポートセクション群。マルチカーネル比較対応。
-- 呼び出し側で予測グリッド X (`gridX`) を渡す。
cliGPSections
  :: T.Text -> T.Text -> DXD.DataFrame -> [Double] -> [Double]
  -> [Double]              -- ^ 予測グリッド X
  -> [GPKernelFit]
  -> [RB.ReportSection]
cliGPSections xCol yCol df xs ys gridX kfits =
  let bestK = case kfits of (k:_) -> Just k; _ -> Nothing
      mainSec = case bestK of
        Just kf ->
          RB.toReport (RB.defaultReportConfig "") df [xCol] yCol
            (RI.GPReport (gkKernel kf) (gkParams kf) (gkResult kf)
                          gridX xs ys (gkLML kf))
        Nothing -> []
      cmpRows = [ [ gkLabel kf
                  , T.pack (printf "%.2f" (gkLML kf)) ]
                | kf <- kfits ]
      cmpSec = case kfits of
        []  -> []
        [_] -> []
        _   -> [RB.secComparisonTable
                  "カーネル比較 (LML 降順)"
                  ["カーネル", "log p(y|X,θ)"] cmpRows (Just 0)]
  in mainSec ++ cmpSec

-- | HBM 用 CLI レポートセクション群。
cliHBMSections
  :: T.Text -> T.Text -> DXD.DataFrame -> [Double] -> [Double]
  -> MCMCcore.Chain -> Maybe T.Text -> Maybe (WAICResult, LOOResult)
  -> [NamedPlot]
  -> [RB.ReportSection]
cliHBMSections xCol yCol df xs ys chain mGraph mModelSel extraPlots =
  let rep = RI.HBMLinearReport
              { RI.hbmrChain     = chain
              , RI.hbmrXs        = xs
              , RI.hbmrYs        = ys
              , RI.hbmrAlphaName = "alpha"
              , RI.hbmrBetaName  = "beta"
              , RI.hbmrSigmaName = "sigma"
              , RI.hbmrGraph     = mGraph
              }
      base = RB.toReport (RB.defaultReportConfig "") df [xCol] yCol rep
      _    = xCol  -- silence warning
      _    = yCol
  in base ++ namedPlotsToSecs extraPlots ++ waicSection mModelSel

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Subcommand dispatcher (Phase C: hybrid CLI)
--
-- Top-level usage:
--   hanalyze <subcommand> [args...]
--   hanalyze <file> <xcols> <ycols> [LM|GLM|...] [opts]   (legacy = regress)
--
-- Implemented:  regress, info, hist, help
-- Stubs:        ridge, kernel, spline, doe, taguchi
-- ---------------------------------------------------------------------------

helpMsg :: String
helpMsg = unlines
  [ "hanalyze \x2014 general-purpose statistical analysis & visualization toolkit"
  , ""
  , "Usage: hanalyze <subcommand> [args...]"
  , "       hanalyze <file> <xcols> <ycols> [LM|GLM|NoReg|GP|HBM] [opts]   (legacy = regress)"
  , ""
  , "Subcommands:"
  , "  regress   Classical/Bayesian regression (LM/GLM/GLMM/GP/HBM)         [implemented]"
  , "  info      Print per-column type and basic statistics                 [implemented]"
  , "  hist      Plot a histogram (optionally with theoretical density)     [implemented]"
  , "  ridge     Regularized regression (Ridge/Lasso/Elastic Net)           [implemented]"
  , "  kernel    Kernel regression / RFF approximation                      [implemented]"
  , "  spline    B-spline / natural cubic regression                        [implemented]"
  , "  quantile  Quantile regression (τ-quantile, MM-IRLS)                  [implemented]"
  , "  gam       Generalized Additive Model (additive B-splines + Ridge)   [implemented]"
  , "  rf        Random Forest regression (CART + bagging + feature subset) [implemented]"
  , "  doe       Orthogonal arrays (L_n) for experimental designs           [implemented]"
  , "  taguchi   Taguchi method (SN ratio + factor effects + inner/outer)   [implemented]"
  , ""
  , "  help      Show this message"
  , "  --help, -h, help   Same as 'help'"
  , ""
  , "Run 'hanalyze regress' (or invoke without a subcommand) to see regression-specific options."
  ]

futureSubcommands :: [(String, String)]
futureSubcommands =
  [
  ]

isFutureSubcommand :: String -> Bool
isFutureSubcommand c = c `elem` map fst futureSubcommands

stubMessage :: String -> String
stubMessage c = case lookup c futureSubcommands of
  Just msg -> msg
  Nothing  -> "subcommand '" ++ c ++ "' is not yet implemented"

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch []                           = putStrLn helpMsg
dispatch ("--help":_)                 = putStrLn helpMsg
dispatch ("-h":_)                     = putStrLn helpMsg
dispatch ("help":_)                   = putStrLn helpMsg
dispatch ("info":rest)                = runInfoCmd rest
dispatch ("hist":rest)                = runHistCmd rest
dispatch ("regress":rest)             = runRegressCmd rest
dispatch ("doe":rest)                 = runDoeCmd rest
dispatch ("taguchi":rest)             = runTaguchiCmd rest
dispatch ("ridge":rest)               = runRidgeCmd rest
dispatch ("kernel":rest)              = runKernelCmd rest
dispatch ("spline":rest)              = runSplineCmd rest
dispatch ("quantile":rest)            = runQuantileCmd rest
dispatch ("gam":rest)                 = runGAMCmd rest
dispatch ("rf":rest)                  = runRFCmd rest
dispatch ("clean":rest)               = runCleanCmd rest
dispatch ("melt":rest)                = runMeltCmd rest
dispatch (cmd:_) | isFutureSubcommand cmd = do
  hPutStrLn stderr $ "hanalyze: " ++ stubMessage cmd
  hPutStrLn stderr "  Run 'hanalyze help' to see implemented subcommands."
dispatch args                         = runRegressCmd args  -- legacy / bare

runRegressCmd :: [String] -> IO ()
runRegressCmd args = case parseArgs args of
  Left err  -> hPutStrLn stderr err
  Right cfg -> runConfig cfg

-- ---------------------------------------------------------------------------
-- info subcommand
-- ---------------------------------------------------------------------------

runInfoCmd :: [String] -> IO ()
runInfoCmd args = do
  let (lopts, rest) = parseLoadOpts args
  case rest of
    []        -> hPutStrLn stderr
                   "Usage: hanalyze info <file> [--no-header] [--skip N] [--comment CH] [--strict]"
    (file:_)  -> do
      result <- loadAutoSafeWith lopts file
      case result of
        Left err          -> hPutStrLn stderr ("Parse error: " ++ err)
        Right (df, lg)    -> do
          Log.printLogReport lg
          printDataFrameInfo file df

-- | 共通フラグを切り出す: '--no-header' / '--skip N' / '--comment CH' /
-- '--strict' を 'LoadOpts' に集約し、残った位置引数を返す。
parseLoadOpts :: [String] -> (LoadOpts, [String])
parseLoadOpts = go defaultLoadOpts []
  where
    go acc rs []                              = (acc, reverse rs)
    go acc rs ("--no-header":xs)              = go acc { loNoHeader = True } rs xs
    go acc rs ("--strict":xs)                 = go acc { loStrict   = True } rs xs
    go acc rs ("--no-sniff":xs)               = go acc { loSniff    = False } rs xs
    go acc rs ("--skip":n:xs)
      | Just k <- readMaybeInt n              = go acc { loSkip = k } rs xs
    go acc rs ("--comment":cs:xs)
      | (c:_) <- cs                           = go acc { loComment = Just c } rs xs
    go acc rs (x:xs)                          = go acc (x:rs) xs

readMaybeInt :: String -> Maybe Int
readMaybeInt s = case reads s of
  [(n, "")] -> Just n
  _         -> Nothing

-- ---------------------------------------------------------------------------
-- clean subcommand (Phase C)
-- ---------------------------------------------------------------------------

runCleanCmd :: [String] -> IO ()
runCleanCmd args0 = do
  let (lopts, args1) = parseLoadOpts args0
      (rules, out, args2) = parseCleanFlags args1
  case args2 of
    [] -> hPutStrLn stderr cleanUsage
    (file:_) -> do
      result <- loadAutoSafeWith lopts file
      case result of
        Left err          -> hPutStrLn stderr ("Parse error: " ++ err)
        Right (df0, lg0)  -> do
          Log.printLogReport lg0
          let (df1, lg1) = Clean.cleanPipeline rules df0
          Log.printLogReport lg1
          case out of
            Nothing -> do
              -- 出力ファイル指定なし: info を出して終わり
              putStrLn "Cleaned DataFrame:"
              putStrLn $ "  Rows / Cols: "
                          ++ show (fst (DX.dimensions df1)) ++ " × "
                          ++ show (length (DX.columnNames df1))
              putStrLn "  Columns:"
              mapM_ (TIO.putStrLn . ("    - " <>)) (DX.columnNames df1)
            Just path -> do
              -- TODO: 簡易 CSV 書出し。今は警告だけ出す。
              hPutStrLn stderr
                ("(--output " ++ path ++ " は未実装。ライブラリ API "
                 ++ "Clean.cleanPipeline + Hackage writeCsv を直接お使いください)")

cleanUsage :: String
cleanUsage = unlines
  [ "Usage: hanalyze clean <file> [--rule COL=RULE]... [--output FILE] [load opts]"
  , ""
  , "Rules (各列に適用):"
  , "  StripUnits        \"12.3kg\" → 12.3"
  , "  ParseCurrency     \"$1,234.56\" → 1234.56"
  , "  ParseDecimalEU    \"3,14\" → 3.14 (decimal point が ,)"
  , "  TrimText          前後空白を除去"
  , "  CoerceNumeric     上記 3 種を順に試す万能変換"
  , ""
  , "例:"
  , "  hanalyze clean data/raw.csv \\"
  , "      --rule price=ParseCurrency \\"
  , "      --rule weight=StripUnits \\"
  , "      --rule note=TrimText"
  , ""
  , "Load opts: --no-header / --skip N / --comment CH / --delim CH / --strict / --no-sniff"
  ]

parseCleanFlags
  :: [String] -> ([(T.Text, Clean.ColumnRule)], Maybe FilePath, [String])
parseCleanFlags = go [] Nothing []
  where
    go rs out kept []                   = (reverse rs, out, reverse kept)
    go rs out kept ("--rule":spec:xs)   = case parseRuleSpec spec of
      Just r  -> go (r:rs) out kept xs
      Nothing -> go rs out kept xs
    go rs _   kept ("--output":p:xs)    = go rs (Just p) kept xs
    go rs _   kept ("-o":p:xs)          = go rs (Just p) kept xs
    go rs out kept (x:xs)               = go rs out (x:kept) xs

-- ---------------------------------------------------------------------------
-- melt subcommand (Phase B/C — wide → long)
-- ---------------------------------------------------------------------------

runMeltCmd :: [String] -> IO ()
runMeltCmd args0 = do
  let (lopts, args1)    = parseLoadOpts args0
      (mopts, args2)    = parseMeltFlags args1
  case args2 of
    []       -> hPutStrLn stderr meltUsage
    (file:_) -> case (moIds mopts, moVars mopts) of
      ([], _) -> hPutStrLn stderr "melt: --id COL1,COL2,... 必須"
      (_, []) -> hPutStrLn stderr "melt: --vars COL1,COL2,... 必須"
      (ids, vars) -> do
        result <- loadAutoSafeWith lopts file
        case result of
          Left err          -> hPutStrLn stderr ("Parse error: " ++ err)
          Right (df0, lg)   -> do
            Log.printLogReport lg
            let df1 = Pp.meltLonger ids vars
                                    (moVarName mopts) (moValueName mopts)
                                    (moParseVar mopts) df0
                (nrows, ncols) = DX.dimensions df1
            putStrLn "Long-form DataFrame:"
            putStrLn $ "  Rows / Cols: " ++ show nrows ++ " × " ++ show ncols
            putStrLn "  Columns:"
            mapM_ (TIO.putStrLn . ("    - " <>)) (DX.columnNames df1)
            case moOut mopts of
              Just path -> do
                writeMeltedCsv path df1
                putStrLn $ "Wrote " ++ path
              Nothing -> return ()

-- | melt 結果を簡易 CSV (Hackage の writeCsv 経由) で書き出す。
writeMeltedCsv :: FilePath -> DXD.DataFrame -> IO ()
writeMeltedCsv path df = DX.writeCsv path df

data MeltOpts = MeltOpts
  { moIds       :: [T.Text]
  , moVars      :: [T.Text]
  , moVarName   :: T.Text
  , moValueName :: T.Text
  , moParseVar  :: Bool
  , moOut       :: Maybe FilePath
  } deriving (Show)

defaultMeltOpts :: MeltOpts
defaultMeltOpts = MeltOpts [] [] "variable" "value" True Nothing

parseMeltFlags :: [String] -> (MeltOpts, [String])
parseMeltFlags = go defaultMeltOpts []
  where
    splitCSV = map T.pack . filter (not . null) . wordsBy (== ',')
    go acc kept []                    = (acc, reverse kept)
    go acc kept ("--id":v:xs)         = go acc { moIds = splitCSV v } kept xs
    go acc kept ("--vars":v:xs)       = go acc { moVars = splitCSV v } kept xs
    go acc kept ("--var":v:xs)        = go acc { moVarName = T.pack v } kept xs
    go acc kept ("--value":v:xs)      = go acc { moValueName = T.pack v } kept xs
    go acc kept ("--no-parse-var":xs) = go acc { moParseVar = False } kept xs
    go acc kept ("--output":p:xs)     = go acc { moOut = Just p } kept xs
    go acc kept ("-o":p:xs)           = go acc { moOut = Just p } kept xs
    go acc kept (x:xs)                = go acc (x:kept) xs

wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p s = case dropWhile p s of
  "" -> []
  s' -> let (w, rest) = break p s' in w : wordsBy p rest

meltUsage :: String
meltUsage = unlines
  [ "Usage: hanalyze melt <file> --id COL1,COL2,... --vars COL1,COL2,..."
  , "                     [--var NAME] [--value NAME]"
  , "                     [--no-parse-var] [--output FILE] [load opts]"
  , ""
  , "wide-form CSV を long-form (tidy) に展開する。"
  , ""
  , "  --id    そのまま残す列 (例: name,x1,x2)"
  , "  --vars  縦方向に展開する wide 列 (例: 1,2,3,4,5,6,7,8,9,10)"
  , "  --var   新しい variable 列名 (default: 'variable'; 例: --var t)"
  , "  --value 新しい value 列名    (default: 'value';    例: --value y)"
  , "  --no-parse-var  variable 列を Double に parse せず Text のまま残す"
  , "  --output FILE   結果を CSV として書き出す"
  , ""
  , "例:"
  , "  hanalyze melt data/io/wide_sample.csv \\"
  , "      --id name,x1,x2 \\"
  , "      --vars 1,2,3,4,5,6,7,8,9,10 \\"
  , "      --var t --value y \\"
  , "      --output data/io/melted_sample.csv"
  ]

parseRuleSpec :: String -> Maybe (T.Text, Clean.ColumnRule)
parseRuleSpec s = case break (== '=') s of
  (col, '=':rule) | not (null col), not (null rule) ->
    case rule of
      "StripUnits"     -> Just (T.pack col, Clean.StripUnits)
      "ParseCurrency"  -> Just (T.pack col, Clean.ParseCurrency)
      "ParseDecimalEU" -> Just (T.pack col, Clean.ParseDecimalEU)
      "TrimText"       -> Just (T.pack col, Clean.TrimText)
      "CoerceNumeric"  -> Just (T.pack col, Clean.CoerceNumeric)
      _                -> Nothing
  _ -> Nothing

printDataFrameInfo :: FilePath -> DXD.DataFrame -> IO ()
printDataFrameInfo file df = do
  let n    = (fst (DX.dimensions df))
      cols = DX.columnNames df
  putStrLn $ "File:    " ++ file
  putStrLn $ "Rows:    " ++ show n
  putStrLn $ "Columns: " ++ show (length cols)
  putStrLn ""
  printf "  %-20s %-7s %5s %10s %10s %10s %10s %10s\n"
         ("name" :: String) ("type" :: String) ("n" :: String)
         ("min" :: String) ("max" :: String) ("mean" :: String)
         ("median" :: String) ("sd" :: String)
  putStrLn (replicate 92 '-')
  mapM_ (printColInfo df) cols

printColInfo :: DXD.DataFrame -> T.Text -> IO ()
printColInfo df name = case getDoubleVec name df of
  Just v -> do
    let xs   = V.toList v
        m    = length xs
        mn   = if null xs then 0 else minimum xs
        mx   = if null xs then 0 else maximum xs
        mean = if null xs then 0 else sum xs / fromIntegral m
        ss   = sort xs
        med  = if m == 0 then 0 else ss !! (m `div` 2)
        var  = if m <= 1 then 0
               else sum [ (x - mean)^(2 :: Int) | x <- xs ]
                  / fromIntegral (m - 1)
        sd_  = sqrt var
    printf "  %-20s %-7s %5d %10.4f %10.4f %10.4f %10.4f %10.4f\n"
           (T.unpack name) ("numeric" :: String) m mn mx mean med sd_
  Nothing -> case getMaybeTextVec name df of
    Just v -> do
      let raw    = V.toList v
          m      = length raw
          xsOnly = [ x | Just x <- raw ]
          nMissNull = length [ () | Nothing <- raw ]
          nMissNA   = length (filter Pp.isNAString xsOnly)
          nMiss     = nMissNull + nMissNA
          uniq      = Set.size (Set.fromList xsOnly)
          topN      = take 3 (countTop xsOnly)
          topStr    = intercalate ", "
                        [ T.unpack k ++ "(" ++ show c ++ ")" | (k, c) <- topN ]
          missStr = if nMiss > 0
                      then "  NA=" ++ show nMiss
                      else ""
      printf "  %-20s %-7s %5d  unique=%-3d top: %s%s\n"
             (T.unpack name) ("text" :: String) m uniq topStr missStr
    Nothing -> printf "  %-20s %-7s     ?  (列の取り出しに失敗)\n"
                      (T.unpack name) ("?" :: String)

-- | Count occurrences and return descending list.
countTop :: Ord a => [a] -> [(a, Int)]
countTop xs =
  let counts = foldr (\x -> insertWithInc x) [] xs
      insertWithInc x []                 = [(x, 1)]
      insertWithInc x ((y, c) : rest)
        | x == y    = (y, c + 1) : rest
        | otherwise = (y, c) : insertWithInc x rest
      sorted = qSortBy (\(_, a) (_, b) -> compare b a) counts
  in sorted
  where
    qSortBy _ []     = []
    qSortBy f (p:rs) = qSortBy f [x | x <- rs, f x p == LT || f x p == EQ]
                    ++ [p]
                    ++ qSortBy f [x | x <- rs, f x p == GT]

-- ---------------------------------------------------------------------------
-- hist subcommand
-- ---------------------------------------------------------------------------

runHistCmd :: [String] -> IO ()
runHistCmd args0 =
  let (lopts, args) = parseLoadOpts args0
  in case parseHistArgs args of
       Left err  -> hPutStrLn stderr err
       Right ho  -> runHistOpts (ho { hoLoadOpts = lopts })

data HistOpts = HistOpts
  { hoFile     :: FilePath
  , hoCol      :: T.Text
  , hoFit      :: Maybe Distribution
  , hoFormat   :: OutputFormat
  , hoOut      :: FilePath
  , hoLoadOpts :: LoadOpts
  } deriving (Show)

parseHistArgs :: [String] -> Either String HistOpts
parseHistArgs (file : col : rest) = goHistOpts rest
  HistOpts { hoFile = file, hoCol = T.pack col
           , hoFit = Nothing, hoFormat = HTML, hoOut = "histogram.html"
           , hoLoadOpts = defaultLoadOpts }
parseHistArgs _ = Left $ unlines
  [ "Usage: hanalyze hist <file> <col> [options]"
  , ""
  , "Options:"
  , "  --fit DIST [PARAMS...]   overlay theoretical density"
  , "                           (e.g. --fit normal 0 1, --fit poisson 3)"
  , "  --format html|png|svg    output format (default: html)"
  , "  --out FILE               output file path (default: histogram.html)"
  ]

goHistOpts :: [String] -> HistOpts -> Either String HistOpts
goHistOpts []                ho = Right ho
goHistOpts ("--fit" : rest)  ho = case rest of
  (name : rest') ->
    let (paramStrs, rest'') = span isNumericToken rest'
        params = map read paramStrs :: [Double]
    in case parseDistribution name params of
         Left err -> Left ("--fit: " ++ err)
         Right d  -> goHistOpts rest'' (ho { hoFit = Just d })
  [] -> Left "--fit requires a distribution name (e.g. --fit normal 0 1)"
goHistOpts ("--format" : v : rest) ho =
  case parseFormat v of
    Left err -> Left err
    Right f  -> goHistOpts rest (ho { hoFormat = f })
goHistOpts ("-f"       : v : rest) ho = goHistOpts ("--format" : v : rest) ho
goHistOpts ("--out"    : v : rest) ho = goHistOpts rest (ho { hoOut = v })
goHistOpts (flag       : _)        _  =
  Left ("hist: unexpected argument '" ++ flag ++ "' (try 'hanalyze hist' for usage)")

runHistOpts :: HistOpts -> IO ()
runHistOpts ho = do
  result <- loadAutoSafeWith (hoLoadOpts ho) (hoFile ho)
  case result of
    Left err -> hPutStrLn stderr ("Parse error: " ++ err)
    Right (df, lg) -> do
      Log.printLogReport lg
      case getDoubleVec (hoCol ho) df of
        Nothing -> hPutStrLn stderr $
          "Error: column '" ++ T.unpack (hoCol ho) ++ "' not found or not numeric"
        Just xVec -> do
          let vals    = V.toList xVec
              histCfg = defaultConfig ("Histogram: " <> hoCol ho)
              outPath = hoOut ho
              fmt     = hoFormat ho
          case hoFit ho of
            Nothing -> do
              histogramPlotFile fmt outPath histCfg (hoCol ho) vals Nothing
              putStrLn $ "Histogram:           " ++ outPath
            Just dist -> do
              histogramWithDensityFile fmt outPath histCfg (hoCol ho) vals Nothing dist
              putStrLn $ "Histogram + density: " ++ outPath
          openInBrowser outPath

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

  result <- loadAutoSafeWith (cfgLoadOpts cfg) (cfgFile cfg)
  case result of
    Left err -> putStrLn ("Parse error: " ++ err)
    Right (df, lg) -> do
      Log.printLogReport lg
      putStrLn $ "Loaded " ++ show ((fst (DX.dimensions df))) ++ " rows from " ++ cfgFile cfg
      putStrLn "Columns:"
      mapM_ (TIO.putStrLn . ("  - " <>)) (DX.columnNames df)

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

runMixedModel :: Config -> DXD.DataFrame -> OutputFormat -> T.Text -> T.Text -> T.Text -> IO ()
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
      case (length (cfgXCols cfg), getDoubleVec xCol1 df, getDoubleVec yCol df, getTextVec grpCol df) of
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
      case getDoubleVec yCol df of
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
            then case (getDoubleVec yCol df, getTextVec grpCol df) of
                   (Just yVec, Just gVec) -> do
                     let xVecPairs = [ (xv, deg) | (xc, deg) <- colDegs
                                     , Just xv <- [getDoubleVec xc df] ]
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

          let rbCfg = RB.defaultReportConfig
                        (T.pack modelKind <> ": " <> yCol <> " | " <> grpCol)
              scatterPlots =
                case (length (cfgXCols cfg), getDoubleVec xCol1 df, getDoubleVec yCol df, getTextVec grpCol df) of
                  (1, Just xVec, Just yVec, Just gVec) ->
                    let ptData  = zip3 (V.toList gVec) (V.toList xVec) (V.toList yVec)
                        lnData  = computeGroupLines lnk cs colDegs (glmmGroups gr) (glmmBLUPs gr) xVec
                        scCfg   = defaultConfig (xCol1 <> " vs " <> yCol <> suffix)
                    in [NamedPlot "vl-scatter" "グループ別散布図"
                         (scatterWithGroups scCfg xCol1 yCol ptData lnData)]
                  _ -> []
              pvsaPlots =
                case getDoubleVec yCol df of
                  Just yVec ->
                    let pvCfg = defaultConfig ("Predicted vs Actual" <> suffix)
                    in [NamedPlot "vl-pvsa" "Predicted vs Actual"
                         (predictedVsActual pvCfg (V.toList yVec) (fittedList (glmmFixed gr)))]
                  Nothing -> []
              plots = scatterPlots ++ pvsaPlots
              sections = cliMixedSections cfg df dist lnk colDegs grpCol gr mModelSel plots
          RB.renderReport path rbCfg sections
          putStrLn $ "Report:              " ++ path
          maybeExportReportPlots cfg path plots
          openInBrowser path

-- ---------------------------------------------------------------------------
-- GLM regression (no random effects)
-- ---------------------------------------------------------------------------

runRegression :: Config -> DXD.DataFrame -> OutputFormat -> T.Text -> T.Text -> IO ()
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
      printf "  %-30s = %9.4f\n" (r2Label dist) (rSquared1 res)

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

      case getDoubleVec yCol df of
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
          let rbCfg = RB.defaultReportConfig
                        (T.pack (modelLabel dist lnk)
                          <> ": " <> yCol <> " ~ "
                          <> T.pack (modelFormula colDegs))
              pvsaPlots = case getDoubleVec yCol df of
                Just yVec ->
                  let pvCfg = defaultConfig ("Predicted vs Actual" <> titleSuffix)
                  in [NamedPlot "vl-pvsa" "Predicted vs Actual"
                       (predictedVsActual pvCfg (V.toList yVec) (fittedList res))]
                Nothing -> []

          -- ── WAIC/LOO-CV 計算 (--waic が指定された場合) ────────────────────
          mModelSelect <-
            if not (cfgWAIC cfg)
            then return Nothing
            else case getDoubleVec yCol df of
              Nothing   -> return Nothing
              Just yVec -> do
                let xVecPairs = [ (xv, deg)
                                | (xc, deg) <- colDegs
                                , Just xv   <- [getDoubleVec xc df] ]
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

          let sections = cliRegressSections cfg df dist lnk colDegs res mSmooth
                            mModelSelect pvsaPlots
          RB.renderReport path rbCfg sections
          putStrLn $ "Report:              " ++ path
          maybeExportReportPlots cfg path pvsaPlots
          openInBrowser path

-- ---------------------------------------------------------------------------
-- Regression / scatter dispatch (non-histogram path)
-- ---------------------------------------------------------------------------

runAnalysis :: Config -> DXD.DataFrame -> OutputFormat -> T.Text -> IO ()
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

runGP :: Config -> DXD.DataFrame -> T.Text -> IO ()
runGP cfg df xCol1 = do
  let yCol = head (cfgYCols cfg)
  case (getDoubleVec xCol1 df, getDoubleVec yCol df) of
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
          path   = maybe "report.html" id (cfgReport cfg)
          rbCfg  = RB.defaultReportConfig
                     ("GP Regression: " <> xCol1 <> " \x2192 " <> yCol)
          sections = cliGPSections xCol1 yCol df xs ys testXs sorted

      RB.renderReport path rbCfg sections
      putStrLn $ "Report: " ++ path
      maybeExportReportPlots cfg path []
      openInBrowser path

    _ -> putStrLn "\nError: column(s) not found or not numeric"

-- ---------------------------------------------------------------------------
-- HBM (Bayesian linear regression via NUTS)
-- ---------------------------------------------------------------------------

runHBM :: Config -> DXD.DataFrame -> T.Text -> IO ()
runHBM cfg df xCol = do
  let yCols = cfgYCols cfg
      xCols = cfgXCols cfg
  case (yCols, xCols, getDoubleVec xCol df) of
    ([yCol], [_], Just xVec) ->
      case getDoubleVec yCol df of
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
  :: [Double] -> [Double] -> T.Text -> T.Text -> DXD.DataFrame -> Config -> IO ()
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

      let mGraph = Just (Viz.ModelGraph.buildMermaid (HBMod.buildModelGraph hbmModel))
          rbCfg  = RB.defaultReportConfig
                     ("HBM Linear Regression: " <> yCol <> " ~ " <> xCol)
          sections = cliHBMSections xCol yCol df xs ys chain mGraph mWaicLoo []
      RB.renderReport path rbCfg sections
      putStrLn $ "Report:              " ++ path
      maybeExportReportPlots cfg path []
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

runHistogram :: Config -> DXD.DataFrame -> OutputFormat -> T.Text -> IO ()
runHistogram cfg df fmt xCol =
  case getDoubleVec xCol df of
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

-- ---------------------------------------------------------------------------
-- doe subcommand (Phase E1: orthogonal arrays)
-- ---------------------------------------------------------------------------

doeUsage :: String
doeUsage = unlines
  [ "Usage: hanalyze doe <action> [args...]"
  , ""
  , "Actions:"
  , "  list                              List available standard arrays"
  , "  ortho <NAME> [opts]               Output an orthogonal array (L4/L8/L9/L12/L16/L18)"
  , ""
  , "ortho options:"
  , "  -f, --factor NAME=v1,v2,...       Assign a factor with comma-separated levels"
  , "                                    (repeat for multiple factors; left-to-right = column 1, 2, ...)"
  , "  --csv | --tsv | --pretty          Output format (default: pretty)"
  , "  --out FILE                        Write to file instead of stdout"
  , ""
  , "Examples:"
  , "  hanalyze doe list"
  , "  hanalyze doe ortho L9 --pretty"
  , "  hanalyze doe ortho L9 -f temp=150,180,210 -f time=10,20,30 -f catalyst=A,B,C --csv"
  , "  hanalyze doe ortho L8 -f A=low,high -f B=0,1 --out design.tsv --tsv"
  ]

runDoeCmd :: [String] -> IO ()
runDoeCmd []                = putStrLn doeUsage
runDoeCmd ["help"]           = putStrLn doeUsage
runDoeCmd ["--help"]         = putStrLn doeUsage
runDoeCmd ("list":_)         = runDoeList
runDoeCmd ("ortho":rest)     = runDoeOrtho rest
runDoeCmd (action:_)         =
  hPutStrLn stderr ("doe: unknown action '" ++ action ++ "'\n" ++ doeUsage)

runDoeList :: IO ()
runDoeList = do
  putStrLn "Available standard orthogonal arrays:"
  mapM_ (\(name, descr) ->
    printf "  %-16s %s\n" (T.unpack name) (T.unpack descr))
    OA.listArrays
  putStrLn ""
  putStrLn "Use 'hanalyze doe ortho <NAME>' to output a specific array."

data OrthoOpts = OrthoOpts
  { ooFactors :: [(T.Text, [T.Text])]   -- name → comma-split levels
  , ooFormat  :: OrthoOutFormat
  , ooOut     :: Maybe FilePath
  } deriving (Show)

data OrthoOutFormat = OrthoCSV | OrthoTSV | OrthoPretty deriving (Show, Eq)

defaultOrthoOpts :: OrthoOpts
defaultOrthoOpts = OrthoOpts [] OrthoPretty Nothing

runDoeOrtho :: [String] -> IO ()
runDoeOrtho [] = hPutStrLn stderr ("doe ortho: missing array name\n" ++ doeUsage)
runDoeOrtho (nameStr : rest) =
  case OA.lookupOA (T.pack nameStr) of
    Nothing -> hPutStrLn stderr $
      "doe ortho: unknown array '" ++ nameStr
      ++ "' (try 'hanalyze doe list')"
    Just oa -> case parseOrthoOpts rest defaultOrthoOpts of
      Left err   -> hPutStrLn stderr ("doe ortho: " ++ err)
      Right opts -> emitOrtho oa opts

parseOrthoOpts :: [String] -> OrthoOpts -> Either String OrthoOpts
parseOrthoOpts [] acc = Right acc
parseOrthoOpts (flag : rest) acc
  | flag `elem` ["-f", "--factor"] = case rest of
      (v : rest') -> case parseFactorSpec v of
        Left err  -> Left err
        Right fac -> parseOrthoOpts rest' (acc { ooFactors = ooFactors acc ++ [fac] })
      [] -> Left "-f/--factor requires an argument like NAME=v1,v2,..."
  | flag == "--csv"    = parseOrthoOpts rest (acc { ooFormat = OrthoCSV })
  | flag == "--tsv"    = parseOrthoOpts rest (acc { ooFormat = OrthoTSV })
  | flag == "--pretty" = parseOrthoOpts rest (acc { ooFormat = OrthoPretty })
  | flag == "--out"    = case rest of
      (v : rest') -> parseOrthoOpts rest' (acc { ooOut = Just v })
      []          -> Left "--out requires a file path"
  | otherwise = Left ("unexpected argument '" ++ flag ++ "'")

parseFactorSpec :: String -> Either String (T.Text, [T.Text])
parseFactorSpec s =
  case break (== '=') s of
    (name, '=' : levelsStr) | not (null name), not (null levelsStr) ->
      let levels = filter (not . T.null) (T.splitOn "," (T.pack levelsStr))
      in if null levels
         then Left ("factor '" ++ name ++ "' has no levels (use NAME=v1,v2,...)")
         else Right (T.pack name, levels)
    _ -> Left ("invalid factor spec '" ++ s ++ "' (expected NAME=v1,v2,...)")

emitOrtho :: OA.OA -> OrthoOpts -> IO ()
emitOrtho oa opts =
  case ooFactors opts of
    [] -> emitText (renderRaw (ooFormat opts) oa) (ooOut opts)
    fs -> do
      let specs = [ OA.FactorSpec name (map toLevelValue levels)
                  | (name, levels) <- fs ]
      case OA.assignFactors oa specs of
        Left err -> hPutStrLn stderr ("doe ortho: " ++ T.unpack err)
        Right ad -> emitText (renderAssigned (ooFormat opts) ad) (ooOut opts)

toLevelValue :: T.Text -> OA.LevelValue
toLevelValue t = case reads (T.unpack t) :: [(Double, String)] of
  [(d, "")] -> OA.LNumeric d
  _         -> OA.LText t

renderRaw :: OrthoOutFormat -> OA.OA -> T.Text
renderRaw OrthoCSV    = OA.renderRawCSV
renderRaw OrthoTSV    = OA.renderRawTSV
renderRaw OrthoPretty = OA.renderRawPretty

renderAssigned :: OrthoOutFormat -> OA.AssignedDesign -> T.Text
renderAssigned OrthoCSV    = OA.renderCSV
renderAssigned OrthoTSV    = OA.renderTSV
renderAssigned OrthoPretty = OA.renderPretty

emitText :: T.Text -> Maybe FilePath -> IO ()
emitText txt Nothing     = TIO.putStrLn txt
emitText txt (Just path) = do
  TIO.writeFile path txt
  putStrLn $ "Written: " ++ path

-- ---------------------------------------------------------------------------
-- taguchi subcommand (Phase E2: SN ratio + factor effects + inner/outer)
-- ---------------------------------------------------------------------------

taguchiUsage :: String
taguchiUsage = unlines
  [ "Usage: hanalyze taguchi <action> [args...]"
  , ""
  , "Actions:"
  , "  sn <type> <values...>             Compute a single SN ratio (dB)"
  , "                                    type: smaller | larger | nominal | nominal-target=M"
  , ""
  , "  analyze <ARRAY> -f F=v1,v2,... [-f ...] --csv FILE [--sntype TYPE] [--report [FILE]]"
  , "                                    Analyze observations from a CSV file:"
  , "                                    rows = inner runs, cols (after factor cols) = repetitions/outer."
  , "                                    Computes per-row SN ratio, factor effects, and optimum levels."
  , "                                    --report writes an interactive HTML report (default: taguchi.html)."
  , ""
  , "  cross <INNER> <OUTER>"
  , "    -f Fc=v1,v2,...   [-f ...]      Inner control factors"
  , "    --noise Fn=v1,v2,...  [...]     Outer noise factors"
  , "    [--out FILE]                    Output the cross-design CSV template"
  , ""
  , "SN types:"
  , "  smaller          smaller-the-better (e.g. defect rate)"
  , "  larger           larger-the-better (e.g. strength)"
  , "  nominal          nominal-the-best (mean^2 / variance)"
  , "  nominal-target=M nominal with target value M"
  , ""
  , "Examples:"
  , "  hanalyze taguchi sn smaller 1.2 1.5 0.9 1.1"
  , "  hanalyze taguchi analyze L9 -f temp=150,180,210 -f time=10,20,30 -f cat=A,B,C"
  , "                              --csv runs.csv --sntype smaller"
  , "  hanalyze taguchi cross L9 L4 -f temp=150,180,210 -f time=10,20,30 -f cat=A,B,C"
  , "                                --noise humidity=low,high --noise vibration=on,off --out cross.csv"
  ]

runTaguchiCmd :: [String] -> IO ()
runTaguchiCmd []                = putStrLn taguchiUsage
runTaguchiCmd ["help"]           = putStrLn taguchiUsage
runTaguchiCmd ["--help"]         = putStrLn taguchiUsage
runTaguchiCmd ("sn":rest)        = runTaguchiSN rest
runTaguchiCmd ("analyze":rest)   = runTaguchiAnalyze rest
runTaguchiCmd ("cross":rest)     = runTaguchiCross rest
runTaguchiCmd (action:_)         =
  hPutStrLn stderr ("taguchi: unknown action '" ++ action ++ "'\n" ++ taguchiUsage)

-- ── sn ──────────────────────────────────────────────────────────────────

runTaguchiSN :: [String] -> IO ()
runTaguchiSN [] = hPutStrLn stderr "taguchi sn: missing type and values"
runTaguchiSN (typeStr : valStrs)
  | null valStrs = hPutStrLn stderr "taguchi sn: need at least one value"
  | otherwise = case parseSNType typeStr of
      Left err -> hPutStrLn stderr ("taguchi sn: " ++ err)
      Right t  ->
        let vals = mapM readMaybeD valStrs
        in case vals of
             Nothing -> hPutStrLn stderr "taguchi sn: non-numeric value(s)"
             Just xs -> do
               let eta = TG.snRatio t xs
               printf "SN(%s) = %.4f dB  (n=%d)\n"
                      (T.unpack (TG.snTypeName t)) eta (length xs)

parseSNType :: String -> Either String TG.SNType
parseSNType s = case s of
  "smaller"           -> Right TG.SmallerBetter
  "smaller-better"    -> Right TG.SmallerBetter
  "larger"            -> Right TG.LargerBetter
  "larger-better"     -> Right TG.LargerBetter
  "nominal"           -> Right TG.NominalBest
  "nominal-best"      -> Right TG.NominalBest
  _ | "nominal-target=" `isPrefixOfStr` s ->
      case readMaybeD (drop (length ("nominal-target=" :: String)) s) of
        Just m  -> Right (TG.NominalBestTarget m)
        Nothing -> Left ("invalid target value in '" ++ s ++ "'")
  _ -> Left ("unknown SN type '" ++ s
          ++ "' (try smaller | larger | nominal | nominal-target=M)")

isPrefixOfStr :: String -> String -> Bool
isPrefixOfStr p s = take (length p) s == p

readMaybeD :: String -> Maybe Double
readMaybeD s = case reads s :: [(Double, String)] of
  [(v, "")] -> Just v
  _         -> Nothing

-- ── analyze ─────────────────────────────────────────────────────────────

data TgAnalyzeOpts = TgAnalyzeOpts
  { toFactors :: [(T.Text, [T.Text])]
  , toCSV     :: Maybe FilePath
  , toSN      :: TG.SNType
  , toReport  :: Maybe FilePath
  } deriving (Show)

defaultTgAnalyzeOpts :: TgAnalyzeOpts
defaultTgAnalyzeOpts = TgAnalyzeOpts [] Nothing TG.SmallerBetter Nothing

runTaguchiAnalyze :: [String] -> IO ()
runTaguchiAnalyze args0 =
  let (lopts, args) = parseLoadOpts args0
  in case args of
       []                  -> hPutStrLn stderr "taguchi analyze: missing array name"
       (arrayStr : rest)   ->
         case OA.lookupOA (T.pack arrayStr) of
           Nothing -> hPutStrLn stderr $
             "taguchi analyze: unknown array '" ++ arrayStr ++ "'"
           Just oa -> case parseTgAnalyzeOpts rest defaultTgAnalyzeOpts of
             Left err   -> hPutStrLn stderr ("taguchi analyze: " ++ err)
             Right opts -> case toCSV opts of
               Nothing   -> hPutStrLn stderr "taguchi analyze: --csv FILE required"
               Just path -> doTaguchiAnalyze oa opts path lopts

parseTgAnalyzeOpts :: [String] -> TgAnalyzeOpts -> Either String TgAnalyzeOpts
parseTgAnalyzeOpts [] acc = Right acc
parseTgAnalyzeOpts (flag : rest) acc
  | flag `elem` ["-f", "--factor"] = case rest of
      (v : rs) -> case parseFactorSpec v of
        Left err  -> Left err
        Right fac -> parseTgAnalyzeOpts rs
                       (acc { toFactors = toFactors acc ++ [fac] })
      [] -> Left "-f/--factor requires NAME=v1,v2,..."
  | flag == "--csv" = case rest of
      (v : rs) -> parseTgAnalyzeOpts rs (acc { toCSV = Just v })
      []       -> Left "--csv requires a file path"
  | flag == "--sntype" = case rest of
      (v : rs) -> case parseSNType v of
        Left err  -> Left err
        Right t   -> parseTgAnalyzeOpts rs (acc { toSN = t })
      [] -> Left "--sntype requires an argument"
  | flag == "--report" = case rest of
      (v : rs) | not (null v) && head v /= '-' ->
        parseTgAnalyzeOpts rs (acc { toReport = Just v })
      _ -> parseTgAnalyzeOpts rest (acc { toReport = Just "taguchi.html" })
  | otherwise = Left ("unexpected argument '" ++ flag ++ "'")

doTaguchiAnalyze :: OA.OA -> TgAnalyzeOpts -> FilePath -> LoadOpts -> IO ()
doTaguchiAnalyze oa opts path lopts = do
  result <- loadAutoSafeWith lopts path
  case result of
    Left err          -> hPutStrLn stderr ("Parse error: " ++ err)
    Right (df, lg)    -> do
      Log.printLogReport lg
      let specs = [ OA.FactorSpec name (map toLevelValue lvls)
                  | (name, lvls) <- toFactors opts ]
      case OA.assignFactors oa specs of
        Left err -> hPutStrLn stderr (T.unpack err)
        Right ad -> runAnalyzeWith ad opts df

runAnalyzeWith :: OA.AssignedDesign -> TgAnalyzeOpts -> DXD.DataFrame -> IO ()
runAnalyzeWith ad opts df = do
  let factorNames = map OA.fsName (OA.adFactors ad)
      yCols = filter (\c -> not (c `elem` factorNames) && c /= "Run")
                     (DX.columnNames df)
      n = length (OA.adRows ad)
  when ((fst (DX.dimensions df)) /= n) $
    hPutStrLn stderr $
      "Warning: CSV has " ++ show ((fst (DX.dimensions df)))
      ++ " rows, expected " ++ show n
  if null yCols
    then hPutStrLn stderr
           "taguchi analyze: no observation columns found in CSV"
    else do
      -- Per-inner-run observations (skip non-numeric rows)
      let yMatrix =
            [ [ case getDoubleVec c df of
                  Just v | i < V.length v -> v V.! i
                  _ -> 0
              | c <- yCols ]
            | i <- [0 .. min ((fst (DX.dimensions df))) n - 1] ]
          sns  = TG.snRatioRows (toSN opts) yMatrix
          fes  = TG.analyzeSN ad sns
          opts' = TG.optimalLevels fes
          predEta = TG.predictSN fes sns

      printf "Array:      %s\n" (T.unpack (OA.oaName (OA.adArray ad)))
      printf "SN type:    %s\n" (T.unpack (TG.snTypeName (toSN opts)))
      printf "Inner runs: %d\n" n
      printf "Repetitions per run: %d (columns %s)\n"
             (length yCols) (T.unpack (T.intercalate ", " yCols))
      putStrLn ""

      putStrLn "--- Per-run SN ratios ---"
      mapM_ (\(i, eta) -> printf "  Run %2d:  SN = %8.3f dB\n" (i :: Int) eta)
            (zip [1..] sns)
      putStrLn ""

      putStrLn "--- Factor effects (mean SN per level) ---"
      mapM_ (printFactorEffect opts') fes
      putStrLn ""

      putStrLn "--- Optimal levels (max SN per factor) ---"
      mapM_ (\(f, lvl, eta) ->
        printf "  %-12s = %-12s  (SN = %8.3f dB)\n"
               (T.unpack f) (T.unpack (lvText lvl)) eta) opts'
      putStrLn ""
      printf "Predicted SN at optimum (additive model): %.3f dB\n" predEta

      -- ── HTML レポート出力 (--report 指定時) ─────────────────────────────
      case toReport opts of
        Nothing -> return ()
        Just path -> do
          let tr = VTG.TaguchiReport
                     { VTG.trTitle     = "Taguchi Analysis: "
                                         <> OA.oaName (OA.adArray ad)
                                         <> " — "
                                         <> TG.snTypeName (toSN opts)
                     , VTG.trArrayName = OA.oaName (OA.adArray ad)
                     , VTG.trSNType    = toSN opts
                     , VTG.trPerRunSN  = sns
                     , VTG.trEffects   = fes
                     , VTG.trOptimal   = opts'
                     , VTG.trPredicted = predEta
                     }
          VTG.renderTaguchiReport path tr
          putStrLn ("Report: " ++ path)
          openInBrowser path
  where
    lvText (OA.LText t)    = t
    lvText (OA.LNumeric d)
      | d == fromIntegral (round d :: Integer) = T.pack (show (round d :: Integer))
      | otherwise                              = T.pack (printf "%g" d)

printFactorEffect :: [(T.Text, OA.LevelValue, Double)] -> TG.FactorEffect -> IO ()
printFactorEffect _opts fe = do
  printf "  %s:\n" (T.unpack (TG.feFactor fe))
  let pairs = zip (TG.feLevels fe) (TG.feSNByLevel fe)
  mapM_ (\(lv, eta) ->
    printf "    %-12s : %8.3f dB\n"
      (T.unpack (lvShow lv)) eta) pairs
  where
    lvShow (OA.LText t)    = t
    lvShow (OA.LNumeric d)
      | d == fromIntegral (round d :: Integer) = T.pack (show (round d :: Integer))
      | otherwise                              = T.pack (printf "%g" d)

-- ── cross ───────────────────────────────────────────────────────────────

data TgCrossOpts = TgCrossOpts
  { tcInner :: [(T.Text, [T.Text])]
  , tcOuter :: [(T.Text, [T.Text])]
  , tcOut   :: Maybe FilePath
  } deriving (Show)

defaultTgCrossOpts :: TgCrossOpts
defaultTgCrossOpts = TgCrossOpts [] [] Nothing

runTaguchiCross :: [String] -> IO ()
runTaguchiCross [] = hPutStrLn stderr "taguchi cross: missing INNER and OUTER array names"
runTaguchiCross [_] = hPutStrLn stderr "taguchi cross: missing OUTER array name"
runTaguchiCross (innerStr : outerStr : rest) =
  case (OA.lookupOA (T.pack innerStr), OA.lookupOA (T.pack outerStr)) of
    (Nothing, _) -> hPutStrLn stderr $
      "taguchi cross: unknown inner array '" ++ innerStr ++ "'"
    (_, Nothing) -> hPutStrLn stderr $
      "taguchi cross: unknown outer array '" ++ outerStr ++ "'"
    (Just innerOA, Just outerOA) ->
      case parseTgCrossOpts rest defaultTgCrossOpts of
        Left err   -> hPutStrLn stderr ("taguchi cross: " ++ err)
        Right opts -> doTaguchiCross innerOA outerOA opts

parseTgCrossOpts :: [String] -> TgCrossOpts -> Either String TgCrossOpts
parseTgCrossOpts [] acc = Right acc
parseTgCrossOpts (flag : rest) acc
  | flag `elem` ["-f", "--factor"] = case rest of
      (v : rs) -> case parseFactorSpec v of
        Left err  -> Left err
        Right fac -> parseTgCrossOpts rs (acc { tcInner = tcInner acc ++ [fac] })
      [] -> Left "-f/--factor requires NAME=v1,v2,..."
  | flag `elem` ["-fn", "--noise"] = case rest of
      (v : rs) -> case parseFactorSpec v of
        Left err  -> Left err
        Right fac -> parseTgCrossOpts rs (acc { tcOuter = tcOuter acc ++ [fac] })
      [] -> Left "-fn/--noise requires NAME=v1,v2,..."
  | flag == "--out" = case rest of
      (v : rs) -> parseTgCrossOpts rs (acc { tcOut = Just v })
      []       -> Left "--out requires a file path"
  | otherwise = Left ("unexpected argument '" ++ flag ++ "'")

doTaguchiCross :: OA.OA -> OA.OA -> TgCrossOpts -> IO ()
doTaguchiCross innerOA outerOA opts = do
  let innerSpecs = [ OA.FactorSpec n (map toLevelValue ls)
                   | (n, ls) <- tcInner opts ]
      outerSpecs = [ OA.FactorSpec n (map toLevelValue ls)
                   | (n, ls) <- tcOuter opts ]
  case (OA.assignFactors innerOA innerSpecs,
        OA.assignFactors outerOA outerSpecs) of
    (Left err, _) -> hPutStrLn stderr ("inner: " ++ T.unpack err)
    (_, Left err) -> hPutStrLn stderr ("outer: " ++ T.unpack err)
    (Right ai, Right ao) -> do
      let io = TG.makeInnerOuter ai ao
          csv = TG.renderInnerOuterCSV io
      emitText csv (tcOut opts)

-- ---------------------------------------------------------------------------
-- ridge / kernel / spline 共通ヘルパ
-- ---------------------------------------------------------------------------

-- | CSV を読み、x 列(複数可) と y 列(1) を numeric vector で取り出す。
-- 'LoadOpts' を反映 (--no-header / --skip / --comment / --strict)。
loadXY :: LoadOpts -> FilePath -> [T.Text] -> T.Text
       -> IO (Either String (DXD.DataFrame, [V.Vector Double], V.Vector Double))
loadXY lopts path xCols yCol = do
  result <- loadAutoSafeWith lopts path
  case result of
    Left err          -> return (Left err)
    Right (df, lg)    -> do
      Log.printLogReport lg
      case (mapM (\c -> getDoubleVec c df) xCols, getDoubleVec yCol df) of
        (Just xs, Just y) -> return (Right (df, xs, y))
        _ -> return (Left $ "Numeric column(s) not found: x="
                      ++ T.unpack (T.intercalate "," xCols)
                      ++ ", y=" ++ T.unpack yCol)

-- | RMSE 計算。
rmseV :: [Double] -> [Double] -> Double
rmseV ys yhat =
  let n = length ys
      sse = sum [ (a - b) ^ (2 :: Int) | (a, b) <- zip ys yhat ]
  in sqrt (sse / fromIntegral (max 1 n))

-- | 散布図 + 滑らか曲線 を出力。
writeSmoothPlot :: OutputFormat -> FilePath -> T.Text
                -> DXD.DataFrame -> T.Text -> T.Text -> SmoothFit -> IO ()
writeSmoothPlot fmt path titleSuffix df xc yc sf =
  scatterWithSmoothFile fmt path
    (defaultConfig (xc <> " vs " <> yc <> "  [" <> titleSuffix <> "]"))
    Nothing df xc yc sf

-- | xMin/xMax から評価グリッドを作る。
makeGrid :: V.Vector Double -> Int -> [Double]
makeGrid xs n =
  let lo = V.minimum xs
      hi = V.maximum xs
  in [ lo + fromIntegral i * (hi - lo) / fromIntegral (n - 1)
     | i <- [0 .. n - 1] ]

-- ---------------------------------------------------------------------------
-- ridge subcommand (Ridge / Lasso / Elastic Net)
-- ---------------------------------------------------------------------------

ridgeUsage :: String
ridgeUsage = unlines
  [ "Usage: hanalyze ridge <file> <xcols> <ycol> [options]"
  , ""
  , "  <xcols>   x column name(s); quote multiple: \"x1 x2\""
  , "  <ycol>    y column name (single)"
  , ""
  , "Options:"
  , "  --penalty TYPE   ridge|lasso|elasticnet (default: ridge)"
  , "  --lambda L       regularization strength (default: 0.1)"
  , "  --alpha A        ElasticNet L1 mixing in [0,1] (default: 0.5; only with --penalty elasticnet)"
  , "  --format FMT     html|png|svg (default: html)"
  , "  --out FILE       scatter+fit output path (default: ridge.html; single x only)"
  , "  --report [FILE]  build composite HTML report (default: ridge.html)"
  , ""
  , "Examples:"
  , "  hanalyze ridge data.csv x y --lambda 0.1"
  , "  hanalyze ridge data.csv \"x1 x2 x3\" y --penalty lasso --lambda 0.05"
  , "  hanalyze ridge data.csv \"x1 x2\" y --penalty elasticnet --lambda 0.1 --alpha 0.5"
  ]

data RidgeOpts = RidgeOpts
  { roPenalty :: T.Text   -- "ridge" / "lasso" / "elasticnet"
  , roLambda  :: Double
  , roAlpha   :: Double
  , roFormat  :: OutputFormat
  , roOut     :: FilePath
  , roReport  :: Maybe FilePath
  }

defaultRidgeOpts :: RidgeOpts
defaultRidgeOpts = RidgeOpts "ridge" 0.1 0.5 HTML "ridge.html" Nothing

runRidgeCmd :: [String] -> IO ()
runRidgeCmd args0 =
  let (lopts, args) = parseLoadOpts args0
  in case args of
       (file : xColsStr : yColStr : rest) ->
         case parseRidgeOpts rest defaultRidgeOpts of
           Left err   -> hPutStrLn stderr ("ridge: " ++ err)
           Right opts -> doRidge file xColsStr yColStr opts lopts
       _ -> putStrLn ridgeUsage

parseRidgeOpts :: [String] -> RidgeOpts -> Either String RidgeOpts
parseRidgeOpts [] acc = Right acc
parseRidgeOpts (flag : rest) acc
  | flag == "--penalty" = case rest of
      (v : rs) | v `elem` ["ridge","lasso","elasticnet"] ->
        parseRidgeOpts rs (acc { roPenalty = T.pack v })
      (v : _) -> Left ("unknown penalty '" ++ v ++ "'")
      []      -> Left "--penalty requires an argument"
  | flag == "--lambda" = case rest of
      (v:rs) -> case reads v :: [(Double, String)] of
        [(d,"")] -> parseRidgeOpts rs (acc { roLambda = d })
        _        -> Left ("invalid --lambda value '" ++ v ++ "'")
      []     -> Left "--lambda requires a value"
  | flag == "--alpha" = case rest of
      (v:rs) -> case reads v :: [(Double, String)] of
        [(d,"")] -> parseRidgeOpts rs (acc { roAlpha = d })
        _        -> Left ("invalid --alpha value '" ++ v ++ "'")
      []     -> Left "--alpha requires a value"
  | flag `elem` ["-f","--format"] = case rest of
      (v:rs) -> case parseFormat v of
        Right f -> parseRidgeOpts rs (acc { roFormat = f })
        Left e  -> Left e
      []     -> Left "--format requires an argument"
  | flag == "--out" = case rest of
      (v:rs) -> parseRidgeOpts rs (acc { roOut = v })
      []     -> Left "--out requires a file path"
  | flag == "--report" = case rest of
      (v:rs) | not (null v) && head v /= '-' ->
        parseRidgeOpts rs (acc { roReport = Just v })
      _ -> parseRidgeOpts rest (acc { roReport = Just "ridge.html" })
  | otherwise = Left ("unexpected argument '" ++ flag ++ "'")

doRidge :: FilePath -> String -> String -> RidgeOpts -> LoadOpts -> IO ()
doRidge file xColsStr yColStr opts lopts = do
  let xCols = map T.pack (words xColsStr)
      yCol  = T.pack yColStr
  result <- loadXY lopts file xCols yCol
  case result of
    Left err -> hPutStrLn stderr err
    Right (df, xVecs, yVec) -> do
      let n        = V.length yVec
          intercept = LA.konst 1 n
          xMat     = LA.fromColumns
                       (intercept : map (LA.fromList . V.toList) xVecs)
          yLA      = LA.fromList (V.toList yVec)
          pen      = case roPenalty opts of
            "ridge"      -> Reg.L2 (roLambda opts)
            "lasso"      -> Reg.L1 (roLambda opts)
            "elasticnet" -> Reg.ElasticNet
                             (roLambda opts * roAlpha opts)
                             (roLambda opts * (1 - roAlpha opts))
            _            -> Reg.L2 (roLambda opts)
          fit      = Reg.fitRegularized pen xMat yLA
          beta     = LA.toList (Reg.rfBeta fit)
          yhat     = LA.toList (Reg.rfYHat fit)
          ys       = V.toList yVec
          rmseVal  = rmseV ys yhat
      printf "Loaded %d rows from %s\n" n file
      printf "Penalty: %s, lambda=%g%s\n"
             (T.unpack (roPenalty opts)) (roLambda opts)
             (if roPenalty opts == "elasticnet"
                then ", alpha=" ++ show (roAlpha opts) else "")
      putStrLn ""
      putStrLn "Coefficients:"
      printf "  %-30s = %9.4f\n" ("intercept" :: String) (head beta)
      mapM_ (\(i, c, b) ->
        printf "  %-30s = %9.4f\n"
               ("β_" ++ show (i :: Int) ++ " (" ++ T.unpack c ++ ")") b)
        (zip3 [1..] xCols (tail beta))
      printf "R²  = %.4f\n" (Reg.rfR2 fit)
      printf "|β| > 1e-8: %d / %d (sparsity)\n"
             (Reg.rfNonZero fit) (length beta)
      printf "RMSE (in-sample) = %.4f\n" rmseVal
      -- 単純散布図 + 予測曲線 (1 変数のみ)
      let coeffPairs = zip ("intercept" : xCols)
                           (map T.pack (map (printf "%.4f") beta) :: [T.Text])
          coeffNumPairs = zip ("intercept" : xCols) beta
          residuals = LA.toList (Reg.rfResid fit)
      case xCols of
        [xc1] -> do
          let xs = V.toList (head xVecs)
              grid = makeGrid (head xVecs) 100
              gridMat = LA.fromColumns
                          [ LA.konst 1 100
                          , LA.fromList grid ]
              gridY = LA.toList (Reg.predictRegularized fit gridMat)
              sf = SmoothFit
                     { sfX = grid
                     , sfFit = gridY
                     , sfLower = []
                     , sfUpper = []
                     , sfHasBand = False
                     }
              _ = xs
              _ = coeffPairs
          writeSmoothPlot (roFormat opts) (roOut opts)
            (T.pack ("Regularized: " ++ T.unpack (roPenalty opts)))
            df xc1 yCol sf
          putStrLn ("Plot: " ++ roOut opts)
          openInBrowser (roOut opts)
          -- HTML レポート
          case roReport opts of
            Nothing -> return ()
            Just rpath -> do
              let smooth = RB.SmoothCurve grid gridY [] []
                  pathSec = mkRidgePathSection xCols xMat yLA opts
                  cfg = ridgeReportConfig opts xCols yCol
                  sections =
                    [ RB.secDataOverview df xCols yCol
                    , RB.secModelOverview (ridgeModelLabel opts)
                        (ridgeFormula opts xCols yCol) Nothing
                    , RB.secCoefficients coeffNumPairs (Just ("R²", Reg.rfR2 fit))
                    , RB.secKeyValue "Fit summary"
                        (ridgeFitKVs opts fit beta rmseVal)
                    , pathSec
                    , RB.secFitScatter xc1 yCol xs ys (Just smooth)
                    , RB.secResiduals yhat residuals
                    ]
              RB.renderReport rpath cfg sections
              putStrLn ("Report: " ++ rpath)
              openInBrowser rpath
        _ -> do
          putStrLn "(scatter plot skipped for multiple x columns)"
          case roReport opts of
            Nothing -> return ()
            Just rpath -> do
              let pathSec = mkRidgePathSection xCols xMat yLA opts
                  cfg = ridgeReportConfig opts xCols yCol
                  sections =
                    [ RB.secDataOverview df xCols yCol
                    , RB.secModelOverview (ridgeModelLabel opts)
                        (ridgeFormula opts xCols yCol) Nothing
                    , RB.secCoefficients coeffNumPairs (Just ("R²", Reg.rfR2 fit))
                    , RB.secKeyValue "Fit summary"
                        (ridgeFitKVs opts fit beta rmseVal)
                    , pathSec
                    , RB.secResiduals yhat residuals
                    ]
              RB.renderReport rpath cfg sections
              putStrLn ("Report: " ++ rpath)
              openInBrowser rpath

-- ---------------------------------------------------------------------------
-- kernel subcommand (Nadaraya-Watson / Kernel Ridge / RFF)
-- ---------------------------------------------------------------------------

kernelUsage :: String
kernelUsage = unlines
  [ "Usage: hanalyze kernel <file> <xcol> <ycol> [options]"
  , ""
  , "Options:"
  , "  --method M        nw|kr|rff (default: kr)"
  , "                    nw  = Nadaraya-Watson"
  , "                    kr  = Kernel Ridge"
  , "                    rff = Random Fourier Features (RBF)"
  , "  --kernel KIND     gaussian|epanechnikov|triangular|tricube|uniform"
  , "                    (default: gaussian; ignored for --method rff)"
  , "  --bandwidth H     kernel bandwidth h (default: auto via LOO-CV grid)"
  , "  --lambda L        ridge regularization (default: 0.01; for kr / rff only)"
  , "  --features D      RFF feature dimension (default: 200; --method rff only)"
  , "  --format FMT      html|png|svg (default: html)"
  , "  --out FILE        scatter+fit output path (default: kernel.html)"
  , "  --report [FILE]   build composite HTML report (default: kernel.html)"
  , ""
  , "Multivariate RFF (--method rff with multiple x columns):"
  , "  --group COL       group column for color-coded scatter+fit (e.g. name)"
  , "  --xaxis COL       column to use as horizontal axis in the plot (e.g. t)"
  , "  --interactive     スライダで副軸を変えると JS が予測曲線を再計算"
  , "                    (--report と併用、--xaxis の列以外がスライダになる)"
  , "  --standardize     入力 X を z-score 化してから fit (スケール差対策)"
  , "  --auto-hp         周辺尤度最大化で (ℓ, σ_n) を自動決定"
  , "                    (--bandwidth / --lambda は無視される)"
  , ""
  , "Examples:"
  , "  hanalyze kernel data.csv x y --method kr --bandwidth 0.5"
  , "  hanalyze kernel data.csv x y --method nw   # auto-bandwidth via LOO-CV"
  , "  hanalyze kernel data.csv x y --method rff --features 200"
  , "  # 多変量 RFF (melted データに対して):"
  , "  hanalyze kernel data/io/melted_sample.csv \"x1 t\" y --method rff \\"
  , "      --features 200 --bandwidth 1.0 --lambda 0.001 \\"
  , "      --group name --xaxis t --out plot.html"
  ]

data KernelOpts = KernelOpts
  { koMethod    :: T.Text       -- "nw" / "kr" / "rff"
  , koKernel    :: Kern.Kernel  -- Gaussian / Epanechnikov / ...
  , koBandwidth :: Maybe Double
  , koLambda    :: Double
  , koFeatures  :: Int
  , koFormat    :: OutputFormat
  , koOut       :: FilePath
  , koReport    :: Maybe FilePath
  , koGroup     :: Maybe T.Text  -- 多変量 RFF プロット用 group 列
  , koXAxis     :: Maybe T.Text  -- 多変量 RFF プロット用 横軸列名
  , koInteractive :: Bool        -- インタラクティブ予測 (--report と併用)
  , koStandardize :: Bool        -- 入力標準化 (Phase 4)
  , koAutoHP      :: Bool        -- 周辺尤度最大化で (ℓ, σ_n) 自動決定 (Phase 4)
  }

defaultKernelOpts :: KernelOpts
defaultKernelOpts = KernelOpts
  { koMethod    = "kr"
  , koKernel    = Kern.Gaussian
  , koBandwidth = Nothing
  , koLambda    = 0.01
  , koFeatures  = 200
  , koFormat    = HTML
  , koOut       = "kernel.html"
  , koReport    = Nothing
  , koGroup     = Nothing
  , koXAxis     = Nothing
  , koInteractive = False
  , koStandardize = False
  , koAutoHP      = False
  }

parseKernelKind :: String -> Either String Kern.Kernel
parseKernelKind s = case s of
  "gaussian"     -> Right Kern.Gaussian
  "epanechnikov" -> Right Kern.Epanechnikov
  "triangular"   -> Right Kern.Triangular
  "tricube"      -> Right Kern.TriCube
  "uniform"      -> Right Kern.Uniform
  _              -> Left ("unknown kernel '" ++ s ++ "'")

runKernelCmd :: [String] -> IO ()
runKernelCmd args0 =
  let (lopts, args) = parseLoadOpts args0
  in case args of
       (file : xColStr : yColStr : rest) ->
         case parseKernelOpts rest defaultKernelOpts of
           Left err   -> hPutStrLn stderr ("kernel: " ++ err)
           Right opts -> doKernel file xColStr yColStr opts lopts
       _ -> putStrLn kernelUsage

parseKernelOpts :: [String] -> KernelOpts -> Either String KernelOpts
parseKernelOpts [] acc = Right acc
parseKernelOpts (flag : rest) acc
  | flag == "--method" = case rest of
      (v:rs) | v `elem` ["nw","kr","rff"] ->
        parseKernelOpts rs (acc { koMethod = T.pack v })
      (v:_) -> Left ("unknown method '" ++ v ++ "'")
      []    -> Left "--method requires an argument"
  | flag == "--kernel" = case rest of
      (v:rs) -> case parseKernelKind v of
        Right k -> parseKernelOpts rs (acc { koKernel = k })
        Left e  -> Left e
      []     -> Left "--kernel requires an argument"
  | flag == "--bandwidth" = case rest of
      (v:rs) -> case reads v :: [(Double, String)] of
        [(d,"")] -> parseKernelOpts rs (acc { koBandwidth = Just d })
        _        -> Left ("invalid --bandwidth '" ++ v ++ "'")
      []     -> Left "--bandwidth requires a value"
  | flag == "--lambda" = case rest of
      (v:rs) -> case reads v :: [(Double, String)] of
        [(d,"")] -> parseKernelOpts rs (acc { koLambda = d })
        _        -> Left ("invalid --lambda '" ++ v ++ "'")
      []     -> Left "--lambda requires a value"
  | flag == "--features" = case rest of
      (v:rs) -> case reads v :: [(Int, String)] of
        [(d,"")] -> parseKernelOpts rs (acc { koFeatures = d })
        _        -> Left ("invalid --features '" ++ v ++ "'")
      []     -> Left "--features requires a value"
  | flag `elem` ["-f","--format"] = case rest of
      (v:rs) -> case parseFormat v of
        Right f -> parseKernelOpts rs (acc { koFormat = f })
        Left e  -> Left e
      []     -> Left "--format requires an argument"
  | flag == "--out" = case rest of
      (v:rs) -> parseKernelOpts rs (acc { koOut = v })
      []     -> Left "--out requires a file path"
  | flag == "--report" = case rest of
      (v:rs) | not (null v) && head v /= '-' ->
        parseKernelOpts rs (acc { koReport = Just v })
      _ -> parseKernelOpts rest (acc { koReport = Just "kernel.html" })
  | flag == "--group" = case rest of
      (v:rs) -> parseKernelOpts rs (acc { koGroup = Just (T.pack v) })
      []     -> Left "--group requires a column name"
  | flag == "--xaxis" = case rest of
      (v:rs) -> parseKernelOpts rs (acc { koXAxis = Just (T.pack v) })
      []     -> Left "--xaxis requires a column name"
  | flag == "--interactive" =
      parseKernelOpts rest (acc { koInteractive = True })
  | flag == "--standardize" =
      parseKernelOpts rest (acc { koStandardize = True })
  | flag == "--auto-hp" =
      parseKernelOpts rest (acc { koAutoHP = True })
  | otherwise = Left ("unexpected argument '" ++ flag ++ "'")

doKernel :: FilePath -> String -> String -> KernelOpts -> LoadOpts -> IO ()
doKernel file xColStr yColStr opts lopts = do
  let xCols = map T.pack (words xColStr)
      yCol  = T.pack yColStr
  case xCols of
    []       -> hPutStrLn stderr "kernel: x 列が指定されていません"
    [xCol]   -> do
      result <- loadXY lopts file [xCol] yCol
      case result of
        Left err -> hPutStrLn stderr err
        Right (df, [xVec], yVec) ->
          runKernelOn df xCol yCol xVec yVec opts
        Right _ -> hPutStrLn stderr "kernel: expected single x column"
    _multiple -> case koMethod opts of
      "rff" -> do
        result <- loadXY lopts file xCols yCol
        case result of
          Left err -> hPutStrLn stderr err
          Right (df, xVecs, yVec) ->
            runKernelMV df xCols yCol xVecs yVec opts
      m -> hPutStrLn stderr $
        "kernel --method " ++ T.unpack m
          ++ " は単一 x 列のみ。多変量入力は --method rff を使ってください。"

-- | 多変量 RFF Ridge を走らせる (Phase B-RFF)。
-- '--group' / '--xaxis' が指定されていれば、グループ別観測点 + 予測曲線の
-- 散布図を出力する。
-- '--standardize' / '--auto-hp' で前処理 / HP 自動決定。
runKernelMV
  :: DXD.DataFrame -> [T.Text] -> T.Text
  -> [V.Vector Double] -> V.Vector Double
  -> KernelOpts -> IO ()
runKernelMV df xCols yCol xVecs yVec opts = do
  let n = V.length yVec
      p = length xCols
      cols   = map V.toList xVecs
      xMatRaw = LA.fromColumns (map LA.fromList cols)
      ys     = V.toList yVec
      yV     = LA.fromList ys
  printf "Loaded %d rows × %d features (%s); method=rff (multivariate)\n"
         n p (T.unpack (T.intercalate "," xCols))

  -- ステップ 1: 標準化 (任意)
  let stdr = if koStandardize opts
               then Std.fitStandardizer xMatRaw
               else Std.identityStandardizer p
      xMat = if koStandardize opts
               then Std.applyStandardizer stdr xMatRaw
               else xMatRaw
  if koStandardize opts
    then do
      putStrLn "  Standardize: ON"
      printf "    μ = [%s]\n" (T.unpack (T.intercalate "," (map (T.pack . printf "%.4g") (Std.stMu stdr))))
      printf "    σ = [%s]\n" (T.unpack (T.intercalate "," (map (T.pack . printf "%.4g") (Std.stSd stdr))))
    else putStrLn "  Standardize: OFF"

  -- ステップ 2: HP の決定
  (ell, lam, sigF) <-
    if koAutoHP opts
      then do
        putStrLn "  Auto-HP: 周辺尤度最大化を実行中..."
        let res = RFF.maximizeMarginalLikRBFMV xMat yV Nothing
            ellOpt = RFF.mlEll res
            sfOpt  = RFF.mlSigmaF res
            snOpt  = RFF.mlSigmaN res
            -- σ_n² = λ (Ridge と GP のノイズ等価関係)
            lamOpt = snOpt * snOpt
        printf "    ℓ      = %.4g\n" ellOpt
        printf "    σ_f    = %.4g\n" sfOpt
        printf "    σ_n    = %.4g  (λ = σ_n² = %.4g)\n" snOpt lamOpt
        printf "    log_mlik = %.4f  (グリッド %d 点評価)\n"
               (RFF.mlLogMlik res) (RFF.mlGridPts res)
        return (ellOpt, lamOpt, sfOpt)
      else do
        let ell0 = case koBandwidth opts of
              Just h  -> h
              Nothing -> defaultLengthScale (map LA.toList (LA.toColumns xMat))
        printf "  ell=%.4f  lambda=%.4f\n" ell0 (koLambda opts)
        return (ell0, koLambda opts, 1.0)

  let d = koFeatures opts
  printf "  D=%d\n" d

  gen   <- createSystemRandom
  feats <- RFF.sampleRFFRBFMV p d ell sigF gen
  let fit  = RFF.rffRidgeMV feats xMat ys lam
      yhat = RFF.predictRFFRidgeMV fit xMat
      sse  = sum (zipWith (\a b -> (a - b)^(2::Int)) ys yhat)
      sst  = let m = sum ys / fromIntegral (max 1 (length ys))
             in sum [(y - m)^(2::Int) | y <- ys]
      r2   = if sst < 1e-12 then 0 else 1 - sse / sst
  printf "RFF (multivariate) Ridge fit:\n"
  printf "  R^2 = %.4f\n" r2
  printf "  RMSE = %.4g\n" (sqrt (sse / fromIntegral n))

  -- --group + --xaxis が両方指定されていればプロット
  case (koGroup opts, koXAxis opts) of
    (Just gCol, Just xCol) -> do
      let outPath = koOut opts
          fmt     = koFormat opts
      writeMVPlot fmt outPath df gCol xCol xCols yCol fit stdr cols ys
      putStrLn $ "Plot: " ++ outPath
      -- --report 指定時は ReportBuilder で統合 HTML を出力
      case koReport opts of
        Just rpath -> do
          let rep    = RI.RFFMVReport
                         { RI.rfmvFit         = fit
                         , RI.rfmvGroup       = gCol
                         , RI.rfmvXAxis       = xCol
                         , RI.rfmvInteractive = koInteractive opts
                         , RI.rfmvStandardizer =
                             if koStandardize opts then Just stdr else Nothing
                         }
              cfg    = RB.defaultReportConfig
                         (yCol <> " — Multivariate RFF Ridge"
                            <> if koInteractive opts then " (interactive)" else "")
              secs   = RB.toReport cfg df xCols yCol rep
          RB.renderReport rpath cfg secs
          putStrLn $ "Report: " ++ rpath
        Nothing -> return ()
    _ -> putStrLn
      "Plot skipped (use --group COL --xaxis COL to draw scatter+fit by group)"

-- | name (group) ごとに観測点と予測曲線をプロット。
-- 標準化 ON のときは予測グリッドを raw → 標準化空間に変換してから predict。
-- 横軸 / 観測点は raw 単位で表示する。
writeMVPlot
  :: OutputFormat -> FilePath
  -> DXD.DataFrame
  -> T.Text -> T.Text -> [T.Text] -> T.Text
  -> RFF.RFFRidgeFitMV
  -> Std.Standardizer
  -> [[Double]]             -- ^ raw cols
  -> [Double]
  -> IO ()
writeMVPlot fmt path df gCol xCol xCols yCol fit stdr cols ys = do
  case getMaybeTextVec gCol df of
    Nothing -> hPutStrLn stderr $
      "plot: group column '" ++ T.unpack gCol ++ "' not found"
    Just gv ->
      let groups = [ maybe "" id g | g <- V.toList gv ]
          xColIdx = case [ i | (i, c) <- zip [0..] xCols, c == xCol ] of
                      (i:_) -> i
                      []    -> 0
          xValuesAll = cols !! xColIdx
          xMin = minimum xValuesAll
          xMax = maximum xValuesAll
          ngrid = 100
          xGrid = [ xMin + fromIntegral i * (xMax - xMin) / fromIntegral (ngrid - 1)
                  | i <- [0 .. ngrid - 1] ]
          ptData = zip3 groups xValuesAll ys
          uniqGroups = uniq groups
          rowsForGroup g = [ i | (i, gg) <- zip [0..] groups, gg == g ]
          repValues g = [ (cols !! j) !! head (rowsForGroup g)
                        | j <- [0 .. length xCols - 1] ]
          mkLineData g =
            let rep = repValues g
                -- raw 値で row を組む
                makeRowRaw t =
                  [ if j == xColIdx then t else rep !! j
                  | j <- [0 .. length xCols - 1] ]
                xMatRaw = LA.fromLists [ makeRowRaw t | t <- xGrid ]
                -- 標準化空間に変換してから predict
                xMatStd = Std.applyStandardizer stdr xMatRaw
                ys'     = RFF.predictRFFRidgeMV fit xMatStd
            in [ (g, t, y') | (t, y') <- zip xGrid ys' ]
          lnData = concatMap mkLineData uniqGroups
          plotCfg = (defaultConfig (yCol <> " by " <> gCol))
                      { plotWidth = 720, plotHeight = 480 }
      in scatterWithGroupsFile fmt path plotCfg xCol yCol ptData lnData

uniq :: Ord a => [a] -> [a]
uniq []     = []
uniq (x:xs) = x : uniq (filter (/= x) xs)

-- | 各列の標準偏差の幾何平均で長さスケールを推定 (median heuristic 簡易版)。
defaultLengthScale :: [[Double]] -> Double
defaultLengthScale cols =
  let stds = [ std c | c <- cols, length c > 1 ]
      std xs = let n  = fromIntegral (length xs)
                   m  = sum xs / n
                   v  = sum [ (x - m)^(2::Int) | x <- xs ] / max 1 (n - 1)
               in sqrt v
      g  = product stds ** (1.0 / fromIntegral (max 1 (length stds)))
  in if g <= 0 then 1.0 else g

runKernelOn :: DXD.DataFrame -> T.Text -> T.Text -> V.Vector Double -> V.Vector Double
            -> KernelOpts -> IO ()
runKernelOn df xCol yCol xVec yVec opts = do
  let n = V.length xVec
      method = koMethod opts
      ker    = koKernel opts
      grid   = makeGrid xVec 100
      gridV  = V.fromList grid
  printf "Loaded %d rows; method=%s, kernel=%s\n"
         n (T.unpack method) (show ker)

  -- Bandwidth selection
  h <- case koBandwidth opts of
    Just hVal -> do
      printf "Bandwidth (specified): h = %.4f\n" hVal
      return hVal
    Nothing -> do
      let xMin = V.minimum xVec
          xMax = V.maximum xVec
          range = xMax - xMin
          hCands = [range/40, range/20, range/10, range/5, range/2.5]
          (bestH, bestRMSE) = Kern.gridSearchBandwidth ker xVec yVec hCands
      printf "Bandwidth (LOO-CV best): h = %.4f  (CV-RMSE = %.4f)\n"
             bestH bestRMSE
      return bestH

  -- Fit + predict on grid
  (gridY, sumStr) <- case method of
    "nw" -> do
      let ys = Kern.nwRegression ker h xVec yVec gridV
      return (V.toList ys, "Nadaraya-Watson, h=" ++ show h)
    "kr" -> do
      let lam = koLambda opts
          fit = Kern.kernelRidge ker h lam xVec yVec
          ys  = Kern.predictKernelRidge fit gridV
      return (V.toList ys
             , "Kernel Ridge, h=" ++ show h ++ ", lambda=" ++ show lam)
    "rff" -> do
      gen   <- createSystemRandom
      feats <- RFF.sampleRFFRBF (koFeatures opts) h 1.0 gen
      let lam = koLambda opts
          fit = RFF.rffRidge feats (V.toList xVec) (V.toList yVec) lam
          ys  = RFF.predictRFFRidge fit grid
      return (ys, "RFF, D=" ++ show (koFeatures opts)
                  ++ ", h=" ++ show h ++ ", lambda=" ++ show lam)
    _ -> error "unreachable"

  -- In-sample RMSE
  let predictX :: V.Vector Double -> [Double]
      predictX xs = case method of
        "nw" -> V.toList (Kern.nwRegression ker h xVec yVec xs)
        "kr" -> V.toList (Kern.predictKernelRidge
                          (Kern.kernelRidge ker h (koLambda opts) xVec yVec) xs)
        _    -> []        -- rff requires gen; skip in-sample for now
      ys = V.toList yVec
  case method of
    "rff" -> printf "Predictions on %d test points; in-sample RMSE skipped (RFF re-samples)\n"
                    (length grid)
    _     -> printf "RMSE (in-sample) = %.4f\n" (rmseV ys (predictX xVec))
  putStrLn $ "(" ++ sumStr ++ ")"

  -- Plot
  let sf = SmoothFit
             { sfX = grid
             , sfFit = gridY
             , sfLower = []
             , sfUpper = []
             , sfHasBand = False
             }
  writeSmoothPlot (koFormat opts) (koOut opts)
    (T.pack ("Kernel: " ++ T.unpack method)) df xCol yCol sf
  putStrLn ("Plot: " ++ koOut opts)
  openInBrowser (koOut opts)

  -- HTML レポート (--report)
  case koReport opts of
    Nothing -> return ()
    Just rpath -> do
      let xs = V.toList xVec
          ys = V.toList yVec
          smooth = RB.SmoothCurve grid gridY [] []
          modelLbl = "Kernel regression (" <> method <> ")"
          formula = T.pack (T.unpack yCol ++ " ~ f(" ++ T.unpack xCol ++ ")")
          cfg = RB.defaultReportConfig
                  ("Kernel regression — " <> yCol <> " ~ " <> xCol)
          baseKVs =
            [ ("Method",    method)
            , ("Kernel",    T.pack (show ker))
            , ("Bandwidth", T.pack (printf "%.4f" h))
            ]
          extraKVs = case method of
            "kr"  -> [("Lambda", T.pack (printf "%g" (koLambda opts)))]
            "rff" -> [("Features", T.pack (show (koFeatures opts)))
                     ,("Lambda",   T.pack (printf "%g" (koLambda opts)))]
            _     -> []
          sections =
            [ RB.secDataOverview df [xCol] yCol
            , RB.secModelOverview modelLbl formula Nothing
            , RB.secKeyValue "Fit summary" (baseKVs ++ extraKVs)
            , RB.secFitScatter xCol yCol xs ys (Just smooth)
            ]
      RB.renderReport rpath cfg sections
      putStrLn ("Report: " ++ rpath)
      openInBrowser rpath

-- ---------------------------------------------------------------------------
-- spline subcommand
-- ---------------------------------------------------------------------------

splineUsage :: String
splineUsage = unlines
  [ "Usage: hanalyze spline <file> <xcol> <ycol> [options]"
  , ""
  , "Options:"
  , "  --type T          bspline|natural (default: bspline)"
  , "  --knots N         number of internal knots (default: 5)"
  , "  --degree D        B-spline degree (default: 3 = cubic)"
  , "  --format FMT      html|png|svg (default: html)"
  , "  --out FILE        scatter+fit output path (default: spline.html)"
  , "  --report [FILE]   build composite HTML report (default: spline.html)"
  , ""
  , "Examples:"
  , "  hanalyze spline data.csv x y --knots 8"
  , "  hanalyze spline data.csv x y --type natural"
  , "  hanalyze spline data.csv x y --type bspline --degree 3 --knots 10"
  ]

data SplineOpts = SplineOpts
  { soType   :: T.Text
  , soKnots  :: Int
  , soDegree :: Int
  , soFormat :: OutputFormat
  , soOut    :: FilePath
  , soReport :: Maybe FilePath
  }

defaultSplineOpts :: SplineOpts
defaultSplineOpts = SplineOpts "bspline" 5 3 HTML "spline.html" Nothing

runSplineCmd :: [String] -> IO ()
runSplineCmd args0 =
  let (lopts, args) = parseLoadOpts args0
  in case args of
       (file : xColStr : yColStr : rest) ->
         case parseSplineOpts rest defaultSplineOpts of
           Left err   -> hPutStrLn stderr ("spline: " ++ err)
           Right opts -> doSpline file xColStr yColStr opts lopts
       _ -> putStrLn splineUsage

parseSplineOpts :: [String] -> SplineOpts -> Either String SplineOpts
parseSplineOpts [] acc = Right acc
parseSplineOpts (flag : rest) acc
  | flag == "--type" = case rest of
      (v:rs) | v `elem` ["bspline","natural"] ->
        parseSplineOpts rs (acc { soType = T.pack v })
      (v:_) -> Left ("unknown spline type '" ++ v ++ "'")
      []    -> Left "--type requires an argument"
  | flag == "--knots" = case rest of
      (v:rs) -> case reads v :: [(Int, String)] of
        [(d,"")] -> parseSplineOpts rs (acc { soKnots = d })
        _        -> Left ("invalid --knots '" ++ v ++ "'")
      []     -> Left "--knots requires a value"
  | flag == "--degree" = case rest of
      (v:rs) -> case reads v :: [(Int, String)] of
        [(d,"")] -> parseSplineOpts rs (acc { soDegree = d })
        _        -> Left ("invalid --degree '" ++ v ++ "'")
      []     -> Left "--degree requires a value"
  | flag `elem` ["-f","--format"] = case rest of
      (v:rs) -> case parseFormat v of
        Right f -> parseSplineOpts rs (acc { soFormat = f })
        Left e  -> Left e
      []     -> Left "--format requires an argument"
  | flag == "--out" = case rest of
      (v:rs) -> parseSplineOpts rs (acc { soOut = v })
      []     -> Left "--out requires a file path"
  | flag == "--report" = case rest of
      (v:rs) | not (null v) && head v /= '-' ->
        parseSplineOpts rs (acc { soReport = Just v })
      _ -> parseSplineOpts rest (acc { soReport = Just "spline.html" })
  | otherwise = Left ("unexpected argument '" ++ flag ++ "'")

doSpline :: FilePath -> String -> String -> SplineOpts -> LoadOpts -> IO ()
doSpline file xColStr yColStr opts lopts = do
  let xCol = T.pack xColStr
      yCol = T.pack yColStr
  result <- loadXY lopts file [xCol] yCol
  case result of
    Left err -> hPutStrLn stderr err
    Right (df, [xVec], yVec) -> do
      let kind = case soType opts of
            "natural" -> Spl.NaturalCubic
            _         -> Spl.BSpline (soDegree opts)
          k    = soKnots opts
          xMin = V.minimum xVec
          xMax = V.maximum xVec
          knots = [ xMin + fromIntegral i * (xMax - xMin) / fromIntegral (k + 1)
                  | i <- [1 .. k] ]
          fit   = Spl.fitSpline kind knots xVec yVec
          grid  = makeGrid xVec 100
          gridV = V.fromList grid
          gridY = V.toList (Spl.predictSpline fit gridV)
          n     = V.length xVec
          ys    = V.toList yVec
          yhatIn = V.toList (Spl.predictSpline fit xVec)
          rmseVal = rmseV ys yhatIn
      printf "Loaded %d rows; type=%s, knots=%d%s\n"
             n (T.unpack (soType opts)) k
             (if soType opts == "bspline"
                then ", degree=" ++ show (soDegree opts) else "")
      printf "RMSE (in-sample) = %.4f\n" rmseVal
      let sf = SmoothFit
                 { sfX = grid
                 , sfFit = gridY
                 , sfLower = []
                 , sfUpper = []
                 , sfHasBand = False
                 }
      writeSmoothPlot (soFormat opts) (soOut opts)
        (T.pack ("Spline: " ++ T.unpack (soType opts))) df xCol yCol sf
      putStrLn ("Plot: " ++ soOut opts)
      openInBrowser (soOut opts)
      -- HTML レポート (--report)
      case soReport opts of
        Nothing -> return ()
        Just rpath -> do
          let smooth = RB.SmoothCurve grid gridY [] []
              modelLbl = "Spline regression (" <> soType opts <> ")"
              formula = T.pack (T.unpack yCol ++ " ~ s("
                                ++ T.unpack xCol ++ "; knots="
                                ++ show k ++ ")")
              cfg = RB.defaultReportConfig
                      ("Spline regression — " <> yCol <> " ~ " <> xCol)
              sections =
                [ RB.secDataOverview df [xCol] yCol
                , RB.secModelOverview modelLbl formula Nothing
                , RB.secKeyValue "Fit summary"
                    [ ("Type",      soType opts)
                    , ("Knots",     T.pack (show k))
                    , ("Degree",    T.pack (show (soDegree opts)))
                    , ("RMSE (in-sample)", T.pack (printf "%.4f" rmseVal))
                    ]
                , RB.secFitScatter xCol yCol (V.toList xVec) ys
                    (Just smooth)
                , RB.secResiduals yhatIn (zipWith (-) ys yhatIn)
                ]
          RB.renderReport rpath cfg sections
          putStrLn ("Report: " ++ rpath)
          openInBrowser rpath
    Right _ -> hPutStrLn stderr "spline: expected single x column"

-- ---------------------------------------------------------------------------
-- ridge report ヘルパ
-- ---------------------------------------------------------------------------

ridgeModelLabel :: RidgeOpts -> T.Text
ridgeModelLabel opts =
  "Regularized regression (" <> roPenalty opts <> ")"

ridgeFormula :: RidgeOpts -> [T.Text] -> T.Text -> T.Text
ridgeFormula opts xCols yCol =
  T.pack (T.unpack yCol ++ " ~ "
          ++ intercalate " + " (map T.unpack xCols)
          ++ "  (lambda=" ++ show (roLambda opts) ++ ")")

ridgeReportConfig :: RidgeOpts -> [T.Text] -> T.Text -> RB.ReportConfig
ridgeReportConfig _opts xCols yCol = RB.defaultReportConfig
  ("Regularized regression — "
   <> yCol <> " ~ " <> T.intercalate " + " xCols)

ridgeFitKVs :: RidgeOpts -> Reg.RegFit -> [Double] -> Double -> [(T.Text, T.Text)]
ridgeFitKVs opts fit beta rmseVal =
  [ ("RMSE (in-sample)", T.pack (printf "%.4f" rmseVal))
  , ("|β| > 1e-8", T.pack (show (Reg.rfNonZero fit) <> " / "
                            <> show (length beta)))
  , ("Penalty", roPenalty opts)
  , ("Lambda", T.pack (printf "%g" (roLambda opts)))
  ]

-- | Regularization path: λ を 1e-4 .. 1e2 で対数スケール掃引、
-- 各 λ で fit して係数を集める。intercept は除外して可視化。
mkRidgePathSection :: [T.Text] -> LA.Matrix Double -> LA.Vector Double
                   -> RidgeOpts -> RB.ReportSection
mkRidgePathSection xCols xMat yLA opts =
  let lambdas = [10 ** (-4 + 0.1 * fromIntegral i) | i <- [0 .. 60 :: Int]]
      mkPen lam = case roPenalty opts of
        "ridge"       -> Reg.L2 lam
        "lasso"       -> Reg.L1 lam
        "elasticnet"  -> Reg.ElasticNet (lam * roAlpha opts)
                                         (lam * (1 - roAlpha opts))
        _             -> Reg.L2 lam
      path = Reg.regularizationPath mkPen lambdas xMat yLA
      -- intercept (係数 0) を除外
      pathNoInt = [ (lam, drop 1 coefs) | (lam, coefs) <- path ]
      title = "Regularization path (" <> roPenalty opts <> ")"
      spec  = RB.regPathSpec xCols pathNoInt
  in RB.secVega title spec

-- ---------------------------------------------------------------------------
-- quantile subcommand
-- ---------------------------------------------------------------------------

quantileUsage :: String
quantileUsage = unlines
  [ "Usage: hanalyze quantile <file> <xcols> <ycol> [options]"
  , ""
  , "  <xcols>   x column name(s); quote multiple: \"x1 x2\""
  , "  <ycol>    y column name (single)"
  , ""
  , "Options:"
  , "  --tau T          quantile in (0, 1) (default: 0.5 = median)"
  , "  --taus T1,T2,... overlay multiple quantiles in the report (e.g. 0.1,0.5,0.9)"
  , "  --format FMT     html|png|svg (default: html)"
  , "  --out FILE       scatter+fit output path (default: quantile.html)"
  , "  --report [FILE]  build composite HTML report (default: quantile.html)"
  , ""
  , "Examples:"
  , "  hanalyze quantile data.csv x y --tau 0.5"
  , "  hanalyze quantile data.csv x y --taus 0.1,0.5,0.9 --report"
  ]

data QuantileOpts = QuantileOpts
  { qoTau    :: Double
  , qoTaus   :: [Double]    -- when not empty, overlay multiple quantiles
  , qoFormat :: OutputFormat
  , qoOut    :: FilePath
  , qoReport :: Maybe FilePath
  }

defaultQuantileOpts :: QuantileOpts
defaultQuantileOpts = QuantileOpts 0.5 [] HTML "quantile.html" Nothing

runQuantileCmd :: [String] -> IO ()
runQuantileCmd args0 =
  let (lopts, args) = parseLoadOpts args0
  in case args of
       (file : xColsStr : yColStr : rest) ->
         case parseQuantileOpts rest defaultQuantileOpts of
           Left err   -> hPutStrLn stderr ("quantile: " ++ err)
           Right opts -> doQuantile file xColsStr yColStr opts lopts
       _ -> putStrLn quantileUsage

parseQuantileOpts :: [String] -> QuantileOpts -> Either String QuantileOpts
parseQuantileOpts [] acc = Right acc
parseQuantileOpts (flag : rest) acc
  | flag == "--tau" = case rest of
      (v:rs) -> case reads v :: [(Double, String)] of
        [(d,"")] | d > 0, d < 1 -> parseQuantileOpts rs (acc { qoTau = d })
        _        -> Left ("invalid --tau '" ++ v ++ "' (must be in (0,1))")
      []     -> Left "--tau requires a value"
  | flag == "--taus" = case rest of
      (v:rs) ->
        let parts = filter (not . null) (splitOnComma v)
        in case mapM (\s -> case reads s :: [(Double, String)] of
                              [(d,"")] | d > 0, d < 1 -> Just d
                              _ -> Nothing) parts of
             Just ds -> parseQuantileOpts rs (acc { qoTaus = ds })
             Nothing -> Left ("invalid --taus '" ++ v
                              ++ "' (comma-separated values in (0,1))")
      [] -> Left "--taus requires a value"
  | flag `elem` ["-f","--format"] = case rest of
      (v:rs) -> case parseFormat v of
        Right f -> parseQuantileOpts rs (acc { qoFormat = f })
        Left e  -> Left e
      []     -> Left "--format requires an argument"
  | flag == "--out" = case rest of
      (v:rs) -> parseQuantileOpts rs (acc { qoOut = v })
      []     -> Left "--out requires a file path"
  | flag == "--report" = case rest of
      (v:rs) | not (null v) && head v /= '-' ->
        parseQuantileOpts rs (acc { qoReport = Just v })
      _ -> parseQuantileOpts rest (acc { qoReport = Just "quantile.html" })
  | otherwise = Left ("unexpected argument '" ++ flag ++ "'")

splitOnComma :: String -> [String]
splitOnComma s = case break (== ',') s of
  (a, ',' : rest) -> a : splitOnComma rest
  (a, _)          -> [a]

doQuantile :: FilePath -> String -> String -> QuantileOpts -> LoadOpts -> IO ()
doQuantile file xColsStr yColStr opts lopts = do
  let xCols = map T.pack (words xColsStr)
      yCol  = T.pack yColStr
  result <- loadXY lopts file xCols yCol
  case result of
    Left err -> hPutStrLn stderr err
    Right (df, xVecs, yVec) -> do
      let n = V.length yVec
          intercept = LA.konst 1 n
          xMat = LA.fromColumns
                   (intercept : map (LA.fromList . V.toList) xVecs)
          yLA  = LA.fromList (V.toList yVec)
          tau  = qoTau opts
          fit  = QR.fitQuantile tau xMat yLA
          beta = LA.toList (QR.qfBeta fit)
      printf "Loaded %d rows from %s\n" n file
      printf "Quantile: tau = %.3f  (median: %s)\n" tau
             (if abs (tau - 0.5) < 1e-9 then ("yes" :: String) else "no")
      printf "MM-IRLS converged in %d iterations\n" (QR.qfIters fit)
      putStrLn ""
      putStrLn "Coefficients:"
      printf "  %-30s = %9.4f\n" ("intercept" :: String) (head beta)
      mapM_ (\(i, c, b) ->
        printf "  %-30s = %9.4f\n"
               ("β_" ++ show (i :: Int) ++ " (" ++ T.unpack c ++ ")") b)
        (zip3 [1..] xCols (tail beta))
      printf "Pinball loss V̂_τ: %.4f\n" (QR.qfPinball fit)
      printf "Pseudo R¹_τ:      %.4f\n" (QR.qfR1 fit)

      -- 単変数なら scatter + fit (+ overlay multiple quantiles)
      case (xCols, xVecs) of
        ([xc1], [xVec]) -> do
          let xs = V.toList xVec
              ys = V.toList yVec
              grid = makeGrid xVec 100
              gridMat = LA.fromColumns
                          [ LA.konst 1 100, LA.fromList grid ]
              gridY = LA.toList (QR.predictQuantile fit gridMat)
              sf = SmoothFit
                     { sfX = grid
                     , sfFit = gridY
                     , sfLower = []
                     , sfUpper = []
                     , sfHasBand = False
                     }
              _ = xs
          writeSmoothPlot (qoFormat opts) (qoOut opts)
            (T.pack ("Quantile τ=" ++ show tau)) df xc1 yCol sf
          putStrLn ("Plot: " ++ qoOut opts)
          openInBrowser (qoOut opts)

          -- HTML レポート
          case qoReport opts of
            Nothing -> return ()
            Just rpath -> do
              let coeffPairs = zip ("intercept" : xCols) beta
                  modelLbl = "Quantile regression (τ=" <> T.pack (show tau) <> ")"
                  formula = T.pack ("Q_τ(" ++ T.unpack yCol ++ "|x) = "
                                    ++ "β₀ + " ++ T.unpack (T.intercalate " + "
                                                              [ "β" <> T.pack (show i)
                                                                <> "·" <> c
                                                              | (i, c) <- zip [(1::Int)..] xCols ]))
                  cfg = RB.defaultReportConfig
                          ("Quantile regression — τ=" <> T.pack (show tau)
                           <> ",  " <> yCol <> " ~ " <> T.intercalate " + " xCols)
                  baseSections =
                    [ RB.secDataOverview df xCols yCol
                    , RB.secModelOverview modelLbl formula Nothing
                    , RB.secCoefficients coeffPairs (Just ("Pseudo R¹_τ", QR.qfR1 fit))
                    , RB.secKeyValue "Fit summary"
                        [ ("τ",                T.pack (printf "%.3f" tau))
                        , ("Pinball loss V̂_τ", T.pack (printf "%.4f" (QR.qfPinball fit)))
                        , ("Iterations",       T.pack (show (QR.qfIters fit)))
                        ]
                    , RB.secFitScatter xc1 yCol xs ys (Just (RB.SmoothCurve grid gridY [] []))
                    , RB.secResiduals (LA.toList (QR.qfYHat fit))
                                      (LA.toList (QR.qfResid fit))
                    ]
                  -- overlay multi quantile chart
                  multiSec = case qoTaus opts of
                    [] -> []
                    taus ->
                      let curves = [ ( T.pack ("τ=" ++ show t)
                                     , LA.toList (QR.predictQuantile
                                                   (QR.fitQuantile t xMat yLA)
                                                   gridMat))
                                   | t <- taus ]
                          spec = multiQuantileSpec xc1 yCol xs ys grid curves
                      in [RB.secVega "Multiple quantile fits" spec]
              RB.renderReport rpath cfg (baseSections ++ multiSec)
              putStrLn ("Report: " ++ rpath)
              openInBrowser rpath
        _ -> putStrLn "(scatter plot skipped for multiple x columns)"

-- 複数分位線を 1 枚の Vega-Lite spec で描く
multiQuantileSpec :: T.Text -> T.Text -> [Double] -> [Double] -> [Double]
                  -> [(T.Text, [Double])] -> VegaLite
multiQuantileSpec xc yc xs ys grid curves =
  VL.toVegaLite
    [ VL.layer
        [ VL.asSpec
            [ VL.dataFromColumns []
                . VL.dataColumn xc (VL.Numbers xs)
                . VL.dataColumn yc (VL.Numbers ys)
                $ []
            , VL.mark VL.Point
                [VL.MOpacity 0.5, VL.MSize 40, VL.MColor "#888888"]
            , VL.encoding
                . VL.position VL.X
                    [VL.PName xc, VL.PmType VL.Quantitative,
                     VL.PAxis [VL.AxTitle xc]]
                . VL.position VL.Y
                    [VL.PName yc, VL.PmType VL.Quantitative,
                     VL.PAxis [VL.AxTitle yc]]
                $ []
            ]
        , VL.asSpec (multiLineLayer xc yc grid curves)
        ]
    , VL.width 640
    , VL.height 320
    ]

multiLineLayer :: T.Text -> T.Text -> [Double] -> [(T.Text, [Double])]
               -> [(VLProperty, VLSpec)]
multiLineLayer xc yc grid curves =
  let rowsX  = concat [ replicate (length grid) lbl | (lbl, _) <- curves ]
      rowsXs = concat [ grid                         | _       <- curves ]
      rowsYs = concat [ ys'                          | (_, ys') <- curves ]
  in [ VL.dataFromColumns []
         . VL.dataColumn "tau" (VL.Strings rowsX)
         . VL.dataColumn xc    (VL.Numbers rowsXs)
         . VL.dataColumn yc    (VL.Numbers rowsYs)
         $ []
     , VL.mark VL.Line [VL.MStrokeWidth 2.2]
     , VL.encoding
         . VL.position VL.X [VL.PName xc, VL.PmType VL.Quantitative]
         . VL.position VL.Y [VL.PName yc, VL.PmType VL.Quantitative]
         . VL.color [VL.MName "tau", VL.MmType VL.Nominal,
                     VL.MScale [VL.SScheme "tableau10" []]]
         $ []
     ]

-- ---------------------------------------------------------------------------
-- gam subcommand
-- ---------------------------------------------------------------------------

gamUsage :: String
gamUsage = unlines
  [ "Usage: hanalyze gam <file> <xcols> <ycol> [options]"
  , ""
  , "  <xcols>  x column names; quote multiple: \"x1 x2 x3\""
  , "  <ycol>   y column name"
  , ""
  , "Options:"
  , "  --knots N        per-feature internal knot count (default: 5)"
  , "  --degree D       B-spline degree (default: 3 = cubic)"
  , "  --lambda L       Ridge regularization on spline coefficients (default: 0.01)"
  , "  --report [FILE]  build composite HTML report with per-feature partials"
  , ""
  , "Example:"
  , "  hanalyze gam data.csv \"x1 x2 x3\" y --knots 8 --lambda 0.05 --report"
  ]

data GAMOpts = GAMOpts
  { goKnots  :: Int
  , goDegree :: Int
  , goLambda :: Double
  , goReport :: Maybe FilePath
  }

defaultGAMOpts :: GAMOpts
defaultGAMOpts = GAMOpts 5 3 0.01 Nothing

runGAMCmd :: [String] -> IO ()
runGAMCmd args0 =
  let (lopts, args) = parseLoadOpts args0
  in case args of
       (file : xColsStr : yColStr : rest) ->
         case parseGAMOpts rest defaultGAMOpts of
           Left err   -> hPutStrLn stderr ("gam: " ++ err)
           Right opts -> doGAM file xColsStr yColStr opts lopts
       _ -> putStrLn gamUsage

parseGAMOpts :: [String] -> GAMOpts -> Either String GAMOpts
parseGAMOpts [] acc = Right acc
parseGAMOpts (flag:rest) acc
  | flag == "--knots" = case rest of
      (v:rs) -> case reads v :: [(Int,String)] of
        [(d,"")] -> parseGAMOpts rs (acc { goKnots = d })
        _ -> Left ("invalid --knots '" ++ v ++ "'")
      [] -> Left "--knots requires a value"
  | flag == "--degree" = case rest of
      (v:rs) -> case reads v :: [(Int,String)] of
        [(d,"")] -> parseGAMOpts rs (acc { goDegree = d })
        _ -> Left ("invalid --degree '" ++ v ++ "'")
      [] -> Left "--degree requires a value"
  | flag == "--lambda" = case rest of
      (v:rs) -> case reads v :: [(Double,String)] of
        [(d,"")] -> parseGAMOpts rs (acc { goLambda = d })
        _ -> Left ("invalid --lambda '" ++ v ++ "'")
      [] -> Left "--lambda requires a value"
  | flag == "--report" = case rest of
      (v:rs) | not (null v) && head v /= '-' ->
        parseGAMOpts rs (acc { goReport = Just v })
      _ -> parseGAMOpts rest (acc { goReport = Just "gam.html" })
  | otherwise = Left ("unexpected argument '" ++ flag ++ "'")

doGAM :: FilePath -> String -> String -> GAMOpts -> LoadOpts -> IO ()
doGAM file xColsStr yColStr opts lopts = do
  let xCols = map T.pack (words xColsStr)
      yCol  = T.pack yColStr
  result <- loadXY lopts file xCols yCol
  case result of
    Left err -> hPutStrLn stderr err
    Right (df, xVecs, yVec) -> do
      let fit = GAM.fitGAM (goDegree opts) (goKnots opts) (goLambda opts)
                            xVecs yVec
          n = V.length yVec
          ys = V.toList yVec
          yhat = LA.toList (GAM.gamYHat fit)
          resid = LA.toList (GAM.gamResid fit)
      printf "Loaded %d rows from %s\n" n file
      printf "GAM: degree=%d, knots=%d/feature, lambda=%g\n"
             (goDegree opts) (goKnots opts) (goLambda opts)
      printf "Features: %d (%s)\n" (length xCols)
             (T.unpack (T.intercalate ", " xCols))
      printf "Intercept: %.4f\n" (GAM.gamIntercept fit)
      printf "R²:        %.4f\n" (GAM.gamR2 fit)
      let rmseVal = sqrt (sum [ r ^ (2 :: Int) | r <- resid ]
                          / fromIntegral n)
      printf "RMSE (in-sample): %.4f\n" rmseVal

      case goReport opts of
        Nothing -> return ()
        Just rpath -> do
          let modelLbl = "Generalized Additive Model"
              formula = yCol <> " = β₀ + " <> T.intercalate " + "
                          [ "s(" <> c <> ")" | c <- xCols ]
              cfg = RB.defaultReportConfig
                      ("GAM — " <> yCol <> " ~ s("
                       <> T.intercalate ") + s(" xCols <> ")")
              partialSecs =
                [ RB.secVega ("Partial effect: s(" <> c <> ")")
                    (gamPartialSpec c xVec fit j)
                | (j, c, xVec) <- zip3 [0..] xCols xVecs ]
              sections =
                [ RB.secDataOverview df xCols yCol
                , RB.secModelOverview modelLbl formula Nothing
                , RB.secKeyValue "Fit summary"
                    [ ("Degree",   T.pack (show (goDegree opts)))
                    , ("Knots",    T.pack (show (goKnots opts)))
                    , ("Lambda",   T.pack (printf "%g" (goLambda opts)))
                    , ("Intercept",T.pack (printf "%.4f"
                                             (GAM.gamIntercept fit)))
                    , ("R²",       T.pack (printf "%.4f" (GAM.gamR2 fit)))
                    , ("RMSE",     T.pack (printf "%.4f" rmseVal))
                    ]
                ] ++ partialSecs ++
                [ RB.secResiduals yhat resid ]
              _ = ys
          RB.renderReport rpath cfg sections
          putStrLn ("Report: " ++ rpath)
          openInBrowser rpath

-- ---------------------------------------------------------------------------
-- rf subcommand
-- ---------------------------------------------------------------------------

rfUsage :: String
rfUsage = unlines
  [ "Usage: hanalyze rf <file> <xcols> <ycol> [options]"
  , ""
  , "Options:"
  , "  --trees N        number of trees (default: 100)"
  , "  --max-depth D    maximum tree depth (default: 12)"
  , "  --min-samples N  minimum samples per leaf (default: 3)"
  , "  --mtry M         features per split (default: max(1, d/3))"
  , "  --report [FILE]  build composite HTML report (with feature importance)"
  , ""
  , "Example:"
  , "  hanalyze rf data.csv \"x1 x2 x3\" y --trees 200 --report"
  ]

data RFOpts = RFOpts
  { roTrees      :: Int
  , roMaxDepth   :: Int
  , roMinSamples :: Int
  , roMtry       :: Maybe Int
  , roReport_    :: Maybe FilePath
  }

defaultRFOpts :: RFOpts
defaultRFOpts = RFOpts 100 12 3 Nothing Nothing

runRFCmd :: [String] -> IO ()
runRFCmd args0 =
  let (lopts, args) = parseLoadOpts args0
  in case args of
       (file : xColsStr : yColStr : rest) ->
         case parseRFOpts rest defaultRFOpts of
           Left err   -> hPutStrLn stderr ("rf: " ++ err)
           Right opts -> doRF file xColsStr yColStr opts lopts
       _ -> putStrLn rfUsage

parseRFOpts :: [String] -> RFOpts -> Either String RFOpts
parseRFOpts [] acc = Right acc
parseRFOpts (flag:rest) acc
  | flag == "--trees" = case rest of
      (v:rs) -> case reads v :: [(Int,String)] of
        [(d,"")] -> parseRFOpts rs (acc { roTrees = d })
        _ -> Left ("invalid --trees '" ++ v ++ "'")
      [] -> Left "--trees requires a value"
  | flag == "--max-depth" = case rest of
      (v:rs) -> case reads v :: [(Int,String)] of
        [(d,"")] -> parseRFOpts rs (acc { roMaxDepth = d })
        _ -> Left ("invalid --max-depth '" ++ v ++ "'")
      [] -> Left "--max-depth requires a value"
  | flag == "--min-samples" = case rest of
      (v:rs) -> case reads v :: [(Int,String)] of
        [(d,"")] -> parseRFOpts rs (acc { roMinSamples = d })
        _ -> Left ("invalid --min-samples '" ++ v ++ "'")
      [] -> Left "--min-samples requires a value"
  | flag == "--mtry" = case rest of
      (v:rs) -> case reads v :: [(Int,String)] of
        [(d,"")] -> parseRFOpts rs (acc { roMtry = Just d })
        _ -> Left ("invalid --mtry '" ++ v ++ "'")
      [] -> Left "--mtry requires a value"
  | flag == "--report" = case rest of
      (v:rs) | not (null v) && head v /= '-' ->
        parseRFOpts rs (acc { roReport_ = Just v })
      _ -> parseRFOpts rest (acc { roReport_ = Just "rf.html" })
  | otherwise = Left ("unexpected argument '" ++ flag ++ "'")

doRF :: FilePath -> String -> String -> RFOpts -> LoadOpts -> IO ()
doRF file xColsStr yColStr opts lopts = do
  let xCols = map T.pack (words xColsStr)
      yCol  = T.pack yColStr
  result <- loadXY lopts file xCols yCol
  case result of
    Left err -> hPutStrLn stderr err
    Right (df, xVecs, yVec) -> do
      let n     = V.length yVec
          rows  = [ [ xv V.! i | xv <- xVecs ] | i <- [0 .. n - 1] ]
          ys    = V.toList yVec
          cfg   = RF.defaultRFConfig
                    { RF.rfTrees      = roTrees opts
                    , RF.rfMaxDepth   = roMaxDepth opts
                    , RF.rfMinSamples = roMinSamples opts
                    , RF.rfMtry       = roMtry opts
                    }
      gen <- createSystemRandom
      forest <- RF.fitRF cfg rows ys gen
      let yhat = map (RF.predictRF forest) rows
          resid = zipWith (-) ys yhat
          yMean = sum ys / fromIntegral n
          tss   = sum [ (y - yMean) ^ (2 :: Int) | y <- ys ]
          rss   = sum [ r ^ (2 :: Int) | r <- resid ]
          r2    = if tss < 1e-12 then 0 else 1 - rss / tss
          rmseVal = sqrt (rss / fromIntegral n)
          imp   = V.toList (RF.featureImportance forest)
          impPairs = zip xCols imp
      printf "Loaded %d rows from %s\n" n file
      printf "RandomForest: trees=%d, max-depth=%d, min-samples=%d\n"
             (roTrees opts) (roMaxDepth opts) (roMinSamples opts)
      printf "R²:               %.4f\n" r2
      printf "RMSE (in-sample): %.4f\n" rmseVal
      putStrLn ""
      putStrLn "Feature importance (split-count fraction):"
      mapM_ (\(c, v) -> printf "  %-20s = %.4f\n" (T.unpack c) v) impPairs

      case roReport_ opts of
        Nothing -> return ()
        Just rpath -> do
          let modelLbl = "Random Forest regression"
              formula = yCol <> " ~ ensemble of " <> T.pack (show (roTrees opts))
                        <> " CART trees over (" <> T.intercalate ", " xCols <> ")"
              cfg' = RB.defaultReportConfig
                       ("Random Forest — " <> yCol <> " ~ "
                        <> T.intercalate " + " xCols)
              sections =
                [ RB.secDataOverview df xCols yCol
                , RB.secModelOverview modelLbl formula Nothing
                , RB.secKeyValue "Fit summary"
                    [ ("Trees",       T.pack (show (roTrees opts)))
                    , ("Max depth",   T.pack (show (roMaxDepth opts)))
                    , ("Min samples", T.pack (show (roMinSamples opts)))
                    , ("R²",          T.pack (printf "%.4f" r2))
                    , ("RMSE",        T.pack (printf "%.4f" rmseVal))
                    ]
                , RB.secBarChart "Feature importance"
                    [ (c, v) | (c, v) <- impPairs ]
                , RB.secResiduals yhat resid
                ]
          RB.renderReport rpath cfg' sections
          putStrLn ("Report: " ++ rpath)
          openInBrowser rpath

-- 1 特徴の partial effect s_j(x_j) を Vega-Lite 散布+曲線で
gamPartialSpec :: T.Text -> V.Vector Double -> GAM.GAMFit -> Int -> VegaLite
gamPartialSpec col xVec fit j =
  let xs = V.toList xVec
      lo = V.minimum xVec
      hi = V.maximum xVec
      grid = [ lo + fromIntegral i * (hi - lo) / 99 | i <- [0..99::Int]]
      gridV = V.fromList grid
      sj = V.toList (GAM.predictGAMComponent fit j gridV)
      -- partial residuals: resid + s_j(x_i) (説明用にプロット)
      partialAtData = V.toList (GAM.predictGAMComponent fit j xVec)
      residList = LA.toList (GAM.gamResid fit)
      partials = zipWith (+) residList partialAtData
  in VL.toVegaLite
       [ VL.layer
           [ VL.asSpec
               [ VL.dataFromColumns []
                   . VL.dataColumn col (VL.Numbers xs)
                   . VL.dataColumn "partial" (VL.Numbers partials)
                   $ []
               , VL.mark VL.Point
                   [VL.MOpacity 0.5, VL.MSize 40, VL.MColor "#888888"]
               , VL.encoding
                   . VL.position VL.X
                       [VL.PName col, VL.PmType VL.Quantitative,
                        VL.PAxis [VL.AxTitle col]]
                   . VL.position VL.Y
                       [VL.PName "partial", VL.PmType VL.Quantitative,
                        VL.PAxis [VL.AxTitle "Partial residual"]]
                   $ []
               ]
           , VL.asSpec
               [ VL.dataFromColumns []
                   . VL.dataColumn col (VL.Numbers grid)
                   . VL.dataColumn "s_j" (VL.Numbers sj)
                   $ []
               , VL.mark VL.Line
                   [VL.MStrokeWidth 2.5, VL.MColor "#DD5566"]
               , VL.encoding
                   . VL.position VL.X
                       [VL.PName col, VL.PmType VL.Quantitative]
                   . VL.position VL.Y
                       [VL.PName "s_j", VL.PmType VL.Quantitative]
                   $ []
               ]
           ]
       , VL.width 500
       , VL.height 240
       ]
