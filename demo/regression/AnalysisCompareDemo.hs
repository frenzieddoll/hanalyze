{-# LANGUAGE OverloadedStrings #-}
-- | AnalysisReport vs ReportBuilder の比較デモ。
--
-- LM / GLM / GLMM / GP / HBM の 5 モデルそれぞれで:
--   1. 既存 'Viz.AnalysisReport' で HTML を生成
--   2. 新 'Viz.ReportBuilder' で同等の HTML を生成
-- → trash/ 以下に 10 ファイルが出力されるので、ブラウザで開いて見比べる。
module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import System.Random.MWC (createSystemRandom)
import Text.Printf (printf)

import DataFrame.Core
import DataIO.CSV          (loadAuto)
import qualified Model.Core as Core
import qualified Model.LM   as LM
import qualified Model.GLM  as GLM
import qualified Model.GLMM as GLMM
import qualified Model.GP   as GP
import qualified Model.HBM  as HBM
import qualified MCMC.NUTS  as NUTS
import qualified MCMC.Core  as MCMCcore
import qualified Stat.MCMC  as StatMCMC

import Model.Core (residualsV, fittedList, coeffList, rSquared1)

import qualified Viz.AnalysisReport as AR
import qualified Viz.ReportBuilder  as RB
import qualified Viz.ModelGraph     as VMG

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

makeGrid :: V.Vector Double -> Int -> [Double]
makeGrid v n =
  let lo = V.minimum v
      hi = V.maximum v
  in [ lo + fromIntegral i * (hi - lo) / fromIntegral (n - 1)
     | i <- [0 .. n - 1] ]

sortAsc :: [Double] -> [Double]
sortAsc [] = []
sortAsc (p:rs) = sortAsc [x | x <- rs, x <= p]
              ++ [p]
              ++ sortAsc [x | x <- rs, x > p]

main :: IO ()
main = do
  putStrLn "============================================================"
  putStrLn " AnalysisReport vs ReportBuilder Comparison Demo"
  putStrLn "============================================================"
  putStrLn ""

  -- データロード
  Right dfLM   <- loadAuto "data/regression/test_lm.csv"
  Right dfPois <- loadAuto "data/regression/test_poisson.csv"

  putStrLn "Loaded:"
  putStrLn $ "  data/regression/test_lm.csv      ("
             ++ show (numRows dfLM) ++ " rows)"
  putStrLn $ "  data/regression/test_poisson.csv ("
             ++ show (numRows dfPois) ++ " rows)"
  putStrLn ""

  doLMDemo  dfLM
  doGLMDemo dfPois
  doGLMMDemo
  doGPDemo  dfLM
  doHBMDemo dfLM

  putStrLn ""
  putStrLn "============================================================"
  putStrLn " All 10 reports written to trash/."
  putStrLn " Open in a browser:"
  putStrLn "   trash/cmp_lm_AR.html      vs trash/cmp_lm_RB.html"
  putStrLn "   trash/cmp_glm_AR.html     vs trash/cmp_glm_RB.html"
  putStrLn "   trash/cmp_glmm_AR.html    vs trash/cmp_glmm_RB.html"
  putStrLn "   trash/cmp_gp_AR.html      vs trash/cmp_gp_RB.html"
  putStrLn "   trash/cmp_hbm_AR.html     vs trash/cmp_hbm_RB.html"
  putStrLn "============================================================"

-- ---------------------------------------------------------------------------
-- LM
-- ---------------------------------------------------------------------------

doLMDemo :: DataFrame -> IO ()
doLMDemo df = do
  putStrLn "--- LM ---"
  case (getNumeric "x" df, getNumeric "y" df) of
    (Just xVec, Just yVec) -> do
      writeARLM df
      writeRBLM df xVec yVec
    _ -> putStrLn "  (LM data not loaded)"

writeARLM :: DataFrame -> IO ()
writeARLM df = do
  case LM.fitPolyWithSmooth (Core.CI 0.95) 100 df "x" "y" of
    Just (fit, sf) -> do
      let smoothData = Just ("x", AR.SmoothData
                              { AR.sdXs    = LM.sfX sf
                              , AR.sdYs    = LM.sfFit sf
                              , AR.sdLower = LM.sfLower sf
                              , AR.sdUpper = LM.sfUpper sf
                              , AR.sdHasBand = LM.sfHasBand sf })
          summary = AR.mkFitSummary GLM.Gaussian GLM.Identity [("x", 1)]
                                    smoothData fit
          rcfg = AR.AnalysisReportConfig "LM (AnalysisReport)"
      AR.writeAnalysisReport "trash/cmp_lm_AR.html" rcfg df ["x"] "y"
        (AR.RegFit summary) []
      putStrLn "  AR: trash/cmp_lm_AR.html"
    Nothing -> putStrLn "  AR: fit failed"

writeRBLM :: DataFrame -> V.Vector Double -> V.Vector Double -> IO ()
writeRBLM df xVec yVec = do
  appendixSec <- RB.secAppendixFromMd "Appendix: model principle"
                   "docs/principles/lm.ja.md"
  case LM.fitPolyWithSmooth (Core.CI 0.95) 100 df "x" "y" of
    Just (fit, sf) -> do
      let beta   = coeffList fit
          coeffs = zip ["β₀ (intercept)", "β₁ (x)"] beta
          fitted = fittedList fit
          resid  = LA.toList (residualsV fit)
          xs = V.toList xVec
          ys = V.toList yVec
          smooth = RB.SmoothCurve (LM.sfX sf) (LM.sfFit sf)
                                  (LM.sfLower sf) (LM.sfUpper sf)
          xMinO = V.minimum xVec
          xMaxO = V.maximum xVec
          ext   = (xMaxO - xMinO) * 0.5
          sd_   = sqrt (sum [ r ^ (2::Int) | r <- resid ]
                        / fromIntegral (max 1 (length resid - 2)))
          im = RB.InteractiveModel
                 { RB.imXCols     = ["x"]
                 , RB.imYCol      = "y"
                 , RB.imXValues   = [[x] | x <- xs]
                 , RB.imYValues   = ys
                 , RB.imIntercept = head beta
                 , RB.imBetas     = drop 1 beta
                 , RB.imLink      = "identity"
                 , RB.imSlider    = [(xMinO - ext, (xMinO + xMaxO)/2, xMaxO + ext)]
                 , RB.imCISigma   = Just sd_
                 }
          cfg   = RB.defaultReportConfig "LM (ReportBuilder)"
          sections =
            [ RB.secDataOverview df ["x"] "y"
            , RB.secModelOverview "線形回帰 (LM)"
                "$y = \\beta_0 + \\beta_1 x + \\varepsilon$, $\\varepsilon \\sim \\text{Normal}(0, \\sigma^2)$ (Gaussian, identity link)"
                Nothing
            , RB.secCollapsible "回帰結果 (係数 + 散布 + 残差)" False
                [ RB.secCoefficients coeffs (Just ("R²", rSquared1 fit))
                , RB.secKeyValue "あてはめの要約"
                    [ ("R²",     T.pack (printf "%.4f" (rSquared1 fit)))
                    , ("方法",   "OLS (QR 分解)")
                    , ("σ_hat",  T.pack (printf "%.4f" sd_))
                    ]
                , RB.secFitScatter "x" "y" xs ys (Just smooth)
                , RB.secResiduals fitted resid
                ]
            , RB.secInteractiveMulti "対話的予測" im
            , appendixSec
            ]
      RB.renderReport "trash/cmp_lm_RB.html" cfg sections
      putStrLn "  RB: trash/cmp_lm_RB.html"
    Nothing -> putStrLn "  RB: fit failed"

-- ---------------------------------------------------------------------------
-- GLM (Poisson)
-- ---------------------------------------------------------------------------

doGLMDemo :: DataFrame -> IO ()
doGLMDemo df = do
  putStrLn "--- GLM (Poisson) ---"
  -- データの y 列名を判別
  let yCol = if columnInDF "count" df then "count"
             else if columnInDF "y" df then "y" else "count"
      xCol = "x"
  case (getNumeric xCol df, getNumeric yCol df) of
    (Just xVec, Just yVec) -> do
      writeARGLM df xCol yCol
      writeRBGLM df xVec yVec xCol yCol
    _ -> putStrLn $ "  (columns " ++ T.unpack xCol ++ "/"
                                  ++ T.unpack yCol ++ " not numeric)"

columnInDF :: T.Text -> DataFrame -> Bool
columnInDF c df = c `elem` columnNames df

writeARGLM :: DataFrame -> T.Text -> T.Text -> IO ()
writeARGLM df xCol yCol = do
  case GLM.fitGLMWithSmooth GLM.Poisson GLM.Log [(xCol, 1)]
                              Core.NoBand 100 df yCol of
    Just (fit, mSmooth) -> do
      let sm = case mSmooth of
            Nothing -> Nothing
            Just sf -> Just (xCol, AR.SmoothData
                              { AR.sdXs = LM.sfX sf
                              , AR.sdYs = LM.sfFit sf
                              , AR.sdLower = LM.sfLower sf
                              , AR.sdUpper = LM.sfUpper sf
                              , AR.sdHasBand = LM.sfHasBand sf })
          summary = AR.mkFitSummary GLM.Poisson GLM.Log [(xCol, 1)] sm fit
          rcfg = AR.AnalysisReportConfig "GLM Poisson (AnalysisReport)"
      AR.writeAnalysisReport "trash/cmp_glm_AR.html" rcfg df [xCol] yCol
        (AR.RegFit summary) []
      putStrLn "  AR: trash/cmp_glm_AR.html"
    Nothing -> putStrLn "  AR: fit failed"

writeRBGLM :: DataFrame -> V.Vector Double -> V.Vector Double
           -> T.Text -> T.Text -> IO ()
writeRBGLM df xVec yVec xCol yCol = do
  appendixSec <- RB.secAppendixFromMd "Appendix: model principle"
                   "docs/principles/glm.ja.md"
  case GLM.fitGLMWithSmooth GLM.Poisson GLM.Log [(xCol, 1)]
                              Core.NoBand 100 df yCol of
    Just (fit, mSmooth) -> do
      let beta = coeffList fit
          coeffs = zip ["β₀ (intercept)", "β₁ (" <> T.unpack xCol <> ")"] beta
          fitted = fittedList fit
          resid  = LA.toList (residualsV fit)
          xs = V.toList xVec
          ys = V.toList yVec
          smooth = case mSmooth of
            Just sf -> RB.SmoothCurve (LM.sfX sf) (LM.sfFit sf)
                          (LM.sfLower sf) (LM.sfUpper sf)
            Nothing -> RB.SmoothCurve [] [] [] []
          xMinO = V.minimum xVec
          xMaxO = V.maximum xVec
          ext   = (xMaxO - xMinO) * 0.5
          im = RB.InteractiveModel
                 { RB.imXCols     = [xCol]
                 , RB.imYCol      = yCol
                 , RB.imXValues   = [[x] | x <- xs]
                 , RB.imYValues   = ys
                 , RB.imIntercept = head beta
                 , RB.imBetas     = drop 1 beta
                 , RB.imLink      = "log"
                 , RB.imSlider    = [(xMinO - ext, (xMinO + xMaxO)/2, xMaxO + ext)]
                 , RB.imCISigma   = Nothing
                 }
          cfg   = RB.defaultReportConfig "GLM Poisson (ReportBuilder)"
          sections =
            [ RB.secDataOverview df [xCol] yCol
            , RB.secModelOverview "GLM Poisson / log link"
                ("$\\log E[\\text{" <> yCol <> "}] = \\beta_0 + \\beta_1 \\text{" <> xCol
                 <> "}$, $\\text{" <> yCol <> "} \\sim \\text{Poisson}(\\lambda)$")
                Nothing
            , RB.secCollapsible "回帰結果 (係数 + 散布 + 残差)" False
                [ RB.secCoefficients
                    [(T.pack k, v) | (k, v) <- coeffs]
                    (Just ("McFadden R²", rSquared1 fit))
                , RB.secKeyValue "あてはめの要約"
                    [ ("ファミリ", "Poisson")
                    , ("リンク",   "log")
                    , ("方法",     "IRLS")
                    ]
                , RB.secFitScatter xCol yCol xs ys (Just smooth)
                , RB.secResiduals fitted resid
                ]
            , RB.secInteractiveMulti "対話的予測" im
            , appendixSec
            ]
      RB.renderReport "trash/cmp_glm_RB.html" cfg sections
      putStrLn "  RB: trash/cmp_glm_RB.html"
    Nothing -> putStrLn "  RB: fit failed"

-- ---------------------------------------------------------------------------
-- GLMM
-- ---------------------------------------------------------------------------

doGLMMDemo :: IO ()
doGLMMDemo = do
  putStrLn "--- GLMM (LME) ---"
  let xs = V.fromList [1,2,3,4, 1,2,3,4, 1,2,3,4 :: Double]
      ys = V.fromList [7.1,6.9,7.0,7.0, 5.0,4.9,5.1,5.0, 3.0,2.9,3.1,3.0]
      gs = V.fromList ["A","A","A","A","B","B","B","B","C","C","C","C"]
      df = mkDataFrame
             [ ("x",     NumericCol xs)
             , ("y",     NumericCol ys)
             , ("group", TextCol    gs) ]
  case GLMM.fitLMEDataFrame [("x", 1)] "group" "y" df of
    Just gr -> do
      writeARGLMM df gr
      writeRBGLMM df gr
    Nothing -> putStrLn "  GLMM fit failed"

writeARGLMM :: DataFrame -> GLMM.GLMMResult -> IO ()
writeARGLMM df gr = do
  let summary = AR.mkGLMMSummary GLM.Gaussian GLM.Identity [("x", 1)]
                                  "group" Nothing gr
      rcfg = AR.AnalysisReportConfig "LME (AnalysisReport)"
  AR.writeAnalysisReport "trash/cmp_glmm_AR.html" rcfg df ["x"] "y"
    (AR.MixFit summary) []
  putStrLn "  AR: trash/cmp_glmm_AR.html"

writeRBGLMM :: DataFrame -> GLMM.GLMMResult -> IO ()
writeRBGLMM df gr = do
  appendixSec <- RB.secAppendixFromMd "Appendix: model principle"
                   "docs/principles/glmm.ja.md"
  let fixedB = coeffList (GLMM.glmmFixed gr)
      coeffs = zip ["β₀ (intercept)", "β₁ (x)"] fixedB
      blups  = zip (V.toList (GLMM.glmmGroups gr))
                   (V.toList (GLMM.glmmBLUPs gr))
      fitted = fittedList (GLMM.glmmFixed gr)
      resid  = LA.toList (residualsV (GLMM.glmmFixed gr))
      cfg    = RB.defaultReportConfig "LME (ReportBuilder)"
      sections =
        [ RB.secDataOverview df ["x"] "y"
        , RB.secModelOverview "線形混合効果 (LME)"
            ("$y_{ij} = \\beta_0 + \\beta_1 x + u_j + \\varepsilon$, "
             <> "$u_j \\sim \\text{Normal}(0, \\sigma^2_u)$, "
             <> "$\\varepsilon \\sim \\text{Normal}(0, \\sigma^2)$")
            Nothing
        , RB.secCollapsible "回帰結果 (固定効果 + 分散成分 + BLUP)" False
            [ RB.secCoefficients coeffs
                (Just ("周辺 R²", rSquared1 (GLMM.glmmFixed gr)))
            , RB.secKeyValue "分散成分"
                [ ("σ²_u (グループ)",  T.pack (printf "%.4f" (GLMM.glmmRandVar gr)))
                , ("σ² (残差)",         T.pack (printf "%.4f" (GLMM.glmmResidVar gr)))
                , ("ICC", T.pack (printf "%.4f (%d%%)"
                                   (GLMM.glmmICC gr)
                                   (round (GLMM.glmmICC gr * 100) :: Int)))
                ]
            , RB.secTable "BLUP (グループ別ランダム切片)"
                ["グループ", "u_j"]
                [ [g, T.pack (printf "%+.4f" u)] | (g, u) <- blups ]
            , RB.secResiduals fitted resid
            ]
        , appendixSec
        ]
  RB.renderReport "trash/cmp_glmm_RB.html" cfg sections
  putStrLn "  RB: trash/cmp_glmm_RB.html"

-- ---------------------------------------------------------------------------
-- GP
-- ---------------------------------------------------------------------------

doGPDemo :: DataFrame -> IO ()
doGPDemo df = do
  putStrLn "--- GP (RBF) ---"
  case (getNumeric "x" df, getNumeric "y" df) of
    (Just xVec, Just yVec) -> do
      let xs = V.toList xVec
          ys = V.toList yVec
          p0 = GP.initParamsFromData xs ys
          paramsOpt = GP.optimizeGP GP.RBF xs ys p0
          model = GP.GPModel GP.RBF paramsOpt
          gridX = let lo = V.minimum xVec
                      hi = V.maximum xVec
                      ex = (hi - lo) * 0.5
                  in [ (lo - ex) + fromIntegral i * ((hi - lo) * 2) / 99
                     | i <- [0..99::Int] ]   -- ±50% 外挿対応
          res = GP.fitGP model xs ys gridX
      writeARGP df xs ys res model paramsOpt
      writeRBGP df xs ys gridX res paramsOpt
    _ -> putStrLn "  (GP data not loaded)"

writeARGP :: DataFrame -> [Double] -> [Double]
          -> GP.GPResult -> GP.GPModel -> GP.GPParams -> IO ()
writeARGP df xs ys res model params = do
  let pd = GP.gpPredData model xs ys
      kfit = AR.GPKernelFit
              { AR.gkLabel    = "RBF"
              , AR.gkKernel   = GP.RBF
              , AR.gkParams   = params
              , AR.gkResult   = res
              , AR.gkLML      = GP.logMarginalLikelihood xs ys GP.RBF params
              , AR.gkPredData = pd
              }
      gfSummary = AR.GPFitSummary
                    { AR.gfKernelFits = [kfit]
                    , AR.gfXCol       = "x"
                    , AR.gfYCol       = "y"
                    , AR.gfTrainXs    = xs
                    , AR.gfTrainYs    = ys
                    }
      rcfg = AR.AnalysisReportConfig "GP RBF (AnalysisReport)"
  AR.writeAnalysisReport "trash/cmp_gp_AR.html" rcfg df ["x"] "y"
    (AR.GPFit gfSummary) []
  putStrLn "  AR: trash/cmp_gp_AR.html"

writeRBGP :: DataFrame -> [Double] -> [Double] -> [Double]
          -> GP.GPResult -> GP.GPParams -> IO ()
writeRBGP df xs ys gridX res params = do
  appendixSec <- RB.secAppendixFromMd "Appendix: model principle"
                   "docs/principles/gp.ja.md"
  let smooth = RB.SmoothCurve gridX (GP.gpMean res)
                              (GP.gpLower res) (GP.gpUpper res)
      xVec = V.fromList xs
      xMinO = V.minimum xVec
      xMaxO = V.maximum xVec
      ext   = (xMaxO - xMinO) * 0.3
      cfg   = RB.defaultReportConfig "GP RBF (ReportBuilder)"
      sections =
        [ RB.secDataOverview df ["x"] "y"
        , RB.secModelOverview "ガウス過程 (RBF カーネル)"
            ("$f \\sim \\text{GP}(0, k_{\\text{RBF}}(x, x'))$, "
             <> "$k(x, x') = \\sigma_f^2 \\exp\\!\\left(-(x-x')^2 / (2\\ell^2)\\right)$")
            Nothing
        , RB.secCollapsible "回帰結果 (ハイパラ + 散布)" False
            [ RB.secKeyValue "最適化されたハイパラメータ"
                [ ("ℓ (長さスケール)", T.pack (printf "%.4f" (GP.gpLengthScale params)))
                , ("σ_f² (信号分散)",T.pack (printf "%.4f" (GP.gpSignalVar params)))
                , ("σ_n² (ノイズ分散)", T.pack (printf "%.4f" (GP.gpNoiseVar params)))
                , ("LML", T.pack (printf "%.4f"
                                  (GP.logMarginalLikelihood xs ys GP.RBF params)))
                ]
            , RB.secFitScatter "x" "y" xs ys (Just smooth)
            ]
        , RB.secInteractiveLM "対話的予測" "x" "y"
            xs ys smooth (xMinO - ext, xMaxO + ext)
        , appendixSec
        ]
  RB.renderReport "trash/cmp_gp_RB.html" cfg sections
  putStrLn "  RB: trash/cmp_gp_RB.html"

-- ---------------------------------------------------------------------------
-- HBM (Bayesian linear regression via NUTS)
-- ---------------------------------------------------------------------------

hbmModel :: [Double] -> [Double] -> HBM.ModelP ()
hbmModel xs ys = do
  a <- HBM.sample "alpha" (HBM.Normal 0 10)
  b <- HBM.sample "beta"  (HBM.Normal 0 10)
  s <- HBM.sample "sigma" (HBM.Exponential 1)
  mapM_ (\(x, y) -> HBM.observe "y" (HBM.Normal (a + b * realToFrac x) s) [y])
        (zip xs ys)

doHBMDemo :: DataFrame -> IO ()
doHBMDemo df = do
  putStrLn "--- HBM (Bayesian LM via NUTS) ---"
  case (getNumeric "x" df, getNumeric "y" df) of
    (Just xVec, Just yVec) -> do
      let xs = V.toList xVec
          ys = V.toList yVec
      gen <- createSystemRandom
      chain <- NUTS.nuts (hbmModel xs ys)
        (NUTS.defaultNUTSConfig { NUTS.nutsIterations = 1000
                                 , NUTS.nutsBurnIn = 200
                                 , NUTS.nutsStepSize = 0.05 })
        (Map.fromList [("alpha", 0.0), ("beta", 0.0), ("sigma", 1.0)])
        gen
      writeARHBM df xs ys chain
      writeRBHBM df xs ys chain
    _ -> putStrLn "  (HBM data not loaded)"

makeHBMSmoothAR :: [Double] -> MCMCcore.Chain -> AR.SmoothData
makeHBMSmoothAR xs chain =
  let alphas = MCMCcore.chainVals "alpha" chain
      betas  = MCMCcore.chainVals "beta"  chain
      xMin   = minimum xs
      xMax   = maximum xs
      ext    = (xMax - xMin) * 0.5    -- 外挿用に ±50% 拡張
      gMin   = xMin - ext
      gMax   = xMax + ext
      grid   = [ gMin + i * (gMax - gMin) / 99 | i <- [0..99] ]
      qsAt p s =
        let n = length s
        in s !! min (n-1) (max 0 (floor (p * fromIntegral n) :: Int))
      atX x =
        let s = sortAsc (zipWith (\a b -> a + b * x) alphas betas)
        in (qsAt 0.5 s, qsAt 0.025 s, qsAt 0.975 s)
      preds = [ atX x | x <- grid ]
      (mid, lo, hi) = unzip3 preds
  in AR.SmoothData grid mid lo hi True

writeARHBM :: DataFrame -> [Double] -> [Double] -> MCMCcore.Chain -> IO ()
writeARHBM df xs ys chain = do
  let aMean = maybe 0 id (MCMCcore.posteriorMean "alpha" chain)
      bMean = maybe 0 id (MCMCcore.posteriorMean "beta"  chain)
      fitted = [aMean + bMean * x | x <- xs]
      resid  = zipWith (-) ys fitted
      yBar   = sum ys / fromIntegral (length ys)
      tss    = sum [(y - yBar) ^ (2 :: Int) | y <- ys]
      rss    = sum [r ^ (2 :: Int) | r <- resid]
      r2     = if tss < 1e-12 then 0 else 1 - rss / tss
      smoothAR = makeHBMSmoothAR xs chain
      fs = AR.FitSummary
             { AR.fsModelType   = "HBM (NUTS)"
             , AR.fsFormula     = "y ~ α + β·x"
             , AR.fsCoeffs      = [("α", aMean), ("β", bMean)]
             , AR.fsR2          = r2
             , AR.fsR2Label     = "R²"
             , AR.fsFitted      = fitted
             , AR.fsResiduals   = resid
             , AR.fsLinkName    = "Normal (identity)"
             , AR.fsXColDegs    = [("x", 1)]
             , AR.fsSmoothData  = Just ("x", smoothAR)
             , AR.fsModelSelect = Nothing
             }
      hs = AR.HBMRegSummary
             { AR.hbmsFit         = fs
             , AR.hbmsModelGraph  = HBM.buildModelGraph (hbmModel xs ys)
             , AR.hbmsChain       = chain
             , AR.hbmsParams      = ["alpha", "beta", "sigma"]
             , AR.hbmsPosteriorRows = mkPosteriorRows chain
             }
      rcfg = AR.AnalysisReportConfig "HBM (AnalysisReport)"
  AR.writeAnalysisReport "trash/cmp_hbm_AR.html" rcfg df ["x"] "y"
    (AR.HBMFit hs) []
  putStrLn "  AR: trash/cmp_hbm_AR.html"

mkPosteriorRows :: MCMCcore.Chain
                -> [(T.Text, Double, Double, Double, Double)]
mkPosteriorRows chain =
  [ (p,
     maybe 0 id (MCMCcore.posteriorMean p chain),
     maybe 0 id (MCMCcore.posteriorSD p chain),
     maybe 0 id (MCMCcore.posteriorQuantile 0.025 p chain),
     maybe 0 id (MCMCcore.posteriorQuantile 0.975 p chain))
  | p <- ["alpha", "beta", "sigma"] ]

writeRBHBM :: DataFrame -> [Double] -> [Double] -> MCMCcore.Chain -> IO ()
writeRBHBM df xs ys chain = do
  appendixSec <- RB.secAppendixFromMd "Appendix: model principle"
                   "docs/principles/hbm.ja.md"
  let aMean = maybe 0 id (MCMCcore.posteriorMean "alpha" chain)
      bMean = maybe 0 id (MCMCcore.posteriorMean "beta"  chain)
      sMean = maybe 0 id (MCMCcore.posteriorMean "sigma" chain)
      smoothAR = makeHBMSmoothAR xs chain
      smoothRB = RB.SmoothCurve (AR.sdXs smoothAR) (AR.sdYs smoothAR)
                                (AR.sdLower smoothAR) (AR.sdUpper smoothAR)
      params = ["alpha", "beta", "sigma"]
      summaryRows =
        [ (p,
           maybe 0 id (MCMCcore.posteriorMean p chain),
           maybe 0 id (MCMCcore.posteriorSD p chain),
           maybe 0 id (MCMCcore.posteriorQuantile 0.025 p chain),
           maybe 0 id (MCMCcore.posteriorQuantile 0.975 p chain),
           StatMCMC.ess (MCMCcore.chainVals p chain),
           Nothing :: Maybe Double)
        | p <- params ]
      cfg = RB.defaultReportConfig "HBM (ReportBuilder)"
      xVec = V.fromList xs
      xMinO = V.minimum xVec
      xMaxO = V.maximum xVec
      ext   = (xMaxO - xMinO) * 0.5
      mgDag = VMG.buildMermaid (HBM.buildModelGraph (hbmModel xs ys))
      sections =
        [ RB.secDataOverview df ["x"] "y"
        , RB.secModelOverview "Bayesian Linear Regression (HBM, NUTS)"
            ("$y_i \\sim \\text{Normal}(\\alpha + \\beta x_i, \\sigma)$, "
             <> "$\\alpha, \\beta \\sim \\text{Normal}(0, 10)$, "
             <> "$\\sigma \\sim \\text{Exponential}(1)$")
            (Just mgDag)
        , RB.secCollapsible "回帰結果 (係数 + 事後要約 + MCMC 診断)" False
            [ RB.secCoefficients
                [ ("α (posterior mean)", aMean)
                , ("β (posterior mean)", bMean)
                , ("σ (posterior mean)", sMean)
                ]
                Nothing
            , RB.secPosteriorSummary "事後要約" summaryRows
            , RB.secFitScatter "x" "y" xs ys (Just smoothRB)
            , RB.secMCMCDiagnostics "MCMC 診断 (KDE + トレース)"
                params chain
            , RB.secMCMCAutocorr "自己相関" 40 params chain
            , RB.secMCMCPair "ペア散布図 (α, β)" "alpha" "beta" chain
            ]
        , RB.secInteractiveLM "対話的予測 (事後中央値)"
            "x" "y" xs ys smoothRB (xMinO - ext, xMaxO + ext)
        , appendixSec
        ]
  RB.renderReport "trash/cmp_hbm_RB.html" cfg sections
  putStrLn "  RB: trash/cmp_hbm_RB.html"
