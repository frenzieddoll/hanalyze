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

import qualified DataFrame                    as DX
import qualified DataFrame.Internal.DataFrame as DXD
import DataIO.Convert      (getDoubleVec, getTextVec)
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
import qualified Viz.ReportInstances as RI
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
             ++ show ((fst (DX.dimensions dfLM))) ++ " rows)"
  putStrLn $ "  data/regression/test_poisson.csv ("
             ++ show ((fst (DX.dimensions dfPois))) ++ " rows)"
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

doLMDemo :: DXD.DataFrame -> IO ()
doLMDemo df = do
  putStrLn "--- LM ---"
  case (getDoubleVec "x" df, getDoubleVec "y" df) of
    (Just xVec, Just yVec) -> do
      writeARLM df
      writeRBLM df xVec yVec
    _ -> putStrLn "  (LM data not loaded)"

writeARLM :: DXD.DataFrame -> IO ()
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

writeRBLM :: DXD.DataFrame -> V.Vector Double -> V.Vector Double -> IO ()
writeRBLM df _xVec _yVec = do
  appendixSec <- RB.secAppendixFromMd "付録: モデルの原理"
                   "docs/principles/lm.ja.md"
  case LM.fitPolyWithSmooth (Core.CI 0.95) 100 df "x" "y" of
    Just (fit, sf) -> do
      let cfg      = RB.defaultReportConfig "LM (ReportBuilder)"
          report   = RI.LMReport fit (Just sf)
          sections = RB.toReport cfg df ["x"] "y" report ++ [appendixSec]
      RB.renderReport "trash/cmp_lm_RB.html" cfg sections
      putStrLn "  RB: trash/cmp_lm_RB.html (Reportable LMReport instance)"
    Nothing -> putStrLn "  RB: fit failed"

-- ---------------------------------------------------------------------------
-- GLM (Poisson)
-- ---------------------------------------------------------------------------

doGLMDemo :: DXD.DataFrame -> IO ()
doGLMDemo df = do
  putStrLn "--- GLM (Poisson) ---"
  -- データの y 列名を判別
  let yCol = if columnInDF "count" df then "count"
             else if columnInDF "y" df then "y" else "count"
      xCol = "x"
  case (getDoubleVec xCol df, getDoubleVec yCol df) of
    (Just xVec, Just yVec) -> do
      writeARGLM df xCol yCol
      writeRBGLM df xVec yVec xCol yCol
    _ -> putStrLn $ "  (columns " ++ T.unpack xCol ++ "/"
                                  ++ T.unpack yCol ++ " not numeric)"

columnInDF :: T.Text -> DXD.DataFrame -> Bool
columnInDF c df = c `elem` DX.columnNames df

writeARGLM :: DXD.DataFrame -> T.Text -> T.Text -> IO ()
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

writeRBGLM :: DXD.DataFrame -> V.Vector Double -> V.Vector Double
           -> T.Text -> T.Text -> IO ()
writeRBGLM df _xVec _yVec xCol yCol = do
  appendixSec <- RB.secAppendixFromMd "付録: モデルの原理"
                   "docs/principles/glm.ja.md"
  case GLM.fitGLMWithSmooth GLM.Poisson GLM.Log [(xCol, 1)]
                              Core.NoBand 100 df yCol of
    Just (fit, mSmooth) -> do
      let cfg      = RB.defaultReportConfig "GLM Poisson (ReportBuilder)"
          report   = RI.GLMReport fit GLM.Poisson GLM.Log mSmooth
          sections = RB.toReport cfg df [xCol] yCol report ++ [appendixSec]
      RB.renderReport "trash/cmp_glm_RB.html" cfg sections
      putStrLn "  RB: trash/cmp_glm_RB.html (Reportable GLMReport instance)"
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
      df = DX.insertColumn "x"     (DX.fromList (V.toList xs :: [Double]))
         $ DX.insertColumn "y"     (DX.fromList (V.toList ys :: [Double]))
         $ DX.insertColumn "group" (DX.fromList (V.toList gs :: [T.Text]))
         $ DX.empty
  case GLMM.fitLMEDataFrame [("x", 1)] "group" "y" df of
    Just gr -> do
      writeARGLMM df gr
      writeRBGLMM df gr
    Nothing -> putStrLn "  GLMM fit failed"

writeARGLMM :: DXD.DataFrame -> GLMM.GLMMResult -> IO ()
writeARGLMM df gr = do
  let summary = AR.mkGLMMSummary GLM.Gaussian GLM.Identity [("x", 1)]
                                  "group" Nothing gr
      rcfg = AR.AnalysisReportConfig "LME (AnalysisReport)"
  AR.writeAnalysisReport "trash/cmp_glmm_AR.html" rcfg df ["x"] "y"
    (AR.MixFit summary) []
  putStrLn "  AR: trash/cmp_glmm_AR.html"

writeRBGLMM :: DXD.DataFrame -> GLMM.GLMMResult -> IO ()
writeRBGLMM df gr = do
  appendixSec <- RB.secAppendixFromMd "付録: モデルの原理"
                   "docs/principles/glmm.ja.md"
  let cfg      = RB.defaultReportConfig "LME (ReportBuilder)"
      rep      = RI.GLMMReport gr GLM.Gaussian GLM.Identity "group"
      sections = RB.toReport cfg df ["x"] "y" rep ++ [appendixSec]
  RB.renderReport "trash/cmp_glmm_RB.html" cfg sections
  putStrLn "  RB: trash/cmp_glmm_RB.html (Reportable GLMMReport instance)"

-- ---------------------------------------------------------------------------
-- GP
-- ---------------------------------------------------------------------------

doGPDemo :: DXD.DataFrame -> IO ()
doGPDemo df = do
  putStrLn "--- GP (RBF) ---"
  case (getDoubleVec "x" df, getDoubleVec "y" df) of
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

writeARGP :: DXD.DataFrame -> [Double] -> [Double]
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

writeRBGP :: DXD.DataFrame -> [Double] -> [Double] -> [Double]
          -> GP.GPResult -> GP.GPParams -> IO ()
writeRBGP df xs ys gridX res params = do
  appendixSec <- RB.secAppendixFromMd "付録: モデルの原理"
                   "docs/principles/gp.ja.md"
  let cfg      = RB.defaultReportConfig "GP RBF (ReportBuilder)"
      lml      = GP.logMarginalLikelihood xs ys GP.RBF params
      rep      = RI.GPReport GP.RBF params res gridX xs ys lml
      sections = RB.toReport cfg df ["x"] "y" rep ++ [appendixSec]
  RB.renderReport "trash/cmp_gp_RB.html" cfg sections
  putStrLn "  RB: trash/cmp_gp_RB.html (Reportable GPReport instance)"

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

doHBMDemo :: DXD.DataFrame -> IO ()
doHBMDemo df = do
  putStrLn "--- HBM (Bayesian LM via NUTS) ---"
  case (getDoubleVec "x" df, getDoubleVec "y" df) of
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

writeARHBM :: DXD.DataFrame -> [Double] -> [Double] -> MCMCcore.Chain -> IO ()
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

writeRBHBM :: DXD.DataFrame -> [Double] -> [Double] -> MCMCcore.Chain -> IO ()
writeRBHBM df xs ys chain = do
  appendixSec <- RB.secAppendixFromMd "付録: モデルの原理"
                   "docs/principles/hbm.ja.md"
  let cfg   = RB.defaultReportConfig "HBM (ReportBuilder)"
      mgDag = VMG.buildMermaid (HBM.buildModelGraph (hbmModel xs ys))
      rep   = RI.HBMLinearReport
                { RI.hbmrChain     = chain
                , RI.hbmrXs        = xs
                , RI.hbmrYs        = ys
                , RI.hbmrAlphaName = "alpha"
                , RI.hbmrBetaName  = "beta"
                , RI.hbmrSigmaName = "sigma"
                , RI.hbmrGraph     = Just mgDag
                }
      sections = RB.toReport cfg df ["x"] "y" rep ++ [appendixSec]
  RB.renderReport "trash/cmp_hbm_RB.html" cfg sections
  putStrLn "  RB: trash/cmp_hbm_RB.html (Reportable HBMLinearReport instance)"
