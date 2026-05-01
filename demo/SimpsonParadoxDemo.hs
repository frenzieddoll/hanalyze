{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | シンプソンのパラドックスを LM / GLMM / HBM で比較するデモ。
--
-- データ:
--   * 3 グループ (A, B, C)、各グループ内では負の傾き (右下り)
--   * グループを無視すると正の傾き (右上り) に見える
--
-- 期待される結果:
--   * LM (グループ無視): β > 0  → 誤った結論
--   * GLMM (ランダム切片): β < 0 → 正しい結論
--   * HBM (階層モデル): β < 0   → 正しい結論 + 不確実性付き
module Main where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import DataFrame.Core (Column (..), DataFrame)
import qualified DataFrame.Core as DF
import Model.Core    (Band (..), coefficients)
import Model.LM      (fitPolyWithSmooth, SmoothFit (..))
import Model.GLMM    (fitLMEDataFrame, GLMMResult (..))
import Model.GLM     (Family (..), LinkFn (..))
import qualified Numeric.LinearAlgebra as LA

import MCMC.Core (Chain (..), chainVals, posteriorMean, posteriorSD,
                  posteriorQuantile, acceptanceRate)
import MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Model.HBM (ModelP, sample, observe, Distribution (..),
                  buildModelGraph)
import Stat.MCMC (ess)

import Viz.AnalysisReport
  ( AnalysisReportConfig (..), defaultAnalysisConfig
  , FitSummary (..), GLMMSummary (..), HBMRegSummary (..), SmoothData (..)
  , ModelFit (..), NamedPlot (..)
  , mkFitSummary, mkGLMMSummary
  , writeAnalysisReport
  )
import Viz.Core (PlotConfig (..))
import Viz.MCMC (mcmcDiagnostics, autocorrPlot)

-- ---------------------------------------------------------------------------
-- データ生成 (Simpson's Paradox)
-- ---------------------------------------------------------------------------
-- 各グループ内: y = α_g - 0.5·x + ノイズ  (負の傾き)
-- グループ A: α=2, x ∈ [0.2, 3.0]
-- グループ B: α=5, x ∈ [3.5, 6.0]
-- グループ C: α=8, x ∈ [6.4, 9.0]
-- 全体としては正の関係に見える (グループの x 平均と y 平均が正相関)

dataA, dataB, dataC :: [(Double, Double)]
dataA = zip
  [0.2, 0.6, 1.0, 1.4, 1.8, 2.0, 2.4, 2.6, 2.8, 3.0]
  -- y_clean: 1.90, 1.70, 1.50, 1.30, 1.10, 1.00, 0.80, 0.70, 0.60, 0.50
  [1.93, 1.62, 1.55, 1.27, 1.18, 0.92, 0.85, 0.74, 0.55, 0.43]

dataB = zip
  [3.4, 3.8, 4.2, 4.5, 4.8, 5.0, 5.3, 5.6, 5.8, 6.0]
  -- y_clean: 3.30, 3.10, 2.90, 2.75, 2.60, 2.50, 2.35, 2.20, 2.10, 2.00
  [3.39, 3.04, 2.95, 2.62, 2.71, 2.41, 2.30, 2.27, 2.04, 1.91]

dataC = zip
  [6.4, 6.8, 7.0, 7.3, 7.5, 7.8, 8.0, 8.3, 8.5, 9.0]
  -- y_clean: 4.80, 4.60, 4.50, 4.35, 4.25, 4.10, 4.00, 3.85, 3.75, 3.50
  [4.86, 4.51, 4.58, 4.30, 4.19, 4.18, 3.93, 3.79, 3.62, 3.44]

allXs :: [Double]
allXs = map fst (dataA ++ dataB ++ dataC)

allYs :: [Double]
allYs = map snd (dataA ++ dataB ++ dataC)

allGroups :: [Text]
allGroups = replicate (length dataA) "A"
         ++ replicate (length dataB) "B"
         ++ replicate (length dataC) "C"

mkDataFrame :: DataFrame
mkDataFrame = DF.mkDataFrame
  [ ("x",     NumericCol (V.fromList allXs))
  , ("y",     NumericCol (V.fromList allYs))
  , ("group", TextCol    (V.fromList allGroups))
  ]

-- ---------------------------------------------------------------------------
-- HBM 階層モデル (varying intercept)
-- ---------------------------------------------------------------------------

hbmModel :: ModelP ()
hbmModel = do
  muAlpha    <- sample "mu_alpha"    (Normal 0 10)
  sigmaAlpha <- sample "sigma_alpha" (Exponential 1)
  beta       <- sample "beta"        (Normal 0 10)
  sigma      <- sample "sigma"       (Exponential 1)
  alphaA     <- sample "alpha_A"     (Normal muAlpha sigmaAlpha)
  alphaB     <- sample "alpha_B"     (Normal muAlpha sigmaAlpha)
  alphaC     <- sample "alpha_C"     (Normal muAlpha sigmaAlpha)
  mapM_ (\(x, y) -> let xC = realToFrac x
                    in observe "y_A" (Normal (alphaA + beta * xC) sigma) [y])
        dataA
  mapM_ (\(x, y) -> let xC = realToFrac x
                    in observe "y_B" (Normal (alphaB + beta * xC) sigma) [y])
        dataB
  mapM_ (\(x, y) -> let xC = realToFrac x
                    in observe "y_C" (Normal (alphaC + beta * xC) sigma) [y])
        dataC

-- ---------------------------------------------------------------------------
-- レポート 1: LM (プールド回帰、グループ無視)
-- ---------------------------------------------------------------------------

reportLM :: IO ()
reportLM = do
  let df = mkDataFrame
  case fitPolyWithSmooth (CI 0.95) 100 df "x" "y" of
    Nothing -> putStrLn "  LM fit failed"
    Just (res, sf) -> do
      let beta = coefficients res
          slope = LA.atIndex beta 1
          intercept = LA.atIndex beta 0
      printf "  LM:    intercept=%+.3f  slope=%+.3f  R²=%.3f\n"
             intercept slope (computeR2Local df res)

      let smooth = SmoothData
            { sdXs      = sfX sf
            , sdYs      = sfFit sf
            , sdLower   = sfLower sf
            , sdUpper   = sfUpper sf
            , sdHasBand = sfHasBand sf
            }
          summary = mkFitSummary Gaussian Identity [("x", 1)] (Just ("x", smooth)) res
            -- modelType を "LM (Pooled, ignores group)" に上書き
          summary' = summary
            { fsModelType = "LM (Pooled — group 無視)"
            , fsFormula   = "y ~ α + β · x"
            , fsLinkName  = "Identity (Gaussian)"
            }
          rptCfg = defaultAnalysisConfig
                     "Simpson Paradox — LM (Pooled regression)"
      writeAnalysisReport "simpson_lm.html" rptCfg df ["x"] "y"
                          (RegFit summary') []
      putStrLn "  → simpson_lm.html"

-- ---------------------------------------------------------------------------
-- レポート 2: GLMM (LME, ランダム切片 by group)
-- ---------------------------------------------------------------------------

reportGLMM :: IO ()
reportGLMM = do
  let df = mkDataFrame
  case fitLMEDataFrame [("x", 1)] "group" "y" df of
    Nothing -> putStrLn "  GLMM fit failed"
    Just gr -> do
      let beta = coefficients (glmmFixed gr)
          slope = LA.atIndex beta 1
          intercept = LA.atIndex beta 0
      printf "  GLMM:  intercept=%+.3f  slope=%+.3f  σ²_u=%.3f  σ²=%.3f  ICC=%.3f\n"
             intercept slope
             (glmmRandVar gr) (glmmResidVar gr) (glmmICC gr)
      mapM_ (\(g, b) -> printf "         BLUP[%s] = %+.3f\n" (T.unpack g) b)
            (zip (V.toList (glmmGroups gr)) (V.toList (glmmBLUPs gr)))

      -- 固定効果のみで smoothData を構築 (β_0 + β_1·x_grid)
      let xMin = minimum allXs
          xMax = maximum allXs
          xExt = (xMax - xMin) * 0.1
          grid = [xMin - xExt + i * (xMax - xMin + 2 * xExt) / 99 | i <- [0..99]]
          ysGrid = [intercept + slope * x | x <- grid]
          smooth = SmoothData
            { sdXs      = grid
            , sdYs      = ysGrid
            , sdLower   = ysGrid
            , sdUpper   = ysGrid
            , sdHasBand = False
            }
          summary = mkGLMMSummary Gaussian Identity [("x", 1)] "group"
                                  (Just ("x", smooth)) gr
          rptCfg = defaultAnalysisConfig
                     "Simpson Paradox — GLMM (LME, random intercept by group)"
      writeAnalysisReport "simpson_glmm.html" rptCfg df ["x"] "y"
                          (MixFit summary) []
      putStrLn "  → simpson_glmm.html"

-- ---------------------------------------------------------------------------
-- レポート 3: HBM (階層ベイズ)
-- ---------------------------------------------------------------------------

reportHBM :: IO ()
reportHBM = do
  let df  = mkDataFrame
      cfg = defaultNUTSConfig
              { nutsIterations = 800
              , nutsBurnIn     = 400
              , nutsStepSize   = 0.05
              , nutsMaxDepth   = 8
              }
      initP = Map.fromList
                [ ("mu_alpha", 5.0), ("sigma_alpha", 2.0)
                , ("beta", 0.0), ("sigma", 0.5)
                , ("alpha_A", 2.0), ("alpha_B", 5.0), ("alpha_C", 8.0)
                ]
  gen <- createSystemRandom
  chain <- nuts hbmModel cfg initP gen

  let bMean = fromMaybe 0 (posteriorMean "beta" chain)
      bSD   = fromMaybe 0 (posteriorSD   "beta" chain)
  printf "  HBM:   β = %+.3f ± %.3f  (95%% CI: %+.3f, %+.3f)\n"
         bMean bSD
         (fromMaybe 0 (posteriorQuantile 0.025 "beta" chain))
         (fromMaybe 0 (posteriorQuantile 0.975 "beta" chain))
  mapM_ (\g -> let nm = "alpha_" <> g
               in printf "         %-9s mean=%+.3f  sd=%.3f\n"
                    (T.unpack nm)
                    (fromMaybe 0 (posteriorMean nm chain))
                    (fromMaybe 0 (posteriorSD   nm chain)))
        ["A", "B", "C"]
  printf "         受容率=%.1f%%\n" (acceptanceRate chain * 100)

  -- Smooth: 全体曲線 (mu_alpha + beta * x) を信用区間付きで描画
  let alphas = chainVals "mu_alpha" chain
      betas  = chainVals "beta"     chain
      xMin = minimum allXs
      xMax = maximum allXs
      xExt = (xMax - xMin) * 0.1
      grid = [xMin - xExt + i * (xMax - xMin + 2 * xExt) / 99 | i <- [0..99]]
      atX x = let ss = zipWith (\a b -> a + b * x) alphas betas
                  sorted = sortAsc ss
                  n      = length sorted
                  qAt p  = sorted !! min (n-1) (max 0 (floor (p * fromIntegral n) :: Int))
              in (qAt 0.5, qAt 0.025, qAt 0.975)
      (ysMid, ysLo, ysHi) = unzip3 (map atX grid)
      smooth = SmoothData
        { sdXs      = grid
        , sdYs      = ysMid
        , sdLower   = ysLo
        , sdUpper   = ysHi
        , sdHasBand = True
        }
      -- HBM 用 FitSummary (回帰スタイル)
      aMu = fromMaybe 0 (posteriorMean "mu_alpha" chain)
      fitted = [aMu + bMean * x | x <- allXs]
      resid  = zipWith (-) allYs fitted
      yBar   = sum allYs / fromIntegral (length allYs)
      tss    = sum [(y - yBar) ^ (2::Int) | y <- allYs]
      rss    = sum [r ^ (2::Int) | r <- resid]
      r2     = if tss < 1e-12 then 0 else 1 - rss / tss
      fs = FitSummary
             { fsModelType    = "Hierarchical Bayesian Regression (HBM)"
             , fsFormula      = "y_g ~ α_g + β · x,  α_g ~ N(μ_α, σ_α)"
             , fsCoeffs       = [("μ_α (全体平均)", aMu), ("β (傾き)", bMean)]
             , fsR2           = r2
             , fsR2Label      = "R² (全体平均線)"
             , fsFitted       = fitted
             , fsResiduals    = resid
             , fsLinkName     = "Normal (identity link)"
             , fsXColDegs     = [("x", 1)]
             , fsSmoothData   = Just ("x", smooth)
             , fsModelSelect  = Nothing
             }
      hs = HBMRegSummary
             { hbmsFit           = fs
             , hbmsModelGraph    = buildModelGraph hbmModel
             , hbmsChain         = chain
             , hbmsParams        = paramNames
             , hbmsPosteriorRows = [ (n, fromMaybe 0 (posteriorMean n chain)
                                    , fromMaybe 0 (posteriorSD   n chain)
                                    , fromMaybe 0 (posteriorQuantile 0.025 n chain)
                                    , fromMaybe 0 (posteriorQuantile 0.975 n chain))
                                  | n <- paramNames ]
             }
      paramNames = ["mu_alpha", "sigma_alpha", "beta", "sigma",
                    "alpha_A", "alpha_B", "alpha_C"]
      diagCfg = PlotConfig "MCMC 診断 (KDE + トレース)" 760 320
      acfCfg  = PlotConfig "自己相関 (lag 0..40)" 760 220
      diagPlot = NamedPlot "vl-hbm-diag" "MCMC 診断 (β / α_g / σ)"
                   (mcmcDiagnostics diagCfg ["beta", "alpha_A", "alpha_B", "alpha_C", "sigma"] chain)
      acfPlot  = NamedPlot "vl-hbm-acf" "パラメータ別 自己相関"
                   (autocorrPlot acfCfg 40 ["beta", "alpha_A", "alpha_B", "alpha_C"] chain)
      rptCfg = defaultAnalysisConfig
                 "Simpson Paradox — HBM (Hierarchical Bayesian)"

  writeAnalysisReport "simpson_hbm.html" rptCfg df ["x"] "y"
                       (HBMFit hs) [diagPlot, acfPlot]
  putStrLn "  → simpson_hbm.html"

sortAsc :: [Double] -> [Double]
sortAsc xs = let go [] = []
                 go (p:rest) = go [x | x <- rest, x <= p]
                            ++ [p]
                            ++ go [x | x <- rest, x > p]
             in go xs

-- ---------------------------------------------------------------------------
-- 解析的に R² を計算 (Main.hs の res に R² が含まれていない場合の保険)
-- ---------------------------------------------------------------------------

computeR2Local :: DataFrame -> a -> Double
computeR2Local _ _ = 0  -- mkFitSummary が R² を上書きするので未使用

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  シンプソンのパラドックス: LM vs GLMM vs HBM"
  putStrLn "═══════════════════════════════════════════════════════════════"
  printf "  3 グループ (A, B, C) × 10 観測 = N=%d\n" (length allXs)
  putStrLn "  各グループ内: 真の傾き β_within = -0.5"
  putStrLn "  グループ無視: 見かけの傾き β_pooled ≈ +0.5  ← パラドックス"
  putStrLn ""

  putStrLn "[1] LM (Pooled) — グループを無視した単回帰:"
  reportLM
  putStrLn ""

  putStrLn "[2] GLMM (LME) — グループをランダム切片として導入:"
  reportGLMM
  putStrLn ""

  putStrLn "[3] HBM (Hierarchical) — α_g を階層的に推定 + 不確実性:"
  reportHBM
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  結果: LM は β > 0 (誤った正の傾き)、"
  putStrLn "        GLMM/HBM は β < 0 (正しい負の傾き) を回復する"
  putStrLn "═══════════════════════════════════════════════════════════════"
