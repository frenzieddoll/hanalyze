{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | HBM のランダム切片 vs ランダム切片+ランダム傾きの比較デモ。
--
-- データ:
--   3 グループ (A, B, C) で **傾きも異なる**
--   * Group A: α=2.0, β=-0.8   (急な右下り)
--   * Group B: α=5.0, β=-0.3   (緩やかな右下り)
--   * Group C: α=8.0, β=+0.2   (わずかに右上り)
--
-- モデル比較:
--   1. M1 (ランダム切片のみ): β を全グループで共有
--      → 単一の β に各グループの異なる傾きを"平均"してしまう
--   2. M2 (ランダム切片+ランダム傾き): β_g をグループごとに推定
--      → 各グループの真の傾きを正しく回復
--
-- WAIC/LOO で M2 が支持されることを示す。
module Main where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import qualified DataFrame                    as DX
import qualified DataFrame.Internal.DataFrame as DXD

import Hanalyze.MCMC.Core (Chain (..), chainVals, posteriorMean, posteriorSD,
                  posteriorQuantile, acceptanceRate)
import Hanalyze.MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Hanalyze.Model.HBM (ModelP, sample, observe, Distribution (..),
                  buildModelGraph, perObsLogLiks)
import Hanalyze.Stat.MCMC (ess)
import Hanalyze.Stat.ModelSelect (waic, loo, WAICResult (..), LOOResult (..))

import Hanalyze.Viz.AnalysisReport
  ( AnalysisReportConfig (..), defaultAnalysisConfig
  , FitSummary (..), HBMRegSummary (..), SmoothData (..)
  , ModelFit (..), NamedPlot (..), CompareEntry (..)
  , writeAnalysisReport, writeComparisonReport
  )
import Hanalyze.Viz.Core (PlotConfig (..))
import Hanalyze.Viz.MCMC (mcmcDiagnostics, autocorrPlot)

-- ---------------------------------------------------------------------------
-- データ生成: グループごとに異なる傾き
-- ---------------------------------------------------------------------------

-- Group A: α=2,  β=-0.8 (急な右下り)
dataA :: [(Double, Double)]
dataA = zip
  [0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0]
  -- y_clean: 1.6, 1.2, 0.8, 0.4, 0.0, -0.4, -0.8, -1.2, -1.6, -2.0
  [1.71, 1.05, 0.92, 0.31, 0.18, -0.51, -0.65, -1.13, -1.74, -1.85]

-- Group B: α=5,  β=-0.3 (緩やかな右下り)
dataB :: [(Double, Double)]
dataB = zip
  [0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0]
  -- y_clean: 4.85, 4.70, 4.55, 4.40, 4.25, 4.10, 3.95, 3.80, 3.65, 3.50
  [4.94, 4.59, 4.66, 4.32, 4.41, 3.96, 4.07, 3.82, 3.51, 3.65]

-- Group C: α=8,  β=+0.2 (わずかに右上り)
dataC :: [(Double, Double)]
dataC = zip
  [0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0]
  -- y_clean: 8.10, 8.20, 8.30, 8.40, 8.50, 8.60, 8.70, 8.80, 8.90, 9.00
  [8.16, 8.05, 8.43, 8.32, 8.61, 8.49, 8.78, 8.71, 9.04, 8.92]

allXs :: [Double]
allXs = map fst (dataA ++ dataB ++ dataC)

allYs :: [Double]
allYs = map snd (dataA ++ dataB ++ dataC)

allGroups :: [Text]
allGroups = replicate (length dataA) "A"
         ++ replicate (length dataB) "B"
         ++ replicate (length dataC) "C"

mkDataFrame :: DXD.DataFrame
mkDataFrame = DX.insertColumn "x"     (DX.fromList (allXs :: [Double]))
            $ DX.insertColumn "y"     (DX.fromList (allYs :: [Double]))
            $ DX.insertColumn "group" (DX.fromList (allGroups :: [T.Text]))
            $ DX.empty

-- ---------------------------------------------------------------------------
-- M1: ランダム切片のみ (β は全グループ共通)
-- ---------------------------------------------------------------------------

modelM1 :: ModelP ()
modelM1 = do
  muAlpha    <- sample "mu_alpha"    (Normal 0 10)
  sigmaAlpha <- sample "sigma_alpha" (Exponential 1)
  beta       <- sample "beta"        (Normal 0 10)
  sigma      <- sample "sigma"       (Exponential 1)
  alphaA <- sample "alpha_A" (Normal muAlpha sigmaAlpha)
  alphaB <- sample "alpha_B" (Normal muAlpha sigmaAlpha)
  alphaC <- sample "alpha_C" (Normal muAlpha sigmaAlpha)
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
-- M2: ランダム切片 + ランダム傾き (β_g もグループ別)
-- ---------------------------------------------------------------------------

modelM2 :: ModelP ()
modelM2 = do
  -- 切片の階層
  muAlpha    <- sample "mu_alpha"    (Normal 0 10)
  sigmaAlpha <- sample "sigma_alpha" (Exponential 1)
  -- 傾きの階層
  muBeta     <- sample "mu_beta"     (Normal 0 5)
  sigmaBeta  <- sample "sigma_beta"  (Exponential 1)
  -- 残差
  sigma      <- sample "sigma"       (Exponential 1)
  -- グループ別パラメータ
  alphaA <- sample "alpha_A" (Normal muAlpha sigmaAlpha)
  alphaB <- sample "alpha_B" (Normal muAlpha sigmaAlpha)
  alphaC <- sample "alpha_C" (Normal muAlpha sigmaAlpha)
  betaA  <- sample "beta_A"  (Normal muBeta  sigmaBeta)
  betaB  <- sample "beta_B"  (Normal muBeta  sigmaBeta)
  betaC  <- sample "beta_C"  (Normal muBeta  sigmaBeta)
  mapM_ (\(x, y) -> let xC = realToFrac x
                    in observe "y_A" (Normal (alphaA + betaA * xC) sigma) [y])
        dataA
  mapM_ (\(x, y) -> let xC = realToFrac x
                    in observe "y_B" (Normal (alphaB + betaB * xC) sigma) [y])
        dataB
  mapM_ (\(x, y) -> let xC = realToFrac x
                    in observe "y_C" (Normal (alphaC + betaC * xC) sigma) [y])
        dataC

-- ---------------------------------------------------------------------------
-- 共通: NUTS 実行 + WAIC/LOO + AnalysisReport 用 ModelFit 構築
-- ---------------------------------------------------------------------------

runHBM
  :: Text                    -- ^ モデルラベル ("M1" / "M2")
  -> Text                    -- ^ 出力 HTML ファイル名
  -> ModelP ()               -- ^ 推論対象モデル
  -> [Text]                  -- ^ 主要パラメータ名 (β または β_g など)
  -> Map.Map Text Double     -- ^ 初期値
  -> [Text]                  -- ^ 全潜在変数名 (事後分布表用)
  -> NUTSConfig
  -> IO (Maybe ModelFit)
runHBM label htmlPath m mainParams initP allParams cfg = do
  putStrLn $ "  [" ++ T.unpack label ++ "] NUTS サンプリング..."
  gen <- createSystemRandom
  chain <- nuts m cfg initP gen
  printf "    受容率=%.1f%%, サンプル数=%d\n"
         (acceptanceRate chain * 100 :: Double)
         (length (chainSamples chain))

  putStrLn $ "  [" ++ T.unpack label ++ "] 主要パラメータ事後:"
  mapM_ (\n ->
    printf "    %-10s mean=%+.3f  sd=%.3f  95%% CI=[%+.3f, %+.3f]\n"
      (T.unpack n)
      (fromMaybe 0 (posteriorMean n chain))
      (fromMaybe 0 (posteriorSD   n chain))
      (fromMaybe 0 (posteriorQuantile 0.025 n chain))
      (fromMaybe 0 (posteriorQuantile 0.975 n chain)))
    mainParams

  -- WAIC/LOO
  let llMat = [ perObsLogLiks m ps | ps <- chainSamples chain ]
      wRes  = waic llMat
      lRes  = loo  llMat
  printf "    WAIC=%.2f  LOO=%.2f  p_WAIC=%.2f\n"
         (waicValue wRes) (looValue lRes) (waicPwaic wRes)

  -- 全体平均線の事後予測 (μ_α + 平均β · x で構築。M2 では平均β = mu_beta)
  let alphas = chainVals "mu_alpha" chain
      -- M1 は "beta", M2 は "mu_beta" を使う
      betas  = case chainVals "mu_beta" chain of
                 [] -> chainVals "beta" chain
                 vs -> vs
      xMin = minimum allXs
      xMax = maximum allXs
      xExt = (xMax - xMin) * 0.1
      grid = [xMin - xExt + i * (xMax - xMin + 2 * xExt) / 99 | i <- [0..99]]
      atX x = let ss     = sortAsc (zipWith (\a b -> a + b * x) alphas betas)
                  n      = length ss
                  qAt p  = ss !! min (n-1) (max 0 (floor (p * fromIntegral n) :: Int))
              in (qAt 0.5, qAt 0.025, qAt 0.975)
      (ysMid, ysLo, ysHi) = unzip3 (map atX grid)
      smooth = SmoothData
        { sdXs = grid, sdYs = ysMid, sdLower = ysLo, sdUpper = ysHi
        , sdHasBand = True
        }

      bMean    = case posteriorMean "mu_beta" chain of
                   Just v  -> v
                   Nothing -> fromMaybe 0 (posteriorMean "beta" chain)
      aMu      = fromMaybe 0 (posteriorMean "mu_alpha" chain)
      fitted   = [aMu + bMean * x | x <- allXs]
      resid    = zipWith (-) allYs fitted
      yBar     = sum allYs / fromIntegral (length allYs)
      tss      = sum [(y - yBar) ^ (2::Int) | y <- allYs]
      rss      = sum [r ^ (2::Int) | r <- resid]
      r2       = if tss < 1e-12 then 0 else 1 - rss / tss

      modelLabelLong = case label of
        "M1" -> "HBM (Random intercept only)"
        "M2" -> "HBM (Random intercept + random slope)"
        _    -> "HBM"

      formula = case label of
        "M1" -> "y_g ~ α_g + β · x  (β 共通)"
        "M2" -> "y_g ~ α_g + β_g · x  (α_g, β_g 階層)"
        _    -> "y ~ α + β · x"

      fs = FitSummary
        { fsModelType    = modelLabelLong
        , fsFormula      = formula
        , fsCoeffs       = [("μ_α (全体切片)", aMu), ("μ_β (平均傾き)", bMean)]
        , fsR2           = r2
        , fsR2Label      = "R² (全体平均線)"
        , fsFitted       = fitted
        , fsResiduals    = resid
        , fsLinkName     = "Normal (identity link)"
        , fsXColDegs     = [("x", 1)]
        , fsSmoothData   = Just ("x", smooth)
        , fsModelSelect  = Just (wRes, lRes)
        }
      hs = HBMRegSummary
        { hbmsFit           = fs
        , hbmsModelGraph    = buildModelGraph m
        , hbmsChain         = chain
        , hbmsParams        = allParams
        , hbmsPosteriorRows =
            [ (n, fromMaybe 0 (posteriorMean n chain)
              , fromMaybe 0 (posteriorSD   n chain)
              , fromMaybe 0 (posteriorQuantile 0.025 n chain)
              , fromMaybe 0 (posteriorQuantile 0.975 n chain))
            | n <- allParams ]
        }
      diagCfg = PlotConfig "MCMC 診断 (KDE + トレース)" 760 320 Nothing Nothing Nothing
      acfCfg  = PlotConfig "自己相関 (lag 0..40)" 760 220 Nothing Nothing Nothing
      diagPlot = NamedPlot "vl-diag" "MCMC 診断"
                   (mcmcDiagnostics diagCfg mainParams chain)
      acfPlot  = NamedPlot "vl-acf"  "自己相関"
                   (autocorrPlot acfCfg 40 mainParams chain)
      rptCfg = defaultAnalysisConfig
                 ("HBM " <> label <> " — " <> modelLabelLong)
  writeAnalysisReport (T.unpack htmlPath) rptCfg mkDataFrame ["x"] "y"
                      (HBMFit hs) [diagPlot, acfPlot]
  putStrLn $ "    → " ++ T.unpack htmlPath
  return (Just (HBMFit hs))

sortAsc :: [Double] -> [Double]
sortAsc = qs
  where
    qs []     = []
    qs (p:rs) = qs [x | x <- rs, x <= p] ++ [p] ++ qs [x | x <- rs, x > p]

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  HBM ランダム傾き比較: M1 (β 共通) vs M2 (β_g グループ別)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  printf "  3 グループ × 10 観測 = N=%d\n" (length allXs)
  putStrLn "  真値:"
  putStrLn "    Group A: α=2.0, β=-0.8  (急な右下り)"
  putStrLn "    Group B: α=5.0, β=-0.3  (緩やかな右下り)"
  putStrLn "    Group C: α=8.0, β=+0.2  (わずかに右上り)"
  putStrLn ""

  let cfg = defaultNUTSConfig
              { nutsIterations = 800
              , nutsBurnIn     = 400
              , nutsStepSize   = 0.05
              , nutsMaxDepth   = 8
              }
      m1Init = Map.fromList
                 [ ("mu_alpha", 5.0), ("sigma_alpha", 2.0)
                 , ("beta", 0.0), ("sigma", 0.5)
                 , ("alpha_A", 2.0), ("alpha_B", 5.0), ("alpha_C", 8.0)
                 ]
      m1Params = ["mu_alpha","sigma_alpha","beta","sigma",
                  "alpha_A","alpha_B","alpha_C"]
      m2Init = Map.fromList
                 [ ("mu_alpha", 5.0), ("sigma_alpha", 2.0)
                 , ("mu_beta", 0.0), ("sigma_beta", 0.5)
                 , ("sigma", 0.3)
                 , ("alpha_A", 2.0), ("alpha_B", 5.0), ("alpha_C", 8.0)
                 , ("beta_A",  -0.5), ("beta_B", -0.5), ("beta_C", 0.0)
                 ]
      m2Params = ["mu_alpha","sigma_alpha","mu_beta","sigma_beta","sigma",
                  "alpha_A","alpha_B","alpha_C",
                  "beta_A","beta_B","beta_C"]

  putStrLn "[M1] ランダム切片のみ (β 共通):"
  mFit1 <- runHBM "M1" "rs_m1.html" modelM1 ["beta"] m1Init m1Params cfg
  putStrLn ""

  putStrLn "[M2] ランダム切片 + ランダム傾き (β_g 階層):"
  mFit2 <- runHBM "M2" "rs_m2.html" modelM2
                  ["mu_beta","beta_A","beta_B","beta_C"]
                  m2Init m2Params cfg
  putStrLn ""

  -- 統合比較レポート
  case (mFit1, mFit2) of
    (Just f1, Just f2) -> do
      putStrLn "[Compare] M1 vs M2 統合レポート:"
      let entries =
            [ CompareEntry "M1 (β 共通)"           "#e41a1c" f1
            , CompareEntry "M2 (β_g グループ別)"    "#4daf4a" f2
            ]
          rptCfg = defaultAnalysisConfig
                     "HBM Random Intercept vs Random Intercept + Slope"
      writeComparisonReport "rs_compare.html" rptCfg
                            mkDataFrame ["x"] "y" entries
      putStrLn "    → rs_compare.html"
    _ -> putStrLn "  比較レポート生成スキップ"

  putStrLn ""
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  解釈: M2 (各グループに β_g) のほうが WAIC/LOO が小さくなれば、"
  putStrLn "        グループ間で傾きが異なる構造をデータが支持していることになる。"
  putStrLn "═══════════════════════════════════════════════════════════════"
