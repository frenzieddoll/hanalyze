{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | HBM (ベイズ階層モデル) を使った単回帰デモ。
--
-- モデル:
--   alpha ~ Normal(0, 10)        -- 切片
--   beta  ~ Normal(0, 10)        -- 傾き
--   sigma ~ Exponential(1)       -- 観測ノイズ
--   y_i   ~ Normal(alpha + beta * x_i, sigma)
--
-- NUTS で事後サンプリング → AnalysisReport を生成:
--   * モデル概要に DAG (依存グラフ)
--   * 回帰結果に MCMC 診断 (KDE/トレース/自己相関)
--   * 対話的予測 (95% 信用区間バンド付き)
module Main where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.List (sort)
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import qualified DataFrame                    as DX
import qualified DataFrame.Internal.DataFrame as DXD
import MCMC.Core (Chain (..), chainVals, posteriorMean, posteriorSD,
                  posteriorQuantile)
import MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Model.HBM (ModelP, sample, observe, Distribution (..),
                  buildModelGraph)
import Stat.MCMC (ess)
import Viz.AnalysisReport
  ( AnalysisReportConfig (..), defaultAnalysisConfig
  , FitSummary (..), SmoothData (..), HBMRegSummary (..)
  , ModelFit (..), NamedPlot (..)
  , writeAnalysisReport
  )
import Viz.Core (PlotConfig (..))
import Viz.MCMC (mcmcDiagnostics, autocorrPlot)

import Model.GLM (Family (..), LinkFn (..))

-- ---------------------------------------------------------------------------
-- 合成データ: y = 2 + 3x + ε,  ε ~ N(0, 1.5²)
-- ---------------------------------------------------------------------------

trueAlpha, trueBeta, trueSigma :: Double
trueAlpha = 2.0
trueBeta  = 3.0
trueSigma = 1.5

xs :: [Double]
xs = [-2.5, -2.1, -1.7, -1.3, -0.9, -0.5, -0.1, 0.3, 0.7, 1.1
     , 1.5, 1.9, 2.3, 2.7, 3.1, 0.0, 0.6, 1.2, 2.0, 2.8
     , -1.0, -1.5, 1.0, 1.4, -0.3, 0.2, 1.8, 2.4, -2.0, 0.9]

-- 真の関係 + 軽いノイズ (再現性のため固定)
ys :: [Double]
ys = zipWith (+) [trueAlpha + trueBeta * x | x <- xs]
                 [-1.41, 0.83, -0.66, 1.55, 0.27, 1.83, -0.30, 0.45, -1.18, 1.52
                 , 0.62, -1.25, 1.09, -0.31, 0.74, 0.18, -1.84, 1.43, -0.96, 1.21
                 , -0.55, 1.79, 0.04, 0.91, -1.43, 0.38, 0.66, -0.80, 0.49, -1.12]

-- ---------------------------------------------------------------------------
-- HBM 回帰モデル (top-level: rank-2 type の monomorphisation を回避)
-- ---------------------------------------------------------------------------

regModel :: ModelP ()
regModel = do
  alpha <- sample "alpha" (Normal 0 10)
  beta  <- sample "beta"  (Normal 0 10)
  sigma <- sample "sigma" (Exponential 1)
  -- yMean は各観測値で異なるため、observe を観測ごとに発行する
  -- (1 つの分布で全観測を扱うと、x が分布パラメータに入らないため)
  mapM_ (\(x, y) ->
    let xC = realToFrac x
    in observe "y" (Normal (alpha + beta * xC) sigma) [y])
    (zip xs ys)

-- ---------------------------------------------------------------------------
-- 事後予測曲線 (信用区間付き) を計算
-- ---------------------------------------------------------------------------

-- グリッド x* 上で、各事後サンプル (α, β) から μ* = α + β·x* を計算し、
-- その分布の 2.5%/50%/97.5% 分位点を返す。
makeSmoothData :: Chain -> SmoothData
makeSmoothData ch =
  let alphas = chainVals "alpha" ch
      betas  = chainVals "beta"  ch
      xMin   = minimum xs
      xMax   = maximum xs
      ext    = (xMax - xMin) * 0.5
      grid   = [xMin - ext + i * (xMax - xMin + 2 * ext) / 99 | i <- [0..99]]
      atX x  = sort (zipWith (\a b -> a + b * x) alphas betas)
      qAt p ss = let n = length ss
                     i = max 0 (min (n - 1) (floor (p * fromIntegral n) :: Int))
                 in ss !! i
      ysMean = [ let s = atX x in qAt 0.5 s | x <- grid ]
      ysLo   = [ let s = atX x in qAt 0.025 s | x <- grid ]
      ysHi   = [ let s = atX x in qAt 0.975 s | x <- grid ]
  in SmoothData
       { sdXs      = grid
       , sdYs      = ysMean
       , sdLower   = ysLo
       , sdUpper   = ysHi
       , sdHasBand = True
       }

-- ---------------------------------------------------------------------------
-- DataFrame の組み立て
-- ---------------------------------------------------------------------------

mkDataFrame :: DXD.DataFrame
mkDataFrame = DX.insertColumn "x" (DX.fromList (xs :: [Double]))
            $ DX.insertColumn "y" (DX.fromList (ys :: [Double]))
            $ DX.empty

-- ---------------------------------------------------------------------------
-- HBM フィット結果から FitSummary を構築
-- ---------------------------------------------------------------------------

mkFitForHBM :: Chain -> FitSummary
mkFitForHBM ch =
  let aMean = maybe 0 id (posteriorMean "alpha" ch)
      bMean = maybe 0 id (posteriorMean "beta"  ch)
      fitted = [aMean + bMean * x | x <- xs]
      resid  = zipWith (-) ys fitted
      yBar   = sum ys / fromIntegral (length ys)
      tss    = sum [(y - yBar) ^ (2::Int) | y <- ys]
      rss    = sum [r ^ (2::Int) | r <- resid]
      r2     = if tss < 1e-12 then 0 else 1 - rss / tss
      smooth = makeSmoothData ch
  in FitSummary
       { fsModelType    = "Bayesian Linear Regression (HBM)"
       , fsFormula      = "y ~ α + β · x"
       , fsCoeffs       = [("(Intercept) α", aMean), ("β (x)", bMean)]
       , fsR2           = r2
       , fsR2Label      = "R²"
       , fsFitted       = fitted
       , fsResiduals    = resid
       , fsLinkName     = "Normal (identity link)"
       , fsXColDegs     = [("x", 1)]
       , fsSmoothData   = Just ("x", smooth)
       , fsModelSelect  = Nothing
       }

mkPosteriorRows :: Chain -> [(Text, Double, Double, Double, Double)]
mkPosteriorRows ch =
  [ ( name
    , maybe 0 id (posteriorMean name ch)
    , maybe 0 id (posteriorSD   name ch)
    , maybe 0 id (posteriorQuantile 0.025 name ch)
    , maybe 0 id (posteriorQuantile 0.975 name ch)
    )
  | name <- ["alpha", "beta", "sigma"]
  ]

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== HBM 単回帰デモ ==="
  printf "  真値: α=%.2f, β=%.2f, σ=%.2f\n" trueAlpha trueBeta trueSigma
  printf "  サンプル数: n=%d\n\n" (length xs)

  putStrLn "[NUTS サンプリング (AD 勾配, dual averaging)]"
  let cfg = defaultNUTSConfig
              { nutsIterations = 2000
              , nutsBurnIn     = 500
              , nutsStepSize   = 0.1
              }
      initP = Map.fromList [("alpha", 0.0), ("beta", 0.0), ("sigma", 1.0)]
  gen <- createSystemRandom
  chain <- nuts regModel cfg initP gen

  printf "  受容率: %.1f%%\n" (acceptanceRateOf chain * 100)
  printf "  サンプル数: %d\n\n" (length (chainSamples chain))

  putStrLn "[事後分布サマリー]"
  printf "  %-8s %10s %10s %10s %10s %10s\n"
    ("param"::String) ("mean"::String) ("sd"::String)
    ("2.5%"::String) ("97.5%"::String) ("ESS"::String)
  mapM_ (\name ->
    printf "  %-8s %10.4f %10.4f %10.4f %10.4f %10.0f\n"
      (T.unpack name)
      (maybe 0 id (posteriorMean name chain))
      (maybe 0 id (posteriorSD   name chain))
      (maybe 0 id (posteriorQuantile 0.025 name chain))
      (maybe 0 id (posteriorQuantile 0.975 name chain))
      (ess (chainVals name chain)))
    ["alpha", "beta", "sigma"]

  -- DAG / FitSummary / 診断プロットを構築
  let graph = buildModelGraph regModel
      fs    = mkFitForHBM chain
      hs    = HBMRegSummary
                { hbmsFit           = fs
                , hbmsModelGraph    = graph
                , hbmsChain         = chain
                , hbmsParams        = ["alpha", "beta", "sigma"]
                , hbmsPosteriorRows = mkPosteriorRows chain
                }
      diagCfg = PlotConfig
                  { plotTitle  = "MCMC 診断 (KDE + トレース)"
                  , plotWidth  = 720
                  , plotHeight = 280
                  }
      acfCfg  = PlotConfig
                  { plotTitle  = "自己相関 (lag 0..40)"
                  , plotWidth  = 720
                  , plotHeight = 220
                  }
      diagPlot = NamedPlot
                   { npName  = "vl-hbm-diag"
                   , npTitle = "MCMC 診断 (KDE + トレース)"
                   , npSpec  = mcmcDiagnostics diagCfg ["alpha", "beta", "sigma"] chain
                   }
      acfPlot  = NamedPlot
                   { npName  = "vl-hbm-acf"
                   , npTitle = "パラメータ別 自己相関"
                   , npSpec  = autocorrPlot acfCfg 40 ["alpha", "beta", "sigma"] chain
                   }
      reportCfg = defaultAnalysisConfig "HBM 単回帰 — AnalysisReport"
      df = mkDataFrame

  putStrLn "\n[HTML レポート生成]"
  writeAnalysisReport "hbm_regression_report.html" reportCfg df ["x"] "y"
                       (HBMFit hs) [diagPlot, acfPlot]
  putStrLn "  hbm_regression_report.html"
  putStrLn "  (DAG + 事後分布 + MCMC 診断 + 信用区間付き対話的予測)"

acceptanceRateOf :: Chain -> Double
acceptanceRateOf ch =
  let t = chainTotal ch
      a = chainAccepted ch
  in if t == 0 then 0 else fromIntegral a / fromIntegral t :: Double

-- 未使用警告の抑制
_unused :: (Family, LinkFn)
_unused = (Gaussian, Identity)
