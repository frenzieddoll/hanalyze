{-# LANGUAGE OverloadedStrings #-}
-- | GP 回帰デモ + HTML レポート生成
--
-- sin(x) + 0.3*cos(3x) の真の関数から 30 点をサンプルしてノイズを加え、
-- RBF / Matérn 5/2 / Periodic の 3 種類のカーネルで GP 回帰を行い
-- 総合 HTML レポートを demo/gp_report.html に出力します。
module Main where

import Model.GP
import Viz.GPReport
import Viz.Core (openInBrowser)
import Text.Printf (printf)

-- 真の関数
trueF :: Double -> Double
trueF x = sin x + 0.3 * cos (3 * x)

-- 決定論的な疑似ノイズ（再現性確保）
pseudoNoise :: Int -> Double -> Double
pseudoNoise seed x = 0.25 * sin (fromIntegral seed * 2.3998 + x * 17.3)

main :: IO ()
main = do
  putStrLn "============================================"
  putStrLn " Gaussian Process Regression Demo"
  putStrLn "============================================"
  putStrLn ""

  -- 訓練データ: [0, 2π] から 30 点
  let n      = 30
      trainX = [ fromIntegral i * (2 * pi) / fromIntegral (n - 1)
               | i <- [0 .. n - 1 :: Int] ]
      trainY = zipWith (\i x -> trueF x + pseudoNoise i x)
                 [0 :: Int ..] trainX
      trainData = zip trainX trainY

  -- テストグリッド (200 点)
  let m     = 200
      testX = [ fromIntegral i * (2 * pi) / fromIntegral (m - 1)
              | i <- [0 .. m - 1 :: Int] ]

  -- データ統計から初期ハイパーパラメータを設定
  let p0 = initParamsFromData trainX trainY
  printf "Initial params: l=%.3f  sf=%.3f  sn=%.3f\n"
    (gpLengthScale p0) (sqrt (gpSignalVar p0)) (sqrt (gpNoiseVar p0))
  putStrLn ""

  -- 各カーネルの最適化とフィット
  putStrLn "Optimizing RBF..."
  let optRBF = optimizeGP RBF trainX trainY p0
  printf "  RBF:     l=%.3f  sf=%.3f  sn=%.4f  LML=%.2f\n"
    (gpLengthScale optRBF) (sqrt (gpSignalVar optRBF))
    (sqrt (gpNoiseVar optRBF))
    (logMarginalLikelihood trainX trainY RBF optRBF)

  putStrLn "Optimizing Matern52..."
  let optM52 = optimizeGP Matern52 trainX trainY p0
  printf "  Matern:  l=%.3f  sf=%.3f  sn=%.4f  LML=%.2f\n"
    (gpLengthScale optM52) (sqrt (gpSignalVar optM52))
    (sqrt (gpNoiseVar optM52))
    (logMarginalLikelihood trainX trainY Matern52 optM52)

  putStrLn "Optimizing Periodic..."
  let p0Per  = p0 { gpPeriod = 2 * pi }
      optPer = optimizeGP Periodic trainX trainY p0Per
  printf "  Periodic: l=%.3f  sf=%.3f  sn=%.4f  p=%.3f  LML=%.2f\n"
    (gpLengthScale optPer) (sqrt (gpSignalVar optPer))
    (sqrt (gpNoiseVar optPer)) (gpPeriod optPer)
    (logMarginalLikelihood trainX trainY Periodic optPer)
  putStrLn ""

  -- フィット結果をまとめる
  let fits =
        [ makeGPFit "RBF"       RBF      optRBF trainX trainY testX
        , makeGPFit "Matern5/2" Matern52 optM52 trainX trainY testX
        , makeGPFit "Periodic"  Periodic optPer trainX trainY testX
        ]

  -- レポート生成
  let rptCfg = defaultGPReportConfig "GP Regression Report"
  writeGPReport "demo/gp_report.html" rptCfg trainData fits
  putStrLn "Saved: demo/gp_report.html"

  openInBrowser "demo/gp_report.html"
