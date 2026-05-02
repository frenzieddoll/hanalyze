{-# LANGUAGE OverloadedStrings #-}
-- | Random Fourier Features (RFF) のデモ。
--
-- - 真の関数から N=200 点を生成
-- - 厳密 GP (O(n³)) と RFF GP (O(n D + D³)) を比較 (固定ハイパラ)
-- - D = 50 / 100 / 200 で RMSE と実行時間を計測
-- - RFF が n が大きいときにほぼ同精度・高速であることを確認
--
-- 注: optimizeGP の最適化は時間がかかるため、デモでは固定ハイパラを使用。
-- 実用では initParamsFromData → optimizeGP で先にカーネルを最適化してから
-- そのパラメータで RFF を構成する。
module Main where

import qualified Numeric.LinearAlgebra as LA
import qualified System.Random.MWC as MWC
import Control.Exception (evaluate)
import Model.GP        as GP
import Model.RFF       as RFF
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf     (printf)

-- 真の関数
trueF :: Double -> Double
trueF x = sin (1.5 * x) + 0.3 * cos (3.0 * x)

-- 決定論的疑似ノイズ
pseudoNoise :: Int -> Double -> Double
pseudoNoise seed x = 0.15 * sin (fromIntegral seed * 2.3998 + x * 17.3)

main :: IO ()
main = do
  putStrLn "================================================"
  putStrLn " Random Fourier Features (RFF) Demo"
  putStrLn "================================================"
  putStrLn ""

  -- 訓練データ: N=1500 点 (厳密 GP の O(n³) が体感できるサイズ)
  let n      = 1500
      trainX = [ fromIntegral i * (2 * pi) / fromIntegral (n - 1)
               | i <- [0 .. n - 1 :: Int] ]
      trainY = zipWith (\i x -> trueF x + pseudoNoise i x)
                 [0 :: Int ..] trainX

      -- テスト点 (200 点)
      m     = 200
      testX = [ 0.5 + fromIntegral i * (2 * pi - 1) / fromIntegral (m - 1)
              | i <- [0 .. m - 1 :: Int] ]
      testY = map trueF testX

      -- 固定ハイパラ (公平な比較のため)
      ell = 0.6 :: Double
      sf  = 1.0 :: Double
      sn  = 0.15 :: Double
      sigF2 = sf * sf
      noiseV = sn * sn

  printf "Training samples: %d\n" n
  printf "Test samples:     %d\n" m
  printf "Fixed hyperparams: l=%.2f  sigma_f^2=%.2f  noise_var=%.4f\n"
         ell sigF2 noiseV
  putStrLn ""

  -- ================================================
  -- 1. 厳密 GP (Model.GP, RBF)
  -- ================================================
  putStrLn "--- Exact GP (RBF, Cholesky O(n^3)) ---"
  t0 <- getCurrentTime
  let paramsX = GPParams { gpLengthScale = ell
                         , gpSignalVar   = sigF2
                         , gpNoiseVar    = noiseV
                         , gpPeriod      = 1.0
                         }
      modelX  = GPModel RBF paramsX
      resX    = fitGP modelX trainX trainY testX
  _ <- evaluate (LA.sumElements (LA.fromList (gpMean resX)))
  t1 <- getCurrentTime
  let exactRMSE = rmse testY (gpMean resX)
      exactTime = diffUTCTime t1 t0
  printf "  RMSE (vs true f): %.4f\n" exactRMSE
  printf "  Time:             %.3fs\n" (realToFrac exactTime :: Double)
  putStrLn ""

  -- ================================================
  -- 2. RFF GP, D = 50, 100, 200
  -- ================================================
  putStrLn "--- RFF GP (RBF, D ∈ {50, 100, 200}) ---"

  gen <- MWC.createSystemRandom

  mapM_ (\d -> do
    t2 <- getCurrentTime
    feats   <- RFF.sampleRFFRBF d ell sf gen
    let fit  = RFF.rffGP feats trainX trainY sn
        pred_ = RFF.predictRFFGP fit testX
    _ <- evaluate (sum (map fst pred_))
    t3 <- getCurrentTime
    let rffRMSE = rmse testY (map fst pred_)
        rffTime = diffUTCTime t3 t2
        speedup = realToFrac exactTime / realToFrac rffTime :: Double
    printf "  D=%-3d  RMSE=%.4f  time=%.3fs  speedup=%.1fx\n"
           d rffRMSE (realToFrac rffTime :: Double) speedup
    ) [50, 100, 200]

  putStrLn ""
  putStrLn "--- RFF Ridge regression (no predictive variance, D=200) ---"
  feats   <- RFF.sampleRFFRBF 200 ell sf gen
  let lam  = 0.01
      ridg = RFF.rffRidge feats trainX trainY lam
      yhat = RFF.predictRFFRidge ridg testX
      ridgeRMSE = rmse testY yhat
  printf "  D=200, lambda=%.3f  RMSE=%.4f\n" lam ridgeRMSE
  putStrLn ""
  putStrLn "Done."

-- 平均二乗誤差の平方根
rmse :: [Double] -> [Double] -> Double
rmse a b =
  let n = length a
      sse = sum [ (x - y) ^ (2 :: Int) | (x, y) <- zip a b ]
  in sqrt (sse / fromIntegral n)
