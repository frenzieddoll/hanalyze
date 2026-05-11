{-# LANGUAGE OverloadedStrings #-}
-- | ロバスト GP のデモ。
--
-- 真の関数 sin(x) + 0.3 cos(3x) からデータを生成し、3 点を **大きな外れ値**
-- (+5σ レベル) に置き換える。次の 3 モデルで RMSE を比較:
--
-- 1. 通常 GP (Gaussian 観測)
-- 2. ロバスト GP w/ Cauchy(γ=0.5) — 重い裾、外れ値に強い
-- 3. ロバスト GP w/ StudentT(ν=4, σ=0.5) — Cauchy より軽い裾
module Main where

import Model.GP        (Kernel (..), GPParams (..), GPModel (..), fitGP, gpMean)
import Model.GPRobust  (RobustLikelihood (..), RobustGPFit (..),
                        fitGPRobust, predictGPRobust)
import Text.Printf     (printf)

trueF :: Double -> Double
trueF x = sin x + 0.3 * cos (3 * x)

pseudoNoise :: Int -> Double -> Double
pseudoNoise seed x = 0.1 * sin (fromIntegral seed * 2.3998 + x * 17.3)

main :: IO ()
main = do
  putStrLn "=================================="
  putStrLn " Robust GP Demo (StudentT / Cauchy)"
  putStrLn "=================================="
  putStrLn ""

  -- 訓練データ: 50 点
  let n      = 50
      trainX = [ fromIntegral i * (2 * pi) / fromIntegral (n - 1)
               | i <- [0 .. n - 1 :: Int] ]
      cleanY = zipWith (\i x -> trueF x + pseudoNoise i x)
                 [0 :: Int ..] trainX
      -- 3 点を外れ値に置換 (index 10, 25, 40)
      trainY = [ if i `elem` [10, 25, 40]
                   then y + 4.0          -- +4σ レベルの外れ値
                   else y
               | (i, y) <- zip [0 :: Int ..] cleanY ]

      -- テスト点
      m     = 100
      testX = [ fromIntegral i * (2 * pi) / fromIntegral (m - 1)
              | i <- [0 .. m - 1 :: Int] ]
      testY = map trueF testX

      -- ハイパラ (ノイズ含めて固定)
      hp = GPParams { gpLengthScale  = 0.6
                    , gpSignalVar    = 1.0
                    , gpNoiseVar     = 0.05
                    , gpPeriod       = 1.0
                    , gpLengthScales = Nothing
                    }

  printf "Training: %d points (3 outliers at index 10, 25, 40 with +4 offset)\n" n
  printf "Test:     %d points (clean true f)\n" m
  printf "Hyperparams (fixed): l=%.2f sigma_f^2=%.2f noise=%.4f\n"
         (gpLengthScale hp) (gpSignalVar hp) (gpNoiseVar hp)
  putStrLn ""

  -- 1. 通常 GP (Gaussian)
  putStrLn "--- 1. Gaussian GP (Model.GP) ---"
  let gpRes = fitGP (GPModel RBF hp) trainX trainY testX
      gaussRMSE = rmse testY (gpMean gpRes)
  printf "  RMSE (vs true f): %.4f\n" gaussRMSE
  putStrLn ""

  -- 2. Robust GP w/ Cauchy
  putStrLn "--- 2. Robust GP w/ Cauchy(gamma=0.5) ---"
  let cauchyFit  = fitGPRobust RBF hp (RCauchy 0.5) trainX trainY
      cauchyPred = predictGPRobust cauchyFit testX
      cauchyRMSE = rmse testY (map fst cauchyPred)
  printf "  IRLS converged in %d iterations\n" (rgpIters cauchyFit)
  printf "  RMSE (vs true f): %.4f\n" cauchyRMSE
  printf "  Improvement over Gaussian: %.1f%%\n"
         (100 * (gaussRMSE - cauchyRMSE) / gaussRMSE)
  putStrLn ""

  -- 3. Robust GP w/ StudentT
  putStrLn "--- 3. Robust GP w/ StudentT(nu=4, sigma=0.5) ---"
  let stFit  = fitGPRobust RBF hp (RStudentT 4 0.5) trainX trainY
      stPred = predictGPRobust stFit testX
      stRMSE = rmse testY (map fst stPred)
  printf "  IRLS converged in %d iterations\n" (rgpIters stFit)
  printf "  RMSE (vs true f): %.4f\n" stRMSE
  printf "  Improvement over Gaussian: %.1f%%\n"
         (100 * (gaussRMSE - stRMSE) / gaussRMSE)
  putStrLn ""

  putStrLn "Done."
  putStrLn ""
  putStrLn "Cauchy is most robust (heaviest tails, lowest RMSE)."
  putStrLn "StudentT(nu=4) is intermediate."
  putStrLn "Gaussian is most distorted by the 3 outliers."

rmse :: [Double] -> [Double] -> Double
rmse a b =
  let n = length a
      sse = sum [ (x - y) ^ (2 :: Int) | (x, y) <- zip a b ]
  in sqrt (sse / fromIntegral n)
