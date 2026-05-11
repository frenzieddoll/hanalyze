{-# LANGUAGE OverloadedStrings #-}
-- | カーネル回帰のデモ (Phase N2)。
--
-- 真の関数: y = sin(2πx) + 0.3 sin(6πx)
-- Spline と同じデータで、Nadaraya-Watson と Kernel Ridge を比較。
module Main where

import qualified Data.Vector as V
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)
import qualified System.Random.MWC.Distributions as MWC

import Hanalyze.Model.Kernel (Kernel (..), nwRegression, kernelRidge,
                     predictKernelRidge, gridSearchBandwidth)
import Hanalyze.Viz.Core (defaultConfig, OutputFormat (..), PlotConfig (..),
                 writeSpec)
import Graphics.Vega.VegaLite

trueF :: Double -> Double
trueF x = sin (2 * pi * x) + 0.3 * sin (6 * pi * x)

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  カーネル回帰デモ (Phase N2)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  gen <- createSystemRandom
  let n = 80
      xs = V.fromList [fromIntegral i / fromIntegral (n - 1)
                       | i <- [0 .. n - 1]]
      ysClean = V.map trueF xs
  noise <- V.replicateM n (MWC.normal 0 0.15 gen)
  let ys = V.zipWith (+) ysClean noise
  printf "観測 n=%d, 真の関数 y = sin(2πx) + 0.3 sin(6πx) + N(0, 0.15)\n" n
  putStrLn ""

  -- Bandwidth 選定 (LOO-CV)
  let hCandidates = [0.02, 0.03, 0.05, 0.08, 0.10, 0.15, 0.20]
  let (bestH, _bestErr) = gridSearchBandwidth Gaussian xs ys hCandidates
  printf "Bandwidth 選定 (LOO-CV, Gaussian カーネル):\n"
  mapM_ (\h ->
            let (_, err) = gridSearchBandwidth Gaussian xs ys [h]
                tag :: String
                tag = if h == bestH then "  ← best" else ""
            in printf "  h=%.3f  RMSE_LOO=%.4f%s\n" h err tag)
        hCandidates
  putStrLn ""

  -- 4 つのカーネルで NW 回帰
  let xGrid = V.fromList [fromIntegral i * 0.001 | i <- [0 .. 1000 :: Int]]
      yGrid = V.map trueF xGrid
      rmse a b = sqrt (V.sum (V.zipWith (\u v -> (u - v)^(2::Int)) a b)
                       / fromIntegral (V.length a))

  putStrLn "[Nadaraya-Watson, h=best (LOO 選定)]"
  let yNwGauss = nwRegression Gaussian     bestH xs ys xGrid
      yNwEpa   = nwRegression Epanechnikov bestH xs ys xGrid
      yNwTri   = nwRegression Triangular   bestH xs ys xGrid
      yNwTC    = nwRegression TriCube      bestH xs ys xGrid
  printf "  Gaussian:     RMSE = %.4f\n" (rmse yNwGauss yGrid)
  printf "  Epanechnikov: RMSE = %.4f\n" (rmse yNwEpa   yGrid)
  printf "  Triangular:   RMSE = %.4f\n" (rmse yNwTri   yGrid)
  printf "  TriCube:      RMSE = %.4f\n" (rmse yNwTC    yGrid)
  putStrLn ""

  -- Kernel Ridge
  putStrLn "[Kernel Ridge, h=best, λ 比較]"
  let lambdas = [0.001, 0.01, 0.1, 1.0]
  yKRs <- mapM
    (\lam -> do
        let fit  = kernelRidge Gaussian bestH lam xs ys
            yKR  = predictKernelRidge fit xGrid
        printf "  λ=%.3f:  RMSE = %.4f\n" lam (rmse yKR yGrid)
        return yKR)
    lambdas
  putStrLn ""

  -- 可視化: 真値 + NW(Gaussian) + Kernel Ridge(λ=0.01) + 観測
  let yKRBest = yKRs !! 1   -- λ = 0.01
  let cfg = (defaultConfig "Kernel regression — NW vs Kernel Ridge")
              { plotWidth = 700, plotHeight = 350 }
      vlSpec = toVegaLite
        [ title (plotTitle cfg) []
        , layer
            [ asSpec   -- 真値 (灰破線)
                [ dataFromColumns []
                    . dataColumn "x" (Numbers (V.toList xGrid))
                    . dataColumn "y" (Numbers (V.toList yGrid))
                    $ []
                , mark Line [MColor "#888888", MStrokeWidth 1.5,
                             MStrokeDash [4, 4]]
                , encoding
                    . position X [PName "x", PmType Quantitative]
                    . position Y [PName "y", PmType Quantitative]
                    $ []
                ]
            , asSpec   -- NW (青)
                [ dataFromColumns []
                    . dataColumn "x" (Numbers (V.toList xGrid))
                    . dataColumn "y" (Numbers (V.toList yNwGauss))
                    $ []
                , mark Line [MColor "#1F77B4", MStrokeWidth 2.0]
                , encoding
                    . position X [PName "x", PmType Quantitative]
                    . position Y [PName "y", PmType Quantitative]
                    $ []
                ]
            , asSpec   -- Kernel Ridge (オレンジ)
                [ dataFromColumns []
                    . dataColumn "x" (Numbers (V.toList xGrid))
                    . dataColumn "y" (Numbers (V.toList yKRBest))
                    $ []
                , mark Line [MColor "#FF8C42", MStrokeWidth 2.5]
                , encoding
                    . position X [PName "x", PmType Quantitative]
                    . position Y [PName "y", PmType Quantitative]
                    $ []
                ]
            , asSpec   -- 観測点 (黒)
                [ dataFromColumns []
                    . dataColumn "x" (Numbers (V.toList xs))
                    . dataColumn "y" (Numbers (V.toList ys))
                    $ []
                , mark Point [MOpacity 0.5, MSize 25, MColor "#222222"]
                , encoding
                    . position X [PName "x", PmType Quantitative]
                    . position Y [PName "y", PmType Quantitative]
                    $ []
                ]
            ]
        , width  (plotWidth cfg)
        , height (plotHeight cfg)
        ]
  writeSpec HTML "kernel.html" vlSpec
  putStrLn "  → kernel.html"
  putStrLn "    真値=灰破線, NW(Gaussian)=青, Kernel Ridge=オレンジ, 観測=黒点"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ Nadaraya-Watson と Kernel Ridge の双方が動作"
  putStrLn "═══════════════════════════════════════════════════════════════"
