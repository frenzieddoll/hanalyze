{-# LANGUAGE OverloadedStrings #-}
-- | Spline 回帰のデモ (Phase N1)。
--
-- 真の関数: y = sin(2πx) + 0.3 sin(6πx)
-- これを n=80 サンプル + ノイズで観測し、B-spline (k=3) と
-- 自然立方スプラインで fit、結果を比較。
module Main where

import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)
import qualified System.Random.MWC.Distributions as MWC

import Model.Spline (SplineKind (..), fitSpline, predictSpline,
                     SplineFit (..), equalSpacedKnots)
import Model.Core (FitResult (..))
import Viz.Core (defaultConfig, OutputFormat (..), PlotConfig (..),
                 writeSpec)
import Graphics.Vega.VegaLite

-- 真の関数
trueF :: Double -> Double
trueF x = sin (2 * pi * x) + 0.3 * sin (6 * pi * x)

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Spline 回帰デモ (Phase N1)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  gen <- createSystemRandom
  let n = 80
      xs = V.fromList [fromIntegral i / fromIntegral (n - 1)
                       | i <- [0 .. n - 1]]
  let ysClean = V.map trueF xs
  noise <- V.replicateM n (MWC.normal 0 0.15 gen)
  let ys = V.zipWith (+) ysClean noise
  printf "観測 n=%d, 真の関数 y = sin(2πx) + 0.3 sin(6πx) + N(0, 0.15)\n" n
  putStrLn ""

  let knots = equalSpacedKnots 8 0 1
  printf "ノット (8): %s\n" (show knots)
  putStrLn ""

  let bsFit = fitSpline (BSpline 3) knots xs ys
      bsCoef = LA.toList (sfBeta bsFit)
      bsR2 = rSquared (sfResult bsFit)
  printf "[B-spline cubic, k=3]  係数次元 = %d, R² = %.4f\n"
         (length bsCoef) bsR2

  let ncFit = fitSpline NaturalCubic knots xs ys
      ncCoef = LA.toList (sfBeta ncFit)
      ncR2 = rSquared (sfResult ncFit)
  printf "[Natural cubic]        係数次元 = %d, R² = %.4f\n"
         (length ncCoef) ncR2
  putStrLn ""

  -- グリッドで予測 → 真値との RMSE
  let xGrid = V.fromList [fromIntegral i * 0.001 | i <- [0 .. 1000]]
      yGrid = V.map trueF xGrid
      yBs   = predictSpline bsFit xGrid
      yNc   = predictSpline ncFit xGrid
      rmse a b = sqrt (V.sum (V.zipWith (\u v -> (u - v)^(2::Int)) a b)
                       / fromIntegral (V.length a))
  printf "  RMSE (B-spline, vs 真値) = %.4f\n" (rmse yBs yGrid)
  printf "  RMSE (Natural,  vs 真値) = %.4f\n" (rmse yNc yGrid)
  putStrLn ""

  let cfg = (defaultConfig "Spline regression — B-spline vs Natural cubic")
              { plotWidth = 700, plotHeight = 350 }
      vlSpec = toVegaLite
        [ title (plotTitle cfg) []
        , layer
            [ asSpec
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
            , asSpec
                [ dataFromColumns []
                    . dataColumn "x" (Numbers (V.toList xGrid))
                    . dataColumn "y" (Numbers (V.toList yBs))
                    $ []
                , mark Line [MColor "#1F77B4", MStrokeWidth 2.5]
                , encoding
                    . position X [PName "x", PmType Quantitative]
                    . position Y [PName "y", PmType Quantitative]
                    $ []
                ]
            , asSpec
                [ dataFromColumns []
                    . dataColumn "x" (Numbers (V.toList xGrid))
                    . dataColumn "y" (Numbers (V.toList yNc))
                    $ []
                , mark Line [MColor "#FF8C42", MStrokeWidth 2.5]
                , encoding
                    . position X [PName "x", PmType Quantitative]
                    . position Y [PName "y", PmType Quantitative]
                    $ []
                ]
            , asSpec
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
  writeSpec HTML "spline.html" vlSpec
  putStrLn "  → spline.html"
  putStrLn "    真値=灰破線, B-spline=青, Natural=オレンジ, 観測=黒点"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ B-spline / Natural cubic spline で非線形 fit"
  putStrLn "═══════════════════════════════════════════════════════════════"
