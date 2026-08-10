{-# LANGUAGE OverloadedStrings #-}
-- | 多出力線形回帰デモ (案 B1)、ReportBuilder 経由の対話的レポート。
--
-- データ: data/io/potential_wide.csv  (21 dose 行 × 100 z 出力列)
-- モデル: Y (n×100) = X (n×2 [1,dose]) · B (2×100)
-- 出力 : trash/potential_multiout.html
module Main where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import Text.Printf (printf)

import qualified Hanalyze.DataIO.CSV as IO
import qualified Hanalyze.DataIO.Convert as Conv
import qualified Hanalyze.Model.MultiLM as ML
import Hanalyze.Model.Core (FitResult (..))
import Hanalyze.Viz.ReportBuilder

zGrid :: [Double]
zGrid =
  let step = 200.0 / 99.0
  in [ fromIntegral i * step | i <- [0 .. 99 :: Int] ]

main :: IO ()
main = do
  Right df <- IO.loadAuto "data/io/potential_wide.csv"
  let yColNames = [ T.pack (printf "y_z%03d" (i :: Int)) | i <- [1..100] ]
      Just doseV = Conv.getDoubleVec "dose" df
      yCols     = map (\c -> case Conv.getDoubleVec c df of
                               Just v  -> v
                               Nothing -> error ("missing column: " ++ T.unpack c))
                      yColNames
      n = V.length doseV
      q = length yCols
      x = LA.fromLists [ [1.0, doseV V.! i] | i <- [0 .. n - 1] ]
      y = LA.fromLists
            [ [ (yCols !! j) V.! i | j <- [0 .. q - 1] ]
            | i <- [0 .. n - 1] ]
      mf = ML.fitMultiLM x y
      betaB = coefficients (ML.mfFit mf)   -- (2 × q)
      res  = residuals (ML.mfFit mf)
      rmse = sqrt (LA.sumElements (res * res) / fromIntegral (n * q))
      r2v  = rSquared (ML.mfFit mf)

  putStrLn "=== Multi-output Linear Regression (B1: dose only) ==="
  printf "  N (rows)     = %d\n" n
  printf "  q (outputs)  = %d\n" q
  printf "  RMSE overall = %.4f\n" rmse
  printf "  R^2 mean     = %.4f  (min %.4f, max %.4f)\n"
    (LA.sumElements r2v / fromIntegral q)
    (LA.minElement r2v) (LA.maxElement r2v)

  let intercepts = LA.toList (betaB LA.! 0)
      slopes     = LA.toList (betaB LA.! 1)
      xObs       = V.toList doseV
      yObs       = [ [ (yCols !! j) V.! i | j <- [0 .. q - 1] ]
                   | i <- [0 .. n - 1] ]
      dMin = minimum xObs - 2.0
      dMax = maximum xObs + 2.0
      dMid = 0.5 * (dMin + dMax)
      imo  = mkInteractiveMOLinear "dose" "potential V" "z [nm]"
                                   zGrid xObs yObs
                                   intercepts slopes
                                   (dMin, dMid, dMax)
      sections =
        [ secModelOverview "Multi-output Linear Regression"
            "$Y_{n\\times q} = X_{n\\times 2} B_{2\\times q} + E$"
            Nothing
        , secStatRow
            [ ("N", T.pack (show n))
            , ("q (outputs)", T.pack (show q))
            , ("RMSE", T.pack (printf "%.4f" rmse))
            , ("R^2 mean", T.pack (printf "%.4f"
                (LA.sumElements r2v / fromIntegral q)))
            ]
        , secInteractiveMultiOut "予測曲線 (dose スライダ)" imo
        ]
      cfg = defaultReportConfig "Potential — Multi-output OLS (B1)"
  renderReport "trash/potential_multiout.html" cfg sections
  putStrLn "Wrote trash/potential_multiout.html"
