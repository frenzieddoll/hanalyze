{-# LANGUAGE OverloadedStrings #-}
-- | 多出力 RBF カーネルリッジ回帰デモ。
--
-- データ: data/io/potential_wide.csv  (21 dose 行 × 100 z 出力列)
-- モデル: ŷ_j(d) = Σ_i K_h(d, d_i) · α_{ij} ;  α = (K + λI)⁻¹ Y
-- HP    : LOOCV 解析解で h, λ をグリッド最適化
-- 出力 : trash/potential_multikr.html
module Main where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import Text.Printf (printf)

import qualified DataIO.CSV as IO
import qualified DataIO.Convert as Conv
import qualified Model.Kernel as K
import Viz.ReportBuilder

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
      ys = LA.fromLists
             [ [ (yCols !! j) V.! i | j <- [0 .. q - 1] ]
             | i <- [0 .. n - 1] ]
      hs   = K.defaultHGrid doseV
      lams = K.defaultLamGrid
      (fit, bestH, bestL, looMSE) =
        K.autoTuneKernelRidgeMulti K.Gaussian doseV ys hs lams
      yhat = K.fittedKernelRidgeMulti fit
      r2v  = K.r2Multi ys yhat
      res  = ys - yhat
      rmse = sqrt (LA.sumElements (res * res) / fromIntegral (n * q))

  putStrLn "=== Multi-output Kernel Ridge (RBF, dose only) ==="
  printf "  N (rows)     = %d\n" n
  printf "  q (outputs)  = %d\n" q
  printf "  best h       = %.4f\n" bestH
  printf "  best lambda  = %.6g\n" bestL
  printf "  LOO MSE      = %.6f\n" looMSE
  printf "  RMSE (train) = %.4f\n" rmse
  printf "  R^2 mean     = %.4f  (min %.4f, max %.4f)\n"
    (V.sum r2v / fromIntegral q)
    (V.minimum r2v) (V.maximum r2v)

  let xObs   = V.toList doseV
      yObs   = [ [ (yCols !! j) V.! i | j <- [0 .. q - 1] ]
               | i <- [0 .. n - 1] ]
      alpha2 = [ LA.toList (LA.flatten (K.krmAlpha fit LA.? [i]))
               | i <- [0 .. n - 1] ]   -- n × q (行抽出)
      dMin = minimum xObs - 2.0
      dMax = maximum xObs + 2.0
      dMid = 0.5 * (dMin + dMax)
      imo  = mkInteractiveMOKernelRBF "dose" "potential V" "z [nm]"
                                      zGrid xObs yObs
                                      xObs alpha2 bestH
                                      (dMin, dMid, dMax)
      sections =
        [ secModelOverview "Multi-output Kernel Ridge (RBF)"
            "$\\hat{y}_j(d) = \\sum_i \\exp(-\\frac{(d-d_i)^2}{2h^2}) \\, \\alpha_{ij}$"
            Nothing
        , secStatRow
            [ ("N", T.pack (show n))
            , ("q (outputs)", T.pack (show q))
            , ("best h", T.pack (printf "%.3f" bestH))
            , ("best λ", T.pack (printf "%.2g" bestL))
            , ("LOO MSE", T.pack (printf "%.4g" looMSE))
            , ("RMSE", T.pack (printf "%.4f" rmse))
            , ("R^2 mean", T.pack (printf "%.4f"
                (V.sum r2v / fromIntegral q :: Double)))
            ]
        , secInteractiveMultiOut "予測曲線 (dose スライダ)" imo
        ]
      cfg = defaultReportConfig "Potential — Multi-output Kernel Ridge (RBF)"
  renderReport "trash/potential_multikr.html" cfg sections
  putStrLn "Wrote trash/potential_multikr.html"
