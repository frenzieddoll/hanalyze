{-# LANGUAGE OverloadedStrings #-}
-- | Cycle 1 と Cycle 9 で追加した計 7 つの新セクション
-- (`secComparisonTable` / `secForestPlot` / `secFeatureImportance` / `secPPC`
--  + `secCalibration` / `sec3DScatter` / `secHeatmap`)
-- を 1 つのレポートで端から端まで使うショーケース。
--
-- 動作:
--   1. data/regression/test_lm.csv を読込
--   2. LM / GAM / RF (Random Forest) でフィット
--   3. 各モデルの RMSE / R² を 'secComparisonTable' で比較 (最良行ハイライト)
--   4. LM の β₀, β₁ について漸近 95% CI を 'secForestPlot' で可視化
--   5. RF の `featureImportance` を 'secFeatureImportance' で表示
--   6. LM の予測分布から 30 個の posterior-predictive 風サンプルを生成し
--      'secPPC' で観測値と重ね描き
--
-- 出力: trash/new_sections_demo.html
module Main where

import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import System.Random.MWC (createSystemRandom, GenIO)
import qualified System.Random.MWC as MWC
import Text.Printf (printf)
import qualified Data.Text as T
import Control.Monad (replicateM)

import DataIO.CSV          (loadAuto)
import DataFrame.Core      (getNumeric)
import qualified Model.LM  as LM
import qualified Model.GAM as GAM
import qualified Model.RandomForest as RF
import Model.Core          (coeffList, fittedList, residualsV, rSquared1)

import qualified Viz.ReportBuilder as RB

-- ---------------------------------------------------------------------------
-- ヘルパ
-- ---------------------------------------------------------------------------

rmseOf :: [Double] -> [Double] -> Double
rmseOf ys yh =
  let n = length ys
      r = zipWith (-) ys yh
  in sqrt (sum [ x * x | x <- r ] / fromIntegral (max 1 n))

r2Of :: [Double] -> [Double] -> Double
r2Of ys yh =
  let yBar = sum ys / fromIntegral (max 1 (length ys))
      tss  = sum [ (y - yBar) ^ (2 :: Int) | y <- ys ]
      rss  = sum [ (y - h)    ^ (2 :: Int) | (y, h) <- zip ys yh ]
  in if tss < 1e-12 then 0 else 1 - rss / tss

-- | 平均 0、SD σ のガウス乱数 (Box-Muller)。
gaussian :: Double -> GenIO -> IO Double
gaussian sigma gen = do
  u1 <- MWC.uniform gen
  u2 <- MWC.uniform gen
  let z = sqrt (-2 * log (max 1e-12 u1)) * cos (2 * pi * u2)
  return (sigma * z)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (p:rs) = quickSort [x | x <- rs, x <= p]
                ++ [p]
                ++ quickSort [x | x <- rs, x > p]

-- ---------------------------------------------------------------------------
-- メイン
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "============================================================"
  putStrLn " New Sections Demo"
  putStrLn " (secComparisonTable / secForestPlot /"
  putStrLn "  secFeatureImportance / secPPC)"
  putStrLn "============================================================"

  Right df <- loadAuto "data/regression/test_lm.csv"
  let Just xVec = getNumeric "x" df
      Just yVec = getNumeric "y" df
      xs = V.toList xVec
      ys = V.toList yVec
      n  = length xs

  -- LM フィット
  let xMat = LA.fromColumns [LA.konst 1 n, LA.fromList xs]
      yLA  = LA.fromList ys
      lmFit = LM.fitLMVec xMat yLA
      lmYhat = fittedList lmFit
      lmRMSE = rmseOf ys lmYhat
      lmR2   = rSquared1 lmFit
      lmBeta = coeffList lmFit
      lmResid = LA.toList (residualsV lmFit)
      sigmaHat = sqrt (sum [ r * r | r <- lmResid ]
                       / fromIntegral (max 1 (n - 2)))
      -- (XᵀX)⁻¹ で漸近 SE を計算
      xtx    = LA.tr xMat LA.<> xMat
      xtxInv = LA.inv xtx
      diagXtxInv = LA.toList (LA.takeDiag xtxInv)
      seBeta = [ sigmaHat * sqrt v | v <- diagXtxInv ]

  -- GAM フィット
  let gamFit = GAM.fitGAM 3 5 0.01 [xVec] yVec
      gamYhat = LA.toList (GAM.gamYHat gamFit)
      gamRMSE = rmseOf ys gamYhat
      gamR2_  = GAM.gamR2 gamFit

  -- RF フィット
  gen <- createSystemRandom
  let rows = [[x] | x <- xs]
  rf <- RF.fitRF RF.defaultRFConfig rows ys gen
  let rfYhat = [ RF.predictRF rf row | row <- rows ]
      rfRMSE = rmseOf ys rfYhat
      rfR2   = r2Of ys rfYhat
      rfImport = V.toList (RF.featureImportance rf)

  printf "  LM:   RMSE = %.4f, R² = %.4f\n" lmRMSE lmR2
  printf "  GAM:  RMSE = %.4f, R² = %.4f\n" gamRMSE gamR2_
  printf "  RF:   RMSE = %.4f, R² = %.4f\n" rfRMSE rfR2

  -- 4 モデル比較行 + 最良 (lowest RMSE) 行のインデックス
  let cmpHeaders = ["モデル", "RMSE", "R²"]
      cmpRows =
        [ ["LM",  T.pack (printf "%.4f" lmRMSE),  T.pack (printf "%.4f" lmR2)]
        , ["GAM", T.pack (printf "%.4f" gamRMSE), T.pack (printf "%.4f" gamR2_)]
        , ["RF",  T.pack (printf "%.4f" rfRMSE),  T.pack (printf "%.4f" rfR2)]
        ]
      bestIdx =
        let rmses = [lmRMSE, gamRMSE, rfRMSE]
            mn = minimum rmses
        in length (takeWhile (/= mn) rmses)

  -- Forest plot: LM の β₀, β₁ について 95% CI = mean ± 1.96 · SE
  let forestRows =
        [ ("β₀ (intercept)",
            head lmBeta - 1.96 * head seBeta,
            head lmBeta,
            head lmBeta + 1.96 * head seBeta)
        , ("β₁ (x)",
            (lmBeta !! 1) - 1.96 * (seBeta !! 1),
            lmBeta !! 1,
            (lmBeta !! 1) + 1.96 * (seBeta !! 1))
        ]

  -- Feature importance: 1 特徴 (x) のみ
  let importPairs = zip ["x"] rfImport

  -- Posterior Predictive Check: LM 予測分布から 30 replicate 生成
  -- y_rep_i ~ Normal(β₀ + β₁ x_i, σ̂)
  reps <- replicateM 30 $ do
    eps <- mapM (\_ -> gaussian sigmaHat gen) xs
    return (zipWith (+) lmYhat eps)

  -- Calibration: LM yhat を sigmoid で 0..1 に圧縮 → 予測確率、観測 = (y > median) の二値
  let medY = let s = quickSort ys in s !! (length s `div` 2)
      pPred = [ 1 / (1 + exp (-(h - medY))) | h <- lmYhat ]
      yBin  = [ if y > medY then 1 else 0 | y <- ys ]

  -- 3D scatter: (x, yhat, residual)
  let zs3d = lmResid

  -- Heatmap: 3 モデルの (RMSE, R², 1-R²) を 3×3 メトリック行列として表示
  let heatRows  = ["LM", "GAM", "RF"]
      heatCols  = ["RMSE", "R²", "1−R²"]
      heatVals  =
        [ [lmRMSE,  lmR2,   1 - lmR2]
        , [gamRMSE, gamR2_, 1 - gamR2_]
        , [rfRMSE,  rfR2,   1 - rfR2]
        ]

  -- レポート組立
  let cfg = RB.defaultReportConfig
              "新セクション 7 種ショーケース (Comparison / Forest / Importance / PPC / Calibration / 3D / Heatmap)"
      sections =
        [ RB.secMarkdown "概要"
            (T.unlines
              [ "Cycle 1 + Cycle 9 で `Viz.ReportBuilder` に追加した計 7 つのセクションを"
              , "1 つのレポートで使うデモ。"
              , ""
              , "データ: `data/regression/test_lm.csv` (50 行、x, y 二列)。"
              , "LM / GAM / RandomForest の 3 モデルをフィットして RMSE/R² を比較し、"
              , "LM の係数 95% CI を Forest plot、RF の特徴量重要度をバーで表示、"
              , "LM の予測分布からの replicate を観測と重ね描きで表示する。"
              , "さらに Calibration plot / 3D scatter / Heatmap を順に追加。"
              ])
        , RB.secComparisonTable
            "モデル比較 (RMSE 最小行をハイライト)"
            cmpHeaders cmpRows (Just bestIdx)
        , RB.secForestPlot "LM 係数の漸近 95% CI" forestRows
        , RB.secFeatureImportance "Random Forest 特徴量重要度" importPairs
        , RB.secPPC "Posterior Predictive Check (LM 予測分布、30 replicate)"
            ys reps
        , RB.secCalibration
            "Calibration plot (sigmoid(yhat - median y) vs (y > median))"
            pPred (map fromIntegral yBin)
        , RB.sec3DScatter
            "3D scatter (擬似: x / yhat / 残差を色エンコード)"
            "x" "yhat" "residual" xs lmYhat zs3d
        , RB.secHeatmap
            "モデル × メトリック ヒートマップ (値の色で大小表現)"
            heatCols heatRows heatVals
        ]

  RB.renderReport "trash/new_sections_demo.html" cfg sections
  putStrLn ""
  putStrLn "Report: trash/new_sections_demo.html"
