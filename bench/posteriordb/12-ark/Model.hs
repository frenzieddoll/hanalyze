{-# LANGUAGE OverloadedStrings #-}
-- | arK-arK (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。AR(K) (K次自己回帰) 時系列モデル
-- (K=5・T=200)。GARCH (03-garch11) と異なり **分散ではなく平均のみが過去に
-- 依存する**ため、全ての y は既知データであり、モデルとしては「K個のラグ
-- 特徴量を使った静的な線形回帰」に帰着する (潜在変数間の自己参照的な
-- 再帰は存在しない)。
--
-- Stan 原典 (posteriordb `models/stan/arK.stan`):
--   parameters { real alpha; array[K] real beta; real<lower=0> sigma; }
--   model {
--     alpha ~ normal(0, 10); beta ~ normal(0, 10); sigma ~ cauchy(0, 2.5);
--     for (t in (K+1):T) {
--       mu = alpha + sum_{k=1}^{K} beta[k]*y[t-k];
--       y[t] ~ normal(mu, sigma);
--     }
--   }
--
-- `sigma ~ cauchy(0, 2.5)` (下限0の半コーシー) は hanalyze の `HalfCauchy`
-- (`PositiveT` 変換) にそのまま対応する (10-rats の Uniform-SD 罠に
-- 該当しない・09-eight-schools の tau と同型)。
--
-- **reference_posterior_name = "arK-arK"** (posteriordb に公式 reference
-- あり・hanalyze vs PyMC vs 公式referenceの3者比較が可能)。
--
-- K 個のラグ特徴量 (@lag1@..@lagK@) を Haskell 側で事前計算し、df 列として
-- 束縛する (dataNamedX を K 回呼ぶ)。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-ark
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Printf (printf)

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedX, dataNamedObs, plateI, plateForM_, (.#),
                                    gradPathLabel)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardFullOf, hbmChainsR, hbmModelSpec)
import Hgg.Plot.Spec (ColData (..))
import Hgg.Plot.Frame (BoundPlot, (|>>))
import Hgg.Plot.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @arK.json@ 形状 ({"K":5, "T":200, "y":[...]})。
data ArKData = ArKData { kLag :: Int, tLen :: Int, yArr :: [Double] }

instance FromJSON ArKData where
  parseJSON = withObject "ArKData" $ \v ->
    ArKData <$> v .: "K" <*> v .: "T" <*> v .: "y"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/12-ark/data/arK.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/12-ark/figures"

readData :: IO ArKData
readData = either fail pure =<< eitherDecodeFileStrict dataPath

-- | ラグ特徴量 (t=K..T-1 の各観測に対し @y[t-1]..y[t-K]@) と目的変数
-- (@y[K]..y[T-1]@) を事前計算する。0始まりのインデックス。
lagDesign :: Int -> [Double] -> ([[Double]], [Double])
lagDesign k ys =
  let yv = V.fromList ys
      n  = V.length yv
      obsIdx = [k .. n - 1]
      targets = [ yv V.! t | t <- obsIdx ]
      lags = [ [ yv V.! (t - lg) | t <- obsIdx ] | lg <- [1 .. k] ]  -- lags!!(lg-1)
  in (lags, targets)

-- | AR(K) 静的線形回帰 (ラグ特徴量は全て既知データ)。
arKModel :: Int -> ModelP ()
arKModel k = do
  alpha <- sample "alpha" (Normal 0 10)
  betas <- plateI "beta" k $ \j -> sample ("beta" .# (j + 1)) (Normal 0 10)
  sigma <- sample "sigma" (HalfCauchy 2.5)
  lagCols <- mapM (\lg -> dataNamedX (T.pack ("lag" ++ show lg)) []) [1 .. k]
  ys <- dataNamedObs "y_obs" []
  plateForM_ "obs" (zip [0 ..] ys) $ \(i, yi) ->
    let mu = alpha + sum [ (betas !! (lg - 1)) * ((lagCols !! (lg - 1)) !! i) | lg <- [1 .. k] ]
    in observe "y_obs" (Normal mu sigma) [yi]

main :: IO ()
main = do
  d <- readData
  let (lags, targets) = lagDesign (kLag d) (yArr d)
      lagDf = [ (T.pack ("lag" ++ show lg), NumData (V.fromList col))
              | (lg, col) <- zip [1 :: Int ..] lags ]
      df = ("y_obs", NumData (V.fromList targets)) : lagDf
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      m = df |-> hbm cfg (arKModel (kLag d))

  -- 勾配経路 = compileGradUV が実際に選ぶ経路 (束縛済モデルで判定)。
  -- Phase 91 A4: AR(K) は静的ラグ線形回帰 = Gaussian LM 閉形式ブロックに吸収。
  -- ★生モデルを synthVecIR に渡すと data 空で Nothing と誤表示するため
  --   'hbmModelSpec m' (df 束縛済) を 'gradPathLabel' に渡す。
  putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardFullOf m "y_obs" :: BoundPlot)

  printSummary $ summarize ["alpha", "beta_1", "beta_2", "beta_3", "beta_4", "beta_5", "sigma"] (hbmChainsR m)
