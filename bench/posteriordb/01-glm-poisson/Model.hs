{-# LANGUAGE OverloadedStrings #-}
-- | GLM_Poisson_Data-GLM_Poisson_model (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。BPA (Kery & Schaub 2011, Ch.3) の
-- 個体数カウントデータ (n=40年) を3次多項式 Poisson 回帰でモデル化する。
-- Stan 原典の "暗黙の一様事前分布" (bounded, no explicit prior) を
-- 'Uniform' distribution で忠実に移植する。
--
-- 高レベル API (`df |-> hbm`) を使用: データは 'dataNamedX'/'dataNamedObs' で
-- df から束縛し、 反復は 'plateForM_' で書く (docs/api-guide/03-bayesian-hbm.md
-- の規約どおり)。 診断図は hgg の 'dashboardFullOf' (構造 DAG /
-- forest / PPC / energy の 2x2 + param ごと [事後分布|trace]) を 1 枚の PNG
-- (rasterific backend) として出力する (PyMC 側の合成ダッシュボード
-- `_common.make_pymc_dashboard` と対)。 chain 数等の設定は PyMC 側
-- (`model.py`) と同じくコード中の定数 (= 4) で揃える (CLI 引数化しない)。
--
-- reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-glm-poisson
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedX, dataNamedObs, plateForM_)
import Hanalyze.Model.HBM (gradPathLabel)
import Hanalyze.Plot (hbmModelSpec)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardFullOf, hbmChainsR)
import Graphics.Hgg.Spec (ColData (..))
import Graphics.Hgg.Frame (BoundPlot, (|>>))
import Graphics.Hgg.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary)

-- | posteriordb の @GLM_Poisson_Data.json@ 形状 ({"year":[...], "C":[...], "n":40})。
data GlmPoissonData = GlmPoissonData
  { year :: [Double]
  , c    :: [Int]
  }

instance FromJSON GlmPoissonData where
  parseJSON = withObject "GlmPoissonData" $ \v ->
    GlmPoissonData <$> v .: "year" <*> v .: "C"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/01-glm-poisson/data/GLM_Poisson_Data.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/01-glm-poisson/figures"

readData :: IO ([Double], [Int])
readData = do
  d <- either fail pure =<< eitherDecodeFileStrict dataPath
  pure (year d, c d)

-- | 3次多項式 Poisson 回帰 (Stan 原典と同一構造)。 データは df 経由で束縛
-- ('dataNamedX'/'dataNamedObs')・反復は 'plateForM_' (docs/api-guide 規約)。
glmPoissonModel :: ModelP ()
glmPoissonModel = do
  alpha <- sample "alpha" (Uniform (-20) 20)
  beta1 <- sample "beta1" (Uniform (-10) 10)
  beta2 <- sample "beta2" (Uniform (-10) 10)
  beta3 <- sample "beta3" (Uniform (-10) 10)
  years <- dataNamedX   "year" []
  cs    <- dataNamedObs "C"    []
  plateForM_ "obs" (zip years cs) $ \(y, c) ->
    let y2 = y * y
        y3 = y2 * y
        logLambda = alpha + beta1 * y + beta2 * y2 + beta3 * y3
    in observe "C" (Poisson (exp logLambda)) [c]

main :: IO ()
main = do
  (years, cs) <- readData
  let df = [ ("year", NumData (V.fromList years))
           , ("C",    NumData (V.fromList (map fromIntegral cs)))
           ] :: [(T.Text, ColData)]
      -- PyMC 側 (model.py) と同じ設定を定数で揃える (draws/tune=1000×4chain)。
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      m = df |-> hbm cfg glmPoissonModel

  -- Phase 96 A2: 勾配経路 = compileGradUV が実際に選ぶ経路 (束縛済
  -- hbmModelSpec で判定・Phase 91 A4 と同型)。
  putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

  -- 診断図 (hgg・PNG・SVG は param 数×draw 数でファイルが重くなる
  -- ため rasterific backend を使う)。 dashboardFullOf 1 枚 (構造+推定+PPC+
  -- 健全性の 2x2 + param ごと [事後分布|trace])。 PyMC 側の
  -- py_dashboard_full.png と対 (trace は dashboardFullOf に必ず含まれる)。
  -- figures/ は事前に用意されている前提 (git 管理下・新規モデル作成時に
  -- 1 度だけ作る) — 実行のたびに作り直さない。
  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardFullOf m "C" :: BoundPlot)

  -- 事後要約 (az.summary 相当・Common.summarize)。
  printSummary $ summarize ["alpha", "beta1", "beta2", "beta3"] (hbmChainsR m)
