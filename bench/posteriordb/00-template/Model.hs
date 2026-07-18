{-# LANGUAGE OverloadedStrings #-}
-- | TODO(<posteriordb-name>) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク雛形。00-template は
-- posteriordb-bench skill の「NN-<slug>/ をコピーして名前を置換」用の
-- スケルトンであり、単体ではビルド対象に含めない (hanalyze.cabal に
-- executable スタンザを追加しない)。実例は ../01-glm-poisson/Model.hs を参照。
--
-- 高レベル API (`df |-> hbm`) を使用: データは 'dataNamedX'/'dataNamedObs' で
-- df から束縛し、 反復は 'plateForM_'/'plateI_' で書く (docs/api-guide/03-bayesian-hbm.md
-- の規約どおり・素の forM_ は使わない)。 診断図は hgg の
-- 'dashboardFullOf' を 1 枚の PNG (rasterific backend) として出力する。
-- chain 数等の設定は PyMC 側 (model.py) と同じくコード中の定数で揃える
-- (CLI 引数化しない)。
--
-- reference_posterior_name: TODO (posteriordb の posteriors/<name>.json を確認)
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-<slug>
--         (00-template 自体は buildable にしない — NN-<slug>/ にコピーしてから追加する)
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedX, dataNamedObs, plateForM_)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardFullOf, hbmChainsR)
import Graphics.Hgg.Spec (ColData (..))
import Graphics.Hgg.Frame (BoundPlot, (|>>))
import Graphics.Hgg.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)
import Text.Printf (printf)

-- | TODO: posteriordb の <data_name>.json 形状に合わせてフィールドを定義する
-- (例: {"x":[...], "y":[...], "n":10})。 JSON キーが大文字始まりのことが
-- あるので (例 "C") withObject + (.:) の明示 instance で吸収する (Generic 不可)。
data TemplateData = TemplateData
  { x :: [Double]
  , y :: [Double]
  }

instance FromJSON TemplateData where
  parseJSON = withObject "TemplateData" $ \v ->
    TemplateData <$> v .: "x" <*> v .: "y"

noDf :: [(T.Text, ColData)]
noDf = []

-- | TODO: NN-<slug> に置換する。
dataPath :: FilePath
dataPath = "bench/posteriordb/00-template/data/template_data.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/00-template/figures"

readData :: IO ([Double], [Double])
readData = do
  d <- either fail pure =<< eitherDecodeFileStrict dataPath
  pure (x d, y d)

-- | TODO: posteriordb Stan モデルの構造を移植する。
templateModel :: ModelP ()
templateModel = do
  alpha <- sample "alpha" (Uniform (-20) 20)
  beta  <- sample "beta" (Uniform (-10) 10)
  xs    <- dataNamedX   "x" []
  ys    <- dataNamedObs "y" []
  plateForM_ "obs" (zip xs ys) $ \(xi, yi) ->
    observe "y" (Normal (alpha + beta * xi) 1) [yi]

main :: IO ()
main = do
  (xs, ys) <- readData
  let df = [ ("x", NumData (V.fromList xs))
           , ("y", NumData (V.fromList ys))
           ] :: [(T.Text, ColData)]
      -- PyMC 側 (model.py) と同じ設定を定数で揃える。
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      m = df |-> hbm cfg templateModel

  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardFullOf m "y" :: BoundPlot)

  printSummary $ summarize ["alpha", "beta"] (hbmChainsR m)
