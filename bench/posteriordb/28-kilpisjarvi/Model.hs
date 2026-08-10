{-# LANGUAGE OverloadedStrings #-}
-- | kilpisjarvi_mod-kilpisjarvi (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。キルピスヤルヴィ (フィン
-- ランド) の気候変動観測データに対する単純な線形回帰。★新ファミリ:
-- **prior のハイパーパラメータ自体がデータとして与えられる** (これまでの
-- モデルは prior の数値をコード中の定数として書いていたが、本モデルは
-- `pmualpha`/`psalpha`/`pmubeta`/`psbeta` をJSONデータから読み込んで
-- prior に渡す)。reference_posterior_name あり (3者比較可能)。
--
-- Stan 原典 (posteriordb `models/stan/kilpisjarvi.stan`):
--   data { ... real pmualpha; real psalpha; real pmubeta; real psbeta; ... }
--   alpha ~ normal(pmualpha, psalpha);
--   beta  ~ normal(pmubeta, psbeta);
--   y ~ normal(alpha + beta*x, sigma);
--   (sigma に明示的priorなし = 暗黙のflat prior。real<lower=0>のため
--   01-glm-poisson/10-ratsと同じ流儀でHalfCauchy(25)に置換)
--
-- reference_posterior_name = "kilpisjarvi_mod-kilpisjarvi" (posteriordb
-- に公式referenceあり・hanalyze vs PyMC vs 公式referenceの3者比較可能)。
--
-- ★2026-07-12: コード準備のみ (データ未取得・ビルド確認は次回)。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-kilpisjarvi
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Printf (printf)

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedX, dataNamedObs, plateForM_)
import Hanalyze.Model.HBM.IR (synthVecIR)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardFullOf, hbmChainsR)
import Graphics.Hgg.Spec (ColData (..))
import Graphics.Hgg.Frame (BoundPlot, (|>>))
import Graphics.Hgg.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @kilpisjarvi_mod.json@ 形状 ({"N":...,"x":[...],
-- "y":[...],"xpred":...,"pmualpha":...,"psalpha":...,"pmubeta":...,
-- "psbeta":...}・xpredは本モデルでは未使用)。
data KilpisjarviData = KilpisjarviData
  { kjX        :: [Double]
  , kjY        :: [Double]
  , kjPmuAlpha :: Double
  , kjPsAlpha  :: Double
  , kjPmuBeta  :: Double
  , kjPsBeta   :: Double
  }

instance FromJSON KilpisjarviData where
  parseJSON = withObject "KilpisjarviData" $ \v ->
    KilpisjarviData <$> v .: "x" <*> v .: "y"
                     <*> v .: "pmualpha" <*> v .: "psalpha"
                     <*> v .: "pmubeta"  <*> v .: "psbeta"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/28-kilpisjarvi/data/kilpisjarvi_mod.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/28-kilpisjarvi/figures"

readData :: IO KilpisjarviData
readData = either fail pure =<< eitherDecodeFileStrict dataPath

-- | 単純な線形回帰・prior のハイパーパラメータはデータから渡される
-- (Stan 原典と同一構造)。
kilpisjarviModel :: Double -> Double -> Double -> Double -> ModelP ()
kilpisjarviModel pmuA psA pmuB psB = do
  alpha <- sample "alpha" (Normal (realToFrac pmuA) (realToFrac psA))
  beta  <- sample "beta"  (Normal (realToFrac pmuB) (realToFrac psB))
  sigma <- sample "sigma" (HalfCauchy 25)
  xs <- dataNamedX   "x" []
  ys <- dataNamedObs "y" []
  plateForM_ "obs" (zip xs ys) $ \(xi, yi) ->
    observe "y" (Normal (alpha + beta * xi) sigma) [yi]

main :: IO ()
main = do
  d <- readData
  let df = [ ("x", NumData (V.fromList (kjX d)))
           , ("y", NumData (V.fromList (kjY d)))
           ] :: [(T.Text, ColData)]
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      model :: ModelP ()
      model = kilpisjarviModel (kjPmuAlpha d) (kjPsAlpha d) (kjPmuBeta d) (kjPsBeta d)
      m = df |-> hbm cfg model

  putStrLn $ "synthVecIR = " ++
    (case synthVecIR model of
       Just _  -> "Just (vecIR高速経路)"
       Nothing -> "Nothing (legacy walk+ad)")

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardFullOf m "y" :: BoundPlot)

  printSummary $ summarize ["alpha", "beta", "sigma"] (hbmChainsR m)
