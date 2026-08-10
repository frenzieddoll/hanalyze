{-# LANGUAGE OverloadedStrings #-}
-- | Rate_1_data-Rate_1_model (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。Bayesian Cognitive Modeling
-- (Lee & Wagenmakers 2013) の最も単純な例「二項比率の推定」
-- (n=10試行・k=5成功)。★新ファミリ: 単一パラメータの共役Beta-Binomial
-- (これまでで最も単純なモデル・他モデルとの対比用ベースライン)。
--
-- Stan 原典 (posteriordb `models/stan/Rate_1_model.stan`):
--   theta ~ beta(1,1); k ~ binomial(n, theta);
--
-- reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-rate1
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import Text.Printf (printf)

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe)
import Hanalyze.Model.HBM.IR (synthVecIR)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardFullOf, hbmChainsR)
import Graphics.Hgg.Spec (ColData (..))
import Graphics.Hgg.Frame (BoundPlot, (|>>))
import Graphics.Hgg.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @Rate_1_data.json@ 形状 ({"n":10,"k":5})。
data Rate1Data = Rate1Data { r1N :: Int, r1K :: Int }

instance FromJSON Rate1Data where
  parseJSON = withObject "Rate1Data" $ \v ->
    Rate1Data <$> v .: "n" <*> v .: "k"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/23-rate1/data/Rate_1_data.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/23-rate1/figures"

readData :: IO Rate1Data
readData = either fail pure =<< eitherDecodeFileStrict dataPath

-- | 単一比率の共役Beta-Binomial (Stan 原典と同一構造)。 n/k は
-- 微分対象ではない定数なので closure で直接渡す。
rate1Model :: Int -> Int -> ModelP ()
rate1Model n k = do
  theta <- sample "theta" (Beta 1 1)
  observe "k" (Binomial n theta) [fromIntegral k]

main :: IO ()
main = do
  d <- readData
  let cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      model :: ModelP ()
      model = rate1Model (r1N d) (r1K d)
      m = noDf |-> hbm cfg model

  putStrLn $ "synthVecIR = " ++
    (case synthVecIR model of
       Just _  -> "Just (vecIR高速経路)"
       Nothing -> "Nothing (legacy walk+ad)")

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardFullOf m "k" :: BoundPlot)

  printSummary $ summarize ["theta"] (hbmChainsR m)
