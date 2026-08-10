{-# LANGUAGE OverloadedStrings #-}
-- | wells_data-wells_interaction_model (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。バングラデシュ井戸水ヒ素汚染
-- 調査「井戸の切替行動」の交互作用ロジスティック回帰 (Gelman & Hill
-- 2006)。距離(dist)とヒ素濃度(arsenic)の交互作用項を含む二項ロジット。
--
-- Stan 原典 (posteriordb `models/stan/wells_interaction_model.stan`):
--   transformed data { dist100 = dist/100; inter = dist100 .* arsenic; }
--   switched ~ bernoulli_logit_glm([dist100, arsenic, inter], alpha, beta);
--   (alpha/beta に明示的priorなし = 暗黙のflat prior)
--
-- 明示的priorが無い暗黙flatは01-glm-poisson/10-ratsと同じ流儀で
-- Normal(0,1000)に置換。 hanalyze の `Bernoulli` は確率パラメータ直接
-- 指定のため 02-dogs/19-surgical と同じく invlogit を手計算する。
--
-- reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
--
-- ★2026-07-12: コード準備のみ (データ未取得・ビルド確認は次回)。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-wells
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

-- | posteriordb の @wells_data.json@ 形状 ({"N":...,"switched":[...],
-- "dist":[...],"arsenic":[...]})。
data WellsData = WellsData
  { wSwitched :: [Int]
  , wDist     :: [Double]
  , wArsenic  :: [Double]
  }

instance FromJSON WellsData where
  parseJSON = withObject "WellsData" $ \v ->
    WellsData <$> v .: "switched" <*> v .: "dist" <*> v .: "arsenic"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/26-wells/data/wells_data.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/26-wells/figures"

readData :: IO WellsData
readData = either fail pure =<< eitherDecodeFileStrict dataPath

-- | 距離×ヒ素濃度の交互作用ロジスティック回帰 (Stan 原典と同一構造)。
wellsModel :: ModelP ()
wellsModel = do
  alpha <- sample "alpha" (Normal 0 1000)
  beta1 <- sample "beta1" (Normal 0 1000)
  beta2 <- sample "beta2" (Normal 0 1000)
  beta3 <- sample "beta3" (Normal 0 1000)
  dist    <- dataNamedX   "dist"    []
  arsenic <- dataNamedX   "arsenic" []
  ys      <- dataNamedObs "switched" []
  plateForM_ "obs" (zip3 dist arsenic ys) $ \(d, ar, yVal) ->
    let dist100 = d / 100
        inter   = dist100 * ar
        logit   = alpha + beta1 * dist100 + beta2 * ar + beta3 * inter
        p       = 1 / (1 + exp (negate logit))
    in observe "switched" (Bernoulli p) [yVal]

main :: IO ()
main = do
  d <- readData
  let dist100Col = map (/ 100) (wDist d)
      interCol   = zipWith (*) dist100Col (wArsenic d)
      df = [ ("dist",     NumData (V.fromList (wDist d)))
           , ("arsenic",  NumData (V.fromList (wArsenic d)))
           , ("inter",    NumData (V.fromList interCol))
           , ("switched", NumData (V.fromList (map fromIntegral (wSwitched d))))
           ] :: [(T.Text, ColData)]
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      m = df |-> hbm cfg wellsModel

  putStrLn $ "synthVecIR = " ++
    (case synthVecIR wellsModel of
       Just _  -> "Just (vecIR高速経路)"
       Nothing -> "Nothing (legacy walk+ad)")

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardFullOf m "switched" :: BoundPlot)

  printSummary $ summarize ["alpha", "beta1", "beta2", "beta3"] (hbmChainsR m)
