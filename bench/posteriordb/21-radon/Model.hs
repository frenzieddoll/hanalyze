{-# LANGUAGE OverloadedStrings #-}
-- | radon_mn-radon_hierarchical_intercept_noncentered (posteriordb) —
-- hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。Gelman ラドン多水準回帰の
-- 古典例 (mc-stan.org radon case study)。ミネソタ州 J=85郡・N=919家屋の
-- 屋内ラドン濃度回帰 (郡ごとの varying intercept + 固定傾き2本、
-- non-centered パラメタ化)。
--
-- Stan 原典 (posteriordb `models/stan/radon_hierarchical_intercept_noncentered.stan`):
--   parameters { vector[J] alpha_raw; vector[2] beta; real mu_alpha;
--                real<lower=0> sigma_alpha; real<lower=0> sigma_y; }
--   transformed parameters { alpha = mu_alpha + sigma_alpha * alpha_raw; }
--   model {
--     sigma_alpha ~ normal(0,1); sigma_y ~ normal(0,1);
--     mu_alpha ~ normal(0,10); beta ~ normal(0,10); alpha_raw ~ normal(0,1);
--     for (n in 1:N) {
--       muj[n] = alpha[county_idx[n]] + log_uppm[n]*beta[1];
--       mu[n] = muj[n] + floor_measure[n]*beta[2];
--       log_radon[n] ~ normal(mu[n], sigma_y);
--     }
--   }
--
-- `sigma_alpha`/`sigma_y` の `<lower=0>` + `Normal(0,1)` prior は
-- half-normal と数学的に等価なので `HalfNormal 1` で移植 (09-eight-schools
-- 等と同じ流儀)。`alpha[j]` は `deterministic` (mu_alpha+sigma_alpha*
-- alpha_raw[j]) で登録し `Data.Vector` 経由で O(1) 索引する
-- (`county_idx` は1-based構造添字なのでclosureで直接渡す・05-mh/16-lda
-- と同じ流儀)。単一階層 (alphaのみ) なので10-ratsの「二重階層」DSL
-- ギャップは該当しない — eight-schools/seedsと同型の構造でvecIR成功が
-- 見込める規模 (J=85・N=919)。
--
-- reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-radon
module Main (main) where

import Control.Monad (unless)
import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Environment (getArgs)
import Text.Printf (printf)

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedX, dataNamedObs, plateI, plateForM_,
                                    (.#), deterministic, augmentChainWithDeterministic)
import Hanalyze.Model.HBM (gradPathLabel)
import Hanalyze.Plot (hbmModelSpec)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardOf, hbmChainsR)
import Hgg.Plot.Spec (ColData (..))
import Hgg.Plot.Frame (BoundPlot, (|>>))
import Hgg.Plot.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @radon_mn.json@ 形状 ({"N":919,"J":85,
-- "floor_measure":[...],"log_radon":[...],"log_uppm":[...],
-- "county_idx":[...]}・county_idx は 1-based)。
data RadonData = RadonData
  { rdJ            :: Int
  , rdCountyIdx    :: [Int]
  , rdFloorMeasure :: [Double]
  , rdLogRadon     :: [Double]
  , rdLogUppm      :: [Double]
  }

instance FromJSON RadonData where
  parseJSON = withObject "RadonData" $ \v ->
    RadonData <$> v .: "J" <*> v .: "county_idx" <*> v .: "floor_measure"
              <*> v .: "log_radon" <*> v .: "log_uppm"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/21-radon/data/radon_mn.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/21-radon/figures"

readData :: IO RadonData
readData = either fail pure =<< eitherDecodeFileStrict dataPath

-- | 郡ごとの varying intercept (non-centered) + 固定傾き2本の階層回帰
-- (Stan 原典と同一構造)。 @countyIdx@ は微分対象ではない構造的添字
-- (1-based) なので closure で直接渡す。
radonModel :: Int -> [Int] -> ModelP ()
radonModel nCounty countyIdx = do
  muAlpha    <- sample "mu_alpha"    (Normal 0 10)
  sigmaAlpha <- sample "sigma_alpha" (HalfNormal 1)
  sigmaY     <- sample "sigma_y"     (HalfNormal 1)
  beta1      <- sample "beta1" (Normal 0 10)
  beta2      <- sample "beta2" (Normal 0 10)
  alphaRaws  <- plateI "county" nCounty $ \j -> sample ("alpha_raw" .# j) (Normal 0 1)
  alphas     <- mapM (\(j, ar) -> deterministic ("alpha" .# j) (muAlpha + sigmaAlpha * ar))
                      (zip [0 :: Int ..] alphaRaws)
  let alphaV = V.fromList alphas
  logUppm      <- dataNamedX   "log_uppm"      []
  floorMeasure <- dataNamedX   "floor_measure" []
  ys           <- dataNamedObs "log_radon"     []
  plateForM_ "obs" (zip4' countyIdx logUppm floorMeasure ys) $ \(cid, uppm, floorM, yVal) ->
    let mu = (alphaV V.! (cid - 1)) + uppm * beta1 + floorM * beta2
    in observe "log_radon" (Normal mu sigmaY) [yVal]
  where
    zip4' (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4' as bs cs ds
    zip4' _ _ _ _ = []

main :: IO ()
main = do
  d <- readData
  -- Phase 102 A1: `prof` 引数で図出力 skip (15-dugongs と同じ理由。
  -- サンプリング設定は本番のまま)。
  args <- getArgs
  let profRun = elem "prof" args
  let df = [ ("log_uppm",      NumData (V.fromList (rdLogUppm d)))
           , ("floor_measure", NumData (V.fromList (rdFloorMeasure d)))
           , ("log_radon",     NumData (V.fromList (rdLogRadon d)))
           ] :: [(T.Text, ColData)]
      -- PyMC 側 (model.py) と同じ設定を定数で揃える。
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      model :: ModelP ()
      model = radonModel (rdJ d) (rdCountyIdx d)
      m = df |-> hbm cfg model

  -- 勾配経路 = compileGradUV が実際に選ぶ経路 (束縛済 hbmModelSpec で判定・
  -- Phase 91 A4: 生モデルを synthVecIR に渡すと data 空で誤表示するため差替)。
  putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  -- J=85郡分のalpha latentを含むため dashboardFullOf ではなく dashboardOf
  -- (健全性2x2パネルのみ・05-mh/10-ratsと同じ判断)。
  unless profRun $
    savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
      (noDf |>> dashboardOf m "log_radon" :: BoundPlot)

  let chainsAug = map (augmentChainWithDeterministic model) (hbmChainsR m)
  printSummary $ summarize
    ["mu_alpha", "sigma_alpha", "sigma_y", "beta1", "beta2"]
    chainsAug
