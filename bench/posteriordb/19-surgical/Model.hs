{-# LANGUAGE OverloadedStrings #-}
-- | surgical_data-surgical_model (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。BUGS 古典例「12病院の心臓手術
-- 死亡率」(N=12病院・階層二項ロジット・共変量なしの最も単純な階層形)。
--
-- Stan 原典 (posteriordb `models/stan/surgical_model.stan`):
--   mu ~ normal(0,1000); sigmasq ~ inv_gamma(0.001,0.001); sigma=sqrt(sigmasq);
--   b[i] ~ normal(mu, sigma);  r[i] ~ binomial_logit(n[i], b[i]);
--
-- hanalyze の `Binomial n p` は確率パラメータ直接指定 (logit link 無し) の
-- ため、05-mh と同じく `p = invlogit(b)` を手計算してから渡す。
-- `n[i]` (病院ごとの手術数) は微分対象ではない構造的定数なので closure で
-- 直接渡す (05-mh の @T@ と同じ流儀)。
--
-- reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-surgical
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Printf (printf)

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedObs, plateI, plateForM_, (.#),
                                    deterministic, augmentChainWithDeterministic)
import Hanalyze.Model.HBM (gradPathLabel)
import Hanalyze.Plot (hbmModelSpec)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardFullOf, hbmChainsR)
import Graphics.Hgg.Spec (ColData (..))
import Graphics.Hgg.Frame (BoundPlot, (|>>))
import Graphics.Hgg.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @surgical_data.json@ 形状 ({"N":12,"n":[...],"r":[...]})。
data SurgicalData = SurgicalData { nOps :: [Int], rDeaths :: [Int] }

instance FromJSON SurgicalData where
  parseJSON = withObject "SurgicalData" $ \v ->
    SurgicalData <$> v .: "n" <*> v .: "r"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/19-surgical/data/surgical_data.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/19-surgical/figures"

readData :: IO SurgicalData
readData = either fail pure =<< eitherDecodeFileStrict dataPath

-- | 病院ごとの階層二項ロジット (Stan 原典と同一構造)。
surgicalModel :: [Int] -> ModelP ()
surgicalModel ns = do
  mu      <- sample "mu"      (Normal 0 1000)
  sigmasq <- sample "sigmasq" (InverseGamma 0.001 0.001)
  sigma   <- deterministic "sigma" (sqrt sigmasq)
  bs      <- plateI "hosp" (length ns) $ \i -> sample ("b" .# i) (Normal mu sigma)
  _       <- deterministic "pop_mean" (1 / (1 + exp (negate mu)))
  rs      <- dataNamedObs "r" []
  plateForM_ "obs" (zip3 ns bs rs) $ \(n, b, rVal) ->
    let p = 1 / (1 + exp (negate b))
    in observe "r" (Binomial n p) [rVal]

main :: IO ()
main = do
  d <- readData
  let df = [ ("r", NumData (V.fromList (map fromIntegral (rDeaths d)))) ] :: [(T.Text, ColData)]
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      model :: ModelP ()
      model = surgicalModel (nOps d)
      m = df |-> hbm cfg model

  -- 勾配経路 = compileGradUV が実際に選ぶ経路 (束縛済 hbmModelSpec で判定・
  -- Phase 91 A4: 生モデルを synthVecIR に渡すと data 空で誤表示するため差替)。
  putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardFullOf m "r" :: BoundPlot)

  let chainsAug = map (augmentChainWithDeterministic model) (hbmChainsR m)
      bNames = [ "b_" <> T.pack (show i) | i <- [0 .. length (nOps d) - 1] ]
  printSummary $ summarize (["mu", "sigma", "pop_mean"] ++ bNames) chainsAug
