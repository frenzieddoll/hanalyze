{-# LANGUAGE OverloadedStrings #-}
-- | irt_2pl-irt_2pl (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89/90: posteriordb 横断ベンチマーク + Phase 90 A5 (vecIR ギャップ
-- 解消: 06-irt-2pl = 独立2ラテント配列を跨ぐ乗算項 (a[i]*(theta[j]-b[i])))。
--
-- Stan 原典 (posteriordb `models/stan/irt_2pl.stan`。 I=20項目×J=100人):
--   parameters { real<lower=0> sigma_theta; vector[J] theta;
--                real<lower=0> sigma_a; vector<lower=0>[I] a;
--                real mu_b; real<lower=0> sigma_b; vector[I] b; }
--   model {
--     sigma_theta ~ cauchy(0,2); theta ~ normal(0, sigma_theta);
--     sigma_a ~ cauchy(0,2); a ~ lognormal(0, sigma_a);
--     mu_b ~ normal(0,5); sigma_b ~ cauchy(0,2); b ~ normal(mu_b, sigma_b);
--     for (i in 1:I) y[i] ~ bernoulli_logit(a[i] * (theta - b[i]));
--   }
--
-- reference_posterior_name = null (posteriordb に公式 reference 無し・
-- hanalyze vs PyMC の2者比較のみ。 06-irt-2pl/README.md の旧記述「あり」は
-- Phase 89 時点の誤りで Phase 90 A5 で実測訂正した)。
--
-- Phase 90 A5: 06-irt-2pl は Phase 89 で「分類未確定」のまま保留されて
-- いたが、A1 実測調査で真因が「独立2ラテント配列を跨ぐ積」自体ではなく
-- `IR.hs` の `famOf` が family absorb (prior のベクトル化) を
-- Normal(m,τ) 構造限定にしていたこと (`a` の LogNormal 事前分布が family
-- 不成立で group ごと丸ごと Nothing になっていた) と判明。A5 で `tryGroup`
-- を「family absorb 失敗は fams から除外するだけ・likelihood 側の吸収は
-- 継続」という fault-tolerant 設計に修正し解消した (`theta`/`b` は
-- Normal 階層事前分布で family absorb、`a` の LogNormal 事前分布は
-- 既存の `constPriorsOf`/残差 AD 経路にフォールバック)。
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedObs, plateI, plateForM_, (.#), withData)
import Hanalyze.Model.HBM (gradPathLabel)
import Hanalyze.Plot (hbmModelSpec)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardOf, hbmChainsR)
import Hgg.Plot.Spec (ColData (..))
import Hgg.Plot.Frame (BoundPlot, (|>>))
import Hgg.Plot.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)
import Text.Printf (printf)

-- | posteriordb の @irt_2pl.json@ 形状 ({"I":20, "J":100, "y":[[0/1,...]×20]})。
data IrtData = IrtData { yMat :: [[Double]] }

instance FromJSON IrtData where
  parseJSON = withObject "IrtData" $ \v -> IrtData <$> v .: "y"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/06-irt-2pl/data/irt_2pl.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/06-irt-2pl/figures"

readData :: IO [[Double]]
readData = do
  d <- either fail pure =<< eitherDecodeFileStrict dataPath
  pure (yMat d)

-- | IRT 2PLモデル。theta (J人) / a,b (I項目) はそれぞれ族 gather
-- (`plateI` + `!!`) で宣言し、観測は (item,person) の全ペアで
-- `plateForM_` する (2000観測・`y`は行優先でフラット化して束縛)。
irt2plModel :: Int -> Int -> ModelP ()
irt2plModel nI nJ = do
  sigmaTheta <- sample "sigma_theta" (HalfCauchy 2)
  thetas <- plateI "person" nJ $ \j -> sample ("theta" .# j) (Normal 0 sigmaTheta)
  sigmaA <- sample "sigma_a" (HalfCauchy 2)
  as <- plateI "item" nI $ \i -> sample ("a" .# i) (LogNormal 0 sigmaA)
  muB <- sample "mu_b" (Normal 0 5)
  sigmaB <- sample "sigma_b" (HalfCauchy 2)
  bs <- plateI "item2" nI $ \i -> sample ("b" .# i) (Normal muB sigmaB)
  ys <- dataNamedObs "y" []
  let pairs = [ (i, j) | i <- [0 .. nI - 1], j <- [0 .. nJ - 1] ]
  plateForM_ "obs" (zip pairs ys) $ \((i, j), yij) ->
    let logit = (as !! i) * ((thetas !! j) - (bs !! i))
    in observe "y" (Bernoulli (1 / (1 + exp (negate logit)))) [yij]

main :: IO ()
main = do
  rows <- readData
  let nI = length rows
      nJ = length (head rows)
      ysFlat = concat rows   -- 行優先 (item-major) フラット化・Model.hs の pairs と対応
      df = [ ("y", NumData (V.fromList ysFlat)) ] :: [(T.Text, ColData)]
      -- PyMC 側 (model.py) と同じ設定を定数で揃える。
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1
                        -- Phase 105 A2: warmup init buffer (M=I 期) の深掘り抑制。
                        -- seed 1/2/3 で wall −1.8〜−11.7%・ESS/s(mu_b) 全 seed 改善
                        -- (05-mh Phase 96 A5 と同型・実測 root = experiments/phase105-*)
                        , hbmWarmupInitMaxDepth = Just 4 }
      m = df |-> hbm cfg (irt2plModel nI nJ)

  -- 勾配経路 = compileGradUV が実際に選ぶ経路 (束縛済 hbmModelSpec で判定・
  -- Phase 91 A4: 生モデルを synthVecIR に渡すと data 空で誤表示するため差替)。
  putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  -- theta(100)+a(20)+b(20) = 140 latent。dashboardFullOf は大規模化するため
  -- 05-mh と同じ理由で dashboardOf (健全性パネルのみ) を使う。
  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardOf m "y" :: BoundPlot)

  printSummary $ summarize ["sigma_theta", "sigma_a", "mu_b", "sigma_b"] (hbmChainsR m)
