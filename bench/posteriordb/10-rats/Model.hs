{-# LANGUAGE OverloadedStrings #-}
-- | rats_data-rats_model (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。BUGS 古典例「ラットの成長曲線」
-- (30匹 × 5時点の体重・縦断的階層線形回帰)。ラットごとに独立な切片
-- alpha[i]・傾き beta[i] を持ち、両方とも部分プーリングされる
-- (mu_alpha/sigma_alpha, mu_beta/sigma_beta)。
--
-- Stan 原典 (posteriordb `models/stan/rats_model.stan`。"Model simplified"
-- 版・sigma_y/sigma_alpha/sigma_beta は真に improper flat prior):
--   parameters { array[N] real alpha; array[N] real beta;
--                real mu_alpha; real mu_beta;
--                real<lower=0> sigma_y; real<lower=0> sigma_alpha; real<lower=0> sigma_beta; }
--   model {
--     mu_alpha ~ normal(0, 100); mu_beta ~ normal(0, 100);
--     alpha ~ normal(mu_alpha, sigma_alpha); beta ~ normal(mu_beta, sigma_beta);
--     y[n] ~ normal(alpha[rat[n]] + beta[rat[n]] * (x[n] - xbar), sigma_y);
--   }
--
-- improper flat prior (下限0のみ・上限なし) は hanalyze に表現できないため、
-- HalfCauchy(25) (09-eight-schools の tau ~ HalfCauchy(5) と同じ流儀の
-- 緩い weakly-informative prior) に置換する。
--
-- ★実測で判明した罠: 当初 Uniform(0,100) で試したところ、全 warmup で
-- acceptanceRate=0.0・chainEnergy=Infinity 全 draw で発散し、事後平均が
-- 全パラメータで厳密に 0.0000 に凍りつく現象が発生した。原因は
-- `Distribution.hs:309-310` の既知の制約:
-- 「Uniform の真の制約変換 (logit-on-(lo,hi)) は現状未実装・unconstrained
-- 扱い」。unconstrained 初期値は raw=0 だが、これが Uniform(lo,hi) では
-- 変換なしにそのまま値 0 として使われる ("下限ちょうど" ではなく
-- "変換前 0" が直接使われる)。sigma_y のような Normal 尤度の SD に
-- Uniform(0,X) を直接使うと、初期値 sigma_y=0 で
-- `Normal(mu, 0)` が退化し log-density が -Infinity になり、HMC が
-- 初手から発散し続けて一切回復しない (01-glm-poisson の alpha/beta の
-- ような「内部の有界パラメータ」なら raw=0 は無害だが、SD パラメータでは
-- 致命的)。`HalfCauchy`/`HalfNormal` は `PositiveT` 変換 (exp 系) を持ち
-- raw=0 が安全な内点にマップされるため、この罠を回避できる
-- (実測: acceptanceRate 0.0 → 0.9 に復帰・確認は cabal repl トイモデルで
-- 実施)。PyMC 側 (model.py) も同じ HalfCauchy(25) に揃える (両者比較可能
-- にするため・Stan 原典の改変幅は同一なので公平性は保たれる)。
--
-- reference_posterior_name = null (posteriordb に公式 reference 無し・2者比較のみ)。
--
-- ラット番号 (rat[]) は df 列ではなく、Mh モデルの T と同じ流儀でモデル関数へ
-- 直接 closure 引数として渡す (df|->hbm の対象データではなく構造情報)。
-- ラットごとの alpha[i]/beta[i] latent は eight-schools 型 plateI + gather
-- パターンで宣言する。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-rats
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Printf (printf)

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedX, dataNamedObs, plateI, plateForM_, (.#))
import Hanalyze.Model.HBM (gradPathLabel)
import Hanalyze.Plot (hbmModelSpec)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardOf, hbmChainsR)
import Hgg.Plot.Spec (ColData (..))
import Hgg.Plot.Frame (BoundPlot, (|>>))
import Hgg.Plot.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @rats_data.json@ 形状
-- ({"N":30, "Npts":150, "rat":[...], "x":[...], "y":[...], "xbar":22})。
data RatsData = RatsData
  { nRats :: Int
  , rat   :: [Int]
  , xArr  :: [Double]
  , yArr  :: [Double]
  , xbar  :: Double
  }

instance FromJSON RatsData where
  parseJSON = withObject "RatsData" $ \v ->
    RatsData <$> v .: "N" <*> v .: "rat" <*> v .: "x" <*> v .: "y" <*> v .: "xbar"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/10-rats/data/rats_data.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/10-rats/figures"

readData :: IO RatsData
readData = either fail pure =<< eitherDecodeFileStrict dataPath

-- | 縦断的階層線形回帰 (ラットごとの切片/傾き)。ratIdx (1始まり)・xbar は
-- データ由来の固定構造として closure で渡す (Mh モデルの T と同じ流儀)。
ratsModel :: Int -> [Int] -> Double -> ModelP ()
ratsModel n ratIdx center = do
  muAlpha    <- sample "mu_alpha"    (Normal 0 100)
  muBeta     <- sample "mu_beta"     (Normal 0 100)
  sigmaY     <- sample "sigma_y"     (HalfCauchy 25)
  sigmaAlpha <- sample "sigma_alpha" (HalfCauchy 25)
  sigmaBeta  <- sample "sigma_beta"  (HalfCauchy 25)
  alphas <- plateI "rat_alpha" n $ \i -> sample ("alpha" .# i) (Normal muAlpha sigmaAlpha)
  betas  <- plateI "rat_beta"  n $ \i -> sample ("beta"  .# i) (Normal muBeta  sigmaBeta)
  xs <- dataNamedX   "x" []
  ys <- dataNamedObs "y" []
  let centerA = realToFrac center
  plateForM_ "obs" (zip3 ratIdx xs ys) $ \(r, xi, yi) ->
    let mu = (alphas !! (r - 1)) + (betas !! (r - 1)) * (xi - centerA)
    in observe "y" (Normal mu sigmaY) [yi]

main :: IO ()
main = do
  d <- readData
  let df = [ ("x", NumData (V.fromList (xArr d)))
           , ("y", NumData (V.fromList (yArr d)))
           ] :: [(T.Text, ColData)]
      -- PyMC 側 (model.py) と同じ設定を定数で揃える。
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      m = df |-> hbm cfg (ratsModel (nRats d) (rat d) (xbar d))

  -- 勾配経路 = compileGradUV が実際に選ぶ経路 (束縛済 hbmModelSpec で判定・
  -- Phase 91 A4: 生モデルを synthVecIR に渡すと data 空で誤表示するため差替)。
  putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  -- N=30 匹分の alpha/beta latent を含むため 'dashboardFullOf' は肥大化する
  -- (05-mh と同じ判断)。健全性 2x2 パネル (DAG/forest/PPC/energy) のみ。
  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardOf m "y" :: BoundPlot)

  printSummary $ summarize ["mu_alpha", "mu_beta", "sigma_y", "sigma_alpha", "sigma_beta"] (hbmChainsR m)
