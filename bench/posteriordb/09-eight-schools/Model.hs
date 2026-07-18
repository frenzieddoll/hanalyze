{-# LANGUAGE OverloadedStrings #-}
-- | eight_schools-eight_schools_noncentered (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。階層モデルの正準例 8-schools
-- (Rubin 1981・8校の補習授業効果) を non-centered パラメタ化で実装する
-- (`docs/api-guide/03-bayesian-hbm.md` §階層モデルの例に準拠)。
--
-- Stan 原典 (posteriordb `models/stan/eight_schools_noncentered.stan`):
--   parameters { vector[J] theta_trans; real mu; real<lower=0> tau; }
--   transformed parameters { theta = theta_trans * tau + mu; }
--   model {
--     theta_trans ~ normal(0, 1);
--     y ~ normal(theta, sigma);   // sigma は既知データ (観測誤差)
--     mu ~ normal(0, 5);
--     tau ~ cauchy(0, 5);
--   }
--
-- reference_posterior_name = "eight_schools-eight_schools_noncentered"
-- (posteriordb に公式 reference posterior あり・hanalyze vs PyMC vs 公式referenceの
-- 3者比較が可能)。
--
-- 高レベル API (`df |-> hbm`) を使用: y/sigma は 'dataNamedObs'/'dataNamedX' で
-- df から束縛し、 学校ごとの潜在変数 eta は 'plateI' (index 版・結果保持) で
-- 8個宣言してから 'plateForM_' で観測に束ねる (api-guide の gather パターン)。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-eight-schools
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedX, dataNamedObs, plateI, plateForM_,
                                    (.#))
import Hanalyze.Model.HBM (gradPathLabel)
import Hanalyze.Plot (hbmModelSpec)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardFullOf, hbmChainsR)
import Hgg.Plot.Spec (ColData (..))
import Hgg.Plot.Frame (BoundPlot, (|>>))
import Hgg.Plot.Backend.Rasterific (savePNGBound)

import Text.Printf (printf)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @eight_schools.json@ 形状 ({"J":8, "y":[...], "sigma":[...]})。
data EightSchoolsData = EightSchoolsData
  { y     :: [Double]
  , sigma :: [Double]
  }

instance FromJSON EightSchoolsData where
  parseJSON = withObject "EightSchoolsData" $ \v ->
    EightSchoolsData <$> v .: "y" <*> v .: "sigma"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/09-eight-schools/data/eight_schools.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/09-eight-schools/figures"

readData :: IO ([Double], [Double])
readData = do
  d <- either fail pure =<< eitherDecodeFileStrict dataPath
  pure (y d, sigma d)

-- | Non-centered 階層モデル: eta_j ~ Normal(0,1)・theta_j = mu + tau*eta_j
-- (`docs/api-guide/03-bayesian-hbm.md` の eightSchools 例と同型・sigma は
-- 観測ごとに既知の定数 (Stan 原典の `sigma` データ) なので dataNamedX で束縛する)。
eightSchoolsModel :: ModelP ()
eightSchoolsModel = do
  mu  <- sample "mu"  (Normal 0 5)
  tau <- sample "tau" (HalfCauchy 5)
  sigmas <- dataNamedX   "sigma" []
  ys     <- dataNamedObs "y"     []
  etas <- plateI "school" 8 $ \j -> sample ("eta" .# j) (Normal 0 1)
  plateForM_ "school_obs" (zip3 [0 ..] sigmas ys) $ \(j, sg, yi) ->
    observe ("y" .# j) (Normal (mu + tau * etas !! j) sg) [yi]

main :: IO ()
main = do
  (ys, sigmas) <- readData
  let df = [ ("y",     NumData (V.fromList ys))
           , ("sigma", NumData (V.fromList sigmas))
           ] :: [(T.Text, ColData)]
      -- PyMC 側 (model.py) と同じ設定を定数で揃える。
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      m = df |-> hbm cfg eightSchoolsModel

  -- Phase 96 A2: 勾配経路 = compileGradUV が実際に選ぶ経路 (束縛済
  -- hbmModelSpec で判定・Phase 91 A4 と同型)。
  putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

  -- サンプリング**のみ**の壁時計 (PyMC run_pymc_matrix.py の t0=perf_counter();
  -- pm.sample() と対応させる・Common.timeSamplingMs 参照)。dashboardFullOf 等
  -- 後続処理は hbmChainsR の thunk が既に強制済みのものを再利用するので、
  -- 二重計算にはならない。
  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardFullOf m "y" :: BoundPlot)

  printSummary $ summarize ["mu", "tau"] (hbmChainsR m)
