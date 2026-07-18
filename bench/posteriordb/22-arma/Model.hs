{-# LANGUAGE OverloadedStrings #-}
-- | arma-arma11 (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。ARMA(1,1) 時系列 (T=200)。
-- ★新ファミリ: AR成分とMA成分を併せ持つ時系列 — 12-ark (純AR)・03-garch11
-- (再帰的分散) とは異なる構造。
--
-- Stan 原典 (posteriordb `models/stan/arma11.stan`):
--   mu ~ normal(0,10); phi ~ normal(0,2); theta ~ normal(0,2);
--   sigma ~ cauchy(0,2.5);
--   nu[1] = mu + phi*mu; err[1] = y[1]-nu[1];       // err[0]=0 とみなす
--   for (t in 2:T) { nu[t] = mu+phi*y[t-1]+theta*err[t-1]; err[t]=y[t]-nu[t]; }
--   err ~ normal(0, sigma);
--
-- err[t] が err[t-1] に依存する逐次再帰 (14-hmm-example の forward
-- algorithmと同系統)。Phase 101 A2: 尤度を `mapAccumL + potential` 直書きから
-- 構造化 primitive 'ArmaNormal' + 'observeMV' へ移行 (密度は同値・'obsLogSum'
-- が同じ err 再帰を呼ぶ)。役割 (μ/φ/θ/σ) が型で見えるため、勾配コンパイラが
-- 逆向き随伴の閉形式 ('armaAnalyticVG'・AD tape ゼロ) を選べる。
--
-- reference_posterior_name = "arma-arma11" (posteriordb に公式 reference
-- あり・hanalyze vs PyMC vs 公式reference の3者比較可能)。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-arma
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Printf (printf)

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observeMV,
                                    dataNamedX)
import Hanalyze.Model.HBM (gradPathLabel)
import Hanalyze.Plot (hbmModelSpec)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardFullOf, hbmChainsR)
import Hgg.Plot.Spec (ColData (..))
import Hgg.Plot.Frame (BoundPlot, (|>>))
import Hgg.Plot.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @arma.json@ 形状 ({"T":200,"y":[...]})。
data ArmaData = ArmaData { arT :: Int, arY :: [Double] }

instance FromJSON ArmaData where
  parseJSON = withObject "ArmaData" $ \v ->
    ArmaData <$> v .: "T" <*> v .: "y"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/22-arma/data/arma.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/22-arma/figures"

readData :: IO ArmaData
readData = either fail pure =<< eitherDecodeFileStrict dataPath

-- | ARMA(1,1) (Stan 原典と同一構造)。
--
-- Phase 101 A2: 尤度を 'ArmaNormal' + 'observeMV' で渡す (err 再帰は
-- 'obsLogSum' 側の同値実装)。 dataNamedX "y" は dashboard の実データ参照用に
-- 残す。
armaModel :: [Double] -> ModelP ()
armaModel ysRaw = do
  mu    <- sample "mu"    (Normal 0 10)
  phi   <- sample "phi"   (Normal 0 2)
  theta <- sample "theta" (Normal 0 2)
  sigma <- sample "sigma" (HalfCauchy 2.5)
  _ys <- dataNamedX "y" []
  observeMV "y_seq" (ArmaNormal mu phi theta sigma) [ysRaw]

main :: IO ()
main = do
  d <- readData
  let df = [ ("y", NumData (V.fromList (arY d))) ] :: [(T.Text, ColData)]
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      m = df |-> hbm cfg (armaModel (arY d))

  -- 勾配経路 = compileGradUV が実際に選ぶ経路 (束縛済 hbmModelSpec で判定・
  -- Phase 91 A4: 生モデルを synthVecIR に渡すと data 空で誤表示するため差替)。
  putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardFullOf m "y" :: BoundPlot)

  printSummary $ summarize ["mu", "phi", "theta", "sigma"] (hbmChainsR m)
