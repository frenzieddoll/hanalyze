{-# LANGUAGE OverloadedStrings #-}
-- | low_dim_gauss_mix-low_dim_gauss_mix (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89/90: posteriordb 横断ベンチマーク + Phase 90 A3 (vecIR ギャップ
-- 解消: 04-low-dim-gauss-mix = 2成分Normal混合)。
--
-- Stan 原典 (posteriordb `models/stan/low_dim_gauss_mix.stan`。 N=1000):
--   parameters { ordered[2] mu; array[2] real<lower=0> sigma;
--                real<lower=0,upper=1> theta; }
--   model {
--     sigma ~ normal(0, 2); mu ~ normal(0, 2); theta ~ beta(5, 5);
--     for (n in 1:N)
--       target += log_mix(theta, normal_lpdf(y[n]|mu[1],sigma[1]),
--                                 normal_lpdf(y[n]|mu[2],sigma[2]));
--   }
--
-- reference_posterior_name = "low_dim_gauss_mix-low_dim_gauss_mix"
-- (posteriordb に公式 reference posterior あり・3者比較可能)。
--
-- Phase 90 A3: `Mixture [w1,w2] [Normal mu1 sg1, Normal mu2 sg2]` を新規
-- vecIR family (`VGMixNorm2`) で高速経路に載せた (`IR.hs`)。Stan の
-- `ordered[2] mu` 制約 (label switching 回避) は hanalyze 側に対応する
-- 順序制約プリミティブが無いため実装せず、mu1<mu2 を確認できた場合のみ
-- 素直に採用する (後述「既知の課題」参照)。
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedObs, plateForM_, withData)
import Hanalyze.Model.HBM (gradPathLabel)
import Hanalyze.Plot (hbmModelSpec)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardFullOf, hbmChainsR)
import Hanalyze.MCMC.Core (Chain (..))
import Hgg.Plot.Spec (ColData (..))
import Hgg.Plot.Frame (BoundPlot, (|>>))
import Hgg.Plot.Backend.Rasterific (savePNGBound)
import qualified Data.Map.Strict as Map

import Common (summarize, printSummary, timeSamplingMs)
import Text.Printf (printf)

-- | posteriordb の @low_dim_gauss_mix.json@ 形状 ({"N":1000, "y":[...]})。
newtype MixData = MixData { y :: [Double] }

instance FromJSON MixData where
  parseJSON = withObject "MixData" $ \v -> MixData <$> v .: "y"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/04-low-dim-gauss-mix/data/low_dim_gauss_mix.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/04-low-dim-gauss-mix/figures"

readData :: IO [Double]
readData = do
  d <- either fail pure =<< eitherDecodeFileStrict dataPath
  pure (y d)

-- | 2成分 Normal 混合 (Phase 90 A3 で追加した `VGMixNorm2` vecIR family を使用)。
lowDimGaussMixModel :: ModelP ()
lowDimGaussMixModel = do
  mu1    <- sample "mu1"    (Normal 0 2)
  mu2    <- sample "mu2"    (Normal 0 2)
  sigma1 <- sample "sigma1" (HalfNormal 2)
  sigma2 <- sample "sigma2" (HalfNormal 2)
  theta  <- sample "theta"  (Beta 5 5)
  ys     <- dataNamedObs "y" []
  plateForM_ "obs" ys $ \yi ->
    observe "y" (Mixture [theta, 1 - theta]
                          [Normal mu1 sigma1, Normal mu2 sigma2]) [yi]

-- | ラベルスイッチング補正 (Stan 原典の `ordered[2] mu` 制約に相当)。
-- 2成分混合は mu1/mu2 を入れ替えても尤度が不変 (非識別) なので、posterior
-- draw ごとに mu1 > mu2 なら (mu1,sigma1,theta) <-> (mu2,sigma2,1-theta) を
-- 入れ替えて mu1 < mu2 に正規化する。 hanalyze は各 chain が個別の順序に
-- 収束しやすく (chain 内 ESS は健全・chain 間で符号が割れて R-hat が数十に
-- 跳ねる)、 この後処理で PyMC/公式referenceと比較可能な要約になる。
orderedChains :: [Chain] -> [Chain]
orderedChains = map orderChain
  where
    orderChain c = c { chainSamples = map orderDraw (chainSamples c) }
    orderDraw ps = case (Map.lookup "mu1" ps, Map.lookup "mu2" ps) of
      (Just m1, Just m2) | m1 > m2 -> swapDraw ps
      _                            -> ps
    swapDraw ps = Map.union (Map.fromList
      [ ("mu1", ps Map.! "mu2"), ("mu2", ps Map.! "mu1")
      , ("sigma1", ps Map.! "sigma2"), ("sigma2", ps Map.! "sigma1")
      , ("theta", 1 - ps Map.! "theta")
      ]) ps

main :: IO ()
main = do
  ys <- readData
  let df = [ ("y", NumData (V.fromList ys)) ] :: [(T.Text, ColData)]
      -- PyMC 側 (model.py) と同じ設定を定数で揃える。
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      m = df |-> hbm cfg lowDimGaussMixModel

  -- 勾配経路 = compileGradUV が実際に選ぶ経路 (束縛済 hbmModelSpec で判定・
  -- Phase 91 A4: 生モデルを synthVecIR に渡すと data 空で誤表示するため差替)。
  putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardFullOf m "y" :: BoundPlot)

  printSummary $ summarize ["mu1", "mu2", "sigma1", "sigma2", "theta"]
                           (orderedChains (hbmChainsR m))
