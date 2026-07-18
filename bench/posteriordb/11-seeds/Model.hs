{-# LANGUAGE OverloadedStrings #-}
-- | seeds_data-seeds_model (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。BUGS 古典例「種子発芽実験」
-- (Crowder 1978・I=21プレート・2種の種子×2種の根の抽出物の2x2要因計画+
-- overdispersion 用のプレートごとランダム切片)。
--
-- Stan 原典 (posteriordb `models/stan/seeds_model.stan`):
--   parameters { real alpha0,alpha1,alpha2,alpha12; real<lower=0> tau;
--                vector[I] b; }
--   transformed parameters { sigma = 1/sqrt(tau); }
--   model {
--     alpha0,alpha1,alpha2,alpha12 ~ normal(0, 1000);
--     tau ~ gamma(1e-3, 1e-3);
--     b ~ normal(0, sigma);
--     n ~ binomial_logit(N, alpha0 + alpha1*x1 + alpha2*x2 + alpha12*x1*x2 + b);
--   }
--
-- hanalyze に `binomial_logit` 相当は無いため `Binomial N p` +
-- `p = invlogit(eta)` へ手動展開する (05-mh の ZeroInflatedBinomial と
-- 同じ流儀)。`tau ~ Gamma(1e-3,1e-3)` は hanalyze の `Gamma` が `PositiveT`
-- 変換 (exp系) を持つため 10-rats で踏んだ「Uniform を SD に使う罠」は
-- 発生しない (既知の一様事前 sd パラメータ発散パターン)。
--
-- reference_posterior_name = null (posteriordb に公式 reference 無し・2者比較のみ)。
--
-- N (試行数、プレートごとに既知の固定整数) は df 列ではなく、Mh モデルの T
-- と同じ流儀でモデル関数へ直接 closure 引数として渡す。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-seeds
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import Data.List (zip4)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Printf (printf)

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedX, dataNamedObs, plateI, plateForM_, (.#))
import Hanalyze.Model.HBM (gradPathLabel)
import Hanalyze.Plot (hbmModelSpec)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardFullOf, hbmChainsR)
import Hgg.Plot.Spec (ColData (..))
import Hgg.Plot.Frame (BoundPlot, (|>>))
import Hgg.Plot.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @seeds_data.json@ 形状
-- ({"I":21, "n":[...], "N":[...], "x1":[...], "x2":[...]})。
data SeedsData = SeedsData
  { nI   :: Int
  , nObs :: [Int]
  , nCap :: [Int]
  , x1v  :: [Double]
  , x2v  :: [Double]
  }

instance FromJSON SeedsData where
  parseJSON = withObject "SeedsData" $ \v ->
    SeedsData <$> v .: "I" <*> v .: "n" <*> v .: "N" <*> v .: "x1" <*> v .: "x2"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/11-seeds/data/seeds_data.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/11-seeds/figures"

readData :: IO SeedsData
readData = either fail pure =<< eitherDecodeFileStrict dataPath

-- | 2x2要因計画のロジスティック回帰 + プレートごとランダム切片。
-- i (プレート数)・trials (試行数 N、プレートごとの固定整数) は
-- データ由来の固定構造として closure で渡す (Mh モデルの T と同じ流儀)。
--
-- Phase 94 A4-2: **非中心化** (non-centered)。 Stan 原典は centered
-- (@b_k ~ Normal(0, σ)@) だが、 σ=1/√τ が小さい領域に funnel の首ができ、
-- chain が凍結して tau ESS を潰す (§A4-1 で実測: centered は 1/4 chain 崩壊・
-- ess(tau)=11)。 @z_k ~ Normal(0,1)@ + @b_k = z_k·σ@ に置換すると z が τ と
-- decouple し首が消える (Stan/PyMC の hierarchical 定番)。 事後は同一。
-- 非中心化後: 崩壊 0・ess(tau)=837 (§A4-2)。
seedsModel :: Int -> [Int] -> ModelP ()
seedsModel i trials = do
  alpha0  <- sample "alpha0"  (Normal 0 1000)
  alpha1  <- sample "alpha1"  (Normal 0 1000)
  alpha2  <- sample "alpha2"  (Normal 0 1000)
  alpha12 <- sample "alpha12" (Normal 0 1000)
  tau     <- sample "tau"     (Gamma 1.0e-3 1.0e-3)
  let sigma = 1 / sqrt tau
  zs <- plateI "plate" i $ \k -> sample ("z" .# k) (Normal 0 1)   -- 非中心化 latent (b_k = z_k·σ)
  x1s <- dataNamedX   "x1" []
  x2s <- dataNamedX   "x2" []
  ns  <- dataNamedObs "n"  []
  plateForM_ "obs" (zip [0 :: Int ..] (zip4 trials x1s x2s ns)) $ \(k, (capK, x1k, x2k, nk)) ->
    let eta = alpha0 + alpha1 * x1k + alpha2 * x2k + alpha12 * x1k * x2k
                + (zs !! k) * sigma
        p   = 1 / (1 + exp (negate eta))
    in observe "n" (Binomial capK p) [nk]

main :: IO ()
main = do
  d <- readData
  let df = [ ("x1", NumData (V.fromList (x1v d)))
           , ("x2", NumData (V.fromList (x2v d)))
           , ("n",  NumData (V.fromList (map fromIntegral (nObs d))))
           ] :: [(T.Text, ColData)]
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      m = df |-> hbm cfg (seedsModel (nI d) (nCap d))

  -- 勾配経路 = compileGradUV が実際に選ぶ経路 (束縛済 hbmModelSpec で判定・
  -- Phase 91 A4: 生モデルを synthVecIR に渡すと data 空で誤表示するため差替)。
  putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardFullOf m "n" :: BoundPlot)

  printSummary $ summarize ["alpha0", "alpha1", "alpha2", "alpha12", "tau"] (hbmChainsR m)
