{-# LANGUAGE OverloadedStrings #-}
-- | M0_data-M0_model (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。BPA本 (Kéry & Schaub 2011)
-- Ch.6 の最も単純な capture-recapture モデル M0 (定数の内包確率omega・
-- 定数の検出確率p・05-mhのMhモデルから個体差ランダム効果を除いた
-- ベースライン変種・同一ファミリの構造比較用)。
--
-- Stan 原典 (posteriordb `models/stan/M0_model.stan`):
--   data { int M; int T; array[M,T] int y; }
--   transformed data { s[i] = sum(y[i]); }  // 個体ごとの捕獲総数
--   omega/p ~ 暗黙Uniform(0,1);
--   for (i in 1:M):
--     if (s[i] > 0): target += bernoulli_lpmf(1|omega) + binomial_lpmf(s[i]|T,p)
--     else: target += log_sum_exp(bernoulli_lpmf(1|omega)+binomial_lpmf(0|T,p),
--                                  bernoulli_lpmf(0|omega))
--
-- 05-mh/README.mdで導出済みのとおり、この尤度構造は数学的に
-- `ZeroInflatedBinomial T (1-omega) p` と厳密に一致する (05-mhと同じ
-- 導出・個体ごとのランダム効果が無い分だけ05-mhより単純)。omega/pの
-- 暗黙Uniform(0,1)はBeta(1,1)で移植 (05-mhと同じくvecIR probe安全性の
-- ため)。
--
-- reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
--
-- ★2026-07-12: コード準備のみ (データ未取得・ビルド確認は次回)。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-m0
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Printf (printf)

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedObs, plateForM_)
import Hanalyze.Model.HBM.IR (synthVecIR)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardFullOf, hbmChainsR)
import Graphics.Hgg.Spec (ColData (..))
import Graphics.Hgg.Frame (BoundPlot, (|>>))
import Graphics.Hgg.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @M0_data.json@ 形状 ({"M":...,"T":...,
-- "y":[[...M行×T列の0/1捕獲履歴...]]})。
data M0Raw = M0Raw { m0T :: Int, m0Y :: [[Int]] }

instance FromJSON M0Raw where
  parseJSON = withObject "M0Raw" $ \v ->
    M0Raw <$> v .: "T" <*> v .: "y"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/29-m0/data/M0_data.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/29-m0/figures"

readData :: IO M0Raw
readData = either fail pure =<< eitherDecodeFileStrict dataPath

-- | M0 capture-recapture モデル (Stan 原典と数学的に厳密に等価な
-- ZeroInflatedBinomial 表現)。 @t@ (サンプリング機会数) は微分対象では
-- ない定数なので closure で渡す。
m0Model :: Int -> ModelP ()
m0Model t = do
  omega <- sample "omega" (Beta 1 1)
  p     <- sample "p"     (Beta 1 1)
  ss    <- dataNamedObs "s" []
  plateForM_ "obs" ss $ \sVal ->
    observe "s" (ZeroInflatedBinomial t (1 - omega) p) [sVal]

main :: IO ()
main = do
  d <- readData
  let sTotals = map (fromIntegral . sum) (m0Y d) :: [Double]
      df = [ ("s", NumData (V.fromList sTotals)) ] :: [(T.Text, ColData)]
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      model :: ModelP ()
      model = m0Model (m0T d)
      m = df |-> hbm cfg model

  putStrLn $ "synthVecIR = " ++
    (case synthVecIR model of
       Just _  -> "Just (vecIR高速経路)"
       Nothing -> "Nothing (legacy walk+ad)")

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardFullOf m "s" :: BoundPlot)

  printSummary $ summarize ["omega", "p"] (hbmChainsR m)
