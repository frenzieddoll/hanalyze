{-# LANGUAGE OverloadedStrings #-}
-- | bones_data-bones_model (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。骨年齢の graded response IRT
-- モデル (BUGS 古典例)。nChild=13人の子供・nInd=34項目 (骨のX線指標)。
-- 各項目の困難度カットポイント (gamma) と識別力 (delta) は**固定データ**
-- (未サンプル)・各子供の能力 theta のみが latent。
--
-- Stan 原典 (posteriordb `models/stan/bones_model.stan`):
--   theta[i] ~ normal(0, 36);
--   for each i,j:
--     Q[i,j,k] = inv_logit(delta[j]*(theta[i]-gamma[j,k]))  for k=1..(ncat[j]-1)
--     p[i,j,1] = 1-Q[i,j,1]; p[i,j,k] = Q[i,j,k-1]-Q[i,j,k]; p[i,j,ncat[j]] = Q[i,j,ncat[j]-1]
--     if grade[i,j] != -1: target += log(p[i,j,grade[i,j]])   // 欠測はスキップ
--
-- 尤度は `observe` を使わず `potential` で直書きする (14-hmm-example/
-- 16-lda と同系統)。 difficulty/discrimination はデータなので
-- Haskell 側では単なる @[Double]@ として closure で渡す。
--
-- reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-bones
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Printf (printf)

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observeMV,
                                    plateI, (.#))
import Hanalyze.Model.HBM (gradPathLabel)
import Hanalyze.Plot (hbmModelSpec)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardOf, hbmChainsR)
import Graphics.Hgg.Spec (ColData (..))
import Graphics.Hgg.Frame (BoundPlot, (|>>))
import Graphics.Hgg.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @bones_data.json@ 形状 ({"ncat":[...34...],
-- "nChild":13,"nInd":34,"grade":[[13x34]],"delta":[...34...],
-- "gamma":[[34x4]]}・grade の @-1@ は欠測)。
data BonesData = BonesData
  { bnCat   :: [Int]
  , bnGrade :: [[Int]]
  , bnDelta :: [Double]
  , bnGamma :: [[Double]]
  }

instance FromJSON BonesData where
  parseJSON = withObject "BonesData" $ \v ->
    BonesData <$> v .: "ncat" <*> v .: "grade" <*> v .: "delta" <*> v .: "gamma"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/20-bones/data/bones_data.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/20-bones/figures"

readData :: IO BonesData
readData = either fail pure =<< eitherDecodeFileStrict dataPath

-- | graded response IRT モデル (Stan 原典と同一構造)。
--
-- Phase 101 A3: 尤度を @logCatProb + potential@ 直書きから構造化 primitive
-- 'GradedResponseIrt' + 'observeMV' へ移行 (密度は同値・'obsLogSum' が同じ
-- Q/p 構成を呼ぶ)。 θ_i のみが latent なため、勾配コンパイラが解析勾配
-- ('gradedIrtAnalyticVG'・dQ/dθ = δ·Q(1−Q) の隣接差・AD tape ゼロ) を選べる。
-- grade 行列は行優先 flatten して 1 観測で渡す (−1 = 欠測は skip)。
bonesModel :: [Int] -> [Double] -> [[Double]] -> [[Int]] -> ModelP ()
bonesModel ncats deltas gammas grades = do
  thetas <- plateI "child" (length grades) $ \i -> sample ("theta" .# i) (Normal 0 36)
  observeMV "grades" (GradedResponseIrt thetas ncats deltas gammas)
            [map fromIntegral (concat grades)]

main :: IO ()
main = do
  d <- readData
  let nChild = length (bnGrade d)
      -- ダッシュボード用の名目 obs 列 (14-hmm-example/16-lda と同様、
      -- 実際の尤度は potential で直書きするため観測ノードは存在しない・
      -- PPCパネルは空)。項目1 (indicator 0) の全児童分のgradeを使う。
      grade1 = [ fromIntegral (row !! 0) | row <- bnGrade d ] :: [Double]
      df = [ ("grade1", NumData (V.fromList grade1)) ] :: [(T.Text, ColData)]
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      model :: ModelP ()
      model = bonesModel (bnCat d) (bnDelta d) (bnGamma d) (bnGrade d)
      m = df |-> hbm cfg model

  -- 勾配経路 = compileGradUV が実際に選ぶ経路 (束縛済 hbmModelSpec で判定・
  -- Phase 91 A4: 生モデルを synthVecIR に渡すと data 空で誤表示するため差替)。
  putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  -- nChild=13の小規模モデルのため dashboardFullOf でも問題ないが、
  -- potential のみで尤度を構成 (PPCパネルは空) するため 05-mh/16-lda と
  -- 同様 dashboardOf (健全性2x2パネルのみ) を使う。
  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardOf m "grade1" :: BoundPlot)

  printSummary $ summarize
    [ "theta_" <> T.pack (show i) | i <- [0 .. nChild - 1] ]
    (hbmChainsR m)
