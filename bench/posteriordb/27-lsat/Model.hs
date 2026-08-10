{-# LANGUAGE OverloadedStrings #-}
-- | lsat_data-lsat_model (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。LSAT (法科大学院適性試験)
-- T=5問・N=1000人の Rasch (1PL) 型 二値IRT — Bock & Lieberman (1970)
-- 古典例。★新ファミリ: 全項目共通の単一識別力 (1PL/Rasch) — 06-irt-2pl
-- (項目ごとの識別力・2PL) や 20-bones (graded response) とは異なる
-- IRTモデル種別。
--
-- Stan 原典 (posteriordb `models/stan/lsat_model.stan`):
--   data { int N; int R; int T; array[R] int culm; array[R,T] int response; }
--   transformed data { r[T,N] を culm/response から展開 (パターン圧縮形式) }
--   alpha[T] ~ normal(0,100);  theta[N] ~ normal(0,1);
--   beta ~ normal(0,100) (real<lower=0> — 正の切断 = HalfNormal(100) と厳密等価);
--   for (k in 1:T): r[k] ~ bernoulli_logit(beta*theta - alpha[k]);
--
-- posteriordbのデータはパターン圧縮形式 (R=32通りの応答パターンと各
-- パターンの人数culmのみを保持) なので、Stanのtransformed dataと同じ
-- 展開ロジック (`unpackResponses`) をHaskell側でも再現する。
--
-- reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
--
-- ★2026-07-12: コード準備のみ (データ未取得・ビルド確認は次回)。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-lsat
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Printf (printf)

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    plateI, plateForM_, (.#))
import Hanalyze.Model.HBM.IR (synthVecIR)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardOf, hbmChainsR)
import Graphics.Hgg.Spec (ColData (..))
import Graphics.Hgg.Frame (BoundPlot, (|>>))
import Graphics.Hgg.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @lsat_data.json@ 形状 (パターン圧縮形式:
-- {"N":1000,"R":32,"T":5,"culm":[...32個の累積人数...],
-- "response":[[...32×5の0/1パターン...]]})。
data LsatRaw = LsatRaw
  { lsN        :: Int
  , lsT        :: Int
  , lsCulm     :: [Int]
  , lsResponse :: [[Int]]
  }

instance FromJSON LsatRaw where
  parseJSON = withObject "LsatRaw" $ \v ->
    LsatRaw <$> v .: "N" <*> v .: "T" <*> v .: "culm" <*> v .: "response"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/27-lsat/data/lsat_data.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/27-lsat/figures"

readData :: IO LsatRaw
readData = either fail pure =<< eitherDecodeFileStrict dataPath

-- | culm (累積人数) と response (パターンごとの回答) から N 人分の
-- [T長さ] 回答行列へ展開する (Stan の transformed data と同型ロジック)。
unpackResponses :: [Int] -> [[Int]] -> [[Int]]
unpackResponses culm response =
  concat [ replicate cnt pat | (pat, cnt) <- zip response counts ]
  where
    counts = zipWith (-) culm (0 : culm)

-- | Rasch (1PL) 型 二値IRT (Stan 原典と同一構造)。 rows は N人分の
-- [T長さ] 回答行列 (0/1)。
lsatModel :: Int -> [[Int]] -> ModelP ()
lsatModel t rows = do
  alphas <- plateI "item"   t         $ \k -> sample ("alpha" .# k) (Normal 0 100)
  thetas <- plateI "person" (length rows) $ \i -> sample ("theta" .# i) (Normal 0 1)
  beta   <- sample "beta" (HalfNormal 100)
  let alphaV = V.fromList alphas
  plateForM_ "obs" (zip thetas rows) $ \(theta, row) ->
    mapM_ (\(k, yVal) ->
             let logit = beta * theta - (alphaV V.! k)
                 p     = 1 / (1 + exp (negate logit))
             in observe "r" (Bernoulli p) [fromIntegral yVal])
          (zip [0 ..] row)

main :: IO ()
main = do
  d <- readData
  let rows = unpackResponses (lsCulm d) (lsResponse d)
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      model :: ModelP ()
      model = lsatModel (lsT d) rows
      m = noDf |-> hbm cfg model

  putStrLn $ "synthVecIR = " ++
    (case synthVecIR model of
       Just _  -> "Just (vecIR高速経路)"
       Nothing -> "Nothing (legacy walk+ad)")

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  -- N=1000のtheta latentを含むため dashboardFullOf でなく dashboardOf
  -- (健全性2x2パネルのみ・05-mh/10-ratsと同じ判断)。
  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardOf m "r" :: BoundPlot)

  printSummary $ summarize
    (["beta"] ++ [ "alpha_" <> T.pack (show k) | k <- [0 .. lsT d - 1] ])
    (hbmChainsR m)
