{-# LANGUAGE OverloadedStrings #-}
-- | three_men1-ldaK2 (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。LDAトピックモデル (K=2固定・
-- V=249語彙・M=6文書・N=4999語インスタンス)。離散潜在トピック割当を
-- 周辺化 (collapsed) した対数尤度を使う (Stan 原典と同型)。
--
-- Stan 原典 (posteriordb `models/stan/ldaK2.stan`):
--   transformed data { int K=2; vector[K] alpha=[1,1]; vector[V] beta=[1,...,1]; }
--   parameters { array[M] simplex[K] theta; array[K] simplex[V] phi; }
--   model {
--     theta[m] ~ dirichlet(alpha); phi[k] ~ dirichlet(beta);
--     for (n in 1:N) {
--       gamma[k] = log(theta[doc[n],k]) + log(phi[k,w[n]]);
--       target += log_sum_exp(gamma);
--     }
--   }
--
-- hanalyze の 'dirichlet' helper (stick-breaking・Phase 39-A4) を使い
-- theta[m]/phi[k] を simplex latent として作る。周辺化尤度は
-- 'Hanalyze.Model.HBM.Util.logSumExpA' (K-way log-sum-exp) を
-- 'potential' で加算する (14-hmm-example の forward algorithm と同系統の
-- 「observe を使わず potential で尤度を直書きする」パターン)。
--
-- ★スケールへの配慮: V=249 の 'dirichlet' は内部で stick-breaking の
-- O(V) 演算 (scanl/scanr + (!!)) を行うため呼び出しコストが V に対して
-- 効くが K=2 回のみ (V に対して 1 回・K に対する繰り返しではない)。
-- 尤度ループ (N=4999) 側の theta/phi 参照は 'dirichlet' が返す素の Haskell
-- リストのまま `!!` で引くと O(V) の索引コストが 4999 回積み上がるため、
-- 'Data.Vector' に変換してから索引する (O(1))。 w/doc (posteriordb は
-- 1-based) は微分対象ではない構造的な添字データなので、`df`/`dataNamedX`
-- 経由の束縛ではなく素の @[Int]@ をクロージャで直接渡す (05-mh の @T@ と
-- 同じ流儀)。
--
-- ★posteriordbの keywords に "multimodal" とある: トピックは交換可能
-- (a priori に θ/φ のラベルに区別が無い) なため、chain ごとに異なる
-- トピックラベルへ収束するラベルスイッチングが起き得る (04-low-dim-gauss-mix
-- と同種の既知の課題)。 14-hmm-example のような加算的順序制約は θ/φ が
-- 同時に入れ替わる構造のため単純には適用できず、本モデルでは対処しない
-- (現象が出たら記録するに留める)。
--
-- reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
--
-- ★実測で判明: 本番設定 (4chain・warmup1000+draws1000) は 30分timeoutで
-- 完走せず (2026-07-11)。中規模probe (1chain・warmup100+draws100=200
-- iteration) は 325.1秒で完走・値は正常収束 (r_hat≈0.99-1.00・NaN無し)
-- だったため実装自体にバグは無いが、~510次元 (M*(K-1)+K*(V-1)=6+496) の
-- legacy walk+ad (vecIR非対応) では 1 iteration あたり平均 1.6秒
-- (単純外挿で本番 2000 iteration/chain ≈ 54分) かかり、13-traffic-accident-nyc
-- (N=1921×2 latent) と同様に実用外の規模と判断・保留 (user 確認済)。
-- 詳細は `16-lda/README.md` 参照。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-lda
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Printf (printf)

import Hanalyze.Model.HBM (ModelP, potential, plateI, plateForM_,
                                    dirichlet, (.#), augmentChainWithDeterministic)
import Hanalyze.Model.HBM.Util (logSumExpA)
import Hanalyze.Model.HBM (gradPathLabel)
import Hanalyze.Plot (hbmModelSpec)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardOf, hbmChainsR)
import Hgg.Plot.Spec (ColData (..))
import Hgg.Plot.Frame (BoundPlot, (|>>))
import Hgg.Plot.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @three_men1.json@ 形状
-- ({"V":249, "M":6, "N":4999, "w":[...], "doc":[...]}・1-based 添字)。
data LdaData = LdaData
  { ldaV   :: Int
  , ldaM   :: Int
  , ldaW   :: [Int]
  , ldaDoc :: [Int]
  }

instance FromJSON LdaData where
  parseJSON = withObject "LdaData" $ \v ->
    LdaData <$> v .: "V" <*> v .: "M" <*> v .: "w" <*> v .: "doc"

noDf :: [(T.Text, ColData)]
noDf = []

kTopics :: Int
kTopics = 2

dataPath :: FilePath
dataPath = "bench/posteriordb/16-lda/data/three_men1.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/16-lda/figures"

readData :: IO LdaData
readData = either fail pure =<< eitherDecodeFileStrict dataPath

-- | LDA (K=2固定・周辺化尤度)。 vVoc/mDocs/ws/docs はデータ由来の固定定数
-- として closure で渡す (05-mh の @T@ と同じ流儀・w/doc は微分対象ではない
-- ため df 経由で束縛しない)。
ldaModel :: Int -> Int -> [Int] -> [Int] -> ModelP ()
ldaModel vVoc mDocs ws docs = do
  thetaLists <- plateI "doc"   mDocs   $ \i -> dirichlet ("theta" .# i) (replicate kTopics 1)
  phiLists   <- plateI "topic" kTopics $ \k -> dirichlet ("phi"   .# k) (replicate vVoc 1)
  let thetaV = V.fromList (map V.fromList thetaLists)  -- M x K (O(1) 索引)
      phiV   = V.fromList (map V.fromList phiLists)     -- K x V (O(1) 索引)
  plateForM_ "obs" (zip docs ws) $ \(d, w) ->
    let gammas = [ log ((thetaV V.! (d - 1)) V.! kk) + log ((phiV V.! kk) V.! (w - 1))
                 | kk <- [0 .. kTopics - 1] ]
    in potential "lda_loglik" (logSumExpA gammas)

main :: IO ()
main = do
  d <- readData
  let vVoc  = ldaV d
      mDocs = ldaM d
      ws    = ldaW d
      docs  = ldaDoc d
      -- ダッシュボード用の名目 obs 列 (14-hmm-example と同様、実際の尤度は
      -- potential で直書きするため観測ノードは存在しない・PPCパネルは空)。
      df = [ ("w", NumData (V.fromList (map fromIntegral ws))) ] :: [(T.Text, ColData)]
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      model :: ModelP ()
      model = ldaModel vVoc mDocs ws docs
      m = df |-> hbm cfg model

  -- 勾配経路 = compileGradUV が実際に選ぶ経路 (束縛済 hbmModelSpec で判定・
  -- Phase 91 A4: 生モデルを synthVecIR に渡すと data 空で誤表示するため差替)。
  putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  -- M*K + K*V = 12+498 = 510 latent (deterministic込み) と大規模なため、
  -- 05-mh/10-rats と同様 dashboardFullOf でなく dashboardOf (健全性2x2パネル
  -- のみ・forestに全latentがコンパクト表示) を使う。
  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardOf m "w" :: BoundPlot)

  -- theta_i_j は dirichlet の deterministic (stick-breaking) のため
  -- augmentChainWithDeterministic で Chain へ注入してから summarize する。
  let chainsAug = map (augmentChainWithDeterministic model) (hbmChainsR m)
      thetaNames = [ "theta_" <> T.pack (show i) <> "_" <> T.pack (show k)
                   | i <- [0 .. mDocs - 1], k <- [0 .. kTopics - 1] ]
  printSummary $ summarize thetaNames chainsAug
