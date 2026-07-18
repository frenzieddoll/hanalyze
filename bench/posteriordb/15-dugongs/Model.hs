{-# LANGUAGE OverloadedStrings #-}
-- | dugongs_data-dugongs_model (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。BUGS 古典例「ジュゴンの成長曲線」
-- (N=27頭・体長 Y と年齢 x の非線形漸近成長曲線回帰)。
--
-- Stan 原典 (posteriordb `models/stan/dugongs_model.stan`):
--   parameters {
--     real alpha; real beta;
--     real<lower=.5,upper=1> lambda;
--     real<lower=0> tau;
--   }
--   transformed parameters { sigma = 1/sqrt(tau); U3 = logit(lambda); }
--   model {
--     m[i] = alpha - beta * pow(lambda, x[i]);
--     Y ~ normal(m, sigma);
--     alpha ~ normal(0,1000); beta ~ normal(0,1000);
--     lambda ~ uniform(.5,1); tau ~ gamma(.0001,.0001);
--   }
--
-- 高レベル API (`df |-> hbm`) を使用。 sigma/U3 は log-density に寄与しない
-- transformed parameters なので `deterministic` (PyMC Deterministic 相当) で
-- 事後サンプルに注入する (14-hmm-example と同じパターン)。
--
-- ★実測で踏んだ罠: `lambda ~ Uniform(.5,1)` をそのまま `sample` すると
-- 10-rats で確認済みの罠が新形態で再現した — hanalyze の Uniform は
-- unconstrained 扱い (Distribution.hs:309-310) で unconstrained 初期値
-- raw=0 がそのまま lambda=0 になる (Uniform(lo,hi) は raw をそのまま
-- 値として使う・変換なし)。 lambda=0 は `Uniform(.5,1)` の台の外なので
-- 初手から `logDensity = -Infinity` となり、全 4 chain・全 warmup で HMC
-- 提案が拒否され続けて `alpha=beta=lambda=0.0000・tau=sigma=1.0000` に
-- 完全凍結する現象を実機で確認した (`ess=1000`・`r_hat=NA` は分散ゼロの
-- 兆候)。解決: `lambda ~ Uniform(.5,1)` を「`u ~ Beta(1,1)`
-- (= Uniform(0,1) と同一分布・`UnitIntervalT` 変換で真に (0,1) に収まる
-- 安全な初期値を持つ) → `lambda = 0.5 + 0.5*u`」というアフィン再パラメタ化
-- に置換 (Jacobian は定数 0.5 で HMC の相対密度に影響しないため厳密に
-- 等価)。 14-hmm-example の順序制約 (加算シフト+potential) と同系統の
-- 「unconstrained分布の代わりに真に制約された分布から affine 変換する」
-- 対処法。
--
-- reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-dugongs
module Main (main) where

import Control.Monad (unless)
import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Environment (getArgs)
import Text.Printf (printf)

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedX, dataNamedObs, plateForM_,
                                    deterministic, augmentChainWithDeterministic)
import Hanalyze.Model.HBM (gradPathLabel)
import Hanalyze.Plot (hbmModelSpec)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardFullOf, hbmChainsR)
import Graphics.Hgg.Spec (ColData (..))
import Graphics.Hgg.Frame (BoundPlot, (|>>))
import Graphics.Hgg.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @dugongs_data.json@ 形状 ({"Y":[...], "x":[...], "N":27})。
data DugongsData = DugongsData
  { dugongsY :: [Double]
  , dugongsX :: [Double]
  }

instance FromJSON DugongsData where
  parseJSON = withObject "DugongsData" $ \v ->
    DugongsData <$> v .: "Y" <*> v .: "x"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/15-dugongs/data/dugongs_data.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/15-dugongs/figures"

readData :: IO ([Double], [Double])
readData = do
  d <- either fail pure =<< eitherDecodeFileStrict dataPath
  pure (dugongsY d, dugongsX d)

-- | 非線形漸近成長曲線回帰 (Stan 原典と同一構造)。
dugongsModel :: ModelP ()
dugongsModel = do
  alpha  <- sample "alpha"  (Normal 0 1000)
  beta   <- sample "beta"   (Normal 0 1000)
  u      <- sample "u"      (Beta 1 1)
  lambda <- deterministic "lambda" (0.5 + 0.5 * u)
  tau    <- sample "tau"    (Gamma 0.0001 0.0001)
  sigma  <- deterministic "sigma" (1 / sqrt tau)
  _      <- deterministic "U3" (log (lambda / (1 - lambda)))
  xs <- dataNamedX   "x" []
  ys <- dataNamedObs "Y" []
  plateForM_ "obs" (zip xs ys) $ \(xi, yi) ->
    observe "Y" (Normal (alpha - beta * (lambda ** xi)) sigma) [yi]

main :: IO ()
main = do
  (ys, xs) <- readData
  -- Phase 102 A1: `prof` 引数で図出力 skip (Rasterific が cost centre を
  -- 汚さないため)。本モデルは full でも sampling ~325ms と小さく、hmm の
  -- `reduced` (1chain 縮小) では prof tick が不足するためサンプリング設定は
  -- 本番のまま据え置く。
  args <- getArgs
  let profRun = elem "prof" args
  let df = [ ("x", NumData (V.fromList xs))
           , ("Y", NumData (V.fromList ys))
           ] :: [(T.Text, ColData)]
      -- PyMC 側 (model.py) と同じ設定を定数で揃える。
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      m = df |-> hbm cfg dugongsModel

  -- 勾配経路 = compileGradUV が実際に選ぶ経路 (束縛済 hbmModelSpec で判定・
  -- Phase 91 A4: 生モデルを synthVecIR に渡すと data 空で誤表示するため差替)。
  putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  unless profRun $
    savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
      (noDf |>> dashboardFullOf m "Y" :: BoundPlot)

  -- sigma/U3 は deterministic (log-density に寄与しない transformed
  -- parameters) のため、summarize の前に augmentChainWithDeterministic で
  -- Chain へ注入する (14-hmm-example と同じ理由)。
  let chainsAug = map (augmentChainWithDeterministic dugongsModel) (hbmChainsR m)
  printSummary $ summarize ["alpha", "beta", "lambda", "tau", "sigma"] chainsAug
