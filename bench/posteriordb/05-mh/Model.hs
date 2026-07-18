{-# LANGUAGE OverloadedStrings #-}
-- | Mh_data-Mh_model (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89/90: posteriordb 横断ベンチマーク + Phase 90 A3 (vecIR ギャップ
-- 解消: 05-mh = capture-recapture・ZeroInflatedBinomial + 個体ごとの
-- ランダム効果)。
--
-- Stan 原典 (posteriordb `models/stan/Mh_model.stan`。 BPA本 Ch.6・
-- M=385個体・T=5回のサンプリング機会):
--   parameters { real<lower=0,upper=1> omega; real<lower=0,upper=1> mean_p;
--                real<lower=0,upper=5> sigma; vector[M] eps_raw; }
--   transformed parameters { vector[M] eps = logit(mean_p) + sigma*eps_raw; }
--   model {
--     eps_raw ~ normal(0, 1);
--     for (i in 1:M) {
--       if (y[i] > 0)
--         target += bernoulli_lpmf(1|omega) + binomial_logit_lpmf(y[i]|T,eps[i]);
--       else
--         target += log_sum_exp(bernoulli_lpmf(1|omega)+binomial_logit_lpmf(0|T,eps[i]),
--                                bernoulli_lpmf(0|omega));
--     }
--   }
--
-- 05-mh/README.md (Phase 89) で代数的に導出済みのとおり、この尤度は
-- hanalyze の `ZeroInflatedBinomial n ψ p` (ψ=1-omega, p=invlogit(eps)) と
-- 厳密に一致する。 reference_posterior_name = null (2者比較のみ)。
--
-- Phase 90 A3: `SDZIBinom`/`VGZIBinom`/`VOZIBinom` (新設 vecIR family) を
-- 使用。 eps_i (個体ごとのランダム効果) は `plateI` で M 個の latent を
-- 宣言し、 族 gather (`!!`) 経由で観測式に取り込む (`docs/api-guide` の
-- eight-schools 型パターンと同型)。 トイモデルで `synthVecIR` = `Just` を
-- 実測確認済み (個体ごとの logit-link random effect を含む形でも通る)。
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedObs, plateI, plateForM_, (.#), withData)
import Hanalyze.Model.HBM (gradPathLabel)
import Hanalyze.Plot (hbmModelSpec)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardOf, hbmChainsR)
import Hgg.Plot.Spec (ColData (..))
import Hgg.Plot.Frame (BoundPlot, (|>>))
import Hgg.Plot.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)
import Control.Monad (unless)
import Data.List (group, sort)
import System.Environment (getArgs)
import Text.Printf (printf)

import Hanalyze.MCMC.Core (chainTreeDepths)

-- | posteriordb の @Mh_data.json@ 形状 ({"M":385, "T":5, "y":[...]})。
data MhData = MhData { yObs :: [Double], tOccasions :: Int }

instance FromJSON MhData where
  parseJSON = withObject "MhData" $ \v ->
    MhData <$> v .: "y" <*> v .: "T"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/05-mh/data/Mh_data.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/05-mh/figures"

readData :: IO ([Double], Int)
readData = do
  d <- either fail pure =<< eitherDecodeFileStrict dataPath
  pure (yObs d, tOccasions d)

-- | capture-recapture (Mh モデル)。 T (サンプリング機会数) はデータ由来の
-- 固定定数として closure で渡す (ctor 引数化・GLM-Poisson 等と同じ流儀)。
mhModel :: Int -> ModelP ()
mhModel t = do
  -- Stan 原典は omega/mean_p ~ Uniform(0,1) (暗黙一様事前分布) だが、
  -- hanalyze の `Uniform` は制約変換が現状 unconstrained 扱い
  -- (`01-glm-poisson/README.md` に既知の課題として記載済) のため、
  -- vecIR probe (2点評価) で mean_p が (0,1) 域外の値を取り
  -- `log(meanP/(1-meanP))` が NaN 化して誤フォールバックする実害が
  -- M=385 (個体ごとの random effect) という多latentモデルで発覚した。
  -- Beta(1,1) は Uniform(0,1) と数学的に同一の分布で、hanalyze では
  -- 実際に (0,1) へ写す変換を持つため確率的に等価かつ probe 安全。
  omega  <- sample "omega"  (Beta 1 1)
  meanP  <- sample "mean_p" (Beta 1 1)
  sigma  <- sample "sigma"  (Uniform 0 5)
  ys     <- dataNamedObs "y" []
  epsRaws <- plateI "ind" (length ys) $ \i -> sample ("eps_raw" .# i) (Normal 0 1)
  let logitMeanP = log (meanP / (1 - meanP))
  plateForM_ "obs" (zip [0 ..] ys) $ \(i, yi) ->
    let eps = logitMeanP + sigma * (epsRaws !! i)
        p   = 1 / (1 + exp (negate eps))
    in observe "y" (ZeroInflatedBinomial t (1 - omega) p) [yi]

main :: IO ()
main = do
  (ys, t) <- readData
  -- Phase 96 A1: `prof` 引数で図出力 skip (Phase 102 A1 の dugongs/radon と
  -- 同型。Rasterific が cost centre を汚さないため)。サンプリング設定は
  -- 本番のまま据え置く。
  args <- getArgs
  let profRun = elem "prof" args
  let df = [ ("y", NumData (V.fromList ys)) ] :: [(T.Text, ColData)]
      -- PyMC 側 (model.py) と同じ設定を定数で揃える。
      -- Phase 96 A5: M=I 期間 (init buffer + 第1 window) の deep tree 抑制。
      -- 無効時は init 期が 61.9 steps/iter (avg depth≈6・ε 中央値 0.107 vs
      -- 収束後 0.242 の鋸歯) で warmup の 32% を浪費する。Just 4 で warmup
      -- evals −25〜28% (116-121k → 85-91k)・seed 1/2/3 で posterior 統計一致・
      -- ess/s 分散も縮小 (実測 root: experiments/phase96-mh-reconfirm/)。
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1
                        , hbmWarmupInitMaxDepth = Just 4 }
      m = df |-> hbm cfg (mhModel t)

  -- 勾配経路 = compileGradUV が実際に選ぶ経路 (束縛済 hbmModelSpec で判定・
  -- Phase 91 A4: 生モデルを synthVecIR に渡すと data 空で誤表示するため差替)。
  putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  -- Phase 96 A4: draws 区間の軌道長を nutpie の n_steps(draws) と同一区間で
  -- 突合するための tree depth 統計 (leapfrog 数 ≈ 2^depth − 1)。
  let depths = concatMap chainTreeDepths (hbmChainsR m)
      hist   = map (\g -> (head g, length g)) (group (sort depths))
      nLeap  = sum [ (2 :: Int) ^ d - 1 | d <- depths ]
  printf "tree depth (draws): hist=%s  leapfrog~=%d  steps/draw~=%.1f\n"
         (show hist) nLeap
         (fromIntegral nLeap / fromIntegral (max 1 (length depths)) :: Double)

  -- M=385 個体分の eps_raw latent を含むため 'dashboardFullOf' (全 param の
  -- [事後分布|trace] グリッド) は 52MB 超の巨大画像になり非実用的 (実測)。
  -- 健全性 2x2 パネル (DAG/forest/PPC/energy) のみの 'dashboardOf' を使う
  -- (forest には全 latent が出るが 1 行/param のコンパクト表示なので実用的)。
  unless profRun $
    savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
      (noDf |>> dashboardOf m "y" :: BoundPlot)

  printSummary $ summarize ["omega", "mean_p", "sigma"] (hbmChainsR m)
