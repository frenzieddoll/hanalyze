{-# LANGUAGE OverloadedStrings #-}
-- | hmm_example-hmm_example (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。単純な隠れマルコフモデル
-- (K=2状態・N=100観測・1次元Gaussian放出)。離散潜在状態は NUTS で直接
-- サンプリングできないため、Stan 原典どおり forward algorithm で状態列を
-- 周辺化 (marginalize) した対数尤度を使う。
--
-- Stan 原典 (posteriordb `models/stan/hmm_example.stan`):
--   parameters { simplex[K] theta1; simplex[K] theta2; positive_ordered[K] mu; }
--   model {
--     mu[1] ~ normal(3,1); mu[2] ~ normal(10,1);
--     // forward algorithm (状態列の周辺化)
--     gamma[1,k] = normal_lpdf(y[1]|mu[k],1);  -- pi0 項なし (暗黙一様)
--     gamma[t,k] = log_sum_exp_j(gamma[t-1,j] + log(theta[j,k])) + normal_lpdf(y[t]|mu[k],1);
--     target += log_sum_exp(gamma[N]);
--   }
--
-- hanalyze には既に `hmmForwardLogLik` (log-space forward recursion・
-- Rabiner 1989) と `dirichlet` helper が実装済み (Phase 39-A4)。
-- `theta1`/`theta2` (simplex・Stan原典は暗黙一様事前分布) は
-- `dirichlet name [1,1]` (= Dirichlet(1,1) = simplex上一様) で移植する。
-- `pi0` は Stan 原典に明示的な項が無い (`gamma[1,k]` に log π_0 が加算
-- されない) ため、`hmmForwardLogLik` の pi0 引数には `replicate k 1`
-- (= log 1 = 0、実質「項なし」と等価) を渡す。
--
-- ★`positive_ordered[K]` (mu[1] < mu[2] の順序制約) に対応する分布は
-- hanalyze に無い。★実測で判明: 順序制約なしで `mu_1 ~ Normal(3,1)`・
-- `mu_2 ~ Normal(10,1)` を独立にサンプリングしたところ、chain間で
-- ラベルスイッチング (mu_1/mu_2 の意味がchainごとに入れ替わる) が発生し
-- r_hat が 17台という壊滅的な値になった (両事前分布が7σ以上離れていても、
-- 制約が全く無いと exchangeability により初期値次第でどちらのラベル
-- 付けにも収束しうる)。
--
-- 解決: `mu_2 = mu_1 + gap` (gap>0) という**加算的な順序制約**を導入し、
-- gap 自身の sample 事前分布 (HalfNormal) の寄与を `potential` で正確に
-- 打ち消して `Normal(10,1)` の寄与に置き換える (数学的に厳密・近似では
-- ない): 総 log-density 寄与 = [HalfNormal(gap) の寄与] + [potential] =
-- logDensity(HalfNormal, gap) + (logDensity(Normal 10 1, mu2) −
-- logDensity(HalfNormal, gap)) = logDensity(Normal 10 1, mu2)。
-- Stan の `positive_ordered` 変換のヤコビアンは加算シフトのため 1
-- (寄与なし) なので、この構成は Stan 原典と数学的に等価。
--
-- **reference_posterior_name = "hmm_example-hmm_example"** (posteriordb
-- に公式 reference あり・hanalyze vs PyMC vs 公式referenceの3者比較可能)。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-hmm
module Main (main) where

import Control.Monad (unless)
import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import Data.List (group, intercalate, sort, transpose)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Environment (getArgs)
import Text.Printf (printf)

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, dataNamedX,
                                    dirichlet, potential, logDensity, observeMV,
                                    deterministic, augmentChainWithDeterministic)
import Hanalyze.Model.HBM (gradPathLabel)
import Hanalyze.Plot (hbmModelSpec)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardFullOf, hbmChainsR)
import Graphics.Hgg.Spec (ColData (..))
import Graphics.Hgg.Frame (BoundPlot, (|>>))
import Graphics.Hgg.Backend.Rasterific (savePNGBound)

import Hanalyze.MCMC.Core (Chain, chainAccepted, chainDivergences,
                                    chainTotal, chainTreeDepths, chainVals)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @hmm_example.json@ 形状 ({"N":100, "K":2, "y":[...]})。
data HmmData = HmmData { nObsHmm :: Int, kStates :: Int, yArr :: [Double] }

instance FromJSON HmmData where
  parseJSON = withObject "HmmData" $ \v ->
    HmmData <$> v .: "N" <*> v .: "K" <*> v .: "y"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/14-hmm-example/data/hmm_example.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/14-hmm-example/figures"

readData :: IO HmmData
readData = either fail pure =<< eitherDecodeFileStrict dataPath

-- | K=2 状態 HMM (forward algorithm で状態列を周辺化)。
--
-- Phase 92 A2: 尤度を @potential (hmmForwardLogLik ...)@ から構造化 primitive
-- 'HmmForwardNormal' + 'observeMV' へ移行 (密度は同値・'obsLogSum' が同じ
-- forward recursion を呼ぶ)。 役割 (π_0/遷移行/emission 平均/σ) が型で見える
-- ため、 勾配コンパイラが forward-backward の閉形式随伴 ('hmmAnalyticVG'・
-- AD tape ゼロ) を選べる。 dataNamedX "y" は dashboard の実データ参照用に残す。
hmmModel :: Int -> [Double] -> ModelP ()
hmmModel k ysRaw = do
  mu1 <- sample "mu_1" (Normal 3 1)
  gap <- sample "gap"  (HalfNormal 5)
  mu2 <- deterministic "mu_2" (mu1 + gap)
  potential "mu2_prior" (logDensity (Normal 10 1) mu2 - logDensity (HalfNormal 5) gap)
  theta1 <- dirichlet "theta1" (replicate k 1)
  theta2 <- dirichlet "theta2" (replicate k 1)
  _ys <- dataNamedX "y" []
  let mus   = [mu1, mu2]
      trans = [theta1, theta2]
      pi0   = replicate k 1
  observeMV "y_seq" (HmmForwardNormal pi0 trans mus 1) [ysRaw]

main :: IO ()
main = do
  d <- readData
  -- Phase 92 A1d: `reduced` 引数で prof 用縮小 run (1chain・warmup200+draws200・
  -- 図出力 skip = Rasterific が cost centre を汚さないため)。既定は本番設定。
  args <- getArgs
  -- Phase 92 ess/draw 調査: `seed N` で乱数 seed を差し替え可能に (ess 推定の
  -- seed 感度確認用)。既定 = 1 (従来記録と bit 一致)。
  let reduced = elem "reduced" args
      seedArg = case dropWhile (/= "seed") args of
                  (_ : s : _) -> read s
                  _           -> 1  -- hbmSeed は Word32
  let df = [ ("y", NumData (V.fromList (yArr d))) ] :: [(T.Text, ColData)]
      cfg | reduced   = defaultHBM { hbmChains = 1, hbmSamples = 200
                                    , hbmWarmup = 200, hbmSeed = Just seedArg }
          | otherwise = defaultHBM { hbmChains = 4, hbmSamples = 1000
                                    , hbmWarmup = 1000, hbmSeed = Just seedArg }
      m = df |-> hbm cfg (hmmModel (kStates d) (yArr d))

  -- 勾配経路 = compileGradUV が実際に選ぶ経路 (束縛済 hbmModelSpec で判定・
  -- Phase 91 A4: 生モデルを synthVecIR に渡すと data 空で誤表示するため差替)。
  putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  -- seed≠1 の ess 感度 run では確定図 (seed1 前提) を上書きしない。
  unless (reduced || seedArg /= 1) $
    savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
      (noDf |>> dashboardFullOf m "y" :: BoundPlot)

  -- theta1_*/theta2_*/mu_2 は deterministic (dirichlet の棒折り変換・
  -- mu2の加算的順序制約) のため、summarize の前に augmentChainWithDeterministic
  -- で Chain へ注入する (無いと chainVals が空リストを返し NaN/ess=0 になる)。
  let chainsAug = map (augmentChainWithDeterministic (hmmModel (kStates d) (yArr d))) (hbmChainsR m)
  printSummary $ summarize ["mu_1", "mu_2", "gap", "theta1_0", "theta1_1", "theta2_0", "theta2_1"] chainsAug

  -- Phase 92 ess/draw 調査: per-draw NUTS 診断 (nutpie sample_stats との
  -- 突き合わせ用)。depth は post-warmup・draw 順 (Core.chainTreeDepths)。
  -- accept は burn-in 込みの粗い受理率 (Chain には per-draw accept-stat が
  -- 無いため参考値)。leapfrog/draw ≈ 2^depth。
  putStrLn "\n== per-chain NUTS diagnostics (depth = post-warmup) =="
  printDiagnostics (hbmChainsR m)

  -- 全 chain の post-warmup draw を CSV 化し、Python 側 (arviz) で PyMC と
  -- 同一指標 (rank-normalized ess_bulk・4 chain) の ESS を計算する。
  -- Common.summarize の ess は chain 0 のみ + Geyer IMSE (tau 下限 1 クランプで
  -- n 頭打ち) のため nutpie の ess_bulk と直接比較できない。
  writeDrawsCSV "bench/posteriordb/14-hmm-example/hmm_draws_postwarmup.csv"
    ["mu_1", "mu_2", "gap", "theta1_0", "theta1_1", "theta2_0", "theta2_1"]
    chainsAug

printDiagnostics :: [Chain] -> IO ()
printDiagnostics chains = do
  mapM_ printOne (zip [0 :: Int ..] chains)
  let allDepths = concatMap chainTreeDepths chains
      histo = map (\g -> (head g, length g)) . group . sort $ allDepths
  putStrLn $ "depth histogram (all chains): "
          ++ intercalate ", " [ show dep ++ ":" ++ show c | (dep, c) <- histo ]
  where
    printOne (i, ch) = do
      let ds    = chainTreeDepths ch
          nd    = fromIntegral (length ds) :: Double
          meanD = fromIntegral (sum ds) / nd
          leap  = sum [ 2 ^ dep | dep <- ds ] :: Int
          acc   = fromIntegral (chainAccepted ch) / fromIntegral (chainTotal ch) :: Double
      printf "chain %d: mean_depth=%.2f  est_leapfrog/draw=%.1f  div=%d  accept(incl-warmup)=%.3f\n"
        i meanD (fromIntegral leap / nd :: Double) (length (chainDivergences ch)) acc

writeDrawsCSV :: FilePath -> [T.Text] -> [Chain] -> IO ()
writeDrawsCSV path pars chains = do
  let rows = concat
        [ [ show ci ++ "," ++ show di ++ ","
              ++ intercalate "," (map (printf "%.17g") vals)
          | (di, vals) <- zip [0 :: Int ..] (transpose cols) ]
        | (ci, ch) <- zip [0 :: Int ..] chains
        , let cols = [ chainVals p ch | p <- pars ] ]
      header = "chain,draw," ++ intercalate "," (map T.unpack pars)
  writeFile path (unlines (header : rows))
  putStrLn $ "draws CSV -> " ++ path
