{-# LANGUAGE OverloadedStrings #-}
-- | gp_pois_regr-gp_regr (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89/90: posteriordb 横断ベンチマーク + Phase 90 A2 (vecIR ギャップ
-- 解消: 07-gp-regr = GP カーネル + Cholesky分解)。
--
-- Stan 原典 (posteriordb `models/stan/gp_regr.stan` — data_name は
-- `gp_pois_regr` だが実際に走るのは `gp_regr` モデル・Gaussian 尤度):
--   parameters { real<lower=0> rho; real<lower=0> alpha; real<lower=0> sigma; }
--   model {
--     matrix[N,N] cov = gp_exp_quad_cov(x, alpha, rho)
--                       + diag_matrix(rep_vector(sigma, N));
--     matrix[N,N] L_cov = cholesky_decompose(cov);
--     rho ~ gamma(25, 4); alpha ~ normal(0, 2); sigma ~ normal(0, 1);
--     y ~ multi_normal_cholesky(rep_vector(0, N), L_cov);
--   }
--
-- reference_posterior_name = "gp_pois_regr-gp_regr" (posteriordb に公式
-- reference posterior あり・3者比較可能)。
--
-- Phase 90 A2: `Hanalyze.Model.HBM.gpExpQuadCov` (Model.hs 追加) で
-- 共分散行列を構築し、既存の `MvNormal` distribution (`obsLogSum` が AD対応
-- 'mvNormalLogDensity' = 'choleskyL' 経由) にそのまま渡す。GP 専用の新しい
-- distribution 型は不要 — 密行列は vecIR に構造的に載らない (Phase 90 A1
-- 調査で判定済み) が、legacy walk+ad 経路でそのまま動く。
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Environment (getArgs)

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observeMV,
                                    dataNamedX, dataNamedObs)
import Hanalyze.Model.HBM (gradPathLabel)
import Hanalyze.Plot (hbmModelSpec)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardFullOf, hbmChainsR)
import Graphics.Hgg.Spec (ColData (..))
import Graphics.Hgg.Frame (BoundPlot, (|>>))
import Graphics.Hgg.Backend.Rasterific (savePNGBound)

import Text.Printf (printf)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @gp_pois_regr.json@ 形状 ({"N":11, "x":[...], "y":[...], "k":[...]})。
-- `gp_regr` モデルが使うのは x/y のみ (k は gp_pois_regr モデル専用・未使用)。
data GpRegrData = GpRegrData
  { x :: [Double]
  , y :: [Double]
  }

instance FromJSON GpRegrData where
  parseJSON = withObject "GpRegrData" $ \v ->
    GpRegrData <$> v .: "x" <*> v .: "y"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/07-gp-regr/data/gp_pois_regr.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/07-gp-regr/figures"

readData :: IO ([Double], [Double])
readData = do
  d <- either fail pure =<< eitherDecodeFileStrict dataPath
  pure (x d, y d)

-- | Phase 95 A4: N-scaling ベンチ用の合成データ (PyMC scaling script と同一式)。
-- x = linspace(-10,10,N)・y = 2 + sin(x/2) + 0.3 cos(3x) (決定的・GP 相当の滑らかさ)。
syntheticData :: Int -> ([Double], [Double])
syntheticData n = (xs, ys)
  where
    xs = [ -10 + 20 * fromIntegral i / fromIntegral (n - 1) | i <- [0 .. n - 1] ]
    ys = [ 2 + sin (xi / 2) + 0.3 * cos (3 * xi) | xi <- xs ]

-- | GP 回帰 (RBF カーネル + Gaussian 尤度)。Phase 95 B-dsl: カーネル役割
-- (x/α/ρ/σ) を型で明示保持する 'MvNormalGpRBF' で 1 個の N 次元同時観測として
-- 尤度評価する (Stan の multi_normal_cholesky と等価・zero mean)。共分散
-- Σ = α² exp(-0.5 d²/ρ²) + (1e-10 + σ)·I は Distribution 側が内部構築し、勾配は
-- 閉形式随伴 ('gpRBFAnalyticVG'・Cholesky を AD tape に載せない) で高速評価される。
gpRegrModel :: ModelP ()
gpRegrModel = do
  rho   <- sample "rho"   (Gamma 25 4)
  alpha <- sample "alpha" (HalfNormal 2)
  sigma <- sample "sigma" (HalfNormal 1)
  xs <- dataNamedX   "x" []
  ys <- dataNamedObs "y" []
  observeMV "y" (MvNormalGpRBF xs alpha rho sigma) [ys]

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- Phase 95 A4: `scale <N>` = 合成データで N-scaling ベンチ (図出力なし・
    -- sampling wall と posterior 平均のみ・PyMC scaling script と対比)。
    ("scale" : nStr : _) -> do
      let n = read nStr :: Int
          (xs, ys) = syntheticData n
          df = [ ("x", NumData (V.fromList xs)), ("y", NumData (V.fromList ys)) ]
               :: [(T.Text, ColData)]
          cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                            , hbmWarmup = 1000, hbmSeed = Just 1 }
          m = df |-> hbm cfg gpRegrModel
      -- Phase 96 A2: 勾配経路 (束縛済 hbmModelSpec で判定・Phase 91 A4 と同型)。
      putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)
      (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
      printf "N=%d  sampling wall = %.1f ms (draws only)\n" n samplingMs
      printSummary $ summarize ["rho", "alpha", "sigma"] (hbmChainsR m)
    _ -> do
      (xs, ys) <- readData
      let df = [ ("x", NumData (V.fromList xs))
               , ("y", NumData (V.fromList ys))
               ] :: [(T.Text, ColData)]
          -- PyMC 側 (model.py) と同じ設定を定数で揃える。
          cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                            , hbmWarmup = 1000, hbmSeed = Just 1 }
          m = df |-> hbm cfg gpRegrModel

      -- Phase 96 A2: 勾配経路 (束縛済 hbmModelSpec で判定・Phase 91 A4 と同型)。
      putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

      -- サンプリング**のみ**の壁時計 (PyMC run_pymc_matrix.py の t0=perf_counter();
      -- pm.sample() と対応させる・Common.timeSamplingMs 参照)。dashboardFullOf 等
      -- 後続処理は hbmChainsR の thunk が既に強制済みのものを再利用するので、
      -- 二重計算にはならない。
      (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
      printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

      savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
        (noDf |>> dashboardFullOf m "y" :: BoundPlot)

      printSummary $ summarize ["rho", "alpha", "sigma"] (hbmChainsR m)
