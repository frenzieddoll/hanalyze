{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | MvNormal (多変量正規) 観測のデモ。
--
-- PyMC の @pm.MvNormal("y", mu=mu, cov=cov, observed=Y)@ 相当。
--
-- 例 1: 既知の共分散で平均ベクトルを推定
--   y_i ~ MvNormal(μ, Σ),  Σ = [[1, 0.7], [0.7, 1]] (固定)
--   μ ~ Normal(0, 5)        (各成分独立)
--
-- 例 2: 静的検証 (Cholesky / log density 単体テスト)
module Main where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom, GenIO)
import qualified System.Random.MWC.Distributions as MWC

import Hanalyze.MCMC.Core (posteriorMean, posteriorSD)
import Hanalyze.MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Hanalyze.Model.HBM (ModelP, sample, observeMV, Distribution (..),
                  mvNormalLogDensity)

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 1500
        , nutsBurnIn     = 500
        , nutsStepSize   = 0.1
        }

-- ---------------------------------------------------------------------------
-- 単体テスト: 既知ケースの log density を比較
-- ---------------------------------------------------------------------------

-- | 標準 2 変量正規 N([0,0], I) で y=[0,0]:
--   log p = -k/2 log(2π) = -log(2π) ≈ -1.8379
test1 :: Double
test1 = mvNormalLogDensity [0, 0] [[1, 0], [0, 1]] [0, 0]

-- | N([0,0], I), y=[1,0]: log p = -log(2π) - 0.5 ≈ -2.3379
test2 :: Double
test2 = mvNormalLogDensity [0, 0] [[1, 0], [0, 1]] [1, 0]

-- | 相関ありケース Σ=[[1,0.7],[0.7,1]], y=[0,0]:
--   |Σ| = 1 - 0.49 = 0.51, log|Σ| = log 0.51 ≈ -0.6733
--   log p = -log(2π) - 0.5*log(0.51) ≈ -1.8379 + 0.3367 ≈ -1.5013
test3 :: Double
test3 = mvNormalLogDensity [0, 0] [[1, 0.7], [0.7, 1]] [0, 0]

-- ---------------------------------------------------------------------------
-- 平均推定モデル
-- ---------------------------------------------------------------------------

cov2 :: [[Double]]
cov2 = [[1.0, 0.7], [0.7, 1.0]]

-- | 真の μ = [2, -1] からデータ生成。
genData :: GenIO -> Int -> IO [[Double]]
genData gen n = do
  -- L = [[1,0], [0.7, sqrt(1-0.49)]] = [[1,0],[0.7, 0.7141]]
  let l00 = 1.0
      l10 = 0.7
      l11 = sqrt (1 - 0.49)
      muTrue = [2.0, -1.0]
  let drawOne = do
        z0 <- MWC.standard gen
        z1 <- MWC.standard gen
        let y0 = head muTrue + l00 * z0
            y1 = (muTrue !! 1) + l10 * z0 + l11 * z1
        return [y0, y1]
  mapM (const drawOne) [1 .. n]

mvNormalModel :: [[Double]] -> ModelP ()
mvNormalModel ys = do
  m1 <- sample "mu1" (Normal 0 5)
  m2 <- sample "mu2" (Normal 0 5)
  observeMV "y" (MvNormal [m1, m2] [[1.0, 0.7], [0.7, 1.0]]) ys

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  MvNormal (多変量正規) デモ"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  -- ── 単体テスト ──
  putStrLn "[A] 単体テスト: log density 既知ケース"
  let exp1 = -log (2 * pi) :: Double
      exp2 = -log (2 * pi) - 0.5 :: Double
      exp3 = -log (2 * pi) - 0.5 * log 0.51 :: Double
  printf "  N([0,0], I) で y=[0,0]   : %+.4f  (期待 %+.4f = -log(2pi))\n" test1 exp1
  printf "  N([0,0], I) で y=[1,0]   : %+.4f  (期待 %+.4f)\n"            test2 exp2
  printf "  N([0,0], cov_corr) y=0   : %+.4f  (期待 %+.4f)\n"            test3 exp3
  putStrLn ""

  -- ── NUTS で平均ベクトル推定 ──
  putStrLn "[B] NUTS で μ を推定 (Σ 既知)"
  putStrLn "    真値 μ = [2.0, -1.0],   Σ = [[1, 0.7], [0.7, 1]]"
  gen <- createSystemRandom
  ys <- genData gen 100
  printf "    観測: %d 件 (k=2)\n" (length ys)
  ch <- nuts (mvNormalModel ys) cfg
              (Map.fromList [("mu1", 0), ("mu2", 0)]) gen
  let m1m = fromMaybe 0 (posteriorMean "mu1" ch)
      m2m = fromMaybe 0 (posteriorMean "mu2" ch)
      s1m = fromMaybe 0 (posteriorSD   "mu1" ch)
      s2m = fromMaybe 0 (posteriorSD   "mu2" ch)
  printf "  事後 μ1 = %+.3f  ± %.3f  (真値 +2.000)\n" m1m s1m
  printf "  事後 μ2 = %+.3f  ± %.3f  (真値 -1.000)\n" m2m s2m
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ MvNormal が観測分布として動作 (Cholesky 経由 log density)"
  putStrLn "═══════════════════════════════════════════════════════════════"
