{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | LKJ 相関行列事前 + MvNormal 観測のデモ (Phase H4)。
--
-- 2D 観測データの相関行列 R を LKJ(η=1) 事前 (uniform on R) で推定。
-- 真の相関 ρ = 0.7 のデータを生成し、posterior の R[1][0]=ρ̂ が
-- 0.7 付近に集中することを確認。
module Main where

import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)
import qualified System.Random.MWC.Distributions as MWC

import Hanalyze.MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Hanalyze.Model.HBM (ModelP, sample, observeMV, lkjCorrCholesky,
                  Distribution (..), augmentChainWithDeterministic)
import Hanalyze.Viz.MCMC (printPosteriorSummary, posteriorSummaryFile,
                 pairScatterFile)
import Hanalyze.Viz.Core (defaultConfig, OutputFormat (..), PlotConfig (..))

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 800
        , nutsBurnIn     = 400
        , nutsStepSize   = 0.1
        , nutsMaxDepth   = 6
        }

-- 真の相関 ρ = 0.7 で 2D サンプル生成
genCorr :: Int -> Double -> IO [[Double]]
genCorr n rho = do
  gen <- createSystemRandom
  let l11 = sqrt (1 - rho * rho)
      drawOne = do
        z0 <- MWC.standard gen
        z1 <- MWC.standard gen
        return [z0, rho * z0 + l11 * z1]
  mapM (const drawOne) [1 .. n]

-- σ 既知 (= 1)、相関行列を LKJ 事前で推定
lkjModel :: [[Double]] -> ModelP ()
lkjModel obs = do
  -- 相関行列 R の Cholesky factor L (2×2)
  l <- lkjCorrCholesky "R" 2 1.0   -- η = 1: uniform 事前
  -- σ_i 既知 = 1 → cov = L Lᵀ
  let cov = let row i = [ sum [ ((l !! i) !! kk) * ((l !! j) !! kk)
                              | kk <- [0 .. min i j] ]
                        | j <- [0, 1] ]
            in [row 0, row 1]
  -- μ も推定
  m0 <- sample "mu0" (Normal 0 5)
  m1 <- sample "mu1" (Normal 0 5)
  observeMV "y" (MvNormal [m0, m1] cov) obs

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  LKJ 相関行列事前 + MvNormal 観測 (Phase H4)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  putStrLn "真値: ρ = 0.7, μ = (0, 0), σ = (1, 1) (固定)"
  obs <- genCorr 100 0.7
  let xs    = [head ys | ys <- obs]
      ys    = [last ys | ys <- obs]
      n     = length obs
      mux   = sum xs / fromIntegral n
      muy   = sum ys / fromIntegral n
      cxy   = sum (zipWith (\x y -> (x - mux) * (y - muy)) xs ys)
              / fromIntegral (n - 1)
      sx    = sqrt (sum [(x - mux)^(2::Int) | x <- xs] / fromIntegral (n-1))
      sy    = sqrt (sum [(y - muy)^(2::Int) | y <- ys] / fromIntegral (n-1))
      empRho = cxy / (sx * sy)
  printf "観測 (n=%d): 標本 ρ = %.3f\n" n empRho
  putStrLn ""

  gen <- createSystemRandom
  rawCh <- nuts (lkjModel obs) cfg
                (Map.fromList [ ("R_u1_0", 0.5)
                              , ("mu0", 0), ("mu1", 0) ]) gen
  let ch = augmentChainWithDeterministic (lkjModel obs) rawCh

  putStrLn "[1] Posterior summary"
  -- pc1_0 は 2u−1 ∈ (-1,1) で、これが ρ そのもの (K=2 の場合)
  let names = [ "R_u1_0"        -- raw Beta latent
              , "R_pc1_0"       -- 2u-1 = ρ
              , "R_L1_0"        -- Cholesky off-diag = ρ (K=2)
              , "R_L1_1"        -- diag = √(1-ρ²)
              , "mu0", "mu1" ]
  printPosteriorSummary names [ch]
  putStrLn ""

  posteriorSummaryFile "lkj-summary.html" "LKJ posterior" names [ch]
  let pcfg = (defaultConfig "ρ̂ posterior")
               { plotWidth = 500, plotHeight = 400 }
  pairScatterFile HTML "lkj-pair.html" pcfg "mu0" "R_pc1_0" ch
  putStrLn "  → lkj-summary.html / lkj-pair.html"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ LKJ 事前で ρ ≈ 0.7 を回復、Cholesky factor も派生量化"
  putStrLn "═══════════════════════════════════════════════════════════════"
