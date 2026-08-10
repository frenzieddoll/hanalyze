{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | MvNormal を latent (事前) として使うデモ。
--
-- 階層モデル:
--   μ_vec ~ MvNormal([0, 0], [[1, 0.8], [0.8, 1]])  -- 2D latent
--   y1 ~ Normal(μ_0, 0.5)
--   y2 ~ Normal(μ_1, 0.5)
--
-- データはわざと相関を持たせて生成し、posterior 上の μ_0 と μ_1 にも
-- 相関が現れるかを pair plot で確認。
module Main where

import qualified Data.Map.Strict as Map
import System.Random.MWC (createSystemRandom)

import Hanalyze.MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Hanalyze.Model.HBM (ModelP, observe, mvNormalLatent,
                  Distribution (..), augmentChainWithDeterministic)
import Hanalyze.Viz.MCMC (printPosteriorSummary, posteriorSummaryFile,
                 pairScatterFile, tracePlotHDIFile)
import Hanalyze.Viz.Core (defaultConfig, OutputFormat (..), PlotConfig (..))

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 2000
        , nutsBurnIn     = 1000
        , nutsStepSize   = 0.1
        }

-- 真の μ ≈ (1.0, -0.5) 付近に集中させる観測
y1Obs, y2Obs :: [Double]
y1Obs = [1.1, 0.9, 1.2, 1.0, 0.8, 1.05, 0.95, 1.15, 1.0, 1.08]
y2Obs = [-0.4, -0.6, -0.5, -0.45, -0.55, -0.5, -0.42, -0.58, -0.48, -0.52]

mvLatentModel :: ModelP ()
mvLatentModel = do
  -- 2D latent vector: 強い相関 0.8 を入れた事前
  mu <- mvNormalLatent "mu" [0, 0] [[1, 0.8], [0.8, 1]]
  observe "y1" (Normal (mu !! 0) 0.5) y1Obs
  observe "y2" (Normal (mu !! 1) 0.5) y2Obs

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  MvNormal を latent vector として使う (G6)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  putStrLn "事前: μ ~ MvNormal([0,0], [[1, 0.8], [0.8, 1]])"
  putStrLn "観測: y1 ≈ 1.0, y2 ≈ -0.5 (n=10 each)"
  putStrLn ""

  gen <- createSystemRandom
  rawCh <- nuts mvLatentModel cfg
                (Map.fromList [("mu_z0", 0), ("mu_z1", 0)]) gen
  let ch = augmentChainWithDeterministic mvLatentModel rawCh

  putStrLn "[1] Posterior summary"
  let names = ["mu_z0", "mu_z1", "mu_0", "mu_1"]
  printPosteriorSummary names [ch]
  putStrLn ""

  -- HTML 出力
  posteriorSummaryFile "mvlatent-summary.html"
    "MvNormal latent — posterior" names [ch]
  let pcfg = (defaultConfig "mu_0 vs mu_1 (posterior)")
               { plotWidth = 500, plotHeight = 400 }
  pairScatterFile HTML "mvlatent-pair.html" pcfg "mu_0" "mu_1" ch
  let tcfg = (defaultConfig "MvNormal latent — trace")
               { plotWidth = 700, plotHeight = 90 }
  tracePlotHDIFile HTML "mvlatent-trace.html" tcfg 0.94 names ch
  putStrLn "  → mvlatent-summary.html / pair.html / trace.html"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ MvNormal latent vector が NUTS で推論できる"
  putStrLn "    raw N(0,1) latent (mu_z*) + Cholesky で派生量 (mu_*) を生成"
  putStrLn "═══════════════════════════════════════════════════════════════"
