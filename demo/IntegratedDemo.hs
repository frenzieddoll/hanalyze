{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | 統合デモ (Phase K2): 1 つのリアルなシナリオで複数機能を組合せる。
--
-- シナリオ: 2 つの病院での治療効果比較 (階層モデル)
--   - 各病院 j で患者 i に効果 y_{ij} を観測
--   - 病院効果 μ_j ~ MvNormal([μ_pop, μ_pop], Σ) で相関を持つ
--     (= 同じ患者層を共有してるので相関がある)
--   - σ_y ~ InverseGamma(2, 3) (Phase I — 共役事前)
--   - 派生量: 治療効果差 Δ = μ_1 - μ_2  (Phase G1 — Deterministic)
--
-- 使う機能:
--   * Phase G6: mvNormalLatent (病院効果 μ_j のベクトル latent)
--   * Phase H4: lkjCorrCholesky で相関を学習
--   * Phase I:  InverseGamma で σ² 事前
--   * Phase G1: deterministic で Δ を保存
--   * Phase F1: posteriorSummaryFile で az.summary 風 HTML
--   * Phase F2: tracePlotHDIFile で 94% HDI トレース
--   * Phase F4: ppcPlotFile で観測との適合性チェック
--   * Phase G4: NUTS divergence 検出 (chainDivergences)
--   * Phase E:  energyPlotFile で BFMI 確認
module Main where

import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)
import qualified System.Random.MWC.Distributions as MWC

import MCMC.Core (chainEnergy, chainDivergences)
import MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Model.HBM (ModelP, sample, observe, deterministic,
                  Distribution (..), augmentChainWithDeterministic,
                  lkjCorrCholesky)
import Stat.MCMC (bfmi)
import Stat.PosteriorPredictive (posteriorPredictive)
import Viz.MCMC (printPosteriorSummary, posteriorSummaryFile,
                 tracePlotHDIFile, energyPlotFile, ppcPlotFile)
import Viz.Core (defaultConfig, OutputFormat (..), PlotConfig (..))

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 1000
        , nutsBurnIn     = 500
        , nutsStepSize   = 0.05
        , nutsMaxDepth   = 7
        }

-- 真値:
--   μ_pop = 1.0,  σ_pop = 0.3
--   病院効果 μ_1 = 1.2, μ_2 = 0.8 (差 Δ_true = 0.4)
--   σ_y = 0.5 (患者ごとの個体差)
hospital1Obs, hospital2Obs :: [Double]
hospital1Obs = [1.4, 0.9, 1.3, 1.2, 1.0, 1.5, 0.8, 1.25, 1.1, 1.18,
                1.35, 1.05, 1.4, 1.15, 1.22]
hospital2Obs = [0.7, 1.0, 0.85, 0.65, 0.9, 0.8, 1.05, 0.75, 0.92, 0.78,
                0.6, 0.95, 0.82, 0.7, 0.88]

-- 共分散構造 (固定 sd=σ_pop=0.3、相関は LKJ 事前で学習)
clinicalModel :: ModelP ()
clinicalModel = do
  -- 母集団パラメタ
  muPop  <- sample "mu_pop"  (Normal 1 2)
  sigPop <- sample "sig_pop" (HalfNormal 1)

  -- 観測ノイズの分散事前 (Phase I: InverseGamma)
  sig2y <- sample "sig2_y" (InverseGamma 2 0.3)
  let sigY = sqrt sig2y

  -- 病院効果の相関行列 (Phase H4: LKJ)
  l <- lkjCorrCholesky "R" 2 1.0   -- η = 1: uniform 事前

  -- 共分散 Σ = diag(σ_pop) × R × diag(σ_pop), R = L Lᵀ
  -- L0 = (1, 0); L1 = (ρ, √(1-ρ²)). σ_pop で scale。
  let l00 = (l !! 0) !! 0
      l10 = (l !! 1) !! 0
      l11 = (l !! 1) !! 1
      sLL = sigPop * sigPop
      cov = [ [sLL * l00 * l00, sLL * l00 * l10]
            , [sLL * l00 * l10, sLL * (l10*l10 + l11*l11)] ]

  -- 病院効果 μ_j を MvNormal latent (Phase G6 mvNormalLatent 相当)
  -- ここでは 2D なので非中心化を直接書く。
  raw0 <- sample "mu_h_raw0" (Normal 0 1)
  raw1 <- sample "mu_h_raw1" (Normal 0 1)
  let muH1 = muPop + sigPop * l00 * raw0
      muH2 = muPop + sigPop * (l10 * raw0 + l11 * raw1)
  _ <- deterministic "mu_h1" muH1
  _ <- deterministic "mu_h2" muH2

  -- 派生量: 治療効果差 (Phase G1)
  _ <- deterministic "delta" (muH1 - muH2)

  -- 観測 (Phase G5 spirit: パラメトリックモデル関数)
  observe "y1" (Normal muH1 sigY) hospital1Obs
  observe "y2" (Normal muH2 sigY) hospital2Obs
  -- 共分散構造を活かして cov 自体は使っていない (簡易版)
  -- (フル MvNormal observation だと両病院の個体間相関が要る)
  let _ = cov  -- 抑制
  return ()

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  統合デモ (Phase K2): 2 病院の治療効果比較 (階層モデル)"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""
  putStrLn "シナリオ:"
  putStrLn "  μ_pop, σ_pop ~ Normal/HalfNormal      (母集団効果)"
  putStrLn "  R ~ LKJ(η=1)                          (病院間相関)"
  putStrLn "  σ²_y ~ InverseGamma(2, 0.3)           (観測ノイズ分散)"
  putStrLn "  μ_h1, μ_h2 = MvN([μ_pop, μ_pop],"
  putStrLn "                  diag(σ_pop) R diag(σ_pop))"
  putStrLn "  Δ = μ_h1 - μ_h2                       (派生量, Deterministic)"
  putStrLn ""
  printf "  観測: 病院 1 (n=%d, mean=%.3f), 病院 2 (n=%d, mean=%.3f)\n"
         (length hospital1Obs) (sum hospital1Obs / fromIntegral (length hospital1Obs))
         (length hospital2Obs) (sum hospital2Obs / fromIntegral (length hospital2Obs))
  putStrLn ""

  gen <- createSystemRandom

  let init0 = Map.fromList
        [ ("mu_pop", 1.0), ("sig_pop", 0.3)
        , ("sig2_y", 0.25)
        , ("R_u1_0", 0.5)
        , ("mu_h_raw0", 0.0), ("mu_h_raw1", 0.0)
        ]

  putStrLn "[1] NUTS (1000 iter, 500 burn-in) を実行中..."
  rawCh <- nuts clinicalModel cfg init0 gen
  let ch = augmentChainWithDeterministic clinicalModel rawCh

  let names = [ "mu_pop", "sig_pop", "sig2_y"
              , "R_pc1_0"        -- 病院間相関 ρ
              , "mu_h1", "mu_h2", "delta" ]
  printPosteriorSummary names [ch]
  putStrLn ""

  -- 診断: BFMI と divergences
  let es     = chainEnergy rawCh
      divs   = chainDivergences rawCh
      bfmiV  = case bfmi es of
        Just v  -> v
        Nothing -> 0/0
  printf "  BFMI = %.3f  (>0.3 で良好、>0.5 で理想)\n" bfmiV
  printf "  Divergences: %d 件 / %d 反復\n"
         (length divs) (nutsIterations cfg)
  putStrLn ""

  -- 出力: F1 / F2 / F4 / E のすべて
  let pcfg t = (defaultConfig t) { plotWidth = 700, plotHeight = 280 }
      hcfg t = (defaultConfig t) { plotWidth = 700, plotHeight = 90 }

  posteriorSummaryFile "integrated-summary.html"
    "Clinical hierarchical model — posterior summary" names [ch]
  putStrLn "  → integrated-summary.html (F1: posterior summary)"

  tracePlotHDIFile HTML "integrated-trace-hdi.html"
    (hcfg "Clinical model — trace with 94% HDI") 0.94 names ch
  putStrLn "  → integrated-trace-hdi.html (F2: HDI 帯付きトレース)"

  energyPlotFile HTML "integrated-energy.html"
    (pcfg "Clinical model — energy plot") rawCh
  putStrLn "  → integrated-energy.html (E: energy plot + BFMI)"

  -- 事後予測 (病院 1 のみ)
  preds <- posteriorPredictive clinicalModel ch gen
  let yReps = [Map.findWithDefault [] "y1" m | m <- preds]
  ppcPlotFile HTML "integrated-ppc.html"
    (pcfg "Clinical model — PP check (hospital 1)") hospital1Obs yReps 50
  putStrLn "  → integrated-ppc.html (F4: posterior predictive check)"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ 階層モデル + 8 種の機能を 1 つのストーリーで統合"
  putStrLn "    (LKJ + InvGamma + non-centered + Deterministic + 4 種の HTML)"
  putStrLn "═══════════════════════════════════════════════════════════════"
