{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | pm.set_data 相当のデモ。
--
-- Haskell では「データを差し替え可能なモデル」を表す自然な方法は
-- データを引数にとるモデル関数 `mkModel :: [Double] -> ModelP ()` を作ること。
-- これは PyMC の `pm.Data` + `pm.set_data` ワークフローと同じ意図を
-- 構文的に表現する。
--
-- DSL レベルでは更に `dataNamed` / `withData` を提供している。
-- これらは Free monad の構造を直接書き換えるので、構造が動的に決まる
-- 場合や、モデル定義部から多くのコードを共有したい場合に便利。
-- ただし polymorphic な ModelP r に対する `withData` の適用は
-- 型システム的に煩雑なので、本デモでは parametric 化パターンを示す。
module Main where

import qualified Data.Map.Strict as Map
import System.Random.MWC (createSystemRandom)

import MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))
import Model.HBM (ModelP, sample, observe, dataNamed,
                  Distribution (..))
import Stat.PosteriorPredictive (posteriorPredictive)
import Viz.MCMC (printPosteriorSummary, ppcPlotFile)
import Viz.Core (defaultConfig, OutputFormat (..), PlotConfig (..))

cfg :: NUTSConfig
cfg = defaultNUTSConfig
        { nutsIterations = 1500
        , nutsBurnIn     = 500
        , nutsStepSize   = 0.1
        }

trainData, testData :: [Double]
trainData =
  [1.2, 0.9, 1.4, 0.7, 1.1, 1.0, 1.3, 0.95, 1.05, 1.15,
   0.85, 1.25, 0.95, 1.18, 1.02]
testData = [1.6, 1.4, 1.5, 1.7, 1.3, 1.5, 1.55, 1.45, 1.48, 1.52]

-- | データを引数にとるモデル。同じ構造を異なるデータで再利用するための
-- 標準パターン (= pm.set_data 相当)。`dataNamed` で名前付きプレースホルダ
-- としても保存しておく (構造分析時に「この観測は y という名前」と判明する)。
mkModel :: [Double] -> ModelP ()
mkModel ys = do
  yObs <- dataNamed "y" ys
  mu   <- sample "mu"    (Normal 0 5)
  sig  <- sample "sigma" (HalfNormal 2)
  observe "y" (Normal mu sig) yObs

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  pm.set_data デモ — データを差し替えて事後予測を取る"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  gen <- createSystemRandom

  -- ── 訓練データで推論 ──
  putStrLn "[1] 訓練データで NUTS 実行 (μ ≈ 1.0 期待)"
  ch <- nuts (mkModel trainData) cfg
              (Map.fromList [("mu", 1), ("sigma", 1)]) gen
  printPosteriorSummary ["mu", "sigma"] [ch]
  putStrLn ""

  -- ── 同じモデル構造をテストデータで適用 (pm.set_data に相当) ──
  putStrLn "[2] 同モデルにテストデータ (μ=1.5 由来) を渡して PP check"
  preds <- posteriorPredictive (mkModel testData) ch gen
  let yReps = [Map.findWithDefault [] "y" m | m <- preds]
  let ppcCfg = (defaultConfig "PP check — train posterior on test data")
                 { plotWidth = 700, plotHeight = 280 }
  ppcPlotFile HTML "set-data-ppc.html" ppcCfg testData yReps 50
  putStrLn "  → set-data-ppc.html"
  putStrLn "    観測 (青) はテストデータ μ≈1.5、予測 (オレンジ) は"
  putStrLn "    訓練データから得た posterior の予測 → 中心が ≈1.0 で"
  putStrLn "    乖離 → 訓練分布と異なるサンプルだと判明。"
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ データを引数にとるモデル関数で pm.set_data 相当を実現"
  putStrLn "    DSL 内では dataNamed / withData も使用可能 (型注釈が要る)"
  putStrLn "═══════════════════════════════════════════════════════════════"
