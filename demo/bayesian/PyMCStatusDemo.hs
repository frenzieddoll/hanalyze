{-# LANGUAGE OverloadedStrings #-}
-- | PyMC との機能比較を可視化するレポート (棒グラフ + テキスト)。
--
-- カテゴリ別に ✅ 実装済み / 🚧 部分実装 / ❌ 未実装 の件数を
-- 積み上げ棒グラフで表示し、最近のブランチで追加されたものを強調する。
module Main where

import qualified Data.Text as T
import Data.Text (Text)
import Text.Printf (printf)

import Hanalyze.Viz.Bar  (stackedBar)
import Hanalyze.Viz.Core (PlotConfig (..), defaultConfig, OutputFormat (..), writeSpec)

-- ---------------------------------------------------------------------------
-- データ: PyMC 機能カテゴリ別の実装状況 (このブランチ完了時点)
-- ---------------------------------------------------------------------------

-- (カテゴリ, 実装済 ✅, 部分実装 🚧, 未実装 ❌)
-- Phase A-J まで完了後の最新数値。
statusByCategory :: [(Text, Int, Int, Int)]
statusByCategory =
  [ -- 分布: Base12 + (Mixture, Truncated, Censored, MvNormal, Dirichlet,
    --        LKJ, Multinomial, NegBinom, ZIP, ZIB, InvGamma, Weibull,
    --        Pareto, BetaBinom, VonMises) - 27; 残り Wishart, MvT, Bound = 3
    ("分布",          27, 0, 3 )
  , -- サンプラー: NUTS/HMC/MH/Gibbs/ADVI/Slice = 6; Full-ADVI/SMC/NormFlow = 3
    ("サンプラー",     6, 0, 3 )
  , -- 事後 Workflow: PPC/PriorPC/Potential/set_data/Deterministic = 5
    ("事後 Workflow",  5, 0, 1 )  -- 多PPC など 1 件残
  , -- 可視化: trace/posterior/pair/acf/forest/energy/BFMI/HDI-trace
    --        /rank/pp_check/summary/divergence-overlay = 12; 残 1
    ("可視化・診断",   12, 0, 1 )
  , -- モデル比較: WAIC/LOO/compareModels = 3; ベイズファクター 1
    ("モデル比較",     3, 0, 1 )
  , -- プリミティブ: 階層/ランダム切片・傾き/Mixture/Trunc/Censored/Potential
    --              /Deterministic/non-centered/AR/MvN-latent/Dirichlet/LKJ = 12
    ("プリミティブ",   12, 1, 3 )  -- GP部分; ODE/BNN/state-space-extended
  ]

-- 完了したフェーズ
addedThisBranch :: [(Text, Text)]
addedThisBranch =
  [ ("Phase A", "pm.Potential プリミティブ")
  , ("Phase B", "pm.Mixture (log-sum-exp)")
  , ("Phase C", "Truncated / Censored")
  , ("Phase D", "MvNormal 観測専用")
  , ("Phase E", "Energy plot / BFMI")
  , ("Phase F", "5 つの可視化基盤 (Summary/HDI-Trace/Rank/PPC/Divergence)")
  , ("Phase G", "6 つの主要機能 (Deterministic/Dir/non-centered/Div/set_data/MvN-latent)")
  , ("Phase H", "6 件の補完 (NB/Multinomial/ZIP/LKJ/withData多相/Hanalyze.Stat.Summary 切出)")
  , ("Phase I", "5 つの新規分布 (InvGamma/Weibull/Pareto/BetaBin/VonMises)")
  , ("Phase J", "LKJ K=3 / AR(1) / Slice sampler")
  ]

-- 残課題 (Stretch)
todoStretch :: [Text]
todoStretch =
  [ "Wishart / Multivariate-t (LKJ で代替推奨)"
  , "Full-rank ADVI / Normalizing flows / SMC"
  , "ODE 尤度 (Runge-Kutta + AD、研究レベル)"
  , "ベイズ NN (隠れ層、研究レベル)"
  , "ベイズファクター / 周辺尤度 (重要度サンプリング系)"
  ]

-- ---------------------------------------------------------------------------
-- 可視化
-- ---------------------------------------------------------------------------

statusChart :: IO ()
statusChart = do
  let cats   = [c | (c, _, _, _) <- statusByCategory]
      vDone  = [d | (_, d, _, _) <- statusByCategory]
      vPart  = [p | (_, _, p, _) <- statusByCategory]
      vMiss  = [m | (_, _, _, m) <- statusByCategory]

      -- stackedBar: 各カテゴリに 3 行 (Done/Partial/Missing) を持たせる
      xs   = concatMap (replicate 3) cats
      vals = concat $ zipWith3 (\d p m -> [fromIntegral d, fromIntegral p, fromIntegral m])
                               vDone vPart vMiss
      kinds = concat $ replicate (length cats) ["Done (✅)", "Partial (🚧)", "Missing (❌)"]

      cfg = (defaultConfig "PyMC parity status — hanalyze")
              { plotWidth = 700, plotHeight = 350 }
  writeSpec HTML "pymc-status.html"
    (stackedBar cfg "category" "count" "status" xs vals kinds)
  putStrLn "  → pymc-status.html (カテゴリ別 stacked bar)"

-- ---------------------------------------------------------------------------
-- テキストレポート
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  PyMC parity ステータスレポート"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  -- カテゴリ別件数
  putStrLn "[1] カテゴリ別 実装状況"
  printf "  %-15s   %4s  %4s  %4s   %s\n" ("Category" :: String)
         ("Done" :: String) ("Part" :: String) ("Miss" :: String)
         ("Total" :: String)
  printf "  %s\n" (replicate 50 '-' :: String)
  let total = sum [d + p + m | (_, d, p, m) <- statusByCategory]
      tDone = sum [d | (_, d, _, _) <- statusByCategory]
      tPart = sum [p | (_, _, p, _) <- statusByCategory]
      tMiss = sum [m | (_, _, _, m) <- statusByCategory]
  mapM_ (\(c, d, p, m) ->
            printf "  %-15s   %4d  %4d  %4d   %4d\n"
                   (T.unpack c) d p m (d + p + m))
        statusByCategory
  printf "  %s\n" (replicate 50 '-' :: String)
  printf "  %-15s   %4d  %4d  %4d   %4d   (%.1f%% complete)\n"
         ("TOTAL" :: String) tDone tPart tMiss total
         (100 * fromIntegral tDone / fromIntegral total :: Double)
  putStrLn ""

  -- 追加した機能
  putStrLn "[2] このブランチで追加された機能"
  mapM_ (\(p, d) -> printf "  %-9s  %s\n" (T.unpack p) (T.unpack d))
        addedThisBranch
  putStrLn ""

  -- TODO (Stretch のみ)
  putStrLn "[3] 残課題 TODO (Stretch — 主要ギャップは完了)"
  mapM_ (\t -> putStrLn ("    [ ] " ++ T.unpack t)) todoStretch
  putStrLn ""

  -- 可視化
  putStrLn "[4] 可視化"
  statusChart
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  詳細表は docs/08-pymc-comparison.ja.md を参照"
  putStrLn "═══════════════════════════════════════════════════════════════"
