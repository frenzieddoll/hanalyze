{-# LANGUAGE OverloadedStrings #-}
-- | PyMC との機能比較を可視化するレポート (棒グラフ + テキスト)。
--
-- カテゴリ別に ✅ 実装済み / 🚧 部分実装 / ❌ 未実装 の件数を
-- 積み上げ棒グラフで表示し、最近のブランチで追加されたものを強調する。
module Main where

import qualified Data.Text as T
import Data.Text (Text)
import Text.Printf (printf)

import Viz.Bar  (stackedBar)
import Viz.Core (PlotConfig (..), defaultConfig, OutputFormat (..), writeSpec)

-- ---------------------------------------------------------------------------
-- データ: PyMC 機能カテゴリ別の実装状況 (このブランチ完了時点)
-- ---------------------------------------------------------------------------

-- (カテゴリ, 実装済 ✅, 部分実装 🚧, 未実装 ❌)
statusByCategory :: [(Text, Int, Int, Int)]
statusByCategory =
  [ ("分布",          15, 1, 9 )   -- Normal..LogNormal+Bernoulli/Cat (12) + Mixture/Trunc/Censored (3) ; MvNormal 部分; Dirichlet/LKJ/MultiN/ZIP/NB/Weibull/Pareto/BetaBin/Wishart/Bound
  , ("サンプラー",     5, 0, 4 )   -- NUTS/HMC/MH/Gibbs/ADVI ; Slice/Full-ADVI/SMC/NormFlow
  , ("事後 Workflow",  3, 0, 3 )   -- PPC/PriorPC/Potential ; set_data/Deterministic/Multi-PPC
  , ("可視化・診断",   7, 2, 4 )   -- trace/posterior/pair/acf/forest/energy/BFMI ; HDI部分/posterior_table部分 ; pp_check/rank/divergences/summary
  , ("モデル比較",     3, 0, 1 )   -- WAIC/LOO/compareModels ; Bayes factor
  , ("プリミティブ",   6, 1, 5 )   -- 階層/ランダム切片/ランダム傾き/Mixture/Trunc/Censored ; GP部分 ; AR/non-centered/ODE/BNN/state-space
  ]

-- 今回のブランチで追加されたもの (PRリリース要約に使う)
addedThisBranch :: [(Text, Text)]
addedThisBranch =
  [ ("Phase A", "pm.Potential 相当 (potential プリミティブ)")
  , ("Phase B", "pm.Mixture (log-sum-exp、観測/サンプル両対応)")
  , ("Phase C", "Truncated / Censored 分布 (任意分布の切り詰め)")
  , ("Phase C+", "Beta / Gamma / Cauchy / StudentT / HalfCauchy の CDF")
  , ("Phase D", "MvNormal 観測専用 (自前 Cholesky、AD 互換)")
  , ("Phase E", "Energy plot / BFMI 診断 (NUTS の病的事後分布検出)")
  ]

-- 残課題 TODO (優先度順)
todoHigh :: [Text]
todoHigh =
  [ "pm.Deterministic (派生量を Chain に保存)"
  , "Dirichlet 分布 + シンプレックス変換"
  , "MvNormal latent (Cholesky factor + LKJ 事前)"
  , "Divergences の検出&可視化 (NUTS pair plot)"
  , "非中心化パラメタ化ヘルパ"
  , "pm.set_data (DSL に Data プレースホルダ)"
  ]

todoMid :: [Text]
todoMid =
  [ "NegativeBinomial / Multinomial / ZeroInflated"
  , "Weibull / Pareto / Beta-Binomial / VonMises"
  , "Wishart / InverseGamma (共役事前)"
  , "事後予測プロット (pp_check)"
  , "Posterior table (az.summary 相当の単独ヘルパ)"
  , "Rank plot (多チェーン収束診断)"
  , "HDI 帯付きトレースプロット"
  ]

todoLow :: [Text]
todoLow =
  [ "LKJCholeskyCov (相関行列の事前)"
  , "Slice sampler / SMC / Full-rank ADVI / Normalizing flows"
  , "AR / 状態空間モデル"
  , "ODE 尤度 (Runge-Kutta + AD)"
  , "ベイズ NN"
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

  -- TODO
  putStrLn "[3] 残課題 TODO (優先度順)"
  putStrLn ""
  putStrLn "  -- 高優先 (主要 PyMC 機能ギャップ) --"
  mapM_ (\t -> putStrLn ("    [ ] " ++ T.unpack t)) todoHigh
  putStrLn ""
  putStrLn "  -- 中優先 (分布・診断の補完) --"
  mapM_ (\t -> putStrLn ("    [ ] " ++ T.unpack t)) todoMid
  putStrLn ""
  putStrLn "  -- 低優先 (Stretch goals) --"
  mapM_ (\t -> putStrLn ("    [ ] " ++ T.unpack t)) todoLow
  putStrLn ""

  -- 可視化
  putStrLn "[4] 可視化"
  statusChart
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  詳細表は docs/08-pymc-comparison.ja.md を参照"
  putStrLn "═══════════════════════════════════════════════════════════════"
