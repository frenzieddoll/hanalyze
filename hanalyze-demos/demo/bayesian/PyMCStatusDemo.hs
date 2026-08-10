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
-- Phase 29 完了 + Phase 37 計画反映 (2026-05-30 更新)。
-- A2 (連続 7) + A3 (離散 6) + A4 (多変量 3) = 16 の分布が Phase 37 計画上で
-- 未実装、 これを missing にカウントする (旧 Bound 1 も含めて 17)。
statusByCategory :: [(Text, Int, Int, Int)]
statusByCategory =
  [ -- 分布: Base12 + (Mixture, Truncated, Censored, MvNormal, Dirichlet,
    --        LKJ, Multinomial, NegBinom, ZIP, ZIB, InvGamma, Weibull,
    --        Pareto, BetaBinom, VonMises) = 27 ✅
    -- + Phase 37-A2 (4) + A3 (5) + A4 (2) ✅ → 38
    -- 残 Phase 37 計画: A2 残 (3) + A3 残 (1) + A4 残 Wishart (1) + Bound (1) = 6 ❌
    ("分布",          38, 0,  6)
  , -- サンプラー: NUTS/HMC/MH/Gibbs/Slice/ADVI/SMC/Full-rank ADVI = 8 ✅
    -- 残: 正規化フロー (Stretch) = 1 ❌
    ("サンプラー",     8, 0, 1 )
  , -- 事後 Workflow: PPC/PriorPC/Potential/set_data/Deterministic = 5 ✅
    ("事後 Workflow",  5, 0, 0 )
  , -- 可視化・診断: trace/posterior/pair/acf/forest/energy/BFMI/HDI-trace
    --              /rank/ppc/summary/divergence-overlay/ESS-R̂表 = 13 ✅
    --              A7 はナビ整理のみで実装 gap なし
    ("可視化・診断",   13, 0, 0 )
  , -- モデル比較: WAIC/LOO/Pseudo-BMA/真BMA/BayesFactor (Bridge) = 5 ✅
    ("モデル比較",     5, 0, 0 )
  , -- プリミティブ: 階層/ランダム切片・傾き/Mixture/Trunc/Censored/Potential
    --              /Deterministic/non-centered/AR/MvN-latent/Dirichlet/LKJ = 12 ✅
    -- + Phase 37-A6: glmmRandomIntercept ✅ → 13
    -- 残 Phase 39: hmmLatent / dpStickBreaking = 2 ❌
    -- Stretch: ODE 尤度 / Bayes NN = 2 ❌
    ("プリミティブ",   13, 1, 4 )  -- GP 部分; hmm/dp + ODE/BNN
  ]

-- 完了したフェーズ (Phase 29 まで)
addedThisBranch :: [(Text, Text)]
addedThisBranch =
  [ ("Phase A-J",   "本ブランチ初期: 27 分布 + サンプラー基盤 + 5 viz")
  , ("Phase 29-A1", "SMC (annealing + bridge 経路)")
  , ("Phase 29-A2", "Bridge Sampling + Bayes Factor")
  , ("Phase 29-A3", "真の BMA (周辺尤度ベース)")
  , ("Phase 37-A0", "HBM 書き方 doc (グループ別 3 形式 + multi-level + crossed)")
  , ("Phase 37-A1", "本 PyMC 比較 doc を Phase 29 反映 + 残 gap 確定")
  , ("Phase 37-A2", "連続分布 +4: SkewNormal / Logistic / Gumbel / AsymmetricLaplace")
  , ("Phase 37-A3", "離散分布 +5: OrderedLogistic / DiscreteUniform / Geometric / HyperGeometric / ZeroInflatedNegativeBinomial")
  , ("Phase 37-A4", "多変量分布 +2: MvStudentT / DirichletMultinomial (Wishart 後回し)")
  , ("Phase 37-A5", "Full-rank ADVI (q = N(μ, LLᵀ)、 viCovU で L 出力)")
  , ("Phase 37-A6", "glmmRandomIntercept helper (Gaussian/Binomial/Poisson)")
  ]

-- 残課題 (Phase 37 計画分 + Stretch)
todoStretch :: [Text]
todoStretch =
  [ "[A2 残] 連続分布 3 (低優先): Triangular/Kumaraswamy/Rice"
  , "[A3 残] 離散 1 (低優先): DiscreteWeibull"
  , "[A4 残] 多変量 1: Wishart (LKJ 代替可、 後回し)"
  , "[Phase 39 候補] 残 helper: hmmLatent / dpStickBreaking (Phase 37 から繰越)"
  , "[Stretch] 正規化フロー / ODE 尤度 / ベイズ NN (研究レベル、 別 Phase)"
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
