# クイックスタート

> 🌐 [English](01-quickstart.md) | **日本語**

## ビルドと実行

```bash
cabal build           # ライブラリ + 全実行ファイル
cabal test            # テストスイート
cabal run hbm-example # 階層ベイズ + 4チェーン NUTS → HTML レポート生成
```

バイナリを直接実行する場合 (cabal run が曖昧な時):
```
dist-newstyle/build/x86_64-linux/ghc-9.6.7/hanalyze-0.1.0.0/x/<demo>/build/<demo>/<demo>
```

CPU 並列化 (多チェーン実行):
```bash
cabal run hbm-example -- +RTS -N4   # 4 スレッド
```

---

## 「やりたいこと」逆引き — どのデモ / CLI を使うか

### 古典的な回帰

| やりたいこと | 推奨アプローチ | 例 |
|---|---|---|
| 単純な線形回帰 (信頼区間付き) | CLI: `LM` | `hanalyze data.csv x y LM --ci 0.95 --report` |
| ロジスティック回帰 / ポアソン回帰 | CLI: `GLM` | `hanalyze data.csv x y GLM -d binomial -l logit --report` |
| 多項式回帰 (列ごとに次数指定) | CLI: `LM --degree -1 2 -2 3` | デモ: `glmm-demo` (LME バリアント) |
| 混合効果 (LME / GLMM) | CLI: `--group COL` | デモ: `glmm-demo` |
| ガウス過程回帰 (非線形) | CLI: `GP` / デモ: `gp-demo` | RBF/Matérn/Periodic を自動比較 |

### ベイズ推論

| やりたいこと | 推奨アプローチ | 例 |
|---|---|---|
| 単回帰のベイズ版 (CLI から一発) | CLI: `HBM --report --waic` | `hanalyze data.csv x y HBM --report` |
| 階層モデル (グループ構造) | デモ: `simpson-paradox` / `hbm-random-slope` | カスタム階層は `Model.HBM` で直接記述 |
| ランダム傾きモデル | デモ: `hbm-random-slope` | M1 (β 共通) vs M2 (β_g) を WAIC 比較 |
| ベイズ A/B テスト | デモ: `clinical-trial` | Beta-Binomial、決定理論 |
| 多チェーン NUTS + R-hat 診断 | デモ: `hbm-example` | 4 チェーン並列、`mcmc_report_multi.html` |

### サンプラー選択 / 性能

| やりたいこと | 推奨アプローチ | 例 |
|---|---|---|
| MH/HMC/NUTS の比較 | デモ: `bench-mcmc` | 易/難の 2 ケースで ESS/s を計測 |
| 共役モデルの高速サンプリング | デモ: `gibbs-hbm-demo` | Gibbs 自動検出、ハイブリッド Gibbs+MH |
| 変分推論 (大規模・高速近似) | デモ: `vi-demo` | ADVI vs NUTS 精度比較 |
| サンプラーの精度確認 | デモ: `test-hmc-nuts` | 1D ガウスで HMC/NUTS 動作検証 |

### モデル比較 / 解釈

| やりたいこと | 推奨アプローチ | 例 |
|---|---|---|
| WAIC / LOO-CV でモデル選択 | CLI: `--waic` または デモ: `gibbs-demo` | LM/GLM/HBM/LME に対応 |
| 複数モデルを 1 つの HTML で並列比較 | デモ: `simpson-paradox` | LM/GLMM/HBM の係数・予測曲線・WAIC を一覧 |
| シンプソンのパラドックスを再現 | デモ: `simpson-paradox` | `simpson_compare.html` |
| HBM 階層拡張の妥当性検証 | デモ: `hbm-random-slope` | ランダム切片のみ vs +ランダム傾き |

### 可視化

| やりたいこと | 推奨アプローチ | 例 |
|---|---|---|
| AnalysisReport (DAG・MCMC 診断・予測曲線統合) | CLI: `--report` | LM/GLM/GLMM/GP/HBM 全対応 |
| 棒グラフ・ヒストグラム単体 | デモ: `bar-demo` / CLI: `--hist COL` | PNG/SVG 出力可能 |
| MCMC 単独レポート (KDE + トレース + DAG) | `Viz.Report.renderReport` | デモ: `hbm-example` |
| プロットを PNG/SVG にエクスポート | CLI: `--format png` | 各 NamedPlot が個別画像化 |
| モデル DAG 単独 HTML | `Viz.ModelGraph.renderModelGraph` | Track 型による依存自動抽出 |

---

## 最小の完全ワークフロー (Haskell から)

5 行で「モデル → NUTS → HTML レポート」まで完結する例:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
import qualified Data.Map.Strict as Map
import System.Random.MWC (createSystemRandom)
import Model.HBM                              -- Distribution (..), sample, observe
import MCMC.NUTS  (nuts, defaultNUTSConfig)
import MCMC.Core  (posteriorMean, posteriorSD)
import Viz.Report (defaultReport, renderReport)

-- 1. モデル: μ ~ Normal(0,10), y ~ Normal(μ, σ=2), 観測 5 点
myModel :: ModelP ()
myModel = do
  mu <- sample "mu" (Normal 0 10)
  observe "y" (Normal mu 2) [1.2, 2.3, 3.1, 2.8, 1.9]

main :: IO ()
main = do
  gen <- createSystemRandom
  -- 2. NUTS (AD 勾配 + dual averaging)
  chain <- nuts myModel defaultNUTSConfig (Map.fromList [("mu", 0.0)]) gen
  -- 3. 事後統計
  print (posteriorMean "mu" chain)
  print (posteriorSD   "mu" chain)
  -- 4. HTML レポート (KDE + トレース + 自己相関)
  renderReport "report.html" (defaultReport "My Model" chain ["mu"])
```

---

## モジュール早見表

| 用途 | モジュール | 主要関数 |
|---|---|---|
| モデル定義 (多相 DSL) | `Model.HBM` | `sample`, `observe`, `ModelP r` |
| HMC | `MCMC.HMC` | `hmc`, `hmcChains` |
| NUTS | `MCMC.NUTS` | `nuts`, `nutsChains` |
| Gibbs (共役自動検出) | `MCMC.Gibbs` | `gibbsMH`, `gibbsFromModel` |
| Random Walk MH | `MCMC.MH` | `metropolis` |
| 変分推論 | `Stat.VI` | `advi` |
| WAIC / LOO | `Stat.ModelSelect` | `waic`, `loo`, `lmPosteriorLogLiks`, `lmePosteriorLogLiks` |
| 古典回帰 | `Model.LM` / `Model.GLM` / `Model.GLMM` | `fitPolyWithSmooth`, `fitGLMWithSmooth`, `fitLMEDataFrame` |
| ガウス過程 | `Model.GP` | `optimizeGP`, `fitGP`, `gpPredData` |
| MCMC レポート | `Viz.Report` | `defaultReport`, `renderReport` |
| 多モデル比較レポート | `Viz.AnalysisReport` | `writeAnalysisReport`, `writeComparisonReport` |
| DAG 可視化 | `Viz.ModelGraph` | `renderModelGraph` |
| 散布図/棒グラフ/ヒストグラム | `Viz.Scatter` / `Viz.Bar` / `Viz.Histogram` | 各 `*File` 関数 |

各機能の詳細は以下を参照:
- [確率的プログラミング DSL](02-probabilistic-model.md)
- [MCMC サンプラー選択ガイド](03-mcmc-samplers.md)
- [Gibbs サンプリング](04-gibbs.md)
- [変分推論 (ADVI)](05-variational-inference.md)
- [モデル比較 (WAIC/LOO)](06-model-comparison.md)
- [可視化](07-visualization.md)
