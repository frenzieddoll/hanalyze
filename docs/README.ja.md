# hanalyze ドキュメント

> 🌐 [English](README.md) | **日本語**

Haskell 製の統計 / 最適化 / 実験計画 toolkit。ここは利用者向けドキュメントの入口です。

読み物は 3 つ ── **始めるならクイックスタート、引くなら API リファレンス、
掘るなら分野別ノート** に集約しています。

## 1. まず動かす (学習導線)

| ページ | 内容 |
|---|---|
| [01 quickstart](01-quickstart.md) | CSV を読んで `df \|-> lm` で当てはめ、`toPlot` で描くまで |
| [02 PyMC との比較](02-pymc-comparison.md) | ベイズ (HBM/NUTS) を PyMC と並べて書く |

インストール手順と package の選び方は [root README](../README.ja.md) にあります。

## 2. API リファレンス (辞書)

**[api-guide/](api-guide/README.ja.md)** ─ 公開 API を topic 別に網羅する辞書。用語・網羅性重視。

| ページ | 内容 |
|---|---|
| [01 quickstart](api-guide/01-quickstart.ja.md) | 最短で fit → 描画 |
| [02 regression](api-guide/02-regression.ja.md) | LM / GLM / Robust / Quantile / WLS / GAM / Spline / Kernel / GP / Formula DSL |
| [03 bayesian-hbm](api-guide/03-bayesian-hbm.ja.md) | HBM (`ModelP` / 分布 / plate) ・NUTS ・事後要約 ・診断抽出子 |
| [04 multivariate](api-guide/04-multivariate.ja.md) | PCA / PLS / RRR / CCA / Discriminant / Cluster / FDA |
| [05 ml](api-guide/05-ml.ja.md) | RandomForest / GBM / DecisionTree / KNN / NaiveBayes / NN / SVM / MDS |
| [06 timeseries](api-guide/06-timeseries.ja.md) | AR / VAR / GARCH / forecast |
| [07 survival](api-guide/07-survival.ja.md) | KaplanMeier / CompetingRisks / AFT / Cox |
| [08 causal](api-guide/08-causal.ja.md) | PropensityScore / IPW / DR / CATE / LiNGAM |
| [09 doe](api-guide/09-doe.ja.md) | 実験計画 (Factorial / RSM / Optimal / Orthogonal / Taguchi / Power) |
| [10 stat](api-guide/10-stat.ja.md) | 記述統計 / 検定 / 相関 / 効果量 / bootstrap |
| [11 data](api-guide/11-data.ja.md) | `Data.*` (Transform / Wrangle) + DataIO + Fit API (`\|->`) |
| [12 plot](api-guide/12-plot.ja.md) | plot 連携 (`toPlot` / 抽出子) |

## 3. 分野別ノート (掘る)

api-guide が「何があるか」を示すのに対し、こちらは **手法ごとの背景・使い分け・実測**を扱います。

| 分野 | 本数 | 内容 |
|---|---:|---|
| [regression/](regression/) | 48 | 回帰の各手法 (罰則付き / ロバスト / 分位点 / GAM / GP ほか) |
| [bayesian/](bayesian/) | 32 | HBM のモデリング・収束診断・可視化 |
| [stat/](stat/) | 26 | 記述統計・検定・効果量・多変量 |
| [doe/](doe/) | 25 | 実験計画 (要因計画 / RSM / 最適計画 / Custom Design) |
| [optim/](optim/) | 12 | 最適化 (Nelder-Mead / L-BFGS / CMA-ES / NSGA-II / ベイズ最適化) |
| [principles/](principles/) | 10 | 設計原則 (API の一貫性・純粋性・数値の正しさ) |
| [io/](io/) | 8 | 読み込み・整形 (CSV / clean / reshape / Fit API) |
| [visualization/](visualization/) | 6 | 可視化 (plot 連携・レポート) |
| [causal/](causal/) | 2 | 因果推論・因果探索 |
| [ml/](ml/) | 2 | 機械学習 |
| [timeseries/](timeseries/) | 2 | 時系列 |
| [fda/](fda/) | 2 | 関数データ解析 |
| [comparison/](comparison/) | 2 | 他ライブラリとの比較 |

## 4. 開発者向け

| ページ | 内容 |
|---|---|
| [dev-notes/](dev-notes/) | 実装ノート |
| [internal/](internal/) | 内部設計 |
| [superpowers/](superpowers/) | 調査・計画の記録 |

package 構成・ビルド方法・貢献手順は [root README](../README.ja.md) と
[CONTRIBUTING.md](../CONTRIBUTING.md) を参照してください。

---

**言語について**: 多くのページは `*.md` (英語) と `*.ja.md` (日本語) の対で用意しています。
API の Haddock も日英併記です。
