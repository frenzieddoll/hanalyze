# hanalyze API リファレンス

公開 API を topic 別に網羅するリファレンス。 「どう学ぶか」 の読み物 (理論・導出・罠) は
各 [`docs/<topic>/`](../) のガイドが担い、 ここは **「この機能の型は? 最小例は?」 に即答する
辞書**に振る。

> **用語**: モデルを当てはめる = **fit** (高レベル動詞 `df |-> spec`)、 当てはめた結果を図に
> する = **`toPlot`** / 抽出子 (`forestOf` / `tracesOf` / …)、 列を持つデータ源 = **`ColumnSource`**
> (assoc list `[(Text, ColData)]` / Hackage `DataFrame` / `Map`)。 描画の文法 (layer / mark) は
> [hgg の API リファレンス](../../../hgg/docs/api-guide/README.md) が一次根拠。

## ページ一覧

| ページ | 内容 |
|---|---|
| [01 quickstart](01-quickstart.md) | 最短で fit → 描画 (`df \|-> lm` + `toPlot` の 3 行) |
| [02 regression](02-regression.md) | LM / GLM / Robust / Quantile / WLS / GAM / Spline / Kernel / RFF / GP / Formula DSL |
| [03 bayesian-hbm](03-bayesian-hbm.md) | HBM (`ModelP` / 分布 / plate・階層) ・ `HBMConfig`/NUTS ・ `\|->!` ・ 事後要約 (`hbmSummary` / `hbmSummaryDf` / `hbmDrawsDf`) ・ 診断抽出子 (dag/trace/marginals/forest/ppc/epred/autocorr/rank/pair/energy) |
| [04 multivariate](04-multivariate.md) | PCA / PLS / RRR / CCA / Discriminant / Cluster / HCluster / FDA |
| [05 ml](05-ml.md) | RandomForest / GBM / DecisionTree / KNN / NaiveBayes / NeuralNetwork / SVM / MDS |
| [06 timeseries](06-timeseries.md) | AR / VAR / GARCH / forecast |
| [07 survival](07-survival.md) | KaplanMeier / CompetingRisks (CIF) / AFT / Cox |
| [08 causal](08-causal.md) | PropensityScore / IPW / DR / CATE / LiNGAM |
| [09 doe](09-doe.md) | 実験計画 (Factorial / RSM / Optimal / Orthogonal / Taguchi / Anova / Power) |
| [10 stat](10-stat.md) | 記述統計 / 検定 / 相関 / 効果量 / PCA / cluster / bootstrap |
| [11 data](11-data.md) | Data.* (Transform / Wrangle) + DataIO (CSV / clean / reshape) + Fit API (`\|->`) |
| [12 plot](12-plot.md) | plot 連携 (`toPlot` / 抽出子) → [hgg api-guide](../../../hgg/docs/api-guide/README.md) |

## 2 つの層 (どの import でも両方使える)

| 層 | 立ち位置 | 例 |
|---|---|---|
| **高レベル (主役)** | データ源から**列名で**直接 fit。 `df \|-> spec` で任意モデルを当てはめ、 `toPlot` で描画 | `df \|-> lm "x" "y"` |
| **低レベル** | 既に `hmatrix` の `Vector`/`Matrix` を持つ時の数値 API (`fitLMVec` 等)。 結果は `FitResult` | `fitLMVec (designMatrix xs) ys` |

各ページは **高レベル `df |-> spec` / `toPlot` を主**に書き、 行列 API 直叩きの低レベルは
`**低レベル**` ラベルで併記する。

## 演算子・抽出子 早見表

モデルを当てはめて図にする経路はこの 4 つが軸。 役割の違いはここを一次根拠にする
(各ページは詳細のみ扱う)。

| 演算子 / 関数 | 役割 | 詳細 |
|---|---|---|
| `\|->` | データ源から列名で**モデルを fit** (純粋・決定的) | [01 quickstart](01-quickstart.md) / [11 data](11-data.md) |
| `\|->!` | `\|->` の **IO 版** (サンプリング進捗バー付き・結果はビット一致) | [03 bayesian-hbm](03-bayesian-hbm.md) |
| `toPlot` | fit 済みモデルを**図 (layer) に変換** (散布図 + 回帰線 + CI 帯 等) | [12 plot](12-plot.md) |
| `\|>>` | データ源を**図に束ねる** (`BoundPlot` を作る純関数・ファイルは書かない) | [11 data](11-data.md) |

HBM の事後図は専用の抽出子で得る (`toPlot` の仲間):

| 抽出子 | 得られる図 |
|---|---|
| `forestOf` | 事後 forest (係数の 94% HDI) |
| `tracesOf` | trace plot (param ごと 1 パネル・発散 rug 既定 ON) |
| `ppcOf` | 事後予測チェック (PPC) |
| `epred` | 期待値予測曲線 (回帰の事後帯) |
| `dagOf` | モデル DAG (plate 折り畳み) |
| `autocorrOf` | 自己相関 (mixing 診断) |
| `rankOf` | rank plot (chain 一様性・要 ≥2 chain) |

`df |-> spec` の **spec 動詞**: 二変量近道 `lm` / `glm` / `spline` / `robust` / `quantile`・
Formula DSL `lmF` / `glmF` / `glmmF`・ベイズ `hbm`・行列入力 `pcaOf` / `plsOf` (Phase 70.A・
列名リストで PCA/PLS)。 まだ spec が無いモデルは fit 関数で当てはめ、 結果型が `Plottable` なら
`toPlot result` を `saveSVG` 直接 (データ重畳時のみ `df |>>`) で描く ([12 plot](12-plot.md))。

## 関連ドキュメント

- **学習導線 (理論 + 導出 + 罠)**: [`docs/`](../) の各 topic ガイド (regression/ ・ bayesian/ ・ stat/ …)
- **fit API の正本**: [`docs/io/04-fit-api.md`](../io/04-fit-api.md) (`df |-> spec` 網羅)
- **plot 連携の正典**: [`docs/visualization/03-plot-integration.md`](../visualization/03-plot-integration.md)
- **描画文法 (layer / mark / scale)**: [hgg API リファレンス](../../../hgg/docs/api-guide/README.md)
