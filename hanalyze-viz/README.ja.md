# hanalyze-viz

[`hanalyze`](../README.ja.md) の**可視化・レポート層**。 解析結果を
[Vega-Lite](https://vega.github.io/vega-lite/) の spec に変換し、
**単発の図** (HTML / PNG / SVG) と **section を積み上げた HTML レポート**の
2 通りで出力する 19 module。

依存は `core` / `frame` / `bayes` / `models` / `design` の 5 層すべて + 外部 13
package (`hvega` / `aeson` / `dataframe-core` 等)。 分割 6 層の最上位で、
umbrella package はこの層の上に乗る。 図の生成に必要な数値計算 (回帰の
fit・MCMC 診断量・パレートフロント等) は下位層に委ね、 この層は **spec 化と
HTML 出力だけ**を担う。

> **Vega-Lite ではなく静的画像 (SVG / PDF / PNG) を直接描きたい場合**は、
> 姉妹 package の [`hanalyze-plot`](../hanalyze-plot/README.ja.md)
> (`toPlot` / `Plottable`) を使う。 両者は排他ではなく、 用途で使い分ける。

## 主要 module (全 19 module)

### 共通基盤

| Module | 役割 |
|---|---|
| `Viz.Core` | 全 `Viz.*` 共有の I/O ヘルパ — `writeSpec` / `openInBrowser` / `vlJson`、 出力形式 `OutputFormat = HTML \| PNG \| SVG` |
| `Viz.PlotConfig` | 全 `Viz.*` 共有のプロット設定 (`PlotConfig` / `defaultConfig`) |
| `Viz.PlotData` | プロットデータの入力元非依存な中間表現 (`PlotData` / `ToPlotData`) |
| `Viz.PlotData.DataFrame` | `dataframe` 用の `ToPlotData` instance (数値列 / テキスト列 → `PlotData`) |
| `Viz.Assets` | オフライン HTML 用に Vega / Vega-Lite / Vega-Embed の JS 本体を同梱する自動生成 module |

### 単発の図

| Module | 役割 |
|---|---|
| `Viz.Scatter` | 散布図と重ね描き — 回帰直線 / 平滑化 / 信頼帯・グループ別・予測 vs 実測 |
| `Viz.Bar` | 棒グラフ (縦 / 横 / 積み上げ / グループ化) |
| `Viz.Histogram` | ヒストグラム + 理論分布密度の重ね描き |
| `Viz.MCMC` | MCMC 診断プロット — トレース / 事後密度 / 自己相関 / forest / energy |
| `Viz.GP` | ガウス過程回帰の可視化 (訓練データ・事後平均・信用区間) |
| `Viz.Pareto` | パレートフロント — 散布 / pairs / 平行座標 / hypervolume 推移 / 比較 |
| `Viz.ModelGraph` | モデル DAG の Mermaid.js 可視化 |
| `Viz.ModelGraphDot` | モデル DAG の Graphviz DOT 出力 (PyMC `model_to_graphviz` 相当の plate 描画) |

### HTML レポート

| Module | 役割 |
|---|---|
| `Viz.ReportBuilder` | **現行推奨**の合成型レポートビルダー — `ReportSection` を `renderReport` に渡すだけ |
| `Viz.ReportInstances` | 各 fit 結果型に対する `Reportable` instance 集 |
| `Viz.Report` | MCMC 結果の統合レポート (DAG・事後要約表・診断プロット・pairs 散布図) |
| `Viz.GPReport` | GP 回帰の統合レポート (データ特性・モデル比較・対話予測・付録) |
| `Viz.Taguchi` | 田口メソッド解析のレポート (SN 比・主効果・最適水準) |
| ⚠ `Viz.AnalysisReport` | **非推奨**。 LM / GLM / GLMM / GP / HBM 専用の sum-type 版 (約 2000 行)。 後継は `Viz.ReportBuilder` で、 既存 CLI (`hanalyze regress --report`) との互換のため残置。 将来削除予定 |

## 単体で使う

```cabal
build-depends: hanalyze-viz, hanalyze-frame
```

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Hanalyze.DataIO.CSV       (loadAuto)
import Hanalyze.Viz.Core         (OutputFormat (HTML))
import Hanalyze.Viz.PlotConfig   (defaultConfig)
import Hanalyze.Viz.Scatter      (scatterPlotFile)
import Hanalyze.Viz.ReportBuilder

main :: IO ()
main = do
  Right df <- loadAuto "flights.csv"
  -- 単発の図を 1 行で HTML へ
  scatterPlotFile HTML "scatter.html"
    (defaultConfig "dep_delay vs month") df "month" "dep_delay"
  -- section を積み上げて 1 枚の HTML レポートへ
  renderReport "report.html" (defaultReportConfig "Flight delays")
    [ secDataOverview df ["month"] "dep_delay"
    , secModelOverview "Linear regression" "dep_delay = b0 + b1 * month" Nothing
    , secCoefficients [("b0", 1.2), ("b1", 2.4)] (Just ("R2", 0.96))
    ]
```

`*File` 系 (`scatterPlotFile` / `histogramPlotFile` …) は `OutputFormat` を取り
ファイルまで書き出す。 spec だけ欲しいときは `*Plot` 系 (`scatterPlot ::
PlotConfig -> DataFrame -> Text -> Text -> VegaLite`) を使い、 `Viz.Core` の
`writeSpec` / `vlJson` で好きなタイミングで出力する。

`ReportBuilder` の section builder は `secDataOverview` / `secCoefficients` /
`secFitScatter` / `secResiduals` のほか、 MCMC 診断 (`secMCMCDiagnostics` /
`secPosteriorSummary` / `secForestPlot`)、 モデル比較 (`secComparisonTable`)、
対話予測 (`secInteractiveLM` 等) まで一式そろっている。

なお、 通常は umbrella package `hanalyze` を依存に書けば
`import Hanalyze` だけで主要な図関数も使える。 層を直接指定するのは
依存を最小化したいときのみで十分。

## 関連 docs

- 可視化の入口: [docs/visualization/01-visualization.ja.md](../docs/visualization/01-visualization.ja.md)
- HTML レポート: [02-report-builder.ja.md](../docs/visualization/02-report-builder.ja.md)
- MCMC 診断プロット: [bayesian/viz-diagnostics.ja.md](../docs/bayesian/viz-diagnostics.ja.md)
- 静的描画との使い分け: [03-plot-integration.ja.md](../docs/visualization/03-plot-integration.ja.md)

← [repository README](../README.ja.md)
