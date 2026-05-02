# 可視化ガイド (Viz.*)

> 🌐 [English](01-visualization.md) | **日本語**

> 関連デモ:
> - [`hbm-example`](../demo/HBMExample.hs) — `Viz.Report` (KDE/トレース/DAG/ペア散布)
> - [`hbm-regression`](../demo/HBMRegressionDemo.hs) — HBM 回帰の HTML レポート (DAG + MCMC + 信用区間付き予測。レポートビルダは `Viz.AnalysisReport` (非推奨) → `Viz.ReportBuilder` (標準) へ移行中)
> - [`simpson-paradox`](../demo/SimpsonParadoxDemo.hs) — `writeComparisonReport` で複数モデル並列比較
> - [`bar-demo`](../demo/BarDemo.hs) — `Viz.Bar` + PNG/SVG エクスポート
> - [`gp-demo`](../demo/GPDemo.hs) — GP 専用レポート
>
> CLI: `--report` で HTML レポート生成 (`regress` は legacy `Viz.AnalysisReport`、その他は標準 `Viz.ReportBuilder`)、`--format png|svg` で個別プロットも画像化。

## 出力形式

全ての可視化関数は以下の形式に対応しています:

```haskell
data OutputFormat = HTML | PNG | SVG
```

- **HTML**: Vega-Lite + vegaEmbed を埋め込んだ自己完結 HTML (vl-convert 不要)
- **PNG / SVG**: vl-convert が必要 (`vl-convert` コマンドが PATH に存在すること)

PNG/SVG 生成に失敗した場合は自動で HTML にフォールバックします。

---

## Viz.Report — MCMC 統合 HTML レポート (最推奨)

`Viz.Report` は診断プロット・モデルグラフ・サマリー統計を
**1つの自己完結 HTML** にまとめます。

```haskell
import Viz.Report

data MCMCReport = MCMCReport
  { reportTitle    :: Text
  , reportGraph    :: Maybe ModelGraph  -- Nothing でモデルグラフを省略
  , reportChain    :: Chain             -- 代表チェーン
  , reportChains   :: [Chain]           -- 全チェーン (空 = 単一チェーンモード)
  , reportParams   :: [Text]
  , reportPairs    :: [(Text, Text)]    -- ペアスキャタープロット
  , reportMaxLag   :: Int               -- 自己相関の最大ラグ (default 40)
  }

defaultReport :: Text -> Chain -> [Text] -> MCMCReport
renderReport  :: FilePath -> MCMCReport -> IO ()
```

### 単一チェーンレポート

```haskell
chain <- nuts model cfg initP gen
let report = (defaultReport "My Model" chain (sampleNames model))
               { reportGraph = Just (buildModelGraph model edges) }
renderReport "report.html" report
```

### 多チェーンレポート (R-hat 付き)

```haskell
chains <- nutsChains model cfg 4 initP gen
let report = (defaultReport "My Model" (head chains) (sampleNames model))
               { reportGraph  = Just graph
               , reportChains = chains        -- これを設定すると多チェーンモード
               , reportPairs  = [("mu","tau")]
               }
renderReport "report_multi.html" report
```

**多チェーンモードの HTML 構成:**
- **Model Graph** — Mermaid.js DAG (reportGraph が Just の場合)
- **Posterior Summary** — Mean / SD / 2.5% / 97.5% / ESS / **R-hat** テーブル  
  (R-hat < 1.01: 緑、≥ 1.01: 赤)
- **MCMC Diagnostics** — KDE 密度 (94% HDI) + チェーン別色分けトレース
- **Autocorrelation** — 自己相関バーチャート
- **Pair Scatter** — 同時事後分布散布図

---

## Viz.MCMC — 個別 MCMC プロット

`Viz.Report` を使わずに個別のプロットを生成したい場合:

```haskell
import Viz.MCMC
import Viz.Core (defaultConfig, OutputFormat (..))

-- 単一チェーン診断 (KDE + トレース縦並び)
mcmcDiagnosticsFile HTML "diag.html" (defaultConfig "Model") names chain

-- 多チェーン診断 (KDE 合算 + 色分けトレース)
mcmcDiagnosticsMultiFile HTML "diag_multi.html" (defaultConfig "Model") names chains

-- 多チェーントレースのみ
multiTracePlotFile HTML "trace.html" (defaultConfig "Trace") names chains

-- 自己相関バーチャート
autocorrPlotFile HTML "acf.html" (defaultConfig "ACF") 40 names chain

-- ペアスキャタープロット
pairScatterFile HTML "pair.html" (defaultConfig "μ vs τ") "mu" "tau" chain

-- KDE 密度のみ
posteriorPlotFile HTML "kde.html" (defaultConfig "KDE") names chain
```

---

## Viz.Bar — 棒グラフ

```haskell
import Viz.Bar
import Viz.Core (defaultConfig, OutputFormat (..))
```

### 縦棒グラフ

```haskell
barChart :: PlotConfig -> Text -> Text -> [Text] -> [Double] -> VegaLite
barChartFile :: OutputFormat -> FilePath -> PlotConfig -> Text -> Text -> [Text] -> [Double] -> IO ()

barChartFile HTML "bar.html" (defaultConfig "成績") "カテゴリ" "点数"
  ["A組", "B組", "C組"] [82.3, 76.1, 91.5]
```

### 横棒グラフ

```haskell
barChartH :: PlotConfig -> Text -> Text -> [Text] -> [Double] -> VegaLite
barChartHFile :: OutputFormat -> FilePath -> ...
```

### 積み上げ棒グラフ

```haskell
stackedBar :: PlotConfig -> Text -> Text -> Text -> [Text] -> [Double] -> [Text] -> VegaLite
-- (cfg, xTitle, yTitle, colorTitle, xVals, yVals, colorVals)

stackedBarFile HTML "stacked.html" (defaultConfig "積み上げ") "月" "売上" "製品"
  ["1月","1月","2月","2月"]   -- x
  [100, 80, 120, 60]          -- y
  ["A","B","A","B"]           -- color
```

### グループ棒グラフ

```haskell
groupedBar :: PlotConfig -> Text -> Text -> Text -> [Text] -> [Double] -> [Text] -> VegaLite
groupedBarFile HTML "grouped.html" (defaultConfig "グループ") "月" "売上" "製品"
  ["1月","1月","2月","2月"]
  [100, 80, 120, 60]
  ["A","B","A","B"]
```

---

## Viz.Histogram — ヒストグラム

```haskell
import Viz.Histogram

-- 純粋なヒストグラム
histogramPlotFile HTML "hist.html"
  (defaultConfig "Score") "score" vals Nothing  -- Nothing = 自動ビン数

-- 理論分布を重ね書き
histogramWithDensityFile HTML "hist_fit.html"
  (defaultConfig "Score") "score" vals Nothing (Normal 2.5 1.0)
```

対応する理論分布 (`--fit` の引数でも使用):
`Normal`, `Binomial`, `Poisson`, `Exponential`, `Gamma`, `Beta`

---

## Viz.Scatter — 散布図・回帰曲線

`app/Main.hs` の CLI から自動生成されますが、ライブラリ関数として直接使うこともできます。

```haskell
import Viz.Scatter
import Viz.Core (defaultConfig)

-- 散布図 + 回帰曲線
scatterWithSmooth :: PlotConfig -> Text -> Text -> [Double] -> [Double] -> SmoothFit -> VegaLite

-- グループ別散布図
scatterWithGroups :: PlotConfig -> Text -> Text -> Text
                  -> [(Text, [Double], [Double], SmoothFit)] -> VegaLite

-- Predicted vs Actual 診断
predictedVsActual :: PlotConfig -> Text -> [Double] -> [Double] -> VegaLite
```

---

## Viz.ModelGraph — モデル構造 DAG

```haskell
import Viz.ModelGraph

buildModelGraph :: Model a -> [(Text, Text)] -> ModelGraph
modelGraphFile  :: OutputFormat -> FilePath -> ModelGraph -> IO ()

-- エッジリスト: (from, to) = (依存元, 依存先)
let graph = buildModelGraph model
              [ ("mu",  "theta_1"), ("mu",  "theta_2")
              , ("tau", "theta_1"), ("tau", "theta_2")
              , ("theta_1", "y_1"), ("theta_2", "y_2")
              ]
modelGraphFile HTML "graph.html" graph
```

**ノードの表示:**
- **矩形**: 潜在変数 (sample) — ルートノードは事前分布パラメータを表示
- **スタジアム形 (角丸矩形)**: 観測変数 (observe)

---

## PlotConfig の設定

```haskell
import Viz.Core

data PlotConfig = PlotConfig
  { plotTitle  :: Text
  , plotWidth  :: Int   -- ピクセル (default 600)
  , plotHeight :: Int   -- ピクセル (default 400)
  }

defaultConfig :: Text -> PlotConfig
-- plotWidth=600, plotHeight=400

-- カスタム
let cfg = (defaultConfig "Title") { plotWidth = 800, plotHeight = 600 }
```

---

## PNG/SVG 出力

vl-convert が必要です。PATH に存在するか確認してください:

```bash
which vl-convert   # インストール確認
```

```haskell
-- PNG 出力 (vl-convert が無い場合は HTML にフォールバック)
barChartFile PNG "output.png" cfg "x" "y" labels vals

-- SVG 出力
mcmcDiagnosticsFile SVG "diag.svg" cfg names chain
```
