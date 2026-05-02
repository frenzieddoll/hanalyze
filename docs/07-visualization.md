# Visualization Guide (Viz.*)

> 🌐 **English** | [日本語](07-visualization.ja.md)

> Related demos:
> - [`hbm-example`](../demo/HBMExample.hs) — `Viz.Report` (KDE / trace / DAG / pair scatter)
> - [`hbm-regression`](../demo/HBMRegressionDemo.hs) — `Viz.AnalysisReport` HBMFit (DAG + MCMC + credible-band prediction)
> - [`simpson-paradox`](../demo/SimpsonParadoxDemo.hs) — multi-model comparison via `writeComparisonReport`
> - [`bar-demo`](../demo/BarDemo.hs) — `Viz.Bar` + PNG/SVG export
> - [`gp-demo`](../demo/GPDemo.hs) — GP-specific report
>
> CLI: `--report` to generate the AnalysisReport, `--format png|svg` to also export plots as images.

## Output formats

All visualization functions support:

```haskell
data OutputFormat = HTML | PNG | SVG
```

- **HTML**: self-contained file with Vega-Lite + vegaEmbed embedded (no `vl-convert` needed)
- **PNG / SVG**: requires `vl-convert` on PATH

If PNG/SVG generation fails, the function automatically falls back to HTML.

---

## Viz.Report — Integrated MCMC HTML report (top recommendation)

`Viz.Report` packages diagnostic plots, model graph, and summary statistics into
**a single self-contained HTML file**.

```haskell
import Viz.Report

data MCMCReport = MCMCReport
  { reportTitle    :: Text
  , reportGraph    :: Maybe ModelGraph  -- Nothing to omit the model graph
  , reportChain    :: Chain             -- main chain
  , reportChains   :: [Chain]           -- all chains (empty = single-chain mode)
  , reportParams   :: [Text]
  , reportPairs    :: [(Text, Text)]    -- pair scatter plots
  , reportMaxLag   :: Int               -- max autocorr lag (default 40)
  }

defaultReport :: Text -> Chain -> [Text] -> MCMCReport
renderReport  :: FilePath -> MCMCReport -> IO ()
```

### Single-chain report

```haskell
chain <- nuts model cfg initP gen
let report = (defaultReport "My Model" chain (sampleNames model))
               { reportGraph = Just (buildModelGraph model) }
renderReport "report.html" report
```

### Multi-chain report (with R-hat)

```haskell
chains <- nutsChains model cfg 4 initP gen
let report = (defaultReport "My Model" (head chains) (sampleNames model))
               { reportGraph  = Just graph
               , reportChains = chains        -- enables multi-chain mode
               , reportPairs  = [("mu","tau")]
               }
renderReport "report_multi.html" report
```

**Multi-chain HTML contents:**
- **Model Graph** — Mermaid.js DAG (when `reportGraph` is `Just`)
- **Posterior Summary** — Mean / SD / 2.5% / 97.5% / ESS / **R-hat** table
  (R-hat < 1.01: green, ≥ 1.01: red)
- **MCMC Diagnostics** — KDE density (94% HDI) + per-chain colored traces
- **Autocorrelation** — autocorr bars
- **Pair Scatter** — joint posterior

---

## Viz.AnalysisReport — Multi-section analysis report

For LM / GLM / GLMM / GP / HBM, generates an HTML with sections for data summary,
model overview, regression results, interactive prediction, and appendix.

```haskell
import Viz.AnalysisReport

data ModelFit
  = RegFit FitSummary       -- LM / GLM
  | MixFit GLMMSummary      -- LME / GLMM
  | GPFit  GPFitSummary     -- Gaussian Process
  | HBMFit HBMRegSummary    -- Hierarchical Bayes (NUTS posterior)
  | NoRegFit

writeAnalysisReport
  :: FilePath -> AnalysisReportConfig -> DataFrame -> [Text] -> Text
  -> ModelFit -> [NamedPlot] -> IO ()
```

### Multi-model comparison report

```haskell
data CompareEntry = CompareEntry
  { ceLabel :: Text   -- e.g. "LM (Pooled)"
  , ceColor :: Text   -- CSS color
  , ceFit   :: ModelFit
  }

writeComparisonReport
  :: FilePath -> AnalysisReportConfig -> DataFrame -> [Text] -> Text
  -> [CompareEntry] -> IO ()
```

This produces a single HTML containing model overview, predictions overlay (all model
curves on one scatter), coefficient table (with HBM 95% CIs), and WAIC/LOO comparison
if available. See [`SimpsonParadoxDemo.hs`](../demo/SimpsonParadoxDemo.hs).

### Export plots as images

```haskell
writeAnalysisReportPlots
  :: FilePath        -- prefix (no extension)
  -> OutputFormat    -- PNG / SVG (HTML uses writeAnalysisReport instead)
  -> [NamedPlot]
  -> IO [FilePath]
```

When the CLI is invoked with `--format png` or `--format svg` together with `--report`,
each `NamedPlot` is exported as a separate image alongside the HTML.

---

## Viz.MCMC — Standalone MCMC plots

For finer control without `Viz.Report`:

```haskell
import Viz.MCMC
import Viz.Core (defaultConfig, OutputFormat (..))

-- Single-chain diagnostics (KDE + trace stacked)
mcmcDiagnosticsFile HTML "diag.html" (defaultConfig "Model") names chain

-- Multi-chain (KDE merged + per-chain colored trace)
mcmcDiagnosticsMultiFile HTML "diag_multi.html" (defaultConfig "Model") names chains

-- Multi-chain trace only
multiTracePlotFile HTML "trace.html" (defaultConfig "Trace") names chains

-- Autocorrelation bar chart
autocorrPlotFile HTML "acf.html" (defaultConfig "ACF") 40 names chain

-- Pair scatter
pairScatterFile HTML "pair.html" (defaultConfig "μ vs τ") "mu" "tau" chain

-- KDE only
posteriorPlotFile HTML "kde.html" (defaultConfig "KDE") names chain
```

---

## Viz.Bar — Bar charts

```haskell
import Viz.Bar
import Viz.Core (defaultConfig, OutputFormat (..))
```

### Vertical bars

```haskell
barChart :: PlotConfig -> Text -> Text -> [Text] -> [Double] -> VegaLite
barChartFile :: OutputFormat -> FilePath -> PlotConfig -> Text -> Text -> [Text] -> [Double] -> IO ()

barChartFile HTML "bar.html" (defaultConfig "Scores") "Class" "Score"
  ["A", "B", "C"] [82.3, 76.1, 91.5]
```

### Horizontal bars

```haskell
barChartH :: PlotConfig -> Text -> Text -> [Text] -> [Double] -> VegaLite
barChartHFile :: OutputFormat -> FilePath -> ...
```

### Stacked bars

```haskell
stackedBar :: PlotConfig -> Text -> Text -> Text -> [Text] -> [Double] -> [Text] -> VegaLite
-- (cfg, xTitle, yTitle, colorTitle, xVals, yVals, colorVals)

stackedBarFile HTML "stacked.html" (defaultConfig "Stacked") "Month" "Sales" "Product"
  ["Jan","Jan","Feb","Feb"]   -- x
  [100, 80, 120, 60]          -- y
  ["A","B","A","B"]           -- color/segment
```

### Grouped bars

```haskell
groupedBar :: PlotConfig -> Text -> Text -> Text -> [Text] -> [Double] -> [Text] -> VegaLite
groupedBarFile HTML "grouped.html" (defaultConfig "Grouped") "Month" "Sales" "Product"
  ["Jan","Jan","Feb","Feb"]
  [100, 80, 120, 60]
  ["A","B","A","B"]
```

---

## Viz.Histogram — Histograms

```haskell
import Viz.Histogram

-- Plain histogram
histogramPlotFile HTML "hist.html"
  (defaultConfig "Score") "score" vals Nothing  -- Nothing = auto bin count

-- With theoretical density overlay
histogramWithDensityFile HTML "hist_fit.html"
  (defaultConfig "Score") "score" vals Nothing (Normal 2.5 1.0)
```

Supported theoretical densities (also via CLI's `--fit`):
`Normal`, `Binomial`, `Poisson`, `Exponential`, `Gamma`, `Beta`.

---

## Viz.Scatter — Scatter plots & regression curves

Auto-generated by the `app/Main.hs` CLI, but also usable directly.

```haskell
import Viz.Scatter
import Viz.Core (defaultConfig)

-- Scatter + regression curve
scatterWithSmooth :: PlotConfig -> Text -> Text -> [Double] -> [Double] -> SmoothFit -> VegaLite

-- Per-group scatter + curves
scatterWithGroups :: PlotConfig -> Text -> Text -> Text
                  -> [(Text, [Double], [Double], SmoothFit)] -> VegaLite

-- Predicted-vs-Actual diagnostic
predictedVsActual :: PlotConfig -> Text -> [Double] -> [Double] -> VegaLite
```

---

## Viz.ModelGraph — Model structure DAG

```haskell
import Viz.ModelGraph
import Model.HBM (buildModelGraph)

-- Auto-extract dependencies via Track type, no manual edges
let graph = buildModelGraph schoolModel
renderModelGraph "graph.html" "School Model" graph
```

**Node shapes:**
- **Rectangle**: latent variable (`sample`); root nodes show concrete prior parameters
- **Stadium (rounded)**: observed variable (`observe`)

---

## PlotConfig

```haskell
import Viz.Core

data PlotConfig = PlotConfig
  { plotTitle  :: Text
  , plotWidth  :: Int   -- pixels (default 600)
  , plotHeight :: Int   -- pixels (default 400)
  }

defaultConfig :: Text -> PlotConfig
-- plotWidth=600, plotHeight=400

-- Customize
let cfg = (defaultConfig "Title") { plotWidth = 800, plotHeight = 600 }
```

---

## PNG / SVG output

`vl-convert` must be on PATH:

```bash
which vl-convert   # check installation
```

```haskell
-- PNG output (falls back to HTML if vl-convert is missing)
barChartFile PNG "output.png" cfg "x" "y" labels vals

-- SVG output
mcmcDiagnosticsFile SVG "diag.svg" cfg names chain
```
