# Visualization guide (Viz.*)

> 🌐 **English** | [日本語](01-visualization.ja.md)

> Related demos:
> - [`hbm-example`](../demo/HBMExample.hs) — `Viz.Report` (KDE / trace / DAG / pair scatter)
> - [`hbm-regression`](../demo/HBMRegressionDemo.hs) — `Viz.AnalysisReport` HBMFit (DAG + MCMC + credible-interval predictions)
> - [`simpson-paradox`](../demo/SimpsonParadoxDemo.hs) — multi-model side-by-side via `writeComparisonReport`
> - [`bar-demo`](../demo/BarDemo.hs) — `Viz.Bar` + PNG/SVG export
> - [`gp-demo`](../demo/GPDemo.hs) — GP-specific report
>
> CLI: `--report` builds an AnalysisReport; `--format png|svg` renders individual plots as images.

## Output formats

All visualization functions support:

```haskell
data OutputFormat = HTML | PNG | SVG
```

- **HTML**: self-contained HTML embedding Vega-Lite + vegaEmbed (no vl-convert needed)
- **PNG / SVG**: requires vl-convert (`vl-convert` must be on the PATH)

If PNG/SVG generation fails, the function automatically falls back to HTML.

---

## Viz.Report — integrated MCMC HTML report (recommended)

`Viz.Report` collects diagnostic plots, the model graph, and summary
statistics into a **single self-contained HTML file**.

```haskell
import Viz.Report

data MCMCReport = MCMCReport
  { reportTitle    :: Text
  , reportGraph    :: Maybe ModelGraph  -- Nothing to omit the model graph
  , reportChain    :: Chain             -- representative chain
  , reportChains   :: [Chain]           -- all chains (empty = single-chain mode)
  , reportParams   :: [Text]
  , reportPairs    :: [(Text, Text)]    -- pair scatter
  , reportMaxLag   :: Int               -- max lag for autocorrelation (default 40)
  }

defaultReport :: Text -> Chain -> [Text] -> MCMCReport
renderReport  :: FilePath -> MCMCReport -> IO ()
```

### Single-chain report

```haskell
chain <- nuts model cfg initP gen
let report = (defaultReport "My Model" chain (sampleNames model))
               { reportGraph = Just (buildModelGraph model edges) }
renderReport "report.html" report
```

### Multi-chain report (with R-hat)

```haskell
chains <- nutsChains model cfg 4 initP gen
let report = (defaultReport "My Model" (head chains) (sampleNames model))
               { reportGraph  = Just graph
               , reportChains = chains        -- setting this enables multi-chain mode
               , reportPairs  = [("mu","tau")]
               }
renderReport "report_multi.html" report
```

**Multi-chain HTML layout:**
- **Model Graph** — Mermaid.js DAG (when reportGraph is Just)
- **Posterior Summary** — Mean / SD / 2.5% / 97.5% / ESS / **R-hat** table
  (R-hat < 1.01: green, ≥ 1.01: red)
- **MCMC Diagnostics** — KDE density (94% HDI) + per-chain colored trace
- **Autocorrelation** — autocorrelation bar chart
- **Pair Scatter** — joint posterior scatter

---

## Viz.MCMC — individual MCMC plots

When you want individual plots without going through `Viz.Report`:

```haskell
import Viz.MCMC
import Viz.Core (defaultConfig, OutputFormat (..))

-- Single-chain diagnostics (KDE + trace stacked vertically)
mcmcDiagnosticsFile HTML "diag.html" (defaultConfig "Model") names chain

-- Multi-chain diagnostics (merged KDE + colored trace)
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

## Viz.Bar — bar charts

```haskell
import Viz.Bar
import Viz.Core (defaultConfig, OutputFormat (..))
```

### Vertical bar chart

```haskell
barChart :: PlotConfig -> Text -> Text -> [Text] -> [Double] -> VegaLite
barChartFile :: OutputFormat -> FilePath -> PlotConfig -> Text -> Text -> [Text] -> [Double] -> IO ()

barChartFile HTML "bar.html" (defaultConfig "Scores") "Class" "Score"
  ["Class A", "Class B", "Class C"] [82.3, 76.1, 91.5]
```

### Horizontal bar chart

```haskell
barChartH :: PlotConfig -> Text -> Text -> [Text] -> [Double] -> VegaLite
barChartHFile :: OutputFormat -> FilePath -> ...
```

### Stacked bar chart

```haskell
stackedBar :: PlotConfig -> Text -> Text -> Text -> [Text] -> [Double] -> [Text] -> VegaLite
-- (cfg, xTitle, yTitle, colorTitle, xVals, yVals, colorVals)

stackedBarFile HTML "stacked.html" (defaultConfig "Stacked") "Month" "Sales" "Product"
  ["Jan","Jan","Feb","Feb"]   -- x
  [100, 80, 120, 60]          -- y
  ["A","B","A","B"]           -- color
```

### Grouped bar chart

```haskell
groupedBar :: PlotConfig -> Text -> Text -> Text -> [Text] -> [Double] -> [Text] -> VegaLite
groupedBarFile HTML "grouped.html" (defaultConfig "Grouped") "Month" "Sales" "Product"
  ["Jan","Jan","Feb","Feb"]
  [100, 80, 120, 60]
  ["A","B","A","B"]
```

---

## Viz.Histogram — histograms

```haskell
import Viz.Histogram

-- Plain histogram
histogramPlotFile HTML "hist.html"
  (defaultConfig "Score") "score" vals Nothing  -- Nothing = auto bin count

-- Overlay theoretical distribution
histogramWithDensityFile HTML "hist_fit.html"
  (defaultConfig "Score") "score" vals Nothing (Normal 2.5 1.0)
```

Supported theoretical distributions (also valid as `--fit` arguments):
`Normal`, `Binomial`, `Poisson`, `Exponential`, `Gamma`, `Beta`.

---

## Viz.Scatter — scatter and regression curves

The CLI in `app/Main.hs` generates these automatically, but they can also
be invoked directly as library functions.

```haskell
import Viz.Scatter
import Viz.Core (defaultConfig)

-- Scatter + regression curve
scatterWithSmooth :: PlotConfig -> Text -> Text -> [Double] -> [Double] -> SmoothFit -> VegaLite

-- Per-group scatter
scatterWithGroups :: PlotConfig -> Text -> Text -> Text
                  -> [(Text, [Double], [Double], SmoothFit)] -> VegaLite

-- Predicted vs Actual diagnostic
predictedVsActual :: PlotConfig -> Text -> [Double] -> [Double] -> VegaLite
```

---

## Viz.ModelGraph — model-structure DAG

```haskell
import Viz.ModelGraph

buildModelGraph :: Model a -> [(Text, Text)] -> ModelGraph
modelGraphFile  :: OutputFormat -> FilePath -> ModelGraph -> IO ()

-- Edge list: (from, to) = (parent, child)
let graph = buildModelGraph model
              [ ("mu",  "theta_1"), ("mu",  "theta_2")
              , ("tau", "theta_1"), ("tau", "theta_2")
              , ("theta_1", "y_1"), ("theta_2", "y_2")
              ]
modelGraphFile HTML "graph.html" graph
```

**Node rendering:**
- **Rectangle**: latent variable (sample) — root nodes display the prior parameters
- **Stadium (rounded rectangle)**: observed variable (observe)

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

-- Custom
let cfg = (defaultConfig "Title") { plotWidth = 800, plotHeight = 600 }
```

---

## PNG/SVG output

vl-convert is required. Confirm it's on the PATH:

```bash
which vl-convert   # check installation
```

```haskell
-- PNG output (falls back to HTML if vl-convert is missing)
barChartFile PNG "output.png" cfg "x" "y" labels vals

-- SVG output
mcmcDiagnosticsFile SVG "diag.svg" cfg names chain
```
