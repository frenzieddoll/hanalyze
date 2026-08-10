# hanalyze-viz

The **visualization / reporting layer** of [`hanalyze`](../README.md).
It turns analysis results into [Vega-Lite](https://vega.github.io/vega-lite/)
specs and emits them either as **single figures** (HTML / PNG / SVG) or as
**composed HTML reports** — 19 modules in total.

It depends on all five lower layers (`core` / `frame` / `bayes` / `models` /
`design`) plus 13 external packages (`hvega`, `aeson`, `dataframe-core`, …).
It is the top of the six split layers; the umbrella package sits above it.
The numerics behind a figure (regression fits, MCMC diagnostics, Pareto
fronts, …) are left to the lower layers — this layer only does **spec
construction and HTML output**.

> **If you want static images (SVG / PDF / PNG) drawn directly** rather than
> Vega-Lite, use the sibling package
> [`hanalyze-plot`](../hanalyze-plot/README.md)
> (`toPlot` / `Plottable`). The two are complementary, not exclusive.

## Main modules (all 19)

### Shared infrastructure

| Module | Role |
|---|---|
| `Viz.Core` | I/O helpers shared by every `Viz.*` module — `writeSpec` / `openInBrowser` / `vlJson`, and `OutputFormat = HTML \| PNG \| SVG` |
| `Viz.PlotConfig` | Plot settings shared by every `Viz.*` module (`PlotConfig` / `defaultConfig`) |
| `Viz.PlotData` | Source-agnostic intermediate representation of plot data (`PlotData` / `ToPlotData`) |
| `Viz.PlotData.DataFrame` | `ToPlotData` instances for `dataframe` (numeric / text columns → `PlotData`) |
| `Viz.Assets` | Generated module bundling the Vega / Vega-Lite / Vega-Embed JS for offline HTML |

### Single figures

| Module | Role |
|---|---|
| `Viz.Scatter` | Scatter plots and overlays — regression line / smoother / CI band, grouping, predicted vs actual |
| `Viz.Bar` | Bar charts (vertical / horizontal / stacked / grouped) |
| `Viz.Histogram` | Histograms, optionally overlaid with a theoretical density |
| `Viz.MCMC` | MCMC diagnostic plots — trace / posterior density / autocorrelation / forest / energy |
| `Viz.GP` | Gaussian-process regression plots (training data, posterior mean, credible band) |
| `Viz.Pareto` | Pareto fronts — scatter / pairs / parallel coordinates / hypervolume history / comparison |
| `Viz.ModelGraph` | Model DAG rendered with Mermaid.js |
| `Viz.ModelGraphDot` | Model DAG as Graphviz DOT (plate notation, equivalent to PyMC's `model_to_graphviz`) |

### HTML reports

| Module | Role |
|---|---|
| `Viz.ReportBuilder` | **The recommended builder** — hand a list of `ReportSection`s to `renderReport` |
| `Viz.ReportInstances` | `Reportable` instances for the various fit-result types |
| `Viz.Report` | Combined MCMC report (DAG, posterior summary table, diagnostics, pairs plot) |
| `Viz.GPReport` | Combined GP report (data overview, model comparison, interactive prediction, appendix) |
| `Viz.Taguchi` | Taguchi-method report (S/N ratios, main effects, optimal levels) |
| ⚠ `Viz.AnalysisReport` | **Deprecated.** The sum-type report dedicated to LM / GLM / GLMM / GP / HBM (~2000 lines). Superseded by `Viz.ReportBuilder`; kept only for compatibility with the existing CLI (`hanalyze regress --report`) and slated for removal |

## Using it standalone

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
  -- a single figure, written straight to HTML
  scatterPlotFile HTML "scatter.html"
    (defaultConfig "dep_delay vs month") df "month" "dep_delay"
  -- sections composed into one HTML report
  renderReport "report.html" (defaultReportConfig "Flight delays")
    [ secDataOverview df ["month"] "dep_delay"
    , secModelOverview "Linear regression" "dep_delay = b0 + b1 * month" Nothing
    , secCoefficients [("b0", 1.2), ("b1", 2.4)] (Just ("R2", 0.96))
    ]
```

The `*File` functions (`scatterPlotFile`, `histogramPlotFile`, …) take an
`OutputFormat` and write the file for you. If you only want the spec, use the
`*Plot` functions (`scatterPlot :: PlotConfig -> DataFrame -> Text -> Text ->
VegaLite`) and emit it later with `writeSpec` / `vlJson` from `Viz.Core`.

Besides `secDataOverview` / `secCoefficients` / `secFitScatter` /
`secResiduals`, `ReportBuilder` ships section builders for MCMC diagnostics
(`secMCMCDiagnostics`, `secPosteriorSummary`, `secForestPlot`), model
comparison (`secComparisonTable`) and interactive prediction
(`secInteractiveLM`, …).

Normally you just depend on the umbrella package `hanalyze`, where
`import Hanalyze` already gives you the common plotting functions.
Depend on a layer directly only when you want to minimise your dependency
footprint.

## Related docs

- Visualization overview: [docs/visualization/01-visualization.md](../docs/visualization/01-visualization.md)
- HTML reports: [02-report-builder.md](../docs/visualization/02-report-builder.md)
- MCMC diagnostic plots: [bayesian/viz-diagnostics.md](../docs/bayesian/viz-diagnostics.md)
- Vega-Lite vs static rendering: [03-plot-integration.md](../docs/visualization/03-plot-integration.md)

← [repository README](../README.md)
