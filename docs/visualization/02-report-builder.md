# HTML Reports — `Viz.ReportBuilder` and `Reportable`

> 🌐 **English** | [日本語](02-report-builder.ja.md)

> Related: [01-visualization.md](01-visualization.md) (one-shot bar / histogram / scatter plots),
> `Viz.AnalysisReport` (**deprecated** — LM/GLM/GLMM/GP/HBM-specific sum-type, kept for `regress --report` legacy compatibility)
>
> **Status**: `Viz.ReportBuilder` is the going-forward standard. `Viz.AnalysisReport` carries a `{-# DEPRECATED #-}` pragma; importers get a GHC warning. New models / visualizations must be implemented on top of `ReportBuilder`.

## Table of contents

1. [Overview & design](#1-overview--design)
2. [Section type and smart constructors](#2-section-type-and-smart-constructors)
3. [`renderReport`](#3-renderreport)
4. [`Reportable` typeclass](#4-reportable-typeclass)
5. [Per-model usage](#5-per-model-usage)
6. [CLI usage](#6-cli-usage)
7. [Custom reports](#7-custom-reports)
8. [Relationship to legacy `Viz.AnalysisReport`](#8-relationship-to-legacy-vizanalysisreport)
9. [Common patterns and pitfalls](#9-common-patterns-and-pitfalls)

---

## 1. Overview & design

`Viz.ReportBuilder` is a **compositional HTML report builder**. You define an
analysis as a list of `ReportSection` values; `renderReport` produces a single
self-contained HTML file with Vega-Lite assets embedded.

### Design principles

| Principle | Detail |
|---|---|
| **Composition** | Build `[ReportSection]`. Each section is an independent HTML chunk. |
| **Format independent** | Internally Vega-Lite specs are JSON-encoded into an HTML template. Mermaid DAGs are also supported. |
| **Easy to extend** | Adding a new model = one `Reportable` instance. |
| **Reuse existing assets** | Built on top of `hvega` and `Viz.Assets` (offline JS bundle). |
| **Successor of AnalysisReport** | All standard models (LM/GLM/GLMM/GP/HBM and beyond) live here via `Reportable` instances. |

### Why a separate module was needed

The legacy `Viz.AnalysisReport` (~2000 LoC) is built on a sum type
`ModelFit = RegFit | MixFit | GPFit | HBMFit | NoRegFit`, requiring source
edits to add each new model. Adding ridge / kernel / spline / RFF / RobustGP /
quantile / GAM / RF would not have scaled. The compositional approach lets
each model live in its own instance.

---

## 2. Section type and smart constructors

`ReportSection` is a sum type representing one HTML section:

```haskell
data ReportSection
  = SecDataOverview DataFrame [Text] Text                  -- column statistics
  | SecModelOverview Text Text [(Text, Text)] (Maybe Text) -- type / formula / extras / Mermaid
  | SecCoefficients [(Text, Double)] (Maybe (Text, Double)) -- coefficient table + R²
  | SecFitScatter Text Text [Double] [Double] (Maybe SmoothCurve)
  | SecResiduals [Double] [Double]
  | SecBarChart Text [(Text, Double)]
  | SecVega Text VegaLite
  | SecMermaid Text
  | SecTable Text [Text] [[Text]]
  | SecKeyValue Text [(Text, Text)]
  | SecMarkdown Text Text
  | SecHtml Text Text                                      -- raw HTML escape hatch
  | SecInteractiveLM ...                                   -- single-x slider + scatter
  | SecInteractiveMulti Text InteractiveModel              -- multi-x slider + scatter
  | SecCollapsible Text Bool [ReportSection]               -- folding group
  | SecCard Text [ReportSection]                           -- card container
  | SecStatRow [(Text, Text)]                              -- horizontal info-box row
```

All smart constructors carry the `sec` prefix. See the [Japanese version](02-report-builder.ja.md#2-section-型と-smart-constructors) for the exhaustive table.

### `SmoothCurve` (smooth curve with optional confidence band)

```haskell
data SmoothCurve = SmoothCurve
  { scXs    :: [Double]   -- grid points
  , scYs    :: [Double]   -- median / predicted curve
  , scLower :: [Double]   -- lower band (empty list = no band)
  , scUpper :: [Double]   -- upper band
  }
```

### Model comparison & diagnostic sections (added in Cycle 1)

| Function | Purpose |
|---|---|
| `secComparisonTable title headers rows mBest`   | Comparison table. Pass `Just i` (0-based) to highlight that row in yellow (best model). |
| `secForestPlot title rows`                       | Forest plot from `[(name, lower, mean, upper)]` — drawn as horizontal HDI bars + median dots. Useful for hierarchical BLUPs / Bayesian coefficients. |
| `secFeatureImportance title items`               | Sorts `[(label, value)]` descending and renders as a bar chart. RF / GBM importances. |
| `secPPC title observed reps`                     | Posterior Predictive Check — observed-data KDE (thick line) overlaid on per-replicate KDEs (light lines). |

### MCMC / posterior sections

| Function | Purpose |
|---|---|
| `secMCMCDiagnostics title params chain`           | KDE + trace (via `Viz.MCMC.mcmcDiagnostics`) |
| `secMCMCDiagnosticsMulti title params chains`     | Multi-chain version (chains color-coded) |
| `secMCMCAutocorr title maxLag params chain`       | Autocorrelation bars |
| `secMCMCPair title pa pb chain`                   | Pair scatter for two parameters |
| `secPosteriorSummary title rows`                  | mean/SD/2.5%/97.5%/ESS/R-hat table |

### Folding / grouping

| Function | Purpose |
|---|---|
| `secCollapsible title open children`              | Wraps children in `<details>`. Use `open=False` for sections kept closed by default (e.g. residual plots, MCMC diagnostics). |
| `secCard title children`                          | Light-background card container, normally inside a `secCollapsible`. |
| `secStatRow [(label, value)]`                     | Flat row of info boxes (no section wrapping). |

### Markdown appendix

| Function | Purpose |
|---|---|
| `secAppendixFromMd title path`                    | Reads a markdown file, parses with the bundled simple parser, and renders an appendix section. |
| `renderSimpleMarkdown txt`                        | The simple parser itself (headings/paragraphs/lists/bold/italic/code/links). |

`docs/principles/{lm,glm,glmm,gp,hbm}.ja.md` ship as principle explanations
auto-loadable via `secAppendixFromMd`.

### Interactive prediction

| Function | Purpose |
|---|---|
| `secInteractiveLM title xc yc xs ys smooth (xMin, xMax)` | Single-variate. Sliding x re-renders predicted point with the supplied `SmoothCurve` (and band). Works for GP / HBM ribbon-style forecasts too. |
| `secInteractiveMulti title im` | Multi-variate. `InteractiveModel` (intercept / β vector / link) → JS-evaluated `β₀ + Σ β_j x_j → invLink(.)` with sliders for each x_j and a primary-axis dropdown. CI band from σ_hat. |
| `secInteractiveMultiOut title imo` | **True multi-output (1 input → q output curves)** (Phase M1-M8). One input slider recomputes all q predictions live in the browser and renders them as a curve via Vega-Lite. `InteractivePredictor = PredLinearMO \| PredKernelRBF1` switches between linear and RBF kernel-ridge predictors. Built with `mkInteractiveMOLinear` / `mkInteractiveMOKernelRBF`. See [io/02-multireg.md](../io/02-multireg.md). |

---

## 3. `renderReport`

```haskell
renderReport :: FilePath -> ReportConfig -> [ReportSection] -> IO ()

data ReportConfig = ReportConfig
  { rcTitle    :: Text
  , rcSubtitle :: Text   -- empty string hides
  }

defaultReportConfig :: Text -> ReportConfig
```

### Minimal example

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Viz.ReportBuilder

main :: IO ()
main = do
  let cfg = defaultReportConfig "Hello report"
      sections =
        [ secMarkdown "Intro" "This is a tiny report."
        , secKeyValue "Stats" [("Total runs", "42"), ("Status", "OK")]
        , secBarChart "Sales" [("Jan", 120), ("Feb", 95), ("Mar", 140)]
        ]
  renderReport "hello.html" cfg sections
```

→ `hello.html` (~830 KB with Vega-Lite assets) opens in any modern browser.

---

## 4. `Reportable` typeclass

```haskell
class Reportable a where
  toReport :: ReportConfig -> DataFrame -> [Text] -> Text -> a
           -> [ReportSection]
```

### Provided instances (`Viz.ReportInstances`)

| Type | Module | Sections produced |
|---|---|---|
| `LMReport`        | `Viz.ReportInstances` | DataOverview / ModelOverview / Collapsible(StatRow + coefficients + scatter + residuals) / InteractiveMulti |
| `GLMReport`       | `Viz.ReportInstances` | DataOverview / ModelOverviewLink / Collapsible(StatRow + coefficients + scatter + residuals) / InteractiveMulti |
| `GLMMReport`      | `Viz.ReportInstances` | DataOverview / ModelOverviewLink / Collapsible(R²/σ²_u/σ²/ICC + fixed effects + **BLUP table** + residuals) / InteractiveMulti |
| `GPReport`        | `Viz.ReportInstances` | DataOverview / ModelOverviewExtras (kernel) / Collapsible(hyperparameters + LML + residuals) / InteractiveLM (with credible band) |
| `HBMLinearReport` | `Viz.ReportInstances` | DataOverview / ModelOverviewExtras (sampler + DAG) / Collapsible(R²/accept-rate + posterior means + **MCMC diagnostics** + residuals) / InteractiveLM (with credible ribbon) |
| `HBMReport`       | `Viz.ReportInstances` | General HBM (multi-x / non-linear). User-supplied posterior summary + ribbon function. (Cycle 7) |
| `QRFit`           | `Model.Quantile`    | DataOverview / ModelOverview / Collapsible(τ + Pseudo R¹ + Pinball + coefficients + scatter + residuals) |
| `GAMFit`          | `Model.GAM`         | DataOverview / ModelOverview / Collapsible(R²/degree/knots + **per-feature partial-effect cards** + residuals) |
| `RFReport`        | `Viz.ReportInstances` | DataOverview / ModelOverview / Collapsible(R² + Trees/Features + **Feature importance** + residuals) |
| `RegFit`          | `Model.Regularized` | DataOverview / ModelOverview / Coefficients (β + R²) / KeyValue (penalty/λ/sparsity) / FitScatter / Residuals |
| `SplineFit`       | `Model.Spline`      | DataOverview / ModelOverview / KeyValue (kind/knots) / FitScatter / Residuals |
| `KernelRidgeFit`  | `Model.Kernel`      | DataOverview / ModelOverview / KeyValue (kernel/h/λ) / FitScatter / Residuals |
| `RFFRidgeFit`     | `Model.RFF`         | DataOverview / ModelOverview / KeyValue (D/ℓ/σ_f/λ) / FitScatter / Residuals |
| `RobustGPFit`     | `Model.GPRobust`    | DataOverview / ModelOverview / KeyValue (kernel/likelihood/IRLS iterations) |

### Library usage

```haskell
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector as V

import DataIO.CSV         (loadAuto)
import DataFrame.Core     (getNumeric)
import Model.Regularized  (Penalty (..), fitRegularized)
import Viz.ReportBuilder
import Viz.ReportInstances ()  -- pull instances into scope

main :: IO ()
main = do
  Right df <- loadAuto "data.csv"
  let Just xVec = getNumeric "x" df
      Just yVec = getNumeric "y" df
      n    = V.length xVec
      xMat = LA.fromColumns
               [ LA.konst 1 n
               , LA.fromList (V.toList xVec) ]
      yLA  = LA.fromList (V.toList yVec)
      fit  = fitRegularized (L2 0.1) xMat yLA
      cfg  = defaultReportConfig "Ridge demo"
  renderReport "ridge.html" cfg (toReport cfg df ["x"] "y" fit)
```

The simplest API: **`toReport cfg df xCols yCol fit`** then pass to `renderReport`.

---

## 5. Per-model usage

The CLI exposes every model via `--report [FILE]`. From the library you go
through the relevant `Reportable` instance.

### Ridge / Lasso / Elastic Net

```bash
hanalyze ridge data.csv "x1 x2 x3" y --penalty lasso --lambda 0.05 --report report.html
```

Sections: data overview / model overview / coefficients + R² / fit summary
(penalty / λ / sparsity / RMSE) / **regularization-path plot** (lambda sweep) /
fit scatter (single-x only) / residuals.

### Spline / Kernel / RFF / Quantile / GAM / RF

```bash
hanalyze spline   data.csv x y --type natural --knots 8 --report
hanalyze kernel   data.csv x y --method kr    --bandwidth 0.5 --report
hanalyze kernel   data.csv x y --method rff   --features 200  --report
hanalyze quantile data.csv x y --taus 0.1,0.5,0.9 --report
hanalyze gam      data.csv "x1 x2 x3" y --knots 8 --report
hanalyze rf       data.csv "x1 x2"     y --trees 200 --report
```

Library calls are direct: build the fit and hand it to `toReport`.

### LM / GLM / GLMM / GP / HBM

```bash
hanalyze regress data.csv x y LM   --ci 0.95 --report
hanalyze regress data.csv x y GLM  -d poisson -l log --report
hanalyze regress data.csv x y LM   --group school --waic --report
hanalyze regress data.csv x y GP   --report
hanalyze regress data.csv x y HBM  --report --waic
```

The CLI builds these via the helpers `cliRegressSections` / `cliMixedSections`
/ `cliGPSections` / `cliHBMSections` in `app/Main.hs`. Library users can use
`LMReport`, `GLMReport`, `GLMMReport`, `GPReport`, `HBMLinearReport`, or the
fully general `HBMReport`.

### Taguchi

Taguchi has its own `Viz.Taguchi.renderTaguchiReport` due to its specialized
factor-effect / SN-ratio structure. CLI:
`hanalyze taguchi analyze L9 -f ... --csv ... --report taguchi.html`.

---

## 6. CLI usage

Every fitting subcommand supports `--report [FILE]`:

```bash
hanalyze regress  data.csv x y LM   --report
hanalyze ridge    data.csv x y      --penalty ridge --report
hanalyze kernel   data.csv x y      --method kr    --report
hanalyze spline   data.csv x y      --report
hanalyze quantile data.csv x y      --tau 0.5      --report
hanalyze gam      data.csv "x1 x2 x3" y --knots 8  --report
hanalyze rf       data.csv "x1 x2" y --trees 100   --report
hanalyze taguchi  analyze L9 -f ... --csv ... --report
```

Omitting the report path uses `<subcommand>.html`. **All subcommands now
render through `Viz.ReportBuilder`** (Phase 2 complete). `Viz.AnalysisReport`
remains in tree as a deprecated legacy module.

---

## 7. Custom reports

When you want to combine multiple models, mix domain-specific tables, or use
a private Vega-Lite spec:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Viz.ReportBuilder
import Graphics.Vega.VegaLite (VegaLite)

myReport :: IO ()
myReport = do
  let cfg = ReportConfig { rcTitle = "Custom comparison", rcSubtitle = "Ridge vs Lasso vs ElasticNet" }
      sections =
        [ secMarkdown "Setup" "We compare three regularized regressions on the same dataset."
        , secComparisonTable "Hyperparameters"
            ["Model", "λ", "α"]
            [ ["Ridge",      "0.10", "—"]
            , ["Lasso",      "0.05", "—"]
            , ["ElasticNet", "0.10", "0.5"]
            ]
            (Just 2)  -- highlight ElasticNet row
        , secBarChart "RMSE comparison"
            [("Ridge", 0.42), ("Lasso", 0.39), ("ElasticNet", 0.38)]
        , secVega "Custom Vega-Lite chart" myCustomSpec
        , secKeyValue "Conclusion"
            [ ("Best model", "ElasticNet")
            , ("Reason",     "Lowest RMSE + sparsity 4/10")
            ]
        ]
  renderReport "comparison.html" cfg sections

myCustomSpec :: VegaLite
myCustomSpec = ...  -- any Vega-Lite spec
```

### Composing existing instances

```haskell
let baseSections = toReport cfg df ["x"] "y" myFit  -- defaults from instance
    extra        = [ secMarkdown "Note" "Cross-validation results below."
                   , secVega "CV trace" cvSpec
                   ]
renderReport "out.html" cfg (baseSections ++ extra)
```

Order is preserved.

---

## 8. Relationship to legacy `Viz.AnalysisReport`

`Viz.AnalysisReport` is **deprecated** (`{-# DEPRECATED #-}` pragma — importers
get a GHC warning). `Viz.ReportBuilder` is the going-forward standard.

| Aspect | `Viz.AnalysisReport` (deprecated) | `Viz.ReportBuilder` (★ standard) |
|---|---|---|
| Coverage | LM / GLM / GLMM / GP / HBM only | All models (RegFit / Spline / Kernel / RFF / RobustGP, LM / GLM / GLMM / GP / HBM, Quantile / GAM / RF) |
| Design | Sum type `ModelFit` (~2000 LoC, tightly coupled) | Section list + `Reportable` typeclass (easy to extend) |
| Extension | Add a `ModelFit` variant + each section handler | Add a single `Reportable` instance |
| Layout | 5 fixed sections (Data / Model / Result / Interactive / Appendix) | Arbitrary section order |
| Interactive prediction | Built-in | `secInteractiveLM` / `secInteractiveMulti` |
| MCMC integration | Built-in for HBM | `secMCMCDiagnostics` / `secMCMCAutocorr` / `secMCMCPair` / `secPosteriorSummary` |
| Status | Removal scheduled (kept while `regress --report` legacy code path retains it) | Active development |

**Selection guide**:
- New code → always `ReportBuilder`.
- Legacy `regress --report` is now switched to `ReportBuilder` (Phase 2 complete).
- HBM-only MCMC diagnostics → `Viz.Report` (a focused MCMC-only report).

### Migration roadmap

1. **Phase 1 (done)**: Add `Reportable` instances for LM / GLM / GLMM / GP / HBM (parity with AR without sum types)
   - ✅ `LMReport` / `GLMReport` (Cycle 2)
   - ✅ `QRFit` / `GAMFit` / `RFReport` (Cycle 3)
   - ✅ `GLMMReport` / `GPReport` / `HBMLinearReport` (Cycle 4)
2. **Phase 2 (done)**: Switch CLI `regress --report` to the ReportBuilder path (Cycle 5)
3. **Phase 3 (paused)**: Remove `Viz.AnalysisReport` — kept as legacy per user request

---

## 9. Common patterns and pitfalls

### File size

Each report is **~800-870 KB** (Vega-Lite + Mermaid assets included). The cost
buys offline operation; assets are hardcoded in `Viz.Assets`.

### Number formatting

`secCoefficients` / `secFitScatter` use `printf "%.4f"` internally. Integers
are displayed as integers (`150`, not `150.0`). For custom formatting use
`secKeyValue` and supply your own pre-formatted text.

### Mermaid diagrams

Mermaid is fetched from a CDN (offline use requires an internet connection
*or* using `Viz.Assets`-based DAG output). For HBM model graphs, `Viz.ModelGraph`
auto-extracts dependencies from the Track interpretation and produces a
Mermaid string suitable for `secMermaid` (or `secModelOverviewExtras` mDag arg).

### `Reportable` interface limitations

`toReport :: ... -> a -> [ReportSection]` accepts only
`(ReportConfig, DataFrame, [Text], Text, a)`. To inject extra info (external
CV, comparison chains, etc.) append your own sections after the base output:

```haskell
let base = toReport cfg df xs y fit
    augmented = base ++ [secVega "External" extSpec]
renderReport path cfg augmented
```

### Choosing a HBM wrapper

| Wrapper | When to use |
|---|---|
| `HBMLinearReport` | Single-x Bayesian linear regression `y ~ Normal(α + β x, σ)` — common case shortcut. |
| `HBMReport`       | Multi-x / non-linear / custom HBM. You supply chain, posterior summary rows, and (optionally) a precomputed prediction ribbon `(grid, mid, lo, hi)`. The wrapper builds standard data overview / model overview / collapsible result + MCMC diagnostics / interactive prediction. |

---

## Related documents

- [01-visualization.md](01-visualization.md) — single-shot plots (bar, histogram, scatter, Mermaid DAG)
- [../doe-optim/03-orthogonal-taguchi.md](../doe-optim/03-orthogonal-taguchi.md) — orthogonal arrays and Taguchi method (incl. `Viz.Taguchi`)
- [../regression/06-quantile-gam-rf.md](../regression/06-quantile-gam-rf.md) — Quantile / GAM / Random Forest
