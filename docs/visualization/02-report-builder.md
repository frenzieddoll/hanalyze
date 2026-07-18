# HTML Report — `Hanalyze.Viz.ReportBuilder` and `Reportable`

> 🌐 **English** | [日本語](02-report-builder.ja.md)

> Related: [01-visualization.md](01-visualization.md) (bar charts, histograms, and other single plots),
> `Hanalyze.Viz.AnalysisReport` (**deprecated** — sum-type based, specialized for LM/GLM/GLMM/GP/HBM, retained for CLI `regress --report` compatibility)
>
> **Status**: `Hanalyze.Viz.ReportBuilder` is the future standard. `Hanalyze.Viz.AnalysisReport` is already deprecated (`{-# DEPRECATED #-}`) and will be removed once feature parity is achieved. All new models and visualizations should be implemented with ReportBuilder.

## Table of Contents

1. [Overview and Design Philosophy](#1-overview-and-design-philosophy)
2. [Section Type and Smart Constructors](#2-section-type-and-smart-constructors)
3. [Generating Reports with `renderReport`](#3-generating-reports-with-renderreport)
4. [The `Reportable` Type Class](#4-the-reportable-type-class)
5. [Usage Examples by Model](#5-usage-examples-by-model)
6. [Using from the CLI](#6-using-from-the-cli)
7. [Creating Custom Reports](#7-creating-custom-reports)
8. [Relationship with Existing `Hanalyze.Viz.AnalysisReport`](#8-relationship-with-existing-hanalyzevizanalysisreport)
9. [Common Patterns and Pitfalls](#9-common-patterns-and-pitfalls)

---

## 1. Overview and Design Philosophy

`Hanalyze.Viz.ReportBuilder` is a **composition-style HTML report builder**. It assembles analysis results as a **sequence of sections** and outputs self-contained HTML (including Vega-Lite assets) in a single file.

### Design Principles

| Principle | Description |
|---|---|
| **Composition** | Build `[ReportSection]` and compose it. Each section is an independent HTML chunk. |
| **Format independence** | Internally converts Vega-Lite specs to JSON and embeds them in HTML templates. Supports Mermaid DAGs as well. |
| **Easy model extension** | To add a new model or analysis, just write one `Reportable` instance. |
| **Reuse existing assets** | Reuses `hvega` (Vega-Lite specs) and `Hanalyze.Viz.Assets` (offline JS bundles). |
| **Successor to AnalysisReport** | Replaces `Hanalyze.Viz.AnalysisReport` (deprecated) as the future standard. All detailed reports for all models including LM/GLM/GLMM/GP/HBM are to be built using ReportBuilder + `Reportable` instances. |

### Why a Separate Module Was Needed

The existing `Hanalyze.Viz.AnalysisReport` (~2000 lines) was specialized for five model types
(`ModelFit = RegFit | MixFit | GPFit | HBMFit | NoRegFit`) using a sum-type approach, requiring edits to the main module each time a new model was added. Adding eight more types (ridge / kernel / spline / RFF / RobustGP / quantile / gam / rf) was not practical, so we switched to a **LEGO-style section-based approach**.

---

## 2. Section Type and Smart Constructors

`ReportSection` is a sum type representing one HTML section:

```haskell
data ReportSection
  = SecDataOverview DataFrame [Text] Text                  -- Data statistics
  | SecModelOverview Text Text (Maybe Text)                -- Model type / formula / Mermaid
  | SecCoefficients [(Text, Double)] (Maybe (Text, Double))  -- Coefficient table + R²
  | SecFitScatter Text Text [Double] [Double] (Maybe SmoothCurve)  -- Scatter + curve
  | SecResiduals [Double] [Double]                         -- fitted vs residual
  | SecBarChart Text [(Text, Double)]                      -- Bar chart
  | SecVega Text VegaLite                                  -- Arbitrary Vega-Lite
  | SecMermaid Text                                        -- Mermaid DAG
  | SecTable Text [Text] [[Text]]                          -- Header + rows
  | SecKeyValue Text [(Text, Text)]                        -- 'Key: Value' table
  | SecMarkdown Text Text                                  -- Description text
  | SecHtml Text Text                                      -- raw HTML (escape hatch)
```

**Smart constructors** all have the `sec` prefix:

| Function | Purpose |
|---|---|
| `secDataOverview df xCols yCol` | Data row count + per-column type/N/min/max/mean/median/sd table |
| `secModelOverview type formula mMermaid` | Model type and formula, optionally with DAG diagram |
| `secCoefficients coeffs (Just ("R²", r2))` | Coefficient table + evaluation metrics (R², Pseudo-R¹, etc.) |
| `secFitScatter xc yc xs ys (Just smooth)` | Observed scatter plot + smooth curve (with optional confidence band) |
| `secResiduals fitted residuals` | Fitted vs residuals scatter plot |
| `secBarChart "Importance" pairs` | Bar chart from (label, value) list |
| `secVega "title" spec` | Embed arbitrary Vega-Lite spec as-is |
| `secMermaid "graph TD; A-->B"` | Embed flow/DAG using Mermaid syntax |
| `secTable "title" headers rows` | Arbitrary table |
| `secKeyValue "title" [("Trees", "100")]` | Simple metrics list |
| `secMarkdown "Notes" text` | Description text |
| `secHtml "title" rawHtml` | Pass HTML directly (escape hatch, no sanitization) |

### `SmoothCurve` (Smooth Curve with Confidence Band)

```haskell
data SmoothCurve = SmoothCurve
  { scXs    :: [Double]   -- Grid points
  , scYs    :: [Double]   -- Median (= prediction curve)
  , scLower :: [Double]   -- Confidence band lower limit (empty list = no band)
  , scUpper :: [Double]   -- Confidence band upper limit
  }
```

If `scLower` / `scUpper` are empty lists, the band is omitted and only the line is drawn.

---

## 3. Generating Reports with `renderReport`

```haskell
renderReport :: FilePath -> ReportConfig -> [ReportSection] -> IO ()

data ReportConfig = ReportConfig
  { rcTitle    :: Text   -- Header title + <title>
  , rcSubtitle :: Text   -- Hidden if empty string
  }

defaultReportConfig :: Text -> ReportConfig
```

### Minimal Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Hanalyze.Viz.ReportBuilder

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

→ `hello.html` (~830 KB, including Vega-Lite assets) is generated and can be opened in a browser.

---

## 4. The `Reportable` Type Class

A type class that generates default section sequences from fit result types:

```haskell
class Reportable a where
  toReport :: ReportConfig
           -> DataFrame
           -> [Text]    -- x column names
           -> Text      -- y column name
           -> a
           -> [ReportSection]
```

### Provided Instances (`Hanalyze.Viz.ReportInstances`)

| Type | Module | Included Sections |
|---|---|---|
| `LMReport`       | `Hanalyze.Viz.ReportInstances` | DataOverview / ModelOverview / Collapsible(regression results: StatRow + coefficients + scatter + residuals) / InteractiveMulti |
| `GLMReport`      | `Hanalyze.Viz.ReportInstances` | DataOverview / ModelOverviewLink / Collapsible(regression results: StatRow + coefficients + scatter + residuals) / InteractiveMulti |
| `GLMMReport`     | `Hanalyze.Viz.ReportInstances` | DataOverview / ModelOverviewLink / Collapsible(R²/σ²_u/σ²/ICC + fixed effects + **BLUP table** + residuals) / InteractiveMulti |
| `GPReport`       | `Hanalyze.Viz.ReportInstances` | DataOverview / ModelOverviewExtras (kernel) / Collapsible(hyperparameters + LML + residuals) / InteractiveLM (with confidence band) |
| `HBMLinearReport`| `Hanalyze.Viz.ReportInstances` | DataOverview / ModelOverviewExtras (sampler + DAG) / Collapsible(R²/acceptance rate + posterior mean coefficients + **MCMC diagnostics (KDE/trace/autocorrelation/pairs)** + residuals) / InteractiveLM (with credible interval) |
| `HBMReport`      | `Hanalyze.Viz.ReportInstances` | **General HBM** (multi-x / nonlinear support). User passes `hbmrPostSummaryG` and `hbmrYHatG`, optionally `hbmrRibbonG` (`HBMRibbon`). Sampler name, DAG, and pair plot targets are freely configurable. |
| `QRFit`          | `Hanalyze.Model.Quantile`    | DataOverview / ModelOverview / Collapsible(τ-quantile + Pseudo R¹ + Pinball + coefficients + scatter + residuals) |
| `GAMFit`         | `Hanalyze.Model.GAM`         | DataOverview / ModelOverview / Collapsible(R²/degree/knots + **partial effect cards per feature** + residuals) |
| `RFReport`       | `Hanalyze.Viz.ReportInstances` | DataOverview / ModelOverview / Collapsible(R² + Trees/Features + **Feature importance** + residuals) |
| `RegFit`         | `Hanalyze.Model.Regularized` | DataOverview / ModelOverview / Coefficients (β + R²) / KeyValue (penalty/λ/sparsity) / FitScatter / Residuals |
| `SplineFit`      | `Hanalyze.Model.Spline`      | DataOverview / ModelOverview / KeyValue (kind/knots) / FitScatter / Residuals |
| `KernelRidgeFit` | `Hanalyze.Model.Kernel`      | DataOverview / ModelOverview / KeyValue (kernel/h/λ) / FitScatter / Residuals |
| `RFFRidgeFit`    | `Hanalyze.Model.RFF`         | DataOverview / ModelOverview / KeyValue (D/ℓ/σ_f/λ) / FitScatter / Residuals |
| `RobustGPFit`    | `Hanalyze.Model.GPRobust`    | DataOverview / ModelOverview / KeyValue (kernel/likelihood/IRLS iter) |

Wrapper types for `LMReport` / `GLMReport`:

```haskell
-- LM
data LMReport = LMReport
  { lmrFit    :: FitResult     -- Result from Model.LM.fitLM etc.
  , lmrSmooth :: Maybe SmoothFit  -- Smooth curve from Model.LM.fitPolyWithSmooth (with optional confidence band)
  }

-- GLM
data GLMReport = GLMReport
  { glmrFit    :: FitResult
  , glmrFamily :: Family       -- Gaussian / Binomial / Poisson
  , glmrLink   :: LinkFn       -- Identity / Log / Logit / Sqrt
  , glmrSmooth :: Maybe SmoothFit
  }
```

Usage example (LM):

```haskell
import qualified Hanalyze.Model.Core as Core
import qualified Hanalyze.Model.LM   as LM
import qualified Hanalyze.Viz.ReportBuilder   as RB
import qualified Hanalyze.Viz.ReportInstances as RI

main = do
  Right df <- DataIO.CSV.loadAuto "data.csv"
  case LM.fitPolyWithSmooth (Core.CI 0.95) 100 df "x" "y" of
    Just (fit, sf) -> do
      let cfg    = RB.defaultReportConfig "My LM"
          report = RI.LMReport fit (Just sf)
      RB.renderReport "lm.html" cfg (RB.toReport cfg df ["x"] "y" report)
    Nothing -> putStrLn "fit failed"
```

### Collapsible / Grouping

| Function | Description |
|---|---|
| `secCollapsible title open children`              | Wraps child sections in `<details>`. `open=False` means initially collapsed. Used for items like regression results or MCMC diagnostics that should be "normally collapsed". |

The data overview section (`secDataOverview`) is also automatically collapsible:
- Statistics (default open) — n/min/Q1/median/Q3/max/mean/SD/**Skew/Kurtosis/Missing**
- Histograms per column (default closed) — Vega-Lite histograms for each numeric column

### MCMC / Posterior Distribution Related Sections

| Function | Description |
|---|---|
| `secMCMCDiagnostics title params chain`           | KDE + trace (via `Hanalyze.Viz.MCMC.mcmcDiagnostics`) |
| `secMCMCDiagnosticsMulti title params chains`     | Multi-chain version (chains color-coded) |
| `secMCMCAutocorr title maxLag params chain`       | Autocorrelation bar chart |
| `secMCMCPair title pa pb chain`                   | 2-parameter pair scatter |
| `secPosteriorSummary title rows`                  | mean/SD/2.5%/97.5%/ESS/R-hat table |

### Model Comparison and Diagnostics Sections

| Function | Description |
|---|---|
| `secComparisonTable title headers rows mBest`   | Model comparison table. `mBest = Just i` highlights that row (0-based) with yellow background. Used to highlight best models by WAIC/LOO/RMSE etc. |
| `secForestPlot title rows`                       | Forest plot — draws HDI/CI horizontal bars + median points from `[(name, lower, mean, upper)]`. Used for hierarchical model BLUPs and Bayesian coefficient comparisons. |
| `secFeatureImportance title items`               | Feature importance bar — converts `[(label, value)]` to bar chart **sorted in descending order**. Used to display importance from RF / GBM. |
| `secPPC title observed reps`                     | Posterior Predictive Check — overlays observed KDE (thick line) + each replicate KDE (thin line). `reps :: [[Double]]` is a group of posterior predictive samples. |

### Additional Visualization Sections

| Function | Description |
|---|---|
| `secCalibration title pPred yObs`                | Calibration plot — predicted probability vs observed frequency for binary classifiers. Points drawn over 10-bin partition + diagonal line (ideal curve). Point size = samples in bin. `yObs` is 0/1 values. |
| `sec3DScatter title xL yL zL xs ys zs`           | 3D scatter (pseudo). Since Vega-Lite doesn't support 3D, uses x/y axes + z encoded as viridis color. For multivariate exploration. |
| `secHeatmap title colLabels rowLabels values`    | 2D heatmap (rect + color encoding). For correlation matrices, confusion matrices, factor × level effects, etc. `values :: [[Double]]` is a rows × cols matrix. |

### Markdown Appendix

| Function | Description |
|---|---|
| `secAppendixFromMd title path`                    | Read specified md file, parse with simple parser, convert to HTML, add as appendix section |
| `renderSimpleMarkdown txt`                        | Standalone markdown→HTML parser (supports headings/paragraphs/lists/bold/italic/code/links) |

Short principle explanations for each model are placed in `docs/principles/{lm,glm,glmm,gp,hbm}.md`. Comparison demos automatically load and convert these to appendix sections.

### Interactive Prediction

| Function | Description |
|---|---|
| `secInteractiveLM title xc yc xs ys smooth (xMin, xMax)` | **Single-variable** version. Slider changes x, grid linear interpolation updates predicted value + confidence band. Works with nonlinear curves like GP/HBM or MCMC-derived prediction curves. |
| `secInteractiveMulti title im` | **Multivariate** version. Pass `InteractiveModel` (coefficients + link function), left side shows sliders for each x_j + main-axis dropdown, right side shows scatter + prediction curve. Each slider change triggers JS to recalculate β₀ + Σβ_j x_j → invLink to y_hat + redraw scatter. CI drawn as σ_hat ± 1.96 band. |
| `secInteractiveMultiOut title imo` | **True multi-output (1 input → q output curves)**. Single input slider lets JS instantly recalculate all q predicted values → drawn as prediction curves in Vega-Lite. `InteractivePredictor = PredLinearMO | PredKernelRBF1` switches between linear / RBF kernel ridge. Built with `mkInteractiveMOLinear` / `mkInteractiveMOKernelRBF`. Details: [regression/07-multireg.md](../regression/07-multireg.md) |

`InteractiveModel`:
```haskell
data InteractiveModel = InteractiveModel
  { imXCols     :: [Text]               -- Explanatory variable names
  , imYCol      :: Text                  -- Response name
  , imXValues   :: [[Double]]           -- Observations (n × p)
  , imYValues   :: [Double]
  , imIntercept :: Double                -- β₀
  , imBetas     :: [Double]              -- [β_j]
  , imLink      :: Text                  -- "identity" | "log" | "logit" | "sqrt"
  , imSlider    :: [(Double, Double, Double)]  -- (min, mid, max) per x
  , imCISigma   :: Maybe Double          -- Residual σ_hat (for CI, Nothing = no band)
  }
```

### LM/GLM/GLMM/GP/HBM Comparison Demo

`cabal run analysis-compare-demo` generates HTMLs for both existing AnalysisReport and new ReportBuilder for each model in `trash/cmp_<model>_{AR,RB}.html`. You can compare contents side-by-side.

| Model | AR Size | RB Size | Main Differences |
|---|---|---|---|
| LM       | ~866 KB | ~870 KB | RB adds interactive prediction + Markdown explanations |
| GLM (Poisson) | ~862 KB | ~875 KB | Same as above |
| GLMM (LME)    | ~849 KB | ~833 KB | RB includes BLUPs table + variance component KeyValue |
| GP (RBF) | ~914 KB | ~866 KB | RB is simplified (no kernel switcher) |
| HBM      | ~867 KB | **~1.1 MB** | RB includes MCMC diagnostics + autocorrelation + pairs + posterior summary all together |

### Usage Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector as V

import Hanalyze.DataIO.CSV         (loadAuto)
import DataFrame.Core     (DataFrame, getNumeric)
import Hanalyze.Model.Regularized  (Penalty (..), fitRegularized)
import Hanalyze.Viz.ReportBuilder
import Hanalyze.Viz.ReportInstances ()    -- expose instances

main :: IO ()
main = do
  Right df <- loadAuto "data.csv"
  let Just xVec = getNumeric "x" df
      Just yVec = getNumeric "y" df
      n = V.length xVec
      xMat = LA.fromColumns
               [ LA.konst 1 n
               , LA.fromList (V.toList xVec) ]
      yLA  = LA.fromList (V.toList yVec)
      fit  = fitRegularized (L2 0.1) xMat yLA
      cfg  = defaultReportConfig "Ridge demo"
  renderReport "ridge_lib.html" cfg (toReport cfg df ["x"] "y" fit)
```

The simplest API for library users: just pass **`toReport cfg df xCols yCol fit`** to `renderReport`.

---

## 5. Usage Examples by Model

All models generate the same HTML structure via `--report [FILE]` from the CLI.
From the library, use the `Reportable` instance.

### Ridge / Lasso / Elastic Net

```bash
hanalyze ridge data.csv "x1 x2 x3" y --penalty lasso --lambda 0.05 --report report.html
```

Report structure:
- Data overview (statistics table for 4 columns)
- Model overview (Lasso, formula y ~ x1 + x2 + x3, λ=0.05)
- Coefficient table + R²
- Fit summary (penalty / λ / sparsity / RMSE)
- **Regularization path plot** (λ ∈ [1e-4, 1e2] logarithmic sweep, sparsification process for Lasso)
- Scatter + curve (single variable only)
- Residual plot

From library:

```haskell
import Hanalyze.Model.Regularized (fitRegularized, Penalty (..))
import Hanalyze.Viz.ReportBuilder
import Hanalyze.Viz.ReportInstances ()
let fit = fitRegularized (L1 0.05) xMat yLA
renderReport "out.html" (defaultReportConfig "Lasso") (toReport cfg df xCols "y" fit)
```

### Spline (B-spline / Natural cubic)

```bash
hanalyze spline data.csv x y --type natural --knots 8 --report
```

Report structure: Data / Model / KeyValue (kind, knots, RMSE) / FitScatter (smooth curve with knots) / Residuals.

```haskell
import Hanalyze.Model.Spline
let fit = fitSpline (BSpline 3) [0, 1, 2, 3, 4, 5] xVec yVec
renderReport "spline.html" cfg (toReport cfg df ["x"] "y" fit)
```

### Kernel (Nadaraya-Watson / Kernel Ridge / RFF)

```bash
hanalyze kernel data.csv x y --method kr --bandwidth 0.5 --report
hanalyze kernel data.csv x y --method rff --features 200 --report
```

From library:

```haskell
import qualified Hanalyze.Model.Kernel as K
import qualified Hanalyze.Model.RFF    as R
let krFit  = K.kernelRidge K.Gaussian 0.5 0.01 xVec yVec   -- KernelRidgeFit
gen <- createSystemRandom
feats <- R.sampleRFFRBF 200 0.6 1.0 gen
let rffFit = R.rffRidge feats (V.toList xVec) (V.toList yVec) 0.01  -- RFFRidgeFit

renderReport "kr.html"  cfg (toReport cfg df ["x"] "y" krFit)
renderReport "rff.html" cfg (toReport cfg df ["x"] "y" rffFit)
```

### Quantile / GAM / Random Forest

In addition to being generated from the CLI via `--report`, equivalent reports can be built from the library via `Reportable` instance:

```bash
hanalyze quantile data.csv x y --taus 0.1,0.5,0.9 --report
hanalyze gam      data.csv "x1 x2 x3" y --knots 8 --report
hanalyze rf       data.csv "x1 x2 x3" y --trees 200 --report
```

Sections included in each report (built directly by CLI handlers):

| Subcommand | Special Sections |
|---|---|
| `quantile` | Multiple quantile fits (when `--taus` specified, multiple τ lines overlaid) |
| `gam`      | Partial effect per feature (s_j(x_j) overlaid with partial residuals) |
| `rf`       | Feature importance (bar chart) |

Example of building directly from library:

```haskell
import qualified Hanalyze.Model.Quantile      as Q
import qualified Hanalyze.Model.GAM           as GAM
import qualified Hanalyze.Model.RandomForest  as RF
import qualified Hanalyze.Viz.ReportBuilder   as RB
import qualified Hanalyze.Viz.ReportInstances as RI

-- Quantile (τ = 0.5 for median regression)
let qfit = Q.fitQuantile 0.5 xMat yVec
RB.renderReport "qr.html" cfg (RB.toReport cfg df ["x"] "y" qfit)

-- GAM
let gfit = GAM.fitGAM 3 5 0.01 [xVec1, xVec2] yVec
RB.renderReport "gam.html" cfg (RB.toReport cfg df ["x1","x2"] "y" gfit)

-- Random Forest (pass yHat/yObs separately)
gen <- createSystemRandom
rf <- RF.fitRF RF.defaultRFConfig rows ys gen
let yHat = V.fromList [ RF.predictRF rf row | row <- rows ]
    rep  = RI.RFReport rf yHat (V.fromList ys)
RB.renderReport "rf.html" cfg (RB.toReport cfg df ["x1","x2"] "y" rep)
```

### Robust GP

```haskell
import Hanalyze.Model.GP        (GPParams (..))
import Hanalyze.Model.GPRobust
let hp  = GPParams 0.6 1.0 0.05 1.0
    fit = fitGPRobust RBF hp (RCauchy 0.5) trainX trainY
renderReport "rgp.html" cfg (toReport cfg df ["x"] "y" fit)
```

Report: Data / Model / KeyValue (kernel/likelihood/IRLS iterations). The fit itself shows no scatter/residual display (visualization of raw GP models requires separate handling).

### Taguchi Analysis — `Hanalyze.Viz.Taguchi`

Taguchi analysis has a unique structure (factor effects + SN ratio), so it has a dedicated `Hanalyze.Viz.Taguchi.renderTaguchiReport`:

```haskell
import qualified Hanalyze.Design.Orthogonal as OA
import qualified Hanalyze.Design.Taguchi    as TG
import qualified Hanalyze.Viz.Taguchi       as VTG

let Right ad = OA.assignFactors OA.l9 specs
    sns      = TG.snRatioRows TG.SmallerBetter yMatrix
    fes      = TG.analyzeSN ad sns
    opts     = TG.optimalLevels fes
    tr = VTG.TaguchiReport
           { VTG.trTitle     = "Taguchi: chemical optimization"
           , VTG.trArrayName = OA.oaName (OA.adArray ad)
           , VTG.trSNType    = TG.SmallerBetter
           , VTG.trPerRunSN  = sns
           , VTG.trEffects   = fes
           , VTG.trOptimal   = opts
           , VTG.trPredicted = TG.predictSN fes sns
           }
VTG.renderTaguchiReport "taguchi.html" tr
```

CLI: `hanalyze taguchi analyze L9 -f ... --csv runs.csv --report taguchi.html`.

---

## 6. Using from the CLI

All fit subcommands support the `--report [FILE]` flag:

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

Omitting the argument to `--report` defaults to `<subcommand>.html` (e.g., `ridge.html`). To specify explicitly: `--report path/to/myreport.html`.

**All subcommands operate via the `Hanalyze.Viz.ReportBuilder` route**. The `regress` subcommand also uses `cliRegressSections` / `cliMixedSections` / `cliGPSections` / `cliHBMSections` in `app/Main.hs` via `RB.renderReport`. `Hanalyze.Viz.AnalysisReport` is retained as legacy despite being deprecated.

---

## 7. Creating Custom Reports

To compare multiple models in a custom report or create domain-specific displays, build sections directly:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Hanalyze.Viz.ReportBuilder
import Graphics.Vega.VegaLite (VegaLite, toVegaLite, dataFromColumns,
                                dataColumn, mark, encoding, position,
                                Mark (..), Position (..), PName, PmType,
                                Numbers, Strings, MType (..))

myReport :: IO ()
myReport = do
  let cfg = ReportConfig
              { rcTitle    = "Custom comparison"
              , rcSubtitle = "Ridge vs Lasso vs ElasticNet"
              }
      sections =
        [ secMarkdown "Setup"
            "We compare three regularized regressions on the same dataset."
        , secTable "Hyperparameters"
            ["Model", "λ", "α"]
            [ ["Ridge",      "0.10", "—"]
            , ["Lasso",      "0.05", "—"]
            , ["ElasticNet", "0.10", "0.5"] ]
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
myCustomSpec = ...  -- arbitrary Vega-Lite spec
```

### Combining Existing Reportable Instances

```haskell
let baseSections = toReport cfg df ["x"] "y" myFit  -- default sections
    extra = [ secMarkdown "Note" "Cross-validation results below."
            , secVega "CV trace" cvSpec
            ]
renderReport "out.html" cfg (baseSections ++ extra)
```

Order is preserved as called.

---

## 8. Relationship with Existing `Hanalyze.Viz.AnalysisReport`

`Hanalyze.Viz.AnalysisReport` is **deprecated**. It carries the `{-# DEPRECATED #-}` pragma, and importing it triggers a GHC warning. The future standard is `Hanalyze.Viz.ReportBuilder`.

| Item | `Hanalyze.Viz.AnalysisReport` (deprecated) | `Hanalyze.Viz.ReportBuilder` (★ standard) |
|---|---|---|
| Target | LM / GLM / GLMM / GP / HBM only | All models (RegFit/Spline/Kernel/RFF/RobustGP already have instances + LM/GLM/GLMM/GP/HBM planned for instancing) |
| Design | sum-type `ModelFit` (~2000 lines, tightly coupled) | Section sequences + `Reportable` typeclass (easy to extend) |
| Extension | Add variant to ModelFit + rewrite each section handler | Just write one `Reportable` instance |
| Report Structure | 5 sections fixed (Data / Model / Result / Interactive / Appendix) | Arbitrary section order |
| Interactive Prediction | Built-in | `secInteractiveLM` / `secInteractiveMulti` |
| MCMC Integration | Built-in for HBM | `secMCMCDiagnostics` / `secMCMCAutocorr` / `secMCMCPair` / `secPosteriorSummary` |
| Status | Planned for deletion (retained for CLI `regress --report` compatibility) | Under development (next task is instancing LM/GLM/GLMM/GP/HBM for `Reportable`) |

**Selection Guide**:
- New implementations → Always use `ReportBuilder`
- `regress` CLI also runs through `ReportBuilder` route
- Only viewing HBM MCMC diagnostics alone → `Hanalyze.Viz.Report`

---

## 9. Common Patterns and Pitfalls

### Vega-Lite Spec hvega Idioms

```haskell
import Graphics.Vega.VegaLite

myChart :: VegaLite
myChart = toVegaLite
  [ dataFromColumns []
      . dataColumn "x" (Numbers [1,2,3])
      . dataColumn "y" (Numbers [2,4,6])
      $ []
  , mark Line [MStrokeWidth 2.5]
  , encoding
      . position X [PName "x", PmType Quantitative]
      . position Y [PName "y", PmType Quantitative]
      $ []
  , width  500
  , height 300
  ]
```

Convert to section with `secVega "title" myChart`.

### File Size

Each report is roughly **800-870 KB** (including Vega-Lite and Mermaid assets).
This is the tradeoff for offline operation. Assets are hardcoded in `Hanalyze.Viz.Assets`.

### Number Formatting

`secCoefficients` / `secFitScatter` etc. internally use `printf "%.4f"` equivalent for 4-digit display. Integer values automatically display as integers (`150` not `150.0`). To customize, pass `printf` results directly via `secKeyValue`.

### Mermaid Diagram Rendering

Mermaid is fetched from CDN (only works when online):

```haskell
secMermaid "graph LR\n  A[mu] --> B[theta]\n  B --> C[y]"
```

If offline support is needed, output from `Hanalyze.Viz.ModelGraph` (auto-extracted DAG from HBM) can be formatted as an HTML string and passed to `secHtml`.

### Information Not Handled by Reportable

Per the signature `toReport :: ... -> a -> [ReportSection]`, arguments are only `(ReportConfig, DataFrame, [Text], Text, a)`. To include other information (e.g., external CV results, comparison from another chain), manually add sections to the generated list:

```haskell
let base = toReport cfg df xs y fit
    augmented = base ++ [secVega "External" extSpec]
renderReport path cfg augmented
```

### Models Without `Reportable` Instances

- `Quantile` / `GAM` / `Random Forest` build sections directly inside CLI handlers (no instances). To use from library, reference CLI code and build sections manually.
- Future plan is to add instances (backlog task).

### `Reportable` Instances for LM/GLM/GLMM/GP/HBM

Currently these are dedicated to `Hanalyze.Viz.AnalysisReport` (sum-type based). The comparison demo `AnalysisCompareDemo.hs` shows examples of building sections directly from each model, which can be referenced when instancing `Reportable`.

Example: HBM section pattern:
```haskell
RB.secDataOverview df xCols yCol
RB.secModelOverview "Bayesian Linear Regression (HBM, NUTS)" formula Nothing
RB.secCoefficients [(α posterior mean), (β posterior mean), (σ posterior mean)] Nothing
RB.secPosteriorSummary "Posterior summary" rows  -- mean/SD/quantile/ESS/R-hat
RB.secMCMCDiagnostics "MCMC diagnostics" params chain
RB.secMCMCAutocorr "Autocorrelation" 40 params chain
RB.secMCMCPair "Pair scatter (α, β)" "alpha" "beta" chain
RB.secFitScatter xc yc xs ys (Just credibleBand)
RB.secInteractiveLM "Interactive prediction" xc yc xs ys credibleBand range
```

---

## Related Documentation

- [01-visualization.md](01-visualization.md) — Single plots (bar charts, histograms, scatter plots, Mermaid DAGs, etc.)
- [../doe/02-orthogonal-taguchi.md](../doe/02-orthogonal-taguchi.md) — Orthogonal arrays and Taguchi method (includes Viz.Taguchi)
- [../regression/06-randomforest.md](../regression/06-randomforest.md) — Quantile / GAM / Random Forest
- Existing `Hanalyze.Viz.AnalysisReport` (LM/GLM/GLMM/GP/HBM only) — recommended to read source code for understanding
