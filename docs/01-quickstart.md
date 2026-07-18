# Quick start

> 🌐 **English** | [日本語](01-quickstart.ja.md)

hanalyze is a **general-purpose statistical analysis toolkit** that covers
classical regression, design of experiments, multi-objective optimization,
Bayesian statistics, MCMC, and more.

## Build & run

```bash
cabal build           # library + all executables
cabal test            # test suite
cabal run <demo-name> # individual demo
```

To run a binary directly (when `cabal run` is ambiguous):
```
dist-newstyle/build/x86_64-linux/ghc-9.6.7/hanalyze-0.1.0.1/x/<demo>/build/<demo>/<demo>
```

CPU parallelism (multi-chain MCMC, etc.):
```bash
cabal run hbm-example -- +RTS -N4   # 4 threads
```

## CLI subcommands

`hanalyze` can be invoked via subcommands (the bare form
`hanalyze <file> <xcols> <ycols> ...` is preserved as a legacy alias of
`regress`):

```bash
cabal run hanalyze -- help              # list subcommands
cabal run hanalyze -- info data.csv     # column types and basic statistics
cabal run hanalyze -- hist data.csv col --fit normal 0 1  # histogram + density
cabal run hanalyze -- regress data.csv x y LM --report    # regression (= bare form)
```

| Subcommand | Status | Purpose |
|---|---|---|
| `regress` / bare | ✅ | LM/GLM/GLMM/GP/HBM regression |
| `info` | ✅ | Per-column type / stats (n / min / max / mean / median / sd / unique) |
| `hist` | ✅ | Standalone histogram (`--fit`/`--format`/`--out`) |
| `doe` | ✅ | Orthogonal arrays Lₙ (L4/L8/L9/L12/L16/L18) |
| `taguchi` | ✅ | Taguchi method (SN ratio + factor effects + inner/outer) |
| `ridge` | ✅ | Ridge / Lasso / Elastic Net (+ regularization path) |
| `kernel` | ✅ | Kernel regression (Nadaraya-Watson / Kernel Ridge / RFF) |
| `spline` | ✅ | B-spline / natural cubic |
| `quantile` | ✅ | Quantile regression (τ-quantile, MM-IRLS) |
| `gam` | ✅ | Generalized Additive Model |
| `rf` | ✅ | Random Forest regression |
| `clean` | ✅ | Column-cleaning DSL (StripUnits / ParseCurrency / ParseDecimalEU / TrimText / CoerceNumeric) |
| `melt` | ✅ | wide → long reshape (`--id`/`--vars`/`--var`/`--value`) |
| `multireg` | ✅ | True multi-output regression (1 input → q output curves; linear / kernel-rbf + interactive HTML) |

---

## "What do you want to do?" — pick the demo / CLI

### 1. Classical regression

| Goal | Recommended approach | Example |
|---|---|---|
| Simple linear regression (with CI) | CLI: `LM` | `hanalyze data.csv x y LM --ci 0.95 --report` |
| Logistic / Poisson regression | CLI: `GLM` | `hanalyze data.csv x y GLM -d binomial -l logit --report` |
| Polynomial regression (per-column degree) | CLI: `LM --degree -1 2 -2 3` | Demo: `glmm-demo` (LME variant) |
| Mixed effects (LME / GLMM) | CLI: `--group COL` | Demo: `glmm-demo` |
| Gaussian process regression (nonlinear) | CLI: `GP` / Demo: `gp-demo` | Auto-compares RBF/Matérn/Periodic |

### 2. Nonlinear & regularized regression

| Goal | Recommended approach | Example |
|---|---|---|
| Spline regression (B-spline / Natural cubic) | Demo: `spline-demo` | RMSE 0.05 |
| Kernel regression (Nadaraya-Watson / Kernel Ridge) | Demo: `kernel-demo` | Bandwidth via LOO-CV |
| Ridge / Lasso / Elastic Net | Demo: `regularized-demo` | Variable selection + sparse models |
| Multi-output (MultiLM / RRR / PLS / CCA) | Demo: `multilm-demo` / `multivariate-demo` | Extract shared low-dimensional structure |
| **True multi-output regression (1 input → q outputs)** | CLI: `multireg` | `hanalyze multireg wide.csv x 'y_*' --method kernel-rbf --auto-hp --report out.html` (one input slider recomputes all q outputs live) |

### 3. Design of Experiments and multi-objective optimization

| Goal | Recommended approach | Example |
|---|---|---|
| Full/fractional factorial / Latin square / RCBD | Demo: `doe-demo` | Orthogonality / D-eff / VIF |
| Response surface methodology (RSM) | Demo: `rsm-demo` | CCD/Box-Behnken + quadratic + optimum |
| D-/A-optimal designs (Fedorov) | Demo: `optimaldoe-demo` | Selection from a candidate set |
| ANOVA / power analysis / sample size | Demo: `doe-demo` | Power for t / F / proportion tests |
| Multi-objective optimization (NSGA-II) | Demo: `nsga-demo` | Pareto fronts on ZDT1 / Schaffer |
| Bayesian Optimization (single-/multi-objective) | Demo: `bayesopt-demo` | EI / acquisition maximization |
| Combined (DOE + 3-objective optimization) | Demo: `materials-moo-demo` | Alloy strength / cost / weight |

### 4. Bayesian inference

| Goal | Recommended approach | Example |
|---|---|---|
| Bayesian simple regression (one-shot CLI) | CLI: `HBM --report --waic` | `hanalyze data.csv x y HBM --report` |
| Hierarchical model (group structure) | Demo: `simpson-paradox` / `hbm-random-slope` | For custom hierarchies, write `Hanalyze.Model.HBM` directly |
| Random-slope model | Demo: `hbm-random-slope` | Compare M1 (shared β) vs M2 (β_g) by WAIC |
| Bayesian A/B test | Demo: `clinical-trial` | Beta-Binomial, decision theory |
| Multi-chain NUTS + R-hat diagnostics | Demo: `hbm-example` | 4 chains in parallel, `mcmc_report_multi.html` |

### Sampler choice / performance

| Goal | Recommended approach | Example |
|---|---|---|
| Compare MH/HMC/NUTS | Demo: `bench-mcmc` | Measure ESS/s on easy / hard cases |
| Fast sampling for conjugate models | Demo: `gibbs-hbm-demo` | Auto Gibbs detection, hybrid Gibbs+MH |
| Variational inference (large-scale, fast) | Demo: `vi-demo` | ADVI vs NUTS accuracy |
| Sampler accuracy check | Demo: `test-hmc-nuts` | HMC/NUTS sanity check on 1D Gaussian |

### Model comparison / interpretation

| Goal | Recommended approach | Example |
|---|---|---|
| Model selection via WAIC / LOO-CV | CLI: `--waic` or Demo: `gibbs-demo` | LM/GLM/HBM/LME |
| Side-by-side comparison in one HTML | Demo: `simpson-paradox` | LM/GLMM/HBM coefficients, predictions, WAIC |
| Reproduce Simpson's paradox | Demo: `simpson-paradox` | `simpson_compare.html` |
| Validate hierarchical extension of HBM | Demo: `hbm-random-slope` | Random intercept only vs +random slope |

### Visualization

| Goal | Recommended approach | Example |
|---|---|---|
| HTML report (DAG / MCMC diagnostics / prediction curves) | CLI: `--report` | LM/GLM/GLMM/GP/HBM all supported (legacy `Hanalyze.Viz.AnalysisReport`; successor is `Hanalyze.Viz.ReportBuilder`) |
| Bar chart / histogram alone | Demo: `bar-demo` / CLI: `--hist COL` | PNG/SVG export available |
| MCMC-only report (KDE + trace + DAG) | `Hanalyze.Viz.Report.renderReport` | Demo: `hbm-example` |
| Export plots as PNG/SVG | CLI: `--format png` | Each NamedPlot becomes a separate image |
| Standalone model DAG HTML | `Hanalyze.Viz.ModelGraph.renderModelGraph` | Auto dependency extraction via the Track type |

---

## Minimal complete workflows (Haskell, by task)

### A. Linear regression → plot (3 lines)

Fit with the universal verb `df |-> spec`, then overlay the fit on a scatter
with `toPlot` (see [io/04-fit-api.md](io/04-fit-api.md) for the full `(|->)` API):

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Vector              as V
import           Hanalyze.Plot     (lm, (|->), toPlot)
import           Hgg.Plot.Spec        (ColData (..), layer, scatter)
import           Hgg.Plot.Frame       ((|>>))
import           Hgg.Plot.Backend.SVG (saveSVGBound)

let df  = [ ("x", NumData (V.fromList xs))
          , ("y", NumData (V.fromList ys)) ]   -- any ColumnSource (CSV DataFrame works too)
    fit = df |-> lm "x" "y"                    -- LMModel: β, ŷ, residuals, R²
saveSVGBound "lm.svg"                          -- scatter + OLS line + 95% CI band
  (df |>> (layer (scatter "x" "y") <> toPlot fit))
```

**Lower-level (matrix API)** — when you already hold `hmatrix` vectors and only
need the numeric `FitResult`:

```haskell
import Hanalyze.Model.LM   (fitLMVec, designMatrix)
import Hanalyze.Model.Core (coefficientsV, rSquared1)

let fit  = fitLMVec (designMatrix xs) ys   -- FitResult: β, ŷ, residuals, R²
    beta = coefficientsV fit
```

### B. Bayesian model (`df |-> hbm` + posterior forest)

The same verb fits a hand-written HBM program; the data-frame columns bind to the
model's `dataNamed*` slots and `hbmModelPure` runs (deterministic by the seed in
the config). Posterior figures come from the extractors (`forestOf` / `tracesOf` / …):

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Vector              as V
import           Hanalyze.Model.HBM (ModelP, sample, observe, dataNamedObs, Distribution (..))
import           Hanalyze.Plot      (hbm, defaultHBM, (|->), toPlot, forestOf)
import           Hgg.Plot.Spec         (ColData (..))
import           Hgg.Plot.Frame        ((|>>))
import           Hgg.Plot.Backend.SVG  (saveSVGBound)
import           Data.Text                 (Text)

myModel :: ModelP ()
myModel = do
  mu <- sample      "mu" (Normal 0 10)
  ys <- dataNamedObs "y" []                 -- bound from the df column "y"
  observe "y" (Normal mu 2) ys

main :: IO ()
main = do
  let df   = [ ("y", NumData (V.fromList [1.2, 2.3, 3.1, 2.8, 1.9])) ]
      m    = df |-> hbm defaultHBM myModel  -- HBMModel: pure NUTS, deterministic by cfg seed
      noDf = [] :: [(Text, ColData)]        -- the forest needs no data columns
  saveSVGBound "forest.svg" (noDf |>> toPlot (forestOf m))   -- posterior forest of mu
```

> The pure verb `(|->)` is silent; for a live progress bar during sampling use the
> IO twin `df |->! hbm defaultHBM myModel` (bit-identical result). See
> [io/04-fit-api.md](io/04-fit-api.md).

**Lower-level (explicit sampler + HTML report)** — call the pure NUTS sampler
directly and render the classic MCMC report:

```haskell
import qualified Data.Map.Strict as Map
import Hanalyze.MCMC.NUTS  (nutsPure, defaultNUTSConfig)
import Hanalyze.Viz.Report (defaultReport, renderReport)

-- nutsPure takes a seed (Word32) and returns a Chain purely / deterministically
let chain = nutsPure myModel defaultNUTSConfig (Map.fromList [("mu", 0.0)]) 42
renderReport "report.html" (defaultReport "My Model" chain ["mu"])
```

### C. Design of experiments + ANOVA

```haskell
import Hanalyze.Design.Factorial (twoLevelFactorial)
import Hanalyze.Design.Anova     (oneWayAnova, printAnovaTable)

let design = twoLevelFactorial 3   -- 2³ full factorial = 8 runs
-- After collecting observations ys:
printAnovaTable (oneWayAnova labels ys)
```

### D. Multi-objective optimization (NSGA-II)

```haskell
import Hanalyze.Optim.NSGA (nsga2, defaultNSGAConfig)

let f xs = [head xs ^ 2, (head xs - 2) ^ 2]   -- 2 objectives
front <- nsga2 defaultNSGAConfig f [(0, 2)] gen
-- front :: [Solution] approximates the Pareto set
```

---

## Module quick reference

### Regression / statistical models
| Use case | Module | Key functions |
|---|---|---|
| OLS / polynomial / confidence band | `Hanalyze.Model.LM` | `fitLM`, `fitLMVec`, `fitPolyWithSmooth` |
| **LM inference stats (SE/t/p, F, AIC/BIC, leverage, Cook's)** | `Hanalyze.Model.LM.Diagnostics` | `lmCoefStats`, `lmFStatistic`, `lmInformationCriteria`, `hatDiagonal`, `cooksDistance` |
| GLM (Gaussian/Binomial/Poisson) | `Hanalyze.Model.GLM` | `fitGLM`, `fitGLMWithSmooth` |
| LME / GLMM | `Hanalyze.Model.GLMM` | `fitLME`, `fitLMEDataFrame` |
| Spline (B-spline / Natural) | `Hanalyze.Model.Spline` | `fitSpline`, `fitSplineMulti` |
| Kernel regression / Kernel Ridge | `Hanalyze.Model.Kernel` | `nwRegression`, `kernelRidge` |
| Ridge / Lasso / Elastic Net | `Hanalyze.Model.Regularized` | `fitRegularized` (sum-type penalty) |
| **RFF (Random Fourier Features)** | `Hanalyze.Model.RFF` | `sampleRFFRBF`, `rffRidge`, `rffGP` |
| Multivariate LR / RRR / PLS / CCA | `Hanalyze.Model.MultiLM` / `Hanalyze.Model.Multivariate` | |
| **Multi-output common base** | `Hanalyze.Model.MultiOutput` | `asMultiY`, `fromMultiY`, `r2Multi`, `rmseMulti` |
| Gaussian process / Multi-output GP | `Hanalyze.Model.GP` / `Hanalyze.Model.MultiGP` | `optimizeGP`, `fitGP`, `fitGPMulti` |
| **Robust GP** (StudentT/Cauchy) | `Hanalyze.Model.GPRobust` | `fitGPRobust`, `predictGPRobust` |
| **Quantile regression** (τ-quantile) | `Hanalyze.Model.Quantile` | `fitQuantile`, `predictQuantile` |
| **GAM** (additive B-spline) | `Hanalyze.Model.GAM` | `fitGAM`, `predictGAM`, `predictGAMComponent` |
| **Random Forest** (regression) | `Hanalyze.Model.RandomForest` | `fitRF`, `predictRF`, `featureImportance` |

### Data I/O & preprocessing
| Use case | Module | Key functions |
|---|---|---|
| CSV / TSV / SSV (cassava) | `Hanalyze.DataIO.CSV` | `loadAuto`, `loadCSV`, `loadTSV` (returns Hackage `DataFrame` directly) |
| Defensive CSV loader | `Hanalyze.DataIO.CSV` | `loadAutoSafe`, `loadAutoSafeWith`, `LoadOpts` (--no-header / --skip / --comment / --delim / --strict / --no-sniff) |
| Parquet / JSON | `Hanalyze.DataIO.External` | `loadParquet`, `loadJSON` |
| DataFrame → vector extraction | `Hanalyze.DataIO.Convert` | `getDoubleVec`, `getTextVec`, `getMaybeTextVec` |
| Structured log | `Hanalyze.DataIO.Log` | `LogEntry`, `LogReport`, `printLogReport` |
| Health checks (W001..W008) | `Hanalyze.DataIO.Health` | `inspectDataFrame`, `inspectWithPreview` |
| Auto-sniff (delim / header / skip) | `Hanalyze.DataIO.Sniff` | `sniffBytes`, `sniffFile` |
| Column cleaning DSL | `Hanalyze.DataIO.Clean` | `ColumnRule`, `applyRule`, `cleanPipeline`, `dedupeColumns`, `fillBlankNames` |
| Wide → long reshape | `Hanalyze.DataIO.Preprocess` | `meltLonger`, `hanalyze melt --id ... --vars ...` |
| Multivariate RFF Ridge | `Hanalyze.Model.RFF` | `RFFFeaturesMV`, `rffRidgeMV`, `predictRFFRidgeMV` (CLI: `hanalyze kernel "x1 t" y --method rff`) |
| Input standardization (z-score) | `Hanalyze.Stat.Standardize` | `Standardizer`, `fitStandardizer`, `applyStandardizer`, `unapplyStandardizer` (CLI: `--standardize`) |
| Marginal-likelihood max (RFF/GP HP) | `Hanalyze.Model.RFF` | `logMarginalLikRBFMV`, `maximizeMarginalLikRBFMV` (CLI: `--auto-hp`) |
| Imputation / filter / derived columns | `Hanalyze.DataIO.Preprocess` | `imputeMean`, `imputeMedian`, `dropMissingRows`, `filterRowsByNumeric`, `deriveNumeric`, `mapNumeric` |
| groupBy / aggregate | `Hanalyze.DataIO.Preprocess` | `groupByMean`, `groupBySum`, `groupByMin`, `groupByMax`, `groupByMedian`, `groupByCount`, `groupByAggregate` |

### Design of Experiments
| Use case | Module | Key functions |
|---|---|---|
| Full / fractional factorial / mixed levels | `Hanalyze.Design.Factorial` | `fullFactorial`, `fractionalFactorial` |
| Latin square / RCBD | `Hanalyze.Design.Block` | `latinSquare`, `randomizedBlock` |
| ANOVA (one-way / two-way) | `Hanalyze.Design.Anova` | `oneWayAnova`, `twoWayAnova` |
| Power analysis / sample size | `Hanalyze.Design.Power` | `powerTTest`, `sampleSizeTTest` |
| Orthogonality / D-eff / VIF / **Cp, Cpk** | `Hanalyze.Design.Quality` | `dEfficiency`, `vifList`, `processCapability`, `processCapabilityUpper`, `processCapabilityLower` |
| RSM (CCD / Box-Behnken) | `Hanalyze.Design.RSM` | `centralComposite`, `boxBehnken` |
| D-/A-optimal | `Hanalyze.Design.Optimal` | `dOptimal`, `aOptimal` |
| **Orthogonal arrays Lₙ (L4/L8/L9/L12/L16/L18)** | `Hanalyze.Design.Orthogonal` | `lookupOA`, `assignFactors`, `renderCSV`, `listArraysWithSize` (OAMetadata) |
| **Taguchi method (SN ratio, factor effects, inner/outer)** | `Hanalyze.Design.Taguchi` | `snRatio`, `analyzeSN`, `optimalLevels`, `makeInnerOuter`, `snRatioWithDetails`, `factorEffectsTable` |

### Optimization
| Use case | Module | Key functions |
|---|---|---|
| Adam / gradient ascent / numeric gradient | `Hanalyze.Optim.Adam` / `Hanalyze.Optim.GradAscent` / `Hanalyze.Optim.Numeric` | |
| NSGA-II + Pareto | `Hanalyze.Optim.NSGA` / `Hanalyze.Optim.Pareto` | `nsga2`, `hypervolume` |
| Bayesian Optimization | `Hanalyze.Optim.BayesOpt` / `Hanalyze.Optim.Acquisition` | `bayesOpt`, `ei` |
| Desirability function | `Hanalyze.Optim.Desirability` | `overallDesirability` |

### Bayesian / MCMC
| Use case | Module | Key functions |
|---|---|---|
| Polymorphic DSL (40+ distributions) | `Hanalyze.Model.HBM` | `sample`, `observe`, `ModelP r` |
| HMC / NUTS | `Hanalyze.MCMC.HMC` / `Hanalyze.MCMC.NUTS` | `nutsPure`, `nutsChainsPure` (pure, recommended); `nuts`/`nutsChains` (IO, legacy) |
| Gibbs / MH / Slice | `Hanalyze.MCMC.Gibbs` / `Hanalyze.MCMC.MH` / `Hanalyze.MCMC.Slice` | |
| Variational inference | `Hanalyze.Stat.VI` | `advi` |
| WAIC / LOO / Pseudo-BMA | `Hanalyze.Stat.ModelSelect` | `waic`, `loo`, `compareModels` |

### Visualization
| Use case | Module | Key functions |
|---|---|---|
| MCMC report (KDE / trace / DAG) | `Hanalyze.Viz.Report` / `Hanalyze.Viz.MCMC` | `renderReport`, `tracePlotHDI` |
| Pareto front (5 styles) | `Hanalyze.Viz.Pareto` | `paretoScatter`, `parallelCoordinates` |
| Scatter / bar / histogram | `Hanalyze.Viz.Scatter` / `Hanalyze.Viz.Bar` / `Hanalyze.Viz.Histogram` | |
| Multi-model comparison report | `Hanalyze.Viz.ReportBuilder` (★ standard) / `Hanalyze.Viz.AnalysisReport` (deprecated) | `renderReport` / `writeAnalysisReport` |
| Interactive prediction (1 input → q outputs) | `Hanalyze.Viz.ReportBuilder` | `secInteractiveMultiOut`, `mkInteractiveMOLinear`, `mkInteractiveMOKernelRBF` |

> **Multi-output unification**: All major models (Regularized / Spline / Kernel / RFF / GP / GPRobust / GLM / GLMM / HBM) follow a unified policy where **multi-output (Y :: Matrix n×q) is the primary API and single-output is a thin wrapper that lifts to a 1-column matrix**. Each module exposes a `fitXMulti` / `XFitMulti` family, callable from both Reportable and CLI layers. See [regression/07-multireg.md](regression/07-multireg.md).
