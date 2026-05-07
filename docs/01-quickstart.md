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
dist-newstyle/build/x86_64-linux/ghc-9.6.7/hanalyze-0.1.0.0/x/<demo>/build/<demo>/<demo>
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
| Hierarchical model (group structure) | Demo: `simpson-paradox` / `hbm-random-slope` | For custom hierarchies, write `Model.HBM` directly |
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
| HTML report (DAG / MCMC diagnostics / prediction curves) | CLI: `--report` | LM/GLM/GLMM/GP/HBM all supported (legacy `Viz.AnalysisReport`; successor is `Viz.ReportBuilder`) |
| Bar chart / histogram alone | Demo: `bar-demo` / CLI: `--hist COL` | PNG/SVG export available |
| MCMC-only report (KDE + trace + DAG) | `Viz.Report.renderReport` | Demo: `hbm-example` |
| Export plots as PNG/SVG | CLI: `--format png` | Each NamedPlot becomes a separate image |
| Standalone model DAG HTML | `Viz.ModelGraph.renderModelGraph` | Auto dependency extraction via the Track type |

---

## Minimal complete workflows (Haskell, by task)

### A. Linear regression (3 lines)

```haskell
import qualified Numeric.LinearAlgebra as LA
import Model.LM (fitLMVec, designMatrix)

let dm    = designMatrix xs
    fit   = fitLMVec dm ys     -- β, ŷ, residuals, R²
    beta  = coefficientsV fit
```

### B. Bayesian hierarchical model (NUTS + HTML report)

```haskell
import qualified Data.Map.Strict as Map
import Model.HBM
import MCMC.NUTS  (nuts, defaultNUTSConfig)
import Viz.Report (defaultReport, renderReport)

myModel :: ModelP ()
myModel = do
  mu <- sample "mu" (Normal 0 10)
  observe "y" (Normal mu 2) [1.2, 2.3, 3.1, 2.8, 1.9]

main :: IO ()
main = do
  gen <- createSystemRandom
  chain <- nuts myModel defaultNUTSConfig
                (Map.fromList [("mu", 0.0)]) gen
  renderReport "report.html"
               (defaultReport "My Model" chain ["mu"])
```

### C. Design of experiments + ANOVA

```haskell
import Design.Factorial (twoLevelFactorial)
import Design.Anova     (oneWayAnova, printAnovaTable)

let design = twoLevelFactorial 3   -- 2³ full factorial = 8 runs
-- After collecting observations ys:
printAnovaTable (oneWayAnova labels ys)
```

### D. Multi-objective optimization (NSGA-II)

```haskell
import Optim.NSGA (nsga2, defaultNSGAConfig)

let f xs = [head xs ^ 2, (head xs - 2) ^ 2]   -- 2 objectives
front <- nsga2 defaultNSGAConfig f [(0, 2)] gen
-- front :: [Solution] approximates the Pareto set
```

---

## Module quick reference

### Regression / statistical models
| Use case | Module | Key functions |
|---|---|---|
| OLS / polynomial / confidence band | `Model.LM` | `fitLM`, `fitLMVec`, `fitPolyWithSmooth` |
| GLM (Gaussian/Binomial/Poisson) | `Model.GLM` | `fitGLM`, `fitGLMWithSmooth` |
| LME / GLMM | `Model.GLMM` | `fitLME`, `fitLMEDataFrame` |
| Spline (B-spline / Natural) | `Model.Spline` | `fitSpline`, `fitSplineMulti` |
| Kernel regression / Kernel Ridge | `Model.Kernel` | `nwRegression`, `kernelRidge` |
| Ridge / Lasso / Elastic Net | `Model.Regularized` | `fitRegularized` (sum-type penalty) |
| **RFF (Random Fourier Features)** | `Model.RFF` | `sampleRFFRBF`, `rffRidge`, `rffGP` |
| Multivariate LR / RRR / PLS / CCA | `Model.MultiLM` / `Model.Multivariate` | |
| **Multi-output common base** | `Model.MultiOutput` | `asMultiY`, `fromMultiY`, `r2Multi`, `rmseMulti` |
| Gaussian process / Multi-output GP | `Model.GP` / `Model.MultiGP` | `optimizeGP`, `fitGP`, `fitGPMulti` |
| **Robust GP** (StudentT/Cauchy) | `Model.GPRobust` | `fitGPRobust`, `predictGPRobust` |
| **Quantile regression** (τ-quantile) | `Model.Quantile` | `fitQuantile`, `predictQuantile` |
| **GAM** (additive B-spline) | `Model.GAM` | `fitGAM`, `predictGAM`, `predictGAMComponent` |
| **Random Forest** (regression) | `Model.RandomForest` | `fitRF`, `predictRF`, `featureImportance` |

### Data I/O & preprocessing
| Use case | Module | Key functions |
|---|---|---|
| CSV / TSV / SSV (cassava) | `DataIO.CSV` | `loadAuto`, `loadCSV`, `loadTSV` (returns Hackage `DataFrame` directly) |
| Defensive CSV loader | `DataIO.CSV` | `loadAutoSafe`, `loadAutoSafeWith`, `LoadOpts` (--no-header / --skip / --comment / --delim / --strict / --no-sniff) |
| Parquet / JSON | `DataIO.External` | `loadParquet`, `loadJSON` |
| DataFrame → vector extraction | `DataIO.Convert` | `getDoubleVec`, `getTextVec`, `getMaybeTextVec` |
| Structured log | `DataIO.Log` | `LogEntry`, `LogReport`, `printLogReport` |
| Health checks (W001..W008) | `DataIO.Health` | `inspectDataFrame`, `inspectWithPreview` |
| Auto-sniff (delim / header / skip) | `DataIO.Sniff` | `sniffBytes`, `sniffFile` |
| Column cleaning DSL | `DataIO.Clean` | `ColumnRule`, `applyRule`, `cleanPipeline`, `dedupeColumns`, `fillBlankNames` |
| Wide → long reshape | `DataIO.Preprocess` | `meltLonger`, `hanalyze melt --id ... --vars ...` |
| Multivariate RFF Ridge | `Model.RFF` | `RFFFeaturesMV`, `rffRidgeMV`, `predictRFFRidgeMV` (CLI: `hanalyze kernel "x1 t" y --method rff`) |
| Input standardization (z-score) | `Stat.Standardize` | `Standardizer`, `fitStandardizer`, `applyStandardizer`, `unapplyStandardizer` (CLI: `--standardize`) |
| Marginal-likelihood max (RFF/GP HP) | `Model.RFF` | `logMarginalLikRBFMV`, `maximizeMarginalLikRBFMV` (CLI: `--auto-hp`) |
| Imputation / filter / derived columns | `DataIO.Preprocess` | `imputeMean`, `imputeMedian`, `dropMissingRows`, `filterRowsByNumeric`, `deriveNumeric`, `mapNumeric` |
| groupBy / aggregate | `DataIO.Preprocess` | `groupByMean`, `groupBySum`, `groupByMin`, `groupByMax`, `groupByMedian`, `groupByCount`, `groupByAggregate` |

### Design of Experiments
| Use case | Module | Key functions |
|---|---|---|
| Full / fractional factorial / mixed levels | `Design.Factorial` | `fullFactorial`, `fractionalFactorial` |
| Latin square / RCBD | `Design.Block` | `latinSquare`, `randomizedBlock` |
| ANOVA (one-way / two-way) | `Design.Anova` | `oneWayAnova`, `twoWayAnova` |
| Power analysis / sample size | `Design.Power` | `powerTTest`, `sampleSizeTTest` |
| Orthogonality / D-eff / VIF | `Design.Quality` | `dEfficiency`, `vifList` |
| RSM (CCD / Box-Behnken) | `Design.RSM` | `centralComposite`, `boxBehnken` |
| D-/A-optimal | `Design.Optimal` | `dOptimal`, `aOptimal` |
| **Orthogonal arrays Lₙ (L4/L8/L9/L12/L16/L18)** | `Design.Orthogonal` | `lookupOA`, `assignFactors`, `renderCSV` |
| **Taguchi method (SN ratio, factor effects, inner/outer)** | `Design.Taguchi` | `snRatio`, `analyzeSN`, `optimalLevels`, `makeInnerOuter` |

### Optimization
| Use case | Module | Key functions |
|---|---|---|
| Adam / gradient ascent / numeric gradient | `Optim.Adam` / `Optim.GradAscent` / `Optim.Numeric` | |
| NSGA-II + Pareto | `Optim.NSGA` / `Optim.Pareto` | `nsga2`, `hypervolume` |
| Bayesian Optimization | `Optim.BayesOpt` / `Optim.Acquisition` | `bayesOpt`, `ei` |
| Desirability function | `Optim.Desirability` | `overallDesirability` |

### Bayesian / MCMC
| Use case | Module | Key functions |
|---|---|---|
| Polymorphic DSL (27 distributions) | `Model.HBM` | `sample`, `observe`, `ModelP r` |
| HMC / NUTS | `MCMC.HMC` / `MCMC.NUTS` | `hmc`, `nuts`, `nutsChains` |
| Gibbs / MH / Slice | `MCMC.Gibbs` / `MCMC.MH` / `MCMC.Slice` | |
| Variational inference | `Stat.VI` | `advi` |
| WAIC / LOO / Pseudo-BMA | `Stat.ModelSelect` | `waic`, `loo`, `compareModels` |

### Visualization
| Use case | Module | Key functions |
|---|---|---|
| MCMC report (KDE / trace / DAG) | `Viz.Report` / `Viz.MCMC` | `renderReport`, `tracePlotHDI` |
| Pareto front (5 styles) | `Viz.Pareto` | `paretoScatter`, `parallelCoordinates` |
| Scatter / bar / histogram | `Viz.Scatter` / `Viz.Bar` / `Viz.Histogram` | |
| Multi-model comparison report | `Viz.ReportBuilder` (★ standard) / `Viz.AnalysisReport` (deprecated) | `renderReport` / `writeAnalysisReport` |
| Interactive prediction (1 input → q outputs) | `Viz.ReportBuilder` | `secInteractiveMultiOut`, `mkInteractiveMOLinear`, `mkInteractiveMOKernelRBF` |

> **Multi-output unification**: All major models (Regularized / Spline / Kernel / RFF / GP / GPRobust / GLM / GLMM / HBM) follow a unified policy where **multi-output (Y :: Matrix n×q) is the primary API and single-output is a thin wrapper that lifts to a 1-column matrix**. Each module exposes a `fitXMulti` / `XFitMulti` family, callable from both Reportable and CLI layers. See [regression/07-multireg.md](regression/07-multireg.md).
