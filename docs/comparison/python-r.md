# hanalyze feature map (Python / R reference)

> 🌐 **English** | [日本語](python-r.ja.md)

> Where hanalyze sits as a statistical-analysis library, listed against
> equivalent **Python** (numpy / scipy / sklearn / statsmodels / pymoo /
> skopt / lifelines / PyMC) and **R** (base + tidyverse + caret +
> survival + forecast + lme4) modules.

> **No numerical comparison against R has been run.** The R column
> simply names a representative package that provides the same feature.
> Measurement happens against Python only — see
> [bench/results/SUMMARY.md](../../bench/results/SUMMARY.md).

## 1. Feature map

Legend: ✅ benched against Python / 🟡 functionality present, no bench /
independent — few comparable packages on the Python or R side.

| Area | hanalyze | Python | R | Bench |
|---|---|---|---|---|
| Classical regression (LM / Ridge / Lasso) | `Hanalyze.Model.LM` / `Hanalyze.Model.Regularized` | sklearn / statsmodels | base R / glmnet | ✅ |
| GLM (Binomial / Poisson) | `Hanalyze.Model.GLM` | statsmodels | base R `glm()` | ✅ |
| Mixed effects (LME / GLMM) | `Hanalyze.Model.GLMM` | statsmodels MixedLM | lme4 | ✅ |
| Kernel methods / GP | `Hanalyze.Model.{Kernel,GP,RFF}` | sklearn KernelRidge / GP | kernlab / GPML | ✅ |
| Single-objective optim | `Hanalyze.Optim.{NM,LBFGS,DE,CMAES,SA,PSO}` | scipy.optimize | optimx | ✅ |
| Multi-objective optim | `Hanalyze.Optim.NSGA` | pymoo | mco / emoa | ✅ |
| Bayesian optim | `Hanalyze.Optim.BayesOpt` | scikit-optimize / GPyOpt | mlrMBO | ✅ |
| Bayesian MCMC | `Hanalyze.MCMC.{MH,HMC,NUTS,Gibbs}` | PyMC / NumPyro | rstan / brms | ✅ |
| HBM (probabilistic programming) | `Hanalyze.Model.HBM` polymorphic DSL | PyMC / Pyro / Stan | rstan / brms | 🟡 (feature parity in [PyMC comparison](../02-pymc-comparison.md)) |
| Variational inference (ADVI) | `Hanalyze.Stat.VI` | PyMC / Pyro | rstan ADVI | 🟡 |
| Model comparison (WAIC / LOO) | `Hanalyze.Stat.ModelSelect` | ArviZ | loo (R) | ✅ (`mcmc_extras` suite) |
| Design of experiments (DOE) | `Hanalyze.Design.*` | pyDOE / pyDOE2 | DoE.base | 🟡 |
| Orthogonal arrays + Taguchi | `Hanalyze.Design.{Orthogonal,Taguchi}` | (limited) | qualityTools / DoE.base | 🟡 |
| Hypothesis tests | `Hanalyze.Stat.Test` | scipy.stats | base R / rstatix | ✅ (`stat_util` suite) |
| Multiple-testing correction | `Hanalyze.Stat.MultipleTesting` | statsmodels | `p.adjust` | ✅ |
| Bootstrap CI / permutation | `Hanalyze.Stat.Bootstrap` | scipy.stats / arch | boot | ✅ |
| Effect size + power analysis | `Hanalyze.Stat.Effect` | statsmodels.stats.power | pwr / effsize | 🟡 |
| PCA / dimensionality reduction | `Hanalyze.Model.PCA` | sklearn.decomposition | prcomp | ✅ (`ml` suite) |
| Clustering (K-means) | `Hanalyze.Model.Cluster` | sklearn.cluster | cluster / mclust | ✅ |
| Decision tree (CART) | `Hanalyze.Model.DecisionTree` | sklearn.tree | rpart / tree | ✅ |
| Time series (ARIMA / Holt-Winters) | `Hanalyze.Model.TimeSeries` | statsmodels.tsa | forecast / fable | ✅ (`survts` / `ts_extras` suite) |
| Survival analysis (KM / Cox) | `Hanalyze.Model.Survival` | lifelines | survival | ✅ |
| Classification metrics (AUC / F1 / Brier) | `Hanalyze.Stat.ClassMetrics` | sklearn.metrics | pROC / mlr3measures | 🟡 |
| Cross-validation | `Hanalyze.Stat.CV` | sklearn.model_selection | caret / rsample | 🟡 |
| Interpretation (permutation imp / PDP / ICE) | `Hanalyze.Stat.Interpret` | sklearn.inspection / shap / pdpbox | iml | 🟡 |
| DataFrame manipulation | Hackage `dataframe` + `Hanalyze.DataIO.Reshape` | pandas | tidyverse | 🟡 |
| Visualization | `Hanalyze.Viz.*` (Vega-Lite) | matplotlib / seaborn / plotly | ggplot2 | (qualitative only) |
| CSV / Parquet / JSON I/O | `Hanalyze.DataIO.{CSV,External}` | pandas / pyarrow | readr / arrow | 🟡 |
| Dirty-data defenses | `Hanalyze.DataIO.{Health,Sniff,Clean}` | (DIY) | janitor (partial) | independent |

## 2. Numerical benchmarks

All measured numbers live in a single file:

- [`bench/results/SUMMARY.md`](../../bench/results/SUMMARY.md) — per-suite
  time / accuracy, side-by-side with Python, with reference truth values
- [`bench/README.md`](../../bench/README.md) — measurement conditions
  (single-thread, fixed seed) and how to reproduce

## 3. Patterns observed in the bench

A short summary of trends visible in `SUMMARY.md`. Numbers belong there.

### Areas where speed tends to be on par or better

- Classical regression (LM / Ridge / LME): hmatrix BLAS path
- Single / multi-objective optimization, Bayesian optimization:
  GHC-compiled core plus a polish step that helps accuracy
- Bayesian NUTS: with mass-matrix adaptation, ESS is comparable to blackjax

### Areas where speed tends to lag

- Large GLM-IRLS / Lasso / Kernel ridge / GP fit (n ≥ 10⁴)
- Reason: sklearn / statsmodels run **Cython / SIMD inner loops**, while
  hmatrix necessarily routes through BLAS dispatch — there is a
  structural overhead that pure Haskell + hmatrix cannot remove
- See [`bench/results/OPEN_ISSUES.md`](../../bench/results/OPEN_ISSUES.md)
  for what would require FFI or hand-written SIMD

## 4. Notes per area

### Strengths

- Type safety — matrix-dimension and dtype mismatches caught at compile time
- HBM polymorphic DSL — one model definition admits 4 interpretations
  (structural inspection, log joint, AD gradient, dependency extraction)
  via Free monad
- Dirty-data defenses (`Hanalyze.DataIO.{Health,Sniff,Clean}`) — explicit
  packages for this in pandas / R are limited
- Single binary — no Python interpreter, ships as a CLI

### Weaknesses

- Cython / Fortran-level SIMD inner loops are not reachable through hmatrix
  (large element-wise ops show the gap)
- Parallelism can backfire due to BLAS lock contention
- Ecosystem size — sklearn extensions and R Bioconductor have wider
  domain coverage
- Haskell type-system learning curve

## 5. Choosing a tool

| Use case | Recommendation |
|---|---|
| Research / experimentation / mid-scale (n ≤ 10⁴) | hanalyze |
| Large-scale ML production (n ≥ 10⁵, throughput-bound) | sklearn / PyTorch |
| Bayesian hierarchical modelling: prototype → interpret | hanalyze (HBM DSL) or PyMC |
| R-ecosystem-specific domains (ecology, econometrics) | R |

## See also

- Numerical benchmarks: [`bench/results/SUMMARY.md`](../../bench/results/SUMMARY.md)
- Bench reproduction: [`bench/README.md`](../../bench/README.md)
- PyMC feature parity: [`docs/02-pymc-comparison.md`](../02-pymc-comparison.md)
- Per-feature usage: [`docs/`](../) chapters
