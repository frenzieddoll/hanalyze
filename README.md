# hanalyze

> 🌐 **English** | [日本語](README.ja.md)

[![License: BSD-3](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](LICENSE)
[![GHC](https://img.shields.io/badge/GHC-9.6.7-blueviolet.svg)](https://www.haskell.org/ghc/)

**Type-safe general-purpose statistical analysis toolkit (Haskell)** — covers classical regression, machine learning, Bayesian MCMC, multi-objective optimisation, design of experiments, and visualisation in one library.
**All algorithms implemented natively in Haskell** (no R/Stan/Python bridge).
On accuracy, hanalyze matches or exceeds Python/R in many domains; on speed, optimisation routines are 10-100× faster, machine-learning routines 1.4-5× slower than sklearn.

---

## Highlights

- **Type-safe**: dimension and dtype mismatches caught at compile time; safe to refactor
- **Pure-Haskell algorithms**: every routine implemented from scratch, no foreign bridge
- **Integrated reporting**: one call produces HTML/PNG/SVG with MathJax, Mermaid, and interactive widgets
- **Dirty-data defence**: 8 warning codes + auto-sniff (delim/header/encoding) + cleaning DSL
- **First-class Hackage `dataframe`**: Polars-like DataFrame used directly; Parquet/JSON native

---

## Capabilities

Features grouped by category. Each capability links to a usage doc and (where relevant) a theory doc.

### Statistical inference (`Stat.*`)

| Feature | Module | Usage | Theory |
|---|---|---|---|
| 12 hypothesis tests (t/χ²/ANOVA/Wilcoxon/KS/Shapiro/Levene/Bartlett/...) | `Stat.Test` | [stat/01-test.md](docs/stat/01-test.md) | — |
| Multiple-testing correction (Bonferroni/Holm/BH/BY) | `Stat.MultipleTesting` | [stat/06-multipletesting.md](docs/stat/06-multipletesting.md) | — |
| Bootstrap CI / permutation tests | `Stat.Bootstrap` | [stat/07-bootstrap.md](docs/stat/07-bootstrap.md) | — |
| Effect size + power analysis (Cohen's d/η²/Cramér V/n estimation) | `Stat.Effect` | [stat/09-effect.md](docs/stat/09-effect.md) | — |
| Cross-validation (k-fold/stratified/LOO) + Grid search | `Stat.CV` | [stat/04-cv.md](docs/stat/04-cv.md) | — |

### Regression (`Model.*`)

| Feature | Module | Usage | Theory |
|---|---|---|---|
| Linear regression (LM) | `Model.LM` | [regression/01-lm.md](docs/regression/01-lm.md) | [principles/lm.md](docs/principles/lm.md) |
| GLM (Binomial / Poisson / Gaussian) | `Model.GLM` | [regression/02-glm.md](docs/regression/02-glm.md) | [principles/glm.md](docs/principles/glm.md) |
| GLMM / mixed-effects model (LME) | `Model.GLMM` | [regression/03-glmm.md](docs/regression/03-glmm.md) | [principles/glmm.md](docs/principles/glmm.md) |
| Spline regression (B-spline / NaturalCubic) | `Model.Spline` | [regression/04-spline.md](docs/regression/04-spline.md) | [regression/theory-regression-extensions.md](docs/regression/theory-regression-extensions.md) |
| Kernel regression (NW / Kernel Ridge) + multi-D inputs | `Model.Kernel` | [regression/04-kernel.md](docs/regression/04-kernel.md) | same |
| Regularised (Ridge / Lasso / ElasticNet) | `Model.Regularized` | [regression/04-regularized.md](docs/regression/04-regularized.md) | same |
| Gaussian process (RBF / Matérn / Periodic + ARD + multi-input) | `Model.GP` | [regression/04-gp.md](docs/regression/04-gp.md) | [principles/gp.md](docs/principles/gp.md) |
| Random Fourier Features (large-scale GP approximation) | `Model.RFF` | [regression/04-rff.md](docs/regression/04-rff.md) | [regression/theory-regression-extensions.md](docs/regression/theory-regression-extensions.md) |
| Multivariate regression / Multi-output GP | `Model.{Multivariate,MultiGP,MultiOutput}` | [regression/05-multivariate.md](docs/regression/05-multivariate.md) | [regression/theory-multivariate.md](docs/regression/theory-multivariate.md) |
| Quantile regression | `Model.Quantile` | [regression/06-quantile.md](docs/regression/06-quantile.md) | [regression/theory-regression-extensions.md](docs/regression/theory-regression-extensions.md) |
| Generalized additive model (GAM) | `Model.GAM` | [regression/06-gam.md](docs/regression/06-gam.md) | same |
| Random forest (regression) | `Model.RandomForest` | [regression/06-randomforest.md](docs/regression/06-randomforest.md) | same |
| Multi-output regression + interactive HTML | `Model.MultiOutput` | [regression/07-multireg.md](docs/regression/07-multireg.md) | [regression/theory-multivariate.md](docs/regression/theory-multivariate.md) |

### Machine learning (`Model.*` / `Stat.*`)

| Feature | Module | Usage | Theory |
|---|---|---|---|
| PCA + cumulative variance + standardisation | `Model.PCA` | [stat/02-pca.md](docs/stat/02-pca.md) | — |
| Clustering (K-means + k-means++ + silhouette) | `Model.Cluster` | [stat/05-cluster.md](docs/stat/05-cluster.md) | — |
| Decision tree (CART classifier) | `Model.DecisionTree` | [regression/08-decisiontree.md](docs/regression/08-decisiontree.md) | — |
| Time series (ARIMA / Holt-Winters / STL / ACF / PACF) | `Model.TimeSeries` | [regression/09-timeseries.md](docs/regression/09-timeseries.md) | — |
| Survival analysis (Kaplan-Meier / Nelson-Aalen / Log-rank / Cox PH) | `Model.Survival` | [regression/10-survival.md](docs/regression/10-survival.md) | — |
| Classification metrics (Confusion / AUC / F1 / MCC / log-loss / Brier) | `Stat.ClassMetrics` | [stat/03-classmetrics.md](docs/stat/03-classmetrics.md) | — |
| Model interpretation (Permutation imp / PDP / ICE) | `Stat.Interpret` | [stat/13-interpret.md](docs/stat/13-interpret.md) | — |

### Bayesian (`MCMC.*` / `Stat.*` / `Model.HBM`)

| Feature | Module | Usage | Theory |
|---|---|---|---|
| 27 probability distributions (Truncated/Censored/MvNormal/LKJ/Multinomial/...) | `Stat.Distribution` | [bayesian/01-distributions.md](docs/bayesian/01-distributions.md) | [bayesian/theory-distributions.md](docs/bayesian/theory-distributions.md) |
| Probabilistic model DSL (HBM polymorphic free monad) | `Model.HBM` | [bayesian/02-probabilistic-model.md](docs/bayesian/02-probabilistic-model.md) | [principles/hbm.md](docs/principles/hbm.md) |
| MCMC samplers (MH / HMC / NUTS / Slice) | `MCMC.{MH,HMC,NUTS,Slice}` | [bayesian/03-mcmc-samplers.md](docs/bayesian/03-mcmc-samplers.md) | [bayesian/theory-mcmc.md](docs/bayesian/theory-mcmc.md) / [theory-hmc-nuts.md](docs/bayesian/theory-hmc-nuts.md) |
| Gibbs sampling (auto-conjugate detection + hybrid) | `MCMC.Gibbs` | [bayesian/04-gibbs.md](docs/bayesian/04-gibbs.md) | [bayesian/theory-mcmc.md](docs/bayesian/theory-mcmc.md) |
| Variational inference (ADVI mean-field Adam) | `Stat.VI` | [bayesian/05-vi.md](docs/bayesian/05-vi.md) | [bayesian/theory-advanced.md](docs/bayesian/theory-advanced.md) |
| Model comparison (WAIC / PSIS-LOO / Pseudo-BMA) | `Stat.ModelSelect` | [bayesian/06-model-comparison.md](docs/bayesian/06-model-comparison.md) | [bayesian/theory-bayesian-basics.md](docs/bayesian/theory-bayesian-basics.md) |
| Posterior predictive checks / PyMC compatibility | `Stat.PosteriorPredictive` | [02-pymc-comparison.md](docs/02-pymc-comparison.md) | — |

### Optimisation (`Optim.*`)

| Feature | Module | Usage | Theory |
|---|---|---|---|
| Single-objective: NM / L-BFGS / Brent / DE / CMA-ES / SA / PSO | `Optim.{NelderMead,LBFGS,LineSearch,DifferentialEvolution,CMAES,SimulatedAnnealing,ParticleSwarm}` | [optim/01-singleobj.md](docs/optim/01-singleobj.md) | [optim/theory-singleobj.md](docs/optim/theory-singleobj.md) |
| Multi-objective (NSGA-II + Pareto) | `Optim.{NSGA,Pareto}` | [optim/02-multi-objective.md](docs/optim/02-multi-objective.md) | [optim/theory-pareto-moo.md](docs/optim/theory-pareto-moo.md) |
| Acquisition functions (EHVI / ParEGO / EI / LCB / PI) | `Optim.Acquisition` | [optim/02-multi-objective.md](docs/optim/02-multi-objective.md) | [optim/theory-bayesopt.md](docs/optim/theory-bayesopt.md) |
| Bayesian optimisation (BO + GP-Hedge + analytic gradient) | `Optim.BayesOpt` | [optim/01-singleobj.md](docs/optim/01-singleobj.md) | [optim/theory-bayesopt.md](docs/optim/theory-bayesopt.md) |
| Algorithm selection guide | — | [optim/03-algorithm-guide.md](docs/optim/03-algorithm-guide.md) | — |

### Design of experiments (`Design.*`)

| Feature | Module | Usage | Theory |
|---|---|---|---|
| DoE (Factorial / Block / Mixed / RSM / Optimal / Power / Quality) | `Design.{Factorial,Block,Mixed,RSM,Optimal,Power,Quality,MultiRSM,Anova}` | [doe/01-doe.md](docs/doe/01-doe.md) | [doe/theory-doe.md](docs/doe/theory-doe.md) |
| Orthogonal arrays (L4/L8/L9/L12/L16/L18) + Taguchi (S/N + inner/outer) | `Design.{Orthogonal,Taguchi}` | [doe/02-orthogonal-taguchi.md](docs/doe/02-orthogonal-taguchi.md) | [doe/theory-doe.md](docs/doe/theory-doe.md) |

### Visualisation (`Viz.*`)

| Feature | Module | Usage |
|---|---|---|
| Scatter / bar / histograms / MCMC diagnostics / GP plot / Pareto plot | `Viz.{Scatter,Bar,Histogram,MCMC,GP,Pareto,ModelGraph,Taguchi}` | [visualization/01-visualization.md](docs/visualization/01-visualization.md) |
| Integrated HTML report (MathJax + Mermaid + interactive) | `Viz.ReportBuilder` | [visualization/02-report-builder.md](docs/visualization/02-report-builder.md) |

### Data I/O (`DataIO.*`)

| Feature | Module | Usage |
|---|---|---|
| CSV/TSV/SSV (cassava) + Parquet/JSON (Hackage `dataframe`) | `DataIO.{CSV,External,Convert}` | [io/01-dirty-data.md](docs/io/01-dirty-data.md) |
| Dirty-data defence (W001-W008 warnings + auto-sniff + clean DSL) | `DataIO.{Health,Sniff,Clean,Log}` | [io/01-dirty-data.md](docs/io/01-dirty-data.md) |
| Reshape (pivot_wider / one-hot / lag-lead / rolling window) | `DataIO.Reshape` | [io/02-reshape.md](docs/io/02-reshape.md) |
| Preprocessing (impute / groupBy / derived columns / melt) | `DataIO.Preprocess` | [io/01-dirty-data.md](docs/io/01-dirty-data.md) |
| Long-form regrid (`regridLong`) | `DataIO.Preprocess` + `Stat.Interpolate` | [io/03-regrid.md](docs/io/03-regrid.md) |

---

## Quick start

```bash
# 1. Clone + build
git clone https://github.com/frenzieddoll/hanalyze
cd hanalyze
cabal build all                              # library + all executables
cabal test                                   # 238 examples

# 2. Use the CLI
hanalyze regress data.csv x y --report report.html
hanalyze info data.csv
hanalyze hist data.csv x --fit Normal
```

As a library:

```haskell
import qualified Stat.Test as ST
import qualified Numeric.LinearAlgebra as LA

main = do
  let xs = LA.fromList [12, 14, 13, 15, 17, 11]
      ys = LA.fromList [18, 22, 20, 19, 25, 17]
      result = ST.tTestWelch xs ys ST.TwoSided
  print (ST.trPValue result, ST.trEffect result)
  -- (0.012, Just ("Cohen's d", -1.85))
```

See [docs/01-quickstart.md](docs/01-quickstart.md) for a fuller introduction.

---

## CLI

```
hanalyze help                     list subcommands
hanalyze regress <file> <x> <y>   LM/GLM/GP/HBM regression + HTML report
hanalyze info <file>              per-column type/statistics
hanalyze hist <file> <col>        histogram with theoretical PDF overlay
hanalyze ridge <file> ...         regularised regression (Ridge/Lasso/EN)
hanalyze kernel <file> ...        kernel regression (NW/KR/RFF), multi-D inputs
hanalyze spline <file> ...        spline regression
hanalyze multireg <file> ...      multi-output regression + interactive HTML
hanalyze melt <file> ...          long-form transform
hanalyze regrid <file> ...        time-axis grid alignment
hanalyze doe ortho <NAME> -f ...  orthogonal-array generation
hanalyze taguchi sn / analyze     Taguchi method
hanalyze clean <file> --rule ...  dirty-data cleaning
```

For per-command flags, run `hanalyze <cmd> --help` or see [docs/01-quickstart.md](docs/01-quickstart.md).

---

## Examples / demos

`demo/` contains 60+ demos. Highlights:

| Demo | Summary |
|---|---|
| `demo/regression/HBMRegressionDemo.hs` | HBM Bayesian linear regression with NUTS + HTML |
| `demo/regression/RFFDemo.hs` | Large-scale GP via Random Fourier Features |
| `demo/regression/RobustGPDemo.hs` | Robust GP with Student-t observation likelihood |
| `demo/doe-optim/NSGADemo.hs` | NSGA-II + Pareto on the ZDT suite |
| `demo/doe-optim/BayesOptDemo.hs` | BO on Branin / Hartmann6 |
| `demo/bayesian/HBMComparisonDemo.hs` | Compare HBMs with WAIC / LOO |
| `demo/bayesian/SimpsonParadoxDemo.hs` | Disentangle Simpson's paradox via hierarchical model |
| `demo/io/DirtyDataDemo.hs` | Auto-defend against 19 dirty CSV variants |

Run: `dist-newstyle/build/x86_64-linux/ghc-9.6.7/hanalyze-0.1.0.0/x/<demo-name>/build/<demo-name>/<demo-name>`.

---

## Comparison vs Python / R

Summary of benchmarks (full details in [docs/comparison/python-r.md](docs/comparison/python-r.md)):

| Domain | hanalyze verdict | Speed | Accuracy |
|---|---|---|---|
| **Single-objective optim** (DE/CMAES/L-BFGS/NM) | ✅ Dominant | 10-100× faster than scipy | 4-29 orders better than scipy on Sphere/Ackley/Levy |
| **Multi-objective optim** (NSGA-II) | ✅ Wins | 1.1-1.6× faster than pymoo | Beats pymoo on all 4 ZDT/DTLZ |
| **Bayesian optim** (BO) | ✅ Wins on real problems | 2.2× slower than skopt | Hartmann6: -3.07 vs skopt -2.77 |
| **Simulated annealing** (Tsallis SA) | ✅ Wins | Comparable | Rastrigin: 0.0 (machine precision) |
| **Classical regression** (LM/Ridge/Lasso/GLMM) | ◎ At parity or better | 1.04-30× faster than sklearn (LME 30×) | Same accuracy |
| **Large-scale GLM/Lasso** (n ≥ 10k) | △ Behind | 3-5× slower than sklearn (Cython native) | Same accuracy |
| **Kernel/GP** | △ Behind | 2.5-4.7× slower than sklearn | Same accuracy |
| **Bayesian MCMC** (NUTS/HMC) | ✅ Pure-Haskell | (vs PyMC: not yet benchmarked) | (todo) |
| **HBM (probabilistic programming)** | ✅ Polymorphic DSL | — | PyMC parity (Truncated/Censored/MvNormal/LKJ/...) |
| **Hypothesis tests / multiple testing / bootstrap / effect** | ◎ Unified API | (vs scipy.stats: not yet benchmarked) | (todo) |
| **Data manipulation** (DataFrame) | ◎ Sufficient | (vs pandas/dplyr: not yet benchmarked) | (todo) |
| **Visualisation** | ◎ Vega-Lite based | — | Grammar-of-graphics parity |
| **Time series** (ARIMA/Holt-Winters) | 🆕 Implemented | (vs statsmodels: not yet benchmarked) | (todo) |
| **Survival analysis** (KM/Cox PH) | 🆕 Implemented | (vs lifelines: not yet benchmarked) | (todo) |
| **PCA / clustering / decision tree** | 🆕 Implemented | (vs sklearn: not yet benchmarked) | (todo) |

For the full breakdown including todo benchmarks, see [docs/comparison/python-r.md](docs/comparison/python-r.md).

---

## Benchmarks

Highlights (full numbers in [bench/results/SUMMARY.md](bench/results/SUMMARY.md)):

- **Sphere_30D/DE**: hanalyze **1.0e-26** vs scipy 2.8e-5 (**21 orders better**)
- **Sphere_30D/L-BFGS**: hanalyze **8.1e-40** vs scipy 2.6e-11 (**29 orders better**)
- **Rastrigin_10D/SA**: hanalyze **0.0** ⭐ (parity with scipy 7.8e-14)
- **Hartmann6/BO**: hanalyze **-3.07** vs skopt -2.77 (wins)
- **DTLZ2_3/NSGA-II**: hanalyze 466 ms vs pymoo 758 ms (1.6× faster)
- **DE Rosenbrock_2D**: hanalyze 1.2 ms vs scipy 164 ms (134× faster)

Run: `OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 cabal run bench-{regression,kernel,optim,mo,bo}`, then `bench/python/bench_*.py` (see [bench/README.md](bench/README.md)).

---

## Architecture

```mermaid
graph TD
  IO[DataIO.* CSV/Parquet/JSON]
  IO --> DF[Hackage dataframe]
  DF --> Models[Model.* regression/ML/Bayesian/TS/Survival]
  DF --> Stat[Stat.* tests/CV/effect/interpret]
  Models --> Optim[Optim.* optimisation]
  Models --> MCMC[MCMC.* samplers]
  Models --> Viz[Viz.* HTML/PNG/SVG]
  Stat --> Viz
  MCMC --> Viz
  Optim --> Design[Design.* DoE/Taguchi]
```

**All modules talk to Hackage `dataframe` directly**. The internal `DataFrame.Core` was retired (Phase 0-7).

---

## Module layout

```
src/
  DataIO/      — CSV/JSON/Parquet IO + health checks + sniff + clean DSL + reshape (9 mods)
  Stat/        — tests/distributions/interpolation/effect/CV/bootstrap/interpret etc. (21 mods)
  Model/       — LM/GLM/GLMM/Spline/Kernel/GP/RFF/HBM/PCA/Cluster/Tree/TS/Survival (23 mods)
  Optim/       — single-obj (NM/LBFGS/DE/CMAES/SA/PSO) + multi-obj (NSGA/BO/Pareto) (18 mods)
  Design/      — Factorial/Block/RSM/Optimal/Orthogonal/Taguchi (11 mods)
  Viz/         — Vega-Lite-based visualisation + ReportBuilder (15 mods)
  MCMC/        — MH/HMC/NUTS/Gibbs/Slice (6 mods)
```

Total: **103 modules, 238 tests passing**.

---

## Build

```bash
cabal build all                  # library + all executables (60+ demos)
cabal test                       # hspec, 238 examples
cabal repl                       # interactive REPL
```

Major dependencies: `hmatrix` (BLAS/LAPACK), `hvega` (Vega-Lite), `statistics`, `mwc-random`, `dataframe` (Hackage Polars-like), `massiv` (parallel arrays), `ad` (auto-diff), `async`.

Tested on GHC 9.6.7 + cabal 3.14.2.

---

## Running benchmarks

```bash
# 1. Generate shared test data (fixed-seed, deterministic)
cabal run bench-data-gen

# 2. Haskell side
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \
  cabal run bench-regression bench-kernel bench-optim bench-mo bench-bo

# 3. Python side (need bench/venv from bench/requirements.txt)
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \
  bench/venv/bin/python bench/python/bench_regression.py
# (similarly for kernel, optim, mo, bo)

# 4. Aggregate (Markdown table)
bench/venv/bin/python bench/aggregate.py > bench/results/SUMMARY.md
```

---

## Development

- **Issues / PRs**: [github.com/frenzieddoll/hanalyze](https://github.com/frenzieddoll/hanalyze)
- **Adding tests**: append hspec specs in `test/Spec.hs`
- **Adding benchmarks**: place `bench/haskell/Bench*.hs` and matching Python script
- **Coding rules**: see `CONTRIBUTING.md` (no list-passing on hot paths, minimise `unsafe*`, ...)

---

## License

BSD-3-Clause License — see [LICENSE](LICENSE).

## Author

Toshiaki Honda <frenzieddoll@gmail.com>
