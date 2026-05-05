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

Comprehensive coverage across statistics, ML, Bayesian, optimisation, DoE, visualisation, and data I/O.

| Category | Feature | Module | Docs |
|---|---|---|---|
| **Statistical inference** | 12 hypothesis tests (t/χ²/ANOVA/Wilcoxon/KS/Shapiro/Levene/Bartlett/...) | `Stat.Test` | [docs/stat/01-test.ja.md](docs/stat/01-test.ja.md) |
| | Multiple-testing correction (Bonferroni/Holm/BH/BY) | `Stat.MultipleTesting` | [06-multipletesting.ja.md](docs/stat/06-multipletesting.ja.md) |
| | Bootstrap CI / permutation tests | `Stat.Bootstrap` | [07-bootstrap.ja.md](docs/stat/07-bootstrap.ja.md) |
| | Effect size + power analysis (Cohen's d/η²/V/n estimation) | `Stat.Effect` | [09-effect.ja.md](docs/stat/09-effect.ja.md) |
| **Regression** | Linear / GLM (Binomial/Poisson) / GLMM (LME) | `Model.{LM,GLM,GLMM}` | [docs/regression/01-lm.md](docs/regression/01-lm.md) |
| | Regularised (Ridge/Lasso/ElasticNet) | `Model.Regularized` | [04-spline-kernel-regularized.md](docs/regression/04-spline-kernel-regularized.md) |
| | Kernel methods (KR/NW) + GP (RBF/Matérn/Periodic + ARD) + RFF | `Model.{Kernel,GP,RFF}` | same |
| | Splines (B-spline/natural) / GAM / Quantile regression | `Model.{Spline,GAM,Quantile}` | [06-quantile-gam-rf.md](docs/regression/06-quantile-gam-rf.md) |
| | Multivariate regression / Multi-output GP | `Model.{Multivariate,MultiGP,MultiOutput}` | [05-multivariate.md](docs/regression/05-multivariate.md) |
| **Machine learning** | PCA + cumulative variance + standardisation modes | `Model.PCA` | [docs/stat/02-pca.ja.md](docs/stat/02-pca.ja.md) |
| | Clustering (K-means + k-means++) + silhouette | `Model.Cluster` | [05-cluster.ja.md](docs/stat/05-cluster.ja.md) |
| | Decision tree (CART classifier) | `Model.DecisionTree` | [docs/regression/08-decisiontree.ja.md](docs/regression/08-decisiontree.ja.md) |
| | Random forest (regression) | `Model.RandomForest` | [06-quantile-gam-rf.md](docs/regression/06-quantile-gam-rf.md) |
| | Time-series (ARIMA/Holt-Winters/STL/ACF/PACF) | `Model.TimeSeries` | [09-timeseries.ja.md](docs/regression/09-timeseries.ja.md) |
| | Survival analysis (KM/Nelson-Aalen/Log-rank/Cox PH) | `Model.Survival` | [10-survival.ja.md](docs/regression/10-survival.ja.md) |
| | Classification metrics (Confusion/AUC/F1/MCC/log-loss/Brier) | `Stat.ClassMetrics` | [03-classmetrics.ja.md](docs/stat/03-classmetrics.ja.md) |
| | Cross-validation (k-fold/stratified/LOO) + Grid search | `Stat.CV` | [04-cv.ja.md](docs/stat/04-cv.ja.md) |
| | Interpretability (Permutation imp/PDP/ICE) | `Stat.Interpret` | [13-interpret.ja.md](docs/stat/13-interpret.ja.md) |
| **Bayesian** | MCMC (MH/HMC/NUTS/Gibbs/Slice) | `MCMC.*` | [docs/bayesian/](docs/bayesian/) |
| | HBM polymorphic DSL (4 interpretations: structure/log-joint/AD/dep DAG) | `Model.HBM` | same |
| | Variational inference (ADVI mean-field Adam) | `Stat.VI` | same |
| | Model comparison (WAIC/PSIS-LOO/Pseudo-BMA) | `Stat.ModelSelect` | same |
| | 27 probability distributions + posterior predictive checks | `Stat.{Distribution,PosteriorPredictive}` | [docs/02-pymc-comparison.md](docs/02-pymc-comparison.md) |
| **Optimisation** | Single-objective (NM/L-BFGS/DE/CMA-ES/SA/PSO/Brent) | `Optim.*` | [docs/optim/01-singleobj.md](docs/optim/01-singleobj.md) |
| | Multi-objective (NSGA-II + Pareto + EHVI/ParEGO) | `Optim.{NSGA,Pareto,Acquisition}` | [02-multi-objective.md](docs/optim/02-multi-objective.md) |
| | Bayesian optimisation (BO + GP-Hedge + analytic gradient) | `Optim.BayesOpt` | [theory-bayesopt.md](docs/optim/theory-bayesopt.md) |
| **Design of experiments** | DoE (Factorial/Block/RSM/Optimal) | `Design.*` | [docs/doe/01-doe.md](docs/doe/01-doe.md) |
| | Orthogonal arrays (L4/L8/L9/L12/L16/L18) + Taguchi (S/N + inner/outer) | `Design.{Orthogonal,Taguchi}` | [02-orthogonal-taguchi.md](docs/doe/02-orthogonal-taguchi.md) |
| **Visualisation** | Scatter / Bar / histograms / MCMC diagnostics | `Viz.{Scatter,Bar,Histogram,MCMC}` | [docs/visualization/](docs/visualization/) |
| | Integrated HTML report (MathJax + Mermaid + interactive) | `Viz.ReportBuilder` | same |
| **Data I/O** | CSV/TSV/SSV (cassava) + Parquet/JSON (dataframe) | `DataIO.{CSV,External}` | [docs/io/](docs/io/) |
| | Dirty-data defence (W001-W008 + auto-sniff + clean DSL) | `DataIO.{Health,Sniff,Clean}` | same |
| | Reshape (pivot_wider/one-hot/lag-lead/rolling window) | `DataIO.Reshape` | [02-reshape.ja.md](docs/io/02-reshape.ja.md) |
| | Preprocessing (impute/groupBy/derived/melt) | `DataIO.Preprocess` | [docs/io/](docs/io/) |

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

Summary of benchmarks (full details in [docs/comparison/python-r.ja.md](docs/comparison/python-r.ja.md)):

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
| **Hypothesis tests** | ◎ Unified API | (vs scipy.stats: not yet benchmarked) | (todo) |
| **Data manipulation** (DataFrame) | ◎ Sufficient | (vs pandas/dplyr: not yet benchmarked) | (todo) |
| **Visualisation** | ◎ Vega-Lite based | — | Grammar-of-graphics parity |
| **Time series** (ARIMA/Holt-Winters) | 🆕 Implemented | (vs statsmodels: not yet benchmarked) | (todo) |
| **Survival analysis** (KM/Cox PH) | 🆕 Implemented | (vs lifelines: not yet benchmarked) | (todo) |
| **PCA / Clustering** | 🆕 Implemented | (vs sklearn: not yet benchmarked) | (todo) |

For the full breakdown including todo benchmarks, see [docs/comparison/python-r.ja.md](docs/comparison/python-r.ja.md).

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
