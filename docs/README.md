# hanalyze documentation

> 🌐 **English** | [日本語](README.ja.md)

A statistics / optimization / design-of-experiments toolkit written in Haskell.
This is the entry point for user-facing documentation.

There are three ways in — **start with the quickstart, look things up in the API
reference, dig deeper in the per-topic notes**.

## 1. Get something running

| Page | Contents |
|---|---|
| [01 quickstart](01-quickstart.md) | Read a CSV, fit with `df \|-> lm`, draw with `toPlot` |
| [02 comparison with PyMC](02-pymc-comparison.md) | Bayesian models (HBM/NUTS) side by side with PyMC |

Installation and how to pick a package are in the [root README](../README.md).

## 2. API reference (the dictionary)

**[api-guide/](api-guide/README.md)** — a topic-by-topic dictionary of the public API.

| Page | Contents |
|---|---|
| [01 quickstart](api-guide/01-quickstart.md) | The shortest path from fit to plot |
| [02 regression](api-guide/02-regression.md) | LM / GLM / robust / quantile / WLS / GAM / spline / kernel / GP / formula DSL |
| [03 bayesian-hbm](api-guide/03-bayesian-hbm.md) | HBM (`ModelP` / distributions / plates), NUTS, posterior summaries, diagnostics |
| [04 multivariate](api-guide/04-multivariate.md) | PCA / PLS / RRR / CCA / discriminant / cluster / FDA |
| [05 ml](api-guide/05-ml.md) | Random forest / GBM / decision tree / k-NN / naive Bayes / NN / SVM / MDS |
| [06 timeseries](api-guide/06-timeseries.md) | AR / VAR / GARCH / forecasting |
| [07 survival](api-guide/07-survival.md) | Kaplan-Meier / competing risks / AFT / Cox |
| [08 causal](api-guide/08-causal.md) | Propensity score / IPW / DR / CATE / LiNGAM |
| [09 doe](api-guide/09-doe.md) | Design of experiments (factorial / RSM / optimal / orthogonal / Taguchi / power) |
| [10 stat](api-guide/10-stat.md) | Descriptive statistics / tests / correlation / effect size / bootstrap |
| [11 data](api-guide/11-data.md) | `Data.*` (Transform / Wrangle) + DataIO + the fit API (`\|->`) |
| [12 plot](api-guide/12-plot.md) | Plotting integration (`toPlot` and the extractors) |

## 3. Per-topic notes (going deeper)

Where the api-guide tells you *what exists*, these cover **background, trade-offs and
measurements** for each method.

| Topic | Pages | Contents |
|---|---:|---|
| [regression/](regression/) | 48 | Individual regression methods (penalized / robust / quantile / GAM / GP …) |
| [bayesian/](bayesian/) | 32 | HBM modelling, convergence diagnostics, visualization |
| [stat/](stat/) | 26 | Descriptive statistics, tests, effect sizes, multivariate methods |
| [doe/](doe/) | 25 | Design of experiments (factorial / RSM / optimal / custom design) |
| [optim/](optim/) | 12 | Optimization (Nelder-Mead / L-BFGS / CMA-ES / NSGA-II / Bayesian optimization) |
| [principles/](principles/) | 10 | Design principles (API consistency, purity, numerical correctness) |
| [io/](io/) | 8 | Loading and reshaping (CSV / clean / reshape / fit API) |
| [visualization/](visualization/) | 6 | Visualization (plotting integration, reports) |
| [causal/](causal/) | 2 | Causal inference and causal discovery |
| [ml/](ml/) | 2 | Machine learning |
| [timeseries/](timeseries/) | 2 | Time series |
| [fda/](fda/) | 2 | Functional data analysis |
| [comparison/](comparison/) | 2 | Comparisons with other libraries |

## 4. For developers

| Page | Contents |
|---|---|
| [dev-notes/](dev-notes/) | Implementation notes |
| [internal/](internal/) | Internal design |
| [superpowers/](superpowers/) | Investigation and planning records |

Package layout, build instructions and contribution guidelines live in the
[root README](../README.md) and [CONTRIBUTING.md](../CONTRIBUTING.md).

---

**On languages**: most pages come as a pair — `*.md` (English) and `*.ja.md` (Japanese).
The Haddock API documentation is bilingual as well.
