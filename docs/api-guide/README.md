# hanalyze API Reference

A comprehensive topic-based API reference for public APIs. Theoretical explorations, derivations, and pitfalls — the "how to learn" material — belong in the [topic guides](../) in `docs/<topic>/`, while this serves as a **dictionary for immediate answers: "What is this function's type? What's a minimal example?"**

> **Terminology**: Fitting a model to data = **fit** (high-level verb `df |-> spec`), converting a fitted result to a plot = **`toPlot`** or extractors (`forestOf` / `tracesOf` / …), and a data source with columns = **`ColumnSource`** (assoc list `[(Text, ColData)]` / Hackage `DataFrame` / `Map`). The drawing grammar (layer / mark) is the primary reference in [hgg's API reference](../../../hgg/docs/api-guide/README.md).

## Page Index

| Page | Content |
|---|---|
| [01 quickstart](01-quickstart.md) | Shortest path: fit → plot (`df \|-> lm` + `toPlot` in 3 lines) |
| [02 regression](02-regression.md) | LM / GLM / Robust / Quantile / WLS / GAM / Spline / Kernel / RFF / GP / Formula DSL |
| [03 bayesian-hbm](03-bayesian-hbm.md) | HBM (`ModelP` / distributions / plate / hierarchy) /  `HBMConfig`/NUTS /  `\|->!` /  posterior summary (`hbmSummary` / `hbmSummaryDf` / `hbmDrawsDf`) /  diagnostic extractors (dag/trace/marginals/forest/ppc/epred/autocorr/rank/pair/energy) |
| [04 multivariate](04-multivariate.md) | PCA / PLS / RRR / CCA / Discriminant / Cluster / HCluster / FDA |
| [05 ml](05-ml.md) | RandomForest / GBM / DecisionTree / KNN / NaiveBayes / NeuralNetwork / SVM / MDS |
| [06 timeseries](06-timeseries.md) | AR / VAR / GARCH / forecast |
| [07 survival](07-survival.md) | KaplanMeier / CompetingRisks (CIF) / AFT / Cox |
| [08 causal](08-causal.md) | PropensityScore / IPW / DR / CATE / LiNGAM |
| [09 doe](09-doe.md) | Design of Experiments (Factorial / RSM / Optimal / Orthogonal / Taguchi / Anova / Power) |
| [10 stat](10-stat.md) | Descriptive Statistics / Hypothesis Testing / Correlation / Effect Size / PCA / Cluster / Bootstrap |
| [11 data](11-data.md) | Data.* (Transform / Wrangle) + DataIO (CSV / clean / reshape) + Fit API (`\|->`) |
| [12 plot](12-plot.md) | Plot integration (`toPlot` / extractors) → [hgg api-guide](../../../hgg/docs/api-guide/README.md) |

## Two Layers (Both Available in Any Import)

| Layer | Position | Example |
|---|---|---|
| **High-level (Main)** | Fit directly by column names from a data source. Combine `df \|-> spec` with any model and draw with `toPlot` | `df \|-> lm "x" "y"` |
| **Low-level** | When you already have `hmatrix` `Vector`/`Matrix`, use numeric APIs (`fitLMVec` etc.). Results are `FitResult` | `fitLMVec (designMatrix xs) ys` |

Each page emphasizes **high-level `df |-> spec` / `toPlot`**, with low-level matrix API direct calls marked **low-level** and documented alongside.

## Operators & Extractors Quick Reference

There are four core paths for fitting data and plotting. Role differences are determined by this table as primary reference (each page covers details only).

| Operator / Function | Role | Details |
|---|---|---|
| `\|->` | **Fit a model** from data source by column name (pure / deterministic) | [01 quickstart](01-quickstart.md) / [11 data](11-data.md) |
| `\|->!` | **IO version** of `\|->` (with sampling progress bar / result is bit-identical) | [03 bayesian-hbm](03-bayesian-hbm.md) |
| `toPlot` | Convert a fitted model to **plot (layer)** (scatter + regression line + CI band etc.) | [12 plot](12-plot.md) |
| `\|>>` | **Bundle data source to plot** (`BoundPlot` pure function / no file output) | [11 data](11-data.md) |

Posterior plots for HBM use dedicated extractors (cousins of `toPlot`):

| Extractor | Plot Produced |
|---|---|
| `forestOf` | Posterior forest (94% HDI for coefficients) |
| `tracesOf` | Trace plot (1 panel per parameter / divergence rug ON by default) |
| `ppcOf` | Posterior predictive check (PPC) |
| `epred` | Expected value prediction curve (posterior band for regression) |
| `dagOf` | Model DAG (plate folded) |
| `autocorrOf` | Autocorrelation (mixing diagnosis) |
| `rankOf` | Rank plot (chain uniformity / requires ≥2 chains) |

**spec verbs** for `df |-> spec`: bivariate shortcuts `lm` / `glm` / `spline` / `robust` / `quantile` / Formula DSL `lmF` / `glmF` / `glmmF` / Bayesian `hbm` / matrix input `pcaOf` / `plsOf` (Phase 70.A / column name list for PCA/PLS). For models without a spec yet, fit with the fit function; if the result type is `Plottable`, render directly with `toPlot result` into `saveSVG` (or `df |>>` for data overlay, see [12 plot](12-plot.md)).

## Related Documentation

- **Learning roadmap (theory + derivations + pitfalls)**: topic guides in [`docs/`](../) (regression/ /  bayesian/ /  stat/ …)
- **Authoritative fit API**: [`docs/io/04-fit-api.md`](../io/04-fit-api.md) (`df |-> spec` comprehensive)
- **Canonical plot integration**: [`docs/visualization/03-plot-integration.md`](../visualization/03-plot-integration.md)
- **Drawing grammar (layer / mark / scale)**: [hgg API Reference](../../../hgg/docs/api-guide/README.md)
