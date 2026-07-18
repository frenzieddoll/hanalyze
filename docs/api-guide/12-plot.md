# Plot Integration

> 🌐 **English** | [日本語](12-plot.ja.md)

> [📚 Index](README.md) | [01 quickstart](01-quickstart.md) | [02 regression](02-regression.md) | [03 bayesian-hbm](03-bayesian-hbm.md) | [04 multivariate](04-multivariate.md) | [05 ml](05-ml.md) | [06 timeseries](06-timeseries.md) | [07 survival](07-survival.md) | [08 causal](08-causal.md) | [09 doe](09-doe.md) | [10 stat](10-stat.md) | [11 data](11-data.md) | **12 plot**

Bridge from fitted models to hgg layer syntax. The **drawing grammar itself** (layer / mark / scale / theme / facet / backend) is documented in the [hgg API reference](../../../hgg/docs/api-guide/README.md). This section covers only the analyze-side `toPlot` and extractors.

---

## Building Visualizations

```haskell
toPlot :: Plottable m => m -> VisualSpec        -- model → figure (layer)
(|>>)  :: ColumnSource d => d -> VisualSpec -> BoundPlot   -- bind data to figure
```

Basic pattern: bundle data with visualization (`VisualSpec` is a `Monoid`, compose with `<>`):

```haskell
saveSVGBound "out.svg" $ df |>> layer (scatter "x" "y") <> toPlot fit
```

Save functions: `saveSVGBound` / `saveSVGBoundStats` × SVG/PDF/PNG ([plot 05 backends](../../../hgg/docs/api-guide/05-backends.md)).

---

## `Plottable` Models

Result types that work directly with `toPlot` (details on corresponding pages):

| Domain | Plottable Type |
|---|---|
| Regression ([02](02-regression.md)) | `LMModel` / `GLMModel` / `WeightedLMModel` / `RobustModel` / `QuantileModel` / `SplineModel` / `GAMModel` / `GPResult` / `RegFit` / `GLMMResultRE` |
| Bayesian ([03](03-bayesian-hbm.md)) | `HBMModel` extractors (below) / `ChainModel` |
| Multivariate ([04](04-multivariate.md)) | `PCAResult` / `PLSFit` / `MultiFit` / `DiscriminantFit` / `KMeansResult` |
| ML ([05](05-ml.md)) | `RandomForest` / `GBRegressor` / `GBClassifier` / `DTree` / `KNNClassifier` / `NBModel` |
| Time Series & Survival ([06](06-timeseries.md)/[07](07-survival.md)) | `ForecastModel` / `GARCHFit` / `KMResult` / `CRFit` / `AFTFit` |
| Causal & Testing ([08](08-causal.md)/[10](10-stat.md)) | `DirectLiNGAMFit` / `TestResult` / `PCAResult` |

> `MultiLMModel` / `MultiGLMModel` are **not `Plottable`**. For effect plots, use `statModelMulti m (along "x") <> holdAt Median` with `toPlot` ([02 regression](02-regression.md#formula-dsl)).

---

## Extractors (`toPlot` companions)

Specialized builders for models where plain `toPlot` is insufficient.

| Extractor | Target | Purpose |
|---|---|---|
| `forestOf` / `tracesOf` / `ppcOf` / `epred` / `dagOf` | `HBMModel` | posterior forest / trace / PPC / predictive mean / DAG ([03](03-bayesian-hbm.md)) |
| `plsScorePlot` / `plsLoadingPlot` / `plsVipPlot` | `PLSFit` | score / loading / VIP ([04](04-multivariate.md)) |
| `decisionBoundaryOf` / `confusionOf` | Classifiers (`ClassPredict`) | decision boundary / confusion matrix ([05](05-ml.md)) |
| `testForest` / `testForestLabeled` / `describeBox` | `TestResult` / raw data | test forest / box plot ([10](10-stat.md)) |
| `aftSurvivalAt` | `AFTFit` | survival curve at arbitrary covariates ([07](07-survival.md)) |

---

## Architecture

`toPlot` and `Plottable` live under cabal flag `plot-integration` (default off = standalone, upstream portable / on = `Hanalyze.Plot` depends on `hgg-core` etc). Dependency is **one-directional `analyze → plot-core`**. Details: [visualization/03-plot-integration](../visualization/03-plot-integration.md).

→ Full drawing grammar: [hgg API reference](../../../hgg/docs/api-guide/README.md)
