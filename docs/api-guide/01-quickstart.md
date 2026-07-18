# Quickstart — Fit and Plot in Seconds

> 🌐 **English** | [日本語](01-quickstart.ja.md)

> [📚 Index](README.md) | **01 quickstart** | [02 regression](02-regression.md) | [03 bayesian-hbm](03-bayesian-hbm.md) | [04 multivariate](04-multivariate.md) | [05 ml](05-ml.md) | [06 timeseries](06-timeseries.md) | [07 survival](07-survival.md) | [08 causal](08-causal.md) | [09 doe](09-doe.md) | [10 stat](10-stat.md) | [11 data](11-data.md) | [12 plot](12-plot.md)

This page shows the shortest path in hanalyze for **fitting data and producing one plot**. The signature and minimal example for each model are detailed references in [02 regression](02-regression.md) and beyond. The full landscape of the fit API is in [11 data](11-data.md) and [`docs/io/04-fit-api.md`](../io/04-fit-api.md), and conversion to plots in [12 plot](12-plot.md).

> **Golden Rule (Just Remember This)**
>
> 1. **Fit** = `df |-> spec`. Connect your data source `df` and "what to fit" the spec
>    (`lm "x" "y"` etc.) with `|->` to get a fitted model.
> 2. **Plot** = `toPlot model` converts the model to a layer, and `df |>> (layer (scatter "x" "y") <> toPlot model)`
>    overlays data and model, saving with `saveSVGBound`.

Structure of this page:
**[30 Seconds, One Plot (LM)](#lm-30s)** | **[Data Sources](#data-source)** | **[Low-level (Matrix API)](#low-level)**

---

## 30 Seconds, One Plot (Linear Regression) {#lm-30s}

Use the universal verb `df |-> lm` to fit, and `toPlot` to overlay a regression line with 95% CI band on a scatter plot.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Vector              as V
import           Hanalyze.Plot     (lm, (|->), toPlot)
import           Hgg.Plot.Spec        (ColData (..), layer, scatter)
import           Hgg.Plot.Frame       ((|>>))
import           Hgg.Plot.Backend.SVG (saveSVGBound)

main :: IO ()
main = do
  let df  = [ ("x", NumData (V.fromList [1,2,3,4,5,6,7,8]))
            , ("y", NumData (V.fromList [2.1,3.9,6.2,7.8,10.3,11.7,14.1,16.0])) ]
      fit = df |-> lm "x" "y"        -- LMModel: β, ŷ, residuals, R²
  saveSVGBound "lm.svg"             -- scatter + OLS line + 95% CI band
    $ df |>> layer (scatter "x" "y") <> toPlot fit
```

![Scatter plot + regression line + CI band](../images/lm-scatter-ci.svg)

The `lm` in `df |-> lm "x" "y"` is one of **9 spec verbs**. The same pattern works with `glm` (GLM) / 
`spline` /  `robust` /  `quantile` /  Formula versions `lmF` / `glmF` / `glmmF` /  Bayesian `hbm`
(see [README quick reference](README.md#operators--extractors-quick-reference)).

---

## Data Sources — What Can You Pass as `df`? {#data-source}

The left side of `|->` / `|>>` accepts any **`ColumnSource`**. If you can look up columns by name, the same syntax works.

| Data Source | Example |
|---|---|
| assoc list | `[("x", NumData (V.fromList xs)), ("y", NumData (V.fromList ys))]` |
| Hackage `DataFrame` | CSV loader (`loadAuto` etc. / [11 data](11-data.md)) returns a `DataFrame` directly |
| `Map Text ColData` | `Map.fromList [("x", NumData …), …]` |

`ColData` has two constructors: numeric column `NumData (V.Vector Double)` and string (categorical) column
`TxtData (V.Vector Text)` (from [`Hgg.Plot.Spec`](../../src/Hanalyze/Plot.hs)). For plots without data (HBM forest etc.),
pass empty source `noDf = [] :: [(Text, ColData)]` ([03 bayesian-hbm](03-bayesian-hbm.md)).

---

## Low-level (Matrix API) {#low-level}

If you already have hmatrix `Vector` / `Matrix` and just want numeric `FitResult`,
call the matrix API directly from the model module (no plotting).

```haskell
import Hanalyze.Model.LM   (fitLMVec, designMatrix)
import Hanalyze.Model.Core (coefficientsV, rSquared1)

let fit  = fitLMVec (designMatrix xs) ys   -- FitResult: β, ŷ, residuals, R²
    beta = coefficientsV fit
    r2   = rSquared1 fit
```

The high-level `df |-> lm` internally calls this matrix API and returns an `LMModel`
that additionally holds the design matrix needed for plotting (so `toPlot` can draw CI bands).
Each model page also emphasizes **high-level as main, with low-level marked `**low-level**` alongside**.

→ Full fit API landscape: [11 data](11-data.md) / [`docs/io/04-fit-api.md`](../io/04-fit-api.md)
→ Plot conversion and extractor list: [12 plot](12-plot.md)
