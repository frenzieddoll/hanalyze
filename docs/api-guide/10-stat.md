# Descriptive Statistics, Hypothesis Testing & Effect Sizes

> 🌐 **English** | [日本語](10-stat.ja.md)

> [📚 Index](README.md) | [01 quickstart](01-quickstart.md) | [02 regression](02-regression.md) | [03 bayesian-hbm](03-bayesian-hbm.md) | [04 multivariate](04-multivariate.md) | [05 ml](05-ml.md) | [06 timeseries](06-timeseries.md) | [07 survival](07-survival.md) | [08 causal](08-causal.md) | [09 doe](09-doe.md) | **10 stat** | [11 data](11-data.md) | [12 plot](12-plot.md)

Descriptive statistics, hypothesis testing, and effect size estimation. Theory and detailed derivations are documented in [`docs/stat/`](../stat/).

| Domain | Module | Main Functions |
|---|---|---|
| Descriptive Statistics | `Stat.Descriptive` | `mean` / `median` / `quantile` / `variance` / `sd` / `iqr` / `range'` |
| Hypothesis Testing | `Stat.Test` | `tTest1Sample` / `tTestWelch` / `anovaOneWay` / `chiSquareGOF` / `shapiroWilk` … |
| Effect Sizes | `Stat.Effect` | `cohenD` / `hedgesG` |
| Preprocessing (Standardization) | `Stat.Standardize` | `fitStandardizer` / `applyStandardizer` / `unapplyStandardizer` |
| PCA / Clustering | → [04 multivariate](04-multivariate.md) | |
| Bootstrap | `Stat.Bootstrap` | → [07-bootstrap](../stat/07-bootstrap.md) |

---

## Descriptive Statistics (`Stat.Descriptive`)

All functions operate on `G.Vector v Double` (pass `[Double]` via `V.fromList` etc.) with a single canonical signature.

```haskell
mean     :: G.Vector v Double => v Double -> Double
median   :: G.Vector v Double => v Double -> Double
quantile :: G.Vector v Double => Double -> v Double -> Double   -- probability (R type-7 linear interpolation)
variance :: G.Vector v Double => v Double -> Double             -- n-1 denominator
sd       :: G.Vector v Double => v Double -> Double
iqr      :: G.Vector v Double => v Double -> Double
range'   :: G.Vector v Double => v Double -> Double
```

> NA handling is the caller's responsibility (remove via `mapMaybe id` etc). → symmetric with [io/04-fit-api](../io/04-fit-api.md) and also usable from aggregators in [11 data](11-data.md).

Visualize raw data distribution as a box plot with `describeBox`:

```haskell
import Hanalyze.Plot (describeBox)
saveSVGBound "box.svg" $ noDf |>> describeBox xs <> title "distribution"
```

![Descriptive statistics box plot](../images/describe-box.svg)

---

## Hypothesis Testing (`Stat.Test`)

Most tests return `TestResult`, which is `Plottable` (effect size + 95% CI forest with zero baseline).

```haskell
tTest1Sample :: LA.Vector Double -> Double -> Alternative -> TestResult   -- sample, μ₀, alternative hypothesis
tTestWelch   :: LA.Vector Double -> LA.Vector Double -> Alternative -> TestResult   -- 2-sample Welch
anovaOneWay  :: [LA.Vector Double] -> TestResult
kruskalWallis :: [LA.Vector Double] -> TestResult
chiSquareGOF :: LA.Vector Double -> LA.Vector Double -> TestResult
shapiroWilk  :: LA.Vector Double -> TestResult     -- normality
leveneTest   :: [LA.Vector Double] -> TestResult   -- equal variance
```

Combine multiple tests in a forest plot using `testForestLabeled`:

```haskell
import Hanalyze.Plot (toPlot, testForestLabeled)
saveSVGBound "forest.svg"
  $ noDf |>> testForestLabeled [("A vs B", tAB), ("A vs C", tAC), ("B vs C", tBC)]
```

![Test effect-size forest](../images/test-forest.svg)

> ★Most tests do not have CIs and are unsuitable for forest plots (only `tTest1Sample` / `tTestWelch` etc. have them).
> The zero baseline is intended for mean differences and effect sizes (null = 0); mixing it with raw means distorts the axis ([01-test](../stat/01-test.md)).

---

## Effect Sizes (`Stat.Effect`)

```haskell
cohenD      :: LA.Vector Double -> LA.Vector Double -> Double
hedgesG     :: LA.Vector Double -> LA.Vector Double -> Double
cohenDCI    :: LA.Vector Double -> LA.Vector Double -> Double -> (Double, Double)  -- α → confidence interval
cohenDPaired :: LA.Vector Double -> LA.Vector Double -> Double                     -- paired d
```

`cohenDCI xs ys 0.05` returns **exact** 95% CI via the noncentral t-distribution (not asymptotic approximation).
Derivation: [stat/usage-misc-stat](../stat/usage-misc-stat.md).

---

## Advanced Analysis (Fit Y by X / Friedman+Dunn / LCA / Graphical Lasso)

Aggregate modules from Phase 13 / 32. Formulation, label switching pitfalls, etc. are detailed in [stat/usage-misc-stat](../stat/usage-misc-stat.md).

**Fit Y by X** (`Model.FitYByX`) — auto-dispatch analysis by X/Y types (JMP "Fit Y by X" equivalent):

```haskell
fitYByX :: LA.Vector Double -> VarType -> LA.Vector Double -> VarType -> FitYByXResult
-- VarType = Continuous | Categorical
-- Cont×Cont → simple regression / Cont×Cat → logistic / Cat×Cont → one-way ANOVA / Cat×Cat → χ² independence
```

**Friedman + Dunn** (`Stat.Test`) — paired multi-group nonparametric test + all-pairs post-hoc:

```haskell
friedmanTest :: LA.Matrix Double -> TestResult              -- n subjects × k treatments, Plottable
dunnTest     :: [LA.Vector Double] -> MultiCompareResult    -- all pairs after Kruskal-Wallis (p_adj, BH default)
```

**LCA** (`Model.LatentClassAnalysis`) — categorical latent class clustering (EM):

```haskell
fitLCA :: Int -> Int -> [[Int]] -> Int -> Double -> MWC.GenIO -> IO LCAFit
--        K     L      rows (n×J)   maxIter tol      RNG
-- lcaPi (mixing π) / lcaRho (K×L conditional probabilities) / lcaResponsibilities (posterior γ)
```

**Graphical Lasso** (`Stat.CorrelationNetwork`) — sparse precision matrix (conditional independence network):

```haskell
graphicalLasso        :: LA.Matrix Double -> Double -> Int -> Double -> GLassoFit   -- X, λ, maxOuter, tol
graphicalLassoFromCov :: LA.Matrix Double -> Double -> Int -> Double -> GLassoFit   -- from existing S
-- glPrecision (Θ=Σ⁻¹ sparse) / glCovariance (Σ) / glConverged / nonZeroPrecision thr Θ
empiricalCov :: LA.Matrix Double -> LA.Matrix Double
```

---

## Preprocessing: Standardization (Stat.Standardize)

Low-level utilities for z-score standardization `(x − μ) / σ` of features: fit, apply, and invert.
Each column stores `(μ, σ)` in a `Standardizer` record (constant columns with `σ ≈ 0` are rounded to `σ = 1` to avoid division by zero).

```haskell
import Hanalyze.Stat.Standardize
  ( Standardizer (..), fitStandardizer, applyStandardizer
  , unapplyStandardizer, applyStandardizerCol, identityStandardizer )

fitStandardizer     :: LA.Matrix Double -> Standardizer            -- learn (μ,σ) per column from n×p matrix
applyStandardizer   :: Standardizer -> LA.Matrix Double -> LA.Matrix Double   -- (x−μ)/σ
unapplyStandardizer :: Standardizer -> LA.Matrix Double -> LA.Matrix Double   -- x·σ+μ (restore to original scale)
applyStandardizerCol :: Standardizer -> Int -> Double -> Double    -- standardize single column, single value (e.g., JS slider)
-- Standardizer { stMu :: [Double], stSd :: [Double] }  -- μ / σ per column (JSON-friendly)
```

```haskell
let s   = fitStandardizer xTrain          -- fix (μ, σ) from training data
    xz  = applyStandardizer s xTrain      -- transform to standardized space
    xz' = applyStandardizer s xTest       -- ★apply same (μ, σ) to test (prevents leakage)
    x   = unapplyStandardizer s xz        -- restore to original scale
```

Convention: do not standardize `y` (preserves output scale of regression). For **transparent standardization during model learning**, do not use this low-level API directly; instead use the wrapper from [05 ml: `standardized` / `standardizedY`](05-ml.md#transparent-standardization-wrapper-standardized--standardizedy) (automatic inversion restores figures and predictions to original scale).

→ [09-effect](../stat/09-effect.md) / [PCA, clustering: 04 multivariate](04-multivariate.md) / [Bootstrap: 07-bootstrap](../stat/07-bootstrap.md)
