# Regression Models

> 🌐 **English** | [日本語](02-regression.ja.md)

> [📚 Index](README.md) | [01 quickstart](01-quickstart.md) | **02 regression** | [03 bayesian-hbm](03-bayesian-hbm.md) | [04 multivariate](04-multivariate.md) | [05 ml](05-ml.md) | [06 timeseries](06-timeseries.md) | [07 survival](07-survival.md) | [08 causal](08-causal.md) | [09 doe](09-doe.md) | [10 stat](10-stat.md) | [11 data](11-data.md) | [12 plot](12-plot.md)

Signatures, minimal examples, and plots for regression models. Bivariate fitting emphasizes **high-level `df |-> spec`**
as the main path (`spec` = `lm`/`glm`/`spline`/`rlm`/`rq`), and `toPlot` for visualization.
Theory, diagnostics, and pitfalls are primary references in the [`docs/regression/`](../regression/) guides.
(Formula input `lmF`/`glmF`/`glmmF` and formula syntax are documented separately in [11-formula-dsl](../regression/11-formula-dsl.md).)

| Model | High-level | Result Type (Plottable) | Plot | Guide |
|---|---|---|---|---|
| Linear Regression (simple OLS) | `df \|-> lm "x" "y"` | `LMModel` | scatter + line + CI | [01-lm](../regression/01-lm.md) |
| Multiple Regression | `df \|-> lmMulti ["x1","x2","x3"] "y"` | `MultiLMModel` | partial effect plot | This page ↓ |
| Generalized Linear (GLM) | `df \|-> glm fam link "x" "y"` | `GLMModel` | scatter + mean + CI | [02-glm](../regression/02-glm.md) |
| Weighted Least Squares (WLS) | `df \|-> weighted "w" (lm "x" "y")` | `WeightedLMModel` | scatter + line + CI | This page ↓ |
| Robust Regression | `df \|-> rlm est "x" "y"` | `RobustModel` | contrast with OLS | [usage-regularized-advanced](../regression/usage-regularized-advanced.ja.md) |
| Quantile Regression | `df \|-> rq taus "x" "y"` | `QuantileModel` | line ensemble by τ | [06-quantile](../regression/06-quantile.md) |
| Spline | `df \|-> spline kind knots "x" "y"` | `SplineModel` | smooth curve + CI | [04-spline](../regression/04-spline.md) |
| GAM | `df \|-> gam cfg "x" "y"` (`gamMulti` for multiple predictors) | `GAMModelN` | smooth curve (basis selection / GCV) | This page ↓ |
| Kernel Method (GP / KRR / RFF) | `df \|-> gp cfg "x" "y"` (`gpMulti` for multiple predictors) | `GPRegModel` / `GPRegModelN` | smooth curve + band (Gp/GpRff) | This page ↓ |
| Penalized (Ridge/Lasso/EN/MCP/SCAD/Adaptive/Group) | `df \|-> ridge \|lasso \|regularized cfg ["x1","x2"] "y"` | `RegModel` | coefficient bar (CV-selected λ) | This page ↓ |

---

## Plot Convention: Difference Between `toPlot fit` and `toPlot (statModel fit)`

Fitting results can be plotted **two ways**. **This chapter unifies on the `statModel` path**:

| Way | Path | Works With |
|---|---|---|
| `toPlot fit` | **Training-point path** — sorted training x, simple line+band version | Default CI only |
| `toPlot (statModel fit)` | **Grid path** — evaluation on uniform grid | `grid N` (density) / `bandMode` (CI/PI/both/none) / `piMethod` (closed-form or bootstrap) |

For straight lines (LM), both look identical, but **band type switching / prediction intervals / grid density only work
via the `statModel` path**. When in doubt, wrap with `statModel`. When overlaying scatter, use
`df |>> (layer (scatter "x" "y") <> toPlot (statModel fit))` form.

---

## Band Selection (`bandMode`: CI / PI / Both / None)

The band around a regression line is controlled by a single `bandMode`. The default is **CI (confidence interval)**,
representing uncertainty in the regression line = conditional mean E[y|x]. You can switch to **PI (prediction interval)**
— the band containing a new individual observation y (wider than CI by the residual variance σ²) — or overlay both nested:

```haskell
data BandMode = BandOff | BandCI | BandPI | BandCIPI   -- default BandCI
bandMode :: BandMode -> ModelSpec

statModel m                       -- default = CI
statModel m <> bandMode BandPI    -- prediction interval only
statModel m <> bandMode BandCIPI  -- CI + PI nested (fan chart)
statModel m <> bandMode BandOff   -- no band
```

`BandCIPI` nests outer PI (light) / inner CI (dark) / center regression line:

```haskell
saveSVGBound "fan.svg" $ df |>> layer (scatter "x" "y")
  <> toPlot (statModel (df |-> lm "x" "y") <> grid 200 <> bandMode BandCIPI)
```

![CI + PI nested (fan chart)](../images/band-cipi-fan.svg)

> PI (outer) always contains CI (inner) (verified PI ⊃ CI pointwise). CI and PI match statsmodels OLS
> `get_prediction().summary_frame()` `mean_ci` / `obs_ci` (cross-checked).
>
> **Models with PI (closed-form)**: `LMModel` / `WeightedLMModel` / Gaussian-identity `GLMModel` /
> `SplineModel` (basis-space OLS) / multiple regression effect plot (`lmMulti` `MultiLMModel`).
> Models without closed-form PI (non-Gaussian GLM / `RobustModel` etc.) default to closed-form and fall back to CI,
> but you can obtain PI via **bootstrap** (see below).

## Bootstrap CI/PI (`piMethod`)

The **computation method** for bands is selected via `piMethod`. Whereas `bandMode` chooses "which band to draw (CI/PI/both/none)",
`piMethod` chooses "how to compute it" — an **orthogonal axis**. Default is `PIClosedForm` (Wald / basis-space OLS).
Specifying `PIBootstrap seed draws` uses **case-resampling bootstrap** for bands: resample training (x,y) with seed → refit → predict on grid, repeated draws times. Quantiles of predictions form the band. This lets you obtain PI for models without closed-form PI (non-Gaussian GLM / robust):

```haskell
data PIMethod = PIClosedForm | PIBootstrap Word32 Int   -- default PIClosedForm
piMethod :: PIMethod -> ModelSpec

-- Poisson GLM CI+PI via bootstrap (seed 42/1000 draws):
df |>> layer (scatter "x" "y")
  <> toPlot (statModel (df |-> glm Poisson Log "x" "y")
              <> bandMode BandCIPI <> piMethod (PIBootstrap 42 1000))
```

![Poisson GLM bootstrap CI+PI](../images/bootstrap-poisson-pi.svg)

- **CI** = quantiles of resampled μ̂ (reflects coefficient uncertainty).
- **PI** = quantiles of new observation y*. For additive-error models (LM/spline/rlm), μ + resampled residuals;
  **for GLM, parametric draw from `Family(μ)`** (Poisson's discrete, asymmetric tail captured correctly).
- `PIBootstrap seed _` is **seed-pure and deterministic** (same seed → bit-identical bands). Compatible models =
  `LMModel` / `GLMModel` / `SplineModel` / `RobustModel` (has `svBootKit` implementation). Refitting draws times is heavier than closed-form.

> ⚠ **Small-sample CI tends to underestimate**: Nonparametric case-resampling narrows bootstrap CI at small n
> (observed: Poisson n=12 yields bootstrap CI 1/2.5–1/4 the width of closed-form Wald CI).
> **For models with closed-form CI** (LM / Gaussian GLM / spline), the default Wald CI is more trustworthy at small n.
> **Primary use of `PIBootstrap` is PI for models without closed-form** (non-Gaussian GLM / robust).
> If using bootstrap for CI, ensure n is large enough.

---

## Linear Regression (LM)

```haskell
lm :: Text -> Text -> LMSpec           -- lm <xcol> <ycol>
```

```haskell
let fit = df |-> lm "x" "y"            -- LMModel: β, ŷ, residuals, R²
saveSVGBound "lm.svg" $ df |>> layer (scatter "x" "y") <> toPlot (statModel fit)
```

![Scatter + regression line + CI band](../images/lm-scatter-ci.svg)

**Low-level** (matrix API): `fitLMVec (designMatrix xs) ys :: FitResult` ([01 quickstart](01-quickstart.md#low-level)).

---

## Multiple Regression

**Multiple regression** fits with multiple explanatory variables (design matrix X is wide).
It's the **multivariate extension of LM**, accepting **column name lists directly in a unified API**.
GLM / robust / quantile / GAM / GP / penalized and other models below **each have a multivariate extension** (`*Multi`)
with the same pattern:

```haskell
lmMulti     :: [Text] -> Text -> LMMultiSpec                       -- multivariate LM
glmMulti    :: Family -> LinkFn -> [Text] -> Text -> GLMMultiSpec  -- multivariate GLM
rlmMulti :: RobustEstimator -> [Text] -> Text -> RobustMultiSpec -- multivariate robust
```

```haskell
let m = df |-> lmMulti ["x1", "x2", "x3"] "y"   -- MultiLMModel (multiple)
```

The design matrix is `[1, x1, x2, x3]`. GLM and robust also multivariate with the same column name list notation
(`glmMulti Poisson Log [...] "y"` etc.).

**Low-level** (matrix API): `fitMultiLM (X :: Matrix) (y :: Vector) :: MultiFit` (→ [04 multivariate](04-multivariate.md)).

### Coefficient Summary (`coefSummary`)

`coefSummary` gives a coefficient table (estimate / standard error / test statistic / p-value / 95% CI) equivalent to statsmodels `.summary()`. OLS-class uses **t** (df = n−p), GLM/robust uses **z** (normal) for inference:

```haskell
coefSummary :: HasCoefSummary m => m -> [CoefRow]

data CoefRow = CoefRow
  { crTerm     :: Text             -- "(Intercept)" / variable name
  , crEstimate :: Double           -- point estimate β̂
  , crStdErr   :: Double           -- standard error
  , crStat     :: Double           -- test statistic (OLS=t, GLM/robust=z)
  , crPValue   :: Double           -- two-sided p-value
  , crCI95     :: (Double, Double)  -- 95% confidence interval
  }

mapM_ print (coefSummary m)       -- print one coefficient per line
```

`HasCoefSummary` instances: `LMModel`/`MultiLMModel`/`WeightedLMModel`/`GLMModel`/
`MultiGLMModel`/`RobustModel`/`MultiRobustModel` (both univariate / multivariate). These are all
**analytically Wald**, statistically sound. Remaining regressions use different coefficient diagnostics, so separate APIs (next section).

### Coefficient Diagnosis for Models Without Analytic Wald

Quantile / penalized / smoothing models don't satisfy `coefSummary`'s analytic Wald assumptions, so separate diagnosis APIs exist:

| Model | Reason `coefSummary` Not Applicable | Use Instead |
|---|---|---|
| rq / rqMulti | Coefficients interpretable but **no analytic SE** | `coefSummaryBoot` (bootstrap SE/CI) |
| Penalized (`RegModel`) | Coefficients interpretable but **Wald SE invalid** (post-selection) | `coefSummaryBoot` (interval only / significance not applicable) |
| GAM / spline | SE exists but **basis coefficients non-interpretable** | `termSummary` (approximate significance by term) |
| GP | **No linear coefficients** (hyperparameters only) | N/A |

```haskell
-- Bootstrap coefficient summary (return type same as coefSummary [CoefRow]).
-- Fixed seed makes it pure/reproducible. Meaning of crStat/crPValue/crCI95 is
-- "empirical distribution" (= percentile CI spans zero?).
coefSummaryBoot :: HasCoefBoot m => Word32 -> Int -> m -> [CoefRow]
mapM_ print (coefSummaryBoot 42 2000 qm)   -- bootstrap coefficient table for quantile regression

-- Smoothing-term-wise approximate significance (mgcv-style edf + approx F). Evaluates "terms" not basis coefs.
termSummary :: HasTermSummary m => m -> [TermRow]
mapM_ print (termSummary gamFit)           -- each smooth term's edf / F / p

data TermRow = TermRow
  { teTerm :: Text, teEdf :: Double, teStat :: Double, tePValue :: Double }
```

> `coefSummaryBoot` is a **separate function name** to signal bootstrap origin
> (return type is still `[CoefRow]`). For penalized regression, intervals are available
> but **due to post-selection, don't interpret as "significance"**.

### Unified Entry Point (`.summary()` style)

For any model, call `modelReport` to get appropriate diagnostics without worrying about the distinctions above.
Output "shape" differs, so it's wrapped in a tagged union `ModelReport`, displayed via `showReport`:

```haskell
modelReport :: HasReport m => m -> ModelReport
showReport  :: ModelReport -> Text

data ModelReport
  = CoefReport [CoefRow]   -- LM=Wald / rq/penalized=bootstrap
  | TermReport [TermRow]   -- GAM / spline term significance
  | NoReport   Text        -- GP etc., "coefficient diagnosis N/A", reason given

TIO.putStr (showReport (modelReport m))    -- statsmodels .summary()-style table
```

### Coefficient Forest Plot (`coefForest`)

Convert the `coefSummary` coefficient table straight to a plot. Each coefficient shown as point (estimate) + horizontal bar (95% CI),
with vertical reference line at 0 (= no effect) (same shape as meta-analysis forest plot):

```haskell
coefForest :: HasCoefSummary m => m -> VisualSpec

saveSVGBound "coef-forest.svg" $ noDf |>> coefForest m   -- m = y ~ x1 + x2 multiple
```

![Coefficient forest (95% CI / zero reference line)](../images/coef-forest.svg)

### Observed vs Predicted Plot (`obsVsPred`)

Diagnostic plot at a glance showing goodness-of-fit. x=observed / y=predicted scatter with `y = x` reference line (gray dashed).
Points close to the line have small residuals:

```haskell
obsVsPred :: HasObsPred m => m -> VisualSpec

saveSVGBound "obs-vs-pred.svg" $ noDf |>> obsVsPred m
```

![Observed vs predicted (y=x reference line)](../images/obs-vs-pred.svg)

`HasObsPred` instances: regression with predictions (LM/multiple/GLM/WLS/robust/quantile(median)/spline/GAM/penalized).
Observed reconstructed as `fitted + residual`. If you need raw (observed, predicted) pairs, call `obsPredPairs` directly.

### Partial Effect Plot (effect plot)

`MultiLMModel` is **not `Plottable`**, so effects are drawn via `statModelMulti` path
(Phase 68 pitfall). Use `along` to select the varying variable, and `holdAt` to fix others:

```haskell
-- Vary x1, fix x2/x3 at median, partial effect + 95% CI band (holdAt Median):
-- Band defaults to CI. Switch via earlier bandMode (BandOff/BandCI/BandPI/BandCIPI).
saveSVGBound "effect.svg" $ df |>> layer (scatter "x1" "y")
  <> toPlot (statModelMulti m (along "x1") <> holdAt Median)
```

![Multiple regression partial effect (x2 fixed at median)](../images/effect-holdat-median.svg)

`byVar` color-codes curves by levels of a second variable (interaction visualization equivalent):

```haskell
saveSVGBound "effect-byvar.svg" $ df |>> layer (scatter "x1" "y")
  <> toPlot (statModelMulti m (along "x1") <> byVar "x2" [1, 5])
```

![Effect by x2 levels (x2=1 / x2=5)](../images/effect-byvar.svg)

### Multivariate Versions of Other Models (Multi)

Beyond `lmMulti`, these models also have **column-name-list multivariate versions**:

- `glm` / `rlm` → `glmMulti` / `rlmMulti` (same column name list notation as this section)
- `rq` → `rqMulti` (multivariate quantile regression / each τ fit to design matrix `[1,x₁..xₚ]`)
- `gam` / `gp` → `gamMulti` / `gpMulti` (partial dependence curves for multiple predictors)
- Penalized regression (`ridge`/`lasso`/…) are **inherently multivariate** (bare takes `[Text]`)
- **`spline` only has univariate high-level**. Smoothing multiple predictors uses additive model `gamMulti`
  (sum of multiple 1D smooths), interactions are tensor-product splines (not yet implemented).

---

## Generalized Linear Model (GLM)

```haskell
glm :: Family -> LinkFn -> Text -> Text -> GLMSpec
-- Family = Gaussian | Binomial | Poisson
-- LinkFn = Identity | Log | Logit | Sqrt
```

```haskell
let fit = df |-> glm Poisson Log "x" "y"   -- GLMModel (canonical link = Log)
saveSVGBound "poisson.svg" $ df |>> layer (scatter "x" "y") <> toPlot (statModel fit)
```

![Poisson GLM mean + CI](../images/glm-poisson-ci.svg)

Logistic regression is `df |-> glm Binomial Logit "x" "y"`. Multiple regression via `glmMulti`.

**Low-level** (matrix API): `fitGLMFull fam link (designMatrix xs) ys :: FitResult` (→ [02-glm](../regression/02-glm.md)).

---

## Weighted Least Squares (WLS)

```haskell
weighted :: Text -> LMSpec -> WeightedLMSpec   -- weight column wCol to LM
```

```haskell
let fit = df |-> weighted "w" (lm "x" "y")   -- WeightedLMModel (√wᵢ-safe wrapper)
saveSVGBound "wls.svg" $ df |>> layer (scatter "x" "y") <> toPlot (statModel fit)
```

![WLS scatter + regression line + CI band (tighter on higher-weight side)](../images/weighted-wls.svg)

Weights `wᵢ` are **columns in df** passed by name (`x` / `y` same data source / row order auto-aligned).
Store reciprocal variance etc. in weight column (statsmodels `wls(…, weights=col)` / R `weights=` equivalent).
Normally plot via `toPlot (statModel …)` like LM (CI band tightens on high-weight observations).

**Low-level**: Scaled design matrix `diag(√w)·X` and `fitLMVec` (this wrapper does it internally).

---

## Robust Regression

```haskell
rlm :: RobustEstimator -> Text -> Text -> RobustSpec
-- RobustEstimator = Huber Double | Tukey Double   (k=1.345 / c=4.685 for 95% efficiency)
```

`RobustModel` also **has CI bands** (Phase 70.C). M-estimator β̂'s asymptotic covariance (sandwich / matches
statsmodels `RLM` cov="H1") yields Wald confidence intervals. Bands appear by default. Below, outlier effect is shown vs OLS
(color/fill via `statColor`/`statFill` / legend via `statLabel`. Colors use named colors from `Hgg.Plot.Color.Named`
like `red` / `blue`):

```haskell
let mH = df |-> rlm (Huber defaultHuberK) "x" "y"   -- RobustModel (CI band included)
    ols = df |-> lm "x" "y"                            -- LMModel (OLS)
saveSVGBound "robust.svg" $ df |>> layer (scatter "x" "y")
  <> toPlot (statModel ols <> statColor blue <> statFill blue <> statLabel "OLS")
  <> toPlot (statModel mH  <> statColor red  <> statFill red  <> statLabel "Robust (Huber)")
```

![Robust regression vs OLS](../images/robust-vs-ols.svg)

> The outlier at the end pulls OLS (blue) down, **CI band also widens**, but robust (red) maintains slope ≈ 2,
> **CI band stays tight** (downweighting outlier makes estimates more confident). SE matches statsmodels
> `RLM` (in limit of all inliers, precisely matches OLS SE per regression test).

**Low-level** (matrix API): `fitRobustLM est (designMatrix xs) ys maxIter tol :: RobustFit` — IRLS fit.
Get coefficient / robust scale / convergence / final weights via `Rob.rfCoef` / `rfScale` / `rfConverged` / `rfWeights`
(`rfWeights` useful for outlier identification). IRLS procedure / Huber/Tukey weight functions / initial-value pitfalls are primary references in
[usage-regularized-advanced](../regression/usage-regularized-advanced.ja.md). Multiple regression via `rlmMulti`.

---

## Quantile Regression

```haskell
rq :: [Double] -> Text -> Text -> QuantileSpec   -- multiple τ at once
```

```haskell
let m = df |-> rq [0.1, 0.5, 0.9] "x" "y"   -- QuantileModel
saveSVGBound "quantile.svg" $ df |>> layer (scatter "x" "y") <> toPlot (statModel m)
```

![Quantile regression lines by τ](../images/quantile-lines.svg)

Multivariate version `rqMulti` documented in [multiple regression](#multiple-regression) section.

**Low-level** (matrix API): `fitQuantile τ (X :: Matrix) (y :: Vector) :: QRFit` (→ [06-quantile](../regression/06-quantile.md)).
Quantile regression lacks SE, so `coefSummary` not applicable.

---

## Spline

```haskell
spline :: SplineKind -> [Double] -> Text -> Text -> SplineSpec
-- SplineKind = BSpline Int | NaturalCubic    second arg = interior knot list
```

```haskell
let m = df |-> spline (BSpline 3) [3,5,7] "x" "y"   -- SplineModel (cubic B-spline)
saveSVGBound "spline.svg" $ df |>> layer (scatter "x" "y") <> toPlot (statModel m)
```

![Spline smoothing + CI](../images/spline-smooth-ci.svg)

**Low-level** (matrix API): `fitSpline kind knots (xs :: Vector) (ys :: Vector) :: SplineFit` (→ [04-spline](../regression/04-spline.md)).
Splines are inherently univariate basis expansion (multiple predictors use additive model `gamMulti`).

---

## GAM (Generalized Additive Model)

`gam` / `gamMulti` are **basis-selectable**, **λ auto-selected by GCV**, and symmetric in naming with `lm` / `lmMulti`:

```haskell
gam      :: GAMConfig -> Text   -> Text -> GAMSpec   -- univariate (lm "x" "y"        parallel)
gamMulti :: GAMConfig -> [Text] -> Text -> GAMSpec   -- multivariate   (lmMulti […] "y"   parallel)
-- Both fit via df|-> → GAMModelN (Plottable). Multiple predictors plot partial dependence,
-- first predictor on x-axis, others at training mean.

data GAMConfig = GAMConfig { gcBasis :: GAMBasis, gcLambda :: GAMLambda }
defaultGAMConfig = GAMConfig (BSplineB 3 6) GCV    -- cubic B-spline/6 interior knots/GCV
```

Available bases `GAMBasis`:

| Constructor | Basis | Args |
|---|---|---|
| `BSplineB deg nKnots` | B-spline | degree / interior knot count |
| `NaturalCubicB nKnots` | Natural cubic regression spline | interior knot count |
| `PolyB deg` | Polynomial (scaled to `[-1,1]`) | degree |
| `FourierB nHarm` | Fourier (sin/cos) | harmonic count (period=data range) |
| `RBFB nCenters bwRel` | Gaussian RBF | center count / bandwidth (center spacing×bwRel) |

λ strategy `GAMLambda`: `FixedL λ` (manual) / `GCV` (generalized cross-validation `λ* = argmin n·RSS/(n−edf)²`).

GAM has **Bayesian confidence bands per mgcv** (`Vβ = (XᵀX+λP)⁻¹·φ̂`、`φ̂ = RSS/(n−edf)`、
half-width `t_{n−edf}·√(b Vβ bᵀ)`). `toPlot (statModel m)` renders smooth curve + CI band:

```haskell
let m = df |-> gam defaultGAMConfig "x" "y"   -- cubic B-spline/GCV auto λ
saveSVGBound "gam.svg" $ df |>> layer (scatter "x" "y") <> toPlot (statModel m)
```

![GAM smooth curve + CI band (mgcv-style Bayesian)](../images/gam-smooth.svg)

When comparing curve shapes across bases, suppress bands with `bandMode BandOff` for overlay:

```haskell
-- Same data smoothed by 3 bases, color-coded (each GCV λ, bands off for comparison):
let bs  = df |-> gam (GAMConfig (BSplineB 3 8)    GCV) "x" "y"
    nc  = df |-> gam (GAMConfig (NaturalCubicB 8) GCV) "x" "y"
    rbf = df |-> gam (GAMConfig (RBFB 10 1.0)     GCV) "x" "y"
saveSVGBound "gam-basis.svg" $ df |>> layer (scatter "x" "y")
  <> toPlot (statModel bs  <> statColor blue  <> statLabel "B-spline"     <> bandMode BandOff)
  <> toPlot (statModel nc  <> statColor green <> statLabel "Natural cubic" <> bandMode BandOff)
  <> toPlot (statModel rbf <> statColor red   <> statLabel "RBF"           <> bandMode BandOff)
```

![GAM basis comparison (B-spline / natural cubic / RBF / GCV auto λ)](../images/gam-basis-compare.svg)

For multiple predictors, partial dependence is plotted with first predictor on x-axis, others at training mean, with CI band at that point.
PI (prediction interval) is not offered, so `bandMode BandPI` falls back to CI.

**Low-level** (matrix API): `fitGAM deg nKnots λ [x₁,x₂,…] y :: GAMFit` (multiple features / `Plottable` not applicable).
Univariate raw plotting wraps with `gamModel` to `GAMModel` (CI band included).

---

## Kernel Method (GP / KRR / RFF) — `gp` / `gpMulti`

GP / Kernel Ridge Regression (KRR) / Random Fourier Features (RFF) are **one family exhausted by 2 axes**.
GP is the core; KRR prediction ≡ GP posterior mean (no band); RFF is a low-rank approximation of both.
So we unify into one spec `gp` / `gpMulti` symmetric with `lm` / `lmMulti`, selecting quadrant / kernel / hyperparameter strategy via `GPConfig`:

|  | Distribution (`Gp`) | No distribution (`Krr` points) |
|---|---|---|
| **Exact** | GP = mean + band | KRR = points |
| **Approximate (RFF)** | RFF-GP = mean + band | RFF-KRR = points |

```haskell
data Kernel       = RBF | Matern52 | Periodic                            -- kernel type (from GP.hs)
data GPMethod     = Gp | Krr | GpRff Int Word32 | KrrRff Int Word32  -- quadrant (D, seed for RFF only)
data HyperStrategy = FixedHyper GPParams | AutoMarginalLik | AutoCV       -- hyperparameter selection
data GPConfig      = GPConfig { gpcKernel :: Kernel, gpcMethod :: GPMethod, gpcHyper :: HyperStrategy }
defaultGP = GPConfig RBF Gp AutoMarginalLik     -- exact GP/RBF/marginal likelihood auto

gp      :: GPConfig -> Text   -> Text -> GPSpec        -- univariate (GPRegModel)
gpMulti :: GPConfig -> [Text] -> Text -> GPMultiSpec   -- multivariate (GPRegModelN/partial dependence)
```

Hyperparameter entity `GPParams` (`FixedHyper` for manual、`Auto*` auto-inferred):

```haskell
data GPParams = GPParams
  { gpLengthScale  :: Double                  -- ℓ  : length scale (larger = smoother)
  , gpSignalVar    :: Double                  -- σ_f²: signal variance (function value range)
  , gpNoiseVar     :: Double                  -- σ_n²: observation noise variance (= KRR penalty λ、0 = interpolation)
  , gpPeriod       :: Double                  -- p  : period (Periodic kernel only)
  , gpLengthScales :: Maybe (Vector Double)   -- ARD: per-dimension ℓ (Just = learn input importance)
  }

defaultGPParams :: GPParams                                -- ℓ=σ_f²=p=1, σ_n²=0.1
initParamsFromData :: [Double] -> [Double] -> GPParams     -- data stats → init (Auto* starting point)
```

```haskell
-- Exact GP (default): scatter + posterior mean curve + credible band
let m = df |-> gp defaultGP "x" "y"
saveSVGBound "gp.svg" $ df |>> layer (scatter "x" "y") <> toPlot (statModel m)
```

![Exact GP (Gp): posterior mean + credible band](../images/gp-exact-ci.svg)

`Krr` quadrant is KRR point prediction (no band), `GpRff D seed` is RFF approximation (same seed = full reproducibility / nearly matches exact GP):

```haskell
df |-> gp (GPConfig RBF Krr      AutoMarginalLik) "x" "y"   -- KRR (points/no band)
df |-> gp (GPConfig RBF (GpRff 500 42) AutoMarginalLik) "x" "y"   -- RFF approx GP (band)
df |-> gp (GPConfig RBF Gp AutoCV)                  "x" "y"   -- LOOCV for hyperparams
```

![KRR (Krr): point prediction / no band](../images/gp-ridge-point.svg)

![RFF approx GP (GpRff): band present / nearly matches exact GP](../images/gp-rff-ci.svg)

Hyperparameter optimization exposed via `gpcHyper`: `AutoMarginalLik` (marginal likelihood) / `AutoCV` (LOOCV/PRESS) / 
`FixedHyper` (manual). RFF only for stationary kernels, so `Periodic` + RFF quadrant auto-falls back to exact.
Low-level APIs (`fitGP` / `optimizeGP` / `fitGPMV` / `kernelRidge` /
`rffRidge` etc.) preserved. Details / 4-quadrant trade-offs in
[04-gp](../regression/04-gp.md) / [04-kernel](../regression/04-kernel.md) /
[04-rff](../regression/04-rff.md).

> ⚠ **Kernel and `GPParams` field correspondence**: Each kernel reads only its own fields,
> **silently ignores** irrelevant fields (no validation / warning). RBF/Matern52 ignore `gpPeriod` and
> `gpLengthScales` (ARD), Periodic ignores ARD, ARD only in MV path (not 1D).
> **Note**: `Auto*` (marginal likelihood/CV) optimize only `gpLengthScale` / `gpSignalVar` /
> `gpNoiseVar`. **`Periodic`'s `gpPeriod` is not learned**, so use true period via `FixedHyper (defaultGPParams { gpPeriod = actual_period })`.

---

## Penalized Regression (Ridge / Lasso / Elastic Net / MCP / SCAD / Adaptive / Group)

7 implemented **penalized methods unified into one spec `regularized`** (+ shortcuts `ridge`/`lasso`/`elasticNet`).
Symmetric with `lmMulti`, **multivariate only** (penalization inherently multi-feature / no univariate version / 
bare takes `[Text]` and `*Multi` is the same name). Like `GPConfig`, **method × λ strategy chosen via
`RegConfig`**. X internally standardized, **coefficients returned in original scale**:

```haskell
data RegMethod
  = Ridge | Lasso | ElasticNet Double      -- L2 / L1 / EN(α=L1 ratio)
  | MCP Double | SCAD Double               -- nonconvex (γ / a)
  | AdaptiveLasso Double | GroupLasso [Int] -- OLS pilot weights(γ) / group IDs per column

data LambdaStrat                           -- λ selection
  = FixedLambda Double                     -- manual
  | LambdaLOOCV                            -- closed-form LOOCV (★linear smoothers only)
  | LambdaCV Int Word32 | LambdaCV1SE Int Word32  -- k-fold CV (k, seed)/best / 1-SE rule

data RegConfig = RegConfig { rcMethod :: RegMethod, rcLambda :: LambdaStrat }

regularized :: RegConfig -> [Text] -> Text -> RegSpec        -- general form
ridge, lasso :: [Text] -> Text -> RegSpec                    -- shortcuts (λ default CV)
elasticNet   :: Double -> [Text] -> Text -> RegSpec
```

Penalty method `RegMethod`:

| Constructor | Penalty | Args |
|---|---|---|
| `Ridge` | L2 `Σβ²` (variance shrinkage / coefficients not zero) | — |
| `Lasso` | L1 `Σ\|β\|` (**variable selection** / shrink to zero) | — |
| `ElasticNet α` | L1+L2 mix | α = L1 ratio (0–1) |
| `MCP γ` | Nonconvex (minimax concave) | γ (relaxation parameter) |
| `SCAD a` | Nonconvex (smoothly clipped) | a |
| `AdaptiveLasso γ` | Weighted L1 (OLS pilot weights) | γ (weight exponent) |
| `GroupLasso gids` | Group-wise L1 | group IDs per column |

λ strategy `LambdaStrat`:

| Constructor | λ Selection | Notes |
|---|---|---|
| `FixedLambda λ` | Manual fixed | — |
| `LambdaLOOCV` | Closed-form LOOCV | Ridge etc. linear smoothers only (L1/nonconvex → `Left`) |
| `LambdaCV k seed` | k-fold CV (best) | seed pure / deterministic |
| `LambdaCV1SE k seed` | k-fold CV (1-SE rule) | R glmnet `lambda.1se`-like / conservative |

Fit via `df |-> ridge/lasso/…`, `RegModel` is `Plottable` (coefficient bar / feature name labels).

```haskell
-- λ auto-selected by 5-fold CV (default). Coefficient bar
let m = df |-> lasso ["x1", "x2", "x3", "x4", "x5"] "y"
saveSVGBound "coef.svg" $ noDf |>> toPlot m
```

**Lasso performs variable selection** (irrelevant / redundant coefficients → 0) / **Ridge shrinks variance** (shared among correlated features,
not zero). Difference shows in coefficient bar (true signal = x1, x2、x3 redundant (correlated with x1)、
x4/x5 pure noise):

![Lasso (left / sparse selection) ↔ Ridge (right / variance shrinkage) coefficient comparison (CV-selected λ)](../images/regularized-coefs.svg)

> True signal x1, x2. x3 redundant (correlated with x1), x4/x5 pure noise. **Lasso (left)** zeros x3-x5 and selects x1,x2 (sparse),
> **Ridge (right)** distributes weight to redundant x3.

**λ strategy**: `LambdaLOOCV` is closed-form LOOCV (Ridge etc. linear smoothers only / L1/nonconvex → `Left`),
`LambdaCV`/`LambdaCV1SE` are k-fold CV (seed pure / latter is R glmnet `lambda.1se`-like conservative). Even nonconvex (MCP/SCAD) or Group/Adaptive use same CV path to select λ:

```haskell
df |-> regularized (RegConfig Ridge LambdaLOOCV)            cols "y"   -- closed-form LOOCV
df |-> regularized (RegConfig (ElasticNet 0.5) (LambdaCV1SE 10 1)) cols "y"
df |-> regularized (RegConfig (GroupLasso [0,0,1,1,2]) (FixedLambda 0.1)) cols "y"
```

**Low-level** matrix API (`fitRegularized` / `fitRidge` / `fitLasso` / `selectLambdaCV` etc.) preserved ([04-regularized](../regression/04-regularized.md)). Advanced penalties (MCP/SCAD/Adaptive/Group)
in `Hanalyze.Model.RegularizedAdvanced` with individual matrix-level fit — all return `RegFit` (`Plottable` / coefficient bar):

```haskell
adaptiveWeightsFromOLS :: Double -> Matrix -> Vector -> Vector            -- γ, X, y → OLS pilot weights
fitAdaptiveLasso :: Double -> Vector -> Matrix -> Vector -> Int -> Double -> RegFit  -- λ, w, X, y, maxIter, tol
fitMCP           :: Double -> Double -> Matrix -> Vector -> Int -> Double -> RegFit  -- λ, γ(=3-5)
fitSCAD          :: Double -> Double -> Matrix -> Vector -> Int -> Double -> RegFit  -- λ, a(=3.7)
fitGroupLasso    :: Double -> [[Int]] -> Matrix -> Vector -> Int -> Double -> RegFit -- λ, column index groups
```

Penalty mathematics (MCP/SCAD piecewise / oracle property) / convexity pitfalls are primary references in
[usage-regularized-advanced](../regression/usage-regularized-advanced.ja.md).

### Regularization Path (coefficient path)

The **regularization path** plots each coefficient value as a function of **λ (penalty strength)** as lines.
As λ increases, coefficients shrink; **Lasso shrinks coefficients to exactly 0 one at a time** — their **dropout order**
indicates variable importance. Horizontal axis is **log₁₀λ** (left=small λ=full model, right=large λ=sparse / R glmnet convention).
Obtained via low-level `regularizationPath` (compute coefficients at λ grid) + `regPathPlot` (plot):

```haskell
import Hanalyze.Model.Regularized (regularizationPath, Penalty (L1))
import Hanalyze.Plot              (regPathPlot)

let lams = [ 0.001 * 1.6 ** k | k <- [0 .. 18] ]              -- λ grid (ascending/log₁₀≈ -3..1)
    path = regularizationPath (\l -> L1 l) lams xMatrix yVec  -- [(λ, [β_j])] = coeffs at each λ
saveSVGBound "lasso-path.svg" $ noDf |>> regPathPlot path <> title "LASSO coefficient path"
```

![Lasso regularization path (coefficient vs log₁₀λ / one-by-one to zero)](../images/lasso-path.svg)
