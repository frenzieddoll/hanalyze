# Formula DSL — declare models as formulas

> How to declare a model as a **formula** with `Hanalyze.Model.Formula{,.Frame,.Design,.RFormula}`,
> turning it into a design matrix and a fitted linear model in one pipeline.
> See also: [01-lm.md](01-lm.md) (linear regression) / [04-spline.md](04-spline.md) (bases)

## Contents

1. [Overview (AST is canonical; two front-ends)](#1-overview)
2. [Quickstart](#2-quickstart)
3. [Native syntax (canonical front-end)](#3-native-syntax-canonical-front-end)
4. [R / patsy syntax (secondary front-end)](#4-r--patsy-syntax-secondary-front-end)
5. [Factors, interactions, basis expansion](#5-factors-interactions-basis-expansion)
6. [Linearity detection](#6-linearity-detection)
7. [Verification](#7-verification)
8. [Phase 47 features (missing policy / contrast / weights·offset / nonlinear)](#8-phase-47-features-missing-policy--contrast--weightsoffset--nonlinear)
9. [Remaining limitations](#9-remaining-limitations)

---

## 1. Overview

The canonical representation is the **`Formula` AST** — not a string, not a typed DSL. Two
front-ends parse into the same AST.

```
string (native or R) ──parse──▶ Formula AST ──modelFrame──▶ ModelFrame ──designMatrixF──▶ design matrix ──fitLM──▶ FitResult
```

- **Native, explicit-coefficient syntax = canonical front-end**: `"y x group = b0 + b1*x + bg ! group"`
  (`+`/`*` are real arithmetic — no false friends).
- **R / patsy syntax = secondary front-end**: `"y ~ x + C(group)"` (statsmodels-compatible; oracle).
- `parseModel` dispatches automatically based on whether the string contains `~`.

Key point: **for linear OLS the coefficient names do not affect the fit** (each design column gets
one coefficient). Names matter only for (1) reporting and (2) nonlinearity detection (a parameter
appearing in a nonlinear position is flagged).

## 2. Quickstart

The high-level verb `df |-> lmF "y ~ x"` fits a formula straight from any
[`ColumnSource`](../io/04-fit-api.md) and returns a `MultiLMModel`. The effect-plot
layer for a multivariate model comes from `statModelMulti m (along "x")` (a
`ModelSpec`, which is `Plottable` — predict along one variable, hold the rest);
`glmF` / `glmmF` cover GLM / mixed models:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Hanalyze.Plot     (lmF, glmF, glmmF, (|->), toPlot, statModelMulti, along, holdAt, HoldAgg (..))
import Hanalyze.Model.GLM (Family (..), LinkFn (..))
import Hgg.Plot.Spec        (ColData (..), layer, scatter)
import Hgg.Plot.Frame       ((|>>))
import Hgg.Plot.Backend.SVG (saveSVGBound)
import qualified Data.Vector as V

main :: IO ()
main = do
  let df = [ ("x", NumData (V.fromList [1,2,3,4,5]))
           , ("y", NumData (V.fromList [2.1,3.9,6.1,7.9,10.1])) ]
      m  = df |-> lmF "y ~ x"            -- MultiLMModel (R-style formula)
  -- df |-> glmF Poisson Log "y ~ x1 + x2"  -- formula GLM   → MultiGLMModel
  -- df |-> glmmF "y ~ x + (1|g)"           -- mixed model   → (GLMMResultRE, [Text])
  saveSVGBound "effect.svg"                -- effect plot: prediction along x, others held
    (df |>> (layer (scatter "x" "y") <> toPlot (statModelMulti m (along "x") <> holdAt Median)))
```

**Lower-level (AST + `fitLMF`)** — drive the parse / design-matrix / fit pipeline
explicitly when you need the `FitResult` and labels:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Hanalyze.Model.Formula          (parseFormula)
import Hanalyze.Model.Formula.RFormula (parseModel)
import Hanalyze.Model.Formula.Design   (fitLMF)
import Hanalyze.Model.Core             (coefficientsV)
import qualified DataFrame as DX
import qualified Numeric.LinearAlgebra as LA

main :: IO ()
main = do
  let df = DX.fromNamedColumns
             [ ("x", DX.fromList ([1,2,3,4,5] :: [Double]))
             , ("y", DX.fromList ([2.1,3.9,6.1,7.9,10.1] :: [Double])) ]
  case parseModel "y x = b0 + b1*x" >>= \f -> fitLMF f df of
    Left err         -> putStrLn err
    Right (fr, lbls) -> do
      print lbls                            -- ["b0","b1*x"]
      print (LA.toList (coefficientsV fr))  -- ~ [2.0e-2, 2.0]  (y ~ 2x)
```

The R syntax `parseModel "y ~ x"` gives the same result.

## 3. Native syntax (canonical front-end)

Form: `"<response> <data vars…> = <rhs expression>"`. The left-hand side declares the response and
data variables; **free names on the right that are not declared on the left are the parameters**.

Operator precedence (high → low): `!` index > `^` > unary `-` > `* /` > `+ -`.

| Written | Meaning |
|---|---|
| `b0` | intercept (constant term) |
| `b1*x` | slope of a continuous variable |
| `b2*log x` | function transform (`log`/`exp`/`sqrt`/`sin`/`cos`/`tan`/`abs`) |
| `b1*x^2` | power (x squared) |
| `b*x*z` | continuous×continuous interaction (real product) |

`parseFormula`/`prettyFormula` round-trip (`parse . pretty == id`).

## 4. R / patsy syntax (secondary front-end)

A string containing `~` is parsed as an R formula into the same AST.

| R syntax | Meaning |
|---|---|
| `y ~ x` | implicit intercept + x. `-1` / `0` removes the intercept |
| `y ~ C(g)` | treat `g` as categorical (factor) — use `C()` explicitly |
| `y ~ a:b` | interaction only / `y ~ a*b` | crossing (= `a + b + a:b`) |
| `y ~ x + I(x**2)` | `I(...)` is arithmetic (`**`/`^`) |
| `y ~ poly(x,2)` / `y ~ bs(x,5)` | polynomial / B-spline basis |

> Because parsing happens without data, mark categoricals **explicitly with `C(g)`** in R syntax
> (no column-type inference). Parameter names are synthesized internally (`_p0`, `_p1`, …).

## 5. Factors, interactions, basis expansion

The index operator `!` writes factors and bases uniformly. **Whether a variable is a factor is
determined by usage — "does it appear to the right of `!`"** — not by column type (so numeric-coded
factors are picked up too).

```haskell
-- factor main effect + factor×continuous (per-level slope)
parseFormula "y g x = b0 + bg ! g + bs ! g * x"

-- factor×factor (left-associative `!` chain = 2-D index)
parseFormula "y g t = b0 + bg!g + bt!t + bgt!g!t"

-- basis expansion
parseFormula "y x = b0 + bp ! poly(x,2)"     -- x^1, x^2
parseFormula "y x = bs ! bspline(x,5)"        -- degree-3 B-spline (knots = quantileKnots 5 x)
```

Identifiability uses **treatment contrast** (with an intercept, the reference level = first sorted
level is dropped for full rank). B-splines are a partition of unity, so the first basis column is
dropped when an intercept is present.

## 6. Linearity detection

If a parameter appears **inside** a data expression, the model is nonlinear and
`fitLMF` / `linearityCheck` return `Left` (OLS does not apply).

```haskell
linearityCheck (parse "y x = b0 + b1*x + b2*log x") df  -- Right () : linear
fitLMF          (parse "y x = a*exp(-b*x)")        df   -- Left "nonlinear: parameter 'b' …"
```

## 7. Verification

Using the principle that fitted values ŷ and R² are **parameterization-invariant** (independent of
contrast/basis choice), correctness is checked with Python-free oracles (the 4 promotion gates of
plan §3.6.2):

| # | Check | Result |
|---|---|---|
| ① | saturated factor×factor | ŷ = cell means; design matrix full rank |
| ② | basis expansion | `poly(x,2)` reproduces a quadratic exactly / `bspline(x,n)` ŷ = `fitSpline (BSpline 3)` |
| ③ | parser robustness | QuickCheck round-trip (`parse . pretty == id`) + golden precedence table |
| ④ | R oracle | same model in R/native syntax gives the same ŷ (5 cases) + **statsmodels cross-check 4/4 ALL PASS** (actually run) |

The statsmodels / scipy cross-check has been **run for real (Phase 47: 6 OLS + WLS + NLS, ALL PASS)**:
`bench/python/bench_formula.py` + `bench/python/formula_haskell_ref.json`.
> The Phase 46 cross-check caught an R² mismatch on `y ~ C(g) + C(g):x` and fixed a bug where the
> reference level was wrongly dropped for factor×continuous terms (the real payoff of an external
> oracle). Phase 47 additionally cross-checks `C(g, Sum)` ŷ/R², WLS coefficients (`smf.wls`), and
> NLS parameters (`scipy.curve_fit`) — all PASS. Reference values are reproducible
> (`cabal run formula-ref-gen` regenerates `formula_haskell_ref.json`).

```bash
# venv = repo root/.venv (numpy/pandas/statsmodels/scipy)
cabal run formula-ref-gen                                          # regenerate Haskell references
OPENBLAS_NUM_THREADS=1 ../.venv/bin/python bench/python/bench_formula.py
```

These tests are part of `cabal test hanalyze-test`.

## 8. Phase 47 features (missing policy / contrast / weights·offset / nonlinear)

On top of the linear core (§1-§7), four features for practical regression are implemented (Phase 47,
all cross-checked against statsmodels/scipy).

### 8.1 Missing-data policy (`MissingPolicy`)

`modelFrameWith :: MissingPolicy -> Formula -> DataFrame -> Either String ModelFrame`. NA detection /
dropping / imputation is centralized in ModelFrame (`modelFrame = modelFrameWith DropRows`).

- `DropRows` (default): drop rows with any NA in involved columns (backward compatible).
- `Impute ImputeMean` / `Impute ImputeMedian`: impute continuous predictors (response/factor NA need another policy).
- `TreatAsCategory`: treat factor NA as a dedicated level `"<NA>"`.
- `ErrorOnMissing`: `Left` with column names + counts if any NA.
- `Pairwise`: degrades to DropRows for linear OLS (single design matrix can't be formed).

### 8.2 Contrast coding (`ContrastCoding`)

Choose factor coding via `C(g, coding)` (both front-ends). Unannotated = `Treatment` (default).

```text
y g = b0 + bg ! C(g, Sum)          # native syntax
y ~ C(g, Helmert)                  # R syntax
```

- `Treatment` / `Sum` (sum-to-zero) / `Helmert` / `Polynomial` (orthogonal) / `CustomContrast` (k×(k-1) matrix via API).
- ŷ/R² are **parameterization invariant**, so the contrast only changes coefficient meaning, not the fit.
- factor×continuous (masked columns) keep full coding / all levels (per the Phase 46 pitfall).

### 8.3 weights / offset = WLS (`fitWLSF`)

`fitWLSF :: WLSConfig -> Formula -> DataFrame -> Either String (FitResult, [Text])`. Mirroring
statsmodels `smf.wls`, weights/offset are passed by column name (`WLSConfig {wcWeights, wcOffset}`).

- weights: scale X/y rows by `√w`, reducing to OLS (WLS).
- offset: fit `y − offset` (fixed addend to η; GLM offset is a separate path, not supported).

### 8.4 Nonlinear fitting = NLS (`fitNLS`)

`fitNLS :: Formula -> DataFrame -> [(Text, Double)] -> Either String NLSResult`
(`Hanalyze.Model.Formula.Nonlinear`). Turns the parsed AST into an evaluator and minimizes SSR via
Nelder-Mead.

```text
y x = a * exp(-b * x)              # §6 returns Left (nonlinear); fitNLS can fit it
```

- Initial values are required (NLS is sensitive to them). Factor indices are unsupported (use the linear side).
- Cross-checked against `scipy.curve_fit` (parameter recovery for `a*exp(-b*x)`).

### 8.5 Random effects = mixed-effects models (`fitMixedLME` / `fitMixedGLMM`, Phase 48)

`Hanalyze.Model.Formula.Mixed`. Adds lme4-style `(1|g)` (random intercept) / `(x|g)` /
`(1+x|g)` (random slope) to the formula and routes to the general random-effects fit in
`Hanalyze.Model.GLMM`.

```text
y ~ x + (1|g)        # random intercept
y ~ x + (1+x|g)      # random intercept + slope
y ~ x + (0+x|g)      # random slope only (intercept suppressed)
```

- `fitMixedLME :: Text -> DataFrame -> Either String (GLMMResultRE, [Text])` (Gaussian LME, EM).
- `fitMixedGLMM :: Family -> LinkFn -> Text -> DataFrame -> ...` (Binomial/Logit, Poisson/Log, Laplace).
- `GLMMResultRE` = fixed effects β + random-effect covariance `G` (r×r) + BLUPs (q×r) + residual variance.
- Frequentist GLMM, so **no prior** is declared on the random effects (the covariance `G` is estimated;
  use the HBM DSL for the Bayesian version). The implementation keeps `Term` unchanged and **extracts
  `(…|g)` blocks with a lexical pre-pass** (the fixed part reuses the existing `parseModel`/`designMatrixF`).
- Single grouping factor only for now (`(…|g1) + (…|g2)` is not supported).
- Cross-checked against `statsmodels smf.mixedlm(re_formula="~x").fit(reml=False)` (ML): β, covariance `G`,
  and σ² for a random-slope model all match (`bench/python/bench_formula.py`).

## 9. Remaining limitations

- **GLM offset** (e.g. Poisson log-exposure): only linear offset is implemented.
- random effects with **multiple grouping factors** (`(…|g1) + (…|g2)`): single group only.
- confidence band for `smooth` (B-spline) — point estimate only for now.

> For design rationale see [spec: analysis-language §2.1/§2.2/§2.4/§3.6](../../specification/spec/hanalyze-analysis-language-spec.md).
