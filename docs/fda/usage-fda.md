# Functional Data Analysis (FDA) (Phase 33)

> 🌐 **English** | [日本語](usage-fda.ja.md)

> Learning guide for `Hanalyze.Model.FDA` added in Phase 33 (2026-05-29).
> It treats sensor and process time series as **one observation = one function**
> in the Ramsay-Silverman style. Closes gap #16 — the last of the original 17 gaps,
> so **the gap list is now completely empty**. Type signatures and minimal examples are
> documented in [api-guide 04-multivariate](../api-guide/04-multivariate.md) as the primary reference.
> This guide covers **smoothing solution derivation, mass matrix, and knot convention traps**.

---

## 0. Overview

| Feature | Use |
|---|---|
| Basis smoothing (`smoothBasis`) | Estimate function with B-spline + P-spline penalty |
| Evaluation (`evalFunctional`) | Evaluate smoothed function on any grid |
| FPCA (`functionalPCA`) | Functional principal components (= SVD in function space) |
| Functional linear regression (`fLM`) | y_i = α + ∫ x_i(t) β(t) dt + ε |

Only B-spline basis is implemented. Fourier basis is a future phase candidate.

---

## 1. Smoothing Solution (smoothBasis)

Solution: `c = (BᵀB + λ DᵀD)⁻¹ Bᵀy`, where `D` is the second-difference operator
(Eilers-Marx 1996). `λ → 0` interpolates, `λ → ∞` over-smooths.

---

## 2. Functional PCA

PCA on the basis-coefficient covariance, with principal component functions evaluated on grid.
For B-spline + dense grid, orthogonal approximation is sufficiently practical. Exact mass-matrix
weighted version is a future extension. Mean function and leading eigenfunctions are both curves
on grid; plotting them together shows mean shape and dominant variation modes at a glance:

![FPCA mean function and leading eigenfunctions (PC1/PC2/PC3 + mean)](../images/fda-fpca.svg)

---

## 3. Functional Linear Regression (fLM)

Model: `y_i = α + ∫ x_i(t) β(t) dt + ε`. Expand β in the same basis,
OLS via mass matrix `J ≈ trapezoidal(BᵀB)`, with second-difference penalty on β.

---

## 4. Unexpected Behaviors to Watch

### `bsplineBasis` knot list includes boundaries

The knot list from `Hanalyze.Model.Spline.bsplineBasis` is the full knot sequence `[t_min, .., t_max]`.
Passing interior-only knots causes dimension mismatch (encountered during Phase 33 initial checkout).

### Choosing `n_basis`

`n_basis = length knots + degree - 1`. With `degree=3` and 12 knots, you get 14 basis functions.
Increase basis count for curvy data and use `λ` to suppress over-fitting — the standard approach
(Ramsay-Silverman 2005 §5).

### Is FPCA orthogonal approximation good enough?

For B-spline + dense grid (sampling interval << basis spacing), approximation error is small.
For **coarse grid** or **few basis** functions, exact mass-matrix-weighted SVD is needed.
This implementation assumes the former; the latter is a future extension.

### fLM with x and β orthogonal → R² ≈ 0

If `∫ x_i(t) β(t) dt = 0` always holds, then `y_i = α + noise` and R² ≈ 0.
When designing the data-generation process, verify that **β-correlated components** are present in x
(encountered as a debugging trap during initial checkout).

---

## 5. Related

- Types and minimal examples: [api-guide 04-multivariate](../api-guide/04-multivariate.md)
- Specification: `specification/phases/phase-33-fda.md`
- Existing dependency: `Hanalyze.Model.Spline.bsplineBasis` (B-spline basis generation)
- References:
  - Ramsay & Silverman (2005) "Functional Data Analysis" 2nd ed.
  - Eilers, Marx (1996) "Flexible smoothing with B-splines and penalties"
    Statist. Sci. 11:89-121.
- Comparisons: R `fda` package, Python `scikit-fda`
