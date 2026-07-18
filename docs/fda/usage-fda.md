# Functional Data Analysis (FDA) (Phase 33)

Phase 33 (2026-05-29) adds `Hanalyze.Model.FDA`, which treats sensor /
process time series as **one observation = one function**
(Ramsay-Silverman style). Closes gap #16 — the last of the original 17,
so **the gap list is now empty**.

---

## 0. Overview

| Feature | API | Use |
|---|---|---|
| Smoothing | `smoothBasis` | B-spline + P-spline penalty function fit |
| Evaluation | `evalFunctional` | Evaluate the smoothed function on any grid |
| FPCA | `functionalPCA` | Functional principal components |
| Functional LM | `fLM` | y_i = α + ∫ x_i(t) β(t) dt + ε |

Only the B-spline basis is implemented. Fourier basis is left for a
future phase.

---

## 1. Smoothing

```haskell
import qualified Numeric.LinearAlgebra as LA
import qualified Hanalyze.Model.FDA   as FDA

let tGrid = LA.fromList [0, 0.02 .. 1.0]
    -- B-spline degree=3, knots include the boundaries
    basis = FDA.BSpline 3 [0, 0.1 .. 1.0]
    fits  = FDA.smoothBasis basis 1e-3 tGrid yMat

let yhat = FDA.evalFunctional (head fits) tGrid
```

Solution: `c = (BᵀB + λ DᵀD)⁻¹ Bᵀy`, where `D` is the second-difference
operator (Eilers-Marx 1996 P-spline). `λ → 0` interpolates; `λ → ∞`
over-smooths.

---

## 2. Functional PCA

```haskell
let pca = FDA.functionalPCA 3 fits
FDA.fpcaEigenvalues pca
FDA.fpcaEigenfn pca
FDA.fpcaScores pca
FDA.fpcaMeanFn pca
```

`FunctionalPCA` is `Plottable`: `toPlot` overlays the mean function and the
leading (up to 3) eigenfunctions on the grid — the figure below:

```haskell
import Hanalyze.Plot       (toPlot)
import Hgg.Plot.Spec          (ColData (..))
import Hgg.Plot.Frame         ((|>>))
import Hgg.Plot.Backend.SVG   (saveSVGBound)

let noDf = [] :: [(Text, ColData)]
saveSVGBound "fda-fpca.svg" (noDf |>> toPlot pca)
```

PCA on the basis-coefficient covariance, projected back to the grid.
Adequate for B-spline + dense grid; exact mass-matrix-weighted SVD is a
future extension.

The mean function and the leading eigenfunctions are themselves curves
over the grid. Plotting them together shows the mean shape plus the
dominant modes of variation:

![FPCA mean function and leading eigenfunctions (PC1/PC2/PC3 + mean)](../images/fda-fpca.svg)

---

## 3. Functional Linear Regression

```haskell
let flm = FDA.fLM fits ys lambdaBeta
FDA.flmAlpha flm
FDA.flmBetaFn flm
FDA.flmR2 flm
```

`FLMResult` is `Plottable`: `toPlot` draws the functional regression
coefficient curve β(t) over the grid:

```haskell
import Hanalyze.Plot       (toPlot)
import Hgg.Plot.Spec          (ColData (..))
import Hgg.Plot.Frame         ((|>>))
import Hgg.Plot.Backend.SVG   (saveSVGBound)

let noDf = [] :: [(Text, ColData)]
saveSVGBound "flm-beta.svg" (noDf |>> toPlot flm)
```

Model: `y_i = α + ∫ x_i(t) β(t) dt + ε`. β expanded in the same basis,
mass matrix `J ≈ trapezoidal(BᵀB)`, OLS plus a second-difference penalty
on β.

---

## 4. Caveats

- **`bsplineBasis` knot list includes boundaries**: from
  `Hanalyze.Model.Spline.bsplineBasis`'s contract. Passing interior-only
  knots gives wrong dimensions (tripped on this during the Phase 33
  initial test run).
- **Basis count**: `n_basis = length knots + degree - 1`. Increase
  basis count for curvy data, control over-fit with `λ`.
- **FPCA orthogonality assumption**: holds for B-spline + dense grid;
  for coarse grids prefer the weighted SVD variant (future work).
- **fLM with x ⟂ β**: if the integral vanishes by construction the
  outcome carries no signal — R² ≈ 0. Make sure x and β are not
  orthogonal by design (debugging note from the initial Phase 33 test).

---

## 5. References

- Plan: `specification/phases/phase-33-fda.md`
- Existing dependency: `Hanalyze.Model.Spline.bsplineBasis`
- Ramsay & Silverman (2005) "Functional Data Analysis" 2nd ed.
- Eilers, Marx (1996) "Flexible smoothing with B-splines and penalties"
  Statist. Sci. 11:89-121.
- Comparable: R `fda` package, Python `scikit-fda`
