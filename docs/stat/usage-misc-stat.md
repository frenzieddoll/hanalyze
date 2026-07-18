# Misc Stat extensions — Fit Y by X / Friedman / Cohen's d CI / LCA / Graphical Lasso (Phase 32)

Phase 32 (2026-05-29) adds **Correlation Network (Graphical Lasso)** and
**Latent Class Analysis**. The other three items originally listed in the
same NN doc — **Fit Y by X / Friedman + Dunn / Cohen's d CI** — turned
out to be already implemented in Phase 13. This page covers all five.

---

## 0. Overview

| Feature | API | Use |
|---|---|---|
| Fit Y by X | `Hanalyze.Model.FitYByX.fitYByX` | Auto-dispatch by var type (LM / GLM / ANOVA / chi²) |
| Friedman test | `Hanalyze.Stat.Test.friedmanTest` | Paired multi-group nonparametric |
| Dunn multiple comparisons | `Hanalyze.Stat.Test.dunnTest` | All-pairs follow-up with p-adjust |
| Cohen's d CI | `Hanalyze.Stat.Effect.cohenDCI` | Effect-size CI via non-central t |
| LCA | `Hanalyze.Model.LatentClassAnalysis.fitLCA` | Categorical latent class clustering (EM) |
| Graphical Lasso | `Hanalyze.Stat.CorrelationNetwork.graphicalLasso` | Sparse precision matrix (conditional-independence network) |

Before any of these, a quick look at the distribution of each variable
is worthwhile. A box plot summarises location and spread at a glance —
`describeBox` takes a raw `[Double]` column:

```haskell
import Hanalyze.Plot       (describeBox)
import Hgg.Plot.Spec          (ColData (..))
import Hgg.Plot.Frame         ((|>>))
import Hgg.Plot.Backend.SVG   (saveSVGBound)

let noDf = [] :: [(Text, ColData)]
saveSVGBound "describe-box.svg" (noDf |>> describeBox xs)
```

![Box plot of a variable's distribution (descriptive statistics)](../images/describe-box.svg)

---

## 1. Fit Y by X (Phase 13 existing)

```haskell
import qualified Hanalyze.Model.FitYByX as FXY
let r = FXY.fitYByX xVec FXY.Continuous yVec FXY.Continuous   -- LM
```

Dispatch table:

| X | Y | Analysis |
|---|---|---|
| Cont | Cont | Simple regression (LM) |
| Cont | Cat  | Logistic GLM |
| Cat  | Cont | One-way ANOVA |
| Cat  | Cat  | Chi-square independence |

---

## 2. Friedman + Dunn (Phase 13 existing)

```haskell
import qualified Hanalyze.Stat.Test as ST
let r  = ST.friedmanTest (LA.fromLists rows)
let mc = ST.dunnTest [g1, g2, g3]
```

`MultiCompareResult` carries `(i, j, p_raw, p_adj)` for every pair.

`friedmanTest` returns a `TestResult`, which is `Plottable` — `toPlot`
draws a one-row forest, and `testForest` lays out several tests together:

```haskell
import Hanalyze.Plot       (toPlot, testForest)
import Hgg.Plot.Spec          (ColData (..))
import Hgg.Plot.Frame         ((|>>))
import Hgg.Plot.Backend.SVG   (saveSVGBound)

let noDf = [] :: [(Text, ColData)]
saveSVGBound "friedman-forest.svg" (noDf |>> toPlot r)
```

---

## 3. Cohen's d CI (Phase 13 existing)

```haskell
import qualified Hanalyze.Stat.Effect as Eff
let (lo, hi) = Eff.cohenDCI xs ys 0.05   -- 95% CI
```

Computed from the non-central t distribution (exact, not asymptotic).

---

## 4. LCA (Phase 32-A2)

```haskell
import qualified Hanalyze.Model.LatentClassAnalysis as LCA
import qualified System.Random.MWC as MWC

gen <- MWC.create
fit <- LCA.fitLCA 2 2 rows 100 1e-4 gen
```

Model: `P(X_i) = Σ_k π_k · Π_j ρ_{k, j, X_{i,j}}`. EM in log space.

**Caveats**: label switching across runs; EM is locally optimal so for
non-trivial problems run several seeds and keep the highest log-likelihood.

---

## 5. Graphical Lasso (Phase 32-A1)

```haskell
import qualified Hanalyze.Stat.CorrelationNetwork as CN
let fit = CN.graphicalLasso xMat 0.05 100 1e-4
CN.glPrecision fit
CN.nonZeroPrecision 0.01 (CN.glPrecision fit)
```

Optimisation:
```
max_{Θ ≻ 0}  log det Θ - tr(SΘ) - λ ‖Θ‖_{1, off-diag}
```

Implementation: FHT 2008 block coordinate descent, inner Lasso solved
on the quadratic form `(1/2) β^T W β - s^T β + λ |β|_1`.

**Caveats**: λ choice (no auto-CV); for `n < p`, `Σ ← S + λI`
initialisation keeps it well-posed.

---

## 6. References

- Phase 13 commit `3a4e056` for the existing entries
- Friedman, Hastie, Tibshirani (2008) Biostatistics 9(3):432-441
- Linzer, Lewis (2011) J Stat Softw 42(10) — poLCA
- Dunn (1964) Technometrics 6
- Smithson (2003) — non-central t for Cohen's d CI
- Comparable: scikit-learn `GraphicalLasso`, R `glasso` / `poLCA` /
  `scikit-posthocs.posthoc_dunn` / `effsize`, JMP "Fit Y by X" platform
