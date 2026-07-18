# Miscellaneous Statistical Extensions — Fit Y by X / Friedman / Cohen's d CI / LCA / Graphical Lasso (Phase 32)

> 🌐 **English** | [日本語](usage-misc-stat.ja.md)

> Phase 32 (2026-05-29) adds **Correlation Network (Graphical Lasso) and
> Latent Class Analysis** as new features. Three other capabilities 
> (**Fit Y by X / Friedman + Dunn / Cohen's d CI**) were already implemented in Phase 13,
> so this guide consolidates all five functions, covering **formulations and pitfalls**.
> Type signatures and minimal examples are documented in [api-guide 10-stat](../api-guide/10-stat.md) as the primary reference.

---

## 0. Overview

| Feature | Use |
|---|---|
| Fit Y by X | Auto-dispatch two variables by type (LM / GLM / ANOVA / chi²) |
| Friedman test | Paired multi-group nonparametric |
| Dunn multiple comparisons | All-pairs comparisons after Kruskal-Wallis + p-adjust |
| Cohen's d CI | Confidence interval for effect size (via non-central t) |
| LCA | Categorical latent class clustering (EM) |
| Graphical Lasso | Sparse precision matrix (= conditional independence network) |

Before any of these analyses, it is helpful to first understand the distribution of each variable
using a box plot (`describeBox`):

![Box plot showing variable distributions (descriptive statistics)](../images/describe-box.svg)

---

## 1. Fit Y by X (Phase 13 existing)

JMP "Fit Y by X" platform equivalent. Auto-dispatch based on type of X / Y
(`Continuous` / `Categorical`):

| X | Y | Analysis |
|---|---|---|
| Cont | Cont | Simple regression (LM) |
| Cont | Cat  | Logistic GLM |
| Cat  | Cont | One-way ANOVA |
| Cat  | Cat  | Chi-square independence test |

---

## 2. Friedman test + Dunn multiple comparisons (Phase 13 existing)

Friedman is a paired nonparametric test for n subjects × k treatments matrix.
Dunn performs all-pairs follow-up comparisons after Kruskal-Wallis, with
`MultiCompareResult` containing `(i, j, p_raw, p_adj)` for each pair
(p-adjustment choice: Bonferroni / BH specified in API; BH is default).
The `TestResult` from `friedmanTest` is `Plottable`, and `toPlot` draws a one-row forest.

---

## 3. Cohen's d CI (Phase 13 existing)

`cohenDCI` computes exact confidence intervals via the non-central t distribution
(not asymptotic approximation). Paired version is `cohenDPaired`.

---

## 4. LCA (Latent Class Analysis, Phase 32-A2 new)

Latent class clustering for categorical variables using EM algorithm. Model:

```
P(X_i) = Σ_k π_k · Π_j ρ_{k, j, X_{i,j}}
```

### Caveats

- **Label switching**: Classes 0 / 1 may randomly swap. When interpreting,
  recommend reordering by the ρ pattern
- **Initialization dependence**: EM finds local optima. If needed, fit multiple
  times with different seeds and choose the result with highest log-likelihood
- **K selection**: BIC / AIC provide external model selection (this implementation fixes K)

---

## 5. Graphical Lasso / Correlation Network (Phase 32-A1 new)

Estimates correlation structure in high-dimensional data via **sparse precision matrix**.
Zero elements represent conditional independence. Optimization:

```
max_{Θ ≻ 0}  log det Θ - tr(SΘ) - λ ‖Θ‖_{1, off-diag}
```

### Caveats

- **λ tuning**: Larger λ produces sparsity. Select via cross-validation or BIC
  (this implementation uses fixed λ). Starting value: roughly σ̂_max × 0.1
- **n < p regime**: Empirical covariance `S` is singular, but initialization with
  `Σ ← S + λI` allows fitting when λ > 0
- **No convergence**: λ may be too large or maxOuter insufficient. Try increasing from 100 to 500
- **Reusing existing covariance**: Use `empiricalCov` → `graphicalLassoFromCov`

---

## 6. Related

- Types and minimal examples: [api-guide 10-stat](../api-guide/10-stat.md)
- Specification: `specification/phases/phase-32-misc-stat.md`
- Existing implementation commit: `3a4e056` (Phase 13, 2026-05-24)
- References:
  - Friedman, Hastie, Tibshirani (2008) Biostatistics 9(3):432-441
  - Linzer, Lewis (2011) J Stat Softw 42(10) — poLCA
  - Dunn (1964) Technometrics 6 — Dunn multiple comparisons
  - Smithson (2003) — Cohen's d CI via non-central t
- Comparisons: scikit-learn `GraphicalLasso`, R `glasso` / `poLCA` /
  `scikit-posthocs.posthoc_dunn` / `effsize`, JMP "Fit Y by X" platform
