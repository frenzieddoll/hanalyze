# Causal Inference: Propensity Score / IPW / DR / CATE (Phase 30)

> 🌐 **English** | [日本語](usage-causal.ja.md)

> Learning guide for the **causal effect estimation from observational data** module added in Phase 30 (2026-05-29).
> Based on the Rubin causal model, it estimates ATE / ATT / CATE by correcting for confounding through covariates.
> Type signatures and minimal examples are documented in [api-guide 08-causal](../api-guide/08-causal.md) as the primary reference.
> This guide covers **derivations of estimators, assumptions, and pitfalls**.

---

## 0. Overview

| Feature | Role |
|---|---|
| Propensity Score | Estimate `p(X) = P(T=1\|X)` via logistic regression + trim to prevent weight divergence |
| IPW | Hajek-normalized ATE / ATT (lower variance than Horvitz-Thompson) |
| Doubly Robust (AIPW) | Uses both outcome model and PS; consistent if either one is correctly specified |
| CATE meta-learners | Heterogeneous treatment effect estimation (S / T / X-learner, base = LM or RF) |

IPW / DR / CATE internally apply propensity score + `defaultPSTrim = (0.01, 0.99)` automatically
(= weights never diverge). Effect estimation assumes the **causal DAG is known**.
When the structure itself is unknown, use causal discovery methods like LiNGAM to estimate
a directed graph from observational data. The figure below shows a DAG estimated by LiNGAM (`x0 → x1 → x2`),
which provides justification for the set of confounders used by the estimators.

![Causal DAG estimated by LiNGAM (x0 → x1 → x2)](../images/lingam-dag.svg)

---

## 1. IPW Estimator (Hajek Normalization)

```
ATE_Hajek = Σ(T·Y/p) / Σ(T/p)  -  Σ((1-T)·Y/(1-p)) / Σ((1-T)/(1-p))
ATT_Hajek = Σ(T·Y) / Σ T       -  Σ((1-T)·p/(1-p)·Y) / Σ((1-T)·p/(1-p))
```

The Hajek estimator normalizes the denominator by weights to sum to 1, making it more stable
in finite samples than Horvitz-Thompson (which uses fixed denominator n). This is the default.

---

## 2. Doubly Robust (AIPW) Double Robustness

Fit per-group OLS for `μ̂_1(X)` / `μ̂_0(X)`, and correct residuals using PS:

```
ATE_AIPW = (1/n) Σ [ μ̂_1(X_i) - μ̂_0(X_i)
                    + T_i (Y_i - μ̂_1(X_i)) / p̂_i
                    - (1-T_i) (Y_i - μ̂_0(X_i)) / (1 - p̂_i) ]
```

**Double robustness**: ATE is consistent if either the outcome model or PS is correctly specified
(not both necessary). The outcome model uses linear OLS (`Model.LM`), so for nonlinearity either expand X with higher-order terms
or use CATE module with random forest base learners.

---

## 3. CATE Meta-learner Selection

Three approaches to estimate heterogeneous treatment effect `τ(X) = E[Y(1) - Y(0) | X]`:

| | Algorithm | Strength | Weakness |
|---|---|---|---|
| **S-learner** | Single model on (X, T) | Sample efficiency (1 model) | T effect may fade, LM without interaction → constant CATE |
| **T-learner** | Separate fits μ_1, μ_0 per group | Recovers heterogeneity directly | High variance when group sizes are imbalanced |
| **X-learner** | Re-regress T-learner residuals + PS-weighted average | Robust to unbalanced groups | Requires 4 sub-models, more estimation steps |

Details: Künzel, Sekhon, Bickel, Yu (2019) PNAS 116:4156-4165.

---

## 4. Unexpected Behaviors to Watch

### PS saturates at 0 / 1

If covariates have a separating hyperplane, `p_i` saturates at 0 / 1 and IPW weights diverge.
Always apply `trimPropensity 0.01 0.99` before use (= `ipw` / `doublyRobust` do this automatically).
If trimming still leaves large variance, the positivity assumption is effectively violated —
consider switching to ATT-only or restricting to the overlap region.

### No unmeasured confounders assumption is user's responsibility

The backend does not verify DAG assumption validity. Covariates X must fully close confounding.
Omitting important variables introduces bias. Sensitivity analysis (Rosenbaum bounds, etc.) is outside Phase 30's scope.

### S-learner with LM trap

S-learner with LM base and no interaction terms yields constant CATE (intercept-shift only).
To observe heterogeneity, use T / X-learner, or add X·T interaction columns manually to X.

---

## 5. Related

- Types and minimal examples: [api-guide 08-causal](../api-guide/08-causal.md)
- Specification: `specification/phases/phase-30-causal.md`
- References:
  - Rosenbaum & Rubin (1983) Biometrika 70:41-55. (Propensity Score)
  - Horvitz & Thompson (1952) JASA 47:663-685. (IPW)
  - Robins, Rotnitzky, Zhao (1994) JASA 89:846-866. (AIPW)
  - Künzel et al. (2019) PNAS 116:4156-4165. (Meta-learners)
- Comparisons: R `MatchIt` / `WeightIt` / `tmle`, Python `econml`, `DoWhy`
