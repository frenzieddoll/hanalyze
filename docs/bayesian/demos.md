# HBM / GLMM related demo guide

> 🌐 **English** | [日本語](demos.ja.md)
>
> A short guide to the hierarchical-model demos under
> `Hanalyze.Model.HBM` / `Hanalyze.Model.GLMM`, focused on "what each demo
> does" and "what to look at". The actual source for each demo lives under
> `demo/bayesian/`.
>
> For the DSL itself, see
> [`02-probabilistic-model.md`](02-probabilistic-model.md); for comparison
> metrics (WAIC/LOO) see [`06-model-comparison.md`](06-model-comparison.md).

## `hbm-example` — hierarchical normal model + 4-chain NUTS

Source: [`demo/bayesian/HBMExample.hs`](../../demo/bayesian/HBMExample.hs)

**Purpose**: Infer a Form-A (per-group data) hierarchical normal model
with NUTS, and emit a 4-chain aggregated report (model graph + posterior
summary + trace + autocorr + pair) as HTML. A full-feature demo.

**Model**: `schoolModel :: [[Double]] -> ModelP ()`
- `μ ~ Normal(0, 100)`, `τ ~ Exponential(0.1)`
- `θ_j ~ Normal(μ, τ)` (3 groups)
- `y_ij ~ Normal(θ_j, σ=5)`

**How to run**:

```bash
cabal run hbm-example
# → mcmc_report.html          (single chain RWM)
# → mcmc_report_multi.html    (4-chain NUTS, includes R̂)
```

**What to look at**:
- Whether the posterior means of μ and τ agree across chains (check R̂ < 1.01)
- Whether the pair plot shows a funnel shape in μ-τ
- Whether ESS is sufficient (≥ 200)

## `hbm-random-slope` — random intercept vs +random slope

Source: [`demo/bayesian/HBMRandomSlopeDemo.hs`](../../demo/bayesian/HBMRandomSlopeDemo.hs)

**Purpose**: For data where the effect of x differs by group, compare a
random-intercept-only model (M1: shared β) and a random-intercept +
random-slope model (M2: per-group β_j) via WAIC / LOO.

**Data**: 3 groups × 10 observations = N=30, true values are
- Group A: α=2.0, β=-0.8 (steep negative slope)
- Group B: α=5.0, β=-0.3 (mild negative slope)
- Group C: α=8.0, β=+0.2 (slightly positive slope)

**How to run**:

```bash
cabal run hbm-random-slope
# → rs_m1.html        (M1 standalone report)
# → rs_m2.html        (M2 standalone report)
# → rs_compare.html   (M1 vs M2 side-by-side report)
```

**What to look at**:
- Whether M2's WAIC / LOO is smaller than M1's (ΔWAIC < 0)
- Whether the posterior means of β_A, β_B, β_C recover the true values
  -0.8 / -0.3 / +0.2
- M1's β (shared) ends up averaging the per-group truths and becomes
  unidentifiable

## `simpson-paradox` — three-way comparison of LM / GLMM / HBM

Source: [`demo/bayesian/SimpsonParadoxDemo.hs`](../../demo/bayesian/SimpsonParadoxDemo.hs)

**Purpose**: Analyse Simpson's paradox (negative slope within groups,
appears positive if groups are ignored) with three methods, showing that
ignoring the group structure leads to the wrong conclusion.

**Data**: 3 groups × 10 observations = N=30, within each group
`y = α_g − 0.5·x + noise` (negative slope). However the means of `α`
and `x` are shifted across groups, so the overall correlation looks
positive.

**How to run**:

```bash
cabal run simpson-paradox
# → simpson_lm.html       (LM, ignores groups → wrong conclusion β > 0)
# → simpson_glmm.html     (GLMM, random intercept → correct β < 0)
# → simpson_hbm.html      (HBM, fully Bayesian → β < 0 + 95% CI)
# → simpson_compare.html  (3-method comparison report)
```

**What to look at**:
- Whether the LM slope is +, while GLMM / HBM slopes are −
- Whether the HBM posterior 95% CI for β excludes 0 and is clearly negative
- Whether WAIC improves substantially for GLMM / HBM (which include group
  structure)

## `glmm-demo` — GLMM (LME) maximum likelihood estimation

Source: [`demo/Demo.hs`](../../demo/Demo.hs)
(executable name `glmm-demo`)

**Purpose**: Demonstrate the use of **classical GLMM (EM / Laplace)**
rather than HBM (fully Bayesian). Fast to fit, invaluable during the
model-exploration phase.

**Data**: 3 classes × 5 observations = N=15, true values are
`score = 64 + u_school + 2 · hours + ε`, `u_A=+20, u_B=0, u_C=-20`.
Class A scores high with little study time, class C scores low with long
study time, so OLS misreads it as "more study time → lower score"
(Simpson).

**How to run**:

```bash
cabal run glmm-demo
# Prints LME fit results to stdout (fixed effects + random effects + ICC)
```

**What to look at**:
- Whether the fixed-effect coefficient `hours` lies near +2 (recovering
  the truth)
- Whether the random effects `u_school` are close to +20 / 0 / -20
- The high ICC (~0.9) shows the importance of the group structure

**Correspondence with HBM (`simpson-paradox`)**: Rewriting the same method
(LME) fully Bayesian yields HBM pattern 4 Form A. GLMM is fast but uses
Wald-approximation SE; HBM is slower but gives the exact posterior
distribution (see the end of
[`docs/principles/glmm.ja.md`](../principles/glmm.ja.md) for guidance on
when to use which).

## `phase37-a0-verify` — execution check of sample code in the docs

Source: [`demo/bayesian/Phase37A0VerifyDemo.hs`](../../demo/bayesian/Phase37A0VerifyDemo.hs)

**Purpose**: Collect the sample code added in
[`02-probabilistic-model.md`](02-probabilistic-model.md) for pattern 4
(Forms A/B/C) / 5 (random slope) / 6 (multi-level) / 7 (crossed) /
8 (prior choice) into a single executable, and verify it builds and runs
via small-scale NUTS. Guarantees that the code pasted in the docs
actually runs.

**How to run**:

```bash
cabal run phase37-a0-verify
# Runs each model once with 100 iter / 50 burn-in,
# printing acceptance rate + posterior mean of the key parameters on one line.
```

**Expected output**: For all 8 models, acceptance rate > 0.8 and the key
parameters land near the true values. For example, the `random slope`
row recovers `beta_1 ≈ -0.80` and `beta_3 ≈ +0.20`.
