# Principles of mixed-effects models (GLMM/LME)

> 🌐 **English** | [日本語](glmm.ja.md)

## Model

GLMM is a hierarchical model combining **fixed effects** $\beta$ (population-level) and
**random effects** $u_j$ (group-specific):

$y_{ij} = X_{ij}\beta + u_j + \varepsilon_{ij}$

with:
- $u_j \sim \text{Normal}(0, \sigma^2_u)$ — per-group $j$ intercept.
- $\varepsilon_{ij} \sim \text{Normal}(0, \sigma^2)$ — observation noise.

## Estimation — EM (LME / Gaussian)

1. **E step**: estimate $u_j$ with the BLUP (Best Linear Unbiased Predictor).
2. **M step**: MLE for $\sigma^2_u, \sigma^2$.

Non-Gaussian GLMM marginalises latent variables via the **Laplace approximation**.

## ICC (intraclass correlation coefficient)

Ratio of between-group to total variance:

$\text{ICC} = \frac{\sigma^2_u}{\sigma^2_u + \sigma^2}$

High ICC → strong group structure.

## Why it matters

A plain LM:
- Ignoring the group structure → standard errors underestimated.
- Group-specific fixed effects → number of parameters explodes.

LME jointly estimates both with **regularisation**.

Details: [docs/regression/03-glmm.md](../regression/03-glmm.md).

---

## Translating GLMM into the `Hanalyze.Model.HBM` DSL

Translation patterns for when you want to write a **fully Bayesian HBM** rather
than the MLE / Laplace pipeline in `Hanalyze.Model.GLMM` (`fitLMEDataFrame`,
etc.). This is the mapping between lme4-style formula notation and the
`Hanalyze.Model.HBM` DSL. For detailed implementations see patterns 4-7 in
[`docs/bayesian/02-probabilistic-model.md`](../bayesian/02-probabilistic-model.md).

| lme4-style formula | Hierarchical structure | DSL formulation |
|---|---|---|
| `y ~ 1 + x` | no hierarchy (LM) | patterns 1-2 (`Hanalyze.Model.LM` also fine) |
| `y ~ 1 + x + (1 \| g)` | random intercept only | pattern 4 forms A/B/C; only α_j is hierarchical |
| `y ~ 1 + x + (1 + x \| g)` | random intercept + random slope | pattern 5 (random slope) |
| `y ~ 1 + (1 \| d/s)` | nested (3 levels) | pattern 6 (multi-level) |
| `y ~ 1 + (1 \| s) + (1 \| t)` | crossed | pattern 7 (crossed) |

### Example: `y ~ 1 + x + (1 + x | g)` as an HBM

In lme4:

```r
lmer(y ~ 1 + x + (1 + x | g), data = df)
```

This means "overall mean μ_α + per-group deviation α_j, overall slope μ_β +
per-group deviation β_j". The DSL's `randomSlope` maps directly to it
(see pattern 5 in
[`02-probabilistic-model.md`](../bayesian/02-probabilistic-model.md);
behavior verified):

```haskell
-- See pattern 5 in 02-probabilistic-model.md for the full implementation
randomSlope :: [[(Double, Double)]] -> ModelP ()
randomSlope groupData = do
  muA  <- sample "mu_alpha"  (Normal 0 10)
  tauA <- sample "tau_alpha" (HalfNormal 5)
  muB  <- sample "mu_beta"   (Normal 0 5)
  tauB <- sample "tau_beta"  (HalfNormal 5)
  -- ... sample per-group α_j, β_j and observe y_ij ~ Normal(α_j + β_j x, σ)
```

### Choosing between GLMM (MLE / Laplace) and HBM (fully Bayesian)

| Aspect | GLMM (`fitLMEDataFrame`) | HBM (NUTS) |
|---|---|---|
| Speed | fast (EM / Laplace) | slow (chains × iter) |
| Uncertainty | Wald-approximate SE | exact posterior |
| Number of random effects | many (~thousands) OK | NUTS becomes heavy when groups ≫ 100 |
| Prior customization | not available (fixed) | free |
| Model comparison | AIC/BIC | WAIC/LOO (Bayesian) |

**Recommendation**: fit with GLMM first → once the model structure is settled,
estimate fully Bayesian with HBM. For a three-way comparison (LM / GLMM / HBM)
on the Simpson's paradox example see
[`simpson-paradox`](../../demo/bayesian/SimpsonParadoxDemo.hs).
