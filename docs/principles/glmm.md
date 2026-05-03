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
