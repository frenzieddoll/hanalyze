# Study Material 8 — Theory of Multivariate Regression

> 🌐 **English** | [日本語](theory-multivariate.ja.md)

## 1. Multivariate linear regression

$$ Y = XB + E $$

- $Y \in \mathbb{R}^{n \times q}$ (n observations, q outputs)
- $B \in \mathbb{R}^{p \times q}$ (coefficients)
- $E \in \mathbb{R}^{n \times q}$, rows $\sim$ MvN(0, Σ)

OLS:
$$ \hat B = (X^T X)^{-1} X^T Y $$

Each column can be solved independently. The **residual covariance** $\hat\Sigma = \frac{1}{n-p} E^T E$ captures cross-response correlations.

## 2. Reduced Rank Regression

Constrain $B$ to rank $\le r$. Truncate the OLS solution to its top-$r$ singular components:

$$ \hat B_{RRR} = U_r \Sigma_r V_r^T $$

Assumes a **shared low-dimensional structure** across responses (signal lives in $r$ dimensions).

## 3. Partial Least Squares (PLS)

NIPALS algorithm:

```text
for k = 1..K:
  w = X^T Y u / ||X^T Y u||      weights (X side)
  t = X w                         scores (X side)
  p = X^T t / (t^T t)             loadings (X side)
  q = Y^T t / (t^T t)             loadings (Y side)
  X ← X - t pᵀ                   deflate
  Y ← Y - t qᵀ
```

Successively extracts directions that **maximise the covariance** between X and Y.

## 4. Canonical Correlation Analysis (CCA)

A pair of bases that **maximise the correlation** between $X$ and $Y$:

$$ M = \Sigma_{xx}^{-1/2} \Sigma_{xy} \Sigma_{yy}^{-1/2} $$

SVD it as $M = U \Sigma V^T$, then $a = \Sigma_{xx}^{-1/2} U$, $b = \Sigma_{yy}^{-1/2} V$.
The diagonal of $\Sigma$ holds the canonical correlations.

## 5. Multi-task / Multi-output GP

GPs extended to $f: \mathbb{R}^d \to \mathbb{R}^q$. The simplest variant,
**Independent GPs**, fits each output independently.

A more sophisticated **ICM (Intrinsic Coregionalization Model)**:

$$ k_{ij}(x, x') = B_{ij} \cdot k_x(x, x') $$

with $B$ a (typically low-rank) cross-output coregionalization matrix.

## 6. Summary

| Method | Rank assumption | Computation |
|---|---|---|
| OLS | arbitrary | $(X^T X)^{-1} X^T Y$ |
| RRR | $\le r$ | OLS + SVD truncate |
| PLS | $\le K$ | NIPALS iterations |
| CCA | (correlation basis) | covariance SVD |
| Multi-GP | (kernel-based) | per-output GP fit |
