# Principles of generalized linear models (GLM)

> 🌐 **English** | [日本語](glm.ja.md)

## Model

GLM extends LM to **exponential families + link functions**:

$g(E[y_i \mid x_i]) = \beta_0 + \beta_1 x_{i1} + \cdots + \beta_p x_{ip}$

where $g$ is the **link function**.

## Major families and canonical links

- **Gaussian + identity**: ordinary LM.
- **Binomial + logit**: $\log(p/(1-p)) = X\beta$ — logistic regression.
- **Poisson + log**: $\log \lambda = X\beta$ — count data.

## Estimation — IRLS

The normal equations have no closed form; use **iteratively reweighted least squares**:

1. Start from an initial $\beta^{(0)}$.
2. Each iteration:
   - Predictions $\hat\mu = g^{-1}(X\beta)$.
   - Weights $W = \text{diag}(1/V(\hat\mu_i) \cdot (g'(\hat\mu_i))^{-2})$.
   - Working response $z = X\beta + g'(\hat\mu)(y - \hat\mu)$.
   - Solve $\beta^{(t+1)} = (X^T W X)^{-1} X^T W z$.
3. Iterate to convergence.

## Evaluation — McFadden R²

Standard R² does not apply; use a pseudo R²:

$R^2_{\text{McFadden}} = 1 - \frac{\log L(\hat\beta)}{\log L(\beta_{\text{null}})}$

Details: [docs/regression/02-glm.md](../regression/02-glm.md).
