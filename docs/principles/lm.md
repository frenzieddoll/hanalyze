# Principles of linear regression (LM)

> 🌐 **English** | [日本語](lm.ja.md)

## Model

Linear regression expresses the response $y$ as a **linear combination of predictors
$\mathbf{x} = (x_1, \ldots, x_p)$ plus normal noise**:

$y_i = \beta_0 + \beta_1 x_{i1} + \cdots + \beta_p x_{ip} + \varepsilon_i$

with $\varepsilon_i \sim \text{Normal}(0, \sigma^2)$.

## Estimation — least squares (OLS)

Minimise the residual sum of squares (RSS):

$\hat{\beta} = (X^T X)^{-1} X^T y$

Implemented via **QR decomposition** for numerical stability ($X = QR$ → $R^{-1} Q^T y$).

## Confidence intervals

Standard error of each coefficient:

$\text{SE}(\hat\beta_j) = \sqrt{\hat\sigma^2 [(X^T X)^{-1}]_{jj}}$

95 % confidence band for the mean response:

$\hat y_* \pm t_{0.025, n-p-1} \hat\sigma \sqrt{\mathbf{x}_*^T (X^T X)^{-1} \mathbf{x}_*}$

## Assumptions (Gauss–Markov)

- **Linearity**: $E[y \mid x] = X\beta$.
- **Independence**: residuals independent.
- **Homoscedasticity**: $\text{Var}(\varepsilon_i) = \sigma^2$.
- **Normality**: $\varepsilon_i \sim \text{Normal}$ (required for confidence intervals).

Details: [docs/regression/01-lm.md](../regression/01-lm.md).
