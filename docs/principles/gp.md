# Principles of Gaussian Process (GP) regression

> 🌐 **English** | [日本語](gp.ja.md)

## Model

A **non-parametric** Bayesian approach placing a prior over the function $f$ itself:

$f \sim \text{GP}(0, k(x, x'))$

At any points $x_1, \ldots, x_n$ the values of $f$ are jointly multivariate normal:

$\mathbf{f} \sim \text{Normal}(\mathbf{0}, K)$, $K_{ij} = k(x_i, x_j)$

Observation model:

$y_i = f_i + \varepsilon_i$, $\varepsilon_i \sim \text{Normal}(0, \sigma^2_n)$

## Kernels

- **RBF (squared exponential)**: $k(x, x') = \sigma^2_f \exp(-(x-x')^2 / (2\ell^2))$.
  - Assumes smooth functions (infinitely differentiable).
- **Matérn 5/2**: coarser functions (twice differentiable).
- **Periodic**: periodic patterns.

## Posterior prediction

At a new point $x_*$:

$\mu_* = K_*^T (K + \sigma^2_n I)^{-1} y$
$\sigma^2_* = k(x_*, x_*) - K_*^T (K + \sigma^2_n I)^{-1} K_*$

## Hyperparameter optimisation

Length scale $\ell$, signal variance $\sigma^2_f$, and noise variance $\sigma^2_n$ are
chosen by maximising the **log marginal likelihood**:

$\log p(y \mid X, \theta) = -\tfrac{1}{2} y^T K_y^{-1} y - \tfrac{1}{2} \log|K_y| - \tfrac{n}{2} \log 2\pi$

Details: [docs/regression/04-regularized.md](../regression/04-regularized.md).
