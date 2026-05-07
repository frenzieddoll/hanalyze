# GLMM (Mixed-effects models) — mixture distributions and computation

> 🌐 **English** | [日本語](03-glmm.ja.md)

> **Mixed-effects models** for hierarchical data (groups, repeated measures).
> This document covers the **theory of mixture distributions** (e.g. how the
> negative binomial arises as a Gamma-Poisson mixture) and the **computational
> approaches** (EM, Laplace approximation, MCMC).
> **Multi-output**: `fitLMEMulti` / `fitGLMMMulti` (per-column EM/Laplace,
> shared grouping information) — see [05-multivariate.md](05-multivariate.md).

## Contents

1. [Why GLMM?](#1-why-glmm)
2. [LME (linear mixed-effects)](#2-lme-linear-mixed-effects)
3. [GLMM (generalized version)](#3-glmm-generalized-version)
4. [Mixture distributions: deriving the negative binomial](#4-mixture-distributions-deriving-the-negative-binomial)
5. [Computational methods](#5-computational-methods)
6. [Implementation in hanalyze](#6-implementation-in-hanalyze)
7. [Non-centered parameterization](#7-non-centered-parameterization)
8. [Diagnostics and pitfalls](#8-diagnostics-and-pitfalls)
9. [References](#9-references)

---

## 1. Why GLMM?

### 1.1 Limits of the LM/GLM independence assumption

LM/GLM assume **independent observations**, but real data often have:

- **Patients in hospitals**: similar within hospital (care levels, …).
- **Students in schools**: correlated by class, teacher, region.
- **Time series**: nearby observations are correlated.
- **Repeated measurements**: multiple observations per subject.

A naive LM:
- **Underestimates** standard errors → confidence intervals too narrow.
- Inflated type-I error rate.

### 1.2 Choosing how to pool

Three extreme strategies:

| Strategy | Description | Issue |
|---|---|---|
| **Complete pooling** | Single LM on all data, ignoring groups | Ignores between-group differences (bias) |
| **No pooling** | Separate LM per group | Overfits small groups |
| **Partial pooling** | Tie group effects via a probability distribution = **GLMM** | Sweet spot, recommended |

Partial pooling adds the prior "don't stray far from the overall mean".
Sparsely observed groups **shrink** towards the overall mean.

### 1.3 Example: Simpson's paradox

"Aggregated, x and y are positively related; per-group, x and y are negatively
related" — the relationship **flips on stratification**.

```bash
cabal run simpson-paradox  # demo
```

GLMM captures the within-stratum structure.

---

## 2. LME (linear mixed-effects)

### 2.1 Model

Continuous response with mixed effects:

$$ y_{ij} = \mathbf{x}_{ij}^T \boldsymbol\beta + \mathbf{z}_{ij}^T \mathbf{u}_j + \varepsilon_{ij} $$

- $i$: index within a group
- $j$: group index (e.g. school ID)
- $\boldsymbol\beta$: **fixed effects** (shared across groups)
- $\mathbf{u}_j \sim \text{Normal}(0, G)$: **random effects** (per group $j$)
- $\varepsilon_{ij} \sim \text{Normal}(0, \sigma^2)$: observation noise
- $\mathbf{z}_{ij}$: design matrix for the random effects

### 2.2 Random-intercept model

The simplest GLMM is **random intercepts**:

$$ y_{ij} = \beta_0 + \beta_1 x_{ij} + u_j + \varepsilon_{ij}, \quad u_j \sim \text{Normal}(0, \sigma_u^2) $$

"Each group has its own intercept; the slope is shared." $\sigma_u^2$ measures between-group variability.

### 2.3 Random-slope model

Slopes also vary across groups:

$$ y_{ij} = (\beta_0 + u_{0j}) + (\beta_1 + u_{1j}) x_{ij} + \varepsilon_{ij} $$

$\mathbf{u}_j = (u_{0j}, u_{1j}) \sim \text{Normal}(0, G)$ where $G$ is a 2×2 covariance matrix
that encodes the intercept-slope correlation.

### 2.4 ICC (Intraclass Correlation Coefficient)

For a random-intercept model the within-group correlation is

$$ \text{ICC} = \frac{\sigma_u^2}{\sigma_u^2 + \sigma^2} $$

- 0: groups irrelevant (plain LM is fine).
- 1: identical within group.
- 0.05–0.30 is typical in real data.

---

## 3. GLMM (generalized version)

For non-Gaussian responses:

$$ y_{ij} \mid \mathbf{u}_j \sim \text{ExpFamily}(\mu_{ij}), \quad g(\mu_{ij}) = \mathbf{x}_{ij}^T \boldsymbol\beta + \mathbf{z}_{ij}^T \mathbf{u}_j $$

Examples:

| Family | Link | Use |
|---|---|---|
| Binomial | Logit | hierarchical logistic regression |
| Poisson | Log | hierarchical Poisson (= overdispersed counts) |
| Gamma | Log | hierarchical waiting times |

LME is the Gaussian + Identity special case.

---

## 4. Mixture distributions: deriving the negative binomial

### 4.1 Motivation

Real count data (sales, insurance claims, …) often have **larger variance than Poisson**
(overdispersion). Why? How to handle it?

**Key**: the rate $\lambda$ **fluctuates** across groups / observations. Conditional on a
specific $\lambda$ the data are Poisson, but marginalising over $\lambda$ gives a different
distribution.

### 4.2 As a hierarchical model

```text
λ ~ Gamma(α, β)          ← rate fluctuates as Gamma
y | λ ~ Poisson(λ)       ← given rate, Poisson
```

Marginalising **over $y$** (= integrating out $\lambda$):

$$ p(y) = \int p(y \mid \lambda) p(\lambda) d\lambda $$

### 4.3 Derivation

Poisson PMF: $p(y \mid \lambda) = \frac{\lambda^y e^{-\lambda}}{y!}$

Gamma PDF (shape α, rate β): $p(\lambda) = \frac{\beta^\alpha}{\Gamma(\alpha)} \lambda^{\alpha-1} e^{-\beta \lambda}$

Integrate:

$$ p(y) = \int_0^\infty \frac{\lambda^y e^{-\lambda}}{y!} \cdot \frac{\beta^\alpha \lambda^{\alpha-1} e^{-\beta\lambda}}{\Gamma(\alpha)} d\lambda $$

$$ = \frac{\beta^\alpha}{y! \Gamma(\alpha)} \int_0^\infty \lambda^{y+\alpha-1} e^{-(1+\beta)\lambda} d\lambda $$

The integral evaluates by the Gamma-function definition to $\Gamma(y+\alpha) / (1+\beta)^{y+\alpha}$:

$$ p(y) = \frac{\Gamma(y+\alpha)}{y! \Gamma(\alpha)} \cdot \frac{\beta^\alpha}{(1+\beta)^{y+\alpha}} $$

Let $p = \beta/(1+\beta)$, $1-p = 1/(1+\beta)$:

$$ \boxed{p(y) = \binom{y+\alpha-1}{y} p^\alpha (1-p)^y} $$

— the **negative binomial** $\text{NegBin}(\alpha, p)$.
Alternative parameterisation: $\mu = \alpha(1-p)/p$ (mean), variance $\mu + \mu^2/\alpha$ (larger than Poisson).

### 4.4 Intuition

- **Gamma** is the prior on $\lambda$ (= magnitude of rate fluctuation).
- **Poisson** is the count given $\lambda$.
- **NegBin** is the marginal of $y$ (= what the observer sees).

Memorise: "**Poisson + Gamma hierarchy = NegBin marginal**".

```bash
cabal run negbinom-demo  # μ=10, α=2 data, comparing Poisson vs NB
```

### 4.5 Other mixture examples

Same pattern of "**Y is Poisson, rate has another distribution**":

| Distribution of rate | Marginal of y |
|---|---|
| Gamma | NegBin (above) |
| LogNormal | Poisson-LogNormal (no closed form) |
| InverseGaussian | Sichel distribution |

For "**Y is Bernoulli, p has a Beta**":

| Distribution of p | Marginal of y |
|---|---|
| Beta(α, β) | **Beta-Binomial** |

hanalyze provides this as a `BetaBinomial` observation.
With n=1 it reduces to a Bernoulli marginal; with multiple trials it captures overdispersed binomials.

### 4.6 GLMM perspective

"**Overdispersion = a hidden random effect**" is a frequent reading.

Example: Poisson regression on per-store sales counts shows overdispersion.
A latent **per-store effect** $u_j$ is the cause. Modelled explicitly as a GLMM:

$$ y_{ij} \mid u_j \sim \text{Poisson}(\mu_{ij} e^{u_j}), \quad u_j \sim \text{Normal}(0, \sigma^2) $$

Integrating over $u$ gives a **Poisson-LogNormal**.
A Gamma random effect gives NegBin (the derivation above).

---

## 5. Computational methods

The marginal likelihood of a GLMM contains an **integral**, so direct MLE does not apply.
Three standard approaches:

### 5.1 EM algorithm (LME)

For LME (Gaussian + Identity), there is a **closed-form EM**.

#### Outline

Treat the random effects $\mathbf{u}_j$ as missing data:

- **E-step**: posterior of $\mathbf{u}_j$ (= **BLUP**) under current $(\boldsymbol\beta, \sigma_u^2, \sigma^2)$.
- **M-step**: update $(\boldsymbol\beta, \sigma_u^2, \sigma^2)$ with $\mathbf{u}_j$ fixed.

In LME the BLUP has a closed form (normal + normal = normal):

$$ \hat{\mathbf{u}}_j = (\sigma_u^2 / (\sigma_u^2 + \sigma^2/n_j)) \cdot (\bar y_j - \bar X_j^T \hat{\boldsymbol\beta}) $$

This is a **shrinkage estimator**: each group's "individual mean" is pulled toward the overall mean.

#### hanalyze

```haskell
-- src/Model/GLMM.hs
fitLME :: LA.Matrix Double         -- fixed-effect design
       -> LA.Vector Double          -- response
       -> V.Vector Int              -- per-observation group ID
       -> V.Vector Text              -- group labels
       -> V.Vector Int              -- group sizes
       -> GLMMResult
```

Use `fitLMEDataFrame` to call directly from a DataFrame.

### 5.2 Laplace approximation (general GLMM)

For non-Gaussian GLMMs the EM E-step has no closed form. Use the **Laplace approximation**:

#### Outline

For the marginal likelihood

$$ p(\mathbf{y} \mid \boldsymbol\theta) = \int p(\mathbf{y} \mid \mathbf{u}, \boldsymbol\theta) p(\mathbf{u} \mid \boldsymbol\theta) d\mathbf{u} $$

approximate the integrand by a **Gaussian centered at the mode**:

1. $\hat{\mathbf{u}} = \arg\max_{\mathbf{u}} \log p(\mathbf{y}, \mathbf{u} \mid \boldsymbol\theta)$ via Newton's method.
2. Compute the Hessian $H$.
3. The integral becomes $\sqrt{(2\pi)^d / |H|} p(\mathbf{y}, \hat{\mathbf{u}} \mid \boldsymbol\theta)$.

Then optimise over $\boldsymbol\theta$ (= MLE).

#### Accuracy

- 1st order: fast but limited (= **PQL**, Penalised Quasi-Likelihood).
- 2nd order: standard, good accuracy.
- **AGHQ** (Adaptive Gauss-Hermite Quadrature): high accuracy, expensive.

hanalyze implements the **2nd-order Laplace** approximation.

#### hanalyze

```haskell
fitGLMM :: Family -> LinkFn
        -> LA.Matrix Double -> LA.Vector Double
        -> V.Vector Int -> V.Vector Text -> V.Vector Int
        -> GLMMResult
```

### 5.3 MCMC (full Bayes)

The most general path: sample all parameters ($\boldsymbol\beta, \mathbf{u}, \sigma_u^2, \sigma^2$)
**from the posterior with MCMC**.

#### Example model

```haskell
import Model.HBM

hierarchicalNormal :: ModelP ()
hierarchicalNormal = do
  muPop  <- sample "mu_pop"  (Normal 0 10)
  sigPop <- sample "sig_pop" (HalfNormal 5)
  -- random intercepts: J groups
  thetas <- mapM (\j -> sample ("theta_" <> tShow j)
                                (Normal muPop sigPop))
                 [1 .. nGroups]
  forM_ (zip thetas dataByGroup) $ \(theta, ys) ->
    observe ("y_" <> ...) (Normal theta sigY) ys
```

### 5.4 Comparison

| Method | Speed | Accuracy | Implementation effort |
|---|---|---|---|
| EM (LME) | fast | exact | medium |
| Laplace | medium | approximate | medium |
| AGHQ | slow | high | high |
| MCMC (NUTS) | slow | true posterior | low (with a DSL) |
| VI (ADVI) | very fast | mean-field approx | medium |

Practical guidance:
- Large data → Laplace.
- Inference precision important → MCMC.
- Exploratory analysis → VI for initialisation, MCMC for the final result.

---

## 6. Implementation in hanalyze

### 6.1 LME (Gaussian + Identity)

```haskell
import Model.GLMM (fitLMEDataFrame, GLMMResult (..),
                   glmmFixed, glmmRandVar, glmmResidVar, glmmICC)
import Model.Core (coefficientsV)

case fitLMEDataFrame [("hours", 1)] "school" "score" df of
  Nothing -> putStrLn "fit failed"
  Just gr -> do
    let beta = coefficientsV (glmmFixed gr)
        ranV = glmmRandVar gr   -- σ_u²
        resV = glmmResidVar gr  -- σ²
        icc  = glmmICC gr       -- ICC
    print beta
    putStrLn $ "ICC = " ++ show icc
```

### 6.2 GLMM (Binomial / Poisson)

```haskell
import Model.GLMM (fitGLMMDataFrame)
import Model.GLM (Family (..), LinkFn (..))

case fitGLMMDataFrame Binomial Logit [("dose", 1)] "patient" "outcome" df of
  Just gr -> ...
```

### 6.3 From the CLI

```bash
# LME (LM with groups)
hanalyze data.csv x y LM --group school --report

# GLMM (GLM with groups)
hanalyze data.csv x y GLM -d binomial -l logit --group hospital --report
```

### 6.4 Bayesian hierarchical models (recommended for complex cases)

Complex hierarchies are easiest in `Model.HBM`:

```haskell
import Model.HBM

complexModel :: ModelP ()
complexModel = do
  -- hyperpriors
  muPop   <- sample "mu_pop"   (Normal 0 10)
  sigPop  <- sample "sig_pop"  (HalfNormal 5)
  sigY    <- sample "sig_y"    (HalfNormal 3)
  -- random intercepts (non-centered, recommended)
  thetas <- mapM (\j -> nonCenteredNormal ("theta_" <> tShow j)
                                          muPop sigPop)
                 [1 .. nGroups]
  -- observations
  forM_ (zip thetas dataByGroup) $ \(theta, ys) ->
    observe ("y_" <> ...) (Normal theta sigY) ys
```

See the `hbm-example`, `simpson-paradox`, and `hbm-random-slope` demos.

---

## 7. Non-centered parameterization

### 7.1 The problem

Centered parameterisation:
```haskell
theta_j <- sample ("theta_" <> tShow j) (Normal mu sigma)
```

When data are sparse the posterior takes a **funnel** shape (Neal's funnel), and HMC struggles:
small σ concentrates θ_j around μ; large σ spreads it widely. The posteriors of σ and θ become
strongly correlated and curvature becomes pathological.

### 7.2 Fix: non-centered

```haskell
-- Old (centered)
theta_j <- sample "theta" (Normal mu sigma)

-- New (non-centered, recommended)
theta_raw <- sample "theta_raw" (Normal 0 1)
let theta_j = mu + sigma * theta_raw
```

`theta_raw` is now **independent** of μ, σ — HMC stabilises.

hanalyze provides `nonCenteredNormal` as a helper:

```haskell
theta_j <- nonCenteredNormal "theta" mu sigma
-- internally samples a raw normal and returns theta = mu + sigma * raw deterministically
```

Empirically: `noncentered-demo` shows BFMI 0.65 → 1.02, ESS ×7.6, divergences 127 → 0.

---

## 8. Diagnostics and pitfalls

### 8.1 Over-shrinkage

If the random-effect $\sigma_u$ is estimated too small, **shrinkage is too aggressive** and
group differences disappear. → soften the hyperprior (e.g. HalfNormal(2)).

### 8.2 Imbalanced data

When group sizes differ widely the estimates become unstable; small groups shrink heavily
(theoretically expected).

### 8.3 Random effect correlated with predictor

If $x_{ij}$ correlates with the group mean, fixed and random effects get confused.
→ split into **between-group** (group mean) and **within-group** (deviation from the mean) and
add both to the design matrix.

### 8.4 R-hat / divergences

Bayesian GLMMs (especially Binomial / Poisson) often stress NUTS:

- non-centered parameterisation is essentially required;
- BFMI < 0.3 or divergences > 5% are warning signs;
- raise `target_accept` to 0.95 and reduce step size.

See `energy-demo` / `noncentered-demo`.

---

## 9. References

- **Pinheiro, J. C., Bates, D. M.** (2000). *Mixed-Effects Models in S and S-PLUS*. Springer.
  → Classic foundational LME reference.
- **Demidenko, E.** (2013). *Mixed Models: Theory and Applications with R* (2nd ed.). Wiley.
  → Mathematically rigorous, comprehensive computational methods.
- **Bolker, B. M.** (2015). *Linear and Generalized Linear Mixed Models*. In Fox et al. (eds.), *Ecological Statistics: Contemporary Theory and Application*. Oxford.
  → Practical, with diagnostics and caveats.
- **Gelman, A., Hill, J.** (2007). *Data Analysis Using Regression and Multilevel/Hierarchical Models*. Cambridge.
  → Bayesian-leaning, philosophy of partial pooling.
- **Hilbe, J. M.** (2011). *Negative Binomial Regression* (2nd ed.). Cambridge.
  → Detailed NB derivation and applications (an extended version of §4 here).

### Related hanalyze docs

- [01-lm.md](01-lm.md) — LM basics
- [02-glm.md](02-glm.md) — GLM (NB discussion)
- [theory-regression-extensions.md](theory-regression-extensions.md) — theory
- [../bayesian/02-probabilistic-model.md](../bayesian/02-probabilistic-model.md) — Bayesian hierarchical models
