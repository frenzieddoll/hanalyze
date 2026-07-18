# Study Material — VI / model selection / advanced topics

> 🌐 **English** | [日本語](theory-advanced.ja.md)

> **Audience**: readers who already have the basics of Bayesian statistics
> (prior/likelihood/posterior, conjugacy, the HBM concept). Each topic is broken down
> **starting from definitions**, with pointers for what to study next.
>
> Related: [theory-bayesian-basics.md](theory-bayesian-basics.md) /
> [theory-mcmc.md](theory-mcmc.md) /
> [theory-hmc-nuts.md](theory-hmc-nuts.md)

## Contents

1. [Variational Inference (VI) — posterior via optimisation](#1-variational-inference-vi--posterior-via-optimisation)
2. [Model selection — WAIC / PSIS-LOO](#2-model-selection--waic--psis-loo)
3. [Mixtures](#3-mixtures)
4. [LKJ — correlation matrix prior](#4-lkj--correlation-matrix-prior)
5. [AR / state-space models](#5-ar--state-space-models)
6. [**Truncated distributions** (★ in depth)](#6-truncated-distributions--in-depth)
7. [**Censored distributions** (★ in depth)](#7-censored-distributions--in-depth)
8. [What to use when](#8-what-to-use-when)
9. [Where to go next](#9-where-to-go-next)
10. [References](#10-references)

---

## 1. Variational Inference (VI) — posterior via optimisation

### 1.1 The problem

We want the posterior $p(\theta \mid y)$, but:

- **The normalising constant $p(y) = \int p(y \mid \theta) p(\theta) d\theta$ is an
  intractable high-dimensional integral.**
- MCMC is exact but **slow** (thousands to tens of thousands of iterations to converge).

→ **VI** approximates the posterior with a **simple distribution**. Reduced to an
optimisation problem, hence fast.

### 1.2 Vocabulary

- **Variational distribution** $q(\theta; \phi)$: a distribution that approximates the
  posterior, parameterised by $\phi$ (μ, σ, …).
- **Mean-field**: $q(\theta) = \prod_i q_i(\theta_i)$. Simplest choice; treats parameters
  as independent.
- **KL divergence** (Kullback–Leibler): asymmetric, non-negative "distance" between two
  distributions; zero iff identical.

### 1.3 Deriving the ELBO

We want to know how close $q$ is to the posterior $p(\theta \mid y)$. Measure with
**KL divergence**:

$$ \text{KL}(q \,\|\, p_{\text{post}}) = E_q\!\left[\log q(\theta) - \log p(\theta \mid y)\right] $$

Since $p(\theta \mid y) = p(\theta, y) / p(y)$:

$$ \text{KL} = E_q[\log q(\theta) - \log p(\theta, y)] + \log p(y) $$

$\log p(y)$ is a constant w.r.t. $\theta$. Define the **negative** of the first term as
the ELBO:

$$ \text{ELBO}(\phi) = E_q[\log p(\theta, y) - \log q(\theta; \phi)] $$

So:

$$ \log p(y) = \text{ELBO}(\phi) + \text{KL}(q_\phi \| p_{\text{post}}) $$

Since $\log p(y)$ is constant and $\text{KL} \ge 0$, the **ELBO is a lower bound on
$\log p(y)$**.
**Maximising the ELBO = minimising the KL** = best posterior approximation.

### 1.4 ADVI: automatic VI

Kucukelbir et al. (2017). Applies to any model automatically:

1. Map all latents to **unconstrained space** (via `PositiveT`/`UnitIntervalT`/…).
2. Approximate each component by a **Normal**$(\mu_i, \sigma_i)$ (mean-field).
3. Compute the ELBO gradient by AD.
4. Optimise with **Adam** etc.

### 1.5 Usage in hanalyze

```haskell
import Hanalyze.Stat.VI (advi, defaultVIConfig, VIConfig (..), VIResult (..))

result <- advi model defaultVIConfig
                  { viIterations = 5000, viLearningRate = 0.05 }
                init0 gen
-- result.viParams :: Map Text (Double, Double)  -- (μ, σ) per parameter
-- result.viELBO   :: [Double]                    -- ELBO history per iteration
```

`vi-demo` compares accuracy with NUTS — VI is ~700× faster.

### 1.6 VI weaknesses

- **Mean-field constraint**: cannot capture cross-parameter correlations (often
  underestimates SDs).
- **Weak on multimodal posteriors**: only captures one mode.
- **Poor tail estimation**: VI minimises KL(q‖p), so $q$ tends to ignore $p$'s tails
  (mode-seeking).

→ Practical: VI for exploration / initialisation, MCMC for the final analysis.

---

## 2. Model selection — WAIC / PSIS-LOO

### 2.1 What these criteria measure

A criterion for "picking a good model from several". They evaluate **predictive accuracy
on new observations** (out-of-sample) rather than fit to observed data.

### 2.2 Vocabulary

- **elpd** (expected log predictive density):
  $\text{elpd} = E_{p(\tilde y)}[\log p(\tilde y \mid y)]$
  — expected log predictive density at a new observation $\tilde y$. **Larger (closer to 0
  from below) is better**.
- **WAIC** (Widely Applicable Information Criterion, Watanabe 2010):
  approximates elpd directly from posterior samples.
- **PSIS-LOO** (Pareto Smoothed Importance Sampling LOO-CV, Vehtari 2017):
  approximates leave-one-out cross-validation by importance sampling.

### 2.3 WAIC formula

$$ \widehat{\text{elpd}}_{\text{WAIC}} = \sum_{i=1}^n \log\!\left(\frac{1}{S}\sum_{s=1}^S p(y_i \mid \theta^{(s)})\right) - \sum_i \text{Var}_s\!\left(\log p(y_i \mid \theta^{(s)})\right) $$

First term: log of the mean posterior predictive density.
Second term: **effective parameter count $p_{\text{WAIC}}$** (overfitting penalty).

### 2.4 PSIS-LOO outline

LOO-CV normally requires $n$ refits ($O(n)$ cost).
**Importance sampling** approximates $n$ LOO predictions from one posterior sample:

$$ p(y_i \mid y_{-i}) \approx \frac{\sum_s w_i^{(s)} p(y_i \mid \theta^{(s)})}{\sum_s w_i^{(s)}} $$

When the weight distribution has heavy tails it becomes unstable. **Smooth with a Pareto** (PSIS):

- Estimate the Pareto shape $\hat k$ from the top weights.
- $\hat k < 0.5$: OK.
- $0.5 \le \hat k < 0.7$: caution.
- $\hat k \ge 0.7$: that observation is unreliable (recommend re-running MCMC excluding it).

### 2.5 Usage in hanalyze

```haskell
import Hanalyze.Stat.ModelSelect (waic, loo, compareModels, ModelInfo (..))

let waicResult = waic loglikMatrix    -- loglik :: [[Double]]  (S × N)
let looResult  = loo  loglikMatrix
-- looKHat looResult :: [Double]       (k̂ per observation)

-- Multi-model comparison
let cmps = compareModels [ ModelInfo "M1" loglik1
                         , ModelInfo "M2" loglik2 ]
-- elpd, se, weights (Pseudo-BMA) per model
```

### 2.6 Pseudo-BMA (model averaging)

Predict by **weighted average** of multiple models:

$$ w_k = \frac{\exp(\text{elpd}_k)}{\sum_l \exp(\text{elpd}_l)} $$

"Model averaging" (using several with probability weights) is often more robust than
"model selection" (picking one).

---

## 3. Mixtures

### 3.1 Definition

**Mixture distribution**:

$$ p(x) = \sum_{k=1}^K w_k \, p_k(x), \quad \sum_k w_k = 1, \, w_k \ge 0 $$

Each $p_k$ is a **component**; $w_k$ is the **mixing weight**.

### 3.2 Uses

- **Clustering**: assume the data is a mixture of normals → infer which component each point belongs to.
- **Outlier model**: main component + a "wide" component → tolerate outliers.
- **Heteroscedastic** errors: mixture of components with different variances.
- **Flexible distribution**: GMM (Gaussian Mixture Model) approximates arbitrary distributions.

### 3.3 log-sum-exp

Numerical stability trick in log space:

$$ \log p(x) = \log \sum_k w_k p_k(x) = \text{logsumexp}(\log w_k + \log p_k(x)) $$

`logSumExpA` provides this helper.

### 3.4 hanalyze

```haskell
mix <- sample "x" (Mixture [0.3, 0.7] [Normal 0 1, Normal 5 2])
-- 30% Normal(0, 1), 70% Normal(5, 2)
```

### 3.5 Label switching

When fitting a mixture model with MCMC, the component labels can swap from iteration to
iteration (the likelihood is invariant under permutation).

Mitigations:
- Impose an order constraint ($\mu_1 < \mu_2 < \cdots$) via `potential`.
- Re-align components post hoc.

---

## 4. LKJ — correlation matrix prior

### 4.1 Motivation

Estimating the **covariance matrix $\Sigma$** of a multivariate normal $\text{MvN}(\boldsymbol\mu, \Sigma)$.
$\Sigma$ is symmetric positive definite — complex structure.

**LKJ decomposition**:

$$ \Sigma = \text{diag}(\boldsymbol\sigma) \, R \, \text{diag}(\boldsymbol\sigma) $$

- $\boldsymbol\sigma$: per-dimension SD (= **scale**).
- $R$: correlation matrix (diag 1, symmetric positive definite).

Place independent priors on each:
- $\sigma_i \sim \text{HalfNormal}$
- $R \sim \text{LKJ}(\eta)$

### 4.2 The LKJ distribution (Lewandowski-Kurowicka-Joe 2009)

A distribution on K×K correlation matrices. Density:

$$ p(R) \propto |R|^{\eta - 1} $$

| $\eta$ | Property |
|---|---|
| $\eta = 1$ | uniform on correlation matrices |
| $\eta > 1$ | concentrates near $I$ (weak correlations) |
| $\eta < 1$ | concentrates near ±1 (strong correlations) |

### 4.3 hanalyze implementation

```haskell
import Hanalyze.Model.HBM (lkjCorrCholesky)

l <- lkjCorrCholesky "R" 3 1.0  -- K=3, η=1
-- l :: [[a]] is L (Cholesky factor; R = L Lᵀ)
```

Internally uses the **CPC (Canonical Partial Correlations)** method to sample $K(K-1)/2$ Beta variables.

### 4.4 Uses

- Covariance prior in multivariate hierarchical models.
- Output-correlation in multi-output GP.
- Intercept-slope correlation in random-slope models.

---

## 5. AR / state-space models

### 5.1 AR(1)

Most basic time series:

$$ x_t = \phi x_{t-1} + \varepsilon_t, \quad \varepsilon_t \sim \text{Normal}(0, \sigma) $$

- $|\phi| < 1$ → **stationary**: long-run mean = 0, variance = $\sigma^2 / (1-\phi^2)$.
- $\phi = 1$ → **random walk**.

### 5.2 State-space models

Latent state $x_t$ + observation $y_t$:

```text
x_t = φ x_{t-1} + ε_t        (state equation)
y_t = x_t + η_t              (observation equation)
```

Treating $\varepsilon_t$ and $\eta_t$ separately enables **denoising**.

### 5.3 hanalyze's `ar1Latent`

```haskell
xs <- ar1Latent "x" T phi sigma
-- internally:
--   raw_t ~ Normal(0, 1)
--   x_0 = (σ / √(1-φ²)) × raw_0           (stationary distribution)
--   x_t = φ x_{t-1} + σ × raw_t           (t > 0)
-- raw_t are independent normals; x_t are recorded as derived quantities
```

This is a **non-centered parameterisation** — see [theory-hmc-nuts.md §6](theory-hmc-nuts.md).
Stable under HMC.

### 5.4 Extensions

- **AR(p)**: depends on the past $p$ steps.
- **VAR**: multivariate time series.
- **Kalman filter**: MLE for linear-Gaussian state-space (not yet in hanalyze).

---

## 6. Truncated distributions (★ in depth)

### 6.1 The situation: why we need it

When **observations exist only within a specific range** and values outside are **never
observed**.

**Example 1: survival truncation**
- Observe survival times of hospitalised patients.
- Observation window is at most 5 years — patients who die after 5 years are **simply
  unknown** (discharged etc.).
- Sample is "patients who died within 5 years".

**Example 2: sensor detection range**
- A sensor reads only [0.1, 100].
- Out-of-range values are not recorded (no signal).

**Example 3: survey self-selection**
- "Days exercised in the past year".
- "People who never exercise" are excluded from the survey, so the data has minimum ≥ 1.

In all cases the **observed sample itself is biased** (selection bias). Plugging into
plain Normal/Exp likelihoods produces **bias**.

### 6.2 Mathematical formulation

Truncate $p(x)$ to the range $[a, b]$:

$$ p_T(x \mid a \le x \le b) = \begin{cases} \dfrac{p(x)}{F(b) - F(a)} & a \le x \le b \\ 0 & \text{otherwise} \end{cases} $$

with $F$ the CDF. The denominator $F(b) - F(a)$ renormalises to 1.

**Intuition**: "scale up the mass within the range".

### 6.3 One-sided range

- Lower only ($x \ge a$): $p_T(x) = p(x) / [1 - F(a)]$.
- Upper only ($x \le b$): $p_T(x) = p(x) / F(b)$.

Example: "exponential observations only when t > 1":

$$ p_T(t \mid t > 1) = \frac{\lambda e^{-\lambda t}}{e^{-\lambda}} = \lambda e^{-\lambda(t-1)} \quad (t \ge 1) $$

Thanks to memorylessness this is a **shifted exponential**. Generally not so simple.

### 6.4 Usage in hanalyze

```haskell
import Hanalyze.Model.HBM (Distribution (..), observe)

-- Survival times truncated to the observation window [0, 5] (Exp)
truncatedSurvival :: ModelP ()
truncatedSurvival = do
  rate <- sample "rate" (HalfNormal 2)
  observe "y" (Truncated (Exponential rate) (Just 0) (Just 5)) survivalTimes
  -- y values lie in [0, 5]; data for patients dying after 5 years is excluded
```

**Key**: `Truncated d (Just lo) (Just hi)` automates the normalisation.

### 6.5 Comparison with the truth

"Mean observed survival time" and "true rate" differ:
- Truncating to [0, 5] makes the rate **look smaller** than the truth (long-lived patients are missing).
- With Truncated, the rate is recovered correctly.

Demonstrated by `trunc-censor-demo`:
```
Truth: rate = 0.5 → mean survival 2 years
With Truncated correction: rate ≈ 0.5  ✓
Without correction (plain Exponential): rate overestimated (survival underestimated)
```

### 6.6 Caveats

- **Two-sided truncation** can have strong log-density discontinuities and may be hard for
  NUTS. Alternative: solve with MH or a Gibbs sampler.
- The base distribution must have a **CDF** (Normal/Exponential/LogNormal/Uniform/Beta/
  Gamma/Cauchy/StudentT/HalfCauchy are supported in hanalyze).

---

## 7. Censored distributions (★ in depth)

### 7.1 Difference from Truncated

**Censored** means **"sample is taken, but only part of the value is known"**:

| | Truncated | Censored |
|---|---|---|
| Existence | out-of-range values **don't exist** | out-of-range values **are recorded (as boundary)** |
| Data count | within-range only | all (boundary values included) |
| Example | only patients dying within 5 years | all patients observed; patients alive after 5 years recorded as ≥5 |

### 7.2 Examples

#### Example A: Tobit model (economics)
- Observe how much each customer spent on a watch.
- "Did not buy" recorded as 0 (i.e. clipped at the detection threshold).
- True value continuous but observation clipped at 0.

#### Example B: detection limit
- Chemical analysis with detection threshold = 0.01 ppm.
- A true value of 0.005 ppm is recorded as "< 0.01" (right-censored at 0.01).

#### Example C: right-censored survival
- Patients still alive at the end of the study.
- True death time unknown but "≥ end-of-study time".

### 7.3 Mathematical formulation

For an observation $y_i$:

- **Within bounds**: ordinary density $p(y_i)$.
- **Equal to lower bound lo** (= true value ≤ lo, censored): $P(Y \le \text{lo}) = F(\text{lo})$.
- **Equal to upper bound hi** (= true value ≥ hi, censored): $P(Y \ge \text{hi}) = 1 - F(\text{hi})$.

Embedding these correctly into the log-likelihood gives unbiased estimation.

### 7.4 hanalyze's `Censored`

```haskell
import Hanalyze.Model.HBM (Distribution (..), observe)

-- Normal observations with detection threshold 1.0
censoredSensor :: ModelP ()
censoredSensor = do
  mu  <- sample "mu"    (Normal 0 5)
  sig <- sample "sigma" (HalfNormal 3)
  observe "y" (Censored (Normal mu sig) (Just 1.0) Nothing) sensorReadings
  -- sensorReadings mixes 1.0 (censored) with regular values
```

Internally:
- If $y_i$ equals `lo`, use the **CDF value** $F(\text{lo})$.
- Otherwise use the **density** $p(y_i)$.

#### Common API for Truncated / Censored

```haskell
data Distribution a
  = ...
  | Truncated (Distribution a) (Maybe a) (Maybe a)
  --             base            lo        hi   (Nothing = -∞ / +∞)
  | Censored  (Distribution a) (Maybe a) (Maybe a)
```

### 7.5 Comparison with the truth

For Censored Normal in `trunc-censor-demo`:

| | μ̂ (estimated) | σ̂ |
|---|---|---|
| With Censored correction (correct model) | ≈ truth | ≈ truth |
| Without correction (treats 1.0 as the true value) | μ **biased upward** | σ **underestimated** |

Treating the threshold as the true value pulls the mean toward it.

### 7.6 Relation to Tobit

The economics **Tobit model** is a special case of Censored Normal:

```text
y* = X β + ε (latent, unobserved)
y  = max(0, y*)  (observed; censored at threshold 0)
```

Equivalent to `Censored (Normal (X β) sigma) (Just 0) Nothing`.

### 7.7 Combining both

In practice both can occur together:

- Survival: window [0, 5] (truncated) + patients alive at end-of-study right-censored.
- Sensor: detection range [0.01, 100] (truncated) + values above 100 clipped to 100 (censored).

Combine the two mechanisms when building the likelihood. In hanalyze these are separate
`Distribution`s; call `observe` twice, etc.

### 7.8 Identifiability

**Censored / Truncated cannot be estimated without enough data**:

- All values at the censoring boundary → μ and σ undetermined.
- Range too narrow → too few observations.

→ Wide posterior HDIs are a warning sign. Strengthen the prior or collect more data.

---

## 8. What to use when

| Goal | Tool |
|---|---|
| Quick posterior on large data | **VI (ADVI)** |
| Multimodal / complex posterior | NUTS (MCMC) — VI captures only one mode |
| Automatic clustering | Mixture |
| Heavy-tailed observations | StudentT, Mixture (main + wide) |
| Multivariate covariance | LKJ + scale decomposition |
| Time series | AR(1), state-space |
| Observations only within a range (selection bias) | **Truncated** |
| Observations clipped at a boundary (= all data, partial info) | **Censored** |
| Survival data | combine Truncated and Censored as needed |
| Model comparison | WAIC + LOO + (compareModels) |

---

## 9. Where to go next

Suggested order for newcomers:

1. **Run and read the demos** ([demo](../../demo/)):
   - `clinical-trial`, `simpson-paradox` (basics)
   - `mixture-demo` (Mixture)
   - `lkj-demo`, `lkj3d-demo` (LKJ)
   - `ar1-demo` (AR)
   - `trunc-censor-demo` (★ Truncated / Censored)
   - `vi-demo` (VI vs NUTS)
   - `forest-compare` (model comparison)

2. **Apply to real data**:
   - Write your model in `Hanalyze.Model.HBM`.
   - Fit with NUTS → inspect the diagnostic HTML via `Hanalyze.Viz.Report`.
   - If assumptions are off, try Truncated/Censored/Mixture.

3. **Deepen the theory** (proceed to the references):
   - Gelman BDA: Bayesian statistics bible.
   - Gelman & Hill: hierarchical models with many examples.
   - Vehtari papers: latest on WAIC/LOO.

4. **Mechanics of HMC/NUTS** — [theory-hmc-nuts.md](theory-hmc-nuts.md).

5. **Next level**:
   - Causal inference (DoWhy, IV, DID).
   - Gaussian Processes (`gp-demo`).
   - Multi-objective optimisation ([../optim/02-multi-objective.md](../optim/02-multi-objective.md)).

---

## 10. References

### VI / model selection
- **Kucukelbir, A., Tran, D., Ranganath, R., Gelman, A., Blei, D. M.** (2017). "Automatic Differentiation Variational Inference". *JMLR*. → ADVI.
- **Watanabe, S.** (2010). "Asymptotic Equivalence of Bayes Cross Validation and WAIC". *JMLR*. → WAIC.
- **Vehtari, A., Gelman, A., Gabry, J.** (2017). "Practical Bayesian model evaluation using leave-one-out cross-validation and WAIC". *Statistics and Computing*. → PSIS-LOO.
- **Yao, Y., Vehtari, A., Simpson, D., Gelman, A.** (2018). "Using Stacking to Average Bayesian Predictive Distributions". *Bayesian Analysis*. → Pseudo-BMA / Stacking.

### Mixture
- **McLachlan, G., Peel, D.** (2000). *Finite Mixture Models*. Wiley. → classic.

### LKJ
- **Lewandowski, D., Kurowicka, D., Joe, H.** (2009). "Generating random correlation matrices based on vines and extended onion method". *J. Multivariate Analysis*.

### Truncated / Censored ★
- **Klein, J. P., Moeschberger, M. L.** (2003). *Survival Analysis: Techniques for Censored and Truncated Data* (2nd ed.). Springer.
  → Standard survival-analysis text — thorough on censoring/truncation.
- **Greene, W. H.** (2017). *Econometric Analysis* (8th ed.). Pearson. Chapter 19.
  → Tobit / Heckman / Censored regression from an economics angle.
- **Cohen, A. C.** (1991). *Truncated and Censored Samples*. Marcel Dekker.
  → Specialist text covering both mathematically.

### Time series
- **Durbin, J., Koopman, S. J.** (2012). *Time Series Analysis by State Space Methods* (2nd ed.). Oxford.

### General
- **Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., Rubin, D. B.** (2013). *Bayesian Data Analysis* (3rd ed.). CRC. [Web (free)](http://www.stat.columbia.edu/~gelman/book/)
  → Bayesian-statistics bible.
- **McElreath, R.** (2020). *Statistical Rethinking* (2nd ed.). CRC.
  → Intuition-driven; best introductory text.

### Related hanalyze docs
- [theory-bayesian-basics.md](theory-bayesian-basics.md) — Bayesian basics
- [theory-mcmc.md](theory-mcmc.md) — MCMC fundamentals
- [theory-hmc-nuts.md](theory-hmc-nuts.md) — HMC/NUTS
- [theory-distributions.md](theory-distributions.md) — distribution catalogue
- [05-vi.md](05-vi.md) — VI implementation and usage
- [06-model-comparison.md](06-model-comparison.md) — WAIC/LOO implementation
