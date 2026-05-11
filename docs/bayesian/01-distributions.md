# Probability distribution relationship map

> 🌐 **English** | [日本語](01-distributions.ja.md)

> A map of the limits, specializations, and conjugacy relations among the
> distributions implemented in hanalyze. Useful as a supplement to the study
> materials for understanding which distribution is a special case of which.

## Legend

```mermaid
graph LR
  A((Parent)) -- "limit / specialization condition" --> B((Child))
```

- **Limit**: pushing a parameter to ∞ or 0 yields a different distribution.
- **Specialization**: fixing a parameter at a specific value matches another distribution.
- **Mixture**: derived from a hierarchy of two distributions.
- **Conjugate**: a pair that gives a closed-form Bayesian posterior.

## 1. Continuous-distribution family tree

```mermaid
graph TD
  N["Normal(μ, σ)"]
  HN["HalfNormal(σ)"]
  LN["LogNormal(μ, σ)"]
  T["StudentT(ν, μ, σ)"]
  C["Cauchy(loc, scale)"]
  HC["HalfCauchy(scale)"]
  E["Exponential(rate)"]
  G["Gamma(α, β)"]
  IG["InverseGamma(α, β)"]
  W["Weibull(k, λ)"]
  P["Pareto(α, x_m)"]
  B["Beta(α, β)"]
  U["Uniform(a, b)"]
  Chi["χ²(k)"]
  VM["VonMises(μ, κ)"]

  N -->|"x ≥ 0 constraint (folding)"| HN
  N -->|"x = log y (log transform)"| LN
  T -->|"ν → ∞"| N
  T -->|"ν = 1"| C
  C -->|"x ≥ 0 constraint"| HC
  G -->|"α = 1"| E
  G -->|"α = k/2, β = 1/2"| Chi
  G -->|"X ~ Gamma → 1/X ~ InvGamma"| IG
  E -->|"k = 1 (Weibull)"| W
  W -->|"shape k = 2"| Rayleigh["Rayleigh(σ=λ/√2)"]
  G -->|"X / (X+Y), Y ~ Gamma same shape → Beta"| B
  B -->|"α = β = 1"| U2["Uniform(0,1)"]
  U2 -->|"linear transform"| U
  P -->|"X = log Y - log x_m, X ~ Exp(α)"| E
  VM -->|"κ → 0"| UAng["Uniform(-π, π)"]
  VM -->|"κ → ∞"| Napprox["Normal(μ, 1/√κ)"]
```

### Common transformations

| Transform | Relation | Note |
|---|---|---|
| `LogNormal(μ, σ)` | log y ~ Normal(μ, σ) | positive values, normal under log |
| `HalfNormal(σ)` | abs(z), z ~ Normal(0, σ) | popular SD prior |
| `Chi²(k)` | sum of k Normal(0,1)² | Gamma special case |
| `Rayleigh(σ)` | √(X² + Y²), X,Y ~ N(0, σ) | equivalent to Weibull(k=2, λ=σ√2) |

## 2. Discrete-distribution family tree

```mermaid
graph TD
  Bern["Bernoulli(p)"]
  Bin["Binomial(n, p)"]
  Multi["Multinomial(n, π)"]
  Cat["Categorical(π)"]
  Poi["Poisson(λ)"]
  NB["NegativeBinomial(μ, α)"]
  Geo["Geometric(p)"]
  ZIP["ZeroInflatedPoisson(ψ, λ)"]
  ZIB["ZeroInflatedBinomial(n, ψ, p)"]
  BB["BetaBinomial(n, α, β)"]
  N["Normal(μ, σ)"]

  Bern -->|"equivalent to n=1"| Bin
  Bin -->|"K=2 (binary outcome)"| Multi2["Multinomial(n, [p, 1-p])"]
  Bern -->|"extend to K categories"| Cat
  Cat -->|"aggregating n trials"| Multi
  Bin -->|"n→∞, p→0, np=λ fixed"| Poi
  Bin -->|"n→∞ (CLT)"| N
  Poi -->|"large λ (CLT)"| N
  NB -->|"α → ∞"| Poi
  NB -->|"r=1 → geometric"| Geo
  Poi -->|"+ structural zero ψ"| ZIP
  Bin -->|"+ structural zero ψ"| ZIB
  Bin -->|"p ~ Beta(α,β) mixture"| BB
  BB -->|"α, β → ∞ (concentration)"| Bin
```

### Important limits / approximations

```mermaid
graph LR
  Bin["Binomial(n, p)"]
  Poi["Poisson(λ=np)"]
  N["Normal(μ=np, σ=√(np(1-p)))"]
  Bin -->|"n→∞, p→0, np=λ fixed: Poisson approx"| Poi
  Bin -->|"n→∞, p fixed: normal approx (CLT)"| N
  Poi -->|"λ→∞: normal approx"| N
```

These are textbook examples of the **De Moivre–Laplace theorem** (binomial → normal),
the **Poisson approximation**, and the **Central Limit Theorem**.

## 3. Conjugate pairs (closed-form Bayesian posterior)

```mermaid
graph TD
  subgraph "Beta-Binomial"
    Bp["Beta(α, β)"] --"prior on p"--> Bb["Binomial(n, p)"]
    Bb --"posterior"--> Bp2["Beta(α+k, β+n-k)"]
  end
  subgraph "Gamma-Poisson"
    Gl["Gamma(α, β)"] --"prior on λ"--> Pp["Poisson(λ)"]
    Pp --"posterior"--> Gl2["Gamma(α+Σy, β+n)"]
  end
  subgraph "Dirichlet-Multinomial"
    D["Dirichlet(α₁,…,α_K)"] --"prior on π"--> M["Multinomial(n, π)"]
    M --"posterior"--> D2["Dirichlet(α_k + count_k)"]
  end
  subgraph "Normal-InverseGamma"
    IG["InverseGamma(α, β)"] --"prior on σ²"--> Nl["Normal(μ, σ)"]
    Nl --"posterior"--> IG2["InverseGamma(α+n/2, β+SS/2)"]
  end
  subgraph "Normal-Normal"
    Np["Normal(μ₀, σ₀)"] --"prior on μ"--> Nl2["Normal(μ, σ)"]
    Nl2 --"posterior"--> Np2["Normal(updated)"]
  end
```

These are used for direct Gibbs sampling of individual parameters.
hanalyze's `Hanalyze.MCMC.Gibbs.gibbsMH` automatically detects and exploits the conjugacy
structure of the prior/likelihood combination.

## 4. Multivariate and correlation

```mermaid
graph TD
  N1["Normal (1D)"]
  MvN["MvNormal([μ_1,...,μ_K], Σ)"]
  LKJ["LKJ(η) on R"]
  Sigma["Σ = diag(σ) R diag(σ)"]
  IG["InverseGamma (per σ_i² prior)"]
  Wish["Wishart"]
  HN["HalfNormal/HalfCauchy (per σ_i prior)"]

  N1 -->|"extend to dim K"| MvN
  LKJ -->|"R = L Lᵀ"| Sigma
  HN -->|"each σ_i"| Sigma
  IG -->|"each σ_i² (alternative)"| Sigma
  Sigma -->|"parameter"| MvN
  Wish -->|"prior on Σ⁻¹ (classical)"| MvN
  Wish -.->|"deprecated (use LKJ + scale)"| LKJ
```

PyMC and Stan prefer the **LKJ + scale** decomposition over **Wishart**.
hanalyze samples the Cholesky factor of R via `lkjCorrCholesky`.

## 5. Time series / state-space

```mermaid
graph TD
  N["Normal (innovation)"]
  AR1["AR(1): x_t = φ x_{t-1} + ε_t"]
  RW["RandomWalk: x_t = x_{t-1} + ε_t"]
  GP["GaussianProcess (RBF/Matérn etc.)"]
  SS["State-Space (= AR + observation)"]

  N -->|"ε_t"| AR1
  AR1 -->|"φ = 1"| RW
  AR1 -->|"continuous-time limit + covariance function"| GP
  AR1 -->|"+ observation noise"| SS
  GP -->|"on a grid = MvNormal"| MvN["MvNormal"]
```

`ar1Latent` (J2) and `Hanalyze.Model.GP` (master) are implemented.

## 6. Truncation / censoring / mixtures

```mermaid
graph TD
  D["Distribution(d)"]
  T["Truncated(d, lo, hi)"]
  Cs["Censored(d, lo, hi)"]
  Mix["Mixture([w_k], [d_k])"]
  ZI["ZeroInflated(d, ψ)"]
  D -->|"range constraint (CDF correction)"| T
  D -->|"detection limit (CDF/SF likelihood)"| Cs
  D -->|"weighted sum of K components"| Mix
  Mix -->|"components are delta_0 and d (= structural zeros)"| ZI
```

`Truncated` / `Censored` / `Mixture` / `ZeroInflated*` can be defined for any base
distribution (those requiring a CDF are limited).

## 7. Angular data

```mermaid
graph TD
  VM["VonMises(μ, κ)"]
  N["Normal(μ, 1/√κ)"]
  Unf["Uniform(-π, π)"]
  WN["WrappedNormal"]
  VM -->|"κ → 0"| Unf
  VM -->|"κ → ∞"| N
  WN -.->|"approximately"| VM
```

`VonMises` is the "normal-like" distribution on angles.

## Summary table — when to use which?

| Data property | First choice | Overdispersed | Angular |
|---|---|---|---|
| 0/1 binary | `Bernoulli` | — | — |
| successes out of n | `Binomial(n, p)` | `BetaBinomial(n, α, β)` | — |
| counts | `Poisson(λ)` | `NegativeBinomial(μ, α)` | — |
| zero-inflated counts | `ZeroInflatedPoisson` | — | — |
| continuous (real) | `Normal(μ, σ)` | `StudentT(ν, μ, σ)` | — |
| continuous (positive) | `LogNormal` / `Gamma` / `Weibull` | — | — |
| proportion (0-1) | `Beta(α, β)` | — | — |
| multinomial (K-category counts) | `Multinomial(n, π)` | — | — |
| unit vector / simplex | `Dirichlet` | — | — |
| multivariate real | `MvNormal(μ, Σ)` | — | — |
| angle | — | — | `VonMises(μ, κ)` |

| Estimand | Conjugate prior | Weakly informative |
|---|---|---|
| `p` of Bernoulli/Binomial | `Beta(α, β)` | `Beta(2, 2)` etc. |
| `λ` of Poisson | `Gamma(α, β)` | `HalfNormal` |
| `π` of Multinomial | `Dirichlet(α₁,…)` | `Dirichlet(1,…)` |
| `μ` of Normal | `Normal(μ₀, σ₀)` | `Normal(0, large)` |
| `σ²` of Normal | `InverseGamma(α, β)` | `HalfNormal` / `HalfCauchy` |
| correlation matrix `R` | `LKJ(η)` | `LKJ(1)` (uniform) |

## References

- Equations and meaning of each distribution: [docs/bayesian/theory-distributions.md](theory-distributions.md).
- Gibbs sampling exploiting conjugate structure: [docs/bayesian/04-gibbs.md](04-gibbs.md).
