# Study Material 4 — Hamiltonian Monte Carlo and NUTS

> 🌐 **English** | [日本語](theory-hmc-nuts.ja.md)

> Geometry, implementation, and diagnostics of HMC and its auto-trajectory variant NUTS,
> for efficient sampling from high-dimensional posteriors.

## 1. The HMC idea

### 1.1 Physics analogy

Treat the posterior $\pi(\theta) \propto \exp(-U(\theta))$, $U(\theta) = -\log \pi(\theta)$
as a **potential energy**.

"Place a ball on a hill and kick it in some direction (= give it a momentum $p$)".
Move it under conservation, then use the new position as a proposal.

| Physics | Statistics |
|---|---|
| position $\mathbf{q}$ | parameter $\theta$ |
| momentum $\mathbf{p}$ | auxiliary variable ($\sim \text{Normal}(0, M)$) |
| potential $U$ | $-\log \pi(\theta)$ |
| kinetic energy $K$ | $\frac{1}{2} \mathbf{p}^T M^{-1} \mathbf{p}$ |
| Hamiltonian $H$ | $U + K$ |

### 1.2 Hamilton's equations

$$ \frac{d\theta}{dt} = \frac{\partial K}{\partial p} = M^{-1} p $$
$$ \frac{dp}{dt} = -\frac{\partial U}{\partial \theta} = \nabla \log \pi(\theta) $$

In continuous time **energy is conserved**, $H(\theta(t), p(t)) = H_0$.
Sampling from the uniform distribution on the joint level set is equivalent to sampling
from $\pi$ (see Neal 2011 for the details).

### 1.3 Why faster than MH

Each step performs **substantial physical motion**, exploring the posterior much faster
than a random walk. As a function of dimension $D$:

| Method | Steps per sample |
|---|---|
| Random Walk Metropolis | $O(D)$ |
| HMC | $O(D^{1/4})$ |

---

## 2. Leapfrog integration

### 2.1 Discretisation

Continuous time cannot be preserved exactly, so discretise with **leapfrog**:

```text
1. p_{1/2} ← p − (ε/2) ∇U(θ)        [half-step momentum]
2. θ' ← θ + ε M⁻¹ p_{1/2}            [position update]
3. p' ← p_{1/2} − (ε/2) ∇U(θ')       [second-half momentum]
```

Repeat $L$ times = $L \epsilon$ of integrated time.

### 2.2 Symplecticity

Leapfrog is **volume-preserving in phase space** and **time-reversible**.
It does not conserve energy exactly, but error does not accumulate over long horizons.

### 2.3 Metropolis correction

Leapfrog is approximate, so $H_0 \ne H'$. Correct via MH:

$$ \alpha = \min\!\left(1, \exp(H_0 - H')\right) $$

When the energy error is small acceptance is essentially always 1.

### 2.4 hanalyze implementation

```haskell
-- src/MCMC/HMC.hs
leapfrogWith :: ([Text] -> Params -> [Double])  -- gradFn
             -> [Text] -> Double -> Int          -- names, ε, L
             -> Params -> [Double]               -- θ, p
             -> (Params, [Double])               -- (θ', p')
```

`gradFn` is computed by AD via `Numeric.AD.Mode.Forward.grad`.

---

## 3. Constraint transforms (constrained → unconstrained)

HMC operates in $\mathbb{R}^D$, but parameters often have constraints (e.g. $\sigma > 0$,
$0 < p < 1$). **Map them to unconstrained space** before leapfrog.

### 3.1 Main transforms

| Constraint | Transform $u = T(\theta)$ | Inverse | log Jacobian |
|---|---|---|---|
| $\theta \in \mathbb{R}$ | $u = \theta$ | $\theta = u$ | 0 |
| $\theta > 0$ | $u = \log \theta$ | $\theta = e^u$ | $u$ |
| $\theta \in (0, 1)$ | $u = \text{logit}(\theta)$ | $\theta = \sigma(u)$ | $\log \sigma(u) (1-\sigma(u))$ |

### 3.2 Jacobian correction

Variable changes require a **Jacobian** in the density:

$$ p_U(u) = p_\Theta(\theta(u)) \left|\frac{d\theta}{du}\right| $$

In log-form:

$$ \log p_U(u) = \log p_\Theta(\theta(u)) + \log |J| $$

`logJointU` includes this correction.

### 3.3 hanalyze's `getTransforms`

```haskell
getTransforms :: ModelP r -> Map Text Transform
-- Inferred per latent from its prior:
-- Normal → UnconstrainedT
-- HalfNormal/Gamma/Exponential/InverseGamma/Weibull/Pareto → PositiveT
-- Beta/BetaBinomial → UnitIntervalT
-- ...
```

---

## 4. NUTS (No-U-Turn Sampler)

### 4.1 Motivation

HMC needs the user to specify the **trajectory length $L$**. Too short → poor exploration;
too long → wasted back-and-forth. NUTS chooses $L$ automatically.

### 4.2 Algorithm (Hoffman & Gelman 2014)

1. **Build a binary tree**: at each depth, double the time (forward / backward at random).
2. **U-turn detection**: at the trajectory ends $\theta_-, \theta_+$ with momenta
   $p_-, p_+$, stop when

   $$ (\theta_+ - \theta_-) \cdot p_- < 0 \quad \text{or} \quad (\theta_+ - \theta_-) \cdot p_+ < 0 $$

   (the trajectory has looped).
3. **Pick a proposal uniformly from the entire tree** (slice variable + ratio selection
   to preserve detailed balance).

### 4.3 Step-size adaptation via dual averaging

Stan-style: during burn-in, drive the step size $\epsilon$ towards a **target acceptance
rate** (default 0.8) using Nesterov dual averaging:

$$ \log \epsilon_{n+1} = \mu - \frac{\sqrt{n}}{\gamma} \bar H_n $$

with $\bar H_n$ the cumulative deviation from the target. Fixed after burn-in.

### 4.4 hanalyze implementation

```haskell
import MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))

ch <- nuts model
        defaultNUTSConfig
          { nutsIterations    = 2000
          , nutsBurnIn        = 1000
          , nutsStepSize      = 0.1     -- initial; tuned by dual averaging
          , nutsTargetAccept  = 0.8     -- target acceptance
          , nutsMaxDepth      = 10      -- max tree depth
          , nutsAdaptStepSize = True
          }
        init0 gen
```

---

## 5. Diagnostics: BFMI and divergences

### 5.1 BFMI (Bayesian Fraction of Missing Information)

Proposed by Betancourt 2016. Computed from the energy history $\{H_t\}$:

$$ \text{BFMI} = \frac{E[(H_t - H_{t-1})^2]}{\text{Var}(H_t)} $$

| BFMI | Interpretation |
|---|---|
| < 0.3 | Pathological (momentum resampling fails to reach the posterior tails) |
| 0.3–0.5 | Watch out |
| > 0.5 | Healthy |

`Stat.MCMC.bfmi`; visualised with `Viz.MCMC.energyPlot`.

### 5.2 Divergences

When leapfrog integration error exceeds a threshold ($|\Delta H| > 1000$) it becomes a
**divergent transition**:

- High local curvature (= pathological posterior).
- Almost always fixed by **non-centered parameterisation**.

`MCMC.NUTS` records divergent iteration indices in `Chain.chainDivergences`.
Visualise their location in parameter space with `Viz.MCMC.pairScatterDiv`.

### 5.3 Canonical example: Neal's funnel

```text
v ~ Normal(0, 3)
x ~ Normal(0, exp(v/2))
```

Small $v$ → tiny scale for $x$; large $v$ → enormous scale → funnel-shaped posterior.
HMC's classic stress test. `noncentered-demo`:

| | Centered | Non-centered |
|---|---|---|
| BFMI | 0.65 | **1.02** |
| ESS(v) | 102 | **781** (×7.6) |
| Divergences | 127 / 2000 | **0** |

---

## 6. Non-centered parameterisation

### 6.1 Centered (pathological)

```haskell
v <- sample "v" (Normal 0 3)
x <- sample "x" (Normal 0 (exp (v / 2)))
```

### 6.2 Non-centered (recommended)

```haskell
v <- sample "v" (Normal 0 3)
x <- nonCenteredNormal "x" 0 (exp (v / 2))
-- Internally:
--   x_raw <- sample "x_raw" (Normal 0 1)
--   x = 0 + exp(v/2) * x_raw         (deterministic)
```

`x_raw` is independent of $v$, simplifying the posterior geometry.

---

## 7. Mass matrix

In leapfrog, momentum $p \sim \text{Normal}(0, M)$ with **mass matrix** $M$:

| Choice | Situation |
|---|---|
| $M = I$ | default |
| $M = \text{diag}(1/\hat\sigma_i^2)$ | normalise by per-component variance (Stan default) |
| $M = \hat\Sigma^{-1}$ | use full covariance (estimated during warmup) |

hanalyze currently uses $M = I$ (room for improvement).

---

## 8. NUTS implementation files

| File | Contents |
|---|---|
| `src/MCMC/NUTS.hs` | `nuts`, `nutsChains`, `buildTree`, `uTurn`, dual averaging |
| `src/MCMC/HMC.hs` | `hmc`, `leapfrogWith` (shared with NUTS), `gradUU` |
| `src/Stat/Distribution.hs` | `Transform` definition, `toUnconstrained`, `logJacobianAdj` |
| `src/Model/HBM.hs` | `getTransforms`, `gradADU`, `logJointUnconstrained` |

NUTS / HMC use AD, namely `Numeric.AD.Mode.Forward`.

---

## 9. Demos

```bash
# Energy plot + BFMI comparison (centered vs non-centered)
cabal run noncentered-demo

# Pair plot showing divergence positions
# → funnel-centered-pair.html (red Xs are divergent transitions)

# Integrated demo prints BFMI / divergences / energy together
cabal run integrated-demo
```

---

## Next steps

- VI / ADVI and model selection → [theory-advanced.md](theory-advanced.md).
- API-level overview: [03-mcmc-samplers.md](03-mcmc-samplers.md).
- Original papers:
  - HMC: Neal "MCMC using Hamiltonian dynamics" (2011).
  - NUTS: Hoffman & Gelman (2014).
  - BFMI: Betancourt "Diagnosing Suboptimal Cotangent Disintegrations" (2016).
