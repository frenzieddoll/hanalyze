# Model Comparison (Stat.ModelSelect)

> 🌐 **English** | [日本語](06-model-comparison.ja.md)

> Related demos:
> - [`gibbs-demo`](../demo/GibbsDemo.hs) — WAIC/LOO comparison of two models
> - [`simpson-paradox`](../demo/SimpsonParadoxDemo.hs) — LM/GLMM/HBM WAIC side-by-side in one HTML
> - [`hbm-random-slope`](../demo/HBMRandomSlopeDemo.hs) — random intercept vs +random slope ΔWAIC
>
> CLI: `--waic` flag embeds WAIC/LOO into LM / GLM / GLMM / HBM reports.

## Overview

`Stat.ModelSelect` provides information-criterion-based model comparison from MCMC chains.

| Criterion | Functions | Description |
|---|---|---|
| WAIC | `waic`, `chainWAIC` | Widely Applicable Information Criterion |
| PSIS-LOO | `loo`, `chainLOO` | Pareto Smoothed Importance Sampling LOO-CV |

Both are on the `−2×elpd` scale — **smaller is better**.

---

## WAIC (Widely Applicable Information Criterion)

**Principle (Watanabe 2010):**
- `lppd = Σᵢ log(E_θ[p(yᵢ|θ)])` — log pointwise predictive density
- `p_waic = Σᵢ Var_θ[log p(yᵢ|θ)]` — effective number of parameters
- `WAIC = −2(lppd − p_waic)` — analogous to AIC

### API

```haskell
import Stat.ModelSelect

data WAICResult = WAICResult
  { waicValue :: Double  -- WAIC = −2(lppd − p_waic)
  , waicLppd  :: Double  -- log pointwise predictive density
  , waicPwaic :: Double  -- effective parameter count p_waic
  , waicSE    :: Double  -- estimated standard error of WAIC
  }

-- Compute directly from a chain (recommended)
chainWAIC :: ModelP r -> Chain -> WAICResult

-- From a log-likelihood matrix (rows=samples, cols=observations)
waic :: [[Double]] -> WAICResult

-- Build the log-lik matrix (used internally by chainWAIC)
chainLogLikMatrix :: ModelP r -> Chain -> [[Double]]
```

### Example: comparing two models

```haskell
import Stat.ModelSelect
import MCMC.NUTS (nuts, defaultNUTSConfig)

-- Model A: weakly informative prior
modelA :: ModelP ()
modelA = do
  mu <- sample "mu" (Normal 0 10)
  observe "y" (Normal mu 2) obsData

-- Model B: informative prior centered away from the truth (true μ=3, prior μ≈5)
modelB :: ModelP ()
modelB = do
  mu <- sample "mu" (Normal 5 1)
  observe "y" (Normal mu 2) obsData

main :: IO ()
main = do
  gen <- createSystemRandom
  let cfg   = defaultNUTSConfig { nutsIterations = 5000 }
      initP = Map.fromList [("mu", 0.0)]

  chainA <- nuts modelA cfg initP gen
  chainB <- nuts modelB cfg initP gen

  let waicA = chainWAIC modelA chainA
      waicB = chainWAIC modelB chainB

  printf "Model A: WAIC=%.3f  lppd=%.3f  p_waic=%.3f  SE=%.3f\n"
    (waicValue waicA) (waicLppd waicA) (waicPwaic waicA) (waicSE waicA)
  printf "Model B: WAIC=%.3f  lppd=%.3f  p_waic=%.3f  SE=%.3f\n"
    (waicValue waicB) (waicLppd waicB) (waicPwaic waicB) (waicSE waicB)

  let delta = waicValue waicA - waicValue waicB
  printf "ΔWAIC(A−B) = %.3f\n" delta
  -- delta < -2 → Model A is significantly better
```

---

## PSIS-LOO (Pareto Smoothed Importance Sampling LOO-CV)

**Principle (Vehtari, Gelman, Gabry 2017):**
- Approximate the leave-one-out predictive density via importance sampling
- Smooth the importance-weight tail with a generalized Pareto distribution
- **Pareto k̂** diagnoses estimate reliability per observation

### Interpreting Pareto k̂

| k̂ | Diagnosis |
|---|---|
| < 0.5 | Good — LOO estimate is reliable |
| 0.5–0.7 | Acceptable — slightly unstable but usable |
| > 0.7 | Concerning — LOO is unstable; prefer WAIC, or examine those observations |

### API

```haskell
data LOOResult = LOOResult
  { looValue   :: Double    -- −2 × elpd_loo (smaller is better)
  , looElpd    :: Double    -- Σᵢ elpd_i
  , looSE      :: Double    -- standard error
  , looKHat    :: [Double]  -- per-observation k̂
  , looKHatBad :: Int       -- count of observations with k̂ > 0.7
  }

chainLOO :: ModelP r -> Chain -> LOOResult
loo      :: [[Double]] -> LOOResult
```

### Example: LOO with k̂ diagnostic

```haskell
let looRes = chainLOO modelA chainA
printf "LOO = %.3f  elpd = %.3f  SE = %.3f  k̂>0.7: %d obs\n"
  (looValue looRes) (looElpd looRes) (looSE looRes) (looKHatBad looRes)

-- Per-observation k̂
mapM_ (\(i, k) -> printf "obs %2d: k̂=%.3f  %s\n" (i::Int) k (khatLabel k))
  (zip [1..] (looKHat looRes))

khatLabel :: Double -> String
khatLabel k | k < 0.5   = "good"
            | k < 0.7   = "ok"
            | otherwise = "concerning"
```

---

## Posterior helpers for frequentist models

For LM / GLM / LME (which don't produce a posterior chain directly), there are
helpers to draw posterior log-likelihood matrices:

```haskell
-- Flat-prior conjugate posterior for LM
lmPosteriorLogLiks
  :: Matrix Double  -- design matrix X (n×p)
  -> Vector Double  -- response y (n)
  -> FitResult      -- OLS fit
  -> Int            -- number of posterior samples
  -> GenIO
  -> IO [[Double]]

-- Laplace approximation around MLE for GLM
glmPosteriorLogLiks
  :: Family -> LinkFn -> Matrix Double -> Vector Double
  -> Matrix Double  -- inverse Fisher information
  -> FitResult -> Int -> GenIO -> IO [[Double]]

-- Conditional posterior for LME (BLUP fixed)
lmePosteriorLogLiks
  :: Matrix Double -> Vector Double
  -> [Double]       -- BLUP offset per observation
  -> FitResult -> Int -> GenIO -> IO [[Double]]
```

The CLI's `--waic` flag uses these internally for LM / GLM / GLMM.

---

## WAIC vs LOO — when to use which

| Situation | Recommendation |
|---|---|
| Routine model comparison | WAIC (lighter to compute) |
| Need per-observation diagnostics | LOO (k̂ included) |
| Many k̂ > 0.7 | LOO unreliable — increase samples or use WAIC |
| Standard practice for Bayesian model selection | LOO (recommended by Vehtari et al.) |

---

## Empirical example

```
=== Section 2: WAIC model comparison ===
  Model A: μ ~ Normal(0, 10)  [weak prior; covers true μ=3 broadly]
  Model B: μ ~ Normal(5,  1)  [informative prior at μ≈5; pulls away from truth]

  Model A  posterior mean=3.2551 (analytical=3.2553)  WAIC= 97.038  lppd=-44.636  p_waic=3.883  SE=5.034
  Model B  posterior mean=4.3981 (analytical=4.4048)  WAIC=108.424  lppd=-51.325  p_waic=2.887  SE=5.621

  ΔWAIC(A − B) = -11.386
  → Model A (weak prior) fits better ✓

=== Section 3: PSIS-LOO diagnostic ===
  Model A: LOO=97.135  elpd=-48.567  SE=5.047  k̂>0.7: 0 obs
  Model B: LOO=108.539 elpd=-54.269  SE=5.638  k̂>0.7: 0 obs

  Pareto k̂ diagnosis (Model A, per obs):
    obs  1: k̂=0.022  good
    obs  2: k̂=0.012  good
    ...
    obs 20: k̂=0.015  good
```

**Interpretation:**
- ΔWAIC = -11.4 is more than 2× the SE → Model A is significantly better
- The weak prior allows the posterior to concentrate around the true μ=3, while
  Model B's informative prior pulls strongly to μ=5, increasing prediction error
