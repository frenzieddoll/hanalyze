# Model comparison (Stat.ModelSelect)

> 🌐 **English** | [日本語](06-model-comparison.ja.md)

> Related demos:
> - [`gibbs-demo`](../demo/GibbsDemo.hs) — compares two models with WAIC / LOO
> - [`simpson-paradox`](../demo/SimpsonParadoxDemo.hs) — places LM/GLMM/HBM WAICs side by side in one HTML
> - [`hbm-random-slope`](../demo/HBMRandomSlopeDemo.hs) — ΔWAIC between random-intercept-only vs. + random-slope
>
> CLI: pass `--waic` to embed WAIC/LOO into LM / GLM / GLMM / HBM reports.

## Overview

`Hanalyze.Stat.ModelSelect` provides information-criterion-based model comparison from MCMC chains.

| Criterion | Function | Description |
|---|---|---|
| WAIC | `waic`, `chainWAIC` | Widely Applicable Information Criterion |
| PSIS-LOO | `loo`, `chainLOO` | Pareto Smoothed Importance Sampling LOO-CV |

**Both: smaller is better** (−2 × elpd scale).

---

## WAIC (Widely Applicable Information Criterion)

**Principle (Watanabe 2010)**:
- `lppd = Σᵢ log(E_θ[p(yᵢ|θ)])` — log pointwise predictive density.
- `p_waic = Σᵢ Var_θ[log p(yᵢ|θ)]` — effective number of parameters.
- `WAIC = −2(lppd − p_waic)` — AIC analogue.

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
chainWAIC :: Model a -> Chain -> WAICResult

-- Compute from a log-likelihood matrix (rows = samples, columns = observations)
waic :: [[Double]] -> WAICResult

-- Construct the log-likelihood matrix (used internally by chainWAIC)
chainLogLikMatrix :: Model a -> Chain -> [[Double]]
```

### Example: comparing two models

```haskell
import Stat.ModelSelect
import MCMC.NUTS (nuts, defaultNUTSConfig)

-- Model A: weakly informative prior
modelA :: Model ()
modelA = do
  mu <- sample "mu" (Normal 0 10)
  observe "y" (Normal mu 2) obsData

-- Model B: informative prior (assumes μ=5, away from the truth μ=3)
modelB :: Model ()
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
  -- delta < -2 → model A is significantly better
```

---

## PSIS-LOO (Pareto Smoothed Importance Sampling LOO-CV)

**Principle (Vehtari, Gelman, Gabry 2017)**:
- Approximate the LOO predictive density (each observation removed) by importance sampling.
- Smooth the tail of the importance weights with a Pareto distribution.
- Use **Pareto k̂** as a per-observation reliability diagnostic.

### Interpreting Pareto k̂

| k̂ | Diagnosis |
|---|---|
| < 0.5 | Good — LOO estimate is reliable. |
| 0.5–0.7 | Acceptable — slightly unstable, mostly usable. |
| > 0.7 | Watch out — LOO is unstable; prefer WAIC or inspect those observations. |

### API

```haskell
data LOOResult = LOOResult
  { looValue   :: Double    -- −2 × elpd_loo (smaller is better)
  , looElpd    :: Double    -- Σᵢ elpd_i
  , looSE      :: Double    -- estimated standard error
  , looKHat    :: [Double]  -- per-observation k̂
  , looKHatBad :: Int       -- count of k̂ > 0.7
  }

chainLOO :: Model a -> Chain -> LOOResult
loo      :: [[Double]] -> LOOResult
```

### Example: LOO + k̂ diagnostics

```haskell
let looRes = chainLOO modelA chainA
printf "LOO = %.3f  elpd = %.3f  SE = %.3f  k̂>0.7: %d obs\n"
  (looValue looRes) (looElpd looRes) (looSE looRes) (looKHatBad looRes)

-- Inspect per-observation k̂
mapM_ (\(i, k) -> printf "obs %2d: k̂=%.3f  %s\n" (i::Int) k (khatLabel k))
  (zip [1..] (looKHat looRes))

khatLabel :: Double -> String
khatLabel k | k < 0.5   = "good"
             | k < 0.7   = "acceptable"
             | otherwise = "watch out"
```

---

## WAIC vs LOO — when to use which

| Situation | Recommendation |
|---|---|
| Routine model comparison | WAIC (cheaper) |
| Need per-observation diagnostics | LOO (with k̂) |
| Many k̂ > 0.7 | Reliability lacking — increase samples or fall back to WAIC |
| Standard recommendation | LOO (Vehtari et al.) |

---

## Worked example

```
=== Section 2: WAIC model comparison ===
  Model A: μ ~ Normal(0, 10)  [weakly informative: covers μ=3 broadly]
  Model B: μ ~ Normal(5,  1)  [informative: strong assumption μ≈5, off from truth]

  Model A  posterior mean=3.2551 (analytic=3.2553)  WAIC=  97.038  lppd= -44.636  p_waic=3.883  SE=5.034
  Model B  posterior mean=4.3981 (analytic=4.4048)  WAIC= 108.424  lppd= -51.325  p_waic=2.887  SE=5.621

  ΔWAIC(A − B) = -11.386
  → Model A (weakly informative prior) fits better ✓

=== Section 3: PSIS-LOO diagnostics ===
  Model A: LOO=97.135  elpd=-48.567  SE=5.047  k̂>0.7: 0 obs
  Model B: LOO=108.539 elpd=-54.269  SE=5.638  k̂>0.7: 0 obs

  Pareto k̂ diagnostics (model A, per observation):
    obs  1: k̂=0.022  good
    obs  2: k̂=0.012  good
    ...
    obs 20: k̂=0.015  good
```

**Interpretation:**
- ΔWAIC = -11.4 is more than 2× the SE → model A is statistically significantly better.
- A weakly informative prior concentrates the posterior near the truth μ=3, while the
  informative prior on model B pulls toward μ=5 and disagrees more with the data.
