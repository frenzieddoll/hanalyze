# Variational Inference (Stat.VI — ADVI)

> 🌐 **English** | [日本語](05-variational-inference.ja.md)

> Related demos:
> - [`vi-demo`](../demo/VIDemo.hs) — VI vs NUTS accuracy / speed comparison

## Overview

A mean-field normal **ADVI** (Automatic Differentiation Variational Inference,
Kucukelbir et al. 2017) implementation.

Whereas MCMC samples *exactly* from the posterior, VI **approximates** the posterior
with a Gaussian family — much faster, but less accurate when the posterior has
strong dependencies between parameters.

### Algorithm sketch

1. **Transform**: map constrained parameters into unconstrained space (same transforms as HMC/NUTS)
2. **Family**: `q(u; φ) = Π_i Normal(u_i; μ_i, σ_i)` (mean field — assumes independence)
3. **Objective**: maximize the ELBO
   `ELBO = E_q[log p(θ,y) + log|J|] + Σ_i H[Normal(μ_i, σ_i)]`
4. **Gradient**: reparameterization trick `u = μ + σ⊙ε, ε~N(0,I)`, finite-difference gradient
5. **Optimizer**: Adam

---

## API

```haskell
import Stat.VI

data VIConfig = VIConfig
  { viIterations   :: Int     -- Adam iterations
  , viSamples      :: Int     -- MC samples for ELBO gradient (recommended: 5-10)
  , viLearningRate :: Double  -- Adam learning rate α
  , viBeta1        :: Double  -- Adam β₁ (default 0.9)
  , viBeta2        :: Double  -- Adam β₂ (default 0.999)
  , viEpsilon      :: Double  -- Adam ε (default 1e-8)
  , viNumDraws     :: Int     -- samples drawn from q after convergence
  , viGradStep     :: Double  -- finite-difference step (default 1e-5)
  }

defaultVIConfig :: VIConfig
-- viIterations=1000, viSamples=5, viLearningRate=0.1, viNumDraws=2000

advi :: ModelP r -> VIConfig -> Params -> GenIO -> IO VIResult
```

---

## VIResult contents

```haskell
data VIResult = VIResult
  { viPostMeans   :: Params    -- posterior means (constrained space)
  , viPostSDs     :: Params    -- posterior SDs   (constrained space)
  , viMuU         :: [Double]  -- variational mean μ (unconstrained)
  , viSigmaU      :: [Double]  -- variational SD  σ (unconstrained)
  , viElboHistory :: [Double]  -- ELBO trajectory (for convergence check)
  , viDraws       :: [Params]  -- posterior samples (constrained, viNumDraws of them)
  }
```

---

## Example: Beta-Binomial model

A model with an analytical solution lets you check VI accuracy directly.

```haskell
import Stat.VI
import qualified Data.Map.Strict as Map
import System.Random.MWC (createSystemRandom)

clinicalModel :: ModelP ()
clinicalModel = do
  pCtrl <- sample "p_ctrl" (Beta 1 1)
  pTrt  <- sample "p_trt"  (Beta 1 1)
  observe "y_ctrl" (Binomial 50 pCtrl) [18]
  observe "y_trt"  (Binomial 50 pTrt)  [31]

main :: IO ()
main = do
  gen <- createSystemRandom
  let cfg = defaultVIConfig
              { viIterations = 500
              , viSamples    = 10
              , viNumDraws   = 5000
              }
      initP = Map.fromList [("p_ctrl", 0.5), ("p_trt", 0.5)]

  result <- advi clinicalModel cfg initP gen

  -- Posterior mean / SD
  print (viPostMeans result)  -- fromList [("p_ctrl", 0.3725), ("p_trt", 0.6078)]
  print (viPostSDs   result)  -- fromList [("p_ctrl", 0.0623), ("p_trt", 0.0651)]

  -- Analytical: Beta(1+k, 1+n-k)
  -- p_ctrl: mean=19/52=0.3654, SD=0.0644
  -- p_trt:  mean=32/52=0.6154, SD=0.0664

  -- Estimate P(p_trt > p_ctrl)
  let draws  = viDraws result
      diffVI = [ (d Map.! "p_trt") - (d Map.! "p_ctrl") | d <- draws ]
      probVI = fromIntegral (length (filter (>0) diffVI)) / fromIntegral (length diffVI) :: Double
  printf "P(p_trt > p_ctrl) = %.4f\n" probVI  -- ≈ 0.9948
```

---

## Checking ELBO convergence

```haskell
let hist  = viElboHistory result
    n     = length hist
    steps = [1, n `div` 4, n `div` 2, 3 * n `div` 4, n]
forM_ steps $ \i ->
  printf "iter %4d: ELBO = %.3f\n" i (hist !! (i-1))

-- iter    1: ELBO = -8.241
-- iter  125: ELBO = -5.823
-- iter  250: ELBO = -5.614
-- iter  375: ELBO = -5.601
-- iter  500: ELBO = -5.598
-- (ELBO change shrinks as it converges)
```

---

## Tuning VIConfig

| Parameter | Typical range | Note |
|---|---|---|
| `viIterations` | 500–2000 | Increase until ELBO stabilizes |
| `viSamples` | 5–15 | More = better gradient, slower |
| `viLearningRate` | 0.05–0.2 | Lower (0.01–0.05) if unstable |
| `viNumDraws` | 2000–10000 | Directly affects SD estimate quality |

---

## VI vs NUTS — empirical comparison

```
=== Model 1: Beta-Binomial (clinical trial) ===

               p_ctrl                    p_trt             time
VI       mean=0.3688 SD=0.0631   mean=0.6098 SD=0.0637   0.218s
NUTS     mean=0.3651 SD=0.0645   mean=0.6148 SD=0.0661   1.432s
Analyt.  mean=0.3654 SD=0.0644   mean=0.6154 SD=0.0664

=== Model 2: Hierarchical normal (3 schools) ===

  param     VI mean   VI SD   |  NUTS mean  NUTS SD
  mu          73.060   6.752  |    73.053   15.562
  tau         16.234   5.893  |    16.047    8.945
  theta_1     71.602   5.741  |    71.440    7.803

  Note: mean-field VI ignores parameter correlations,
        so it underestimates SDs in hierarchical models (mu SD: 6.75 vs 15.56)
```

---

## VI limitations & when to use what

| Situation | Recommendation |
|---|---|
| Large model, fast approximation needed | VI |
| Analytical solution available (e.g. Beta-Binomial) | VI is sufficient |
| Strongly correlated posterior (hierarchical) | NUTS (VI underestimates SDs) |
| Need rigorous uncertainty quantification | NUTS |

**Fundamental limitation of mean-field VI**: the variational family
`Π_i Normal(u_i; μ_i, σ_i)` cannot represent parameter correlations. In
hierarchical models where μ and τ are strongly correlated, VI overstates posterior
precision.

To mitigate this you'd need full-rank covariance VI or normalizing-flow VI;
the current implementation supports mean field only.
