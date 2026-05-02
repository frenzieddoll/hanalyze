# Gibbs Sampling (MCMC.Gibbs)

> 🌐 **English** | [日本語](04-gibbs.ja.md)

> Related demos:
> - [`gibbs-demo`](../demo/GibbsDemo.hs) — Gibbs + WAIC/LOO model comparison
> - [`gibbs-hbm-demo`](../demo/GibbsHBMDemo.hs) — HBM DSL × Gibbs (auto-conjugate detection)

## Overview

Gibbs sampling samples each parameter from its **conditional posterior given all others**.

**Pros:**
- Conjugate models give analytical conditional posteriors → **direct sampling** (no rejection)
- Without rejection steps, ESS/time often beats NUTS

**Cons:**
- You typically need to write update functions yourself (or use the auto-detection)
- Doesn't apply to non-conjugate parameters (combine with NUTS in that case)

---

## Built-in conjugate updates

### `normalNormal` — Normal-Normal conjugacy

Model: `μ ~ Normal(μ₀, σ₀)`, `yᵢ ~ Normal(μ, σ_lik)`

Conditional posterior: `μ | y ~ Normal(μ_post, σ_post)`
where `σ_post² = 1/(1/σ₀² + n/σ_lik²)`, `μ_post = σ_post² × (μ₀/σ₀² + nȳ/σ_lik²)`

```haskell
normalNormal
  :: Text     -- parameter name
  -> Double   -- prior mean μ₀
  -> Double   -- prior SD σ₀
  -> [Double] -- observed data y
  -> Double   -- known likelihood SD σ_lik
  -> GibbsUpdate
```

### `betaBinomial` — Beta-Binomial conjugacy

Model: `p ~ Beta(α, β)`, `y ~ Binomial(n, p)`, k successes observed

Conditional posterior: `p | y ~ Beta(α+k, β+n-k)`

```haskell
betaBinomial
  :: Text   -- parameter name
  -> Double -- prior α
  -> Double -- prior β
  -> Int    -- trials n
  -> Int    -- successes k
  -> GibbsUpdate
```

### `gammaPoisson` — Gamma-Poisson conjugacy

Model: `λ ~ Gamma(α, β)`, `yᵢ ~ Poisson(λ)`

Conditional posterior: `λ | y ~ Gamma(α + Σyᵢ, β + n)`

```haskell
gammaPoisson
  :: Text     -- parameter name
  -> Double   -- prior shape α
  -> Double   -- prior rate β
  -> [Double] -- observed data
  -> GibbsUpdate
```

---

## Basic usage

```haskell
import MCMC.Gibbs
import qualified Data.Map.Strict as Map
import System.Random.MWC (createSystemRandom)

obsData :: [Double]
obsData = [3.2, 1.8, 4.1, 2.9, 3.5]

main :: IO ()
main = do
  gen <- createSystemRandom

  let updates = [ normalNormal "mu" 0 10 obsData 2.0 ]  -- σ_lik = 2 known
      cfg     = defaultGibbsConfig { gibbsIterations = 5000, gibbsBurnIn = 500 }
      initP   = Map.fromList [("mu", 0.0)]

  chain <- gibbs updates cfg initP gen

  print (posteriorMean "mu" chain)  -- Just 3.06 (example)
  print (posteriorSD   "mu" chain)  -- Just 0.42 (example)
```

---

## GibbsConfig

```haskell
data GibbsConfig = GibbsConfig
  { gibbsIterations :: Int  -- post-burn-in samples
  , gibbsBurnIn     :: Int  -- burn-in steps to discard
  }

defaultGibbsConfig :: GibbsConfig
-- gibbsIterations=2000, gibbsBurnIn=500
```

---

## Multiple parameter updates

Pass multiple `GibbsUpdate`s — they are applied in order each iteration.

```haskell
-- Beta-Binomial: control + treatment groups together
let updates =
      [ betaBinomial "p_ctrl" 1 1 50 18  -- control: 18/50
      , betaBinomial "p_trt"  1 1 50 31  -- treatment: 31/50
      ]
chain <- gibbs updates cfg (Map.fromList [("p_ctrl", 0.5), ("p_trt", 0.5)]) gen
```

---

## Multi-chain runs

```haskell
-- gibbsChains uses independent RNG seeds per chain
chains <- gibbsChains updates cfg 4 initP gen

-- Convergence via R-hat
let r = rhat (map (chainVals "mu") chains)
print r  -- Just 1.000 (Gibbs typically converges immediately)
```

---

## Gibbs vs NUTS performance

```
=== Section 1: Gibbs vs NUTS (Normal mean estimation) ===

  Data: n=20, ȳ=3.255, σ_lik=2.0 (known), true μ=3.0

  Gibbs    mean= 3.2553  SD= 0.4399  ESS=4967.7  ESS/s=4827.9
  NUTS     mean= 3.2551  SD= 0.4392  ESS=4459.5  ESS/s=1243.3
  Analyt.  mean= 3.2553  SD= 0.4399
```

**Gibbs achieves ~3.9× the ESS/sec of NUTS on conjugate models.**
NUTS, however, applies to any model — choose Gibbs when you can.

---

## Auto-conjugate detection from a HBM model

`gibbsFromModel :: ModelP r -> ([GibbsUpdate], [Text])` inspects an HBM model and
returns conjugate updates for every detectable conjugate pair, plus the names of
non-conjugate parameters that need a different sampler.

```haskell
let model = do
      mu    <- sample "mu"    (Normal 0 10)        -- Normal-Normal conjugacy
      sigma <- sample "sigma" (Exponential 1)      -- non-conjugate (handled by MH)
      observe "y" (Normal mu sigma) ys

let (updates, nonConjugate) = gibbsFromModel model
-- updates       = [normalNormal "mu" 0 10 ys (current sigma)]
-- nonConjugate  = ["sigma"]
```

Use `gibbsMH` to run a hybrid Gibbs + MH chain in one call:

```haskell
-- Gibbs for "mu", random-walk MH for "sigma"
chain <- gibbsMH model cfg
                 (Map.fromList [("sigma", 0.3)])  -- MH step sizes
                 (Map.fromList [("mu", 0), ("sigma", 1)])
                 gen
```

---

## Writing custom update functions

Anything matching `GibbsUpdate = Params -> GenIO -> IO (Text, Double)` works.

```haskell
import MCMC.Gibbs (GibbsUpdate)
import qualified Data.Map.Strict as Map
import System.Random.MWC.Distributions (normal)

-- Custom: μ ~ Normal(0,10), y ~ Normal(μ,σ) conditional posterior
myMuUpdate :: [Double] -> Double -> GibbsUpdate
myMuUpdate ys sigLik _params gen = do
  let n       = fromIntegral (length ys) :: Double
      ybar    = sum ys / n
      precPri = 1 / 100            -- prior precision (σ₀=10)
      precLik = 1 / sigLik^2
      precPos = precPri + n * precLik
      muPos   = (0 * precPri + n * ybar * precLik) / precPos
      sigPos  = sqrt (1 / precPos)
  newMu <- normal muPos sigPos gen
  return ("mu", newMu)
```
