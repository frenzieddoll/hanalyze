# Bayesian Inference Advanced: SMC / Bridge Sampling / Bayes Factor / BMA (Phase 29)

> 🌐 **English** | [日本語](07-advanced-marginal-likelihood.ja.md)

> Manual for **marginal likelihood-based advanced features** added in Phase 29
> (2026-05-29). Extends existing HBM (`Hanalyze.Model.HBM`) + MCMC
> (NUTS / MH / Gibbs) posterior chains for **model comparison, hypothesis testing,
> and multi-model prediction** via API guide.

---

## 0. Overview

| Feature | API | Purpose |
|---|---|---|
| SMC (Sequential Monte Carlo) | `Hanalyze.MCMC.SMC.smc` | Efficient sampling of multi-modal posteriors; as a side effect, rough log marginal estimate |
| Bridge Sampling | `Hanalyze.Stat.BridgeSampling.bridgeSampling` | **High-precision log marginal likelihood estimate** (Meng-Wong 1996) |
| Bayes Factor | `Hanalyze.Stat.BayesFactor.bayesFactor` | Evidence strength between 2 models: BF_{10} = p(y\|M_1)/p(y\|M_0) + Kass-Raftery interpretation |
| BMA | `Hanalyze.Stat.BayesianModelAveraging.bayesianModelAveraging` | Posterior weights for K models + weighted average prediction |

Dependencies: BF / BMA use Bridge log marginal as input. SMC is an independent sampler.

---

## 1. SMC: Tempered Sequential Monte Carlo

For multi-modal or peaked posteriors where NUTS suffers from poor mixing,
sample stably using particle ensembles + temperature annealing.

```haskell
import qualified Hanalyze.MCMC.SMC as SMC
import qualified Hanalyze.Model.HBM as HBM
import qualified Data.Map.Strict as M
import qualified System.Random.MWC as MWC

model :: HBM.ModelP ()
model = do
  mu <- HBM.sample "mu" (HBM.Normal 0 10)
  HBM.observe "y" (HBM.Normal mu 1) [4.8, 5.2, 5.0, 4.9, 5.1]

main = do
  gen <- MWC.create
  let cfg = SMC.defaultSMCConfig ["mu"]
  res <- SMC.smc model cfg (M.fromList [("mu", 0)]) gen
  print (SMC.smcChain res)        -- Particle posterior as Chain
  print (SMC.smcLogMarginal res)  -- Rough log marginal estimate (note: bias)
  print (SMC.smcESSHistory res)   -- ESS per temperature step
```

**Important**: SMC's `smcLogMarginal` assumes **initial particles approximate the prior**.
This implementation generates initial particles as jittered Gaussian around `init_`,
which biases the estimate when the prior is broad. **Use Bridge Sampling for rigorous
log marginal.**

Configuration (`SMCConfig`):

- `smcNParticles`: Number of particles (typical: 500-2000)
- `smcNSteps`: Temperature steps T (typical: 10-50; more → higher accuracy)
- `smcMHIterations`: MH moves per temperature K (typical: 5-20)
- `smcESSThreshold`: Systematic resample when ESS < N · this (typical: 0.5)

---

## 2. Bridge Sampling: Log Marginal Likelihood

Estimate log marginal from an existing MCMC chain + diagonal Gaussian proposal
(auto-fit from chain) using Meng-Wong iterative formula.

```haskell
import qualified Hanalyze.MCMC.NUTS as NUTS
import qualified Hanalyze.Stat.BridgeSampling as BS

main = do
  gen <- MWC.create
  chain <- NUTS.nuts model NUTS.defaultNUTSConfig (M.fromList [("mu", 5)]) gen
  gen2 <- MWC.create
  br <- BS.bridgeSampling model BS.defaultBridgeConfig chain gen2
  print (BS.brLogMarginal br)   -- log p(y) estimate
  print (BS.brConverged br)     -- True if converged within tolerance
  print (BS.brIterations br)    -- Iterations to convergence
```

Configuration (`BridgeConfig`):

- `bcNProposal`: Proposal samples count (typical: ≈ chain size ≈ 500-2000)
- `bcMaxIter`: Iteration upper bound (typical: 100; usually < 20 for convergence)
- `bcTolerance`: Convergence criterion |Δ log r̂| < tol (typical: 1e-6)

**Accuracy evidence** (Phase 29-A2 unit test):
Gaussian × Gaussian model (μ ~ N(0, 10), 10 obs y=5) matches analytical solution
log p(y) = -12.7686 with **absolute error < 0.5**.

---

## 3. Bayes Factor: Evidence Strength Between 2 Models

```haskell
import qualified Hanalyze.Stat.BayesFactor as BF

main = do
  -- Obtain chains for M_0, M_1 separately
  ch0 <- NUTS.nuts m0 cfg init_ gen0
  ch1 <- NUTS.nuts m1 cfg init_ gen1
  gen2 <- MWC.create
  r <- BF.bayesFactor m0 ch0 m1 ch1 BS.defaultBridgeConfig gen2
  print (BF.bfLogE r)             -- log_e BF_{10}
  print (BF.bfLog10 r)            -- log_10 BF_{10}
  print (BF.interpretBF (BF.bfLogE r))  -- Negligible / Positive / Strong / VeryStrong
```

**Kass-Raftery interpretation** (1995 Table 1):

| log_e BF | log_10 BF | Interpretation |
|---|---|---|
| 0..1 | 0..0.5 | Negligible (weak) |
| 1..3 | 0.5..1.3 | Positive (substantial) |
| 3..5 | 1.3..2 | Strong |
| 5+ | 2+ | Very strong (decisive) |

---

## 4. BMA: True Bayesian Model Averaging

From Bridge-estimated log marginals, compute posterior weights for K models
and take weighted average of predictions. Returns **Bayes Factor / hypothesis test
consistent weights**, superior to pseudo-BMA (PSIS-LOO approximation).

```haskell
import qualified Hanalyze.Stat.BayesianModelAveraging as BMA

main = do
  -- Gather chains + Bridge log marginals for K models
  lms <- forM [m0, m1, m2] $ \m -> do
    ch <- NUTS.nuts m cfg init_ =<< MWC.create
    BS.brLogMarginal <$> BS.bridgeSampling m BS.defaultBridgeConfig ch =<< MWC.create
  let bma = BMA.bayesianModelAveraging lms Nothing  -- Nothing = uniform model prior
  print (BMA.bmaWeights bma)   -- Posterior model weights (sum to 1)

  -- Per-model predictions (posterior mean at x*) averaged by BMA
  let v0 = LA.fromList [1.0, 2.0, 3.0]
      v1 = LA.fromList [1.1, 2.1, 3.1]
      v2 = LA.fromList [1.2, 2.2, 3.2]
      avg = BMA.averagePredictions bma [v0, v1, v2]
  print avg
```

---

## 5. SMC vs Bridge Sampling Cross-check

Phase 29-A1/A2 positions SMC and Bridge as **independent estimation paths**:

- SMC log marginal: Incremental log-mean-weight over temperature schedule
- Bridge log marginal: Meng-Wong solution from posterior chain + Gaussian proposal

When both agree closely, it supports chain convergence + appropriate schedule.
Large divergence signals NUTS chain mixing issues / coarse SMC schedule / misfit proposal.

**Note**: SMC log marginal carries bias from initial particle distribution.
Use Bridge as primary, SMC as sanity check when precision matters.

---

## 6. Known Limitations / Cautions

- Bridge Sampling proposal is **diagonal Gaussian** (ignores correlations).
  Accuracy drops when posterior has strong correlations. Future: full covariance fit option.
- SMC is **advantaged for single chain on multi-modal** targets, but NUTS wins
  on unimodal posteriors in ESS / wall-time.
- BMA is sensitive to **absolute log marginal scale**. Take care when models use
  different priors / parameterizations (beware Lindley paradox).

---

## References

- Del Moral, Doucet, Jasra (2006) "Sequential Monte Carlo samplers". JRSSB 68.
- Meng & Wong (1996) "Simulating ratios of normalising constants". Statistica Sinica 6.
- Gronau et al. (2017) "A tutorial on bridge sampling". J. Math. Psych. 81.
- Kass & Raftery (1995) "Bayes factors". JASA 90.
- Hoeting, Madigan, Raftery, Volinsky (1999) "Bayesian Model Averaging:
  A Tutorial". Statistical Science 14(4).
