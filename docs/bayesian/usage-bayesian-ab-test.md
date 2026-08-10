# Bayesian A/B test — decisions from HDI and ROPE

Module: `Hanalyze.MCMC.BayesianTest` (`hanalyze-bayes`)

Samples the **posterior** of the mean difference (μ_B − μ_A) between two
groups with NUTS and decides using an HDI (highest density interval) against a
ROPE (region of practical equivalence). Where the frequentist
[`goodVsBad`](../stat/usage-group-comparison.md) screens many variables with
Welch t + Cohen's d, this examines **one comparison in depth**.

Unlike a p-value, a ROPE lets you make the positive claim that a difference is
practically zero.

## 0. Model

```
μ_A  ~ Normal(0, priorScale)      σ_A ~ HalfNormal(sigmaScale)
μ_B  ~ Normal(0, priorScale)      σ_B ~ HalfNormal(sigmaScale)
y_A  ~ Normal(μ_A, σ_A)           y_B ~ Normal(μ_B, σ_B)
diff = μ_B − μ_A
```

Each group gets its own σ, so **equal variance is not assumed** — the Bayesian
counterpart of Welch's t-test.

## 1. API

```haskell
bayesianAB
  :: BayesianABConfig
  -> [Double]        -- group A observations
  -> [Double]        -- group B observations
  -> MWC.GenIO
  -> IO BayesianABResult
```

`BayesianABConfig` (defaults in `defaultBayesianABConfig`):

| Field | Meaning |
|---|---|
| `babCredible` | Credible level of the HDI (default 0.95) |
| `babRule` | `HDIOnly` (no verdict) or `ROPEDecision lo hi` |
| `babPriorScale` | Prior σ for μ (default 10) |
| `babSigmaScale` | Scale of the HalfNormal prior on σ (default 5) |
| `babNUTS` | Sampler settings (`NUTSConfig`) |

`ROPEDecision lo hi` declares `[lo, hi]` to be practically indistinguishable
from zero:

| Condition | `ABDecision` |
|---|---|
| HDI does not overlap the ROPE | `RejectH0` (a real difference) |
| HDI lies entirely inside the ROPE | `AcceptH0` (practically zero) |
| Partial overlap | `Inconclusive` (not enough data) |
| `HDIOnly` was given | `NoRuleApplied` |

## 2. Usage

```haskell
import qualified System.Random.MWC as MWC
import Hanalyze.MCMC.BayesianTest

main :: IO ()
main = do
  gen <- MWC.create
  let cfg = defaultBayesianABConfig { babRule = ROPEDecision (-0.5) 0.5 }
      ysA = [4.8, 5.1, 4.9, 5.3, 5.0, 4.7, 5.2, 5.0]
      ysB = [6.1, 5.9, 6.4, 6.0, 6.3, 5.8, 6.2, 6.5]
  r <- bayesianAB cfg ysA ysB gen
  print (babMeanDiff r, babHDI r, babDecision r, babProbDiffPos r)
```

Output (`MWC.create` is a fixed seed):

```
(1.1499825845264315,(0.8868282100120641,1.428183701462828),RejectH0,1.0)
```

The posterior mean difference is 1.15 with a 95% HDI of [0.89, 1.43]. It does
not overlap the ROPE [-0.5, 0.5], hence `RejectH0`, and
`P(μ_B > μ_A) = 1.0` (every draw is positive).

## 3. The `BayesianABResult` type

| Field | Content |
|---|---|
| `babPosteriorDiff` | Post-burn-in draws of the mean difference (`[Double]`) |
| `babMeanDiff` | Posterior mean of μ_B − μ_A |
| `babHDI` | HDI at the `babCredible` level |
| `babDecision` | `ABDecision` |
| `babProbDiffPos` | Posterior P(μ_B > μ_A) |
| `babChain` | The raw chain (μ_A / μ_B / σ_A / σ_B / diff) |

`babChain` is the core `Chain` type, so it feeds straight into diagnostics
(`rhat` / `ess`) and visualisation (`Hanalyze.Viz.*`). **If the HDI
looks suspiciously narrow or the verdict is unstable, check convergence before
believing the result.**

## 4. Choosing a ROPE

A ROPE is a domain judgement, not a statistical one. Rules of thumb:

- Use the width of a difference that would not change your decision
  (e.g. ±0.5% yield).
- With no domain anchor, a "small effect" convention is common
  (Kruschke illustrates ±0.1 on a standardised scale).
- Or use `HDIOnly`, report the HDI, and let a human decide.

## 5. Choosing an approach

| Situation | Use |
|---|---|
| Screen many variables at once | [Good vs Bad](../stat/usage-group-comparison.md) |
| Judge one comparison from its posterior | `bayesianAB` (this doc) |
| Positively claim "no difference" | This doc (ROPE) or [Bayes factors](07-advanced-marginal-likelihood.ja.md) (ja) |
| A classical test suffices | [stat/01-test.md](../stat/01-test.md) |

## 6. References

- Kruschke, J.K. (2013) "Bayesian estimation supersedes the t test",
  *Journal of Experimental Psychology: General* 142(2) — BEST and the ROPE
- Hoffman & Gelman (2014) "The No-U-Turn Sampler", *JMLR* 15 — the sampler used

## See also

- Package: [hanalyze-bayes](../../hanalyze-bayes/README.md)
- Sampler configuration: [03-mcmc-samplers.md](03-mcmc-samplers.md)
- Convergence diagnostics: [viz-diagnostics.md](viz-diagnostics.md)
