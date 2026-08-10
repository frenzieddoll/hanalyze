# Weibull maximum-likelihood estimation and B_p life

Module: `Hanalyze.Model.Weibull` (`hanalyze-models`)

Fits a Weibull distribution to lifetime / time-to-failure data, giving the
maximum-likelihood estimates of the shape k and scale λ, and the **B_p
life** (the time at which a fraction p of the population has failed). This
is the most fundamental distribution in semiconductor and materials
reliability assessment.

```
f(x) = (k/λ) (x/λ)^(k-1) exp(-(x/λ)^k)      x > 0
S(x) = exp(-(x/λ)^k)
```

Reading the shape k (bathtub curve):

| k | Failure rate | Typical |
|---|---|---|
| k < 1 | Decreasing over time | Infant mortality (burn-in period) |
| k = 1 | Constant | Exponential distribution (random failures) |
| k > 1 | Increasing | Wear-out failure |

For accelerated life testing, which models the relationship between stress
(temperature / voltage) and lifetime, see
[usage-reliability.md](usage-reliability.md).

## 0. API

```haskell
data WeibullFit = WeibullFit
  { wfShape  :: !Double                    -- k (> 0)
  , wfScale  :: !Double                    -- λ (> 0)
  , wfLogLik :: !Double                    -- MLE log-likelihood value
  , wfN      :: !Int                       -- total number of observations (including censored)
  , wfRObs   :: !Int                       -- number of failures (excluding censored)
  , wfFisher :: !(Double, Double, Double)  -- Fisher information matrix (I_kk, I_kλ, I_λλ)
  }

fitWeibullMLE      :: Vector Double -> Either Text WeibullFit
fitWeibullCensored :: Vector Double -> Vector Bool -> Either Text WeibullFit

bxLife   :: Double -> WeibullFit -> Double                      -- p -> B_p
bxLifeCI :: Double -> Double -> WeibullFit
         -> (Double, Double, Double)                            -- (estimate, lower, upper)

weibullParameterSE         :: WeibullFit -> (Double, Double)
weibullParameterCovariance :: WeibullFit -> (Double, Double, Double)
```

`Vector` is **`Data.Vector`** (boxed) — not `hmatrix`'s `Vector`.

## 1. Complete failure data

```haskell
import qualified Data.Vector as V
import Hanalyze.Model.Weibull

main :: IO ()
main = do
  let ts = V.fromList [ 5.1, 8.3, 11.0, 12.4, 15.9, 18.2, 21.5, 25.0 ]
  case fitWeibullMLE ts of
    Left err -> putStrLn ("error: " ++ show err)
    Right f  -> do
      print (wfShape f, wfScale f)
      print (bxLife 0.10 f)
      print (weibullParameterSE f)
      print (bxLifeCI 0.10 0.05 f)
```

Output:

```
(2.546174113334201,16.580410383584077)
6.851029056082719
(0.7451839904102958,2.11069724624789)
(6.851029056082719,2.433033727866307,11.269024384299133)
```

k = 2.55 > 1, so this is wear-out failure. The B_10 life is 6.85 (the time
at which 10% have failed), with a 95% confidence interval of 2.43 to 11.27.
With only n = 8, the interval is wide.

> **The second argument of `bxLifeCI` is α, not the confidence level**. Pass
> `0.05` for a 95% CI (passing `0.95` gives a 5% interval with z = 0.063).

## 2. Right-censored data

When a study includes units that were withdrawn before failing, pass
`fitWeibullCensored` an event indicator vector where **`True` = failed /
`False` = censored**.

```haskell
  let ts2 = V.fromList [ 5.1, 8.3, 11.0, 12.4, 15.9, 18.2, 21.5, 25.0 ]
      ev  = V.fromList [ True, True, True, True, True, False, False, False ]
  case fitWeibullCensored ts2 ev of
    Left err -> putStrLn ("error: " ++ show err)
    Right f  -> print (wfShape f, wfScale f, wfN f, wfRObs f)
```

```
(1.678639197705706,20.616867764344537,8,5)
```

With the same time series, treating the last 3 as "not yet failed" moves k
from 2.55 → 1.68 and λ from 16.6 → 20.6. Treating censored units as failures
(the §1 approach) **underestimates the lifetime**, so always use this
function for data from a study that was stopped early. `wfN` holds the total
count (8) and `wfRObs` the number of failures (5), which is useful for
checking the censoring rate.

## 3. Confidence interval for B_p life (delta method)

`bxLifeCI p α fit` propagates the variance of B_p via the delta method:

```
Var(B_p) ≈ (∂B_p/∂k)² Var(k) + (∂B_p/∂λ)² Var(λ) + 2 (∂B_p/∂k)(∂B_p/∂λ) Cov(k,λ)
```

The variance-covariance is obtained from the inverse of the Fisher
information matrix:

```haskell
  print (weibullParameterCovariance f)      -- (Var(k), Cov(k,λ), Var(λ))
```

```
(0.5552991795638119,0.6004104002867886,4.455042865318426)
```

`weibullParameterSE` is a convenience function returning the square root of
these diagonal entries (= `(0.745, 2.111)`). When the covariance is not
positive definite and the SE cannot be computed, `bxLifeCI` returns
`(estimate, estimate, estimate)` with zero interval width (the lower bound
is clipped to 0 since lifetime is non-negative).

## 4. Choosing an approach

| Goal | Use |
|---|---|
| Fit Weibull and get k / λ and B_p | `fitWeibullMLE` (this doc) |
| Test data including censored units | `fitWeibullCensored` (this doc) |
| Relationship between stress and lifetime (accelerated testing) | [usage-reliability.md](usage-reliability.md) |
| Nonparametric survival curve | [10-survival.md](10-survival.md) (Kaplan-Meier / Cox) |
| System reliability for series / parallel / k-of-n | [`Model.ReliabilityBlockDiagram`](../api-guide/07-survival.md) |
| The distribution's raw pdf / cdf / random generation | `Hanalyze.Stat.Distribution` (core layer; no dedicated doc yet) |

## See also

- Package: [hanalyze-models](../../hanalyze-models/README.md)
- Accelerated life testing: [usage-reliability.md](usage-reliability.md)
- Survival analysis: [10-survival.md](10-survival.md)
