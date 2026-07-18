# Stat.Bootstrap — Bootstrap + Permutation Testing

> 🌐 **English** | [日本語](07-bootstrap.ja.md)

> Resampling-based statistical inference. CI and hypothesis testing possible
> without parametric assumptions.

## 1. Bootstrap

```haskell
import qualified Hanalyze.Stat.Bootstrap as Boot
import qualified System.Random.MWC as MWC

gen <- MWC.createSystemRandom

let xs = LA.fromList [1.2, 1.5, 0.8, 2.1, 1.7, ...]

-- Bootstrap distribution of the mean (1000 resamples)
distribution <- Boot.bootstrap 1000 Boot.sampleMean xs gen

-- 95% percentile CI
(lo, hi) <- Boot.bootstrapCI 5000 0.95 Boot.sampleMean xs gen

-- BCa (bias-corrected & accelerated, Efron 1987)
(loBca, hiBca) <- Boot.bootstrapBcaCI 5000 0.95 Boot.sampleMean xs gen
```

## 2. Permutation Testing

```haskell
-- Test mean difference between 2 groups under H0 (no difference)
let groupA = LA.fromList [1, 2, 3, 4, 5]
    groupB = LA.fromList [4, 5, 6, 7, 8]

(diff, pVal) <- Boot.permutationTest 5000 groupA groupB gen
-- diff = mean(A) - mean(B) = -3.0
-- pVal = 0.008  (significant)
```

## 3. Utility Statistics

```haskell
Boot.sampleMean   :: Vector Double -> Double
Boot.sampleVar    :: Vector Double -> Double  -- unbiased (n-1)
Boot.sampleMedian :: Vector Double -> Double
```

Any `Vector Double -> Double` can be passed to bootstrap.

## 4. CI Selection Guide

| CI Type | Application | Computation |
|---|---|---|
| `bootstrapCI` (percentile) | Statistic roughly symmetric | Standard |
| `bootstrapBcaCI` (BCa) | Statistic biased / skewed (e.g., median, var) | + jackknife (n-fold) |
| Normal-theory CI (`Hanalyze.Stat.Test`) | Normality holds | O(1) |

## 5. Best Practices

- **Resample count**: 5000+ for CI, 5000+ for testing
- **Reproducibility**: Fix seed via `MWC.initialize seed`
- **Non-bell-shaped statistic**: Use BCa (median, var, ratio, etc.)
- **Small sample (n < 30)**: Bootstrap more robust than parametric
