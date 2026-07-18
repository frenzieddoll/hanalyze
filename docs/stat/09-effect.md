# Stat.Effect — Effect Size and Power Analysis

> 🌐 **English** | [日本語](09-effect.ja.md)

> Equivalent to R's `pwr` / Python's `statsmodels.stats.power`.

## 1. Effect Size

**Statistic complementing p-value** that measures the magnitude of an effect.

### Location

```haskell
import qualified Hanalyze.Stat.Effect as Eff

-- Cohen's d (two groups, pooled SD)
Eff.cohenD groupA groupB
-- 0.2 = small, 0.5 = medium, 0.8 = large

-- Paired samples
Eff.cohenDPaired pre post

-- Hedges' g (small-sample correction)
Eff.hedgesG groupA groupB  -- ≤ |cohenD|
```

### ANOVA / Regression

```haskell
-- η² (eta-squared)
Eff.eta2 [g1, g2, g3]    -- biased upward

-- ω² (omega-squared, unbiased)
Eff.omega2 [g1, g2, g3]

-- Cohen's f (from η²)
Eff.cohensF eta2Value  -- 0.10 small / 0.25 medium / 0.40 large
```

### Categorical

```haskell
-- Cramér's V (from χ²)
Eff.cramerV chi2 n rows cols

-- φ (for 2x2)
Eff.phiCoeff chi2 n

-- Odds ratio
Eff.oddsRatio ((20, 5), (5, 20))  -- 16.0
```

## 2. Power Analysis

### What Do You Want to Compute?

| Input → Output | Function |
|---|---|
| (n, α, effect) → power | `powerXxx` |
| (power, α, effect) → n | `sampleSizeXxx` |

### t-test

```haskell
-- power: n=30 per group, α=0.05, d=0.5 → ?
Eff.powerTTest 30 0.05 0.5
-- 0.475 (= 47.5% power)

-- Required sample: power=0.80, α=0.05, d=0.5 → ?
Eff.sampleSizeTTest 0.80 0.05 0.5
-- 64 (per group)
```

### ANOVA

```haskell
-- 4 groups, 20 per cell, α=0.05, f=0.25 → power
Eff.powerANOVA 20 4 0.05 0.25

-- Required cell count
Eff.sampleSizeANOVA 0.80 4 0.05 0.25
```

### Correlation

```haskell
-- ρ=0.3, n=50, α=0.05 → power
Eff.powerCorrelation 50 0.05 0.3
```

## 3. Examples

### A/B Test Design

```haskell
-- Expected effect d=0.3 (small-medium), achieve 80% power
let n = Eff.sampleSizeTTest 0.80 0.05 0.3
-- 175 per group

-- After experiment, compute observed effect size
let d = Eff.cohenD treatment control
    actualPower = Eff.powerTTest n 0.05 d
```

### Multi-group Experiment

```haskell
let f = Eff.cohensF (Eff.eta2 groups)
    needN = Eff.sampleSizeANOVA 0.80 (length groups) 0.05 f
```

## 4. Why Not Report p-value Alone

- p-value is **n-dependent** (increase n, any small effect reaches p < 0.05)
- Effect size is **n-independent**
- Academic standard: report p-value + effect size + CI together
