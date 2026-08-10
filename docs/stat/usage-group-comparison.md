# Good vs Bad — bulk two-group comparison

Module: `Hanalyze.Stat.GroupComparison` (`hanalyze-core`)

Splits observations into two groups by a boolean label (good / bad, pass /
fail) and **compares many explanatory variables at once, ranked by effect
size** — the equivalent of Spotfire's "Good vs Bad". Typical use is
manufacturing / semiconductor quality analysis: which parameter is most
associated with the failures?

It is a helper optimised for **parallel comparison of many variables**, not a
rigorous single test. For one variable, use `Hanalyze.Stat.Test`
directly.

## 0. API

```haskell
goodVsBad
  :: [(Text, Vector Double)]   -- (variable name, values)
  -> Vector Bool               -- labels (True = good, False = bad)
  -> Either Text [GroupCompResult]
```

For each variable it computes (i) the mean difference, (ii) Cohen's d and
(iii) the Welch t-test p-value, and returns the results **sorted by
|Cohen's d| descending**.

`Left` is returned when: the variable list is empty, the labels are empty, a
variable's length does not match the labels, or either group has fewer than 2
observations (required by Welch's t-test).

## 1. Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Vector as V
import Hanalyze.Stat.GroupComparison

main :: IO ()
main = case goodVsBad vars labels of
  Left err -> putStrLn ("error: " ++ show err)
  Right rs -> mapM_ (\r -> print (gcrVarName r, gcrMeanDiff r, gcrEffect r, gcrPValue r)) rs
  where
    labels = V.fromList [True, True, True, True, False, False, False, False]
    vars =
      [ ("temp",     V.fromList [200, 202, 199, 201, 215, 218, 212, 216])
      , ("pressure", V.fromList [ 30,  31,  29,  30,  31,  29,  30,  30])
      ]
```

Output:

```
("temp",14.75,7.41371417676301,2.504838886050367e-4)
("pressure",0.0,0.0,1.0)
```

`temp` averages 14.75 higher in the bad group with an extreme Cohen's d of
7.41; `pressure` shows no difference. The result is **already ranked**, so the
first few entries are the variables that matter.

## 2. The `GroupCompResult` type

| Field | Content |
|---|---|
| `gcrVarName` | Variable name |
| `gcrMeanG` / `gcrMeanB` | Mean of the good (`True`) / bad (`False`) group |
| `gcrMeanDiff` | **Mean(bad) − Mean(good)** (mind the sign) |
| `gcrEffect` | Cohen's d (signed; ranking uses the absolute value) |
| `gcrPValue` | Two-sided Welch t-test p-value |
| `gcrNG` / `gcrNB` | Group sizes |

## 3. Multiple-comparison correction

`goodVsBad` returns **raw, uncorrected p-values**. The more variables you
screen, the more false positives, so pass them through
`Hanalyze.Stat.MultipleTesting`:

```haskell
import qualified Hanalyze.Stat.MultipleTesting as MT

let ps       = map gcrPValue rs
    adjusted = MT.benjaminiHochberg ps    -- FDR control
```

## 4. Choosing an approach

| Goal | Use |
|---|---|
| Screen many variables and rank them | `goodVsBad` (this doc) |
| Test a single variable rigorously | [`Stat.Test`](01-test.md), e.g. `tTestWelch` |
| Judge the size of the difference from a posterior | [Bayesian A/B test](../bayesian/usage-bayesian-ab-test.md) |
| Test all variables jointly | [Hotelling T² / MANOVA](usage-multivariate-test.md) |

## See also

- Package: [hanalyze-core](../../hanalyze-core/README.md)
- Interpreting effect sizes: [09-effect.md](09-effect.md)
- Multiple testing: [06-multipletesting.md](06-multipletesting.md)
