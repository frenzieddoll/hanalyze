# Multivariate mean tests — Hotelling T² and MANOVA

Module: `Hanalyze.Stat.Test` (`hanalyze-core`)

Tests differences between mean **vectors**, handling several response
variables at once. Running a t-test per variable inflates false positives
through multiplicity and ignores the correlation between variables;
Hotelling T² and MANOVA collapse it into a single test.

Like every other test here, the result comes back as the shared `TestResult`.

## 0. API

| Function | Null hypothesis |
|---|---|
| `hotellingsT2 :: Matrix Double -> Vector Double -> TestResult` | One sample: μ = μ₀ |
| `hotellingsT2TwoSample :: Matrix Double -> Matrix Double -> TestResult` | Two samples (equal covariance): μ_X = μ_Y |
| `manova :: [Matrix Double] -> TestResult` | One-way: all group means equal (Wilks' Λ) |

Input matrices are **rows = observations, columns = variables**, as hmatrix
`Matrix Double`.

## 1. Usage

```haskell
import qualified Numeric.LinearAlgebra as LA
import qualified Hanalyze.Stat.Test as ST

grpA, grpB :: LA.Matrix Double
grpA = LA.fromLists [[5.1,3.5],[4.9,3.0],[4.7,3.2],[4.6,3.1],[5.0,3.6]]
grpB = LA.fromLists [[6.3,3.3],[5.8,2.7],[7.1,3.0],[6.3,2.9],[6.5,3.0]]

main :: IO ()
main = do
  let t2 = ST.hotellingsT2TwoSample grpA grpB
      mv = ST.manova [grpA, grpB]
  print (ST.trMethod t2, ST.trStatistic t2, ST.trPValue t2)
  print (ST.trMethod mv, ST.trStatistic mv, ST.trPValue mv)
```

Output:

```
("Hotelling T\178 (2-sample)",33.06381127031984,2.713676352444471e-4)
("MANOVA (one-way, Wilks' \923)",33.06381127031985,2.7136763524444673e-4)
```

**With two groups, MANOVA is mathematically equivalent to Hotelling T²**, so
the statistic and p-value agree up to floating-point rounding. MANOVA earns
its keep from three groups onwards.

## 2. Assumptions and the "no result" case

These functions never throw. When an assumption fails they return a "no
result" `TestResult` (`trStatistic = 0`, `trPValue = 1/0` i.e. Infinity,
`trDf = Nothing`, reason in `trNote`) — **always check `trNote`**.

| Function | Requirements |
|---|---|
| `hotellingsT2` | n ≥ 2 observations, p ≥ 1 variables, **n > p** (non-singular covariance), `length μ₀ == p` |
| `hotellingsT2TwoSample` | n ≥ 2 per group, matching variable counts, **n₁ + n₂ > p + 1** |
| `manova` | k ≥ 2 groups, n ≥ 2 per group, matching variable counts across groups |

**n > p** is the constraint you hit most often in practice. With more
variables than observations, reduce the dimension first (see
[02-pca.md](02-pca.md)).

## 3. Reading the result

| Field | Content |
|---|---|
| `trMethod` | `"Hotelling T² (2-sample)"` / `"MANOVA (one-way, Wilks' Λ)"`, … |
| `trStatistic` | Test statistic (after the F transform) |
| `trDf` | `Just (df1, Just df2)` — numerator and denominator degrees of freedom |
| `trPValue` | p-value |
| `trNote` | Why the test could not be computed, when applicable |

Once the joint test is significant and you want to know *which* variable
drives it, move on to a per-variable comparison
([usage-group-comparison.md](usage-group-comparison.md)) or individual tests
with multiplicity correction.

## 4. Choosing a test

| Situation | Use |
|---|---|
| One group against a known reference vector | `hotellingsT2` |
| Two groups | `hotellingsT2TwoSample` (= two-group MANOVA) |
| Three or more groups | `manova` |
| Rank the individual variables | [usage-group-comparison.md](usage-group-comparison.md) |
| A single variable | [01-test.md](01-test.md) |

## See also

- Package: [hanalyze-core](../../hanalyze-core/README.md)
- Tests in general: [01-test.md](01-test.md)
- Multiple testing: [06-multipletesting.md](06-multipletesting.md)
