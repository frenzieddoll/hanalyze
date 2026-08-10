# Statistical process control (SPC) — control charts and rules

Module: `Hanalyze.Stat.SPC` (`hanalyze-core`)

Fits control charts and applies special-cause detection rules (Western
Electric / Nelson). Fitting and rule checking are separated: the rules are
pure functions over a fitted `SPCChartResult`.

## 0. Overview

| API | Role |
|---|---|
| `fitSPC :: SPCChart -> SPCInput -> Either Text [SPCChartResult]` | Fits a chart. X̄-R and I-MR return two charts |
| `checkRules :: [SPCRule] -> SPCChartResult -> [SPCViolation]` | Applies rules and lists violating points |
| `westernElectricRules` / `nelsonRules` | The built-in rule sets |

## 1. Chart kinds and their inputs

`fitSPC` returns `Left` when the chart kind and the input do not match.

| `SPCChart` | Matching `SPCInput` | Use |
|---|---|---|
| `XR` | `VarSubgroups (Vector (Vector Double))` | Subgroup mean and range. All subgroups must have the same size (2–15) |
| `IMR` | `VarIndividual (Vector Double)` | Individuals + moving range (when subgroups are not available) |
| `P` | `AttrProportion (Vector Int) (Vector Int)` | Defective proportion (defectives, sample size); sample size may vary |
| `NP` | `AttrCount (Vector Int) Int` | Defective count with a constant sample size |
| `C` | `AttrDefects (Vector Int)` | Defects per unit (constant unit size) |
| `U` | `AttrDefectRate (Vector Int) (Vector Int)` | Defect rate per unit (variable unit size) |
| `EWMAChart` | `EWMAInput xs λ L μ₀ σ₀` | Exponentially weighted moving average; good at small mean shifts |
| `CUSUMChart` | `CUSUMInput xs μ₀ σ₀ k h` | Two-sided cumulative sum (C⁺ / C⁻); good at sustained shifts |

For EWMA / CUSUM, passing `σ₀ <= 0` substitutes the sample standard deviation
of `xs`.

X̄-R limits use the Montgomery (9th ed.) Appendix VI constants (A2 / D3 / D4 /
d2), so a subgroup size outside 2–15 yields `Left`.

## 2. Basic usage (X̄-R chart)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Vector as V
import Hanalyze.Stat.SPC

subs :: V.Vector (V.Vector Double)
subs = V.fromList (map V.fromList
  [ [10.1, 10.3,  9.8, 10.0]
  , [10.2,  9.9, 10.4, 10.1]
  , [ 9.7, 10.0,  9.9,  9.8]
  , [10.5, 10.6, 10.4, 10.7]
  , [10.0,  9.8, 10.2, 10.1]
  , [11.8, 11.6, 11.9, 11.7]   -- clearly out of control
  ])

main :: IO ()
main = case fitSPC XR (VarSubgroups subs) of
  Left err     -> putStrLn ("error: " ++ show err)
  Right charts -> mapM_ report charts
  where
    report r = do
      putStrLn (show (spcChartName r) ++ ": CL=" ++ show (spcCenter r)
                ++ " UCL=" ++ show (V.head (spcUCL r))
                ++ " LCL=" ++ show (V.head (spcLCL r)))
      print (checkRules westernElectricRules r)
```

Output:

```
"X-bar": CL=10.395833333333334 UCL=10.675283333333335 LCL=10.116383333333333
[SPCViolation {vRuleName = "Western Electric 1", vRuleNumber = 1, vPointIndex = 5, vChartName = "X-bar"}
,SPCViolation {vRuleName = "Western Electric 3", vRuleNumber = 3, vPointIndex = 4, vChartName = "X-bar"}]
"R": CL=0.38333333333333314 UCL=0.8747666666666662 LCL=0.0
[]
```

The sixth subgroup (`vPointIndex = 5`, 0-origin) breaks the 3σ limit on the X̄
chart while the R chart stays in control — the level shifted, not the spread.

## 3. The `SPCChartResult` type

| Field | Content |
|---|---|
| `spcPoints` | Per-point statistic (X̄ / R / individual / MR / p̂ / np / c / u …) |
| `spcCenter` | Centre line (CL) |
| `spcUCL` / `spcLCL` | Control limits, **as a per-point Vector** |
| `spcSigma` | Estimated σ, used for zone A/B/C boundaries |
| `spcChartName` | `"X-bar"` / `"R"` / `"I"` / `"MR"` / `"p"` / `"np"` / `"c"` / `"u"` |

Invariant: `length spcPoints == length spcUCL == length spcLCL`. Fixed-limit
charts (X̄-R / I-MR / np / c) have constant UCL/LCL; variable-limit charts
(p / u) differ per point.

## 4. Rules

```haskell
data SPCRule = SPCRule
  { ruleName   :: Text                      -- "Western Electric 1", …
  , ruleNumber :: Int                       -- 1..8
  , ruleCheck  :: SPCChartResult -> [Int]   -- 0-origin indices of violations
  }
```

`westernElectricRules` and `nelsonRules` are the built-in sets. Rule 1 is the
same in both ("beyond 3σ"); the rest differ in how they detect runs and trends
(e.g. Nelson 2 = 9 points on the same side, Nelson 3 = 6 monotone points).

Since a rule set is just a list, you can extend or subset it:

```haskell
-- Detectors are plain SPCChartResult -> [Int] functions you write yourself;
-- the internal pattern helpers (runSameSide etc.) are not exported.
let overCenter r = [ i | (i, x) <- zip [0 ..] (V.toList (spcPoints r))
                       , x > spcCenter r + 2 * spcSigma r ]
    myRules = take 2 westernElectricRules
              ++ [SPCRule "custom: beyond CL+2sigma" 99 overCenter]
in checkRules myRules chart
```

## 5. Caveats

- On variable-limit charts (`P` / `U`), `spcSigma` is a representative value
  derived from the mean sample size, so zone-based rules (2σ / 1σ bands) are
  **approximate**. Rule 1 (beyond 3σ) is unaffected because it uses the
  per-point limits.
- X̄-R requires a subgroup size of 2–15; otherwise consider `IMR`,
  `EWMAChart` or `CUSUMChart`.
- For small (≈1σ) mean shifts, EWMA / CUSUM detect faster than X̄-R.

## 6. References

- Montgomery, D.C. *Introduction to Statistical Quality Control*, 9th ed. —
  source of the chart constants (Appendix VI) and the Western Electric rules
- Nelson, L.S. (1984) "The Shewhart Control Chart — Tests for Special Causes",
  *Journal of Quality Technology* 16(4)

## See also

- Package: [hanalyze-core](../../hanalyze-core/README.md)
- Group comparison: [usage-group-comparison.md](usage-group-comparison.md)
