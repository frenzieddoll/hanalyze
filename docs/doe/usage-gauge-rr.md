# Gauge R&R (Measurement System Analysis)

Module: `Hanalyze.Design.GaugeRR` (`hanalyze-design`)

Measurement System Analysis (MSA) answers the question "is the variation in
this data really process variation, or is it just how we're measuring it?"
Gauge R&R decomposes measurement variance via ANOVA into

- **σ²_part** — the part-to-part variation (the signal we actually want)
- **σ²_repeatability** — variation when the same operator re-measures the
  same part (equipment-driven)
- **σ²_reproducibility** — variation between operators / instruments

following the AIAG MSA Manual, 4th ed.

| API | Layout |
|---|---|
| `gaugeRRCrossed` | **Crossed** — every operator measures every part (the usual case) |
| `gaugeRRNested` | **Nested** — each operator measures a different set of parts (e.g. destructive testing) |

## 0. API

```haskell
gaugeRRCrossed
  :: V.Vector Int       -- operator ID (length n)
  -> V.Vector Int       -- part ID     (length n)
  -> V.Vector Double    -- measurement (length n)
  -> Either Text GaugeRRResult

gaugeRRNested :: V.Vector Int -> V.Vector Int -> V.Vector Double
              -> Either Text GaugeRRResult
```

`V.Vector` is **`Data.Vector`** (boxed). The three vectors must share the
same length and be passed in "long format" — one row per measurement.

## 1. Usage

A crossed trial with 2 operators × 3 parts × 2 repeats = 12 measurements.

```haskell
import qualified Data.Vector as V
import Hanalyze.Design.GaugeRR

main :: IO ()
main = do
  let ops   = V.fromList [1,1,1,1,1,1, 2,2,2,2,2,2]
      parts = V.fromList [1,1,2,2,3,3, 1,1,2,2,3,3]
      ys    = V.fromList [ 10.1, 10.2, 12.0, 12.1, 14.2, 14.0
                         , 10.3, 10.2, 12.2, 12.3, 14.1, 14.3 ]
  case gaugeRRCrossed ops parts ys of
    Left err -> putStrLn ("error: " ++ show err)
    Right r  -> do
      print (grrPctGRR r, grrPctPart r, grrNumDistinct r)
      print (grrRepeatVar r, grrReproducVar r, grrPartVar r, grrTotalVar r)
      print (grrPctRepeat r, grrPctReproduc r)
```

Output:

```
(0.4678860059549129,99.53211399404508,20.565093992226018)
(1.0000000000000049e-2,8.333333333333285e-3,3.8999999999999986,3.918333333333332)
(0.255210548702681,0.21267545725223191)
```

Reading it:

- **%GRR = 0.47%** — the share of total variance attributable to the
  measurement system. Part-to-part differences (10 / 12 / 14) dwarf the
  measurement wobble (±0.1), so the measurement system passes
- **%Part = 99.5%** — the signal we care about dominates
- **ndc = 20.6** — number of distinct categories, `1.41 · (σ_part / σ_GRR)`,
  measures how many distinct levels the measurement system can resolve
  between parts. **AIAG requires ≥ 5**, and this clears that bar with room
  to spare
- The breakdown is repeatability 0.26% vs. reproducibility 0.21% — both
  negligible

## 2. `GaugeRRResult`

| Field | Meaning |
|---|---|
| `grrPartVar` | σ²_part |
| `grrRepeatVar` | σ²_repeatability |
| `grrReproducVar` | σ²_reproducibility (between operators) |
| `grrTotalVar` | sum of the three above |
| `grrPctRepeat` / `grrPctReproduc` | each component ÷ total × 100 |
| `grrPctGRR` | (repeat + reproduc) ÷ total × 100 |
| `grrPctPart` | part ÷ total × 100 |
| `grrNumDistinct` | ndc = `1.41 · (σ_part / σ_GRR)` |

> **`grrPct*` are variance-ratio percentages (σ²)**. The %Study Variation
> commonly seen on AIAG report forms is a standard-deviation-ratio
> percentage (σ), so **the values won't match**. If you need the σ ratio,
> compute it yourself:
>
> ```haskell
> sqrt ((grrRepeatVar r + grrReproducVar r) / grrTotalVar r) * 100
> -- for the data above: 6.84021933825892 (σ ratio 6.84% vs. the variance ratio's 0.47%)
> ```

Rule-of-thumb thresholds (note that these are conventionally stated in terms
of the σ ratio, not the variance ratio):

| %GRR (σ ratio) | Verdict |
|---|---|
| < 10% | Acceptable |
| 10 – 30% | Acceptable depending on the application |
| > 30% | Unacceptable — the measurement system needs improvement |

## 3. Crossed vs. Nested

```haskell
  -- e.g. destructive testing, where each operator can only measure a different set of parts
  case gaugeRRNested ops parts ys of ...
```

In a nested layout, "part" is nested within operator, so operator variation
and part variation can't be separated. **Use crossed unless measurement
destroys the part.**

`Left` is returned when: the three vectors have mismatched lengths, there
are no repeats to estimate variance from, or there aren't enough levels.

## 4. Which to use

| Goal | Use |
|---|---|
| Decompose measurement variation and pass/fail the system | `gaugeRRCrossed` (this doc) |
| Only a nested layout is possible (destructive testing) | `gaugeRRNested` (this doc) |
| Check whether a process is in control over time | [SPC control charts](../stat/usage-spc.md) |
| General ANOVA to test whether a factor matters | [`Design.Anova`](01-doe.md) |
| Process capability indices (Cp / Cpk) | [`Design.Quality`](01-doe.md) |

## See also

- Package: [hanalyze-design](../../hanalyze-design/README.md)
- SPC control charts: [stat/usage-spc.md](../stat/usage-spc.md)
- DoE entry point: [01-doe.md](01-doe.md)
