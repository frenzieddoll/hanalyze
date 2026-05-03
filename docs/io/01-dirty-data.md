# Reading messy data (Phase A)

> 🌐 **English** | [日本語](01-dirty-data.ja.md)

Real-world CSV files almost always have something broken. `hanalyze` makes the breakage
explicit through **warning codes** and lets you fix things selectively via **`LoadOpts`**.
This document walks through the common breakage patterns and their fixes per fixture.

## Picking a load API

| API | Returns | Use |
|---|---|---|
| `loadAuto` | `IO (Either ParseError DXD.DataFrame)` | Cleanest data only; simplest. No health checks. |
| `loadAutoSafe` | `IO (Either ParseError (Loaded DXD.DataFrame))` | Defensive open: every exception becomes `Left`; `inspectWithPreview` runs after load. |
| `loadAutoSafeWith opts` | same | `LoadOpts` lets you specify skip / comment / no-header / strict. |

`Loaded a = (a, LogReport)` returns the value paired with logs.

```haskell
import DataIO.CSV (loadAutoSafeWith, defaultLoadOpts, LoadOpts (..))
import qualified DataIO.Log as Log

Right (df, lg) <- loadAutoSafeWith
                    (defaultLoadOpts { loSkip = 3 })
                    "noisy.csv"
Log.printLogReport lg
-- df can be passed straight to Model.* / Viz.*
```

## Warning codes

| Code | Meaning | Source |
|---|---|---|
| W001 | Column names are all numeric → header line likely missing | `inspectDataFrame` |
| W002 | Lines starting with `#` / `!` near the top | `inspectWithPreview` |
| W003 | Number of non-null cells per column varies (= ragged) | `inspectDataFrame` |
| W004 | Duplicate / empty / leading-trailing-whitespace column names; column-count mismatch | `inspectDataFrame` + `inspectWithPreview` |
| W005 | Single-column DataFrame + raw bytes contain frequent `;` / `\t` / `|` = delimiter mismatch | `inspectWithPreview` |
| W006 | Multiple NA encodings (NA / null / n/a / - / empty …) mixed in one column | `inspectDataFrame` |
| W007 | Text column where >50 % of cells are "number + unit" (`12.3kg`) | `inspectDataFrame` |
| W008 | Text column where >50 % of cells contain currency / thousands separator (`$1,234.56`) | `inspectDataFrame` |

I010 / I011 / I012 are Info codes documenting `LoadOpts` preprocessing (skip / comment / no-header).

## 19 fixtures and recommended fixes

Under `data/dirty/` are the 19 fixtures listed below. `cabal run dirty-data-demo` reads
them all and lists the codes raised.

| Fixture | Symptom | Expected W codes | Recommended fix |
|---|---|---|---|
| `01_clean.csv` | healthy | (none) | — |
| `02_no_header.csv` | no header | W001 | `--no-header` |
| `03_preamble.csv` | 3 comment lines | W002 | `--skip 3` or `--comment '#'` |
| `04_ragged.csv` | varying column counts | W003 (rows ≥ 6) | reformat the CSV |
| `05_dup_header.csv` | duplicate column names `x,y,x` | W004 ×2 | reformat the CSV |
| `06_blank_unnamed.csv` | empty column names `x,,y,` | W004 ×4 | reformat the CSV |
| `07_mixed_na.csv` | NA / null / n/a / - / empty | W003 + W006 | normalise via `imputeMean` etc. |
| `08_thousands_currency.csv` | `$1,234.56` etc. | W008 | Phase C `parseCurrency` |
| `09_quotes_commas.csv` | RFC 4180 quote escape | (none) | handled correctly |
| `10_bom.csv` | UTF-8 BOM | (none) | stripped automatically |
| `11_semicolon_eu.csv` | `;`-separated EU | W005 | (Phase B sniff) |
| `12_real.tsv` | TSV (correct extension) | (none) | — |
| `13_crlf.csv` | tab + `.csv` extension + CRLF | W005 | fix extension or use Phase B |
| `14_wrong_ext.csv` | same | W005 | same |
| `15_trailing_blank.csv` | trailing blank line | (none) | automatic |
| `16_dates_units.csv` | `5kg` / `11.5cm` | W007 | Phase C `stripUnits` |
| `17_empty.csv` | completely empty | Left | — |
| `18_header_only.csv` | header only | Left | — |
| `19_whitespace.csv` | leading / trailing whitespace | (none) | auto-trimmed |

## Fixes from the CLI

All subcommands (`info` / `regress` / `hist` / `taguchi analyze` / `ridge` / `kernel` /
`spline` / `quantile` / `gam` / `rf` / `clean` / `melt` / `multireg`) accept the same flags.

```bash
# No header → generate col0/col1, then regress
hanalyze regress --no-header data/dirty/02_no_header.csv col0 col1 LM

# Skip 3 comment lines
hanalyze regress --skip 3 data/dirty/03_preamble.csv x y LM

# strict: stop on warnings (CI use)
hanalyze regress --strict data/raw.csv x y LM
```

## Library usage (working with the LogReport)

```haskell
import DataIO.CSV    (loadAutoSafe)
import DataIO.Log    (entries, hasWarnings, lgCode)
import qualified DataIO.Log as Log

main :: IO ()
main = do
  Right (df, lg) <- loadAutoSafe "raw.csv"
  Log.printLogReport lg
  -- Stop on warnings if desired
  when (hasWarnings lg && wantStrict) $
    error ("aborted: " ++ show (map lgCode (entries lg)))
  -- Otherwise carry on
  ...
```

## Phase B: auto-inference (`DataIO.Sniff`, complete)

Reads the first 8 KB to fill in items the user did not specify.
- Delimiter (chosen from `,;\t|` by ascending variance + descending median).
- Comment character (consecutive lines at the top starting with `#` / `!`).
- Header presence (no header if the first line consists entirely of numeric tokens).

`LoadOpts.loSniff` defaults to `True`; `--no-sniff` disables it.
The result is logged as the `I013` Info code, so what was auto-fixed is always traceable.
This makes `data/dirty/{02,03,11,13,14}` readable without any flags (5/19 → 14/19 default
clean reads).

```bash
# All readable with no extra arguments (sniff auto-inferred)
hanalyze info data/dirty/02_no_header.csv
hanalyze info data/dirty/03_preamble.csv
hanalyze info data/dirty/11_semicolon_eu.csv
```

## Wide-form data → long (melt / pivot_longer)

Wide CSV with "one row per condition, columns are levels (time / position / index),
sparse cells" — e.g. `data/io/wide_sample.csv`:

```
name,x1,x2,1,2,3,4,5,6,7,8,9,10
a,1,0,1,,3,,5,,7,,9,
b,2,0,,4,,8,10,,14,,,20
c,3,0,0.1,0.2,,0.4,,0.6,,0.8,,1
d,4,0,,1,1.5,2,2.5,,,4,,5
e,5,0,3,,9,12,,18,,,,30
```

is hard to regress against directly: column-by-column y means losing across-t continuity
and dealing with sparsity. `DataIO.Preprocess.meltLonger` rearranges into long-form (tidy),
turning the column names (1–10) into a new predictor t, and dropping NA cells naturally as
"unobserved samples".

### CLI

```bash
# wide → long
hanalyze melt data/io/wide_sample.csv \
    --id name,x1,x2 \
    --vars 1,2,3,4,5,6,7,8,9,10 \
    --var t --value y \
    --output data/io/melted_sample.csv
# → 27 rows × 5 columns (name, x1, x2, t, y); NA dropped automatically

# Multivariate RFF Ridge captures column-wise non-linearity
hanalyze kernel data/io/melted_sample.csv "x1 t" y \
    --method rff --features 200 --bandwidth 1.0 --lambda 0.001 \
    --group name --xaxis t \
    --out trash/rff_mv_plot.html \
    --report trash/rff_mv_report.html
# → R² = 1.0000; interactive scatter (x = t, y = y, color = name) + curves,
#   plus a ReportBuilder integrated HTML report.
```

### Library

```haskell
import qualified DataIO.Preprocess as Pp
import qualified Model.RFF         as RFF
import qualified Viz.ReportBuilder as RB
import qualified Viz.ReportInstances as RI

main = do
  Right (df0, _) <- CSV.loadAutoSafe "data/io/wide_sample.csv"
  let df = Pp.meltLonger ["name", "x1", "x2"]
                         (map (T.pack . show) [1..10 :: Int])
                         "t" "y" True df0
  -- ... fit RFF ridge multivariate ...
  let rep = RI.RFFMVReport fit "name" "t"
      cfg = RB.defaultReportConfig "Multivariate RFF Ridge"
  RB.renderReport "out.html" cfg
    (RB.toReport cfg df ["x1", "t"] "y" rep)
```

### Interactive prediction (`--interactive`)

Adding `--interactive` together with `--report` makes the secondary-axis sliders
(everything except the column passed to `--xaxis`) recompute the prediction curve in the
browser by re-evaluating the RFF features. Weights and frequencies are embedded as JSON
and JavaScript evaluates `φ_j(x_new) = σ_f √(2/D) cos(ω_jᵀx_new + b_j)`.

### Standardisation (`--standardize`) and HP auto-tuning (`--auto-hp`)

Features with very different scales (energy 30–200 keV, dose 1e13–2e15 cm⁻², z 0–200 nm)
will not fit accurately under a single shared length scale ℓ. Two flags address this:

| Flag | Behaviour |
|---|---|
| `--standardize` | z-score X before fitting (`Stat.Standardize`). Predict / plot / interactive JS receive raw values, lift them into standardised space, then evaluate the RFF. |
| `--auto-hp`     | Auto-tune `(ℓ, σ_f, σ_n)` via marginal-likelihood maximisation in `Model.RFF.maximizeMarginalLikRBFMV`. `--bandwidth` / `--lambda` are ignored (σ_n² corresponds to λ). |

Marginal-likelihood maximisation:

1. Build K_ij = σ_f² · exp(-‖x_i - x_j‖² / (2ℓ²)) exactly.
2. Compute log p(y|θ) = -½ yᵀ(K+σ_n²I)⁻¹y - ½ log|K+σ_n²I| - n/2 log(2π) via Cholesky.
3. Evaluate (log ℓ, log σ_f, log σ_n) on a log-spaced 20×8×8 grid.
4. Refine with one coarse-to-fine pass at 1/3-width around the best grid point.
5. With ℓ fixed, sample the RFF (D ω vectors) and run a Ridge fit.

The code is straightforward but grid-based, with a local-optimum risk. For small n (n ≤ 200)
all 2560 points evaluate in seconds.

### Example: a semiconductor potential profile

8 conditions (energy/dose) × 30 z points = 240-row dopant profile
(`data/io/potential_long.csv`). Simple physics (B in Si):
`Rp(E) = 1.5·E^0.7`, `σ(E) = 0.4·Rp`, `N_peak = D / (√(2π)·σ)`.

```bash
# Generator is git-ignored (verification only). Re-generation requires temporarily adding a cabal exe.

# Fix horizontal axis to z; energy / dose sliders move the curve; auto-HP + standardisation
hanalyze kernel data/io/potential_long.csv "energy dose z" y \
    --method rff --features 400 \
    --standardize --auto-hp \
    --group name --xaxis z \
    --out trash/potential_plot.html \
    --report trash/potential_report.html \
    --interactive
```

Sample stdout:
```
  Standardize: ON
    μ = [105, 8.3e14, 63.7]
    σ = [60, 7.7e14, 47]
  Auto-HP: maximising marginal likelihood ...
    ℓ = 0.41   (standardised space)
    σ_f = 1.0e13
    σ_n = 6.1e11   (λ = σ_n² = 3.7e23)
    log_mlik = -7085   (2560 points)
RFF (multivariate) Ridge fit:
  R^2 = 0.9947
  RMSE = 8.3e11
```

In the browser the energy / dose sliders show **raw units**; on each input JS does
`(v-μ)/σ` to lift to standardised space and re-renders the prediction.

### Future: ARD-RFF

Even after `--standardize` aligns scales, distinguishing "physically important variables"
from "essentially irrelevant" ones benefits from **per-feature length scales ℓ_k**
(Automatic Relevance Determination). The implementation only requires extending
`sampleRFFRBFMV` to take `[Double]` (one ℓ per dimension); the CV / marginal-likelihood
grid grows by that many dimensions. Standardisation alone covers many cases, so ARD is
not yet implemented.

After melting the data is just a regular DataFrame, so in principle most models accept it,
though multi-predictor support varies by model:

| Multivariate OK | 1-variable only |
|---|---|
| LM / GLM / GLMM | GP (`Model.GP`) |
| Ridge / Lasso / ElasticNet | Spline (`Model.Spline`) |
| HBM | Kernel NW / KR (1D version of `Model.Kernel`) |
| GAM (each feature with its own spline) | |
| Random Forest | |
| Quantile | |
| **RFF (`rffRidgeMV`, `hanalyze kernel ... --method rff`)** | |

Multivariate extensions of GP / Spline / Kernel NW · KR are future work.

## Phase C: cleaning DSL (`DataIO.Clean`, complete)

Currency / thousands separators / units / decimal-point variants, etc., that Phase A
flagged as warnings can be explicitly normalised by applying rules to selected columns.

### `ColumnRule`

| Rule | Example | Result |
|---|---|---|
| `StripUnits`     | `"12.3kg"`    | `12.3`     |
| `ParseCurrency`  | `"$1,234.56"` | `1234.56`  |
| `ParseDecimalEU` | `"3,14"`      | `3.14`     |
| `TrimText`       | `"  abc  "`   | `"abc"`    |
| `CoerceNumeric`  | any of the above | first successful conversion wins |

Each rule emits `I100`–`I104` Info codes, plus an additional `I*L` warning when the success
rate falls below 50 % (suggesting an alternative rule).

### Library

```haskell
import qualified DataIO.Clean as Clean
import           DataIO.Clean (ColumnRule (..), cleanPipeline)

(df', lg) = cleanPipeline
  [ ("price",  ParseCurrency)
  , ("weight", StripUnits)
  , ("price2", CoerceNumeric)  -- catch-all
  ] df
Log.printLogReport lg
```

### CLI (`hanalyze clean`)

```bash
# Strip units
hanalyze clean data/dirty/16_dates_units.csv \
    --rule weight=StripUnits \
    --rule length_cm=StripUnits

# Currency + thousands
hanalyze clean data/dirty/08_thousands_currency.csv \
    --rule price=ParseCurrency

# Catch-all (first matching rule wins)
hanalyze clean data/dirty/08_thousands_currency.csv \
    --rule price=CoerceNumeric
```

Like every other CLI, `hanalyze clean` accepts `--no-header` / `--skip N` /
`--comment CH` / `--delim CH` / `--strict` / `--no-sniff`.
