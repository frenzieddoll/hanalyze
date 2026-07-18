# Reading messy data

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
import Hanalyze.DataIO.CSV (loadAutoSafeWith, defaultLoadOpts, LoadOpts (..))
import qualified Hanalyze.DataIO.Log as Log

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
| `08_thousands_currency.csv` | `$1,234.56` etc. | W008 | `parseCurrency` (below) |
| `09_quotes_commas.csv` | RFC 4180 quote escape | (none) | handled correctly |
| `10_bom.csv` | UTF-8 BOM | (none) | stripped automatically |
| `11_semicolon_eu.csv` | `;`-separated EU | W005 | sniff auto-fixed |
| `12_real.tsv` | TSV (correct extension) | (none) | — |
| `13_crlf.csv` | tab + `.csv` extension + CRLF | W005 | fix extension or rely on sniff |
| `14_wrong_ext.csv` | same | W005 | same |
| `15_trailing_blank.csv` | trailing blank line | (none) | automatic |
| `16_dates_units.csv` | `5kg` / `11.5cm` | W007 | `stripUnits` (below) |
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
import Hanalyze.DataIO.CSV    (loadAutoSafe)
import Hanalyze.DataIO.Log    (entries, hasWarnings, lgCode)
import qualified Hanalyze.DataIO.Log as Log

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

## Auto-inference (`Hanalyze.DataIO.Sniff`)

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

## Wide → Long reshape (`hanalyze melt`)

Reshape "one row per condition, columns are levels (time / position / index),
sparse cells" wide CSV into **long-form (tidy)**. Column names become the values of the new
variable column; NA cells are dropped per row.

```bash
hanalyze melt data/io/wide_sample.csv \
    --id name,x1,x2 \
    --vars 1,2,3,4,5,6,7,8,9,10 \
    --var t --value y \
    --output data/io/melted_sample.csv
# → 27 rows × 5 columns (name, x1, x2, t, y); NA dropped automatically
```

Library API: `Hanalyze.DataIO.Preprocess.meltLonger`.

> For multivariate regression on the long-form output, and for the wide-form-direct
> true multi-output regression (`hanalyze multireg`), see
> [regression/07-multireg.md](../regression/07-multireg.md).

## Cleaning DSL (`Hanalyze.DataIO.Clean`)

Currency / thousands separators / units / decimal-point variants — columns that the
health checks could only flag as warnings — can be explicitly normalised by applying rules
to selected columns.

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
import qualified Hanalyze.DataIO.Clean as Clean
import Hanalyze.DataIO.Clean (ColumnRule (..), cleanPipeline)

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
