# Design of Experiments (DOE)

> 🌐 **English** | [日本語](09-doe.ja.md)

> [📚 Index](README.md) | [01 quickstart](01-quickstart.md) | [02 regression](02-regression.md) | [03 bayesian-hbm](03-bayesian-hbm.md) | [04 multivariate](04-multivariate.md) | [05 ml](05-ml.md) | [06 timeseries](06-timeseries.md) | [07 survival](07-survival.md) | [08 causal](08-causal.md) | **09 doe** | [10 stat](10-stat.md) | [11 data](11-data.md) | [12 plot](12-plot.md)

`Hanalyze.Design.*` provides experimental design. Returns design matrices, orthogonal arrays, ANOVA tables, power (mostly numerical results). Theory references: [01-doe](../doe/01-doe.md) and [02-orthogonal-taguchi](../doe/02-orthogonal-taguchi.md).

## API Overview (Organized by Return Type)

High-level DOE API classified by **result type**. Functions with the same result type are minimal (1-2); these are grouped under **Others**.

### Create Factors (→ `DesignFactor`)

| Function | Type | Role |
|---|---|---|
| `contFactor` | `Text -> (Double,Double) -> DesignFactor` | Continuous factor (name + real bounds/lower-upper; 2 levels) |
| `numFactor` | `Text -> [Double] -> DesignFactor` | Numeric ordered factor (ordered real levels; ≥3 levels; formula uses `opoly`) |
| `catFactor` | `Text -> [Text] -> DesignFactor` | Categorical factor (name + level names) |

Factors are pure; which factors belong to which layer (whole-plot / block) is specified not by the factor itself but by the **`CustomSpec` `Structure`** by name (→ later "Hierarchical Design").

### Create Plans (→ `Design`)

| Function | Type | Role |
|---|---|---|
| `factorialDesign` | `[DesignFactor] -> Design` | Full factorial (continuous 2-level × categorical m-level all combinations; full interaction model `y ~ x1 * x2`) |
| `centralCompositeDesign` | `[DesignFactor] -> Design` | Rotatable CCD (quadratic model `y ~ …+x1:x2+I(x1^2)+I(x2^2)`; continuous only) |
| `boxBehnkenDesign` | `[DesignFactor] -> Design` | Box-Behnken RSM (**k=3,4,5**; no ±α points; quadratic model; continuous only) |
| `fractionalDesign` | `[DesignFactor] -> Resolution -> Design` | Fractional factorial (auto resolution; minimum aberration; **k=3~11,15**; main effects model; binary categories only) |
| `fractionalDesignGen` | `[DesignFactor] -> [[Int]] -> Design` | Fractional factorial (explicit generator; e.g. `[[1,2,3]]`=D=ABC; binary categories only) |
| `fractionalDesignInter` | `[DesignFactor] -> Resolution -> Design` | Fractional factorial (with interactions; auto resolution; main + main-unconfounded 2FI representative) |
| `fractionalDesignGenInter` | `[DesignFactor] -> [[Int]] -> Design` | Fractional factorial (with interactions; explicit generator) |
| `taguchiDesign` | `[DesignFactor] -> Design` | Taguchi orthogonal array (auto minimal OA; 2-level L4/L8/L12/L16 + 3-level L9/L27 + mixed L18; main effects model; continuous/numeric/categorical) |
| `taguchiDesignOA` | `OATable -> [DesignFactor] -> Design` | Taguchi orthogonal array (explicit via enum `OATable` = `L4`/`L8`/`L9`/`L12`/`L16`/`L18`/`L27`; typos caught at compile time) |
| `optimalDesign` | `[DesignFactor] -> Formula -> Int -> Design` | D-optimal plan (model + run count `n`; seed 42; auto candidate levels; categorical ok) |
| `optimalDesignLevels` | `Int -> [DesignFactor] -> Formula -> Int -> Design` | Optimal plan (explicit candidate grid levels; otherwise same as `optimalDesign`) |
| `optimalDesignWith` | `OptCriterion -> Maybe Int -> Int -> [DesignFactor] -> Formula -> Int -> Design` | Optimal plan (full control: criterion / levels / seed) |
| `customDesign` | `CustomSpec -> Design` | **Full custom design** (coordinate exchange; **pure/seed deterministic**; constraints + criterion + hierarchical `Structure` in one `CustomSpec`; sole generation entry point) |

### Create Fitting Specs (→ `…Spec`, fit with `filledDf \|-> …`)

Models are **3 families: LM (`designModel`) / GP (`designModelGP`) / HBM (`designModelHBM`)**.

| Function | Type | Role |
|---|---|---|
| `designModel` | `Design -> Text -> DesignModelSpec` | LM (general linear). `filledDf \|-> designModel plan "y"` → `MultiLMModel` |
| `designModelGP` | `GPConfig -> Design -> Text -> DesignModelGPSpec` | GP (nonlinear nonparametric; continuous factors only). → `GPRegModelN` |
| `designModelHBM` | `HBMConfig -> Design -> [RandomSpec] -> Text -> DesignModelHBMSpec` | Hierarchical Bayes (mixed-effects; group as RE). → `DesignHBMFit` |
| `multiOutput` | `[Text] -> (Text -> spec) -> MultiOutputSpec spec` | Multiple responses batch fit → `[(response name, Fitted spec)]` |

### Visualization (→ `ProfilerSpec` / `VisualSpec`)

| Function | Type | Role |
|---|---|---|
| `profiler` | `[(Text,m)] -> [Text] -> ProfilerSpec m` | Profiler grid (rows=responses × columns=factors) |
| `profilerResidual` | `ResidualMode -> ProfilerSpec m` | Point mode combination (`Raw` observed / `Partial` partial residual) |
| `contourOf` | `MultiVarModel m => m -> Text -> Text -> VisualSpec` | 2-factor RSM contour / response surface (plane) |
| `surfaceOf` | `MultiVarModel m => m -> Text -> Text -> VisualSpec3D` | 2-factor 3D response surface (SVG via `saveSVG3D` / WebGL via `saveHTML3D`) |

### Model Terms (Effects DSL; → `Formula`)

Used in optimal design (`optimalDesign` family) model specification.

| Function | Type | Role |
|---|---|---|
| `mainEffects` | `[Text] -> Formula` | Main effects `y ~ x1 + x2 + …` |
| `twoWay` | `[Text] -> Formula` | Main effects + all 2-factor interactions |
| `quadratic` | `[Text] -> Formula` | Main effects + interactions + quadratic terms (RSM equivalent) |

### Others (Single-Use Return Types)

| Function | Type | Role |
|---|---|---|
| `designTable` | `Design -> [(Text,[Double])]` | Runsheet contents (uncoded real values, run numbers, `ColumnSource`, continuous/numeric only; error if categorical) |
| `designFrame` | `Design -> DataFrame` | Runsheet as formatted table (continuous/numeric=Double column / categorical=Text column; `print` yields ASCII table) |
| `designFrameRound` | `Int -> Design -> DataFrame` | `designFrame` with precision (continuous/numeric values rounded to `n` decimals; CCD axis points etc. readability) |
| `saveDesign` | `FilePath -> Design -> IO ()` | Save runsheet to CSV (pass to experimenter) |
| `planFromFrame` | `[DesignFactor] -> Formula -> DataFrame -> Design` | Recover `Design` from DataFrame (factors + formula explicit; CSV reload) |
| `customSpec` | `[DesignFactor] -> Formula -> Int -> Int -> CustomSpec` | `CustomSpec` smart ctor (factors/formula/run count/seed; default = DOpt, no constraints, CRD). Extend via record update for `csCriterion`/`csConstraints`/`csStructure` |
| `splitPlot` | `[Text] -> Int -> Structure` | Split-plot structure (whole-plot factor names + whole-plot count; η=1.0/group name `"wholePlot"` default) |
| `stripPlot` | `[Text] -> Int -> [Text] -> Int -> Structure` | Strip-plot structure (whole-plot × strip cross 2-level; group names `"wholePlot"`/`"strip"` default) |
| `blocked` | `Int -> Structure` | Randomized block structure (block count; η=1.0/group name `"block"` default) |
| `formulaToCustomModel` | `[Text] -> Formula -> Either Text Model` | Convert effects DSL / `Formula` to Custom Design layer `Model` (`[ModelTerm]`) (`customDesign` uses internally) |
| `modelFor` | `Text -> [(Text,m)] -> m` | Extract one model from `multiOutput` result by response name (`contourOf`/`surfaceOf` input) |
| `designFactorNames` | `Design -> [Text]` | Factor name list |
| `designFormula` | `Design -> Text -> Text` | Implied model formula string |
| `aliasStructure` | `Design -> [(Text,[Text])]` | Alias structure (effect → confounding effect labels; non-empty only for `fractionalDesignInter` family) |
| `ranIntercept` | `Text -> RandomSpec` | HBM random intercept `(1\|g)` (→ `designModelHBM`) |
| `ranSlope` | `[Text] -> Text -> RandomSpec` | HBM correlated random slope `(1+s\|g)` (→ `designModelHBM`) |
| `rsmAnalysis` | `Design -> [Double] -> RSMReport` | RSM stationary point, property, canonical, R² (natural units) |
| `steepestAscentNatural` | `Bool -> Design -> [Double] -> Double -> Int -> [[(Text,Double)]]` | Steepest ascent path (natural units; direction via coded gradient) |

## Workflow

DOE is **① Design plan → ② Run experiment (sim / prototype) → ③ Fit model to data, assess accuracy** (iterative). The library provides ① design runsheet, ② reusable fitting, ③ visualization (profiler); sim/prototype execution and loop decisions are user-side (IHaskell interactive).

```haskell
import Hanalyze.Plot   -- contFactor/catFactor/factorialDesign/centralCompositeDesign/designTable/designModel/designModelGP/designModelHBM/ranIntercept/multiOutput/profiler etc.

-- ① Design plan (pure, df not needed). Create factors with contFactor (continuous) / catFactor (categorical).
--    Design implies model formula.
let plan = centralCompositeDesign [contFactor "temp" (150,180), contFactor "time" (10,20)]   -- Response surface (quadratic model)
    runsheet = designTable plan   -- Runsheet for execution (uncoded real values, run numbers). ColumnSource

-- Confirm design with formatted table (print yields type-tagged ASCII).
print (designFrame plan)

-- ② Execute runsheet and collect responses (sim / prototype; external). Return filled df.

-- ③ Fit LM to data. Multiple responses (strength/yield…) via multiOutput for batch fitting.
--    Same plan reused across responses, sim/real.
let model = filledDf |-> multiOutput ["strength","yield"] (designModel plan)
--  model :: [(Text, MultiLMModel)]  -- (response name, model). Single response = ["y"] with 1 element.

-- Visualization: response × each factor (prediction line + 95% CI + points) = JMP Prediction Profiler equivalent.
-- toPlot renders, <> composes options (points computed from model observations; bundle with noDf).
noDf |>> toPlot (profiler model ["temp","time"])
```

![DOE prediction profiler (rows=responses × columns=factors + CI + observed)](../images/doe-profiler.svg)

The 3 steps are explained in chapters below — **① Create plan** (design type), **② Fit model** (LM / GP / HBM), **③ Visualize** (profiler / response surface).

## Create Plan

DOE starts with "which factors at which levels" = **design (`Design`)**. Create factors with `contFactor` / `numFactor` / `catFactor`, pass to design constructor matching your goal → `Design` result. Design **implies model formula** (full factorial = interactions, RSM = quadratic, etc.); same `Design` reusable across sim/real and multiple responses. Below, design types ordered **by textbook complexity** (full factorial → fractional → screening → response surface → optimal).

### Factors (Factors and Levels)

3 factor types —

- **Continuous** `contFactor "temp" (150,180)` (lower/upper bounds; coded ±1 as 2 levels)
  - **Log-scale continuous** `contFactorLog "conc" (0.01,10)` (bounds are **positive**; coded↔real mapping via geometric `10^…`). For factors with wide ranges (concentration, time etc.), center point and levels are **geometrically spaced**; model is linear in `log(x)`. Default is linear scale (`contFactor`); log is opt-in.
- **Numeric ordered** `numFactor "temp" [150,165,180]` (ordered real level list; ≥3 levels). Formula uses orthogonal polynomials `opoly`, decomposing observed intervals into linear+quadratic (for Taguchi 3-level arrays).
- **Categorical** `catFactor "cat" ["A","B","C"]` (unordered level names). Becomes Text column in DataFrame; fitting uses treatment contrast (R character→factor equivalence).

Design constructors accept factor types as follows —

- **`factorialDesign`** … continuous (2-level) and categorical (m-level) all combinations.
- **`optimalDesign`** … continuous and categorical (candidate grid with contrast expansion in design matrix).
- **`fractionalDesign` / `fractionalDesignGen`** … continuous and **2-level (binary) categorical only** (≥3 levels error).
- **`centralCompositeDesign` / `boxBehnkenDesign`** … continuous only (±α points / 3-level numeric; categorical error).
- **`taguchiDesign` / `taguchiDesignOA`** … continuous (2-level), numeric ordered / categorical (2/3 levels) assigned to orthogonal table columns.

### Confirm Design (Universal)

Any design type uses the same workflow to confirm `Design` contents (following sections assume this). `designTable plan :: [(Text,[Double])]` is runsheet **contents** (column-oriented; `ColumnSource`). `designFrame plan :: DataFrame` (= `toFrame . designTable`) yields **formatted table** via `print`. `designFactorNames plan :: [Text]` gets factor names, `designFormula plan "y" :: Text` retrieves implied formula.

`print (designFrame (factorialDesign [contFactor "temp" (150,180), contFactor "time" (10,20)]))`:

```
------------------------
 run   |  temp  |  time
-------|--------|-------
Double | Double | Double
-------|--------|-------
1.0    | 150.0  | 10.0
2.0    | 150.0  | 20.0
3.0    | 180.0  | 10.0
4.0    | 180.0  | 20.0
```

Designs with categorical factors have string columns in runsheet — use **`designFrame`** to confirm and fit (numeric-only `designTable` errors if categorical present; continuous/numeric factors only: `designTable` ok).

RSM (CCD) axis points (±α) and other **irrational long decimals** make runsheet hard to read. **`designFrameRound n plan`** yields runsheet with continuous/numeric real values **rounded to `n` decimals** (`designFrame` same rows/columns; run numbers/categorical/groups unchanged). Rounded values become runsheet values (experiment uses rounded levels), fit directly like `designFrame`.

`print (designFrameRound 2 (centralCompositeDesign [contFactor "temp" (150,180), contFactor "time" (10,20)]))`:

```
------------------------
 run   |  temp  |  time
-------|--------|-------
Double | Double | Double
-------|--------|-------
1.0    | 150.0  | 10.0
2.0    | 150.0  | 20.0
3.0    | 180.0  | 10.0
4.0    | 180.0  | 20.0
5.0    | 143.79 | 15.0
6.0    | 186.21 | 15.0
7.0    | 165.0  | 7.93
8.0    | 165.0  | 22.07
9.0    | 165.0  | 15.0
10.0   | 165.0  | 15.0
```

### Save and Restore Design / Reload from DataFrame

DOE cycles: "①Design plan → ②External experiment → ③Fit to data" so design must be **saved for experimenter** and returned data **reloaded to plan**. Two paths:

- **`saveDesign path plan`** … write runsheet (`designFrame`) to **CSV**. Experimenter executes each run, fills response column, returns.
- **`planFromFrame factors formula df`** … recover `Design` from loaded DataFrame (runsheet + responses). Specify factors (bounds/levels) and model formula (effects DSL `mainEffects` / `quadratic` etc.), encode factor columns to coded, wrap as `KCustom` design.

```haskell
-- ① Save design (runsheet for experimenter).
saveDesign "runsheet.csv" plan

-- ② Experimenter fills responses and returns → reload CSV (loadCSV is DataIO.CSV; → 11-data).
filledDf <- loadCSV "runsheet-filled.csv"

-- ③ Recover plan from DataFrame and fit / RSM analysis.
let plan2 = planFromFrame [contFactor "temp" (150,180), contFactor "time" (10,20)]
                          (quadratic ["temp","time"]) filledDf
filledDf |-> designModel plan2 "y"
```

`designModel` fit works with **formula + df only**, but `rsmAnalysis` / `steepestAscentNatural` use coded geometry, so **factor bounds/levels must match original design** (else stationary point / direction shifts). Factor columns missing or type mismatch → error.

### Full Factorial Design

**`factorialDesign`** runs all factor level combinations (most basic). The `designFrame` example (2 factors continuous) shows **4 corners in 4 runs**; implied model is all interactions `y ~ temp * time`. Categorical factors mix in —

```haskell
-- Continuous (temp) × categorical 3-level (catalyst) full factorial = 2×3 = 6 runs.
let plan = factorialDesign [ contFactor "temp"     (150,180)
                           , catFactor  "catalyst" ["A","B","C"] ]
print (designFrame plan)                 -- Below formatted table; 6 runs
filledDf |-> designModel plan "y"        -- y ~ temp * catalyst (catalyst contrast-expanded)
```

```
--------------------------
 run   |  temp  | catalyst
-------|--------|---------
Double | Double |   Text
-------|--------|---------
1.0    | 150.0  | A
2.0    | 150.0  | B
3.0    | 150.0  | C
4.0    | 180.0  | A
5.0    | 180.0  | B
6.0    | 180.0  | C
```

Categorical column preserved as Text (contrast-expanded at fit time).

### Fractional Factorial Design

Many factors → full factorial 2^k explodes (7 factors: 128). **Fractional factorial** `fractionalDesign` aliases some interactions to main effects, reducing to **2^(k-p)**. Alias heaviness measured by **resolution (resolution)**, auto-select minimum **minimum aberration** design meeting desired resolution (generators from Montgomery Table 8-14 / NIST standard; k=3~11, 15. 8 runs=k≤7, 16 runs=k≤11,15, 32 runs=k≤11).

```haskell
-- 7 factors at resolution III: full factorial 128 → 8 runs (estimate main effects).
let plan = fractionalDesign [contFactor "a" (0,1), …, contFactor "g" (0,1)] ResIII
print (designFrame plan)         -- Below 8-run runsheet
filledDf |-> designModel plan "y"  -- Formula = main effects only: y ~ a + b + … + g
```

```
---------------------------------------------------------------------
 run   |   a    |   b    |   c    |   d    |   e    |   f    |   g
-------|--------|--------|--------|--------|--------|--------|-------
Double | Double | Double | Double | Double | Double | Double | Double
-------|--------|--------|--------|--------|--------|--------|-------
1.0    | 0.0    | 0.0    | 0.0    | 1.0    | 1.0    | 1.0    | 0.0
2.0    | 0.0    | 0.0    | 1.0    | 1.0    | 0.0    | 0.0    | 1.0
3.0    | 0.0    | 1.0    | 0.0    | 0.0    | 1.0    | 0.0    | 1.0
4.0    | 0.0    | 1.0    | 1.0    | 0.0    | 0.0    | 1.0    | 0.0
5.0    | 1.0    | 0.0    | 0.0    | 0.0    | 0.0    | 1.0    | 1.0
6.0    | 1.0    | 0.0    | 1.0    | 0.0    | 1.0    | 0.0    | 0.0
7.0    | 1.0    | 1.0    | 0.0    | 1.0    | 0.0    | 0.0    | 0.0
8.0    | 1.0    | 1.0    | 1.0    | 1.0    | 1.0    | 1.0    | 1.0
```

- `Res III` … main effects aliased with 2-factor interactions (most aggressive reduction).
- `Res IV`  … main effects not aliased with 2-factor interactions, but 2FI's are.
- `Res V+`  … main and 2-factor interactions nearly independent.

Experts can specify generator: `fractionalDesignGen specs [[1,2,3]]` (= D=ABC, 2^(4-1)). `fractionalDesign` / `fractionalDesignGen` formulas are **main effects only** (interactions aliased, not included).

#### Include Interactions (`fractionalDesignInter` / `aliasStructure`)

To include 2-factor interactions (2FI) unaliased from main effects, use **`fractionalDesignInter`** / **`fractionalDesignGenInter`** (same design points as `fractionalDesign`, formula differs). Compute alias cosets, add representative 2FI unaliased from main effects to main effects.

```haskell
-- Res IV (D=ABC, 8 runs). 4 main + 3 unaliased 2FI reps = full rank.
let plan = fractionalDesignGenInter [contFactor "a" (0,1), …, contFactor "d" (0,1)] [[1,2,3]]
designFormula plan "y"       -- y ~ a + b + c + d + a:b + a:c + a:d
aliasStructure plan          -- [("a:b",["c:d"]), ("a:c",["b:d"]), …] — a:b aliases c:d
```

- Resolution determines which 2FI enter — **Res V+** all 2FI independent, **Res IV** one rep per alias set (estimate = "sum of aliased 2FI"; main effects unbiased), **Res III** only main-unaliased 2FI.
- **`aliasStructure plan :: [(Text,[Text])]`** retrieves what each effect aliases (lookup "a:b" in result). Res IV reps have alias partners; check during interpretation.

### Screening / Orthogonal Arrays (Plackett-Burman, Taguchi)

Many factors but "just which are active" — use **Taguchi orthogonal arrays** `taguchiDesign`. Auto-select **minimum-run standard orthogonal table** matching each factor's **required level count**; assign factors to columns. Like `fractionalDesign`, formula is **main effects only**.

- 2-level tables: L4 (≤3 factors) → L8 (≤7) → L12 (≤11, Plackett-Burman) → L16 (≤15)
- 3-level tables: L9 (≤4 factors, 3⁴) → L27 (≤13, 3¹³)
- Mixed-level: L18 (2¹×3⁷; 1 two-level + 7 three-level factors)

Required level count = factor type — continuous `contFactor` = 2, numeric/categorical = level count.

```haskell
-- ① 11 continuous factors, 12 runs screening (Plackett-Burman L12).
let plan = taguchiDesign [contFactor "a" (0,1), …, contFactor "k" (0,1)]  -- 11 factors → L12 auto
print (designFrame plan)             -- 12-run runsheet
filledDf |-> designModel plan "y"    -- y ~ a + b + … + k (main effects only)

-- ② 3-level factors as numFactor (numeric ordered) / catFactor (categorical). 4 factors → L9 (9 runs).
let planL9 = taguchiDesign [ numFactor "temp" [150,165,180]   -- Numeric ordered 3-level
                           , numFactor "time" [10,20,30]
                           , catFactor "cat"  ["A","B","C"]    -- Categorical 3-level
                           , catFactor "mat"  ["X","Y","Z"] ]
designFormula planL9 "y"   -- y ~ opoly(temp,2) + opoly(time,2) + cat + mat
filledDf |-> designModel planL9 "y"

-- ③ Mixed-level — continuous 1 + 3-level categorical 2 → L18 auto (18 runs).
let planMix = taguchiDesign [ contFactor "p" (0,1)
                            , catFactor  "q" ["a","b","c"]
                            , catFactor  "r" ["x","y","z"] ]
```

`planL9` output (4 factors in 9 runs — 2 numeric + 2 categorical Text columns):

```
--------------------------------------
 run   |  temp  |  time  | cat  | mat
-------|--------|--------|------|-----
Double | Double | Double | Text | Text
-------|--------|--------|------|-----
1.0    | 150.0  | 10.0   | A    | X
2.0    | 150.0  | 20.0   | B    | Y
3.0    | 150.0  | 30.0   | C    | Z
4.0    | 165.0  | 10.0   | B    | Z
5.0    | 165.0  | 20.0   | C    | X
6.0    | 165.0  | 30.0   | A    | Y
7.0    | 180.0  | 10.0   | C    | Y
8.0    | 180.0  | 20.0   | A    | Z
9.0    | 180.0  | 30.0   | B    | X
```

- **★L12 (Plackett-Burman)** is the flagship — 11 factors in 12 runs. Interactions spread thinly across all columns, so **main-effect screening only** (narrow down active factors, then move to refined plan).
- **Numeric ordered factors `numFactor`** pass real level values directly (need not be equally spaced). Formula uses **orthogonal polynomials** `opoly(name, levels−1)` (linear + quadratic …), decomposing **by observed spacing** (raw powers differ; orthogonal decomp ensures linear ⊥ quadratic for independent tests). Curvature (optimum at intermediate level?) estimable.
- **Categorical factors `catFactor`** become Text columns, contrast-expanded at fit (unordered levels). Formula is main effect name.
- L8/L16 mathematically equivalent to fractional factorial but transparent under "assign factors to orthogonal table columns" (Taguchi frame). Fix run count with `taguchiDesignOA L9 specs` (explicit enum `OATable` = escape hatch vs. fractional's `fractionalDesignGen`).
- Designs with categorical → use **`designFrame`** to confirm/fit (numeric `designTable` errors with categorical; continuous/numeric only: `designTable` ok).

### Response Surface Methodology

Optimization (max/min search) needs quadratic model to estimate curvature. **`centralCompositeDesign`** builds rotatable **CCD (central composite design)** — cube (4 corners, full factorial) + axial points (±α, 2k) + center (k). Implied model is quadratic `y ~ …+x1:x2+I(x1^2)+I(x2^2)`.

```haskell
-- 2-factor response surface (CCD). Cube 4 corners + axial (±α) + center = 10 runs for quadratic.
let plan = centralCompositeDesign [contFactor "temp" (150,180), contFactor "time" (10,20)]
print (designFrame plan)                        -- 10-run runsheet (uncoded real values)
let model = filledDf |-> designModel plan "y"   -- model :: MultiLMModel (y ~ temp*time + I(temp^2) + I(time^2))
noDf |>> toPlot (profiler [("y", model)] ["temp","time"])   -- Prediction line + CI profiler
```

`print (designFrame plan)`:

```
------------------------------------------------
 run   |        temp        |        time
-------|--------------------|-------------------
Double |       Double       |       Double
-------|--------------------|-------------------
1.0    | 150.0              | 10.0                ┐
2.0    | 150.0              | 20.0                │ cube (4 corners, full factorial)
3.0    | 180.0              | 10.0                │
4.0    | 180.0              | 20.0                ┘
5.0    | 143.78679656440357 | 15.0                ┐ axial points (temp = 165 ± 15√2)
6.0    | 186.21320343559643 | 15.0                ┘
7.0    | 165.0              | 7.9289321881345245  ┐ axial points (time = 15 ± 5√2)
8.0    | 165.0              | 22.071067811865476  ┘
9.0    | 165.0              | 15.0                ┐ center point ×2
10.0   | 165.0              | 15.0                ┘
```

RSM alternatively uses **Box-Behnken** (`boxBehnkenDesign`, k=3,4,5). Unlike CCD's cube + extreme axial points (±α), all points sit on edge midpoints (factors at −1/0/+1 levels), avoiding extreme combinations. Useful when factor bounds are hard limits. Example (3 factors): 12 edge points + center = 15 runs for quadratic.

#### Stationary Point, Canonical, Steepest Ascent (Natural Units)

Solving response surface yields stationary point, its property, next search direction. **Coded vs. uncoded key point**: fit in coded space or natural units **doesn't affect prediction** (same LM term = reparameterization; prediction/R²/profiler identical). Coding matters for **scale-dependent geometry** — stationary direction, canonical axis, steepest ascent — only. Without coding factor ranges to unit (coded `[-1,1]`), narrow-range factors dominate direction. So library solves geometry in coded space internally, **reports results in natural units**. Users handle runsheet and analysis results consistently in natural units.

`rsmAnalysis plan ys` fits quadratic to response `ys` (run order), returns stationary point (natural units), property (`RMaximum` / `RMinimum` / `RSaddle`), canonical (eigenvalues + coded direction), R² (like R `rsm::canonical`).

```haskell
let rep = rsmAnalysis plan ys
rsmStationary rep   -- [("temp", 168.3), ("time", 14.1)]  Stationary point (natural units)
rsmNature     rep   -- RMaximum / RMinimum / RSaddle
rsmInRegion   rep   -- True if stationary within experimental region (False = extrapolation)
rsmCanonical  rep   -- [(eigenvalue, coded direction)] ascending. Eigenvalue sign shows curvature
rsmR2         rep
```

If stationary out-of-region (`rsmInRegion = False`) or saddle, optimize further away. First-order: **steepest ascent** moves region center along coded gradient (scale-invariant) next. `steepestAscentNatural maximize plan ys step nSteps` uses coded gradient for direction, **decodes path points to natural units**. `step` is coded-space step size (e.g. `0.5`); first point is design center.

```haskell
steepestAscentNatural True plan ys 0.5 5
-- [ [("temp",165.0),("time",15.0)]     -- Center
-- , [("temp",172.5),("time",13.2)]     -- +1 step (natural units; next experiment condition)
-- , ... ]                              -- More steps upward
```

Both **continuous factors only** (categorical in design → error). Pass `ys` in same run order as `designTable` / `designFrame`.

### Optimal / Computer-Generated Design

Standard grid plans (factorial / RSM) fix model and run count to a pre-set form. **Optimal design** `optimalDesign` is opposite: specify **desired model (formula) and run count `n` first**, then select `n` points from candidate set optimizing information matrix XᵀX criterion (default = **D-optimal** = `det(XᵀX)` maximum, Fedorov exchange). Use when budget constrains run count or non-standard model needed.

```haskell
-- 2-factor quadratic (p=6) in 10 runs, D-optimal.
let plan = optimalDesign [contFactor "temp" (150,180), contFactor "time" (10,20)]
                         (quadratic ["temp","time"])   -- Model = effects DSL
                         10                             -- Run count n (required)
print (designFrame plan)             -- 10-run runsheet (uncoded real values)
filledDf |-> designModel plan "y"    -- Fit LM with baked formula
```

```
------------------------
 run   |  temp  |  time
-------|--------|-------
Double | Double | Double
-------|--------|-------
1.0    | 150.0  | 10.0
2.0    | 180.0  | 10.0
3.0    | 165.0  | 15.0
4.0    | 150.0  | 20.0
5.0    | 150.0  | 15.0
6.0    | 150.0  | 10.0
7.0    | 180.0  | 20.0
8.0    | 165.0  | 10.0
9.0    | 180.0  | 15.0
10.0   | 165.0  | 20.0
```

Candidate grid (3 levels × 2 factors = 9 points) selects highest-info points. **Exact D-optimal with point repeats** — if `n` exceeds candidate points, boosts D by repeating top points (table: runs 1 & 6 both `(150,10)` = repeat for 10 total). For finer candidates, increase factor/level count or use `optimalDesignLevels` to raise grid levels.

Model specified via **`Formula` unified interface**. Two entry points:

- **Effects DSL** (interactive, typed): `mainEffects names` (`y ~ x1 + x2 + …`), `twoWay names` (main + all 2FI), `quadratic names` (main + interaction + quadratic, RSM-equivalent).
- **String formula**: `parseRFormula "y ~ a + b + a:b"` result (fit directly as `Formula`, for app/external model building).

```haskell
let Right fml = parseRFormula "y ~ temp + time + temp:time"
    plan      = optimalDesign specs fml 8
```

- **Candidate set** = auto grid from factors (each factor coded `[-1,1]` equally spaced). Default levels: **3** if model has quadratic, else **2**. Explicit: `optimalDesignLevels 3 specs fml n`.
- **Change criterion / seed**: `optimalDesignWith crit (Just levels) seed specs fml n` (`crit` = `DOpt` / `AOpt` / `IOpt` / `EOpt` / `GOpt` / `Compound …` / `BayesianD …`). Default seed = 42.
- **`n` required** (DOE = run cost). `n < p` (formula columns) = singular info matrix → **error**.
- **Categorical factors** mix via `catFactor` (grid-expanded, contrast-fit). Example: `optimalDesign [contFactor "x" (-1,1), catFactor "cat" ["A","B","C"]] (mainEffects ["x","cat"]) n`.

### Custom Design (Coordinate Exchange, Pure)

`optimalDesign` picks from coarse candidate grid (Fedorov exchange). **`customDesign`** moves each factor 1 coordinate at a time on fine grid (coordinate exchange); `n` run points unrestricted to grid levels — intermediate (e.g. `163.5` / `15.5` below). **Pure function** (seed argument; same seed → same result always; no IO) enables reproducible deterministic design as value.

Plan specified via **`CustomSpec`** (single). `customSpec factors formula nRuns seed` builds default (DOpt, no constraints, CRD); extend via record update for criterion/constraints/hierarchy.

```haskell
-- 2-factor quadratic in 10 runs. customSpec args 3/4 = run count / seed (deterministic).
let plan = customDesign (customSpec [contFactor "temp" (150,180), contFactor "time" (10,20)]
                                    (quadratic ["temp","time"]) 10 42)
print (designFrame plan)
filledDf |-> designModel plan "y"    -- Fit LM with baked formula (same as optimalDesign)
```

```
------------------------
 run   |  temp  |  time
-------|--------|-------
Double | Double | Double
-------|--------|-------
1.0    | 150.0  | 10.0
2.0    | 180.0  | 20.0
3.0    | 150.0  | 20.0
4.0    | 165.0  | 20.0
5.0    | 180.0  | 15.5
6.0    | 150.0  | 15.0
7.0    | 180.0  | 10.0
8.0    | 165.0  | 15.0
9.0    | 180.0  | 10.0
10.0   | 163.5  | 10.0
```

- **Factor types**: continuous (`contFactor`) / categorical (`catFactor`) / numeric ordered (`numFactor`) all supported. Numeric ordered optimized at level index (equal spacing), runsheet shows real values.
- **Model**: same effects DSL (`mainEffects`/`twoWay`/`quadratic`) / `parseRFormula`. Returned `Design` bakes formula (fit like `optimalDesign`).
- `optimalDesign` (grid Fedorov, no constraints) vs. `customDesign` (continuous coordinate exchange) are **separate implementations**. Coarse grid sufficient → `optimalDesign`; fine search / constraints / hierarchy needed → `customDesign`.

**Apply constraints and criteria**. Criteria (non-D) or **feasible-region constraints** specified in `CustomSpec` record fields (via update). Constraints satisfied during init and each coordinate search; no infeasible runs generated. **Natural units `csNatConstraints` recommended** (below); low-level coded `csConstraints` coexist, both accumulate.

```haskell
-- D-optimal 8 runs under temp ≤ 160 (natural units). No infeasible points selected.
let plan = customDesign (customSpec [contFactor "temp" (150,180), contFactor "time" (10,20)]
                                    (twoWay ["temp","time"]) 8 42)
             { csNatConstraints = [ natLeq [("temp",1)] 160 ] }
             -- Change criterion: { csCriterion = AOpt } etc. same pattern
```

Criteria (`csCriterion :: OptCriterion`) = `DOpt` / `AOpt` / `IOpt` / `EOpt` / `GOpt` / `Compound …` / `BayesianD …` (default `DOpt`). `customSpec` defaults = no constraints, CRD, so bare `customDesign (customSpec fs fml n seed)` = unconstrained D-optimal CRD.

#### Natural Units Constraint Writing (Recommended; `csNatConstraints`)

In practice, constraints are phrased in **experiment terms** (real temp, real flow). `csNatConstraints :: [NatConstraint]` references factors in **natural units**, auto-normalized to coded via factor bounds/scale inside `customDesign` (ignore coded `[-1,1]` or index). **This is recommended API.**

- **`natLeq [(factor,coeff)] rhs`** / **`natGeq …`** / **`natEq …`** — linear inequality/equality `Σ coeffᵢ·x_natᵢ  rel  rhs` (natural-unit coefficients).
  - **Continuous factors**: natural half-space directly (`temp <= 160` writable). Multi-factor sums ok.
  - **Numeric ordered (`numFactor`)**: **single-term only** — filters levels not meeting threshold (not half-space; level subsetting). E.g. `natLeq [("temp",1)] 160` with levels {150,165,180} → **150 only**. Linear combo with other factors → error.
  - **Log factors** (`contFactorLog`): **single-factor bounds only** (`natLeq [("conc",1)] 0.1`). Linear combos → error (nonlinear).
  - **Categorical**: no ordering, not referenceable (error). Use `natForbid` instead.
- **`natForbid [(factor, value)]`** — disallowed combinations. Categorical = level name (`FVText "A"`), numeric/continuous = **real value** (`FVDouble 180`; converted internally to index/coded).

```haskell
-- Real temp ≤ 160 and real flow ≥ 12, D-optimal 8 runs (continuous factors, natural units).
customDesign (customSpec [contFactor "temp" (150,180), contFactor "time" (10,20)]
                         (quadratic ["temp","time"]) 8 42)
  { csNatConstraints = [ natLeq [("temp",1)] 160, natGeq [("time",1)] 12 ] }

-- Numeric ordered threshold: temp<=160 → levels {150,165,180} → 150 only.
customDesign (customSpec [numFactor "temp" [150,165,180], contFactor "time" (10,20)]
                         (mainEffects ["temp","time"]) 8 42)
  { csNatConstraints = [ natLeq [("temp",1)] 160 ] }

-- Log-factor bound + categorical disallowed combo.
customDesign (customSpec [contFactorLog "conc" (0.01,10), catFactor "cat" ["A","B"]]
                         (mainEffects ["conc","cat"]) 8 7)
  { csNatConstraints = [ natLeq [("conc",1)] 0.1, natForbid [("cat", FVText "A")] ] }
```

**Infeasibility**: error reports **valid constraints and factor range in natural units** (e.g. `1.0·temp ≤ 100.0` / `temp ∈ [150.0, 180.0]`) for easy debugging.

> **JMP alignment**: JMP Custom Design also restricts linear constraints to continuous factors, natural input, categorical/discrete via Disallowed Combinations (= `natForbid`). First-class log factors are library advantage.

#### Low-Level Constraint Writing (Coded; `csConstraints`)

`csConstraints :: [Constraint]` is coded-unit **low-level escape hatch**. Complex constraints unspeakable in natural units (conditional, range override, etc.) or legacy code compatibility. Normally use `csNatConstraints` (above). **Factor reference in coded units** — continuous factor coded `[-1,1]` (`FVDouble`), categorical = level name (`FVText "A"`), numeric ordered = level index (`FVDouble`). Constraints satisfied during init (rejection sample) and each coordinate search; no infeasible runs. **4 `Constraint` types:**

**① `LinearIneq [(factor,coeff)] rel rhs`** — linear inequality/equality `Σ coeffᵢ·xᵢ  rel  rhs`. `rel` = `CLeq` (≤) / `CEq` (=) / `CGeq` (≥). **Continuous factors only** (tolerance 1e-9).

```haskell
LinearIneq [("x1",1),("x2",1)]  CLeq 0.5    -- x1 + x2 <= 0.5
LinearIneq [("x1",1),("x2",-1)] CEq  0      -- x1 = x2  (diagonal only)
LinearIneq [("x1",2),("x2",1)]  CGeq (-1)   -- 2·x1 + x2 >= -1
```

**② `RangeBound factor lo hi`** — narrow continuous factor to coded `[lo,hi]` (override default, tighten).

```haskell
RangeBound "temp" (-0.5) 1     -- temp coded range to [-0.5, 1] (use upper half, etc.)
```

**③ `Forbidden [(factor, value)]`** — disallow rows where **all** `(factor,value)` match (AND). Values: `FVText "level"` (categorical) / `FVDouble coded` (continuous/numeric index). **Mainly categorical disallowed combos** (continuous `FVDouble` brittle; exact grid match required).

```haskell
Forbidden [("catalyst", FVText "A"), ("temp", FVText "high")]  -- Catalyst A × high temp forbidden
Forbidden [("mode", FVText "off"), ("x1", FVDouble 1)]         -- mode=off and x1=+1(coded) forbidden
```

**④ `Conditional guard [constraints]`** — inner constraints active only if `guard` true. `guard` = `GuardEq factor value` / `GuardLeq factor c` / `GuardGeq factor c` / `GuardAnd [guards]` / `GuardOr [guards]`.

```haskell
-- If catalyst=A, restrict temp to coded [0,1] (high-temp half).
Conditional (GuardEq "catalyst" (FVText "A")) [ RangeBound "temp" 0 1 ]

-- If x1 >= 0 and x2 >= 0 (quadrant), enforce x1 + x2 <= 1.
Conditional (GuardAnd [GuardGeq "x1" 0, GuardGeq "x2" 0])
            [ LinearIneq [("x1",1),("x2",1)] CLeq 1 ]
```

**Notes:**

- **Factor reference in coded units** (this low-level section only). Coordinate exchange solves in coded space, so use coded (`-1`~`+1`), not real (150~180°C etc.). **For natural units use `csNatConstraints` (`natLeq` etc.)** (auto coded-conversion at entry). Manual: `coded = (real − center) / half-width`.
- **`LinearIneq` / `RangeBound` = continuous only**. Factor not found or categorical (type mismatch) = "violated" = rejected. For categorical conditions, use `Forbidden` / `Conditional` `GuardEq … FVText`.
- **Over-tight constraints** may find no coordinates → **error** (broaden feasible region or reconsider run count/levels).

### Hierarchical Design (Split-Plot, Strip-Plot, Block)

Experiments with "hard-to-change factors" (e.g., process temp) use **split-plot** structure — factor changed in **whole-plot batches** (no full randomization). Phase 79 specifies hierarchy not by factor role but **`CustomSpec` `csStructure :: Structure`** — factors stay pure factors; `Structure` **names** hierarchy membership. 4 `Structure` types:

- **`splitPlot ["temp"] 4`** — `temp` = whole-plot factor; 4 whole-plots. Whole-plot factor constant within group.
- **`stripPlot ["A"] 3 ["B"] 4`** — `A` = whole-plot (3 groups) × `B` = strip (4 groups), **cross 2-level**.
- **`blocked 3`** — 3 randomized blocks. All factors free within block; block = run allocation only (affects covariance).
- Default: **`CRD`** (completely randomized; `customSpec` default).

Internally, encode obs covariance `M = I + Σ η_g Z_g Z_gᵀ` (η = variance ratio, default 1.0) into D-optimal (`det(Xᵀ M⁻¹ X)` max) via **group-wise coordinate exchange** (Goos-Vandebroek 2003; **pure**). Works with constraints.

```haskell
let plan = customDesign (customSpec [contFactor "temp" (150,180), contFactor "rate" (10,20)]
                                    (twoWay ["temp","rate"]) 8 50)
             { csStructure = splitPlot ["temp"] 4 }   -- temp = whole-plot, 4 groups
print (designFrame plan)
```

```
-------------------------------------
 run   |  temp  |  rate  | wholePlot
-------|--------|--------|-----------
Double | Double | Double |    Text
-------|--------|--------|-----------
1.0    | 150.0  | 20.0   | wholePlot0
2.0    | 150.0  | 10.0   | wholePlot0
3.0    | 180.0  | 10.0   | wholePlot1
4.0    | 180.0  | 20.0   | wholePlot1
5.0    | 150.0  | 20.0   | wholePlot2
6.0    | 150.0  | 10.0   | wholePlot2
7.0    | 180.0  | 20.0   | wholePlot3
8.0    | 180.0  | 10.0   | wholePlot3
```

**Whole-plot factor `temp` constant within group** (`wholePlot`); sub-plot factor `rate` varies. `designFrame` outputs each group as **Text label column** (`splitPlot` → `wholePlot` column `wholePlot0…`); `stripPlot` → `wholePlot` + `strip` columns; `blocked` → `block` column. Group columns feed directly to **HBM random intercepts** — design → hierarchical analysis **seamlessly connected**:

```haskell
-- Whole-plot groups as random intercept. Separate group variation as RE; estimate fixed effects unbiased.
filledDf |-> designModelHBM defaultHBM plan [ranIntercept "wholePlot"] "y"

-- Combine rate<=0.5 (coded) constraint + split-plot (just add csConstraints):
--   customDesign (customSpec fs fml n seed)
--     { csStructure = splitPlot ["temp"] 4, csConstraints = [LinearIneq [("rate",1)] CLeq 0.5] }

-- Strip-plot: 2 group columns → 2 random intercepts (group column stream → multiple ranIntercept):
--   let plan = customDesign (customSpec fs fml 12 7) { csStructure = stripPlot ["A"] 4 ["B"] 3 }
--   df |-> designModelHBM defaultHBM plan [ranIntercept "wholePlot", ranIntercept "strip"] "y"
```

(Random effect declaration `ranIntercept` / `ranSlope` → below "Fit Model → Hierarchical").

## Fit Model

Given `Design` + data, fit model. **3 families: LM (`designModel`) / GP (`designModelGP`) / HBM (`designModelHBM`)**; all fit same plan/data by swapping spec, `multiOutput` / `profiler` work universally. LM is basic.

### Fit via LM (Basic; Implied Formula Linear)

**`designModel`** auto-expands design's **implied formula into general linear model (LM)**. Model not fixed; **design type determines** — full factorial → main + interaction (`y ~ temp * time`), RSM → quadratic (`+ I(temp^2) + I(time^2)`), fractional/OA → main only. `designFormula plan "y"` inspects actual formula. Profiler bands = LM normal CI. Multiple responses via `multiOutput`.

```haskell
-- Implied-formula LM. Multiple responses via multiOutput (single = ["y"] 1-element).
let model = filledDf |-> multiOutput ["strength","yield"] (designModel plan)
--  model :: [(Text, MultiLMModel)]

noDf |>> toPlot (profiler model ["temp","time"])   -- Prediction line + 95% CI + observed (workflow example)
```

### Fit via GP (Nonlinear, Nonparametric Profiler)

`designModel` fits implied-formula LM. Small-n DOE with **curvy response beyond quadratic** → **`designModelGP`** fits same plan/data via **GP (Gaussian Process regression)**. No formula needed — kernel learns nonlinearity directly; profiler band = LM normal CI → **GP posterior predictive band** (small-n nonparametric uncertainty). `multiOutput` / `profiler` unchanged (swap `designModel` → spec).

```haskell
-- Swap designModel → designModelGP defaultGP. defaultGP = RBF kernel, exact GP,
-- hyperparams auto via marginal likelihood (= GPConfig RBF Gp AutoMarginalLik).
let gpModel = filledDf |-> multiOutput ["strength","yield"] (designModelGP defaultGP plan)
--  gpModel :: [(Text, GPRegModelN)]

noDf |>> toPlot (profiler gpModel ["temp","time"])
```

![DOE profiler (GP version; band = GP posterior band)](../images/doe-profiler-gp.svg)

Kernel, approximation (exact GP / KRR / RFF), hyperparams (`AutoMarginalLik` / `AutoCV` / `FixedHyper`) via `GPConfig` (→ [02-regression](02-regression.md) kernel section). **Continuous factors only** (categorical → error).

Bands from **distribution-equipped approximations `Gp` / `GpRff`** (posterior band). **`Krr` / `KrrRff` (mean-only)** = **no band, line only** (KRR posterior mean = GP-identical, zero variance). DOE is small-n where band is profiler focus; default `Gp` apt. Large-n + band speed → `GpRff` (RFF approx).

### Fit via Hierarchical Model (Mixed-Effects; Group Variation as RE)

`designModel` / `designModelGP` fit single fixed effect across all data. But small-n DOE often sees **lot / day / operator / batch** groups with shifted response level; ignoring groups forces LM to push group differences into residuals, biasing coefficients/bands. **`designModelHBM`** fits same plan/data as **hierarchical Bayes (mixed-effects)**: implied formula fixed + **random group effect** (partial pool). Profiler band = **HBM posterior predictive** (fixed β draw variance + obs σ² noise, marginalized over group mean).

Groups specified via **typed random effect**. `ranIntercept "lot"` ≈ lme4 `(1|lot)`.

```haskell
-- Swap designModel → designModelHBM defaultHBM, specify lot as random intercept.
-- defaultHBM = NUTS. filledDf = has temp + lot group column.
let hbmModel = filledDf |-> designModelHBM defaultHBM plan [ranIntercept "lot"] "strength"
--  hbmModel :: DesignHBMFit

noDf |>> toPlot (profiler [("strength", hbmModel)] ["temp"])
```

![DOE profiler (HBM version; band = hierarchical Bayes posterior; lot random intercept)](../images/doe-profiler-hbm.svg)

Typed RE = `ranIntercept :: Text -> RandomSpec` (random intercept `(1|g)`) and `ranSlope :: [Text] -> Text -> RandomSpec` (correlated random slope `(1+s|g)`). **Both supported** (lme4 string formula → smart constructor). `ranSlope` includes intercept; group-varying slope design captures group-dependent interaction:

```haskell
-- lot-specific temp slope (interaction group-dependent). Absorb slope variation in RE; shrink obs σ.
let hbmSlope = filledDf |-> designModelHBM defaultHBM plan [ranSlope ["temp"] "lot"] "strength"
--  hbmSlope :: DesignHBMFit
```

Fixed-effect slope = partial-pooled group-average slope; group-wise slope differences absorbed as RE, shrinking σ (smaller vs. intercept-only fit). Internals = non-centered parameterization (`b_g = diag(τ)·Lcorr·z_g`) avoids funnel, lands on compiled gradient path (small-n NUTS convergent).

**Fixed effects = design-implied** (LM/GP). `designModelHBM` unfixed; implies formula via `designFormula plan y` (full factorial → main+interaction, RSM → quadratic), adds **specified random effects**. So "implied + optional randomization" — fixed part automatic, random part explicit.

**GP scope**: `designModelGP` = continuous only (categorical → error). `designModelHBM` = **categorical/group column as random block** — hierarchical model forte, opposite GP. `multiOutput` / `profiler` identical (swap `designModel` → spec; response last arg, curried → `multiOutput`).

#### HBM Diagnostics (DAG, Trace, etc.)

`DesignHBMFit` holds trained HBM via `dhfModel`, so [03-bayesian-hbm](03-bayesian-hbm.md) **diagnostic extractors** apply directly — `dagOf` (structure DAG), `tracesOf` (convergence trace), `ppcOf` (posterior predictive check), `energyOf` / `pairOf` (funnel), `forestOf` (HDI), etc. DOE can confirm NUTS convergence, group effect presence.

```haskell
let hbmModel = filledDf |-> designModelHBM defaultHBM plan [ranIntercept "lot"] "strength"
    m        = dhfModel hbmModel      -- Extract trained HBMModel

noDf |>> vconcat (tracesOf m)         -- Param trace stack (divergence rug ON default)
noDf |>> toPlot (dagOf m)             -- Structure DAG (plate-folded)
```

## Visualize

Fitted model via **profiler grid** (`profiler` + `profilerResidual`) and **response surface** (`contourOf`) overview. Points computed from model obs, bundled with `noDf`.

Default points = `Raw` (obs y), scatter around prediction line due to other factors (multivariate correct). **Partial residual** via `<> profilerResidual Partial` to inspect per-factor fit. Points → partial residual @fⱼ(xⱼ) + (full model residual)@, removing other-factor contribution; points align prediction (R `termplot(partial.resid=TRUE)` / `car::crPlots` equivalent).

```haskell
noDf |>> toPlot (profiler model ["temp","time"] <> profilerResidual Partial)
```

![DOE profiler (partial residual; points on prediction line)](../images/doe-profiler-partial.svg)

2-factor response plane flat-view → `contourOf` (**single model**). Grid 2 factors, fix others at median, eval response μ̂, **filled contour + contour lines** (R `rsm::contour` / matplotlib `contourf+contour` equivalent). `contourOf` takes 1 model, so from `multiOutput` result `[(Text,m)]`, **extract by response via `modelFor`** (vs. `snd (head model)`).

```haskell
noDf |>> contourOf (modelFor "strength" model) "temp" "time"   -- strength surface
```

![DOE response surface (RSM contour; plane)](../images/doe-contour.svg)

### 3D Response Surface (`surfaceOf`; Static SVG / WebGL)

3D response visualize → `surfaceOf`. Grid 2 factors, return `VisualSpec3D`, 3D renderer outputs. `modelFor` selects response (same as `contourOf`).

```haskell
import Hgg.Plot.ThreeD.Easy    (saveSVG3D)      -- Static (SVG / PDF / PNG)
import Hgg.Plot.ThreeD.Browser (saveHTML3D, showBrowser)  -- WebGL (interactive)

let surf = surfaceOf (modelFor "strength" model) "temp" "time"

saveSVG3D  "rsm-surface.svg"  surf   -- Static SVG (doc embed, print)
saveHTML3D "rsm-surface.html" surf   -- WebGL self-contained HTML (orbit camera)
showBrowser surf                     -- WebGL in-browser immediately
```

![DOE 3D response surface (quadratic regression + CCD points)](../images/rsm-surface-3d.svg)

- **Static**: `saveSVG3D` (vector, doc embed), `savePDF3D`, `savePNG3D` (`Hgg.Plot.ThreeD.Easy`).
- **WebGL**: `saveHTML3D` (self-contained), `showBrowser` (in-browser now) from `Hgg.Plot.ThreeD.Browser`. Orbit camera, rotate/zoom interactive.

## Low-Level API (Raw Design Generation)

High-level `Design` workflow underlies low-level functions handling designs as matrices (`[[Double]]`) — `Design.Factorial` / `Design.RSM` / `Design.Optimal` / `Design.Orthogonal` / `Design.Anova` / `Design.Power` / `Design.Quality` / `Design.Custom.*`. Use only for hand-held candidates or non-standard DIY designs (high-level API standard).

→ **Low-Level DOE API Reference** (internal document, not published) consolidated (full factorial / RSM / optimal / OA-Taguchi / ANOVA-power / constrained optimal / process capability / custom design [coordinate exchange, split-plot, Bayesian D]).
