# Semiconductor Device Development Workflow Manual

> 🌐 **English** | [日本語](semiconductor-design-workflow.ja.md)

> **Target Readers**: Semiconductor device designers / Process Technology (TEG) engineers / Technical staff responsible for maintaining reliability, yield, and performance margins. Written at a level understandable to those without deep statistical expertise.
>
> **Purpose of This Manual**: To reorganize the typical production workflow — "center design via Sim → margin exploration on real devices → verification with empirical rules" — using a methodology of **multi-factor DoE + multi-level sampling** (not relying on single-factor variation) combined with **surrogate model acceleration for Sim**. This handbook integrates library `hanalyze` capabilities and provides a practical procedure guide implementable in real-world operations.
>
> **Branch**: feature/phase28-jmp-equivalence-gaps (Phase 22-35 complete)
>
> **Status**: Phase 4 complete (chapters 1–10 + Appendices A and B; §3.1.1 / §4.0.4 / §5.1.5 / §7.3.1–7.3.3 / §7.6 deep dives). Appendix B implementation integrated in executable form via `cabal run cis-implant-workflow-demo` (includes LiNGAM causal search + DAG DOT export)

---

## Table of Contents

1. [Introduction](#1-introduction)
2. [Typical Design Workflow](#2-typical-design-workflow)
3. [Organizing Factors and Responses](#3-organizing-factors-and-responses)
4. [Broad Multi-Level DoE Capturing Interactions](#4-broad-multi-level-doe-capturing-interactions)
   - 4.0 [Operational Constraints: **1 Lot = 25 Levels, 2 Held at Center**](#40-operational-constraints-1-lot--25-levels-2-held-at-center)
   - 4.1 [Definitive Screening Design (DSD)](#41-definitive-screening-design-dsd)
   - 4.2 [Custom Design (I-optimal / D-optimal)](#42-custom-design-i-optimal--d-optimal)
   - 4.3 [Space-filling (Halton / Sobol / LHS)](#43-space-filling-halton--sobol--lhs)
   - 4.4 [Full / Fractional Factorial / Orthogonal Array](#44-full--fractional-factorial--orthogonal-array)
5. [Sim-Stage Efficiency via Surrogate Models](#5-sim-stage-efficiency-via-surrogate-models) — to be filled in Phase 2
6. [Real Device Margin Exploration](#6-real-device-margin-exploration) — to be filled in Phase 2
7. [Nonlinear and Boundary-Value Response Analysis](#7-nonlinear-and-boundary-value-response-analysis) — to be filled in Phase 2
8. [Multi-Objective Optimization](#8-multi-objective-optimization) — to be filled in Phase 2
9. [Best Practice Checklist](#9-best-practice-checklist)
10. [Pitfall Collection](#10-pitfall-collection)
- [Appendix A: Use Case → Library Function Quick Reference](#appendix-a-use-case--library-function-quick-reference)
- Appendix B: Sample Code (end-to-end) — to be added in Phase 2

---

## 1. Introduction

### 1.1 What Problem Does This Manual Solve?

Typical semiconductor device development flows as follows:

1. **Sim** (device + process) designs center conditions
2. Satisfying all requirements simultaneously via Sim (yield, drive current, leakage, reliability, …) often proves impossible → **real devices** must be evaluated
3. Empirical knowledge (previous-generation knobs, equipment operator intuition) guides which axes to vary
4. However, most approaches employ **single-factor-at-a-time (OFAT) variation**, obscuring factor interactions
5. Analysis often relies on "linear approximation" fitting, yet real responses exhibit count structure (yield = N_pass / N_die, leakage counts) or range constraints (spec limits, minimum drive), and we seek **quadratic extrema** or **threshold margins**

This manual reorganizes the above production flow using **multi-factor DoE + surrogate models + appropriate response models**, extracting **more information from the same experiment count**. Best practices for practical implementation are provided.

### 1.2 Operational Constraints Assumed in This Manual

Constraints confirmed from the user environment (2026-05-30):

- **1 lot = 25 levels (= 25 runs)**
- Of these, **2 levels are held at center condition** (= 2 forced center points)
- Effectively, **23 runs available for factor variation**
- A single lot prototype takes weeks to months + high cost → maximizing information extracted per experiment is critical

DoE design options under this constraint are detailed in §4.0.

### 1.3 Library `hanalyze` Positioning

This library is a Haskell-implemented statistics + DoE + surrogate + optimization toolkit. It provides capabilities roughly corresponding to JMP's function suite (`Fit Y by X` / `Fit Model` / `DoE > Custom Design` / `Profiler`, etc.), with functionality expanding across phases (currently Phase 35 complete, Phase 36 under consideration).

Where **code examples** appear in this manual, they use the public API of `Hanalyze.*` modules. Corresponding modules/functions are marked as **Related Code** at each section's end (file:line format).

---

## 2. Typical Design Workflow

### 2.1 Overview Diagram

```
   ┌──────────────────────────────────────────────────────────────┐
   │  Phase A: Sim-Driven Design                                 │
   │                                                              │
   │   1. Requirements definition (spec, target margin)           │
   │   2. Factor identification (control + noise)                 │
   │   3. Space-filling Sim initial exploration (Halton / Sobol)  │
   │   4. Surrogate model construction (RFF Ridge / GP / RF)      │
   │   5. Sim on surrogate → center condition (proposal)          │
   │                                                              │
   └─────────────────────────┬────────────────────────────────────┘
                             │ Center proposal + outstanding issues
                             ▼
   ┌──────────────────────────────────────────────────────────────┐
   │  Phase B: Real Device Margin Exploration (1 lot, 25 runs)    │
   │                                                              │
   │   6. Factor reduction (Sim surrogate sensitivity top + knobs)│
   │   7. **DoE selection** (DSD / Custom / OA; see §4)           │
   │      - Fit 2 forced center runs within 25 total             │
   │   8. Lot execution → response measurement                    │
   │   9. Analysis: main / interaction / quadratic decomposition  │
   │      - Assess whether linear suffices or nonlinear needed   │
   │  10. Profiler: response surface + margin visualization       │
   │                                                              │
   └─────────────────────────┬────────────────────────────────────┘
                             │ Improvement direction + concerns
                             ▼
   ┌──────────────────────────────────────────────────────────────┐
   │  Phase C: Supplemental Verification (augment, robust check)  │
   │                                                              │
   │  11. Augment (add axial / center) for quadratic / LOF        │
   │  12. Noise factors (temperature, lot-to-lot, wafer position) │
   │  13. Multi-objective optimization + validation (final lot)   │
   │                                                              │
   └──────────────────────────────────────────────────────────────┘
```

### 2.2 Phase Responsibilities

| Phase | Primary Goal | Cycle Time | Output |
|---|---|---|---|
| A | Factor screening + surrogate construction | days to weeks (sim volume dependent) | Surrogate model + center proposal |
| B | Real response collection + actual margin identification | 1 lot (weeks to months) | Effect decomposition + improvement direction |
| C | Reinforcement and confirmation | additional lots | Final proposal + validation data |

Maintaining consistent factor definitions and response metrics across phases is critical for comparing surrogates with real data (definition methods unified in §3).

---

## 3. Organizing Factors and Responses

### 3.1 Factor (Factor) Type Classification

DoE and analysis require early confirmation of factor types. Align with the `Factor` type handled by library `Hanalyze.Design.Custom.Factor`:

| Library Representation | Semiconductor Example |
|---|---|
| `Continuous lo hi` | implant dose (1e13–5e13), anneal temperature (900–1100°C), gate length (28–36 nm) |
| `DiscreteNum [xs]` | thickness list [3, 5, 7, 10 nm], step count [1, 2, 3] |
| `Mixture lo hi` | ratio (sum to 1) — CMP slurry ratio, reactant ratio |
| `Categorical [tags]` | mask type, equipment unit (Cat A / B / C), gas species |
| `Ordinal [tags]` | recipe generation (gen1 < gen2 < gen3) |

**Rules**:
- **If equipment makes discrete selections despite apparent continuity** (discrete setting increments), declare as `DiscreteNum`
- For categories where effect order matters, use `Ordinal`
- Sim may use `Continuous`, but switch to `DiscreteNum` at real device stage

**Related Code**: `src/Hanalyze/Design/Custom/Factor.hs:1`

#### 3.1.1 Discrete / Categorical Factors in DoE (Deep Dive)

**"Discrete factors warrant single-factor variation" is incorrect.** Within DoE, discrete factors are manipulated simultaneously with others. Here, discrete factor coding and design construction pitfalls are clarified.

##### Coding Schemes

| Coding | Example (3-level A/B/C) | Purpose | Quadratic |
|---|---|---|---|
| **Reference (Treatment) Contrast** | A → (0,0), B → (1,0), C → (0,1) | LM / GLM, effect difference relative to reference | ✗ |
| **Sum / Effect Contrast** | A → (1,0), B → (0,1), C → (-1,-1) | ANOVA, zero-center all levels | ✗ |
| **Ordinal Numeric Coding** | gen1 → 1, gen2 → 2, gen3 → 3 | Ordinal factors assuming equal spacing | △ (pseudo) |
| **Polynomial Contrast** | -1, 0, +1 (3 levels) | Ordinal separating linear + quadratic | ◯ |

**For Categorical**: Unordered discrete (equipment unit, mask type). Choose Reference or Sum contrast. **Quadratic is meaningless** (no inter-level distance). Library `Categorical [tags]` internally expands via Reference contrast when passed to Custom Design.

**For Ordinal**: Ordered discrete (generation, grade). If equal spacing holds, **polynomial contrast** directly estimates linear + quadratic. Library `Ordinal [tags]` handles this.

**For DiscreteNum**: Numeric values constrained by equipment (e.g., tilt angle = {0°, 7°, 15°, 30°}). Using numeric values directly, **2 levels yield linear, 3+ enable quadratic** (note: unequal spacing may degrade precision).

##### Pitfall: "Level Count = Quadratic Estimability"

Quadratic effects require **at least 3 levels** (to uniquely specify one curve).

| Level Count | Estimable Effects |
|---|---|
| 2 | linear only |
| 3 | linear + quadratic |
| 4–5 | linear + quadratic + cubic |
| 5+ | higher order (practically capped at 5 levels in production) |

For "broad multi-level variation," **5 levels is the practical ceiling.** Beyond this, run cost outweighs precision gains (overfitting / pure error increase).

##### Pitfall: Passing Discrete Levels to Custom Design I-optimal/D-optimal

Library `Hanalyze.Design.Optimal` presumes continuous factor coded space [-1, 1], optimizing via Fedorov exchange + local search. For `DiscreteNum [xs]`, **restrict search to xs alone**, respecting discrete constraints (handled internally by `factorGrid`).

Concretely:

```haskell
-- Tilt angle (equipment constraint) declared as 4-level discrete
let tiltFactor = F.discreteNumFactor "tilt_deg" [0, 7, 15, 30]
-- → I-optimal search restricts candidates to {0, 7, 15, 30} combinations
```

Writing this as `Continuous` and manually discretizing later causes the library's optimizer to return continuous optima (e.g., tilt = 4.3°), impossible to implement. **Always declare `DiscreteNum`.**

**Related Code**: `src/Hanalyze/Design/Custom/Factor.hs` `DiscreteNum` constructor, `src/Hanalyze/Design/Optimal.hs` search set construction

### 3.2 Response (Response) Type Classification

Response type determines **analysis model choice** before execution; error leads to precision loss via universal linear regression:

| Response Type | Example | Recommended Model | Library |
|---|---|---|---|
| Continuous (~Normal) | drive current, Vth, on-resistance | OLS LM / RFF Ridge | Phase 13, 17 |
| Count (yield numerator) | good dies / lot | Poisson GLM / Binomial GLM | Phase 13 GLM |
| Ratio (0,1 range) | yield (= pass rate), spec compliance | Logistic / Beta regression | Phase 13 |
| Binary (pass/fail) | reliability test pass/fail | Logistic GLM | Phase 13 |
| Time with censoring | TDDB / EM failure time | Weibull AFT / Kaplan-Meier | Phase 12 AFT |
| Count time series | in-line measurement evolution | State-space + Poisson | Phase 15 |

**"Just use linear LM for everything"** is a common production trap (§10.2). Using **correct link function + distribution** via GLM improves RMSE 3–5× routinely (confirm via comparison at start).

**Related Code**: `src/Hanalyze/Model/GLM.hs:1`, `src/Hanalyze/Model/AFT.hs:1`

### 3.3 Requirements (Spec) and Margin Target Expression

For each response, specify:

- **Specification (LSL, USL)**: product pass/fail threshold
- **Target**: design aim value (may differ from spec midpoint)
- **Target Margin**: desired σ in 6σ notation (backsolve from yield requirement)

Omitting this creates ambiguity during DoE design (§4) regarding response weight allocation and multi-objective optimization (§8).

---

## 4. Broad Multi-Level DoE Capturing Interactions

### 4.0 Operational Constraints: 1 Lot = 25 Levels, 2 Held at Center

User environment constraints (2026-05-30 confirmed):

- **N = 25 runs / 1 lot**
- **2 runs held at center condition** (all factors = center), pre-fixed (equipment tuning + drift monitoring assumed)
- Remaining **23 runs available for factor variation**

Three practical library solutions are presented. Optimality depends on factor count k and whether main effects alone suffice or quadratic + interactions are needed.

#### Option A — Custom Design (I-optimal) N=23 + AddCenter 2 (Recommended First)

**Procedure**:

1. Assemble Custom Design Spec with `cdsRuns = 23` (model = main + 2-way interactions + quadratic)
2. I-optimal optimizes 23 rows
3. `augmentMenu (AddCenter 2)` adds 2 center rows → 25 total
4. At lot execution, equipment places **2 forced center runs** at **timeline start + end** (or similar distribution; pair with time-randomization if drift monitoring desired)

**Strengths**:
- **Free choice of factor count / level count / estimation model** (k=4–10 practical)
- Center run placement (start-fixed / end-fixed / mid-insertion) adjustable via row reordering **after augment**
- Simultaneous quadratic + interaction estimation
- **Direct use of latest library Phase 22–26 features**

**Weaknesses**:
- Orthogonality not fully guaranteed (I-optimal minimizes prediction variance, D-optimal maximizes information matrix determinant; neither strictly orthogonal)
- Large k with 23 runs may lack information for full quadratic + interaction estimation → model reduction (linear + principal 2-way) may be necessary

**Related Code**:
- `Hanalyze.Design.Custom.Coordinate.CustomDesignSpec` (`src/Hanalyze/Design/Custom/Coordinate.hs:80` vicinity)
- `Hanalyze.Design.Optimal.iOptimalDesign` / `dOptimalDesign` (`src/Hanalyze/Design/Optimal.hs:1`)
- `Hanalyze.Design.Custom.Augment.augmentAddCenter` (`src/Hanalyze/Design/Custom/Augment.hs:116`)

**Code Example (Pseudocode)**:

```haskell
-- 4 continuous factors: implant, anneal temperature, gate length, thickness
let factors =
      [ continuousFactor "imp"    1e13  5e13
      , continuousFactor "anneal" 900   1100
      , continuousFactor "Lg"     28    36
      , continuousFactor "tox"    1.5   3.0
      ]
    spec0 = defaultCustomDesignSpec
              { cdsFactors = factors
              , cdsModel   = mainEffects <> twoWayInteractions <> pureQuadratic
              , cdsRuns    = 23
              , cdsCriterion = IOptimal
              }
base   <- runCustomDesignBuild spec0       -- 23 rows
final  <- augmentAddCenter factors base 2  -- + 2 center → 25 rows
```

#### Option B — Definitive Screening Design (DSD) k=11, N=25 (3 Centers)

DSD simultaneously estimates k-factor main effects, quadratic terms, and principal 2-way interactions with **2k+1 runs** (Jones-Nachtsheim 2011). Center points are structurally embedded.

**N=25 Configurations**:

| k | DSD Runs | Center Points | Total |
|---|---|---|---|
| 12 | 24 (= 2k) | 1 | 25 |
| **11** | **22** | **3** | **25** |
| 10 | 20 | 5 | 25 |

**Recommendation**: **k=11 + 3 centers**, interpreting user's 2 forced centers as **2 of 3 centers placed at lot start and end** (remaining 1 center mid-lot).

**Strengths**:
- **Orthogonality guaranteed** (DSD main effects orthogonal, quadratic orthogonal to main)
- **Up to k=11 factors** (satisfies "broad multi-level" need)
- Principal interactions estimable without aliasing (Jones-Nachtsheim property)
- Center points structurally embedded → equipment drift monitoring naturally integrated

**Weaknesses**:
- Each factor **3 levels only** (-1 / 0 / +1). "Broad multi-level" (5+ levels) unsuitable
- "Threshold margins" (responses with sharp single-sided transitions) may require 5+ levels (→ §7.3 RSM + augment reinforces)
- Library `dsdDesign 11` is **structural DSD** (verified only for k=4; k≥5 uses Hadamard-like approximation). Orthogonality approximate, not strict conference matrix (`dsdHasOptimal = False`). → See §10.4 pitfall

**Related Code**:
- `Hanalyze.Design.DSD.dsdDesign` (`src/Hanalyze/Design/DSD.hs:51`)
- Output: coded space `{-1, 0, +1}` as `(2k+1) × k` matrix

**Code Example (Pseudocode)**:

```haskell
case dsdDesign 11 of
  Left err  -> error (show err)
  Right res -> do
    -- res = 23 runs (= 2*11 + 1), one is center
    -- Add 2 forced centers → total 25 runs (3 centers)
    let base   = dsdMatrix res            -- 23 × 11
        full   = base LA.=== centerRows   -- + 2 center rows
        ...
```

#### Option C — Standard Orthogonal Array (L18) + 7 Augment (Not Recommended)

Library lacks **L25 (5^6)** (only L4 / L8 / L9 / L12 / L16 / L18 available). Augmenting L18 (18 runs) with 7 center + axial runs to reach 25 is possible but **orthogonality not fully preserved**, so not actively recommended.

**When to Use**: Only when "8 two-level + 1 three-level factor" is clearly defined and L18 fits perfectly. Treat augment portion as "equipment drift monitor" + "extrapolation axis."

**Related Code**:
- `Hanalyze.Design.Orthogonal.l18` (`src/Hanalyze/Design/Orthogonal.hs:133`)

#### Comparison Summary

| Aspect | A: Custom (Recommended) | B: DSD k=11 | C: L18 + Augment |
|---|---|---|---|
| Factor Count | Flexible (4–10 practical) | Fixed 11 | 8–9 |
| Level Count | Flexible | 3 only | Mostly 2 (some 3) |
| Quadratic Estimation | ◎ | ◎ | △ (post-augment only) |
| Interaction Estimation | ◎ | ○ (principal only) | × (heavy aliasing) |
| Clean 2-center Integration | ◎ (AddCenter) | △ (2 of 3 centers) | △ |
| Orthogonality | △ (I-/D-optimal: prediction-optimal) | ○ (approximate) | × (post-augment) |
| "Broad Multi-Level" | ◎ | △ | × |
| Library Maturity | ◎ (Phase 22–26 primary) | ○ (k=4 verified only) | △ |

**Recommendation**: For 4–8 factors with multi-level (5+ levels), use **A (Custom)**. For ~11-factor screening, use **B (DSD)**.

#### 4.0.4 Few Factors (k=3), Ample Runs, Discrete Mix Strategy

User's typical case (ion implant: **dose / energy / tilt angle**, 3 factors) with **limited factors and 25 runs surplus** allocates slack to **multi-level expansion + replication (pure error) + lack-of-fit detection**.

##### Degrees of Freedom Budget

For 3 factors + 25 runs (= 23 + 2 center) fitting **quadratic model (main + 2-way + quadratic)**:

- intercept: 1
- main effects: 3
- 2-way interactions: C(3,2) = 3
- pure quadratic: 3
- **Total: 10 parameters**

23 runs - 10 = **13 degrees of freedom surplus**. This enables:

- **Lack-of-fit** detection with high power (model inadequacy reveals response warping)
- **Pure error** estimation via replication placement
- **5+ level expansion** capturing high-order (cubic, threshold) effects

##### Multi-Level Expansion (Continuous Factors)

Continuous dose / energy factors can expand to **5 levels** (= -1, -0.5, 0, +0.5, +1), simultaneously estimating quadratic + threshold (saturation).

```haskell
-- dose / energy declared 5-level continuous. Library internally
-- I-optimal searches continuous range; real device assignment
-- rounds to 5 discrete levels.
let factors =
      [ F.continuousFactor "dose"   1e13   5e13     -- 5 levels: 1e13, 2e13, 3e13, 4e13, 5e13
      , F.continuousFactor "energy" 5      50       -- 5 levels: 5, 16.25, 27.5, 38.75, 50 [keV]
      , F.discreteNumFactor "tilt"  [0, 7, 15, 30]  -- 4 levels, equipment constraint
      ]
```

Or **explicitly pass DiscreteNum** 5-level, restricting library search set (no rounding needed in deployment):

```haskell
let factors =
      [ F.discreteNumFactor "dose"   [1e13, 2e13, 3e13, 4e13, 5e13]
      , F.discreteNumFactor "energy" [5, 15, 25, 35, 50]
      , F.discreteNumFactor "tilt"   [0, 7, 15, 30]
      ]
```

Candidate combinations: 5 × 5 × 4 = 100 → I-optimal selects 23 runs. I-optimal **auto-allocates level usage** (typically **center levels frequent**, **edge levels minimal**).

##### Replication and Pure Error Estimation

I-optimal selection may naturally duplicate some conditions (information maximization result). These **natural replicates** yield **pure error σ² estimation**. JMP calls this "Replication Check" (standard feature).

To explicitly add replication in library:

```haskell
-- Add replicate on top of 23 I-optimal runs. Since 23+1+2 = 26 > 25,
-- reduce to 22, then: 22 + replicate 1 + forced center 2 = 25 (clean structure)
let spec  = ...{ cdsRuns = 22, cdsCriterion = Coord.IOptimal }
ebase  <- Coord.runCustomDesignBuild spec
withRep <- Aug.augmentMenu spec (Aug.Replicate 1)  -- 22 + 1 = 23
final  <- Aug.augmentMenu spec (Aug.AddCenter 2)   -- 23 + 2 = 25
```

Post-design, **verify actual duplicates** (library diagnostics via `Hanalyze.Design.Diagnostics` provides replication report).

##### Lack-of-Fit Detection

Once pure error is estimable, quadratic model **lack-of-fit F-test** executes:

```
F_LOF = MS_LOF / MS_PureError
```

Large F signals "quadratic cannot capture all nonlinearity (cubic / threshold / saturation)." Detected LOF → proceed to §7.3 / §7.4 (RSM + segmented / GP / GBM).

**Related Code**: `Hanalyze.Design.Anova.lackOfFit` (verify via grep):

```bash
grep -n "lackOfFit\|LackOfFit" src/Hanalyze/Design/Anova.hs
```

If absent, combine F-test from `Hanalyze.Stat.Test`.

##### k=3 Alternative Designs (Reference)

Other 25-run options for 3 factors:

| Design | Runs | Factor Levels | Quadratic | Use Case |
|---|---|---|---|---|
| **Custom I-optimal (Recommended)** | 23 + 2c = 25 | Flexible (5+5+4, etc.) | ◎ | Standard |
| Box-Behnken k=3 | 12 + 3c = 15 | 3 | ◎ | Fewer runs desired |
| **Box-Behnken k=3 + Replicate** | 12 + 3c + 10rep = 25 | 3 | ◎ + High LOF Power | LOF + Pure Error Priority |
| Face-Centered CCD k=3 | 8 + 6 + 1c = 15 | 3 (face) | ◎ | Pure RSM |
| **Face-Centered CCD + Replicate** | 8 + 6 + 1c + 10 = 25 | 3 | ◎ | LOF + Pure Error Priority |
| DSD k=3 | 7 (= 2k+1) | 3 | ◎ (structural) | Screening-leaning |
| **DSD k=3 + Replicate / Augment** | 7 + 18 augment | 3–5 | ◎ | Screening → RSM Transition |

**Recommendation**: For 5-level threshold detection, use **Custom I-optimal 5+5+4 levels with 23 runs + 2 centers**. For pure quadratic sufficiency, **Box-Behnken + replicate** also attractive (simpler implementation).

> Fact: `Hanalyze.Design.RSM` implements `boxBehnken :: Int -> Int -> [[Double]]` and `centralComposite` (verified in `src/Hanalyze/Design/RSM.hs:1`)

### 4.1 Definitive Screening Design (DSD)

(§4.0 detailed this; here, essentials only)

- **2k+1 runs** efficiently estimate k-factor main effects + quadratic + principal 2-way interactions
- **Primary tool for screening** (narrow which factors matter)
- For 5+ levels, proceed to §4.2 (Custom Design)
- Related Code: `src/Hanalyze/Design/DSD.hs:1`

### 4.2 Custom Design (I-optimal / D-optimal)

Library's **primary design tool** (Phase 22–26: 5 phases invested).

#### 4.2.1 D-optimal vs. I-optimal Distinction

- **D-optimal**: Maximize information matrix determinant. **Improves parameter estimate precision**
  - Use when: Interest in coefficients themselves (sensitivity comparison, etc.)
- **I-optimal**: Minimize factor space prediction variance integral. **Improves prediction accuracy**
  - Use when: Response profiling for optimal point search (RSM-like goal)
- Semiconductor margin search **defaults to I-optimal** (typical workflow: visualize response, select optimum condition)

#### 4.2.2 Custom Design Spec Assembly

```haskell
data CustomDesignSpec = CustomDesignSpec
  { cdsFactors     :: [Factor]
  , cdsModel       :: Model           -- main / interaction / quadratic composition
  , cdsRuns        :: Int             -- total runs (here: 23 → augment +2)
  , cdsCriterion   :: OptCriterion    -- IOptimal / DOptimal
  , cdsInitial     :: Maybe (LA.Matrix Double)  -- existing design (augment origin)
  , ...
  }
```

**Related Code**: `src/Hanalyze/Design/Custom/Coordinate.hs:80`

#### 4.2.3 Augment Menu

Post-design additions via `augmentMenu` offer 5 types:

- `Replicate n`  — duplicate existing (pure error estimation)
- `AddCenter n` — **case here**: add 2 centers
- `AddAxial α`  — add ±α axial points per continuous factor (quadratic reinforcement)
- `AddRuns n`   — Fedorov exchange adds n rows (information strengthening)
- `Foldover`    — sign-flip alias resolution

**Related Code**: `src/Hanalyze/Design/Custom/Augment.hs:80`

### 4.3 Space-filling (Halton / Sobol / LHS)

**Quasi-random sampling for surrogate training**, distinct from DoE. Phase A (Sim) uniformly samples factor space:

- **Halton Sequence**: Low-dimensional (~10) even density; library-implemented
- **Sobol Sequence**: High-dimensional maintains evenness
- **Latin Hypercube**: Equalizes marginal distributions per factor

**Related Code**: `src/Hanalyze/Design/SpaceFilling.hs:1`, `src/Hanalyze/Stat/QuasiRandom.hs:1`

### 4.4 Full / Fractional Factorial / Orthogonal Array

**Full Factorial** requires runs = (factor count) × (level count) product (e.g., 2^5 = 32 runs). Under 25-run constraint, 4-factor 2-level = 16 maximum; 5 factors infeasible. → Typically default to Fractional or DSD / Custom.

**Orthogonal Array (Taguchi-style)** available in library: L4 / L8 / L9 / L12 / L16 / L18. **L25 unimplemented.** Useful for noise factor arrangement (§6.3 robust design).

**Related Code**: `src/Hanalyze/Design/Orthogonal.hs:1`, `src/Hanalyze/Design/Taguchi.hs:1`

---

## 5. Sim-Stage Efficiency via Surrogate Models

When single Sim runs require hours to days, naively exhaustive Sim is infeasible. **Build surrogate models** interpolating responses from sparse Sim samples, then optimize over surrogates (broad search + optimization).

### 5.1 Surrogate Type Trade-offs

| Surrogate | Strength | Weakness | Recommended | Library |
|---|---|---|---|---|
| **Polynomial (LM)** | Interpretable, fast, uncertainty available | Weak on nonlinear / local structure | RSM range (~30 points, 2nd surface) | `Hanalyze.Model.LM` (Phase 13) |
| **Gaussian Process (GP)** | Prediction + uncertainty, smoothness prior | O(n³) compute; n>1000 impractical | n=50–500, BO use | `Hanalyze.Model.GP` |
| **RFF Ridge** | GP approximate, high-speed (n>1000 OK) | Uncertainty coarser than GP, weak extrapolation | n=100–10000, smooth response | `Hanalyze.Model.RFF` |
| **RandomForest** | Interpretable (importance), robust | Non-smooth (stepped), no extrapolation | Nonlinear + feature importance | `Hanalyze.Model.RandomForestRegressor` (Phase 17) |
| **GBM** | High accuracy, strong residual-based | Tuning required, BO lacks uncertainty | Pure prediction priority | `Hanalyze.Model.GradientBoosting` (Phase 34-A1) |

**Initial recommendation**: GP (BO native partner + uncertainty available). As n>500, switch to RFF Ridge. For feature-effect visibility, pair RF importance.

#### 5.1.5 Sim "Heavy" vs. "Light" Strategy by Cost

Single Sim **runtime dictates viable strategies**. Rules of thumb:

| Sim 1-point | Feasible Strategy | Recommended Surrogate / Approach | Realistic n |
|---|---|---|---|
| **Seconds–Minutes** | Full-space brute-force possible | LM / RF sufficient; BO unnecessary | n=1000–10000 direct |
| **Tens of min.–1 hr** | Initial N=50–100 + surrogate | **GP + BO**, standard (§5.3) | n=50–200 |
| **Hours** | Strict budget discipline required | **GP + BO** add points 1 per iteration + **early stopping** (halt on stalled improvement) | n=20–80 |
| **Half-day–1 day** | Surrogate alone insufficient | **Multi-fidelity**: coarse Sim abundant + fine Sim sparse, correlation-model merged | Fine: n=5–20; Coarse: n=50–200 |
| **Days–Weeks** | Sim-driven optimization impractical | Screen via surrogate, real device DoE primary (§4.0 25-run design iterated) | Fine: n=3–10 |

##### "Light" Case (~minutes): Brute-force + Simple Model

Minutes-per-point Sim favors **uniform space scanning + visualization**:

```haskell
-- 3 factors, 1000-point Halton → full Sim → visualization
let xs = haltonSamples 1000  -- ←§5.2 reference
ys <- mapM runSim (map (toRaw factors) xs)
-- Fit RF, inspect importance + partial dependence
let rf = RFR.fit (LA.fromLists xs) (LA.fromList ys) ...
```

LM / RF surrogates suffice; BO setup overhead unjustified.

##### "Moderate" Case (~hour): GP + BO Standard Cycle

This is **where library BO shines** (§5.3 detail):

```haskell
let cfg = BO.defaultBayesOptConfig { BO.boIters = 50 }
res <- BO.bayesOptND cfg evalSimND  -- 50 iterations = 50 Sim calls
```

##### "Heavy" Case (~half-day): Early Stopping + Acquisition Tuning

Tighter budget:

1. Initial N=20 rough surrogate
2. **BO 5–10 iterations**, record improvement (best_y change) each
3. **Halt if improvement stalls ≤ threshold 3 iterations running** (early stopping)
4. Acquisition: **EI → UCB small κ** (exploitation-weighted)

```haskell
let cfg = BO.defaultBayesOptConfig
            { BO.boIters       = 10
            , BO.boAcquisition = BO.UCB 1.0    -- κ=1, exploitation-biased
            -- early stopping via manual callback
            }
```

##### "Super-Heavy" Case (~day): Multi-Fidelity Surrogate

When **both coarse and fine Sim available**:

- Coarse Sim abundant (N=200–500) **learns global structure**
- Fine Sim sparse (N=5–20) **learns coarse-fine gap**
- Composite: prediction = coarse + gap-correction

```haskell
-- Coarse surrogate (RFF Ridge, fast)
let rffCoarse = RFF.fitRFFRidge ... xCoarse yCoarse
-- Gap surrogate (GP, smooth)
let yDiff = zipWith (-) yFine (map (RFF.predictRFFRidge rffCoarse) xFine)
    gpDiff = GP.fitGP ... xFine yDiff
-- Composite prediction
predict x = RFF.predictRFFRidge rffCoarse x + GP.predictGP gpDiff x
```

**Implementation Note**: Library presently lacks single multi-fidelity function, requiring **manual 2-fit composition**. Future Phase NN candidate (multi-fidelity Bayes opt).

##### "Colossal" Case (~week): Abandon Sim-Driven Optimization

Days+ per Sim point makes "Sim optimize + verify" unrealistic. Instead:

- **Sim checks direction only** (3–5 points, sanity check)
- **Real device DoE leads** empirically (§4.0 25-run design iterated)
- **Post-verification via Sim** (pursue outstanding issues)

This scenario relies primarily on §6 (real device margin search).

### 5.2 Initial Sample Size Decision

Empirical guideline (Loeppky-Sacks-Welch 2009):

- **k continuous factors → N₀ = 10k** standard recommendation
- 5 factors → N₀ = 50. Sim @ 1 hr/point → 50 hours ≈ 2 days, initial fit feasible
- Uncertainty-capable surrogate (GP) **adds points in high-variance regions** → ~20–30% sample reduction possible (§5.3 BO active)

**Initial point selection**: **Halton or Sobol sequence** uniformly fills factor space. Library `Hanalyze.Stat.QuasiRandom.radicalInverse` per-Halton-dimension; `Hanalyze.Design.SpaceFilling` LHS available.

```haskell
-- 5 factors, 50 Halton points
import qualified Hanalyze.Stat.QuasiRandom as QR
let xs = [ [ QR.radicalInverse (primes !! d) i | d <- [0..4] ]
         | i <- [1..50] ]
    primes = [2, 3, 5, 7, 11]
-- Each xs[i] ∈ [0,1]^5. Convert to raw factor units via (lo, hi) scaling → Sim input
```

**Related Code**: `src/Hanalyze/Stat/QuasiRandom.hs:1`, `src/Hanalyze/Design/SpaceFilling.hs:1`

### 5.3 Adaptive Sampling (Bayesian Optimization)

BO couples GP surrogate + **acquisition function** recommending next Sim point(s) iteratively. Samples selected for **maximum information value**.

#### Typical BO Cycle

```
1. Initial N₀ points via Halton → Sim → (X, y) collected
2. GP fit → acquisition function (EI / UCB) evaluated over factor space
3. Acquisition argmax point → 1 additional Sim → (X, y) extended
4. Repeat steps 2–3 until budget exhausted (Sim count or improvement plateau)
```

Library:

```haskell
import qualified Hanalyze.Optim.BayesOpt as BO
import qualified Hanalyze.Model.GP       as GP

-- 1D sample
let cfg = BO.defaultBayesOptConfig
result <- BO.bayesOpt cfg evaluateSim  -- evaluateSim :: Double -> IO Double

-- N-dimensional
result <- BO.bayesOptND cfg evaluateSimND  -- :: [Double] -> IO Double

-- Multi-objective (scalar aggregation)
result <- BO.bayesOptScalarMO 100 cfg evalListMulti
```

**Related Code**: `src/Hanalyze/Optim/BayesOpt.hs:1`, `src/Hanalyze/Model/GP.hs:1`

#### Acquisition Function Selection (Practical)

| Function | Meaning | Usage |
|---|---|---|
| **EI** (Expected Improvement) | Maximize expected gain over known best | Local optimization, exploit near-known-good |
| **UCB** (Upper Confidence Bound) | Maximize μ + κσ | Exploration-exploitation balance; more global |
| **PI** (Probability of Improvement) | Maximize improvement probability | Conservative, fine-tune local optima |

> Fact: Library `BayesOptConfig` allows acquisition / κ tuning (`src/Hanalyze/Optim/BayesOpt.hs:1`)

**Practical Tips**:
- Early: **UCB large κ** (broad search) → Later: **EI / UCB small κ** (exploit)
- BO **does not shrink search bounds** (no boundary bias). If equipment risk near limits, **add explicit constraints** to BO or manually shrink bounds

### 5.4 Surrogate Accuracy Evaluation

**Leave-One-Out CV** measures surrogate prediction error from N-point fit:

1. For i=1..N: Omit i, fit N-1 points → predict i → residual
2. Aggregate residuals → RMSE / R² / residual plot

**Residual Plot Interpretation**:

- **No pattern** = surrogate OK
- **Funnel-shaped (heteroskedastic)** = variance change; consider GLM / weighted regression
- **Curved pattern** = linear terms insufficient; try quadratic / GP / RF
- **Cluster offset** = unmodeled interactions; add interaction terms

Surrogate accuracy meeting tolerance (response scale 5% below target) → proceed to Phase B (real device DoE). Otherwise, **add samples or switch surrogate type**.

---

## 6. Real Device Margin Exploration

Real device DoE integrates Sim-surrogate-identified **top sensitivity factors** with **empirical knobs**. Operate within §4.0's 25-run / 2 forced-center constraint.

### 6.1 Transition from Single-Factor to Multi-Factor DoE

Where OFAT production culture dominates:

| Step | Content | Notes |
|---|---|---|
| 1 | Re-analyze past OFAT data; visualize **main effects** | `Hanalyze.Stat.Test` t / Wilcoxon |
| 2 | Narrow to 5–8 factors (OFAT large effects + empirical knobs) | Overcrowding → poor 23-run estimation |
| 3 | **Calculate DoE power estimate** | `Hanalyze.Design.Power` (Phase 14) |
| 4 | Compare DoE vs. OFAT results **on same response** | Opposing effects → suspect interaction |
| 5 | Vet DoE-discovered interactions with equipment/process engineers | Physically meaningful? |

OFAT re-analysis covered largely by Phase 13 Fit Y by X / ANOVA.

**Related Code**: `src/Hanalyze/Design/Anova.hs:1`, `src/Hanalyze/Design/Power.hs:1`

### 6.2 Embedding Empirical Rules as Constraints

"Implant dose ≥ 3e13 AND temperature ≥ 1050°C ruins diffusion profile" (forbidden region) → incorporate into DoE:

**Custom Design Constraints** express linear boundaries:

```haskell
-- Constraint: 2*imp + temp ≤ 1.5 (coded space)
-- = "High imp + high temp forbidden"
import qualified Hanalyze.Design.Custom.Constraint as Con

let cons = Con.linearConstraint [(0, 2), (1, 1)] Con.LE 1.5
    spec = ...{ cdsConstraints = [cons] }
```

**Related Code**: `src/Hanalyze/Design/Custom/Constraint.hs:1`

Non-linear empirical constraints (curved exclusion) → **pre-filter grid removing forbidden points**, pass allowed list to `Optimal` search:

- I-optimal / D-optimal selects 23 rows from allowed pool

### 6.3 Noise Factor Orthogonal Variation (Robust Design)

Taguchi inner ×outer arrangement:

- **Inner (control factors)**: Controllable. §4.0 Custom 23 runs
- **Outer (noise factors)**: Uncontrollable (temperature, lot-to-lot, wafer position, equipment unit)

Outer swept across 4 levels (L4 / L8), crossed with inner 23 → 92 runs total, exceeding 25-run constraint (separate budget: 4 sub-lots per lot).

**Practical Production Solution**:
- Sweep outer across **multiple lots** (1 lot = inner 25, 4 lots = 4 noise conditions)
- Treat inter-lot noise as **block factor**, input to GLM / LM

**Related Code**: `src/Hanalyze/Design/Block.hs:1`, `src/Hanalyze/Design/Taguchi.hs:1`

---

## 7. Nonlinear and Boundary-Value Response Analysis

Real device analysis requires **response-type-appropriate models** (§3.2). Typical patterns below.

### 7.1 Count Data → Poisson / NB GLM

Non-negative integer responses (good dies, leakage bits, failure modes) → Poisson distribution. Over-dispersion (Var > Mean) → Negative Binomial.

```haskell
import qualified Hanalyze.Model.GLM as GLM

-- y = good dies (Poisson)
let fit = GLM.fitGLM GLM.Poisson GLM.LogLink x y
    pred = GLM.predictGLM fit xNew
-- Over-dispersion check: deviance / df > 1.5 → switch to NB
```

**Poisson link = log**, so coefficient β = 0.3 means **exp(0.3) ≈ 1.35× occurrence rate** (multiplicative interpretation).

**Related Code**: `src/Hanalyze/Model/GLM.hs:1`

### 7.2 Bounds (0,1 Range) → Logistic / Beta / Tobit

Ratio responses (yield = pass rate, spec compliance):

- **Per-die binary** (pass/fail per die, available): **Logistic GLM** fit at die level (highest information)
- **Lot-aggregated ratio only**: **Beta regression** (requires GLMM extension) or **arcsin√p + LM** (classical approximation)
- **Lower-bound 0 (left-censored)**: **Tobit model** (library unimplemented; implementable as GLM derivative if needed)

```haskell
-- Per-die binary
let fit = GLM.fitGLM GLM.Binomial GLM.LogitLink x y
```

**Logit link** → coefficient β as **log-odds**; **odds ratio = exp(β)** interpretation.

### 7.3 Quadratic Extrema / Threshold Margins → RSM + Canonical Analysis

Responses with **quadratic extrema** (e.g., drive current vs. gate length) → **Response Surface quadratic fit**:

```haskell
import qualified Hanalyze.Design.RSM as RSM

-- Expand design to quadratic basis
let qDesign = RSM.quadraticDesign xData  -- main + 2-way + quadratic
    qFit    = RSM.fitQuadratic xData y
    (xStar, yStar, eigvals) = RSM.optimumPoint qFit
-- xStar: estimated extremum factor coordinates
-- yStar: response at extremum
-- eigvals: eigenvalues (sign determines extremum type)
```

**Canonical Analysis Interpretation**:

- **All positive eigenvalues** = local minimum (response smallest here)
- **All negative eigenvalues** = local maximum (response largest)
- **Mixed sign (saddle point)** = can improve by moving along negative-eigenvalue direction
- **Small |eigenvalue|** = flat response direction (movement negligible) → **process margin axis** (absorb variation)

Threshold responses ("ramping from inactive to active") require higher model orders. **Quadratic alone cannot capture threshold** (only smooth parabola); §7.3.2 addresses this.

**Related Code**: `src/Hanalyze/Design/RSM.hs:1`, `src/Hanalyze/Design/MultiRSM.hs:1`

#### 7.3.1 Quadratic Extrema / Saddle Point Engineering Judgment (Deep Dive)

From `optimumPoint` eigenvalue + eigenvector → design decision patterns:

##### Pattern 1: All Eigenvalues > 0 (Local Minimum, Response Minimized)

- Example: "Minimize leakage Ioff" with all factors convex downward → center is minimum
- **Decision**: Candidate `xStar` for adoption. Verify proximity to factor range center (robust if centered, risky if near boundary where range expansion may lower further)

##### Pattern 2: All Eigenvalues < 0 (Local Maximum, Response Maximized)

- Example: "Maximize yield" with upward-convex → `xStar` maximizes
- **Decision**: Adoption candidate, but if spec satisfied, consider **slightly inward** (interior to boundary, more robust against parameter drift)

##### Pattern 3: Mixed Sign (Saddle Point)

- Example: Drive current vs. Lg / tox often exhibits mixed curvature
- **Decision**: `xStar` is saddle. **Improvement direction** = negative-eigenvalue eigenvector → augment experiment in that direction

```haskell
let (xStar, yStar, eigs) = RSM.optimumPoint qFit
    posDirs = [ vi | (eig, vi) <- zip eigs eigVecs, eig > 0 ]
    negDirs = [ vi | (eig, vi) <- zip eigs eigVecs, eig < 0 ]
-- Augment: push from `xStar` along negDirs by ±α
```

##### Pattern 4: |Eigenvalue| Minimal (Flat Response Direction)

- Example: "Electrically inactive direction" = process variation absorption axis
- **Decision**: That eigenvector direction is **process margin** (lot-to-lot drift / wafer position variation tolerated here)

#### 7.3.2 Capturing "Threshold Margin"

Quadratic functions **only produce smooth parabola**, missing:

- **Threshold (on/off)**: dose < D_th → no effect; dose ≥ D_th → sharp rise
- **Saturation**: energy increase → initial gain; plateaus beyond E_sat
- **Step cliff**: tilt beyond T_cliff → abrupt degradation

Quadratic **averages these away**. Solutions:

##### A. Segmented (Piecewise Linear) Regression

Assume 1 threshold ("knot"), fit separate slopes pre/post:

```
y = β₀ + β₁ x          (x ≤ knot)
y = β₀ + β₁ x + β₂ (x - knot)   (x > knot)
```

Library presently lacks dedicated segmented API (Phase NN candidate). Workarounds:
- **Grid-search knot** over candidates, compare fit RSS
- **Adaptive Lasso** (Phase 31) over hinge basis (max(0, x - c)) candidates
- **Spline basis** (Phase 33): `Hanalyze.Model.FDA` `smoothBasis`

##### B. Hinge Basis + LM

Place **candidate threshold points** as hinge basis (`max(0, x - knot)`):

```haskell
-- Candidate thresholds for dose
let doseKnots = [1.5e13, 2e13, 2.5e13, 3e13, 3.5e13]
    hingeBasis x k = max 0 (x - k)
    -- Add hinge columns to design matrix
    xExt = LA.fromColumns
             (LA.toColumns x ++
              [ LA.fromList [ hingeBasis (LA.atIndex x (i,0)) k | i <- [0..n-1] ]
              | k <- doseKnots ])
    fit = LM.fitLM xExt y
-- Adaptive Lasso (Phase 31-A1) selects important knots from candidates
```

Once threshold identified: **margin = threshold – device setting center**.

##### C. GP with Non-Standard Kernel

RBF GP too smooth; **Matern 1/2** or **piecewise-constant kernel** better captures thresholds:

```haskell
let kernel = GP.Matern52   -- Less smooth, threshold-sensitive
    gpRes  = GP.fitGP (GP.GPModel kernel ...) ...
-- GP prediction slope |d/dx| large → threshold candidate
```

> Fact: `Hanalyze.Model.GP.Kernel` includes RBF / Matern families (verified in `src/Hanalyze/Model/GP.hs:1`)

##### D. Quadratic + Lack-of-Fit Signal

When §4.0.4 LOF is **significant along dose**, it signals "dose exhibits threshold/saturation beyond quadratic capture." LOF → automatically advance to §7.3.2 analyses (segmented / GP).

#### 7.3.3 Dose / Energy / Tilt Typical Response Patterns (Example)

Ion implant empirical patterns (user's typical case):

| Factor | Expected Response | Recommended Model |
|---|---|---|
| **Dose** vs. Sheet Resistance | Threshold (D_th onward: sharp drop) + Saturation | Hinge + LM, or Matern GP |
| **Dose** vs. Leakage | Central minimum (low dose: junction defect; high dose: crystal damage) | Quadratic RSM + canonical |
| **Energy** vs. Junction Depth | Near-linear (E ↑ → x_j ↑) | LM (first-order only) |
| **Energy** vs. Damage | Quadratic (central minimum or monotone rise) | Quadratic RSM |
| **Tilt** vs. Channeling Suppression | Step-wise (0°: strong; 4–7°: sharp drop; high tilt: rise) | Discrete factor + mean/variance ANOVA |
| **Dose × Energy** | Strong correlation (effective dose) | 2-way interaction essential |
| **Dose × Tilt** | Tilt-level-dependent dose effect offset | 2-way interaction |
| **Energy × Tilt** | Projected range shifts with tilt | 2-way interaction |

**Conclusion**: Dose / Energy **5-level continuous** capturing threshold; Tilt **4-level discrete** (equipment choices); **all 2-way interactions**, minimum. Aligns with §4.0.4 "Custom I-optimal 5+5+4 with 23 runs + 2 centers."

### 7.4 Severe Nonlinearity → GP / GBM / RF Visualization

Quadratic fails; **non-parametric visualization** needed:

```haskell
-- 1D profile: vary factor j, fix others at center
let gpModel = GP.GPModel ...
    gpRes   = GP.fitGP gpModel xs ys hyperparams
-- Partial dependence plot derived from gpRes
```

**RF / GBM partial dependence**:

```haskell
-- Vary factor j over a grid, marginalize others via the train distribution
let pd j = [ avgPredict rf (fixCol j v xTrain) | v <- gridOf j ]
```

Visualization via (Python matplotlib integration or `Hanalyze.Viz.*`):
- `Hanalyze.Viz.GP` — GP profile / uncertainty bands
- `Hanalyze.Viz.Pareto` — multi-objective Pareto front

**Related Code**: `src/Hanalyze/Model/GP.hs:1`, `src/Hanalyze/Model/GradientBoosting.hs:1`, `src/Hanalyze/Viz/GP.hs:1`

### 7.5 Interaction Discovery

DoE analysis uncovering **interaction effects**:

1. **Fit Model + 2-way terms** → Lasso selects principal interactions
   - `Hanalyze.Model.Lasso` (Phase 13/31)
2. **ANOVA 2-way F-test**
   - `Hanalyze.Design.Anova` (Phase 12)
3. **Interaction plot** (visualize) → confirm physical meaning
   - `Hanalyze.Viz.*`

**Adaptive Lasso (Phase 31-A1)** unbiased selection of weak 2-way terms, effective for sparse interaction structure.

### 7.6 Response-Space Causal Discovery (LiNGAM) + DoE Integration

Beyond factor → response (DoE), often **response → response causality** interests: does dark current degradation directly delete fwc, or via defect → leakage → fwc pathway?

Library **`Hanalyze.Model.LiNGAM.*`** + **`Hanalyze.Model.DAG`** learn causal DAG from observational data. Distinct from Phase 30 (causal inference with known DAG), LiNGAM **discovers structure**.

#### 7.6.1 LiNGAM Variant Selection

| Variant | When to Use |
|---|---|
| **DirectLiNGAM** | First choice. No ICA, stable, medium-scale (p ≤ 20) |
| **Bootstrap** | Edge confidence desired (agreement rate + frequency) |
| **Pairwise** | Directional test between 2 variables (lightweight) |
| **ICA-LiNGAM** | Large p, parallelizable (Shimizu 2006 original) |
| **VAR-LiNGAM** | Time series (in-line monitor evolution) |
| **MultiGroup** | Shared structure across factory / equipment / generation |
| **PARCE** | Suspected latent confounding (unobserved variable effects) |

#### 7.6.2 Applicability Conditions (Critical)

LiNGAM assumptions:

- **Linear**: X = B X + e (response linearly dependent on others)
- **Non-Gaussian**: noise e components non-Gaussian
- **Acyclic**: No causal cycles (DAG structure)
- **Independent Noise**: No inter-noise correlation (absence of latent confounding)

Perfect Gaussian responses → ΔMI ≈ 0, order unidentifiable. **Semiconductor responses** (defect count, yield, log dark current) typically **non-Gaussian → applicable**.

#### 7.6.3 Count Data (Defect, Thousands Scale) + LiNGAM

Poisson-like defect counts inputted directly risk:

- **Linearity broken**: defect ~ exp(linear) typical; **log-transform** aligns with LiNGAM linearity
- **Huge values** (thousands): Central limit → approximate Gaussian → identification weakens. **Log + standardize** safer
- **Over-dispersion**: Variance > mean; LiNGAM uses OLS residuals, so over-dispersion affects B values only, not order

Practical:

```haskell
-- Assemble response matrix; log-transform defect count
let respMat = LA.fromColumns
      [ LA.cmap log (LA.fromList (map fromIntegral defects))  -- log(defect)
      , yFwc                                                  -- linear
      , LA.cmap log darks                                     -- log(dark)
      ]
    fit = LNG.fitDirectLiNGAM LNG.defaultDirectLiNGAMConfig respMat
```

#### 7.6.4 DoE Factors + LiNGAM Causal Discovery Integration Pattern

DoE (factors → responses) and LiNGAM (responses → responses) yield **orthogonal information**. Combined:

1. **DoE**: dose × tilt → defect strong (main + interaction)
2. **LiNGAM**: defect → fwc (inter-response causality)
3. **Unified**: dose × tilt's impact reaches fwc **via defect pathway** = dose × tilt **true knobs for fwc control**

Integration visualization: "DoE factors" + "response DAG" in one diagram. Library `DAG.toDOT` exports Graphviz; manual editing expected (future: combined-DAG auto-construction, Phase NN candidate).

#### 7.6.5 Bootstrap Edge Confidence

Single LiNGAM fit has edge volatility (noise + n-dependent). **BootstrapLiNGAM**: B resamples, track appearance frequency + sign agreement:

```haskell
import qualified Hanalyze.Model.LiNGAM.Bootstrap as LBoot

let cfg = LBoot.defaultBootstrapConfig { LBoot.bcNumBootstraps = 100 }
res <- LBoot.fitBootstrapLiNGAM cfg respMat
let dag = LBoot.confidenceDAG 0.7 0.8 res
       -- Retain edges: frequency ≥ 0.7, sign agreement ≥ 0.8
```

Practical baseline: **frequency 0.7 / agreement 0.8** minimum. Below → weak causality or false positive → discard; adopt above.

#### 7.6.6 DAG Visualization + Graphviz

`DAG.toDOT` converts to Graphviz DOT:

```haskell
import qualified Data.Text.IO as TIO
import qualified Hanalyze.Model.DAG as DAG

let dag = LNG.dlDAG cfg fit
    dagWithLabels = DAG.withNames (V.fromList ["defect", "fwc", "log_dark"]) dag
TIO.writeFile "dag.dot" (DAG.toDOT dagWithLabels)
```

Shell:

```
$ dot -Tpng dag.dot -o dag.png
```

or:

```
$ dot -Tsvg dag.dot -o dag.svg
```

**Implementation Demo**: `cabal run cis-implant-workflow-demo` runs this manual's typical case (3 responses + 3 factors), demonstrating §6 LiNGAM causal search, §7 DOT export, end-to-end executable.

---

## 8. Multi-Objective Optimization

Multiple responses (yield / drive current / leakage / reliability) all meeting spec, **maximizing overall score**. Library offers 2 approaches:

### 8.1 Desirability Function (Derringer-Suich)

Define desirability d ∈ [0, 1] per response (0 = unacceptable, 1 = ideal):

- **Target type** (aim value): d = 1 at target, 0 at LSL/USL, linear decay
- **Maximize** (higher better): d = 0 below LSL, d = 1 above target
- **Minimize** (lower better): d = 0 above USL, d = 1 below target

Composite score (geometric mean) D = (d₁ · d₂ · … · d_m)^(1/m) (one zero → all zero; balanced emphasis):

```haskell
import qualified Hanalyze.Optim.Desirability as Des

let types = [ Des.Maximize 100  200       -- Drive current: LSL=100, target=200
            , Des.Minimize 1e-9 1e-12    -- Leakage: USL=1e-9, target=1e-12
            , Des.Target   1.0  0.5 1.5   -- Vth: target=1.0, spec=0.5–1.5
            ]
    score = Des.overallDesirability types [drainI, leak, vth]
-- Maximize score over factor space (BO or Optimal)
```

**Related Code**: `src/Hanalyze/Optim/Desirability.hs:1`

### 8.2 Pareto Front + Hypervolume

**Skip desirability; see all trade-offs** via Pareto optimality (non-dominated solutions):

```haskell
import qualified Hanalyze.Optim.Pareto as Pa

-- candidates :: [[Double]] (response vectors per sample)
let front = Pa.paretoFront candidates    -- Keep non-dominated only
    hv    = Pa.hypervolume refPoint front  -- Aggregate quality in 1 number
```

**Usage**:

- Sim: generate 100 candidates → surrogate evaluate → Pareto front visualize
- User selects **preferred trade-off** (domain knowledge final choice)
- BO multi-objective: `bayesOptScalarMO` maximizes hypervolume

**Related Code**: `src/Hanalyze/Optim/Pareto.hs:1`, `src/Hanalyze/Viz/Pareto.hs:1`

### 8.3 Validation Experiment Planning

Before adopting optimization result:

1. **3–5 additional experiments near optimum** (center 1 + edge 2–4) planned
2. Verify responses match prediction; major deviation → model misspecification suspected
3. **Robustness check**: do spec limits hold under noise variation? (§6.3)
4. Final condition **2–3 lot repeats** confirm reproducibility

---

---

## 9. Best Practice Checklist

### 9.1 Before Phase A (Sim-Driven Design) Initiation

- [ ] Requirements (LSL / USL / Target) documented for all responses
- [ ] Factor types (Continuous / Discrete / Categorical) finalized
- [ ] Factor exploration ranges (lo, hi) determined from equipment spec / experience
- [ ] Single Sim point runtime measured (surrogate necessity gauge)
- [ ] Noise factors (temperature, lot, etc.) separated from control factors

### 9.2 Phase A → B Transition

- [ ] Surrogate hold-out RMSE within tolerance (response scale 5% below target rule)
- [ ] Surrogate optimum **not boundary-pinned** (if pinned, widen range)
- [ ] Outstanding issues list documented (Sim-unverifiable risks)

### 9.3 Before Phase B (Real Device DoE) Initiation

- [ ] **DoE selected from §4.0 options A/B/C** (mandatory manual step)
- [ ] Forced center 2 run timing (start / end / mid) coordinated with equipment
- [ ] In-lot **run randomization** vs. drift axis decision made
- [ ] Response measurement standard (die location, point average) finalized

### 9.4 Phase B → C Transition

- [ ] Effect decomposition (main / interaction / quadratic) significance + physical meaning confirmed
- [ ] **Linear assumption integrity** checked via residual plot + lack-of-fit (if violated, §7.3 RSM / §7.4 nonlinear)
- [ ] Surrogate vs. real **prediction discrepancy factors** identified
- [ ] Augment plan (axial? additional lot?) with explicit goals documented

---

## 10. Pitfall Collection

### 10.1 Backsliding to OFAT

Temptation: "DoE cumbersome; single-factor easier to interpret." **OFAT misses optima when interactions are strong** (Box-Hunter-Hunter 1978, proven). With 25-run slack, **simultaneously vary all factors** far more efficient.

### 10.2 Linear Regression Trap

Default "fit LM and done":

- **Binomial yield** via LM → predictions escape [0, 1], funnel-shaped residuals, precision collapse
- **Quadratic extrema** (e.g., current vs. Lg) invisible to LM
- **Threshold margin** via LM pulls coefficients toward pre-threshold region, degrading threshold estimate

Solution: **Always compare GLM + RSM** (§7). 3–5× RMSE improvement common.

### 10.3 Surrogate Over-Trust

Surrogate extrapolation fails:

- RFF Ridge → 0 boundary (kernel property)
- RandomForest → train min/max clipping
- GP → ballooning prediction variance (signal unreliability); **never ignore variance**

Always examine **uncertainty estimates** (§5.3 BO).

### 10.4 DSD `dsdHasOptimal = False` Overlooked

Library `dsdDesign k` **verified only for k=4** (Jones-Nachtsheim 2011 Table 1). k≥5 via Hadamard-like approximation; orthogonality approximate, not strict. **Using k=11**: recommend **augment + separate lot** re-verification of effect significance.

**Code Basis**: `src/Hanalyze/Design/DSD.hs:48-53`

### 10.5 Forced Center Placement Clustering

Both center runs "together at lot start" → drift **concentrates on centers only**, corrupting center estimate. **Spread across timeline** (start + end, or start + mid + end for 3 centers) to diffuse drift.

### 10.6 Conjecture-Based Progression

"This should work" / "LM probably fine" absent measurement. Common disasters later. Per CLAUDE.md: **"Measure, not guess."** Surrogate selection / model choice / DoE type → always grounded in **CV / hold-out / LOF metrics**.

---

## Appendix A: Use Case → Library Function Quick Reference

| Task | Library Function / Module | Corresponding Phase |
|---|---|---|
| 25 runs / 2 forced center multi-factor design | `Hanalyze.Design.Custom.*` + `augmentAddCenter` | Phase 22–26 |
| k=11 factor screening (3-level, 2k+1 runs) | `Hanalyze.Design.DSD.dsdDesign` | (existing) |
| L9 / L18 orthogonal array | `Hanalyze.Design.Orthogonal.{l9,l18,lookupOA}` | (existing) |
| Halton / Sobol / LHS sample | `Hanalyze.Stat.QuasiRandom`, `Hanalyze.Design.SpaceFilling` | (existing) |
| I-optimal / D-optimal search | `Hanalyze.Design.Optimal` | Phase 14 |
| Augment (Replicate/AddCenter/AddAxial/AddRuns/Foldover) | `Hanalyze.Design.Custom.Augment` | Phase 25–28 |
| Continuous response LM / RFF Ridge | `Hanalyze.Model.{LM,RFFRidge}` | Phase 13, 17 |
| Count / ratio response GLM | `Hanalyze.Model.GLM` | Phase 13 |
| Binary / reliability test AFT | `Hanalyze.Model.{GLM, AFT}` (Logit/Weibull) | Phase 13, 12 |
| Quadratic surface (RSM) | `Hanalyze.Design.RSM`, `Hanalyze.Design.MultiRSM` | (existing) |
| RandomForest / GBM surrogate | `Hanalyze.Model.{RandomForestRegressor, GradientBoosting}` | Phase 17, 34 |
| ANOVA / Fit Y by X | `Hanalyze.Design.Anova`, `Hanalyze.Stat.Test.*` | Phase 12–13 |
| Robust regression (outlier-tolerant) | `Hanalyze.Model.Robust` | Phase 31 |
| Bayesian Optimization | `Hanalyze.Optim.BayesOpt` | (verify) |
| Causal discovery (LiNGAM family) | (Phase NN doc drafted, unimplemented) | Unstarted |

> ※ "Verify" items: implementation existence unconfirmed via grep at Phase 1 manual drafting. Confirmed in Phase 2 draft.

---

## Appendix B: Sample Code (End-to-End)

Hypothetical: **4 factors (implant dose, anneal temperature, gate length Lg, thickness tox), 3 responses (drive current Id, leakage Ioff, Vth)** executing Sim → real device → analysis → optimization.

> ⚠ Design template illustrating API coherence; **execution / runtime verification pending**. Compiled operation + numerical validation planned for next session in `examples/` deployment.

### B.1 Factor Definition

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Examples.SemiWorkflow where

import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector          as V
import qualified Data.Text            as T

import qualified Hanalyze.Design.Custom.Factor      as F
import qualified Hanalyze.Design.Custom.Coordinate  as Coord
import qualified Hanalyze.Design.Custom.Augment     as Aug
import qualified Hanalyze.Design.Optimal            as Opt
import qualified Hanalyze.Stat.QuasiRandom          as QR
import qualified Hanalyze.Model.RFF                 as RFF
import qualified Hanalyze.Model.GP                  as GP
import qualified Hanalyze.Model.GLM                 as GLM
import qualified Hanalyze.Model.LM                  as LM
import qualified Hanalyze.Design.RSM                as RSM
import qualified Hanalyze.Optim.BayesOpt            as BO
import qualified Hanalyze.Optim.Desirability        as Des
import qualified Hanalyze.Optim.Pareto              as Pa

-- 4 continuous factors (raw units)
factors :: [F.Factor]
factors =
  [ F.continuousFactor "imp_dose"  1e13  5e13
  , F.continuousFactor "anneal_T"  900   1100
  , F.continuousFactor "Lg_nm"     28    36
  , F.continuousFactor "tox_nm"    1.5   3.0
  ]
```

### B.2 Phase A: Sim Sample + Surrogate

```haskell
-- Halton: 50 points, 4D even distribution
haltonSamples :: Int -> [[Double]]
haltonSamples n =
  let primes = [2, 3, 5, 7]
  in [ [ QR.radicalInverse (primes !! d) i | d <- [0..3] ]
     | i <- [1..n] ]

-- Convert coded [0,1]^4 to raw units
toRaw :: [F.Factor] -> [Double] -> [Double]
toRaw fs cs = zipWith codedToRaw fs cs
  where
    codedToRaw f c =
      let (lo, hi) = F.factorRange f  -- hypothetical helper
      in lo + c * (hi - lo)

-- Sim execution (placeholder)
runSim :: [Double] -> IO (Double, Double, Double)
                                -- (Id, Ioff, Vth) returned
runSim = error "TODO: Sim binding"

-- 1) 50-point Sim
phaseA_sim :: IO ([[Double]], [(Double, Double, Double)])
phaseA_sim = do
  let xsCoded = haltonSamples 50
      xsRaw   = map (toRaw factors) xsCoded
  ys <- mapM runSim xsRaw
  pure (xsCoded, ys)

-- 2) GP surrogate fit to Id response
phaseA_surrogate :: [[Double]] -> [Double] -> GP.GPResult
phaseA_surrogate xsCoded yId =
  let xMat   = LA.fromLists xsCoded
      yVec   = LA.fromList yId
      hyper0 = GP.initParamsFromDataMV xMat yVec
      kernel = GP.RBF  -- placeholder: RBF kernel
      hyper  = GP.optimizeGP kernel (concat xsCoded) yId hyper0
      gpRes  = GP.fitGP (GP.GPModel kernel hyper) (concat xsCoded) yId
                        (concat xsCoded)  -- LOO predict at training
  in gpRes
```

### B.3 Phase B: Real Device I-optimal DoE (23 + 2 Centers)

```haskell
-- Custom Design Spec: 23 runs, model = main + 2-way + quadratic
phaseB_design :: IO (LA.Matrix Double)
phaseB_design = do
  let model = Coord.mainEffects <> Coord.twoWayInteractions <> Coord.pureQuadratic
      spec0 = Coord.defaultCustomDesignSpec
                { Coord.cdsFactors   = factors
                , Coord.cdsModel     = model
                , Coord.cdsRuns      = 23
                , Coord.cdsCriterion = Coord.IOptimal
                }
  ebase <- Coord.runCustomDesignBuild spec0  -- :: IO (Either Text Matrix)
  case ebase of
    Left err   -> error (T.unpack err)
    Right base -> do
      -- AddCenter 2 → 25 rows
      case Aug.augmentAddCenter factors base 2 of
        Left err  -> error (T.unpack err)
        Right res -> pure (Aug.amrMatrix res)
```

### B.4 Real Device Analysis: Multi-Response Parallel Fit

```haskell
-- Execute 25 runs, measure 3 responses (CSV assumed)
type Lot = LA.Matrix Double  -- 25 × 4 (factors)
type Resp = (LA.Vector Double, LA.Vector Double, LA.Vector Double)
                                -- (Id, Ioff, Vth) length 25 each

analyze :: Lot -> Resp -> IO ()
analyze x (yId, yIoff, yVth) = do
  -- 1) Id (continuous): quadratic RSM
  let qFit = RSM.fitQuadratic (LA.toLists x) (LA.toList yId)
      (xStarId, yStarId, eigvalsId) = RSM.optimumPoint qFit
  putStrLn $ "Id extremum: " ++ show xStarId ++ " → " ++ show yStarId
  putStrLn $ "  eigenvalues: " ++ show eigvalsId

  -- 2) Ioff (count-like, log-scale): Poisson GLM (or LM on log response)
  let glmFit = GLM.fitGLM GLM.Poisson GLM.LogLink x (LA.cmap round yIoff)
  putStrLn $ "Ioff Poisson coefficients: " ++ show (GLM.glmCoef glmFit)

  -- 3) Vth (continuous, ±range): LM + residuals check LOF
  let lmFit = LM.fitLM x yVth
  putStrLn $ "Vth LM coefficients: " ++ show (LM.lmCoef lmFit)
```

### B.5 Multi-Objective Optimization (Desirability)

```haskell
-- Spec: Id ≥ 100 target 200 / Ioff ≤ 1e-9 target 1e-12 / Vth = 1.0 ± 0.5
desirabilityScore :: (Double, Double, Double) -> Double
desirabilityScore (id_, ioff, vth) =
  let types = [ Des.Maximize 100  200
              , Des.Minimize 1e-9 1e-12
              , Des.Target   1.0  0.5 1.5
              ]
  in Des.overallDesirability types [id_, ioff, vth]

-- Desirability-max via BO on surrogate (Sim-accelerated)
phaseB_optimize :: IO [Double]
phaseB_optimize = do
  let cfg = BO.defaultBayesOptConfig
      evalScore :: [Double] -> IO Double
      evalScore xCoded = do
        let xRaw = toRaw factors xCoded
        -- Typically: surrogate predict, no Sim call
        (id_, ioff, vth) <- predictFromSurrogate xRaw
        pure (desirabilityScore (id_, ioff, vth))
  res <- BO.bayesOptND cfg evalScore
  pure (BO.boBestX res)  -- optimal condition (coded)
  where
    predictFromSurrogate = error "TODO: 3-channel surrogate predict"
```

### B.6 Pareto Front Visualization (Human-Assisted Selection)

```haskell
-- 1000 candidate Halton → surrogate predict → Pareto front
phaseB_pareto :: IO [[Double]]
phaseB_pareto = do
  let candidates_coded = haltonSamples 1000
      candidates_raw   = map (toRaw factors) candidates_coded
  preds <- mapM predictFromSurrogate candidates_raw  -- [(Id, Ioff, Vth)]
  -- Maximize-direction alignment (Pareto assumes max): negate Ioff, |Vth - 1|
  let objs = [ [id_, -ioff, -(abs (vth - 1.0))] | (id_, ioff, vth) <- preds ]
      front = Pa.paretoFront objs
  pure front  -- Visualization deferred (use Hanalyze.Viz.Pareto)
  where
    predictFromSurrogate = error "TODO"
```

---

---

## Revision History

- 2026-05-30 v0.1: Phase 1 (skeleton + chapters 1–4, 9–10, Appendix A) created
  - §4.0 specifies 3 selection options (A/B/C) for 25-run / 2-forced-center constraint
- 2026-05-30 v0.4: Phase 4 LiNGAM causal integration (§7.6 new, user-requested)
  - §7.6.1–7.6.6: 7 LiNGAM variants, applicability conditions, count-data handling (log + standardize), DoE × LiNGAM combined pattern, Bootstrap edge confidence, DAG Graphviz export
  - Aligned with cis-implant-workflow-demo §6 §7 (end-to-end executable reference)
- 2026-05-30 v0.3: Phase 3 deep dives (A+B+D+E, user-requested 2026-05-30)
  - §3.1.1: discrete/categorical coding schemes (Reference/Sum/Polynomial/Ordinal numeric), level count → quadratic estimability, DiscreteNum → Custom Design protocol (pitfall: Continuous auto-discrete → fails)
  - §4.0.4: k=3 few + 25-run slack strategy (5+5+4 levels, 23 runs + 2 centers, 13 df spare → LOF + pure error, Box-Behnken + replicate alternative)
  - §5.1.5: Sim cost-by-cost strategy (~min brute / ~hr BO standard / ~half-day early-stop / ~day multi-fidelity / ~week hands-off)
  - §7.3.1: quadratic extremum / saddle / flat direction engineering 4 patterns
  - §7.3.2: threshold margin capture (segmented / hinge + Adaptive Lasso / Matern GP / LOF signal)
  - §7.3.3: dose/energy/tilt typical response patterns table
- 2026-05-30 v0.2: Phase 2 fill (chapters 5–8, Appendix B template)
  - Chapter 5: surrogate comparison + N₀ = 10k rule + BO cycle + LOO-CV
  - Chapter 6: OFAT → DoE migration protocol + empirical constraint embedding + Robust Design
  - Chapter 7: GLM (Poisson/Logit) / RSM canonical / GP partial dependence / Adaptive Lasso interaction
  - Chapter 8: Desirability + Pareto front + validation planning
  - Appendix B: 4-factor 3-response end-to-end sample (Phase A surrogate → Phase B I-optimal 23+2 → analysis RSM/GLM/LM parallel → Desirability/Pareto)
  - **Appendix B runtime verification + `examples/` deployment pending next session**
