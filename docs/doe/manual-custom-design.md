# Custom Design Feature Manual (Phases 23-28 Unified)

> 🌐 **English** | [日本語](manual-custom-design.ja.md)

> Complete manual for **JMP Pro "Custom Design" equivalent features** added on
> 2026-05-29. Bridges use cases to API signatures, covering "what became possible,"
> "when to use it," and "how to write code."
>
> Detailed API references for each feature are listed at the end under
> "Detailed doc index." Classic DoE features (Factorial / Block / ANOVA / Power /
> standard RSM / orthogonal arrays / Taguchi) are documented in
> [01-doe.md](01-doe.md).

---

## 0. Overview: What Became Possible

As of hanalyze 0.1.0.1 (2026-05-29), the following can be handled by **a single API**
(equivalent to JMP "Custom Design" platform):

| Domain | Main API | Capability |
|---|---|---|
| Arbitrary model × arbitrary runs optimal design | `coordinateExchange` | continuous × discrete × categorical mixed, no candidate set needed |
| Nested factors | `TNested A B` (Phase 28-1) | A within B (both Categorical/Ordinal), K_B × (K_A-1) expanded columns |
| Constrained design | LinearIneq / Forbidden / Conditional / RangeBound | half-space / exact-match-ban / if-then / range override in **same ADT** |
| Hard-to-Change factors | `generateSplitPlot` | split-plot experiment for high-cost factors (REML D-opt), Categorical WP supported (Phase 28-3) |
| Strip-plot (Phase 28-2) | `generateSplitPlot` + `spcNStrip=Just n` | `VeryHardToChange` with 2 strata, M⁻¹ via buildMInvStrip |
| Existing design augmentation | `augmentMenu` | Replicate / center / axial (raw-unit matched, Phase 28-10) / additional runs / Foldover (level-swap enabled, Phase 28-7) |
| Bayesian D-optimality with prior info | `BayesianD` + `cdsDJConvention=True` (Phase 28-12) | DuMouchel-Jones K addition + paper §2.2 convention auto-applied |
| I-optimal (region exact) | `IOpt` (Phase 28-4) | analytic integral + constraint-aware MC fallback (`regionMomentMatrixMC`) |
| Design comparison | `compareDesigns` / `compareDesignsWithResponses` (Phase 28-8) | D/A/G/I efficiency (BayesianD-aware D column, Phase 28-5) + FDS + alias norm (cat 2fi + TPower extended, Phase 28-6) + MCp / MCpk |
| Compound criterion | `Compound` + `compoundGeometric` (Phase 28-9) | linear + geometric mean + `dEfficiency` / `aEfficiency` helpers |
| Power per term | `designPower` | direct power calculation from noncentrality λ of each model term |
| Constraint-aware candidate reduction | `Hanalyze.Design.Constraint.filterCandidates` | pre-filter for classic Fedorov |
| Non-normal process capability | `processCapabilityGamma` / `NonNormalFit` | right-skewed data Cp + AIC auto-select |
| Multivariate process capability | `processCapabilityMultivariate` | Mahalanobis-based MCp / MCpk |

**Implementation status** (2026-05-29, Phase 28 complete):

- **583 tests pass** (+24 in Phase 28), all commits tagged `(hanalyze-portable)`
- **5/5 bench metrics pass**:
  - Jones-Goos (2012) Table 2 Split-Plot D-opt: ratio **1.0000 exact match**
  - DuMouchel-Jones (1994) Example 3 "Both" Bayesian-D: ratio **1.0000 exact match** (DJ §2.2 auto-applied)
  - JMP RSM Constraints + Categorical 18-run I-opt: ratio **0.9791** (MC region: hanalyze 2.1% improvement)
- Awaiting literature access: Meyer-Nachtsheim (1995) only (Phase 27-4 deferred, paywall)
- Cherry-pickable to upstream hanalyze

---

## 1. Use Case → Feature Mapping

Quick lookup: "I want to do X" → "call which API." Details in linked docs.

### A. Create a design

| Goal | Feature | Details |
|---|---|---|
| Continuous 2-3 factor + quadratic model (RSM equivalent) | `coordinateExchange` + `TIntercept/TMain/TInter/TPower` | [usage-custom-design](usage-custom-design.md) |
| Continuous + categorical mixed | Same (`Categorical` factor added) | Same |
| Linear constraint (x1+x2 ≤ 1) | `LinearIneq` | Same §3 |
| Forbidden combination (cat=A and x1=1 etc.) | `Forbidden` | Same §3 |
| "Temperature upper limit only when cat=A" | `Conditional + GuardEq` | Same §3 |
| Hard-to-Change factor for split-plot | `generateSplitPlot` | [usage-augment-splitplot](usage-augment-splitplot.md) |
| Add center point / axial to existing design | `augmentMenu (AddCenter\|AddAxial)` | Same §1 |
| Add optimal additional runs to existing design | `augmentMenu (AddRuns n)` | Same §1 |
| Foldover existing design with sign flip | `augmentMenu (Foldover ...)` | Same §1 |
| Quadratic term with weak prior in D-opt | `BayesianD + priorPrecisionDefault` | [usage-bayesian-d](usage-bayesian-d.md) |
| D and I weighted 7:3 composite | `Compound [(0.7, DOpt), (0.3, IOpt)]` | Same §3 |

### B. Evaluate a design

| Goal | Feature | Details |
|---|---|---|
| D/A/G/I efficiency comparison across designs | `compareDesigns.dcEffTable` | [usage-custom-design](usage-custom-design.md) §5 |
| FDS plot data (prediction variance distribution) | `compareDesigns.dcFDS` | Same |
| Alias matrix Frobenius norm | `compareDesigns.dcAliasNorm` | Same |
| Power per term (n + effect size needed) | `designPower` | Same §5 |
| VIF / single design efficiency | `Hanalyze.Design.Diagnostics.diagnostics` | [01-doe](01-doe.md) §5 |
| G-optimal (minimize max leverage) | `OptCriterion = GOpt` | [usage-classic-extensions](usage-classic-extensions.md) §1 |

### C. Analyze observed y (post-hoc)

| Goal | Feature | Details |
|---|---|---|
| Multivariate normal y process capability | `processCapabilityMultivariate` | [usage-classic-extensions](usage-classic-extensions.md) §4 |
| Right-skewed (Gamma) Cp | `processCapabilityGamma` | Same §3 |
| Process capability with unknown distribution (Box-Cox / Johnson SU / Gamma auto-select) | `NonNormalFit` based | Same §3 |
| Custom Design fit with y via linear model | `Hanalyze.Model.LM` (existing) | [01-doe](01-doe.md) §3 |

---

## 2. Minimal Workflow Examples (3 Types)

### Example 1: Continuous 2-factor RSM via Custom Design

```haskell
import Hanalyze.Design.Custom.Factor
import Hanalyze.Design.Custom.Model
import Hanalyze.Design.Custom.Coordinate
import Hanalyze.Design.Optimal (OptCriterion (..))

main :: IO ()
main = do
  let f1 = Factor "x1" (Continuous (-1) 1) Controllable
      f2 = Factor "x2" (Continuous (-1) 1) Controllable
      model = Model
        [ TIntercept
        , TMain "x1", TMain "x2"
        , TInter ["x1","x2"]
        , TPower "x1" 2, TPower "x2" 2
        ] NCoded
      spec = CustomDesignSpec
        { cdsFactors     = [f1, f2]
        , cdsModel       = model
        , cdsConstraints = []
        , cdsNRuns       = 12
        , cdsCriterion   = DOpt
        , cdsBudget      = defaultBudget       -- 21 grid, 5 restart, maxIter 50
        , cdsSeed        = Just 42
        , cdsInitial     = Nothing
        }
  Right cd <- coordinateExchange spec
  print (cdMatrix cd)
```

### Example 2: Constrained design + comparison + power

```haskell
import qualified Hanalyze.Design.Custom.Constraint as CC
import qualified Hanalyze.Design.Custom.Compare    as Cmp
import qualified Hanalyze.Design.Custom.Power      as Pwr

main = do
  -- Constrained design
  let spec2 = spec
        { cdsConstraints = [CC.LinearIneq [("x1",1),("x2",1)] CC.CLeq 1] }
  Right cdConstrained <- coordinateExchange spec2

  -- Compare vs. unconstrained
  Right cdFree <- coordinateExchange spec
  let comp = Cmp.compareDesigns
        [("free", cdFree), ("constrained", cdConstrained)]
  print (Cmp.dcEffTable  comp)     -- D/A/G/I efficiency per design
  print (Cmp.dcAliasNorm comp)     -- alias norm
  -- print (Cmp.dcFDS comp)        -- prediction variance sorted vector per design (FDS plot)

  -- Power (sigma=1.0, effect 0.5)
  let powers = Pwr.designPower cdConstrained 1.0
        [("x1", 0.5), ("x1:x2", 0.3)] 0.05
  mapM_ print powers
```

### Example 3: Split-plot with Hard-to-Change factor (temperature) as Whole-Plot

```haskell
import qualified Hanalyze.Design.Custom.SplitPlot as SP

main = do
  let fTemp = Factor "temp" (Continuous 100 200) HardToChange
      fRate = Factor "rate" (Continuous   0   1) Controllable
      modelSP = Model
        [TIntercept, TMain "temp", TMain "rate"
        , TInter ["temp","rate"]] NCoded
      spec = CustomDesignSpec
        { cdsFactors = [fTemp, fRate]
        , cdsModel   = modelSP
        , cdsConstraints = []
        , cdsNRuns   = 12
        , cdsCriterion = DOpt
        , cdsBudget = defaultBudget
        , cdsSeed   = Just 100
        , cdsInitial = Nothing
        }
      cfg = SP.defaultSplitPlotConfig 4    -- 4 WP × 3 runs = 12
  Right spd <- SP.generateSplitPlot spec cfg
  print (SP.spdMatrix      spd)
  print (SP.spdWholePlotId spd)   -- [0,0,0,1,1,1,2,2,2,3,3,3]
  -- Guarantees temp is constant within each WP (setup cost = 4)
```

---

## 3. Critical Design Assumptions (Verify Before Use)

### A. Raw matrix categorical encoding is **type-unsafe** (option α)

Categorical / Ordinal columns in `Matrix Double` hold **level index 0..K-1 as Double**.
`expandDesignMatrix` applies reference (treatment) coding to K-1 columns (reference = index 0).

- ✅ Arithmetic fast, high hmatrix affinity
- ❌ Cannot prevent 0.5 or out-of-range indices at type level (runtime `Left` on check)
- Future **type-safe redesign (option β, R `model.matrix` style)** recorded as Phase 27
  candidate in phase-plan (trigger: canvas API schema finalized or incident)

### B. Internal grid is coded space `[-1, 1]`

Grid for `Continuous lo hi` **ignores lo / hi, uses `[-1, 1] linspace`**.
If raw-unit design is needed, rescale post-generation on caller side:

```haskell
let raw = cdMatrix cd
    rescaled = LA.fromColumns
      [ scaleColumn (factors !! j) (LA.flatten (raw LA.? [j]))
      | j <- [0 .. nF - 1] ]
```

### C. All functions return **IO (Either Text ...)**

Random search + constraint-aware rejection sampling requires `IO`. Failures are **values,
not exceptions (`Left Text`)**. Always pattern-match:

```haskell
r <- coordinateExchange spec
case r of
  Left  e  -> putStrLn ("Failure: " <> T.unpack e)
  Right cd -> ...
```

### D. `cdsBudget` Tuning Guidelines

| Parameter | Default | Increase | Decrease |
|---|---|---|---|
| `dbCxStepGrid` | 21 | Resolution up, compute high | Resolution down, compute fast |
| `dbRestarts`   | 5  | Near global optimum, compute high | Risk of local optimum |
| `dbMaxIter`    | 50 | Strong convergence, compute high | Risk of incomplete convergence |
| `dbTol`        | 1e-6 | Early termination | Strict convergence |

Default `defaultBudget` suffices in practice. For high dimension (≥ 5 factors),
increase `dbRestarts` to 10+ for better global search.

---

## 4. Detailed Doc Index

Feature-specific details (API signatures, limitations, sample code):

| Scope | Detailed doc |
|---|---|
| Classic extensions (G-opt, Compound, Constraint, non-normal Cp, multivariate Cp) | [usage-classic-extensions.md](usage-classic-extensions.md) |
| Custom Design Core (Factor / Model / Constraint / Coordinate / Compare / Power) | [usage-custom-design.md](usage-custom-design.md) |
| Split-Plot + Augment 5 menus | [usage-augment-splitplot.md](usage-augment-splitplot.md) |
| Bayesian-D + Compound enhancement | [usage-bayesian-d.md](usage-bayesian-d.md) |
| (Reference) Classic DoE features overall | [01-doe.md](01-doe.md) |
| (Reference) DoE theory | [theory-doe.md](theory-doe.md) |

Developer notes:

- `docs/dev-notes/upstream-hmatrix-accum.md` — hmatrix `LA.accum` argument order doc
  ambiguity (upstream issue candidate)
- `specification/spec/hanalyze-doe-custom-design-spec.md` v0.1.1 — specification
- `specification/spec/hanalyze-doe-spec.md` v0.2 — classic extensions spec

---

## 5. Known Limitations Summary (For Morning Review)

Most resolved in Phase 28 (a-z). Remaining limitations listed only:

| Domain | Limitation | Resolution / Note |
|---|---|---|
| Categorical input | Type-unsafe level index convention | Phase 29 (trigger-based, type-separation redesign) |
| SplitPlot REML | Simplified version (chol-based X̃), absolute-value comparison invalid | Goos-Vandebroek strict version, future |
| Strip-plot (Phase 28-2) | Simplified η_WP = η_Strip | Separate η support future |
| Compare FDS | All-factor-independent uniform region | Constrained region via rejection; MC fallback for IOpt only (28-4c) |
| Conditional constraint | AND/OR positive logic only, no NOT | Deferred (NP-hard complexity) |
| TNested (Phase 28-1) | Both A and B must be Categorical/Ordinal | Continuous A within Categorical B future |

**Phase 28 resolved limitations** (reference):

| Former limitation | Resolved commit (Phase) |
|---|---|
| `iValueSelfM` = p/n degeneracy | Phase 28-4a/b (region integral version) |
| Constraint / Mixture IOpt | Phase 28-4c (MC fallback) |
| BayesianD Compare D-eff column | Phase 28-5 (BayesianD-aware) |
| Compare alias Z missing categorical/TPower | Phase 28-6 (Z range extended) |
| Foldover categorical level swap | Phase 28-7 (CategoricalSwap) |
| Multivariate Cp Compare integration | Phase 28-8 (compareDesignsWithResponses) |
| Compound geometric mean | Phase 28-9 (compoundGeometric) |
| AddAxial raw range scaling | Phase 28-10 (rawUnits flag) |
| Strip-plot (VeryHardToChange) | Phase 28-2 (spcNStrip) |
| Categorical Whole-Plot | Phase 28-3 (constraint lifted) |
| DJ §2.2 convention (Bayesian-D) | Phase 28-12 (auto via cdsDJConvention) |
| TNested unsupported | Phase 28-1 (Categorical/Ordinal nesting enabled) |

---

## 6. Troubleshooting

### `Left "no restart produced a design"` appears

→ All restarts failed (all initial solutions infeasible). Constraints too tight.
   Try loosening `cdsConstraints` or increasing `dbRestarts`. If unresolved,
   feasible region may be too small.

### `Left "factor X is categorical/ordinal — treatment coding not implemented"` appears

→ May be skeleton from Phase 24-1. Phase 24-2 now implements it.
   Try clearing build cache: `cabal clean && cabal build`.

### D-efficiency extremely low (< 0.3)

→ Random search stuck in local optimum. Try `dbRestarts = 10-20`.
   If still poor, model may be overspecified vs. n_runs (insufficient degrees of freedom).
   Increase `cdsNRuns` or simplify model.

### `BayesianD` returns `Left "model invalid"`

→ K dimension (p × p) does not match expanded column count.
   Use `priorPrecisionDefault` for auto-alignment; avoid hand-written K matrices.

### SplitPlot WP factor not constant within WP

→ `fRole = HardToChange` may not be set. Check `whichRoleIsWP factors`
   returns non-empty list; else `Left` is triggered.

---

## 7. References

- Meyer & Nachtsheim (1995). "The Coordinate-Exchange Algorithm for
  Constructing Exact Optimal Experimental Designs". *Technometrics* 37:60-69.
  → Foundation for `coordinateExchange`
- Goos & Vandebroek (2003). "D-Optimal Split-Plot Designs". *J Quality Tech* 35:1-15.
  → REML information matrix for `generateSplitPlot`
- DuMouchel & Jones (1994). "A Simple Bayesian Modification of D-Optimal
  Designs to Reduce Dependence on an Assumed Model". *Technometrics* 36:37-47.
  → `BayesianD` + `priorPrecisionDefault`
- Wang, Hubele, Lawrence (2000). "Comparison of Three Multivariate Process
  Capability Indices". *J Quality Tech* 32:263-275.
  → `processCapabilityMultivariate` MCp-style index
