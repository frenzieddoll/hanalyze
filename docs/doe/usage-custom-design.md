# Custom Design (JMP-equivalent) — Usage

> JMP Pro "Custom Design" equivalent that generates **arbitrary model ×
> arbitrary constraint × arbitrary runs** designs in a single call.
> Continuous factors use coordinate exchange (Meyer-Nachtsheim 1995);
> categorical factors use Modified Fedorov; both are unified under a single
> per-column-grid outer loop.
>
> Spec: `specification/spec/hanalyze-doe-custom-design-spec.md` v0.1.1
> Phases: 24-1 through 24-9.

(Japanese reference: [`usage-custom-design.ja.md`](usage-custom-design.ja.md))

## Module map

| Module | Role |
|---|---|
| `Hanalyze.Design.Custom.Factor`     | Factor ADT (Role × Kind orthogonal) |
| `Hanalyze.Design.Custom.Model`      | Model term ADT + `expandDesignMatrix` |
| `Hanalyze.Design.Custom.Constraint` | LinearIneq / Forbidden / Conditional / RangeBound |
| `Hanalyze.Design.Custom.Coordinate` | `coordinateExchange` (multi-start search) |
| `Hanalyze.Design.Custom.Compare`    | `compareDesigns` (D/A/G/I + FDS + alias norm) |
| `Hanalyze.Design.Custom.Power`      | `designPower` (design-matrix power analysis) |

## Categorical raw representation (caveat)

The raw `Matrix Double` is **type-unsafe** for Categorical / Ordinal columns:
levels are stored as **integer index `0..K-1` cast to `Double`**.
`expandDesignMatrix` performs reference (treatment) coding with reference = index 0.
A type-safe redesign (R `model.matrix` / patsy-style separation) is registered
as Phase 27 in `specification/phase-plan.md`.

## Quick example

```haskell
import Hanalyze.Design.Custom.Factor
import Hanalyze.Design.Custom.Model
import Hanalyze.Design.Custom.Coordinate
import Hanalyze.Design.Optimal (OptCriterion (..))

f1 = Factor "x1" (Continuous (-1) 1) Controllable
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
  , cdsBudget      = defaultBudget
  , cdsSeed        = Just 42
  , cdsInitial     = Nothing
  }

main = do
  Right cd <- coordinateExchange spec
  print (cdMatrix cd)
```

## Known limitations (Phase 24)

- I-efficiency uses self-moment approximation (region-integral version: future)
- Alias matrix considers only continuous × continuous 2fi
- FDS region assumes independent uniform factors; constrained regions need
  rejection sampling
- Split-plot (Hard-to-Change): Phase 25
- Augment menu (5 modes): Phase 25
- Bayesian-D (DuMouchel-Jones): Phase 26
- `cdsInitial` / `TNested` / `TCustom` are not yet supported

For full Japanese tutorial including examples: see `usage-custom-design.ja.md`.
