# Custom Design Manual (Phase 23-26 integration)

> Comprehensive manual for **JMP Pro "Custom Design" equivalent features**
> added on 2026-05-29 (Phases 23-26).
>
> Japanese reference (primary): [`manual-custom-design.ja.md`](manual-custom-design.ja.md).
>
> Classic DoE features (Factorial, Block, ANOVA, Power, standard RSM, orthogonal
> arrays, Taguchi) are documented in [`01-doe.md`](01-doe.md).

## At a glance

| Domain | Main API | Capability |
|---|---|---|
| Optimal design (any model × any runs) | `coordinateExchange` | continuous × discrete × categorical mixed, no candidate set needed |
| Constraints | LinearIneq / Forbidden / Conditional / RangeBound | half-space / exact-match-ban / if-then / range override in one ADT |
| Hard-to-Change factors | `generateSplitPlot` | split-plot REML D-opt |
| Augment existing design | `augmentMenu` | Replicate / AddCenter / AddAxial / AddRuns / Foldover |
| Bayesian D-opt | `BayesianD` | DuMouchel-Jones K-matrix prior |
| Design comparison | `compareDesigns` | D/A/G/I efficiencies + FDS + alias norm |
| Power per term | `designPower` | noncentrality λ from `(X'X)^{-1}` diagonal |
| Constraint-aware Fedorov | `Hanalyze.Design.Constraint.filterCandidates` | classic candidate-set filter |
| Non-normal Cp | `processCapabilityGamma`, `NonNormalFit` | right-skewed Gamma + AIC auto-fit |
| Multivariate Cp | `processCapabilityMultivariate` | Mahalanobis MCp / MCpk |

559 tests pass; every commit is tagged `(hanalyze-portable)` for upstream
hanalyze cherry-pick.

## Minimal example

```haskell
import Hanalyze.Design.Custom.Factor
import Hanalyze.Design.Custom.Model
import Hanalyze.Design.Custom.Coordinate
import Hanalyze.Design.Optimal (OptCriterion (..))

let f1 = Factor "x1" (Continuous (-1) 1) Controllable
    f2 = Factor "x2" (Continuous (-1) 1) Controllable
    model = Model
      [ TIntercept, TMain "x1", TMain "x2"
      , TInter ["x1","x2"], TPower "x1" 2, TPower "x2" 2
      ] NCoded
    spec = CustomDesignSpec
      { cdsFactors = [f1, f2], cdsModel = model
      , cdsConstraints = [], cdsNRuns = 12
      , cdsCriterion = DOpt, cdsBudget = defaultBudget
      , cdsSeed = Just 42, cdsInitial = Nothing
      }
main = do
  Right cd <- coordinateExchange spec
  print (cdMatrix cd)
```

## Documentation map

| Topic | Detailed doc |
|---|---|
| Classic DoE extensions (Phase 23): G-opt, Compound, Constraint, non-normal Cp, multivariate Cp | [usage-classic-extensions.md](../doe/usage-classic-extensions.ja.md) (ja primary) |
| Custom Design Core (Phase 24) | [usage-custom-design.md](usage-custom-design.md) |
| Split-Plot + Augment (Phase 25) | [usage-augment-splitplot.md](usage-augment-splitplot.md) |
| Bayesian-D (Phase 26) | [usage-bayesian-d.md](usage-bayesian-d.md) |
| Classic DoE | [01-doe.md](01-doe.md) |

## Key assumptions

- **Type-unsafe categorical encoding**: raw `Matrix Double` stores categorical
  level indices as `Double`. `expandDesignMatrix` applies treatment coding.
  Type-safe redesign deferred to Phase 27 (trigger-based).
- **Grid in coded space `[-1, 1]`**: rescale outputs externally if raw units needed.
- **All generators return `IO (Either Text _)`**: failures are values, not exceptions.

## Known limitations

See [`manual-custom-design.ja.md` §5](manual-custom-design.ja.md) for the full
table. Highlights: strip-plot and categorical whole-plot not supported;
Bayesian D-efficiency uses classic D; Foldover does not flip categorical;
Conditional constraints support AND/OR only.

## References

- Meyer & Nachtsheim (1995), *Technometrics* 37:60-69 — coordinate exchange
- Goos & Vandebroek (2003), *J Quality Tech* 35:1-15 — split-plot D-opt
- DuMouchel & Jones (1994), *Technometrics* 36:37-47 — Bayesian D
- Wang, Hubele, Lawrence (2000), *J Quality Tech* 32:263-275 — multivariate Cp
