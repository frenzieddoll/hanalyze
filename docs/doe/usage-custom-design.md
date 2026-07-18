# Custom Design (JMP Equivalent) Usage

> 🌐 **English** | [日本語](usage-custom-design.ja.md)

> JMP Pro "Custom Design" equivalent: generate **arbitrary model × arbitrary
> constraint × arbitrary runs** designs in one function call. Unlike classic
> D-optimal with candidate sets (`Hanalyze.Design.Optimal`), continuous factors
> use coordinate exchange (Meyer-Nachtsheim 1995) and categorical factors use
> Modified Fedorov, unified in a hybrid algorithm. API signatures and minimal
> examples reference [api-guide 09-doe](../api-guide/09-doe.md) as primary; here
> we address **design philosophy, raw representation conventions, known limitations**.
>
> Specification: `specification/spec/hanalyze-doe-custom-design-spec.md` v0.1.1
> Related Phases: 24-1 through 24-9 (Phase 24 complete).

## Module Quick Reference

| Module | Role |
|---|---|
| `Design.Custom.Factor`     | Factor ADT (Role × Kind orthogonal axes) |
| `Design.Custom.Model`      | Model term ADT + `expandDesignMatrix` |
| `Design.Custom.Constraint` | Constraint ADT (LinearIneq / Forbidden / Conditional / RangeBound) |
| `Design.Custom.Coordinate` | `coordinateExchange` core (multi-start search) |
| `Design.Custom.Compare`    | `compareDesigns` (D/A/G/I efficiency + FDS + alias norm) |
| `Design.Custom.Power`      | `designPower` (design-matrix power analysis) |

---

## 1. Factor Role (`FactorRole`)

- `FactorKind` is 5 types: `Continuous lo hi` / `DiscreteNum [Double]` /
  `Mixture lo hi` / `Categorical [Text]` / `Ordinal [Text]`
- `FactorRole` is operational purpose (Controllable / HardToChange / Blocking / etc.).
  In Phase 24, role does not affect design generation (Phase 25 split-plot uses it).

### Categorical Factor Raw Representation Convention (Critical)

**Type-unsafe**: Categorical / Ordinal columns in `Matrix Double` hold **level
index 0..K-1 as Double** (option α). `expandDesignMatrix` applies reference
(treatment) coding to K-1 columns, reference = index 0.
Details and future type-safe redesign (Phase 27 candidate) in spec.

---

## 2. Constraint Integration

`Constraint` integrates into `coordinateExchange` as a per-grid-point filter.
Grid points violating constraints skip criterion evaluation. Random initialization
uses rejection sampling (200 attempts per row max). Categorical TMain expands to
K-1 columns; TInter uses cartesian product.

---

## 3. Evaluation Metrics (`Compare`, `Power`)

- `compareDesigns` `dcEffTable`: D/A/G/I efficiency (4 columns per design),
  `dcFDS`: prediction variance sorted vector (Halton 500 points),
  `dcAliasNorm`: continuous 2fi alias matrix Frobenius norm.
- `designPower`: per-term power from effect size and sigma via noncentral F approximation.

---

## 4. JMP Golden Test Cases

Production-level behavioral pinning in `test/Spec.hs` describe blocks:

- "Custom Design golden ex1: 2 factor 2nd-order RSM"
- "Custom Design golden ex2: 1 cont + 1 cat(3) + main+int model"
- "Custom Design golden ex3: LinearIneq constraint + 2 factor"

Generated with `defaultBudget` + fixed seed; D-efficiency, row count, and
constraint satisfaction verified against pinned values.

---

## Known Limitations (Phase 24 Scope)

- I-efficiency: self-moment approximation (region integral version: future)
- Alias matrix: continuous × continuous 2fi only (categorical absent / TPower
  extension: future)
- FDS region: all-factor-independent uniform; constrained regions need rejection
  sampling
- Split-Plot (Hard-to-Change): Phase 25 ([usage-augment-splitplot](usage-augment-splitplot.md))
- Augment 5 menus: Phase 25
- Bayesian-D (DuMouchel-Jones): Phase 26 ([usage-bayesian-d](usage-bayesian-d.md))
- `cdsInitial` (Augment input) / `TNested` / `TCustom`: unsupported

Input API type-safety enhancement (Categorical level index → type-separated)
registered as Phase 27 candidate in phase-plan.md.
