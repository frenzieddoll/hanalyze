# Custom Design: Augment + Split-Plot Usage

> 🌐 **English** | [日本語](usage-augment-splitplot.ja.md)

> Assumes Custom Design Core from Phase 24. Covers existing design **augmentation
> (Augment)** and **split-plot (Split-Plot)** for hard-to-change factors. API
> signatures and minimal examples reference [api-guide 09-doe](../api-guide/09-doe.md)
> as primary; here we address **menu semantics, REML information matrix, known limitations**.
>
> Specification: `specification/spec/hanalyze-doe-custom-design-spec.md` v0.1.1
> §2.5 / §2.6 / Related Phases: 25-3 through 25-9 / Prerequisite: Phase 24 complete

## Module Quick Reference

| Module | Role |
|---|---|
| `Design.Custom.Augment`   | `augmentMenu` (Replicate / AddCenter / AddAxial / AddRuns / Foldover) |
| `Design.Custom.SplitPlot` | `generateSplitPlot` (HardToChange factor REML D-opt) |

---

## 1. Augment 5 Menu Semantics

- `Replicate k` — Duplicate existing design k times
- `AddCenter n` — Add n center point rows (all continuous = 0)
- `AddAxial α` — Add ±α axial points across all continuous factors
- `AddRuns N` — Add N rows from candidate set to existing design
- `Foldover kind` — Sign-flip replica. `FullFoldover` flips all continuous factor signs,
  doubling row count. `PartialFoldover ["x1"]` flips only x1.

### Limitations

- `cdsInitial = Nothing` returns **`Left`** (existing design required)
- `AddAxial` **assumes coded space ([-1, 1])**. Caller must rescale for raw-unit α
- `AddRuns` candidate set = continuous ±1 corners + categorical all-level cartesian
  product. High-dimensional factors risk candidate explosion; use carefully

---

## 2. Split-Plot REML Information Matrix

`fRole = HardToChange` factor stays constant within each whole-plot (WP) = setup count.
`SplitPlotConfig { spcNWhole, spcVarRatio=η }`. Goos-Vandebroek (2003) D-opt:

```
I_β = Xᵀ M⁻¹ X,   M = I + η · Z Zᵀ
```

where Z is WP indicator (n × n_WP), η = σ²_WP / σ².
- η = 0 degenerates to standard D-opt
- η → ∞ reduces WP factor weight to near-zero (= unestimable within WP)

Implementation: X̃ = chol(X' M⁻¹ X) evaluated via `critValueM` (DOpt det) is
**simplified version**. Direction matches standard Goos-Vandebroek, but exact
criterion value absolute comparison is invalid (relative comparison only meaningful).
`spdWholePlotId` returns per-run WP membership (e.g., `[0,0,0,1,1,1,...]`).

### Limitations

- **VeryHardToChange (strip-plot) returns `Left`** (future support)
- **Categorical HardToChange returns `Left`** (deferred with GLMM integration)
- spcNWhole **user-specified mandatory** (not inferred; follows spec §2.5 principle)
- spcVarRatio (η) default 1.0 may be inappropriate by domain. Set if known variance
  ratio available

---

## 3. Design Flow (Augment → Split-Plot)

1. Generate standard custom design via `coordinateExchange`
2. If augmentation desired: `cdsInitial = Just (cdMatrix cd)` in `augmentMenu` to add
   center / axial / etc.
3. Subsequent batch: `generateSplitPlot` with HardToChange considered for split-plot layout

---

## Known Limitations (Phase 25 Complete, Spec-Documented)

- Strip-plot (VeryHardToChange) unsupported
- Categorical WP / Categorical strip-plot unsupported
- Foldover categorical factors: indices unchanged (no sign concept)
- Conditional constraint NOT-clause unsupported (AND/OR positive logic only, spec §2.4)

Phase 26 adds Bayesian-D (DuMouchel-Jones) and Compound criterion enhancement
([usage-bayesian-d](usage-bayesian-d.md)).
