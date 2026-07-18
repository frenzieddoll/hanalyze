# DoE Classical Extensions (Phase 23)

> 🌐 **English** | [日本語](usage-classic-extensions.ja.md)

> **Advanced capabilities equivalent to JMP / Spotfire** added to existing DoE features
> (`Hanalyze.Design.*`). Also prerequisite for Custom Design (Phase 24+).
> Type signatures and minimal examples: [api-guide 09-doe](../api-guide/09-doe.md) is primary reference;
> this document covers **intent and cautions per feature**.
>
> Spec: `specification/spec/hanalyze-doe-spec.md` v0.2
> Phases: 23-a / 23-b / 23-c / 23-d (all 4 commits already merged to develop)

Extensions included:

1. **OptCriterion extension** — G-optimal + Compound (alphabetic)
2. **Constraint module (new)** — Linear / Forbidden pre-filter candidate set
3. **Non-normal Process Capability** — Gamma Cp + unified `NonNormalFit` entry
4. **Multivariate Process Capability** — Mahalanobis MCp / MCpk + InSpecRate

---

## 1. OptCriterion Extension

- **G-optimal**: Minimize max leverage (= minimize worst prediction variance in design).
  Here: **self-G definition** (= max of design's hat diagonal).
  Whole-candidate-space max prediction variance handled in Custom Design.
- **Compound (alphabetic)**: Weighted sum of multiple criteria as single criterion.
  Warning: inner criteria have inconsistent scales (D = determinant, A = trace;
  different units). Convert to efficiency form ([0,1]) before combining for meaningful blend.
  If weight normalization needed, see Phase 26's `Compare.normalizeCompoundWeights`.

---

## 2. Constraint Module (classical Fedorov)

- **Linear inequalities** (continuous factors) pre-reduce candidate set (`x1 + x2 ≤ 1`, etc.)
- **Exact-match forbidden** (categorical-capable) pin-point exclude candidates
- Violation checking via `checkDesign`; filtering via `filterCandidates`

Cautions:
- Linear constraints reference continuous / discrete numeric factors only
  (categorical expressed via Forbidden).
- Custom Design (Phase 24)'s `Design.Custom.Constraint` is separate ADT;
  this module is strictly "candidate-set-based" classical Fedorov.

---

## 3. Non-normal Process Capability

- **Gamma distribution** Cp/Cpk: Process capability for right-skewed data
- **`NonNormalFit` unified dispatch**: When "Box-Cox / Johnson SU / Gamma fit uncertain",
  auto-select best via AIC

Cautions:
- Gamma assumes **positive data** (negative values require shift)
- `NonNormalFit` auto-selection is AIC-based; small samples (< 30) → unstable selection.
  Fix distribution by domain knowledge when safe.

---

## 4. Multivariate Process Capability

Simultaneous process capability across multiple responses (y1, …, yk):
- MCp: Spread index via Mahalanobis distance (Wang-Hubele-Lawrence style)
- MCpk: Index with center-offset penalty (`MCpk ≤ MCp` guaranteed)
- InSpecRate: Probability data fall within spec, empirical ∈ [0, 1]

Cautions:
- Input y assumes **multivariate normal** (severe deviations → different approach needed)
- Spec length = y column count (mismatch → `Left` error)

---

## Related Links

- Type / minimal examples: [api-guide 09-doe](../api-guide/09-doe.md)
- Upstream (full classical DoE): [01-doe.md](01-doe.md)
- Custom Design Core (Phase 24): [usage-custom-design.md](usage-custom-design.md)
- Theory: [theory-doe.md](theory-doe.md)
