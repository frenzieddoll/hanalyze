# True multi-output regression (`hanalyze multireg`)

> 🌐 **English** | [日本語](07-multireg.ja.md)

For data with the structure "one scalar input → q output curves" — for example, a
**potential profile observed at 100 z positions for one dose** — `hanalyze multireg`
trains the model from a wide-form CSV in one line and produces an interactive HTML where a
single input slider recomputes all q predictions live in the browser.

## Data format (wide CSV)

| dose | y_z001 | y_z002 | ... | y_z100 |
|---|---|---|---|---|
| 6.0 | 3.16 | 3.05 | ... | 0.01 |
| 6.4 | 3.15 | 3.04 | ... | 0.01 |
| ... | ... | ... | ... | ... |
| 14.0 | 3.12 | 3.01 | ... | 0.00 |

- Column 1 = input (e.g. dose).
- Remaining q columns = outputs (function values on a z grid).

## Basic commands

### Linear multi-output (closed-form OLS)
```
hanalyze multireg data/io/potential_wide.csv dose 'y_z*' \
    --method linear \
    --xaxis 'z [nm]' --xaxis-min 0 --xaxis-max 200 \
    --report trash/multireg_lin.html
```

`B = (XᵀX)⁻¹ XᵀY` solved in a single LAPACK call for all q columns. Under 1 ms for
N=21 doses × 100 outputs.

### RBF kernel-ridge multi-output (LOOCV auto-HP)
```
hanalyze multireg data/io/potential_wide.csv dose 'y_z*' \
    --method kernel-rbf --auto-hp \
    --xaxis 'z [nm]' --xaxis-min 0 --xaxis-max 200 \
    --report trash/multireg_kr.html
# → best h=8.000  λ=2.15e-3  LOO MSE=1.16e-2  RMSE=0.091
```

`α = (K + λI)⁻¹ Y` computes α for all q columns at once. LOOCV uses the diagonal of the
hat matrix once and reuses it across all q outputs, so total work is one O(n³) Cholesky
plus the grid evaluations.

## yspec syntax

| Form | Meaning |
|---|---|
| `'y_z*'` | All columns starting with `y_z` (quote in the shell). |
| `y_z001,y_z002,y_z003` | Comma-separated explicit list. |

## Output HTML structure

A `Hanalyze.Viz.ReportBuilder.SecInteractiveMultiOut` section is embedded:

- **Input slider** (e.g. dose 6–14): a single slider.
- **Predicted curve** (red): JS recomputes all q outputs at the slider value and renders via Vega-Lite.
- **Observed points** (coloured scatter): each observed dose's `y_z*` overlaid on the z axis.

## Internal stack

| Layer | API | Role |
|---|---|---|
| Data | wide CSV | `dose,y_z001,...,y_z100` |
| Common base | `Hanalyze.Model.MultiOutput` | `asMultiY` / `fromMultiY` / `r2Multi` |
| Model | `Hanalyze.Model.MultiLM.fitMultiLM` | linear (B=(XᵀX)⁻¹XᵀY) |
|       | `Hanalyze.Model.Kernel.kernelRidgeMulti` + `autoTuneKernelRidgeMulti` | RBF kernel ridge + LOOCV |
| Report | `Hanalyze.Viz.ReportBuilder.secInteractiveMultiOut` + `mkInteractiveMOLinear` / `mkInteractiveMOKernelRBF` | interactive HTML |

## Design notes

- **One input dimension only**: currently `xcol` accepts a single column. For multi-input
  + multi-output, use `Hanalyze.Model.RFF.rffRidgeMVMulti` directly or wait for an extended CLI.
- **Output grid**: `--xaxis-min` / `--xaxis-max` set the z-axis range. Without them the
  grid expands linearly over `1..q`.
- **Number of data points**: with `kernel-rbf` aim for N ≥ 10 doses (LOOCV stability
  threshold). Six works mechanically but watch for overfitting.
- **Output count q**: q=100..1000 is practical. The HTML grows by the size of the α
  matrix (n × q).

## Related: multi-output across all models

The major models in `Hanalyze.Model.*` follow a single unified policy: **multi-output is the
primary API and single-output is a thin wrapper**.

- `Hanalyze.Model.Regularized.fitRegularizedMulti` (Ridge closed form, Lasso/EN per-column CD)
- `Hanalyze.Model.Spline.fitSplineMulti`
- `Hanalyze.Model.Kernel.kernelRidgeMulti` / `nwRegressionMulti`
- `Hanalyze.Model.RFF.rffRidgeMulti` (1D input) / `rffRidgeMVMulti` (multi-input)
- `Hanalyze.Model.GP.fitGPMulti` (shared Ky⁻¹ and shared variance)
- `Hanalyze.Model.GPRobust.fitGPRobustMulti`
- `Hanalyze.Model.GLM.fitGLMMulti` (per-column IRLS)
- `Hanalyze.Model.GLMM.fitLMEMulti` / `fitGLMMMulti`
- `Hanalyze.Model.HBM.observeColumns` (multi-output DSL helper)

q=1 numerically matches the legacy single-output API (verified in `test/Spec.hs` under
the "Multi-output equivalence (q=1)" describe block — 10 cases).
