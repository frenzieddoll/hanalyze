# Orthogonal arrays and the Taguchi method

> 🌐 **English** | [日本語](03-orthogonal-taguchi.ja.md)

> **TODO**: English translation pending. Please refer to the [Japanese version](03-orthogonal-taguchi.ja.md) for now.

## Summary

A consolidated guide on orthogonal arrays Lₙ (`Design.Orthogonal`) and the
Taguchi method (`Design.Taguchi`). Covers theory (Lₙ notation, orthogonality
definition, mixed-level Plackett-Burman), the four SN ratios
(SmallerBetter / LargerBetter / NominalBest / NominalBestTarget),
inner/outer arrays for control vs. noise factors, factor-effect analysis,
optimal-level selection under additive main-effects modeling, end-to-end CLI
workflow (`hanalyze doe ortho`, `hanalyze taguchi cross|analyze`), HTML
reports via `Viz.Taguchi` and `Viz.ReportBuilder`, plus a worked chemical
process optimization example and common pitfalls.
