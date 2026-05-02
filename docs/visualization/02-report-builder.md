# HTML Reports — `Viz.ReportBuilder` and `Reportable`

> 🌐 **English** | [日本語](02-report-builder.ja.md)

> **TODO**: English translation pending. Please refer to the [Japanese version](02-report-builder.ja.md) for now.

## Summary

A compositional HTML-report builder. Define analyses as a list of `ReportSection`
values; `renderReport` produces a single self-contained HTML (Vega-Lite assets
embedded). The `Reportable` typeclass lets each fit type generate its default
section list. Covers ridge / kernel / spline / RFF / RobustGP / quantile / gam /
rf — complementary to the existing `Viz.AnalysisReport` (LM / GLM / GLMM / GP /
HBM).
