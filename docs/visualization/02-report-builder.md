# HTML Reports — `Viz.ReportBuilder` and `Reportable`

> 🌐 **English** | [日本語](02-report-builder.ja.md)

> **TODO**: English translation pending. Please refer to the [Japanese version](02-report-builder.ja.md) for now.

## Summary

A compositional HTML-report builder. Define analyses as a list of `ReportSection`
values; `renderReport` produces a single self-contained HTML (Vega-Lite assets
embedded). The `Reportable` typeclass lets each fit type generate its default
section list. Currently covers ridge / kernel / spline / RFF / RobustGP / quantile / gam / rf.

**Status**: `Viz.ReportBuilder` is the going-forward standard. The legacy
`Viz.AnalysisReport` (LM / GLM / GLMM / GP / HBM, sum-type based) is **deprecated**
(`{-# DEPRECATED #-}` pragma) and kept only for CLI `regress --report`
compatibility. New models / visualizations must be implemented on top of
`ReportBuilder`. `Reportable` instances for LM/GLM/GLMM/GP/HBM are the next
milestone, after which `AnalysisReport` will be removed.
