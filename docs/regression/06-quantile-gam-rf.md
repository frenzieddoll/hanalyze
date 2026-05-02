# Quantile / GAM / Random Forest

> 🌐 **English** | [日本語](06-quantile-gam-rf.ja.md)

> **TODO**: English translation pending. Please refer to the [Japanese version](06-quantile-gam-rf.ja.md) for now.

## Summary

Three regression methods that complement OLS / GLM for problems with
outliers, asymmetric distributions, complex non-linearity, and feature
interactions:

- **Quantile regression** (`Model.Quantile`) — fits the τ-quantile via
  Hunter-Lange MM-IRLS; medians (τ=0.5) are outlier-resistant; multiple τ
  values give prediction bands.
- **GAM** (`Model.GAM`) — additive B-spline model y = β₀ + Σ s_j(x_j),
  Ridge-regularized, with per-feature partial effects for interpretability.
- **Random Forest** (`Model.RandomForest`) — CART trees + bagging + random
  feature subspaces; handles interactions automatically; produces feature
  importance.

CLI subcommands: `hanalyze quantile|gam|rf`. Reports are generated through
direct `Viz.ReportBuilder` section construction (typeclass `Reportable`
instances pending). Includes a comparison-and-selection guide for choosing
among LM / Quantile / Spline / GAM / Kernel / RF based on linearity, outlier
sensitivity, interactions, scale-invariance, and runtime.
