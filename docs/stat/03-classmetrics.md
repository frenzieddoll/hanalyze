# Stat.ClassMetrics — Classification Model Evaluation

> 🌐 **English** | [日本語](03-classmetrics.ja.md)

> Provides classification evaluation metrics equivalent to sklearn.metrics in Haskell.
> Covers both hard-prediction (labels) and soft-prediction (probabilities) families.

## 1. Binary Classification

```haskell
-- Label-based
let c = CM.confusionMatrix [1,0,1,0,1] [1,0,1,0,0]
CM.accuracy c       -- 0.8
CM.precision c      -- TP / (TP + FP)
CM.recall c         -- TP / (TP + FN)
CM.f1Score c        -- harmonic mean
CM.matthewsCorr c   -- MCC, [-1, 1]

-- Probability-based
let ys     = [0, 0, 0, 1, 1, 1]
    scores = [0.1, 0.3, 0.4, 0.6, 0.7, 0.9]
CM.auc ys scores             -- ROC AUC
CM.averagePrecision ys scores -- PR AUC
CM.logLoss ys scores         -- cross-entropy
CM.brierScore ys scores      -- MSE on probabilities
```

## 2. Multi-Class

```haskell
let cm = CM.confusionMulti [0, 1, 2, 0, 1, 2] [0, 1, 2, 1, 1, 2]
CM.accuracyMulti cm  -- 0.833
CM.macroF1 cm        -- mean of per-class F1
CM.weightedF1 cm     -- weighted by class support
```

## 3. Metric Selection Guide

| Scenario | Recommended |
|---|---|
| Balanced data | `accuracy`, `f1Score` |
| Imbalanced data (e.g., rare event) | `f1Score`, `matthewsCorr`, `auc` |
| Probability calibration evaluation | `brierScore`, `logLoss` |
| Ranking task | `auc` |
| Precision/recall trade-off emphasis | `prCurve`, `averagePrecision` |
| Multi-class + imbalanced | `macroF1`, `weightedF1` |

## 4. ROC / PR Curve

```haskell
let roc = CM.rocCurve ys scores  -- [(FPR, TPR), ...]
    pr  = CM.prCurve ys scores   -- [(recall, precision), ...]
```

Visualization via Viz module provides HTML output. For multi-class classification,
coloring decision boundaries by region and overlaying class means shows how the
classifier partitions feature space intuitively. The figure below shows the decision
boundary from LDA on 3 classes.

![LDA decision boundary and class means (3 classes, regions colored by class)](../images/lda-decision-boundary.svg)
