# Stat.ClassMetrics — 分類モデル評価

> sklearn.metrics 相当の分類評価メトリクスを Haskell に提供。
> hard-prediction (ラベル) と soft-prediction (確率) の両系統。

## 1. 二値分類

```haskell
-- ラベルベース
let c = CM.confusionMatrix [1,0,1,0,1] [1,0,1,0,0]
CM.accuracy c       -- 0.8
CM.precision c      -- TP / (TP + FP)
CM.recall c         -- TP / (TP + FN)
CM.f1Score c        -- harmonic mean
CM.matthewsCorr c   -- MCC, [-1, 1]

-- 確率ベース
let ys     = [0, 0, 0, 1, 1, 1]
    scores = [0.1, 0.3, 0.4, 0.6, 0.7, 0.9]
CM.auc ys scores             -- ROC AUC
CM.averagePrecision ys scores -- PR AUC
CM.logLoss ys scores         -- cross-entropy
CM.brierScore ys scores      -- MSE on probabilities
```

## 2. 多クラス

```haskell
let cm = CM.confusionMulti [0, 1, 2, 0, 1, 2] [0, 1, 2, 1, 1, 2]
CM.accuracyMulti cm  -- 0.833
CM.macroF1 cm        -- mean of per-class F1
CM.weightedF1 cm     -- weighted by class support
```

## 3. メトリクスの選び方

| 状況 | 推奨 |
|---|---|
| バランスデータ | `accuracy`, `f1Score` |
| 不均衡データ (例: rare event) | `f1Score`, `matthewsCorr`, `auc` |
| 確率校正の評価 | `brierScore`, `logLoss` |
| ranking タスク | `auc` |
| precision/recall trade-off 重視 | `prCurve`, `averagePrecision` |
| 多クラス + 不均衡 | `macroF1`, `weightedF1` |

## 4. ROC / PR curve

```haskell
let roc = CM.rocCurve ys scores  -- [(FPR, TPR), ...]
    pr  = CM.prCurve ys scores   -- [(recall, precision), ...]
```

可視化は Viz モジュールで HTML 出力可能。多クラス分類では、決定境界を領域色で
塗り分けてクラス平均を重ねると、分類器がどのように特徴空間を分割しているかを
直感的に把握できます。下図は LDA による 3 クラスの決定境界です。

![LDA の決定境界とクラス平均 (3 クラスを領域色で塗り分け)](../images/lda-decision-boundary.svg)
