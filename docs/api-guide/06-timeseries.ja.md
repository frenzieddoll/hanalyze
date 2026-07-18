# 時系列

> 🌐 [English](06-timeseries.md) | **日本語**

> [📚 索引](README.ja.md) ｜ [01 quickstart](01-quickstart.ja.md) ｜ [02 regression](02-regression.ja.md) ｜ [03 bayesian-hbm](03-bayesian-hbm.ja.md) ｜ [04 multivariate](04-multivariate.ja.md) ｜ [05 ml](05-ml.ja.md) ｜ **06 timeseries** ｜ [07 survival](07-survival.ja.md) ｜ [08 causal](08-causal.ja.md) ｜ [09 doe](09-doe.ja.md) ｜ [10 stat](10-stat.ja.md) ｜ [11 data](11-data.ja.md) ｜ [12 plot](12-plot.ja.md)

自己回帰・多変量自己回帰・ボラティリティ・予測。 理論は [09-timeseries](../regression/09-timeseries.ja.md) ・
[usage-ts-surv-advanced](../timeseries/usage-ts-surv-advanced.ja.md) が一次根拠。

| モデル | fit | 結果型 | 図 |
|---|---|---|---|
| AR(p) + 予測 | `forecastModel p h ys` | `ForecastModel` (Plottable) | 予測区間 |
| VAR(p) | `fitVAR p yMat` | `VARFit` | (forecastVAR で点予測) |
| GARCH(1,1) | `fitGARCH ys` | `GARCHFit` (Plottable) | ボラティリティ |

---

## AR(p) + 予測

```haskell
fitAR         :: Int -> LA.Vector Double -> ARFit          -- 当てはめのみ
forecastModel :: Int -> Int -> LA.Vector Double -> ForecastModel   -- p, 地平 h, 系列
```

`ForecastModel` は `Plottable` (履歴 + 予測 + 区間)。

```haskell
let fm = forecastModel 2 10 ys           -- AR(2)・10 ステップ予測
saveSVGBound "forecast.svg" $ noDf |>> toPlot fm
```

![AR 予測](../images/ts-forecast.svg)

---

## VAR(p)

```haskell
fitVAR :: Int -> LA.Matrix Double -> VARFit       -- 列 = 変数、 行 = 時刻
```

```haskell
let fit = fitVAR 1 yMat              -- VAR(1)
    fc  = forecastVAR fit yMat 12    -- 12 ステップ点予測
```

`VARFit` は `Plottable` instance を持たない (点予測は `forecastVAR`)。 残差 `varResiduals`
((n−p)×K)・残差共分散 `varSigma` を取れる。 方程式別 OLS が MLE になる根拠は
[usage-ts-surv-advanced](../timeseries/usage-ts-surv-advanced.ja.md)。

---

## GARCH(1,1)

```haskell
fitGARCH :: LA.Vector Double -> GARCHFit          -- 収益率系列
```

`GARCHFit` は `Plottable` (`toPlot` = 収益率 + 条件付きボラティリティ)。

```haskell
let fit = fitGARCH ys                -- de-mean 可能な収益率系列
    fc  = forecastGARCH fit 10       -- 10 ステップ σ² 予測 (→ ω/(1-α-β) に収束)
saveSVGBound "garch-volatility.svg" $ noDf |>> toPlot fit
```

![GARCH ボラティリティ](../images/garch-volatility.svg)

GARCH モデル式 `σ²_t = ω + α ε²_{t-1} + β σ²_{t-1}`・定常性制約の再パラメタ化
(softplus + stick-breaking) は [usage-ts-surv-advanced](../timeseries/usage-ts-surv-advanced.ja.md)。

---

## State Space / Kalman フィルタ (Phase 15)

`Hanalyze.Model.StateSpace` に線形ガウス状態空間モデルと Kalman
フィルタ / 平滑化 (`toPlot` 非対象・数値結果)。

```haskell
data StateSpaceModel = StateSpaceModel { ssmF, ssmH, ssmQ, ssmR :: LA.Matrix Double }
kalmanFilter   :: StateSpaceModel -> LA.Matrix Double -> FilterResult     -- 前向きフィルタ
kalmanSmoother :: StateSpaceModel -> LA.Matrix Double -> SmootherResult    -- RTS 平滑化
```
