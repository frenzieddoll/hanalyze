# Model.TimeSeries — 時系列モデル

> statsmodels.tsa / R の forecast 相当。
> ARIMA、Holt-Winters、ACF/PACF、STL 分解。

## 1. ACF / PACF (探索的解析)

```haskell
import qualified Model.TimeSeries as TS

let y = LA.fromList [1.0, 1.2, 1.5, 1.8, 1.6, 1.4, 1.7, ...]

let acf  = TS.autocorrelation 20 y     -- lag 0..20
    pacf = TS.partialAutocorrelation 20 y
```

ACF/PACF を見て次数選定:
- AR(p): PACF が lag p で cut off
- MA(q): ACF が lag q で cut off
- ARMA: 両方 trail off

## 2. AR (autoregressive)

```haskell
-- AR(2) を Yule-Walker で fit
let fit = TS.fitAR 2 y
TS.arPhi fit         -- φ_1, φ_2 (length 2)
TS.arIntercept fit   -- μ
TS.arResidVar fit    -- innovation variance

-- 予測
let fc = TS.forecastAR fit y 10  -- 10-step ahead
```

## 3. MA (moving average)

```haskell
let fit = TS.fitMA 1 y
TS.maTheta fit       -- θ_1
TS.maResiduals fit   -- innovation 系列

let fc = TS.forecastMA fit 5
```

## 4. ARIMA(p, d, q)

```haskell
-- ARIMA(2, 1, 1)
let fit = TS.fitARIMA 2 1 1 y

-- 予測 (inverse differencing 込み)
let fc = TS.forecastARIMA fit 12  -- 12-step ahead
```

`d` は階差回数。非定常データを定常化するために。

## 5. 指数平滑

### Single

```haskell
-- α=0.3 の単純指数平滑
let smoothed = TS.simpleExpSmoothing 0.3 y
```

### Holt-Winters (triple)

```haskell
-- 月次データで季節性 12
let fit = TS.holtWinters TS.HWAdditive 12 y

TS.hwLevel fit
TS.hwTrend fit
TS.hwSeasonal fit
TS.hwFitted fit       -- in-sample 適合値

-- 24 ヶ月先予測
let fc = TS.hwForecast fit 24
```

`HWAdditive` vs `HWMultiplicative`:
- 季節成分の振幅が一定 → Additive
- 季節成分の振幅が水準に比例 → Multiplicative

## 6. STL 分解

```haskell
let (trend, seasonal, residual) = TS.stlDecompose 12 y
```

- trend: 中心化移動平均
- seasonal: 季節成分 (周期 12 で繰り返す、平均 0)
- residual: y - trend - seasonal

## 7. ヘルパ

```haskell
TS.movingAverage 7 y           -- 中心化 MA (端は NaN)
TS.differencing y               -- y'_t = y_t - y_{t-1}
TS.inverseDifferencing y diff   -- 累積で復元
```

## 8. モデル選択 (推奨手順)

1. プロット → trend / 季節性 / 構造変化を目視
2. `differencing` で定常化、ADF 検定 (今後追加予定)
3. ACF/PACF を見て (p, d, q) 候補
4. `fitARIMA` or `holtWinters` で fit
5. 残差プロット + Ljung-Box (今後) で診断
6. `forecastARIMA` で予測
