# Model.TimeSeries — Time Series Models

> 🌐 **English** | [日本語](09-timeseries.ja.md)

> Equivalent to statsmodels.tsa / R's forecast.
> ARIMA, Holt-Winters, ACF/PACF, STL decomposition.

## 1. ACF / PACF (Exploratory Analysis)

```haskell
import qualified Hanalyze.Model.TimeSeries as TS

let y = LA.fromList [1.0, 1.2, 1.5, 1.8, 1.6, 1.4, 1.7, ...]

let acf  = TS.autocorrelation 20 y     -- lag 0..20
    pacf = TS.partialAutocorrelation 20 y
```

Order selection by examining ACF/PACF:
- AR(p): PACF cuts off at lag p
- MA(q): ACF cuts off at lag q
- ARMA: Both trail off

## 2. AR (Autoregressive)

```haskell
-- Fit AR(2) using Yule-Walker
let fit = TS.fitAR 2 y
TS.arPhi fit         -- φ_1, φ_2 (length 2)
TS.arIntercept fit   -- μ
TS.arResidVar fit    -- innovation variance

-- Forecast
let fc = TS.forecastAR fit y 10  -- 10-step ahead
```

Appending AR forecast to historical series shows the forecast interval band
widening with the forecast horizon (forecast step):

![Time series history + AR forecast + forecast interval band](../images/ts-forecast.svg)

## 3. MA (Moving Average)

```haskell
let fit = TS.fitMA 1 y
TS.maTheta fit       -- θ_1
TS.maResiduals fit   -- innovation series

let fc = TS.forecastMA fit 5
```

## 4. ARIMA(p, d, q)

```haskell
-- ARIMA(2, 1, 1)
let fit = TS.fitARIMA 2 1 1 y

-- Forecast (with inverse differencing)
let fc = TS.forecastARIMA fit 12  -- 12-step ahead
```

`d` is the differencing order. Used to stationarize non-stationary data.

## 5. Exponential Smoothing

### Single

```haskell
-- Simple exponential smoothing with α=0.3
let smoothed = TS.simpleExpSmoothing 0.3 y
```

### Holt-Winters (Triple)

```haskell
-- Monthly data with seasonality 12
let fit = TS.holtWinters TS.HWAdditive 12 y

TS.hwLevel fit
TS.hwTrend fit
TS.hwSeasonal fit
TS.hwFitted fit       -- in-sample fitted values

-- Forecast 24 months ahead
let fc = TS.hwForecast fit 24
```

`HWAdditive` vs `HWMultiplicative`:
- Seasonal component amplitude constant → Additive
- Seasonal component amplitude proportional to level → Multiplicative

## 6. STL Decomposition

```haskell
let (trend, seasonal, residual) = TS.stlDecompose 12 y
```

- trend: Centered moving average
- seasonal: Seasonal component (repeats with period 12, mean 0)
- residual: y - trend - seasonal

## 7. Helpers

```haskell
TS.movingAverage 7 y           -- Centered MA (ends are NaN)
TS.differencing y               -- y'_t = y_t - y_{t-1}
TS.inverseDifferencing y diff   -- Restore via cumulative sum
```

## 8. Model Selection (Recommended Procedure)

1. Plot → visually inspect trend / seasonality / structural breaks
2. `differencing` to stationarize, ADF test (planned for future)
3. Examine ACF/PACF for (p, d, q) candidates
4. Fit with `fitARIMA` or `holtWinters`
5. Diagnostic: residual plots + Ljung-Box test (planned)
6. Forecast with `forecastARIMA`
