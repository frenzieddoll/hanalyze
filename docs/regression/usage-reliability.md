# Accelerated life testing models (Arrhenius / Eyring / Inverse Power Law)

Module: `Hanalyze.Model.Reliability` (`hanalyze-models`)

Accelerated life testing reproduces failures that would take years under
normal operating conditions by applying high stress (high temperature /
high voltage) over a short time, then **regresses the relationship between
stress and lifetime and extrapolates it back to operating conditions**. This
module provides three classical models as linear regressions of log
lifetime.

| Function | Model | Primary stress |
|---|---|---|
| `fitArrhenius` | `t = A · exp(Ea / (k_B · T))` | Temperature |
| `fitEyring` | `t · T = A · exp(Ea / (k_B · T)) · exp(B · S)` | Temperature + one more |
| `fitInversePower` | `t = A · S^(-n)` | Voltage / mechanical stress |

For estimating the lifetime distribution itself (Weibull shape / scale), see
[usage-weibull.md](usage-weibull.md). This module models the **average
level** of lifetime as a function of stress and does not specify a
distributional form (OLS assuming Gaussian residuals).

## 0. API

```haskell
kBoltzmann :: Double        -- Boltzmann constant (eV/K)

fitArrhenius    :: [(Double, [Double])]         -> Either Text ArrheniusFit
fitEyring       :: [(Double, Double, [Double])] -> Either Text EyringFit
fitInversePower :: [(Double, [Double])]         -> Either Text InversePowerFit

accelerationFactor :: ArrheniusFit -> Double -> Double -> Double
```

Every input has the shape **"a list of lifetimes per stress level"**, which
maps directly onto real data from testing several units at each level.

Conditions that yield `Left`: an empty input / zero total observations, only
one stress level (the slope is undetermined), or a temperature ≤ 0 or
lifetime ≤ 0 (the log cannot be taken).

## 1. Arrhenius (temperature acceleration)

3 temperature levels × 3 lifetimes each. Temperature is passed in
**absolute Kelvin**.

```haskell
import Hanalyze.Model.Reliability

main :: IO ()
main = do
  let arr = [ (348.15, [ 1050, 980, 1120 ])     -- 75 C
            , (373.15, [  310, 290,  335 ])     -- 100 C
            , (398.15, [  105,  95,  115 ]) ]   -- 125 C
  case fitArrhenius arr of
    Left err -> putStrLn ("error: " ++ show err)
    Right f  -> do
      print (afA f, afEa f, afN f)
      print (accelerationFactor f 298.15 398.15)
  print kBoltzmann
```

Output:

```
(1.1385057209261695e-5,0.5503049757365821,9)
216.93288276839078
8.617333262145e-5
```

- **`afEa` = 0.55 eV** — the activation energy. Typical failure mechanisms
  in semiconductors fall in the 0.3–1.0 eV range, so a wildly different
  value should make you question the test conditions or the data.
- **`afA` = 1.14e-5** — the pre-exponential factor. Its units depend on the
  units of lifetime.
- **`afN` = 9** — the total number of (temperature, lifetime) observations,
  not the number of levels.
- **Acceleration factor 216.9** — `accelerationFactor fit T_use T_test`
  computes `exp(Ea/k_B · (1/T_use − 1/T_test))`. This corresponds to "1 hour
  at 125 C = 217 hours at 25 C". The argument order is
  **(operating condition, test condition)**; passing the same temperature
  twice gives 1.

## 2. Inverse Power Law (voltage / stress acceleration)

```haskell
  let ipl = [ (10, [ 4200, 3900, 4400 ])
            , (15, [  820,  760,  880 ])
            , (20, [  260,  240,  275 ]) ]
  case fitInversePower ipl of
    Left err -> putStrLn ("error: " ++ show err)
    Right f  -> print (ipfA f, ipfN f, ipfNobs f)
```

```
(4.278453355340438e7,4.011957418619679,9)
```

`ipfN` = 4.01 is the power exponent n. The interpretation is that
**doubling the stress shortens lifetime by a factor of 2^4 = 16**. For
insulator voltage acceleration, n = 3–5 is the empirical range. Note the
confusing field names `ipfNobs` (observation count) versus `ipfN`
(exponent).

## 3. Eyring (temperature + one more stress)

Handles temperature and a second stress (humidity / current density /
voltage, etc.) simultaneously. Input is `(temperature K, stress,
[lifetimes])`.

```haskell
  let eyr = [ (348.15, 0.0, [ 1050, 980 ])
            , (348.15, 1.0, [  520, 495 ])
            , (373.15, 0.0, [  310, 290 ])
            , (373.15, 1.0, [  155, 148 ]) ]
  case fitEyring eyr of
    Left err -> putStrLn ("error: " ++ show err)
    Right f  -> print (efA f, efEa f, efB f, efN f)
```

```
(1.3425050183030706e-2,0.5125061323103351,-0.6878818176120822,8)
```

`efB` = −0.688 is the coefficient for the second stress. Since the model is
`exp(B · S)`, **a negative B means lifetime decreases as stress
increases**. Indeed `exp(-0.688) = 0.503`, consistent with an input where
moving S from 0 to 1 roughly halves lifetime.

Internally, `y = log t + log T` is solved as a 2-variable OLS on
`(1/T, S)`; the `log T` term entering the model is what distinguishes this
from Arrhenius.

## 4. Result types

| Type | Fields |
|---|---|
| `ArrheniusFit` | `afA` / `afEa` (eV) / `afLogLik` / `afN` |
| `EyringFit` | `efA` / `efEa` / `efB` / `efLogLik` / `efN` |
| `InversePowerFit` | `ipfA` / `ipfN` (exponent) / `ipfLogLik` / `ipfNobs` |

`*LogLik` is the log-likelihood under the Gaussian-residual assumption. It
can be used, for example, to compare the fit of Arrhenius versus Inverse
Power Law on the same data (with the same number of parameters, higher is
better).

## 5. Choosing an approach

| Goal | Use |
|---|---|
| Temperature acceleration only, get the activation energy | `fitArrhenius` (this doc) |
| Temperature + humidity / voltage simultaneously | `fitEyring` (this doc) |
| Power law for voltage / stress | `fitInversePower` (this doc) |
| The lifetime's distributional form (k / λ / B_p) | [usage-weibull.md](usage-weibull.md) |
| Nonparametric survival analysis with censoring | [10-survival.md](10-survival.md) |
| System reliability (series / parallel / k-of-n) | [`Model.ReliabilityBlockDiagram`](../api-guide/07-survival.md) |

## See also

- Package: [hanalyze-models](../../hanalyze-models/README.md)
- Weibull MLE / B_p life: [usage-weibull.md](usage-weibull.md)
- Survival analysis: [10-survival.md](10-survival.md)
