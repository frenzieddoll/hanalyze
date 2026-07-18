# Model.Survival — Survival Analysis

> 🌐 **English** | [日本語](10-survival.ja.md)

> Equivalent to R's `survival` package / Python's `lifelines`.
> Handles right censoring.

## 1. Data Representation

```haskell
data SurvSample = SurvSample
  { ssTime  :: Double      -- Observation time
  , ssEvent :: Event       -- Observed | Censored
  }

data Event = Censored | Observed
```

Example: patient dies at day 30 → `SurvSample 30 Observed`, still alive at end of observation → `SurvSample 30 Censored`.

## 2. Kaplan-Meier Estimation

```haskell
import qualified Hanalyze.Model.Survival as Surv

let samples =
      [ Surv.SurvSample 5 Surv.Observed
      , Surv.SurvSample 7 Surv.Censored
      , Surv.SurvSample 10 Surv.Observed
      , Surv.SurvSample 15 Surv.Observed
      , ...
      ]

let km = Surv.kaplanMeier samples

Surv.kmrTimes km      -- Distinct event times
Surv.kmrSurvival km   -- Ŝ(t) at each
Surv.kmrAtRisk km     -- Risk set size
Surv.kmrEvents km     -- Event count
Surv.kmrCensored km   -- Censored count
```

The estimated survival function Ŝ(t) is a step function that jumps downward at event times. The plot below shows an example Kaplan-Meier survival curve, where each step represents the reduction in survival probability at that event time.

![Kaplan-Meier survival curve](../images/km-survival.svg)

## 3. Nelson-Aalen Cumulative Hazard

```haskell
let na = Surv.nelsonAalen samples
Surv.narCumHazard na   -- Ĥ(t) = Σ d_j/n_j (monotone increasing)
```

## 4. Group Comparison (Log-rank Test)

```haskell
let groupA = [Surv.SurvSample t Surv.Observed | t <- [...]]
    groupB = [Surv.SurvSample t Surv.Observed | t <- [...]]

let lr = Surv.logRankTest [groupA, groupB]

Surv.lrChi2 lr      -- χ² statistic
Surv.lrDf lr        -- k-1
Surv.lrPValue lr    -- p-value
```

Tests `H_0: S_A(t) = S_B(t)`. Handles multiple groups (k ≥ 3).

## 5. Cox Proportional Hazard Regression

```haskell
-- Covariates and event data
let xs = [LA.fromList [age, treatment, sex] | (...) <- patients]
    ys = [Surv.SurvSample timeFollowup eventStatus | ...]

let fit = Surv.coxPH xs ys

Surv.coxBeta fit       -- Coefficients (length p)
Surv.coxSE fit         -- SE (from Fisher information)
Surv.coxLogLik fit     -- Log partial likelihood
Surv.coxIters fit      -- Newton iterations

-- Hazard ratio (HR)
let hr = exp (LA.atIndex (Surv.coxBeta fit) 0)
-- HR > 1: covariate increases hazard
```

## 6. Baseline Hazard (Breslow Estimation)

```haskell
let baselineH = Surv.coxBaselineHazard fit xs ys
-- [(t_1, H_0(t_1)), (t_2, H_0(t_2)), ...]
```

Actual survival function: S(t | x) = exp(-H_0(t) × exp(β·x))

When multiple competing events exist (e.g., different causes of death), the cumulative incidence function (CIF) rather than a single survival function represents the probability of each event. The plot below shows an example CIF under competing risks, with each curve representing the cumulative incidence probability over time for each event type.

![Cumulative incidence function (CIF) under competing risks](../images/cif-competing.svg)

## 7. Algorithms

- **KM**: Stepwise Ŝ(t) = Π(1 - d_j/n_j)
- **NA**: Ĥ(t) = Σ d_j/n_j
- **Log-rank**: Aggregate observed - expected at each time point, χ² approximation
- **Cox PH**: Maximize partial likelihood via Newton-Raphson, Hessian by central difference

## 8. Notes

- Input requires **time + event** information. Provide censoring information correctly
- Cox PH assumes **proportional hazards** (recommended to verify via log-log plot)
- Ties are handled via Breslow approximation (Efron not yet implemented)
