# Survival Analysis

> 🌐 **English** | [日本語](07-survival.ja.md)

> [📚 Index](README.md) | [01 quickstart](01-quickstart.md) | [02 regression](02-regression.md) | [03 bayesian-hbm](03-bayesian-hbm.md) | [04 multivariate](04-multivariate.md) | [05 ml](05-ml.md) | [06 timeseries](06-timeseries.md) | **07 survival** | [08 causal](08-causal.md) | [09 doe](09-doe.md) | [10 stat](10-stat.md) | [11 data](11-data.md) | [12 plot](12-plot.md)

Survival time analysis. Theory references: [10-survival](../regression/10-survival.md) and [usage-ts-surv-advanced](../timeseries/usage-ts-surv-advanced.md).

| Method | fit | Result Type | Plot |
|---|---|---|---|
| Kaplan-Meier | `kaplanMeier samples` | `KMResult` (Plottable) | Survival curve |
| Competing Risks (CIF) | `Hanalyze.Model.CompetingRisks` | `CRFit` (Plottable) | Cumulative incidence |
| AFT (Accelerated Failure Time) | `fitAFT dist x t δ` | `IO (Either Text AFTFit)` (Plottable) | Survival curve |
| Cox Proportional Hazard | `Hanalyze.Model.Survival` | (`toPlot` not supported) | — |

---

## Kaplan-Meier

```haskell
kaplanMeier :: [SurvSample] -> KMResult
-- data SurvSample = SurvSample { observation time, event indicator, … }
```

`KMResult` is `Plottable` (survival curve + censoring marks).

```haskell
saveSVGBound "km.svg" $ noDf |>> toPlot (kaplanMeier samples)
```

![Kaplan-Meier survival curve](../images/km-survival.svg)

---

## Competing Risks (Cumulative Incidence Function CIF)

`CRFit` is `Plottable` (`toPlot` = cumulative incidence curves by event type).

```haskell
fitCompetingRisks :: [CRSample] -> CRFit
-- data CRSample = CRSample { crTime :: Double, crCause :: Int }  -- crCause 0=censored / ≥1=cause
```

```haskell
let samples = [ CR.CRSample 1.2 1, CR.CRSample 2.5 2, CR.CRSample 3.0 0 ]
saveSVGBound "cif.svg" $ noDf |>> toPlot (CR.fitCompetingRisks samples)
```

![Competing Risks CIF](../images/cif-competing.svg)

> Kalbfleisch-Prentice estimator. The naive method of "1−KM by cause" has upward bias, so this CIF is the classical correction. Derivation is detailed in [usage-ts-surv-advanced](../timeseries/usage-ts-surv-advanced.md).

---

## AFT (Accelerated Failure Time Model)

```haskell
fitAFT :: AFTDistribution -> LA.Matrix Double -> LA.Vector Double -> V.Vector Bool -> IO (Either Text AFTFit)
--        distribution                  x = covariate matrix       t = time            δ = event indicator
```

`AFTFit` is `Plottable` (`toPlot` = baseline covariate survival curve). Survival curve at arbitrary x via `aftSurvivalAt`.

```haskell
import Hanalyze.Plot (toPlot, aftSurvivalAt)

Right fit <- fitAFT AFTWeibull xMat tVec deltaVec
-- Figure below = baseline survival curve (toPlot). Arbitrary covariate survival via aftSurvivalAt fit [1, 0.5].
saveSVGBound "aft-survival.svg" $ noDf |>> toPlot fit <> title "AFT survival curve (baseline)"
```

![AFT survival curve (baseline)](../images/aft-survival.svg)

---

## Cox Proportional Hazard

`Hanalyze.Model.Survival` provides Cox proportional hazard. Currently `toPlot` not supported (extract coefficients / hazard ratios numerically). See [10-survival](../regression/10-survival.md).

---

## Reliability Block Diagram (RBD)

`Hanalyze.Model.ReliabilityBlockDiagram` — computes system reliability from series / parallel / k-of-n structure (`toPlot` not supported; scalar result).

```haskell
data RBD = Leaf Double | Series [RBD] | Parallel [RBD] | KofN Int [RBD]
reliabilityOf :: RBD -> Double
-- Series = ∏ Rᵢ / Parallel = 1−∏(1−Rᵢ) / KofN k = k-of-n success (Poisson-binomial DP)
```

```haskell
let sys = RBD.KofN 2 [ RBD.Series [RBD.Leaf 0.95, RBD.Leaf 0.99]
                     , RBD.Series [RBD.Leaf 0.95, RBD.Leaf 0.99]
                     , RBD.Series [RBD.Leaf 0.95, RBD.Leaf 0.99] ]
    r   = RBD.reliabilityOf sys
```

Independence of block failures is assumed (textbook RBD). See [usage-ts-surv-advanced](../timeseries/usage-ts-surv-advanced.md).
