# Time-series & survival extensions (Phase 35: GARCH / VAR / Competing Risks / RBD)

> Phase 35 (2026-05-29) bundles four advanced modules that the original
> `Hanalyze.Model.{TimeSeries, Survival, Weibull, Reliability}` did not
> cover. State Space / Kalman Filter is **already implemented** in
> `Hanalyze.Model.StateSpace` (Phase 15) and is mentioned only here.

---

## 0. Module map

| Feature | API | Notes |
|---|---|---|
| GARCH(1,1) | `Hanalyze.Model.GARCH` | Gaussian QMLE via L-BFGS |
| VAR(p) | `Hanalyze.Model.VAR` | equation-by-equation OLS |
| Competing Risks (CIF) | `Hanalyze.Model.CompetingRisks` | Kalbfleisch-Prentice |
| Reliability Block Diagram | `Hanalyze.Model.ReliabilityBlockDiagram` | Series / Parallel / k-of-n |
| State Space / Kalman | `Hanalyze.Model.StateSpace` | **Phase 15 既実装** |

---

## 1. GARCH(1,1) (35-A1)

```haskell
import qualified Hanalyze.Model.GARCH as GARCH

let fit = GARCH.fitGARCH ys                  -- y is a de-meanable return series
    fc  = GARCH.forecastGARCH fit 10         -- 10-step σ² forecast
```

`GARCHFit` is `Plottable`: `toPlot` (= `garchVolatility`) draws the return
series with a `μ ± 2σ_t` conditional-volatility band — the figure below:

```haskell
import Hanalyze.Plot       (toPlot)
import Hgg.Plot.Spec          (ColData (..))
import Hgg.Plot.Frame         ((|>>))
import Hgg.Plot.Backend.SVG   (saveSVGBound)

let noDf = [] :: [(Text, ColData)]
saveSVGBound "garch-volatility.svg" (noDf |>> toPlot fit)
```

- Model: `σ²_t = ω + α · ε²_{t-1} + β · σ²_{t-1}`, `ε_t = y_t - μ`
- Stationarity (`ω > 0, α ≥ 0, β ≥ 0, α + β < 1`) enforced by
  reparametrization (softplus for ω, stick-breaking sigmoid pair for α, β)
- `gLogLik` is the maximized Gaussian log-likelihood. Long-horizon
  forecasts converge to the unconditional variance `ω / (1 - α - β)`

The fitted conditional volatility tracks the clustering of large returns,
shown here as a band around the return series:

![GARCH(1,1) conditional volatility band over the return series](../images/garch-volatility.svg)

---

## 2. VAR(p) (35-A2)

```haskell
import qualified Hanalyze.Model.VAR as VAR

let fit = VAR.fitVAR 2 yMat                  -- yMat is (n × K)
    fc  = VAR.forecastVAR fit yMat 12        -- 12-step point forecast
```

- Model: `yₜ = c + Σ_l Aₗ · yₜ₋ₗ + εₜ`, each `Aₗ` is `K × K`
- Estimated by equation-by-equation OLS — the MLE under Gaussian
  innovations because all equations share the same regressors (Lütkepohl
  2005, §3.2)
- `varResiduals` is `(n − p) × K`, `varSigma` is the residual covariance

---

## 3. Competing Risks / CIF (35-A3)

```haskell
import qualified Hanalyze.Model.CompetingRisks as CR

let samples = [ CR.CRSample 1.2 1, CR.CRSample 2.5 2, CR.CRSample 3.0 0, ... ]
    fit     = CR.fitCompetingRisks samples
```

`CRFit` is `Plottable`: `toPlot` draws the cumulative-incidence curves:

```haskell
import Hanalyze.Plot       (toPlot)
import Hgg.Plot.Spec          (ColData (..))
import Hgg.Plot.Frame         ((|>>))
import Hgg.Plot.Backend.SVG   (saveSVGBound)

let noDf = [] :: [(Text, ColData)]
saveSVGBound "cif.svg" (noDf |>> toPlot fit)
```

- `crCause = 0` means censored; `≥ 1` denotes a specific failure cause
- Estimator: `F̂_k(t) = Σ_{t_i ≤ t} Ŝ(t_i⁻) · d_{k,i} / n_i` with overall
  KM `Ŝ` treating any cause as an event (Kalbfleisch & Prentice 1980)
- Important: this is the canonical fix for the upward bias of the
  naïve "1 − KM on cause-specific data" estimator
- Identity that holds at every event time: `Σ_k F̂_k(t) + Ŝ(t) = 1`

For the parametric survival counterpart, an AFT model yields a smooth
survival curve `S(t | x)` whose location shifts with the covariates.
`fitAFT` returns `IO (Either Text AFTFit)` (covariate matrix `x`, event
times `t > 0`, event indicators `delta`):

```haskell
import qualified Hanalyze.Model.AFT as AFT
import Hanalyze.Plot       (toPlot, aftSurvivalAt)
import Hgg.Plot.Spec          (ColData (..))
import Hgg.Plot.Frame         ((|>>))
import Hgg.Plot.Backend.SVG   (saveSVGBound)

Right fit <- AFT.fitAFT AFT.AFTWeibull xMat tVec deltaVec
let noDf = [] :: [(Text, ColData)]
-- toPlot = baseline-covariate curve; aftSurvivalAt picks an arbitrary x
saveSVGBound "aft-survival.svg" (noDf |>> aftSurvivalAt fit [1, 0.5])
```

`AFTFit` is `Plottable` (`toPlot` = baseline-covariate survival curve).

![AFT parametric survival curves S(t|x)](../images/aft-survival.svg)

---

## 4. Reliability Block Diagram (35-A4)

```haskell
import qualified Hanalyze.Model.ReliabilityBlockDiagram as RBD

-- two-out-of-three redundancy of three series strings
let sys = RBD.KofN 2 [ RBD.Series [RBD.Leaf 0.95, RBD.Leaf 0.99]
                    , RBD.Series [RBD.Leaf 0.95, RBD.Leaf 0.99]
                    , RBD.Series [RBD.Leaf 0.95, RBD.Leaf 0.99] ]
    r   = RBD.reliabilityOf sys
```

- `Leaf p` — component with reliability `p ∈ [0, 1]`
- `Series bs` — `∏ Rᵢ`, all blocks required
- `Parallel bs` — `1 − ∏ (1 − Rᵢ)`, any block suffices
- `KofN k bs` — at least `k` of `n` working, computed by
  Poisson-binomial DP (works with heterogeneous block reliabilities)
- Failure independence between blocks is assumed (standard RBD)

---

## 5. State Space / Kalman (mentioned only)

`Hanalyze.Model.StateSpace` was implemented in Phase 15. Public API:

```haskell
import qualified Hanalyze.Model.StateSpace as SS

let m  = SS.StateSpaceModel { ssmF = ..., ssmH = ..., ssmQ = ..., ssmR = ... }
    fs = SS.kalmanFilter m y
    ss = SS.kalmanSmoother m y
```

See `test/Spec.hs:6443` for a worked example.
