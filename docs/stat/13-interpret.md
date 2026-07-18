# Stat.Interpret — Model Interpretation

> 🌐 **English** | [日本語](13-interpret.ja.md)

> Equivalent to SHAP / LIME / `sklearn.inspection`.
> Model-agnostic explanation techniques.

## 1. Permutation Feature Importance (Breiman 2001)

Identify the most impactful features:

```haskell
import qualified Hanalyze.Stat.Interpret as Interp
import qualified System.Random.MWC as MWC

gen <- MWC.createSystemRandom

-- Any model (predict function) and score function
let predict :: [[Double]] -> [Double]
    predict = -- from your fitted model

    score :: [Double] -> [Double] -> Double
    score yTrue yPred = -- e.g., R², accuracy (higher is better)

    cfg = Interp.defaultPermutationConfig  -- 30 repeats

result <- Interp.permutationImportance cfg predict score xs ys gen

Interp.piMeanImportance result  -- per-feature mean importance
Interp.piStdImportance result   -- standard deviation
Interp.piBaselineScore result   -- score without shuffling
```

Shuffle each feature 30 times and compute importance as mean score decrease.

## 2. Partial Dependence Plot (Friedman 2001)

**Marginal effect** of one feature (others marginalized out):

```haskell
-- Vary feature 0 over [0, 1, 2, 3, 4, 5]
let grid = [0, 1, 2, 3, 4, 5]
    pdp  = Interp.partialDependence predict xs 0 grid

Interp.pdpFeatureValues pdp   -- grid as-is
Interp.pdpMeanPredict pdp     -- mean prediction at each grid value
```

Non-linear relationships become visible.

## 3. ICE (Individual Conditional Expectation, Goldstein 2015)

Partial dependence curve per individual sample:

```haskell
let ice = Interp.icePlot predict xs 0 grid

Interp.iceCurves ice   -- per-sample curves
Interp.iceMean ice     -- average curve (= partial dependence)
```

Dispersed ICE curves indicate **interaction effects** (heterogeneous response).

## 4. Usage Example

```haskell
-- Interpret Random Forest classifier
import qualified Hanalyze.Model.RandomForest as RF
import qualified Hanalyze.Stat.ClassMetrics as CM

let cfg = RF.defaultRFConfig  -- ... (existing module)
rf <- RF.fitRF cfg xsTrain ysTrain gen

let predict :: [[Double]] -> [Double]
    predict = map (RF.predictRF rf)

    score :: [Double] -> [Double] -> Double
    score yt yp =
      let yi  = map round yt :: [Int]
          ypi = map round yp :: [Int]
          c   = CM.confusionMatrix yi ypi
      in CM.accuracy c

-- Which feature matters most?
imp <- Interp.permutationImportance Interp.defaultPermutationConfig
                                    predict score xsTest ysTest gen

-- Check partial dependence of top features for relationship
let topIdx = -- argmax of imp
    grid   = [from .. to]  -- feature range
    pdp    = Interp.partialDependence predict xsTest topIdx grid
```

## 5. Method Selection Guide

| Question | Technique |
|---|---|
| Which feature is most important? | `permutationImportance` |
| Is feature effect monotone? Non-linear? | `partialDependence` |
| Is effect uniform? Individual variation? | `icePlot` |
| Local contribution (per-sample) | SHAP (to be added) |

## 6. Cautions

- **Computational cost**: permutation imp is `nFeatures × nRepeats × predict-cost`
- **Correlated features**: Shuffling can generate artificial points; conditional permutation better in some cases
- **Train or test data?**: Test data recommended (doesn't reflect overfit)
