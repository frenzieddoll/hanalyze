# ML extensions (Phase 34: GBM / SVM / MDS / k-NN / Naive Bayes)

> Phase 34 (2026-05-29) bundles five classical ML algorithms that were
> missing from hanalyze. Already implemented (mentioned only here):
> Random Forest classifier (`RandomForestClassifier`) and MLP neural net
> (`NeuralNetwork`).

---

## 0. Module map

| Feature | API | Variants |
|---|---|---|
| Gradient Boosting | `Hanalyze.Model.GradientBoosting` | regression + binary classification |
| SVM (kernel-selectable) | `Hanalyze.Model.SVM` | binary + multiclass (OvR), Linear/Poly/RBF, CV tuning |
| MDS | `Hanalyze.Stat.MDS` | classical + Sammon |
| k-NN | `Hanalyze.Model.KNN` | regression + classification |
| Naive Bayes | `Hanalyze.Model.NaiveBayes` | Gaussian + Multinomial |

---

## 1. Gradient Boosting (34-A1)

```haskell
import qualified Hanalyze.Model.GradientBoosting as GB

let cfg = GB.defaultGBConfig { GB.gbNRounds = 200, GB.gbLearnRate = 0.05 }
    gb  = GB.fitGBRegressor cfg xMat ys
    yhat = GB.predictGBR gb xMat

let gbc = GB.fitGBClassifier cfg xMat yCls
    ps  = GB.predictGBCProbs gbc xMat
```

For the high-level picture, both `GBRegressor` and `GBClassifier` are
`Plottable`, so `toPlot` draws the feature-importance bar directly:

```haskell
import Hanalyze.Plot       (toPlot)
import Hgg.Plot.Spec          (ColData (..))
import Hgg.Plot.Frame         ((|>>))
import Hgg.Plot.Backend.SVG   (saveSVGBound)

let noDf = [] :: [(Text, ColData)]
saveSVGBound "gbm-importance.svg" (noDf |>> toPlot gb)
```

Weak learner reuses `Hanalyze.Model.RandomForest.buildTreeV` (bootstrap off,
mtry = d) so each round fits a depth-limited full-data regression tree on the
negative gradient. Squared loss for regression, log-loss for binary
classification.

Accumulating the split-gain contribution of each feature across all
rounds gives a feature-importance ranking:

![Gradient boosting feature importance](../images/gbm-importance.svg)

---

## 2. SVM (34-A2 / kernel unification Phase 75.15 / renamed Phase 75.19 / CV tuning 75.20)

```haskell
import qualified Hanalyze.Model.SVM as SVM
import           Hanalyze.Model.Kernel (Kernel (..), KernelParams (..), defaultKernelParams)

-- Default kernel = Linear (linear SVM). yLab in {0,1}.
let svm  = SVM.fitSVM SVM.defaultSVM xMat yLab
    yhat = SVM.predictSVM svm xMat

-- Switch kernel via config (RBF / Poly / Linear). gamma = 1/(2 l^2).
let rbf  = SVM.defaultSVM { SVM.svmKernel = RBF
                          , SVM.svmParams = defaultKernelParams { kpLengthScale = 1.0 } }
    svmM = SVM.fitSVMMulti rbf xMat yMulti     -- multiclass one-vs-rest

-- k-fold CV grid search over C / kernel / l (deterministic; GridSearchCV-style).
let (bestCfg, cvAcc) = SVM.tuneSVM SVM.defaultSVM SVM.defaultSVMTuneGrid xMat yMulti
```

A single dual C-SVC (hinge loss) solved by SMO (Platt 1998, deterministic / no
RNG). The kernel is chosen via `svmKernel` from the shared kernel vocabulary
(`Hanalyze.Model.Kernel`: `Linear` / `Poly d` / `RBF` / `Matern52` /
`Periodic`), exactly as GP / KRR do; hyperparameters live in `svmParams ::
KernelParams` (γ derived from `kpLengthScale` as `γ = 1/(2ℓ²)`; no observation
noise σ_n², so SVM depends on `KernelParams`, not `GPParams` — Phase 75.18). The
dual form yields true sparse support vectors (α>0). Multiclass via one-vs-rest
with max-score voting. The earlier standalone linear L2-SVM module was removed —
use `svmKernel = Linear` for a linear boundary. `tuneSVM` / `SVMTuneGrid` add CV
hyperparameter selection (Phase 75.20). The module and types dropped the "Kernel"
prefix (`Model.SVM` / `SVMConfig` / `fitSVM` …) in Phase 75.19.

---

## 3. MDS (34-A3)

```haskell
import qualified Hanalyze.Stat.MDS as MDS

let d   = MDS.euclideanDist xMat
    emb = MDS.mdsClassical d 2
    embS = MDS.mdsSammon MDS.defaultSammonConfig d 2
```

Classical (Torgerson): B = -1/2 H D² H, eigendecompose, take top k positive
eigenvalues. Sammon: gradient descent on stress, classical MDS as init.

---

## 4. k-NN (34-A4)

```haskell
import qualified Hanalyze.Model.KNN as KNN

let knnR = KNN.fitKNNR 5 xTrain yTrain
    yR   = KNN.predictKNNR knnR xTest

let knnC = KNN.fitKNNC 5 xTrain yClsTrain
    yC   = KNN.predictKNNC knnC xTest
```

`KNNClassifier` is `Plottable` (`toPlot` = label-coloured scatter of the
training points), and the shared `decisionBoundaryOf` / `confusionOf`
helpers draw the two figures below. **Note (Phase 75.22)**: the filled
decision *region* of `decisionBoundaryOf` is currently **not implemented**
(a continuous-axis tile/raster mark is pending — see the plot backlog
phase); it emits a "not implemented" annotation instead of the earlier
striped square-scatter fill. Overlay data points via `<> toPlot clf`. For
models with `ScorePredict` (SVM) use `decisionLineOf` for a clean boundary
line today.

```haskell
import Hanalyze.Plot       (toPlot, decisionBoundaryOf, confusionOf)
import Hgg.Plot.Spec          (ColData (..))
import Hgg.Plot.Frame         ((|>>))
import Hgg.Plot.Backend.SVG   (saveSVGBound)

let noDf = [] :: [(Text, ColData)]
-- decision boundary over a 2-feature grid, with the training points on top
saveSVGBound "knn-decision-boundary.svg"
  (noDf |>> (decisionBoundaryOf knnC (xlo, xhi) (ylo, yhi) 80 <> toPlot knnC))
-- confusion matrix heatmap on held-out data
saveSVGBound "knn-confusion.svg" (noDf |>> confusionOf knnC xTest yClsTest)
```

Brute force Euclidean (O(n_test · n_train · d)). KD-tree out of scope.

Majority vote among the `k` nearest neighbours produces a piecewise
decision boundary over the feature plane:

![k-NN decision boundary](../images/knn-decision-boundary.svg)

The confusion matrix on held-out data summarises classification accuracy
per class:

![k-NN confusion matrix heatmap](../images/knn-confusion.svg)

---

## 5. Naive Bayes (34-A5)

```haskell
import qualified Hanalyze.Model.NaiveBayes as NB

let gnb = NB.fitGNB xMat yLab
    yh  = NB.predictNB (NB.NBGaussian gnb) xMat

let mnb = NB.fitMNB 1.0 xCount yLab
    yh  = NB.predictNB (NB.NBMultinomial mnb) xCount
```

`NBModel` is `Plottable`: `toPlot (NB.NBGaussian gnb)` draws the class-mean
scatter, and `toPlot (NB.NBMultinomial mnb)` the class-prior bar:

```haskell
import Hanalyze.Plot       (toPlot)
import Hgg.Plot.Spec          (ColData (..))
import Hgg.Plot.Frame         ((|>>))
import Hgg.Plot.Backend.SVG   (saveSVGBound)

let noDf = [] :: [(Text, ColData)]
saveSVGBound "nb-class-means.svg" (noDf |>> toPlot (NB.NBGaussian gnb))
```

Gaussian per-feature Gaussians with sklearn-compatible var smoothing.
Multinomial with Laplace α. `predictNBLogProbs` returns log posteriors
normalised via log-sum-exp.

---

## 6. Mention-only (already implemented)

| Feature | Existing API |
|---|---|
| Random Forest classifier | `Hanalyze.Model.RandomForestClassifier` (Phase 13.5) |
| MLP neural net | `Hanalyze.Model.NeuralNetwork` (Phase 16) |

---

## 7. Out of scope / future work

* Multiclass GBM (softmax with K trees per round).
* k-NN KD-tree / Ball tree for high-d or large-n workloads.
* Bernoulli Naive Bayes (binary features); use Multinomial as a substitute.
