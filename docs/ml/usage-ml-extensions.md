# ML Extensions (Phase 34: GBM / SVM / MDS / k-NN / Naive Bayes)

> 🌐 **English** | [日本語](usage-ml-extensions.ja.md)

> Learning guide for the five representative machine learning algorithms bundled in Phase 34 (2026-05-29).
> Originated from 17 gap items + new ml-extensions addition. Type signatures, minimal examples, and 
> `df |->`/`toPlot` paths are documented in [api-guide 05-ml](../api-guide/05-ml.md) as the primary reference.
> This guide covers **formulations, implementation strategy, and scope decisions**. Mentioned for aggregation only:
> Random Forest classification (`RandomForestClassifier`) and MLP NN (`NeuralNetwork`) are existing.

---

## 0. Full Feature Map

| Feature | Type |
|---|---|
| Gradient Boosting | Regression + binary classification |
| Linear SVM | Binary + multiclass (OvR) |
| MDS | Classical + Sammon |
| k-NN | Regression + classification |
| Naive Bayes | Gaussian + Multinomial |

---

## 1. Gradient Boosting (34-A1)

The weak learner reuses `RandomForest.buildTreeV` (no bootstrap, mtry=d for full-data tree).
Loss: regression = squared error residuals, classification = log-loss (gradient = y - sigmoid(F)).
Accumulating split-gain contributions of each feature across all rounds yields feature importance ranking:

![Feature importance from gradient boosting](../images/gbm-importance.svg)

---

## 2. Linear SVM (34-A2)

L2-SVM (squared hinge) primal solved via analytical gradients passed to `Hanalyze.Optim.LBFGS`.
Internally converts y ∈ {-1, +1}. Multiclass handled via one-vs-rest. Kernel SVM (RBF, etc.) is out of scope.

---

## 3. MDS (34-A3)

* `mdsClassical`: Torgerson method. Construct centered matrix `B = -1/2 H D² H` via centering matrix H,
  eigendecompose with `eigSH`, use top k eigenvectors as coordinates.
* `mdsSammon`: Initialize with classical MDS, then minimize Sammon stress (weighted residuals emphasizing small distances)
  via gradient descent.

---

## 4. k-NN (34-A4)

Brute force Euclidean distance (O(n_test · n_train · d)). KD-tree out of scope.
Majority vote among the `k` nearest neighbors produces piecewise decision boundaries over the feature plane:

![k-NN decision boundary](../images/knn-decision-boundary.svg)

Confusion matrix on holdout data summarises classification accuracy per class:

![k-NN confusion matrix heatmap](../images/knn-confusion.svg)

---

## 5. Naive Bayes (34-A5)

* Gaussian: Per-class Gaussian for each feature `N(μ_j, σ²_j)`, sklearn-compatible variance smoothing.
* Multinomial: Laplace smoothing α (typical value 1.0), accumulate `log p(feature_j | c)`.
* Posterior log-probability for prediction normalized via log-sum-exp (sum of exponentials = 1).

---

## 6. Mention-only (existing)

| Feature | Existing API | Note |
|---|---|---|
| Random Forest classification | `Hanalyze.Model.RandomForestClassifier` | Phase 13.5 existing |
| MLP NN | `Hanalyze.Model.NeuralNetwork` | Phase 16 has fitMLPRegressor / fitMLPClassifier |

---

## 7. Out of scope / Future extensions

* Multiclass GBM (softmax + K trees per iteration) — add when needed.
* Kernel SVM (RBF / poly) — custom QP solver not worth ROI, use sklearn integration as alternative.
* k-NN KD-tree / Ball tree — add for high-dimensional or large-scale cases where brute force is impractical.
* Bernoulli Naive Bayes — for binary features; currently Multinomial can substitute.

---

## 8. Related

- Types, minimal examples, `df |->`/`toPlot` paths: [api-guide 05-ml](../api-guide/05-ml.md)
- Specification: `specification/phases/phase-34-ml-extensions.md`
