# Multivariate Analysis

> 🌐 **English** | [日本語](04-multivariate.ja.md)

> [📚 Index](README.md) | [01 quickstart](01-quickstart.md) | [02 regression](02-regression.md) | [03 bayesian-hbm](03-bayesian-hbm.md) | **04 multivariate** | [05 ml](05-ml.md) | [06 timeseries](06-timeseries.md) | [07 survival](07-survival.md) | [08 causal](08-causal.md) | [09 doe](09-doe.md) | [10 stat](10-stat.md) | [11 data](11-data.md) | [12 plot](12-plot.md)

Dimension reduction, multivariate regression, and clustering: signatures + minimal examples + plots. These do not carry the `df |-> spec` verb, so fit on matrices and render results with `toPlot` (or specialized plot functions). Theory is documented in [`docs/regression/`](../regression/) and [`docs/stat/`](../stat/).

| Technique | High-level (`df \|->`) | Result type | Plot |
|---|---|---|---|
| PCA | `df \|-> pcaOf std mK cols` | `PCAResult` (Plottable) | scree |
| PLS | `df \|-> plsOf cfg xcols ycols` | `PLSFit` (Plottable=score) | score / loading / VIP |
| CCA | `df \|-> ccaOf xcols ycols` | `CCAFit` (not Plottable) | — |
| Multivariate regression (RRR etc) | `MultiFit` series | `MultiFit` (Plottable) | residual correlation heatmap |
| Discriminant analysis (LDA) | `df \|-> ldaOf cols clsCol` | `DiscriminantFit` (Plottable) | decision boundary |
| k-means | `df \|-> kmeansOf cfg seed cols` | `KMeansResult` (Plottable) | cluster scatter |
| Hierarchical clustering | `Hanalyze.Model.HierarchicalCluster` | `HClusterFit` (Plottable) | dendrogram |
| Functional data analysis (FDA) | `Hanalyze.Model.FDA` | `FunctionalPCA` / `FLMResult` (Plottable) | eigenfunctions / β(t) |

---

## PCA

**High-level** (by column names; `df |->`, Phase 70.A):

```haskell
pcaOf :: PCAStandardize -> Maybe Int -> [Text] -> PCASpec
--       standardization approach        retained components k    variable columns
```

```haskell
let res = df |-> pcaOf CenterScale (Just 3) ["x1","x2","x3"]   -- PCAResult
saveSVG "scree.svg" $ toPlot res                                -- scree plot of variance explained (data-free plot)
```

![PCA scree plot](../images/pca-scree.svg)

> **Standardization approach** (`PCAStandardize`) = column preprocessing before SVD: `NoStandardize` (nothing) /
> `Center` (subtract column mean; PCA default) / `CenterScale` (subtract mean and divide by SD = correlation matrix PCA).
> If variables have different units, use `CenterScale`.
>
> **Why use `saveSVG`**: Plots like PCA scree that do not overlay data columns are saved directly with `saveSVG (toPlot res)` (`noDf |>>` not needed). Use `df |>> (layer (scatter …) <> toPlot …)` only when overlaying on a scatter plot.

**Low-level** (matrix API): `pca :: PCAStandardize -> Maybe Int -> LA.Matrix Double -> PCAResult`.
Use `pcaTransform` / `pcaInverse` / `pcaCumExplained` for projection / reconstruction / cumulative explained variance.

---

## PLS (Partial Least Squares)

**PLS is regression** (X p-dimensional → Y q-dimensional). The fit result `PLSFit` holds regression coefficients `plsCoef :: Matrix (p×q)`, and `predictPLS :: PLSFit -> Matrix -> Matrix` produces ŷ.

**High-level** (by column names; `df |->`, Phase 70.A):

```haskell
plsOf :: PLSConfig -> [Text] -> [Text] -> PLSSpec     -- X columns, Y columns specified separately
```

```haskell
let m = df |-> plsOf defaultPLSConfig ["x1","x2"] ["y1","y2"]   -- PLSFit
saveSVG "pls.svg" $ toPlot m                                     -- representative plot = score plot
```

`defaultPLSConfig` has k=2, NIPALS, **scale=True** (X, Y standardized column-wise).

**Diagnostic plots**: `PLSFit` is `Plottable` with `toPlot = scoreView` (representative = score). Score / loading / VIP are unified under intermediate Plottable spec `PLSView` (Phase 70.B); `toPlot` renders them:

```haskell
scoreView, loadingView, vipView :: PLSFit -> PLSView   -- PLSView is Plottable

saveSVG "score.svg" $ toPlot (scoreView   m)   -- = toPlot m (representative)
saveSVG "loading.svg" $ toPlot (loadingView m)
saveSVG "vip.svg" $ toPlot (vipView     m)
```

![PLS score](../images/pls-score.svg)

![PLS loading](../images/pls-loading.svg)

![PLS VIP](../images/pls-vip.svg)

> Score / loading / VIP are **latent space diagnostics**, not the regression lines themselves (PLS typically multi-input, multi-output).

**Effect plots** (Phase 70.B): To see regression as "how does predicted ŷ move when an input changes", compose frame-preserving wrapper `plsModel` with `statModelMulti` (same `along` / `holdAt` / `byVar` as LM/GLM).
Multi-output uses `selectOutput "y2"` to choose which Y to plot:

```haskell
plsModel     :: PLSConfig -> [Text] -> [Text] -> d -> Either String PLSModel  -- ColumnSource d
selectOutput :: Text -> PLSModel -> PLSModel                                   -- select Y output to plot

let Right pm = plsModel defaultPLSConfig ["x1","x2"] ["y1","y2"] df
-- vary x1, fix others at median; effect curve for second output y2:
saveSVG "pls-effect.svg"
  $ toPlot (statModelMulti (selectOutput "y2" pm) (along "x1") <> holdAt Median)
```

> PLS lacks closed-form CI, so effect curves are **bands not provided** (μ̂ line only; same as GAM).
> **Low-level**: matrix version `fitPLS :: PLSConfig -> X -> Y -> Either Text PLSFit` /
> `pls :: Int -> X -> Y -> PLSFit` / `predictPLS :: PLSFit -> Matrix -> Matrix`.

---

## CCA / Multivariate regression

**High-level** (by column names; `df |->`, Phase 70.A):

```haskell
ccaOf :: [Text] -> [Text] -> CCASpec     -- df |-> ccaOf ["x1","x2"] ["y1","y2"]
```

```haskell
let m = df |-> ccaOf ["x1","x2"] ["y1","y2"]   -- CCAFit (ccaCorr = canonical correlations)
```

`CCAFit` is currently not `Plottable` (retrieve `ccaCorr` / `ccaScoresX` etc as numbers). **Low-level**:
`cca :: LA.Matrix Double -> LA.Matrix Double -> CCAFit`.

True multi-output regression (`MultiFit`) is `Plottable` and can render residual correlation heatmaps.

![multivariate regression residual correlation](../images/multilm-resid-corr.svg)

→ [05-multivariate](../regression/05-multivariate.md) / [07-multireg](../regression/07-multireg.md)

---

## Discriminant analysis (LDA)

**High-level** (by column names; `df |->`, Phase 70.A; class column is rounded to integer):

```haskell
ldaOf :: [Text] -> Text -> LDASpec       -- df |-> ldaOf ["x1","x2"] "class"
```

```haskell
let m = df |-> ldaOf ["x1","x2"] "class"   -- DiscriminantFit
```

`DiscriminantFit` is `Plottable`. Decision boundary via the classifier-generic `decisionBoundaryOf` (⚠ region fill is not yet implemented; see note [05-ml](05-ml.md#k-nn)).
**Low-level**: `fitLDA :: LA.Matrix Double -> V.Vector Int -> Either Text DiscriminantFit`.

```haskell
case fitLDA xMat yInt of
  Right fit -> saveSVGBound "lda.svg"
    $ noDf |>> decisionBoundaryOf fit (xlo, xhi) (ylo, yhi) 80 <> toPlot fit
  Left err  -> putStrLn (T.unpack err)
```

![LDA decision boundary](../images/lda-decision-boundary.svg)

---

## k-means clustering

**High-level** (`df |->`, Phase 70.A): `kmeansOf :: KMeansConfig -> Word32 -> [Text] -> KMeansSpec`
(second arg = random seed).

```haskell
defaultKMeansConfig :: Int -> KMeansConfig    -- arg = cluster count k
```

```haskell
let res = df |-> kmeansOf (defaultKMeansConfig 3) 42 ["x1","x2"]  -- KMeansResult
-- plot data points colored by cluster (clusterScatterOf) + centroid as ✚ (centroidsOf) overlaid:
saveSVGBound "kmeans.svg" $ df |>> clusterScatterOf df res "x1" "x2" <> centroidsOf res 0 1
```

> k-means uses randomness internally. To fit within `df |->` (pure), it calls the seed-purified version
> `kMeansPure :: KMeansConfig -> LA.Matrix Double -> Word32 -> KMeansResult` (same seed → bit-identical,
> same principle as HBM's `nutsPure`). The IO version `kMeans` is also available as before.

![k-means clusters](../images/kmeans-clusters.svg)

→ [05-cluster](../stat/05-cluster.md)

---

## Hierarchical clustering

`Hanalyze.Model.HierarchicalCluster` for agglomerative clustering. `HClusterFit` is `Plottable` (`toPlot` = dendrogram). The tree diagram renders U-shaped links via custom plot marks (Phase 48; `hgg-custom` `dendrogramMark`).
→ [05-cluster](../stat/05-cluster.md)

---

## Functional data analysis (FDA, `Hanalyze.Model.FDA`)

Sensor / process time series treated as **1 observation = 1 function** in the Ramsay-Silverman style. Smooth with B-splines + P-spline penalty; fit functional PCA (FPCA) or functional linear regression (fLM). Smoothing solutions, mass matrices, and knot traps are documented in [fda/usage-fda](../fda/usage-fda.md).

```haskell
data Basis = BSpline Int [Double]    -- degree, knots (★ includes boundary knots in full knot sequence)
smoothBasis    :: Basis -> Double -> LA.Vector Double -> LA.Matrix Double -> [FunctionalDatum]
--                basis    λ(penalty)  t grid             samples (n×n_grid)   → list of smoothed functions
evalFunctional :: FunctionalDatum -> LA.Vector Double -> LA.Vector Double      -- evaluate at arbitrary grid
```

**FPCA** (`functionalPCA`) — SVD in function space. `FunctionalPCA` is `Plottable`
(`toPlot` = mean function + top eigenfunctions):

```haskell
functionalPCA :: Int -> [FunctionalDatum] -> FunctionalPCA   -- top K components
-- fpcaEigenvalues (descending) / fpcaEigenfn (K×n_grid) / fpcaScores (n×K) / fpcaMeanFn
saveSVGBound "fda-fpca.svg" $ noDf |>> toPlot (functionalPCA 3 fits)
```

![FPCA mean function and top eigenfunctions (PC1/PC2/PC3 + mean)](../images/fda-fpca.svg)

**fLM** (`fLM`) — functional linear regression `y_i = α + ∫ x_i(t) β(t) dt + ε`. `FLMResult` is `Plottable` (`toPlot` = coefficient curve β(t)):

```haskell
fLM :: [FunctionalDatum] -> LA.Vector Double -> Double -> FLMResult   -- fits, y, λβ
-- flmAlpha (α̂) / flmBetaFn (β̂(t) grid) / flmFitted (ŷ) / flmR2
```
