# plot 連携

> [📚 索引](README.md) ｜ [01 quickstart](01-quickstart.md) ｜ [02 regression](02-regression.md) ｜ [03 bayesian-hbm](03-bayesian-hbm.md) ｜ [04 multivariate](04-multivariate.md) ｜ [05 ml](05-ml.md) ｜ [06 timeseries](06-timeseries.md) ｜ [07 survival](07-survival.md) ｜ [08 causal](08-causal.md) ｜ [09 doe](09-doe.md) ｜ [10 stat](10-stat.md) ｜ [11 data](11-data.md) ｜ **12 plot**

fit 済みモデルを hgg の layer 文法に載せるブリッジ。 **描画文法そのもの**
(layer / mark / scale / theme / facet / backend) は
[hgg API リファレンス](../../../hgg/docs/api-guide/README.md) が一次根拠。
ここは analyze 側の `toPlot` / 抽出子だけを扱う。

---

## 描画の組み立て

```haskell
toPlot :: Plottable m => m -> VisualSpec        -- モデル → 図 (layer)
(|>>)  :: ColumnSource d => d -> VisualSpec -> BoundPlot   -- データを図に束ねる
```

データと重畳して保存する基本形 (`VisualSpec` は `Monoid` なので `<>` で重ねる):

```haskell
saveSVGBound "out.svg" $ df |>> layer (scatter "x" "y") <> toPlot fit
```

保存関数は `saveSVGBound` / `saveSVGBoundStats` × SVG/PDF/PNG
([plot 05 backends](../../../hgg/docs/api-guide/05-backends.md))。

---

## `Plottable` なモデル一覧

`toPlot` がそのまま使える結果型 (各モデルの詳細は対応ページへ):

| 分野 | Plottable 型 |
|---|---|
| 回帰 ([02](02-regression.md)) | `LMModel` / `GLMModel` / `WeightedLMModel` / `RobustModel` / `QuantileModel` / `SplineModel` / `GAMModel` / `GPResult` / `RegFit` / `GLMMResultRE` |
| ベイズ ([03](03-bayesian-hbm.md)) | `HBMModel` 抽出子 (下記) / `ChainModel` |
| 多変量 ([04](04-multivariate.md)) | `PCAResult` / `PLSFit` / `MultiFit` / `DiscriminantFit` / `KMeansResult` |
| ML ([05](05-ml.md)) | `RandomForest` / `GBRegressor` / `GBClassifier` / `DTree` / `KNNClassifier` / `NBModel` |
| 時系列・生存 ([06](06-timeseries.md)/[07](07-survival.md)) | `ForecastModel` / `GARCHFit` / `KMResult` / `CRFit` / `AFTFit` |
| 因果・検定 ([08](08-causal.md)/[10](10-stat.md)) | `DirectLiNGAMFit` / `TestResult` / `PCAResult` |

> `MultiLMModel` / `MultiGLMModel` は **`Plottable` で無い**。 effect plot は
> `statModelMulti m (along "x") <> holdAt Median` を `toPlot` する ([02 regression](02-regression.md#formula-dsl))。

---

## 抽出子 (`toPlot` の仲間)

単純な `toPlot` では足りないモデルの専用ビルダー。

| 抽出子 | 対象 | 用途 |
|---|---|---|
| `forestOf` / `tracesOf` / `ppcOf` / `epred` / `dagOf` | `HBMModel` | 事後 forest / trace / PPC / 期待値予測 / DAG ([03](03-bayesian-hbm.md)) |
| `plsScorePlot` / `plsLoadingPlot` / `plsVipPlot` | `PLSFit` | score / loading / VIP ([04](04-multivariate.md)) |
| `decisionBoundaryOf` / `confusionOf` | 分類器 (`ClassPredict`) | 決定境界 / 混同行列 ([05](05-ml.md)) |
| `testForest` / `testForestLabeled` / `describeBox` | `TestResult` / 生データ | 検定 forest / box plot ([10](10-stat.md)) |
| `aftSurvivalAt` | `AFTFit` | 任意共変量の生存曲線 ([07](07-survival.md)) |

---

## アーキテクチャ

`toPlot` / `Plottable` は cabal flag `plot-integration` 配下 (既定 off = standalone・
upstream portable / on = `Hanalyze.Plot` が `hgg-core` 等に依存)。 依存は
**一方向 `analyze → plot-core`**。 詳細は [visualization/03-plot-integration](../visualization/03-plot-integration.md)。

→ 描画文法の全体: [hgg API リファレンス](../../../hgg/docs/api-guide/README.md)
