# hanalyze-models

[`hanalyze`](../README.ja.md) の**モデル層**。 古典的な回帰から
機械学習・多変量解析・時系列・生存/信頼性・因果推論までの **model zoo** を
担う、 分割 6 層で最も大きい package (67 module)。

依存は `core` (数値基盤) / `frame` (dataframe interop) / `bayes` (MCMC) の
3 層 + 外部 12 package。 `-design` (DoE) と `-viz` はこの層の上に乗る。
Formula DSL のパーサを持つため `megaparsec` に依存するのはこの層だけ。

## 主要 module (全 67 module)

### 回帰の基本 (`Hanalyze.Model.*`)

| Module | 役割 |
|---|---|
| `Model.LM` / `Model.LM.Diagnostics` | 最小二乗回帰と残差診断 (てこ比 / Cook 距離 / VIF) |
| `Model.GLM` / `Model.GLMM` | IRLS による GLM (Gaussian / Binomial / Poisson を統一) と混合効果 GLM |
| `Model.MultiLM` | 多出力線形回帰 |
| `Model.Regularized` / `Model.RegularizedAdvanced` | Lasso / Ridge / Elastic Net + MCP / SCAD / Adaptive / Group。 λ は k-fold CV + 1-SE ルールで自動選択 |
| `Model.Robust` / `Model.Quantile` | M 推定によるロバスト回帰 / 分位点回帰 |
| `Model.Spline` / `Model.GAM` | スプライン平滑化 / 一般化加法モデル |

### Formula DSL (`Model.Formula.*`)

| Module | 役割 |
|---|---|
| `Model.Formula` | **Formula DSL 正本 front-end** の parser と AST (`y ~ x1 + x2*x3`) |
| `Model.Formula.Design` / `.Frame` | AST → model matrix / dataframe との結合 |
| `Model.Formula.Mixed` / `.Nonlinear` / `.RFormula` | 混合効果 / 非線形項 / R 互換 formula の解釈 |

### 多変量・次元圧縮

| Module | 役割 |
|---|---|
| `Model.PCA` | 主成分分析 (応答を使わない分散最大化) |
| `Model.PLS` | 部分最小二乗回帰 — 応答との共分散を最大化 + VIP + CV による成分数選択 |
| `Model.Discriminant` | 判別分析 (LDA = 線形境界 / QDA = 二次境界) |
| `Model.Multivariate` | RRR / PLS / CCA の多変量回帰族 |
| `Model.MDS` / `Model.FDA` | 多次元尺度構成法 / 関数データ解析 |
| `Model.Cluster` / `Model.HierarchicalCluster` / `Model.LatentClassAnalysis` | k-means / 階層クラスタリング / 潜在クラス分析 |

### 機械学習

| Module | 役割 |
|---|---|
| `Model.RandomForest` / `Model.RandomForestClassifier` | ランダムフォレスト (回帰 / 分類) |
| `Model.DecisionTree` / `Model.GradientBoosting` | 決定木 / 勾配ブースティング |
| `Model.SVM` / `Model.KNN` / `Model.NaiveBayes` / `Model.NeuralNetwork` | SVM / k 近傍 / 単純ベイズ / NN |
| `Model.Kernel` / `Model.KernelRegression` | カーネル関数群 / カーネル回帰 |
| `Model.PartialDependence` | PDP / ICE によるモデル解釈 |

### ガウス過程・多出力

| Module | 役割 |
|---|---|
| `Model.GP` / `Model.GPRobust` | GP 回帰 (RBF / Matérn / Periodic + ARD) / 外れ値に強い GP |
| `Model.MultiGP` / `Model.MultiOutput` | 多出力 GP / 多出力回帰の統合 API |
| `Model.RFF` | Random Fourier Features による大規模 GP 近似 |

### 時系列

| Module | 役割 |
|---|---|
| `Model.TimeSeries` | ARIMA 系の入口 |
| `Model.VAR` / `Model.GARCH` / `Model.StateSpace` | ベクトル自己回帰 / GARCH / 状態空間モデル (カルマンフィルタ) |

### 生存時間・信頼性

| Module | 役割 |
|---|---|
| `Model.Survival` | Kaplan-Meier / Cox 比例ハザード |
| `Model.AFT` / `Model.CompetingRisks` | 加速故障時間モデル / 競合リスク (CIF) |
| `Model.Weibull` | Weibull MLE (打ち切り対応) + B_p 寿命 + Wald 信頼区間 |
| `Model.Reliability` | 加速寿命試験 (Arrhenius / Eyring / Inverse Power Law) |
| `Model.ReliabilityBlockDiagram` | 直列 / 並列 / k-of-n の系統信頼度 |

### 因果推論

| Module | 役割 |
|---|---|
| `Model.LiNGAM.Direct` / `.ICA` / `.Pairwise` / `.Parce` | 非ガウス性を使った構造推定 (DirectLiNGAM / ICA-LiNGAM 等) |
| `Model.LiNGAM.VAR` / `.Bootstrap` / `.MultiGroup` | 時系列版 / ブートストラップ信頼度 / 多群同時推定 |
| `Model.DAG` | DAG の表現と探索 |
| `Stat.Causal.PropensityScore` / `.IPW` / `.DoublyRobust` / `.CATE` | 傾向スコア / IPW / 二重ロバスト推定 / 条件付き平均処置効果 |

### その他

| Module | 役割 |
|---|---|
| `Model.FitYByX` | JMP の "Fit Y by X" 相当 — 変数の型の組合せから手法を自動選択 |
| `Stat.ModelSelect` | AIC / BIC によるモデル選択 |
| `Optim.BayesOpt` | ベイズ最適化 (GP + 獲得関数)。 GP を使うためこの層に置く |

## 単体で使う

umbrella が不要なら、 この package を直接依存に書ける。 結果型
`FitResult` は core 層にあるため、 **`hanalyze-core` も明示的に要る**:

```cabal
build-depends: hanalyze-models, hanalyze-core, hmatrix
```

```haskell
import qualified Numeric.LinearAlgebra as LA
import Hanalyze.Model.Core (FitResult (..))
import Hanalyze.Model.LM   (fitLM)

main :: IO ()
main = do
  let x = LA.fromLists [ [1, 1.0], [1, 2.0], [1, 3.0], [1, 4.0], [1, 5.0] ]
      y = LA.fromLists [ [2.1], [3.9], [6.2], [7.8], [10.1] ]
      fit = fitLM x y
  print (LA.toLists (coefficients fit))
  print (LA.toList (rSquared fit))
  -- [[5.000000000000132e-2],[1.9899999999999998]]
  -- [0.9973053289009771]
```

切片列 (`1`) は自分で入れる。 formula から model matrix を作るなら
`Model.Formula` を使う。

なお、 通常は umbrella package `hanalyze` を依存に書けば
`import Hanalyze` だけで上記もすべて使える。 層を直接指定するのは
依存を最小化したいときのみで十分。

## 関連 docs

- 線形回帰: [docs/regression/01-lm.ja.md](../docs/regression/01-lm.ja.md) /
  GLM: [02-glm.ja.md](../docs/regression/02-glm.ja.md)
- 罰則付き回帰: [04-regularized.ja.md](../docs/regression/04-regularized.ja.md) /
  [usage-regularized-advanced.ja.md](../docs/regression/usage-regularized-advanced.ja.md)
- PLS: [usage-pls.ja.md](../docs/regression/usage-pls.ja.md) /
  判別分析 (LDA·QDA): [usage-discriminant.ja.md](../docs/regression/usage-discriminant.ja.md)
- Weibull / B_p 寿命: [usage-weibull.ja.md](../docs/regression/usage-weibull.ja.md) /
  加速寿命試験: [usage-reliability.ja.md](../docs/regression/usage-reliability.ja.md)
- 生存時間解析: [10-survival.ja.md](../docs/regression/10-survival.ja.md) /
  多出力: [05-multivariate.ja.md](../docs/regression/05-multivariate.ja.md)
- Formula DSL: [11-formula-dsl.ja.md](../docs/regression/11-formula-dsl.ja.md)
- 因果推論: [docs/causal/](../docs/causal/) /
  ベイズ最適化: [docs/optim/01-singleobj.ja.md](../docs/optim/01-singleobj.ja.md)

← [repository README](../README.ja.md)
