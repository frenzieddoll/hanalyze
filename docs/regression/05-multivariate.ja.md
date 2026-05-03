# 多出力モデルの使い方

> 🌐 [English](05-multivariate.md) | **日本語**

> 多目的最適化や複数応答の同時推定で使う多出力回帰モデル群。
> 理論は [docs/regression/theory-multivariate.ja.md](theory-multivariate.ja.md) を参照。

## 設計方針 (Phase M1-M8)

hanalyze の主要回帰モデルはすべて **「多出力 (Y :: Matrix n×q) を主 API、
1 出力は 1 列行列化して委譲する薄いラッパ」** という統一ポリシで実装されています。

- 共通基盤: `Model.MultiOutput` (`asMultiY` / `fromMultiY` / `r2Multi` / `rmseMulti` / `mseMulti`)
- 各モデルに `fitXMulti` / `XFitMulti` 系列を提供
- q=1 と q>1 で旧 API と数値一致 (テスト 10 件で検証済)

## モジュール早見表

| モジュール | 単出力 | 多出力 (主 API) |
|---|---|---|
| `Model.LM`           | `fitLMVec`            | `fitLM` (元から多出力対応) |
| `Model.MultiLM`      | —                     | `fitMultiLM` + 残差共分散 Σ |
| `Model.GLM`          | `fitGLM`              | `fitGLMMulti` (列ごと IRLS) |
| `Model.GLMM`         | `fitLME` / `fitGLMM`  | `fitLMEMulti` / `fitGLMMMulti` |
| `Model.Spline`       | `fitSpline`           | `fitSplineMulti` (基底共有) |
| `Model.Kernel`       | `kernelRidge` / `nwRegression` | `kernelRidgeMulti` / `nwRegressionMulti` |
| `Model.Regularized`  | `fitRegularized`      | `fitRegularizedMulti` (Ridge/OLS は閉形式行列) |
| `Model.RFF`          | `rffRidge` / `rffRidgeMV` | `rffRidgeMulti` / `rffRidgeMVMulti` |
| `Model.GP`           | `fitGP`               | `fitGPMulti` (Ky⁻¹ 共有、分散も共有) |
| `Model.GPRobust`     | `fitGPRobust`         | `fitGPRobustMulti` (列ごと IRLS、K 共有) |
| `Model.MultiGP`      | —                     | `fitMultiGP` (出力ごと別 HP の Independent GPs) |
| `Model.Multivariate` | —                     | RRR / PLS / CCA |
| `Model.HBM`          | `observe` (1 列)      | `observeColumns` ヘルパ + `MvNormal` 観測 |

---

## 1. Multivariate Linear Regression (`Model.MultiLM`)

残差共分散 Σ も推定したい場合の専用 API:

```haskell
import Model.MultiLM

let mf    = fitMultiLM xMat yMat   -- X (n×p), Y (n×q)
let yPred = predictMultiLM mf xNewMat
let sigma = mfResidCov mf          -- 残差共分散 q×q
let corr  = mfResidCor mf          -- 残差相関 q×q
```

API:
- `fitMultiLM :: Matrix -> Matrix -> MultiFit`
- `predictMultiLM :: MultiFit -> Matrix -> Matrix`
- `mfResidCov`, `mfResidCor`: 残差の共分散 / 相関

---

## 2. 多出力 Spline / Kernel / Regularized / RFF

すべて行列形式 1 回求解 (Ridge/OLS) または共有計算 + 列ごと反復 (Lasso/EN/IRLS):

```haskell
import Model.Spline      (fitSplineMulti, predictSplineMulti, BSpline (..))
import Model.Kernel      (kernelRidgeMulti, predictKernelRidgeMulti
                         , autoTuneKernelRidgeMulti, defaultHGrid, defaultLamGrid)
import Model.Regularized (fitRegularizedMulti, predictRegularizedMulti, Penalty (..))
import Model.RFF         (rffRidgeMulti, predictRFFRidgeMulti, sampleRFFRBF)

-- Spline (q 出力同時)
let sf    = fitSplineMulti (BSpline 3) knots xs yMat
let yPred = predictSplineMulti sf xsNew

-- Kernel Ridge (Cholesky 1 回で q 出力)
let kr    = kernelRidgeMulti Gaussian 0.1 0.05 xs yMat
let yPred = predictKernelRidgeMulti kr xsNew

-- LOOCV 解析解で h, λ を自動決定
let (krBest, h*, λ*, looMSE) =
      autoTuneKernelRidgeMulti Gaussian xs yMat
        (defaultHGrid xs) defaultLamGrid

-- Regularized (Ridge は閉形式 B = (X'X+λI)⁻¹ X'Y)
let rf    = fitRegularizedMulti (L2 0.1) xMat yMat
let yPred = predictRegularizedMulti rf xNewMat

-- RFF Ridge (1D 入力)
gen <- createSystemRandom
rff <- sampleRFFRBF 100 1.0 1.0 gen
let mf    = rffRidgeMulti rff xList yMat 0.01
let yPred = predictRFFRidgeMulti mf xListNew
```

---

## 3. RRR / PLS / CCA (`Model.Multivariate`)

```haskell
import Model.Multivariate

-- RRR (rank 制約)
let rrr = reducedRankRegression r xMat yMat
let yPred = predictRRR rrr xNew

-- PLS (k 成分)
let pls' = pls k xMat yMat
let yPred = predictPLS pls' xNew

-- CCA
let ccaFit = cca xMat yMat
-- ccaCorr ccaFit: 各 canonical correlation
-- ccaA, ccaB:    各サイドの基底 (n × r)
```

---

## 4. Multi-output GP

### 共有カーネル版 (高速、`Model.GP.fitGPMulti`)
全 q 出力で同じカーネルとハイパーパラメータを共有 → Cholesky 1 回:

```haskell
import qualified Model.GP as GP

let model = GP.GPModel GP.RBF GP.defaultGPParams
let (meanMat, varList) = GP.fitGPMulti model trainX trainYMat testX
-- meanMat :: Matrix (m × q)
-- varList :: [Double] (length m, q 出力で共有)
```

### 出力ごと独立 HP 版 (`Model.MultiGP`)
各出力に独自のハイパーパラメータを学習したい場合:

```haskell
import Model.MultiGP

let res = fitMultiGP RBF trainX trainYs testX
-- mgpMean, mgpLower, mgpUpper, mgpModels
```

---

## 5. Robust Multi-output GP (`Model.GPRobust`)

StudentT / Cauchy 観測尤度で外れ値耐性、列ごと IRLS:

```haskell
import qualified Model.GP as GP
import qualified Model.GPRobust as GPR

let mf = GPR.fitGPRobustMulti GP.RBF GP.defaultGPParams
                              (GPR.RStudentT 4 0.3) trainX trainYMat
let (meanMat, varList) = GPR.predictGPRobustMulti mf testX
```

---

## 6. CLI: `hanalyze multireg`

wide CSV (1 行 = 入力 1 値、列 = 出力タスク) を直接渡せる:

```bash
hanalyze multireg data.csv x 'y_*' \
    --method kernel-rbf --auto-hp \
    --xaxis 'z [nm]' --xaxis-min 0 --xaxis-max 200 \
    --report out.html
```

入力スライダで全 q 予測値を JS が即時再計算する対話的 HTML を生成。
詳細は [io/02-multireg.ja.md](../io/02-multireg.ja.md)。

---

## 7. デモ

```bash
cabal run multilm-demo                  # B̂ ≈ B、Σ̂ ≈ Σ_true
cabal run multivariate-demo             # RRR/PLS/CCA で rank 1 構造を検出
cabal run potential-multiout-demo       # 多出力 OLS、対話的 HTML
cabal run potential-multikr-demo        # 多出力 RBF Kernel Ridge + LOOCV 自動 HP
```
