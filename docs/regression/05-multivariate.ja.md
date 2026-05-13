# 多出力モデルの使い方

> 🌐 [English](05-multivariate.md) | **日本語**

> 多目的最適化や複数応答の同時推定で使う多出力回帰モデル群。
> 理論は [docs/regression/theory-multivariate.ja.md](theory-multivariate.ja.md) を参照。

## 設計方針

hanalyze の主要回帰モデルはすべて **「多出力 (Y :: Matrix n×q) を主 API、
1 出力は 1 列行列化して委譲する薄いラッパ」** という統一ポリシで実装されています。

- 共通基盤: `Hanalyze.Model.MultiOutput` (`asMultiY` / `fromMultiY` / `r2Multi` / `rmseMulti` / `mseMulti`)
- 各モデルに `fitXMulti` / `XFitMulti` 系列を提供
- q=1 と q>1 で旧 API と数値一致 (テスト 10 件で検証済)

## モジュール早見表

| モジュール | 単出力 | 多出力 (主 API) |
|---|---|---|
| `Hanalyze.Model.LM`           | `fitLMVec`            | `fitLM` (元から多出力対応) |
| `Hanalyze.Model.MultiLM`      | —                     | `fitMultiLM` + 残差共分散 Σ |
| `Hanalyze.Model.GLM`          | `fitGLM`              | `fitGLMMulti` (列ごと IRLS) |
| `Hanalyze.Model.GLMM`         | `fitLME` / `fitGLMM`  | `fitLMEMulti` / `fitGLMMMulti` |
| `Hanalyze.Model.Spline`       | `fitSpline`           | `fitSplineMulti` (基底共有) |
| `Hanalyze.Model.Kernel`       | `kernelRidge` / `nwRegression` | `kernelRidgeMulti` / `nwRegressionMulti` |
| `Hanalyze.Model.Regularized`  | `fitRegularized`      | `fitRegularizedMulti` (Ridge/OLS は閉形式行列) |
| `Hanalyze.Model.RFF`          | `rffRidge` / `rffRidgeMV` | `rffRidgeMulti` / `rffRidgeMVMulti` |
| `Hanalyze.Model.GP`           | `fitGP`               | `fitGPMulti` (Ky⁻¹ 共有、分散も共有) |
| `Hanalyze.Model.GPRobust`     | `fitGPRobust`         | `fitGPRobustMulti` (列ごと IRLS、K 共有) |
| `Hanalyze.Model.MultiGP`      | —                     | `fitMultiGP` / `fitMultiGPMV` (shared HP, RBF, デフォルト) + `fitMultiGPIndep` / `fitMultiGPMVIndep` (出力ごと別 HP) |
| `Hanalyze.Model.Multivariate` | —                     | RRR / PLS / CCA |
| `Hanalyze.Model.HBM`          | `observe` (1 列)      | `observeColumns` ヘルパ + `MvNormal` 観測 |

---

## 1. Multivariate Linear Regression (`Hanalyze.Model.MultiLM`)

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

## 3. RRR / PLS / CCA (`Hanalyze.Model.Multivariate`)

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

3 系統あり、出力間で HP をどう共有するかで選ぶ:

| API | HP 最適化回数 | カーネル | 用途 |
|---|---|---|---|
| `GP.fitGPMulti` / `GP.fitGPMVMulti` | 0 (HP は外部指定) | 任意 | HP が既知、Cholesky 共有で q 出力を一括予測 |
| `MultiGP.fitMultiGP` / `fitMultiGPMV` (★デフォルト) | **1 回** (合算尤度 + Cholesky 共有) | RBF のみ | sklearn 流: 等質な q 出力。`q > 1` で per-output 比 ~q× 速い |
| `MultiGP.fitMultiGPIndep` / `fitMultiGPMVIndep` | **q 回** (各出力独立) | 任意 (RBF / Matérn52 / Periodic) | 出力ごとに別の length-scale や noise が必要 |

いずれも出力間相関 (Co-kriging / LMC) は実装しない (`B = I` 固定)。

### Shared HP (デフォルト, RBF, `Hanalyze.Model.MultiGP.fitMultiGP`)
全 q 出力で 1 セットの HP を **合算周辺尤度** `Σ_q log p(y_q | θ)` の最大化で学習し、`Ky = K + σ_n² I` の Cholesky を 1 回だけ計算して q 出力で再利用する。sklearn の `GaussianProcessRegressor.fit(X, Y::(n,q))` 相当:

```haskell
import Hanalyze.Model.MultiGP

let res = fitMultiGP trainX trainYs testX
-- mgpMean, mgpLower, mgpUpper, mgpModels (q 個すべて同一)
```

多入力版 (X が `n × p` matrix):

```haskell
let resMV = fitMultiGPMV trainX trainYs testX
-- mgpmvMean :: [Vector Double], mgpmvModels :: [GPModel] (q 個同一)
```

### Cholesky 共有 (HP 既知、`Hanalyze.Model.GP.fitGPMulti`)
事前に HP を確定済みのとき。MCMC で HP を固定したい用途等:

```haskell
import qualified Hanalyze.Model.GP as GP

let model = GP.GPModel GP.RBF GP.defaultGPParams
let (meanMat, varList) = GP.fitGPMulti model trainX trainYMat testX
-- meanMat :: Matrix (m × q)
-- varList :: [Double] (length m, q 出力で共有)
```

### 出力ごと独立 HP (任意カーネル, `Hanalyze.Model.MultiGP.fitMultiGPIndep`)
各出力に独自の length-scale / noise を学習させたい、または Matérn 5/2 や Periodic kernel を使いたい場合:

```haskell
import Hanalyze.Model.MultiGP

let res = fitMultiGPIndep RBF trainX trainYs testX
-- mgpModels は出力ごとに別の GPParams を持つ
```

---

## 5. Robust Multi-output GP (`Hanalyze.Model.GPRobust`)

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
詳細は [07-multireg.ja.md](07-multireg.ja.md)。

---

## 7. デモ

```bash
cabal run multilm-demo                  # B̂ ≈ B、Σ̂ ≈ Σ_true
cabal run multivariate-demo             # RRR/PLS/CCA で rank 1 構造を検出
cabal run potential-multiout-demo       # 多出力 OLS、対話的 HTML
cabal run potential-multikr-demo        # 多出力 RBF Kernel Ridge + LOOCV 自動 HP
```
