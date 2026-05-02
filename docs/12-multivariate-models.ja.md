# 多次元出力モデルの使い方

> 多目的最適化や複数応答の同時推定で使う多変量回帰モデル群。
> 理論は [docs/learn/08-multivariate-theory.ja.md](learn/08-multivariate-theory.ja.md) を参照。

## モジュール早見表

| モジュール | 用途 |
|---|---|
| `Model.MultiLM`        | 多変量線形回帰 (Y = XB + E) + 残差共分散 Σ |
| `Model.Spline` (`Multi*`) | 多出力スプライン回帰 |
| `Model.Kernel` (`Multi*`) | 多出力 Kernel Ridge |
| `Model.Regularized` (`Multi*`) | Ridge/Lasso/EN を q 列に拡張 |
| `Model.Multivariate`   | RRR / PLS / CCA |
| `Model.MultiGP`        | 多出力 GP (Independent GPs) |

---

## 1. Multivariate Linear Regression

```haskell
import Model.MultiLM

let mf = fitMultiLM xMat yMat   -- X (n×p), Y (n×q)
let yPred = predictMultiLM mf xNewMat
let sigma = mfResidCov mf       -- 残差共分散 q×q
let corr  = mfResidCor mf       -- 残差相関 q×q
```

API:
- `fitMultiLM :: Matrix -> Matrix -> MultiFit`
- `predictMultiLM :: MultiFit -> Matrix -> Matrix`
- `mfResidCov`, `mfResidCor`: 残差の共分散 / 相関

---

## 2. 多出力 Spline / Kernel / Regularized

各モジュールに `Multi` 版を提供:

```haskell
import Model.Spline (fitSplineMulti, predictSplineMulti)
import Model.Kernel (kernelRidgeMulti, predictKernelRidgeMulti)
import Model.Regularized (fitRegularizedMulti, predictRegularizedMulti, Penalty (..))

-- Spline (q 出力)
let sf = fitSplineMulti (BSpline 3) knots xs yMat
let yPred = predictSplineMulti sf xsNew

-- Kernel Ridge
let kr = kernelRidgeMulti Gaussian 0.1 0.05 xs yMat
let yPred = predictKernelRidgeMulti kr xsNew

-- Regularized
let rf = fitRegularizedMulti (L1 0.05) xMat yMat
let yPred = predictRegularizedMulti rf xNewMat
```

---

## 3. RRR / PLS / CCA

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

```haskell
import Model.MultiGP

let res = fitMultiGP RBF trainX trainYs testX
-- mgpMean, mgpLower, mgpUpper, mgpModels
```

各出力で独立に GP fit (Independent GPs)。

---

## 5. demo

```bash
cabal run multilm-demo         # B̂ ≈ B、Σ̂ ≈ Σ_true
cabal run multivariate-demo    # RRR/PLS/CCA で rank 1 構造を検出
```
