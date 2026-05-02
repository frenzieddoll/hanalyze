# 学習資料 8 — 多変量回帰の理論

## 1. 多変量線形回帰

$$ Y = XB + E $$

- $Y \in \mathbb{R}^{n \times q}$ (n 観測, q 出力)
- $B \in \mathbb{R}^{p \times q}$ (係数)
- $E \in \mathbb{R}^{n \times q}$, 行 $\sim$ MvN(0, Σ)

OLS:
$$ \hat B = (X^T X)^{-1} X^T Y $$

各列は独立に解ける。**残差共分散** $\hat\Sigma = \frac{1}{n-p} E^T E$ が応答間の相関を捕捉。

## 2. Reduced Rank Regression

$B$ に rank $\le r$ 制約。OLS の SVD 上位 $r$ 特異値で truncate:

$$ \hat B_{RRR} = U_r \Sigma_r V_r^T $$

応答間の **共通な低次元構造** を仮定 (信号は $r$ 次元のみ)。

## 3. Partial Least Squares (PLS)

NIPALS アルゴリズム:

```text
for k = 1..K:
  w = X^T Y u / ||X^T Y u||      重み (X 側)
  t = X w                         スコア (X 側)
  p = X^T t / (t^T t)             ローディング (X 側)
  q = Y^T t / (t^T t)             ローディング (Y 側)
  X ← X - t pᵀ                   deflate
  Y ← Y - t qᵀ
```

**X と Y の共分散** を最大化する方向を逐次抽出。

## 4. Canonical Correlation Analysis (CCA)

$X$ と $Y$ の **相関** を最大化する基底ペア:

$$ M = \Sigma_{xx}^{-1/2} \Sigma_{xy} \Sigma_{yy}^{-1/2} $$

を SVD して $M = U \Sigma V^T$、$a = \Sigma_{xx}^{-1/2} U$、$b = \Sigma_{yy}^{-1/2} V$。
$\Sigma$ の対角が canonical correlations。

## 5. Multi-task / Multi-output GP

$f: \mathbb{R}^d \to \mathbb{R}^q$ への GP 拡張。最も簡単な **Independent GPs**:
各出力で独立に GP fit。

より高度な **ICM (Intrinsic Coregionalization Model)**:

$$ k_{ij}(x, x') = B_{ij} \cdot k_x(x, x') $$

$B$ は出力間相関 (低ランクで OK)。

## 6. 比較表

| 手法 | rank 仮定 | 計算 |
|---|---|---|
| OLS | 任意 | $(X^T X)^{-1} X^T Y$ |
| RRR | $\le r$ | OLS + SVD truncate |
| PLS | $\le K$ | NIPALS 反復 |
| CCA | (相関基底) | 共分散 SVD |
| Multi-GP | (kernel) | per output GP fit |
