# 一般化線形モデル (GLM) の原理

## モデル

GLM は LM を **指数型分布族 + リンク関数** に拡張します:

$g(E[y_i \mid x_i]) = \beta_0 + \beta_1 x_{i1} + \cdots + \beta_p x_{ip}$

ここで $g$ は **リンク関数**。

## 主要なファミリーと標準リンク

- **Gaussian + identity**: 通常の LM
- **Binomial + logit**: $\log(p/(1-p)) = X\beta$ — ロジスティック回帰
- **Poisson + log**: $\log \lambda = X\beta$ — カウントデータ

## 推定 — IRLS

正規方程式が閉形式で解けないため、**反復重み付き最小二乗** (IRLS):

1. 初期値 $\beta^{(0)}$ から開始
2. 各反復:
   - 予測値 $\hat\mu = g^{-1}(X\beta)$
   - 重み $W = \text{diag}(1/V(\hat\mu_i) \cdot (g'(\hat\mu_i))^{-2})$
   - 修正応答 $z = X\beta + g'(\hat\mu)(y - \hat\mu)$
   - 解 $\beta^{(t+1)} = (X^T W X)^{-1} X^T W z$
3. 収束まで繰り返し

## 評価 — McFadden R²

通常の R² が定義できないため疑似 R² を使う:

$R^2_{\text{McFadden}} = 1 - \frac{\log L(\hat\beta)}{\log L(\beta_{\text{null}})}$

詳細: [docs/regression/02-glm.ja.md](../regression/02-glm.ja.md)
