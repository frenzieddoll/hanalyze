# 線形回帰 (LM) の原理

> 🌐 [English](lm.md) | **日本語**

## モデル

線形回帰は応答 $y$ を説明変数 $\mathbf{x} = (x_1, \ldots, x_p)$ の
**線形結合 + 正規ノイズ** で表現します:

$y_i = \beta_0 + \beta_1 x_{i1} + \cdots + \beta_p x_{ip} + \varepsilon_i$

ここで $\varepsilon_i \sim \text{Normal}(0, \sigma^2)$。

## 推定 — 最小二乗法 (OLS)

残差平方和 RSS を最小化:

$\hat{\beta} = (X^T X)^{-1} X^T y$

実装は数値安定性のため **QR 分解** を使う ($X = QR$ → $R^{-1} Q^T y$)。

## 信頼区間

各係数の標準誤差:

$\text{SE}(\hat\beta_j) = \sqrt{\hat\sigma^2 [(X^T X)^{-1}]_{jj}}$

平均応答の 95% 信頼帯:

$\hat y_* \pm t_{0.025, n-p-1} \hat\sigma \sqrt{\mathbf{x}_*^T (X^T X)^{-1} \mathbf{x}_*}$

## 仮定 (Gauss-Markov)

- **線形性**: $E[y \mid x] = X\beta$
- **独立性**: 残差が独立
- **等分散性**: $\text{Var}(\varepsilon_i) = \sigma^2$
- **正規性**: $\varepsilon_i \sim \text{Normal}$ (信頼区間に必要)

詳細: [docs/regression/01-lm.ja.md](../regression/01-lm.ja.md)
