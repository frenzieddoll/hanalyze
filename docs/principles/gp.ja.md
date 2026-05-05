# ガウス過程 (GP) 回帰の原理

> 🌐 [English](gp.md) | **日本語**

## モデル

関数 $f$ そのものに事前分布を置く **ノンパラメトリック** ベイズ:

$f \sim \text{GP}(0, k(x, x'))$

任意の点 $x_1, \ldots, x_n$ で $f$ の評価値が多変量正規:

$\mathbf{f} \sim \text{Normal}(\mathbf{0}, K)$, $K_{ij} = k(x_i, x_j)$

観測モデル:

$y_i = f_i + \varepsilon_i$, $\varepsilon_i \sim \text{Normal}(0, \sigma^2_n)$

## カーネル

- **RBF (二乗指数)**: $k(x, x') = \sigma^2_f \exp(-(x-x')^2 / (2\ell^2))$
  - 滑らかな関数を仮定 (無限階微分可能)
- **Matérn 5/2**: より粗い関数 (2 階微分可能)
- **Periodic**: 周期パターン

## 事後予測

新しい点 $x_*$ での予測:

$\mu_* = K_*^T (K + \sigma^2_n I)^{-1} y$
$\sigma^2_* = k(x_*, x_*) - K_*^T (K + \sigma^2_n I)^{-1} K_*$

## ハイパラメータ最適化

長さスケール $\ell$、信号分散 $\sigma^2_f$、ノイズ分散 $\sigma^2_n$ を
**対数周辺尤度** の最大化で決定:

$\log p(y \mid X, \theta) = -\tfrac{1}{2} y^T K_y^{-1} y - \tfrac{1}{2} \log|K_y| - \tfrac{n}{2} \log 2\pi$

詳細: [docs/regression/04-regularized.ja.md](../regression/04-regularized.ja.md)
