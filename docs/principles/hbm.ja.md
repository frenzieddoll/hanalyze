# 階層ベイズモデル (HBM) の原理

## ベイズ推論の枠組み

**事後分布** = **尤度** × **事前分布** / 周辺尤度

$p(\theta \mid y) \propto p(y \mid \theta) \cdot p(\theta)$

線形回帰のベイズ版:

$\alpha \sim \text{Normal}(0, 10),\ \beta \sim \text{Normal}(0, 10),\ \sigma \sim \text{Exponential}(1)$
$y_i \sim \text{Normal}(\alpha + \beta x_i, \sigma)$

## なぜ MCMC か

事後分布 $p(\theta \mid y)$ は通常 **解析的に解けない** ため、
マルコフ連鎖モンテカルロ (MCMC) で **サンプル列** を生成して近似する。

## NUTS (No-U-Turn Sampler)

Hamiltonian Monte Carlo (HMC) の進化版:
- リープフロッグで擬似 Hamiltonian 軌道を生成
- 「U ターン判定」で軌道長を自動決定 → チューニング不要
- Stan / PyMC のデフォルト

hanalyze は **AD (自動微分)** で対数事後勾配を正確に計算。

## 診断指標

- **R-hat** < 1.01: チェーン間の収束
- **ESS** (Effective Sample Size) > 数百: 自己相関を考慮した実効サンプル数
- **Trace plot**: チェーンの混合具合を視覚確認
- **Pair plot**: 同時事後の構造 (相関、funnel 等)

## 信用区間 vs 信頼区間

- **信頼区間 (Frequentist CI)**: 反復実験を仮定した区間
- **信用区間 (Bayesian Credible Interval)**: 事後分布の 2.5%〜97.5% パーセンタイル
  → 「真値が 95% の確率でこの区間にある」と直接解釈可能

詳細: [docs/bayesian/02-probabilistic-model.ja.md](../bayesian/02-probabilistic-model.ja.md)
