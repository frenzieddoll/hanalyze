# 混合効果モデル (GLMM/LME) の原理

## モデル

GLMM は **固定効果** $\beta$ (集団全体) と **ランダム効果** $u_j$
(グループ固有) を併せ持つ階層モデル:

$y_{ij} = X_{ij}\beta + u_j + \varepsilon_{ij}$

ここで:
- $u_j \sim \text{Normal}(0, \sigma^2_u)$ — グループ $j$ ごとの切片
- $\varepsilon_{ij} \sim \text{Normal}(0, \sigma^2)$ — 観測ノイズ

## 推定 — EM アルゴリズム (LME / Gaussian)

1. **E ステップ**: BLUP (Best Linear Unbiased Predictor) で $u_j$ を推定
2. **M ステップ**: $\sigma^2_u, \sigma^2$ を最尤推定

非ガウス GLMM は **Laplace 近似** で潜在変数を周辺化。

## ICC (級内相関係数)

グループ間/総分散の比:

$\text{ICC} = \frac{\sigma^2_u}{\sigma^2_u + \sigma^2}$

ICC が高い → グループ構造が強い。

## なぜ重要か

通常の LM では:
- グループ構造を無視 → 標準誤差を過小評価
- グループ別の固定効果 → 因子数が膨大

LME はこれらを **正則化付き** で同時推定。

詳細: [docs/regression/03-glmm.ja.md](../regression/03-glmm.ja.md)
