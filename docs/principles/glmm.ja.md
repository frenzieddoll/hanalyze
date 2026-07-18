# 混合効果モデル (GLMM/LME) の原理

> 🌐 [English](glmm.md) | **日本語**

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

---

## GLMM → `Hanalyze.Model.HBM` DSL 翻訳

`Hanalyze.Model.GLMM` の最尤 / Laplace 系 (`fitLMEDataFrame` 等) ではなく、
**完全ベイズ HBM** として書きたい場合の翻訳パターン。 lme4 風 formula 表記と
`Hanalyze.Model.HBM` DSL のマッピングです。 詳細な実装は
[`docs/bayesian/02-probabilistic-model.ja.md`](../bayesian/02-probabilistic-model.ja.md)
のパターン 4-7 を参照。

| lme4 風 formula | 階層構造 | DSL での書き方 |
|---|---|---|
| `y ~ 1 + x` | 階層なし (LM) | パターン 1-2 (`Hanalyze.Model.LM` も可) |
| `y ~ 1 + x + (1 \| g)` | ランダム切片のみ | パターン 4 形式 A/B/C。 α_j のみ階層 |
| `y ~ 1 + x + (1 + x \| g)` | ランダム切片 + ランダム傾き | パターン 5 (random slope) |
| `y ~ 1 + (1 \| d/s)` | nested (3 階層) | パターン 6 (multi-level) |
| `y ~ 1 + (1 \| s) + (1 \| t)` | crossed | パターン 7 (crossed) |

### 例: `y ~ 1 + x + (1 + x | g)` を HBM に

lme4 で書くと:

```r
lmer(y ~ 1 + x + (1 + x | g), data = df)
```

これは「全体平均 μ_α + 群別偏差 α_j、 全体傾き μ_β + 群別偏差 β_j」 を意味
します。 DSL の `randomSlope` がそのまま対応します
([`02-probabilistic-model.ja.md`](../bayesian/02-probabilistic-model.ja.md)
の パターン 5 参照、 動作確認済):

```haskell
-- 詳細実装は 02-probabilistic-model.ja.md パターン 5 を参照
randomSlope :: [[(Double, Double)]] -> ModelP ()
randomSlope groupData = do
  muA  <- sample "mu_alpha"  (Normal 0 10)
  tauA <- sample "tau_alpha" (HalfNormal 5)
  muB  <- sample "mu_beta"   (Normal 0 5)
  tauB <- sample "tau_beta"  (HalfNormal 5)
  -- ... 群別 α_j, β_j を sample し、 y_ij ~ Normal(α_j + β_j x, σ) を observe
```

### GLMM (最尤 / Laplace) と HBM (完全ベイズ) の使い分け

| 観点 | GLMM (`fitLMEDataFrame`) | HBM (NUTS) |
|---|---|---|
| 速度 | 速い (EM / Laplace) | 遅い (chain 数 × iter) |
| 不確実性 | Wald 近似 SE | 厳密な事後分布 |
| ランダム効果数 | 多数 (~数千) OK | NUTS は群数 ≫ 100 で重くなる |
| 事前分布のカスタマイズ | 不可 (固定) | 自由 |
| モデル比較 | AIC/BIC | WAIC/LOO (Bayesian) |

**推奨**: まず GLMM で fit → モデル構造が固まったら HBM で完全ベイズ推定。
シンプソンのパラドックス例での 3 手法 (LM / GLMM / HBM) 比較は
[`simpson-paradox`](../../demo/bayesian/SimpsonParadoxDemo.hs) を参照。
