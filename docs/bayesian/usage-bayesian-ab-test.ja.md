# Bayesian A/B test — HDI と ROPE による意思決定

対象 module: `Hanalyze.MCMC.BayesianTest` (`hanalyze-bayes`)

2 群の平均差 (μ_B − μ_A) の**事後分布**を NUTS でサンプルし、
HDI (highest density interval) と ROPE (region of practical equivalence)
で判定する。 頻度論版の
[`goodVsBad`](../stat/usage-group-comparison.ja.md) が Welch t + Cohen's d で
多変数を並列比較するのに対し、 こちらは **1 つの比較を深く**扱う。

p 値と違い「差が実質ゼロと言える」 という**帰無仮説側の主張**ができるのが
ROPE の利点。

## 0. モデル

```
μ_A  ~ Normal(0, priorScale)      σ_A ~ HalfNormal(sigmaScale)
μ_B  ~ Normal(0, priorScale)      σ_B ~ HalfNormal(sigmaScale)
y_A  ~ Normal(μ_A, σ_A)           y_B ~ Normal(μ_B, σ_B)
diff = μ_B − μ_A
```

群ごとに独立な σ を置くので、 **等分散を仮定しない** (Welch の t 検定に対応する
ベイズ版)。

## 1. API

```haskell
bayesianAB
  :: BayesianABConfig
  -> [Double]        -- 群 A の観測値
  -> [Double]        -- 群 B の観測値
  -> MWC.GenIO
  -> IO BayesianABResult
```

`BayesianABConfig` (既定値は `defaultBayesianABConfig`):

| フィールド | 意味 |
|---|---|
| `babCredible` | HDI の信用水準 (既定 0.95) |
| `babRule` | `HDIOnly` (判定しない) または `ROPEDecision lo hi` |
| `babPriorScale` | μ の事前分布 σ (既定 10) |
| `babSigmaScale` | HalfNormal の scale (既定 5) |
| `babNUTS` | NUTS の設定 (`NUTSConfig`) |

判定ルール `ROPEDecision lo hi` は「実用上 0 と区別しない区間 `[lo, hi]`」 を
指定する。 判定は:

| 条件 | `ABDecision` |
|---|---|
| HDI が ROPE と重ならない | `RejectH0` (明確に差がある) |
| HDI が ROPE に完全に含まれる | `AcceptH0` (実用上 0) |
| 部分的に重なる | `Inconclusive` (データ不足) |
| `HDIOnly` を指定 | `NoRuleApplied` |

## 2. 使い方

```haskell
import qualified System.Random.MWC as MWC
import Hanalyze.MCMC.BayesianTest

main :: IO ()
main = do
  gen <- MWC.create
  let cfg = defaultBayesianABConfig { babRule = ROPEDecision (-0.5) 0.5 }
      ysA = [4.8, 5.1, 4.9, 5.3, 5.0, 4.7, 5.2, 5.0]
      ysB = [6.1, 5.9, 6.4, 6.0, 6.3, 5.8, 6.2, 6.5]
  r <- bayesianAB cfg ysA ysB gen
  print (babMeanDiff r, babHDI r, babDecision r, babProbDiffPos r)
```

実行結果 (`MWC.create` = 固定 seed):

```
(1.1499825845264315,(0.8868282100120641,1.428183701462828),RejectH0,1.0)
```

平均差の事後平均は 1.15、 95% HDI は [0.89, 1.43]。 ROPE [-0.5, 0.5] と
重ならないので `RejectH0`、 `P(μ_B > μ_A) = 1.0` (サンプル中すべて正)。

## 3. 結果型 `BayesianABResult`

| フィールド | 内容 |
|---|---|
| `babPosteriorDiff` | 平均差の post-burn-in サンプル (`[Double]`) |
| `babMeanDiff` | 事後平均 (μ_B − μ_A) |
| `babHDI` | `babCredible` 水準の HDI |
| `babDecision` | `ABDecision` |
| `babProbDiffPos` | P(μ_B > μ_A) の事後確率 |
| `babChain` | 生 chain (μ_A / μ_B / σ_A / σ_B / diff) |

`babChain` は core の `Chain` なので、 診断 (`rhat` / `ess`) や
可視化 (`Hanalyze.Viz.*`) にそのまま渡せる。 **HDI が狭すぎる /
判定が不安定なときは、 結論の前に必ず収束診断を見ること**。

## 4. ROPE の決め方

ROPE は統計ではなく**ドメインの判断**。 目安:

- 実務上の意思決定が変わらない差の幅を直接置く (例: 歩留まり ±0.5%)
- 基準が無い場合は「小さい効果量」 相当を使う慣習がある
  (Kruschke は標準化差で ±0.1 を例示)
- `HDIOnly` で HDI だけ出し、 判定は人間が下す運用でもよい

## 5. 使い分け

| 状況 | 使うもの |
|---|---|
| 多数の変数を一括スクリーニング | [Good vs Bad](../stat/usage-group-comparison.ja.md) |
| 1 つの比較を事後分布で判断 | 本 doc の `bayesianAB` |
| 「差が無い」 と積極的に言いたい | 本 doc (ROPE) または [Bayes factor](07-advanced-marginal-likelihood.ja.md) |
| 古典的な検定で足りる | [stat/01-test.ja.md](../stat/01-test.ja.md) |

## 6. 参考文献

- Kruschke, J.K. (2013) "Bayesian estimation supersedes the t test",
  *Journal of Experimental Psychology: General* 142(2) — BEST 法と ROPE の出典
- Hoffman & Gelman (2014) "The No-U-Turn Sampler", *JMLR* 15 — 内部で使う sampler

## 関連

- package: [hanalyze-bayes](../../hanalyze-bayes/README.ja.md)
- サンプラの設定: [03-mcmc-samplers.ja.md](03-mcmc-samplers.ja.md)
- 収束診断: [viz-diagnostics.ja.md](viz-diagnostics.ja.md)
