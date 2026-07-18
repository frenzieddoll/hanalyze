# dugongs (`dugongs_data-dugongs_model`)

BUGS 古典例「ジュゴンの成長曲線」(`stan-dev/example-models/bugs_examples/vol2`
由来・Kane Lindsay 2021-07-15 追加)。N=27頭の体長 `Y` と年齢 `x` を非線形
漸近成長曲線 `m[i] = alpha - beta * lambda^x[i]` で回帰する。

出典: `stan-dev/posteriordb` (`posterior_database/models/stan/dugongs_model.stan`・
`posterior_database/data/data/dugongs_data.json.zip`)。

- `reference_posterior_name: null` — posteriordb に公式 reference posterior 無し。
  hanalyze vs PyMC の2者比較のみ。
- Prior (Stan原典): `alpha ~ Normal(0,1000)`・`beta ~ Normal(0,1000)`・
  `lambda ~ Uniform(.5,1)`・`tau ~ Gamma(.0001,.0001)`
  (`sigma = 1/sqrt(tau)`・`U3 = logit(lambda)` は transformed parameters)。

## 経路確認

★Phase 102 A1 / Phase 96 A2 (2026-07-17) 実測: runtime `gradPathLabel` =
**「vecIR (ベクトル式 IR 高速経路)」** (root:
`experiments/phase102-logdensityrd-general-walk/run-dugongs.log` +
`experiments/phase96-mh-reconfirm/pathlabels.tsv`)。Phase 90-98 の vecIR
適格拡大 (`pow` 対応含む) で高速経路に載った。

以下は Phase 89 起票時の旧記録 (**stale**):

> `synthVecIR = Nothing` (legacy walk+ad へのフォールバック)。非線形の
> `pow(lambda, x[i])` (`lambda ** xi`) が vecIR 未対応と見られる。

## ★実測で踏んだ罠: Uniform(.5,1) 境界外初期値でHMCが完全凍結

Stan 原典どおり `lambda ~ Uniform(.5,1)` をそのまま `sample` すると、
**全 4 chain・全 warmup で `alpha=beta=lambda=0.0000`・`tau=sigma=1.0000`
に完全凍結** (`ess=1000`・`r_hat=NA`) する現象が実機で発生した。

原因: hanalyze の `Uniform` 制約変換は現状 unconstrained 扱い
(`Distribution.hs:309-310`)。unconstrained 初期値 `raw=0` が `Uniform(lo,hi)`
では変換なしにそのまま値として使われるため `lambda=0` となり、これは
`Uniform(.5,1)` の台の外 (`logDensity = -Infinity`)。初手から HMC 提案が
拒否され続け、chain が凍結する (10-rats で発見した「Uniform(0,X) をSD
パラメータに使うと発散する罠」の同型バリエーション — 今回は SD ではなく
growth curve の底として実害化した)。

解決 (厳密な等価変形): `lambda ~ Uniform(.5,1)` を

```haskell
u      <- sample "u" (Beta 1 1)              -- Uniform(0,1) と同一分布・UnitIntervalT変換
lambda <- deterministic "lambda" (0.5 + 0.5 * u)
```

に置換。`Beta(1,1)` は `UnitIntervalT` 変換 (logit系) を持ち、unconstrained
初期値 `raw=0` は `u=0.5` (真に (0,1) 内部) にマップされ、`lambda=0.75` は
`[.5,1]` の内部で安全。affine 変換 (`lambda = 0.5 + 0.5*u`) の Jacobian は
定数 `0.5` で HMC の相対密度に影響しないため、`Uniform(.5,1)` と厳密に
等価。14-hmm-example の順序制約 (加算シフト+potential) と同系統の
「unconstrained分布の代わりに真に制約された分布から affine 変換する」
対処法。

## 精度 (4 chain・warmup1000+draws1000・独立検証 = arviz)

| パラメータ | hanalyze | pymc (numba・最速CPU) |
|---|---|---|
| alpha | 2.654 ± 0.071 | 2.652 ± 0.069 |
| beta | 0.976 ± 0.074 | 0.976 ± 0.075 |
| lambda | 0.863 ± 0.032 | 0.862 ± 0.032 |
| tau | 110.5 ± 30.3 | 109 ± 31 |
| sigma | 0.098 ± 0.014 | 0.099 ± 0.015 |

全パラメータ小数第2〜3位まで一致。R-hat = 1.00〜1.01 (両系統とも)。
`alpha≈2.65/beta≈0.97/lambda≈0.86/sigma≈0.1` は BUGS dugongs 例の古典的
推定値とも整合。

## 速度 (4 chain・warmup1000+draws1000・CPU 1コア固定)

PyMC は sampler×backend 7 通りを計測 (blackjax は他モデルと同一の
`ValueError: cannot select an axis to squeeze` により失敗・原因未解明の
ため参考記録):

| system | wall (ms) | ESS (alpha・arviz計算) | ESS/sec | 対 hanalyze |
|---|---:|---:|---:|---:|
| **hanalyze (Haskell・legacy walk+ad)** | 324.6 | (`Common.summarize` 参照) | — | 基準 |
| pymc + Numba (**PyMC最速CPU**) | 5920.0 | 1363 | 230.24 | 18.2× |
| nutpie + Numba | 5561.9 | 663 | 119.20 | 17.1× (r_hat=1.010・僅かに収束劣化) |
| numpyro | 8935.4 | 1240 | 138.77 | 27.5× |
| pymc + CVM (真の C) | 12710.5 | 1548 | 121.79 | 39.2× |
| nutpie + JAX | 6625.7 | 942 | 142.17 | 20.4× |
| pymc + JAX (own NUTS) | 26264.9 | 1294 | 49.27 | 80.9× |
| blackjax | (失敗・後述) | — | — | — |

`synthVecIR=Nothing` (legacy walk+ad) にもかかわらず、**hanalyze (324.6ms) が
PyMC最速CPU (pymc+numba・5920.0ms) の約18.2倍高速** — N=27という小規模
データでは legacy 経路の O(N) 残差AD再計算コスト自体が小さく、GHCネイティブ
コードの実行効率がPyMCのJIT起動コストを上回った (01-glm-poisson/12-ark と
同様のパターン)。

## 図 — 両側とも「フルダッシュボード」1 枚に統一

[hs_dashboard_full.png](./figures/hs_dashboard_full.png) /
[py_dashboard_full.svg](./figures/py_dashboard_full.svg)

## 既知の課題

- **Uniform(lo,hi) 境界外初期値の罠**: 上記参照。`Distribution.hs:309-310`
  の既知課題が「SDパラメータ」以外 (growth curve の底) でも実害化する
  4例目 (01-glm-poisson/10-rats に続く)。
- **vecIRギャップ (b)**: `pow(lambda, x[i])` (非整数指数の冪) が vecIR
  未対応。N=27と小規模のため実用上は無害。
- blackjaxエラーは他モデルと同型 (原因未解明・深掘りしない方針)。
