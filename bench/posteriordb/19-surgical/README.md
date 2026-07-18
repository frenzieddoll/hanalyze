# surgical (`surgical_data-surgical_model`)

BUGS 古典例「12病院の心臓手術死亡率」。N=12病院・共変量なしの最も単純な
階層二項ロジットモデル (病院ごとの死亡ロジットが共通の Normal(mu,sigma)
から部分プーリングされる)。出典: `stan-dev/posteriordb`
(`posterior_database/models/stan/surgical_model.stan`・
`posterior_database/data/data/surgical_data.json.zip`)。
**`reference_posterior_name: null`** (posteriordb に公式 reference 無し・
2者比較のみ)。

## Prior (Stan原典どおり)

- `mu ~ Normal(0,1000)`
- `sigmasq ~ InverseGamma(0.001,0.001)`・`sigma = sqrt(sigmasq)`
- `b[i] ~ Normal(mu, sigma)` (i=1..12・病院ごとの死亡ロジット)
- 尤度: `r[i] ~ Binomial(n[i], invlogit(b[i]))`

hanalyze の `Binomial n p` は確率パラメータ直接指定 (logit link 無し) の
ため、05-mh と同じく `p = invlogit(b)` を手計算してから渡した。

## 経路確認

★Phase 96 A2 (2026-07-17) 実測: runtime `gradPathLabel` = **「vecIR
(ベクトル式 IR 高速経路)」** (root:
`experiments/phase96-mh-reconfirm/pathlabels.tsv`)。Phase 90-98 の vecIR
適格拡大で高速経路に載った。

以下は Phase 89 起票時の旧記録 (**stale**):

> `synthVecIR = Nothing` (legacy walk+ad へのフォールバック)。N=12と小規模
> のため実用上問題なし。

## 精度 (4 chain・warmup1000+draws1000・独立検証 = arviz)

| パラメータ | hanalyze | pymc (numba・最速CPU) |
|---|---|---|
| mu | -2.548 ± 0.148 | -2.557 ± 0.151 |
| sigma | 0.395 ± 0.154 | 0.410 ± 0.155 |
| pop_mean | 0.073 ± 0.010 | 0.073 ± 0.010 |
| b_0..b_11 | -2.93〜-1.98 (12病院) | -2.97〜-1.97 (12病院・小数第1〜2位まで一致) |

全パラメータ小数第1〜2位まで一致。R-hat = 1.00 (両系統とも)。sigma
(sigmasqのInvGamma(1e-3,1e-3)拡散事前分布由来) はESSがやや低め
(hanalyze sigma ESS=249.6) だが両系統とも同様の傾向で、既知の
diffuse-precision事前分布のfunnel様ジオメトリ (05-mh/11-seedsと同種)。

## 速度 (4 chain・warmup1000+draws1000・CPU 1コア固定)

PyMC は sampler×backend 7 通りを計測 (blackjax は他モデルと同一の
`ValueError: cannot select an axis to squeeze` により失敗・原因未解明の
ため参考記録):

| system | wall (ms) | ESS (mu・arviz計算) | ESS/sec | 対 hanalyze |
|---|---:|---:|---:|---:|
| **hanalyze (Haskell・legacy walk+ad)** | 2795.2 | (`Common.summarize` 参照) | — | 基準 |
| pymc + Numba (**PyMC最速CPU**) | 3741.8 | 2964 | 792.13 | 1.34× |
| nutpie + Numba | 4534.2 | 2777 | 612.45 | 1.62× |
| nutpie + JAX | 4820.7 | 2169 | 449.93 | 1.72× |
| numpyro | 8868.8 | 2792 | 314.81 | 3.17× |
| pymc + CVM (真の C) | 10089.2 | 2861 | 283.57 | 3.61× |
| pymc + JAX (own NUTS) | 16201.8 | 2757 | 170.17 | 5.80× |
| blackjax | (失敗・後述) | — | — | — |

`synthVecIR=Nothing` (legacy walk+ad) にもかかわらず、**hanalyze
(2795.2ms) が PyMC最速CPU (pymc+numba・3741.8ms) の約1.34倍高速** —
N=12という極小規模データではlegacy経路のO(N)残差AD再計算コストが
無視できるほど小さく、GHCネイティブコードの実行効率がPyMCのJIT起動
コストを上回った (01-glm-poisson/12-ark/18-loss-curvesと同様のパターン)。

## 図 — 両側とも「フルダッシュボード」1 枚に統一

[hs_dashboard_full.png](./figures/hs_dashboard_full.png) /
[py_dashboard_full.svg](./figures/py_dashboard_full.svg)

## 既知の課題

- **sigma (sigmasqのInvGamma拡散事前分布) のESSがやや低め**: 05-mh/
  11-seedsと同種のfunnel様ジオメトリ (両系統共通・深掘りしない方針)。
- blackjaxエラーは他モデルと同型 (原因未解明・深掘りしない方針)。
