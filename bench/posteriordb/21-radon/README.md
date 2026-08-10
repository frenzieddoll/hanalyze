# radon (`radon_mn-radon_hierarchical_intercept_noncentered`)

Gelman ラドン多水準回帰の古典例 (mc-stan.org radon case study)。ミネソタ
州 J=85郡・N=919家屋の屋内ラドン濃度回帰 (郡ごとの varying intercept +
固定傾き2本、non-centered パラメタ化)。出典: `stan-dev/posteriordb`
(`posterior_database/models/stan/radon_hierarchical_intercept_noncentered.stan`・
`posterior_database/data/data/radon_mn.json.zip`)。
**`reference_posterior_name: null`** (posteriordb に公式 reference 無し・
2者比較のみ)。★新ファミリ: 多水準回帰 (varying intercept, non-centered)
— Phase 89 で初めて扱うモデル種別。

## Prior・尤度 (Stan原典どおり)

- `mu_alpha ~ Normal(0,10)`・`sigma_alpha ~ HalfNormal(1)`・
  `sigma_y ~ HalfNormal(1)` (Stan の `<lower=0>`+`Normal(0,1)` は
  half-normal と数学的に等価)
- `beta1, beta2 ~ Normal(0,10)` (log_uppm・floor_measureの係数)
- `alpha_raw[j] ~ Normal(0,1)` (j=1..85) → `alpha[j] = mu_alpha +
  sigma_alpha*alpha_raw[j]` (non-centered パラメタ化・deterministic)
- 尤度: `log_radon[n] ~ Normal(alpha[county_idx[n]] +
  log_uppm[n]*beta1 + floor_measure[n]*beta2, sigma_y)`

単一階層 (alphaのみ) のため10-ratsの「二重階層」DSLギャップは該当しない。

## 経路確認

★Phase 102 A1 / Phase 96 A2 (2026-07-17) 実測: runtime `gradPathLabel` =
**「vecIR (ベクトル式 IR 高速経路)」** (root:
`experiments/phase102-logdensityrd-general-walk/run-radon.log` +
`experiments/phase96-mh-reconfirm/pathlabels.tsv`)。Phase 90-98 の vecIR
適格拡大 (グループ添字 gather 対応含む) で高速経路に載った。

以下は Phase 89 起票時の旧記録 (**stale**):

> `synthVecIR = Nothing` (legacy walk+ad へのフォールバック)。単一階層
> (eight-schools/seedsと同型) だが県ごとのグループ添字 (`county_idx`) +
> 2本の固定傾きの組み合わせがvecIR未対応と見られる。

## 精度 (4 chain・warmup1000+draws1000・独立検証 = arviz)

| パラメータ | hanalyze | pymc (nutpie+numba・最速CPU) |
|---|---|---|
| mu_alpha | 1.416 ± 0.038 | 1.415 ± 0.038 |
| sigma_alpha | 0.152 ± 0.050 | 0.147 ± 0.049 |
| sigma_y | 0.730 ± 0.019 | 0.730 ± 0.018 |
| beta1 (log_uppm) | 0.782 ± 0.100 | 0.787 ± 0.101 |
| beta2 (floor_measure) | -0.640 ± 0.066 | -0.638 ± 0.067 |

全パラメータ小数第2〜3位まで一致。R-hat = 1.00 (両系統とも)。
`beta2≈-0.64` (floor測定で室内ラドンが低下) は Gelman radon 例の既知の
傾向と整合。

## 速度 (4 chain・warmup1000+draws1000・CPU 1コア固定)

PyMC は sampler×backend 7 通りを計測 (blackjax は他モデルと同一の
`ValueError: cannot select an axis to squeeze` により失敗・原因未解明の
ため参考記録):

| system | wall (ms) | ESS (mu_alpha・arviz計算) | ESS/sec | 対 hanalyze |
|---|---:|---:|---:|---:|
| **hanalyze (Haskell・legacy walk+ad)** | 4905.5 | (`Common.summarize` 参照) | — | 基準 |
| nutpie + Numba (**PyMC最速CPU**) | 5395.5 | 1589 | 294.50 | 1.10× |
| pymc + Numba | 7228.7 | 2810 | 388.73 | 1.47× |
| numpyro | 11049.3 | 3407 | 308.34 | 2.25× |
| nutpie + JAX | 10040.9 | 1095 | 109.05 | 2.05× |
| pymc + CVM (真の C) | 15305.0 | 3070 | 200.59 | 3.12× |
| pymc + JAX (own NUTS) | 53133.7 | 3192 | 60.07 | 10.83× |
| blackjax | (失敗・後述) | — | — | — |

nutpie+numba (r_hat=1.000・完全収束) が全組み合わせ中最速だったため
「PyMC最速CPU」として採用 (09-eight-schools/12-arkと同じ判断基準)。
`synthVecIR=Nothing` (legacy walk+ad) にもかかわらず、**hanalyze
(4905.5ms) が PyMC最速CPU (nutpie+numba・5395.5ms) の約1.10倍高速** —
N=919・J=85latentという中規模データでlegacy経路のO(N)残差AD再計算
コストとGHCネイティブコードの実行効率が拮抗した (17-nes/18-loss-curves
に近いパターン)。

## 図 — hanalyze側「健全性2x2パネル」・PyMC側は「フルダッシュボード」

J=85郡分のalpha latentを含むため dashboardFullOf でなく dashboardOf
(05-mh/10-ratsと同じ判断)。

[hs_dashboard_full.png](./figures/hs_dashboard_full.png) /
[py_dashboard_full.svg](./figures/py_dashboard_full.svg)

## 既知の課題

- 特になし (要改善記録は無し・罠も踏まなかった)。blackjaxエラーのみ
  他モデルと同型 (原因未解明・深掘りしない方針)。
