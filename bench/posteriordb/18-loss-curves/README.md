# loss-curves (`loss_curves-losscurve_sislob`)

保険数理の損失三角形 (loss reserving)。n_cohort=10 (契約年度)・
n_time=10 (経過年)・n_data=55 (=10+9+...+1・下三角が未観測の古典的
三角形構造)。Weibull型成長曲線 (`growthmodel_id=1`、データで確認済み)
で損失の発展パターンをモデル化する。出典: `stan-dev/posteriordb`
(`posterior_database/models/stan/losscurve_sislob.stan`・
`posterior_database/data/data/loss_curves.json.zip`)。
**`reference_posterior_name: null`** (posteriordb に公式 reference
無し・2者比較のみ)。

## Prior (Stan原典どおり・全てLogNormal/Normalで境界の罠なし)

- `omega/theta ~ LogNormal(0,0.5)` (Weibull成長曲線の形状パラメータ)
- `mu_LR ~ Normal(0,0.5)`・`sd_LR ~ LogNormal(0,0.5)` (コホート間LR階層)
- `LR[i] ~ LogNormal(mu_LR, sd_LR)` (i=1..10・コホートごとのloss ratio)
- `loss_sd ~ LogNormal(0,0.7)`
- 尤度: `gf[t] = 1 - exp(-(t/theta)^omega)`・
  `lm[i] = LR[cohort_id[i]]*premium[cohort_id[i]]*gf[t_idx[i]]`・
  `loss[i] ~ Normal(lm[i], loss_sd*premium[cohort_id[i]])`

全 prior が LogNormal (PositiveT変換) / Normal (UnconstrainedT) のため、
01-glm-poisson/10-rats/15-dugongs で確立した「Uniform境界外初期値の罠」
は該当しない (Uniform を一切使っていない)。

## 経路確認

★Phase 96 A2 (2026-07-17) 実測: runtime `gradPathLabel` = **「vecIR
(ベクトル式 IR 高速経路)」** (root:
`experiments/phase96-mh-reconfirm/pathlabels.tsv`)。Phase 90-98 の vecIR
適格拡大で高速経路に載った。

以下は Phase 89 起票時の旧記録 (**stale**):

> `synthVecIR = Nothing` (legacy walk+ad へのフォールバック)。コホートごと
> のLR階層+成長曲線の非線形合成 (`(t/theta)^omega`) が vecIR 未対応と
> 見られる。

## 精度 (4 chain・warmup1000+draws1000・独立検証 = arviz)

| パラメータ | hanalyze | pymc (numba・最速CPU) |
|---|---|---|
| omega | 1.462 ± 0.047 | 1.459 ± 0.048 |
| theta | 2.210 ± 0.042 | 2.211 ± 0.044 |
| mu_LR | -0.111 ± 0.114 | -0.106 ± 0.111 |
| sd_LR | 0.347 ± 0.112 | 0.344 ± 0.105 |
| loss_sd | 0.0283 ± 0.0034 | 0.0283 ± 0.0034 |
| LR_0..LR_9 | 0.640〜1.433 (10コホート) | 0.640〜1.433 (10コホート・小数第2〜3位まで一致) |

全パラメータ小数第2〜3位まで一致。R-hat = 1.00 (両系統とも)。

## 速度 (4 chain・warmup1000+draws1000・CPU 1コア固定)

PyMC は sampler×backend 7 通りを計測 (blackjax は他モデルと同一の
`ValueError: cannot select an axis to squeeze` により失敗・原因未解明の
ため参考記録):

| system | wall (ms) | ESS (omega・arviz計算) | ESS/sec | 対 hanalyze |
|---|---:|---:|---:|---:|
| **hanalyze (Haskell・legacy walk+ad)** | 5129.1 | (`Common.summarize` 参照) | — | 基準 |
| pymc + Numba (**PyMC最速CPU**) | 5192.7 | 2802 | 539.60 | 1.01× |
| nutpie + Numba | 7156.1 | 3235 | 452.06 | 1.40× |
| nutpie + JAX | 9035.1 | 3408 | 377.20 | 1.76× |
| numpyro | 12986.0 | 2043 | 157.32 | 2.53× |
| pymc + CVM (真の C) | 20019.1 | 3221 | 160.90 | 3.90× |
| pymc + JAX (own NUTS) | 30709.6 | 3060 | 99.64 | 5.99× |
| blackjax | (失敗・後述) | — | — | — |

`synthVecIR=Nothing` (legacy walk+ad) にもかかわらず、**hanalyze
(5129.1ms) は PyMC最速CPU (pymc+numba・5192.7ms) とほぼ拮抗** (約1.01倍
高速) — N=55×約15latentという小規模モデルではlegacy経路のO(N)残差AD
再計算コストが小さく、GHCネイティブコードの実行効率がPyMCのJIT起動
コストと釣り合った (02-dogs/17-nesに近いパターン)。本モデルでは
nutpie系がPyMC own-NUTSより速いにもかかわらずどちらもpymc+numbaに劣後
する珍しいケース (小規模モデルではJIT起動コストの比重が相対的に大きい
ため)。

## 図 — 両側とも「フルダッシュボード」1 枚に統一

`figures/hs_dashboard_full.png` /
`figures/py_dashboard_full.svg`

(図はベンチ実行後に `figures/` へ生成される。リポジトリには含めていない)

## 既知の課題

- 特になし (要改善記録は無し・罠も踏まなかった)。blackjaxエラーのみ
  他モデルと同型 (原因未解明・深掘りしない方針)。
