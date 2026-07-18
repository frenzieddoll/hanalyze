# rats (posteriordb: `rats_data-rats_model`)

BUGS 古典例「ラットの成長曲線」(30匹 × 5時点の体重・縦断的階層線形回帰・
Gelman & Hill 系譜)。出典: `stan-dev/posteriordb`
(`posterior_database/models/stan/rats_model.stan`・
`posterior_database/data/data/rats_data.json.zip`)。

- `reference_posterior_name: null` (posteriordb に公式 reference 無し・
  hanalyze vs PyMC の2者比較のみ)。
- Prior: `mu_alpha, mu_beta ~ Normal(0,100)`・
  `sigma_y, sigma_alpha, sigma_beta ~ HalfCauchy(25)` (Stan 原典は
  improper flat prior・置換理由は下記「Uniform の罠」参照)・
  `alpha_i ~ Normal(mu_alpha, sigma_alpha)`・`beta_i ~ Normal(mu_beta, sigma_beta)`
  (i=1..30、ラットごとに独立な切片/傾き・両方とも部分プーリング)。
- 尤度: `y_n ~ Normal(alpha[rat_n] + beta[rat_n]*(x_n - xbar), sigma_y)`
  (xbar=22・Npts=150・N=30)。

## ★実測で踏んだ罠: Uniform(0,X) を SD パラメータに直接使うと HMC が全 warmup で発散する

当初 Stan 原典に忠実に `sigma_y/sigma_alpha/sigma_beta ~ Uniform(0,100)` で
実装したところ、4 chain 全てで **acceptanceRate=0.0・chainEnergy=Infinity
が全 draw で継続し、事後平均が全パラメータで厳密に 0.0000 に凍りつく**
現象が発生した (`cabal repl` で `chainDivergences`/`chainEnergy` を直接
確認・全 draw が発散と確定)。

原因は `src/hanalyze/Analyze/Model/HBM/Distribution.hs:309-310` の既知の
制約 (01-glm-poisson で既に文書化済み): **`Uniform` の真の制約変換
(logit-on-(lo,hi)) が未実装で unconstrained 扱い**。unconstrained の初期値
raw=0 は、`Uniform(lo,hi)` では変換なしにそのまま値として使われるため、
`Uniform(0,100)` の初期値は **0 (下限そのもの)** になる。01-glm-poisson の
`alpha ~ Uniform(-20,20)` のように「有界だが内部の値」として使うぶんには
無害だが、**Normal 尤度の SD パラメータとして直接使うと `Normal(mu, 0)`
という退化分布になり log-density が `-Infinity` になる**ため、HMC が初手
から発散し続けて一切回復しない。

`HalfCauchy`/`HalfNormal` は `PositiveT` 変換 (exp 系) を持ち raw=0 が
安全な内点 (exp(0)=1 相当) にマップされるため、この罠を回避できる
(切り分けは `cabal repl` の縮小トイモデルで実施: `Uniform 0 100` →
acceptanceRate=0.0、`HalfCauchy 25` → acceptanceRate=0.9)。PyMC 側
(`model.py`) も同じ `HalfCauchy(25)` に揃えた (Stan 原典からの改変幅を
両言語で同一に保ち、比較の公平性を維持するため)。

**教訓 (今後の新モデル追加時の注意)**: Stan 原典が `Uniform(0,X)` や
improper flat prior を SD パラメータに使っている場合、hanalyze へそのまま
移植すると本罠を踏む。`HalfCauchy`/`HalfNormal` 等の `PositiveT` 変換を
持つ分布に置換すること。

## ファイル

- `model.py` — PyMC 実装。
- `Model.hs` — hanalyze 実装 (`plateI` でラットごとの `alpha`/`beta` を
  30個ずつ宣言・`plateForM_` で観測に束ねる eight-schools 型パターン)。
- `run_pymc_matrix.py` — PyMC の CPU sampler×backend マトリクス。
- `data/rats_data.json` — posteriordb 由来データ (`N=30`・`Npts=150`・
  `rat`・`x`・`y`・`xbar=22`)。
- `figures/` — `hs_dashboard_full.png`/`py_dashboard_full.svg`
  (★N=30匹×2 latent のため `dashboardFullOf` ではなく `dashboardOf`
  (健全性2x2パネルのみ) を使用・05-mh と同じ判断)。

## 実行方法

```bash
bench/venv/bin/python bench/posteriordb/10-rats/model.py
bench/venv/bin/python bench/posteriordb/10-rats/run_pymc_matrix.py

cabal build --project-file=cabal.project.plot posteriordb-rats
cabal run   --project-file=cabal.project.plot posteriordb-rats
```

**★順次実行必須** (並行実行禁止・OOM対策は `.claude/skills/posteriordb-bench/SKILL.md`
step5 参照)。

## 経路確認

★Phase 96 A2 (2026-07-17) 実測: runtime `gradPathLabel` = **「Gaussian LM
閉形式ブロック (解析勾配)」** (root:
`experiments/phase96-mh-reconfirm/pathlabels.tsv`)。legacy walk+ad ではない。

以下は Phase 89 起票時の旧記録 (Phase 90-98 の経路拡大以前・**stale**):

> `synthVecIR` = `Nothing` (legacy walk+ad へのフォールバック)。
> (b) 遅い経路へのフォールバック — ラットごとに独立な**2つ**の階層
> (alpha[i]・beta[i] を同じ個体インデックスで同時に部分プーリング) という
> 構造が vecIR 未対応と見られる。

## 結果

### 精度 (4 chain・warmup1000+draws1000・2者比較・reference無し)

| パラメータ | hanalyze | pymc (nutpie+numba・最速CPU) |
|---|---|---|
| mu_alpha    | 242.42 ± 2.85 | 242.5 ± 2.76 |
| mu_beta     | 6.189 ± 0.113 | 6.186 ± 0.115 |
| sigma_y     | 6.135 ± 0.474 | 6.1 ± 0.47 |
| sigma_alpha | 14.80 ± 2.22  | 14.76 ± 2.06 |
| sigma_beta  | 0.532 ± 0.090 | 0.533 ± 0.095 |

全パラメータ小数第1〜2位まで一致。R-hat = 0.999-1.001 (hanalyze)・1.00
(pymc)。

### 速度 (4 chain・warmup1000+draws1000・CPU 1コア固定)

PyMC は sampler×backend 7 通りを計測 (blackjax は他モデルと同型の
`ValueError: cannot select an axis to squeeze` で失敗・既知の課題・
深掘りしない):

| system | wall (ms) | ess_bulk(mu_alpha) | ESS/sec | rhat |
|---|---:|---:|---:|---:|
| nutpie + Numba (**PyMC最速CPU**) | 5592.0 | 6076 | 1086.55 | 1.000 |
| pymc + Numba | 7628.9 | 4796 | 628.67 | 1.000 |
| numpyro | 12596.0 | 4467 | 354.64 | 1.000 |
| nutpie + JAX | 21181.1 | 6058 | 286.01 | 1.000 |
| pymc + CVM (真の C) | 17876.9 | 5218 | 291.89 | 1.000 |
| pymc + JAX (own NUTS) | 79109.3 | 4835 | 61.12 | 1.000 |
| blackjax | (失敗・後述) | — | — | — |
| **hanalyze (Haskell・legacy walk+ad・サンプリングのみ)** | **50328.2** | — | — | 0.999-1.001 |

`synthVecIR = Nothing` (legacy walk+ad 経路) のため、**hanalyze (50328ms)
は PyMC 最速 CPU (nutpie+numba・5592ms) の約 9.0 倍遅い**。N=30 匹 ×
2 latent (alpha/beta) = 60 latent + 5 ハイパーパラメータの毎 leapfrog
step で O(latent 数) の残差 AD 再計算経路に落ちている筋の通った結果
(07-gp-regr の GP と同様のパターン)。

### 図

[hs_dashboard_full.png](./figures/hs_dashboard_full.png) /
[py_dashboard_full.svg](./figures/py_dashboard_full.svg)

### 既知の課題

- **Uniform(0,X) を SD パラメータに使うと HMC が発散する罠** (上記
  「★実測で踏んだ罠」参照)。DSL機能ギャップというより hanalyze の
  `Uniform` 制約変換未実装 (`Distribution.hs:309-310`) の既知課題が
  実害化した3例目 (01-glm-poisson で発見・05-mh で probe 経路に実害化・
  本モデルでサンプリング自体の完全停止として実害化)。
- **vecIR ギャップ (b) 遅い経路**: 「同一グループへの2本の階層
  (alpha[i]・beta[i])」構造が vecIR 未対応 (上記「経路確認」参照)。
- blackjax エラーは他モデルと同型 (原因未解明・深掘りしない方針)。
