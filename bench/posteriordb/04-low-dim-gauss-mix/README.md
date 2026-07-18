# low-dim-gauss-mix (posteriordb: `low_dim_gauss_mix-low_dim_gauss_mix`)

2成分 Gaussian 混合 (N=1000)。出典: `stan-dev/posteriordb`
(`posterior_database/models/stan/low_dim_gauss_mix.stan`・
`posterior_database/data/data/low_dim_gauss_mix.json.zip`)。

- `reference_posterior_name` あり — posteriordb に公式 reference posterior
  あり (3者比較可能)。
- Prior: `mu1,mu2 ~ Normal(0,2)`・`sigma1,sigma2 ~ HalfNormal(2)`・
  `theta ~ Beta(5,5)` (Stan 原典どおり)。
- 尤度: `y ~ Mixture([theta,1-theta], [Normal(mu1,sigma1), Normal(mu2,sigma2)])`
  (Stan の `log_mix` と数式一致)。

## Phase 90 A3 (vecIR ギャップ解消) との関係

このモデルは Phase 89 で「(a) DSL機能ギャップ: vecIR が Mixture 尤度を
未サポート」としてフォールバック確定・実装ファイル未作成のまま保留され
ていた。Phase 90 A3 で `IR.hs` に2成分 Normal 混合限定の新規 vecIR family
(`VGMixNorm2`/`SDMixNorm2`/`VOMixNorm2`) を追加し、数値安定な log-sum-exp
(`SMaxO` + `logSumExp2`。 `max(a,b)+log(1+exp(-|a-b|))`) を経由して
高速経路に載せた。トイモデルで `synthVecIR` = `Just` を実測確認済み。
副産物として `vgExpr1`/`vgExpr2` (最大2フィールド限定の古い設計) を
`vgExprAll` (任意個数のUExpフィールドに対応) に一般化した (既存family
含め全面的に書き換え・回帰確認済み)。

## 既知の課題: ラベルスイッチング (Stan `ordered[2]` 制約の欠如)

2成分混合は `(mu1,sigma1,theta) ↔ (mu2,sigma2,1-theta)` を入れ替えても
尤度が不変 (非識別)。Stan 原典は `ordered[2] mu` (mu1<mu2 を強制) で
これを回避するが、hanalyze に対応する順序制約プリミティブが無いため
未実装 (PyMC 側も同条件を揃えるため `pm.Potential` 等で制約せず、素の
mu1/mu2 で実装)。

生の MCMC 出力は両システムともラベルスイッチングを示す
(`figures/hs_dashboard_full.png`/`py_dashboard_full.svg` で可視化 — ダッシュ
ボードは補正前の生データ)。**興味深い違いが観測された**: PyMC は
chain **内** でラベルが頻繁に反転する (mu1 の trace 図が同一 chain 内で
-2.8/+2.8 を往復・ess_bulk=7・r_hat=1.53) のに対し、hanalyze は各 chain が
1つのモードに **ロックされたまま** 動く (chain内 ESS=1000 は健全・
chain間でモードが割れるため素の r_hat のみ数十に跳ねる)。

**要約統計への対応**: `Model.hs` に `orderedChains` (posterior draw ごとに
`mu1>mu2` なら成分を入れ替えて `mu1<mu2` に正規化する後処理) を実装し、
`printSummary` の直前でのみ適用 (ダッシュボードは生データのまま・
問題を隠さず可視化する)。補正後は r_hat ≈ 0.999 に正常化し、公式
referenceと直接比較可能になった (下記「結果」参照)。PyMC 側は同様の
補正を行っていない (chain内switchingがあるため単純な後処理では直せない・
本 Phase の対象外)。

## ファイル

- `model.py` — PyMC 実装 (`pm.Mixture(w=[theta,1-theta], comp_dists=[...])`)。
- `Model.hs` — hanalyze 実装 (`Mixture` distribution + `orderedChains` 補正)。
- `run_pymc_matrix.py` — PyMC の CPU sampler×backend マトリクス
- `data/low_dim_gauss_mix.json` — posteriordb 由来データ (`N=1000`・`y`)
- `figures/` — `hs_dashboard_full.png`/`py_dashboard_full.svg` (生データ・
  ラベルスイッチング可視化)

## 実行方法

```bash
bench/venv/bin/python bench/posteriordb/04-low-dim-gauss-mix/model.py
bench/venv/bin/python bench/posteriordb/04-low-dim-gauss-mix/run_pymc_matrix.py

cabal build --project-file=cabal.project.plot posteriordb-low-dim-gauss-mix
cabal run   --project-file=cabal.project.plot posteriordb-low-dim-gauss-mix
```

**★順次実行必須** (並行実行禁止・OOM対策は `.claude/skills/posteriordb-bench/SKILL.md`
step5 参照)。

## 経路確認

`synthVecIR` = `Just` (データ束縛後のモデルで実測確認・Phase 90 A3 で
新設した `VGMixNorm2` family が効いている)。

## 結果

### 精度 (4 chain・warmup1000+draws1000・3者比較・ラベルスイッチング補正後)

| パラメータ | hanalyze (補正後) | 公式 reference |
|---|---|---|
| mu1    | -2.731 ± 0.040 | -2.734 ± 0.042 |
| mu2    |  2.870 ± 0.054 |  2.870 ± 0.055 |
| sigma1 |  1.029 ± 0.031 |  1.028 ± 0.031 |
| sigma2 |  1.023 ± 0.040 |  1.024 ± 0.040 |
| theta  |  0.6215 ± 0.0148 | 0.6215 ± 0.0155 |

全パラメータ小数第2-3位まで良好に一致 (`theta` は完全一致)。PyMC は
chain内ラベルスイッチングのため単純な後処理での補正ができず、本表からは
除外 (「既知の課題」参照)。

### 速度 (4 chain・warmup1000+draws1000・CPU 1コア固定)

PyMC は sampler×backend 7 通りを計測。**全経路で ess_bulk(theta)=7・
r_hat=1.53** (ラベルスイッチングの影響・「既知の課題」参照。速度の
相対比較の参考値として記録するが、収束していないため実質的な意味は
限定的):

| system | wall (ms) | ESS (theta・未収束) | 備考 |
|---|---:|---:|---|
| pymc + Numba (**PyMC最速CPU**) | 5245.3 | 7.0 | r_hat=1.53 (未収束) |
| nutpie + JAX | 7466.4 | 7.0 | r_hat=1.53 |
| nutpie + Numba | 9474.5 | 7.0 | r_hat=1.53 |
| numpyro | 10775.5 | 7.0 | r_hat=1.53 |
| pymc + JAX (own NUTS) | 17369.3 | 7.0 | r_hat=1.53 |
| pymc + CVM (真の C) | 29321.1 | 7.0 | r_hat=1.53 |
| blackjax | (失敗・後述) | — | — |
| **hanalyze (Haskell・vecIR・サンプリングのみ)** | **3816.2** | 1000.0 (chain内は健全) | 補正後r_hat≈0.999 |

hanalyze はサンプリング時間そのものは PyMC 最速 (pymc+numba) より速い
(3816ms vs 5245ms) が、収束状況が非対称 (PyMC=未収束/hanalyze=chain内は
健全) なため、この速度比較は**参考値に留める**(フェアな比較には
Stan の `ordered[2]` 相当の識別可能化が両システムに必要)。

### 図 — 両側とも「フルダッシュボード」1 枚に統一 (生データ・補正前)

- **Haskell**: `hs_dashboard_full.png` — mu1/mu2/theta の trace で chain
  ごとにモードがロックされている様子が見える (chain内ESSは健全)。
- **Python**: `py_dashboard_full.svg` — mu1 の trace が chain内で頻繁に
  反転している様子が見える。

### 既知の課題

- ラベルスイッチング (上記参照)。正攻法は Stan の `ordered[2]` 相当の
  順序制約プリミティブを hanalyze に実装すること (本 Phase 90 のスコープ
  外・需要が生じたら別途起票)。
- blackjax エラーは他モデルと同型 (原因未解明・深掘りしない方針)。
- PyMC 側の `overflow encountered in dot` 警告 (質量行列適応中) は
  ラベルスイッチングに起因する可能性が高いが未確認。
