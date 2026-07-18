# ark (posteriordb: `arK-arK`)

AR(K) (K次自己回帰) 時系列モデル (K=5・T=200)。出典: `stan-dev/posteriordb`
(`posterior_database/models/stan/arK.stan`・
`posterior_database/data/data/arK.json.zip`)。

- **`reference_posterior_name: "arK-arK"`** (posteriordb に公式 reference
  あり・hanalyze vs PyMC vs 公式referenceの**3者比較が可能**)。
- Prior: `alpha ~ Normal(0,10)`・`beta_k ~ Normal(0,10)` (k=1..5)・
  `sigma ~ HalfCauchy(2.5)` (Stan 原典の `cauchy(0,2.5)` に対応する
  半コーシー)。
- 尤度: `y[t] ~ Normal(alpha + sum_k beta_k*y[t-k], sigma)` (t=K+1..T)。

## GARCH (03-garch11) と異なり平均のみが過去に依存 — 静的線形回帰に帰着

GARCH は**分散**が過去に再帰的に依存するため実装・計測とも難しかった
(03-garch11 は PyMC 側 OOM で保留中)。AR(K) は**平均**のみが過去の値に
依存し、しかも `y` は全て既知データ (潜在変数ではない) のため、モデルと
しては「K個のラグ特徴量 (`y[t-1]..y[t-K]`) を使った静的な線形回帰」に
帰着する — 自己参照的な再帰は存在しない。実装・計測とも01-glm-poisson
と同程度に単純。

## ファイル

- `model.py` — PyMC 実装 (`pm.math.dot(lags, beta)` でラグ特徴量の内積)。
- `Model.hs` — hanalyze 実装 (`lagDesign` でラグ特徴量を Haskell 側で
  事前計算し、`lag1`..`lag5` の5列として df 束縛)。
- `run_pymc_matrix.py` — PyMC の CPU sampler×backend マトリクス。
- `data/arK.json` — posteriordb 由来データ (`K=5`・`T=200`・`y`)。
- `figures/` — `hs_dashboard_full.png`/`py_dashboard_full.svg`
  (7 param のみのため `dashboardFullOf` をそのまま使用)。

## 実行方法

```bash
bench/venv/bin/python bench/posteriordb/12-ark/model.py
bench/venv/bin/python bench/posteriordb/12-ark/run_pymc_matrix.py

cabal build --project-file=cabal.project.plot posteriordb-ark
cabal run   --project-file=cabal.project.plot posteriordb-ark
```

**★順次実行必須** (並行実行禁止・OOM対策は `.claude/skills/posteriordb-bench/SKILL.md`
step5 参照)。

## 経路確認

★Phase 96 A2 (2026-07-17) 実測: runtime `gradPathLabel` = **「Gaussian LM
閉形式ブロック (解析勾配)」** (root:
`experiments/phase96-mh-reconfirm/pathlabels.tsv`)。Phase 91 A4 でも同旨を
確認済み (旧記録は生モデルを `synthVecIR` に渡した誤診断由来)。

以下は Phase 89 起票時の旧記録 (**stale**):

> `synthVecIR` = `Nothing` (legacy walk+ad へのフォールバック)。ただし
> T=200・K=5 という小規模な静的回帰のため、legacy 経路でも十分高速。

## 結果

### 精度 (4 chain・warmup1000+draws1000・**3者比較**)

| パラメータ | hanalyze | pymc (nutpie+numba・最速CPU) | posteriordb 公式reference |
|---|---|---|---|
| alpha   | -0.0011 ± 0.0112 | -0.0006 ± 0.0104 | -0.0007 ± 0.0107 |
| beta_1  |  0.6901 ± 0.0700 |  0.693 ± 0.07    |  0.6922 ± 0.0706 |
| beta_2  |  0.4370 ± 0.0858 |  0.439 ± 0.086   |  0.439 ± 0.0873 |
| beta_3  |  0.1079 ± 0.0903 |  0.104 ± 0.092   |  0.1058 ± 0.0931 |
| beta_4  | -0.0324 ± 0.0869 | -0.036 ± 0.087   | -0.0354 ± 0.086 |
| beta_5  | -0.3049 ± 0.0693 | -0.301 ± 0.071   | -0.3015 ± 0.0699 |
| sigma   |  0.1504 ± 0.0074 |  0.1503 ± 0.0075 |  0.1506 ± 0.0078 |

**3系統とも小数第2〜3位まで一致** (公式referenceは10chain×1000drawの
参照実装・詳細は `posterior_database/reference_posteriors/draws/draws/arK-arK.json.zip`)。
R-hat: hanalyze 0.999-1.000・pymc 1.00・ess は hanalyze/pymc とも
2700-3000+台 (混合良好)。

### 速度 (4 chain・warmup1000+draws1000・CPU 1コア固定)

PyMC は sampler×backend 7 通りを計測 (blackjax は他モデルと同型の
`ValueError` で失敗):

| system | wall (ms) | ess_bulk(alpha) | ESS/sec | rhat |
|---|---:|---:|---:|---:|
| nutpie + Numba (**PyMC最速CPU**) | 5121.0 | 5184 | 1012.29 | 1.000 |
| pymc + Numba | 8314.3 | 2987 | 359.26 | 1.000 |
| numpyro | 9734.8 | 4171 | 428.46 | 1.000 |
| pymc + CVM (真の C) | 13715.4 | 3440 | 250.81 | 1.000 |
| nutpie + JAX | 19775.3 | 5471 | 276.66 | 1.010 |
| pymc + JAX (own NUTS) | 62259.4 | 3267 | 52.47 | 1.000 |
| blackjax | (失敗・後述) | — | — | — |
| **hanalyze (Haskell・legacy walk+ad・サンプリングのみ)** | **1142.7** | 1000 (ess固定表示・実質混合良好) | — | 0.999-1.000 |

`synthVecIR = Nothing` (legacy walk+ad 経路) だが、**hanalyze (1142.7ms)
は PyMC 最速CPU (nutpie+numba・5121.0ms) の約 4.5 倍高速**。GARCH
(分散再帰) と異なり、AR(K) は「静的な小規模線形回帰 (T=200・7 latent)」
に帰着するため、vecIR 非対応でも legacy 経路の O(N) 残差AD再計算コスト
自体が小さく、GHCネイティブコードの実行効率がPyMCのJIT起動コストを
上回った筋の通った結果 (01-glm-poisson と同様のパターン)。

### 図

`figures/hs_dashboard_full.png` /
`figures/py_dashboard_full.svg`

(図はベンチ実行後に `figures/` へ生成される。リポジトリには含めていない)

### 既知の課題

- 特になし (要改善記録は無し)。blackjaxエラーのみ他モデルと同型
  (原因未解明・深掘りしない方針)。
