# dogs (posteriordb: `dogs-dogs`)

Solomon & Wynne (1953) の犬の回避学習実験 (Gelman & Hill 2006, ARM本 Ch.24)。
30 匹の犬 × 25 試行、各試行で電気ショックを回避できたか (`y=0`) されたか
(`y=1`) を記録した二値データ。累積の回避回数 (`n_avoid`)・被ショック回数
(`n_shock`) を共変量とするロジスティック回帰でモデル化する。出典:
`stan-dev/posteriordb` (`posterior_database/models/stan/dogs.stan`・
`posterior_database/data/data/dogs.json.zip`)。

- `reference_posterior_name: null` — posteriordb に公式 reference posterior
  無し。hanalyze vs PyMC の2者比較のみ。
- Prior: `beta ~ normal(0, 100)` (3パラメータ、実質 flat に近い弱情報事前)。
- Stan 原典の transformed parameters (`n_avoid`/`n_shock`) は observed data
  `y` のみに依存する累積和 (beta 非依存)。Stan は毎反復再計算するが数学的
  には同一なので、両実装ともサンプリング前の前処理として1度だけ計算する。

## ファイル

- `model.py` — PyMC 実装 (`cumulative_counts` で n_avoid/n_shock を前処理) +
  合成ダッシュボード生成 (`py_dashboard_full.svg`・`../_common.py` の
  `make_pymc_dashboard` を使用)。
- `Model.hs` — hanalyze 実装 (`df |-> hbm` 高レベル API・
  `dataNamedX`/`dataNamedObs`/`plateForM_`・aeson で JSON 読込・
  `cumulativeCounts` で同じ前処理)。診断図は hgg `dashboardFullOf`
  で PNG 出力 (rasterific backend)。`writeDrawsCSV` で生 draw を
  `hs_draws.csv` にダンプ (arviz 独立検証用)。
- `run_pymc_matrix.py` — PyMC の CPU sampler×backend マトリクス。
- `data/dogs_data.json` — posteriordb 由来データ (`n_dogs`/`n_trials`/`y`)。
- `figures/` — `hs_dashboard_full.png`/`py_dashboard_full.svg`。

## 実行方法

```bash
bench/venv/bin/python bench/posteriordb/02-dogs/model.py
bench/venv/bin/python bench/posteriordb/02-dogs/run_pymc_matrix.py

cabal build --project-file=cabal.project.plot posteriordb-dogs
cabal run   --project-file=cabal.project.plot posteriordb-dogs
```

## 経路確認

`synthVecIR` = `Just` (vecIR 高速経路・`cabal repl` で dogsModel に直接
`synthVecIR` を呼んで実測確認)。**要改善記録は無し** — 正式に数値を記録する。

## 結果

### 精度 (4 chain・warmup1000+draws1000・独立検証 = arviz)

hanalyze の生 draw (`hs_draws.csv`) を `az.from_dict` で読み込み、hanalyze
自身の診断コードは使わず arviz で独立に再計算した。

| パラメータ | hanalyze | pymc (numba・最速CPU) |
|---|---|---|
| beta1 | 1.808 ± 0.231 | 1.8 ± 0.233 |
| beta2 | -0.358 ± 0.038 | -0.358 ± 0.04 |
| beta3 | -0.211 ± 0.044 | -0.21 ± 0.044 |

全パラメータ小数第2〜3位まで一致。R-hat = 1.00 (両系統とも)。

### 速度 (4 chain・warmup1000+draws1000・CPU 1コア固定)

PyMC は sampler×backend 7 通りを計測 (blackjax はこのモデルでも
`ValueError: cannot select an axis to squeeze` により失敗・未解明のため
参考記録)。hanalyze の wall は図生成/CSV出力を除いたサンプリング単体 (5回
計測の中央値、PyMC 側 (`run_pymc_matrix.py`) が `pm.sample` のみを計測する
方式と揃えた)。

| system | wall (ms) | ESS (beta1・arviz計算) | ESS/sec | 対 hanalyze |
|---|---:|---:|---:|---:|
| **hanalyze (Haskell・自作 IR)** | 4682 | 2609 | **557.2** | 基準 |
| pymc + Numba (**PyMC最速CPU**) | 5148.7 | 1475 | 286.5 | 1.95× |
| pymc + CVM (真の C) | 5890.8 | 1683 | 285.7 | 1.95× |
| nutpie + Numba | 4797.3 | 1245 | 259.5 | 2.15× |
| numpyro | 8773.5 | 1850 | 210.9 | 2.64× |
| nutpie + JAX | 10394.2 | 1289 | 124.0 | 4.49× |
| pymc + JAX (own NUTS) | 30246.6 | 1677 | 55.4 | 10.06× |
| blackjax | (失敗・後述) | — | — | — |

**hanalyze は PyMC 最速 CPU 組み合わせ (numba/CVM 僅差) の約 2 倍**。
GLM-Poisson (18.4×) と比べ倍率は大きく縮小した — 750 観測 (30匹×25試行)・
共変量間の強い相関 (`n_avoid`/`n_shock` は同一試行で片方が増えれば他方は
増えない排他的累積量) により幾何がより難しく、双方とも ESS/sec が伸び
悩んだ結果と見られる (深掘りは本 Phase の主眼外)。

### 図 — 両側とも「フルダッシュボード」1 枚に統一

各モデル2枚のみ (`hs_dashboard_full.png`/`py_dashboard_full.svg`)。構成は
共通: 上段2×2 (モデル構造DAG/forest/PPC/energy) + 下段paramごと
[事後分布|trace]。

- [hs_dashboard_full.png](./figures/hs_dashboard_full.png)
- [py_dashboard_full.svg](./figures/py_dashboard_full.svg)

### 既知の課題

- **blackjax エラー**: GLM-Poisson (Phase 89 モデル1) と同一の
  `ValueError` (shape=(4,2) の squeeze 失敗)。2 モデル連続で発生・
  bounded/unbounded 事前分布どちらでも起きるため「特定モデル固有」説は
  後退し、pmap ベースの blackjax 統合自体の既知の癖である可能性が高まった
  が未解明のまま (深掘りは本 Phase の主眼外・参考記録のみ)。
- **hanalyze 自身の `ess` (Common.hs) は Geyer-tau クランプにより真の効率を
  過小評価する** (`printSummary` が報告する beta1 の ess=613.6 は、
  arviz 独立再計算の ess_bulk=2609 の4分の1以下)。GLM-Poisson でも判明
  済みの既知の癖だが、本モデルで改めて確認された。速度比較には必ず
  arviz 独立計算値を使うこと (この README の速度表も arviz 値を採用)。

全体サマリは `bench/posteriordb/README.md` にも記録。
