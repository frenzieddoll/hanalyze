# hmm-example (posteriordb: `hmm_example-hmm_example`)

単純な隠れマルコフモデル (K=2状態・N=100観測・1次元Gaussian放出)。離散
潜在状態は forward algorithm で周辺化する。出典: `stan-dev/posteriordb`
(`posterior_database/models/stan/hmm_example.stan`・
`posterior_database/data/data/hmm_example.json.zip`)。

- **`reference_posterior_name: "hmm_example-hmm_example"`** (posteriordb
  に公式 reference あり・hanalyze vs PyMC vs 公式referenceの**3者比較が
  可能**)。
- Prior: `mu_1 ~ Normal(3,1)`・`mu_2 ~ Normal(10,1)` (Stan原典は
  `positive_ordered[2]` で mu_1<mu_2 を強制・下記「順序制約」参照)・
  `theta1,theta2 ~ Dirichlet(1,1)` (simplex上一様・Stan原典の暗黙一様
  事前分布に対応)。
- 尤度: forward algorithm で状態列を周辺化した周辺対数尤度
  (`hmmForwardLogLik`・`potential`)。pi0 (初期分布) 項は Stan 原典に
  無い (`gamma[1,k]` に log π_0 が加算されない) ため実装しない。

## hanalyzeに既存の `hmmForwardLogLik`/`dirichlet` helperをそのまま活用

Phase 39-A4 で実装済みの `hmmForwardLogLik` (log-space forward
recursion・Rabiner 1989) と `dirichlet` helper をそのまま利用できた。
新規実装が必要だったのは HMM 固有のモデル構造の組み立てのみ。

## ★実測で踏んだ罠1: 順序制約なしだとラベルスイッチングでr_hat=17台

`positive_ordered[K]` (mu[1]<mu[2]) に対応する分布は hanalyze に無い。
当初 `mu_1 ~ Normal(3,1)`・`mu_2 ~ Normal(10,1)` を**独立に (順序制約
なしで)** サンプリングしたところ、**chain間でラベルスイッチング
(mu_1/mu_2 の意味がchainごとに入れ替わる) が発生し r_hat が17台**という
壊滅的な値になった。両事前分布が7σ以上離れていても、順序を強制する
制約が全く無いと exchangeability により初期値次第でどちらのラベル
付けにも収束しうる。

**解決**: `mu_2 = mu_1 + gap` (`gap ~ HalfNormal(5)`) という**加算的な
順序制約**を導入し、gap 自身の sample 事前分布の寄与を `potential` で
正確に打ち消して `Normal(10,1)` の寄与に置き換えた
(`potential "mu2_prior" (logDensity (Normal 10 1) mu2 - logDensity (HalfNormal 5) gap)`)。
これは近似ではなく**数学的に厳密**: 総寄与 = HalfNormal(gap)の寄与 +
potential = logDensity(Normal 10 1, mu2) と完全に一致する (加算シフトの
ヤコビアンは1なので Stan の `positive_ordered` 変換と等価)。修正後は
r_hat 1.00・reference posteriorとも良く一致 (下記「結果」参照)。
PyMC側 (`model.py`) も同じ構成 (`pm.Potential` で打ち消し) を使う。

## ★実測で踏んだ罠2: PyMC own-NUTSがr_hat=1.5で収束せず・nutpie/numpyroは収束

`pytensor.scan` ベースの forward algorithm potential を使った場合、
PyMC 自前の NUTS (`nuts_sampler="pymc"`、numba/cvm/jax いずれのbackendも)
は全パラメータで r_hat≈1.5 という収束失敗を示した。`nutpie`・`numpyro`
に切り替えると r_hat=1.00 まで正常に収束する (下記「速度」表参照)。
scan を含むモデルでの PyMC own-NUTS のマスマトリクス適応が上手く
機能しなかったと見られる (原因未解明・深掘りは本Phaseの主眼外)。
`model.py` の `main()` は `nutpie` を既定にし、`run_pymc_matrix.py` は
全sampler×backendを網羅して比較記録する。

## ファイル

- `model.py` — PyMC 実装 (`pytensor.scan` で forward algorithm を手動
  実装・`nuts_sampler="nutpie"` を既定に使用)。
- `Model.hs` — hanalyze 実装 (`hmmForwardLogLik`/`dirichlet` を活用)。
  Phase 92 B4 で per-draw NUTS 診断出力 + 全 chain draw の CSV export
  (`hmm_draws_postwarmup.csv`・生成物につき非追跡) + `seed N` 引数を計装。
- `run_pymc_matrix.py` — PyMC の CPU sampler×backend マトリクス。
- `run_nutpie_diag.py` / `compute_hanalyze_ess.py` — Phase 92 B4 の
  ess/draw 効率調査 (per-draw 診断 + 同一指標 ess_bulk 比較)。生ログ =
  `hmm_ess_diag_20260717.log`。
- `data/hmm_example.json` — posteriordb 由来データ (`N=100`・`K=2`・`y`)。
- `figures/` — `hs_dashboard_full.png` (hanalyze・PPCパネルは空 — 標準
  `observe` を使わずpotentialのみで尤度を構成したため)・
  `py_dashboard_forest.svg`/`py_dashboard_full.svg` (PyMC・PPC不可の
  ため forest/energy のみの簡易ダッシュボード)。

## 実行方法

```bash
bench/venv/bin/python bench/posteriordb/14-hmm-example/model.py
bench/venv/bin/python bench/posteriordb/14-hmm-example/run_pymc_matrix.py

cabal build --project-file=cabal.project.plot posteriordb-hmm
cabal run   --project-file=cabal.project.plot posteriordb-hmm
```

**★順次実行必須** (並行実行禁止・OOM対策は `.claude/skills/posteriordb-bench/SKILL.md`
step5 参照)。

## 経路確認

Phase 92 A2 以降 = **HMM forward-backward 閉形式随伴** (`HmmForwardNormal`
構造化 primitive・`hmmAnalyticVG`・AD tape ゼロ)。それ以前は `potential` の
raw log-density 項が vecIR 対象外のため `synthVecIR = Nothing` (legacy
walk+ad) だった。

## 結果

### 精度 (4 chain・warmup1000+draws1000・**3者比較**)

| パラメータ | hanalyze | pymc (nutpie・収束した中で採用) | posteriordb 公式reference |
|---|---|---|---|
| mu_1     | 3.0213 ± 0.2226 | 3.03 ± 0.232  | 3.0215 ± 0.2245 |
| mu_2     | 8.8186 ± 0.1086 | 8.827 ± 0.11  | 8.8273 ± 0.1106 |
| theta1_0 | 0.6668 ± 0.0985 | 0.667 ± 0.103 | 0.6666 ± 0.1012 |
| theta1_1 | 0.3332 ± 0.0985 | 0.333 ± 0.103 | 0.3334 ± 0.1012 |
| theta2_0 | 0.0723 ± 0.0263 | 0.0732 ± 0.0284 | 0.0731 ± 0.0284 |
| theta2_1 | 0.9277 ± 0.0263 | 0.9268 ± 0.0284 | 0.9269 ± 0.0284 |

**3系統とも小数第2〜3位まで一致**。R-hat: hanalyze 0.999-1.000・
pymc(nutpie) 1.00。順序制約の厳密な打ち消し構成 (上記「罠1」参照) が
正しく機能していることを reference posterior との一致で確認できた。

### 速度 (4 chain・warmup1000+draws1000・CPU 1コア固定)

★この節の初出記録 (hanalyze 71704.3ms・numpyro 17508.3ms 等) は **stale**
(別時点・是正の経緯は Phase 92 A1)。最新の確定値 (2026-07-17・Phase 92
A2+B2+B3 後・`hmm_ess_diag_20260717.log`):

| 指標 (統一基準) | hanalyze | nutpie+jax (PyMC最速) | 比 |
|---|---:|---:|---:|
| sampling wall (tune/warmup+draws・compile除外) | 5325.3ms | ≈2965ms | 1.80× 遅い |
| (参考) compile 込み wall | 5325.3ms | ≈3711ms | 1.43× 遅い |
| ess_bulk(mu_1)/draw (同一指標 arviz・seed1-3) | 0.77-0.86 | 0.31-0.35 | **2.4-2.5× 優位** |
| **ESS/sec (mu_1)** | **~590-650** | ~420-470 | **1.3-1.4× 勝ち** |

- 改善前 (legacy walk+ad) = 28870.0ms (`hmm_before_A1.log`)。Phase 92 の
  `HmmForwardNormal` 閉形式随伴 + B2/B3 精錬で累積 5.42×。
- **ess/draw の見かけの劣後は指標アーチファクトだった** (Phase 92 B4):
  Haskell 側 `summarize` の ess は chain 0 のみの Geyer IMSE (tau 下限
  クランプで n=1000 頭打ち) であり、arviz の rank-normalized ess_bulk
  (4chain) と直接比較できない。同一指標では hanalyze の方が高効率
  (平均 tree depth 3.1 vs 2.4 の長軌道で自己相関が低い・divergence 両側 0)。
  → **B4-② で根本対処済**: `summarize` を arviz 互換の `essBulk`
  (rank-normalized・全 chain) に切替 (`hmm_after_B4.log` = 新表示の
  リファレンス・全 7 パラメータで arviz と一致確認済)。
- PyMC own-NUTS (numba/cvm/jax) は r_hat≈1.5 で収束せず (下記「罠2」)・
  blackjax は他モデル同型エラーで失敗。旧記録の詳細は
  `pymc_matrix_A1.log` と git 履歴参照。

### 図

`figures/hs_dashboard_full.png` /
`figures/py_dashboard_forest.svg` /
`figures/py_dashboard_full.svg`

(図はベンチ実行後に `figures/` へ生成される。リポジトリには含めていない)

### 既知の課題

- **PyMC own-NUTSがscanベースpotentialモデルで収束しない**: nutpie/
  numpyroへの切替で解消 (原因未解明・深掘りしない方針)。
- ~~**vecIRギャップ (b)**: forward algorithmの逐次再帰がvecIR未対応で
  約4倍の遅延~~ → **Phase 92 で解消** (`HmmForwardNormal` 閉形式随伴・
  28870→5325ms)。
- blackjaxエラーは他モデルと同型 (原因未解明・深掘りしない方針)。
- PPCパネルが空 (標準observeを使わずpotentialのみで尤度を構成した
  ため・HMMのようなmarginalized likelihoodモデルの構造的な制約)。
