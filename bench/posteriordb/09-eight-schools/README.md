# eight-schools (posteriordb: `eight_schools-eight_schools_noncentered`)

階層モデルの正準例「8校の補習授業効果」(Rubin 1981)。J=8 校それぞれの
推定効果 `y[j]` と既知の標準誤差 `sigma[j]` から、全体平均 `mu` と学校間
分散 `tau` を推定する部分プーリングモデル。Non-centered パラメタ化
(`theta_j = mu + tau*eta_j`・`eta_j ~ Normal(0,1)`) で funnel を回避する。
出典: `stan-dev/posteriordb`
(`posterior_database/models/stan/eight_schools_noncentered.stan`・
`posterior_database/data/data/eight_schools.json.zip`)。

- `reference_posterior_name: "eight_schools-eight_schools_noncentered"` —
  posteriordb に公式 reference posterior あり。hanalyze vs PyMC vs 公式
  reference の3者比較が可能。
- Prior: `mu ~ Normal(0,5)`・`tau ~ HalfCauchy(5)`・`eta_j ~ Normal(0,1)`
  (Stan 原典どおり)。

## ファイル

- `model.py` — PyMC 実装 + 合成ダッシュボード生成 (`py_dashboard_full.svg`・
  `../_common.py` の `make_pymc_dashboard` を使用)。chain 数等はコード中の
  定数 (`chains=4`) で固定。
- `Model.hs` — hanalyze 実装 (cabal exe `posteriordb-eight-schools`・
  `df |-> hbm` 高レベル API・`dataNamedX`/`dataNamedObs`・学校ごとの潜在
  変数 `eta` は `plateI`+`.#`+`!!` で宣言・gather する
  (`docs/api-guide/03-bayesian-hbm.md` の eightSchools 例と同型)。診断図は
  hgg `dashboardFullOf` で PNG 出力。
- `run_pymc_matrix.py` — PyMC の CPU sampler×backend マトリクス
- `data/eight_schools.json` — posteriordb 由来データ (`J=8`・`y`・`sigma`)
- `figures/` — `hs_dashboard_full.png`/`py_dashboard_full.svg`
  (詳細は `bench/posteriordb/README.md` 参照)

## 実行方法

```bash
# PyMC 最速 CPU 組み合わせの選定 (+ py_dashboard_full.svg 生成)
bench/venv/bin/python bench/posteriordb/09-eight-schools/model.py
bench/venv/bin/python bench/posteriordb/09-eight-schools/run_pymc_matrix.py

# hanalyze (+ hs_dashboard_full.png 生成・az.summary 相当の要約表を標準出力へ)
cabal build --project-file=cabal.project.plot posteriordb-eight-schools
cabal run --project-file=cabal.project.plot posteriordb-eight-schools
```

**★順次実行必須** (並行実行禁止・OOM対策は `.claude/skills/posteriordb-bench/SKILL.md`
step5 参照)。

## 経路確認 (「要改善」判定)

事前の静的解析は行わず、実際に `cabal build` → 実行して確認した
(`VGGauss` 族のみを使う小規模モデルのため vecIR 経路が通ることは自明で、
明示的な `synthVecIR` 呼び出しによる別確認は省略)。ビルド・実行とも成功
(fallback していれば速度が桁違いに遅くなるため、実測ベンチが事実上の
経路確認を兼ねる)。**要改善記録は無し** — 数値を正式記録する。

★Phase 96 A2 (2026-07-17): 省略していた明示確認を runtime `gradPathLabel`
で補完 = **「vecIR (ベクトル式 IR 高速経路)」** (root:
`experiments/phase96-mh-reconfirm/pathlabels.tsv`)。上記の自明判定どおり。

## 結果

### 精度 (4 chain・warmup1000+draws1000・3者比較)

posteriordb 公式 reference posterior
(`reference_posteriors/summary_statistics/{mean_value,mean_squared_value}`)
から sd を逆算 (`sd = sqrt(E[X²] - E[X]²)`) して比較。hanalyze/PyMC は各々の
実行時要約 (hanalyze: `Common.summarize`、PyMC: `az.summary`)。

| パラメータ | hanalyze | pymc (numba・最速CPU) | 公式 reference |
|---|---|---|---|
| mu  | 4.247 ± 3.077 | 4.4 ± 3.3 | 4.411 ± 3.309 |
| tau | 3.888 ± 3.497 | 3.6 ± 3.3 | 3.602 ± 3.198 |

3 系統とも小数第1位のオーダーで一致。8-schools は funnel 構造ゆえに
`tau` の裾が重く分散が大きいのは既知の性質 (non-centered パラメタ化で
funnel 自体は回避しているが、`tau` の事後分布自体は元々広い)。

### 速度 (4 chain・warmup1000+draws1000・CPU 1コア固定)

PyMC は sampler×backend 7 通りを計測 (blackjax は他モデル (01-glm-poisson)
と同型の `ValueError: cannot select an axis to squeeze` で失敗・原因未解明・
参考記録のみ):

| system | wall (ms) | ESS (mu) | ESS/sec | 対 hanalyze |
|---|---:|---:|---:|---:|
| **hanalyze (Haskell・自作 IR・サンプリングのみ)** | **146.7** | 1000.0 | 6816.6 | 基準 |
| pymc + Numba (**PyMC最速CPU**) | 2857.0 | 3487 | 1220.5 | 19.5× |
| nutpie + Numba | 4248.3 | 4578 | 1077.6 | 29.0× |
| nutpie + JAX | 4322.6 | 4427 | 1024.2 | 29.5× |
| numpyro | 6723.7 | 3678 | 547.0 | 45.8× |
| pymc + CVM (真の C) | 7095.0 | 3710 | 522.9 | 48.4× |
| pymc + JAX (own NUTS) | 14782.4 | 4288 | 290.1 | 100.8× |
| blackjax | (失敗・後述) | — | — | — |

**hanalyze は PyMC 最速 CPU 組み合わせ (nutpie+Numba) の 19.5 倍**
(2026-07-11 追記: `Common.timeSamplingMs` でサンプリングのみ分離計測して
訂正。以前記載していた「4007ms・プロセス全体」計測は GHC起動+vecIR
コンパイル試行+`dashboardFullOf` PNG生成の固定コスト (≈3860ms) を含んでいた
誤った比較だった)。ESS の算出方法が hanalyze (Geyer's IMSE・`Common.hs`
既知の注記) と arviz (rank-normalized bulk ESS) で異なるため ESS/sec の
絶対値比較は目安に留め、**同一設定 (4chain×1000draws×1000warmup) での
壁時計比較**を主指標とする。

### 図 — 両側とも「フルダッシュボード」1 枚に統一

- **Haskell**: `hs_dashboard_full.png`
- **Python**: `py_dashboard_full.svg`

### 既知の課題 (2026-07-11 訂正済み)

- ~~速度比較が他モデルと異質~~ → **解消済み**。当初は `time <binary>` で
  プロセス全体 (GHC起動+モデルコンパイル試行+サンプリング+
  `dashboardFullOf` PNG生成) を計測していたため、J=8 の極小データでは
  固定コスト (≈3860ms) がサンプリング時間 (146.7ms) を圧倒し比較不能
  だった。`bench/posteriordb/Common.hs` に `timeSamplingMs`
  (`hbmChainsR` の thunk を `evaluate . force` で強制する共通ヘルパ・
  PyMC の `t0=perf_counter(); pm.sample()` と対応) を追加して分離計測し、
  上表のとおり正しく比較できるようになった。
- blackjax エラーは 01-glm-poisson と同型 (原因未解明・深掘りしない方針)。
