# irt-2pl (posteriordb: `irt_2pl-irt_2pl`)

項目反応理論 2PL モデル (I=20 項目 × J=100 人・`a[i]*(theta[j]-b[i])` という、
2 つの独立ラテント配列 (受験者能力 `theta`・項目識別力 `a`・困難度 `b`) を
跨ぐ乗算項)。出典: `stan-dev/posteriordb`
(`posterior_database/models/stan/irt_2pl.stan`・
`posterior_database/data/data/irt_2pl.json.zip`)。

- `reference_posterior_name: null` (posteriordb に公式 reference 無し・
  hanalyze vs PyMC の2者比較のみ。★Phase 89 時点の README は「あり」と
  誤記していたが Phase 90 A5 で実測訂正した — posteriordb の
  `posteriors/irt_2pl-irt_2pl.json` を直接確認済み)。
- Prior: `sigma_theta,sigma_a,sigma_b ~ HalfCauchy(2)`・
  `theta_j ~ Normal(0,sigma_theta)`・`a_i ~ LogNormal(0,sigma_a)`・
  `mu_b ~ Normal(0,5)`・`b_i ~ Normal(mu_b,sigma_b)` (Stan 原典どおり)。
- 尤度: `y[i][j] ~ Bernoulli(invlogit(a_i*(theta_j-b_i)))` (2000観測)。

## Phase 90 A5 (vecIR ギャップ解消) との関係

Phase 89 では小規模トイモデルで `synthVecIR` が一貫して `Nothing` になる
ことを確認し、「(a) DSL機能ギャップ/(b) 遅い経路フォールバック いずれとも
言い切れない」として分類未確定のまま保留していた。

Phase 90 A1 実測調査でトイモデルを再検証したところ、**「独立2ラテント
配列を跨ぐ積」自体は vecIR 対象外ではない** (`as!!i * (thetas!!j-bs!!i)`
という構造だけなら `synthVecIR` = `Just`) と判明。真因は別のところに
あった: `IR.hs` の `famOf` (family absorb = prior のベクトル化) が
「family 全員の事前分布が構造同一の `Normal(m,τ)` であること」を要求して
おり、`tryGroup` が `mapM famOf (...)` で実装されていたため **family の
うち1つでも非Normal事前分布 (本モデルでは `a ~ LogNormal`) だと、
likelihood 側の吸収 (`g`) ごと丸ごと `Nothing` になっていた** (family
absorb の失敗が likelihood 吸収の成功を巻き添えにする設計だった)。

Phase 90 A5 で `tryGroup` を「family absorb 失敗は `fams` から単に除外
するだけ・likelihood 側の吸収は継続する」という fault-tolerant 設計に
修正 (`IR.hs` 1箇所・`fams <- mapM famOf (...)` → `let fams = [f | Just f
<- map famOf (...)]`)。`theta`/`b` (Normal階層事前分布) は従来どおり
family absorb され、`a` (LogNormal事前分布) は既存の `constPriorsOf`/
残差AD経路 (`Gradient.hs`。元々 Uniform/LogNormal 等の解析勾配対応
分布を扱うための既存メカニズム) に自然にフォールバックする。

トイモデル (`a ~ Uniform`) で `synthVecIR` = `Just` を実測確認、実サンプ
リングで `a` の draw が正しく `[0,5]` に収まることも確認済み。`cabal test`
全1356件PASS (既存モデルへの回帰なし)。

## ファイル

- `model.py` — PyMC 実装 (`a[:,None]*(theta[None,:]-b[:,None])` で
  I×J行列を構築し `pm.Bernoulli(logit_p=...)`)。
- `Model.hs` — hanalyze 実装 (`plateI` で theta/a/b を族宣言・`plateForM_`
  で (item,person) 全ペアを観測)。
- `run_pymc_matrix.py` — PyMC の CPU sampler×backend マトリクス
- `data/irt_2pl.json` — posteriordb 由来データ (`I=20`・`J=100`・`y` = 20×100行列)
- `figures/` — `hs_dashboard_full.png`/`py_dashboard_full.svg`
  (★140 latent (theta:100+a:20+b:20) のため `dashboardOf` = 健全性パネル
  のみ使用 — 05-mh と同じ理由)

## 実行方法

```bash
bench/venv/bin/python bench/posteriordb/06-irt-2pl/model.py
bench/venv/bin/python bench/posteriordb/06-irt-2pl/run_pymc_matrix.py

cabal build --project-file=cabal.project.plot posteriordb-irt-2pl
cabal run   --project-file=cabal.project.plot posteriordb-irt-2pl
```

**★順次実行必須** (並行実行禁止・OOM対策は `.claude/skills/posteriordb-bench/SKILL.md`
step5 参照)。2000観測×140latentのNUTSは数分かかる (下記「速度」参照)。

## 経路確認

`synthVecIR` = `Just` (データ束縛後のモデルで実測確認・Phase 90 A5 の
fault-tolerant famOf 修正が効いている)。

## 結果

### 精度 (4 chain・warmup1000+draws1000・2者比較・reference無し)

| パラメータ | hanalyze | pymc (nutpie+numba・最速CPU) |
|---|---|---|
| sigma_theta | 0.999 ± 0.166 | (要再計測・matrix実行では未収集) |
| sigma_a     | 0.532 ± 0.184 | (同上) |
| mu_b        | -0.924 ± 0.537 | -0.9 ± 0.5 (pymc既定numba) |
| sigma_b     | 2.160 ± 0.484 | 1.9 ± 0.6 (pymc既定numba) |

平均値のオーダーは近い。**収束の質に明確な違いが観測された**:
hanalyze は r_hat 1.0002-1.0493 (良好) だったのに対し、PyMC 既定
(pymc+numba) は `sigma_theta`/`sigma_a`/`sigma_b` で r_hat 1.53-1.54・
ess_bulk=7 と収束していない (`a` の trace に 20-30 まで飛ぶ明らかな
発散が見られる・「図」参照)。PyMC 側で `nutpie` (別アルゴリズム) を
使うと r_hat=1.000 まで改善する (下記「速度」表) — つまり**このモデルは
中心化パラメタ化 (funnel 構造) がサンプラーの選択に敏感**であり、
hanalyze (walk+ad + vecIR混成経路) は PyMC の既定 (pymc+numba) より
むしろ安定して収束した、という結果になった。

### 速度 (4 chain・warmup1000+draws1000・CPU 1コア固定)

PyMC は sampler×backend 7 通りを計測。ess/sec 基準で最速を選定
(blackjax は他モデルと同型のエラーで失敗)。numpyro は r_hat=2.3 と
収束破綻のため実質参考外:

| system | wall (ms) | ESS (mu_b) | ESS/sec | rhat |
|---|---:|---:|---:|---:|
| nutpie + Numba (**PyMC最速CPU・ess/s基準**) | 21875.2 | 3199 | 146.24 | 1.000 |
| nutpie + JAX | 41842.2 | 2458 | 58.74 | 1.000 |
| pymc + Numba | 23239.5 | 75 | 3.23 | 1.140 |
| pymc + CVM (真の C) | 40047.4 | 72 | 1.80 | 1.120 |
| pymc + JAX (own NUTS) | 71164.4 | 69 | 0.97 | 1.150 |
| numpyro | 19138.9 | 6 | 0.31 | 2.300 (収束破綻) |
| blackjax | (失敗・後述) | — | — | — |
| **hanalyze (Haskell・vecIR+残差AD混成・サンプリングのみ)** | **161404.0** | 137.9-249.2 (要素ごと) | ≈1.6 (mu_b基準) | 1.0002-1.0493 |

wall clock では hanalyze (161秒) が PyMC最速 (nutpie+numba・22秒) の
**約7.4倍遅い**。140 latent中、vecIR で吸収されるのは theta/b の
family absorb + likelihood のみで、`a` (LogNormal) の prior・
`sigma_theta`/`sigma_a`/`mu_b`/`sigma_b` の4ハイパラメータは既存の
残差AD経路 (legacy `ad`・O(N)の勾配再計算) に残るため、GP/gp-regr同様
「vecIR部分は速いが混成モデル全体としては最適化の余地あり」という
結果になった。ただし**収束の質 (r_hat) では hanalyze が既定PyMCより
明確に優れている**点は特筆に値する。

### 図

- **Haskell**: `hs_dashboard_full.png` — DAG は theta(100)/a(20)/b(20)を
  それぞれ plate で集約表示。
- **Python**: `py_dashboard_full.svg` (pymc+numba) — `a` の trace に
  明らかな発散 (20-30まで飛ぶ) が見える。

### 既知の課題

- 中心化パラメタ化 (`theta ~ Normal(0,sigma_theta)` 等) の funnel構造は
  サンプラー依存性が高い。非中心化パラメタ化 (`theta_raw ~ Normal(0,1)`;
  `theta = sigma_theta*theta_raw`) にすればPyMC側の収束も改善する可能性が
  高いが、Stan原典に忠実な実装を優先し変更していない。
- hanalyze の速度 (161秒) は PyMC最速 (nutpie) に対し見劣りする。混成
  モデル (vecIR吸収部分+残差AD部分) の残差AD側最適化は本Phaseのスコープ外。
- blackjax エラーは他モデルと同型 (原因未解明・深掘りしない方針)。
