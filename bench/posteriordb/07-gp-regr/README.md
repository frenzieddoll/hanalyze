# gp-regr (posteriordb: `gp_pois_regr-gp_regr`)

Gaussian Process 回帰 (RBF/exponentiated-quadratic カーネル + Gaussian
尤度)。N=11 点の観測 `(x_i, y_i)` から、GP のハイパーパラメータ
(長さスケール `rho`・振幅 `alpha`・観測ノイズ `sigma`) を推定する。出典:
`stan-dev/posteriordb` (`posterior_database/models/stan/gp_regr.stan`・
`posterior_database/data/data/gp_pois_regr.json.zip`)。

- `reference_posterior_name: "gp_pois_regr-gp_regr"` — posteriordb に公式
  reference posterior あり。hanalyze vs PyMC vs 公式 reference の3者比較が
  可能。
- Prior: `rho ~ Gamma(25,4)`・`alpha ~ HalfNormal(2)`・`sigma ~ HalfNormal(1)`
  (Stan 原典どおり。`<lower=0>` 制約付き `normal(0,·)` は hanalyze の
  `HalfNormal` と数学的に等価)。
- 尤度: `y ~ MultiNormalCholesky(0, L_cov)` (`L_cov` = `gp_exp_quad_cov(x,
  alpha, rho) + diag(sigma)` の Cholesky分解)。

## Phase 90 A2 (vecIR ギャップ解消) との関係

このモデルは Phase 89 で「(a) DSL機能ギャップ: GP カーネル+Cholesky分解の
プリミティブが無い」としてフォールバック確定・実装ファイル未作成のまま
保留されていた。Phase 90 A1 実測調査で「密行列は vecIR に構造的に載らない
が、既存の AD対応 Cholesky (`choleskyL`、`mvNormalLatent`/`lkjCorrCholesky`
と共用) を流用すれば legacy walk+ad 経路でそのまま動く」と判定し、A2 で
以下を実装した:

- `Hanalyze.Model.HBM.gpExpQuadCov`/`gpLatent`
  (`Model.hs`。GP カーネル構築 + 既存 Cholesky の再利用)。本モデル自体は
  尤度が直接 `MvNormal` (Stan の `multi_normal_cholesky` と等価) なので
  `gpExpQuadCov` のみ使用 (`gpLatent` の non-centered 潜在変数化は不要)。
- **副次的に判明したバグ**: `dashboardFullOf` の PPC (事後予測チェック)
  パネルが、観測分布を問わず `sampleDist` (スカラー専用) を呼んでおり、
  `MvNormal` 観測 (1個の k-vector 観測がフラット化された形) では即座に
  `error` していた (`Sampling.hs:107` `MvNormal: observation-only`)。
  `Sampling.hs` に次元 k で ys をチャンクして `sampleMvDist` に委譲する
  `sampleObsRep` を追加し、`Plot/Bayes.hs` の `sampleYRep`/`epredPIAtHeld`
  から呼ぶよう修正した (`cabal test` 全1356件PASS・既存挙動に影響なし)。

## ファイル

- `model.py` — PyMC 実装 (`pt.linalg.cholesky` で GP カーネル + 観測ノイズを
  手動構築し `pm.MvNormal(chol=...)` で尤度評価) + 合成ダッシュボード生成。
- `Model.hs` — hanalyze 実装 (`gpExpQuadCov` + 既存 `MvNormal` distribution・
  `observeMV`)。
- `run_pymc_matrix.py` — PyMC の CPU sampler×backend マトリクス
- `data/gp_pois_regr.json` — posteriordb 由来データ (`N=11`・`x`・`y`。`k`
  列は `gp_pois_regr` モデル専用で本モデルでは未使用)
- `figures/` — `hs_dashboard_full.png`/`py_dashboard_full.svg`

## 実行方法

```bash
bench/venv/bin/python bench/posteriordb/07-gp-regr/model.py
bench/venv/bin/python bench/posteriordb/07-gp-regr/run_pymc_matrix.py

cabal build --project-file=cabal.project.plot posteriordb-gp-regr
cabal run   --project-file=cabal.project.plot posteriordb-gp-regr
```

**★順次実行必須** (並行実行禁止・OOM対策は `.claude/skills/posteriordb-bench/SKILL.md`
step5 参照)。

## 経路確認

`synthVecIR` = `Nothing` (Phase 90 A1 で判定済み・密行列は vecIR に構造的
に載らない)。legacy walk+ad 経路 (`grad fFull`) で正しく動作することを
実測確認した (N=11・数値も PyMC/公式referenceと一致・後述)。

## 結果

### 精度 (4 chain・warmup1000+draws1000・3者比較)

posteriordb 公式 reference posterior から sd を逆算 (`sd = sqrt(E[X²] -
E[X]²)`) して比較。

| パラメータ | hanalyze | pymc (numba・最速CPU) | 公式 reference |
|---|---|---|---|
| rho   | 6.875 ± 1.183 | 6.91 ± 1.28 | 6.874 ± 1.266 |
| alpha | 2.417 ± 0.749 | 2.43 ± 0.79 | 2.442 ± 0.782 |
| sigma | 1.831 ± 0.474 | 1.83 ± 0.48 | 1.829 ± 0.505 |

3系統とも小数第1-2位まで良好に一致。R-hat ≈ 1.00 (両系統とも)。

### 速度 (4 chain・warmup1000+draws1000・CPU 1コア固定)

PyMC は sampler×backend 7 通りを計測 (blackjax は他モデルと同型の
`ValueError: cannot select an axis to squeeze` で失敗・原因未解明・参考記録):

| system | wall (ms) | ESS (alpha) | ESS/sec | 対 hanalyze |
|---|---:|---:|---:|---:|
| hanalyze (旧記録・stale) | ~~4970.1~~ | 1000.0 | 201.2 | — |
| pymc + Numba (旧記録・stale) | ~~3830.8~~ | 5547 | 1448.0 | — |
| nutpie + JAX | 6020.1 | 5666 | 941.2 | 1.21× |
| numpyro | 8639.7 | 3534 | 409.0 | 1.74× |
| pymc + JAX (own NUTS) | 13763.3 | 5547 | 403.0 | 2.77× |
| nutpie + Numba | 9206.7 | 4742 | 515.1 | 1.85× |
| pymc + CVM (真の C) | 18409.7 | 5547 | 301.3 | 3.70× |
| blackjax | (失敗・後述) | — | — | — |

**★Phase 95 で速度記録を全面是正 (旧 4970/3830ms は両方 stale)。** 上表は
別マシン/pymc 5.x 由来。同一マシン fresh 実測 (record 値を鵜呑みにせず両者再測):

| system | wall (ms) | ESS(alpha) | 対 hanalyze |
|---|---:|---:|---:|
| **hanalyze (legacy walk+ad・sampling-only)** | **3354** | — | 基準 |
| **PyMC 6.1.0 warm (sampling 相当)** | **~1350** | 5547 | **0.40× (hanalyze 約 2.5× 遅)** |
| PyMC 6.1.0 cold (compile 込) | ~11800 | 5547 | — |

- **記録の「1.30× 遅」は非対称比較の誤り**: hanalyze=sampling-only vs PyMC=compile込
  (run_pymc_matrix 第1 combo cold) を比べていた。両者 sampling-only で揃えると
  **hanalyze 3354 vs PyMC warm 1350 = 約 2.5× 遅**が真の姿。
- **経路 `synthVecIR = Nothing` (legacy walk+ad) は正着**: GP 密行列は vecIR に構造的に
  載らない (Phase 90 A1 判定どおり)。09-eight-schools と逆に hanalyze が劣後するのは、
  毎 leapfrog で N×N 共分散構築 + Cholesky (O(N³)) を reverse-AD テープ込みで再計算する
  ため。**Phase 95 profiling で内訳確定**: sampling の約 69% が GP 固有の密行列
  (choleskyL 32%/gpExpQuadCov 17%/forwardSub 6%/mvNormalLogDensity 4%)、汎用 AD tape は
  ~18%。真因は list 演算ではなく **tape ノード alloc** (脱リスト化 A2 は N=11 で利得ゼロを実測)。
- **N=11 では改善不能 = 既知の限界**: 脱リスト化 (A2) も解析随伴 (A3) も crossover ≈ N40-50
  未満で利得なし。A3 (Cholesky を LAPACK + 解析随伴で tape に載せない) は proto で正しさ
  (有限差分 1e-9) + 大 N 効果 (N=180 で 11×) を実証し、大 N GP モデル向け TODO 化した。
  詳細は `specification/phases/phase-95-gp-regr-speed.md`。

### 図 — 両側とも「フルダッシュボード」1 枚に統一

- **Haskell**: `hs_dashboard_full.png` (PPC パネルは Phase 90 A2 のバグ修正
  後に正常描画・修正前は `MvNormal: observation-only` で crash していた)。
- **Python**: `py_dashboard_full.svg`

### 既知の課題 (2026-07-11 訂正済み)

- **速度: hanalyze が PyMC 最速CPU に約 2.5× 遅 (Phase 95 で fresh 実測・上表)**。
  GP 密行列は vecIR 非対応 (legacy walk+ad が正着)。毎 leapfrog の Cholesky (O(N³)) を
  reverse-AD tape 込みで再計算する tape ノード alloc が主因。**N=11 では改善不能 = 既知の
  限界** (脱リスト化 A2・解析随伴 A3 とも crossover ≈ N40-50 未満で利得ゼロを実測)。
  A3 解析随伴 (LAPACK + detach) は proto で大 N 11× を実証済で大 N GP モデル向け TODO。
- blackjax エラーは他モデルと同型 (原因未解明・深掘りしない方針)。
- `Stat.PosteriorPredictive.genFromObserves` (別の PPC 実装・`dashboardFullOf`
  とは別経路) にも同型の `sampleDist` 直呼び出しが残っている
  (`PosteriorPredictive.hs:143`)。本モデルのベンチでは使われないため
  Phase 90 A2 では未修正 (対象外・気づいたら別途記録)。