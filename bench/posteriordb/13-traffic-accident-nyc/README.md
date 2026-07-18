# traffic-accident-nyc (posteriordb: `traffic_accident_nyc-bym2_offset_only`) — ✅ 完了 (Phase 90 A10 で保留解除)

## 結果サマリ (2026-07-12・Phase 90 A11-4①+② 適用後・単独逐次実行)

4chain×(warmup/tune 1000 + draws 1000)・seed 1・cores=1:

| system | sampling wall (ms) | beta0 | sigma | rho |
|---|---:|---|---|---|
| **hanalyze (vecIR + VGPot + A11-4①+②)** | **185,095** | -6.6128±0.0245 | 1.1878±0.0377 | 0.5469±0.0426 |
| hanalyze (A11-4① のみ) | 210,479 | -6.6134±0.0233 | 1.1839±0.0341 | 0.5441±0.0385 |
| hanalyze (A11-4 前・A10-4 baseline) | 279,592 | -6.6134±0.0233 | 1.184±0.034 | 0.544±0.039 |
| pymc デフォルト (nuts_sampler="pymc") | **31,015** | -6.6124±0.0227 | 1.186±0.035 | 0.545±0.041 |

- 事後は全系統で**小数 2 位まで一致** (r_hat: hanalyze ≤1.017 / pymc ≤1.01)。
- **A11-4① (arena/adj 再利用 + u-turn 融合) で 279,592 → 210,479 ms = 24.7%
  短縮**。 **A11-4② (log∘exp 畳み込み + ICAR gather 融合) で追加 12.1% →
  185,095 ms**。 **累積 33.8% 短縮 (1.51× 高速化)**。 phase md §A11-4②/A11-5 参照。
- ②の F3b (log∘exp→id 代数簡約) で draws は ulp 変化するが**事後分布は不変**
  (beta0/sigma/rho すべて PyMC と MC 誤差内一致)。
- **速度は hanalyze が約 6.0 倍遅い** (A11-4 前は 9.0 倍)。 残る差の源泉は
  葉勾配 closure の解釈 dispatch (A11-4 ③ で継続追跡)。
- PyMC 側は **user 指示によりデフォルト設定のみ** (`run_pymc_matrix.py` の
  バックエンド行列は未実施)。venv = `~/.virtualenvs/pymc312`
  (python3.12 + pymc 6.1.0・2026-07-11 新設)。
- `figures/py_dashboard_full.svg` は未生成 (arviz 1.2 で `_common` の
  dashboard が `lam value too large` — best-effort スキップ・計測は成立)。

---

以下は経緯の記録 (Phase 89 当時の保留理由と Phase 90 での解決過程)。

BYM2 空間疫学モデル (Morris et al. 2019) — NYC 交通事故データ
(N=1921地域・N_edges=5461隣接ペア)。ICAR (intrinsic conditional
autoregressive) 事前分布を隣接ペアごとの差分ペナルティで表現する Stan の
標準的な BYM2 実装。出典: `stan-dev/posteriordb`
(`posterior_database/models/stan/bym2_offset_only.stan`・
`posterior_database/data/data/traffic_accident_nyc.json.zip`)。

- `reference_posterior_name: null` (posteriordb に公式 reference 無し・
  hanalyze vs PyMC の2者比較のみ)。
- Prior: `beta0 ~ Normal(0,1)`・`sigma ~ HalfNormal(1)`・
  `rho ~ Beta(0.5,0.5)`・`theta_i ~ Normal(0,1)` (i=1..1921)。`phi` は
  Stan 原典で固有の周辺 prior を持たない (ICAR ペナルティ+ソフトゼロ和
  制約のみが情報源・実質 improper flat)。hanalyze には improper flat
  distribution が無いため `phi_i ~ Normal(0,1000)` という diffuse な
  近似を与えた (他モデルの Uniform 箱置換と同じ流儀)。
- 尤度: `y_i ~ Poisson(exp(log_E_i + beta0 + convolved_re_i*sigma))`
  (`convolved_re_i = sqrt(1-rho)*theta_i + sqrt(rho/scaling_factor)*phi_i`)。
- ICAR: `potential "icar" (-0.5 * sum[(phi[a]-phi[b])^2 | (a,b) in edges])`
  (Stan の `target += -0.5*dot_self(phi[node1]-phi[node2])` に対応)。
- ソフトゼロ和制約: `potential "sum_zero" (logDensity (Normal 0 (0.001*N)) (sum phis))`
  (Stan の `sum(phi) ~ normal(0, 0.001*N)` に対応)。

## ⏸ 保留の理由: モデル構築/`synthVecIR` 解析がタイムアウトするまでハング

**実装は完了した** (`Model.hs` に `bym2Model` として存在・`potential` /
`logDensity` API を使って ICAR ペナルティ+ソフトゼロ和制約を忠実に移植
できることを確認済み)。しかし実行してみたところ、**極端に縮小した
プローブ設定 (1 chain・warmup=3・draws=3、計6反復) ですら5分間
(300秒タイムアウト) 経過しても最初の1行 (`synthVecIR = ...`) すら出力
されずハングした** (`cabal build` は正常に完了・型エラー等は無し)。

出力が全く無いまま5分ハングするという症状は、**Phase 90 A4
(08-hudson-lynx-hare・ODE モデル) で確認済みの `synthVecIRWalk` 指数的
ハング (RK4深さ1につき約60-65倍の計算時間増大・共有部分式の重複走査が
原因と推定) と同型のパターン**と見られる。本モデルは N=1921 latent
(theta+phi=3842) + `potential` 項内の 5461-way sum という、通常の
sample/observe だけのモデルとは桁違いに巨大な式木を持つため、同種の
式木解析の指数的コストに再度当たった可能性が高い (未確認・深掘りは
本 Phase の主眼外)。

**本 Phase (89) では計算コストが実用外と判断し保留**。根治には
Phase 90 側の `synthVecIRWalk`/式木解析関数のメモ化改修 (共有部分式の
hash-consing 等) が必要と見られ、Phase 90 の完了を待って再挑戦する方針
(03-garch11/08-hudson-lynx-hare と同じ「保留」区分)。

### 2026-07-11 更新: Phase 90 A9 再プローブ結果

Phase 90 A8 (synthVecIR 合成の共有保存 DAG 化) 適用後に再プローブ:

- **縮小プローブ (1chain・warmup=3・draws=3) は完走した** (exit 0)。
  判定は起動数秒で出力 — 前回の「判定自体が5分無出力ハング」は
  **A8 で解消** (なお前回の「出力すら出ない」には GHC のパイプ block
  バッファリングも混入していた可能性あり。pty 経由だと部分出力が
  観測できる)。
- サンプリングは **368秒/6反復 (≈61 s/反復)**。
  N 段階縮小プローブ (`posteriordb-bym2 <N>`・Model.hs の probe モード)
  の実測で N^2.3 程度の多項式 — 指数ハングではなく「構造的に遅い」。
  フルベンチ (4chain×2000反復) は ≈5.6 日と非実用のため引き続き保留。
- 実測表は `specification/phases/phase-90-vecir-gap-extensions.md` §A9。

### 2026-07-11 同日訂正 + 原因確定: Phase 90 A10-1

- A9 時点で「`synthVecIR = Nothing` (raw potential ゆえ対象外・想定
  どおり)」と記録したが、これは **Model.hs の診断バグ** — `main` の判定が
  `dataNamedX "log_E" []` (df 束縛前の空データモデル) に対して行われて
  おり、観測行 0 → Nothing を印字していただけ。**実データ束縛済み
  モデルは N=25 でもフル N=1921 でも `synthVecIR = Just`** (= Poisson
  尤度と theta/phi 族は既に vecIR で走っていた)。Model.hs は data list
  引数束縛に修正済み (probe モードの表示も `Just` になる)。
- 遅さの真因 = **`potential` 2 項 (icar / sum_zero) の残差 ad**。フル N の
  勾配 1 回: 実モデル ~0.13s / potential 無し対照 ~0.009s (**93% が
  potential 残差**)。sum_zero 1 項だけでも ~0.082s — 残差が 1 項でも
  残ると全 walk の boxed ad 固定費を払う構造
  (`residualFreeOfDensity` は `Potential → False` 固定)。
- 切り分け probe = `experiments/phase90-13traffic-onboard/ProbeA10.hs`。
  高速化 (potential の vecIR 吸収) は Phase 90 A10 で対応中。

## ファイル

- `model.py` — PyMC 実装 (Phase 90 A10-4 で作成・デフォルト sampler のみ)。
- `Model.hs` — hanalyze 実装 (probe モード `posteriordb-bym2 <N> ` 付き)。
- `run_pymc_matrix.py` — 未作成 (user 指示によりバックエンド行列は回さない)。
- `data/traffic_accident_nyc.json` — posteriordb 由来データ (`N=1921`・
  `N_edges=5461`・`node1`/`node2`・`y`・`E`・`scaling_factor`)。
- `figures/` — 未生成。

## 経路確認

`synthVecIR` の呼び出し自体が完了しない (上記「保留の理由」参照)。
`potential` (raw log-density 項) を使うモデルは構造的に vecIR 対象外の
はずだが、判定そのものに時間がかかりすぎる可能性がある (未確認)。

## 既知の課題 (Phase90へ引き継ぎ)

- **`synthVecIRWalk` (または関連する式木解析) が大規模 `potential` 項
  (数千〜数万要素の sum) で指数的/多項式的に遅くなる可能性**。
  08-hudson-lynx-hare (ODE) と合わせて2例目の類似症状。Phase 90 の
  スコープ (`synthVecIRWalk` 根本原因調査) で本モデルも再現ケースとして
  扱えると良い。
- 再挑戦時のチェックリスト: (1) `cabal repl` で `synthVecIR` 単体呼び出し
  のみを分離してタイミング計測 (モデル全体のサンプリングを待たずに
  切り分ける)。(2) N/N_edges を段階的に縮小したトイモデル (例: N=50・
  edges=100程度) で発散/多項式/指数関数のどれに近いか実測する
  (08-hudson-lynx-hare の「深さ1につき60-65倍」実測と同じ手法)。
