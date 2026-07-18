# posteriordb 横断ベンチマーク (Phase 89)

自作 radon 1 本への依存を脱するため、posteriordb (stan-dev 公式・120 モデルの
参照ベンチマーク基盤) から多様なモデルを移植し比較する。詳細な方針・対象
モデルの選定根拠は
`specification/phases/phase-89-posteriordb-benchmark.md` 参照。

比較軸: **hanalyze vs PyMC (CPU 限定でモデルごとに最速の sampler×backend
組み合わせを選定)**。精度は PyMC・可能なら posteriordb 公式 reference
posterior とも突合する。回せない/遅い経路にフォールバックしたモデルは
数値を記録せず改善点のみ記録する ((a) DSL 機能ギャップ / (b) 遅い経路
フォールバックの2分類)。詳細 (精度表・課題・出典) は各モデルディレクトリの
`README.md` を参照。

**ディレクトリ番号について**: `NN-<slug>/` の連番は posteriordb 由来ではない
(posteriordb 自体に数値 ID は無く、`GLM_Poisson_Data-GLM_Poisson_model` の
ような文字列名のみのカタログ)。番号は `00-template/` をディレクトリコピー
した順。

**★番号付与ルール (2026-07-11 確定)**: `00-template/` をコピーして
`NN-<slug>/` を作った時点で番号が確定し、以後**そのディレクトリと番号は
不変**(欠番も含め詰め直さない)。**vecIR フォールバックが判明したモデル
(数値ベンチマークをしないモデル) も例外なく番号付きディレクトリを作る**
— チャット内の `synthVecIR` 直接呼び出しだけで済ませてディレクトリを
作らない運用は禁止。

## モデル一覧

| # | ディレクトリ | posteriordb 名 | ファミリ | 状態 | hanalyze 完了時間 | pymc 完了時間 (最速CPU) |
|---|---|---|---|---|---:|---:|
| 1 | [`01-glm-poisson/`](./01-glm-poisson/) | `GLM_Poisson_Data-GLM_Poisson_model` | 基本 GLM | ✅ 完了 | 494.6ms | 5785.3ms (nutpie+numba) |
| 2 | [`02-dogs/`](./02-dogs/) | `dogs-dogs` | 逐次・累積構造 | ✅ 完了 | 4682ms | 5148.7ms (numba) |
| 3 | [`03-garch11/`](./03-garch11/) | `garch-garch11` | 再帰的分散時系列 | ⏸ 保留 (PyMC側 OOM kill@3G・原因未調査) | — | — |
| 4 | [`04-low-dim-gauss-mix/`](./04-low-dim-gauss-mix/) | `low_dim_gauss_mix-low_dim_gauss_mix` | 混合モデル | ✅ 完了 (Phase 90 A3 で vecIR 化。旧「フォールバック確定」は stale・Phase 96 A2 runtime 実測でも vecIR) | 3816.2ms (速度比較は参考値 — PyMC 側未収束・詳細 README) | 5245.3ms (pymc+numba・ラベルスイッチングで未収束 r̂=1.53) |
| 5 | [`05-mh/`](./05-mh/) | `Mh_data-Mh_model` | capture-recapture | ✅ 完了 (Phase 96 改善対象) | 8950ms (2026-07-18 Phase 96 A5 `hbmWarmupInitMaxDepth=4`・warmup evals −25〜28%。A5 前 fresh 9470.1・起票時記録 16123.3 は stale) | 6447.2ms (nutpie+numba・compile 込み・2026-07-17 fresh。統一基準 sampling ≈3095ms → wall **2.9× 負け**・ESS/sec 78-80 vs 47.9 = **1.6-1.7× 負け**。残余 = per-eval 2.1× → `docs/dev-notes/ffi-simd-kernel-potential.md`) |
| 6 | [`06-irt-2pl/`](./06-irt-2pl/) | `irt_2pl-irt_2pl` | IRT (2PL) | ✅ 完了 (Phase 98 → **Phase 105 改善対象**) | **17623.6ms** (2026-07-18 Phase 105 A2 `hbmWarmupInitMaxDepth=4` + A3 op 特殊化。Phase 98 後 fresh 21268.0 → 17623.6 = −17%・ess_bulk(mu_b) 3436.5→4260.5) | 8828.2ms (nutpie+jax・compile 込み・2026-07-18 fresh。統一基準 sampling 8615.6ms → **2.05×**・ESS/s(mu_b) 241.7 vs 414.0 = 1.71× 負け。ess/draw はほぼ同等 = 残余は per-eval バルク FP → `docs/dev-notes/ffi-simd-kernel-potential.md`) |
| 7 | [`07-gp-regr/`](./07-gp-regr/) | `gp_pois_regr-gp_regr` | Gaussian Process | ✅ 完了 (Phase 95) | **803ms** (A7 後・旧 3354) | 1350ms (pymc+numba warm・6.1.0 再測) → **0.59× 勝ち** |
| 8 | (実装ファイルなし・番号のみ) | `hudson-lynx-hare-lotka-volterra` | ODE (生態) | ⏸ 保留 (synthVecIR指数的ハング) | — | — |
| 9 | [`09-eight-schools/`](./09-eight-schools/) | `eight_schools-eight_schools_noncentered` | 階層モデル (部分プーリング) | ✅ 完了 | 146.7ms | 2857ms (nutpie+numba) |
| 10 | [`10-rats/`](./10-rats/) | `rats_data-rats_model` | 縦断的成長曲線 | ✅ 完了 (Phase 93) | **2387ms** (A2 後・旧 24768 このマシン再測) | 2381ms (nutpie+jax・このマシン matrix 再測) → **1.00× 同着** |
| 11 | [`11-seeds/`](./11-seeds/) | `seeds_data-seeds_model` | 集計二項データ・ランダム効果 | ✅ 完了 (Phase 94) | 371ms (vecIR・group-merge) | 2462ms (pymc+numba・6.1.0 再測) |
| 12 | [`12-ark/`](./12-ark/) | `arK-arK` | 自己回帰時系列 AR(K) | ✅ 完了 | 1142.7ms | 5121.0ms (nutpie+numba) |
| 13 | [`13-traffic-accident-nyc/`](./13-traffic-accident-nyc/) | `traffic_accident_nyc-bym2_offset_only` | 空間統計 (BYM2/CAR) | ✅ 完了 (Phase 90 A10-A11) | 185095ms | 31015ms (pymc default・行列未実施) |
| 14 | [`14-hmm-example/`](./14-hmm-example/) | `hmm_example-hmm_example` | 隠れマルコフモデル | ✅ 完了 (Phase 92 改善対象) | 5325.3ms (2026-07-17 Phase 92 A2+B2+B3 後・改善前 28870.0 は `hmm_before_A1.log`。★同一指標 ess_bulk の ESS/sec では hanalyze 1.3-1.4× 勝ち = B4) | 3331.2ms (nutpie+jax・compile 込み。sampling wall 統一基準 ≈2965ms = 1.80×・2026-07-17 B4) |
| 15 | [`15-dugongs/`](./15-dugongs/) | `dugongs_data-dugongs_model` | 非線形成長曲線回帰 | ✅ 完了 | 324.6ms | 5920.0ms (pymc+numba) |
| 16 | [`16-lda/`](./16-lda/) | `three_men1-ldaK2` | LDAトピックモデル | ⏸ 保留 (約502次元・本番設定30分でtimeout) | — | — |
| 17 | [`17-nes/`](./17-nes/) | `nes1972-nes` | 線形回帰 (9変数・ARM本Ch.4) | ✅ 完了 | 12430.3ms | 14450.6ms (pymc+numba) |
| 18 | [`18-loss-curves/`](./18-loss-curves/) | `loss_curves-losscurve_sislob` | 保険数理損失三角形 | ✅ 完了 | 5129.1ms | 5192.7ms (pymc+numba) |
| 19 | [`19-surgical/`](./19-surgical/) | `surgical_data-surgical_model` | 階層二項ロジット (共変量なし・BUGS古典例) | ✅ 完了 | 2795.2ms | 3741.8ms (pymc+numba) |
| 20 | [`20-bones/`](./20-bones/) | `bones_data-bones_model` | 順序ロジット (graded response IRT) | ✅ 完了 (Phase 101 改善対象) | 2992.7ms (2026-07-17 Phase 101 A3 `GradedResponseIrt` 解析勾配・改善前 18332.4 = fresh/起票時記録 31272.9 は stale) | 10774.3ms (nutpie+jax・compile 込み・2026-07-17 fresh。**hanalyze が 3.6× 勝ち**・ESS/sec 1771 vs 571) |
| 21 | [`21-radon/`](./21-radon/) | `radon_mn-radon_hierarchical_intercept_noncentered` | 多水準回帰 (varying intercept, non-centered) | ✅ 完了 | 4905.5ms | 5395.5ms (nutpie+numba) |
| 22 | [`22-arma/`](./22-arma/) | `arma-arma11` | ARMA(1,1) 時系列 | ✅ 完了 (Phase 101 改善対象) | 376.4ms (2026-07-17 Phase 101 A2 `ArmaNormal` 閉形式随伴・改善前 8097.2 = fresh/起票時記録 13097.1 は stale) | 1539.1ms (nutpie+jax・compile 込み・2026-07-17 fresh。**hanalyze が 4.1× 勝ち**・ESS/sec 17233 vs 3301) |

各モデルディレクトリの構成:

```
NN-<slug>/
├── model.py            # PyMC 実装 + arviz 診断図生成
├── Model.hs             # hanalyze (ModelP) 実装。df |-> hbm 高レベル API・
│                         #   plateI_/plateForM_ で反復・hgg で診断図生成
├── run_pymc_matrix.py   # PyMC の CPU sampler×backend マトリクス (最速選定)
├── data/                 # posteriordb 由来データ (JSON そのまま)
├── figures/              # 生成図 (dag/forest/dashboard = Haskell側・
│                         #   trace/forest/ppc = Python側・両方 gitignore 対象外)
└── README.md            # モデル概要・prior・精度表・既知の課題・出典
```

ビルド: `cabal build --project-file=cabal.project.plot posteriordb-<slug>`
(hgg 連携につき plot-integration flag 必須)。

**★実行上の注意 (2026-07-11 確定・OOM事故を踏まえて)**: PyMC (`model.py`/
`run_pymc_matrix.py`) と hanalyze (`posteriordb-<slug>`) は**必ず片方の
完了を待ってから次を実行する**(並行実行は方法論違反かつ計測汚染の原因)。
実行コマンド・メモリ上限の付け方の詳細は `posteriordb-bench` skill
(`.claude/skills/posteriordb-bench/SKILL.md` の「モデル追加のワークフロー」
step 5) を参照。

---

（2026-07-11: 11-seeds/12-ark/14-hmm-example/15-dugongs/17-nes/
18-loss-curves 完了・13-traffic-accident-nyc/16-lda は保留。
15-dugongsで`Uniform(lo,hi)`境界外初期値によるHMC完全凍結の罠を新たに
発見・`Beta(1,1)`からのaffine再パラメタ化で解消 (詳細は
`15-dugongs/README.md`)。16-ldaは実装自体は正常 (中規模probeで正常収束
確認済) だが約502次元のsimplex latentが本番設定30分で完走せず保留
(13-traffic-accident-nycの「ハング」とは異なり「完走はするが遅すぎる」
パターン・詳細は`16-lda/README.md`)。17-nesは9変数線形回帰・公式
referenceとも3系統で良く一致・罠なし (hanalyzeがPyMC最速CPUの約1.16倍
高速)。18-loss-curvesは保険数理損失三角形・全prior が LogNormal/Normal
のため罠なし (hanalyzeとPyMC最速CPUがほぼ拮抗・約1.01倍高速)。
posteriordb-bench Phase 89 の当初選定8モデル完了 + Phase 90拡張分含め
全18モデルを消化 (完了13・保留2・フォールバック確定1)。

2026-07-12: 19番から新ファミリを多様性優先で追加選定 (19-surgical/
20-bones/21-radon/22-arma 予定)。19-surgicalは共変量なしの最も単純な
階層二項ロジット (BUGS古典例12病院)・罠なし・hanalyzeがPyMC最速CPUの
約1.34倍高速。sigma (InvGamma拡散事前分布) のESSがやや低めだが両系統
共通の既知傾向。20-bonesは順序ロジット(graded response IRT)の★新
ファミリ・罠なしだが**hanalyzeがPyMC最速CPUより約2.0倍遅い**
(06-irt-2plに次ぐ2例目・入れ子ループ+可変長カテゴリのlegacy walk+ad
コストが顕著)。PyMC側の合成ダッシュボードヘルパ(`_common.py`)に
「potential-onlyモデルでPPCパネルがKeyErrorで落ちる」罠を発見・全モデル
共有の修正を実施済み。21-radonは多水準回帰(varying intercept,
non-centered)の★新ファミリ・単一階層(J=85郡)+2固定傾き・罠なし
(hanalyzeがPyMC最速CPU=nutpie+numbaの約1.10倍高速)。22-armaはAR+MA複合
時系列(T=200・公式referenceあり3者比較)の★新ファミリ・3系統とも小数
第3-4位まで一致・罠なしだが**hanalyzeがPyMC最速CPUより約1.81倍遅い**
(逐次再帰・14-hmm-example/20-bonesに次ぐ3例目)。PyMC実装で
`pytensor.scan`のgradient計算バグ2件 (RandomVariableノード衝突・
non_sequences未指定) を発見・回避策を確立。19番からの新ファミリ追加
4件 (19-surgical/20-bones/21-radon/22-arma) 全て完了)

2026-07-17 (Phase 92 B4・14-hmm-example ess/draw 調査): 「ess/draw 0.192 vs
0.291 = 1.5× 劣後」は**指標アーチファクトと確定** (hanalyze 旧 ess =
`Common.summarize` の chain0 のみ Geyer IMSE・n=1000 頭打ち値を 4000 で割って
いた)。**同一指標** (arviz rank-normalized ess_bulk・4chain・seed3 種) では
hanalyze 0.77-0.86 vs nutpie+jax 0.31-0.35 = **hanalyze が 2.4-2.5× 優位**
(機構 = 平均 depth 3.1 vs 2.4 の長軌道で自己相関減・divergence 両側 0)。
**ESS/sec でも hanalyze ~590-650 vs ~420-470 = 1.3-1.4× 勝ち**。wall 計測の
統一基準も確定: **sampling wall (tune/warmup+draws・compile 除外)** で
hanalyze 5325 vs nutpie+jax ≈2965ms = 1.80× (表の 3331.2ms は compile 込み)。
詳細 = phase-92 md B4 + `14-hmm-example/hmm_ess_diag_20260717.log`。
**B4-② で根本対処済**: `Stat/MCMC.hs` に arviz 互換の `essBulk` を新設し
`Common.summarize` の ESS 列を切替 (mean/sd/HDI も全 chain プール化・ヘッダ
`ess_bulk`)。以後の全 posteriordb bench の ESS 表示は PyMC 側 `az.summary`
と直接比較可能 (unit golden 4 件 + hmm 実 draw 全 7 パラメータ一致で検証済)。
★過去の README/表に記録された旧 ess 値 (chain0 Geyer IMSE) と新表示は
連続しない点に注意。

2026-07-17 (Phase 92 A2+B2+B3・14-hmm-example 改善): `HmmForwardNormal`
構造化 primitive (forward-backward 閉形式随伴・A2) + 閉包の脱リスト化/AD 1
パス化 (B2) + 定数 hyperparam lgamma の Double 畳み込み (B3・`logDensityRD`) で
28870.0 → 8233 → 5927 → **5325.3ms** (**累積 5.42×**)。対 PyMC 最速
(nutpie+jax 3331.2ms) = **1.60×** (A1 時点 8.67×)。B3 は chain bit 一致
(事後・ess・rhat 完全一致) で値保存を確認。残ギャップの主因は per-eval では
なく ess/draw 効率 (0.192 vs 0.291) と見立てたが、**B4 で指標アーチファクトと
判明** (上記) — 詳細 = phase-92 md の B3/B4。

2026-07-16 (Phase 92 A1・14-hmm-example 事実是正): 上表 #14 の旧記録
(hanalyze 71704.3ms・PyMC 17508.3ms numpyro) は**両方 stale**だったため是正した。
同一マシン fresh 再測 (4chain・warmup1000+draws1000・seed1・CPU1コア) で
hanalyze = **28870.0ms** (経路 = legacy walk+ad・2026-07-11 記録から 2.5× 改善済で
記録が古い)、PyMC matrix 7 combos の最速 = **nutpie+jax 3331.2ms** (numpyro 5342.5ms・
pymc-own-NUTS 系 3 種は rhat(mu_1)=1.52 で非収束)。真ギャップ = 28870/3331 =
**約 8.67× hanalyze 遅い** (旧記録の 4.1× より悪い)。ess/draw も hanalyze 0.160 vs
nutpie+jax 0.291・numpyro 0.470 で 1.8〜2.9× 劣後。詳細 =
`specification/phases/phase-92-numeric-ad-kernel-fusion.md` A1 +
`14-hmm-example/{hmm_before_A1.log,pymc_matrix_A1.log}`。

2026-07-13 (Phase 94・11-seeds 事実是正): 上表 #11 の旧記録
(hanalyze 9119.5ms・PyMC 6276.0ms) は**両方 stale**だったため是正した。
(1) hanalyze は実は **vecIR 高速経路** (`synthVecIR=Just`。旧 README の
「legacy walk+ad へフォールバック」は診断バグ由来の誤り) で、同一マシン
fresh 実測は約 5630ms だった。さらに Binomial の n をプレート毎に持つ
group-merge 改修 (Phase 94 A3・`IR.hs` の group key から n を除外) で
**17-group→1-group** に集約し、**5678→371ms = 約 15× 高速化**。
(2) PyMC 側も同一マシンで pymc **6.1.0** 再測 (record の 6276ms は恐らく
pymc 5.x・別マシン)。中心化 wall 2462ms・非中心化 wall 2760ms。
→ group-merge 後の wall 倍率は 371/2462 = **約 6.6× hanalyze 高速**
(hanalyze=draws-only / PyMC=compile込 record 手法の非対称。純 draws でも
約 3× 見込)。(3) tau の収束難 (旧 ess 10.2・rhat 1.10) は seeds の
**非中心化** (b=z·σ・Phase 94 A4) で解消し **ess(tau) 659・rhat 1.00**
(PyMC 非中心化 fair 比較で **ESS/秒 約 3.1× hanalyze 勝ち**)。詳細な計測
経緯は `specification/phases/phase-94-seeds-onboarding.md`・
`11-seeds/README.md` 参照)

2026-07-13 (Phase 95・07-gp-regr 事実是正 + 速度下限確定): 上表 #7 の旧記録
(hanalyze 4970ms・PyMC 3831ms) も**両方 stale**だったため是正。同一マシン
fresh 実測で両者 sampling-only を揃えると **hanalyze 3354ms vs PyMC warm ~1350ms
= hanalyze 約 2.5× 遅** (記録の「1.30× 遅」は hanalyze=sampling-only vs
PyMC=compile込 の非対称比較 + 両値 stale が原因)。PyMC 6.1.0 再測: cold(compile込)
~11800ms・warm(sampling 相当)~1350ms。**経路 `synthVecIR=Nothing` (legacy walk+ad) は
正着** (GP 密行列は vecIR に構造的に載らない・Phase 90 判定どおり。seeds #11 の「legacy
は診断バグ由来の誤り」とは異なり、gp-regr の legacy は正しい)。profiling で 2.5× 差の
内訳を確定 = sampling の
約 69% が GP 固有の密行列 (choleskyL/gpExpQuadCov を reverse-AD tape で走らせる
コスト・list 演算ではなく tape ノード alloc が本体)。**N=11 では脱リスト化 (A2) も
解析随伴 (A3) も利得ゼロ (crossover ≈ N40-50)** = gp-regr の速度は現状がほぼ下限
(既知の限界)。A3 解析随伴は proto で正しさ + 大 N 11× を実証し、大 N 向け TODO 化。
詳細は `specification/phases/phase-95-gp-regr-speed.md`)

2026-07-13 続報 (Phase 95 A6/A7・上の「下限」を更新): A3 の「N=11 では下限」判断は
A6/A7 で覆った。**A6 = GP 専用 `MvNormalGpRBF` distribution + 閉形式随伴** (Σ を AD tape に
展開せず ∂Σ/∂θ を閉形式で・LAPACK 逆行列) + **A7 = 4 opts (Frobenius 随伴で N×N 一時行列
全廃 / 対角直接加算 / kMat の C ベクトル化 exp / Cholesky+mbChol)** で **N=11 を 3354→803ms
= 対 PyMC 0.59× 逆転**。合成 N-scaling: N=25 1912ms(1.08×)・N=50 6019ms(1.94×・旧 193s から
31×)・N=100 30153ms(3.10×)・N=200 182393ms(4.26×)。**比が N で増大するのは次数差ではなく
定数倍**: hanalyze/PyMC はともに **O(N³)** (区間指数差 Δ が 0.84→0.46 と縮小・hanalyze 指数は
3 に下から接近)、hanalyze が O(N³) 支配域に早く入るため遷移域で比が開く (漸近比 ~5-6× で
頭打ち見込み)。残る定数倍 (kernel+cov+chol+随伴の 1 パス C 化) は **Phase 97 (FFI 融合)** へ分離。
