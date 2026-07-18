# PyMC / NumPyro との 3 系ベンチマーク (Phase 84)

hanalyze の HBM (NUTS) を **素の PyMC (PyTensor default)** と **PyMC の
NumPyro backend** (`nuts_sampler="numpyro"`・JAX/XLA・CPU) と、 同一モデル・同一
データ・同一 NUTS 設定 (chains=1・warmup=500・target_accept=0.8・max_treedepth=10)・
**単スレッド CPU 固定** (`taskset -c 0`・`JAX_PLATFORMS=cpu`・OMP/OPENBLAS=1) で比較する。

- ドライバ: `bench/run_hbm_3way.sh <suite>`・集約: `bench/python/agg_hbm_3way.py <stem>`
- 精度基準 = PyMC。 乱数ゆえ「完全一致」でなく **MC 誤差内一致**を合格とする。

---

## 題材 1: Radon (flagship・相関 varying intercept+slope)

Gelman radon (MN・919 obs・85 郡)。 μ = β0 + β1·floor + β2·uranium + b0_c + b1_c·floor、
(b0,b1) 群効果は相関 (LKJ Cholesky・非中心化)。 主役 = **floor 係数**。 grid=[50,100,200,400]・
reps=2 (相関 RE + deep tree で重いため M 系より短縮)。 データ = `bench/data/radon.csv`。

### 精度 (floor 係数の事後平均・基準 = PyMC) — 合格

| iter | hanalyze | pymc | numpyro | Δ(hanalyze) | Δ(numpyro) |
|---|---|---|---|---|---|
| 50  | -0.601 | -0.610 | -0.610 | 0.009 | 0.000 |
| 100 | -0.609 | -0.620 | -0.610 | 0.012 | 0.010 |
| 200 | -0.616 | -0.620 | -0.610 | 0.004 | 0.010 |
| 400 | -0.614 | -0.620 | -0.610 | 0.007 | 0.010 |

3 系とも floor ≈ -0.61 (radon 文献値) に一致。 hanalyze の PyMC からの差は全 iter で
**|Δ| < 0.012** = MC 誤差内。 **精度は合格**。

### 速度 (wall-time ms / ESS-per-sec)

| iter | hanalyze_ms | pymc_ms | numpyro_ms | ess/s hanalyze | ess/s pymc | ess/s numpyro |
|---|---|---|---|---|---|---|
| 50  | 9602 | 3726 | 8993* | 5.2  | 8.6  | 2.5* |
| 100 | 7403 | 2499 | 4761  | 13.5 | 29.6 | 18.5 |
| 200 | 7708 | 2603 | 4935  | 26.0 | 84.1 | 30.2 |
| 400 | 8240 | 2844 | 4887  | 48.5 | 129  | 55.5 |

`*` numpyro iter=50 は XLA JIT コンパイル固定費を含む初回ゆえ割高 (以降は償却)。

- radon では hanalyze は **PyMC-C の ~0.35 倍速 (= 約 3 倍遅い)**、 numpyro とほぼ同等〜やや下。
- ただし hanalyze の **ESS/draw ≈ 1.0** (ess = draw 数) で PyMC (0.6〜0.9) より**混合は良い**。
  = 「1 draw の質は高いが、 深い木 (tree depth ~10 飽和) で 1 draw が高コスト」。
- tree depth 飽和は 3 系共通 (相関 radon の幾何が難しい・numpyro も 9.7)。
  ※85.3 追記: 現 CSV (`bench/results/python/hbm_scaling_radon*.csv`) の tree_depth は
  pymc/numpyro とも **4.0** で本記述 (~10/9.7) と矛盾 — 旧 run 由来とみられる。
  hanalyze 側の tree depth は HS bench が未出力 = **未計測**。

### 【Phase 85.3 追記 (2026-07-10)】 AD 融合後の hanalyze 再計測

Phase 85.3 (恒等演算畳み込み + superinstruction 融合・`8941af5`/`ffa2767`) で
radon の勾配 per-eval を **103→43.5µs (2.4×)** に短縮後、 同一ハーネス
(`bench-hbm-scaling radon`・taskset -c 0) で hanalyze 列のみ再計測:

| iter | hanalyze_ms (84→85.3) | ess/s hanalyze (84→85.3) | ess/s pymc | ess/s numpyro |
|---|---|---|---|---|
| 50  | 9602 → **4949** | 5.2 → **10.1** | 8.6  | 2.5* |
| 100 | 7403 → **4965** | 13.5 → **17.6** | 29.6 | 18.5 |
| 200 | 7708 → **4112** | 26.0 → **42.6** | 84.1 | 30.2 |
| 400 | 8240 → **4555** | 48.5 → **76.5** | 129  | 55.5 |

- **対 PyMC-C: 0.35× → 0.59×** (iter400 ess/s 比)。 **対 numpyro: 逆転** (76.5 vs 55.5)。
- **【85.3-iv 追記】** gather 内蔵/3 項融合 (`6f03e0d`) 後: per-eval **36µs (基準 2.9×)**・
  iter400 wall **3.85s**・ess/s **103.9 = PyMC-C 比 0.81×**。 hanalyze の tree_depth を
  計測できるようにした結果 **4.0 = pymc/numpyro と同一** (深い tree 仮説は否定・
  `8cdb83b`)。 事後 floor −0.62 (MC 誤差内)。
- **【85.6 追記・対等条件で確定】** warmup 深掘りの原因 = M=I 期間 (init buffer +
  第 1 窓) に相関幾何で受容率 0.8 を満たす ε が存在せず DA が鋸歯振動し depth
  7-10 を掘ること (warmup leapfrog の 68%・`bench-warmup-prof` 実測)。
  - **標準機構での対策 = `nutsInitEpsSearch` (Stan Algorithm 4・既定 ON)**:
    warmup leapfrog **-9%** のみ (深掘りは幾何本質で ε₀ 較正では解消しない)。
    **対等条件 (標準機構のみ) の radon iter400 = wall 3.79s・ess/s 105.7 =
    PyMC-C 比 0.82×**。 これが公正なヘッドライン。
  - **`nutsWarmupInitMaxDepth` (M=I 期間の depth cap) は opt-in (既定 OFF)**:
    参照実装に無いヒューリスティックゆえベンチ対象外。 参考値 = `Just 6` で
    warmup leapfrog -57%・ess/s 163.7 (PyMC-C 比 1.27×)・radon/funnel/M系の
    品質は実測で維持。 実務ユーザ向けの高速化ノブとして提供。
- **【Phase 86 追記 (2026-07-10)】 warmup の seed 依存爆発を解消 (標準機構)**:
  seed を振ると radon warmup leapfrog は旧実装で 31.5k-134k (M 更新直後の window で
  DA restart anchor = 鋸歯瞬間値が新 M と桁で乖離 → 1 window 丸ごと深掘り)。
  **PyMC 側にも同じ M=I 深掘りがある** (tune500 実測: iter[0,102) が warmup
  leapfrog の 70%・depth 7-8・seed 分布 24.2k-53.2k) ため M=I 期間自体は参照実装
  同等 = 削減余地なし、 差は window 末の anchor だった。 Stan
  (`adapt_diag_e_nuts`) と同じく **M 更新のたび `init_stepsize` (受容比 0.8・
  現 ε 起点) で ε を再較正して DA restart** する形へ `nutsInitEpsSearch` を拡張
  (既定 ON・標準機構のみ)。 radon warmup = **32.3k-37.1k (6 seeds・爆発全消滅)**。
  同日交互 A/B: radon iter400 ess/s 74.9/78.0 → 74.5/104.1 (中立〜改善)・
  funnel τ ESS 10 seeds mean 302→293 (min 74→241 = 頑健化)・M1-M8 事後一致
  (iter1600 |Δ|≤0.012)・M1-M4 wall 1.1-1.7× 改善。 ※絶対値が 85.6c 記録
  (105.7) と違うのは WSL2 日次ドリフト (同日 A/B のみ有効)。
  **Phase 86 コードでの同日 3 系実走 (radon iter400・CSV 更新済)**: ess/s
  hanalyze 114.1 vs PyMC-C 143.5 vs numpyro 59.6 = **PyMC-C 比 0.80×
  (85.6c の 0.82× と同水準)・numpyro 比 1.9×**。 iter50 は wall で PyMC-C を
  1.21× 逆転 (固定費差)。 精度 floor |Δ|≤0.0124 (numpyro 自身の対 PyMC 差
  0.01 と同帯 = MC 誤差内)。 ESS/draw = 1.00 vs 0.92 vs 0.68。 Phase 86 の
  主効果は平均でなく **tail seed の爆発解消** (ヘッドライン seed は元々
  爆発していないため 0.82×→0.80× は日内誤差)。
- **【Phase 87 追記 (2026-07-10)】 radon 実運用 iter で PyMC-C と同着へ**:
  87.1 = 最終 window 末の DA restart 廃止 (PyMC 流連続 DA・ε̄ が 0.22-0.32 の
  PyMC 帯に着地・sampling depth 5→4)。87.2 = alpha probe 廃止 (Stan accept_stat
  = tree 内平均 ᾱ) + **value_and_grad 融合 & 端点勾配キャッシュ** (葉あたり
  grad2+logPi1 → 融合 eval1・チェーン bit 同一)。
  **radon iter1600 同日 A/B: ess/s 403.6/404.1 vs PyMC-C 407.4/412.3 =
  0.98-0.99× (同着・Phase 86 時点の 0.49-0.68× から)**。radon iter400 wall
  1.85× 短縮。M1-M8 はさらに 1.4-2.3× (M1@1600 = 57k ess/s)。全て標準機構。
- 精度: floor = -0.607/-0.621/-0.626/-0.618 — 従来同様 PyMC と MC 誤差内一致。
- **pymc-C と numpyro の速度差の内訳** (iter100→400 の増分から線形分解・iter50 は
  初回 JIT 費で除外): per-iter 単価は numpyro **0.42 ms/iter** < pymc **1.15 ms/iter**
  (JIT 済 XLA が per-draw では ~2.7× 安い)。 しかし固定費 (JIT compile + warmup) が
  numpyro ~4.8s vs pymc ~2.4s と大きく、 **交点 ≈ 3300 iter** — 本 grid (≤400) では
  固定費が支配して pymc-C が速く見える。 加えて numpyro は同 iter の ESS が低い
  (400 で 271 vs 367 = ESS/draw 0.68 vs 0.92・適応/実装差) ため ess/s でさらに不利。
  tree_depth は両者 4.0 で同一 = **tree 深さ起因ではない**。 精度は 3 系一致ゆえ
  **速度と精度のトレードオフでもない** (短 run での固定費未償却 + 混合効率差)。

### 経路診断 (推測するな計測せよ)

「HS が遅い = コンパイル経路を外れている?」 を計測で否定:

| n(obs) | `synthVecIR` | 経路 | per-eval |
|---|---|---|---|
| 96  | JUST | (a) vecIR | ~100µs |
| 200 | JUST | (a) vecIR | ~70µs |
| **935 (radon)** | **JUST** | **(a) vecIR** | **~160µs** |

→ radon は **source-to-source コンパイル経路 (a) に乗っている** (fallback ではない)。
compile 119ms は 1 回のみ・per-eval は線形。 遅さは経路でなく **per-eval が XLA より
~5-10× 遅い (Phase 80: XLA 融合なし・CPU arena) × 深い木**の積。

---

## 題材 2: Eight Schools (精度エッジ・funnel)

古典階層正規 (8 校・観測 SE 既知)。 非中心化 θ_j = μ + τ·θ̃_j。 μ~N(0,5)・
τ~HalfCauchy(5)。 主役 = **τ** (funnel の首・重い裾)。 grid=[50…1600]。

### 速度 (ess/sec) — hanalyze 圧勝 (小モデル)

| iter | hanalyze_ms | pymc_ms | numpyro_ms | speedup(対PyMC) | ess/s hanalyze | ess/s pymc | ess/s numpyro |
|---|---|---|---|---|---|---|---|
| 50   | 34.5  | 834  | 2476 | **24.2×** | 1028 | 52  | 8.9 |
| 200  | 52.0  | 841  | 2382 | **16.2×** | 3149 | 45  | 76  |
| 800  | 81.9  | 995  | 2628 | **12.2×** | 8778 | 311 | 163 |
| 1600 | 104.6 | 1351 | 2447 | **12.9×** | 8130 | 473 | 383 |

小モデルゆえ hanalyze の compile 費ゼロが効き **PyMC-C の 12〜24×**。 numpyro は JIT
固定費 (~2.4s) が小モデルを支配し最も遅い。

### 精度 (τ 事後平均・基準 = PyMC)

| iter | hanalyze | pymc | numpyro | Δ(hanalyze) |
|---|---|---|---|---|
| 50   | 4.48 | 3.20 | 3.30 | 1.28 |
| 100  | 4.32 | 3.40 | 3.60 | 0.92 |
| 200  | 4.03 | 4.00 | 3.50 | **0.03** |
| 400  | 3.72 | 3.80 | 3.60 | **0.08** |
| 800  | 3.86 | 3.60 | 3.70 | 0.26 |
| 1600 | 3.86 | 3.70 | 3.70 | 0.16 |

τ は funnel の重い裾で **低 draw では全系ノイジー** (iter=50 は hanalyze/pymc とも
不安定)。 **iter≥200 で hanalyze ≈ PyMC (Δ 0.03〜0.26)** に収束 (τ≈3.7・文献値圏)。
μ (安定・非表示) はより早く一致。 精度は draw を積めば合格。

---

## 題材 3: M1-M9 スケーリング系 (合成・`hbm_scaling`)

pooled / 階層 ranint / ranint+slope / 多変量 X / 非線形 / 階層×非線形 / Poisson /
logistic (96〜200 obs)。 3 系すべて base grid [50…1600] で実走。

### 速度 (代表・ess/sec) — hanalyze 全 M 系で圧勝

| model (iter=400) | hanalyze_ms | pymc_ms | numpyro_ms | 対PyMC | ess/s hana | ess/s pymc | ess/s npy |
|---|---|---|---|---|---|---|---|
| M1 pooled       | 18.5  | 1211 | 3328 | **65.6×** | 21666 | 279 | 79  |
| M2 ranint       | 116.0 | 2370 | 3001 | **20.4×** | 2895  | 121 | 63  |
| M3 ranint+slope | 299.1 | 3299 | 3575 | **11.0×** | 815   | 30  | 19  |
| M4 multi-X      | 133.2 | 2136 | 2480 | **16.0×** | 3004  | 306 | 419 |
| M5 nonlinear    | 188.9 | 1894 | 3101 | **10.0×** | 1920  | 85  | 56  |

- **hanalyze は全 M 系で PyMC-C の 5.5〜104× 速い** (小モデルで compile 費ゼロが効く)。
- numpyro は JIT 固定費 (~2.4s) が小モデルを支配し **PyMC-C より遅い** (speedup_npy<1)。
  M4 (多変量・重い) でのみ numpyro が pymc に追いつく (ess/s 419 vs 306)。

### 精度 — hanalyze ≈ PyMC (Δ < 0.06)

M1-M6 の主役パラメタ事後平均は hanalyze と PyMC が全 iter で **|Δ| < 0.06** (大半 <0.02)。
draw を積むほど縮小 (M1 iter1600 で Δ=0.0017)。 ranslope/nonlinear の低 draw のみ
funnel/非線形で一時的に Δ〜0.05 だが iter≥200 で収束。 numpyro とも同水準。
(※ M7/M8 の PyMC-C base 列は既存データに欠落・numpyro/hanalyze は取得済。)

**結論**: hanalyze は **小・単純モデルで圧勝** (5〜100×)、 **大・相関階層 (radon) でのみ
XLA/C コンパイルに劣る** (~3×) という非対称が本ベンチの主要な発見。

---

## まとめ

| 題材 | 規模 | 速度 (対 PyMC-C) | 精度 (対 PyMC) |
|---|---|---|---|
| M1 pooled | 96 obs | **~102× 速い** | 一致 |
| Eight Schools | 8 obs | **12〜24× 速い** | iter≥200 で一致 (τ funnel) |
| Radon (相関) | 919 obs | ~0.35× → 0.82× (85 系) → **iter1600 で 0.98-0.99× = 同着 (Phase 87・標準機構のみ)** ※86 で warmup seed 爆発も解消 | 一致 (floor \|Δ\|<0.012) |

1. **精度**: hanalyze の事後は 3 題材とも PyMC と MC 誤差内で一致 (draw を積めば)。 ✅
2. **速度は規模・構造依存の非対称**: 小・単純モデルは hanalyze 圧勝 (compile/JIT 費ゼロ)、
   大・相関階層は PyMC-C 有利 (~3×・XLA/C コンパイルの差)。 numpyro は JIT 固定費ゆえ
   小モデルで最遅・大モデルで hanalyze と同等圏。
3. **混合品質**: hanalyze の ESS/draw が高い (radon ~1.0)。 サンプラ質は良い。
4. **改善余地** (次テーマ候補): 大規模での per-eval (XLA 融合なし・CPU arena) と
   tree depth 飽和 (step-size 適応)。 = Phase 80 の (c)→(a) 移植/arena 削減
   (`phase-NN-ad-c-path-migration.md`) と接続。
5. **【85.3 追記】** per-eval は Phase 85.3 (恒等演算畳み込み + superinstruction 融合
   + gather 内蔵/3 項融合) で **103→36µs (2.9×)**・radon の対 PyMC-C は
   **0.35×→0.81×**・numpyro 比 1.9× (詳細 = 題材 1 の追記節・
   `specification/phases/phase-85-ad-c-path-migration.md`)。 tree_depth は
   hanalyze=4.0 で pymc/numpyro と同一と実測 (深い tree 仮説は否定)。 残り gap
   (~0.8×) の候補 = warmup 固定費と per-draw の残オーバーヘッド。

TODO: M 系の numpyro 列を base grid で埋め M1-M9 も 3 系完成させる (速度曲線 84.2)。
