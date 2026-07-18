# HBM サンプラ性能スケーリング — hanalyze NUTS vs PyMC (M1-M6)

最終更新: 2026-06-10 (M3-M6 拡張・同一セッション再突合)

## 目的

ユーザ要望「HBM が複雑モデルで遅い。PyMC と**計算量オーダー (iter 依存性) が
一致するか**を iter を小さい方から掃いて検証したい」 に答える。本 md は
**harness 検証段 (M1/M2)**。M3-M6 は本 harness 確認後に順次拡張する。

## 計測条件 (両エンジン共通)

| 項目 | 値 |
|---|---|
| iter グリッド (本サンプル数) | 50, 100, 200, 400, 800, 1600 |
| warmup (tune) | **500 に固定** |
| chains | 1 |
| target_accept | 0.8 |
| max tree depth | 10 |
| seed | 固定 (HS=42 / PyMC=42) |
| データ | 同一 CSV (`bench/data/hbm_m{1,2}.csv`、HS 生成 → 両者が読む) |
| 計測 | HS=5回 median (`timeitIO`) / PyMC=3回 median |

- **M1** pooled 単回帰 `y_i ~ N(a + b·x_i, σ)` (n=100, latent 3)
- **M2** 階層 random intercept `y_ij ~ N(β0 + β1·x_ij + u_g, σ)`,
  `u_j ~ N(0,τ_u)` (n=96, 8群, latent 13)

両モデルは HS=`ModelP`・PyMC で同一 prior・同一 DGP。
harness: `bench/haskell/BenchHBMScaling.hs` / `bench/python/bench_hbm_scaling.py`。
生 CSV: `bench/results/{haskell,python}/hbm_scaling.csv`。

## 計測手法 (重要)

warmup を固定して**本サンプル数だけ掃く**ことで、wall-time を

```
total = (固定費) + (1 サンプル単価) × iter
```

の **affine モデル**で線形フィットし、切片 (固定費) と傾き (per-draw 単価) を
分離する。固定費 = HS は warmup 500 回、PyMC は **logp コンパイル + tune 500 回**
(PyMC のみコンパイル費が乗る。HS は AD ランタイム評価でコンパイル無し)。

★**log-log 傾きで O(iter) を読むのは誤り**: total は affine ゆえ log-log 傾きは
固定費が無視できる `iter ≫ 固定費/単価` の領域でしか 1 に漸近しない。
本レンジでは固定費が支配的なので **線形フィット (時間 vs iter) の R²** で
線形性 = O(iter) を判定する。

## 結果

### M1 (pooled 単回帰)

| iter | HS ms | PyMC ms | HS ess/s | Py ess/s | Py 平均tree depth |
|---:|---:|---:|---:|---:|---:|
| 50   | 943.1  | 1363.9 | 53.0  | 49.1   | 1.98 |
| 100  | 996.8  | 1260.7 | 100.3 | 65.1   | 2.02 |
| 200  | 1324.2 | 1211.5 | 151.0 | 149.4  | 2.01 |
| 400  | 1578.4 | 1195.8 | 253.4 | 282.7  | 2.00 |
| 800  | 2187.7 | 1425.5 | 365.7 | 600.5  | 2.00 |
| 1600 | 3453.3 | 1310.4 | 463.3 | 1495.8 | 1.99 |

- **HS 線形フィット**: `time_ms ≈ 907 + 1.60·iter` (**R²=0.9961**) →
  ★**O(iter) を強く確認**。per-draw = **1.60 ms**、warmup 固定費 ≈ 907 ms。
- **PyMC 線形フィット**: `time_ms ≈ 1273 + 0.042·iter` (R²=0.08, ほぼ平坦)。
  → サンプリング費が固定費 (compile+tune ≈ 1.27 s) の**計測ノイズに埋もれる**
  ほど安い。per-draw 0.042 ms は R² 0.08 ゆえ有効数字なし (「draw はほぼ無料」)。

### M2 (階層 random intercept)

| iter | HS ms | PyMC ms | HS ess/s | Py ess/s | Py 平均tree depth |
|---:|---:|---:|---:|---:|---:|
| 50   | 17028.3 | 1763.2 | 2.6  | 25.0  | 3.70 |
| 100  | 19551.6 | 1811.3 | 5.1  | 53.6  | 3.78 |
| 200  | 22348.6 | 1697.3 | 9.0  | 82.5  | 3.66 |
| 400  | 26928.9 | 1982.8 | 14.9 | 144.2 | 3.62 |
| 800  | 35512.3 | 2160.5 | 22.5 | 217.1 | 3.61 |
| 1600 | 44544.1 | 2368.4 | 35.9 | 461.1 | 3.60 |

- **HS 線形フィット**: `time_ms ≈ 18511 + 17.41·iter` (**R²=0.962**) →
  ★**O(iter) を確認**。per-draw = **17.41 ms**、warmup 固定費 ≈ **18.5 s**。
- **PyMC 線形フィット**: `time_ms ≈ 1745 + 0.417·iter` (R²=0.91)。
  per-draw = **0.417 ms**、固定費 ≈ 1.7 s。

## 結論 (事実)

1. **オーダーは一致する。HBM が遅いのは計算量オーダーではなく定数倍が原因。**
   HS の wall-time は両モデルとも iter に対し**強い線形** (R² 0.96–0.996) =
   PyMC と同じ **O(iter)**。「複雑モデルで遅い」 のは O(iter) の係数 (per-draw
   単価) と固定費が大きいため。

2. **per-draw 単価の差 (定数倍)**:
   - M1: HS 1.60 ms vs PyMC 「ノイズ未満」(実測下限 ~0.04 ms) → **HS が数十倍重い**。
   - M2: HS 17.41 ms vs PyMC 0.417 ms → **HS が約 42× 重い**。

3. **モデル複雑化での per-draw 増大 (HS 内)**: M1 1.60 ms → M2 17.41 ms = **約 11×**。
   - 観測数はほぼ同じ (100 vs 96) ので obs 数は主因でない。
   - 差分は **latent 次元 (3 → 12)** と **平均 tree depth (M1 2.0 → M2 3.6, PyMC計測)**。
     tree depth 3.6 vs 2.0 ≈ leapfrog 数 2^3.6/2^2 ≈ **3×**、残り ~3.6× は
     **前進モード AD による勾配の O(p) 倍率** (下記 Phase 53 診断で確定)。

4. **固定費 (warmup) も HS の方が重い**: M2 で HS 18.5 s vs PyMC 1.7 s (約 11×)。
   warmup も per-iter は同じ NUTS なので、warmup 500 回ぶんの per-draw 単価差が
   そのまま固定費差に出ている。

5. **ESS/秒**: PyMC が一貫して上 (M2 で約 10–13×)。HS の ESS は **n で clamp**
   される実装 (例: ess=800.0/1600.0 と iter ぴったり、M2_iter50 のみ 43.9<50) ので、
   混合品質自体は両者良好 (autocorr≈0)。差は ESS でなく **per-draw wall-time**。

## 検証メモ (推測と事実の区別)

- **事実**: 線形性 (R²)、per-draw 単価、固定費、tree depth (PyMC)、ESS/秒。
- **確定 (下記 Phase 53 診断)**: per-draw の主因 = **前進モード AD** で勾配が
  latent 数 p に比例 (gradADU/logJoint 比 ≈ p を実測)。
- **残課題**: HS 側 `SampleEvent` に tree depth / leapfrog 数を持たない (現状
  `seEnergy`/`seDivergent`/`seAccepted`/`seStepSize` のみ) ため HS の平均 tree depth
  を直接計測できない。reverse 化後の per-draw 改善検証時にテレメトリ追加を検討。

## 次段 (M3-M6)

harness は機能確認済。次は M3 (intercept+slope 階層) / M4 (多変量 X) /
M5 (パラメタ非線形) / M6 (組合せ) を同 harness に追加し、per-draw 単価が
モデル次元・非線形性でどう伸びるかを同じ線形フィットで測る。
→ ★**2026-06-10 実施済**。 末尾「M3-M6 拡張 + PyMC 同一セッション再突合」 節を参照。

---

## 追記 (2026-06-09): Phase 53 ボトルネック診断

「HBM にすると(NUTS 単体より)さらに遅い・複雑モデルで急に遅くなる」の真因を
`bench/haskell/BenchHBMProfile.hs` で計測。

### 真因 (事実・一次根拠 + 実測)

**`gradADU` が前進モード AD** (`src/Hanalyze/Model/HBM.hs:146` =
`import Numeric.AD.Mode.Forward (grad)`)。ad-4.5.6 ソースが
「forward mode grad は reverse mode より **O(n) 遅い** (n=入力次元)」と明記。

決定的計測 = 「1 勾配 (gradADU) / 1 log-joint (logJoint)」の時間比が
**latent 数 p にぴったり追従**:

| モデル | p | logJoint | gradADU | 比 (≈p) |
|---|--:|--:|--:|--:|
| M1 pooled (100obs) | 3 | 0.031 ms | 0.080 ms | 2.6 |
| M2 ranint (8群)    | 12 | 0.040 ms | 0.489 ms | 12.1 |
| M2 群2  | 6  | — | 0.075 ms | 6.0 |
| M2 群4  | 8  | — | 0.192 ms | 8.4 |
| M2 群8  | 12 | — | 0.505 ms | 11.5 |
| M2 群16 | 20 | — | 1.597 ms | 17.6 |
| M2 群32 | 36 | — | 6.108 ms | 30.0 |

- 観測数 sweep (M1 p=3 固定): 比は N_obs に依らず ~3 で一定 (logJoint/gradADU とも
  N_obs に線形)。→ **per-eval = O(p × N_obs)、勾配の O(p) 倍率は p (latent 数) が源**。
- NUTS は leapfrog 1 歩ごとに gradADU を呼ぶので、この O(p) がそのまま per-draw に乗る。
  M1 (p=3) → M2 (p=12) の per-draw 11× の主因 (残りは tree depth)。

### 最適化方針 (Phase 53)

`gradADU` を **reverse モード** (`Numeric.AD.Mode.Reverse.grad`) に切替。
reverse は scalar 出力の全勾配を **~1 sweep** で出す (p 非依存)。
見込み速度向上 ≈ p× (M2 で ~10×、p=36 で ~30×)。
要検証: reverse 勾配が forward と要素一致 (回帰テスト)、全 test green、
hbm_scaling 再計測で per-draw 改善を定量化。M3-M6 拡張はこの改善後。

---

## 追記 (2026-06-09): Phase 53 reverse-mode 化の実測結果

`gradADU`/`gradAD` を `Numeric.AD.Mode.Reverse.grad` に切替後 (53.2/53.3)、
同条件で再計測。

### gradADU/logJoint 比 (BenchHBMProfile・p 非依存になったか)

| p | forward 比 (≈p) | reverse 比 |
|--:|--:|--:|
| 6  | 6.0  | 4.3 |
| 8  | 8.4  | 2.6 |
| 12 | 11.5 | 4.1 |
| 20 | 17.6 | 4.0 |
| 36 | 30.0 | 4.1 |

→ ★**reverse は比が p によらず ~4 で一定** (O(p) ボトルネック解消)。
gradADU 絶対値 (p=36): forward 6.108ms → reverse **1.007ms (6.1× 速)**。
ただし p=3 (M1) は forward 0.080ms → reverse 0.164ms と**逆に遅い** (tape 構築
オーバヘッド)。 交差点は p≈5-6。

### NUTS スケーリング (warmup 固定・線形フィット)

| | per-draw (ms) | warmup 固定費 (ms) | 総計@1600 (ms) |
|---|--:|--:|--:|
| **M1 (p=3)** forward  | 1.60  | 907   | 3453 |
| **M1 (p=3)** reverse  | 2.225 | 1452  | 4994 |
| → M1 変化 | **1.39× 遅** | 1.6× 遅 | 1.45× 遅 |
| **M2 (p=12)** forward | 17.41 | 18511 | 44544 |
| **M2 (p=12)** reverse | 9.94  | 6533  | 22757 |
| → M2 変化 | **1.75× 速** | 2.8× 速 | **2.0× 速** |

(M1 reverse: `1452 + 2.225·iter` R²=0.999 / M2 reverse: `6533 + 9.938·iter` R²=0.995)

### 結論 (事実)

- **狙い通り、 latent が多い階層モデル (HBM の本来の用途) で高速化**:
  M2 (p=12) は per-draw 1.75×・warmup 2.8×・総計 2.0× 速。 p がさらに大きいほど
  gradADU の倍率は拡大 (p=36 で勾配 6.1×)。
- **トレードオフ = 低次元 (p≲5) モデルは reverse の tape オーバヘッドで ~1.4× 遅い**
  (M1 pooled)。 ただし pooled 単回帰は通常 OLS の領分で NUTS 用途は薄い。
- per-draw 改善 (1.75×) が gradADU 単体の改善 (2.2×) より小さいのは、 NUTS per-draw が
  勾配以外 (leapfrog 数=tree depth・運動量サンプル等) も含むため。 warmup 固定費の
  改善 (2.8×) は warmup 中の大量の勾配評価が効いている。

### 残課題・補足

- ★**選択肢**: 低次元の悪化を避けるなら **p 閾値で forward/reverse を切替** (p<~5 は
  forward) も可能 (gradADU に分岐追加)。 全 reverse 統一は単純だが M1 系を犠牲にする。
- ★**Phase 53 と無関係の既存バグ** (本計測中に発覚): `buildModelGraph` の Phase 38
  「複雑 9 例」 で deterministic ノード (Dirichlet の `p_k`・ar1Latent の `x_t`) が
  グラフから落ちる失敗が **develop でも同一に再現** (6 件)。 reverse 化前から存在する
  別件の回帰 (Phase 39-52 のどこか)。 別途修正対象。

---

## 追記 (2026-06-09): Phase 53 最終 — Reverse.Double に確定

generic `Reverse` が低次元 (M1) で tape boxing により forward より遅い問題を受け、
ad の 4 モードを直接比較 (`bench/haskell/BenchHBMADModes.hs`)。

### AD モード別 1 勾配時間 (ms・logJointUnconstrained の grad)

| p | forward | generic reverse | **Reverse.Double** | Kahn |
|--:|--:|--:|--:|--:|
| 3 (M1) | 0.190 | 0.136 | **0.100** | 0.527 |
| 6  | 0.150 | 0.051 | **0.037** | 0.170 |
| 8  | 0.345 | 0.090 | 0.095 | 0.308 |
| 12 | 1.018 | 0.183 | **0.169** | 0.932 |
| 20 | 3.434 | 0.518 | **0.271** | 2.032 |
| 36 | 11.537 | 0.779 | **0.578** | 4.805 |

→ ★**`Numeric.AD.Mode.Reverse.Double` (Double 特化) が全 p で最速**。 generic Reverse の
tape boxing オーバヘッドを避けるため、 低次元 (p=3) でも forward の 1.9× 速。 p=36 で
forward の **20× 速**。 ∴ **閾値分岐は不要・revDouble に統一**。 Kahn は最遅で不採用。

### NUTS スケーリング最終 (Reverse.Double・warmup 固定線形フィット)

| | per-draw (ms) | warmup (ms) | 総計@1600 (ms) | vs forward |
|---|--:|--:|--:|---|
| **M1 (p=3)** forward       | 1.60  | 907   | 3453  | — |
| **M1 (p=3)** Reverse.Double | 1.219 | 1129  | 3121  | per-draw **1.31× 速** |
| **M2 (p=12)** forward       | 17.41 | 18511 | 44544 | — |
| **M2 (p=12)** Reverse.Double | 5.894 | 4238  | 13560 | per-draw **3.0× 速**・総計 **3.3× 速** |

(M1 revDouble: `1129 + 1.219·iter` R²=0.988 / M2 revDouble: `4238 + 5.894·iter` R²=0.999)

### 最終結論 (事実)

- **`gradADU`/`gradAD` を `Numeric.AD.Mode.Reverse.Double` に統一** (Phase 53.2/53.3、
  HBM.hs + Stat/AD.hs)。 generic Reverse から更に Double 特化版へ。
- **全領域で forward を上回る**: 低次元 M1 (p=3) で per-draw 1.31× 速、 階層 M2 (p=12) で
  per-draw 3.0×・総計 3.3× 速。 p が大きいほど倍率拡大 (勾配 p=36 で 20×)。 低次元の
  犠牲ゼロ・閾値分岐不要。
- per-draw 改善 (M2 3.0×) が gradADU 単体改善 (p=12 で ~6×) より小さいのは NUTS per-draw が
  勾配以外も含むため。 warmup 固定費 4.4× 改善は warmup 中の大量勾配評価が効く。
- PyMC との per-draw 差 (M2): 旧 forward 17.41ms vs PyMC 0.417ms (42×) → revDouble 5.89ms で
  **約 14× 差**まで縮小。 残差は JAX の XLA コンパイル + ベクトル化 vs Haskell の
  Free-monad 逐次解釈・Map/Text オーバヘッド (別途最適化余地)。

---

## Phase 54.0 feasibility spike (2026-06-09・観測尤度ベクトル化 × Reverse.Double)

実体: `bench/haskell/BenchHBMVecSpike.hs` (exe `bench-hbm-vecspike`、 `-f benches`)。
Gaussian 線形回帰 y_i ~ Normal(a + b·x_i, σ) の対数尤度を 3 表現で書き、
`Numeric.AD.Mode.Reverse.Double.grad` の (Q1) 勾配保存と (Q2) per-grad 時間を計測。
HBM 本体は不変 (観測和の 3 表現だけを切り出した独立実験)。

3 表現: ① scalar list 内包 (現状 `obsLogSum` 形) / ② 非ボックス Storable Vector の手動 fold /
③ 十分統計量による fused 閉形式 (Σresid² を Σy・Σxy・Σx²… の Double 定数に展開、 O(1)/eval)。

### (Q1) 勾配の数値一致 (RevD.grad vs 中心差分・最大相対誤差)

| n | scalar | vec | fused | vec-vs-scalar | fused-vs-scalar |
|--:|--:|--:|--:|--:|--:|
| 50   | 1.69e-10 | 1.69e-10 | 1.69e-10 | 0 (ビット一致) | 5.1e-14 |
| 200  | 1.66e-10 | 1.66e-10 | 1.65e-10 | 0 | 3.1e-13 |
| 1000 | 2.36e-9  | 2.36e-9  | 2.36e-9  | 0 | 5.8e-13 |

→ **事実: Reverse.Double.grad はベクトル化/fused 表現でも勾配を保存する** (中心差分一致は
1e-10 = 中心差分の数値床。 vec は scalar とビット一致、 fused は和の順序差で 1e-13)。
**54.2 (観測尤度ベクトル化) の AD 前提は成立**。

### (Q2) per-grad 時間 (ms・median of 50) と scalar 比

| n | scalar | vec (×) | fused (×) |
|--:|--:|--:|--:|
| 50   | 0.0354 | 0.0354 (×1.00) | 0.0037 (×9.6) |
| 200  | 0.1665 | 0.1548 (×1.08) | 0.0066 (×25.3) |
| 1000 | 0.9292 | 0.8922 (×1.04) | 0.0042 (×220) |
| 5000 | 5.7525 | 5.8067 (×0.99) | 0.0041 (×1397) |

### 結論 (事実・54.2 設計への示唆)

- ★**利得は「Vector fold で list を排除」 からは出ない** (②は scalar 比 ×1.0)。 O(n) の
  AD tape ノード数が支配的で、 list alloc 除去は誤差。 → **単なる `observeVec` の
  `VS.foldl'` 化では速くならない** (推測で「ベクトル化すれば速い」 とすると外す)。
- ★**利得は「観測和を fused 閉形式に畳んで AD tape を O(1) 化」 から出る** (③は n=1000 で
  ×220、 n=5000 で ×1397)。 fused は per-obs の tape ノードを消すので n に依らず一定時間。
- ⚠ **適用条件**: ③が効くのは eta_i が **線形** (a + b·x_i) で、 二乗残差和が十分統計量
  (Σy, Σxy, Σx², n …) に展開できる Gaussian-恒等リンクの特殊構造ゆえ。 非 Gaussian
  (Poisson/Bernoulli 等) や非線形 eta では per-obs に eta_i 評価が残り O(n) tape は消えない
  → fused の桁違いの利得は **線形 Gaussian (回帰/GLMM Gaussian) が最良ケース**。 一般の
  vector-mean observe では suff-stat 畳み込みが常に可能とは限らない (54.2 計画の前提を補正)。
- → **54.2 の正しい標的 = 観測ブロックの AD tape ノード数削減**。 Gaussian は suff-stat
  fused、 GLM 系は eta_i のベクトル評価を 1 パスに融合 (リンク+密度を配列演算で)。
  「vector-mean observe」 は API としては良いが、 速度利得は密度が tape を畳めるか次第。

---

## Phase 54 専用ベクトル化 AD feasibility spike (2026-06-09・path B 判断)

実体: `bench/haskell/BenchHBMVecADSpike.hs` (exe `bench-hbm-vecad`、 `-f benches`)。
階層 Gaussian (random intercept、 M2 同型) の unconstrained logp の勾配を、
(a) `ad` Reverse.Double.grad (現状・スカラ tape) と (b) 手書きベクトル化解析勾配
(Storable/Unboxed 配列上の reduction のみ・tape なし) で計算し per-grad 時間を比較。
(b) は中心差分 (同一 logp) で rel err ~1e-8 を確認済 (正しさ担保のうえ計時)。

| nG | p (=2+nG+2) | n | ad (ms) | vec (ms) | speedup |
|--:|--:|--:|--:|--:|--:|
| 2  | 6  | 24  | 0.0284 | 0.0009 | ×30.5 |
| 4  | 8  | 48  | 0.0458 | 0.0016 | ×28.7 |
| 8  | 12 | 96  | 0.0890 | 0.0039 | ×22.7 |
| 16 | 20 | 192 | 0.1890 | 0.0076 | ×25.0 |
| 32 | 36 | 384 | 0.3929 | 0.0105 | ×37.3 |

### 結論 (path B = compile+vectorize の feasibility)

- ★**tape-free ベクトル化勾配は `ad` の ~22〜37× 速い** (階層モデル・正しさ検証済)。
  現状 hanalyze は PyMC の 14× 遅いので、 この天井の一部でも実現すれば numpyro に
  **追いつく/超える**余地がある。 → **専用ベクトル化 AD の構築は feasibility GREEN**。
- ⚠ **これは解析勾配 = 絶対下限**。 汎用ベクトル化 reverse-mode AD エンジンは
  グラフ走査・中間配列確保のオーバヘッドで**この速度には届かない** (現実は ×5〜15 程度の
  見込み・要・第2 spike で実測)。 「同等」 と過大表現しない。
- ★**win の本質 = AD tape が「ベクトル演算 1 個 = 1 ノード」 になり O(#vector-ops) に
  縮む** (現状の `ad` は per-scalar-op で O(n) ノード)。 これは観測尤度を
  eta = Xβ + Zu の**ベクトル演算で表現**できて初めて成立 → Phase 54.1 の `ObserveLM`
  (X と β を構造保持) が前提足場として効く。
- **次の一手**: 汎用エンジン (案: `backprop` ライブラリ + hmatrix vector op、 または
  hand-roll した Storable 上 reverse-mode) で M2 logp を試作し、 解析下限のどれだけを
  取れるか・`ad` を有意に上回るかを第2 spike で実測してから本実装判断。

## Phase 54.3 第2 spike — 汎用ベクトル化 reverse-mode AD の実測 (2026-06-10)

54.2 の解析勾配 (絶対下限) に対し、 **汎用エンジン 2 案**を同一 logp (階層 Gaussian・
M2 同型) で実装し per-grad を実測。 両案とも中心差分一致 (rel err 1e-8) を確認済。

- **案A = `backprop` ライブラリ** (BSD3)。 theta を 1 本の Storable Vector とみなし
  `gradBP` で微分。 ベクトル演算 (scale/add/sub/gather/dot) は `liftOp` で随伴を手書き
  (tape = ベクトル演算 1 個 = 1 ノード)。 chain rule と tape 所有は backprop が担う。
  ※計画の「hmatrix-backprop static op」 は型レベル Nat 儀式が spike に重く、 実行時に
  nG を変える本 spike には不適ゆえ plain backprop + Data.Vector.Storable を採用。
- **案B = 自作・最小 reverse-mode** (vector-op tape)。 forward でベクトル演算ごとに発番し
  随伴更新クロージャを逆順に積む (自前 Wengert tape・ST + STArray)。 backward で逆位相順に
  replay。 随伴の式は案A と同一 → **案A/案B の差は「tape をライブラリが持つか自前か」に純化**。

| nG | p | n | ad (ms) | vec=下限 (ms) | bp=案A (ms) | hr=案B (ms) | ad/bp | ad/hr | hr/vec |
|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|
| 2  | 6  | 24  | 0.0263 | 0.0008 | 0.0107 | 0.0050 | ×2.5 | ×5.3  | 6.0 |
| 4  | 8  | 48  | 0.0475 | 0.0016 | 0.0158 | 0.0067 | ×3.0 | ×7.1  | 4.2 |
| 8  | 12 | 96  | 0.0944 | 0.0027 | 0.0185 | 0.0071 | ×5.1 | ×13.2 | 2.7 |
| 16 | 20 | 192 | 0.1945 | 0.0053 | 0.0288 | 0.0107 | ×6.8 | ×18.2 | 2.0 |
| 32 | 36 | 384 | 0.4119 | 0.0114 | 0.0498 | 0.0171 | ×8.3 | ×24.1 | 1.5 |

### 結論・判断 (54.3 ゲート: 汎用版が `ad` を ≥5× 上回れば 54.4 本実装へ)

- ★**GREEN・採用は案B (自作 vector-op tape)**。 ad 比 **×5.3〜×24.1** (規模とともに伸長)、
  解析下限からの乖離 (hr/vec) も nG=32 で **×1.5** まで縮む。 ゲート (≥5×) を全域で通過。
- 案A (backprop) も正しく動き ad 比 ×2.5〜×8.3 だが、 ライブラリ簿記で**案B の約 3 倍遅い**
  (bp/vec は nG=32 で ×4.4)。 → 採用は案B。 案A の役割は **「汎用 tape が正しく・速くなる
  ことの独立検証器」** (speedup が手書き由来の artefact でないと確認) として十分果たした。
- 採用理由 (計測 + 設計の両面): ① 計測で最速・下限に最も近い ② 動的 Free walk への構造適合
  (54.4 = Free を 1 回走査して vector-op 列にコンパイルする形と 1:1) ③ 外部依存ゼロ
  (案A 採用なら backprop + array dep だが、 案B は array のみ)。
- ⚠ **過大表現しない**: 本 spike は**勾配カーネル単体**の比較。 per-draw 全体 (NUTS 統合後)
  の改善は 54.4 実装後に再計測が必要。 PyMC 差 14× を埋める「見込み」 であって既達ではない。
- 実体: `BenchHBMVecADSpike.hs` に gradBackprop (案A)・gradHandroll (案B) 追加。
  GHC 9.6.7 の exitification パス panic 回避に `-fno-exitification` (4 手法同一・比較不変)。

## Phase 54.4a 本実装 per-call 計測 — ハイブリッド gradADU (vec-tape ObserveLM) (2026-06-10)

54.3 で採用した案B (自前 vector-op tape) を本実装へ移植し、 `gradADU` をハイブリッド化
(Gaussian-恒等リンク `ObserveLM` ブロックの観測尤度を vec-tape・prior/jacobian/scalar
observe/非 Gaussian LM は従来 `ad`)。 同一の階層 Gaussian (M2 random intercept) を 2 通りに
エンコードして **gradADU 1 回の median 時間** を比較 (`bench-hbm-54a`)。 NUTS は 1 draw あたり
leapfrog ごとに gradADU を多数回呼ぶので per-call 単価が per-draw コストの支配項。

### ★罠と修正: ランダム効果の密 one-hot 展開は階層モデルで逆効果 (計測で確定)

最初、 群効果 u_j を `observeLM` の**密設計行列の one-hot 指示列**として畳んだところ、

| nG | p | n | sc=scalar全ad (ms) | vl=vecLM (ms) | sc/vl |
|--:|--:|--:|--:|--:|--:|
| 2  | 2 | 24  | 0.0435 | 0.0221 | ×2.0 |
| 8  | 2 | 96  | 0.1217 | 0.0709 | ×1.7 |
| 16 | 2 | 192 | 0.2651 | 0.3213 | ×0.8 🔻 |
| 32 | 2 | 384 | 0.6181 | 2.0162 | ×0.3 🔻 |

小規模では速いが **nG が増えると逆に遅くなる** (nG=32 で 3 倍遅い)。 真因 = 密 one-hot 列は
`eta = Σ_k β_k col_k` が (p+nG) 本の密ベクトル演算 = **O(nG·n)** になる (指示列は本来疎)。
54.2/54.3 spike が ×22〜37 を出せたのは u を **gather (O(n))** で扱っていたから。

→ **修正**: `ObserveLM` に gather スロット `REff [Text] [Int]` (u 名 + 群 id) を追加
(`observeLMR` combinator)。 vec-tape は `gatherHR` で u 効果を O(n) 寄与させる。

### 修正後 (REff gather) per-call gradADU

| nG | p | n | sc=scalar全ad (ms) | vl=vecLM gather (ms) | sc/vl | relErr(vs央差) |
|--:|--:|--:|--:|--:|--:|--:|
| 2  | 2 | 24  | 0.0591 | 0.0288 | ×2.1 | 1.1e-8 |
| 4  | 2 | 48  | 0.0958 | 0.0399 | ×2.4 | 7.4e-8 |
| 8  | 2 | 96  | 0.1808 | 0.0530 | ×3.4 | 1.3e-7 |
| 16 | 2 | 192 | 0.3733 | 0.0785 | ×4.8 | 2.5e-7 |
| 32 | 2 | 384 | 0.8077 | 0.1392 | **×5.8** | 5.0e-7 |

- ★**逆転が解消し、 speedup が規模とともに伸びる** (×2.1→×5.8)。 正しさは中心差分一致
  (relErr 1e-7) を全域で維持。 nG=32 で per-call gradADU が **×5.8 速い**。
- spike の ×22 (解析下限) に届かないのは、 ハイブリッドが **prior 部の `ad` 勾配** (model walk)
  + 制約変換の chain rule + 簿記を依然払うため (vec-tape は観測尤度部のみ)。 それでも主目的の
  階層モデルで右肩上がりの利得。
- ⚠ **過大表現しない**: per-call gradADU の比較であって **per-draw NUTS 全体ではない**
  (NUTS 統合後の wall-time・PyMC 差 14× の縮小は 54.4b/54.5 で別途再計測)。
- 実体: `BenchHBM54a.hs` (scalar=`glmmRandomIntercept` / vecLM=`observeLMR`+`REff`)。

### per-draw NUTS wall-time (54.4a helper 書換え後・実 deliverable)

`glmmRandomIntercept` を `observeLMR` 発行に書換え (公開 API 不変・観測は PyMC/Stan 同様の
単一ベクトル化ノード "y"・旧 per-obs y_i n 個展開を廃止)。 これで実モデルが vec 経路に乗る。
M2 (random intercept) を scalar per-obs observe (全 ad) と比較 (warmup 300 + 300 draws・3 reps)。

| nG | n | scalar (ms/draw) | vecLM (ms/draw) | sc/vl |
|--:|--:|--:|--:|--:|
| 8  | 96  | 14.39 | 7.13  | ×2.0 |
| 32 | 384 | 89.64 | 33.68 | **×2.7** |

- ★**per-draw NUTS wall-time が階層モデルで ×2.0〜×2.7**・規模で伸長。 これが 54.4a の実 deliverable
  (per-call gradADU ×5.3 より小さいのは NUTS の運動量サンプリング・tree 構築・leapfrog 簿記・
  prior 部 `ad` が per-draw に乗るため)。
- ⚠ **過大表現しない**: PyMC との直接比較 (差 14× の縮小幅) は `bench_hbm_scaling.py` の再実行が要る
  (本表は HS 内 scalar vs vecLM の比)。 ×2.7 は「14× を ~5× に縮める見込み」 を**実測で裏打ち**する
  が、 PyMC 突合は未実施。 54.4b (IR キャッシュ) で walk 除去の追加利得を測る。
- 実体: `BenchHBM54a.hs` per-draw NUTS 節。

## Phase 54.4b 静的部分の hoisting (compileGradU) per-call/per-draw 計測 (2026-06-10)

モデル構造は draw 間で不変ゆえ、 `gradADU` の静的部分 (Gaussian LM ブロック抽出・設計列の
ベクトル化 `row !! k` = O(n·p²)・gids unbox・ys Storable 化・`ad` クロージャ構築) を
`compileGradU` で **1 度だけ**前処理し、 返ったクロージャ `[Double]->[Double]` を NUTS の
全 leapfrog で再利用する (旧: 毎勾配評価で `gradADU` を呼び静的部分を再構築)。 数値は不変。

### per-call gradADU (vl=rebuild=54.4a・vlc=compiled-reuse=54.4b)

| nG | p | n | sc=全ad | vl=rebuild | vlc=reuse | sc/vlc | **vl/vlc** | relErr |
|--:|--:|--:|--:|--:|--:|--:|--:|--:|
| 2  | 2 | 24  | 0.0569 | 0.0302 | 0.0243 | ×2.3 | ×1.2 | 1.1e-8 |
| 8  | 2 | 96  | 0.1762 | 0.0470 | 0.0380 | ×4.6 | ×1.2 | 1.3e-7 |
| 16 | 2 | 192 | 0.3781 | 0.0761 | 0.0598 | ×6.3 | ×1.3 | 2.5e-7 |
| 32 | 2 | 384 | 0.7688 | 0.1584 | 0.1052 | **×7.3** | **×1.5** | 5.0e-7 |

### per-draw NUTS wall-time (NUTS は compileGradU 経由に変更済)

| nG | n | scalar (ms/draw) | vecLM (ms/draw) | sc/vl |
|--:|--:|--:|--:|--:|
| 8  | 96  | 18.98 | 6.82  | ×2.8 |
| 32 | 384 | 90.25 | 31.06 | **×2.9** |

- ★**静的部分 hoisting は per-call で明確に ×1.2〜×1.5** (規模で伸長)。 sc/vlc は nG=32 で ×7.3。
- per-draw NUTS は 54.4a (×2.0/×2.7) から **×2.8/×2.9** へ微増 (~5-8%)。 ⚠**過大表現しない**:
  per-draw の改善が控えめなのは勾配評価が per-draw コストの一部で、 **prior 部の `ad`・tape の
  毎回再構築・運動量サンプリング/tree 構築**が残るため。 tape 構造自体のキャッシュ (= 真の IR
  キャッシュ) や prior のベクトル化は 54.4c 以降の課題。
- 数値不変 (relErr 1e-7・全 1016 test 中 fail は既知 Phase 38 stale 6 件のみ)。
- 実体: `compileGradU` (HBM.hs)・`CompiledLMBlock`/`compileLMBlock`/`gradCompiledLMBlock`・
  NUTS `gradFn` が `compileGradU` を 1 度呼ぶよう変更。 bench = `BenchHBM54a.hs` vlc 列。

## ★Phase 54.4a/b PyMC 突合 — 階層 M2 で PyMC 差 14.1× → 6.0× (2026-06-10)

54.4a/b 後の Haskell `bench-hbm-scaling` (M2 = random intercept・nG=8・n=96・glmm helper は
`observeLMR` 発行 + NUTS は `compileGradU` 経由) を再計測し、 **同一データ CSV・同一設定**
(warmup 500・iter grid・target_accept 0.8・maxdepth 10) の既存 PyMC 結果と突合。 PyMC 側は
コード/データ/設定とも不変ゆえ既存 `bench/results/python/hbm_scaling.csv` を再利用 (pymc 再
install 不要・per-draw は PyMC コードの性質で不変)。 per-draw 単価は iter グリッドの線形フィット
(total = 固定費 + 単価·iter) の傾き。

| 系 | per-draw (ms) | PyMC 比 | 固定費 (warmup, ms) |
|---|--:|--:|--:|
| PyMC (NUTS) | 0.417 | 1.0× | 1745 (compile+tune) |
| **HS 旧** (54.4 前・per-call ad rebuild) | 5.894 | **14.1×** | 4238 |
| **HS 新** (54.4a/b) | 2.493 | **6.0×** | 1533 |

- ★**Phase 54.4a/b は階層 M2 の PyMC 差を 14.1× → 6.0× に縮めた** (HS 自身の per-draw 改善 ×2.36)。
  旧 14.1× は [[hbm-nuts-perf-bottleneck]] の「PyMC 差 14×」 と一致 (ベースライン検証 OK)。
- 副次: warmup 固定費も 4238 → 1533 ms に低下し、 **HS 固定費が PyMC (1745・logp compile 込み) を
  下回った** (HS は AD ランタイム評価で compile 費なし)。
- ⚠ 残 6.0× は依然開き。 真因は **prior 部の `ad` (model walk) + vec-tape の毎勾配再構築**
  (54.4b で静的データは hoist 済だが tape 構造は毎回再構築) → 54.4c (真の IR tape キャッシュ +
  prior ベクトル化) の対象。 また PyMC は C コンパイル済 logp + 多群でのベクトル化が効く。
- ⚠ PyMC 数値は前回 run (同一 data/config) の再利用であって同一セッション再実行ではない。 マシン/
  BLAS 状態を厳密に揃えた fresh 比較が要るなら pymc install + `bench_hbm_scaling.py` 再実行が要る。

## ★Phase 54.4c 内訳計測 — 残ボトルネックは prior の ad (tape 再構築でない) (2026-06-10)

54.4b 後の per-call gradADU (compiled-reuse) を **prior 部 (ad) と LM 部 (vec-tape)** に分解
(prior-only モデル `m2PriorOnly` の gradADU = priorGrad 単体を計測・差分で LM 分)。

| nG | p | n | vlc 全体 (ms) | prior=ad (ms) | LM≈vec (ms) | **prior 割合** |
|--:|--:|--:|--:|--:|--:|--:|
| 2  | 2 | 24  | 0.0340 | 0.0182 | 0.0158 | 53% |
| 8  | 2 | 96  | 0.0302 | 0.0141 | 0.0160 | 47% |
| 16 | 2 | 192 | 0.0408 | 0.0222 | 0.0186 | 54% |
| 32 | 2 | 384 | 0.0732 | 0.0549 | 0.0184 | **75%** |

- ★**残ボトルネックは prior 部の `ad`** (per-grad の 44〜75%・**nG とともに増大**)。 主因は
  u_j ~ Normal(0, τ_u) の nG 個 = O(nG) のスカラ ad reverse。 LM 部 (vec-tape) は **~0.016ms で
  ほぼ一定** (gather で O(n) 維持済・既に十分速い)。
- ⚠ **計測で方針転換**: 54.4c は当初「tape 構造キャッシュ」 を想定していたが (LM 部 25%・一定ゆえ
  効果薄)、 計測で **真の標的は prior 勾配のベクトル化** (特に O(nG) のランダム効果 prior) と判明。
  推測で tape-cache に進んでいたら逆方向だった。
- → **Phase 54.4c = prior 勾配のベクトル化**: u_j ~ Normal(0,τ) 等の iid 階層 prior を解析/vec-tape
  勾配に置換し `ad` の O(nG) を外す。 値依存分岐のある分布は scalar `ad` に fallback。

## ★Phase 54.4c 本実装 — 群効果 prior の解析勾配 + ad 除外 (2026-06-10)

第一級ランダム効果値 (`reNormal`/`at`・`REff` に prior スケール名を載せる) を導入し、 `compileGradU`
が `u_j ~ Normal(0, τ)` の u-prior 勾配を **解析的に** (∂/∂u_j=-u_j/τ²・∂/∂τ=-nG/τ+Σu²/τ³・
O(nG) の素な Double) 計算、 対応する `u_j` `Sample` ノードを `logJointExclBlocks` の `ad` walk から
除外する。 vl=54.4b (prior は ad)・vla=54.4c (prior は解析)。 同一モデル・同一数値 (下記 relErr)。

### per-call gradADU (vlc=54.4b prior-ad・vla=54.4c prior-解析・compiled-reuse)

| nG | p | n | vlc (ms) | vla (ms) | **vlc/vla** | relErr (vs ad/中心差分) |
|--:|--:|--:|--:|--:|--:|--:|
| 2  | 2 | 24  | 0.0491 | 0.0238 | **×2.06** | 1.08e-8 |
| 4  | 2 | 48  | 0.0308 | 0.0279 | ×1.10 | 7.42e-8 |
| 8  | 2 | 96  | 0.0506 | 0.0328 | ×1.54 | 1.32e-7 |
| 16 | 2 | 192 | 0.0622 | 0.0490 | ×1.27 | 2.45e-7 |
| 32 | 2 | 384 | 0.1067 | 0.0867 | **×1.23** | 5.00e-7 |

### per-draw NUTS wall-time (warmup 300 + 300 draws・3 reps median)

| nG | n | sc=scalar 全ad (ms/dr) | vl=54.4b (ms/dr) | vla=54.4c (ms/dr) | sc/vla | **vl/vla** |
|--:|--:|--:|--:|--:|--:|--:|
| 8  | 96  | 17.66 | 6.48  | 6.24  | ×2.8 | ×1.04 |
| 32 | 384 | 85.04 | 29.79 | 22.53 | ×3.8 | **×1.32** |

- ★**54.4c は 54.4b 比で nG とともに伸長**: per-call ×1.1〜2.1・per-draw ×1.04 (nG=8)〜**×1.32**
  (nG=32)。 標的どおり prior の O(nG) ad が外れ、 群数が増えるほど効く。 scalar 比は per-draw ×3.8
  (nG=32)。 relErr ≤ 5e-7 (ad/中心差分) で**数値不変**を確認 (test でも 1e-7/1e-4 担保)。
- ⚠ **過大表現しない**: 本表は **HS 内比** (vl/vla)。 PyMC との突合 (54.4a/b で 6.0×) の再計測は
  config を揃えた `bench-hbm-scaling` 再実行が要る (pymc install 後)。 ここでは「54.4b から
  per-draw が nG=32 で 1.32× 速くなった」 が実測の主張で、 PyMC 差の縮小は別途。
- prior 部が消えた残りの per-draw コストは LM 部 vec-tape + NUTS leapfrog 本体 + warmup 適応。
  さらなる短縮は 54.5 で PyMC config 突合 + 必要なら他 prior (β 等) のベクトル化を検討。

## ★Phase 54.5 PyMC config 突合 + クローズ — M2 (nG=8) で PyMC 差 6.0× → 5.0× (2026-06-10)

54.4c 後の Haskell `bench-hbm-scaling` (M2 = glmmRandomIntercept・nG=8・n=96・**reNormal/at 経由で
解析 prior**) を再計測し、 54.4a/b と同一手法 (iter グリッド `[50,100,200,400,800,1600]`・warmup 500・
total = 固定費 + per-draw·iter の線形フィット傾き) で per-draw 単価を抽出、 **同一データ・同一設定**の
既存 PyMC 結果 (`bench/results/python/hbm_scaling.csv`・コード/データ/設定不変ゆえ再利用) と突合。
フィット手法は旧データで HS 2.4932 / PyMC 0.4170 (= 6.0×) を完全再現する検証済み。

| 系 | per-draw (ms) | PyMC 比 | 固定費 (warmup, ms) |
|---|--:|--:|--:|
| PyMC (NUTS) | 0.417 | 1.0× | 1745 |
| HS 54.4a/b | 2.493 | 5.98× | 1533 |
| **HS 54.4c** | **2.068** | **4.96×** | 1185 |

- ★**M2 (nG=8) の PyMC 差は 5.98× → 4.96×** (per-draw 54.4b 比 ×1.21)。 prior の O(nG) ad 除去が
  標準 M2 でも効いた。 固定費も 1533→1185ms に低下 (PyMC 1745 を引き続き下回る)。
- ⚠ **過大表現しない・cross-session の注意**: この HS 値は 54.4c の **fresh 単独 run** で、 PyMC は
  54.4a/b 時の既存 CSV (別 run) の再利用。 固定費も約 23% 落ちており、 マシン状態の run-to-run 変動が
  per-draw 短縮 (×1.21) に一部混入している可能性がある。 **54.4c 固有の variance-free な差**は同一
  セッション制御 A/B である 54.4c 本実装節 (`bench-hbm-54a`) の方が信頼でき、 そこでは per-draw
  ×1.04 (nG=8)〜×1.32 (nG=32)・per-call ×1.54 (nG=8)。 厳密な fresh PyMC 突合が要るなら
  pymc install + `bench_hbm_scaling.py` 同一セッション再実行が要る。
- ★**Phase 54 ここまでの到達点** (M2 nG=8・per-draw): 54 前 5.894ms (PyMC 14.1×) → 54.4a/b 2.493 (6.0×)
  → 54.4c 2.068 (5.0×)。 AD モード (53) + 観測尤度ベクトル化 (54.4a) + hoisting (54.4b) + 解析 prior
  (54.4c) で **per-draw を約 2.85× 高速化**・PyMC 差を 14.1× から ~5× に縮小。
- ⚠ 本節の旧版は「ここで Phase 54 クローズ・残課題 = leapfrog ベクトル化等」 と書いたが、 残差理由が
  **憶測**だとユーザ指摘 → 撤回。 54.4c 後の cost-centre profile で残 gap の内訳を実測したのが次節。
  **Phase 54 は 54.4d/e へ継続**。

## ★Phase 54.4c 後の cost-centre profile — 残 gap は「値評価 46% + 残 prior ad 19%」 (2026-06-10)

`prof-nuts` (M2 nG=8・warmup 500 + 800 draws・glmm=reNormal/at 経由) を profiling ビルドで再実行。
total 7.16s (Phase 53 時の同条件 23.58s → 3.3×)。 生 prof = `prof-nuts-54.4c.prof`。

| 経路 | %time (inherited) | 中身 (個別 %time) |
|---|--:|---|
| **logp 値評価** (`logJointUnconstrained`・27,703 回) | **46.1%** | `logJoint` Free walk 16.6 / **`logDensityObs` 21.8** (= 2,659,488 回 = 27,703×96 obs の per-obs スカラ) / prior `logDensity` 3.4 / モデル再構築 (`reNormal`/`indexed` Text 生成 22 万回) 2.9 |
| **勾配** (`leapfrogWithMVS`→`compileGradU` closure・26,403 回) | **49.9%** | vec-tape 演算 (gather/dot/scale/add/sub/runTape) ~15.9 / **`ad` 固定費 (`reifyTypeable` tape 生成 52,806 回) 18.9** (配下 prior `logDensity` 5.2 含む) / `compileGradU` self 6.0 |

- ★**残 gap の最大要因は logp「値」評価が未ベクトル化なこと (46.1%)**: NUTS は tree node ごとに
  エネルギー (logp の値) を評価する。 勾配は vec 化したが値は手つかずで、 Free walk + per-obs スカラ
  `logDensityObs` のまま (270 万回呼出)。 PyMC は値も勾配も同じコンパイル済みカーネル。
- ★**第二要因は残 prior (β/σ/τ = 4 個) のための `ad` 固定費 (18.9%)**: 毎勾配呼出しで reflection
  ベースの tape を生成 (`reifyTypeable`)。 ad 内 `logDensity` 呼出 211,224 回 = 52,806 × **ちょうど 4**
  — 54.4c の u_j 除外が効いている裏付けでもある。
- vec-tape 演算本体は ~16% で健全 (LM 勾配計算そのもの)。
- → **54.4d = logp 値評価のコンパイル** (`CompiledLMBlock` の forward 値 + 解析 u-prior 値 + 残り
  スカラ walk を NUTS エネルギー評価に配線)・**54.4e = 残 prior 勾配の解析化** (定数パラメタ prior の
  解析勾配 + residual 空なら ad 完全省略 = reifyTypeable 除去)。 両標的で per-draw の ~65% を攻める。

## ★Phase 54.4d/e 本実装 — M2 per-draw 2.068→0.574ms・PyMC 差 4.96×→1.38× (2026-06-10)

54.4d = `compileLogPU` (logp **値** 評価のコンパイル: LM ブロック値を素 Double ベクトル演算 +
解析 u-prior 値 + 残り Double walk・residual 空なら walk 自体も省略) + NUTS の logPiFn 配線。
54.4e = 定数パラメタ prior (extractDeps deps ∅ + 対応 13 分布) の解析勾配 + 除外後に密度項が
残らなければ **`ad` クロージャを丸ごと省略** (reflection tape 生成ゼロ・logJac 勾配も解析式)。
scalar Observe / Potential / 非 Gauss LM 残存時は従来経路に fallback (test で担保)。

### per-draw NUTS wall-time (`bench-hbm-54a`・warmup 300 + 300 draws・同一セッション A/B)

| nG | n | sc=scalar 全ad | vl=REff Nothing | vla=reNormal/at | sc/vla | 54.4c 時 vla → 今回 |
|--:|--:|--:|--:|--:|--:|--:|
| 8  | 96  | 14.32 | 3.10  | **1.38** | **×10.4** | 6.24 → 1.38 (×4.5) |
| 32 | 384 | 69.39 | 12.35 | **5.79** | **×12.0** | 22.53 → 5.79 (×3.9) |

per-call gradADU (vla) も 0.0328→0.0108ms (nG=8)・0.0867→0.0384ms (nG=32)。 relErr ≤ 5e-7 不変。

### bench-hbm-scaling 線形フィット + PyMC 突合 (M2 nG=8・iter グリッド・既存 PyMC CSV)

| 系 | per-draw (ms) | PyMC 比 | 固定費 (warmup, ms) |
|---|--:|--:|--:|
| PyMC (NUTS) | 0.417 | 1.0× | 1745 |
| HS 54.4a/b | 2.493 | 5.98× | 1533 |
| HS 54.4c | 2.068 | 4.96× | 1185 |
| **HS 54.4d/e** | **0.574** | **1.38×** | **347** |

- ★**M2 の PyMC 差は 1.38× まで縮小** (Phase 54 開始時 14.1×)。 固定費 347ms は PyMC (1745) の 1/5。
  posterior 品質は不変 (beta_1 mean 0.828・ESS/accept 同水準)。 ⚠PyMC CSV は既存 run 再利用
  (cross-session・従来と同じ注意)。
- ⚠**M1 (pooled) は 26.7× のまま** (per-draw 1.12ms vs PyMC 0.042ms): M1 は per-obs scalar
  `observe` 手書きで **ObserveLM 経路に乗らない** (全 ad + 全 walk の fallback)。 高速経路は
  `observeLM`/`observeLMR`/`glmmRandomIntercept` 等の構造化観測で書かれていることが条件。
- ⚠ FP 和順序が変わるため chain は 54.4c とビット非同一 (数値等価は test 1e-9/1e-7/中心差分で担保)。

### cost-centre profile (54.4d/e 後・`prof-nuts-54.4e.prof`・同一ワークロード)

total **7.16s → 1.85s** (×3.9)。 旧 hotspot は全て消滅:

| 旧 (54.4c) | % | 今回 (54.4d/e) | % |
|---|--:|---|--:|
| logp 値評価 (logJoint walk + logDensityObs) | 46.1 | **消滅** (compileLogPU self に置換) | 14.7 |
| 残 prior ad (reifyTypeable/bind/partials) | ~19 | **消滅** (解析勾配・ad ゼロ) | 0 |
| vec-tape 演算 | ~16 | vec-tape 演算 (gather/dot/scale/add/sub/runTape/scalar) | ~52 |
| compileGradU self | 6.0 | compileGradU self (Map 組立・zip 等) | 17.9 |
| NUTS 本体 (nutsStream+leapfrog) | ~6 | NUTS 本体 | ~13 |

- 残 1.38× の内訳 (実測): **vec-tape カーネル ~52% + クロージャ self ~33%** (compileGradU 17.9 +
  compileLogPU 14.7 = 毎呼出の `Map.fromList paramsC` 組立・name zip 等の plumbing) + NUTS 本体 ~13%。
  → さらに縮めるなら標的は (a) per-call の Map 組立を positional vector 化 (b) vec-tape の
  演算 fusion。 ただし**ここから先の利得見込みは未計測** — 着手前に per-op 内訳の計測が要る。

## ★Phase 54.6 解析閉形式カーネル + positional vector 化 — M2 PyMC 比 0.68× (追い越し) (2026-06-10)

54.6a = Gaussian-恒等リンク LM の勾配は**閉形式** (∂β_k=X_kᵀr/σ²・∂u_j=Σ_{i∈g_j}r_i/σ²・
∂σ=-n/σ+sumR2/σ³) ゆえ汎用 vec-tape を撤去し、 fused 1 パス残差 + 直接計算 (`gradLMBlockIx`/
`valueLMBlockIx`)。 54.6b = 名前→index を compile 時に解決 (`CompiledLMBlockIx`/`ReffPriorIx`)、
新 `compileGradUV`/`compileLogPUV` は VS.Vector native (Text-key Map 組立・VS↔list 変換なし・
勾配集約は ST mutable vector)。 NUTS は V 版を直接配線。 list API は wrapper で不変。

### per-draw NUTS wall-time (`bench-hbm-54a`・同一セッション A/B)

| nG | n | sc=scalar 全ad | vl | vla | sc/vla | 54.4d/e 時 vla → 今回 |
|--:|--:|--:|--:|--:|--:|--:|
| 8  | 96  | 14.29 | 2.66  | **0.627** | **×22.8** | 1.38 → 0.627 (×2.2) |
| 32 | 384 | 70.75 | 10.62 | **2.738** | **×25.8** | 5.79 → 2.74 (×2.1) |

per-call gradADU (vla nG=32) 0.0384→0.0203ms。 relErr ≤ 5e-7 不変 (test green 1019)。

### bench-hbm-scaling 線形フィット + PyMC 突合 (M2 nG=8)

| 系 | per-draw (ms) | PyMC 比 | 固定費 (ms) |
|---|--:|--:|--:|
| PyMC (NUTS) | 0.417 | 1.0× | 1745 |
| HS 54.4d/e | 0.574 | 1.38× | 347 |
| **HS 54.6** | **0.283** | **0.68×** | **186** |

- ★**M2 (階層 random intercept) で HS が PyMC を追い越した** (per-draw 0.68×・固定費 1/9)。
  Phase 54 開始時 14.1× → 0.68× = 累計 **約 20.8× 高速化** (5.894→0.283 ms/draw)。
  posterior 品質不変 (beta_1 mean 0.828・ESS 健全)。 ⚠PyMC CSV は既存 run 再利用 (cross-session・
  マシン/BLAS 揃え fresh 比較が要るなら pymc install + 同一セッション再実行)。
- ⚠**M1 (per-obs scalar 手書き) は 30.2× のまま** (fallback 経路・54.8 の標的)。

### cost-centre profile (54.6 後・`prof-nuts-54.6.prof`)

total **1.85s → 1.10s** (Phase 53 時 23.58s → 累計 ×21)。 内訳:

| cost centre | %time | %alloc | 中身 |
|---|--:|--:|---|
| compileGradUV (self・全カーネル込) | 68.0 | 84.1 | fused 残差 + p dots + scatter + runST/freeze (毎 grad 呼出の alloc が支配) |
| compileLogPUV | 16.7 | 1.8 | fused 残差の値版 |
| nutsStream self | 11.3 | 10.5 | tree 構築・RNG・mass 適応等 (SCC 無しでこれ以上割れない) |
| leapfrogWithMVS self | 3.4 | 3.3 | 運動量/位置の VS 更新 |

- **54.7 (NUTS 本体) の計測結果**: NUTS 本体 (nutsStream + leapfrog) は **~15%・絶対値 ~0.1ms/draw**。
  さらに割るには SCC 追加が要る。 PyMC 追い越し済みの現状でここを攻める価値は要ユーザ判断。
- 勾配カーネル (68%・alloc 84%) のさらなる縮小余地 = dot の `VS.zipWith`+`VS.sum` 中間ベクトルを
  手動 fused ループ化・runST/freeze の再利用等。 **利得見込みは未計測**。

## ★Phase 54.7a カーネル割当の検証 + fused ループ化 — M2 PyMC 比 0.40× (2026-06-10)

**(a)-0 検証 (本当に割当があるかから)**: prof の alloc 84% は profiling ビルド計測で fusion 阻害の
産物の疑い → **通常ビルド** `+RTS -s` で実測 = **2.56GB** (prof 2.58GB と同等) → **割当は本物**。
per-grad ~48-82KB (素朴な期待 ~2KB の 20 倍超)。 原因 = `VS.generate` 内のリスト fold・毎呼出の
`zip`/`toList` 再構築・dot/sumR2 の中間ベクトル・scatter の `VU.convert`+`accumulate`。

**(a)-1 実装**: row-major 設計行列の前計算 (`cliXMat`) + 残差/sumR2 の 1 パス手動ループ
(`lmResidualS`・割当は r 1 本のみ) + dot/scatter の明示ループ + 値側は r を materialize せず
sumR2 のみ累積 (割当ゼロ)。

| 指標 | 54.6 | **54.7a** |
|---|--:|--:|
| 割当 (prof-nuts 同一ワークロード) | 2.56 GB | **0.49 GB** (×5.2 減) |
| wall (同) | 0.510 s | **0.253 s** (×2.0) |
| per-draw vla nG=8 / nG=32 (54a bench) | 0.627 / 2.74 ms | **0.396 / 1.149 ms** |
| scalar 比 (同) | ×22.8 / ×25.8 | **×44.6 / ×60.4** |
| scaling fit M2 per-draw | 0.283 ms (0.68×) | **0.166 ms (PyMC 比 0.40×)** |
| 固定費 | 186 ms | **103 ms** (PyMC 1745 の ~1/17) |

- ★Phase 54 累計 (M2): 5.894 → **0.166 ms/draw = 約 35.5× 高速化**・PyMC 比 14.1× → **0.40×**。
  posterior 不変 (beta_1 mean 0.826-0.828)・test 1019 green。
- M1 (per-obs fallback) は不変 (~30-41×・run 変動込み) — 54.8 の標的。
- 残り = (b) NUTS 本体の SCC 内訳 (54.7a 後の相対比重は再 prof で要確認)。

## ★Phase 54.7b NUTS 本体の SCC 内訳 → SPECIALIZE で RNG 系を解消 — M2 PyMC 比 0.25× (2026-06-10)

**SCC 配置 + prof (`prof-nuts-54.7b-pre.prof`・54.7a 時点・762 ticks)**: nutsStream/leapfrog 内に
明示 SCC を置いて初めて NUTS 本体 (~22%) の内訳が割れた:

| 経路 | %time | %alloc | entries | 中身 |
|---|--:|--:|--:|---|
| カーネル (compileGradUV+compileLogPUV) | 77.8 | 32.9 | — | 既知 (54.7a 済) |
| **nuts_sampleMomentum** | **5.8** | **13.3** | 1,300 | p=12 正規乱数 1 回 ~54KB alloc (異常) |
| **nuts_rng_uniform (4 site 計)** | **5.4** | 11.8 | ~29,400 | mwc uniform 1 回 ~µs 級 (異常) |
| leapfrog VS 更新 (vec_pos 等) | 4.1 | 13.2 | 25,342 | SCC で fusion 阻害された分を含む |
| nuts_buildTree self (bookkeeping) | 1.4 | 9.5 | 5,346 | NUTSTree record |
| nuts_uturn | 0.7 | 7.2 | 24,888 | delta 中間ベクトル |
| welford/dualavg/toConstrained/alphaProbe | ~0.5 | — | — | 無視できる (alphaProbe は warmup のみ lazy 評価) |

**真因 = Phase 50 の `PrimMonad m` 多相化に SPECIALIZE が無かったこと**: `nuts`/`nutsStream`/
`buildTree` が dictionary 渡しのまま走り、 mwc-random の `uniform`/`standard` (INLINE 前提の API)
が unbox されず boxed Double + クロージャ割当を毎回払っていた。 RNG 系 11.2%/alloc 25% は
mwc の素の速度 (~20-30ns/call) では説明不能な規模で、 これが手がかり。

**修正**: `{-# SPECIALIZE #-}` を IO / `ST s` の両具体型で 3 関数に追加 (コード変更なし・API 不変)。
`nutsPure`/`nutsChainsPure` (runST 経由) も ST 特殊化でカバー。

| 指標 | 54.7a | **54.7b** |
|---|--:|--:|
| prof: nuts_sampleMomentum + rng_uniform | 11.2% | **閾値以下に消滅** (NUTS 側 ~22%→~9%) |
| 割当 (prof-nuts 同一ワークロード・通常ビルド) | 0.49 GB | **0.337 GB** (×1.46 減) |
| wall (同・3 回中央値) | 0.253 s | **0.151 s** (×1.67) |
| per-draw vla nG=8 / nG=32 (54a bench) | 0.396 / 1.149 ms | **0.251 / 1.053 ms** (×1.58/×1.09) |
| scalar 比 (同) | ×44.6 / ×60.4 | **×53.0 / ×66.5** |
| scaling fit M2 per-draw (R²=0.999) | 0.166 ms (0.40×) | **0.106 ms (PyMC 比 0.25×・約 3.9 倍速)** |
| 固定費 | 103 ms | **73 ms** (PyMC 1745 の ~1/24) |

- ★Phase 54 累計 (M2): 5.894 → **0.106 ms/draw = 約 55.7× 高速化**・PyMC 比 14.1× → **0.25×**。
  posterior 不変 (beta_1 mean 0.824-0.832・relErr ≤ 5e-7)。 ⚠PyMC 値は既存 CSV (cross-session)。
- 小規模ほど効く (nG=8 ×1.58 vs nG=32 ×1.09) — NUTS 本体の相対比重が大きい側から削れた形。
- M1 (per-obs fallback) fit = 1.268 ms = **30.2× のまま** (54.8 の標的・不変)。
- 残り (prof 実測・SPECIALIZE 後): NUTS 側 ~9% = leapfrog VS 更新 (vec_pos alloc 16.2%)・
  uturn delta 中間ベクトル (alloc 10.8%)・buildTree record。 ⚠SCC 自体が fusion を阻害するため
  prof の alloc% は過大の可能性 (通常ビルドでは SCC 無効)。 さらに攻めるかは要ユーザ判断。
- ★教訓: **`PrimMonad m` 多相化 (Phase 50) は SPECIALIZE とセットが必須**。 多相 RNG ホットループは
  dictionary 渡しで mwc が boxed 化し、 通常 bench では「そういう速度」 として見過ごされる
  (SCC 内訳で初めて異常規模と判明)。

## ★Phase 54.8 per-obs 手書きの自動 ObserveLM 化 — M1 PyMC 比 30.2× → 0.40× (2026-06-10)

**背景**: 高速経路 (54.4-54.7) は構造化観測 (`observeLM` 系) が条件で、 per-obs scalar
`observe` 手書きの M1 は walk + `ad` の fallback のまま 30.2× だった (54.7b 時点も実測・不変)。

**実装** (`synthGaussLMBlocks`・commit 8d0ed9a): affine 追跡 interpreter (`AffV` =
AffC 定数 / AffL 係数Map+offset / NA・非線形演算で NA 化) を `Sample` 継続に給餌して walk し、
scalar `Observe (Normal μ σ)` の μ=affine・σ=単一 latent の行を Gaussian LM ブロックに自動合成。
係数常 1 + prior `Normal(0,τ)` 共有 + 各行ちょうど 1 つの latent 族は one-hot→`REff` gather に
昇格 (dense one-hot は O(nG·n) 逆効果 — 54.4a 計測)。 定数 offset は ys に畳む。 安全網 2 段 =
① 非定数比較を error poison → try/force 捕捉で fallback (値依存分岐の誤抽出防止)
② probe 2 点 (per-param 値) で吸収 Observe の walk 評価と突合。 統合点 = `gaussLMBlocksAuto` →
`analyzeGaussModel` (吸収済 Observe 名を除外集合へ・公開 API/authoring 不変)。

### bench-hbm-scaling 線形フィット + PyMC 突合 (iter グリッド 50-1600・5 reps)

| モデル | 指標 | 54.7b (合成前) | **54.8 (自動合成)** | PyMC | HS/PyMC |
|---|---|--:|--:|--:|--:|
| M1_pooled | per-draw fit | 1.268 ms | **0.0169 ms (×75)** | 0.042 ms | **0.40×** |
| M1_pooled | 固定費 (R²=0.995) | 928 ms | **12 ms** | 1273 ms | ~1/106 |
| M2_ranint | per-draw fit (R²=0.9997) | 0.106 ms | **0.098 ms** (対象外・微改善) | 0.417 ms | **0.24×** |

- ★**M1 = per-draw 75× 高速化・PyMC 比 30.2× → 0.40× (約 2.5 倍速)**。 手書き per-obs モデルが
  そのまま (書き換えなしで) M2 と同じ解析閉形式カーネルに乗った。
- posterior 品質不変: M1 iter1600 で b mean 1.4299→1.4303・ESS 1600/1600・accept 0.949→0.954。
  M2 beta_1 mean 0.825-0.832 (従来同値域)。 test 1022 (新規 3 込)・fail は既知 Phase 38 stale 6 のみ。
- M1 改善幅 (75×) が M2 の scalar→vec 比 (×53-66) より大きいのは、 M1 は p=2・nG=0 で
  「walk + per-obs スカラ logDensityObs + ad tape」 の全部が固定費的に消えるため。
- ⚠PyMC 値は既存 CSV (cross-session)。 同一マシン・同一 config だが同時実行ではない。

## ★M3-M6 拡張 + PyMC 同一セッション再突合 — 「汎用ではまだ勝てない」を実測で確定 (2026-06-10)

ユーザ問い「まだ汎用的には勝てませんよね？」 に答えるため、 計画当初の M3-M6 を harness に追加し
**M1-M6 全部を HS/PyMC とも同一セッションで fresh 再計測** (旧 cross-session の注意を解消)。
M3-M6 は全て **per-obs scalar observe の手書き** (汎用 authoring) で組み、 54.8 自動合成が
乗るか fallback するかをそのまま反映させる:

- **M3** 階層 ranint+slope `y ~ N(β0+β1·x+u_g+v_g·x, σ)` (n=96, 8群, latent 21):
  u_g は係数 1 → REff gather 化、 **v_g は係数 x → dense 列 + v-prior が residual ad walk に残る** (中間)
- **M4** 多変量 X pooled (n=200, p=10+intercept, latent 12): 全 affine → 完全解析経路
- **M5** パラメタ非線形 `y ~ N(a·exp(-b·x)+c, σ)` (n=100, latent 4): **非 affine → fallback**
- **M6** 階層×非線形 `y ~ N(a_g·exp(-b·x), σ)` (n=96, latent 12): **fallback**

### 線形フィット per-draw (iter 50-1600・HS 5 reps / PyMC 3 reps median・同一 CSV データ)

| モデル | HS/draw | HS 固定費 | HS R² | PyMC/draw | PyMC 固定費 | PyMC R² | **HS/PyMC** |
|---|--:|--:|--:|--:|--:|--:|--:|
| M1_pooled | 0.0159 ms | 11 ms | 0.988 | 0.209 ms† | 1194 ms | 0.74 | **0.08×** |
| M2_ranint | 0.103 ms | 66 ms | 0.998 | 0.644 ms | 2045 ms | 0.96 | **0.16×** |
| M3_ranslope | 2.52 ms | 1361 ms | 0.956 | 0.833 ms | 2821 ms | 0.92 | **3.0×** |
| M4_multix | 0.101 ms | 70 ms | 0.999 | 0.368 ms | 2021 ms | 0.99 | **0.28×** |
| M5_nonlin | 3.59 ms | 2456 ms | 1.000 | 0.134 ms† | 1938 ms | 0.13 | **≳27×** |
| M6_hier_nonlin | 2.46 ms | 1968 ms | 1.000 | 0.546 ms† | 1823 ms | 0.75 | **4.5×** |

†PyMC は固定費 (compile+tune ~2s) 支配で per-draw が計測ノイズに埋もれる (R² 低)。 当該行の
HS/PyMC 比は有効数字が乏しい (M5 の 27× は「PyMC の draw はほぼ無料」 との比で、 下限側の目安)。

- posterior mean は全 6 モデルで HS=PyMC 一致 (iter1600): M3 beta_1 0.952/0.950・M4 beta_1
  1.230/1.229・M5 b 1.167/1.160・M6 b 1.034/1.031。 計算の正しさは両エンジンで同等。
- ⚠同一セッション再計測により M1/M2 の PyMC per-draw は旧 CSV (0.042/0.417) から 0.209/0.644 に
  変動 (run-to-run 変動・固定費支配域の fit 不安定)。 比較は本表 (同一セッション) を正とする。

### 結論 (事実)

1. **affine 構造 (M1/M2/M4) は PyMC より 3.6-12× 速い** — 54.8 自動合成 + 解析閉形式カーネルが
   per-obs 手書きでも機能。
2. **汎用 (非 affine) ではまだ勝てない**: M5 (パラメタ非線形) ≳27×・M6 (階層×非線形) 4.5×・
   M3 (random slope) 3.0× 負け。 PyMC は任意グラフを compile+vectorize するのに対し、 HS の
   高速経路は Gaussian-恒等リンク + affine μ に限定されるため。
3. 負け方の内訳 (実測ベースの構造理解・要 prof 確認):
   - M3: v_g (係数 x) は one-hot 検出外 → v-prior 8 個が residual **ad walk** に残る + dense
     8 列のカーネル代。 随伴の拡張候補 = **係数付き gather (random slope の REff 化)** +
     階層 prior の解析勾配一般化。
   - M5/M6: 非 affine μ は合成対象外 → **全体が walk+ad fallback** (Phase 53 時点の実行モデル)。
     拡張候補 = 非線形項の vec-tape 化 (54.3 案B の汎用化) か μ の部分 affine 分解。
4. **次の標的は M5/M6 系 (非線形 μ の高速化)** — 絶対値も大きい (3.6/2.5 ms/draw)。 着手前に
   prof で内訳確定が必要 (推測するな計測せよ)。

## ★Phase 54.9 非 affine 系 (M3/M5/M6) の cost-centre profile — 支配項はモデル別に異なる (2026-06-10)

`prof-nuts` を M2/M3/M5/M6 引数選択に拡張 (モデル定義・DGP・seed は `BenchHBMScaling.hs` と
同一・warmup 500 + 800 draws・seed 42)。 生 prof = `prof-nuts-54.9-m{3,5,6}.prof` (gitignore・
ローカルのみ)。 posterior probe 値は通常/prof ビルドでビット一致 (決定性確認)。

### 全体 (通常ビルド `+RTS -s`・無負荷で再計測・alloc は 2 回の実行で完全一致)

| モデル | total | alloc | M2 比 | GC% | prof ビルド total (倍率) |
|---|--:|--:|--:|--:|--:|
| M2_ranint (高速経路・参照) | 0.199s | 0.337 GB | — | ~3% | — |
| M3_ranslope (中間) | 3.95s | 18.1 GB | ×20 | <2% | 8.30s (×2.1) |
| M5_nonlin (fallback 本命) | 6.62s | 32.7 GB | ×33 | <3% | 14.54s (×2.2) |
| M6_hier_nonlin (fallback) | 4.24s | 19.5 GB | ×21 | <3% | 7.77s (×1.8) |

GC は全モデル 3% 以下 = **MUT (純計算+割当) 支配**。 prof ビルドの alloc (excludes overheads) は
通常ビルドと ~1% 以内で一致 → SCC の fusion 阻害による alloc 歪みは無し (54.7b の罠は今回は非該当)。

### M5 (本命・prof 14538 ticks): 支配 = **仮説③ per-obs スカラ密度 (AD 上) ~52%**

経路配分: **勾配 (全体 `ad` fallback) 94.1% / 値評価 (Double walk) 5.2% / NUTS 本体 <1%**。
勾配経路の内訳 (leapfrog grad1+grad2 合算・%は total 比):

| 項目 | %time | %alloc | 仮説 | 備考 |
|---|--:|--:|---|---|
| `logDensityObs` (per-obs スカラ Normal 密度・AD 上) | **47.8** | 45.2 | ③ | entries 2,036,500/grad 経路 = 20,365 grad × 100 obs |
| `obsLogSum` self | 4.4 | 4.6 | ③ | per-obs 和の plumbing |
| `m5Model` walk (μ=a·exp(-b·x)+c の AD 演算 + Free 構築) | **20.0** | 24.8 | ①+② | getTape/primal 8.1M 回 — 大半は AD スカラ演算 (Free 構築 self は `observe`+`liftF` ~0.5% と小) |
| `logJoint` self (walk 駆動) | 5.2 | 5.8 | ① | |
| `logDensity` (prior 4 個) | 2.6 | 1.1 | — | |
| `partials` (backward pass) | 9.6 | 8.4 | ② | |
| `bind`/`reifyTypeable` self 等 (tape 管理) | ~2.4 | ~1 | ② | |

- grad 評価 40,730 回 (= 20,365 leapfrog × 2)・1,300 trajectory → 通常ビルド換算
  per-grad ≈ **0.153ms**・per-obs ≈ 1.5µs (スカラ密度+μ の AD 1 点分)。
- **判定: ③ (per-obs スカラ密度) が単独最大 ~52%。 ただし ②(tape 管理+backward ~12%) と
  ①②混合の μ AD 演算 ~25% を合わせると「per-obs スカラ AD」 への帰着が ~90%** —
  密度だけベクトル化しても上限 ×2.1 (Amdahl)。 仮説④ (NUTS) は <1% で棄却。

### M3 (中間・prof 8302 ticks): 支配 = **仮説① Free walk 再構築 ~31% (alloc ~60%)**

経路配分: **勾配 75.8% / 値評価 21.8% / NUTS ~1.5%**。 内訳 (%は total 比):

| 項目 | %time | %alloc | 仮説 | 備考 |
|---|--:|--:|---|---|
| 残差 `ad` (fExcl・reifyTypeableTape 配下) 計 | **48.3** | 63.6 | — | v-prior 8 個のために**モデル全体を毎 grad walk** |
| ├ `m3Model` Free walk 再構築 (AD 上) | **21.1** | 42.2 | ① | 96 Observe ノード + `T.pack` 名 + `us!!g`/`vs!!g` を毎回再構築 (密度は excl で足さないのに walk 代だけ残る) |
| ├ `logDensity` (v-prior 8 + tau_v 等スカラ密度) | 10.7 | 10.5 | ③ | |
| ├ `reifyTypeable` self (tape 初期化・毎 grad) | 13.0 | 7.2 | ② | |
| └ `partials` (backward) | 1.9 | 2.6 | ② | |
| `compileGradUV` self (dense 8 列 + β カーネル) | 20.8 | 5.8 | — | v_g が one-hot 検出外 → dense 列 (gather 化の余地) |
| 値評価: `compileLogPUV` self | 11.5 | 1.5 | — | |
| 値評価: `m3Model` Double walk (residual 値) | 9.6 | 19.4 | ① | 同じ walk 再構築が値側にも |
| NUTS 本体 | ~1.5 | ~0.5 | ④ | 棄却 |

- grad 評価 69,102 回 (= 34,551 × 2)。 値評価 (energy) 35,851 回。
- **判定: ① (walk 再構築) が最大 30.7% (grad 21.1 + 値 9.6)・alloc ~60%**。 v-prior 8 個の
  密度のためだけに 96 観測ノード込みの AST を毎回 (grad 69k + 値 36k 回) 組み直している。

### M6 (差分確認のみ・prof 7773 ticks): **M5 と同型を確認**

top: `logDensityObs` 43.9% + `m6Model` 19.2% + `partials` 8.1% + `logJoint` 9.2% — M5 と同構成。
差分 = 階層 prior (a_g×8): `logDensity` 5.9% (M5 は 2.5%)。 → **54.11 の設計は M5 で代表させ、
階層 prior のスカラ密度も同経路に乗せる必要がある** ことだけ追加要件。

### Amdahl 利得上限 → 54.10/54.11 の含意

| 候補 | 標的 (実測) | 除去上限 | 速度上限 | PyMC 比の見込み |
|---|---|--:|--:|---|
| **54.10 REff 重み付き gather (M3)** | 残差 ad 48.3% + 値側 walk 9.6% (= residual 完全消滅で noResid 経路) + dense→gather で `compileGradUV` self の一部 | ~58-65% | **×2.4-2.9** | M3 3.0× → **~1.0-1.3×** (固定費 1361ms も walk 由来分が縮む見込み・要再計測) |
| **54.11-② vec-tape (ベクトル IR) 化 (M5/M6)** | 勾配経路 ~94% 全体 (per-obs スカラ AD の総体) | 理論上 ~94% | 大 (spike で実測要) | 54.3 spike で `ad` 比 ×5.3-24.1 の実績。 ゲート ≥3× |
| 54.11-③ 観測密度のベクトル化のみ | logDensityObs+obsLogSum ~52% | ~52% | **×2.1** | M5 ≳27× → ≳13× で**不足** |
| 54.11-① walk スケルトンキャッシュのみ | M5 の Free 構築 self ~0.5%・M3 の walk 31% | M5 では効果僅少 | M5 ×1.0 | M3 には効くが 54.10 が同領域をより深く解決 |

**結論 (実測ベース)**: ④NUTS は全モデル <2% で棄却。 M3 と M5/M6 は**負け方が別物** —
M3 = ①walk 再構築 (54.10 の REff 重み化で residual 自体を消すのが直撃)、 M5/M6 = ③per-obs
スカラ密度を筆頭に勾配経路全体が per-obs スカラ AD (54.11 は ②ベクトル IR 路線でないと
上限 ×2.1 で頭打ち)。 実装の優先順位・着手可否はユーザ判断待ち。

## ★Phase 54.10 係数付き gather (REff 重み) — M3 per-draw 2.52→0.258ms・PyMC 比 0.31× (2026-06-10)

ユーザ判断 (2026-06-10) = 「54.10 → 54.11 spike を連続実行」。 54.10 = random slope の REff 化:

- **`REff` に per-row 重みスロット** (`Maybe [Double]`・`Nothing` = 全 1 = 後方互換):
  @η_i += w_i·u^{re}[gid_i]@。 重みは汎用 walk ('lmReffEta')・コンパイル済カーネル
  ('lmResidualS'/'gradLMBlockIx' scatter = Σ w_i·r_i/σ²・'valueLMBlockIx') の両経路対応。
  prior 解析勾配 ('gradReffPriorIx') は @u_j ~ N(0,τ)@ 同形のため**無変更で再利用**。
- **54.8 synth 族検出の一般化**: 「係数常 1」 を撤廃 → 「prior @Normal(0,τ)@ 共有 + 各行に
  ちょうど 1 つ」 (係数任意・係数列を重みとして抽出・全 1 なら `Nothing`)。 これで M3 の
  v_g (係数 x_i) も REff gather に昇格し、 **residual ad walk が完全消滅** (noResid 経路 =
  ad クロージャ・値側 walk とも省略)。 probe/poison 安全網は既存流用。
- test: 新 2 件 (M3 形 u/v 二重族の REff 2 本化・期待重み一致 + 明示重み付き observeLMR の
  per-obs 手書きとの値/勾配一致)。 全 1024 中 fail = 既知 Phase 38 stale 6 のみ (新規ゼロ)。

### 実測

| 指標 | 54.9 時点 | **54.10** | 倍率 |
|---|--:|--:|--:|
| prof-nuts m3 (warmup500+800・同一セッション A/B) | 3.95s | **0.587s** | ×6.7 |
| 同 alloc (決定的) | 18.1 GB | **0.78 GB** | ×23 |
| bench fit M3 per-draw | 2.52 ms | **0.258 ms** | ×9.8† |
| 同 固定費 | 1361 ms | **214 ms** | ×6.4† |
| **M3 HS/PyMC 比** | 3.0× (負け) | **0.31× (追い越し)** | — |

†bench fit は cross-session (本日は環境が全体に ~×1.4 遅め: M2 0.103→0.157ms 等)。
同一セッション A/B は prof-nuts 行 (×6.7)・per-draw 改善は M2/M4 との同日比でも
M3 0.258 vs M2 0.157/M4 0.133 と affine 系と同オーダーに到達。 PyMC 比 0.31× は
遅め環境込みでも追い越しを確定できる保守側の数字。

- 非回帰: M1/M2/M4/M5/M6 は **alloc 完全一致** (経路不変)・posterior mean@1600 も前回と一致
  (M1 1.4303 / M2 0.8282 / M4 1.2302 / M5 1.1668 / M6 1.0337)。
- M3 posterior: beta_1 mean@1600 = 0.969 (PyMC 0.950・ESS 707)。 logp/勾配は従来 walk と
  1e-9・中心差分 1e-4 一致 (test 担保) = target 同一・差は FP 順序由来の MC 変動。
- 副次効果: 単独 latent でも prior @Normal(0,τ)@ + 任意係数で全行 1 回現れれば族化される
  (例: ridge 風 @b ~ N(0,τ); y_i ~ N(b·x_i, σ)@ → b の prior も解析経路へ)。

## ★Phase 54.11 spike: 非線形 μ の vec-tape — ゲート GREEN (M5 ×18.3 / M6 ×13.5) (2026-06-10)

54.9 で確定した設計②「ベクトル式 IR」 の feasibility を、 本実装 (IR 追跡 interpreter) の前に
**手組み vec-tape** で実測 (`bench-hbm-vecir` = `BenchHBMVecIRSpike.hs`)。 VecAD に elementwise
op 3 つを追加 (`vexpHR`/`bcastAddHR`/`hadamardHR`) し、 M5/M6 の **unconstrained 全勾配**
(観測尤度 + 全 prior + jacobian) を tape per-call 構築込みで計算:

| モデル | (a) RevD 直書き | (a') gradADU 実経路 | (b) vec-tape 手組み | **(a')/(b)** | (a)/(b) |
|---|--:|--:|--:|--:|--:|
| M5 (n=100, θ=4) | 0.1012 ms | 0.1210 ms | **0.0066 ms** | **×18.3** | ×15.3 |
| M6 (n=96, nG=8, θ=12) | 0.0991 ms | 0.1275 ms | **0.0095 ms** | **×13.5** | ×10.5 |

- 検証: vec-tape ≡ RevD ≡ gradADU (1e-9 相対) ≡ 中心差分 (1e-4)・M5/M6 とも全 pass。
- **ゲート (実経路比 ≥3×) は GREEN** — 大幅超過。 M6 (gather + hadamard + 階層 prior の
  ベクトル化) も同水準で乗ることを確認 = 本実装の設計リスク (M6 要件) も解消。
- ⚠ (b) は手組み = IR 追跡 interpreter (Free walk から式を持ち上げる層) のオーバヘッドを
  含まない**楽観側の上限**。 walk は構築 1 回 + draw 間再利用にできる (54.4b 前例) ため
  per-draw では近い値が狙えるが、 本実装後に再計測で確定する。 「PyMC 同等」 とは言わない。
- 粗い見込み (Amdahl・54.9 内訳): M5 per-draw の勾配経路 94% が ×18 なら per-draw ~×9
  (3.59→~0.4ms/draw)。 PyMC の M5 per-draw (0.134ms・R²=0.13 の下限目安) との比較は
  本実装後の実測で。

## ★Phase 54.11 本実装: ベクトル式 IR — M5 3.59→0.296ms (×12.1)・M6 2.46→0.274ms (×9.0) (2026-06-11)

spike ゲート GREEN を受けて本実装 (commit 0e6533f)。 AffV (54.8 affine 限定) の代役として
**スカラ式 IR (`SExp`) を Sample 継続に給餌する追跡 interpreter** で per-obs scalar
`Observe (Normal μ σ)` の μ 式 (非線形可) を行ごとに収集し、 行間で式の形が同型なら
定数 leaf を列ベクトルに束ねて「ベクトル式 IR (`UExp`)」 へ持ち上げる (μ⃗ = f(θ, x⃗))。
階層 prior (a_g ~ Normal(m,τ) の構造同一族) も gather + ベクトル化密度で同 IR に乗せる
(M6 要件)。 評価は VecAD vector-op tape (勾配・per-call 構築) / 素 Double ベクトル演算 (値)。
IR 持ち上げ + 静的解析 (`synthVecIR`/`compileVecIR`) は compile 時 1 回・draw 間再利用。
統合点 = `compileGradUV`/`compileLogPUV` の全体 ad fallback の手前 (affine 経路優先・
公開 API/authoring 不変)。 安全網 = 54.8 同様の poison (値依存分岐) + probe 2 点突合。

### per-call A/B (同一セッション・`bench-hbm-vecir`: 実経路 gradADU が IR 化された後)

| モデル | (a') gradADU (=IR 経路) | (b) vec-tape 手組み (spike 上限) | (a')/(b) |
|---|--:|--:|--:|
| M5 | **0.0063 ms** | 0.0066 ms | ×1.0 (上限到達) |
| M6 | **0.0093 ms** | 0.0125 ms | ×0.7 (手組みより速い) |

- 検証: RevD ≡ gradADU ≡ vec-tape (1e-9) ≡ 中心差分 — 全 pass。
- spike 時の gradADU (walk+ad fallback) は M5 0.121 / M6 0.128 ms → per-call **×19 / ×14**。
- IR interpreter 層のオーバヘッドは観測されず (持ち上げは compile 時 1 回ゆえ per-call はゼロ)。
  M6 で手組みより速いのは IR の族 prior 構成がノード数で僅かに少ないため (broadcast 1 段省略)。

### per-draw 線形フィット (iter 50-1600・HS fresh run vs 既存 PyMC CSV = ⚠cross-session)

| モデル | HS/draw (54.10 時) | HS/draw (54.11) | HS 固定費 | HS R² | PyMC/draw | **HS/PyMC** |
|---|--:|--:|--:|--:|--:|--:|
| M5_nonlin | 3.59 ms | **0.296 ms (×12.1)** | 2456→197 ms | 1.000 | 0.134 ms† | ≳27× → **2.2×†** |
| M6_hier_nonlin | 2.46 ms | **0.274 ms (×9.0)** | 1968→211 ms | 0.999 | 0.546 ms† | 4.5× → **0.50×†** |

†PyMC per-draw は固定費支配で R² 低 (M5 0.13 / M6 0.75) = 比は**下限目安** (M5 の 2.2× は
「PyMC の draw がほぼ無料」 との比)。 加えて HS は fresh run・PyMC は前セッション CSV の
cross-session 比較。 「PyMC 同等」 とは言わない。

- **非回帰 (chain ビット一致)**: M1-M4 は acc_main@1600 が前回 CSV と完全一致
  (M1 1.430347 / M2 0.828236 / M3 0.969208 / M4 1.230157) = affine 経路は不変。
  時間列の差 (M1 47→36ms 等) は run-to-run 変動。
- M5/M6 は FP 和順序が変わり chain は別物だが posterior は一致: M5 b 1.1606 (PyMC 1.160・
  ESS@1600 1031)・M6 b 1.0301 (PyMC 1.031・ESS@1600 1345)。 logp/勾配は従来 ad と 1e-9・
  中心差分と 1e-4 一致 (test 4 件で担保)。
- test: 全 1028 中 fail = 既知 Phase 38 stale 6 のみ (新規ゼロ)。
- 残課題: M5 の PyMC 比 2.2×† は PyMC 側の per-draw が計測ノイズに埋もれており確定できない。
  これ以上詰めるなら PyMC 側を iter を増やした同一セッション再突合で per-draw を出し直すのが先。

## ★M5 iter 延長 PyMC 再突合 — per-draw 確定: HS/PyMC = **0.52× (追い越し)** (2026-06-11)

54.11 の残課題 (M5 の PyMC 比 2.2׆ は PyMC per-draw が固定費に埋もれ R²=0.13 で不定) を
解消するため、 **iter を 25600 まで延長した同一セッション突合**を実施 (ユーザ指示)。
grid = [400, 800, 1600, 3200, 6400, 12800, 25600]・warmup 500 固定・データ/モデル/seed は
従来と同一。 実行体 = `bench-hbm-scaling m5-long` (HS) / `bench_hbm_m5_long.py` (PyMC)、
結果 CSV = `hbm_scaling_m5_long.csv` (両 system・通常 bench とは別ファイル)。

| | per-draw | 固定費 | R² |
|---|--:|--:|--:|
| hanalyze NUTS (54.11 IR 経路) | **0.246 ms** | 131 ms | 0.999 |
| PyMC | **0.476 ms** | 1431 ms | **0.9999** |

→ **HS/PyMC = 0.52× (M5 も追い越し)**。 短 grid で見えていた「PyMC 0.134ms/draw」 は
固定費支配域の fit アーティファクトで、 draw 部分を固定費より大きくすると傾きは 0.476ms に
確定 (R² 0.13 → 0.9999)。 54.11 時点の「2.2׆」 表記は過大 (PyMC に甘い側) だった。

- posterior 一致: b mean@25600 = HS 1.1605 / PyMC 1.161。
- HS の per-draw 0.246ms は短 grid fit (0.296ms) と整合 (長 grid で漸近単価に寄る)。
- これで **M1-M6 全モデルで PyMC 追い越し**: M1 0.08× / M2 0.23× / M3 0.30× / M4 0.33× /
  **M5 0.52×** / M6 0.50׆ (M6 の † は短 grid のまま。 M5 の結果から PyMC 側 0.546ms/draw
  (R² 0.75) は概ね真値近傍とみられるが、 確定させる場合は同じ long grid 突合で足りる)。

## ★Phase 55.1 GLM 系 (M7/M8) baseline + cost-centre profile — 支配項は per-obs スカラ AD で M5/M6 同型 (2026-06-11)

Phase 55 (HBM 高速経路のカバレッジ拡張) のゲート計測。 bench / prof を非 Gaussian
観測の 2 モデルに拡張 (どちらも per-obs scalar observe の手書き = 高速経路対象外):

- **M7_pois**: y_i ~ Poisson(exp(a + b·x_i)) (n=100・log link・seed 71/72)
- **M8_logit**: y_i ~ Bernoulli(invLogit(a + b·x_i)) (n=100・logit link・seed 81/82)

データ = `hbm_m{7,8}.csv` (PyMC と共有)。 実行体 = `bench-hbm-scaling glm` /
`m7-long` / `m8-long` (HS)、 `bench_hbm_scaling.py glm` / `m7-long` / `m8-long`
(PyMC・venv `~/.virtualenvs/pymc`)。 M5 の教訓どおり **最初から long grid
(400-25600) を併用**し、 短 grid (50-1600) の傾きでは断言しない。

### per-draw 線形フィット (改善前 baseline・同一セッション突合)

| モデル | grid | HS per-draw | HS 固定費 | HS R² | PyMC per-draw | PyMC 固定費 | PyMC R² | HS/PyMC |
|---|---|--:|--:|--:|--:|--:|--:|--:|
| M7_pois | short | 0.814 ms | 482 ms | 0.9995 | 0.174 ms | 797 ms | 0.69 | (4.7׆) |
| M7_pois | **long** | **0.842 ms** | 467 ms | 0.9997 | **0.160 ms** | 772 ms | 0.9996 | **5.27×** |
| M8_logit | short | 0.784 ms | 503 ms | 0.9963 | 0.149 ms | 843 ms | 0.988 | (5.3×) |
| M8_logit | **long** | **0.761 ms** | 517 ms | 0.9999 | **0.160 ms** | 860 ms | 0.9991 | **4.77×** |

- **確定 baseline (long grid・R² > 0.999 両系): M7 = 5.27× / M8 = 4.77× (HS が遅い)**。
- posterior 一致: M7 b = HS 0.7879 / PyMC 0.788 (真値 0.8)、 M8 b = HS 1.2834 /
  PyMC 1.286 (真値 1.2)。 ESS@25600 = HS 19.9k/20.6k vs PyMC 13.9k/19.5k。

### cost-centre profile (`prof-nuts m7`/`m8`・warmup 500 + 800 draws・seed 42)

全体 (vanilla `+RTS -s`): M7 total 1.06s・alloc 6.48 GB / M8 0.99s・6.84 GB、
GC ~2% → **MUT (純計算+割当) 支配**。 prof ビルド total はともに ~2.1s (×2.0)。
posterior probe 値は vanilla/prof ビルドでビット一致 (決定性確認)。 生 prof =
`prof-nuts-55.1-m{7,8}.prof` (gitignore・ローカルのみ)。

経路配分: **勾配 (compileGradUV → 全体 `ad` fallback) = M7 88.5% / M8 89.7%**、
値評価 (compileLogPUV) = M7 10.2% / M8 9.1%、 NUTS 本体 ~1%。

| 項目 (個別 %time) | M7 | M8 | 備考 |
|---|--:|--:|---|
| `logDensityObs` (per-obs 離散密度・AD 上) | **43.1** | 18.4 | entries 1,921,000 / 1,997,200 (= walk 回数 × 100 obs) |
| `m{7,8}Model` walk (link 込み μ の AD 演算 + Free 再構築) | 26.1 | **48.6** | M8 は invLogit (exp+除算/obs) が密度より重い |
| `logJoint` self (walk 駆動) | 8.0 | 7.7 | |
| `partials` (backward pass) | 7.3 | 9.3 | |
| `obsLogSum` self | 6.9 | 7.1 | |
| `logDensity` (prior 2 個) | 2.3 | 2.1 | |
| `reifyTypeable` + `bind` (tape 管理) | ~2.8 | ~2.6 | |

### 判定 (55.1 ゲート → 55.4 設計の確定)

- **M5/M6 (54.9) と同型の「per-obs スカラ AD 帰着 ~90%」 を確認** → 55.4
  (ベクトル式 IR に分布別観測密度ノードを追加) は設計どおり進められる。
- M7 と M8 で内訳の重心は違う (M7 = 密度 43% 筆頭 / M8 = link の AD 演算 49% 筆頭)
  が、 54.11 の IR は η⃗ (link 込みの式全体) を列演算化するためどちらも吸収対象。
  「観測密度のみのベクトル化では不足 (Amdahl 上限 ×2.1)」 という 54.9 M5 の結論とも
  整合 (M8 では密度のみだと上限 ×1.2 程度でさらに不足)。
- logFactorial (Poisson の Σlog y!) は prof 上で独立項として現れない (logDensityObs
  に内包・y 定数ゆえ 55.4 では compile 時前計算に逃がす設計で問題なし)。

## ★Phase 55.2+55.3 IR カバレッジ拡張 — 式形混在サブグループ化 + σ 式/heteroscedastic (2026-06-11)

- **55.2**: `synthVecIRWalk` のグループキーを (σ名) → (σ名, μ式形指紋 `sexpShape`) に。
  同一 σ 下で式形が混在しても形ごとに独立吸収 (従来は unifyMany 失敗で σ グループ
  丸ごと residual 落ち)。 σ 共有グループの随伴は tape 上で自然に加算 (下流変更なし)。
- **55.3**: `collectSymRows` の σ 位置を「単一 latent」 → 任意 SExp に拡張。 σ 側の
  グループキーは名前付き指紋 (`sexpKeyNamed`・σ leaf の族 gather 化はしない保守設計)。
  スカラ σ 式 (例 `2*s`) は従来密度のまま、 行依存 σ⃗ (heteroscedastic・例
  `exp(g0+g1·z_i)`) はベクトル版密度 -Σlogσ_i - Σr_i²/(2σ_i²) を値/tape 両方に追加。
  σ = 単一 latent ('RUV') の場合は tape ノード追加ゼロ = 54.11 と同一 tape。

### per-call 勾配 A/B (`bench-hbm-het`・mHet: y_i ~ N(a, exp(g0+g1·z_i))・n=100・θ=3)

| 経路 | ms/call | 比 |
|---|--:|--:|
| gradADU (55.3 後 = IR 吸収・実経路) | **0.0106** | — |
| RevD walk (旧 fallback 相当・全体 ad) | 0.0998 | **×9.4** |

relErr = 3.9e-16 (数値一致)。 per-draw への波及と M 系 bench への heteroscedastic
追加可否は 55.5 で判断。 test: 1031 examples・fail = 既知 Phase 38 stale 6 のみ
(新規 4 test: 形混在×2 / 定数倍 σ / heteroscedastic)。

## ★Phase 56.2 IR 記号微分化 — per-call ×1.4-2.0 改善 + 旧 tape の NaN 潜在バグ修正 (2026-06-11)

観測密度を family 別の手書き tape ノード → **IR 式 (densityIR) + compile 時
記号 reverse-mode** に置換。 命令列 (SSA/ANF・構造 CSE = RUExp の Eq/Ord で
hash-consing) を compile 時 1 回生成し、 per-call は 1 本の unboxed arena 上の
forward/backward 実行のみ (boxed 中間表現なし・VecAD per-call tape 撤去)。

### per-call 勾配 A/B (同一マシン・直前後)

| kernel | 旧 tape (54.11-55.4) | 56.2 arena+CSE | 比 |
|---|--:|--:|--:|
| het (heteroscedastic n=100) | 0.0128 ms | **0.0060-0.0067** | **×1.9-2.1** |
| M5 (非線形 μ n=100) | 0.0064 ms | **0.0045** | ×1.4 |
| M6 (階層×非線形 n=96) | 0.0089 ms | **0.0045** | ×2.0 |

改善の主因 = **CSE** (guard 式と `let t = r/σ in t*t` 等の共有を slot 単位で
実体化・旧 tape は per-call 構築ゆえ共有が毎回再演算) + per-call 構築コストの消滅。
⚠実装過程の教訓: ① boxed UVal 中間表現は tape 比 ×1.7-2 劣化 (Maybe/箱/即値
ベクトル確保)、 ② arena でも `forM_ [0..n-1]` は fusion されず劣化 — **手動再帰
+ bang + op 特殊化ループ**が必須 (どちらも計測で確定してから書き直した)。

### ★旧 tape の潜在バグ発見・修正 (M8 で顕在化)

bench M8 再計測で per-draw 1.27ms・固定費 4.2s の激烈退行 → 広域勾配突合で
真因確定: **invLogit の FP 飽和 (η > ~37 で p == 1.0) 行で、 walk 経路は guard が
行を定数 -∞ にして勾配 0 (他行の勾配は有効) を返すのに対し、 IR 経路は勾配側
unguarded で log(0) = -∞ → NaN が全勾配を汚染** → NUTS の u-turn 判定が常に
false になり max-depth 迷走。 **旧 tape (54.11-55.4) も同罪の潜在バグ** (chain の
FP 経路が変わって初めて踏んだ)。 修正 = guard 違反 call のみ walk+ad 勾配へ
per-call fallback ('constPriorGradD' の「guard 違反 = ad と一致」 原則の観測版・
境界点のみの稀ケースで per-draw 影響なし)。 修正後は境界域含め walk と勾配一致。

### per-draw (bench-hbm-scaling glm・短 grid・修正後)

| モデル | 55.5 (tape) | 56.2 | |
|---|--:|--:|--:|
| M7_pois | 0.0938 ms | **0.0759 ms** | ×1.24 |
| M8_logit | 0.1519 ms | **0.1115 ms** | ×1.36 |

posterior 不変 (M7 b 0.7857 / M8 b 1.2830)。 PyMC 比の確定は 56.6 の long grid
突合で行う (短 grid の傾きで断言しない)。 test 1036・fail = 既知 6 のみ。

## ★Phase 55.4+55.5 非 Gaussian 観測の IR 化 + 再計測 — M7 0.60× (追い越し)・M8 1.04× (ほぼ同水準) (2026-06-11)

55.4 で `collectSymRows` を Poisson / Bernoulli の scalar Observe にも拡張
(`SymDist`/`VecGroupSrc`/`VecObsIR`・分布別密度ノード・Σlog y! は compile 時前計算・
Bernoulli は round 済 0/1 の定数係数化)。 観測値 guard (y<0 / y∉{0,1}) に掛かる行を
含むグループは収集時に弾く (walk の -∞ 縮退を残す安全方向)。 詳細 = 計画 md 55.4。

### per-draw 線形フィット (55.4 後・同一セッション PyMC 突合・long grid 主)

| モデル | grid | HS per-draw | HS 固定費 | PyMC per-draw | PyMC R² | HS/PyMC | baseline 比 |
|---|---|--:|--:|--:|--:|--:|--:|
| M7_pois | short | 0.0930 ms | 56 ms | 0.141 ms | 0.94 | (0.66׆) | |
| M7_pois | **long** | **0.0938 ms** | 58 ms | **0.157 ms** | 0.9997 | **0.60×** | **×9.0** (0.842→0.094) |
| M8_logit | short | 0.1616 ms | 103 ms | 0.121 ms | 0.95 | (1.33׆) | |
| M8_logit | **long** | **0.1519 ms** | 121 ms | **0.146 ms** | 0.9991 | **1.04×** | **×5.0** (0.761→0.152) |

- **M7 (Poisson 回帰) は PyMC 追い越し (0.60×)・M8 (logistic) はほぼ同水準 (1.04×・
  僅差で PyMC 優位)**。 55.1 baseline の 5.27× / 4.77× から ×9.0 / ×5.0。
- posterior 一致: M7 b@25600 = HS 0.7876 / PyMC 0.7880、 M8 b = HS 1.2860 /
  PyMC 1.2860。 ESS@25600 = HS 19.0k/20.1k。
- HS 固定費も激減: M7 467→58ms / M8 517→121ms (warmup が高速経路に乗るため)。

### M1-M6 非回帰 (full 再実行・旧 CSV と突合)

- **M1-M4: chain ビット一致** (acc_main/ESS 完全一致)。
- **M5/M6 も chain ビット一致** — σ を scalar leaf 直参照 → RUExp 評価に変えたが、
  σ = 単一 latent ('RUV') は tape ノード追加ゼロで 54.11 と同一 tape になる設計が
  実測で裏付けられた (per-draw fit も M5 0.264 / M6 0.225ms で 54.11 と同水準)。
- M7/M8 の chain は baseline と別物 (評価経路変更で FP 順序が変わるため・想定どおり)。
  posterior は上記のとおり PyMC と一致。

### 残 gap (M8 1.04× の内訳は未計測)

M8 の対 PyMC 僅差 (+4%) の内訳 (Bernoulli 密度ノードの log 2 回 + 1-p 経由 vs
PyMC の log_sigmoid 系単一カーネル等の差が候補) は**未計測**。 詰めるなら prof が先
(「推測するな計測せよ」)。 heteroscedastic の M 系追加は見送り (PyMC 側に対応する
標準 idiom があり比較可能だが、 per-call ×9.4 で改善は確認済・需要が出たら追加)。

## ★Phase 56.3-56.6 観測分布 IR Part 2 — 16 family 吸収 + M9_negbin PyMC 突合 (per-draw 1.48×・total は固定費で HS 有利) (2026-06-11)

56.2 の記号微分化 (上節) を土台に、 観測分布の IR 吸収を 12 分布追加して計
**16 family** にした (Gauss/Pois/Bern + StudentT(ν=SC)/Cauchy/Logistic/Gumbel
(56.3) + Expo/Weibull/LogN/Gamma/Beta (56.4) + Binom/Geom/NegBin (56.5))。
全分布**勾配コードゼロ** (densityIR の式 + 値 guard のみ・導関数は記号微分で自動)。
ZIP は見送り (logSumExp 系 op 無し)。 詳細 = 計画 md
`phases/phase-56-hbm-obs-dist-ir-part2.md`。

### 分布別 per-call 勾配 A/B (bench-hbm-dist 新設・n=100 canonical 回帰形)

IR 吸収 (gradADU 実経路) vs 全体 ad walk (旧 fallback 相当)。 計測前に全 family の
IR 吸収 + relErr ≤ 4e-14 を確認。 **per-call のみで per-draw への波及は M9 以外
未計測** (CSV `haskell/hbm_dist_grad_ab.csv`)。

| family | IR ms/call | walk ms/call | 倍率 | family | IR | walk | 倍率 |
|---|--:|--:|--:|---|--:|--:|--:|
| gauss | 0.0014 | 0.0966 | ×67.6 | lognormal | 0.0028 | 0.1414 | ×49.9 |
| pois | 0.0050 | 0.0727 | ×14.4 | gamma | 0.0082 | 0.5572 | ×68.1 |
| bern | 0.0093 | 0.0702 | ×7.6 | beta | 0.0440 | 1.7281 | ×39.2 |
| studentt | 0.0049 | 0.3245 | ×65.8 | binomial | 0.0122 | 0.2116 | ×17.3 |
| cauchy | 0.0044 | 0.0987 | ×22.5 | geometric | 0.0126 | 0.1275 | ×10.2 |
| logistic | 0.0081 | 0.1463 | ×18.1 | **negbin** | **0.0234** | **1.2277** | **×52.5** |
| gumbel | 0.0065 | 0.1213 | ×18.8 | expo | 0.0072 | 0.0847 | ×11.8 |

lgamma 持ち (studentt 定数化を除く gamma/beta/negbin) は walk 側が特に重く倍率大。

### M9_negbin (y ~ NegBin(exp(a+b·x), α)・n=100・α latent) PyMC 突合

パラメタ化は実行前に数値突合: pm.NegativeBinomial(mu, alpha) と
`logDensityObs (NegativeBinomial mu α)` は p=α/(α+μ) で同一 (5 点 diff ≤ 9e-15)。
IR 吸収は standalone 確認 (groups=1・obsAbsorbed=100)。

| 系 | grid | per-draw | 固定費 | R² | b (真値 0.8) |
|---|---|--:|--:|--:|--:|
| HS | short | 0.314 ms | 195 ms | 0.996 | 0.8100 |
| HS | **long** | **0.366 ms** | 157 ms | 0.998 | 0.8119 |
| PyMC | short | (0.352 ms†) | 2806 ms | 0.92 | 0.8100 |
| PyMC | **long** | **0.247 ms** | 2511 ms | 0.995 | 0.8110 |

†short grid の PyMC 傾きは固定費支配の fit アーティファクト (M5 の教訓どおり)。

- **per-draw 確定 (long): HS/PyMC = 1.48× (HS が遅い)**。 posterior は parity。
- 実用域 total は PyMC の compile+tune 固定費 ~2.5s が支配的で HS 有利
  (iter1600: HS 0.69s vs PyMC 2.80s)。
- ⚠ESS/sec の直接比較は不採用: hanalyze `ess` は τ≥1 clip で **ESS≤n が構造上限**
  (`Stat/MCMC.hs`)・ArviZ bulk ESS は n 超え可 (M9@25600: HS 25600 clip vs
  PyMC 32073) で非対称。
- baseline (NegBin 吸収直前 3e64859・walk fallback) per-draw は実測を試みたが
  **M9 short grid だけで ≥49 分・未完のため中断** (現行 ~13 秒)。 言えるのは
  **wall 下限 ≥×200** のみ (per-draw 傾きの baseline 確定値は無し)。

### M1-M8 非回帰 (full 再実行・55.5 時点 CSV と突合)

- **M1-M5/M7: chain ビット一致** (要件は M1-M4・M5/M7 は bonus)。
- M6/M8 は 56.2 の評価経路変更で FP 順序が変わり chain 別物 (計画許容)。
  posterior parity: M6 iter1600 rel 4.5e-4・M8 ~1e-2 (短 grid の差は MC ノイズ・
  長 grid で同値収束を確認)。 M7/M8 は 56.2 時点の glm CSV とは**ビット一致**。
- 副次: M7/M8 wall が 55.5 比 ~12% 改善 (56.2 記号微分化の波及とみられるが
  単一セッション比較のため断定しない)。

### 追補: PyMC numpyro (JAX) backend 突合 (ユーザ要望 2026-06-11)

`BENCH_NUTS_SAMPLER=numpyro` を `bench_hbm_scaling.py` に追加 (結果 CSV は
`_numpyro` suffix で C backend と分離)。 venv に numpyro 0.21.0 / jax 0.10.1
(CPU) を install (ユーザ許可)。 JAX は
`XLA_FLAGS="--xla_cpu_multi_thread_eigen=false intra_op_parallelism_threads=1"`
で走らせたが、 **fresh-compile 1 回の実測で cpu/wall = 1.50** — 並列は JIT
compile 部に集中しているとみられ (固定費側)、 per-draw 傾きはキャッシュ済み
call の計測でほぼ単スレッド挙動。 thread 別の厳密分離は未計測 (注意つきで読む)。

| モデル (long grid) | HS per-draw | PyMC-C | numpyro | HS/numpyro | numpyro 固定費 |
|---|--:|--:|--:|--:|--:|
| M7_pois | 0.0938 ms | 0.1566 ms | 0.0252 ms († R²=0.89) | 3.7× | 1165 ms |
| M8_logit | 0.1519 ms | 0.1461 ms | 0.0325 ms (R²=0.96) | 4.7× | 1172 ms |
| M9_negbin | 0.3664 ms | 0.2468 ms | **0.0780 ms** (R²=0.99) | **4.7×** | 1672 ms |

- **per-draw は numpyro が 3 系統最速** (XLA JIT のベクトル化+融合)。 HS 比
  3.7-4.7× — Phase 53 以来の比較対象だった PyMC-C backend より厳しい基準。
- ただし**固定費は numpyro が最大** (JIT・1.2-1.7s vs HS 0.06-0.16s)。 損益分岐
  (total が HS を下回る draws) は M9 ≈ 5,300 / M8 ≈ 8,800 / M7 ≈ 16,000 draw —
  **実用域 (warmup 500 + draws 数千以下) では HS の total が依然最小**。
- posterior は 3 系統 parity (M9 b: HS 0.8119 / C 0.8110 / numpyro 0.8110)。
- †M7 numpyro の R²=0.89 は短い total (最大 ~1.8s) に対する固定費ゆらぎ由来で
  傾きの確度が他より低い (M8/M9 は R² ≥ 0.96)。
