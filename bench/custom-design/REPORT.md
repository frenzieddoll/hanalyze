# Custom Design (JMP 同等性) 検証ベンチ REPORT

> Phase 27 成果物。 文献例題 + JMP 公式 example の参照値 (D-eff / criterion) と
> hanalyze (Phase 23-26 で実装した Custom Design 機能) の出力を比較し、
> 「JMP Pro Custom Design platform と数値的に同等」 を **エビデンスベース** で
> 公称するための報告書。

## ステータス

- Phase 27-1 ✅: 環境セットアップ (`bench/custom-design/` + cabal `bench-custom-design`)
- Phase 27-2 ✅ (Table 2 のみ): **Jones-Goos (2012) Table 2** 20-run Split-Plot
  D-opt 例題、 D-efficiency = **1.0000** で文献 D-criterion 値と完全一致
  - 元案の Goos-Vandebroek (2003) Table 1 は paywalled のため、 Jones-Goos (2012)
    U Antwerp Research Paper 2012-002 に差し替え。 Goos-Vandebroek (2003) は
    将来追加候補
  - Table 1 (完全ランダム化) / Table 4 (28-run 3 因子) は Range B/C で追加候補
- Phase 27-3 ✅ (Phase 28-12 完了): **DuMouchel-Jones (1994) Example 3**
  Bayesian-D 例題、 **自動 DJ 適用後**: raw ratio = **1.0000 完全一致** (pass)
  - 27-3 時点: ratio = 1.4878 (raw) / 1.0268 (pth root) FAIL (DJ 規約未適用)
  - 28-12a (`Custom.Bayesian.djTransformColumns`) で DJ §2.2 規約 (centering +
    primary 直交化 + range=1 正規化) を実装、 paper §2.2 例 (x²→x²-0.5、
    x³→(x³-0.85x)/0.6) と完全一致を unit test で確認
  - 28-12 自動統合 (commit `b76f487`): `cdsDJConvention=True` で
    coordinateExchange 内部で DJ 変換を自動適用、 raw ratio 1.0000 で完全一致
    (= hanalyze が paper "Both" 設計と同じ DJ-aware criterion で同等品質の設計に
    収束)
- Phase 27-4 ⏸ deferred: **Meyer-Nachtsheim (1995)** 連続 D-opt 例題、 原典 PDF
  自動取得不可 (Technometrics 37 paywalled、 author 公開版 / 大学資料 / 代替 URL
  すべて 403 or 図書館ゲート)。 ユーザ手動取得 待ち、 着手延期
- Phase 27-5 ⏸ deferred: **JMP 公式 example (Response Surface Design with
  Constraints and Categorical Factor)** 原典 PDF 取得済、 18-run mixed factor
  I-opt 設計を `bench/custom-design/golden/jmp-rsm-constraints-categorical-design.csv`
  に保存。 ただし **live 比較は Phase 28-4 後に実施**: hanalyze の
  `iValueSelfM` (= IOpt criterion) は trace((X'X)⁻¹ · (X'X/n)) = p/n を返す
  **degenerate 関数** で設計に依らず定数、 I-eff 比較が成立しない。 詳細は
  下記 §Phase 27-5 参照 → **Phase 28-4 priority bump**
- Phase 27-6 ✅ (本 commit で確定): 全体総括 + phase-24/26 doc 証跡追記
- Phase 27-7 ✅ (本 commit で確定): 27-2 golden 値を `test/Spec.hs` に pinned
  (27-3 FAIL / 27-5 deferred は対象外)
- Phase 27-3: DuMouchel-Jones (1994) Example 1-3 Bayesian-D 例題 — 未着手
- Phase 27-4: Meyer-Nachtsheim (1995) 連続 D-opt 例題 — 未着手
- Phase 27-5: JMP 公式 example (RSM / Mixed / Constrained) — 未着手
- Phase 27-6: 全例題総括レポート — 未着手
- Phase 27-7: golden 値 → `test/Spec.hs` pinned — 未着手

## ディレクトリ構成

```
bench/custom-design/
├── REPORT.md                            ← 本ファイル
├── golden/                              ← 参照値 (文献 or JMP) CSV (27-2 以降)
│   └── (27-2 で goos-vandebroek-2003-table1.csv 等を配置)
└── results/
    └── golden-comparison.csv            ← 比較結果 (bench 実行ごと更新)
```

## golden CSV schema

- header: `metric,value`
- 縦持ち、 1 metric = 1 row
- 例:
  ```csv
  metric,value
  D-eff,0.872
  n_runs,12
  ```
- 公開不可な JMP CSV の数値だけを golden に切り出し、 原 CSV は別管理

## results/golden-comparison.csv schema

- header: `example,metric,hanalyze_value,reference_value,ratio,tolerance,pass`
- `ratio = hanalyze_value / reference_value`
- `pass  = |ratio - 1| <= tolerance`
- 各 Phase 27-2..27-5 でこの CSV に行を append していく
- Phase 27-6 で 全行 `pass = true` + ratio ≥ 0.99 を確認 → 「JMP 同等」 確定

## 実行方法

```
cabal run bench-custom-design
```

deterministic seed 固定なので再実行で値が変わらない (CI で同値検証可能)。

## 実測結果

### Phase 27-2: Jones-Goos (2012) Table 2 (20-run Split-Plot D-opt)

実行: `cabal run bench-custom-design` (seed=42, η=1, deterministic)

| metric | hanalyze_value | reference_value | ratio | tolerance | pass |
|---|---|---|---|---|---|
| D-criterion-ratio-raw | 2684.4444 | 2684.4444 | 1.0000 | 0.02 | ✅ |
| D-efficiency-pth-root | 1.0000 | 1.0000 | 1.0000 | 0.02 | ✅ |

**評価**: hanalyze の `generateSplitPlot` (Phase 25-3/4 実装) は
Jones-Goos (2012) Table 2 の D-opt 設計と **完全に同じ D-criterion 値** に
到達。 設計行列自体は別解の可能性あり (D-opt 解は一般に複数) だが、 criterion
値で同等性が確認できた。

**前提条件**:
- 仕様: 4 WP × 5 SP runs (n=20)、 1 WP 因子 w + 1 SP 因子 s、 連続 [-1, 1]
- 模型: full quadratic (Intercept + w + s + w·s + w² + s²、 6 項)
- η = σ²_WP / σ² = 1.0
- 簡易 REML criterion (SplitPlot.hs §「本 commit (25-3/4) のスコープ」、
  X̃ = chol(X' M⁻¹ X) を渡し det(X̃' X̃) = det(X' M⁻¹ X) を最大化)

**副次的成果 (本 phase でついでに実施)**:
- `src/Hanalyze/Design/Custom/SplitPlot.hs::chol` を `LA.chol` → `LA.mbChol`
  に置換 (非 PD 時 zero matrix → 候補 rejection)。 既知の脆弱箇所 (line 334-336
  「正定でない場合は inf 評価に任せる」 とコメントされていた未対応箇所) を解消。
  559 tests pass、 regression なし。

### Phase 27-3: DuMouchel-Jones (1994) Example 3 "Both" (9-run Bayesian-D)

実行: `cabal run bench-custom-design` (seed=42, τ=1, dbCxStepGrid=3, deterministic)

**Phase 28-12 自動統合適用後 (2026-05-29)**:

| metric | hanalyze_value | reference_value | ratio | tolerance | pass |
|---|---|---|---|---|---|
| BayesianD-criterion-ratio-raw-DJ | 2.0992e7 | 2.0992e7 | 1.0000 | 0.02 | ✅ |
| BayesianD-efficiency-pth-root-DJ | 1.0000 | 1.0000 | 1.0000 | 0.02 | ✅ |

**27-3 当初 (DJ 規約未適用)**: ratio = 1.4878 (raw) / 1.0268 (pth root) FAIL。
仮説 = DJ §2.2 規約 (`x² → x² - 0.5` 等) 未実装、 28-12a で paper §2.2 例
完全一致 helper を実装 → 28-12 自動統合 (`cdsDJConvention=True`) で
coordinateExchange が DJ-aware criterion を直接最適化 → **完全一致**。

**観察事実**: hanalyze の `coordinateExchange` (BayesianD criterion) は、
DuMouchel-Jones (1994) Example 3 「Both」 設計 (8-run resolution IV 2^(4-1) FF +
centerpoint) より **det(X'X + K) で 1.49 倍大きい値** に到達。 設計行列を
直接比較してはいないが、 同じ K + 同じ模型展開で別の design が高 criterion。

**仮説 (未検証、 Phase 28 で要検証)**: DuMouchel-Jones (1994) §2.2 (page 39) は
prior τ=1 既定が意味を持つよう、 potential terms に以下の規約を要求している:

- 各 nonconstant primary term: range [-1, 1]
- 各 potential term: range = 1 (centering/scaling)
- primary との直交化 (Σ_candidates X_pot X_pri = 0)

例: primary (1, x)、 potential x², x³ なら **z₁ = x² − 0.5**、
**z₂ = (x³ − 0.85x)/0.6** に置換 (paper §2.2 末尾)。

hanalyze の `TPower "A" 2` は **生の A²** (range [0, 1], mean 1/3) を
返すため、 同じ K = diag(0..0,1..1) を当てても det(X'X + K) は DJ の意図とは
異なる量を計算している可能性が高い。 → これが Phase 28 ギャップ項目。

**前提条件**:
- 仕様: 4 連続因子 A/B/C/D ∈ {-1, 0, 1}、 n = 9
- 模型 (15 項): intercept + 4 main + 4 squares + 6 2fi
- prior: priorPrecisionDefault (DJ 既定 classifier: intercept/main = 0、
  square/2fi = τ² = 1)
- 文献参照設計 (Table 1 "Both" 列): 8-run res IV 2^(4-1) (I=ABCD) + 1 centerpoint
- hanalyze 設定: dbCxStepGrid = 3 (= 3⁴ = 81 候補、 論文と同じ)、
  seed = 42、 dbRestarts = 10

**Phase 28 候補項目**: `Hanalyze.Design.Custom.Bayesian` に DJ centering/scaling
規約 (potential term の auto-orthogonalization + range-1 scaling) を追加するか、
ユーザ向け docs で「BayesianD を DJ 1994 通りに使うには手動で z₁ = x² − 0.5
を Model に積むこと」 を明記する。

### Phase 28-4d: JMP 公式 example (RSM Constraints + Categorical、 18-run I-opt) — ✅ live 比較成立

**Phase 28-4a/b で region-integral I-criterion を実装したため Phase 27-5
deferred を解除し、 live 比較を有効化** (2026-05-29)。

実測結果 (cabal run bench-custom-design):

```
=== Phase 28-4d: JMP RSM Constraints + Categorical (18-run I-opt) ===
  ref design IOptRegion = 0.517288
  hanalyze IOptRegion    = 0.511214  ratio=0.9883
```

- **JMP 設計の IOptRegion 値 = 0.517288** (= trace((X'X)⁻¹·M_R)、 analytic M_R)
- **hanalyze IOpt 設計の IOptRegion 値 = 0.511214** (同 spec + 同制約 + seed=654321)
- **ratio = 0.9883** (hanalyze が JMP より 1.17% 小さい = わずかに改善)
- tolerance = 0.05 (≤ 5% 増 で pass)、 → **pass**

意義:
- Phase 27-5 で指摘した「`iValueSelfM` = p/n 縮退」 が解消、 設計に依存した
  有限の I-criterion を返す
- hanalyze IOpt が JMP I-opt を上回らないことを確認 (ratio < 1 = 同等 or 改善)
- ただし **analytic M_R は制約を無視** している点に注意: 厳密な constrained-
  region I-criterion は Phase 28-4c (MC fallback) で実装予定

設定:
- 因子: Time ∈ [500, 560] (coded [-1, 1])、 Temperature ∈ [350, 750] (coded
  [-1, 1])、 Catalyst ∈ {A, B, C}
- 模型: RSM (intercept + main + 全 2fi + Time² + Temp²)、 p = 12
- 制約: Conditional (B → Temp_coded ≥ -0.75)、 Conditional (C → Temp_coded ≤ 0.5)
  (元: B → Temp≥400、 C → Temp≤650 を coded 換算)
- hanalyze 側: seed=654321、 dbRestarts = defaultBudget の値

### Phase 27-5 (旧、 deferred 記録) — Phase 28-4d で解除済

**取得済の素材** (`bench/custom-design/golden/jmp-rsm-constraints-categorical-design.csv`):

- 一次根拠: JMP 12 「Design of Experiments Example: A Response Surface Design
  with Constraints and a Categorical Factor」 PDF (JMP community sample-data
  attachment、 公開資料)
- 仕様:
  - 因子: Time (Continuous [500, 560] sec)、 Temperature (Continuous [350, 750] K)、
    Catalyst (Categorical {A, B, C})
  - 模型: RSM (intercept + main + Time² + Temperature² + 全 2fi)
  - 制約: Catalyst=B → Temperature ≥ 400、 Catalyst=C → Temperature ≤ 650
    (= 「Disallowed Combinations Filter」 で B/T<400 と C/T>650 を禁止)
  - JMP 設定: I-optimal criterion、 Random Seed = 654321、 Number of Starts = 1000
  - 結果: 18-run design matrix (golden CSV 参照)

**deferred 理由**: hanalyze の現状 IOpt 実装は **数学的に degenerate**:

```haskell
-- src/Hanalyze/Design/Custom/Coordinate.hs::iValueSelfM
iValueSelfM x =
  let xtx    = X' X
      inv    = (X'X)⁻¹
      moment = X'X / n   -- self-moment 近似
  in  sum (diag (inv · moment))
   -- = trace((X'X)⁻¹ · (X'X/n)) = trace(I_p) / n = p / n
```

設計に依らず常に `p/n` を返すため、 JMP I-opt 設計と hanalyze I-opt 設計を
iValueSelfM で比較すると **trivial pass (ratio = 1.0)** になり、 実質的な
JMP 同等性検証にならない。 当該 module の `iValueSelfM` 実装は self-moment
近似と称しているが、 self-moment = X'X/n を使うと **任意の非特異 X で恒等的に
p/n** となる縮退状態。

**Phase 28-4 priority bump**: 「I-efficiency region 厳密化」 で
true region-integral 版 I-criterion を実装したのち、 本 JMP example の
live 比較を有効化する。 golden CSV は本 commit で配置済。

**前提条件 (Phase 28-4 後の予定)**:
- 仕様: 上記 JMP PDF の通り
- hanalyze 設定: dbCxStepGrid を fine (≥21) に、 IOpt = region-integral 版、
  seed = 42、 dbRestarts = 10
- 比較 metric: hanalyze の region-integral I-criterion vs JMP 設計の
  same metric (両方 ratio ≥ 0.99 を目標)

## Phase 27 全体総括 (27-6)

Phase 27 (JMP 同等性検証ベンチ) 区切り時点の結論。 計画 7 sub-phase 中、
27-1 / 27-2 / 27-6 / 27-7 完了、 27-3 ⚠️ gap 検出済、 27-4 / 27-5 ⏸ deferred。

### 確定 trigger (phase-24 / phase-26 doc) の達成状況

phase-24 doc 「連続 main + 2 因子 interaction で classic D-optimal (Fedorov,
candidate=full grid) と criterion ≥ 0.99 倍を確認」:
- → ✅ **Phase 27-2 で Jones-Goos (2012) Table 2 と D-criterion = 1.0000 (完全一致)
  で達成**。 Split-Plot 例題で示したが、 SplitPlot は generalized REML D-opt
  であり、 純粋な Fedorov candidate=full grid との直接比較ではない点に留意

phase-26 doc 「DuMouchel-Jones (1994) 例題で JMP との一致 (criterion 小数 3 桁)
が確認できる見通し」:
- → ⚠️ **Phase 27-3 で未達成**。 ratio = 1.4878 (raw) で 99% 一致せず。 仮説は
  DJ §2.2 centering/scaling 未実装 (Phase 28-12 候補)。 真の trigger 達成は
  Phase 28-12 後に再評価

### Phase 28 への引き継ぎ事項 (Phase 27 由来 ✦ 印付き)

| Phase 28 sub | 内容 | Phase 27 由来 |
|---|---|---|
| 28-4 ✦ | I-efficiency region 厳密化 | 27-5 で iValueSelfM = p/n 縮退検出 |
| 28-12 ✦ | DJ §2.2 centering/scaling 規約対応 | 27-3 で det 比 1.4878 検出 |

phase-28 計画 doc にこれら ✦ 印を反映済。

### 「JMP 同等」 公称の現時点での妥当性

- **Split-Plot D-optimal (Phase 25-3/4)**: ✅ Jones-Goos (2012) Table 2 で
  D-criterion 完全一致を示した。 「同等」 と言って差し支えなし
- **Custom Design Bayesian-D (Phase 26)**: ⚠️ DuMouchel-Jones (1994) 規約と
  乖離。 「同等」 と言うには Phase 28-12 が必要
- **Custom Design I-optimal**: ❌ iValueSelfM が縮退しており、 そもそも
  I-criterion として機能していない。 「同等」 公称は **Phase 28-4 後に再評価**
- **連続 D-opt (Meyer-Nachtsheim 1995)**: 未検証、 文献入手後に着手

### 副次的成果 (Phase 27 の本筋ではないが本 phase で実施)

- `src/Hanalyze/Design/Custom/SplitPlot.hs::chol`: `LA.chol` → `LA.mbChol`
  置換 (非 PD 時 zero matrix → 候補 rejection)。 既知の脆弱箇所を解消、
  559 tests pass

## 文献値メモ (Jones-Goos 2012、 一次根拠)

Phase 27-2 で golden CSV 化する対象。 一次根拠 = 公開 working paper
"I-optimal versus D-optimal split-plot response surface designs"
(Jones & Goos、 U Antwerp Research Paper 2012-002、 January 2012)。

### Table 1 (完全ランダム化、 2 因子、 quadratic、 20 runs、 3×3 grid)

- D-opt avg relative variance of prediction = **0.233**、 I-opt = **0.183**
- D-opt の I-efficiency = **78.5%**、 I-opt の D-efficiency = **94.9%**
- factor-effect estimates relative variance (Intercept / x1 / x2 / x1·x2 / x1² / x2²):
  D-opt = (0.302, 0.068, 0.068, 0.083, 0.282, 0.282)、
  I-opt = (0.179, 0.083, 0.083, 0.125, 0.214, 0.214)

### Table 2/3 (Split-Plot、 20-run、 4 WP × 5 SP、 1 WP w + 1 SP s、 quadratic)

D-Optimal 設計行列 (w, s):
```
WP1: (-1,-1) (-1,-1) (-1, 0) (-1, 1) (-1, 1)
WP2: ( 0,-1) ( 0,-1) ( 0, 0) ( 0, 0) ( 0, 1)
WP3: ( 1,-1) ( 1,-1) ( 1, 0) ( 1, 1) ( 1, 1)
WP4: ( 1,-1) ( 1,-1) ( 1, 0) ( 1, 1) ( 1, 1)
```

I-Optimal 設計行列 (w, s):
```
WP1: (-1,-1) (-1,-1) (-1, 0) (-1, 1) (-1, 1)
WP2: ( 0,-1) ( 0, 0) ( 0, 0) ( 0, 0) ( 0, 1)
WP3: ( 0,-1) ( 0, 0) ( 0, 0) ( 0, 0) ( 0, 1)
WP4: ( 1,-1) ( 1,-1) ( 1, 0) ( 1, 1) ( 1, 1)
```

評価値:
- I-opt は **93.4% D-efficient** (η に依存しない)
- D-opt avg relative variance of prediction (η=1) = **0.973**、 I-opt = **0.717**
- D-opt の I-efficiency: **73.8%** (η=1) / **75.9%** (η=0.1) / **72.9%** (η=10)

Table 3: factor-effect estimates の relative variance (η=1):
| Effect | D-Opt | I-Opt |
|---|---|---|
| Intercept | 1.301 | 0.640 |
| w | 0.450 | 0.600 |
| s | 0.075 | 0.083 |
| ws | 0.092 | 0.125 |
| w² | 1.665 | 1.240 |
| s² | 0.279 | 0.250 |

η=0.1 / η=10 の値も紙面 Table 3 に記載 (本ファイル §関連で原典参照)。

### Table 4 (Split-Plot、 28-run、 7 WP × 4 SP、 1 WP + 2 SP、 quadratic)

- 設計行列 4 種 (Design I/II = D-opt for η<3.10 / η>3.10、 III/IV = I-opt for
  η<2.05 / η>2.05) 全て紙面に座標明示。 必要時に golden CSV に転記する。

## CLAUDE.md 規律

- **推測するな、 計測せよ**: ratio は実測値、 「JMP と一致するはず」 で済まさない
- **事実か憶測か明示**: golden 値は論文記載値 or JMP CSV を一次根拠とする
- 文献値 / JMP 数値を私 (Claude) の記憶で書かない
