# Phase 78.G-f2 高速化計画 — centered 相関ランダム傾きで compiled 尤度経路に載せる

作成 2026-07-06 / ブランチ `feature/phase-78-doe-workflow` (同 phase 継続)。

## 背景と根本原因 (実測済)

`designModelHBM` に相関ランダム傾き `ranSlope`（lme4 `(1+s|g)`）を **非中心化 `b_j = Λ·z_j`** で
実装したところ、fit が **約 460× 遅い**（実測: 同一データ・1 chain・50 iter で 切片のみ 0.017 s /
相関 7.82 s。100 iter で 21.8 s = 2.79× 超線形）。

**原因 (ソース + 実測で確定):**
- HBM の勾配コンパイラ `compileGradUV` は、Gaussian observe の **平均 μ が affine (latent の線形結合)**
  の時のみ compiled/vectorized 高速勾配を作る (`IR.hs:151` の affine 追跡 `synthGaussLMBlocks`)。
- 非中心化の μ は `Σ β·X + Σ_c (Λ·z)_c·x_c` で、`Λ·z` = **latent 同士の積 (双線形)** ゆえ非 affine。
- → `synthVecIR` が `Nothing`、`compileGradUV` が `gradFull` = **モデル全体の generic `ad` (reflection
  tape) を毎 leapfrog** に落とす (`Gradient.hs:127-128, 205-210`)。切片のみは centered `reNormal` +
  固定重み observe で affine 条件を満たし解析勾配経路 = 桁違いに速い。

## 方針 — centered 化して尤度を compiled ブロックに載せる

観測 μ を **affine に保つ**のが根治。`b_j` を非中心化の deterministic 変換でなく **直接 latent**
として持ち、固定重み `x_c` で observe に載せる。相関は `b_j` の **prior 側**に置く。

- **尤度 (per-obs・大きい)**: μ = `Σ β·X + Σ_c b_{g}^c · x_c`。`b` は直接 latent、`x_c` は固定 Double。
  → affine。`synthGaussLMBlocks` の affine 追跡は **Phase 54.10 で `v_g·x_i` の重み付き REff gather を
  既にサポート** (`IR.hs:156`)。`compileLMBlock` も REff 重み `Just w` を compile 済 (`Gradient.hs:625,658`)。
  → 尤度は compiled ブロック化。
- **相関 prior (小さい・n 非依存)**: `b_g ~ MvNormal(0, Σ)`、Σ = `diag(τ)·R·diag(τ)`、
  R = LKJ 相関。実装は Λ = `diag(τ)·lkjCorrCholesky` を作り、`potential` に
  `mvNormalLogDensity`（or `MvNormalChol` 密度）を積む。base に scalar `Normal(0,1)` を置き
  `potential += logMvNorm(b_g;0,Λ) − Σ_c logN(b_g^c;0,1)` で net = MvNormal に補正。
  → `potential` があると hybrid 経路 (`Gradient.hs:179`) に入り、`mPriorGrad = Just (grad (fExcl))`
  = **compiled ブロックを除いた残差 (= prior のみ) の ad**。prior のパラメタは `O(群数·k + k²)` で
  **観測数 n に依存しない** → full ad (O(n·全 latent)) より桁違いに軽い。

要するに: **尤度=compiled / 相関 prior=小さな残差 ad closure** の hybrid にして、460× の主因
(full ad) を消す。狙いは切片のみと同オーダーの速度。

## TDD タスク

### Task 1 — de-risk 計測 (最優先・measure-first)
centered 版を最小実装し、既存 `[PROFILE]` テスト (WorkflowSpec) で **相関の壁時計が
7.82 s → 切片のみ同オーダー (目標 <0.3 s @1×50)** に落ちることを実測。
- 落ちれば hybrid 経路に載った確証 → Task 2 以降で本実装。
- 落ちなければ、どの residual が compile を阻むか (`gaussLMBlocksAuto` が空か / `residualFreeOfDensity`)
  を切り分けてから設計を見直す。ここで一旦報告。

### Task 2 — 相関 prior の符号化を確定
`b_g ~ MvNormal(0, Σ(τ,L))` を `sample b (Normal 0 1)` base + `potential` 補正で表現。
- Λ = `diag(τ)·lkjCorrCholesky ("Lcorr_"<>tag) k 2.0`、密度は `MvNormalChol` を利用。
- 勾配正当性を **有限差分 vs ad** で tiny モデル検証 (prior の符号・スケール取り違え検出)。

### Task 3 — designHBMProgram の相関分岐を centered に書き換え
`reContrib` を撤去し、centered 版へ:
- 群ごと `b_g^0..b_g^{k-1}` を `sample` (base Normal(0,1))。
- 各成分 c を全群で集め、重み (`c=0`→ones / `c≥1`→slope 列 `x_{c-1}`) 付き `REff` を作り
  `observeLMR` に渡す（`at` の重み版 = `REff … (Just w) …` を作る小 helper）。
- τ・LKJ を sample、`potential` で MvNormal prior を積む。
- **切片のみ高速経路は不変** (退行させない)。

### Task 4 — 正しさ + 速度の本検証
σ 縮小テストを **defaultHBM** で再実行し (a) σ→noise (b) 高速 を確認。先の failing (σ=1.57) が
多チェーン/収束アーティファクトだった件も、群数を増やす等で決着させ、防御的な assertion に直す。

### Task 5 — 一時 `[PROFILE]` テスト撤去 (or 1 本を perf 回帰ガードに転用)。

### Task 6 — api-guide 09-doe 更新
`ranSlope` = 相関 `(1+s|g)` 対応・速度は切片並みと明記。図は `gen-doc-figures.sh` 経由で再生成
（**図はユーザ承認後 push**）。

### Task 7 — spec / phase-plan 更新 (G-f2 高速化を記載)。

## 主リスク
- Task 1: affine 追跡が **直接 sample した** (reNormal 経由でない) latent の重み付き gather を昇格するか。
  IR.hs:156 は `v_g·x_i` を明記するが、reNormal/at 構築を前提にしている可能性 → 計測で確認。
- Task 2: MvNormal prior が hybrid の残差 ad に正しく入り、full ad に落ちない (Potential の扱い)。

## 触る対象
```
src/hanalyze/Analyze/Fit.hs      designHBMProgram 1463 / Fit instance 1499 / prepRE 1586
src/hanalyze/Analyze/Model/HBM/Model.hs   potential 362 / mvNormalLogDensity / at 299 (重み版 helper)
test/hanalyze/Analyze/Design/WorkflowSpec.hs   σ 縮小 test + [PROFILE] 群
docs/api-guide/09-doe.md         階層モデル節 :87-116
```

---

# 【2026-07-06 実測結果と中断判断】★このセクションが最新状況

## 状況: G-f2 は**中断**。 先に HBM 高速化 (別 phase = Phase 79) を終わらせてから戻る。

計画 (「centered 化で affine → compiled 高速経路に載せる」) を実装し 3 パラメータ化を実測した
結果、 **当初計画は成立しなかった**。 高速化には HBM core 側の拡張が必要と判明したため、 それを
**Phase 79 (HBM 相関 RE 高速化) として切り出し**、 完了後に本 G-f2 (DOE 側の配線・doc) に戻る。

### 実測 (同一データ 2群×12obs・1 chain・同 config)

| 実装 | correlated 1×50 | intercept 比 | iter scaling (100/50) |
|---|---|---|---|
| 非中心化 `b=Λz` + per-obs observe | 7.82 s | 460× | 2.79× |
| centered + per-obs observe | 3.32 s | 210× | 1.66× |
| **centered + observeLMR (確実に compiled 尤度)** | **2.73 s** | **160×** | **2.97×** |
| (基準) intercept のみ (observeLMR) | 0.017 s | 1× | — |

### 判明した根本原因 (計測で確定・当初計画が崩れた理由)

- **尤度の勾配コンパイルは主因ではない。** observeLMR (確実に compiled ブロック) にしても 2.73s =
  まだ 160×。 → 「affine に書けば速い」 は**不十分**。
- **iter scaling が超線形 (2.97×)** = 1 反復あたり leapfrog 数 (NUTS 軌道長) が増大 =
  **centered パラメータ化の funnel** (階層分散モデルの古典的難地形・Betancourt)。
- **本質的トレードオフ (実測で確定):**
  - centered → 尤度 affine (勾配速い) **だが funnel で軌道爆発** → 遅い
  - 非中心化 → funnel 無し **だが μ 非 affine で full ad** → 遅い
  - **両方とも別理由で遅い。**

→ 解決には **非中心化 (funnel 無し) の相関 RE に専用 compiled 勾配経路** を core に足す必要がある
   (= Phase 79)。 これは DOE 固有でなく HBM 汎用の改善なので別 phase が妥当。

### 現在のコード状態 (branch `feature/phase-78-doe-workflow`)

- `Fit.hs designHBMProgram`: **centered + observeLMR + potential(MvNormal prior)** の状態。
  **コンパイル OK・実行 OK・正しさ OK** (σ→noise で群別傾き差を捉える)。 ただし遅い (160×)。
- `WorkflowSpec`: σ 縮小 test + `[PROFILE]` 3 本 (一時計測用)。
- これは「正しいが遅い」 参照実装。 Phase 79 完了後、 この設計を高速経路へ載せ替える。

### Phase 79 完了後に G-f2 で残る作業 (Task 4〜7)

1. Phase 79 の高速経路に designHBMProgram を載せ替え、 [PROFILE] で intercept 同オーダーを確認。
2. σ 縮小テストを defaultHBM で本検証 (先の σ=1.57 が funnel/多チェーン由来か決着)。
3. `[PROFILE]` 一時テスト撤去 (or perf 回帰ガード化)。
4. api-guide 09-doe 更新 (相関 `(1+s|g)` 対応明記・図はユーザ承認後 push)。
5. spec / phase-plan 更新。

### 参照 (レビュー用に _trash に退避したドラフト)
- `_trash/phase-78-gf2-slow-vs-fast.hs` (遅い/速い書き方の対比)
- `_trash/phase-78-gf2-clean.hs` (plate ヘルパー版・読みやすい形)
