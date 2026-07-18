# Custom Design 機能マニュアル (Phase 23-28 統合)

> 2026-05-29 に追加された **JMP Pro "Custom Design" 同等機能** の全体マニュアル。
> 「何ができるようになったか」 「どんな時に使うか」 「どう書くか」 を、
> ユースケースから API への橋渡し形式で整理する。
>
> 各機能の詳細リファレンスは末尾の「詳細 doc 一覧」 を参照。
> 古典 DoE 機能 (Factorial / Block / ANOVA / Power / 標準 RSM / 直交表 / Taguchi)
> は [01-doe.ja.md](01-doe.ja.md)。

---

## 0. 概観: 何ができるようになったか

hanalyze 0.1.0.1 / 2026-05-29 時点で、 以下を **1 API で扱える** ように
なった (= JMP "Custom Design" platform 相当):

| 領域 | 主な API | 何ができるか |
|---|---|---|
| 任意モデル × 任意 runs の最適設計 | `coordinateExchange` | 連続 × 離散 × categorical 混合、 候補集合不要 |
| 階層因子 | `TNested A B` (Phase 28-1) | A within B (両 Categorical/Ordinal)、 K_B × (K_A-1) 列展開 |
| 制約付き設計 | LinearIneq / Forbidden / Conditional / RangeBound | 半空間制約 / 完全一致禁止 / If-then 制約 / 範囲上書きを **同じ ADT で表現** |
| Hard-to-Change 因子 | `generateSplitPlot` | 段取り高い因子の分割実験 (REML D-opt)、 Categorical WP も対応 (Phase 28-3) |
| Strip-plot (Phase 28-2) | `generateSplitPlot` + `spcNStrip=Just n` | `VeryHardToChange` で 2 stratum、 buildMInvStrip で M⁻¹ |
| 既存設計の増補 | `augmentMenu` | Replicate / 中心点 / axial (raw 単位対応、 Phase 28-10) / 追加 runs / Foldover (level swap 対応、 Phase 28-7) |
| 事前情報入りの D-opt | `BayesianD` + `cdsDJConvention=True` (Phase 28-12) | DuMouchel-Jones の K 加算 + paper §2.2 規約自動適用 |
| I-optimal (region 厳密) | `IOpt` (Phase 28-4) | analytic 解析積分 + 制約有り MC fallback (`regionMomentMatrixMC`) |
| 設計の比較 | `compareDesigns` / `compareDesignsWithResponses` (Phase 28-8) | D/A/G/I efficiency (BayesianD-aware D 列、 Phase 28-5) + FDS + alias norm (cat 2fi + TPower 拡張、 Phase 28-6) + MCp / MCpk |
| 合成 criterion | `Compound` + `compoundGeometric` (Phase 28-9) | 線形 + 幾何平均 + `dEfficiency` / `aEfficiency` helper |
| 検出力 (Power) | `designPower` | 各 model term の noncentrality λ から Power を直算 |
| 制約付き候補削減 | `Hanalyze.Design.Constraint.filterCandidates` | 古典 Fedorov 用の事前フィルタ |
| 工程能力 (非正規) | `processCapabilityGamma` / `NonNormalFit` | 右歪みデータ用 Cp と AIC 自動選択 |
| 工程能力 (多変量) | `processCapabilityMultivariate` | Mahalanobis ベース MCp / MCpk |

**実装状況** (2026-05-29、 Phase 28 全完了時点):

- **583 tests pass** (Phase 28 で +24)、 全 commit `(hanalyze-portable)` タグ付き
- **bench 5/5 metrics pass**:
  - Jones-Goos (2012) Table 2 Split-Plot D-opt: ratio **1.0000 完全一致**
  - DuMouchel-Jones (1994) Example 3 "Both" Bayesian-D: ratio **1.0000 完全一致** (DJ §2.2 自動適用後)
  - JMP RSM Constraints + Categorical 18-run I-opt: ratio **0.9791** (MC region で hanalyze 2.1% 改善)
- 文献入手待ち = Meyer-Nachtsheim (1995) のみ (Phase 27-4 deferred、 paywall)
- upstream hanalyze へ cherry-pick 可能

---

## 1. ユースケース → 機能の対応表

「こういう事がしたい」 から「どの API を呼ぶか」 を一望できる対応表。 詳細は
リンク先 doc。

### A. 設計を作りたい

| やりたい事 | 機能 | 詳細 |
|---|---|---|
| 連続 2-3 因子 + 二次モデル (RSM 相当) | `coordinateExchange` + `TIntercept/TMain/TInter/TPower` | [usage-custom-design](usage-custom-design.ja.md) |
| 連続 + categorical 混合 | 同上 (`Categorical` 因子追加) | 同上 |
| 線形制約 (x1+x2 ≤ 1) | `LinearIneq` | 同上 §3 |
| 禁止組合せ (cat=A かつ x1=1 等) | `Forbidden` | 同上 §3 |
| 「cat=A の時だけ温度上限」 | `Conditional + GuardEq` | 同上 §3 |
| Hard-to-Change 因子で分割実験 | `generateSplitPlot` | [usage-augment-splitplot](usage-augment-splitplot.ja.md) |
| 既存設計に中心点 / axial を足す | `augmentMenu (AddCenter\|AddAxial)` | 同上 §1 |
| 既存設計に最適な追加 runs | `augmentMenu (AddRuns n)` | 同上 §1 |
| 既存設計を符号反転で foldover | `augmentMenu (Foldover ...)` | 同上 §1 |
| 二次項に弱い prior を入れた D-opt | `BayesianD + priorPrecisionDefault` | [usage-bayesian-d](usage-bayesian-d.ja.md) |
| D と I を 7:3 で重み付き合成 | `Compound [(0.7, DOpt), (0.3, IOpt)]` | 同上 §3 |

### B. 設計を評価したい

| やりたい事 | 機能 | 詳細 |
|---|---|---|
| 複数設計の D/A/G/I efficiency 比較 | `compareDesigns.dcEffTable` | [usage-custom-design](usage-custom-design.ja.md) §5 |
| FDS plot 用データ (予測分散分布) | `compareDesigns.dcFDS` | 同上 |
| Alias matrix の Frobenius norm | `compareDesigns.dcAliasNorm` | 同上 |
| 各 term の Power (n + effect size 必要) | `designPower` | 同上 §5 |
| VIF / 単一設計の効率値 | `Hanalyze.Design.Diagnostics.diagnostics` | [01-doe](01-doe.ja.md) §5 |
| max leverage を最小化したい (= G-opt) | `OptCriterion = GOpt` | [usage-classic-extensions](usage-classic-extensions.ja.md) §1 |

### C. 観測 y を解析したい (post-hoc)

| やりたい事 | 機能 | 詳細 |
|---|---|---|
| 多変量正規 y の工程能力 | `processCapabilityMultivariate` | [usage-classic-extensions](usage-classic-extensions.ja.md) §4 |
| 右歪み (Gamma) の Cp | `processCapabilityGamma` | 同上 §3 |
| 分布が不明な工程能力 (Box-Cox / Johnson SU / Gamma 自動選択) | `NonNormalFit` 経由 | 同上 §3 |
| Custom Design に y を当てた線形モデル fit | `Hanalyze.Model.LM` (既存) | [01-doe](01-doe.ja.md) §3 |

---

## 2. 最小ワークフロー例 3 種

### 例 1: 連続 2 因子 RSM の Custom Design

```haskell
import Hanalyze.Design.Custom.Factor
import Hanalyze.Design.Custom.Model
import Hanalyze.Design.Custom.Coordinate
import Hanalyze.Design.Optimal (OptCriterion (..))

main :: IO ()
main = do
  let f1 = Factor "x1" (Continuous (-1) 1) Controllable
      f2 = Factor "x2" (Continuous (-1) 1) Controllable
      model = Model
        [ TIntercept
        , TMain "x1", TMain "x2"
        , TInter ["x1","x2"]
        , TPower "x1" 2, TPower "x2" 2
        ] NCoded
      spec = CustomDesignSpec
        { cdsFactors     = [f1, f2]
        , cdsModel       = model
        , cdsConstraints = []
        , cdsNRuns       = 12
        , cdsCriterion   = DOpt
        , cdsBudget      = defaultBudget       -- 21 grid, 5 restart, maxIter 50
        , cdsSeed        = Just 42
        , cdsInitial     = Nothing
        }
  Right cd <- coordinateExchange spec
  print (cdMatrix cd)
```

### 例 2: 制約付き設計 + 比較 + 検出力

```haskell
import qualified Hanalyze.Design.Custom.Constraint as CC
import qualified Hanalyze.Design.Custom.Compare    as Cmp
import qualified Hanalyze.Design.Custom.Power      as Pwr

main = do
  -- 制約付き設計
  let spec2 = spec
        { cdsConstraints = [CC.LinearIneq [("x1",1),("x2",1)] CC.CLeq 1] }
  Right cdConstrained <- coordinateExchange spec2

  -- 制約なし版と比較
  Right cdFree <- coordinateExchange spec
  let comp = Cmp.compareDesigns
        [("free", cdFree), ("constrained", cdConstrained)]
  print (Cmp.dcEffTable  comp)     -- 各設計の D/A/G/I efficiency
  print (Cmp.dcAliasNorm comp)     -- alias norm
  -- print (Cmp.dcFDS comp)        -- 各設計の予測分散 sorted vector (FDS plot)

  -- 検出力 (sigma=1.0、 effect 0.5)
  let powers = Pwr.designPower cdConstrained 1.0
        [("x1", 0.5), ("x1:x2", 0.3)] 0.05
  mapM_ print powers
```

### 例 3: Hard-to-Change 因子 (温度) を Whole-Plot にした分割実験

```haskell
import qualified Hanalyze.Design.Custom.SplitPlot as SP

main = do
  let fTemp = Factor "temp" (Continuous 100 200) HardToChange
      fRate = Factor "rate" (Continuous   0   1) Controllable
      modelSP = Model
        [TIntercept, TMain "temp", TMain "rate"
        , TInter ["temp","rate"]] NCoded
      spec = CustomDesignSpec
        { cdsFactors = [fTemp, fRate]
        , cdsModel   = modelSP
        , cdsConstraints = []
        , cdsNRuns   = 12
        , cdsCriterion = DOpt
        , cdsBudget = defaultBudget
        , cdsSeed   = Just 100
        , cdsInitial = Nothing
        }
      cfg = SP.defaultSplitPlotConfig 4    -- 4 WP × 3 runs = 12
  Right spd <- SP.generateSplitPlot spec cfg
  print (SP.spdMatrix      spd)
  print (SP.spdWholePlotId spd)   -- [0,0,0,1,1,1,2,2,2,3,3,3]
  -- 各 WP 内で temp が一定 (段取り回数 = 4 で済む) を保証
```

---

## 3. 設計上の重要な前提 (使う前に必ず確認)

### A. raw matrix の Categorical 表現は **型不安全** (案 α)

`Matrix Double` 内の Categorical / Ordinal 列は **level index 0..K-1 を Double**
で保持する。 `expandDesignMatrix` が reference (treatment) coding で K-1 列に展開
する (reference = index 0)。

- ✅ 数値演算は速い、 hmatrix 親和性が高い
- ❌ 0.5 や範囲外 index を型で防げない (runtime check で `Left`)
- 将来の **型安全な再設計 (案 β、 R `model.matrix` 流)** は Phase 27 候補として
  phase-plan に登録済 (canvas API schema 確定または事故発生時に着手)

### B. 内部 grid は coded space `[-1, 1]` 想定

`Continuous lo hi` の grid は **lo / hi を無視して `[-1, 1] linspace`**。
raw 単位の設計が欲しい場合は、 生成後の matrix を呼び出し側でスケーリング:

```haskell
let raw = cdMatrix cd
    rescaled = LA.fromColumns
      [ scaleColumn (factors !! j) (LA.flatten (raw LA.? [j]))
      | j <- [0 .. nF - 1] ]
```

### C. 全関数は **IO (Either Text ...)** で失敗を返す

ランダム探索 + 制約付き rejection sampling のため `IO`。 失敗は **例外でなく
`Left Text`**。 必ずパターンマッチで取り出すこと。

```haskell
r <- coordinateExchange spec
case r of
  Left  e  -> putStrLn ("失敗: " <> T.unpack e)
  Right cd -> ...
```

### D. `cdsBudget` の調整指針

| パラメータ | 既定 | 増やすと | 減らすと |
|---|---|---|---|
| `dbCxStepGrid` | 21 | 解像度上がる、 計算高 | 解像度落ちる、 計算速 |
| `dbRestarts`   | 5  | 大域最適に近づく、 計算高 | 局所最適に嵌るリスク |
| `dbMaxIter`    | 50 | 収束強い、 計算高 | 不十分収束のリスク |
| `dbTol`        | 1e-6 | 早く打ち切る | 厳密に収束させる |

業務での既定は `defaultBudget` で十分。 高次元 (因子数 ≥ 5) では `dbRestarts`
を 10 以上にすると良い。

---

## 4. 詳細 doc 一覧

機能ごとの詳細 (API シグネチャ、 制限、 サンプルコード):

| 範囲 | 詳細 doc |
|---|---|
| 古典側拡張 (G-opt, Compound, Constraint, 非正規 Cp, 多変量 Cp) | [usage-classic-extensions.ja.md](usage-classic-extensions.ja.md) |
| Custom Design Core (Factor / Model / Constraint / Coordinate / Compare / Power) | [usage-custom-design.ja.md](usage-custom-design.ja.md) |
| Split-Plot + Augment 5 メニュー | [usage-augment-splitplot.ja.md](usage-augment-splitplot.ja.md) |
| Bayesian-D + Compound 強化 | [usage-bayesian-d.ja.md](usage-bayesian-d.ja.md) |
| (参考) 古典 DoE 機能全般 | [01-doe.ja.md](01-doe.ja.md) |
| (参考) DoE 理論 | [theory-doe.ja.md](theory-doe.ja.md) |

開発者向けメモ:

- `docs/dev-notes/upstream-hmatrix-accum.md` — hmatrix `LA.accum` の引数順 doc
  が判別不能な件 (upstream 報告 candidate)
- `specification/spec/hanalyze-doe-custom-design-spec.md` v0.1.1 — 仕様書
- `specification/spec/hanalyze-doe-spec.md` v0.2 — 古典側拡張仕様

---

## 5. 既知の制限まとめ (寝起きで全部俯瞰する用)

Phase 28 (a-z) で大半が解消、 残る制限のみ列挙。

| 領域 | 制限 | 対応 / メモ |
|---|---|---|
| Categorical 入力 | 型不安全な level index 規約 | Phase 29 (trigger 待ち、 型分離再設計) |
| SplitPlot REML | 簡易版 (chol で X̃ 化)、 絶対値比較は不可 | Goos-Vandebroek 厳密化、 将来 |
| Strip-plot (Phase 28-2) | η_WP = η_Strip 共通の簡略化 | 別 η 対応は将来 |
| Compare の FDS | 全因子独立 uniform region | 制約付き region は rejection、 28-4c の MC fallback は IOpt のみ |
| Conditional 制約 | AND/OR 正論理のみ、 NOT 無し | NP-hard 化のため見送り |
| TNested (Phase 28-1) | A/B 両者 Categorical/Ordinal のみ | 連続 A within cat B は将来 |

Phase 28 で **解消** された旧制限 (参考):

| 旧制限 | 解消 commit (Phase) |
|---|---|
| `iValueSelfM` = p/n 縮退 | Phase 28-4a/b (region 積分版) |
| 制約 / Mixture 因子の IOpt | Phase 28-4c (MC fallback) |
| BayesianD の Compare D-eff 列 | Phase 28-5 (BayesianD-aware) |
| Compare alias Z の categorical/TPower 不在 | Phase 28-6 (Z 範囲拡張) |
| Foldover categorical level swap | Phase 28-7 (CategoricalSwap) |
| 多変量 Cp の Compare 統合 | Phase 28-8 (compareDesignsWithResponses) |
| Compound 幾何平均 | Phase 28-9 (compoundGeometric) |
| AddAxial raw range scaling | Phase 28-10 (rawUnits flag) |
| Strip-plot (VeryHardToChange) | Phase 28-2 (spcNStrip) |
| Categorical Whole-Plot | Phase 28-3 (制約解除) |
| DJ §2.2 規約 (Bayesian-D) | Phase 28-12 (auto via cdsDJConvention) |
| TNested 未対応 | Phase 28-1 (Categorical/Ordinal nested 対応) |

---

## 6. トラブルシューティング

### `Left "no restart produced a design"` が出る

→ 全 restart で失敗 (= 全初期解が infeasible)。 制約がきつ過ぎる可能性。
   `cdsConstraints` を緩めるか、 `dbRestarts` を増やしても解消しないなら
   実現可能領域が小さすぎる。

### `Left "factor X is categorical/ordinal — treatment coding 未実装"` が出る

→ Phase 24-1 時点の skeleton と思った可能性。 現在は Phase 24-2 で実装済、
   build キャッシュをクリーンしてみる: `cabal clean && cabal build`。

### D-eff が極端に低い (< 0.3)

→ ランダム探索で局所最適に嵌っている可能性。 `dbRestarts` を 10-20 に増やす。
   それでも改善しない場合は model が n_runs に対して overspecified
   (= 自由度不足) の可能性。 `cdsNRuns` を増やすか model を簡素化。

### `BayesianD` で `Left "model invalid"`

→ K の次元 (p × p) が expand 後の列数と一致していない。 `priorPrecisionDefault`
   を使えば自動で揃うので、 手書きの K matrix は使わない方が安全。

### SplitPlot で WP 因子が WP 内 constant にならない

→ `fRole = HardToChange` が設定されていない可能性。 `whichRoleIsWP factors`
   で空 list が返ると `Left` になる。

---

## 7. 参考文献

- Meyer & Nachtsheim (1995). "The Coordinate-Exchange Algorithm for
  Constructing Exact Optimal Experimental Designs". *Technometrics* 37:60-69.
  → `coordinateExchange` の基盤アルゴリズム
- Goos & Vandebroek (2003). "D-Optimal Split-Plot Designs". *J Quality Tech* 35:1-15.
  → `generateSplitPlot` の REML 情報行列
- DuMouchel & Jones (1994). "A Simple Bayesian Modification of D-Optimal
  Designs to Reduce Dependence on an Assumed Model". *Technometrics* 36:37-47.
  → `BayesianD` + `priorPrecisionDefault`
- Wang, Hubele, Lawrence (2000). "Comparison of Three Multivariate Process
  Capability Indices". *J Quality Tech* 32:263-275.
  → `processCapabilityMultivariate` の MCp 風指標
