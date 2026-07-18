# Custom Design (JMP 同等) の使い方

> JMP Pro "Custom Design" 相当の **任意モデル × 任意制約 × 任意 runs** を
> 1 関数で生成する。 候補集合ベースの古典 D-optimal (`Hanalyze.Design.Optimal`)
> と異なり、 連続因子は coordinate exchange (Meyer-Nachtsheim 1995)、 categorical
> 因子は Modified Fedorov の hybrid で動く。 型シグネチャ・最小例は
> [api-guide 09-doe](../api-guide/09-doe.md) を一次根拠に、 ここは **設計思想・raw
> 表現規約・既知制限** を扱う。
>
> 仕様: `specification/spec/hanalyze-doe-custom-design-spec.md` v0.1.1
> 関連 Phase: 24-1 〜 24-9 (Phase 24 全体)。

## モジュール早見表

| モジュール | 役割 |
|---|---|
| `Design.Custom.Factor`     | 因子 ADT (Role × Kind の直交軸) |
| `Design.Custom.Model`      | モデル項 ADT + `expandDesignMatrix` |
| `Design.Custom.Constraint` | 制約 ADT (LinearIneq / Forbidden / Conditional / RangeBound) |
| `Design.Custom.Coordinate` | `coordinateExchange` 本体 (multi-start 探索) |
| `Design.Custom.Compare`    | `compareDesigns` (D/A/G/I efficiency + FDS + alias norm) |
| `Design.Custom.Power`      | `designPower` (設計行列ベース power analysis) |

---

## 1. 因子の役割 (`FactorRole`)

- `FactorKind` は 5 種: `Continuous lo hi` / `DiscreteNum [Double]` /
  `Mixture lo hi` / `Categorical [Text]` / `Ordinal [Text]`
- `FactorRole` は運用上の役割 (Controllable / HardToChange / Blocking / etc.)。
  Phase 24 では役割の違いは設計生成に影響しない (Phase 25 split-plot で使う)。

### Categorical 因子の raw 表現規約 (重要)

**型不安全**: `Matrix Double` 内の Categorical / Ordinal 列は **level index
0..K-1 を Double で保持** する規約 (案 α)。 `expandDesignMatrix` が
reference (treatment) coding で K-1 列に展開、 reference = index 0。
詳細と将来の型安全な再設計 (Phase 27 候補) は spec を参照。

---

## 2. 制約の統合方法

`Constraint` は `coordinateExchange` の per-grid-point filter として統合される。
制約違反の grid 点は criterion 評価をスキップ。 初期 random init は rejection
sampling (1 row あたり 200 回上限)。 Categorical TMain は K-1 列に展開、
TInter は cartesian product。

---

## 3. 評価指標 (`Compare`, `Power`)

- `compareDesigns` の `dcEffTable` は各設計の D/A/G/I efficiency (4 列)、
  `dcFDS` は予測分散 sorted vector (Halton 500 点)、 `dcAliasNorm` は
  連続 2fi の alias matrix Frobenius norm。
- `designPower` は各 term の effect size と sigma から noncentral F 近似で power。

---

## 4. JMP 例題 (golden)

実際の挙動を pin したテストは `test/Spec.hs` の以下 describe block:

- "Custom Design golden ex1: 2 factor 2nd-order RSM"
- "Custom Design golden ex2: 1 cont + 1 cat(3) + main+int model"
- "Custom Design golden ex3: LinearIneq constraint + 2 factor"

これらは `defaultBudget` + 固定 seed で生成され、 D-efficiency / row 数 /
制約満足を pinned 値で検証する。

---

## 既知の制限 (Phase 24 範囲)

- I-efficiency: self-moment 近似 (region 積分版は将来)
- alias matrix: 連続 × 連続 2fi のみ (categorical absent / TPower 拡張は将来)
- FDS region: 全因子独立 uniform、 制約付き region は rejection sampling 必要
- Split-Plot (Hard-to-Change): Phase 25 ([usage-augment-splitplot](usage-augment-splitplot.ja.md))
- Augment 5 メニュー: Phase 25
- Bayesian-D (DuMouchel-Jones): Phase 26 ([usage-bayesian-d](usage-bayesian-d.ja.md))
- `cdsInitial` (Augment 用) / `TNested` / `TCustom`: 未対応

入力 API の型安全強化 (Categorical level index → 型分離) は Phase 27 候補
として記録済 (phase-plan.md 参照)。
</content>
