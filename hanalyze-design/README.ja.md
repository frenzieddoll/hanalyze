# hanalyze-design

[`hanalyze`](../README.ja.md) の**実験計画法 (DoE) 層**。 計画の
**生成** (要因計画 / 直交表 / RSM / 最適計画 / 空間充填 / 配合) と、
得られたデータの**評価** (分散分析 / 検出力 / 工程能力 / 測定システム解析) を
担う 30 module。

依存は `core` / `frame` / `models` の 3 層 + 外部 10 package。
モデル fit を `-models` に委ねるため、 RSM の二次モデルや最適計画の
情報行列計算はこの層で完結せず models 層の回帰を使う。 `-viz` は
この層の上に乗る。

## 主要 module (全 30 module)

### 古典的な計画の生成 (`Hanalyze.Design.*`)

| Module | 役割 |
|---|---|
| `Design.Factorial` | 完全 / 2 水準 / 3 水準 / 一部実施 / 混合水準の要因計画 = DoE の入口 |
| `Design.Orthogonal` / `Design.Taguchi` | 直交表 (L8 / L9 / L12 …) / SN 比と内側・外側配置のロバスト設計 |
| `Design.Mixed` / `Design.Block` | 混合水準計画 / ブロック化 (乱塊法) |
| `Design.DSD` | Definitive Screening Design (Jones-Nachtsheim 2011) |
| `Design.RSM` | 応答曲面法 — CCD / Box-Behnken の生成、 二次モデル fit、 極値の解析解 |

### 最適計画・空間充填・配合

| Module | 役割 |
|---|---|
| `Design.Optimal` | D / A / I / E / G-optimal (Fedorov 交換法) + 既存計画への追加 (`augmentDesign`) |
| `Design.SpaceFilling` | 空間充填計画 — LHS / Maximin LHS / Halton (コンピュータ実験用) |
| `Design.Mixture` | 配合計画 — Simplex Lattice / Simplex Centroid (成分比の合計 = 1) |
| `Design.MultiRSM` | 複数応答の同時最適化 (Desirability との併用) |

### Custom Design (`Design.Custom.*`)

因子・モデル・制約をユーザが組み立てる汎用計画生成系。 11 module。

| Module | 役割 |
|---|---|
| `Custom.Factor` / `Custom.Model` | 因子定義 (連続 / カテゴリ / 離散数値) / モデル項の指定 |
| `Custom.Constraint` / `Design.Constraint` | 線形・非線形制約下での候補点の絞り込み |
| `Custom.Augment` | 追加実験メニュー (`AddRuns` 等) — `Design.Optimal.augmentDesign` を呼ぶ |
| `Custom.SplitPlot` | 分割実験 (whole plot / sub plot) |
| `Custom.Bayesian` | Bayesian D-optimal (DuMouchel-Jones 1994) |
| `Custom.Power` / `Custom.Compare` | 検出力評価 / 複数計画の比較 |
| `Custom.Coordinate` / `Custom.Structured` / `Custom.RegionMoment` | 座標交換法 / 構造化計画 / 領域モーメント行列 (I-optimal) |

### 解析・評価

| Module | 役割 |
|---|---|
| `Design.Anova` | 分散分析 (要因効果の有意性) |
| `Design.Diagnostics` | 計画の診断 (交絡・エイリアス構造・条件数) |
| `Design.Power` | 検出力・必要実験数の計算 |
| `Design.Quality` | 工程能力指数 (Cp / Cpk 等) |
| `Design.GaugeRR` | Gauge R&R — 測定システム解析 (AIAG MSA 4th ed. 準拠) |

### 逐次・ワークフロー

| Module | 役割 |
|---|---|
| `Design.Sequential` | 逐次 RSM — 最急上昇パス生成と次の CCD 配置 |
| `Design.Workflow` | 計画 → 実験 → 解析 → 次の計画という一連の流れの支援 |

## 単体で使う

計画の生成だけなら、 この package 単体で足りる:

```cabal
build-depends: hanalyze-design
```

```haskell
import Hanalyze.Design.Factorial (twoLevelFactorial, fullFactorial)

main :: IO ()
main = do
  mapM_ print (twoLevelFactorial 3)
  -- [-1.0,-1.0,-1.0] / [-1.0,-1.0,1.0] / … / [1.0,1.0,1.0]  (2³ = 8 run)
  mapM_ print (fullFactorial [[180, 200, 220], [10, 20]])
  -- [180.0,10.0] / [180.0,20.0] / [200.0,10.0] / … / [220.0,20.0]  (3×2 = 6 run)
```

`twoLevelFactorial k` は coded (`±1`) の `2^k` 計画、 `fullFactorial` は
因子ごとの水準リストをそのまま直積するので**実単位のまま**計画表になる。

なお、 通常は umbrella package `hanalyze` を依存に書けば
`import Hanalyze` だけで上記もすべて使える。 層を直接指定するのは
依存を最小化したいときのみで十分。

## 関連 docs

- DoE 入口: [docs/doe/01-doe.ja.md](../docs/doe/01-doe.ja.md) /
  理論: [theory-doe.ja.md](../docs/doe/theory-doe.ja.md)
- 直交表・田口法: [02-orthogonal-taguchi.ja.md](../docs/doe/02-orthogonal-taguchi.ja.md)
- Custom Design: [usage-custom-design.ja.md](../docs/doe/usage-custom-design.ja.md) /
  [manual-custom-design.ja.md](../docs/doe/manual-custom-design.ja.md)
- 実験の追加・分割実験: [usage-augment-splitplot.ja.md](../docs/doe/usage-augment-splitplot.ja.md) /
  低レベル API: [usage-doptimal-augment.ja.md](../docs/doe/usage-doptimal-augment.ja.md)
- Bayesian D-optimal: [usage-bayesian-d.ja.md](../docs/doe/usage-bayesian-d.ja.md)
- 空間充填: [usage-space-filling.ja.md](../docs/doe/usage-space-filling.ja.md) /
  配合計画: [usage-mixture.ja.md](../docs/doe/usage-mixture.ja.md)
- 逐次 RSM: [usage-sequential-rsm.ja.md](../docs/doe/usage-sequential-rsm.ja.md)
- Gauge R&R: [usage-gauge-rr.ja.md](../docs/doe/usage-gauge-rr.ja.md)

← [repository README](../README.ja.md)
