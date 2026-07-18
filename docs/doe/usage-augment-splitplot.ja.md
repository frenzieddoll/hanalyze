# Custom Design: Augment + Split-Plot の使い方

> 🌐 [English](usage-augment-splitplot.md) | **日本語**

> Phase 24 の Custom Design Core (`Hanalyze.Design.Custom.*`) を前提に、
> 既存設計の **増補 (Augment)** と **分割実験 (Split-Plot)** を扱う。 型シグネチャ・
> 最小例は [api-guide 09-doe](../api-guide/09-doe.ja.md) を一次根拠に、 ここは
> **各メニューの意味論・REML 情報行列・既知制限** を扱う。
>
> 仕様: `specification/spec/hanalyze-doe-custom-design-spec.md` v0.1.1
> §2.5 / §2.6 / 関連 Phase: 25-3 〜 25-9 / 前提: Phase 24 完了

## モジュール早見表

| モジュール | 役割 |
|---|---|
| `Design.Custom.Augment`   | `augmentMenu` (Replicate / AddCenter / AddAxial / AddRuns / Foldover) |
| `Design.Custom.SplitPlot` | `generateSplitPlot` (HardToChange 因子の REML D-opt) |

---

## 1. Augment 5 メニューの意味論

- `Replicate k` — 既存 design を k 回複製
- `AddCenter n` — 中心点 (全連続 = 0) を n 行追加
- `AddAxial α` — ±α の axial 点を全連続因子で追加
- `AddRuns N` — 既存設計に候補集合から N 行追加
- `Foldover kind` — 符号反転コピー。 `FullFoldover` は各行の連続因子符号を
  全反転した行を追加 (= 行数 2 倍)、 `PartialFoldover ["x1"]` は x1 のみ反転

### 制限

- `cdsInitial` が `Nothing` の場合は **`Left`** (既存 design 必須)
- `AddAxial` は **coded space ([-1, 1]) 想定**。 raw 単位での α 指定は呼び出し側で
  スケーリングすること
- `AddRuns` は候補集合 = 連続因子 ±1 corners + categorical 全 level の cartesian
  product。 因子数が多いと候補爆発するので、 高次元では使用注意

---

## 2. Split-Plot の REML 情報行列

`fRole = HardToChange` の因子は 1 whole-plot (WP) 内で同じ値を取る (= 段取り回数
= WP 数)。 `SplitPlotConfig { spcNWhole, spcVarRatio=η }`。 Goos-Vandebroek (2003)
の D-opt は:

```
I_β = Xᵀ M⁻¹ X,   M = I + η · Z Zᵀ
```

ここで Z は WP indicator (n × n_WP)、 η = σ²_WP / σ²。
- η = 0 で通常の D-opt に縮退
- η → ∞ で WP factor の重みが激減 (= WP 内推定不能)

本実装は X̃ = chol(X' M⁻¹ X) を `critValueM` (DOpt の det) で評価する **簡易版**。
標準の Goos-Vandebroek 形と方向は一致するが、 厳密な criterion 値の絶対比較は
妥当でない (relative 比較のみ意味あり)。 `spdWholePlotId` で各 run の WP 帰属
(例 `[0,0,0,1,1,1,...]`) を返す。

### 制限

- **VeryHardToChange (strip-plot) は `Left`** (将来対応)
- **Categorical HardToChange 因子は `Left`** (GLMM 連携と一緒に将来 commit)
- spcNWhole は **ユーザ必須指定** (推論しない、 spec §2.5 の流儀)
- spcVarRatio (η) のデフォルト 1.0 は実験ドメインによっては不適切。 既知の
  分散比があれば設定推奨

---

## 3. 設計フロー (Augment → Split-Plot)

1. 通常の custom design を `coordinateExchange` で生成
2. 不足を感じたら `cdsInitial = Just (cdMatrix cd)` にして `augmentMenu` で中心点等を追加
3. 後続バッチで HardToChange を考慮した `generateSplitPlot` で分割実験を組む

---

## 既知の制限 (Phase 25 全体、 spec で明示)

- Strip-plot (VeryHardToChange) は未対応
- Categorical WP / Categorical strip-plot は未対応
- Foldover の categorical 因子は flip しない (符号の概念無し)
- Conditional 制約の NOT は未対応 (AND/OR 正論理のみ、 spec で明記)

Phase 26 で Bayesian-D (DuMouchel-Jones) と Compound criterion 強化
([usage-bayesian-d](usage-bayesian-d.ja.md))。
</content>
