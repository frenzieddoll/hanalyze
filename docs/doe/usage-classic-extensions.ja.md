# DoE 古典側の拡張機能 (Phase 23)

> 既存の DoE 機能 (`Hanalyze.Design.*`) に **JMP / Spotfire 同等の高度な機能** を
> 追加した分。 Custom Design (Phase 24+) の前提でもある。 型シグネチャ・最小例は
> [api-guide 09-doe](../api-guide/09-doe.md) を一次根拠に、 ここは **各機能の意図と
> 注意点** を扱う。
>
> 仕様: `specification/spec/hanalyze-doe-spec.md` v0.2
> Phase: 23-a / 23-b / 23-c / 23-d (全 4 commit、 既に develop マージ済)

含まれる拡張:

1. **OptCriterion 拡張** — G-optimal + Compound (alphabetic)
2. **Constraint モジュール新規** — Linear / Forbidden で candidate set を事前 filter
3. **非正規 Process Capability** — Gamma 分布の Cp + 統一エントリ `NonNormalFit`
4. **多変量 Process Capability** — Mahalanobis ベース MCp / MCpk + InSpecRate

---

## 1. OptCriterion 拡張

- **G-optimal**: max leverage を最小化 (= 設計内の最悪予測分散を最小化)。 ここでは
  **self-G 定義** (= 設計自身の hat 対角の最大)。 候補空間全体の max prediction
  variance は Custom Design 側で扱う
- **Compound (alphabetic)**: 複数 criterion の重み付き和を 1 criterion として扱う。
  inner criterion はスケールが揃っていないと意味のある合成にならない (D は det,
  A は trace、 単位が違う)。 efficiency 形 ([0, 1]) に変換してから渡すのが安全。
  重み正規化が必要なら Phase 26 の `Compare.normalizeCompoundWeights`

---

## 2. Constraint モジュール (古典 Fedorov 用)

- 連続因子の **線形不等式** で候補集合を事前削減 (`x1 + x2 ≤ 1` 等)
- カテゴリ含む **完全一致禁止** (`Forbidden`) で候補からピンポイント除外
- 違反行を `checkDesign`、 違反候補のフィルタを `filterCandidates`

注意:
- Linear 制約は連続 / 離散数値因子のみ参照可能 (categorical は Forbidden で表現)
- Custom Design (Phase 24) の `Design.Custom.Constraint` は別 ADT で、 本モジュールは
  あくまで「candidate set ベース」 の古典 Fedorov 用

---

## 3. 非正規 Process Capability

- **Gamma 分布** の Cp/Cpk: 強い右歪みデータの工程能力推定
- **`NonNormalFit` 統一 dispatch**: 「Box-Cox / Johnson SU / Gamma のどれが適合
  するか分からない」 ときに AIC で最良 fit を自動選択

注意:
- Gamma は **正値データ** が前提 (負値があるとシフトが必要)
- `NonNormalFit` の自動選択は AIC ベース、 サンプル数が小さい (< 30) と
  選択が不安定。 ドメイン知識で分布を固定するのが安全な場合あり

---

## 4. 多変量 Process Capability

複数応答 (y1, …, yk) の同時工程能力:
- MCp: Mahalanobis 距離による広がり指標 (Wang-Hubele-Lawrence 風)
- MCpk: 中心オフセット penalty 込みの指標 (`MCpk ≤ MCp` が成立)
- InSpecRate: spec 内に入る確率の経験値 ∈ [0, 1]

注意:
- 入力 y は **多変量正規** を仮定 (深く外れる場合は別アプローチ要)
- spec の長さ = y の列数 が必須 (不一致は `Left`)

---

## 関連リンク

- 型・最小例: [api-guide 09-doe](../api-guide/09-doe.md)
- 上流 (古典 DoE 全体): [01-doe.ja.md](01-doe.ja.md)
- Custom Design Core (Phase 24): [usage-custom-design.ja.md](usage-custom-design.ja.md)
- 理論: [theory-doe.ja.md](theory-doe.ja.md)
</content>
