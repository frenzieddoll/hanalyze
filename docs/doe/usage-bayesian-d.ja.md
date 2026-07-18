# Bayesian D-optimal Design の使い方

> 🌐 [English](usage-bayesian-d.md) | **日本語**

> Phase 24 / 25 の Custom Design を前提に、 DuMouchel-Jones (1994) の Bayesian
> modification + Compound (alphabetic) criterion + 多変量 Cp の組み合わせ評価を扱う。
> 型シグネチャ・最小例は [api-guide 09-doe](../api-guide/09-doe.ja.md) を一次根拠に、
> ここは **K 行列の設計思想・DJ §2.2 規約・Compound 合成の注意** を扱う。
>
> 仕様: `specification/spec/hanalyze-doe-custom-design-spec.md` v0.1.1 §2.7
> 関連 Phase: 26 / 前提: Phase 24 (Core) + 25 (SplitPlot / Augment) 完了

## 1. Bayesian D とは

通常の D-opt は `det(XᵀX)` を最大化する。 二次項や交互作用が「実は無視できる
かもしれない」 という事前情報を入れたいとき、 prior precision K を使った
**Bayesian D** `max det(XᵀX + K)` を最大化する。 K は p × p の対角行列で、
各列に対応するパラメータの **事前精度** (= 1/事前分散) を表す。
DuMouchel-Jones の典型:

| 項の種類 | K_jj |
|---|---|
| intercept    | 0   |
| 主効果       | 0   |
| 2 因子交互作用 | τ² |
| 二次項       | τ² |
| nested       | τ² |

τ² ≈ 1.0 (coded space 想定) が標準的な開始値。 `priorPrecisionDefault` がこの
規約で K を構築し、 term ごとに違う精度を入れたいときは `priorPrecisionFromTerms`
に term→precision の関数を渡す。 K = 0 で classic D に完全縮退する
(`bayesianDValueM (PriorPrecision 0) x == det(X'X)`・後方互換性確認済)。

## 2. Compound (Alphabetic) Criterion

`Compound [(weight, criterion)]` で複数 criterion を重み付き和にする。 重みは正を
想定し、 負値や合計 ≠ 1 は `Compare.normalizeCompoundWeights` で正規化
(負は 0 にクリップして再正規化)。

注意: inner criterion のスケールはユーザが揃える責任あり (例: D は det、
A は 1/trace なので単位次元が違う)。 efficiency 形 ([0, 1] 範囲) に正規化
してから渡すと意味のある重み付け和になる。

## 3. 多変量 Process Capability (Cp) との連携

Phase 23-d の `Design.Quality.processCapabilityMultivariate` (Mahalanobis ベース
MCp / MCpk) は、 Custom Design で生成した設計に観測 y (多変量 response) を当てた
後の **post-hoc 評価** に使う。 `Compare.compareDesigns` 側の自動統合はスコープ外
(Cp は y 観測が必要だが Compare は design (x) のみを受け取るため、
`compareDesignsWithResponses` 的なシグネチャ拡張が要る。 canvas 連携の要件確定後)。

## 4. 既知の制限

- K は対角を想定 (非対角を入れても動作するが、 解釈は工夫が要る)
- Bayesian D の D-efficiency (Compare の dcEffTable D 列) は **classic D** で
  計算される。 Bayesian D 同士の比較は `crCriterionValue` を直接見る
- Compound の重み正規化は線形和。 幾何平均ベース (= log-合成) は将来検討
- 多変量 Cp の Compare 統合は将来 commit (要 API 拡張)

## 5. DuMouchel-Jones §2.2 規約 (Phase 28-12)

DJ (1994) §2.2 は、 prior τ² が「effect size 1σ_error」 と等価に解釈される
よう、 potential terms (`TInter` len≥2 / `TPower` k≥2 / `TNested`) に以下の
変換を要求する:

1. **centering**: 候補集合上で平均を引く
2. **primary との直交化**: primary 列 (TIntercept / TMain / TInter len 1) に
   LS 直交化
3. **range = 1 正規化**: 直交化後の (max − min) で割る

paper §2.2 末尾の例 (primary {1, x}、 候補 {-1, -0.5, 0, 0.5, 1}):
- `TPower x 2` → `z₁ = x² − 0.5`
- `TPower x 3` → `z₂ = (x³ − 0.85x)/0.6`

hanalyze の `TPower` / `TInter` は **生の量** (例: 生 x²、 生 xy) を
返すため、 同じ K を当てるだけだと `det(X'X + K)` は DJ の意図とは異なる量を
計算する。 文献値と一致させたい場合は `djTransformColumns` (候補集合から fit、
設計 X に apply) で列変換する (`djFitTransform` / `djApplyTransform` で fit と
apply を分離可能 = 同じ候補集合で複数設計を比較するとき係数を再利用)。

**自動適用 (Phase 28-12 完了)**: `CustomDesignSpec` に `cdsDJConvention = True`
を渡すと `coordinateExchange` が候補集合から DJ 変換を自動 fit し、 内部の
criterion 評価で expand 後に `djApplyTransform` を適用する。 `cdMatrix` は raw
表現のまま、 `crCriterionValue` は DJ 変換後の −det。 手動呼び出しは
**post-hoc に既存設計を評価する** とき (例: 文献設計の比較) にのみ必要。

ベンチ実証 (bench/custom-design/REPORT.md §27-3): 自動 DJ 適用後、
DuMouchel-Jones (1994) Example 3 "Both" 設計と hanalyze 設計の
det(X_t' X_t + K) = **完全一致 (ratio = 1.0000)**、 paper の意図する
criterion で同等品質の設計を発見。
</content>
