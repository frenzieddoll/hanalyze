# Generalized Regression advanced + Robust Regression (Phase 31)

> 2026-05-29 Phase 31 で追加された **罰則項回帰の advanced 機能**
> (Adaptive Lasso / MCP / SCAD / Group Lasso) と **M-estimator ロバスト回帰**
> (Huber / Tukey biweight) の学習ガイド。 JMP "Generalized Regression" +
> "Robust Fit" 同等。 型シグネチャ・最小例・`df |->`/`toPlot` 経路は
> [api-guide 02-regression](../api-guide/02-regression.md) を一次根拠に、 ここは
> **罰則の数式・推定アルゴリズム・凸性条件の罠** を扱う。

---

## 0. 概観

| 機能 | 役割 |
|---|---|
| Adaptive Lasso | 真スパース性向上 (oracle property、 Zou 2006) |
| MCP | non-convex 罰則、 大係数の bias を Lasso より縮める |
| SCAD | 同上 (Fan-Li 2001、 3 領域 piecewise) |
| Group Lasso | group 単位の選択 (categorical 多水準等) |
| Huber M-estimator | 外れ値混入データの線形回帰 |
| Tukey biweight | 外れ値を完全棄却したいとき |

---

## 1. Adaptive Lasso

重み `w_j = 1 / |β̂_j^OLS|^γ` (γ=1 が典型)。 大きい OLS 推定を持つ列は罰則を
弱める → 真の零係数を Lasso より強く 0 に潰す (oracle property、 Zou 2006)。

**実装**: column reweighting trick で `x_j' = x_j / w_j` に変形 → 標準 Lasso →
解 `β_j = β_j' / w_j` で復元。 追加 CD ループ不要。

---

## 2. MCP (Minimax Concave Penalty)

```
p(β) = λ|β| - β²/(2γ)   if |β| ≤ γλ
     = γλ²/2            if |β| > γλ
```

`γ → ∞` で Lasso に縮退、 `γ → 1` で hard-threshold 寄り。 推奨 `γ = 3-5`。

**前提**: `cSq > 1/γ` (= `Xⱼ` の標準化 + γ ≥ 3 で自動的に満たす)。 違反時は
inner CD が OLS 解で fallback (発散しない)。

---

## 3. SCAD (Smoothly Clipped Absolute Deviation)

3 領域 piecewise threshold:

- `|β| ≤ λ`:        Lasso 領域 (一定の縮小)
- `λ < |β| ≤ aλ`:  SCAD 移行領域 (二次的に減衰)
- `|β| > aλ`:      OLS 領域 (縮小なし)

推奨 `a = 3.7` (Fan-Li 2001)。

---

## 4. Group Lasso

罰則 `λ Σ_g √|g| · |β_g|₂` で group 全体を 0 / non-0 にする。 categorical
多水準 (= dummy 列束) や時系列 lag 群に有用。

**実装**: block coordinate descent (Yuan-Lin 2006 simplified)。 各 group で
部分残差 `r_g = r + X_g β_g` を作り `β_g_new = (1 - λ√|g|/|z_g|₂)_+ · z_g/cSq`。

---

## 5. Robust Regression (Huber / Tukey biweight)

IRLS アルゴリズム:

1. β を OLS で初期化
2. 残差 → MAD-based ロバストスケール `σ̂`
3. `u_i = r_i / σ̂` から重み `w_i` (Huber か Tukey)
4. 加重 LS: `β ← (X^T W X)^{-1} X^T W y`
5. 収束まで反復

| Estimator | 重み関数 | 特徴 |
|---|---|---|
| `Huber k` (k=1.345) | `1` if `|u|≤k`、 `k/|u|` else | 線形 + 線形クリップ、 滑らか |
| `Tukey c` (c=4.685) | `(1-(u/c)²)²` if `|u|≤c`、 `0` else | 外れ値完全棄却、 多峰目的関数 (OLS 初期化必須) |

系列の末尾に外れ値を 1 点入れると OLS 線はそちらへ引っ張られるが、 Huber
ロバスト回帰は大多数のデータが示す傾きを保つ:

![外れ値 1 点に対する Huber ロバスト回帰と OLS の対比](../images/robust-vs-ols.svg)

---

## 6. 想定外の振る舞いに注意

### MCP / SCAD の凸性条件

`cSq ≤ 1/γ` (MCP) や `cSq ≤ 1/(a-1)` (SCAD) を満たすと non-convex 最適化で
複数極小点があり、 inner CD は OLS 解で fallback する。 確実に欲しい挙動が
出ない場合は **X を標準化** + **`γ ≥ 3` / `a ≥ 3.7`** にする。

### Tukey biweight の初期値依存性

完全棄却 (重み 0) の領域があるため目的関数は多峰。 OLS で初期化する本実装
では合理的に動くが、 真値から大きく離れた pilot は誤った局所解に収束する
可能性。 心配なら **Huber 結果を初期値に Tukey を再 fit** する 2-stage。

### Adaptive Lasso の `w_j = 0`

`w_j = 0` は実装上「列 j を消す (`β_j = 0` 強制)」 として扱う。 罰則を完全
ゼロにしたいなら `w_j = 1e-8` 等の微小正値にする。

---

## 7. 関連

- 型・最小例・`df |->`/`toPlot` 経路: [api-guide 02-regression](../api-guide/02-regression.md)
- 計画書: `specification/phases/phase-31-regression-advanced.md`
- 文献:
  - Zou (2006) JASA 101 — Adaptive Lasso
  - Zhang (2010) Ann. Stat. 38 — MCP
  - Fan-Li (2001) JASA 96 — SCAD
  - Yuan-Lin (2006) JRSSB 68 — Group Lasso
  - Breheny-Huang (2011) Ann. Appl. Stat. 5:232-253 — non-convex CD 更新式
  - Huber (1964) / Tukey (1977) / Rousseeuw-Leroy (1987)
- 比較先: R `glmnet` (adaptive)、 `ncvreg` (MCP/SCAD)、 `grpreg`、
  `MASS::rlm`、 JMP "Generalized Regression" / "Robust Fit"
- 別 Phase 候補: Dantzig Selector (LP 依存)、 LTS (組合せ最適化、 FAST-LTS)
</content>
