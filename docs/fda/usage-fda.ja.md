# Functional Data Analysis (FDA) (Phase 33)

> 2026-05-29 Phase 33 で `Hanalyze.Model.FDA` を新規追加。 センサ / プロセス
> 時系列を **1 観測 = 1 関数**として扱う Ramsay-Silverman 流の FDA 基礎。
> 由来 gap #16 (元 17 件中最後の残) を消化、 **全 gap 解決**。 型シグネチャ・最小例は
> [api-guide 04-multivariate](../api-guide/04-multivariate.ja.md) を一次根拠に、 ここは
> **平滑化解の導出・mass matrix・knot 規約の罠** を扱う。

---

## 0. 概観

| 機能 | 用途 |
|---|---|
| basis 平滑化 (`smoothBasis`) | B-spline + P-spline penalty で関数推定 |
| 評価 (`evalFunctional`) | smooth した関数を任意 grid で評価 |
| FPCA (`functionalPCA`) | 関数主成分 (= 関数空間の SVD) |
| 関数線形回帰 (`fLM`) | y_i = α + ∫ x_i(t) β(t) dt + ε |

basis は `BSpline degree knots` のみ実装。 Fourier basis は別 Phase 候補。

---

## 1. 平滑化 (smoothBasis) の解

解: `c = (BᵀB + λ DᵀD)⁻¹ Bᵀy`、 ここで `D` は二階差分作用素 (Eilers-Marx
1996)。 `λ → 0` で interpolate、 `λ → ∞` で over-smooth。

---

## 2. Functional PCA

basis 係数行列の covariance に PCA、 主成分関数を grid 上で評価する。 B-spline +
dense grid なら直交近似で十分実用。 厳密 mass-matrix 版は将来拡張。 平均関数と
上位固有関数はいずれも grid 上の曲線で、 重ねて描くと平均形状と主要な変動モードが
一目で分かる:

![FPCA の平均関数と上位固有関数 (PC1/PC2/PC3 + mean)](../images/fda-fpca.svg)

---

## 3. Functional Linear Regression (fLM)

モデル: `y_i = α + ∫ x_i(t) β(t) dt + ε`。 β を同 basis で展開、 mass matrix
`J ≈ trapezoidal(BᵀB)` 経由で OLS、 β に二階差分 penalty を掛ける。

---

## 4. 想定外の振る舞いに注意

### bsplineBasis の knot 列は境界を含む

`Hanalyze.Model.Spline.bsplineBasis` の knot 列は `[t_min, .., t_max]` を含む
全 knot 列。 内部 knot だけ渡すと dimension 不整合になる (= Phase 33 着手時に
ハマった点)。

### `n_basis` の選び方

`n_basis = length knots + degree - 1`。 `degree=3`、 knots 12 個なら 14 basis。
データの曲率に応じて basis 数を増やし、 `λ` で over-fit を抑える組み合わせが
標準 (Ramsay-Silverman 2005 §5)。

### FPCA は basis 直交近似で OK か?

B-spline + dense grid (= サンプリング間隔 ≪ basis 間隔) なら近似誤差小。
**粗い grid** や **少 basis** だと厳密 mass matrix 重み付き SVD が必要。
本実装は前者前提、 後者は将来拡張。

### fLM の x と β が直交だと R² 0

`∫ x_i(t) β(t) dt = 0` が常に成立すると y_i = α + noise になり R² ≈ 0。
DGP 設計時に **β と相関する成分** が x に含まれているか確認 (= 着手時の
バグ判定で踏んだ罠)。

---

## 5. 関連

- 型・最小例: [api-guide 04-multivariate](../api-guide/04-multivariate.ja.md)
- 計画書: `specification/phases/phase-33-fda.md`
- 既存依存: `Hanalyze.Model.Spline.bsplineBasis` (B-spline basis 生成)
- 文献:
  - Ramsay & Silverman (2005) "Functional Data Analysis" 2nd ed.
  - Eilers, Marx (1996) "Flexible smoothing with B-splines and penalties"
    Statist. Sci. 11:89-121.
- 比較先: R `fda` package、 Python `scikit-fda`
</content>
