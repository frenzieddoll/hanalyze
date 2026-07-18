# 時系列・生存解析の拡張 (Phase 35: GARCH / VAR / Competing Risks / RBD)

> Phase 35 (2026-05-29) は、 既存の `Hanalyze.Model.{TimeSeries, Survival,
> Weibull, Reliability}` でカバーされていない時系列・生存解析の advanced
> 機能 4 個を追加する学習ガイド。 型シグネチャ・最小例・`toPlot` 経路は
> [api-guide 06-timeseries](../api-guide/06-timeseries.md) /
> [07-survival](../api-guide/07-survival.md) を一次根拠に、 ここは **各モデルの
> 定式化と推定の根拠** を扱う。 State Space / Kalman Filter は
> `Hanalyze.Model.StateSpace` (Phase 15) で既実装。

---

## 0. モジュール対応

| 機能 | 備考 |
|---|---|
| GARCH(1,1) | Gaussian QMLE + L-BFGS |
| VAR(p) | 方程式別 OLS |
| 競合リスク (CIF) | Kalbfleisch-Prentice |
| 信頼性ブロック図 | 直列 / 並列 / k-of-n |
| State Space / Kalman | **Phase 15 既実装** |

---

## 1. GARCH(1,1) (35-A1)

- モデル: `σ²_t = ω + α · ε²_{t-1} + β · σ²_{t-1}`, `ε_t = y_t - μ`
- 定常性制約 (`ω > 0, α ≥ 0, β ≥ 0, α + β < 1`) は再パラメタ化で回避
  (ω に softplus、 α/β に stick-breaking sigmoid 2 個)
- `gLogLik` は最大化された Gaussian 対数尤度。 長期予測は無条件分散
  `ω / (1 - α - β)` に収束

推定された条件付きボラティリティは大きな収益率のクラスタリングを追従する:

![収益率系列に重ねた GARCH(1,1) の条件付きボラティリティ帯](../images/garch-volatility.svg)

---

## 2. VAR(p) (35-A2)

- モデル: `yₜ = c + Σ_l Aₗ · yₜ₋ₗ + εₜ`、 各 `Aₗ` は `K × K`
- 方程式別 OLS で推定 — Gaussian イノベーション下では全方程式が同じ
  回帰子を共有するため SUR = OLS となり MLE (Lütkepohl 2005, §3.2)
- `varResiduals` は `(n − p) × K`、 `varSigma` は残差共分散

---

## 3. 競合リスク / CIF (35-A3)

- `crCause = 0` は打ち切り、 `≥ 1` は特定の cause からの failure
- 推定量: `F̂_k(t) = Σ_{t_i ≤ t} Ŝ(t_i⁻) · d_{k,i} / n_i`
  (Ŝ は全 cause を event 扱いした overall KM、 Kalbfleisch & Prentice 1980)
- 重要: 「cause 別データに 1 − KM」 という素朴な手法は上方バイアスを持つ。
  この CIF はその古典的補正
- 任意 event time で `Σ_k F̂_k(t) + Ŝ(t) = 1` が成立

パラメトリック生存解析の対応物として、 AFT モデルは共変量で位置がシフトする
滑らかな生存曲線 `S(t | x)` を与える (`fitAFT`・`aftSurvivalAt` で任意 x の曲線):

![AFT パラメトリック生存曲線 S(t|x)](../images/aft-survival.svg)

---

## 4. 信頼性ブロック図 (35-A4)

- `Leaf p` — 信頼度 `p ∈ [0, 1]` のコンポーネント
- `Series bs` — `∏ Rᵢ`、 全 block が動作する必要あり
- `Parallel bs` — `1 − ∏ (1 − Rᵢ)`、 1 個でも動けば OK
- `KofN k bs` — `n` 個のうち少なくとも `k` 個が動作。 異種信頼度では
  Poisson-binomial DP で正確計算 (二項分布は同質特例)
- block 間の故障独立性は前提 (textbook RBD)

---

## 5. State Space / Kalman (言及のみ)

`Hanalyze.Model.StateSpace` は Phase 15 で実装済 (`kalmanFilter` /
`kalmanSmoother`)。 具体例は `test/Spec.hs:6443` を参照。

---

## 6. 関連

- 型・最小例: [api-guide 06-timeseries](../api-guide/06-timeseries.md) /
  [07-survival](../api-guide/07-survival.md)
- 計画書: `specification/phases/phase-35-timeseries-survival.md`
- 文献: Lütkepohl (2005) — VAR / Kalbfleisch & Prentice (1980) — CIF
</content>
