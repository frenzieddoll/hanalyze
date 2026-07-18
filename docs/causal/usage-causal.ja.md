# 因果推論: Propensity Score / IPW / DR / CATE (Phase 30)

> 2026-05-29 Phase 30 で追加された **観測データからの因果効果推定** モジュール
> の学習ガイド。 Rubin causal model に基づき、 共変量で交絡を補正した
> ATE / ATT / CATE を推定する。 型シグネチャ・最小例は
> [api-guide 08-causal](../api-guide/08-causal.md) を一次根拠に、 ここは
> **推定量の導出・前提・罠** を扱う。

---

## 0. 概観

| 機能 | 役割 |
|---|---|
| Propensity Score | `p(X) = P(T=1\|X)` を logistic で推定 + trim で重み発散防止 |
| IPW | Hajek 正規化 ATE / ATT (Horvitz-Thompson より低分散) |
| Doubly Robust (AIPW) | 結果モデル + PS 両方を使い、 どちらか片方正しければ一致 |
| CATE meta-learners | 異質 treatment effect 推定 (S / T / X-learner、 base = LM \| RF) |

IPW / DR / CATE は内部で propensity score + `defaultPSTrim = (0.01, 0.99)` を
自動適用する (= 重み発散しない)。 効果推定は **因果 DAG が既知**であることを
前提とする。 構造そのものが未知の場合は、 LiNGAM 等の因果探索で観測データから
有向グラフを推定する。 下図は LiNGAM が推定した DAG (`x0 → x1 → x2`) の例で、
推定量で用いる交絡変数集合の根拠を与える。

![LiNGAM が推定した因果 DAG (x0 → x1 → x2)](../images/lingam-dag.svg)

---

## 1. IPW の推定量 (Hajek 正規化)

```
ATE_Hajek = Σ(T·Y/p) / Σ(T/p)  -  Σ((1-T)·Y/(1-p)) / Σ((1-T)/(1-p))
ATT_Hajek = Σ(T·Y) / Σ T       -  Σ((1-T)·p/(1-p)·Y) / Σ((1-T)·p/(1-p))
```

分母で重みを正規化する Hajek 推定量は、 重みの和を 1 に揃えるため
Horvitz-Thompson (分母を `n` 固定) より finite-sample で stable。 これが既定。

---

## 2. Doubly Robust (AIPW) の二重ロバスト性

群別 OLS で `μ̂_1(X)` / `μ̂_0(X)` を fit し、 PS で残差補正する:

```
ATE_AIPW = (1/n) Σ [ μ̂_1(X_i) - μ̂_0(X_i)
                    + T_i (Y_i - μ̂_1(X_i)) / p̂_i
                    - (1-T_i) (Y_i - μ̂_0(X_i)) / (1 - p̂_i) ]
```

**二重ロバスト性**: outcome model か PS のどちらか一方が正しければ ATE は一致
(両方とも正しい必要なし)。 結果モデルは線形 OLS (`Model.LM`) を流用するので、
非線形が必要なら呼び出し側で X を拡張 (二次項等を column 追加) するか、 CATE
module で RF base learner を使う。

---

## 3. CATE meta-learner の使い分け

異質 treatment effect `τ(X) = E[Y(1) - Y(0) | X]` を推定する 3 方式:

| | アルゴリズム | 強み | 弱み |
|---|---|---|---|
| **S-learner** | 単一モデル on (X, T) | サンプル効率 (1 model) | T の影響が薄れがち、 LM だと interaction 無いと constant CATE |
| **T-learner** | 群別 fit μ_1, μ_0 | 異質性をそのまま回復 | 群サイズが偏ると分散大 |
| **X-learner** | T-learner の残差を再回帰 + PS 重み平均 | 群サイズ偏在に強い | 4 sub-model 必要、 推定 step 増 |

詳細: Künzel, Sekhon, Bickel, Yu (2019) PNAS 116:4156-4165.

---

## 4. 想定外の振る舞いに注意

### PS が 0 / 1 に張り付くケース

共変量に分離面があると `p_i` が 0 / 1 に張り付き、 IPW 重みが発散する。
`trimPropensity 0.01 0.99` で必ずクリップしてから使う (= `ipw` / `doublyRobust`
は内部で自動適用)。 trim でも分散が大きいときは positivity assumption が
事実上崩れているので、 ATT / overlap 領域に絞った推定に切り替える検討を。

### no unmeasured confounders 仮定はユーザ責任

backend は DAG 仮定の妥当性を検証しない。 共変量 X が交絡を完全に閉じている
ことが前提。 重要な変数を抜くと推定は biased になる。 sensitivity analysis
(Rosenbaum bound 等) は本 phase の範囲外。

### S-learner with LM の罠

S-learner で LM base + interaction 項なしだと、 CATE は constant (=
intercept-shift) になる。 異質性を見たいなら T / X-learner、 または S-learner
+ X·T interaction 列を手動で X に追加する。

---

## 5. 関連

- 型・最小例: [api-guide 08-causal](../api-guide/08-causal.md)
- 計画書: `specification/phases/phase-30-causal.md`
- 文献:
  - Rosenbaum & Rubin (1983) Biometrika 70:41-55. (Propensity Score)
  - Horvitz & Thompson (1952) JASA 47:663-685. (IPW)
  - Robins, Rotnitzky, Zhao (1994) JASA 89:846-866. (AIPW)
  - Künzel et al. (2019) PNAS 116:4156-4165. (Meta-learners)
- 比較先: R `MatchIt` / `WeightIt` / `tmle`、 Python `econml`、 `DoWhy`
</content>
</invoke>
