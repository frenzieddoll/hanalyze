# 因果推論

> [📚 索引](README.md) ｜ [01 quickstart](01-quickstart.md) ｜ [02 regression](02-regression.md) ｜ [03 bayesian-hbm](03-bayesian-hbm.md) ｜ [04 multivariate](04-multivariate.md) ｜ [05 ml](05-ml.md) ｜ [06 timeseries](06-timeseries.md) ｜ [07 survival](07-survival.md) ｜ **08 causal** ｜ [09 doe](09-doe.md) ｜ [10 stat](10-stat.md) ｜ [11 data](11-data.md) ｜ [12 plot](12-plot.md)

傾向スコア・IPW・二重頑健・CATE と、 因果探索 (LiNGAM)。 推定量は数値結果を返す
(`toPlot` 非対象)、 LiNGAM のみ DAG を `toPlot` で描ける。 理論は
[usage-causal](../causal/usage-causal.ja.md) が一次根拠。

| 手法 | 関数 (`Hanalyze.Stat.Causal.*`) | 結果型 |
|---|---|---|
| 傾向スコア | `propensityScore x t` | `PropensityScore` |
| IPW (Hajek) | `ipw x t y` | `IPWResult` |
| 二重頑健 (AIPW) | `doublyRobust x t y` | (ATE 推定) |
| CATE (meta-learner) | `fitCATE learner base …` | (CATE 推定) |
| 因果探索 LiNGAM (7 variant) | `df \|-> directLingam cfg cols` 他 (下記) | `LiNGAMFitted _` (Plottable・名前付き DAG) |

---

## 傾向スコア / IPW / 二重頑健

```haskell
propensityScore :: LA.Matrix Double -> LA.Vector Double -> PropensityScore
--                 共変量 X            処置 T (0/1)
ipw             :: LA.Matrix Double -> LA.Vector Double -> LA.Vector Double -> IPWResult
--                 X                   T                   結果 Y
doublyRobust    :: LA.Matrix Double -> LA.Vector Double -> LA.Vector Double -> DoublyRobustResult
```

```haskell
import qualified Hanalyze.Stat.Causal.PropensityScore as PS
import qualified Hanalyze.Stat.Causal.IPW             as IPW
import qualified Hanalyze.Stat.Causal.DoublyRobust    as DR

let ps = PS.propensityScore x t        -- ロジスティック GLM で p(X)=P(T=1|X)
    r  = IPW.ipw x t y                  -- Hajek 正規化 ATE/ATT
print (IPW.ipwATE r, IPW.ipwATT r)
let rDR = DR.doublyRobust x t y         -- 結果モデル + PS (どちらか正しければ整合)
print (DR.drATE rDR)
```

結果フィールド・補助関数:

| 関数 / フィールド | 役割 |
|---|---|
| `PS.psBeta ps` / `PS.psScores ps` | logistic 係数 / 長さ `n` の推定確率 `p̂(X)` |
| `PS.trimPropensity 0.01 0.99 ps` | スコアを `[0.01, 0.99]` にクリップ (重み発散防止・**推奨**) |
| `PS.ipwWeights ps' t` | ATE 用 IPW 重み vector |
| `IPW.ipwATE r` / `IPW.ipwATT r` | ATE / ATT 推定値 |
| `DR.drATE rDR` | AIPW の ATE 推定値 |

`ipw` / `doublyRobust` は内部で PS 推定 + `defaultPSTrim = (0.01, 0.99)` を自動適用する。
既存 PS を再利用するなら `IPW.ipwWith ps' t y` / `DR.doublyRobustWith …`。

> IPW は Hajek 正規化が既定 (Horvitz-Thompson より低分散)。 推定量の式・positivity 仮定の
> 崩れ方は [usage-causal](../causal/usage-causal.ja.md) が一次根拠。

---

## CATE (条件付き平均処置効果)

```haskell
fitCATE  :: CATELearner -> CATEBaseLearner
         -> LA.Matrix Double -> LA.Vector Double -> LA.Vector Double   -- X, T, Y
         -> MWC.GenIO -> IO CATEResult                                 -- RNG → IO
-- CATELearner     = SLearner | TLearner | XLearner   (meta-learner)
-- CATEBaseLearner = CATELM | CATERF RFConfig          (基底学習器)
```

```haskell
import qualified Hanalyze.Stat.Causal.CATE as CATE
import qualified Hanalyze.Model.RandomForest as RF
import qualified System.Random.MWC as MWC

gen <- MWC.create
r  <- CATE.fitCATE CATE.TLearner CATE.CATELM x t y gen          -- LM 基底
let rfCfg = RF.defaultRFConfig { RF.rfTrees = 100 }
r' <- CATE.fitCATE CATE.XLearner (CATE.CATERF rfCfg) x t y gen  -- RF 基底 (非線形)
print (CATE.cateATE r)               -- 全体平均
LA.toList (CATE.cateEstimates r)     -- per-unit τ̂_i
```

S/T/X-learner を基底学習器 (`CATELM` | `CATERF`) に組み合わせる。 3 learner の使い分け
(サンプル効率 vs 異質性回復) と S-learner+LM の constant-CATE 罠は
[usage-causal](../causal/usage-causal.ja.md) が一次根拠。

---

## 因果探索 (LiNGAM)

LiNGAM は誤差が**独立な非ガウス**の線形 SEM を仮定し、観測データから**因果の向き**(DAG)を
同定する。他のモデルと同じ高レベル `df |-> *Lingam cfg cols` で学習でき、結果 `LiNGAMFitted _` は
`Plottable` なので `toPlot` で**実変数名の DAG** を描く(変数名は `cols` から付く)。

```haskell
import Hanalyze.Plot (directLingam, toPlot, (|->), (|>>))
import Hanalyze.Model.LiNGAM.Direct (defaultDirectLiNGAMConfig)

let fit = df |-> directLingam defaultDirectLiNGAMConfig ["smoking","tar","cancer"]
saveSVGBound "lingam.svg" $ noDf |>> toPlot fit
```

![DirectLiNGAM 推定 DAG (実変数名)](../images/lingam-dag.svg)

**相関 ≠ 因果**: ペアプロットで周辺相関を見ると多くの対が相関するが、LiNGAM の DAG には
**直接因果の辺だけ**が出る(mediator を挟む間接相関・交絡は辺にならない)。

![ペアプロット(周辺相関) vs 直接因果](../images/lingam-pairs.svg)

相関でも「`|r| > 閾値` の対を辺にする」 グラフは描ける(`df |-> correlationOf thr cols`)。ただし
**間接相関・交絡もすべて辺になり過剰に密**(下図は 12 辺)。LiNGAM は非ガウス性を使ってこれを
**直接因果だけに削減し向きも同定**する(上の DAG は 7 辺)。

```haskell
import Hanalyze.Plot (correlationOf, toPlot, (|->), (|>>))

noDf |>> toPlot (df |-> correlationOf 0.3 ["genetics","diet","exercise","bmi","bp","chd"])
```

![相関グラフ(|r|>0.3・12 辺)= 間接相関も辺に](../images/corr-graph.svg)

### 7 variant

用途に応じ 7 種を高レベル API で使える(すべて `LiNGAMFitted` を返し `toPlot` で DAG)。

| variant | 高レベル API | 特徴 / 図 |
|---|---|---|
| Direct | `directLingam cfg cols` | 基本(因果順序探索・Shimizu 2011) |
| Parce | `parceLingam cfg cols` | bottom-up sink 探索・潜在交絡に頑健 |
| ICA | `icaLingam cfg cols` | ICA ベース(Shimizu 2006・旧手法ゆえ精度は Direct 未満) |
| VAR | `varLingam cfg cols` | 時系列(同時刻 + **時間ラグ DAG** `x_j[t-l]→x_i[t]`) |
| MultiGroup | `multiGroupLingam cfg cols groupCol` | 複数群の**共通** DAG(数値群コード列で分割) |
| Pairwise | `pairwiseLingam thr xcol ycol` | 2 変数の**向き**のみ(強い単方向で有効) |
| Bootstrap | `bootstrapLingam cfg cols` | エッジ**確信度**(B 回リサンプルの出現確率) |

```haskell
-- VAR: 時系列因果 (時間ラグ DAG)
noDf |>> toPlot (df |-> varLingam defaultVARLiNGAMConfig ["a","b"])
-- Bootstrap: 確信度 DAG (prob>=0.5) + エッジ確率ヒートマップ
let bs = df |-> bootstrapLingam defaultBootstrapConfig cols
noDf |>> toPlot bs                     -- 確信度 DAG
noDf |>> bootstrapEdgeProbOf bs        -- エッジ出現確率ヒートマップ
```

![VAR-LiNGAM 時間ラグ DAG](../images/lingam-var-lag.svg)

![Bootstrap エッジ出現確率(確信度)](../images/lingam-bootstrap-prob.svg)

> **計算量**: DirectLiNGAM は O(p³·n)(変数数 p が律速・標本数 n は線形)。実測で p≤40 は
> 対話的(~15s)、p≤80 でバッチ(~2 分)、p>100 は非現実的。Bootstrap は B 倍。p が大きいときは
> ICA-LiNGAM や次元圧縮を検討する。
>
> IO 版 (`fitBootstrapLiNGAM`/`fitICALiNGAM`) は乱数を使うが、seed 固定で決定的。`df |->` 経路は
> seed 純粋版 (`*Pure`) を呼ぶので**同 seed でビット一致**。低レベル `fit*LiNGAM cfg xMat`
> (行列直接)も各 `Model.LiNGAM.*` に残る。
