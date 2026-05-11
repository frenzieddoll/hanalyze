# 真の多出力回帰 (`hanalyze multireg`)

> 🌐 [English](07-multireg.md) | **日本語**

「1 つのスカラ入力 → q 個の出力カーブ」という構造のデータ — たとえば
**1 dose に対して 100 個の z 位置で観測される電位プロファイル** — を、
wide-form CSV からワンライナーで学習し、入力スライダで全 q 予測を JS が
即時再計算する対話的 HTML を生成します。

## データ形式 (wide CSV)

| dose | y_z001 | y_z002 | ... | y_z100 |
|---|---|---|---|---|
| 6.0 | 3.16 | 3.05 | ... | 0.01 |
| 6.4 | 3.15 | 3.04 | ... | 0.01 |
| ... | ... | ... | ... | ... |
| 14.0 | 3.12 | 3.01 | ... | 0.00 |

- 1 列目 = 入力 (例 dose)
- 残り q 列 = 出力 (z grid 上の関数値)

## 基本コマンド

### 線形多出力 (closed-form OLS)
```
hanalyze multireg data/io/potential_wide.csv dose 'y_z*' \
    --method linear \
    --xaxis 'z [nm]' --xaxis-min 0 --xaxis-max 200 \
    --report trash/multireg_lin.html
```

`B = (XᵀX)⁻¹ XᵀY` を 1 回の LAPACK 呼び出しで全 q 列同時に求解。
N=21 dose × 100 出力で 1ms 未満。

### RBF カーネルリッジ多出力 (LOOCV 自動 HP)
```
hanalyze multireg data/io/potential_wide.csv dose 'y_z*' \
    --method kernel-rbf --auto-hp \
    --xaxis 'z [nm]' --xaxis-min 0 --xaxis-max 200 \
    --report trash/multireg_kr.html
# → best h=8.000  λ=2.15e-3  LOO MSE=1.16e-2  RMSE=0.091
```

`α = (K + λI)⁻¹ Y` で全 q 列の α を一括計算。LOOCV は Hat 行列の
対角を 1 回計算して全 q 出力で再利用するので O(n³) の Cholesky 1 回 +
グリッド評価のみ。

## yspec の指定

| 形式 | 意味 |
|---|---|
| `'y_z*'` | `y_z` で始まる全列 (シェルでクォート必須) |
| `y_z001,y_z002,y_z003` | カンマ区切り明示 |

## 出力 HTML の構造

`Hanalyze.Viz.ReportBuilder.SecInteractiveMultiOut` セクションが埋め込まれます:

- **入力スライダ** (例 dose 6〜14): 1 本
- **予測曲線** (赤線): スライダ値で全 q 出力を JS が再計算 →
  Vega-Lite で描画
- **観測点** (色分け散布): 各観測 dose の y_z\* を z 軸上に重ね描き

## 内部構造

| レイヤ | API | 役割 |
|---|---|---|
| データ | wide CSV | `dose,y_z001,...,y_z100` |
| 共通基盤 | `Hanalyze.Model.MultiOutput` | `asMultiY`/`fromMultiY`/`r2Multi` |
| モデル | `Hanalyze.Model.MultiLM.fitMultiLM` | 線形 (B=(XᵀX)⁻¹XᵀY) |
|        | `Hanalyze.Model.Kernel.kernelRidgeMulti` + `autoTuneKernelRidgeMulti` | RBF カーネルリッジ + LOOCV |
| レポート | `Hanalyze.Viz.ReportBuilder.secInteractiveMultiOut` + `mkInteractiveMOLinear` / `mkInteractiveMOKernelRBF` | 対話的 HTML 生成 |

## 設計上の注意

- **入力 1 次元のみ**: 現状は xcol に 1 列しか取れない。多入力 + 多出力は
  `Hanalyze.Model.RFF.rffRidgeMVMulti` を直接使うか、別の CLI コマンドが必要。
- **出力グリッド**: `--xaxis-min`/`--xaxis-max` で z 軸の範囲を指定。
  指定しないと `1..q` で線形展開。
- **データ点数**: kernel-rbf を使うなら N ≥ 10 dose 水準を推奨
  (LOOCV が安定する目安、6 でも動くが過適合確認は要)。
- **出力数 q**: q=100..1000 まで実用的。q が大きくなると HTML サイズが
  α 行列 (n × q) ぶん増える。

## 関連: 多出力モデル全般

`Hanalyze.Model.*` の主要モデルは「多出力 = 主、1 出力 = 特殊化」のポリシで
統一されています:

- `Hanalyze.Model.Regularized.fitRegularizedMulti` (Ridge は閉形式、Lasso/EN は列ごと CD)
- `Hanalyze.Model.Spline.fitSplineMulti`
- `Hanalyze.Model.Kernel.kernelRidgeMulti` / `nwRegressionMulti`
- `Hanalyze.Model.RFF.rffRidgeMulti` (1D 入力) / `rffRidgeMVMulti` (多入力)
- `Hanalyze.Model.GP.fitGPMulti` (Ky⁻¹ 共有、分散も共有)
- `Hanalyze.Model.GPRobust.fitGPRobustMulti`
- `Hanalyze.Model.GLM.fitGLMMulti` (列ごと IRLS)
- `Hanalyze.Model.GLMM.fitLMEMulti` / `fitGLMMMulti`
- `Hanalyze.Model.HBM.observeColumns` (DSL 多出力ヘルパ)

q=1 と q>1 で旧 API と数値一致するかは `test/Spec.hs` の
"Multi-output equivalence (q=1)" describe ブロックで検証済 (10 件)。
