# BayesOpt Branin 性能ギャップ調査

(2026-05-05)

## 課題

| 問題 | hanalyze (median 5 seeds) | skopt (deterministic) | true f* |
|---|---|---|---|
| Branin (2D) | 4.00 | 0.398 (機械精度) | 0.398 |
| Hartmann6 (6D) | -2.83 | -2.77 | -3.32 |

Branin で hanalyze は 10× 悪い結果。Hartmann6 では skopt を凌駕済。
**Branin に固有の問題** がある。

## 当初仮説の検証 — kernel 選択 (#6 の元タイトル) ではない

`bench-bo` で `boKernel = RBF` に切替えて単独テスト:

  - Matern52: Branin 4.00、Hartmann6 -2.83
  - **RBF**:    Branin **3.75**、Hartmann6 -1.66

RBF にしても Branin は 3.75 とほぼ同じ (Hartmann6 は逆に大幅悪化)。
**kernel 選択が主因ではない**。

## 真の原因候補 (コード解析より)

### #1 ★最有力: y の正規化なし

skopt の `GaussianProcessRegressor` はデフォルト `normalize_y=True`:

  - 訓練前に `y' = (y - mean(y)) / std(y)` で z-score 化
  - 予測時に逆変換
  - GP HP 最適化で signal_var, noise_var が **意味のあるスケール** で
    始まるため L-BFGS が安定収束

hanalyze: `Model.GP` で y を生スケールのまま使用 (`logMarginalLikelihood`,
`fitGPMV` 等)。Branin は y ∈ [0.4, 300+] で 3 桁レンジ →
`initParamsFromData` で `signalVar = yVar` を huge value に初期化 →
HP optimization が極端な値域で揺れ、まともな posterior を作れない。

```haskell
-- Model.GP.initParamsFromData
{ gpLengthScale = max 0.01 ((xMax - xMin) / 4)
, gpSignalVar   = max 0.01 yVar       -- ← Branin で 5000+ になる
, gpNoiseVar    = max 1e-4 (yVar * 0.05)
, gpPeriod      = max 0.01 (xMax - xMin)
}
```

### #2 ★主要: isotropic kernel (1 length scale)

Branin の bounds は **[-5, 10] × [0, 15]** で大きさは互角だが、
3 つの global minima が x1 軸方向に偏って分布:

  - (-π, 12.275)、(π, 2.275)、(9.42, 2.475)

isotropic kernel (1 ℓ) では x1 軸の構造を捉えづらい。pymoo のような
**ARD kernel (per-dimension length scale)** であれば、x1 と x2 の
変動スケールを別々に学習できる。

`Model.GP.GPParams.gpLengthScale` は **scalar 1 つ**。
`buildKernelMatrixMV` も `applyKernel RBF p s = sf * exp (- s / (2 * l²))`
で l 1 つを共有。

### #3 ★中程度: GP HP optimization の単一 init

`Optim.BayesOpt.bayesOptND` の中で `optimizeGPMV kern xMat yVec p0`
は **1 回呼ぶのみ** (p0 は固定の `initParamsFromData`)。
log marginal likelihood は multimodal なので、L-BFGS が悪い basin に
落ちる確率がある。

`optimizeGPMVRestart` は実装済 (B6 phase) だが、bayesOptND では
使われていない (試行で逆効果と判断 → revert)。

skopt は `n_restarts_optimizer=0` (= 1 fit) だが、kernel に prior が
組み込まれているため robust。我々は flat な探索なので restart が必要。

### #4 ★中程度: 内側 L-BFGS が numeric gradient

`runLBFGSNumeric` を使っているため per-step で 2*p+1 = 5 回の f 評価。
GP fit が遅い場合、acquisition 最大化が現実的時間内に十分 polish
できない。analytic gradient を計算すれば 1 回の fit で済む。

### #5 ★小: acquisition は EI のみ

skopt は `acq_func='gp_hedge'` (EI / LCB / PI 動的混合) がデフォルト。
hanalyze は EI 固定 + ξ=0.01。Branin の局所最適にハマる傾向。

### #6 ★小: ξ=0.01 固定

Branin で y ∈ [0.4, 300+] のとき ξ=0.01 は実質 0 で exploit が強すぎ。
y range の relative scale で ξ を調整する案あり。

## 推定影響度ランキング (1 つずつ修正してギャップが何 % 埋まるか)

| 修正 | 期待される Branin f | 工数 |
|---|---|---|
| #1 y 正規化 (Stat.Standardize 流用) | 4.0 → ~1.0 | 1-2 時間 |
| #1 + #2 ARD kernel (per-dim ℓ) | 4.0 → ~0.5 | 半日 |
| #1 + #2 + #3 multi-restart HP opt | 4.0 → ~0.4 (skopt 並) | 1 日 |
| #4 analytic gradient | 速度 5-10× (精度はほぼ同じ) | 半日 |

## 修正方針 (実装は別 phase、許可待ち)

### Phase BO1 (最重要): y 正規化を BayesOpt 経路に組み込む

`Optim.BayesOpt.bayesOptND` 内で訓練データ y を z-score 化し、GP
fit に渡す。予測時に逆変換。`Model.GP` 本体には触れず、wrapper として
追加。

### Phase BO2: ARD kernel (per-dim length scale)

`Model.GP.GPParams.gpLengthScale :: Double` を `gpLengthScales :: LA.Vector Double`
(per-dim) に拡張。`buildKernelMatrixMV` で per-dim スケーリング。
既存 1D API は scalar `gpLengthScale` を 1 要素 Vector で wrap して
互換性維持。

### Phase BO3: multi-restart HP opt with retry safeguard

B6b の `optimizeGPMVRestart` を bayesOptND で実際に使う。前回試行で
逆効果になった原因 (= random init で L-BFGS が悪い basin に落ちる)
を、初期点を **best HP so far ± log-uniform 摂動** に変えれば回避可。

### Phase BO4 (= optional): 解析勾配で内側 L-BFGS

negEI の解析勾配は GP の解析勾配 + EI の chain rule で計算可能
(数十行)。

## まとめ

#6 のオリジナルタイトル "kernel auto-select" は **誤り**:
- kernel 選択は問題でない (RBF/Matern52 で結果ほぼ同じ)
- 真の原因は y 正規化なし、isotropic kernel、GP HP single init

優先度順に **BO1 → BO2 → BO3** で実装すれば skopt 並みに到達可能。
全部で 1-2 日。

実装着手の許可をお願いします (BO1 単独先行 → 効果計測 → 段階的に
BO2/BO3 を追加)。

**Mutable Vector 不使用** で実装可能。
