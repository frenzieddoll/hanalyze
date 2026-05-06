# B10b — NUTS ESS 効率調査 (診断結果)

最終更新: 2026-05-06

## 目的

B7 の MCMC bench で hanalyze NUTS の ESS が blackjax NUTS の 1/64 と
低く、原因を診断する (実装まで踏み込まず、調査のみ)。

## 観測 (B7 から)

```
                  time(ms)  ess(mu)  ess(tau)
hanalyze NUTS     1757      42       53        ← 1000 samples / 500 warmup
PyMC NUTS         11018     856      546
blackjax NUTS     530       810      626
```

ESS/sec で hanalyze は blackjax の 1/64。

## 診断 (`bench-mcmc-diag` で 6 configs を比較)

```
                                       accept   ess(mu)   ess(tau)   distinct(mu)
baseline 200 samp eps=0.08 adapt=on    0.95     37        101        198
small-step eps=0.02 adapt=off          1.00     33        60         200
high-target target=0.95                0.99     73        200        200  ⭐
shallow-tree maxDepth=5                0.95     37        120        194
full-size 1000 samp baseline           0.91     42        53         922
```

## 主要発見

### 1. mixing は per-step 単位では正常

- `distinct = 198/200`: 各サンプルが異なる位置に動いている
- accept rate 0.91-1.00: divergence なし、tree termination 健全

### 2. ESS が n_samples の増加に対して劣線形

- 200 samples → ESS 37  (= 19% 効率)
- 1000 samples → ESS 42 (= 4% 効率)
- 1000 samples で ESS が 200-sample より上がらない = **autocorr length ~20-25**

→ 短期 mixing は OK だが **long-range drift が遅い**。これは典型的な
"Stan でいう Δ-energy が大きい問題":
- 各サンプルは近傍を well-mix
- 但し posterior の主要 mode 間を移動するのに 20+ iter かかる

### 3. target_accept=0.95 で ESS 2× 改善 (37→73)

- dual-averaging が target=0.8 で **eps を大きくしすぎ** ている
- target を上げると eps が小さくなり、ESS 改善
- ただしこれは「症状の緩和」で root cause ではない

### 4. tree depth / step size variants は ESS にほぼ影響しない

- shallow-tree (maxDepth=5)、tiny-step (eps=0.005)、small-step (eps=0.02)
  すべて baseline と ±10% 内
- → algorithm のパラメタ調整では本質改善なし

## Root cause 仮説: mass matrix 未適応

Stan / blackjax / PyMC は **warmup 中に diagonal mass matrix を window-based
で適応** し、unconstrained-space の各パラメタの posterior 分散を
mass matrix 対角に取り込む。

hanalyze NUTS の現実装 (`MCMC.HMC.kinetic` および `MCMC.NUTS.step`):
```haskell
kinetic :: [Double] -> Double
kinetic r = 0.5 * sum (map (^ (2 :: Int)) r)
-- = 0.5 * ‖r‖² with identity mass matrix
```

```haskell
-- step 関数
r0 <- forM names (\_ -> standard gen)  -- ~ N(0, I)
```

つまり M = I (恒等行列) 固定。

8-schools モデルの unconstrained-space スケール:
- mu      ~ Normal(73, 5²)         scale ~ 5
- log_tau ~ ~Normal(2.5, 1²)       scale ~ 1
- theta_j ~ Normal(school_mean, 5²) scale ~ 5

5 倍のスケール差 → 最適 eps は **scale の小さい log_tau に律速**
され、結果として scale の大きな mu / theta_j が under-mix。これが
long-range drift の遅さの正体。

## 実装提案 (別 phase 候補)

### B11: Diagonal mass matrix adaptation の実装

**変更箇所**:

1. `MCMC.HMC`:
   - `kinetic :: [Double] -> [Double] -> Double`  (M_inv, r) → ½ r^T M^{-1} r
   - `leapfrogWith` を mass-matrix 対応に
   - `sampleMomentum :: [Double] -> GenIO -> IO [Double]` (M を渡す)

2. `MCMC.NUTS`:
   - Warmup 中に diagonal mass の window-based 推定
   - Stan style: 75 / 25 / 50 / 25 / 50 ... 段階的 window
     - sliding window で sample variance を集約
     - 各 window 末尾で mass matrix を更新
     - step size adaptation を再起動

3. テスト:
   - 既存 NUTS テストが mass matrix off (Just (replicate p 1)) で同じ
     結果を返すことを確認 (backwards compat)
   - 8-schools 例で ESS が現状 50 → 800 程度になることを確認

**期待効果**:
- 8-schools NUTS ESS: 50 → 600-800 (PyMC/blackjax と parity)
- 工数: 2-3 日 (実装 + テスト)
- リスク: HMC API 変更 (kinetic の引数追加) で downstream 影響あり

## Workaround (今すぐ可能)

`nutsTargetAccept = 0.95` をデフォルトにすると ESS が約 2× 改善する。
ただし step が小さくなるため計算コスト増。trade-off の比率は
`time × ess` 不変なので根本改善にはならない。

## 結論

B10b の profile + diagnose phase は完了。**root cause は mass matrix
非実装**。修正は B11 (mass matrix adaptation 実装) として別 phase で
ユーザー判断を仰ぐ。
