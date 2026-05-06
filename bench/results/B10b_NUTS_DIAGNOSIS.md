# B10b — NUTS ESS 効率調査 (診断結果) → B11 で解決

最終更新: 2026-05-07 (B11 で resolved)

## TL;DR (B11 解決後)

B10b で特定した root cause (mass matrix 非実装) を B11 で解決。

| | time | ess(mu) | ess(tau) | ess/sec(min) |
|---|---|---|---|---|
| 旧 (mass=I) | 1757 ms | 42 | 53 | 24 |
| **新 (B11 mass adapt)** | **1492 ms** | **839** | **571** | **383** |
| blackjax 参考 | 530 ms | 810 | 626 | 1180 |
| PyMC 参考 | 11018 ms | 856 | 546 | 50 |

ESS は blackjax を超え (839 vs 810)、PyMC を 7.4× 上回る速度に到達。
詳細は本ドキュメント末尾の「B11 実装後追記」を参照。

---


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

---

## B11 実装後追記 (2026-05-07)

### 実装内容 (`src/MCMC/NUTS.hs`)

Stan-style multi-window adaptation を実装:

1. **`MCMC.HMC.kineticM` / `leapfrogWithM`**: 対角 M⁻¹ を取る版を追加
   (既存の `kinetic` / `leapfrogWith` は不変、後方互換)
2. **`NUTSConfig.nutsAdaptMass :: Bool`** フラグ (デフォルト False、opt-in)
3. **3 phase schedule** (`stanWindows :: Int -> ([Int], Int, Int)`):
   - init buffer (15% / 最低 75 iter): step-size のみ adapt、M=I
   - window phase: 25 → 50 → 100 → 200 ... と倍々で拡大、各 window 末尾で
     - その window 内の Welford diagonal variance から M⁻¹ 更新
     - dual averaging を **再起動** (新しい M に対して ε を再 adapt)
   - term buffer (10% / 最低 50 iter): M 凍結、step-size 継続
4. **Welford online accumulator**: `data Welford = Welford !Int ![Double] ![Double]`、
   per-window でリセット (drift bias を避ける)
5. **Stan 式 shrinkage**: `σ̂² = (n/(n+5))·sample_var + 1e-3·(5/(n+5))`

### Convention の bug と修正

最初の prototype は **M⁻¹ = 1/sample_var (= M = posterior_var)** と設定して
しまっており、ESS が悪化 (51 vs 42) + time が 9× 化していた。

正しい Stan/blackjax convention は **M⁻¹ ≈ posterior_covariance** 直接保持
(つまり `M⁻¹_ii = sample_var_i`):

- kinetic: `½ rᵀ M⁻¹ r` で M⁻¹ が posterior var を持つ
- 運動量 `r ~ N(0, M = 1/posterior_var)` → `|r_i|` は小さい
- 位置 step `ε · M⁻¹ · r` は absolute units で `ε · posterior_sd` 規模
  = posterior-sd units で `ε` → NUTS の理想動作 (depth ~ 1/ε)

逆だと位置 step が `ε / posterior_sd²` ≈ 0.0005 sd unit となり、tree depth
が maxDepth=10 まで張り付く。修正は `welfordMInv` の最後の `1 / ...` を
削除する 1 行。

### 結果

`bench-mcmc-diag` (warmup=500, samples=1000):

| | time | ess(mu) | ess(tau) |
|---|---|---|---|
| mass=OFF | 3.22 s | 42.0 | 53.1 |
| mass=ON | 1.37 s | 838.7 | 570.9 |

`bench-mcmc-b7` で Python 比較を更新 (`bench/results/haskell/mcmc.csv`):

| system | time | ess(mu) | ess(tau) |
|---|---|---|---|
| hanalyze NUTS (mass) | 1492 ms | 839 | 571 |
| blackjax NUTS | 530 ms | 810 | 626 |
| PyMC NUTS | 11018 ms | 856 | 546 |

ESS は blackjax を超え (839 > 810)、time は PyMC を 7.4× 上回る。
blackjax との時間差 (2.8×) は JAX JIT 差で構造的天井の側だが、
ESS 品質は対等以上に到達。

### 残課題

- `nutsAdaptMass` のデフォルトを `True` に切り替えるかは別判断
  (API 変更を伴うため、当面は opt-in にして B7 ベンチで使用)
- 多次元の dense mass matrix は未実装 (Stan の dense option 相当)
  (8-schools 級では diagonal で十分。Cov 構造が強い funnel 系でのみ
  必要になる)
