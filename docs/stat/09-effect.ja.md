# Stat.Effect — 効果量と Power 解析

> R の `pwr` / Python の `statsmodels.stats.power` 相当。

## 1. 効果量 (Effect Size)

p-value を補完する **効果の大きさ** を測る指標。

### 位置 (location)

```haskell
import qualified Hanalyze.Stat.Effect as Eff

-- Cohen's d (二群、pooled SD)
Eff.cohenD groupA groupB
-- 0.2 = small, 0.5 = medium, 0.8 = large

-- 対応のあるサンプル
Eff.cohenDPaired pre post

-- Hedges' g (small-sample 補正)
Eff.hedgesG groupA groupB  -- ≤ |cohenD|
```

### ANOVA / 回帰

```haskell
-- η² (eta-squared)
Eff.eta2 [g1, g2, g3]    -- biased upward

-- ω² (omega-squared、unbiased)
Eff.omega2 [g1, g2, g3]

-- Cohen's f (η² から)
Eff.cohensF eta2Value  -- 0.10 small / 0.25 medium / 0.40 large
```

### カテゴリ

```haskell
-- Cramér's V (chi² から)
Eff.cramerV chi2 n rows cols

-- φ (2x2 用)
Eff.phiCoeff chi2 n

-- Odds ratio
Eff.oddsRatio ((20, 5), (5, 20))  -- 16.0
```

## 2. Power 解析

### 何を計算したいか?

| 入力 → 出力 | 関数 |
|---|---|
| (n, α, effect) → power | `powerXxx` |
| (power, α, effect) → n | `sampleSizeXxx` |

### t-test

```haskell
-- power: n=30 per group, α=0.05, d=0.5 → ?
Eff.powerTTest 30 0.05 0.5
-- 0.475 (= 47.5% power)

-- 必要サンプル: power=0.80, α=0.05, d=0.5 → ?
Eff.sampleSizeTTest 0.80 0.05 0.5
-- 64 (per group)
```

### ANOVA

```haskell
-- 4 群、cells per group=20、α=0.05、f=0.25 → power
Eff.powerANOVA 20 4 0.05 0.25

-- 必要 cell 数
Eff.sampleSizeANOVA 0.80 4 0.05 0.25
```

### Correlation

```haskell
-- ρ=0.3、n=50、α=0.05 → power
Eff.powerCorrelation 50 0.05 0.3
```

## 3. 実例

### A/B テスト設計

```haskell
-- 期待効果 d=0.3 (small-medium)、power 80% を達成する n
let n = Eff.sampleSizeTTest 0.80 0.05 0.3
-- 175 per group

-- 実験後、観測効果量を計算
let d = Eff.cohenD treatment control
    actualPower = Eff.powerTTest n 0.05 d
```

### 多群実験

```haskell
let f = Eff.cohensF (Eff.eta2 groups)
    needN = Eff.sampleSizeANOVA 0.80 (length groups) 0.05 f
```

## 4. p-value だけ報告しない理由

- p-value は **n に依存** (n を増やせばどんな小さい効果でも p < 0.05)
- effect size は **n に独立**
- 学術論文での標準: p-value + effect size + CI を併記
