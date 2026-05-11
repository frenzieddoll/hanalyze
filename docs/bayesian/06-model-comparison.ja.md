# モデル比較 (Stat.ModelSelect)

> 🌐 [English](06-model-comparison.md) | **日本語**

> 関連デモ:
> - [`gibbs-demo`](../demo/GibbsDemo.hs) — WAIC/LOO で 2 モデル比較
> - [`simpson-paradox`](../demo/SimpsonParadoxDemo.hs) — LM/GLMM/HBM の WAIC を 1 つの HTML に並列表示
> - [`hbm-random-slope`](../demo/HBMRandomSlopeDemo.hs) — ランダム切片 vs +ランダム傾きの ΔWAIC 比較
>
> CLI: `--waic` フラグで LM / GLM / GLMM / HBM レポートに WAIC/LOO を埋め込み。

## 概要

`Hanalyze.Stat.ModelSelect` は MCMC チェーンを使った情報量規準によるモデル比較を提供します。

| 指標 | 関数 | 説明 |
|---|---|---|
| WAIC | `waic`, `chainWAIC` | Widely Applicable Information Criterion |
| PSIS-LOO | `loo`, `chainLOO` | Pareto Smoothed Importance Sampling LOO-CV |

どちらも **小さい値ほど良い** (−2×elpd スケール)。

---

## WAIC (広義適用情報量規準)

**原理 (Watanabe 2010)**:
- `lppd = Σᵢ log(E_θ[p(yᵢ|θ)])` — 対数点予測密度
- `p_waic = Σᵢ Var_θ[log p(yᵢ|θ)]` — 有効パラメータ数
- `WAIC = −2(lppd − p_waic)` — AIC アナログ

### API

```haskell
import Stat.ModelSelect

data WAICResult = WAICResult
  { waicValue :: Double  -- WAIC = −2(lppd − p_waic)
  , waicLppd  :: Double  -- log pointwise predictive density
  , waicPwaic :: Double  -- 有効パラメータ数 p_waic
  , waicSE    :: Double  -- WAIC の推定標準誤差
  }

-- チェーンから直接計算 (推奨)
chainWAIC :: Model a -> Chain -> WAICResult

-- 対数尤度行列から計算 (行=サンプル, 列=観測値)
waic :: [[Double]] -> WAICResult

-- 対数尤度行列の生成 (chainWAIC の内部で使用)
chainLogLikMatrix :: Model a -> Chain -> [[Double]]
```

### 例: 2モデルの比較

```haskell
import Stat.ModelSelect
import MCMC.NUTS (nuts, defaultNUTSConfig)

-- モデル A: 弱情報事前分布
modelA :: Model ()
modelA = do
  mu <- sample "mu" (Normal 0 10)
  observe "y" (Normal mu 2) obsData

-- モデル B: 情報事前分布 (真値 μ=3 から外れた μ=5 を仮定)
modelB :: Model ()
modelB = do
  mu <- sample "mu" (Normal 5 1)
  observe "y" (Normal mu 2) obsData

main :: IO ()
main = do
  gen <- createSystemRandom
  let cfg   = defaultNUTSConfig { nutsIterations = 5000 }
      initP = Map.fromList [("mu", 0.0)]

  chainA <- nuts modelA cfg initP gen
  chainB <- nuts modelB cfg initP gen

  let waicA = chainWAIC modelA chainA
      waicB = chainWAIC modelB chainB

  printf "モデル A: WAIC=%.3f  lppd=%.3f  p_waic=%.3f  SE=%.3f\n"
    (waicValue waicA) (waicLppd waicA) (waicPwaic waicA) (waicSE waicA)
  printf "モデル B: WAIC=%.3f  lppd=%.3f  p_waic=%.3f  SE=%.3f\n"
    (waicValue waicB) (waicLppd waicB) (waicPwaic waicB) (waicSE waicB)

  let delta = waicValue waicA - waicValue waicB
  printf "ΔWAIC(A−B) = %.3f\n" delta
  -- delta < -2 ならモデル A が有意に良い
```

---

## PSIS-LOO (Pareto Smoothed Importance Sampling LOO-CV)

**原理 (Vehtari, Gelman, Gabry 2017)**:
- 各観測値 yᵢ を抜いた LOO 予測密度を IS で近似
- 重要度重みの裾を Pareto 分布で平滑化
- **Pareto k̂** で各観測値の推定信頼性を診断

### Pareto k̂ の解釈

| k̂ | 診断 |
|---|---|
| < 0.5 | 良好 — LOO 推定は信頼できる |
| 0.5〜0.7 | 許容 — やや不安定だが概ね使える |
| > 0.7 | 要注意 — LOO が不安定、WAIC を優先するか k̂ > 0.7 の観測値を確認 |

### API

```haskell
data LOOResult = LOOResult
  { looValue   :: Double    -- −2 × elpd_loo (小さいほど良い)
  , looElpd    :: Double    -- Σᵢ elpd_i
  , looSE      :: Double    -- 推定標準誤差
  , looKHat    :: [Double]  -- 観測値ごとの k̂
  , looKHatBad :: Int       -- k̂ > 0.7 の観測値数
  }

chainLOO :: Model a -> Chain -> LOOResult
loo      :: [[Double]] -> LOOResult
```

### 例: LOO + k̂ 診断

```haskell
let looRes = chainLOO modelA chainA
printf "LOO = %.3f  elpd = %.3f  SE = %.3f  k̂>0.7: %d 観測\n"
  (looValue looRes) (looElpd looRes) (looSE looRes) (looKHatBad looRes)

-- 観測値ごとの k̂ を確認
mapM_ (\(i, k) -> printf "obs %2d: k̂=%.3f  %s\n" (i::Int) k (khatLabel k))
  (zip [1..] (looKHat looRes))

khatLabel :: Double -> String
khatLabel k | k < 0.5   = "良好"
             | k < 0.7   = "許容"
             | otherwise = "要注意"
```

---

## WAIC vs LOO の使い分け

| 状況 | 推奨 |
|---|---|
| 通常のモデル比較 | WAIC (計算が軽い) |
| 観測値ごとの診断が必要 | LOO (k̂ 診断付き) |
| k̂ > 0.7 が多い | 信頼性不足 — サンプル数を増やすか WAIC を使う |
| モデル比較の標準的な選択 | LOO (Vehtari らが推奨) |

---

## 実測比較例

```
=== Section 2: WAIC モデル比較 ===
  モデル A: μ ~ Normal(0, 10)  [弱情報事前: 真値 μ=3 を広くカバー]
  モデル B: μ ~ Normal(5,  1)  [情報事前: μ≈5 を強く仮定、真値からずれ]

  モデル A  事後 mean=3.2551 (解析=3.2553)  WAIC=  97.038  lppd= -44.636  p_waic=3.883  SE=5.034
  モデル B  事後 mean=4.3981 (解析=4.4048)  WAIC= 108.424  lppd= -51.325  p_waic=2.887  SE=5.621

  ΔWAIC(A − B) = -11.386
  → モデル A (弱情報事前) の方が良い当てはまり ✓

=== Section 3: PSIS-LOO 診断 ===
  モデル A: LOO=97.135  elpd=-48.567  SE=5.047  k̂>0.7: 0 観測
  モデル B: LOO=108.539 elpd=-54.269  SE=5.638  k̂>0.7: 0 観測

  Pareto k̂ 診断 (モデル A, 観測値ごと):
    obs  1: k̂=0.022  良好
    obs  2: k̂=0.012  良好
    ...
    obs 20: k̂=0.015  良好
```

**解釈:**
- ΔWAIC = -11.4 は SE の 2 倍以上 → モデル A が統計的に有意に良い
- 弱情報事前分布は真値 μ=3 周辺に事後分布を集中させられるが、
  情報事前分布のモデル B は μ=5 への強い引力でデータとの乖離が大きくなる
