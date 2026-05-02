# 変分推論 (Stat.VI — ADVI)

> 🌐 [English](05-variational-inference.md) | **日本語**

> 関連デモ:
> - [`vi-demo`](../demo/VIDemo.hs) — VI vs NUTS の精度・速度比較

## 概要と原理

**ADVI (Automatic Differentiation Variational Inference)**
(Kucukelbir et al. 2017) の平均場正規 VI 実装です。

MCMC が事後分布から「正確にサンプリング」するのに対し、
VI は事後分布を **正規分布族で近似** することで高速に推論します。

### アルゴリズム概要

1. **変換**: 制約付きパラメータを unconstrained 空間に変換 (HMC/NUTS と同じ変換)
2. **近似族**: `q(u; φ) = Π_i Normal(u_i; μ_i, σ_i)` (平均場: パラメータ間独立を仮定)
3. **目的関数**: ELBO を最大化  
   `ELBO = E_q[log p(θ,y) + log|J|] + Σ_i H[Normal(μ_i, σ_i)]`
4. **勾配**: Reparameterization trick で `u = μ + σ⊙ε, ε~N(0,I)` と書き、有限差分勾配
5. **最適化**: Adam オプティマイザ

---

## API

```haskell
import Stat.VI

data VIConfig = VIConfig
  { viIterations   :: Int     -- Adam 反復回数
  , viSamples      :: Int     -- ELBO 勾配の MC サンプル数 (推奨: 5〜10)
  , viLearningRate :: Double  -- Adam 学習率 α
  , viBeta1        :: Double  -- Adam β₁ (default 0.9)
  , viBeta2        :: Double  -- Adam β₂ (default 0.999)
  , viEpsilon      :: Double  -- Adam ε (default 1e-8)
  , viNumDraws     :: Int     -- 収束後に q から引くサンプル数
  , viGradStep     :: Double  -- 有限差分刻み幅 (default 1e-5)
  }

defaultVIConfig :: VIConfig
-- viIterations=1000, viSamples=5, viLearningRate=0.1, viNumDraws=2000

advi :: Model a -> VIConfig -> Params -> GenIO -> IO VIResult
```

---

## VIResult の内容

```haskell
data VIResult = VIResult
  { viPostMeans   :: Params    -- 事後平均 (constrained space)
  , viPostSDs     :: Params    -- 事後 SD  (constrained space)
  , viMuU         :: [Double]  -- 変分平均 μ (unconstrained space)
  , viSigmaU      :: [Double]  -- 変分 SD  σ (unconstrained space)
  , viElboHistory :: [Double]  -- ELBO の時系列 (収束確認用)
  , viDraws       :: [Params]  -- 事後サンプル (constrained, viNumDraws 本)
  }
```

---

## 使い方: Beta-Binomial モデル

解析解が存在するモデルで VI の精度を確認できます。

```haskell
import Stat.VI
import qualified Data.Map.Strict as Map
import System.Random.MWC (createSystemRandom)

clinicalModel :: Model ()
clinicalModel = do
  pCtrl <- sample "p_ctrl" (Beta 1 1)
  pTrt  <- sample "p_trt"  (Beta 1 1)
  observe "y_ctrl" (Binomial 50 pCtrl) [18]
  observe "y_trt"  (Binomial 50 pTrt)  [31]

main :: IO ()
main = do
  gen <- createSystemRandom
  let cfg = defaultVIConfig
              { viIterations = 500
              , viSamples    = 10
              , viNumDraws   = 5000
              }
      initP = Map.fromList [("p_ctrl", 0.5), ("p_trt", 0.5)]

  result <- advi clinicalModel cfg initP gen

  -- 事後平均・SD
  print (viPostMeans result)  -- fromList [("p_ctrl", 0.3725), ("p_trt", 0.6078)]
  print (viPostSDs   result)  -- fromList [("p_ctrl", 0.0623), ("p_trt", 0.0651)]

  -- 解析解: Beta(1+k, 1+n-k)
  -- p_ctrl: mean=19/52=0.3654, SD=0.0644
  -- p_trt:  mean=32/52=0.6154, SD=0.0664

  -- P(p_trt > p_ctrl) の推定
  let draws   = viDraws result
      diffVI  = [ (d Map.! "p_trt") - (d Map.! "p_ctrl") | d <- draws ]
      probVI  = fromIntegral (length (filter (>0) diffVI)) / fromIntegral (length diffVI) :: Double
  printf "P(p_trt > p_ctrl) = %.4f\n" probVI  -- ≈ 0.9948
```

---

## ELBO 収束の確認

```haskell
let hist  = viElboHistory result
    n     = length hist
    steps = [1, n `div` 4, n `div` 2, 3 * n `div` 4, n]
forM_ steps $ \i ->
  printf "iter %4d: ELBO = %.3f\n" i (hist !! (i-1))

-- iter    1: ELBO = -8.241
-- iter  125: ELBO = -5.823
-- iter  250: ELBO = -5.614
-- iter  375: ELBO = -5.601
-- iter  500: ELBO = -5.598
-- (収束すると ELBO の変化が小さくなる)
```

---

## VIConfig のチューニング

| パラメータ | 目安 | 備考 |
|---|---|---|
| `viIterations` | 500〜2000 | ELBO が安定するまで増やす |
| `viSamples` | 5〜15 | 増やすと勾配推定精度↑だが速度↓ |
| `viLearningRate` | 0.05〜0.2 | 不安定なら 0.01〜0.05 に下げる |
| `viNumDraws` | 2000〜10000 | SD 推定精度に直結 |

---

## VI vs NUTS の実測比較

```
=== モデル 1: Beta-Binomial (臨床試験) ===

               p_ctrl                    p_trt             時間
VI       mean=0.3688 SD=0.0631   mean=0.6098 SD=0.0637   0.218s
NUTS     mean=0.3651 SD=0.0645   mean=0.6148 SD=0.0661   1.432s
解析解   mean=0.3654 SD=0.0644   mean=0.6154 SD=0.0664

=== モデル 2: 階層正規モデル (3校) ===

  param     VI 平均   VI SD   |  NUTS 平均  NUTS SD
  mu          73.060   6.752  |    73.053   15.562
  tau         16.234   5.893  |    16.047    8.945
  theta_1     71.602   5.741  |    71.440    7.803

  注: 平均場 VI は各パラメータ間の相関を無視するため、
      階層モデルでは SD を過小評価する (mu SD: 6.75 vs 15.56)
```

---

## VI の限界と使い分け

| 状況 | 推奨 |
|---|---|
| モデルが大きい・高速近似が必要 | VI |
| 解が解析的に検証できる (Beta-Binomial など) | VI で十分 |
| 強い相関のある事後分布 (階層モデルなど) | NUTS (VI は SD を過小評価) |
| 正確な不確かさ定量化が必要 | NUTS |

**平均場 VI の根本的な限界**: 変分族 `Π_i Normal(u_i; μ_i, σ_i)` はパラメータ間の
相関を表現できません。μ と τ が強相関する階層モデルでは VI が事後分布を過信します。

この限界を緩和するには Full-rank (full covariance) VI または正規化フロー VI が
必要ですが、現実装は mean-field のみです。
