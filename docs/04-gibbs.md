# Gibbs サンプリング (MCMC.Gibbs)

## 概要と原理

Gibbs サンプリングは、各パラメータを **他の全パラメータを固定した条件付き事後分布** から
逐次サンプリングする手法です。

**利点:**
- 共役モデルでは条件付き分布が解析的に求まり、**直接サンプリング**できる
- 棄却ステップがないため ESS/時間が NUTS より高くなることが多い

**制限:**
- 自分でアップデート関数を定義する必要がある
- 共役でないパラメータには適用できない (その場合は NUTS を混在させる)

---

## 実装された共役アップデート

### `normalNormal` — Normal-Normal 共役

モデル: `μ ~ Normal(μ₀, σ₀)`, `yᵢ ~ Normal(μ, σ_lik)`

条件付き事後 (解析解): `μ | y ~ Normal(μ_post, σ_post)`  
ただし `σ_post² = 1/(1/σ₀² + n/σ_lik²)`, `μ_post = σ_post² × (μ₀/σ₀² + nȳ/σ_lik²)`

```haskell
normalNormal
  :: Text     -- パラメータ名
  -> Double   -- 事前平均 μ₀
  -> Double   -- 事前 SD σ₀
  -> [Double] -- 観測データ y
  -> Double   -- 尤度 SD σ_lik (既知)
  -> GibbsUpdate
```

### `betaBinomial` — Beta-Binomial 共役

モデル: `p ~ Beta(α, β)`, `y ~ Binomial(n, p)`, 観測 k 回成功

条件付き事後: `p | y ~ Beta(α+k, β+n-k)`

```haskell
betaBinomial
  :: Text   -- パラメータ名
  -> Double -- 事前 α
  -> Double -- 事前 β
  -> Int    -- 試行数 n
  -> Int    -- 成功数 k
  -> GibbsUpdate
```

### `gammaPoisson` — Gamma-Poisson 共役

モデル: `λ ~ Gamma(α, β)`, `yᵢ ~ Poisson(λ)`

条件付き事後: `λ | y ~ Gamma(α + Σyᵢ, β + n)`

```haskell
gammaPoisson
  :: Text     -- パラメータ名
  -> Double   -- 事前 shape α
  -> Double   -- 事前 rate β
  -> [Double] -- 観測データ
  -> GibbsUpdate
```

---

## 基本的な使い方

```haskell
import MCMC.Gibbs
import qualified Data.Map.Strict as Map
import System.Random.MWC (createSystemRandom)

obsData :: [Double]
obsData = [3.2, 1.8, 4.1, 2.9, 3.5]

main :: IO ()
main = do
  gen <- createSystemRandom

  let updates = [ normalNormal "mu" 0 10 obsData 2.0 ]  -- σ_lik = 2 は既知
      cfg     = defaultGibbsConfig { gibbsIterations = 5000, gibbsBurnIn = 500 }
      initP   = Map.fromList [("mu", 0.0)]

  chain <- gibbs updates cfg initP gen

  print (posteriorMean "mu" chain)  -- Just 3.06 (例)
  print (posteriorSD   "mu" chain)  -- Just 0.42 (例)
```

---

## GibbsConfig

```haskell
data GibbsConfig = GibbsConfig
  { gibbsIterations :: Int  -- バーンイン後のサンプル数
  , gibbsBurnIn     :: Int  -- 破棄するバーンインステップ数
  }

defaultGibbsConfig :: GibbsConfig
-- gibbsIterations=2000, gibbsBurnIn=500
```

---

## 複数パラメータの同時更新

複数の `GibbsUpdate` を渡すと、1 イテレーションで順番に更新されます。

```haskell
-- Beta-Binomial: 対照群と治療群を同時に更新
let updates =
      [ betaBinomial "p_ctrl" 1 1 50 18  -- 対照: 50試行中18成功
      , betaBinomial "p_trt"  1 1 50 31  -- 治療: 50試行中31成功
      ]
chain <- gibbs updates cfg (Map.fromList [("p_ctrl", 0.5), ("p_trt", 0.5)]) gen
```

---

## 多チェーン実行

```haskell
-- gibbsChains でチェーンごとに独立した乱数シードを使う
chains <- gibbsChains updates cfg 4 initP gen

-- R-hat で収束確認
let r = rhat (map (chainVals "mu") chains)
print r  -- Just 1.000 (Gibbs は通常すぐ収束)
```

---

## Gibbs vs NUTS の性能比較

```
=== Section 1: Gibbs vs NUTS (Normal 平均推定) ===

  データ: n=20, ȳ=3.255, σ_lik=2.0 (既知), 真値 μ=3.0

  Gibbs    mean= 3.2553  SD= 0.4399  ESS=4967.7  ESS/s=4827.9
  NUTS     mean= 3.2551  SD= 0.4392  ESS=4459.5  ESS/s=1243.3
  解析解   mean= 3.2553  SD= 0.4399
```

**Gibbs は共役モデルで NUTS より約 3.9 倍の ESS/秒** を達成します。
ただし、共役でないモデルには使えないため、汎用性は NUTS が上です。

---

## カスタムアップデート関数の書き方

`GibbsUpdate = Params -> GenIO -> IO (Text, Double)` を満たせば任意の分布が使えます。

```haskell
import MCMC.Gibbs (GibbsUpdate)
import qualified Data.Map.Strict as Map
import System.Random.MWC.Distributions (normal)

-- カスタム: μ ~ Normal(0,10), y ~ Normal(μ,σ)  の条件付き事後
myMuUpdate :: [Double] -> Double -> GibbsUpdate
myMuUpdate ys sigLik params gen = do
  let n       = fromIntegral (length ys) :: Double
      ybar    = sum ys / n
      precPri = 1 / 100            -- 事前分散の逆数 (σ₀=10)
      precLik = 1 / sigLik^2
      precPos = precPri + n * precLik
      muPos   = (0 * precPri + n * ybar * precLik) / precPos
      sigPos  = sqrt (1 / precPos)
  newMu <- normal muPos sigPos gen
  return ("mu", newMu)
```
