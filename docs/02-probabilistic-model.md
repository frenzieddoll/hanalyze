# 確率的プログラミング DSL (Model.HBM)

> ## ⚠️ 重要: API 移行のお知らせ
>
> **本ページで解説している `Model.HBM` は廃止予定です。**
> 新規コードは多相版 [`Model.HBMP`](#modelhbmp-への移行-推奨) を使用してください。
> 構文はほぼ同じで、AD 勾配・依存抽出・多相解釈が追加されています。
>
> **次回メジャーバージョン**: `Model.HBM` を削除し、`Model.HBMP` を `Model.HBM` にリネームします。
> 同じく `MCMC.{HMC,NUTS,Gibbs}P` → `MCMC.{HMC,NUTS,Gibbs}` への統合を予定しています。

## 概要

`Model.HBM` は Free Monad で実装した軽量な確率的プログラミング DSL です。
Stan や PyMC のように宣言的にモデルを書けます。
内部では `logJoint`/`logPrior`/`logLikelihood` インタープリタが
モデル構造を走査してサンプラーに渡す対数密度を計算します。

`Model.HBMP` は同じ DSL の継続を多相化したもので、
`forall a. (Floating a, Ord a) => Model a r` として記述すると、
同一モデルから 4 通りの解釈 (構造検査・log joint・AD 勾配・依存抽出) を取り出せます。

---

## 基本 API

```haskell
import Model.HBM
import Stat.Distribution (Distribution (..))

-- 潜在変数の宣言
sample  :: Text -> Distribution -> Model Double

-- 観測データへの条件付け (i.i.d. 仮定)
observe :: Text -> Distribution -> [Double] -> Model ()
```

`sample` の返り値 `Double` は後続の `sample`/`observe` で依存関係を
形成するために使えます (Stan の `~` 構文に相当)。

---

## 使用できる分布

```haskell
Normal      Double Double   -- Normal(μ, σ)    — 連続、実数全体
Binomial    Int    Double   -- Binomial(n, p)  — 離散、[0,n]
Poisson     Double          -- Poisson(λ)      — 離散、非負整数
Exponential Double          -- Exponential(λ)  — 連続、正値のみ
Gamma       Double Double   -- Gamma(α, β)     — 連続、正値のみ (rate=β)
Beta        Double Double   -- Beta(α, β)      — 連続、(0,1)
```

HMC/NUTS は制約付き分布 (Exponential/Gamma → 正値、Beta → 単位区間) を
自動的に unconstrained 空間に変換してサンプリングします。

---

## パターン 1: 単純な正規モデル

```haskell
-- μ ~ Normal(0, 10)
-- y_i ~ Normal(μ, σ=2)  (σ 既知)
normalMean :: [Double] -> Model ()
normalMean ys = do
  mu <- sample "mu" (Normal 0 10)
  observe "y" (Normal mu 2) ys
```

---

## パターン 2: 制約付きパラメータ (σ 未知)

```haskell
-- μ ~ Normal(0, 10)
-- σ ~ Exponential(1)   ← HMC/NUTS が対数変換で正値を保証
-- y_i ~ Normal(μ, σ)
normalUnknownSigma :: [Double] -> Model ()
normalUnknownSigma ys = do
  mu    <- sample "mu"    (Normal 0 10)
  sigma <- sample "sigma" (Exponential 1)
  observe "y" (Normal mu sigma) ys
```

---

## パターン 3: A/B テスト (Beta-Binomial)

```haskell
import Stat.Distribution (Distribution (..))

-- p_ctrl ~ Beta(1,1),  y_ctrl ~ Binomial(50, p_ctrl), k_ctrl=18 回復
-- p_trt  ~ Beta(1,1),  y_trt  ~ Binomial(50, p_trt),  k_trt =31 回復
clinicalModel :: Model ()
clinicalModel = do
  pCtrl <- sample "p_ctrl" (Beta 1 1)
  pTrt  <- sample "p_trt"  (Beta 1 1)
  observe "y_ctrl" (Binomial 50 pCtrl) [18]
  observe "y_trt"  (Binomial 50 pTrt)  [31]
```

---

## パターン 4: 階層正規モデル (3校)

do 記法の返り値を使って下位レベルの分布パラメータを上位から受け取れます。

```haskell
import Control.Monad (forM_)
import qualified Data.Text as T

-- μ ~ Normal(0, 100)
-- τ ~ Exponential(0.1)
-- θ_j ~ Normal(μ, τ)
-- y_ij ~ Normal(θ_j, 5)
schoolModel :: [[Double]] -> Model ()
schoolModel groupData = do
  mu  <- sample "mu"  (Normal 0 100)
  tau <- sample "tau" (Exponential 0.1)
  forM_ (zip [1::Int ..] groupData) $ \(j, ys) -> do
    theta <- sample (T.pack ("theta_" ++ show j)) (Normal mu tau)
    observe (T.pack ("y_" ++ show j)) (Normal theta 5) ys

schoolData :: [[Double]]
schoolData =
  [ [72, 68, 75, 71]   -- 学校 1
  , [85, 88, 82, 90]   -- 学校 2
  , [61, 65, 58, 63]   -- 学校 3
  ]
```

---

## モデル構造の確認

```haskell
-- 潜在変数名リストの取得
sampleNames :: Model a -> [Text]
sampleNames (schoolModel schoolData)
-- ["mu","tau","theta_1","theta_2","theta_3"]

-- 対数密度の評価 (サンプラーのデバッグ用)
logJoint      :: Model a -> Params -> Double  -- log p(θ, y)
logPrior      :: Model a -> Params -> Double  -- log p(θ)
logLikelihood :: Model a -> Params -> Double  -- log p(y | θ)
```

```haskell
import qualified Data.Map.Strict as Map
let ps = Map.fromList [("mu",73),("tau",10),
                       ("theta_1",71.5),("theta_2",86.25),("theta_3",61.75)]
logJoint (schoolModel schoolData) ps  -- ≈ -52.4
```

---

## モデルグラフの生成

Mermaid.js の DAG を HTML で可視化します。
エッジ (`src → dst`) は DSL からは自動検出できないため、手動で指定します。

```haskell
import Viz.ModelGraph (buildModelGraph, modelGraphFile)
import Viz.Core (OutputFormat (..))

let graph = buildModelGraph model
              [ ("mu",  "theta_1"), ("mu",  "theta_2"), ("mu",  "theta_3")
              , ("tau", "theta_1"), ("tau", "theta_2"), ("tau", "theta_3")
              , ("theta_1", "y_1"), ("theta_2", "y_2"), ("theta_3", "y_3")
              ]
modelGraphFile HTML "model.html" graph
-- ブラウザで開くと DAG が表示される
```

---

## 観測値ごとの対数尤度

WAIC / LOO 計算 (`Stat.ModelSelect`) の内部で使われますが、
直接呼び出してデバッグにも使えます。

```haskell
perObsLogLiks :: Model a -> Params -> [Double]
-- 各 observe ノードの各観測値の logDensity を平坦リストで返す
```

```haskell
perObsLogLiks (schoolModel schoolData) ps
-- [-2.1, -2.3, -1.8, -2.0, ...]  (全観測値分)
```

---

## Model.HBMP への移行 (推奨)

### なぜ移行するのか

旧 `Model.HBM` は継続が `Double` に固定されているため、AD 勾配や依存追跡ができません。
`Model.HBMP` は継続を多相化することで以下を実現します:

- **AD 勾配** — 数値微分 (相対誤差 ~10⁻⁹) ではなく machine epsilon 精度 (~10⁻¹⁰)
- **依存グラフの自動抽出** — `extractDeps` で Mermaid DAG 用のノード/エッジを自動生成 (手動指定不要)
- **多相解釈** — 同一モデルから構造検査・log joint・勾配・依存追跡を取り出せる

### 構文の差分

```haskell
-- 旧 (Model.HBM)
import Model.HBM
import Stat.Distribution (Distribution (..))

normalModel :: [Double] -> Model ()
normalModel ys = do
  mu    <- sample "mu"    (Normal 0 10)
  sigma <- sample "sigma" (Exponential 1)
  observe "y" (Normal mu sigma) ys

-- 新 (Model.HBMP)
import Model.HBMP   -- Distribution (..), sample, observe を提供
                    -- (Stat.Distribution は不要)

normalModel :: [Double] -> ModelP ()
normalModel ys = do
  mu    <- sample "mu"    (Normal 0 10)
  sigma <- sample "sigma" (Exponential 1)
  observe "y" (Normal mu sigma) ys
```

差分は **2 点だけ**:
1. `import Model.HBM` → `import Model.HBMP` (Distribution は HBMP から取得)
2. 型注釈 `Model ()` → `ModelP ()` (`type ModelP r = forall a. (Floating a, Ord a) => Model a r`)

### サンプラーの差分

| 用途 | 旧 (廃止予定) | 新 (推奨) |
|------|--------------|----------|
| HMC | `MCMC.HMC.hmc` | `MCMC.HMCP.hmcP` |
| HMC 並列チェーン | `MCMC.HMC.hmcChains` | `MCMC.HMCP.hmcPChains` |
| NUTS | `MCMC.NUTS.nuts` | `MCMC.NUTSP.nutsP` |
| NUTS 並列チェーン | `MCMC.NUTS.nutsChains` | `MCMC.NUTSP.nutsPChains` |
| Gibbs+MH | `MCMC.Gibbs.gibbsMH` | `MCMC.GibbsP.gibbsMHP` |
| 共役自動検出 | `MCMC.Gibbs.gibbsFromModel` | `MCMC.GibbsP.gibbsFromModelP` |

シグネチャはほぼ同じです (引数順・`HMCConfig`/`NUTSConfig`/`GibbsConfig` も共通)。

### HBMP 専用機能

```haskell
-- AD 勾配 (machine epsilon 精度)
gradAD :: ModelP r -> [Text] -> [Double] -> [Double]

let g = gradAD (normalModel obs) ["mu", "sigma"] [1.5, 1.2]
-- [-15.235, -2.41]   (∇log p(θ,y) at θ=[1.5, 1.2])

-- 依存グラフの自動抽出 (Track 型による伝播)
extractDeps :: ModelP r -> [Node]   -- Node に nodeDeps :: Set Text を含む

let nodes = extractDeps (hierModel obs)
-- [Node "tau"   LatentN ...                           {}        -- 依存なし
-- ,Node "mu"    LatentN ...                           {"tau"}   -- tau に依存
-- ,Node "sigma" LatentN ...                           {}
-- ,Node "y"     (ObservedN 10) ...                    {"mu","sigma"}
-- ]
-- → そのまま Mermaid DAG 生成に使える (手動エッジ指定不要)
```

### 次回バージョンの統合計画

次回メジャーバージョンで以下のリネームを行います:

- `Model.HBM` を削除し、`Model.HBMP` を `Model.HBM` にリネーム
- `MCMC.{HMC,NUTS,Gibbs}P` を `MCMC.{HMC,NUTS,Gibbs}` に統合 (旧 API は削除)

そのため新規コードは最初から `Model.HBMP` で書くことを強く推奨します。
リネーム時の作業は import 文 1 行と型注釈 1 箇所の置換のみになります。
