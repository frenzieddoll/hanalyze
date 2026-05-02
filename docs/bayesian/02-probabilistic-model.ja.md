# 確率的プログラミング DSL (Model.HBM)

> 🌐 [English](02-probabilistic-model.md) | **日本語**

> 関連デモ:
> - [`hbm-example`](../demo/HBMExample.hs) — 階層正規モデル + 4 チェーン NUTS
> - [`hbm-regression`](../demo/HBMRegressionDemo.hs) — ベイズ単回帰 (HTML レポート付き; legacy `Viz.AnalysisReport` 経由)
> - [`clinical-trial`](../demo/ClinicalTrial.hs) — Beta-Binomial A/B テスト
> - [`simpson-paradox`](../demo/SimpsonParadoxDemo.hs) — シンプソン例で LM/GLMM/HBM 比較
> - [`hbm-random-slope`](../demo/HBMRandomSlopeDemo.hs) — ランダム傾き拡張

## 概要

`Model.HBM` は Free Monad で実装した多相な確率的プログラミング DSL です。
Stan や PyMC のように宣言的にモデルを書けます。

継続を `forall a. (Floating a, Ord a) => Model a r` として多相化してあるため、
同一のモデル定義から **4 通りの解釈** を取り出せます:

| 解釈 | 特殊化 | 用途 |
|---|---|---|
| 構造検査 | `a = Double` | `collectNodes`, `describeModel` |
| log joint 評価 | `a = Double` | `logJoint`, `logPrior`, `logLikelihood` |
| AD 勾配 | `a = Forward Double` | `gradAD`, `gradADU` (machine epsilon 精度) |
| 依存追跡 | `a = Track` | `extractDeps`, `buildModelGraph` (DAG 自動抽出) |

サンプラー (`MCMC.HMC`/`NUTS`/`Gibbs`) は AD 勾配と自動制約変換を活用します。

---

## 基本 API

```haskell
import Model.HBM     -- Distribution(..), sample, observe を提供

-- 多相モデルの型エイリアス
type ModelP r = forall a. (Floating a, Ord a) => Model a r

-- 潜在変数の宣言 (返り値は a で後続の sample/observe に流れる)
sample  :: Text -> Distribution a -> Model a a

-- 観測データへの条件付け (i.i.d. 仮定)
observe :: Text -> Distribution a -> [Double] -> Model a ()
```

`sample` の返り値は `a` 型 (多相) で、後続の `sample`/`observe` の分布パラメータに
そのまま流せます (Stan の `~` 構文に相当)。

> **注**: `ModelP` は rank-2 型のため `let m = schoolModel dat` のような
> ローカル束縛では monomorphisation 問題が起きます。トップレベル束縛
> (`m :: ModelP () ; m = schoolModel dat`) を使うか、関数呼び出しで
> 毎回インライン展開してください。

---

## 使用できる分布

```haskell
data Distribution a
  = Normal      a a       -- Normal(μ, σ)    — 連続、実数全体
  | Binomial    Int a     -- Binomial(n, p)  — 離散、[0,n]
  | Poisson     a         -- Poisson(λ)      — 離散、非負整数
  | Exponential a         -- Exponential(λ)  — 連続、正値のみ
  | Gamma       a a       -- Gamma(α, β)     — 連続、正値のみ (rate=β)
  | Beta        a a       -- Beta(α, β)      — 連続、(0,1)
```

分布パラメータは多相 `a` なので、別の `sample` から得た値をそのまま渡せます
(例: `Normal mu sigma` で `mu, sigma :: a`)。

HMC/NUTS は制約付き分布 (Exponential/Gamma → 正値、Beta → 単位区間) を
自動的に unconstrained 空間に変換してサンプリングします。

---

## パターン 1: 単純な正規モデル

```haskell
-- μ ~ Normal(0, 10)
-- y_i ~ Normal(μ, σ=2)  (σ 既知)
normalMean :: [Double] -> ModelP ()
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
normalUnknownSigma :: [Double] -> ModelP ()
normalUnknownSigma ys = do
  mu    <- sample "mu"    (Normal 0 10)
  sigma <- sample "sigma" (Exponential 1)
  observe "y" (Normal mu sigma) ys
```

---

## パターン 3: A/B テスト (Beta-Binomial)

```haskell
-- p_ctrl ~ Beta(1,1),  y_ctrl ~ Binomial(50, p_ctrl), k_ctrl=18 回復
-- p_trt  ~ Beta(1,1),  y_trt  ~ Binomial(50, p_trt),  k_trt =31 回復
clinicalModel :: ModelP ()
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
schoolModel :: [[Double]] -> ModelP ()
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
sampleNames :: ModelP r -> [Text]
sampleNames (schoolModel schoolData)
-- ["mu","tau","theta_1","theta_2","theta_3"]

-- 対数密度の評価 (サンプラーのデバッグ用)
logJoint      :: ModelP r -> Params -> Double  -- log p(θ, y)
logPrior      :: ModelP r -> Params -> Double  -- log p(θ)
logLikelihood :: ModelP r -> Params -> Double  -- log p(y | θ)
```

```haskell
import qualified Data.Map.Strict as Map
let ps = Map.fromList [("mu",73),("tau",10),
                       ("theta_1",71.5),("theta_2",86.25),("theta_3",61.75)]
logJoint (schoolModel schoolData) ps  -- ≈ -52.4
```

---

## モデルグラフの生成 (依存自動抽出)

Mermaid.js の DAG を HTML で可視化します。
依存関係は `Track` 型による自動微分風の伝播で **自動抽出** されるため、
エッジを手動で書く必要はありません。

```haskell
import Model.HBM      (buildModelGraph, extractDeps)
import Viz.ModelGraph (renderModelGraph)

-- 依存グラフを自動構築 (DSL の Track 型で各ノードの parent を伝播)
let graph = buildModelGraph (schoolModel schoolData)
renderModelGraph "model.html" "School Model" graph
-- ブラウザで開くと DAG が表示される

-- ノード単位の依存抽出も可能
extractDeps (schoolModel schoolData)
-- [Node "mu"      LatentN "Normal"      {}
-- ,Node "tau"     LatentN "Exponential" {}
-- ,Node "theta_1" LatentN "Normal"      {"mu","tau"}    -- mu, tau に依存
-- ,Node "y_1"     (ObservedN 4) "Normal" {"theta_1"}    -- theta_1 に依存
-- ,...]
```

`Viz.Report.MCMCReport` の `reportGraph` フィールドにこの `ModelGraph` を渡すと、
MCMC レポート HTML 内に DAG が埋め込まれます。

---

## 観測値ごとの対数尤度

WAIC / LOO 計算 (`Stat.ModelSelect`) の内部で使われますが、
直接呼び出してデバッグにも使えます。

```haskell
perObsLogLiks :: ModelP r -> Params -> [Double]
-- 各 observe ノードの各観測値の logDensity を平坦リストで返す
```

```haskell
perObsLogLiks (schoolModel schoolData) ps
-- [-2.1, -2.3, -1.8, -2.0, ...]  (全観測値分)
```

---

## AD 勾配 (machine epsilon 精度)

`Numeric.AD.Mode.Forward` を使った正確な勾配を計算できます。
HMC/NUTS は内部でこれを使うため、通常はユーザーが直接呼ぶ必要はありません。

```haskell
gradAD  :: ModelP r -> [Text] -> [Double] -> [Double]
gradADU :: ModelP r -> [Text] -> [Transform] -> [Double] -> [Double]  -- 制約変換込み

-- ∂log p(θ,y) / ∂θ を θ=(1.5, 1.2) で評価
let g = gradAD (normalUnknownSigma obs) ["mu", "sigma"] [1.5, 1.2]
-- 数値微分 (中心差分 h=1e-5) と比較すると相対誤差は ~10⁻¹⁰ に収まる
```

`gradADU` は事前分布から検出した制約変換 (`PositiveT`/`UnitIntervalT`) を適用した
unconstrained 空間での勾配を返します (HMC/NUTS の内部で使用)。

---

## 多相解釈の仕組み

`type ModelP r = forall a. (Floating a, Ord a) => Model a r` という rank-2 型のおかげで、
同じモデル定義を異なる `a` に特殊化することで複数の解釈が得られます:

```haskell
-- a = Double           → log joint の数値評価
logJoint myModel ps :: Double

-- a = Forward s Double → AD 勾配
gradAD myModel names xs :: [Double]

-- a = Track            → 依存グラフ自動抽出
extractDeps myModel :: [Node]

-- a = Double (placeholder)
collectNodes myModel  :: [Node]    -- 構造のみ (依存情報なし)
```

`Track` 型は `Floating` インスタンスを持ち、各算術演算で依存集合 `Set Text` を伝播します。
`Normal mu sigma` を構築すると自動的に「この分布は `mu` と `sigma` に依存する」と記録されるため、
`buildModelGraph` がエッジを自動構築できます。
