# hanalyze

Haskell による統計解析・可視化ライブラリ。
CLI ツールとしても、Haskell ライブラリとしても使えます。

---

## ビルド

```bash
cabal build        # ライブラリ + 全実行ファイル
cabal test         # テスト
cabal run hbm-example   # HBM デモ → mcmc_report.html を生成
cabal run glmm-demo     # GLMM デモ
```

---

## CLI ツールとして使う

```
cabal run hanalyze -- <file> <xcols> <ycols> [LM|GLM|NoReg] [options]
```

| オプション | 説明 |
|---|---|
| `-d DIST` | 分布: `gaussian` / `binomial` / `poisson` |
| `-l LINK` | リンク関数: `identity` / `log` / `logit` / `sqrt` |
| `--degree SPEC` | 多項式次数。`N` で全列、`-1 N1 -2 N2` で列ごと指定 |
| `--ci [LEVEL]` | 信頼区間 (デフォルト 0.95) |
| `--pi [LEVEL]` | 予測区間 (Gaussian のみ) |
| `--group COL` | 混合効果モデル (LME / GLMM) |
| `--hist COL` | ヒストグラム表示 |
| `--fit DIST` | 理論分布の密度を重ね書き |

```bash
# 線形回帰
cabal run hanalyze -- data.tsv x y LM --ci 0.95

# ポアソン GLM (多項式次数指定)
cabal run hanalyze -- data.tsv "x1 x2" y GLM -d poisson -l log --degree -1 2 -2 3

# 混合効果モデル
cabal run hanalyze -- data.tsv x y LM --group school

# ヒストグラム + 正規分布フィット
cabal run hanalyze -- data.csv x y NoReg --hist score --fit normal
```

---

## ライブラリとして使う

`hanalyze.cabal` の `build-depends` に `hanalyze` を追加してください。

---

## API リファレンス

### `Stat.Distribution` — 確率分布

```haskell
import Stat.Distribution

data Distribution
  = Normal     Double Double   -- μ σ
  | Binomial   Int    Double   -- n p
  | Poisson    Double          -- λ (rate)
  | Exponential Double         -- λ (rate)
  | Gamma      Double Double   -- α (shape) β (rate)
  | Beta       Double Double   -- α β

density         :: Distribution -> Double -> Double
logDensity      :: Distribution -> Double -> Double
isContinuous    :: Distribution -> Bool
supportRange    :: Distribution -> (Double, Double)  -- (下限, 上限)
distributionName :: Distribution -> Text
parseDistribution :: Text -> Maybe Distribution
```

```haskell
-- 使用例
logDensity (Normal 0 1) 1.96  -- ≈ -2.837
density    (Poisson 3)  2     -- P(X=2 | λ=3)
supportRange (Exponential 1)  -- (0.0, Infinity)
```

---

### `Stat.MCMC` — MCMC 診断統計量

```haskell
import Stat.MCMC

autocorr :: Int -> [Double] -> [(Int, Double)]
-- ^ autocorr maxLag samples → [(lag, acf)]

hdi :: Double -> [Double] -> (Double, Double)
-- ^ 最短区間 HDI。hdi 0.94 samples → (lower, upper)

ess :: [Double] -> Double
-- ^ 実効サンプルサイズ (Geyer の初期単調列推定量)
```

---

### `Model.HBM` — 階層ベイズモデル DSL

```haskell
import Model.HBM
import Stat.Distribution

-- モデルの型
type Model a  -- Free Monad over ModelF

-- 確率変数の宣言
sample  :: Text -> Distribution -> Model Double
observe :: Text -> Distribution -> [Double] -> Model ()
```

#### モデル定義の例

```haskell
import Control.Monad (forM)
import qualified Data.Text as T

-- 3校の正規階層モデル
-- μ ~ Normal(0, 100)
-- τ ~ Exponential(0.1)
-- θ_j ~ Normal(μ, τ)  j=1..J
-- y_ij ~ Normal(θ_j, σ)   σ=5 (既知)
schoolModel :: [[Double]] -> Model [Double]
schoolModel groupData = do
  mu  <- sample "mu"  (Normal 0 100)
  tau <- sample "tau" (Exponential 0.1)
  forM (zip [1..] groupData) $ \(j, ys) -> do
    theta <- sample (T.pack ("theta_" ++ show j)) (Normal mu tau)
    observe (T.pack ("y_" ++ show j)) (Normal theta 5) ys
    return theta
```

#### 構造の確認

```haskell
-- ノード一覧
collectNodes :: Model a -> [NodeInfo]
-- NodeInfo { nodeName :: Text, nodeDist :: Distribution, nodeRole :: NodeRole }
-- NodeRole = Latent | Observed [Double]

-- 人間可読な要約
describeModel :: Model a -> Text
-- 例:
--   Model nodes:
--     [latent]   mu ~ Normal(0.0, 100.0)
--     [latent]   tau ~ Exponential(0.1)
--     [observed] y_1 ~ Normal(θ_1, 5.0)  (n=4)

-- 潜在変数名の一覧 (MCMC 初期化に使用)
sampleNames :: Model a -> [Text]
```

#### 対数密度の評価

```haskell
type Params = Map Text Double

logJoint       :: Model a -> Params -> Double  -- log p(θ, y)
logPrior       :: Model a -> Params -> Double  -- log p(θ)
logLikelihood  :: Model a -> Params -> Double  -- log p(y | θ)
```

```haskell
import qualified Data.Map.Strict as Map

let m  = schoolModel schoolData
    ps = Map.fromList [("mu", 73.0), ("tau", 10.0),
                       ("theta_1", 71.5), ("theta_2", 86.25), ("theta_3", 61.75)]

logJoint      m ps  -- ≈ -52.4
logPrior      m ps  -- ≈ -20.3
logLikelihood m ps  -- ≈ -32.1
```

---

### `Model.MCMC` — Random Walk Metropolis サンプラー

```haskell
import Model.MCMC
import System.Random.MWC (createSystemRandom)

data MCMCConfig = MCMCConfig
  { mcmcIterations :: Int              -- バーンイン後のサンプル数
  , mcmcBurnIn     :: Int              -- 破棄するバーンインステップ数
  , mcmcStepSizes  :: Map Text Double  -- パラメータごとの提案分布の SD
  }

defaultMCMCConfig :: [Text] -> MCMCConfig
-- mcmcIterations=2000, mcmcBurnIn=500, stepSize=1.0 (全パラメータ)

data Chain = Chain
  { chainSamples  :: [Params]  -- バーンイン後のサンプル列
  , chainAccepted :: Int
  , chainTotal    :: Int
  }

metropolis :: Model a -> MCMCConfig -> Params -> GenIO -> IO Chain
```

```haskell
-- 使用例
main :: IO ()
main = do
  let m     = schoolModel schoolData
      names = sampleNames m
      cfg   = (defaultMCMCConfig names)
                { mcmcIterations = 5000
                , mcmcBurnIn     = 1000
                , mcmcStepSizes  = Map.fromList
                    [("mu", 5.0), ("tau", 2.0),
                     ("theta_1", 3.0), ("theta_2", 3.0), ("theta_3", 3.0)]
                }
      init_ = Map.fromList [("mu", 73.0), ("tau", 10.0),
                            ("theta_1", 71.5), ("theta_2", 86.25), ("theta_3", 61.75)]

  gen   <- createSystemRandom
  chain <- metropolis m cfg init_ gen

  putStrLn $ "Acceptance rate: " ++ show (acceptanceRate chain)
  -- 目標: 0.20 ~ 0.50。低い場合は stepSize を小さく、高い場合は大きく。
```

#### 事後統計量

```haskell
acceptanceRate   :: Chain -> Double
posteriorMean    :: Text -> Chain -> Maybe Double
posteriorSD      :: Text -> Chain -> Maybe Double
posteriorQuantile :: Double -> Text -> Chain -> Maybe Double
-- posteriorQuantile 0.025 "mu" chain  → 下側 2.5%
-- posteriorQuantile 0.975 "mu" chain  → 上側 97.5%
```

```haskell
-- ESS の計算 (Stat.MCMC.ess を組み合わせる)
import Stat.MCMC (ess)
import qualified Data.Map.Strict as Map

let essOf pname =
      ess [v | ps <- chainSamples chain, Just v <- [Map.lookup pname ps]]
```

---

### `Model.HBM` — モデルグラフ

```haskell
data ModelGraph = ModelGraph
  { mgNodes :: [NodeInfo]      -- collectNodes の結果
  , mgEdges :: [(Text, Text)]  -- (親ノード名, 子ノード名)
  }

buildModelGraph :: Model a -> [(Text, Text)] -> ModelGraph
```

```haskell
-- エッジは明示的に指定する (DSL は Double ベースのため自動検出不可)
let edges =
      [ ("mu",  "theta_1"), ("mu",  "theta_2"), ("mu",  "theta_3")
      , ("tau", "theta_1"), ("tau", "theta_2"), ("tau", "theta_3")
      , ("theta_1", "y_1"), ("theta_2", "y_2"), ("theta_3", "y_3")
      ]
    graph = buildModelGraph m edges
```

---

### `Viz.ModelGraph` — DAG の可視化

```haskell
import Viz.ModelGraph

-- 単独 HTML ファイルとして出力
renderModelGraph :: FilePath -> Text -> ModelGraph -> IO ()

-- Mermaid.js ダイアグラム文字列だけ取得 (Viz.Report 内埋め込みに使用)
buildMermaid :: ModelGraph -> Text
```

```haskell
renderModelGraph "model_graph.html" "School Model" graph
```

---

### `Viz.Report` — MCMC 統合レポート (推奨)

複数のプロットを1つの HTML ファイルにまとめた統合ビュー。

```haskell
import Viz.Report

data MCMCReport = MCMCReport
  { reportTitle    :: Text
  , reportGraph    :: Maybe ModelGraph  -- Nothing でモデルグラフセクションを省略
  , reportChain    :: Chain
  , reportParams   :: [Text]
  , reportPairs    :: [(Text, Text)]    -- ペアスキャタープロットの変数ペア
  , reportMaxLag   :: Int               -- 自己相関の最大ラグ
  }

defaultReport :: Text -> Chain -> [Text] -> MCMCReport
-- reportGraph=Nothing, reportPairs=[], reportMaxLag=40

renderReport :: FilePath -> MCMCReport -> IO ()
```

```haskell
-- 使用例: モデルグラフ + 全診断プロット を1ファイルに出力
let report = (defaultReport "School Model" chain names)
               { reportGraph = Just graph
               , reportPairs = [("mu", "tau")]   -- μ と τ の同時事後分布
               , reportMaxLag = 40
               }
renderReport "mcmc_report.html" report
```

HTML の構成:
- **Model Graph** — Mermaid.js による DAG (latent: 青、observed: 橙)
- **Posterior Summary** — 受容率ボックス + Mean/SD/2.5%/97.5%/ESS テーブル
- **MCMC Diagnostics** — パラメータごとの事後ヒストグラム (94% HDI) + トレースプロット
- **Autocorrelation** — ラグ別自己相関バーチャート
- **Pair Scatter** — 指定した変数ペアの同時事後分布散布図

---

### `Viz.MCMC` — 個別 MCMC プロット

`Viz.Report` を使わずにプロットを個別に制御したい場合。

```haskell
import Viz.MCMC
import Viz.Core (defaultConfig, OutputFormat (..))

-- PyMC 風の [事後ヒスト | トレース] 縦並び
mcmcDiagnostics     :: PlotConfig -> [Text] -> Chain -> VegaLite
mcmcDiagnosticsFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> Chain -> IO ()

-- 自己相関バーチャート
autocorrPlot     :: PlotConfig -> Int -> [Text] -> Chain -> VegaLite
autocorrPlotFile :: OutputFormat -> FilePath -> PlotConfig -> Int -> [Text] -> Chain -> IO ()

-- ペアスキャタープロット
pairScatter     :: PlotConfig -> Text -> Text -> Chain -> VegaLite
pairScatterFile :: OutputFormat -> FilePath -> PlotConfig -> Text -> Text -> Chain -> IO ()

-- トレースのみ / 事後ヒストのみ
tracePlot     :: PlotConfig -> [Text] -> Chain -> VegaLite
tracePlotFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> Chain -> IO ()
posteriorPlot     :: PlotConfig -> [Text] -> Chain -> VegaLite
posteriorPlotFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> Chain -> IO ()
```

```haskell
let cfg = defaultConfig "My Model"
mcmcDiagnosticsFile HTML "diag.html" cfg names chain
autocorrPlotFile    HTML "acf.html"  cfg 40 names chain
pairScatterFile     HTML "pair.html" (defaultConfig "μ vs τ") "mu" "tau" chain
```

---

### `Viz.Histogram` — ヒストグラム

```haskell
import Viz.Histogram
import Viz.Core (defaultConfig, OutputFormat (..))

-- 純粋なヒストグラム
histogramPlot     :: PlotConfig -> Text -> [Double] -> Maybe Int -> VegaLite
histogramPlotFile :: OutputFormat -> FilePath -> PlotConfig -> Text -> [Double] -> Maybe Int -> IO ()

-- 理論分布の密度を重ね書き
histogramWithDensity     :: PlotConfig -> Text -> [Double] -> Maybe Int -> Distribution -> VegaLite
histogramWithDensityFile :: OutputFormat -> FilePath -> PlotConfig -> Text -> [Double] -> Maybe Int -> Distribution -> IO ()
```

```haskell
import Stat.Distribution (Distribution (..))

let vals = [1.2, 3.4, 2.1, ...]
histogramWithDensityFile HTML "hist.html"
  (defaultConfig "Score Distribution") "score" vals Nothing (Normal 0 1)
```

---

## フルワークフロー例 (HBM)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Random.MWC (createSystemRandom)

import Model.HBM
import Model.MCMC
import Stat.Distribution
import Viz.Core    (openInBrowser)
import Viz.Report  (MCMCReport (..), defaultReport, renderReport)

-- 1. モデル定義
myModel :: [[Double]] -> Model [Double]
myModel groups = do
  mu  <- sample "mu"  (Normal 0 100)
  tau <- sample "tau" (Exponential 0.1)
  forM (zip [1..] groups) $ \(j, ys) -> do
    theta <- sample (T.pack $ "theta_" ++ show j) (Normal mu tau)
    observe (T.pack $ "y_" ++ show j) (Normal theta 5) ys
    return theta

main :: IO ()
main = do
  let groupData = [[72,68,75,71], [85,88,82,90], [61,65,58,63]]
      m         = myModel groupData
      names     = sampleNames m

  -- 2. MCMC
  gen <- createSystemRandom
  let cfg = (defaultMCMCConfig names)
              { mcmcIterations = 5000, mcmcBurnIn = 1000 }
      init_ = Map.fromList [("mu",73),("tau",10),
                            ("theta_1",71.5),("theta_2",86.25),("theta_3",61.75)]
  chain <- metropolis m cfg init_ gen

  -- 3. モデルグラフ
  let edges = [("mu","theta_1"),("mu","theta_2"),("mu","theta_3"),
               ("tau","theta_1"),("tau","theta_2"),("tau","theta_3"),
               ("theta_1","y_1"),("theta_2","y_2"),("theta_3","y_3")]
      graph = buildModelGraph m edges

  -- 4. 統合レポート
  let report = (defaultReport "My HBM Report" chain names)
                 { reportGraph = Just graph
                 , reportPairs = [("mu","tau")]
                 }
  renderReport "report.html" report
  openInBrowser "report.html"
```

---

## 注意事項

- **ステップサイズの調整**: `mcmcStepSizes` を調整して受容率が 20〜50% になるようにしてください。正値制約のあるパラメータ (τ など) は小さめの値が必要な場合があります。
- **ESS が低い場合**: トレースプロットで混合の悪さを確認し、ステップサイズを再調整するか、イテレーション数を増やしてください。HMC (Phase 7) で大幅に改善される予定です。
- **非ルートノードの分布表示**: `collectNodes` はプレースホルダー値 `0` で潜在変数を継続するため、非ルートノードの分布パラメータ (例: `Normal(0, 0)`) は意味を持ちません。モデルグラフではファミリー名のみ表示します。
- **テストデータ**: `demo/` ディレクトリに配置してください (`/tmp` は使わない)。
