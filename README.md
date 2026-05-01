# hanalyze

Haskell による統計解析・可視化ライブラリ。
CLI ツールとしても、Haskell ライブラリとしても使えます。

---

## ビルド

```bash
cabal build              # ライブラリ + 全実行ファイル
cabal test               # テスト
cabal run hbm-example    # HBM + 4チェーン NUTS デモ → mcmc_report*.html を生成
cabal run bench-mcmc     # MH / HMC / NUTS パフォーマンス比較
cabal run test-hmc-nuts  # HMC / NUTS 精度テスト
cabal run glmm-demo      # GLMM デモ
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
# 線形回帰 + 信頼区間
cabal run hanalyze -- data.tsv x y LM --ci 0.95

# ポアソン GLM (列ごとに多項式次数を指定)
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

## モジュール構成

```
Stat/
  Distribution.hs  -- 確率分布 (Normal / Gamma / Beta / ...)
  MCMC.hs          -- 診断統計量 (ESS / HDI / R-hat / KDE)

Model/
  HBM.hs           -- 確率的プログラミング DSL (Free Monad)

MCMC/
  Core.hs          -- Chain 型・事後統計量 (独立して使用可)
  MH.hs            -- Random Walk Metropolis-Hastings
  HMC.hs           -- Hamiltonian Monte Carlo (制約変換付き)
  NUTS.hs          -- No-U-Turn Sampler (制約変換付き)

Viz/
  MCMC.hs          -- 診断プロット (KDE / トレース / 自己相関 / ペア散布図)
  Report.hs        -- 統合 HTML レポート
  ModelGraph.hs    -- Mermaid.js DAG
  Histogram.hs     -- ヒストグラム (理論分布重ね書き対応)
  Scatter.hs       -- 散布図・回帰曲線
  Core.hs          -- PlotConfig / OutputFormat / openInBrowser
```

---

## API リファレンス

### `Stat.Distribution` — 確率分布

```haskell
import Stat.Distribution

data Distribution
  = Normal      Double Double   -- μ σ
  | Binomial    Int    Double   -- n p
  | Poisson     Double          -- λ (rate)
  | Exponential Double          -- λ (rate)
  | Gamma       Double Double   -- α (shape) β (rate)
  | Beta        Double Double   -- α β

density          :: Distribution -> Double -> Double
logDensity       :: Distribution -> Double -> Double
isContinuous     :: Distribution -> Bool
supportRange     :: Distribution -> (Double, Double)
distributionName :: Distribution -> Text
parseDistribution :: String -> [Double] -> Either String Distribution
```

```haskell
logDensity (Normal 0 1) 1.96   -- ≈ -2.837
density    (Poisson 3)  2      -- P(X=2 | λ=3)
supportRange (Beta 2 5)        -- (0.0, 1.0)
```

---

### `Stat.MCMC` — MCMC 診断統計量

```haskell
import Stat.MCMC

-- 自己相関 (lag 0..maxLag)
autocorr :: Int -> [Double] -> [(Int, Double)]

-- 最短区間 HDI
hdi :: Double -> [Double] -> (Double, Double)
-- hdi 0.94 samples → (lower, upper)

-- 実効サンプルサイズ (Geyer の初期単調列推定量)
ess :: [Double] -> Double

-- Split-R-hat 収束診断 (Vehtari et al. 2021)
-- 入力: チェーンごとのサンプルリスト (同一パラメータ)
-- R-hat < 1.01 で収束とみなす
rhat :: [[Double]] -> Maybe Double

-- Kernel Density Estimation (ガウスカーネル, Silverman バンド幅)
-- nPoints 点の (x, 密度) ペアを返す
kde :: Int -> [Double] -> [(Double, Double)]
```

```haskell
import Stat.MCMC
import MCMC.Core (chainVals)

-- ESS と R-hat の計算例
let muSamples = map (chainVals "mu") chains   -- chains :: [Chain]
    essVal    = ess (head muSamples)
    rhatVal   = rhat muSamples                -- R-hat < 1.01 = 収束

-- KDE 密度プロット用データ生成
let kdePoints = kde 200 (chainVals "mu" chain)  -- [(x, density)]
```

---

### `Model.HBM` — 確率的プログラミング DSL

```haskell
import Model.HBM
import Stat.Distribution

type Model a   -- Free Monad over ModelF

-- 潜在変数の宣言 (返り値がモデル内で依存関係を形成)
sample  :: Text -> Distribution -> Model Double
-- 観測データの条件付け (i.i.d. 仮定)
observe :: Text -> Distribution -> [Double] -> Model ()
```

#### モデル定義の例

```haskell
import Control.Monad (forM)
import qualified Data.Text as T

-- 3校の正規階層モデル
-- μ ~ Normal(0, 100),  τ ~ Exponential(0.1)
-- θ_j ~ Normal(μ, τ)  (j=1..J)
-- y_ij ~ Normal(θ_j, 5)
schoolModel :: [[Double]] -> Model [Double]
schoolModel groupData = do
  mu  <- sample "mu"  (Normal 0 100)
  tau <- sample "tau" (Exponential 0.1)
  forM (zip [1..] groupData) $ \(j, ys) -> do
    theta <- sample (T.pack ("theta_" ++ show j)) (Normal mu tau)
    observe (T.pack ("y_" ++ show j)) (Normal theta 5) ys
    return theta

-- 構造の確認
describeModel (schoolModel dat)
-- Model nodes:
--   [latent]   mu ~ Normal(0.0, 100.0)
--   [latent]   tau ~ Exponential(0.1)
--   [latent]   theta_1 ~ Normal(...)
--   [observed] y_1 ~ Normal(...)  (n=4)
--   ...
```

#### 対数密度の評価

```haskell
type Params = Map Text Double

logJoint      :: Model a -> Params -> Double  -- log p(θ, y)
logPrior      :: Model a -> Params -> Double  -- log p(θ)
logLikelihood :: Model a -> Params -> Double  -- log p(y | θ)
sampleNames   :: Model a -> [Text]            -- 潜在変数名リスト
```

```haskell
import qualified Data.Map.Strict as Map

let m  = schoolModel dat
    ps = Map.fromList [("mu", 73.0), ("tau", 10.0),
                       ("theta_1", 71.5), ("theta_2", 86.25), ("theta_3", 61.75)]

logJoint      m ps  -- ≈ -52.4
logPrior      m ps  -- ≈ -20.3
logLikelihood m ps  -- ≈ -32.1
```

---

### `MCMC.Core` — Chain 型と事後統計量

MCMC アルゴリズムに依存しない共通型。単独でインポートして使えます。

```haskell
import MCMC.Core

data Chain = Chain
  { chainSamples  :: [Map Text Double]  -- バーンイン後サンプル
  , chainAccepted :: Int
  , chainTotal    :: Int
  }

acceptanceRate   :: Chain -> Double
posteriorMean    :: Text -> Chain -> Maybe Double
posteriorSD      :: Text -> Chain -> Maybe Double
posteriorQuantile :: Double -> Text -> Chain -> Maybe Double
-- posteriorQuantile 0.025 "mu" chain  → 下側 2.5%

-- R-hat 計算などに渡すサンプル列
chainVals :: Text -> Chain -> [Double]

-- 並列チェーン用: 基底 GenIO から独立した子 GenIO を生成
spawnGen :: GenIO -> IO GenIO
```

---

### `MCMC.MH` — Random Walk Metropolis-Hastings

```haskell
import MCMC.MH
import System.Random.MWC (createSystemRandom)

data MCMCConfig = MCMCConfig
  { mcmcIterations :: Int              -- バーンイン後のサンプル数
  , mcmcBurnIn     :: Int              -- 破棄するバーンインステップ数
  , mcmcStepSizes  :: Map Text Double  -- パラメータごとの提案分布 SD
  }

defaultMCMCConfig :: [Text] -> MCMCConfig
-- mcmcIterations=2000, mcmcBurnIn=500, stepSize=1.0

metropolis       :: Model a -> MCMCConfig -> Params -> GenIO -> IO Chain
metropolisChains :: Model a -> MCMCConfig -> Int    -> Params -> GenIO -> IO [Chain]
-- metropolisChains m cfg 4 init_ gen  -- 4 チェーン並列実行 (+RTS -N で CPU 並列)
```

```haskell
main :: IO ()
main = do
  let m   = schoolModel dat
      cfg = (defaultMCMCConfig (sampleNames m))
              { mcmcIterations = 5000
              , mcmcBurnIn     = 1000
              , mcmcStepSizes  = Map.fromList
                  [("mu", 5.0), ("tau", 2.0),
                   ("theta_1", 3.0), ("theta_2", 3.0), ("theta_3", 3.0)]
              }
      init_ = Map.fromList [("mu",73),("tau",10),
                            ("theta_1",71.5),("theta_2",86.25),("theta_3",61.75)]
  gen   <- createSystemRandom
  chain <- metropolis m cfg init_ gen
  -- 目標受容率: 0.20 ~ 0.50
```

---

### `MCMC.HMC` — Hamiltonian Monte Carlo

制約付きパラメータ (Exponential / Gamma → 正値、Beta → 単位区間) を
対数変換・ロジット変換で unconstrained 空間にマッピングしてリープフロッグを行います。
Jacobian 補正が自動適用されるため、初期値は通常のパラメータ値で渡せます。

```haskell
import MCMC.HMC
import System.Random.MWC (createSystemRandom)

data HMCConfig = HMCConfig
  { hmcIterations    :: Int
  , hmcBurnIn        :: Int
  , hmcStepSize      :: Double  -- リープフロッグのステップ幅 ε
  , hmcLeapfrogSteps :: Int     -- リープフロッグのステップ数 L
  }

defaultHMCConfig :: HMCConfig
-- hmcIterations=2000, hmcBurnIn=500, hmcStepSize=0.1, hmcLeapfrogSteps=10

hmc       :: Model a -> HMCConfig -> Params -> GenIO -> IO Chain
hmcChains :: Model a -> HMCConfig -> Int    -> Params -> GenIO -> IO [Chain]
```

```haskell
main :: IO ()
main = do
  let m   = gaussianModel observed   -- μ ~ Normal(0,10), σ ~ Exponential(1)
      cfg = defaultHMCConfig
              { hmcIterations    = 3000
              , hmcBurnIn        = 500
              , hmcStepSize      = 0.1
              , hmcLeapfrogSteps = 10
              }
      init_ = Map.fromList [("mu", 0.0), ("sigma", 1.0)]
  gen   <- createSystemRandom
  chain <- hmc m cfg init_ gen
  -- σ のサンプルは必ず > 0 (対数変換により支持域外に逸脱しない)
  print (posteriorMean "sigma" chain)

  -- 4 チェーン並列
  chains <- hmcChains m cfg 4 init_ gen
```

**チューニングの目安:**
- 受容率が 60〜80% になるよう `hmcStepSize` を調整
- 階層モデルや強相関では `hmcLeapfrogSteps` を 20〜50 に増やすと効率改善

---

### `MCMC.NUTS` — No-U-Turn Sampler

Hoffman & Gelman (2014) Algorithm 3 の実装。
軌道長を U-Turn 判定で自動決定するため `hmcLeapfrogSteps` のチューニングが不要。
HMC と同様に制約変換を自動適用します。

```haskell
import MCMC.NUTS
import System.Random.MWC (createSystemRandom)

data NUTSConfig = NUTSConfig
  { nutsIterations    :: Int
  , nutsBurnIn        :: Int
  , nutsStepSize      :: Double  -- 初期ステップ幅 ε₀
  , nutsMaxDepth      :: Int     -- ツリーの最大深さ (デフォルト 10)
  , nutsAdaptStepSize :: Bool    -- バーンイン中に dual averaging でε自動調整 (デフォルト True)
  , nutsTargetAccept  :: Double  -- 目標受容率 δ (デフォルト 0.8)
  }

defaultNUTSConfig :: NUTSConfig
-- nutsIterations=2000, nutsBurnIn=500, nutsStepSize=0.1,
-- nutsMaxDepth=10, nutsAdaptStepSize=True, nutsTargetAccept=0.8

nuts       :: Model a -> NUTSConfig -> Params -> GenIO -> IO Chain
nutsChains :: Model a -> NUTSConfig -> Int    -> Params -> GenIO -> IO [Chain]
```

```haskell
main :: IO ()
main = do
  let m   = schoolModel dat
      -- nutsAdaptStepSize=True (デフォルト) でバーンイン中にεを自動調整
      cfg = defaultNUTSConfig { nutsStepSize = 0.1 }
      -- 自動調整を無効にして固定ε使用:
      -- cfg = defaultNUTSConfig { nutsStepSize = 0.08, nutsAdaptStepSize = False }
      init_ = Map.fromList [("mu",73),("tau",10),
                            ("theta_1",71.5),("theta_2",86.25),("theta_3",61.75)]
  gen <- createSystemRandom

  -- 単一チェーン
  chain <- nuts m cfg init_ gen

  -- 4 チェーン並列 (R-hat で収束確認)
  chains <- nutsChains m cfg 4 init_ gen
  let rhatMu = rhat (map (chainVals "mu") chains)
  print rhatMu  -- Just 1.001 → 収束
```

---

### 多チェーン実行と R-hat 収束診断

```haskell
import MCMC.NUTS  (nutsChains, defaultNUTSConfig, NUTSConfig (..))
import MCMC.Core  (chainVals)
import Stat.MCMC  (rhat, ess)
import System.Random.MWC (createSystemRandom)

main :: IO ()
main = do
  gen <- createSystemRandom
  let cfg = defaultNUTSConfig { nutsIterations = 2000, nutsStepSize = 0.1 }

  -- 4 チェーンを並列実行 (+RTS -N でスレッド数を指定すると CPU 並列になる)
  chains <- nutsChains model cfg 4 initParams gen

  -- パラメータごとに R-hat を確認
  let params = sampleNames model
  mapM_ (\p -> do
    let r = rhat (map (chainVals p) chains)
    putStrLn $ show p ++ ": R-hat = " ++ show r
    ) params
  -- "mu":    R-hat = Just 1.001  (< 1.01 = 収束)
  -- "sigma": R-hat = Just 1.003
```

---

### `Viz.Report` — MCMC 統合 HTML レポート (推奨)

```haskell
import Viz.Report
import MCMC.Core (Chain)
import Model.HBM (ModelGraph)

data MCMCReport = MCMCReport
  { reportTitle    :: Text
  , reportGraph    :: Maybe ModelGraph  -- Nothing でモデルグラフを省略
  , reportChain    :: Chain             -- 代表チェーン (autocorr / pair に使用)
  , reportChains   :: [Chain]           -- 並列チェーン全体 (空 = 単一チェーンモード)
  , reportParams   :: [Text]
  , reportPairs    :: [(Text, Text)]    -- ペアスキャタープロット
  , reportMaxLag   :: Int               -- 自己相関の最大ラグ
  }

defaultReport :: Text -> Chain -> [Text] -> MCMCReport
-- reportGraph=Nothing, reportChains=[], reportPairs=[], reportMaxLag=40

renderReport :: FilePath -> MCMCReport -> IO ()
```

**単一チェーンレポート:**

```haskell
let report = (defaultReport "My Model" chain names)
               { reportGraph = Just graph
               , reportPairs = [("mu", "tau")]
               }
renderReport "report.html" report
```

**多チェーンレポート (R-hat 列付き):**

```haskell
chains <- nutsChains model cfg 4 initParams gen

let report = (defaultReport "My Model" (head chains) names)
               { reportGraph  = Just graph
               , reportChains = chains   -- これを設定すると多チェーンモードになる
               , reportPairs  = [("mu", "tau")]
               }
renderReport "report_multi.html" report
```

多チェーンモードの HTML 構成:
- **Model Graph** — Mermaid.js DAG
- **Posterior Summary** — stat-box + Mean/SD/2.5%/97.5%/ESS/**R-hat** テーブル (R-hat < 1.01 は緑、≥ 1.01 は赤)
- **MCMC Diagnostics** — KDE 密度 (94% HDI) + チェーン別色分けトレース
- **Autocorrelation** — 代表チェーンの自己相関
- **Pair Scatter** — 同時事後分布散布図

---

### `Viz.MCMC` — 個別 MCMC プロット

`Viz.Report` を使わずにプロットを個別に制御したい場合。

```haskell
import Viz.MCMC
import Viz.Core (defaultConfig, OutputFormat (..))

-- 単一チェーン: [KDE | トレース] 縦並び (PyMC スタイル)
mcmcDiagnostics     :: PlotConfig -> [Text] -> Chain  -> VegaLite
mcmcDiagnosticsFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> Chain  -> IO ()

-- 多チェーン: [KDE (合算) | 色分けトレース] 縦並び
mcmcDiagnosticsMulti     :: PlotConfig -> [Text] -> [Chain] -> VegaLite
mcmcDiagnosticsMultiFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> [Chain] -> IO ()

-- 多チェーントレースのみ
multiTracePlot     :: PlotConfig -> [Text] -> [Chain] -> VegaLite
multiTracePlotFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> [Chain] -> IO ()

-- 自己相関バーチャート
autocorrPlot     :: PlotConfig -> Int -> [Text] -> Chain -> VegaLite
autocorrPlotFile :: OutputFormat -> FilePath -> PlotConfig -> Int -> [Text] -> Chain -> IO ()

-- ペアスキャタープロット (同時事後分布)
pairScatter     :: PlotConfig -> Text -> Text -> Chain -> VegaLite
pairScatterFile :: OutputFormat -> FilePath -> PlotConfig -> Text -> Text -> Chain -> IO ()

-- KDE 密度のみ / トレースのみ
posteriorPlot     :: PlotConfig -> [Text] -> Chain -> VegaLite
tracePlot         :: PlotConfig -> [Text] -> Chain -> VegaLite
```

```haskell
let cfg = defaultConfig "School Model"

-- 単一チェーン診断
mcmcDiagnosticsFile HTML "diag.html" cfg names chain

-- 多チェーン診断
mcmcDiagnosticsMultiFile HTML "diag_multi.html" cfg names chains

-- 個別プロット
autocorrPlotFile HTML "acf.html"  cfg 40 names chain
pairScatterFile  HTML "pair.html" (defaultConfig "μ vs τ") "mu" "tau" chain
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
let vals = [1.2, 3.4, 2.1, ...]
histogramWithDensityFile HTML "hist.html"
  (defaultConfig "Score Distribution") "score" vals Nothing (Normal 2.5 1.0)
```

---

## フルワークフロー例 (NUTS 4 チェーン)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Random.MWC (createSystemRandom)

import Model.HBM
import MCMC.Core  (chainVals, posteriorMean, posteriorSD)
import MCMC.NUTS  (nutsChains, defaultNUTSConfig, NUTSConfig (..))
import Stat.Distribution
import Stat.MCMC  (rhat, ess)
import Viz.Core   (openInBrowser)
import Viz.Report (MCMCReport (..), defaultReport, renderReport)

-- 1. モデル定義 (σ は Exponential 事前分布 → 正値制約を自動処理)
myModel :: [Double] -> Model Double
myModel ys = do
  mu    <- sample "mu"    (Normal 0 10)
  sigma <- sample "sigma" (Exponential 1)
  observe "y" (Normal mu sigma) ys
  return mu

main :: IO ()
main = do
  let dat   = [1.2, 2.3, 3.1, 2.8, 1.9]
      m     = myModel dat
      names = sampleNames m
      cfg   = defaultNUTSConfig { nutsIterations = 3000, nutsStepSize = 0.1 }
      init_ = Map.fromList [("mu", 0.0), ("sigma", 1.0)]

  gen <- createSystemRandom

  -- 2. 4 チェーン並列 NUTS
  chains <- nutsChains m cfg 4 init_ gen

  -- 3. R-hat で収束確認
  mapM_ (\p -> do
    let r = rhat (map (chainVals p) chains)
    putStrLn $ T.unpack p ++ ": R-hat = " ++ show r
    ) names

  -- 4. モデルグラフ定義
  let graph = buildModelGraph m [("mu", "y"), ("sigma", "y")]

  -- 5. 多チェーン統合レポート
  let report = (defaultReport "Gaussian Model" (head chains) names)
                 { reportGraph  = Just graph
                 , reportChains = chains   -- 多チェーンモード: R-hat 列 + 色分けトレース
                 , reportPairs  = []
                 }
  renderReport "report.html" report
  openInBrowser "report.html"
```

---

## サンプラー選択ガイド

| サンプラー | 向いているケース | 注意点 |
|---|---|---|
| `MCMC.MH` (Metropolis) | 簡単なモデルの動作確認 | 高次元・強相関で ESS が激減 |
| `MCMC.HMC` | 連続パラメータ、中規模モデル | `stepSize` と `leapfrogSteps` の両方を調整 |
| `MCMC.NUTS` | **ほとんどの場合の推奨** | `stepSize` だけ調整、`leapfrogSteps` 不要 |

**ステップサイズの目安:**
- NUTS 受容率は 60〜85% が理想。低すぎる → `stepSize` を小さく
- HMC 受容率は同上。低い場合は `leapfrogSteps` も減らす
- MH 受容率は 20〜50% が目安。`mcmcStepSizes` でパラメータごとに調整

**制約付きパラメータ:**
- `Exponential` / `Gamma` → 正値制約 (`PositiveT`: 対数変換)
- `Beta` → 単位区間制約 (`UnitIntervalT`: ロジット変換)
- HMC / NUTS は Jacobian 補正を自動適用するため、初期値は通常の値で渡せます

---

## 注意事項

- **ESS が低い場合**: トレースプロットで混合の悪さを確認し、ステップサイズを再調整してください。NUTS は HMC より ESS/時間が大幅に改善します。
- **R-hat が高い場合** (≥ 1.01): バーンインを増やすか、初期値を分散させるか、ステップサイズを調整してください。
- **非ルートノードの分布表示**: `collectNodes` はプレースホルダー値 `0` で潜在変数を継続するため、依存関係のあるノードの分布パラメータは意味を持ちません。モデルグラフではファミリー名のみ表示します。
- **テストデータ**: `demo/` ディレクトリに配置してください (`/tmp` は使わない)。
- **CPU 並列化**: `nutsChains` / `hmcChains` / `metropolisChains` は `+RTS -N` フラグで OS スレッド並列になります。例: `cabal run hbm-example -- +RTS -N4`
- **旧モジュール** `Model.MCMC` / `Model.HMC` / `Model.NUTS` は削除されました。`MCMC.MH` / `MCMC.HMC` / `MCMC.NUTS` を使用してください。
