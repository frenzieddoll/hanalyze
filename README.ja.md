# hanalyze

> 🌐 [English](README.md) | **日本語**

Haskell による統計解析・可視化ライブラリ。
CLI ツールとしても、Haskell ライブラリとしても使えます。

---

## DSL の特徴

`Model.HBM` は多相 Free Monad DSL で、同一モデルから 4 通りの解釈を取り出せます:

```haskell
-- 一度書けば、4 通りに使える
type ModelP r = forall a. (Floating a, Ord a) => Model a r

myModel :: ModelP ()
myModel = do
  mu    <- sample "mu"    (Normal 0 10)
  sigma <- sample "sigma" (Exponential 1)
  observe "y" (Normal mu sigma) [1.5, 2.0, 1.8]
```

| 解釈 | 特殊化 | 用途 |
|---|---|---|
| 構造検査 | `a = Double` | `collectNodes`, `describeModel` |
| log joint | `a = Double` | `logJoint`, `logPrior`, `logLikelihood` |
| AD 勾配 | `a = Forward Double` | `gradAD`, `gradADU` (machine epsilon 精度) |
| 依存追跡 | `a = Track` | `extractDeps` で DAG を自動抽出 |

サンプラー (`MCMC.HMC`/`NUTS`/`Gibbs`/`MH`) は全て `ModelP` を受け取り、AD 勾配と自動制約変換 (PositiveT/UnitIntervalT) で動作します。

---

## ドキュメント (docs/)

| ページ | 内容 |
|---|---|
| [クイックスタート](docs/01-quickstart.ja.md) | ビルド・最小ワークフロー・**やりたい事 → どのデモ**早見表 |
| [確率的プログラミング DSL](docs/02-probabilistic-model.ja.md) | Model.HBM のパターン集 (Beta-Binomial / 階層正規 / 多相解釈・依存自動抽出) |
| [MCMC サンプラー選択ガイド](docs/03-mcmc-samplers.ja.md) | MH / HMC / NUTS の使い分け・チューニング・R-hat |
| [Gibbs サンプリング](docs/04-gibbs.ja.md) | 共役アップデート・ESS/s 比較 |
| [変分推論 (ADVI)](docs/05-variational-inference.ja.md) | VI vs NUTS・ELBO 収束・平均場の限界 |
| [モデル比較 (WAIC/LOO)](docs/06-model-comparison.ja.md) | WAIC・PSIS-LOO・Pareto k̂ 診断 |
| [可視化](docs/07-visualization.ja.md) | Report・Bar・Histogram・PNG/SVG 出力 |
| [PyMC 比較 & ロードマップ](docs/08-pymc-comparison.ja.md) | PyMC との機能差・実装計画 |
| [確率分布の関係図](docs/09-distribution-relationships.ja.md) | Mermaid 図で「Bin→Poi/Normal、Beta-Bin、Gamma-Poi 共役」等を可視化 |
| [学習資料 1 — 確率分布の基礎](docs/learn/01-probability-distributions.ja.md) | 全実装分布の数式・直観・用途 |
| [学習資料 2 — ベイズ統計の基礎](docs/learn/02-bayesian-basics.ja.md) | 事前/尤度/事後、共役、HBM、事後予測、ワークフロー |
| [学習資料 3 — MCMC の原理](docs/learn/03-mcmc-foundations.ja.md) | マルコフ連鎖、エルゴード性、MH、Gibbs、Slice、収束診断 |
| [学習資料 4 — HMC / NUTS](docs/learn/04-hmc-nuts.ja.md) | Hamiltonian、leapfrog、制約変換、NUTS、dual averaging、BFMI、divergence、非中心化 |
| [学習資料 5 — VI / モデル選択 / 高度トピック](docs/learn/05-vi-modelselect-advanced.ja.md) | ELBO、ADVI、WAIC、PSIS-LOO、Mixture、LKJ、AR、Censored 等の理論と使い分け |
| [回帰拡張 (Spline / Kernel / Regularized)](docs/10-regression-extensions.ja.md) | B-spline / Natural cubic / Kernel Ridge / Ridge / Lasso / ElasticNet の使い方 |
| [実験計画法 (DOE)](docs/11-design-of-experiments.ja.md) | 完全/部分要因 / ラテン方格 / 乱塊 / RSM / D-optimal / ANOVA / Power 解析 |
| [学習資料 6 — 回帰拡張の理論](docs/learn/06-regression-extensions.ja.md) | スプライン基底、カーネルメソッド、L1/L2 正則化、bias-variance tradeoff |
| [学習資料 7 — 実験計画法の理論](docs/learn/07-doe-foundations.ja.md) | 直交性、効率指標、RSM、検出力、サンプルサイズ、DOE の実務手順 |
| [多次元出力モデル](docs/12-multivariate-models.ja.md) | MultiLM / RRR / PLS / CCA / MultiGP の使い方 |
| [多目的最適化](docs/13-multi-objective-optimization.ja.md) | NSGA-II / Pareto / Bayesian MOO の使い方 |
| [学習資料 8 — 多変量回帰の理論](docs/learn/08-multivariate-theory.ja.md) | OLS / RRR / PLS / CCA / Multi-GP の数学的背景 |
| [学習資料 9 — Pareto 効率と MOO](docs/learn/09-pareto-and-moo.ja.md) | NSGA-II アルゴリズム、HV/IGD、scalarization、ZDT |
| [学習資料 10 — Bayesian Optimization](docs/learn/10-bayesian-optimization.ja.md) | EI / UCB / PI / EHVI / ParEGO / q-EHVI |

---

## ビルド

```bash
cabal build              # ライブラリ + 全実行ファイル
cabal test               # テスト
```

## デモ一覧

`cabal run <demo-name>` で実行 (HTML/PNG が生成されカレントディレクトリに出力)。
詳しい用途別の使い分けは [docs/01-quickstart.ja.md](docs/01-quickstart.ja.md) を参照。

### 入門 (まずはこれ)

| デモ | 内容 | 主に学べること |
|---|---|---|
| `hbm-example`     | 階層正規モデル + 4 チェーン NUTS → `mcmc_report*.html` | HBM DSL の書き方、MCMC レポート |
| `hbm-regression`  | ベイズ単回帰 + AnalysisReport (DAG・MCMC・信用区間) | HBM 回帰の AnalysisReport 統合 |
| `gp-demo`         | GP 回帰 (RBF/Matérn/Periodic) + LML 比較 | カーネル選択、GP の使い方 |

### モデル比較・パラドックス

| デモ | 内容 | 主に学べること |
|---|---|---|
| `simpson-paradox` | LM/GLMM/HBM をシンプソンのパラドックスで比較 → 4 つの HTML | 階層構造の重要性、モデル選択 |
| `hbm-random-slope`| ランダム切片 vs ランダム切片+ランダム傾き (M1 vs M2) を WAIC で比較 | 階層モデル拡張、WAIC によるモデル選択 |
| `clinical-trial`  | ベイズ A/B テスト (臨床試験 Beta-Binomial) | 二群比較、決定理論 |

### サンプラー深堀り

| デモ | 内容 | 主に学べること |
|---|---|---|
| `bench-mcmc`     | MH / HMC / NUTS のパフォーマンス比較 | サンプラー選択、ESS/s |
| `test-hmc-nuts`  | HMC/NUTS 精度テスト (1D ガウスで動作確認) | サンプラー検証 |
| `gibbs-demo`     | Gibbs + WAIC/LOO モデル比較 | 共役更新、モデル比較 |
| `gibbs-hbm-demo` | Gibbs × HBM DSL 統合 (共役自動検出) | 共役検出、ハイブリッド Gibbs+MH |
| `vi-demo`        | 変分推論 (ADVI) vs NUTS | VI の速度と限界 |

### 古典的回帰・可視化

| デモ | 内容 | 主に学べること |
|---|---|---|
| `glmm-demo`     | LME / GLMM (ランダム切片) | 混合効果モデル |
| `bar-demo`      | Viz.Bar (棒グラフ・積み上げ) + PNG/SVG 出力 | 可視化、画像エクスポート |

### PyMC 互換機能の追加 (このブランチ)

| デモ | 内容 | 主に学べること |
|---|---|---|
| `new-distrib-demo`  | 連続分布 6 種 (Uniform / StudentT / Cauchy / HalfNormal / HalfCauchy / LogNormal) | ロバスト事前・観測分布 |
| `discrete-obs-demo` | Bernoulli / Categorical 観測 | 離散観測尤度 |
| `ppc-demo`          | 事前/事後予測サンプリング + ベイズ p 値 | PPC ワークフロー |
| `forest-compare`    | Forest plot + Pseudo-BMA モデル比較 | 複数モデル要約、ArviZ 風出力 |
| `potential-demo`    | `pm.Potential` 相当 (ソフト制約・カスタム尤度・正則化) | 任意 log 項の追加 |
| `mixture-demo`      | `pm.Mixture` (2 成分ガウス混合) | log-sum-exp、潜在クラスタ |
| `trunc-censor-demo` | `Truncated` / `Censored` 分布 (生存解析・Tobit) | CDF を使った観測モデル |
| `cdf-test`          | Beta/Gamma/Cauchy/StudentT/HalfCauchy の CDF 検証 | 不完全ガンマ・不完全ベータ |
| `mvnormal-demo`     | `MvNormal` 観測専用 (Cholesky 経由) | 多変量観測尤度 |
| `energy-demo`       | NUTS の Energy plot + BFMI 診断 | 病的事後分布の検出 |
| `pymc-status-demo`  | PyMC parity ステータスレポート (カテゴリ別件数 + TODO 一覧) | 実装状況の可視化 |
| `summary-demo`      | Posterior summary (`az.summary` 相当) + HDI トレース + rank plot + PPC + divergence overlay | 可視化基盤 5 種 |
| `deterministic-demo` | `pm.Deterministic` で派生量 (τ=1/σ², log σ, snr=μ/σ) を Chain に保存 | 派生量の宣言 |
| `noncentered-demo`  | Neal's funnel で centered vs non-centered (BFMI 0.65→1.02, ESS 7.6x) | non-centered 化 + divergence 検出 |
| `dirichlet-demo`    | Dirichlet 事前 (stick-breaking) + Categorical 観測 → 共役解と一致 | Dirichlet latent |
| `setdata-demo`      | `withData` で訓練→テストにデータ差し替え (Rank-2 多相) | `pm.set_data` |
| `mvnormal-latent-demo` | 2D 階層モデル `μ ~ MvN([0,0], [[1,0.8],[0.8,1]])` を NUTS で推論 | MvNormal latent |
| `negbinom-demo`     | NegativeBinomial で過分散カウント (μ=10, α=2 を回復、Poisson との比較) | 過分散モデル |
| `multinomial-demo`  | Multinomial 観測 + Dirichlet 事前 (T=5 試行 × N=20、共役と完全一致) | 多項観測 |
| `zeroinflated-demo` | ZIP で構造的ゼロ (ψ=0.4) を分離回復 | ゼロ過剰 |
| `lkj-demo` / `lkj3d-demo` | LKJ(η=1) 事前で 2D / 3D の相関行列を回復 | 相関行列の事前 |
| `newdistribs-demo`  | InverseGamma / Weibull / Pareto / BetaBinomial / VonMises を一括検証 | 5 つの新規分布 |
| `ar1-demo`          | AR(1) 状態空間モデル (ϕ=0.7 を 30 ステップ系列から推定) | 時系列 |
| `slice-demo`        | Slice sampler を MH/NUTS と比較 (調整不要、勾配不要、高 ESS) | Slice 法 |

### 回帰拡張 (LM 派生モデル)

| デモ | 内容 | 主に学べること |
|---|---|---|
| `spline-demo`       | B-spline (k=3, 10 係数) と Natural cubic spline を sin 関数 + ノイズに fit (RMSE 0.05) | 非線形平滑化 |
| `kernel-demo`       | Nadaraya-Watson + Kernel Ridge、LOO-CV で bandwidth 選定 | 非パラメトリック回帰 |
| `regularized-demo`  | OLS / Ridge / Lasso / Elastic Net を sparse β=[3,-2,0,0,1.5,0,…] で比較 | 正則化と変数選択 |

### 実験計画法 (DOE)

| デモ | 内容 | 主に学べること |
|---|---|---|
| `doe-demo`          | 完全/部分要因 / ラテン方格 / 乱塊 / ANOVA / Power 解析 / 質指標を一括検証 | DOE 基本セット |
| `rsm-demo`          | CCD (rotatable/face-centered) + Box-Behnken + 二次回帰、極値推定 (0.975, -0.517, 5.06 ≈ 真値 1, -0.5, 5) | 応答曲面法 |
| `optimaldoe-demo`   | Fedorov 交換で D-optimal を構築 (D-eff=1.0、ランダム比 1.7x 改善) | 最適計画 |

> 📊 **PyMC 機能比較とロードマップ**: 詳細は [docs/08-pymc-comparison.ja.md](docs/08-pymc-comparison.ja.md) を参照。
> 全カテゴリの実装状況の棒グラフは `cabal run pymc-status-demo` で `pymc-status.html` として出力できる。

---

## CLI ツールとして使う

```
cabal run hanalyze -- <file> <xcols> <ycols> [LM|GLM|NoReg|GP|HBM] [options]
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
| `--report [FILE]` | HTML 分析レポート生成 (default: `report.html`) |
| `--waic` | WAIC / LOO-CV を計算してレポートに表示 |
| `--format FMT` | `html` / `png` / `svg`。`png/svg` はレポート内のプロットも画像化 |

```bash
# 線形回帰 + 信頼区間 + AnalysisReport
cabal run hanalyze -- data.tsv x y LM --ci 0.95 --report

# ポアソン GLM (列ごとに多項式次数を指定) + WAIC
cabal run hanalyze -- data.tsv "x1 x2" y GLM -d poisson -l log --degree -1 2 -2 3 --waic --report

# 混合効果モデル (LME) + WAIC
cabal run hanalyze -- data.tsv x y LM --group school --waic --report

# ベイズ線形回帰 (HBM): NUTS で α/β/σ の事後を推定 → AnalysisReport
cabal run hanalyze -- data.csv x y HBM --report --waic

# ガウス過程回帰 (RBF/Matérn/Periodic 比較)
cabal run hanalyze -- data.csv x y GP --report

# ヒストグラム + 正規分布フィット
cabal run hanalyze -- data.csv x y NoReg --hist score --fit normal

# レポート + プロット PNG エクスポート
cabal run hanalyze -- data.csv x y LM --report --format png
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
  ModelSelect.hs   -- モデル比較 (WAIC / PSIS-LOO)
  VI.hs            -- 変分推論 (ADVI / Adam)

Model/
  HBM.hs           -- 多相確率的プログラミング DSL (AD 勾配・Track 依存抽出対応)

MCMC/
  Core.hs          -- Chain 型・事後統計量 (独立して使用可)
  MH.hs            -- Random Walk Metropolis-Hastings
  HMC.hs           -- Hamiltonian Monte Carlo (AD 勾配)
  NUTS.hs          -- No-U-Turn Sampler (AD 勾配 + dual averaging)
  Gibbs.hs         -- Gibbs サンプリング + ハイブリッド Gibbs+MH (共役自動検出)

Viz/
  MCMC.hs          -- 診断プロット (KDE / トレース / 自己相関 / ペア散布図)
  Report.hs        -- 統合 HTML レポート (R-hat 付き多チェーン対応)
  ModelGraph.hs    -- Mermaid.js DAG
  Bar.hs           -- 棒グラフ (縦 / 横 / 積み上げ / グループ)
  Histogram.hs     -- ヒストグラム (理論分布重ね書き対応)
  Scatter.hs       -- 散布図・回帰曲線
  Core.hs          -- PlotConfig / OutputFormat / openInBrowser (PNG/SVG via vl-convert)
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

### `Model.HBM` — 多相確率的プログラミング DSL

継続を `forall a. (Floating a, Ord a) => Model a r` として多相化した DSL。
同一モデルから構造検査・log joint・AD 勾配・依存追跡の 4 通りの解釈を取り出せます。

```haskell
import Model.HBM   -- Distribution (..), sample, observe を提供

-- 多相 DSL 型
type ModelP r = forall a. (Floating a, Ord a) => Model a r

-- 潜在変数の宣言
sample  :: Text -> Distribution a -> Model a a
-- 観測データの条件付け (i.i.d. 仮定)
observe :: Text -> Distribution a -> [Double] -> Model a ()
```

#### モデル定義の例

```haskell
import qualified Data.Text as T

-- 3 校の正規階層モデル
-- μ ~ Normal(0, 100),  τ ~ Exponential(0.1)
-- θ_j ~ Normal(μ, τ)  (j=1..J)
-- y_ij ~ Normal(θ_j, 5)
schoolModel :: [[Double]] -> ModelP ()
schoolModel groupData = do
  mu  <- sample "mu"  (Normal 0 100)
  tau <- sample "tau" (Exponential 0.1)
  mapM_ (\(j, ys) -> do
    theta <- sample (T.pack ("theta_" ++ show j)) (Normal mu tau)
    observe (T.pack ("y_" ++ show j)) (Normal theta 5) ys)
    (zip [1::Int ..] groupData)

-- 構造の確認
describeModel (schoolModel dat)
-- Model nodes:
--   [latent]   mu ~ Normal
--   [latent]   tau ~ Exponential
--   [latent]   theta_1 ~ Normal
--   [observed] y_1 ~ Normal  (n=4)
--   ...
```

> **注**: rank-2 型の `ModelP` は `let` で束縛できないため、`m :: ModelP () = schoolModel dat`
> は使えません。トップレベル束縛 (`m = schoolModel dat`) を使うか、関数呼び出しで毎回インライン展開してください。

#### 4 通りの解釈

```haskell
import qualified Data.Map.Strict as Map

let ps = Map.fromList [("mu", 73.0), ("tau", 10.0), ...]

-- 1. 構造検査 (a = Double)
collectNodes  (schoolModel dat)              -- :: [Node]
describeModel (schoolModel dat)              -- :: Text

-- 2. log joint 数値評価 (a = Double)
logJoint      (schoolModel dat) ps           -- log p(θ, y)
logPrior      (schoolModel dat) ps           -- log p(θ)
logLikelihood (schoolModel dat) ps           -- log p(y | θ)

-- 3. AD 勾配 (a = Forward Double, machine epsilon 精度)
gradAD  (schoolModel dat) ["mu","tau"] [0,1] -- :: [Double]
gradADU (schoolModel dat) names trans us     -- 制約変換込み (HMC 用)

-- 4. 依存追跡 (a = Track)
extractDeps (schoolModel dat)                -- :: [Node] (nodeDeps 付き)
buildModelGraph (schoolModel dat)            -- Mermaid DAG 自動生成
```

#### 主要 API

```haskell
type Params = Map Text Double

-- インタープリタ
logJoint, logPrior, logLikelihood :: (Floating a, Ord a) => Model a r -> Map Text a -> a
sampleNames    :: ModelP r -> [Text]
collectNodes   :: ModelP r -> [Node]
describeModel  :: ModelP r -> Text
perObsLogLiks  :: ModelP r -> Params -> [Double]   -- WAIC/LOO 用

-- AD 勾配
gradAD  :: ModelP r -> [Text] -> [Double] -> [Double]
gradADU :: ModelP r -> [Text] -> [Transform] -> [Double] -> [Double]

-- 依存追跡 + DAG
extractDeps     :: ModelP r -> [Node]            -- Node に nodeDeps :: Set Text
buildModelGraph :: ModelP r -> ModelGraph        -- 依存グラフを自動構築 (手動 edge 不要)

-- 制約変換 (HMC/NUTS/VI 用)
getTransforms        :: ModelP r -> Map Text Transform   -- 事前分布から自動検出
logJointUnconstrained :: (Floating a, Ord a) => Model a r -> [Text] -> [Transform] -> Map Text a -> a

-- 構造抽出 (Gibbs 共役検出用)
runObserveDists :: Model Double r -> Map Text Double -> [(Text, Distribution Double, [Double])]
priorList       :: Model Double r -> [(Text, Distribution Double)]
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

`Numeric.AD.Mode.Forward` による正確な勾配で動作します。
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
| `MCMC.Gibbs` | 共役モデル (超高速) | 共役でないパラメータには使えない |

詳細 → [MCMC サンプラー選択ガイド](docs/03-mcmc-samplers.ja.md) / [Gibbs サンプリング](docs/04-gibbs.ja.md)

**ステップサイズの目安:**
- NUTS 受容率は 60〜85% が理想。低すぎる → `stepSize` を小さく
- HMC 受容率は同上。低い場合は `leapfrogSteps` も減らす
- MH 受容率は 20〜50% が目安。`mcmcStepSizes` でパラメータごとに調整

**制約付きパラメータ:**
- `Exponential` / `Gamma` → 正値制約 (`PositiveT`: 対数変換)
- `Beta` → 単位区間制約 (`UnitIntervalT`: ロジット変換)
- HMC / NUTS は Jacobian 補正を自動適用するため、初期値は通常の値で渡せます

---

### `MCMC.Gibbs` — Gibbs サンプリング

共役事後分布が存在するパラメータを**直接サンプリング**します。
棄却ステップがないため共役モデルでは NUTS より 3〜5 倍高い ESS/秒を達成します。

```haskell
import MCMC.Gibbs

-- 実装済み共役アップデート
normalNormal :: Text -> Double -> Double -> [Double] -> Double -> GibbsUpdate
-- μ ~ Normal(μ₀,σ₀), y ~ Normal(μ,σ_lik) の条件付き事後から直接サンプリング

betaBinomial :: Text -> Double -> Double -> Int -> Int -> GibbsUpdate
-- p ~ Beta(α,β), y ~ Binomial(n,p), k 成功 → Beta(α+k, β+n-k)

gammaPoisson :: Text -> Double -> Double -> [Double] -> GibbsUpdate
-- λ ~ Gamma(α,β), y ~ Poisson(λ) → Gamma(α+Σy, β+n)

gibbs       :: [GibbsUpdate] -> GibbsConfig -> Params -> GenIO -> IO Chain
gibbsChains :: [GibbsUpdate] -> GibbsConfig -> Int    -> Params -> GenIO -> IO [Chain]
```

```haskell
let updates = [ normalNormal "mu" 0 10 obsData 2.0 ]  -- σ_lik=2 は既知
    cfg     = defaultGibbsConfig { gibbsIterations = 5000 }
chain <- gibbs updates cfg (Map.fromList [("mu", 0.0)]) gen
```

詳細 → [Gibbs サンプリングガイド](docs/04-gibbs.ja.md)

---

### `Stat.VI` — 変分推論 (ADVI)

事後分布を正規分布族で近似し、ELBO を Adam で最大化します。
NUTS より高速ですが、平均場近似のためパラメータ間相関を無視します。

```haskell
import Stat.VI

advi :: Model a -> VIConfig -> Params -> GenIO -> IO VIResult

data VIResult = VIResult
  { viPostMeans   :: Params    -- 事後平均
  , viPostSDs     :: Params    -- 事後 SD
  , viElboHistory :: [Double]  -- ELBO 収束履歴
  , viDraws       :: [Params]  -- 事後サンプル
  }
```

```haskell
let cfg = defaultVIConfig { viIterations = 500, viNumDraws = 5000 }
result <- advi model cfg initP gen
print (viPostMeans result)
```

詳細 → [変分推論ガイド](docs/05-variational-inference.ja.md)

---

### `Stat.ModelSelect` — モデル比較 (WAIC / PSIS-LOO)

MCMC チェーンから情報量規準を計算してモデルを比較します。値が小さいほど良いモデル。

```haskell
import Stat.ModelSelect

chainWAIC :: Model a -> Chain -> WAICResult
chainLOO  :: Model a -> Chain -> LOOResult

data WAICResult = WAICResult
  { waicValue :: Double  -- WAIC (小さいほど良い)
  , waicLppd  :: Double  -- log pointwise predictive density
  , waicPwaic :: Double  -- 有効パラメータ数
  , waicSE    :: Double  -- 標準誤差
  }

data LOOResult = LOOResult
  { looValue   :: Double    -- LOO-CV (小さいほど良い)
  , looKHat    :: [Double]  -- 観測値ごとの Pareto k̂ (> 0.7 は要注意)
  , looKHatBad :: Int       -- k̂ > 0.7 の観測値数
  }
```

```haskell
let waicA = chainWAIC modelA chainA
    waicB = chainWAIC modelB chainB
printf "ΔWAIC(A−B) = %.3f\n" (waicValue waicA - waicValue waicB)
-- 負なら A が良い、|ΔWAIC| > SE が目安
```

詳細 → [モデル比較ガイド](docs/06-model-comparison.ja.md)

---

### `Viz.Bar` — 棒グラフ

```haskell
import Viz.Bar
import Viz.Core (defaultConfig, OutputFormat (..))

-- 縦棒 / 横棒
barChartFile  HTML "bar.html"  cfg "カテゴリ" "値" labels vals
barChartHFile HTML "barh.html" cfg "値" "カテゴリ" labels vals

-- 積み上げ棒 / グループ棒
stackedBarFile HTML "stacked.html" cfg "x" "y" "group" xs ys groups
groupedBarFile HTML "grouped.html" cfg "x" "y" "group" xs ys groups
```

詳細 → [可視化ガイド](docs/07-visualization.ja.md)

---

## 注意事項

- **ESS が低い場合**: トレースプロットで混合の悪さを確認し、ステップサイズを再調整してください。NUTS は HMC より ESS/時間が大幅に改善します。
- **R-hat が高い場合** (≥ 1.01): バーンインを増やすか、初期値を分散させるか、ステップサイズを調整してください。
- **非ルートノードの分布表示**: `collectNodes` はプレースホルダー値 `0` で潜在変数を継続するため、依存関係のあるノードの分布パラメータは意味を持ちません。モデルグラフではファミリー名のみ表示します。
- **テストデータ**: `demo/` ディレクトリに配置してください (`/tmp` は使わない)。
- **CPU 並列化**: `nutsChains` / `hmcChains` / `metropolisChains` は `+RTS -N` フラグで OS スレッド並列になります。例: `cabal run hbm-example -- +RTS -N4`
- **旧モジュール** `Model.MCMC` / `Model.HMC` / `Model.NUTS` は削除されました。`MCMC.MH` / `MCMC.HMC` / `MCMC.NUTS` を使用してください。
