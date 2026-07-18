# ベイズ階層モデル (HBM)

> 🌐 [English](03-bayesian-hbm.md) | **日本語**

> [📚 索引](README.ja.md) ｜ [01 quickstart](01-quickstart.ja.md) ｜ [02 regression](02-regression.ja.md) ｜ **03 bayesian-hbm** ｜ [04 multivariate](04-multivariate.ja.md) ｜ [05 ml](05-ml.ja.md) ｜ [06 timeseries](06-timeseries.ja.md) ｜ [07 survival](07-survival.ja.md) ｜ [08 causal](08-causal.ja.md) ｜ [09 doe](09-doe.ja.md) ｜ [10 stat](10-stat.ja.md) ｜ [11 data](11-data.ja.md) ｜ [12 plot](12-plot.ja.md)

手書きの HBM プログラムを `df |-> hbm` で当てはめ、 専用の抽出子で事後図を得る。 分布・
plate・NUTS・収束の理論は [`docs/bayesian/`](../bayesian/) のガイドが一次根拠。

| 段階 | API |
|---|---|
| モデル記述 | `ModelP` モナド (`sample` / `observe` / `dataNamed*` / `plate`) |
| 当てはめ | `df \|-> hbm defaultHBM model` (純粋・決定的) / `df \|->! …` (IO・進捗バー) |
| サンプラ (低レベル) | `nutsPure` / `nutsChainsPure` (純粋・seed で決定的) |
| 事後要約 | `printHBMSummary` / `hbmSummary` / `hbmSummaryDf` / `hbmDrawsDf` |
| 診断図 | `dagOf` / `tracesOf` / `forestOf` / `ppcOf` / `epred` |

---

## モデルを書く (`ModelP`)

```haskell
sample      :: Text -> Distribution a -> Model a a        -- 潜在変数を引く
observe     :: Text -> Distribution a -> [Double] -> Model a ()  -- 観測を尤度に入れる
dataNamedObs :: Text -> [Double] -> Model a [Double]      -- df 列を観測スロットに束ねる
dataNamedX  :: Text -> [Double] -> Model a [a]            -- df 列を予測子スロットに束ねる
dataNamedIx :: Text -> [Int]    -> Model a [Ix]           -- df の群 index 列を束ねる (slot タグ付き)
plate       :: Text -> Int -> Model a r -> Model a r      -- 反復 (Pyro/NumPyro 流の plate)
```

**plate notation の全変種** (Phase 40・いずれも `plate` の糖衣で DAG 描画にのみ作用・サンプラ不変):

```haskell
plate      :: Text -> Int -> Model a r -> Model a r             -- 素の plate bracket
plateI     :: Text -> Int -> (Int -> Model a r) -> Model a [r]  -- plate + forM  [0..n-1] (index 反復・結果 [r])
plateI_    :: Text -> Int -> (Int -> Model a r) -> Model a ()   -- plate + forM_ [0..n-1] (index 反復・破棄)
plateForM  :: Text -> [b]  -> (b -> Model a r) -> Model a [r]   -- plate + forM  rows (行リスト反復・n は自動)
plateForM_ :: Text -> [b]  -> (b -> Model a r) -> Model a ()    -- plate + forM_ rows (行リスト反復・破棄)
withPlate  :: Text -> Int -> Model a r -> Model a r             -- 低レベル primitive (nested plate 自作用・= plate)
```

**索引・slot 演算子** (indexed RV / 群効果の gather):

```haskell
indexed :: Text -> Int -> Text                 -- "theta" 0  → "theta_0" (名前生成)
(.#)    :: Text -> Int -> Text                 -- 中置版: "theta" .# 0 == "theta_0"  (infixl 9)
(!!!)   :: TrackTag b => [b] -> Ix -> b         -- bs !!! g = bs !! ixVal g + Track 解釈で slot エッジ注入 (infixl 9)
at      :: REffect a -> [Int] -> REff           -- 群効果を [Int] gids で gather (PyMC b0[gid] 同型)
atIx    :: REffect a -> [Ix]  -> REff           -- ↑ の Ix 版 (slot タグ付き・DAG に slot→観測エッジ)
data Ix = Ix { ixVal :: Int, ixSlot :: Maybe Text }   -- slot タグ付き離散 index (dataNamedIx が返す)
```

`Distribution a` は `Normal μ σ` / `HalfNormal σ` / … の 40+ 分布
([01-distributions](../bayesian/01-distributions.ja.md))。

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Hanalyze.Model.HBM (ModelP, sample, observe, dataNamedObs, Distribution (..))

model :: ModelP ()
model = do
  mu <- sample      "mu" (Normal 0 10)
  ys <- dataNamedObs "y" []          -- df の "y" 列が束ねられる
  observe "y" (Normal mu 2) ys
```

> モデルの DAG・plate 帰属の規律は [02-probabilistic-model](../bayesian/02-probabilistic-model.ja.md)、
> plate 記法は [plate-notation](../bayesian/plate-notation.ja.md) を参照。

### plate と階層モデル

同じ分布の繰り返し (群・個体) は `plate "name" n` で囲む (Pyro/NumPyro 流)。 DAG では
角丸の plate 枠 + 件数で表示され、 サンプラには影響しない (= 構造の宣言)。 階層モデルの
正準例 8-schools は **non-centered パラメタ化** (`eta ~ Normal(0,1)`、 `theta = mu + tau·eta`)
で書くと funnel を避けられる:

**最も高レベルの書き方** — 反復は `plateI` / `plateI_` (index 版・結果あり/破棄)、 indexed
名は `.#` で生成する (素の `plate` + `forM` + `"eta_" <> T.pack (show j)` を畳む):

```haskell
import qualified Hanalyze.Model.HBM as HBM
import           Hanalyze.Model.HBM ((.#))   -- 演算子は unqualified import が読みやすい

eightSchools :: [Double] -> HBM.ModelP ()
eightSchools ys = do
  mu  <- HBM.sample "mu"  (HBM.Normal 0 5)
  tau <- HBM.sample "tau" (HBM.HalfCauchy 5)
  etas <- HBM.plateI "school" 8 $ \j ->
            HBM.sample ("eta" .# j) (HBM.Normal 0 1)               -- non-centered
  HBM.plateI_ "school" 8 $ \j ->
    HBM.observe ("y" .# j) (HBM.Normal (mu + tau * etas !! j) 1) [ys !! j]
```

群が**データ駆動** (各観測がどの群か列で与えられる) なら、 `dataNamedIx` + `!!!` で
PyMC `b0[gid]` 同型に gather する (`!!!` は Track 解釈で slot→利用先エッジを DAG に出す):

```haskell
groupModel :: Int -> [Int] -> [Double] -> HBM.ModelP ()
groupModel nG gids ys = do
  bs    <- HBM.plateI "group" nG $ \g -> HBM.sample ("b" .# g) (HBM.Normal 0 1)
  gixs  <- HBM.dataNamedIx "g" gids                  -- [Ix] (由来 slot 名タグ付き)
  HBM.plateForM_ "obs" (zip gixs ys) $ \(g, yi) -> do
    mu <- HBM.deterministic "mu" (bs !!! g)          -- b[g]
    HBM.observe "y" (HBM.Normal mu 1) [yi]
```

> 糖衣の対応 (結果あり/破棄 × index/行リストの 2×2): `plateI`/`plateI_` = `plate name n (forM/forM_ [0..n-1] f)`、
> `plateForM`/`plateForM_` = `plate name (length rows) (forM/forM_ rows f)`。
> `withPlate` は nested plate 自作用の低レベル primitive。 名前生成 `.#` (= `indexed`) と
> gather `!!!` は `infixl 9` (`!!` と同優先度) なので `mu + tau * etas !! j` は括弧不要。
> random-effect 経路で gather するなら `atIx`。
> 群レベルパラメータの中心化 vs 非中心化の選び分けは
> [02-probabilistic-model パターン 4](../bayesian/02-probabilistic-model.ja.md)、 funnel の診断は
> 後述の `pairOf` / `energyOf` を見る。

---

## 当てはめる (`df |-> hbm`)

```haskell
hbm        :: HBMConfig -> ModelP () -> HBMSpec
defaultHBM :: HBMConfig
```

```haskell
import Hanalyze.Plot (hbm, defaultHBM, (|->), toPlot, forestOf)

let df = [ ("y", NumData (V.fromList [1.2,2.3,3.1,2.8,1.9])) ]
    m  = df |-> hbm defaultHBM model   -- HBMModel (純粋 NUTS・cfg の seed で決定的)
saveSVGBound "forest.svg" $ noDf |>> toPlot (forestOf m)
```

### サンプラ設定 (`HBMConfig`)

`hbm` の第 1 引数で chain 数・draws・warmup・seed・質量行列適応を設定する。 既定
`defaultHBM` は brms 相当 (4 chains × 1000 draws + 1000 warmup・質量適応 ON):

```haskell
data HBMConfig = HBMConfig
  { hbmChains    :: Int          -- chain 数 (既定 4)
  , hbmSamples   :: Int          -- post-warmup draws (既定 1000)
  , hbmWarmup    :: Int          -- warmup / burn-in (既定 1000)
  , hbmSeed      :: Maybe Word32 -- 乱数シード (Nothing = 実行ごとに変わる)
  , hbmAdaptMass :: Bool         -- 対角質量行列の適応 (既定 True)
  }

-- seed 固定で完全再現・warmup を伸ばす:
let m = df |-> hbm defaultHBM { hbmSeed = Just 42, hbmWarmup = 2000 } model
```

> **`hbmAdaptMass` は既定 ON のままにする**。 `a`/`b` と スケール `s` のように posterior の
> スケールが大きく違うモデルでは、 質量適応 OFF だと scale パラメータが収束しない
> (R̂ 悪化)。 適応 ON で `s` が収束することを実測確認済。
> 収束しないときはまず `hbmWarmup` を増やし、 funnel 由来なら non-centered 化する。

### IO 版 `|->!` (進捗バー付き)

`(|->)` は純粋で無音。 サンプリングの進捗バーが要るなら IO 版 `|->!` を使う
(**結果はビット一致**・chain は並列実行。 実 OS スレッド並列には `-threaded +RTS -N`):

```haskell
(|->!) :: (ColumnSource d, Fit spec) => d -> spec -> IO (Fitted spec)

main = do
  m <- df |->! hbm defaultHBM model      -- 進捗バーを出しつつ fit
  saveSVGBound "forest.svg" (noDf |>> toPlot (forestOf m))
```

→ [11 data](11-data.ja.md) / [`docs/io/04-fit-api.md`](../io/04-fit-api.ja.md)

**低レベル** (明示サンプラ): NUTS を直接呼んでチェーンを得る。

```haskell
nutsPure       :: ModelP r -> NUTSConfig -> Params -> Word32 -> Chain      -- 1 チェーン
nutsChainsPure :: ModelP r -> NUTSConfig -> Int -> Params -> Word32 -> [Chain]  -- 複数チェーン
```

```haskell
import qualified Data.Map.Strict as Map
import Hanalyze.MCMC.NUTS (nutsPure, defaultNUTSConfig)

let chain = nutsPure model defaultNUTSConfig (Map.fromList [("mu", 0.0)]) 42
```

---

## 事後要約 (`hbmSummary` / DataFrame 化)

学習済 `HBMModel` から要約表・事後 draw へ一発で到達する (ArviZ `az.summary` /
`idata.posterior` 相当)。 `deterministic` 派生量も既定で要約に含まれる。

### 要約する (→ `[SummaryRow]` / `IO ()`)

| 関数 | 型 | 役割 |
|---|---|---|
| `hbmSummary` | `HBMModel -> [SummaryRow]` | mean / sd / 94% HDI / ess_bulk (+ multi-chain 時 r_hat) の要約行 (純粋) |
| `printHBMSummary` | `HBMModel -> IO ()` | 上記を stdout の表で表示 |
| `hbmSummaryNames` | `HBMModel -> [Text]` | 要約対象のパラメタ名 (latent 宣言順 → deterministic 宣言順) |

### DataFrame にする (→ `DataFrame`)

| 関数 | 型 | 役割 |
|---|---|---|
| `hbmSummaryDf` | `HBMModel -> DataFrame` | 要約表を df で (列 = param / mean / sd / hdi_lo / hdi_hi / ess_bulk・multi-chain 時 + r_hat) |
| `hbmDrawsDf` | `HBMModel -> DataFrame` | 事後 draw を df で (1 パラメタ = 1 列・全 chain を chain 順に連結) |

```haskell
import Hanalyze (printHBMSummary, hbmSummaryDf, hbmDrawsDf)
import qualified Hanalyze.Data.Wrangle as W

let m = df |-> hbm defaultHBM model
printHBMSummary m          -- az.summary 風の表
-- 列: Parameter / mean / sd / hdi_3% / hdi_97% / ess_bulk (+ multi-chain 時 r_hat)。
-- 行: latent (宣言順) → deterministic 派生量 (宣言順)。

let drs = hbmDrawsDf m     -- draw を df 化 → Wrangle 動詞で自由集計
W.summarise [ "mu_mean" W.=: W.meanOf "mu"
            , "mu_q95"  W.=: W.quantileOf 0.95 "mu" ] drs
```

`SummaryRow` の中身 (低レベルで直接使う場合) と名前指定の手動経路
(`posteriorSummary` / `printPosteriorSummary`) は
[viz-diagnostics](../bayesian/viz-diagnostics.ja.md) を参照。 draw の df 集計動詞は
[11 data](11-data.ja.md) の Wrangle 節。

---

## 診断図 (抽出子)

`HBMModel` を直接取る抽出子。 モデルは自分の事後を持つので、 `epred` 以外はデータ源不要 (`noDf`)。
並びは PyMC / ArviZ のワークフローで使う頻度順 (構造 → 収束 → 推定 → 当てはまり → 深掘り → 予測)。

| 抽出子 | 型 | 図 | ArviZ 相当 |
|---|---|---|---|
| `dagOf` | `HBMModel -> DagSpec` | モデル構造 DAG (plate 折り畳み) | `model_to_graphviz` |
| `dagOfModel` / `dagOfModelWith` | `ModelP () -> DagSpec` / `[(Text,[Double])] -> ModelP () -> DagSpec` | **学習前**の構造 DAG (fit 不要) | `model_to_graphviz` |
| `tracesOf` | `HBMModel -> [VisualSpec]` | trace (param ごと・発散 rug 既定 ON) | `plot_trace` |
| `forestOf` | `HBMModel -> ForestSpec` | 94% HDI forest | `plot_forest` |
| `marginalsOf` | `HBMModel -> [VisualSpec]` | 周辺事後密度 (param ごと・KDE) | `plot_posterior` |
| `ppcOf` | `HBMModel -> Text -> PPCSpec` | 事後予測チェック (引数 = observe 名) | `plot_ppc` |
| `rankOf` | `HBMModel -> [VisualSpec]` | rank plot (chain 一様性・**要 ≥2 chain**) | `plot_rank` |
| `autocorrOf` | `HBMModel -> [VisualSpec]` | 自己相関 (param ごと・lag 0..30) | `plot_autocorr` |
| `energyOf` | `HBMModel -> VisualSpec` | energy (marginal vs ΔE・BFMI 診断) | `plot_energy` |
| `pairOf` | `HBMModel -> [(Text,Text)] -> [VisualSpec]` | 2 param joint 散布 + 発散強調 | `plot_pair` |
| `divergencesOf` | `HBMModel -> [Int]` | 発散 draw の通し index (NUTS) | — |
| `epred` | `HBMModel -> Text -> Text -> ModelSpec` | 期待値予測曲線 (予測子名, 平均ノード名) | — |
| `dashboardOf` / `dashboardFullOf` | `HBMModel -> Text -> VisualSpec` | 抽出子を束ねた診断ダッシュボード | — |
| `traceDensityOf` | `HBMModel -> VisualSpec` | trace + 事後分布だけのダッシュボード | `plot_trace` |

> 収束診断の使い分け: **R̂ が悪い** → `tracesOf` / `rankOf` (chain 分離・rank 偏り)、
> **自己相関が高い (ESS 低)** → `autocorrOf`、 **funnel / 発散** → `pairOf` / `energyOf`。

共通の import と前準備:

```haskell
import Hanalyze.Plot (hbm, defaultHBM, (|->), toPlot,
                             dagOf, tracesOf, forestOf, marginalsOf, ppcOf,
                             rankOf, autocorrOf, energyOf, pairOf, epred,
                             dashboardOf, dashboardFullOf)
import Hgg.Plot.Spec    (layer, scatter, vconcat)

let m    = df |-> hbm defaultHBM model
    noDf = [] :: [(Text, ColData)]
```

`tracesOf` / `marginalsOf` / `rankOf` / `autocorrOf` / `pairOf` は **param ごとに 1 パネル**を
返すので `vconcat` (= `subplots ss <> subplotCols 1`) で縦に束ねる。 chain 別重畳は
`tracesOfWith defaultTraceOpts { toByChain = True }`、 発散 rug を消すなら `{ toShowDivergences = False }`。
HTML (VegaLite) 経路の同等図は [viz-diagnostics](../bayesian/viz-diagnostics.ja.md) を参照。

### 診断ダッシュボード — まず全体を一目で

個別の図を見る前に `dashboardOf` で要点を一望する。 **構造** (`dagOf`・左上)・**推定値**
(`forestOf`)・**当てはまり** (`ppcOf`・観測 vs 事後予測の密度重ね)・**サンプラ健全性**
(`energyOf`・BFMI) の 2×2。 各 1 パネルゆえ param 数に依らず見やすい (引数は `ppc` 用の
observe ノード名。 係数が増えても forest が縦に密になるだけ):

```haskell
noDf |>> dashboardOf m "obs"
```

![compact dashboard 2×2: 構造 DAG / 推定値 forest / 当てはまり PPC / サンプラ健全性 energy](../images/hbm-dashboard.svg)

収束 (R̂) まで含め徹底点検するなら `dashboardFullOf`。 上段は同じ 2×2、 その下に param ごと
**[事後分布 (左) | trace (右)]** を 2 列で連結する (ArviZ `plot_trace` 流・chain は色違いで重畳)。
全体が 1 つの 2 列グリッドなので **係数が増えると下に行が増えるだけ**:

```haskell
noDf |>> dashboardFullOf m "obs"
```

![full dashboard: 上段 2×2 + param ごと 事後分布｜trace](../images/hbm-dashboard-full.svg)

trace と事後分布だけを見たいなら `traceDensityOf` (= ArviZ `plot_trace` 相当・param ごと
**[事後分布｜trace]** を 2 列で・chain は色違い)。 収束 (定常・chain 一致) と事後の形を同時に確認:

```haskell
noDf |>> traceDensityOf m
```

![traceDensityOf: param ごと 事後分布｜trace (ArviZ plot_trace 相当)](../images/hbm-trace-density.svg)

### 個別の診断図

ワークフロー順に 1 枚ずつ (図と読み方を 1 単位で示す)。

**モデル構造 (`dagOf`)** — サンプリング前に確率変数の依存を確認する (PyMC `model_to_graphviz`)。

```haskell
noDf |>> toPlot (dagOf m)
```

![dagOf: 確率変数の依存 DAG (plate 折り畳み)](../images/hbm-dag.svg)

> **学習前にモデルの形だけ見る** (`dagOfModel` / `dagOfModelWith`): `dagOf` は学習済 `HBMModel` を
> 取るが、 DAG は事後に依らないので **fit (サンプリング) せず** に生の `ModelP` から直接描ける
> (PyMC `pm.model_to_graphviz(model)` 相当)。 plate サイズを**データ長から決める**モデル
> (`plateForM_` / `observeColumns`) は、 データ未束縛だとループ本体 (mu / obs) が出ないため
> `dagOfModelWith` でデータを束ねてから描く (それでも **NUTS は走らない**)。 plate を明示
> (`plate name N` / `plateI`) するモデルは `dagOfModel` だけで完全に出る。
>
> ```haskell
> noDf |>> toPlot (dagOfModelWith [("x", xs), ("y", ys)] model)  -- 学習前・サンプリングなし
> noDf |>> toPlot (dagOfModel model)                              -- 明示 plate モデルはデータ不要
> ```
>
> ![dagOfModelWith: 学習前にモデルから直接描いた DAG (fit 不要)](../images/hbm-dag-model.svg)

**trace (`tracesOf`)** — 収束の第一確認。 chain が同じ帯を毛虫状に往復し定常なら OK。 発散は
下端の赤 rug (既定 ON)。

```haskell
noDf |>> vconcat (tracesOf m)
```

![tracesOf: 各 param の trace (発散 rug 既定 ON)](../images/hbm-trace.svg)

**HDI forest (`forestOf`)** — 全 param の点推定 + 94% HDI を 1 枚で並べて比較する。

```haskell
noDf |>> toPlot (forestOf m)
```

![forestOf: 94% HDI forest](../images/hbm-forest.svg)

**周辺事後密度 (`marginalsOf`)** — param ごとの事後分布の形 (PyMC `plot_posterior`)。

```haskell
noDf |>> vconcat (marginalsOf m)
```

![marginalsOf: param ごとの周辺事後密度](../images/hbm-marginals.svg)

**事後予測チェック (`ppcOf`)** — モデルが生む複製データ y_rep (青) の分布に観測 (黒) が収まるかで
**当てはまり**を点検する (PyMC `plot_ppc`)。 引数は observe ノード名。

```haskell
noDf |>> toPlot (ppcOf m "obs")
```

![ppcOf: 観測 (黒) vs 各 draw の事後予測 y_rep (青)](../images/hbm-ppc.svg)

**rank plot (`rankOf`)** — 収束の補助。 各ビンで chain がほぼ同高なら rank 一様 = 収束 (要 ≥2 chain)。

```haskell
noDf |>> vconcat (rankOf m)
```

![rankOf: 各ビンで chain が同高なら収束 (rank 一様)](../images/hbm-rank.svg)

**自己相関 (`autocorrOf`)** — lag 0=1 から速く 0 に減衰すれば mixing 良好 (ESS 高)。

```haskell
noDf |>> vconcat (autocorrOf m)
```

![autocorrOf: 速く 0 に減衰すれば mixing 良好](../images/hbm-autocorr.svg)

**energy (`energyOf`)** — HMC / NUTS の健全性。 ΔE (橙) が marginal E (青) より著しく狭いと
低 BFMI = 探索不足 (下図は funnel な中心化 8-schools fit)。

```haskell
noDf |>> energyOf m
```

![energyOf: ΔE (橙) が marginal E (青) より狭いと低 BFMI](../images/hbm-energy.svg)

**pair plot (`pairOf`)** — funnel / 発散の深掘り。 2 param の joint 散布に発散 (赤) を重ね、
漏斗の首に集中するのを見る (PyMC `plot_pair(divergences=True)`・下図は 8-schools の τ–θ funnel)。

```haskell
noDf |>> head (pairOf m [("tau", "theta_1")])
```

![pairOf: τ–θ の漏斗の首に発散 (赤) が集中](../images/hbm-pair.svg)

### 期待値予測 (`epred`) — 予測子に沿った事後予測

`epred` は **deterministic 平均ノードを予測子に沿って grid 評価**し、 散布データに事後予測の
曲線 + 帯を重ねる (頻度論の回帰 effect plot に相当)。 既定は 94% HDI 帯・grid 100 点で、 `<>` で
`grid` / `statLevel` 等を合成できる:

```haskell
df |>> layer (scatter "x" "y") <> toPlot (epred m "x" "mu")            -- 散布 + 事後予測曲線
df |>> layer (scatter "x" "y") <> toPlot (epred m "x" "mu" <> grid 200 <> statLevel 0.9)
```

![epred: 散布 + 事後予測平均 + 94% HDI 帯](../images/hbm-epred.svg)

**多予測子モデル** (`mu = a + b*x1 + c*x2` 等) では、 軸にしない予測子を頻度論 effect plot と
**同じ語彙** (`holdAt` / `byVar`、 [02-regression](02-regression.ja.md)) で固定する (既定は非軸を
平均 `Mean` で固定):

```haskell
import Hanalyze.Plot (holdAt, byVar, HoldAgg (..))   -- Mean / Median / Fixed [(name, val)]

noDf |>> toPlot (epred m "x1" "mu" <> holdAt Median)                  -- x2 を中央値で固定
noDf |>> toPlot (epred m "x1" "mu" <> holdAt (Fixed [("x2", 5)]))     -- x2 を 5 に固定
noDf |>> toPlot (epred m "x1" "mu" <> byVar "x2" [0, 1] <> grid 200)  -- x2 の水準別に色分け重畳
```

**CI / PI / ファンチャート** — 既定の帯は μ の事後 HDI (頻度論の **CI** 相当) だが、 頻度論モデルと
**同じ語彙** `bandMode` ([02-regression](02-regression.ja.md)) で観測ノイズ込みの **予測区間 (PI)** や
その入れ子へ切り替えられる。 PI は観測ノードの予測分布をモデルから自動検出してサンプルするので
観測ノード名は不要 (頻度論と完全対称)・任意の観測分布 (Normal/Poisson/NegBinom…) に効く:

```haskell
import Hanalyze.Plot (bandMode, BandMode (..))   -- BandCI / BandPI / BandCIPI / BandOff

noDf |>> toPlot (epred m "x" "mu")                     -- 既定 = CI (μ の事後 HDI)
noDf |>> toPlot (epred m "x" "mu" <> bandMode BandPI)   -- PI (観測ノイズ込み・CI より広い)
noDf |>> toPlot (epred m "x" "mu" <> bandMode BandCIPI) -- 外=PI 薄・内=CI 濃 のファンチャート
```

`statModel m <> bandMode BandPI` (頻度論) と**字面が一致**する。 PI のサンプリングは固定 seed で
純粋・決定的に閉じる (`epred` の純粋 `ModelSpec` 性は不変)。

![epred の CI / PI / CIPI 帯 (bandMode で切替・頻度論と同綴り)](../images/hbm-epred-pi.svg)

---

## 関連

- 分布マップ: [01-distributions](../bayesian/01-distributions.ja.md)
- モデル記述・DAG・plate: [02-probabilistic-model](../bayesian/02-probabilistic-model.ja.md) / [plate-notation](../bayesian/plate-notation.ja.md)
- サンプラ (MH/HMC/NUTS/Gibbs/Slice): [03-mcmc-samplers](../bayesian/03-mcmc-samplers.ja.md)
- モデル比較 (WAIC/LOO): [06-model-comparison](../bayesian/06-model-comparison.ja.md)
- 診断図の作り方: [viz-diagnostics](../bayesian/viz-diagnostics.ja.md)
