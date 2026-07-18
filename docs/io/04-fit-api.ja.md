# DataFrame からモデルを学習: `df |-> spec`

> 🌐 [English](04-fit-api.md) | **日本語**

`hanalyze` は **任意のデータ源**から**任意のモデル**を学習する単一の動詞 `(|->)` を
提供する。 R の `lm(y ~ x, data = df)` 体験の Haskell 版である。

```haskell
df |-> lm "x" "y"                      -- 単回帰 (列名2つ)        → LMModel
df |-> glm Gaussian Identity "x" "y"   -- 二変量 GLM              → GLMModel
df |-> lmF  "y ~ x1 + x2"              -- formula LM (R 流)       → MultiLMModel
df |-> glmF Poisson Log "y ~ x1 + x2"  -- formula GLM             → MultiGLMModel
df |-> glmmF "y ~ x + (1|g)"           -- 線形混合モデル          → (GLMMResultRE, [Text])
df |-> hbm defaultHBM model            -- HBM (手書き確率プログラム) → HBMModel
```

★**新規の数値核は無い** — どの spec も「必要な列を取り出して既存の fit 関数
(`lmModel` / `multiLMModel` / `hbmModelPure` …) を呼ぶ」 だけの薄いラッパである。

## データ源: `ColumnSource`

「この列を数値で寄こせ」 に答えられる型はすべてデータ源になる。

```haskell
class ColumnSource d where
  lookupCol   :: Text -> d -> Maybe [Double]   -- 数値列 (無ければ Nothing)
  columnNames :: d -> [Text]
  toFrame     :: d -> DataFrame                -- formula 経路 (Phase 47) 用
```

| 源 | 配置 |
|---|---|
| `[(Text, [Double])]` | core (portable) |
| `Map Text [Double]` | core (portable) |
| `DataFrame` (Hackage `dataframe`) | core (portable) |
| `[(Text, ColData)]` (hgg) | flag `plot-integration` |

`DataFrame` 源は `toFrame = id` ゆえ factor 列・欠損が **Phase 47** 経路
(`MissingPolicy` / contrast / 応答列判定) をそのまま通る。 assoc / `Map` 源は
構造的に数値列のみ。

## 二変量近道 vs. formula

| ビルダー | 種別 | 返り型 |
|---|---|---|
| `lm "x" "y"`, `glm`, `spline`, `robust`, `quantile` | 列名2つ (便宜形) | 単変数モデル → `toPlot` ルート1 |
| `lmF`, `glmF`, `glmmF "y ~ …"` | R 流 formula | 多変量モデル → effect plot |

`F` サフィックスは *formula* の意。 二変量近道は単変数モデル型 (`LMModel` 等) を返すので
ルート1 描画コンビネータに直結する。

## pure と total

```haskell
fitWith   :: (ColumnSource d, Fit spec) => spec -> d -> Fitted spec                 -- pure (失敗=error)
fitEither :: (ColumnSource d, Fit spec) => spec -> d -> Either String (Fitted spec) -- total
(|->)     :: (ColumnSource d, Fit spec) => d -> spec -> Fitted spec                 -- = fitWith spec d
```

`(|->)` / `fitWith` は **pure だが total ではない**: 列欠落・formula parse 失敗は
`error` (Phase 50 の純粋サンプラと同じ規約)。 検証パイプラインでは `Left msg` を返す
`fitEither` を使う。

## 進捗つき IO 動詞: `df |->! spec`

```haskell
fitIO  :: (ColumnSource d, Fit spec) => spec -> d -> IO (Fitted spec)  -- 既定 = pure . fitWith
(|->!) :: (ColumnSource d, Fit spec) => d -> spec -> IO (Fitted spec)  -- = fitIO spec d
```

MCMC の学習は数秒〜数分かかるが、 純粋動詞は構造上無言。 `(|->!)` (Phase 61) は
その IO 版で、 `hbm` spec ならサンプリング中 stderr に進捗 1 行を描画する —

```
chains 2/4 done | draw 3400/8000 (warmup) | div 12 | 380.0 it/s
```

(端末では `\r` 上書き、 非 TTY では 10% ごとに 1 行)。 **動詞の選択が副作用の
選択**: `|->` は純粋で無言、 `|->!` は進捗を出す。 結果は同 cfg の純粋動詞と
**ビット一致** (chain seed の導出規則を共有) なので、 対話では `|->!` で
試しつつ test / パイプラインは `|->` のままでよい。 `hbm` 以外の spec は
既定実装 `fitIO = pure . fitWith` (error 意味論も `|->` と同じ)。

## HBM を一行で — そして `dataScatterOf`

HBM は formula ではなく手書き `ModelP` を取る。 `df |-> hbm cfg model` は df の列を
モデルのデータ slot (`dataNamedX` / `dataNamedObs` / `dataNamedIx` の三点セット。
`dataNamed` は `dataNamedX` の同義) に bind し `hbmModelPure` を走らせる
(cfg の seed で決定的)。

Phase 60.3 から束縛は次の規則 (黙殺ゼロ):

- `dataNamedX` / `dataNamedObs` slot ← 数値列 (Double / Int / **Integer** /
  Maybe 系 / 数値文字列の Text)。 **空 placeholder** (`dataNamedX "x" []`) なのに
  対応列が無ければ `fitEither` は `Left` (旧: 黙って空 `[]` で学習が走る罠)。
  実値入り placeholder の列欠落は default 続行。
- `dataNamedIx` slot (離散 index・slot 名タグ付き `Ix`) ← Int / Integer 列は直結、
  **Text factor 列は sort 順 (辞書順) levels で自動コード化** (R `factor()` /
  pandas parity・行順 shuffle に不変)。 levels は fit 後に `hbmFactorLevels m`
  で引ける (`[("g", ["A","B"])]` → コード 0 = "A")。 非整数の数値列は `Left`。

```haskell
model = do
  gs <- dataNamedIx  "g" []     -- Text factor 列 "g" から 0,1,2.. に自動コード化
  x  <- dataNamedX   "x" []     -- 値はモデル数値型 [a] (realToFrac 不要)
  ys <- dataNamedObs "y" []     -- observe に渡す生 [Double]
  ...
  let mu = b0s !!! g + b1 * xi  -- round/realToFrac 不要・DAG に g→mu エッジ
```
学習済モデルはデータを保持するので、 `dataScatterOf` を使えば df を**1 回だけ**書いて
各抽出子に観測散布図を重ねられる:

```haskell
let m = df |-> hbm defaultHBM model            -- df はここだけ
noDf |>> (dataScatterOf m "x" "y" <> toPlot (epred m "x" "mu"))
noDf |>> toPlot (forestOf m)
```

**パラメータの選択** (= ArviZ `var_names` 相当): 多変数の階層モデルで注目パラメータ
だけ見たいときは、 hgg **Phase 18** の `<>` コンビネータがそのまま効く
(per-param 抽出子は panel title に、 `forestOf` は cat 行にパラメータ名を焼いている)。
どちらも**選択 + 列挙順 = 表示順**:

```haskell
-- trace grid から 3 変数だけ (この順で縦に・chain 別重畳)
noDf |>> subplots (tracesOfWith defaultTraceOpts { toByChain = True } m)
       <> selectPanels ["b1_0", "b1_1", "sigma"] <> subplotCols 1
-- forest から群係数だけ (上から b1_0, b1_1, b1_2)
noDf |>> toPlot (forestOf m) <> scaleYDiscreteLimits ["b1_0", "b1_1", "b1_2"]
```

**サンプリング診断** (Phase 59・ArviZ 流): NUTS の発散 draw とエネルギーを図示する
抽出子。 階層モデルの funnel (発散が τ 小領域に集中) を一目で見つける:

```haskell
-- trace + 発散位置の rug (ArviZ plot_trace 流・発散 rug は tracesOf 既定 ON)。 selectPanels も効く
noDf |>> subplots (tracesOfWith defaultTraceOpts { toByChain = True } m) <> selectPanels ["tau_b1", "b1_2"]
-- joint 散布 + 発散強調 (ArviZ plot_pair(divergences=True) 流・funnel 診断の本命)
noDf |>> head (pairOf m [("tau_b1", "b1_2")])
-- marginal vs transition energy (ArviZ plot_energy 流)
noDf |>> energyOf m
-- 発散 draw の通し index (全 chain pool・mergeChains と同順)
divergencesOf m :: [Int]
```

`dagOf` は plate 内の indexed RV を 1 ノードに畳んだ PyMC `model_to_graphviz`
同等の見た目が既定 (Phase 59.3)。 indexed 個別ノードで見たいときは `dagOfRaw`。

## この API の対象外 (後続)

- **formula → HBM** 自動生成 (brms 風 `bayes "y ~ x + (1|g)"`) — 別 Phase。
- **ルート1 spec → ルート2 stat** 自動生成 (`statOf (lm "x" "y")`)。
