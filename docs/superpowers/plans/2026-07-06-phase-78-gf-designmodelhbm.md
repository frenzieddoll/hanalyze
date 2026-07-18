# Phase 78.G-f `designModelHBM` (階層ベイズ DOE) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** DOE 設計 (`Design`) を **真の階層ベイズモデル** (`y ~ 固定効果 + (1|lot)`) で当てはめる高レベル API `designModelHBM` を追加し、profiler/contour に**事後予測帯**を開通する。

**Architecture:** `designModel` (LM) / `designModelGP` (GP) の隣に `designModelHBM` を純追加する。固定効果の設計行列は `designFormula plan` (factorial=交互作用 / RSM=2次) を `designMatrixF` で数値行列化して得る。ランダム効果は**型付き RE 項** (`RandomSpec`) をグルーピング列から index 化し、**手書き `ModelP`** (`sample` で係数/σ/τ、`reNormal` で群効果、`observeLMR` で観測) を組んで既存 `hbm` (NUTS) で学習する。学習済 `HBMModel` から係数・σ の事後 draw を取り出して `DesignHBMFit` に保持し、その `MultiVarModel` instance が評価点で**事後予測帯** (`mean ± z·√(Var(μ_draws)+σ̄²)`) を返す。描画層 (profiler/contour) は `MultiVarModel` class 越しで**無改修**。

**Tech Stack:** Haskell (GHC 9.6.7)・既存 `Hanalyze` (Fit / Model.HBM / Model.Formula.Design / Plot.Core / Plot.ML)・hspec。

## Global Constraints

- 事実: **HBM に formula 文字列→モデルの経路は無い** (`Fit.hs:1540`)。モデルは**手書き `ModelP`**。文字列 formula は固定効果**設計行列の構築**にのみ使う (OLS 版 `designModel` と同じ)。
- グルーピング指定は**型付き `RandomSpec`** (`Model/Formula/Mixed.hs:48`)。裸の列名リスト・lme4 文字列リストは**不可** (設計確定)。
- 推定 = **ベイズ NUTS** (`hbm` 経由)。頻度論 `fitMixedLME` は使わない。
- 帯 = **事後予測帯** (G-e GP と parity)。`MultiVarModel` の CI slot に載せる。PI slot は `Nothing`。
- v1 scope = **解析側 a のみ** (既存/sim データを群込みで fit)。設計*生成*側 (blocked/split-plot 最適計画) は後段 sub / 別 Phase。
- **連続因子の固定効果に加え `Cat`/群因子をランダム効果に使える** (GP 版が `Cat` error にしたのと逆)。
- コーディング規約: インデント 2 スペース・`camelCase`/`PascalCase`・モジュール/セクションに日本語コメント。
- ブランチ = `feature/phase-78-doe-workflow` (現在ここ)。commit メッセージに `Phase 78.G-f` を含める (guard-phase-branch hook)。commit 後は push。
- 全 test green を維持 (現状 1286 example)。既存 `designModel`/`designModelGP` は**無改変**。

---

## File Structure

- `src/hanalyze/Analyze/Fit.hs` — `designModelHBM` spec 型 + smart ctor + `Fit` instance + `DesignHBMFit` 型 + ModelP ビルダー。`designModelGP` (1422-1435) の直後に追加。export list に追記。
- `src/hanalyze/Analyze/Plot/ML.hs` — `instance MultiVarModel DesignHBMFit` (事後予測帯)。`instance MultiVarModel GPRegModelN` (1744) の直後に追加。
- `src/hanalyze/Analyze/Plot.hs` — re-export に `designModelHBM` / `ranIntercept` / `ranSlope` / `DesignHBMFit` を追記。
- `test/hanalyze/Analyze/Design/WorkflowSpec.hs` — fit/収束/帯/error の hspec。
- `docs/api-guide/09-doe.md` — 「階層モデル (mixed-effects DOE)」節 + 図。
- `demo-plot/PlotIntegrationDemo.hs` (or 既存 DOE demo) — profiler 図生成。`scripts/gen-doc-figures.sh` に登録。

**Interfaces (全タスク共通で参照する確定シグネチャ):**
```haskell
-- 既存 (再利用・無改変)
parseFormula   :: Text -> Either String Formula                                  -- Model.Formula
designFormula  :: Design -> Text -> Text                                         -- Design.Workflow
designMatrixF  :: Formula -> ModelFrame -> Either String (LA.Matrix Double, [Text]) -- Model.Formula.Design
responseVec    :: ModelFrame -> Either String (V.Vector Double)                  -- Model.Formula.Design
buildGroups    :: V.Vector Text -> (V.Vector Text, V.Vector Int, V.Vector Int)   -- Model.GLMM (labels, idx, sizes)
sample         :: Text -> Distribution a -> Model a a                            -- Model.HBM
reNormal       :: Num a => Text -> Int -> Text -> a -> Model a (REffect a)        -- Model.HBM
at             :: REffect a -> [Int] -> REff                                     -- Model.HBM
observeLMR     :: Text -> [Text] -> [[Double]] -> [REff] -> LMFamily -> [Double] -> Model a ()
hbm            :: HBMConfig -> ModelP () -> HBMSpec                              -- Fit (df |-> hbm cfg model :: HBMModel)
hbmDraws       :: Text -> HBMModel -> [Double]                                   -- Plot.Bayes (named param 事後 draw)
defaultHBM     :: HBMConfig
data RandomSpec = RandomSpec { rsIntercept :: Bool, rsSlopes :: [Text], rsGroup :: Text }  -- Model.Formula.Mixed
data ModelFrame = ModelFrame { mfRoles :: [(Text, Role)], mfNRows :: Int }       -- Role = RoleContinuous v | RoleResponse v | (factor)
class MultiVarModel m where
  mvFrame       :: m -> ModelFrame
  mvEvalFrame   :: m -> Double -> ModelFrame -> ([Double], Maybe ([Double], [Double]))
  mvEvalFramePI :: m -> Double -> ModelFrame -> Maybe ([Double], [Double])       -- 既定 Nothing
```

---

### Task 1: `RandomSpec` smart constructor (`ranIntercept` / `ranSlope`)

**Files:**
- Modify: `src/hanalyze/Analyze/Fit.hs` (export list + 定義。`designModelGP` の直前あたり)
- Test: `test/hanalyze/Analyze/Design/WorkflowSpec.hs`

**Interfaces:**
- Consumes: `RandomSpec (..)` from `Hanalyze.Model.Formula.Mixed` (import 追加)。
- Produces:
  ```haskell
  ranIntercept :: Text -> RandomSpec           -- (1|g)
  ranSlope     :: [Text] -> Text -> RandomSpec  -- (1 + s1 + s2 | g)  (intercept 込み)
  ```

- [ ] **Step 1: 失敗するテストを書く**

`WorkflowSpec.hs` の describe ブロックに追加:
```haskell
    it "ranIntercept は (1|g) 相当の RandomSpec" $
      ranIntercept "lot" `shouldBe` RandomSpec True [] "lot"
    it "ranSlope は intercept 込みの (1+s|g)" $
      ranSlope ["temp"] "lot" `shouldBe` RandomSpec True ["temp"] "lot"
```
import に `RandomSpec (..)` と `ranIntercept, ranSlope` を追加。

- [ ] **Step 2: 失敗を確認**

Run: `cabal test analyze-test --test-options='-m "ranIntercept"'`
Expected: FAIL (`ranIntercept` not in scope)。

- [ ] **Step 3: 最小実装**

`Fit.hs` に追加 (import に `Hanalyze.Model.Formula.Mixed (RandomSpec (..))`):
```haskell
-- | 型付きランダム効果項 (Phase 78.G-f)。lme4 の @(1|g)@ / @(1+s|g)@ を型で表す。
--   文字列 formula を経由せず 'designModelHBM' に渡す。
ranIntercept :: Text -> RandomSpec
ranIntercept g = RandomSpec True [] g

ranSlope :: [Text] -> Text -> RandomSpec
ranSlope slopes g = RandomSpec True slopes g
```
export list に `ranIntercept`, `ranSlope` を追加。

- [ ] **Step 4: テスト通過を確認**

Run: `cabal test analyze-test --test-options='-m "ran"'`
Expected: PASS (2 例)。

- [ ] **Step 5: commit**

```bash
git add src/hanalyze/Analyze/Fit.hs test/hanalyze/Analyze/Design/WorkflowSpec.hs
git commit -m "add ranIntercept/ranSlope: 型付き RandomSpec smart ctor (Phase 78.G-f)"
git push
```

---

### Task 2: ModelP ビルダー `designHBMProgram` (純粋関数)

固定効果設計行列 + グルーピング + prior から**手書き `ModelP ()`** を組む純粋関数。ここが G-f の核心。

**Files:**
- Modify: `src/hanalyze/Analyze/Fit.hs`
- Test: `test/hanalyze/Analyze/Design/WorkflowSpec.hs`

**Interfaces:**
- Consumes: `parseFormula`, `designMatrixF`, `responseVec`, `buildGroups`, `sample`, `reNormal`, `at`, `observeLMR`, `LMGaussian`, `Normal`, `HalfNormal` (すべて確定シグネチャ)。
- Produces:
  ```haskell
  -- 事前に検証済の材料から ModelP を組む。σ/τ prior は v1 固定 (弱情報)。
  designHBMProgram
    :: [[Double]]   -- ^ designX 行 = obs, 列 = 固定効果 (designMatrixF の行列を [[Double]] 化)
    -> [Text]       -- ^ betaNames (designMatrixF が返す列名)
    -> [([Int], Int, Bool, [Text])]  -- ^ 各 RE: (群 idx/行, 群数 nG, intercept?, slope 変数名)
    -> [[Double]]   -- ^ slope 用の生列 (行=obs) — v1 では intercept のみ使用 (下記 scope)
    -> [Double]     -- ^ ys
    -> ModelP ()
  ```
  ★v1 scope: **random intercept のみ**実装 (`rsSlopes` は Task で error にし、slope は後段)。上記 slope 引数は将来用に受けるが空で通す。

- [ ] **Step 1: 失敗するテストを書く**

モデルが構築でき、既知の lot 差データで NUTS 学習が有限の事後を返すことを検証する統合テスト (Task 3 の fit 経由で確認するのが自然なので、ここでは**ビルダーが例外なく `ModelP` を返し `sampleNames` に betaNames と `u_<g>` が現れる**ことだけを確認):
```haskell
    it "designHBMProgram は betaNames と群効果を含む ModelP を組む" $ do
      let x   = [[1,0],[1,1],[1,0],[1,1]]      -- intercept + temp
          bn  = ["b0","temp"]
          res = [([0,0,1,1], 2, True, [])]     -- lot: 行0,1=群0 / 行2,3=群1
          ys  = [1.0, 2.0, 1.1, 2.2]
          prog = designHBMProgram x bn res [] ys
          nms  = sampleNames prog
      nms `shouldContain` ["b0"]
      nms `shouldContain` ["temp"]
      nms `shouldContain` ["u_g0_0"]           -- reNormal "u_g0" が展開する群 0 効果名 (実際の命名規則は下記で確認)
```
> 注: `reNormal base nG ...` の群効果名の実命名規則は `Model/HBM/Model.hs:278-291` を読んで `shouldContain` の期待値を合わせること (`base <> "_" <> show j` 形)。`sampleNames :: ModelP r -> [Text]` は `Model.HBM` から import。

- [ ] **Step 2: 失敗を確認**

Run: `cabal test analyze-test --test-options='-m "designHBMProgram"'`
Expected: FAIL (`designHBMProgram` not in scope)。

- [ ] **Step 3: 最小実装**

`Fit.hs` に追加:
```haskell
-- | DOE 階層モデルの手書き 'ModelP' (Phase 78.G-f・核心)。固定効果 = designX·β
--   (β に弱情報 prior)、ランダム切片 = 群ごと 'reNormal'、観測 = 'observeLMR'。
--   ★HBM に formula 文字列を食わせる経路は無い (Fit.hs の方針) ため手書きで組む。
--   v1 = random intercept のみ (slope は error 済で未到達)。
designHBMProgram :: [[Double]] -> [Text] -> [([Int], Int, Bool, [Text])] -> [[Double]] -> [Double] -> ModelP ()
designHBMProgram designX betaNames res _slopeCols ys = do
  -- 固定効果係数: 各 betaName を弱情報 Normal(0,10) で宣言 (observeLMR が名前で参照)
  mapM_ (\nm -> sample nm (Normal 0 10)) betaNames
  -- 観測ノイズ SD
  _sigma <- sample "sigma" (HalfNormal 5)
  -- 各ランダム効果 (v1: intercept のみ)
  reffs <- mapM mkRE (zip [0 ..] res)
  observeLMR "y" betaNames designX reffs (LMGaussian "sigma") ys
  where
    mkRE :: (Int, ([Int], Int, Bool, [Text])) -> Model a REff
    mkRE (gi, (idxRow, nG, _intercept, _slopes)) = do
      let base = "u_g" <> T.pack (show gi)
      tau <- sample ("tau_" <> base) (HalfNormal 5)
      u   <- reNormal base nG ("tau_" <> base) tau
      pure (u `at` idxRow)
```
> `Model a REff` / `ModelP` / `sample` / `reNormal` / `at` / `observeLMR` / `LMGaussian` / `Normal` / `HalfNormal` を `Hanalyze.Model.HBM` から import。`Distribution` の `Normal`/`HalfNormal` は同モジュール。

- [ ] **Step 4: テスト通過を確認**

Run: `cabal test analyze-test --test-options='-m "designHBMProgram"'`
Expected: PASS。`sampleNames` に `b0`/`temp`/群効果名が含まれる。

- [ ] **Step 5: commit**

```bash
git add src/hanalyze/Analyze/Fit.hs test/hanalyze/Analyze/Design/WorkflowSpec.hs
git commit -m "add designHBMProgram: DOE 階層モデルの手書き ModelP ビルダー (Phase 78.G-f)"
git push
```

---

### Task 3: `DesignHBMFit` 型 + `designModelHBM` Fit instance

材料を data から用意し `designHBMProgram` を組んで `hbm` で学習、事後 draw を `DesignHBMFit` に格納する。

**Files:**
- Modify: `src/hanalyze/Analyze/Fit.hs`
- Test: `test/hanalyze/Analyze/Design/WorkflowSpec.hs`

**Interfaces:**
- Consumes: Task 2 の `designHBMProgram`・`designFormula`・`parseFormula`・`designMatrixF`・`responseVec`・`buildGroups`・`hbm`・`hbmDraws`。ModelFrame 構築は**既存 LM 経路を踏襲** (`Plot/Linear.hs:47` / `Model/Wrappers.hs:220` の `designMatrixF f mf` 呼び出しで `mf` を得る手順をそのまま読んで再利用する — 同じ helper でデータフレーム→ModelFrame を作る)。
- Produces:
  ```haskell
  data DesignHBMFit = DesignHBMFit
    { dhfFormula   :: !Formula      -- 固定効果 (designFormula plan y を parse)
    , dhfBetaNames :: ![Text]
    , dhfBetaDraws :: ![[Double]]   -- draws × p (dhfBetaNames 順)
    , dhfSigmaDraws:: ![Double]
    , dhfFrame     :: !ModelFrame   -- 訓練 (mvFrame 用)
    }
  designModelHBM :: HBMConfig -> Design -> [RandomSpec] -> Text -> DesignModelHBMSpec
  data DesignModelHBMSpec = DesignModelHBMSpec !HBMConfig !Design ![RandomSpec] !Text
  -- instance Fit DesignModelHBMSpec where type Fitted = DesignHBMFit
  ```

- [ ] **Step 1: 失敗するテストを書く**

lot 差のある合成データ (固定効果 temp の傾き + lot ごとの切片ずれ) で fit し、固定効果係数が真値に近く収束することを検証:
```haskell
    it "designModelHBM は lot 群込みで固定効果を回復する" $ do
      -- temp∈{-1,1}, lot∈{A,B}; y = 2 + 3*temp + lotShift + noise
      let df = -- WorkflowSpec 既存の DataFrame ヘルパで構築 (temp, lot, y 列)
               mkFrameHBM
          plan = factorialDesign [contFactor "temp" (-1,1)]   -- 主効果モデル
          fit  = df |-> designModelHBM defaultHBM plan [ranIntercept "lot"] "y"
      case fit of
        Left e  -> expectationFailure e
        Right m -> do
          let bTemp = mean0 (drawsFor "temp" m)   -- 事後平均
          bTemp `shouldSatisfy` (\b -> abs (b - 3) < 1.0)
```
> `mkFrameHBM` は WorkflowSpec 内に固定シードの合成データ (n≈12・2 lot) を作るローカルヘルパとして書く。`drawsFor nm m = dhfBetaDraws` の該当列。`mean0` は既存 (ReportInstances で使用) か WorkflowSpec 内に `mean0 xs = sum xs / fromIntegral (length xs)` を定義。

- [ ] **Step 2: 失敗を確認**

Run: `cabal test analyze-test --test-options='-m "designModelHBM は lot"'`
Expected: FAIL (`designModelHBM` not in scope)。

- [ ] **Step 3: 最小実装**

`Fit.hs` に `designModelGP` (1422-1435) の直後へ追加:
```haskell
data DesignHBMFit = DesignHBMFit
  { dhfFormula    :: !Formula
  , dhfBetaNames  :: ![Text]
  , dhfBetaDraws  :: ![[Double]]
  , dhfSigmaDraws :: ![Double]
  , dhfFrame      :: !ModelFrame
  }

data DesignModelHBMSpec = DesignModelHBMSpec !HBMConfig !Design ![RandomSpec] !Text

-- | DOE 設計の **階層ベイズ (mixed-effects)** 解析 spec (Phase 78.G-f)。固定効果 =
--   'designFormula' (factorial=交互作用 / RSM=2次)、ランダム効果 = 'RandomSpec' の群。
--   手書き 'ModelP' を 'hbm' (NUTS) で学習。profiler/contour に事後予測帯を出す。
--   @filledDf |>> designModelHBM defaultHBM plan [ranIntercept "lot"] "y"@。
designModelHBM :: HBMConfig -> Design -> [RandomSpec] -> Text -> DesignModelHBMSpec
designModelHBM = DesignModelHBMSpec

instance Fit DesignModelHBMSpec where
  type Fitted DesignModelHBMSpec = DesignHBMFit
  fitEither (DesignModelHBMSpec cfg plan res y) d = do
    -- v1: random intercept のみ (slope は非対応 = 明示 error)
    case [ g | RandomSpec _ (_:_) g <- res ] of
      (g:_) -> Left ("designModelHBM: v1 は random slope 未対応 (群 " <> T.unpack g <> ")。ranIntercept を使うこと。")
      []    -> Right ()
    fml <- parseFormula (designFormula plan y)
    let mf = buildModelFrameFor fml d            -- ★LM 経路と同じ helper (Linear.hs:47 参照)
    (xMat, betaNames) <- designMatrixF fml mf
    yv                <- responseVec mf
    let designX = map LA.toList (LA.toRows xMat)
        ys      = V.toList yv
    -- 各 RandomSpec の群列を idx 化
    resPrepared <- mapM (prepRE d) res
    let prog = designHBMProgram designX betaNames resPrepared [] ys
        model = d |-> hbm cfg prog :: HBMModel   -- ★fit 実体 (df |-> hbm cfg prog)
        betaDraws = [ hbmDraws nm model | nm <- betaNames ]   -- p 本 × draws
    pure DesignHBMFit
      { dhfFormula    = fml
      , dhfBetaNames  = betaNames
      , dhfBetaDraws  = transposeL betaDraws        -- draws × p に転置
      , dhfSigmaDraws = hbmDraws "sigma" model
      , dhfFrame      = mf
      }
    where
      prepRE frame (RandomSpec _ _ g) =
        let gv = textColumnOf g frame              -- 群列を V.Vector Text で取得
            (labels, idx, _) = buildGroups gv
        in Right (V.toList idx, V.length labels, True, [])
```
> 補助:
> - `buildModelFrameFor fml d` = LM 経路 (`multiLMModel` / `Plot/Linear.hs:47`) がデータフレーム `d` から `ModelFrame` を作る手順をそのまま踏襲する (該当 helper を読んで再利用。無ければ `d |-> ...` の Fit 文脈が既に ModelFrame を持つ形に合わせる)。
> - `textColumnOf g d` = DataFrame の群列を `V.Vector Text` で取り出す (既存 df アクセサ。`Cat`/Int/Text 列に対応)。
> - `transposeL :: [[a]] -> [[a]]` = `Data.List.transpose`。
> - import: `Data.List (transpose)`・`Numeric.LinearAlgebra as LA`・`parseFormula` (Model.Formula)・`Formula` 型。

- [ ] **Step 4: テスト通過を確認**

Run: `cabal test analyze-test --test-options='-m "designModelHBM は lot"'`
Expected: PASS (事後平均 bTemp ≈ 3)。NUTS が数秒で回る規模 (n≈12・少パラメータ)。

- [ ] **Step 5: commit**

```bash
git add src/hanalyze/Analyze/Fit.hs test/hanalyze/Analyze/Design/WorkflowSpec.hs
git commit -m "add designModelHBM: DOE 階層ベイズ fit + DesignHBMFit (Phase 78.G-f)"
git push
```

---

### Task 4: `instance MultiVarModel DesignHBMFit` (事後予測帯)

profiler/contour に描画層無改修で載せる。

**Files:**
- Modify: `src/hanalyze/Analyze/Plot/ML.hs` (`instance MultiVarModel GPRegModelN` (1744) の直後)
- Test: `test/hanalyze/Analyze/Design/WorkflowSpec.hs`

**Interfaces:**
- Consumes: `DesignHBMFit (..)` (Task 3)・`designMatrixF`・`quantileNormal` (ML.hs で GP instance が使用済)。
- Produces: `instance MultiVarModel DesignHBMFit`。
- 帯の定義 (GP と parity): 評価点 `Xnew` (行 = 評価点) に対し、各事後 draw の β で `μ_draw = Xnew·β_draw` を計算。中心 = draw 平均 `μ̄`、帯 = `μ̄ ± z·√(Var_draws(μ) + σ̄²)` (σ̄² = σ draw の2乗平均 = 事後予測帯)。ランダム効果は集団平均 (=0) で marginalize (profiler は「代表条件での予測」ゆえ群固定しない)。

- [ ] **Step 1: 失敗するテストを書く**

profiler 用に along grid の評価で帯 (Just) が返り、中心が LM fit と近いことを検証:
```haskell
    it "MultiVarModel DesignHBMFit は事後予測帯を返す" $ do
      let df = mkFrameHBM
          plan = factorialDesign [contFactor "temp" (-1,1)]
          Right m = df |-> designModelHBM defaultHBM plan [ranIntercept "lot"] "y"
          ef = evalFrameAt m [("temp", [-1, 0, 1])]   -- 評価 ModelFrame (WorkflowSpec helper)
          (mu, band) = mvEvalFrame m 0.95 ef
      length mu `shouldBe` 3
      band `shouldSatisfy` isJust
      -- 中心 μ は temp とともに増加 (傾き 3)
      (last mu - head mu) `shouldSatisfy` (> 3)
```
> `evalFrameAt` は評価点の `ModelFrame` を作る helper (`mfRoles` に `RoleContinuous` の grid、他は Median 固定)。GP instance の test が同型のものを既に持つはずなので流用 (`WorkflowSpec` / `Plot` test を grep して同じ helper を使う)。

- [ ] **Step 2: 失敗を確認**

Run: `cabal test analyze-test --test-options='-m "事後予測帯"'`
Expected: FAIL (`MultiVarModel DesignHBMFit` instance 無し = 型エラー or no instance)。

- [ ] **Step 3: 最小実装**

`ML.hs` に追加 (import に `DesignHBMFit (..)` を Fit から):
```haskell
-- | DOE 階層ベイズ fit の effect plot 開通 (Phase 78.G-f)。固定効果 β の事後 draw で
--   評価点の μ を計算し、事後予測帯 (μ の分散 + 観測 noise σ²) を CI slot に載せる。
--   ランダム効果は集団平均で marginalize (profiler = 代表条件の予測)。
instance MultiVarModel DesignHBMFit where
  mvFrame = dhfFrame
  mvEvalFrame m level ef =
    case designMatrixF (dhfFormula m) ef of
      Left _        -> ([], Nothing)
      Right (xMat, _) ->
        let rows  = map LA.toList (LA.toRows xMat)          -- 評価点 × p
            draws = dhfBetaDraws m                          -- draws × p
            -- 各評価点で μ_draw = row·β_draw の分布
            muAt row = [ sum (zipWith (*) row bd) | bd <- draws ]
            perPoint = map muAt rows                        -- 評価点ごとの draw 列
            z     = quantileNormal (1 - (1 - level) / 2)
            s2bar = let ss = dhfSigmaDraws m
                    in if null ss then 0 else sum (map (^ (2::Int)) ss) / fromIntegral (length ss)
            center = map mean0L perPoint
            sds    = map (\ds -> sqrt (varL ds + s2bar)) perPoint
        in ( center
           , Just ( zipWith (\c s -> c - z * s) center sds
                  , zipWith (\c s -> c + z * s) center sds ) )
    where
      mean0L xs = if null xs then 0 else sum xs / fromIntegral (length xs)
      varL   xs = let mu = mean0L xs
                  in if null xs then 0 else sum (map (\x -> (x - mu)^(2::Int)) xs) / fromIntegral (length xs)
```
> `quantileNormal` は ML.hs で既に GP instance が使用 (import 済)。`LA.toRows`/`LA.toList` は `Numeric.LinearAlgebra`。

- [ ] **Step 4: テスト通過を確認**

Run: `cabal test analyze-test --test-options='-m "事後予測帯"'`
Expected: PASS (帯 = Just・μ が temp で増加)。

- [ ] **Step 5: commit**

```bash
git add src/hanalyze/Analyze/Plot/ML.hs test/hanalyze/Analyze/Design/WorkflowSpec.hs
git commit -m "add MultiVarModel DesignHBMFit: 事後予測帯で profiler 開通 (Phase 78.G-f)"
git push
```

---

### Task 5: re-export + `multiOutput` 対称性 + 全体 test

**Files:**
- Modify: `src/hanalyze/Analyze/Plot.hs` (re-export)
- Test: `test/hanalyze/Analyze/Design/WorkflowSpec.hs`

**Interfaces:**
- Consumes: `designModelHBM`・`ranIntercept`・`ranSlope`・`DesignHBMFit`。
- Produces: なし (公開面の整備)。

- [ ] **Step 1: 失敗するテストを書く**

`multiOutput` が LM/GP と対称に使えること (カレー化 `Text -> spec` が効く) を検証:
```haskell
    it "multiOutput で designModelHBM を複数応答に適用できる" $ do
      let df = mkFrameHBM2   -- y1, y2 列を持つ合成データ
          plan = factorialDesign [contFactor "temp" (-1,1)]
          Right ms = df |-> multiOutput ["y1","y2"] (designModelHBM defaultHBM plan [ranIntercept "lot"])
      map fst ms `shouldBe` ["y1","y2"]
```

- [ ] **Step 2: 失敗を確認**

Run: `cabal test analyze-test --test-options='-m "multiOutput で designModelHBM"'`
Expected: FAIL (import 経路 or 未 export)。

- [ ] **Step 3: 最小実装**

`Plot.hs` の export/re-export に追加:
```haskell
  , designModelHBM
  , ranIntercept
  , ranSlope
  , DesignHBMFit (..)
```
(`designModelGP` を re-export している箇所と同じブロック。`Fit` から re-export。)

- [ ] **Step 4: テスト通過 + 全体 green**

Run: `cabal test analyze-test`
Expected: 全 example PASS (既存 1286 + 本 Phase 追加分)。

- [ ] **Step 5: commit**

```bash
git add src/hanalyze/Analyze/Plot.hs test/hanalyze/Analyze/Design/WorkflowSpec.hs
git commit -m "add designModelHBM を Plot re-export + multiOutput 対称性 test (Phase 78.G-f)"
git push
```

---

### Task 6: docs 09-doe「階層モデル」節 + profiler 図 + gen-doc 登録

**Files:**
- Modify: `docs/api-guide/09-doe.md`
- Modify: `demo-plot/PlotIntegrationDemo.hs` (or 既存 DOE 図 demo) — profiler 図生成コード追加
- Modify: `scripts/gen-doc-figures.sh` — 新図の生成を登録
- Modify: `CHANGELOG.md` — Phase 78.G-f 節

**Interfaces:**
- Consumes: 完成した `designModelHBM` + profiler。
- Produces: `docs/images/doe-profiler-hbm.svg` (要 gen-doc 登録)。

- [ ] **Step 1: demo に図生成を追加**

`PlotIntegrationDemo.hs` に lot 差データで `df |>> toPlot (profiler [model] ["temp"])` の profiler を描き `doe-profiler-hbm.svg` を吐くブロックを追加 (既存 `doe-profiler-gp` の生成コードを手本にする)。

- [ ] **Step 2: gen-doc-figures.sh に登録**

`scripts/gen-doc-figures.sh` の DOE 図生成節に `doe-profiler-hbm` を追加 (手動コピー禁止・basename 一致で design→docs/images へ)。

- [ ] **Step 3: 図を一括生成し目視**

Run: `bash scripts/gen-doc-figures.sh`
Expected: `docs/images/doe-profiler-hbm.svg` 生成。PNG 化して目視 (`rsvg-convert`) — lot ランダム効果込みの事後予測帯が水平に退化せず temp で傾く。**ユーザに PNG 送付して承認を得る** (図は PNG・承認後 push)。

- [ ] **Step 4: 09-doe.md に節追加**

`09-doe.md` に「階層モデル (mixed-effects DOE・lot 間差)」節を追加: 動機 (小 n DOE で LM は lot 差を誤る)・API (`designModelHBM cfg plan [ranIntercept "lot"] "y"`)・型付き RE (`ranIntercept`/`ranSlope`)・図埋め込み `![](../images/doe-profiler-hbm.svg)`・GP 版との象限比較 (GP=連続専用/HBM=群をランダム効果)。api-guide skill の規約に従う。

- [ ] **Step 5: CHANGELOG + commit (図はユーザ承認後)**

```bash
git add docs/api-guide/09-doe.md docs/images/doe-profiler-hbm.svg demo-plot/PlotIntegrationDemo.hs scripts/gen-doc-figures.sh CHANGELOG.md
git commit -m "add 09-doe に階層モデル節 + doe-profiler-hbm 図 (Phase 78.G-f)"
git push
```

---

### Task 7: phase-78 spec の G-f を完了に更新

**Files:**
- Modify: `specification/phases/phase-78-doe-workflow.md`

- [ ] **Step 1: G-f を [x] 完了に**

`- [ ] **G-f.` を `- [x] **G-f.` にし、完了メモ (landing した 3 コンポーネント + 象限注記 + test 数) を G-e と同じ体裁で追記。scope a 完了・本丸 b (設計生成側) は後段/別 Phase を明記。

- [ ] **Step 2: phase-plan.md の phase 行更新**

`specification/phase-plan.md:7` の phase 行を「G-f 完了・次 = …」へ更新。

- [ ] **Step 3: commit**

```bash
git add specification/phases/phase-78-doe-workflow.md specification/phase-plan.md
git commit -m "docs: Phase 78.G-f 完了マーク + phase-plan 更新"
git push
```

---

## Self-Review

**1. Spec coverage (phase-78 G-f 節との突合):**
- 手書き ModelP → Task 2 ✅ / 型付き RE (RandomSpec) → Task 1,3 ✅ / designModelHBM 糖衣 → Task 3 ✅ /
  MultiVarModel 事後予測帯 → Task 4 ✅ / multiOutput 対称 → Task 5 ✅ / docs+図+test → Task 6 ✅ /
  Cat を群に使える → Task 3 `buildGroups` が Text 群列を扱う ✅ / scope a のみ・slope は error → Task 3 明示 ✅。
- **未カバーで意図的に落としたもの**: prior 差し替え (`HBMDesignConfig`) は v1 で `HBMConfig` 再利用 + 固定弱情報 prior に簡約 (spec の config は将来)。random slope は v1 非対応 (error)。本丸 b (設計生成) は別 sub。→ すべて spec の「非スコープ (後段)」と整合。

**2. Placeholder scan:** コード steps は全て具体コード。3 箇所だけ既存 helper の**再利用先を file:line で指名**している (`buildModelFrameFor`=Linear.hs:47 踏襲 / `textColumnOf`=既存 df アクセサ / `evalFrameAt`=GP test 流用)。これらは「その場所を読んで同じものを使え」という具体指示であり placeholder ではないが、実装者は着手時に該当行を Read して正確なシンボル名に合わせること。

**3. Type consistency:** `DesignHBMFit` フィールド (`dhfFormula/dhfBetaNames/dhfBetaDraws/dhfSigmaDraws/dhfFrame`) は Task 3 定義 → Task 4 使用で一致。`designHBMProgram` の引数順 (Task 2) と Task 3 の呼び出しが一致。`ranIntercept :: Text -> RandomSpec` は Task 1 定義 → Task 3,5 使用で一致。`mvEvalFrame` の戻り型 `([Double], Maybe (...))` は class 定義と一致。

## 実装前の注意 (fresh context 向け)
- 着手前に **memory `phase-78-gf-hbm-hierarchical-decision.md`** を読むこと (設計確定の一次情報)。
- `buildModelFrameFor` / `textColumnOf` / `evalFrameAt` の正確なシンボルは既存コードに合わせる (LM/GP 経路を grep)。ここだけ本計画は「再利用先の指名」に留めている。
- NUTS の規模: test データは n≈12・少パラメータで数秒。`defaultHBM` の iterations で収束するか確認し、足りなければ cfg で増やす (test 側で明示)。
