# 実験計画 (DOE) — 低レベル API (素の設計生成関数)

> [← 09-doe (高レベル DOE ワークフロー)](../api-guide/09-doe.md)

高レベルの `Design` を組む [09-doe](../api-guide/09-doe.md) が既定。 以下は設計を行列
(`[[Double]]`) として直接扱う低レベル関数で、 設計オブジェクトに包まれる素の中身にあたる。
既に候補集合や水準行列を手元に持つ・非標準の設計を自前で組む、 等のときに使う。

| 領域 | モジュール | 主な関数 |
|---|---|---|
| 要因計画 | `Design.Factorial` | `fullFactorial` / `fractionalFactorial` |
| ブロック | `Design.Block` | `latinSquare` / `randomizedBlock` |
| 応答曲面 (RSM) | `Design.RSM` | `centralComposite` / `boxBehnken` |
| 最適計画 | `Design.Optimal` | `dOptimal` / `aOptimal` / `optimalDesign` (低レベル・下記 同名注意) |
| 直交表 / タグチ | `Design.Orthogonal` / `Design.Taguchi` | `lookupOA` / `assignFactors` / `snRatio` |
| ANOVA | `Design.Anova` | `oneWayAnova` / `twoWayAnova` |
| 検出力 | `Design.Power` | `powerTTest` / `sampleSizeTTest` |

## 要因計画

```haskell
fullFactorial       :: [[Double]] -> [[Double]]      -- 各因子の水準リスト → 全組合せ
fractionalFactorial :: Int -> [[Int]] -> [[Double]]
```

```haskell
let design = fullFactorial [[-1,1],[-1,1],[-1,1]]   -- 2³ = 8 runs
```

---

## 応答曲面法 (RSM)

```haskell
centralComposite :: Int -> CCDType -> Int -> [[Double]]   -- 因子数, CCD 種, 中心点数
boxBehnken       :: Int -> Int -> [[Double]]
```

応答曲面の 3D 図 (二次回帰 + CCD 点) は plot の 3D 経路で描く ([01-doe](../doe/01-doe.md) に作例)。

![二次応答曲面 + 等高線 + CCD 点](../images/rsm-surface-3d.svg)

![方位を 0/45/90/135° 回転](../images/rsm-rotation.svg)

---

## 最適計画 (`Design.Optimal`)

```haskell
dOptimal :: [[Double]] -> Int -> Int -> ([Int], [[Double]])   -- 候補集合 → D 最適部分集合
aOptimal :: [[Double]] -> Int -> Int -> ([Int], [[Double]])   -- 候補集合 → A 最適部分集合
```

---

## 直交表 / タグチメソッド

```haskell
lookupOA      :: Text -> Maybe OA                       -- "L9" 等 (L4/L8/L9/L12/L16/L18)
assignFactors :: OA -> [FactorSpec] -> Either Text AssignedDesign
```

SN 比・要因効果は `Design.Taguchi` (`snRatio` / `analyzeSN` / `optimalLevels`)。
→ [02-orthogonal-taguchi](../doe/02-orthogonal-taguchi.md)

---

## ANOVA / 検出力

```haskell
oneWayAnova :: [Text] -> [Double] -> AnovaTable        -- 群ラベル, 観測値
twoWayAnova :: [Text] -> [Text] -> [Double] -> AnovaTable   -- 因子A, 因子B, 観測値
powerTTest  :: Double -> Int -> Int -> Double -> Double      -- Cohen's d, n₁, n₂, α → 検出力
```

```haskell
printAnovaTable (oneWayAnova labels ys)
```

→ [01-doe](../doe/01-doe.md)

---

## 最適計画の拡張 (G-opt / Compound / 制約フィルタ)

`Design.Optimal` の criterion 拡張と、 候補集合を事前に絞る `Design.Constraint`
(古典 Fedorov 用)。 理論は [usage-classic-extensions](../doe/usage-classic-extensions.ja.md)。

> **同名注意**: ここの `Design.Optimal.optimalDesign` (候補集合 `[[Double]]` → D/G 等の最適部分
> 集合を返す低レベル Fedorov) は、 高レベル workflow の `optimalDesign`
> (`Hanalyze.Design.Workflow`・factor specs + `Formula` + run 数 → `Design`) とは**別関数**。
> 対話では高レベル版を使う。

```haskell
data OptCriterion = DOpt | AOpt | IOpt | EOpt | GOpt | Compound [(Double, OptCriterion)]
gOptimal      :: [[Double]] -> Int -> Word32 -> ([Int], [[Double]])   -- G-最適 Fedorov
optimalDesign :: OptCriterion -> [[Double]] -> Int -> Word32 -> ([Int], [[Double]])

-- 候補集合の事前フィルタ (Design.Constraint)
data LinearConstraint = LinearConstraint [(Text, Double)] CmpOp Double  -- CLeq/CGeq/CEq
filterCandidates :: [Text] -> [LinearConstraint] -> [[Double]] -> [[Double]]
checkDesign      :: [Text] -> [LinearConstraint] -> [[Double]] -> [Int]   -- 違反行 index
```

`Compound` の inner criterion はスケールを揃える責任が呼び手にある (D=det / A=trace で
単位が違う → efficiency 形 [0,1] に正規化してから渡す)。

---

## 工程能力 (Process Capability・`Design.Quality`)

非正規 (Gamma / 自動 fit) と多変量 (Mahalanobis) の Cp/Cpk。

```haskell
processCapabilityGamma       :: [Double] -> (Double, Double) -> Either Text Capability   -- (LSL,USL)
processCapabilityNonNormal   :: [Double] -> (Double, Double) -> Either Text NonNormalFit -- AIC で分布自動選択
processCapabilityMultivariate :: LA.Matrix Double -> [(Double, Double)] -> Either Text MultiCap
-- capCp / capCpk (単変量) ・ mcMCp / mcMCpk / mcInSpecRate (多変量・MCpk ≤ MCp)
```

---

## Custom Design (JMP Pro 同等・`Design.Custom.*`)

候補集合に依らず **任意モデル × 任意制約 × 任意 runs** を coordinate exchange
(Meyer-Nachtsheim 1995) で生成。 型安全規約・既知制限は
[usage-custom-design](../doe/usage-custom-design.ja.md) が一次根拠。

```haskell
-- 因子 (Role × Kind の直交軸・Design.Custom.Factor)
data Factor = Factor Text FactorKind FactorRole
data FactorKind = Continuous Double Double | DiscreteNum [Double]
                | Mixture Double Double | Categorical [Text] | Ordinal [Text]
data FactorRole = Controllable | HardToChange | Blocking  -- …

-- モデル項 (Design.Custom.Model)
data Term = TIntercept | TMain Text | TInter [Text] | TPower Text Int
expandDesignMatrix :: [Factor] -> Model -> LA.Matrix Double -> Either Text (LA.Matrix Double)

-- 制約 (Design.Custom.Constraint)
data Constraint = LinearIneq [(Text,Double)] CmpOp Double | Forbidden [(Text,FactorValue)]
                | Conditional Guard [Constraint] | RangeBound Text Double Double

-- 設計生成 (Design.Custom.Coordinate)
coordinateExchange     :: CustomDesignSpec -> IO (Either Text CustomDesign)
coordinateExchangePure :: CustomDesignSpec -> Either Text CustomDesign   -- Phase 78.M: seed 決定的 pure (runST)
-- CustomDesignSpec { cdsFactors, cdsModel, cdsConstraints, cdsNRuns, cdsCriterion,
--                    cdsBudget=defaultBudget, cdsSeed, cdsInitial, cdsDJConvention }
-- 結果: cdMatrix (raw) / cdReport → crCriterionValue (最小化方向: DOpt なら −det)
-- ※Pure 版は cdsSeed=Nothing のとき defaultPureSeed で全域化。同 seed で IO 版とビット一致。
--   高レベル `customDesign` (09-doe) がこの pure 版を包む。
```

設計の比較・検出力 (`Design.Custom.Compare` / `.Power`):

```haskell
compareDesigns :: [(Text, CustomDesign)] -> DesignComparison   -- D/A/G/I eff + FDS + alias norm
designPower    :: CustomDesign -> Double -> [(Text, Double)] -> Double -> [(Text, Double)]
--                設計           σ        [(term, effect)]      α       → term 別 power
```

### Augment + Split-Plot (`Design.Custom.Augment` / `.SplitPlot`)

```haskell
data AugmentMenu = Replicate Int | AddCenter Int | AddAxial Double | AddRuns Int | Foldover FoldoverKind
augmentMenu     :: CustomDesignSpec -> AugmentMenu -> IO (Either Text AugmentResult)  -- 要 cdsInitial

data SplitPlotConfig = SplitPlotConfig { spcNWhole :: Int, spcVarRatio :: Double }    -- η=σ²_WP/σ²
generateSplitPlot     :: CustomDesignSpec -> SplitPlotConfig -> IO (Either Text SplitPlotDesign)
generateSplitPlotPure :: CustomDesignSpec -> SplitPlotConfig -> Either Text SplitPlotDesign  -- Phase 78.M: pure
-- HardToChange 因子は 1 whole-plot 内で固定。 REML 情報行列の導出は usage-augment-splitplot
-- ※Phase 79 以降、製品パス (高レベル `customDesign` + `Structure`) は `Design.Custom.Structured`
--   (役割非依存の構造駆動エンジン) を使う。本モジュール (`.SplitPlot`) は **内部 legacy**
--   (bench-custom-design の 3 エンジン比較 + Jones-Goos 低レベル golden の証跡) として温存。
```

### Bayesian D-optimal (DuMouchel-Jones・`Design.Custom.Bayesian`)

prior precision K で `det(XᵀX + K)` を最大化 (K=0 で classic D に縮退):

```haskell
priorPrecisionDefault :: [Factor] -> Model -> Double -> PriorPrecision   -- τ² (intercept/主効果=0, 高次=τ²)
precisionToMatrix     :: PriorPrecision -> [[Double]]
-- cdsCriterion = BayesianD (precisionToMatrix pp) で coordinateExchange に渡す
-- cdsDJConvention = True で DJ §2.2 列変換 (centering+直交化+range正規化) を自動適用
```

K 行列の設計思想・DJ §2.2 規約・Compound 重み正規化は
[usage-bayesian-d](../doe/usage-bayesian-d.ja.md) が一次根拠。
