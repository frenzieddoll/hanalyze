# Model.Survival — 生存解析

> R の `survival` パッケージ / Python の `lifelines` 相当。
> 右側打ち切り (right censoring) 対応。

## 1. データ表現

```haskell
data SurvSample = SurvSample
  { ssTime  :: Double      -- 観測時間
  , ssEvent :: Event       -- Observed | Censored
  }

data Event = Censored | Observed
```

例: 患者 30 日目に死亡 → `SurvSample 30 Observed`、生存中で観察終了 → `SurvSample 30 Censored`。

## 2. Kaplan-Meier 推定

```haskell
import qualified Hanalyze.Model.Survival as Surv

let samples =
      [ Surv.SurvSample 5 Surv.Observed
      , Surv.SurvSample 7 Surv.Censored
      , Surv.SurvSample 10 Surv.Observed
      , Surv.SurvSample 15 Surv.Observed
      , ...
      ]

let km = Surv.kaplanMeier samples

Surv.kmrTimes km      -- distinct event times
Surv.kmrSurvival km   -- Ŝ(t) at each
Surv.kmrAtRisk km     -- risk set 数
Surv.kmrEvents km     -- events 数
Surv.kmrCensored km   -- censored 数
```

推定された生存関数 Ŝ(t) はイベント時点で下方へジャンプする階段関数になる。下図は Kaplan-Meier 生存曲線の例で、各段差がそのイベント時点での生存確率の減少を表す。

![Kaplan-Meier 生存曲線](../images/km-survival.svg)

## 3. Nelson-Aalen 累積ハザード

```haskell
let na = Surv.nelsonAalen samples
Surv.narCumHazard na   -- Ĥ(t) = Σ d_j/n_j (monotone increasing)
```

## 4. 群間比較 (Log-rank test)

```haskell
let groupA = [Surv.SurvSample t Surv.Observed | t <- [...]]
    groupB = [Surv.SurvSample t Surv.Observed | t <- [...]]

let lr = Surv.logRankTest [groupA, groupB]

Surv.lrChi2 lr      -- χ² 統計量
Surv.lrDf lr        -- k-1
Surv.lrPValue lr    -- p-value
```

`H_0: S_A(t) = S_B(t)` を検定。多群 (k ≥ 3) 対応。

## 5. Cox 比例ハザード回帰

```haskell
-- 共変量と event データ
let xs = [LA.fromList [age, treatment, sex] | (...) <- patients]
    ys = [Surv.SurvSample timeFollowup eventStatus | ...]

let fit = Surv.coxPH xs ys

Surv.coxBeta fit       -- 係数 (length p)
Surv.coxSE fit         -- SE (Fisher 情報量から)
Surv.coxLogLik fit     -- log partial likelihood
Surv.coxIters fit      -- Newton iteration 数

-- ハザード比 (HR)
let hr = exp (LA.atIndex (Surv.coxBeta fit) 0)
-- HR > 1: その共変量がハザードを増加させる
```

## 6. ベースラインハザード (Breslow 推定)

```haskell
let baselineH = Surv.coxBaselineHazard fit xs ys
-- [(t_1, H_0(t_1)), (t_2, H_0(t_2)), ...]
```

実際の生存関数: S(t | x) = exp(-H_0(t) × exp(β·x))

複数の競合する事象 (例: 異なる死因) がある場合は、単一の生存関数ではなく事象ごとの累積発生関数 (CIF; cumulative incidence function) で各事象の発生確率を表す。下図は競合リスク下での CIF の例で、各曲線が時間とともに増加する事象別の累積発生確率を示す。

![競合リスクの累積発生関数 (CIF)](../images/cif-competing.svg)

## 7. アルゴリズム

- **KM**: 段階的に Ŝ(t) = Π(1 - d_j/n_j)
- **NA**: Ĥ(t) = Σ d_j/n_j
- **Log-rank**: 各時点での observed - expected を全時点で集計、χ² 近似
- **Cox PH**: 部分尤度を Newton-Raphson で最大化、Hessian は中央差分

## 8. 注意

- 入力は **time + event 必須**。打ち切り情報を正しく与える
- Cox PH は **比例ハザード仮定** を要求 (ログ-対数プロットで確認推奨)
- ties は Breslow 近似 (Efron は未実装)
