# hanalyze-core

[`hanalyze`](../README.ja.md) の**最下層**。 dataframe にも
ベイズにも依存しない、 純粋な数値計算の土台を担う ——
記述統計 / 検定 / 分布 / 最適化 / MCMC の抽象基盤。

依存は `base` / `hmatrix` / `vector` / `statistics` / `containers` /
`mwc-random` 等の 10 package のみで、 **この repo の他 package には一切
依存しない**。 上位層 (`-frame` / `-bayes` / `-models` / `-design` / `-viz`)
はすべてこの層を経由する。

## 主要 module (全 44 module)

### 統計 (`Hanalyze.Stat.*`)

| Module | 役割 |
|---|---|
| `Stat.Descriptive` | 一次元記述統計の**単一の正** (平均 / 分散 / 分位点 / 歪度・尖度)。 上位層の集約はすべてここへ委譲 |
| `Stat.Test` | 検定群を単一の `TestResult` 型に統一 (t / Welch / F / χ² / ノンパラ検定 / Hotelling T² 1・2 標本 / 一元配置 MANOVA) |
| `Stat.Distribution` | 40+ 分布の pdf / cdf / 分位点 / 乱数 |
| `Stat.Effect` | 効果量 (Cohen's d / Hedges' g / η² / Cliff's δ) |
| `Stat.Bootstrap` / `Stat.CV` | ブートストラップ信頼区間 / 交差検証の分割器 |
| `Stat.MultipleTesting` | 多重比較補正 (Bonferroni / Holm / BH-FDR) |
| `Stat.SPC` | 統計的工程管理 — 変数管理図 (X̄-R / I-MR) + 属性管理図 (p / np / c / u) + EWMA / CUSUM と判定ルール (Western Electric / Nelson) |
| `Stat.GroupComparison` | 良品 vs 不良品の一括群間比較 (`goodVsBad` — 全変数を Welch t 検定 + Cohen's d で順位付け) |
| `Stat.ClassMetrics` | 分類指標 (混同行列 / ROC-AUC / F1) |

### 最適化 (`Hanalyze.Optim.*`)

| Module | 役割 |
|---|---|
| `Optim.NelderMead` | R の `optim(method="Nelder-Mead")` 既定に相当する導関数不要法 |
| `Optim.LBFGS` / `Optim.GradAscent` / `Optim.Adam` | 勾配法 |
| `Optim.CMAES` / `Optim.DifferentialEvolution` / `Optim.ParticleSwarm` / `Optim.SimulatedAnnealing` | 大域的最適化 |
| `Optim.NSGA` / `Optim.Pareto` | 多目的最適化 — NSGA-II (Deb et al. 2002) と Pareto フロント評価 |
| `Optim.Constrained` / `Optim.Desirability` | 拡張ラグランジュ法による制約付き最適化 / Desirability 関数 (Derringer & Suich 1980) による多目的スカラー化 |

### 基盤 (`MCMC.Core` / `Model.Core` / `Math.*`)

| Module | 役割 |
|---|---|
| `MCMC.Core` | サンプラ非依存の `Chain` 型と事後統計量 (`posteriorMean` / `posteriorSD` / `posteriorQuantile`)。 `MCMC.*` を単体サンプリング library として使うときの基盤 |
| `Stat.MCMC` | MCMC 診断 — `rhat` / `ess` / `essBulk` / `hdi` / `autocorr` / `bfmi` (サンプラ本体は `-bayes` 層) |
| `Model.Core` | 全回帰モデル共通の Result 型と `Model` 型クラス |
| `Math.HSIC` / `Math.ICA` / `Math.Hungarian` | HSIC 独立性統計量 / FastICA (Hyvärinen 1999) / Hungarian 法による割当問題 |

## 単体で使う

上位層が不要なら、 この package だけを直接依存に書ける:

```cabal
build-depends: hanalyze-core, hmatrix
```

```haskell
import qualified Hanalyze.Stat.Test as ST
import qualified Numeric.LinearAlgebra as LA

main = do
  let xs = LA.fromList [12, 14, 13, 15, 17, 11]
      ys = LA.fromList [18, 22, 20, 19, 25, 17]
      result = ST.tTestWelch xs ys ST.TwoSided
  print (ST.trPValue result, ST.trEffect result)
  -- (1.688e-3, Just ("Cohen's d", -2.527))
```

なお、 通常は umbrella package `hanalyze` を依存に書けば
`import Hanalyze` だけで上記もすべて使える。 層を直接指定するのは
依存を最小化したいときのみで十分。

## 関連 docs

- 検定: [docs/stat/01-test.ja.md](../docs/stat/01-test.ja.md) /
  多変量検定 (Hotelling T² / MANOVA): [docs/stat/usage-multivariate-test.ja.md](../docs/stat/usage-multivariate-test.ja.md)
- 管理図と判定ルール (SPC): [docs/stat/usage-spc.ja.md](../docs/stat/usage-spc.ja.md)
- 群間比較 (良品 vs 不良品): [docs/stat/usage-group-comparison.ja.md](../docs/stat/usage-group-comparison.ja.md)
- 効果量: [docs/stat/09-effect.ja.md](../docs/stat/09-effect.ja.md) /
  ブートストラップ: [docs/stat/07-bootstrap.ja.md](../docs/stat/07-bootstrap.ja.md)
- 最適化: [docs/optim/01-singleobj.ja.md](../docs/optim/01-singleobj.ja.md) /
  [docs/optim/02-multi-objective.ja.md](../docs/optim/02-multi-objective.ja.md)

← [repository README](../README.ja.md)
