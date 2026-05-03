# 単目的最適化 (`Optim.*`)

> 🌐 [English](01-singleobj.md) | **日本語**

> 関連: [02-multi-objective.ja.md](02-multi-objective.ja.md) (多目的)、
> [theory-singleobj.ja.md](theory-singleobj.ja.md) (理論)、
> [theory-bayesopt.ja.md](theory-bayesopt.ja.md) (BO)

`f: ℝ^n → ℝ` の最小/最大化を行う 5 つのアルゴリズムを統一インターフェースで提供。

## 共通インターフェース (`Optim.Common`)

```haskell
import Optim.Common

data OptimResult = OptimResult
  { orBest      :: [Double]   -- 最良点 x*
  , orValue     :: Double     -- 最良値 f(x*) (元尺度)
  , orHistory   :: [Double]   -- 反復ごとの best 値推移
  , orIters     :: Int
  , orConverged :: Bool
  }

data StopCriteria = StopCriteria
  { stMaxIter :: Int
  , stTolFun  :: Double
  , stTolX    :: Double
  }

data Direction = Minimize | Maximize
```

各オプティマイザの実行関数は概ね `runX :: XConfig -> ([Double] -> Double) -> [Double] -> IO OptimResult` の形に揃っている。

## アルゴリズム選択ガイド

| 状況 | 推奨 |
|---|---|
| 1D 単峰 | **Brent / Golden Section** (`Optim.LineSearch`) |
| 滑らか・勾配あり (or 数値勾配 OK) | **L-BFGS** (`Optim.LBFGS`) |
| 微分不能・低次元 (≤ 20) | **Nelder-Mead** (`Optim.NelderMead`) |
| 非凸・大域・微分不要 (≤ 30 次元) | **Differential Evolution** (`Optim.DifferentialEvolution`) |
| 非凸・連続・自動チューニング (10〜100 次元) | **CMA-ES** (`Optim.CMAES`) |

---

## 1. Nelder-Mead — `Optim.NelderMead`

n+1 頂点の単体 (simplex) を反射 / 拡張 / 縮小で更新。微分不要、低次元の局所最適化に強い。
R の `optim(method="Nelder-Mead")` の標準。

```haskell
import qualified Optim.NelderMead as NM

let f xs = sum [x*x | x <- xs]                    -- sphere
r <- NM.runNelderMead f [3, -2, 1]
-- orValue ~ 0、orBest ~ [0, 0, 0]
```

設定変更:

```haskell
let cfg = NM.defaultNMConfig
            { NM.nmStop = OC.defaultStopCriteria { OC.stMaxIter = 5000 }
            , NM.nmInitStep = 1.0    -- 初期 simplex のステップ幅
            }
r <- NM.runNelderMeadWith cfg rosenbrock [-1.2, 1.0]
```

## 2. L-BFGS — `Optim.LBFGS`

準ニュートン法 (Liu-Nocedal 1989)。Two-loop recursion で逆 Hessian × 勾配を計算、
履歴サイズ m=10 (典型)。**滑らかな MLE / GP HP 最適化のゴールドスタンダード**。

```haskell
import qualified Optim.LBFGS as LBFGS

-- 解析勾配版
r <- LBFGS.runLBFGS f gradF x0

-- 数値勾配版 (中央差分)
r <- LBFGS.runLBFGSNumeric LBFGS.defaultLBFGSConfig f x0
```

`Model.GP.optimizeGP` は内部で L-BFGS を使用 (旧 GradAscent 比 5-10 倍速)。

## 3. Brent / Golden Section — `Optim.LineSearch`

1D 単峰最適化:

- `brent`: 放物線補間 + 黄金分割の hybrid (超線形収束、`scipy.optimize.brent` 互換)
- `goldenSection`: 黄金分割 (線形収束、確実)

```haskell
import qualified Optim.LineSearch as LS

let r = LS.brent LS.defaultBrentConfig (\[x] -> (x - 2.5)^2 + 1) 0 5
-- orBest = [2.5]、orValue = 1.0
```

`Model.Kernel.autoBandwidthBrent` は内部で Brent を使用 (グリッド列挙不要)。

## 4. Differential Evolution — `Optim.DifferentialEvolution`

DE/rand/1/bin (Storn-Price 1997)。微分不要、大域、実装シンプルで実用堅牢。
連続 5〜30 次元の非凸問題に向く。

```haskell
import qualified Optim.DifferentialEvolution as DE
import qualified System.Random.MWC as MWC

gen <- MWC.createSystemRandom
let bounds = replicate 5 (-5.12, 5.12)
let cfg = (DE.defaultDEConfig bounds)
            { DE.deStop = OC.defaultStopCriteria { OC.stMaxIter = 400 }
            , DE.deF    = 0.7    -- mutation 係数
            , DE.deCR   = 0.9    -- crossover 確率
            }
r <- DE.runDEWith cfg rastrigin gen
```

## 5. CMA-ES — `Optim.CMAES`

Covariance Matrix Adaptation Evolution Strategy (Hansen 2001) の **簡易対角版**。
非凸連続最適化の事実上のベスト。フルランク C 更新は省略しているが、
Rastrigin / Ackley の中規模ベンチで十分機能する。

```haskell
import qualified Optim.CMAES as CMAES

gen <- MWC.createSystemRandom
let cfg = CMAES.defaultCMAESConfig { CMAES.cmSigma0 = 1.0 }
r <- CMAES.runCMAESWith cfg sphere [3, -2, 1, 0.5, -1.5] gen
```

---

## ベンチマーク

`cabal run single-opt-bench-demo` で 5 アルゴリズム × 3 ベンチ (Sphere / Rosenbrock / Rastrigin) の
収束履歴を HTML レポートで比較できる。

## Bayesian Optimization 内側 (`Optim.BayesOpt`) との統合

`Optim.BayesOpt` の獲得関数最大化は、新オプティマイザに置換済 (Phase O8):

| 関数 | 内側 |
|---|---|
| `bayesOpt` (1D 単目的) | **Brent** + 粗グリッド bracket |
| `bayesOptND` (N 次元単目的) | **L-BFGS multi-start** (nStarts 個の初期点から並列探索) |
| `bayesOptScalarMO` (多目的 ParEGO 風) | random scalarization + **L-BFGS multi-start** |
| `bayesOptMOWithNSGA` (多目的、Pareto front 全探索) | **NSGA-II** (適性あり、残置) |

GP の Cholesky / SVD 失敗は内部で `try (evaluate ...)` でキャッチして
ペナルティ値 (1e30) を返すため、極小 length scale 等で optimizer がクラッシュしない。

## CLI について

単目的最適化用の CLI サブコマンド (`hanalyze optim` 等) は提供していない。
理由:

- 目的関数を CLI 引数で渡す自然な方法がない (Haskell 関数を文字列で渡せない)
- HP チューニング用途は各モデルの `--auto-hp` フラグで既に露出済
- ベンチマーク用途は demo (上記 `single-opt-bench-demo`) で十分

ライブラリから直接呼び出して使用する。

---

## 参考

- 理論: [theory-singleobj.ja.md](theory-singleobj.ja.md)
- 多目的: [02-multi-objective.ja.md](02-multi-objective.ja.md)
- BO 詳細: [theory-bayesopt.ja.md](theory-bayesopt.ja.md)
