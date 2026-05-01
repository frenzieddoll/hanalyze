# クイックスタート

> ## ⚠️ API 移行のお知らせ
>
> 本ページの例は旧 `Model.HBM` を使っていますが、**新規コードは `Model.HBMP` を推奨**します。
> サンプラーも `MCMC.HMCP.hmcP` / `MCMC.NUTSP.nutsP` / `MCMC.GibbsP.gibbsMHP` を使ってください。
> 次回メジャーバージョンで `Model.HBM` は削除され、`Model.HBMP` が `Model.HBM` にリネームされます。
> 詳細は [確率的プログラミング DSL — HBMP への移行](02-probabilistic-model.md#modelhbmp-への移行-推奨) を参照してください。

## ビルドと実行

```bash
cabal build           # ライブラリ + 全実行ファイル
cabal test            # テストスイート
cabal run vi-demo     # 変分推論デモ (VI vs NUTS 比較)
cabal run gibbs-demo  # Gibbs + WAIC/LOO デモ
cabal run hbm-example # 階層ベイズ + 4チェーン NUTS → HTML レポート生成
```

バイナリを直接実行する場合 (cabal run が曖昧な時):
```
dist-newstyle/build/x86_64-linux/ghc-9.6.7/hanalyze-0.1.0.0/x/vi-demo/build/vi-demo/vi-demo
```

CPU 並列化 (多チェーン実行):
```bash
cabal run hbm-example -- +RTS -N4   # 4スレッド
```

---

## 最小の完全ワークフロー

5行で「モデル → NUTS → HTML レポート」まで完結する例です。

### 推奨: `Model.HBMP` + `MCMC.NUTSP`

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
import qualified Data.Map.Strict as Map
import System.Random.MWC (createSystemRandom)
import Model.HBMP                            -- Distribution (..), sample, observe
import MCMC.NUTSP (nutsP)
import MCMC.NUTS  (defaultNUTSConfig)        -- NUTSConfig は共通
import MCMC.Core  (posteriorMean, posteriorSD)
import Viz.Report (defaultReport, renderReport)

-- 1. モデル: μ ~ Normal(0,10), y ~ Normal(μ, σ=2), 観測 5点
myModel :: ModelP ()
myModel = do
  mu <- sample "mu" (Normal 0 10)
  observe "y" (Normal mu 2) [1.2, 2.3, 3.1, 2.8, 1.9]

main :: IO ()
main = do
  gen <- createSystemRandom
  -- 2. NUTS (AD 勾配版) でサンプリング
  chain <- nutsP myModel defaultNUTSConfig (Map.fromList [("mu", 0.0)]) gen
  -- 3. 事後統計
  print (posteriorMean "mu" chain)
  print (posteriorSD   "mu" chain)
  -- 4. HTML レポート
  renderReport "report.html" (defaultReport "My Model" chain ["mu"])
```

### 旧 API (廃止予定): `Model.HBM` + `MCMC.NUTS`

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map.Strict as Map
import System.Random.MWC (createSystemRandom)
import Model.HBM
import Stat.Distribution (Distribution (..))
import MCMC.NUTS  (nuts, defaultNUTSConfig)
import MCMC.Core  (posteriorMean, posteriorSD)
import Viz.Report (defaultReport, renderReport)

myModel :: Model ()
myModel = do
  mu <- sample "mu" (Normal 0 10)
  observe "y" (Normal mu 2) [1.2, 2.3, 3.1, 2.8, 1.9]

main :: IO ()
main = do
  gen   <- createSystemRandom
  chain <- nuts myModel defaultNUTSConfig (Map.fromList [("mu", 0.0)]) gen
  renderReport "report.html" (defaultReport "My Model" chain ["mu"])
```

---

## 何を使えばいいか — 全機能早見表

| やりたいこと | 推奨モジュール | 旧 API (廃止予定) | デモファイル |
|---|---|---|---|
| モデル定義 (DSL) | `Model.HBMP` | `Model.HBM` | `HBMPDemo.hs` |
| HMC サンプリング | `MCMC.HMCP` (`hmcP`) | `MCMC.HMC` (`hmc`) | `HBMComparisonDemo.hs` |
| NUTS サンプリング | `MCMC.NUTSP` (`nutsP`) | `MCMC.NUTS` (`nuts`) | `HBMPMCMCDemo.hs` |
| 共役モデルの高速サンプリング | `MCMC.GibbsP` (`gibbsMHP`) | `MCMC.Gibbs` (`gibbsMH`) | `GibbsPTest.hs` |
| 変分推論 (大規模・高速近似) | `Stat.VI` | — | `VIDemo.hs` |
| モデル比較 | `Stat.ModelSelect` | — | `GibbsDemo.hs` |
| 診断プロット・HTML レポート | `Viz.Report` | — | `HBMExample.hs` |
| 棒グラフ | `Viz.Bar` | — | `BarDemo.hs` |

各機能の詳細は以下のドキュメントを参照してください:
- [確率的プログラミング DSL](02-probabilistic-model.md)
- [MCMC サンプラー選択ガイド](03-mcmc-samplers.md)
- [Gibbs サンプリング](04-gibbs.md)
- [変分推論 (ADVI)](05-variational-inference.md)
- [モデル比較 (WAIC/LOO)](06-model-comparison.md)
- [可視化](07-visualization.md)
