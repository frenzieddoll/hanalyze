# Random Fourier Features (RFF)

> 🌐 [English](04-rff.md) | **日本語**

> **大規模 GP** を `O(nD)` で近似する Bochner の定理ベース手法。
> Rahimi & Recht (2007)、`Hanalyze.Model.RFF` モジュール。
>
> 関連: [04-gp.ja.md](04-gp.ja.md) (厳密 GP) / [04-kernel.ja.md](04-kernel.ja.md) (カーネル回帰)

## 1. アイデア

定常カーネル `k(x, x') = k(x - x')` は Fourier 変換で正の測度 `p(ω)` に対応 (Bochner 定理):

\[ k(x - x') = \int p(\omega) e^{i\omega^\top(x-x')}\,d\omega \]

ω を `D` 個サンプリング、`b` を `[0, 2π)` から: \[\varphi(x) = \sigma_f \sqrt{2/D} [\cos(\omega_1^\top x + b_1), \ldots, \cos(\omega_D^\top x + b_D)] \]

すると `k(x, x') ≈ φ(x)^⊤ φ(x')`、リッジ回帰が `O(nD)` で可能 (vs 厳密 GP の `O(n³)`)。

## 2. API

```haskell
import Model.RFF

-- 1D
sampleRFFRBF       :: Int -> Double -> Double -> GenIO -> IO RFFFeatures
sampleRFFMatern52  :: ...
rffFeatures        :: RFFFeatures -> Vector Double -> Matrix Double  -- φ(x)
rffRidge           :: RFFFeatures -> Double -> Vector Double -> Vector Double -> RFFRidgeFit

-- 多入力 (n × p)
sampleRFFRBFMV     :: Int -> Int -> Double -> Double -> GenIO -> IO RFFFeaturesMV
rffFeaturesMV      :: RFFFeaturesMV -> Matrix Double -> Matrix Double  -- φ_MV(X)
rffRidgeMV         :: RFFFeaturesMV -> Double -> Matrix Double -> Vector Double -> RFFRidgeFitMV
predictRFFRidgeMV  :: RFFRidgeFitMV -> Matrix Double -> Vector Double

-- 周辺尤度最大化で hyper を選ぶ
maximizeMarginalLikRBFMV    :: ...
gridSearchLOOCVRBFMV        :: ...
```

## 3. ミニマル例

```haskell
import qualified System.Random.MWC as MWC
import qualified Numeric.LinearAlgebra as LA
import Model.RFF

gen <- MWC.createSystemRandom

-- D = 256 個の random feature
let xMat = LA.fromLists [[x1, x2], ...]  -- n × 2
    yVec = LA.fromList [...]
    sf2 = 1.0
    ell = 1.0

rff <- sampleRFFRBFMV 256 (LA.cols xMat) ell sf2 gen
let fit = rffRidgeMV rff 1e-3 xMat yVec
    yPred = predictRFFRidgeMV fit xTestMat
```

## 4. D (特徴数) の選び方

| D | 性能 |
|---|---|
| 64-128 | 速いが粗い近似、デバッグ用 |
| 256-512 | バランス良 (デフォルト) |
| 1024+ | 厳密 GP に近づく、メモリ大 |

経験則: `D = 4-8 × log n` 程度から始める。

## 5. Matérn52 vs RBF

`sampleRFFMatern52` で StudentT(5) 由来の ω をサンプル → Matern52 カーネルを近似。
RBF より裾が重く外れ値に頑健。

## 6. 大規模 ベンチ

| n=2000, D=256 | hanalyze | sklearn (RBFSampler+Ridge) |
|---|---|---|
| RFF fit | 65 ms | 6 ms |
| 厳密 GP fit | 384 ms | 176 ms |

(上記は scaling の illustration; 詳細は [bench/results/SUMMARY.md](../../bench/results/SUMMARY.md) 参照)

## 7. 多出力 / 多入力

`rffRidgeMVMulti` で行列 `Y :: n × q` (q 出力) を一括 fit。`W = (ΦᵀΦ + λI)⁻¹ Φᵀ Y` の閉形式。

## 8. CLI

```bash
hanalyze kernel data.csv "x1 x2 x3" y --method rff --features 256 --report fit.html
hanalyze kernel data.csv "x t" y --method rff --group name --xaxis t --report fit.html
```

`--group` で系列ごとに色分け、`--xaxis` で副軸を選択。`--auto-hp` で marginal likelihood 最大化。

## 9. 対話的 GUI

`--interactive` フラグで RFF の重み + ω + b が HTML に埋込まれ、副軸スライダ操作のたびに JS 側で `φ(x_new)·w` を再計算。データ生成器を含めれば「歯抜け wide → long → 多変量 RFF → name で色分けした予測曲線 HTML」がワンライナーで作れる。

## 関連リンク

- 厳密 GP: [04-gp.ja.md](04-gp.ja.md)
- カーネル回帰: [04-kernel.ja.md](04-kernel.ja.md)
- 多出力対応: [05-multivariate.ja.md](05-multivariate.ja.md)
