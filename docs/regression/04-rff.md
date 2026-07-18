# Random Fourier Features (RFF)

> 🌐 **English** | [日本語](04-rff.ja.md)

> **Large-scale GP** approximation in `O(nD)` using Bochner's theorem.
> Rahimi & Recht (2007), `Hanalyze.Model.RFF` module.
>
> Related: [04-gp.md](04-gp.ja.md) (Exact GP) / [04-kernel.md](04-kernel.md) (Kernel regression)

> 💡 **High-level entry**: RFF is the **approximation quadrant** of the integrated spec `gp` / `gpMulti`
> as `GpRff D seed` (with distribution and band) / `KrrRff D seed` (point prediction) via
> `df |-> gp (GPConfig RBF (GpRff 500 42) AutoMarginalLik) "x" "y"`.
> Takes a seed for reproducible results. See the 4-quadrant overview at
> [04-gp.md §0 Integrated API](04-gp.ja.md#0-統合-api--gp--gpmulti-推奨の入口).
> This page is a low-level reference for Φ construction, LOOCV search, etc.

## 1. Idea

A stationary kernel `k(x, x') = k(x - x')` corresponds via Fourier transform to a positive measure `p(ω)` (Bochner's theorem):

\[ k(x - x') = \int p(\omega) e^{i\omega^\top(x-x')}\,d\omega \]

Sample ω in `D` draws, `b` from `[0, 2π)`:
\[\varphi(x) = \sigma_f \sqrt{2/D} [\cos(\omega_1^\top x + b_1), \ldots, \cos(\omega_D^\top x + b_D)] \]

Then `k(x, x') ≈ φ(x)^⊤ φ(x')`, enabling ridge regression in `O(nD)` (vs exact GP's `O(n³)`).

## 2. API

```haskell
import Hanalyze.Model.RFF

-- 1D
sampleRFFRBF       :: Int -> Double -> Double -> GenIO -> IO RFFFeatures
sampleRFFMatern52  :: ...
rffFeatures        :: RFFFeatures -> Vector Double -> Matrix Double  -- φ(x)
rffRidge           :: RFFFeatures -> Double -> Vector Double -> Vector Double -> RFFRidgeFit

-- Multivariate input (n × p)
sampleRFFRBFMV     :: Int -> Int -> Double -> Double -> GenIO -> IO RFFFeaturesMV
rffFeaturesMV      :: RFFFeaturesMV -> Matrix Double -> Matrix Double  -- φ_MV(X)
rffRidgeMV         :: RFFFeaturesMV -> Double -> Matrix Double -> Vector Double -> RFFRidgeFitMV
predictRFFRidgeMV  :: RFFRidgeFitMV -> Matrix Double -> Vector Double

-- Select hyperparameters by marginal likelihood maximization
maximizeMarginalLikRBFMV    :: ...
gridSearchLOOCVRBFMV        :: ...
```

## 3. Minimal Example

```haskell
import qualified System.Random.MWC as MWC
import qualified Numeric.LinearAlgebra as LA
import Hanalyze.Model.RFF

gen <- MWC.createSystemRandom

-- D = 256 random features
let xMat = LA.fromLists [[x1, x2], ...]  -- n × 2
    yVec = LA.fromList [...]
    sf2 = 1.0
    ell = 1.0

rff <- sampleRFFRBFMV 256 (LA.cols xMat) ell sf2 gen
let fit = rffRidgeMV rff 1e-3 xMat yVec
    yPred = predictRFFRidgeMV fit xTestMat
```

## 4. Choosing D (Number of Features)

| D | Performance |
|---|---|
| 64-128 | Fast but coarse approximation, for debugging |
| 256-512 | Well-balanced (default) |
| 1024+ | Close to exact GP, high memory |

Rule of thumb: start with `D = 4-8 × log n`.

## 5. Matérn52 vs RBF

`sampleRFFMatern52` samples ω from StudentT(5) → approximates Matérn52 kernel.
Heavier tails than RBF, more robust to outliers.

## 6. Large-scale Benchmark

| n=2000, D=256 | hanalyze | sklearn (RBFSampler+Ridge) |
|---|---|---|
| RFF fit | 65 ms | 6 ms |
| Exact GP fit | 384 ms | 176 ms |

(The above is a scaling illustration; see [bench/results/SUMMARY.md](../../bench/results/SUMMARY.md) for details)

## 7. Multiple Outputs / Inputs

`rffRidgeMVMulti` fits matrix `Y :: n × q` (q outputs) at once. Closed form: `W = (ΦᵀΦ + λI)⁻¹ Φᵀ Y`.

## 8. CLI

```bash
hanalyze kernel data.csv "x1 x2 x3" y --method rff --features 256 --report fit.html
hanalyze kernel data.csv "x t" y --method rff --group name --xaxis t --report fit.html
```

`--group` color-codes by series, `--xaxis` selects secondary axis. `--auto-hp` maximizes marginal likelihood.

## 9. Interactive GUI

The `--interactive` flag embeds RFF weights + ω + b in HTML; each secondary axis slider interaction retriggers JS-side `φ(x_new)·w` recalculation. Combined with data generators, you get "sparse wide → long → multivariate RFF → predicted curves colored by name in HTML" in one command.

## Related Links

- Exact GP: [04-gp.md](04-gp.ja.md)
- Kernel regression: [04-kernel.md](04-kernel.md)
- Multioutput support: [05-multivariate.md](05-multivariate.md)
