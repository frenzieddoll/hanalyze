{-# LANGUAGE OverloadedStrings #-}
-- | Gaussian Process 回帰 (GP Regression)
--
-- カーネル関数を選択し、訓練データに当てはめて任意のテスト点で事後予測を行います。
-- ハイパーパラメータは対数周辺尤度 (log marginal likelihood) の最大化で自動最適化されます。
--
-- @
-- import Model.GP
--
-- -- 訓練データ
-- let xs = [0, 0.5 .. 5]
--     ys = map (\x -> sin x + 0.1 * noise) xs
--
-- -- ハイパーパラメータをデータから初期化し最適化
-- let p0  = initParamsFromData xs ys
--     opt = optimizeGP RBF xs ys p0
--     res = fitGP (GPModel RBF opt) xs ys testXs
--
-- -- gpMean res, gpLower res, gpUpper res で結果を取得
-- @
module Model.GP
  ( -- * カーネル型
    Kernel (..)
  , kernelName
    -- * ハイパーパラメータ
  , GPParams (..)
  , defaultGPParams
  , initParamsFromData
    -- * モデルと結果
  , GPModel (..)
  , GPResult (..)
    -- * カーネル計算
  , kernelFn
  , buildKernelMatrix
    -- * 推論
  , logMarginalLikelihood
  , fitGP
  , fitGPMulti
  , optimizeGP
    -- * 対話的予測用データ
  , GPPredData (..)
  , gpPredData
  ) where

import Data.Text (Text)
import qualified Numeric.LinearAlgebra as LA
import qualified Optim.GradAscent
import qualified Optim.Numeric
import qualified Optim.LBFGS as LBFGS
import qualified Optim.Common as OC
import System.IO.Unsafe (unsafePerformIO)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | GP カーネル関数の種類。
data Kernel
  = RBF
    -- ^ Squared Exponential: k(x,x') = σ_f² exp(−r²/(2ℓ²))
    --   滑らかな関数に適している。最もよく使われるカーネル。
  | Matern52
    -- ^ Matérn 5/2: k(x,x') = σ_f²(1+√5 r/ℓ+5r²/(3ℓ²))exp(−√5 r/ℓ)
    --   RBF より少し荒れた関数に対応。物理系でよく使われる。
  | Periodic
    -- ^ Periodic: k(x,x') = σ_f² exp(−2 sin²(π r/p)/ℓ²)
    --   周期的なパターンを持つ関数に適している。gpPeriod を適切に設定する。
  deriving (Show, Eq)

-- | カーネル名 (表示用)。
kernelName :: Kernel -> Text
kernelName RBF       = "RBF"
kernelName Matern52  = "Mat\xe9rn 5/2"
kernelName Periodic  = "Periodic"

-- | GP ハイパーパラメータ。
data GPParams = GPParams
  { gpLengthScale :: Double
    -- ^ ℓ: 長さスケール。大きいほど滑らか。
  , gpSignalVar   :: Double
    -- ^ σ_f²: シグナル分散。関数値の変動幅。
  , gpNoiseVar    :: Double
    -- ^ σ_n²: 観測ノイズ分散。0 に近いと補間、大きいと平滑化。
  , gpPeriod      :: Double
    -- ^ p: 周期 (Periodic カーネルのみ使用)。
  } deriving (Show)

-- | デフォルトハイパーパラメータ。
defaultGPParams :: GPParams
defaultGPParams = GPParams 1.0 1.0 0.1 1.0

-- | データの統計量から初期ハイパーパラメータを設定する。
-- 最適化の初期値として使う。
initParamsFromData :: [Double] -> [Double] -> GPParams
initParamsFromData xs ys = GPParams
  { gpLengthScale = max 0.01 ((xMax - xMin) / 4)
  , gpSignalVar   = max 0.01 yVar
  , gpNoiseVar    = max 1e-4 (yVar * 0.05)
  , gpPeriod      = max 0.01 (xMax - xMin)
  }
  where
    xMin  = minimum xs
    xMax  = maximum xs
    yMean = sum ys / fromIntegral (length ys)
    yVar  = sum (map (\y -> (y - yMean) ^ (2 :: Int)) ys) / fromIntegral (length ys)

-- | GP モデル: カーネルとハイパーパラメータの組み合わせ。
data GPModel = GPModel
  { gpKernel :: Kernel
  , gpParams :: GPParams
  } deriving (Show)

-- | GP 事後予測の結果。
data GPResult = GPResult
  { gpTestX  :: [Double]   -- ^ テスト点 x_*
  , gpMean   :: [Double]   -- ^ 事後平均 μ(x_*)
  , gpVar    :: [Double]   -- ^ 事後分散 σ²(x_*)
  , gpLower  :: [Double]   -- ^ 平均 − 2σ (≈95% 信用区間下限)
  , gpUpper  :: [Double]   -- ^ 平均 + 2σ (≈95% 信用区間上限)
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Kernel
-- ---------------------------------------------------------------------------

-- | カーネル関数 k(x, x') の計算。
kernelFn :: Kernel -> GPParams -> Double -> Double -> Double
kernelFn RBF p x x' =
  let d = x - x'
      l = gpLengthScale p
  in gpSignalVar p * exp (-(d * d) / (2 * l * l))
kernelFn Matern52 p x x' =
  let d = abs (x - x')
      l = gpLengthScale p
      s = sqrt 5 * d / l
  in gpSignalVar p * (1 + s + s * s / 3) * exp (-s)
kernelFn Periodic p x x' =
  let d = abs (x - x')
      l = gpLengthScale p
      s = sin (pi * d / gpPeriod p)
  in gpSignalVar p * exp (-2 * s * s / (l * l))

-- | カーネル行列 K(xs, xs') を構築する。結果のサイズは (|xs|, |xs'|)。
buildKernelMatrix :: Kernel -> GPParams -> [Double] -> [Double] -> LA.Matrix Double
buildKernelMatrix ker p xs xs' =
  (n LA.>< m) [kernelFn ker p x x' | x <- xs, x' <- xs']
  where
    n = length xs
    m = length xs'

-- ---------------------------------------------------------------------------
-- Inference
-- ---------------------------------------------------------------------------

-- ノイズ付きカーネル行列 K_y = K(X,X) + σ_n² I を構築する（最小ジッター付き）。
noiseKernel :: Kernel -> GPParams -> [Double] -> LA.Matrix Double
noiseKernel ker p xs =
  let n      = length xs
      k      = buildKernelMatrix ker p xs xs
      jitter = max (gpNoiseVar p) 1e-6
  in k `LA.add` LA.scale jitter (LA.ident n)

-- | 対数周辺尤度 log p(y | X, θ) を計算する。
-- ハイパーパラメータ最適化の目的関数として使用する。
--
-- log p = −½ yᵀ Ky⁻¹ y − ½ log|Ky| − n/2 log(2π)
logMarginalLikelihood :: [Double] -> [Double] -> Kernel -> GPParams -> Double
logMarginalLikelihood trainX trainY ker params =
  let n    = length trainX
      ky   = noiseKernel ker params trainX
      y    = LA.fromList trainY
      -- Cholesky 分解: ky = Rᵀ R  (R は上三角)
      r    = LA.chol (LA.sym ky)
      logDet  = 2 * sum (map log (LA.toList (LA.takeDiag r)))
      alpha   = ky LA.<\> y
      dataFit = LA.dot y alpha
  in -0.5 * dataFit - 0.5 * logDet - fromIntegral n / 2 * log (2 * pi)

-- | テスト点 testX での GP 事後予測 (単出力)。
-- 多出力 'fitGPMulti' に y を 1 列行列化して委譲、列 0 を取り出す。
--
-- 事後平均: μ_* = K_*ᵀ Ky⁻¹ y
-- 事後分散: σ²_i = k(x*_i, x*_i) − K_*[i] Ky⁻¹ K_*[i]ᵀ
fitGP :: GPModel -> [Double] -> [Double] -> [Double] -> GPResult
fitGP model trainX trainY testX =
  let yMat = LA.asColumn (LA.fromList trainY)
      (meanMat, varList) = fitGPMulti model trainX yMat testX
      mu = LA.toList (LA.flatten (meanMat LA.¿ [0]))
      stdList = map sqrt varList
  in GPResult
       { gpTestX  = testX
       , gpMean   = mu
       , gpVar    = varList
       , gpLower  = zipWith (\m s -> m - 2 * s) mu stdList
       , gpUpper  = zipWith (\m s -> m + 2 * s) mu stdList
       }

-- | 多出力 GP 事後予測。Y は n × q (列 = 出力タスク)、すべて同じカーネルと
-- ハイパーパラメータを共有する (Cholesky / Ky⁻¹ も共有)。
--
-- 戻り値: (事後平均行列 m × q, 事後分散ベクトル 長さ m)。
-- 分散は y に依らないため q 出力で共通。
fitGPMulti :: GPModel -> [Double] -> LA.Matrix Double -> [Double]
           -> (LA.Matrix Double, [Double])
fitGPMulti model trainX trainY testX =
  let ker    = gpKernel model
      params = gpParams model
      ky     = noiseKernel ker params trainX
      kyInv  = LA.inv ky
      alpha  = kyInv LA.<> trainY                  -- (n × q)
      kStar  = buildKernelMatrix ker params testX trainX  -- (m × n)
      meanMt = kStar LA.<> alpha                    -- (m × q)
      w      = kStar LA.<> kyInv                    -- (m × n)
      diagKss = [kernelFn ker params x x | x <- testX]
      varList = zipWith3 (\d ks wi -> max 0 (d - LA.dot ks wi))
                  diagKss (LA.toRows kStar) (LA.toRows w)
  in (meanMt, varList)

-- ---------------------------------------------------------------------------
-- Hyperparameter optimisation
-- ---------------------------------------------------------------------------

-- | 対数周辺尤度を最大化してハイパーパラメータを最適化する。
-- log-space で (ℓ, σ_f², σ_n²) を **L-BFGS** (準ニュートン) で最大化。
-- 数値勾配 (中央差分) を使うので gradFn の解析実装は不要。
--
-- 旧実装 (`Optim.GradAscent` + 数値勾配) より一般に 5-10 倍速く、初期値
-- 依存性も低い。`unsafePerformIO` を使うが L-BFGS は決定的なので参照透過。
optimizeGP :: Kernel -> [Double] -> [Double] -> GPParams -> GPParams
optimizeGP ker trainX trainY p0 =
  let u0   = [log (gpLengthScale p0), log (gpSignalVar p0), log (gpNoiseVar p0)]
      -- L-BFGS は最小化なので、log-mlik を最大化したいときは Maximize 指定
      cfg  = LBFGS.defaultLBFGSConfig
               { LBFGS.lbDir   = OC.Maximize
               , LBFGS.lbStop  = OC.defaultStopCriteria
                                   { OC.stMaxIter = 200, OC.stTolFun = 1e-8 }
               }
      result = unsafePerformIO $ LBFGS.runLBFGSNumeric cfg obj u0
      uOpt   = OC.orBest result
  in p0
       { gpLengthScale = exp (uOpt !! 0)
       , gpSignalVar   = exp (uOpt !! 1)
       , gpNoiseVar    = exp (uOpt !! 2)
       }
  where
    toParams u = p0
      { gpLengthScale = exp (u !! 0)
      , gpSignalVar   = exp (u !! 1)
      , gpNoiseVar    = exp (u !! 2)
      }
    obj u = logMarginalLikelihood trainX trainY ker (toParams u)

-- ---------------------------------------------------------------------------
-- Interactive prediction data (for Viz.GPReport)
-- ---------------------------------------------------------------------------

-- | JavaScript 対話予測に必要な内部データ。
-- Ky⁻¹ と α = Ky⁻¹ y を事前に計算して保持する。
data GPPredData = GPPredData
  { pdTrainX :: [Double]     -- ^ 訓練点 X
  , pdAlpha  :: [Double]     -- ^ α = Ky⁻¹ y (長さ n)
  , pdKyInv  :: [[Double]]   -- ^ Ky⁻¹ を行リストで表現 (n × n)
  } deriving (Show)

-- | 訓練データから GPPredData を計算する。
gpPredData :: GPModel -> [Double] -> [Double] -> GPPredData
gpPredData model trainX trainY =
  let ker    = gpKernel model
      params = gpParams model
      n      = length trainX
      k      = buildKernelMatrix ker params trainX trainX
      jitter = max (gpNoiseVar params) 1e-6
      ky     = k `LA.add` LA.scale jitter (LA.ident n)
      kyInv  = LA.inv ky
      alpha  = LA.toList (kyInv LA.#> LA.fromList trainY)
  in GPPredData trainX alpha (map LA.toList (LA.toRows kyInv))
