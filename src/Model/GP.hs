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
  , optimizeGP
    -- * 対話的予測用データ
  , GPPredData (..)
  , gpPredData
  ) where

import Data.Text (Text)
import qualified Numeric.LinearAlgebra as LA

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

-- | テスト点 testX での GP 事後予測を行う。
--
-- 事後平均: μ_* = K_*ᵀ Ky⁻¹ y
-- 事後分散: σ²_i = k(x*_i, x*_i) − K_*[i] Ky⁻¹ K_*[i]ᵀ
fitGP :: GPModel -> [Double] -> [Double] -> [Double] -> GPResult
fitGP model trainX trainY testX =
  let ker    = gpKernel model
      params = gpParams model
      ky     = noiseKernel ker params trainX
      y      = LA.fromList trainY
      kyInv  = LA.inv ky
      -- 事後平均
      alpha   = kyInv LA.#> y
      kStar   = buildKernelMatrix ker params testX trainX  -- (m, n)
      meanVec = kStar LA.#> alpha                          -- (m,)
      -- 事後分散
      w        = kStar LA.<> kyInv             -- (m, n): K_* Ky⁻¹
      diagKss  = [kernelFn ker params x x | x <- testX]
      varList  = zipWith3 (\d ks wi -> max 0 (d - LA.dot ks wi))
                   diagKss (LA.toRows kStar) (LA.toRows w)
      stdList  = map sqrt varList
      mu       = LA.toList meanVec
  in GPResult
       { gpTestX  = testX
       , gpMean   = mu
       , gpVar    = varList
       , gpLower  = zipWith (\m s -> m - 2 * s) mu stdList
       , gpUpper  = zipWith (\m s -> m + 2 * s) mu stdList
       }

-- ---------------------------------------------------------------------------
-- Hyperparameter optimisation
-- ---------------------------------------------------------------------------

-- | 対数周辺尤度を最大化してハイパーパラメータを最適化する。
-- log-space で (ℓ, σ_f², σ_n²) に対して数値勾配上昇法を適用する。
optimizeGP :: Kernel -> [Double] -> [Double] -> GPParams -> GPParams
optimizeGP ker trainX trainY p0 =
  let u0   = [log (gpLengthScale p0), log (gpSignalVar p0), log (gpNoiseVar p0)]
      uOpt = gradientAscent (400 :: Int) 0.1 u0
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

    numGrad u =
      [ (obj (upd i (u !! i + h)) - obj (upd i (u !! i - h))) / (2 * h)
      | i <- [0 .. 2]
      ]
      where
        h     = 1e-4
        upd i v = take i u ++ [v] ++ drop (i + 1) u

    gradientAscent :: Int -> Double -> [Double] -> [Double]
    gradientAscent 0   _  u = u
    gradientAscent itr lr u =
      let g     = numGrad u
          gnorm = sqrt (sum (map (\x -> x * x) g))
      in if gnorm < 1e-8
           then u
           else gradientAscent (itr - 1) (lr * 0.995)
                  (zipWith (\ui gi -> ui + lr * gi / gnorm) u g)

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
