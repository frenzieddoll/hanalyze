{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Model.Kernel
-- Description : GP/SVM/カーネル法で共通のカーネル語彙 (RBF/Matern52/Periodic/Linear/Poly)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 共有カーネル語彙 (GP / SVM / カーネル法で共通) — Phase 75.18 で 'Model.GP'
-- から分離。
--
-- GP 族の定常/内積カーネル ('RBF' / 'Matern52' / 'Periodic' / 'Linear' / 'Poly') と
-- そのハイパーパラメータ 'KernelParams' (ℓ / σ_f² / period / ARD per-dim ℓ) を集約する。
-- 'GPParams' (= 'KernelParams' + 観測ノイズ σ_n²) に依存しないので、 SVM 等
-- ノイズを持たないカーネル法はこのモジュールだけを import すればよい
-- ('Model.GP' を import しない)。
--
-- 評価関数:
--
--   * 'kernelFn'            — 1D 入力の @k(x, x')@。
--   * 'buildKernelMatrix'   — 1D の Gram 行列 @K(xs, xs')@。
--   * 'applyKernel'         — 二乗距離行列 → カーネル行列 (距離カーネル専用)。
--   * 'kernelOfParams'      — 固定パラメータの @s ↦ k(s)@ (距離カーネル専用・INLINE)。
--   * 'ardScaleXY'          — ARD 列スケーリング。
--   * 'buildKernelMatrixMV' — 多入力 Gram 行列 (全カーネル)。
--   * 'kEvalMV'             — 多入力の点対点評価 @k(a, b)@ (全カーネル・SVM 等の汎用経路)。
--
-- 距離カーネル (RBF/Matern52/Periodic) は二乗距離から、 内積カーネル
-- (Linear/Poly) は内積から評価する。 'applyKernel' / 'kernelOfParams' は距離専用で、
-- 内積カーネルを渡すと error (multi-input gram は 'buildKernelMatrixMV' が内積経路へ
-- 分岐するためそこには到達しない)。
module Hanalyze.Model.Kernel
  ( -- * カーネル型
    Kernel (..)
  , kernelName
    -- * カーネルハイパーパラメータ
  , KernelParams (..)
  , defaultKernelParams
    -- * 評価
  , kernelFn
  , buildKernelMatrix
  , applyKernel
  , kernelOfParams
  , ardScaleXY
  , buildKernelMatrixMV
  , kEvalMV
  ) where

import           Data.Text (Text)
import qualified Data.Text                    as T
import qualified Numeric.LinearAlgebra        as LA
import qualified Hanalyze.Stat.KernelDist as KD
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Control.Monad.ST             (runST)

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | GP / SVM 族のカーネル種別。
data Kernel
  = RBF
    -- ^ Squared exponential: @k(x,x') = σ_f² exp(−r²/(2ℓ²))@.
    --   Best for smooth functions; the most commonly used kernel.
  | Matern52
    -- ^ Matérn 5/2: @k(x,x') = σ_f²(1+√5 r/ℓ+5r²/(3ℓ²)) exp(−√5 r/ℓ)@.
    --   Slightly rougher than RBF; common in physical systems.
  | Periodic
    -- ^ Periodic: @k(x,x') = σ_f² exp(−2 sin²(π r/p)/ℓ²)@.
    --   For periodic patterns; set 'kpPeriod' appropriately.
  | Linear
    -- ^ Linear (dot-product): @k(x,x') = σ_f² (x·x')@. A non-stationary
    --   kernel; with SVM gives a linear decision boundary. (Phase 75.14)
  | Poly !Int
    -- ^ Polynomial of degree @d@: @k(x,x') = (γ (x·x') + 1)^d@ with
    --   @γ = 1/(2ℓ²)@ (shared with the SVM γ convention). A
    --   non-stationary kernel. (Phase 75.14)
  deriving (Show, Eq)

-- | Display name of a kernel.
kernelName :: Kernel -> Text
kernelName RBF       = "RBF"
kernelName Matern52  = "Mat\xe9rn 5/2"
kernelName Periodic  = "Periodic"
kernelName Linear    = "Linear"
kernelName (Poly d)  = "Poly(" <> T.pack (show d) <> ")"

-- | カーネルハイパーパラメータ (観測ノイズ σ_n² は含まない)。
data KernelParams = KernelParams
  { kpLengthScale  :: Double
    -- ^ Isotropic length scale @ℓ@; larger means smoother. Used unless
    --   'kpLengthScales' is 'Just' (= ARD), in which case the per-dim
    --   vector overrides this for multi-input kernel evaluation.
  , kpSignalVar    :: Double
    -- ^ Signal variance @σ_f²@; the variability of the function values.
  , kpPeriod       :: Double
    -- ^ Period @p@ (only used by the @Periodic@ kernel).
  , kpLengthScales :: Maybe (LA.Vector Double)
    -- ^ Per-dim length scales for ARD (Automatic Relevance
    --   Determination). When 'Just' v, the multi-input kernel uses
    --   @D_ARD[i,j] = Σ_d (X[i,d] − X'[j,d])² / ℓ_d²@ instead of the
    --   isotropic distance / ℓ². Has no effect on the 1D 'kernelFn'
    --   path. 'Nothing' = isotropic (default).
  } deriving (Show)

-- | Default kernel hyperparameters: @ℓ = σ_f² = p = 1@, isotropic.
defaultKernelParams :: KernelParams
defaultKernelParams = KernelParams 1.0 1.0 1.0 Nothing

-- ---------------------------------------------------------------------------
-- 1D 評価
-- ---------------------------------------------------------------------------

-- | Evaluate the kernel function @k(x, x')@ for scalar inputs.
kernelFn :: Kernel -> KernelParams -> Double -> Double -> Double
kernelFn RBF p x x' =
  let d = x - x'
      l = kpLengthScale p
  in kpSignalVar p * exp (-(d * d) / (2 * l * l))
kernelFn Matern52 p x x' =
  let d = abs (x - x')
      l = kpLengthScale p
      s = sqrt 5 * d / l
  in kpSignalVar p * (1 + s + s * s / 3) * exp (-s)
kernelFn Periodic p x x' =
  let d = abs (x - x')
      l = kpLengthScale p
      s = sin (pi * d / kpPeriod p)
  in kpSignalVar p * exp (-2 * s * s / (l * l))
kernelFn Linear p x x' =
  -- 内積カーネル: 1D では x·x' = x*x'。
  kpSignalVar p * (x * x')
kernelFn (Poly d) p x x' =
  -- (γ x·x' + 1)^d, γ = 1/(2ℓ²)。1D では x·x' = x*x'。
  let l = kpLengthScale p
      g = 1 / (2 * l * l)
  in (g * (x * x') + 1) ^^ d

-- | Build the kernel matrix @K(xs, xs')@ of shape @|xs| × |xs'|@.
--
-- Phase 11b (2026-05-14): fill a flat 'Storable.Vector' via @runST +
-- MVector@ instead of materialising the @|xs|·|xs'|@ lazy @[Double]@
-- list (one allocation per kernel call). 'kernelFn' itself is unchanged
-- so 'Periodic' (signed-difference dependent) keeps working.
buildKernelMatrix :: Kernel -> KernelParams -> [Double] -> [Double] -> LA.Matrix Double
buildKernelMatrix ker p xs xs' =
  let xv = VS.fromList xs
      yv = VS.fromList xs'
      n  = VS.length xv
      m  = VS.length yv
      out = runST $ do
        v <- VSM.unsafeNew (n * m)
        let go !i !j
              | i >= n    = pure ()
              | j >= m    = go (i + 1) 0
              | otherwise = do
                  let xi = VS.unsafeIndex xv i
                      yj = VS.unsafeIndex yv j
                  VSM.unsafeWrite v (i * m + j) (kernelFn ker p xi yj)
                  go i (j + 1)
        go 0 0
        VS.unsafeFreeze v
  in LA.reshape m out

-- ---------------------------------------------------------------------------
-- 多入力 (multivariate) 評価
-- ---------------------------------------------------------------------------

-- | Apply the kernel function to an @m × n@ matrix of squared distances.
-- 距離カーネル (RBF/Matern52/Periodic) 専用。 内積カーネル (Linear/Poly) は
-- 二乗距離から復元できないため error (multi-input gram は 'buildKernelMatrixMV'
-- が内積経路へ分岐するためここには到達しない)。
applyKernel :: Kernel -> KernelParams -> LA.Matrix Double -> LA.Matrix Double
applyKernel RBF p d2 =
  let l2 = kpLengthScale p ** 2
      sf = kpSignalVar p
  in KD.mapMatrix (\s -> sf * exp (- s / (2 * l2))) d2
applyKernel Matern52 p d2 =
  let l  = kpLengthScale p
      sf = kpSignalVar p
  in KD.mapMatrix (\s -> let r = sqrt (max 0 s)
                             u = sqrt 5 * r / l
                         in sf * (1 + u + u * u / 3) * exp (- u)) d2
applyKernel Periodic p d2 =
  let l  = kpLengthScale p
      sf = kpSignalVar p
      pr = kpPeriod p
  in KD.mapMatrix (\s -> let r = sqrt (max 0 s)
                             ss = sin (pi * r / pr)
                         in sf * exp (- 2 * ss * ss / (l * l))) d2
applyKernel Linear   _ _ = error "applyKernel: Linear は内積カーネル。buildKernelMatrixMV/kEvalMV を使うこと"
applyKernel (Poly _) _ _ = error "applyKernel: Poly は内積カーネル。buildKernelMatrixMV/kEvalMV を使うこと"

-- | Apply ARD scaling to (X, X') if 'kpLengthScales' is 'Just'. Returns
-- the (possibly rescaled) matrices and a 'KernelParams' with @ℓ = 1@ so
-- that 'applyKernel' divides by 1 (the per-dim ℓ_d already absorbed into
-- the column scaling). 'Nothing' = isotropic, returns inputs and params
-- unchanged. The 'Periodic' kernel does not support ARD.
ardScaleXY
  :: Kernel -> KernelParams -> LA.Matrix Double -> LA.Matrix Double
  -> (LA.Matrix Double, LA.Matrix Double, KernelParams)
ardScaleXY Periodic p x y = (x, y, p)
ardScaleXY _        p x y = case kpLengthScales p of
  Nothing -> (x, y, p)
  Just ls ->
    let p_     = LA.cols x
        lsExt  = if LA.size ls == p_
                   then ls
                   else LA.konst (kpLengthScale p) p_  -- safety fallback
        invL   = LA.cmap (1 /) lsExt                 -- 1 / ℓ_d
        scaleCols m = m LA.<> LA.diag invL
        x'     = scaleCols x
        y'     = scaleCols y
        p'     = p { kpLengthScale = 1.0 }
    in (x', y', p')

-- | Build the kernel matrix @K(X, X')@ of shape @|X| × |X'|@ from
-- multi-input matrices. @X@ is @n × p@; @X'@ is @m × p@.
--
-- When 'kpLengthScales' is 'Just', uses ARD: each input dimension is
-- scaled by @1 / ℓ_d@ before computing pairwise squared distances.
buildKernelMatrixMV
  :: Kernel -> KernelParams -> LA.Matrix Double -> LA.Matrix Double
  -> LA.Matrix Double
buildKernelMatrixMV Linear p x x' =
  -- 内積カーネル: K = σ_f² X X'ᵀ (距離経路を通さない)。
  LA.scale (kpSignalVar p) (x LA.<> LA.tr x')
buildKernelMatrixMV (Poly d) p x x' =
  -- (γ X X'ᵀ + 1)^d, γ = 1/(2ℓ²)。
  let l = kpLengthScale p
      g = 1 / (2 * l * l)
  in LA.cmap (\ip -> (g * ip + 1) ^^ d) (x LA.<> LA.tr x')
buildKernelMatrixMV ker p x x' =
  let (xs, ys, p') = ardScaleXY ker p x x'
  in applyKernel ker p' (KD.pairwiseSqDistXY xs ys)

-- | 多入力カーネル評価 @k(a, b)@ (全カーネル対応・SVM 等の汎用経路)。
-- 距離カーネル (RBF/Matern52/Periodic) は二乗距離、 内積カーネル (Linear/Poly)
-- は内積から評価する。 (Phase 75.14)
kEvalMV :: Kernel -> KernelParams -> LA.Vector Double -> LA.Vector Double -> Double
kEvalMV Linear   p a b = kpSignalVar p * (a LA.<.> b)
kEvalMV (Poly d) p a b =
  let l = kpLengthScale p
      g = 1 / (2 * l * l)
  in (g * (a LA.<.> b) + 1) ^^ d
kEvalMV ker      p a b =
  let d = a - b
  in kernelOfParams ker p (d LA.<.> d)   -- 距離カーネル: s = ‖a−b‖²

-- | Specialized kernel function for a fixed parameter set, returning a
-- monomorphic @Double -> Double@ that GHC can inline tightly into the
-- @mkNoiseKernelFromD2@ inner loop (in 'Model.GP'). 距離カーネル専用。
{-# INLINE kernelOfParams #-}
kernelOfParams :: Kernel -> KernelParams -> (Double -> Double)
kernelOfParams RBF p =
  let !l2 = kpLengthScale p ** 2
      !sf = kpSignalVar p
      !inv2L2 = 1 / (2 * l2)
  in \s -> sf * exp (- s * inv2L2)
kernelOfParams Matern52 p =
  let !l  = kpLengthScale p
      !sf = kpSignalVar p
      !invL = sqrt 5 / l
  in \s -> let r = sqrt (max 0 s)
               u = invL * r
           in sf * (1 + u + u * u / 3) * exp (- u)
kernelOfParams Periodic p =
  let !l  = kpLengthScale p
      !sf = kpSignalVar p
      !pr = kpPeriod p
      !invL2 = 1 / (l * l)
      !invPr = pi / pr
  in \s -> let r  = sqrt (max 0 s)
               ss = sin (invPr * r)
           in sf * exp (- 2 * ss * ss * invL2)
kernelOfParams Linear   _ = error "kernelOfParams: Linear は内積カーネル。kEvalMV を使うこと"
kernelOfParams (Poly _) _ = error "kernelOfParams: Poly は内積カーネル。kEvalMV を使うこと"
