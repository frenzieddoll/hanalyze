-- |
-- Module      : Hanalyze.Model.FDA
-- Description : 関数データ解析 (Functional Data Analysis, FDA)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Functional Data Analysis (FDA)。
--
-- センサ / プロセス時系列を __1 観測 = 1 関数__として扱う Ramsay-Silverman
-- FDA の基礎機能。 個別の生時系列ではなく、 関数空間上の主成分 / 回帰を
-- 直接扱う。
--
-- ## 構成
--
-- - 'smoothBasis': 各サンプルを B-spline basis + 二階差分 (P-spline) penalty
--   で smooth fit → 'FunctionalSample' (basis 係数表現)
-- - 'functionalPCA': basis 係数行列の covariance に PCA、 関数主成分
-- - 'fLM': functional linear regression @y_i = α + ∫ x_i(t) β(t) dt + ε@
--
-- 既存 'Hanalyze.Model.Spline' の @bsplineBasis@ を basis 生成として再利用。
-- Fourier basis は将来拡張 (現状は対象外)。
--
-- Reference: Ramsay & Silverman (2005) "Functional Data Analysis" 2nd ed.
-- Eilers-Marx (1996) "Flexible smoothing with B-splines and penalties" —
-- P-spline 二階差分 penalty。
--
-- [English]: Functional Data Analysis (FDA).
--
-- Basic Ramsay-Silverman FDA functionality that treats sensor\/process
-- time series as __one observation = one function__. Rather than
-- individual raw time series, it works directly with principal components
-- \/ regression over function space.
--
-- == Structure
--
-- - 'smoothBasis': smooth-fits each sample with a B-spline basis + a
--   second-difference (P-spline) penalty → 'FunctionalSample' (basis
--   coefficient representation)
-- - 'functionalPCA': PCA on the covariance of the basis coefficient
--   matrix, giving functional principal components
-- - 'fLM': functional linear regression @y_i = α + ∫ x_i(t) β(t) dt + ε@
--
-- Reuses the existing 'Hanalyze.Model.Spline''s @bsplineBasis@ for
-- basis generation. A Fourier basis is a future extension (out of scope
-- for now).
--
-- Reference: Ramsay & Silverman (2005) "Functional Data Analysis" 2nd ed.
-- Eilers-Marx (1996) "Flexible smoothing with B-splines and penalties" —
-- the P-spline second-difference penalty.
module Hanalyze.Model.FDA
  ( Basis (..)
  , FunctionalSample (..)
  , smoothBasis
  , evalFunctional
    -- * FPCA
  , FunctionalPCA (..)
  , functionalPCA
    -- * Functional Linear Regression
  , FLMResult (..)
  , fLM
  ) where

import qualified Numeric.LinearAlgebra        as LA
import qualified Data.Vector                  as V
import qualified Hanalyze.Model.Spline        as Sp

-- ---------------------------------------------------------------------------
-- 基底
-- ---------------------------------------------------------------------------

-- | [日本語]: basis 種別。 現在は B-spline のみ実装、 Fourier は将来拡張。
--   [English]: Basis kind. Currently only B-spline is implemented; Fourier
--   is a future extension.
data Basis
  = BSpline !Int ![Double]   -- ^ [日本語]: (degree, interior knots、 境界含む) [English]: (degree, interior knots, including boundary)
  deriving (Show)

-- | [日本語]: smooth した関数表現 (basis 係数 + 元 grid)。
--   [English]: A smoothed functional representation (basis coefficients + original grid).
data FunctionalSample = FunctionalSample
  { fsCoef  :: !(LA.Vector Double)   -- ^ [日本語]: basis 係数 [English]: Basis coefficients
  , fsBasis :: !Basis
  , fsGrid  :: !(LA.Vector Double)   -- ^ [日本語]: 元の時間 grid (eval 用) [English]: The original time grid (for evaluation)
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- 33-A1: smoothBasis (P-spline)
-- ---------------------------------------------------------------------------

-- | [日本語]: 複数サンプルを basis + roughness penalty で smooth fit。
--
-- 解: @c = (BᵀB + λ DᵀD)⁻¹ Bᵀy@ (= P-spline、 D は二階差分作用素)。
-- @λ → 0@ で interpolate、 @λ → ∞@ で over-smooth (≈ 一次関数)。
--
-- 入力 @y@ は @n_samples × n_grid@、 各行が 1 サンプル。
--
--   [English]: Smooth-fit multiple samples with a basis + roughness penalty.
--
-- Solution: @c = (BᵀB + λ DᵀD)⁻¹ Bᵀy@ (a P-spline, where D is the
-- second-difference operator). @λ → 0@ interpolates; @λ → ∞@ over-smooths
-- (≈ a linear function).
--
-- Input @y@ is @n_samples × n_grid@, with each row a single sample.
smoothBasis
  :: Basis              -- ^ [日本語]: basis (B-spline) [English]: The basis (B-spline)
  -> Double             -- ^ [日本語]: roughness penalty @λ@ [English]: Roughness penalty @λ@
  -> LA.Vector Double   -- ^ [日本語]: 時間 grid @t@ (長さ @n_grid@) [English]: Time grid @t@ (length @n_grid@)
  -> LA.Matrix Double   -- ^ [日本語]: 観測 @y@ (@n_samples × n_grid@) [English]: Observations @y@ (@n_samples × n_grid@)
  -> [FunctionalSample]
smoothBasis basis@(BSpline deg intKnots) lambda tGrid yMat =
  let tV  = V.fromList (LA.toList tGrid)
      bMat = Sp.bsplineBasis deg intKnots tV   -- n_grid × d
      d   = LA.cols bMat
      btb = LA.tr bMat LA.<> bMat
      penalty = diff2Penalty d
      reg = btb + LA.scale lambda penalty
      -- 各行 (= 1 サンプル) について解く: c = (BᵀB+λΩ)⁻¹ Bᵀy_i
      btY = LA.tr bMat LA.<> LA.tr yMat        -- d × n_samples
      cMat = reg LA.<\> btY                     -- d × n_samples
      n = LA.rows yMat
  in [ FunctionalSample
         { fsCoef  = LA.flatten (cMat LA.¿ [i])
         , fsBasis = basis
         , fsGrid  = tGrid
         }
     | i <- [0 .. n - 1] ]

-- | [日本語]: smooth した関数を任意 grid で評価。
--   [English]: Evaluate the smoothed function on an arbitrary grid.
evalFunctional :: FunctionalSample -> LA.Vector Double -> LA.Vector Double
evalFunctional fs tNew =
  case fsBasis fs of
    BSpline deg intKnots ->
      let tV = V.fromList (LA.toList tNew)
          bM = Sp.bsplineBasis deg intKnots tV
      in bM LA.#> fsCoef fs

-- | [日本語]: 二階差分 penalty 行列 @DᵀD@ (= 連続二階微分の量を有限差分で近似)。
-- @D@ は @(d-2) × d@、 @D_{i,j} = 1 if j=i、 -2 if j=i+1、 1 if j=i+2@。
--
--   [English]: The second-difference penalty matrix @DᵀD@ (approximating
-- the continuous second derivative via finite differences). @D@ is
-- @(d-2) × d@, with @D_{i,j} = 1 if j=i, -2 if j=i+1, 1 if j=i+2@.
diff2Penalty :: Int -> LA.Matrix Double
diff2Penalty d
  | d <= 2    = LA.konst 0 (d, d)
  | otherwise =
      let dM = LA.fromLists
            [ [ if j == i then 1
                else if j == i + 1 then -2
                else if j == i + 2 then 1
                else 0
              | j <- [0 .. d - 1] ]
            | i <- [0 .. d - 3] ]
      in LA.tr dM LA.<> dM

-- ---------------------------------------------------------------------------
-- 33-A2: Functional PCA
-- ---------------------------------------------------------------------------

data FunctionalPCA = FunctionalPCA
  { fpcaScores      :: !(LA.Matrix Double)   -- ^ [日本語]: n × K (各サンプルの主成分得点) [English]: n × K (each sample's principal-component scores)
  , fpcaEigenfn     :: !(LA.Matrix Double)   -- ^ [日本語]: K × n_grid (主成分関数を grid 上で評価) [English]: K × n_grid (principal-component functions evaluated on the grid)
  , fpcaEigenvalues :: !(LA.Vector Double)   -- ^ [日本語]: length K (降順) [English]: length K (descending)
  , fpcaMeanFn      :: !(LA.Vector Double)   -- ^ [日本語]: length n_grid (平均関数) [English]: length n_grid (mean function)
  } deriving (Show)

-- | [日本語]: basis 係数行列の covariance に PCA。 簡略実装として basis 係数空間で
-- PCA を行い、 主成分関数を grid 上で評価して返す (= basis が直交近似で
-- ある前提)。 厳密版は basis mass matrix @J = ∫ B B^T@ で重み付き SVD が
-- 必要だが、 B-spline + dense grid なら直交近似で十分実用に耐える。
--
--   [English]: PCA on the covariance of the basis coefficient matrix. As a
-- simplified implementation, this runs PCA in the basis coefficient
-- space and evaluates the principal-component functions on the grid
-- (assuming the basis is approximately orthogonal). An exact version
-- would need a weighted SVD using the basis mass matrix @J = ∫ B B^T@,
-- but for B-spline + a dense grid the orthogonal approximation is
-- accurate enough for practical use.
functionalPCA
  :: Int                  -- ^ [日本語]: 主成分数 K [English]: Number of principal components K
  -> [FunctionalSample]
  -> FunctionalPCA
functionalPCA k samples =
  let cMat = LA.fromColumns (map fsCoef samples)  -- d × n
      n    = LA.cols cMat
      d    = LA.rows cMat
      mu   = LA.scale (1 / fromIntegral n)
               (cMat LA.#> LA.konst 1 n)
      cCentered = cMat - LA.asColumn mu  -- d × n
      cov = LA.scale (1 / fromIntegral (max 1 (n - 1)))
              (cCentered LA.<> LA.tr cCentered)  -- d × d
      (eigVals, eigVecs) = LA.eigSH (LA.trustSym cov)
      -- hmatrix eigSH は降順で返す
      kEff = min k d
      topVecs = eigVecs LA.¿ [0 .. kEff - 1]    -- d × K
      topVals = LA.subVector 0 kEff eigVals
      -- score: K × n、 各列 = 係数空間での座標
      scoresT = LA.tr topVecs LA.<> cCentered
      -- 主成分関数を grid 上で評価
      sampleBasis = fsBasis (head samples)
      tGrid = fsGrid (head samples)
      eigFn = case sampleBasis of
        BSpline deg intKnots ->
          let bM = Sp.bsplineBasis deg intKnots
                     (V.fromList (LA.toList tGrid))   -- n_grid × d
          in LA.tr (bM LA.<> topVecs)  -- K × n_grid
      meanFn = case sampleBasis of
        BSpline deg intKnots ->
          let bM = Sp.bsplineBasis deg intKnots
                     (V.fromList (LA.toList tGrid))
          in bM LA.#> mu
  in FunctionalPCA
       { fpcaScores      = LA.tr scoresT
       , fpcaEigenfn     = eigFn
       , fpcaEigenvalues = topVals
       , fpcaMeanFn      = meanFn
       }

-- ---------------------------------------------------------------------------
-- 33-A3: Functional Linear Regression
-- ---------------------------------------------------------------------------

data FLMResult = FLMResult
  { flmAlpha  :: !Double                  -- ^ [日本語]: intercept [English]: Intercept
  , flmBetaFn :: !(LA.Vector Double)      -- ^ [日本語]: β(t) を共通 grid 上で評価 [English]: β(t) evaluated on the common grid
  , flmFitted :: !(LA.Vector Double)      -- ^ [日本語]: ŷ_i (length n) [English]: ŷ_i (length n)
  , flmR2     :: !Double
  } deriving (Show)

-- | [日本語]: Functional linear regression: @y_i = α + ∫ x_i(t) β(t) dt + ε@.
--
-- @β(t)@ を同じ basis で展開: @β(t) = B(t)^T γ@。 すると
-- @∫ x_i(t) β(t) dt = c_i^T J γ@ ここで @J = ∫ B(t) B(t)^T dt@ (mass matrix)。
-- 設計行列 @[1, c_i^T J]@ で OLS + 任意の roughness penalty。
--
-- mass matrix @J@ は trapezoidal 積分で近似:
-- @J ≈ Δt · B^T diag(w) B@ where @w@ は等間隔積分重み (端点 0.5、 内点 1)。
--
--   [English]: Functional linear regression: @y_i = α + ∫ x_i(t) β(t) dt + ε@.
--
-- Expand @β(t)@ in the same basis: @β(t) = B(t)^T γ@. Then
-- @∫ x_i(t) β(t) dt = c_i^T J γ@ where @J = ∫ B(t) B(t)^T dt@ (the mass
-- matrix). Runs OLS on the design matrix @[1, c_i^T J]@ plus an optional
-- roughness penalty.
--
-- The mass matrix @J@ is approximated by trapezoidal integration:
-- @J ≈ Δt · B^T diag(w) B@ where @w@ is the evenly-spaced integration
-- weight (0.5 at the endpoints, 1 at interior points).
fLM
  :: [FunctionalSample]   -- ^ [日本語]: X_i(t) [English]: X_i(t)
  -> LA.Vector Double     -- ^ [日本語]: y (n samples) [English]: y (n samples)
  -> Double               -- ^ [日本語]: λ (β(t) の二階差分 penalty) [English]: λ (second-difference penalty on β(t))
  -> FLMResult
fLM samples y lambda =
  let sample0 = head samples
      basis@(BSpline deg intKnots) = fsBasis sample0
      tGrid = fsGrid sample0
      tV    = V.fromList (LA.toList tGrid)
      bM    = Sp.bsplineBasis deg intKnots tV
      nGrid = LA.size tGrid
      -- trapezoidal 重み
      dt    = if nGrid >= 2
                then (LA.atIndex tGrid (nGrid - 1) - LA.atIndex tGrid 0)
                       / fromIntegral (nGrid - 1)
                else 1
      wVec  = LA.fromList
                ([0.5] ++ replicate (max 0 (nGrid - 2)) 1.0 ++ [0.5])
      wScaled = LA.scale dt wVec
      -- mass matrix J = B^T diag(w) B (d × d)
      jMat  = LA.tr bM LA.<> (LA.asColumn wScaled * bM)
      -- 設計行列: 各 i 行 = [1, c_i^T J] (length 1 + d)
      cMat  = LA.fromRows (map fsCoef samples)    -- n × d
      ciJ   = cMat LA.<> jMat                     -- n × d
      n     = LA.rows cMat
      xDes  = LA.fromColumns
                (LA.konst 1 n : LA.toColumns ciJ)  -- n × (1 + d)
      -- penalty: intercept は 0、 γ には二階差分 penalty
      d     = LA.cols cMat
      pen   = diff2Penalty d
      penFull = LA.diagBlock [LA.scalar 0, LA.scale lambda pen]
      reg   = LA.tr xDes LA.<> xDes + penFull
      xty   = LA.tr xDes LA.#> y
      coefs = LA.flatten (reg LA.<\> LA.asColumn xty)
      alpha = LA.atIndex coefs 0
      gamma = LA.subVector 1 d coefs
      yHat  = xDes LA.#> coefs
      resid = y - yHat
      yMean = LA.sumElements y / fromIntegral n
      ssTot = LA.sumElements ((y - LA.scalar yMean) ^ (2 :: Int))
      ssRes = LA.sumElements (resid ^ (2 :: Int))
      r2    = if ssTot == 0 then 0 else 1 - ssRes / ssTot
      betaFn = bM LA.#> gamma
  in FLMResult
       { flmAlpha  = alpha
       , flmBetaFn = betaFn
       , flmFitted = yHat
       , flmR2     = r2
       }
