{-# LANGUAGE OverloadedStrings #-}
-- | 正則化回帰 (Ridge / Lasso / Elastic Net) を 1 つのモジュールに統合。
--
-- ペナルティを sum-type 'Penalty' で表現し、'fitRegularized' で
-- 4 種類のモデルすべてに対応する:
--
-- > NoPen                          -- 通常の OLS
-- > L2 lambda                      -- Ridge regression
-- > L1 lambda                      -- Lasso regression
-- > ElasticNet lambda1 lambda2     -- Elastic Net (L1 + L2)
--
-- Ridge は閉形式、Lasso と Elastic Net は coordinate descent。
--
-- 注意: Lasso / Elastic Net は X の列スケールに敏感。事前に
-- standardize (各列を平均 0、分散 1 に) しておくのが一般的。
module Model.Regularized
  ( Penalty (..)
  , RegFit (..)
  , fitRegularized
  , predictRegularized
  , standardize
  , unstandardizeBeta
  ) where

import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import Data.List (foldl')

-- ---------------------------------------------------------------------------
-- ペナルティ型
-- ---------------------------------------------------------------------------

-- | 正則化ペナルティ。
data Penalty
  = NoPen                       -- ^ 通常 OLS (= λ = 0)
  | L2 Double                   -- ^ Ridge: 0.5 λ ||β||₂²
  | L1 Double                   -- ^ Lasso: λ ||β||₁
  | ElasticNet Double Double    -- ^ ElasticNet: λ₁ ||β||₁ + 0.5 λ₂ ||β||₂²
  deriving (Show, Eq)

-- | フィット結果。
data RegFit = RegFit
  { rfBeta     :: LA.Vector Double
  , rfYHat     :: LA.Vector Double
  , rfResid    :: LA.Vector Double
  , rfR2       :: Double
  , rfPenalty  :: Penalty
  , rfNonZero  :: Int           -- ^ |β_j| > 1e-8 の数 (Lasso の sparsity 評価)
  , rfIters    :: Int           -- ^ 反復回数 (CD アルゴリズム用; 閉形式は 0)
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- メイン API
-- ---------------------------------------------------------------------------

-- | ペナルティ付き回帰を fit。
fitRegularized :: Penalty -> LA.Matrix Double -> LA.Vector Double -> RegFit
fitRegularized pen x y = case pen of
  NoPen        -> fitOLS x y
  L2 lambda    -> fitRidge lambda x y
  L1 lambda    -> fitLasso lambda x y 1000 1e-7
  ElasticNet l1 l2 -> fitElasticNet l1 l2 x y 1000 1e-7

-- | 予測。
predictRegularized :: RegFit -> LA.Matrix Double -> LA.Vector Double
predictRegularized fit xNew = xNew LA.#> rfBeta fit

-- ---------------------------------------------------------------------------
-- OLS (NoPen)
-- ---------------------------------------------------------------------------

fitOLS :: LA.Matrix Double -> LA.Vector Double -> RegFit
fitOLS x y =
  let beta = LA.flatten (x LA.<\> LA.asColumn y)
      yHat = x LA.#> beta
      r    = y - yHat
  in mkRegFit beta yHat r y NoPen 0

-- ---------------------------------------------------------------------------
-- Ridge (closed form)
-- ---------------------------------------------------------------------------

-- | Ridge 回帰: β = (XᵀX + λI)⁻¹ Xᵀy
fitRidge :: Double -> LA.Matrix Double -> LA.Vector Double -> RegFit
fitRidge lambda x y =
  let p    = LA.cols x
      xtx  = LA.tr x LA.<> x
      reg  = xtx + LA.scale lambda (LA.ident p)
      xty  = LA.tr x LA.#> y
      beta = LA.flatten (reg LA.<\> LA.asColumn xty)
      yHat = x LA.#> beta
      r    = y - yHat
  in mkRegFit beta yHat r y (L2 lambda) 0

-- ---------------------------------------------------------------------------
-- Lasso (Coordinate Descent + Soft-thresholding)
-- ---------------------------------------------------------------------------

-- | ソフト閾値関数: S(z, γ) = sign(z) × max(|z| − γ, 0)
softThreshold :: Double -> Double -> Double
softThreshold z gamma
  | z >  gamma = z - gamma
  | z < -gamma = z + gamma
  | otherwise  = 0

-- | Lasso 回帰: β = argmin (1/2n) ||y - Xβ||² + λ ||β||₁
--
-- Coordinate descent: 各 β_j を順に更新。
--   r = y - X β
--   ρ_j = (1/n) X_jᵀ r + β_j × (1/n) ||X_j||²
--   β_j ← S(ρ_j, λ) / ((1/n) ||X_j||²)
fitLasso :: Double -> LA.Matrix Double -> LA.Vector Double
         -> Int -> Double -> RegFit
fitLasso lambda x y maxIter tol =
  let n       = fromIntegral (LA.rows x) :: Double
      p       = LA.cols x
      cols    = [LA.flatten (x LA.¿ [j]) | j <- [0 .. p - 1]]
      colSqN  = [LA.sumElements (c * c) / n | c <- cols]
      beta0   = LA.fromList (replicate p 0)
      -- coordinate descent 1 反復: 全 j 更新
      sweep beta =
        foldl' updateJ beta [0 .. p - 1]
        where
          updateJ b j =
            let xj      = cols !! j
                yPred   = x LA.#> b
                resid   = y - yPred
                bj      = b `LA.atIndex` j
                rho     = (LA.sumElements (xj * resid)) / n + bj * (colSqN !! j)
                bjNew   = softThreshold rho lambda / (colSqN !! j)
            in LA.fromList
                 [ if k == j then bjNew else b `LA.atIndex` k
                 | k <- [0 .. p - 1] ]
      iterate' k beta
        | k >= maxIter = (beta, k)
        | otherwise =
            let beta' = sweep beta
                diff  = LA.norm_2 (beta' - beta)
            in if diff < tol then (beta', k)
                 else iterate' (k + 1) beta'
      (betaFinal, iters) = iterate' 0 beta0
      yHat = x LA.#> betaFinal
      r    = y - yHat
  in mkRegFit betaFinal yHat r y (L1 lambda) iters

-- ---------------------------------------------------------------------------
-- Elastic Net (Coordinate Descent)
-- ---------------------------------------------------------------------------

-- | Elastic Net: β = argmin (1/2n) ||y - Xβ||² + λ₁ ||β||₁ + 0.5 λ₂ ||β||²
--
-- Coordinate descent: β_j ← S(ρ_j, λ₁) / ((1/n) ||X_j||² + λ₂)
fitElasticNet :: Double -> Double -> LA.Matrix Double -> LA.Vector Double
              -> Int -> Double -> RegFit
fitElasticNet lambda1 lambda2 x y maxIter tol =
  let n       = fromIntegral (LA.rows x) :: Double
      p       = LA.cols x
      cols    = [LA.flatten (x LA.¿ [j]) | j <- [0 .. p - 1]]
      colSqN  = [LA.sumElements (c * c) / n | c <- cols]
      beta0   = LA.fromList (replicate p 0)
      sweep beta =
        foldl' updateJ beta [0 .. p - 1]
        where
          updateJ b j =
            let xj      = cols !! j
                yPred   = x LA.#> b
                resid   = y - yPred
                bj      = b `LA.atIndex` j
                rho     = (LA.sumElements (xj * resid)) / n + bj * (colSqN !! j)
                bjNew   = softThreshold rho lambda1
                          / ((colSqN !! j) + lambda2)
            in LA.fromList
                 [ if k == j then bjNew else b `LA.atIndex` k
                 | k <- [0 .. p - 1] ]
      iterate' k beta
        | k >= maxIter = (beta, k)
        | otherwise =
            let beta' = sweep beta
                diff  = LA.norm_2 (beta' - beta)
            in if diff < tol then (beta', k)
                 else iterate' (k + 1) beta'
      (betaFinal, iters) = iterate' 0 beta0
      yHat = x LA.#> betaFinal
      r    = y - yHat
  in mkRegFit betaFinal yHat r y (ElasticNet lambda1 lambda2) iters

-- ---------------------------------------------------------------------------
-- 共通ヘルパ
-- ---------------------------------------------------------------------------

mkRegFit :: LA.Vector Double -> LA.Vector Double -> LA.Vector Double
         -> LA.Vector Double -> Penalty -> Int -> RegFit
mkRegFit beta yHat r y pen iters =
  let mu   = LA.sumElements y / fromIntegral (LA.size y)
      ssT  = LA.sumElements ((y - LA.scalar mu) ^ (2 :: Int))
      ssR  = LA.sumElements (r ^ (2 :: Int))
      r2   = if ssT == 0 then 0 else 1 - ssR / ssT
      nz   = length [v | v <- LA.toList beta, abs v > 1e-8]
  in RegFit beta yHat r r2 pen nz iters

-- ---------------------------------------------------------------------------
-- Standardization
-- ---------------------------------------------------------------------------

-- | 各列を mean=0, sd=1 に標準化。
-- 戻り値: (標準化済 X, 列平均, 列 sd)。元データに戻すには
-- @X_std = (X - μ) / σ@、係数を元スケールに戻すには 'unstandardizeBeta' を使う。
standardize :: LA.Matrix Double
            -> (LA.Matrix Double, V.Vector Double, V.Vector Double)
standardize x =
  let n     = LA.rows x
      p     = LA.cols x
      means = V.fromList
        [ LA.sumElements (LA.flatten (x LA.¿ [j])) / fromIntegral n
        | j <- [0 .. p - 1] ]
      sds   = V.fromList
        [ let c   = LA.flatten (x LA.¿ [j])
              mu  = means V.! j
              var = LA.sumElements ((c - LA.scalar mu) ^ (2 :: Int))
                    / fromIntegral (n - 1)
          in sqrt var
        | j <- [0 .. p - 1] ]
      cols' = [ let c   = LA.flatten (x LA.¿ [j])
                    mu  = means V.! j
                    sd  = sds V.! j
                in (c - LA.scalar mu) / LA.scalar (if sd == 0 then 1 else sd)
              | j <- [0 .. p - 1] ]
      xStd  = LA.fromColumns cols'
  in (xStd, means, sds)

-- | 標準化空間で fit した β を元スケールに戻す。
-- β_orig_j = β_std_j / σ_j、切片はモデル外で別途調整。
unstandardizeBeta :: V.Vector Double -> LA.Vector Double -> LA.Vector Double
unstandardizeBeta sds betaStd =
  let p = LA.size betaStd
  in LA.fromList
       [ (betaStd `LA.atIndex` j) / (sds V.! j)
       | j <- [0 .. p - 1] ]
