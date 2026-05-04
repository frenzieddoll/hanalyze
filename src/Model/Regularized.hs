{-# LANGUAGE OverloadedStrings #-}
-- | Regularized regression (Ridge / Lasso / Elastic Net) in one module.
--
-- The penalty is encoded as the sum type 'Penalty', and 'fitRegularized'
-- handles all four models:
--
-- > NoPen                          -- ordinary OLS
-- > L2 lambda                      -- Ridge regression
-- > L1 lambda                      -- Lasso regression
-- > ElasticNet lambda1 lambda2     -- Elastic Net (L1 + L2)
--
-- Ridge has a closed form; Lasso and Elastic Net use coordinate descent.
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
    -- * Multi-output (primary API)
  , RegFitMulti (..)
  , fitRegularizedMulti
  , predictRegularizedMulti
  , regFitFromMulti
    -- * Regularization path
  , regularizationPath
  ) where

import qualified Data.Vector                  as V
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Numeric.LinearAlgebra        as LA
import           Control.Monad                (forM_, when)
import           Data.List                    (foldl')
import           System.IO.Unsafe             (unsafePerformIO)

-- ---------------------------------------------------------------------------
-- ペナルティ型
-- ---------------------------------------------------------------------------

-- | Regularization penalty.
data Penalty
  = NoPen                       -- ^ Ordinary OLS (@λ = 0@).
  | L2 Double                   -- ^ Ridge: @0.5 λ ‖β‖₂²@.
  | L1 Double                   -- ^ Lasso: @λ ‖β‖₁@.
  | ElasticNet Double Double    -- ^ Elastic Net: @λ₁ ‖β‖₁ + 0.5 λ₂ ‖β‖₂²@.
  deriving (Show, Eq)

-- | Regularized-regression fit result.
data RegFit = RegFit
  { rfBeta    :: LA.Vector Double
  , rfYHat    :: LA.Vector Double
  , rfResid   :: LA.Vector Double
  , rfR2      :: Double
  , rfPenalty :: Penalty
  , rfNonZero :: Int           -- ^ Number of @|β_j| > 1e-8@ (Lasso sparsity).
  , rfIters   :: Int           -- ^ Iteration count (coordinate descent;
                               --   0 for closed-form solvers).
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- メイン API
-- ---------------------------------------------------------------------------

-- | Single-output regularized-regression fit. Delegates to
-- 'fitRegularizedMulti' by promoting @y@ to a one-column matrix and
-- returns column 0 as a 'RegFit'.
fitRegularized :: Penalty -> LA.Matrix Double -> LA.Vector Double -> RegFit
fitRegularized pen x y =
  regFitFromMulti 0 (fitRegularizedMulti pen x (LA.asColumn y))

-- | Single-output prediction.
predictRegularized :: RegFit -> LA.Matrix Double -> LA.Vector Double
predictRegularized fit xNew = xNew LA.#> rfBeta fit

-- ---------------------------------------------------------------------------
-- OLS (NoPen)
-- ---------------------------------------------------------------------------

-- | Plain ordinary-least-squares fit (no penalty).
fitOLS :: LA.Matrix Double -> LA.Vector Double -> RegFit
fitOLS x y =
  let beta = LA.flatten (x LA.<\> LA.asColumn y)
      yHat = x LA.#> beta
      r    = y - yHat
  in mkRegFit beta yHat r y NoPen 0

-- ---------------------------------------------------------------------------
-- Ridge (closed form)
-- ---------------------------------------------------------------------------

-- | Ridge regression: @β = (XᵀX + λI)⁻¹ Xᵀy@.
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

-- | Soft-threshold operator: @S(z, γ) = sign(z) × max(|z| − γ, 0)@.
softThreshold :: Double -> Double -> Double
softThreshold z gamma
  | z >  gamma = z - gamma
  | z < -gamma = z + gamma
  | otherwise  = 0

-- | Lasso regression: @β = argmin (1/2n) ‖y − Xβ‖² + λ ‖β‖₁@.
--
-- Solved by coordinate descent (one update per @β_j@):
--
-- @
-- r   = y − X β
-- ρ_j = (1/n) X_jᵀ r + β_j × (1/n) ‖X_j‖²
-- β_j ← S(ρ_j, λ) / ((1/n) ‖X_j‖²)
-- @
fitLasso :: Double                -- ^ Penalty @λ@.
         -> LA.Matrix Double      -- ^ Design matrix @X@.
         -> LA.Vector Double      -- ^ Response @y@.
         -> Int                   -- ^ Maximum CD iterations.
         -> Double                -- ^ Convergence tolerance.
         -> RegFit
fitLasso lambda x y maxIter tol =
  let (betaFinal, iters) = cdLoop x y maxIter tol
                             (\rho cSq -> softThreshold rho lambda / cSq)
      yHat = x LA.#> betaFinal
      r    = y - yHat
  in mkRegFit betaFinal yHat r y (L1 lambda) iters

-- ---------------------------------------------------------------------------
-- Elastic Net (Coordinate Descent)
-- ---------------------------------------------------------------------------

-- | Elastic-Net regression:
-- @β = argmin (1/2n) ‖y − Xβ‖² + λ₁ ‖β‖₁ + 0.5 λ₂ ‖β‖²@.
--
-- Coordinate descent update:
-- @β_j ← S(ρ_j, λ₁) / ((1/n) ‖X_j‖² + λ₂)@.
fitElasticNet :: Double -> Double -> LA.Matrix Double -> LA.Vector Double
              -> Int -> Double -> RegFit
fitElasticNet lambda1 lambda2 x y maxIter tol =
  let (betaFinal, iters) = cdLoop x y maxIter tol
                             (\rho cSq -> softThreshold rho lambda1
                                          / (cSq + lambda2))
      yHat = x LA.#> betaFinal
      r    = y - yHat
  in mkRegFit betaFinal yHat r y (ElasticNet lambda1 lambda2) iters

-- ---------------------------------------------------------------------------
-- Shared CD loop with incremental residual maintenance
-- ---------------------------------------------------------------------------

-- | Coordinate descent loop shared by 'fitLasso' and 'fitElasticNet'.
--
-- The caller supplies a /closed-form coordinate update/ @upd ρ_j cSq_j@
-- that returns @β_j_new@ given the partial-residual correlation @ρ_j@
-- and the column-norm @cSq_j = ‖X_j‖²/n@.
--
-- Implementation (R2): the inner sweep runs in 'IO' on
-- 'Data.Vector.Storable.Mutable' buffers. Both @β@ and the residual
-- @r = y − Xβ@ are updated in place, and the columns of @X@ are looked
-- up through a boxed 'Data.Vector.Vector' for @O(1)@ indexing (the
-- previous list-based @cols !! j@ paid @O(p)@ per coordinate). This is
-- the moral equivalent of sklearn's Cython coordinate-descent inner
-- loop; the user-visible behaviour is identical to the prior Vector
-- implementation up to floating-point rounding.
cdLoop
  :: LA.Matrix Double                  -- X (n × p)
  -> LA.Vector Double                  -- y
  -> Int                               -- max iterations
  -> Double                            -- tolerance on |Δβ|₂
  -> (Double -> Double -> Double)      -- (ρ, cSq) → β_j_new
  -> (LA.Vector Double, Int)
cdLoop x y maxIter tol upd = unsafePerformIO $ do
  let n      = fromIntegral (LA.rows x) :: Double
      p      = LA.cols x
      colsB  = V.fromList (LA.toColumns x)        -- O(1) indexing
      colSqN = LA.fromList
                 [ LA.sumElements (c * c) / n | c <- LA.toColumns x ]

  -- Mutable buffer for β (single-index updates each coordinate step).
  bMut <- VS.thaw (LA.konst 0 p :: LA.Vector Double)

  -- The residual r is kept as an /immutable/ 'LA.Vector Double' between
  -- coordinate updates so that @r ← r − d · x_j@ can use BLAS axpy
  -- (a single optimized call) rather than a per-element Haskell loop.
  let sweep r = do
        beforeSnap <- VS.freeze bMut
        let stepCoord rCur j = do
              let xj  = colsB V.! j
                  cSq = colSqN `LA.atIndex` j
              bjOld <- VSM.unsafeRead bMut j
              let rho   = (xj LA.<.> rCur) / n + bjOld * cSq
                  bjNew = upd rho cSq
                  d     = bjNew - bjOld
              if d == 0
                then return rCur
                else do
                  VSM.unsafeWrite bMut j bjNew
                  -- BLAS axpy: r' = r - d * x_j
                  return (rCur - LA.scale d xj)
        rEnd <- foldM' stepCoord r [0 .. p - 1]
        afterSnap <- VS.freeze bMut
        return (beforeSnap, afterSnap, rEnd)

  let go k r = do
        if k >= maxIter
          then return k
          else do
            (before, after, r') <- sweep r
            let diff = LA.norm_2 (after - before)
            if diff < tol then return (k + 1) else go (k + 1) r'

  iters     <- go 0 y     -- initial residual = y (since β₀ = 0)
  betaFinal <- VS.freeze bMut
  return (betaFinal, iters)
  where
    -- Strict foldM that discards no intermediate results (folds an
    -- accumulator @r@ through @f@).
    foldM' :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
    foldM' _ acc []     = return acc
    foldM' f acc (z:zs) = do
      acc' <- f acc z
      acc' `seq` foldM' f acc' zs

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

-- | Standardize each column to mean 0 and standard deviation 1.
--
-- Returns @(X_std, column means, column sds)@. The transformation is
-- @X_std = (X − μ) / σ@; use 'unstandardizeBeta' to map coefficients
-- back to the original scale.
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

-- | Map coefficients fitted in standardized space back to the original
-- scale: @β_orig_j = β_std_j / σ_j@. The intercept must be adjusted
-- separately, outside this helper.
unstandardizeBeta :: V.Vector Double -> LA.Vector Double -> LA.Vector Double
unstandardizeBeta sds betaStd =
  let p = LA.size betaStd
  in LA.fromList
       [ (betaStd `LA.atIndex` j) / (sds V.! j)
       | j <- [0 .. p - 1] ]

-- ---------------------------------------------------------------------------
-- 多出力対応 (主 API)
-- ---------------------------------------------------------------------------

-- | Multi-output regularized-regression fit result.
-- Y は n × q、係数 B は p × q、予測 Ŷ = X B。
-- 'rfmFits' は列ごとの単出力 'RegFit' (R²、|β|>0 の数、反復回数を提供)。
data RegFitMulti = RegFitMulti
  { rfmFits     :: [RegFit]            -- ^ 列ごとの単出力 fit
  , rfmBeta     :: LA.Matrix Double    -- ^ p × q
  , rfmYHat     :: LA.Matrix Double    -- ^ n × q
  , rfmResid    :: LA.Matrix Double    -- ^ n × q
  , rfmR2       :: [Double]            -- ^ 列ごとの R²
  , rfmPenalty  :: Penalty
  } deriving (Show)

-- | Multi-output regularized regression. @Y@ has shape @n × q@.
--
-- - OLS / Ridge: 行列形式 1 回の線形求解で全 q 列を一括処理 (高速)。
-- - Lasso / Elastic Net: 列ごと座標降下 (列間に依存なし、独立並列可)。
fitRegularizedMulti :: Penalty -> LA.Matrix Double -> LA.Matrix Double
                    -> RegFitMulti
fitRegularizedMulti pen x y = case pen of
  NoPen        -> fitOLSMulti x y
  L2 lambda    -> fitRidgeMulti lambda x y
  L1 lambda    -> fitColumnwise (fitLasso lambda) pen x y
  ElasticNet l1 l2 -> fitColumnwise (fitElasticNet l1 l2) pen x y

-- | Multi-output prediction.
predictRegularizedMulti :: RegFitMulti -> LA.Matrix Double -> LA.Matrix Double
predictRegularizedMulti mf xNew = xNew LA.<> rfmBeta mf

-- | Extract column @j@ of a 'RegFitMulti' as a 'RegFit'.
regFitFromMulti :: Int -> RegFitMulti -> RegFit
regFitFromMulti j mf
  | j < length (rfmFits mf) = rfmFits mf !! j
  | otherwise = error ("regFitFromMulti: column " ++ show j ++ " out of range")

-- | Matrix-form OLS: @B = X \\ Y@ in a single LAPACK call.
fitOLSMulti :: LA.Matrix Double -> LA.Matrix Double -> RegFitMulti
fitOLSMulti x y =
  let beta = x LA.<\> y
  in mkRegFitMulti beta x y NoPen (replicate (LA.cols y) 0)

-- | 行列形式の Ridge: B = (XᵀX + λI)⁻¹ XᵀY (1 回の Cholesky/LU)。
fitRidgeMulti :: Double -> LA.Matrix Double -> LA.Matrix Double -> RegFitMulti
fitRidgeMulti lambda x y =
  let p    = LA.cols x
      reg  = LA.tr x LA.<> x + LA.scale lambda (LA.ident p)
      xty  = LA.tr x LA.<> y
      beta = reg LA.<\> xty
  in mkRegFitMulti beta x y (L2 lambda) (replicate (LA.cols y) 0)

-- | 列ごと CD (Lasso / Elastic Net 用)。
fitColumnwise
  :: (LA.Matrix Double -> LA.Vector Double -> Int -> Double -> RegFit)
  -> Penalty
  -> LA.Matrix Double -> LA.Matrix Double
  -> RegFitMulti
fitColumnwise fitCol pen x y =
  let q     = LA.cols y
      fits  = [ fitCol x (LA.flatten (y LA.¿ [j])) 1000 1e-7
              | j <- [0 .. q - 1] ]
      bMat  = LA.fromColumns [rfBeta f | f <- fits]
      yHat  = LA.fromColumns [rfYHat f | f <- fits]
      res   = LA.fromColumns [rfResid f | f <- fits]
      r2s   = [rfR2 f | f <- fits]
  in RegFitMulti fits bMat yHat res r2s pen

-- | 共通: B 行列から RegFitMulti を組み立て。各列の R² と非零係数数も計算。
mkRegFitMulti :: LA.Matrix Double -> LA.Matrix Double -> LA.Matrix Double
              -> Penalty -> [Int] -> RegFitMulti
mkRegFitMulti beta x y pen iters =
  let yHat  = x LA.<> beta
      res   = y - yHat
      q     = LA.cols y
      colFit j =
        let b   = LA.flatten (beta LA.¿ [j])
            yh  = LA.flatten (yHat LA.¿ [j])
            rj  = LA.flatten (res LA.¿ [j])
            yj  = LA.flatten (y LA.¿ [j])
        in mkRegFit b yh rj yj pen (iters !! j)
      fits  = [colFit j | j <- [0 .. q - 1]]
  in RegFitMulti fits beta yHat res [rfR2 f | f <- fits] pen

-- ---------------------------------------------------------------------------
-- Regularization path
-- ---------------------------------------------------------------------------

-- | 与えられた λ の系列に対して係数推移を計算する (regularization path)。
-- 戻り値: 各 λ に対する係数ベクトル。
--
-- 利用例 (Ridge):
--
-- @
-- let lams = [10 ** (-4 + 0.1 * i) | i <- [0..60]]
--     path = regularizationPath L2 lams xMat yVec
-- -- path :: [(Double, [Double])]  -- (λ, [β₀, β₁, ...])
-- @
regularizationPath
  :: (Double -> Penalty)         -- ^ λ → Penalty (e.g. @L2@, @L1@,
                                 --   @\\l -> ElasticNet (l*α) (l*(1-α))@)
  -> [Double]                    -- ^ λ 系列
  -> LA.Matrix Double            -- ^ X (intercept 列付き)
  -> LA.Vector Double            -- ^ y
  -> [(Double, [Double])]        -- ^ [(λ, 係数ベクトル)]
regularizationPath mkPen lambdas x y =
  [ (lam, LA.toList (rfBeta (fitRegularized (mkPen lam) x y)))
  | lam <- lambdas ]

