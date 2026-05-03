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
    -- * 多出力 (主 API)
  , RegFitMulti (..)
  , fitRegularizedMulti
  , predictRegularizedMulti
  , regFitFromMulti
    -- * 正則化パス
  , regularizationPath
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

-- | ペナルティ付き回帰を fit (単出力)。
-- 多出力 'fitRegularizedMulti' に Y を 1 列行列化して委譲し、
-- 結果の列 0 を 'RegFit' として返す。
fitRegularized :: Penalty -> LA.Matrix Double -> LA.Vector Double -> RegFit
fitRegularized pen x y =
  regFitFromMulti 0 (fitRegularizedMulti pen x (LA.asColumn y))

-- | 予測 (単出力)。
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

-- ---------------------------------------------------------------------------
-- 多出力対応 (主 API)
-- ---------------------------------------------------------------------------

-- | 多出力正則化回帰のフィット結果。
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

-- | 多出力正則化回帰: Y は n × q。
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

predictRegularizedMulti :: RegFitMulti -> LA.Matrix Double -> LA.Matrix Double
predictRegularizedMulti mf xNew = xNew LA.<> rfmBeta mf

-- | RegFitMulti の j 列目を 'RegFit' として取り出す。
regFitFromMulti :: Int -> RegFitMulti -> RegFit
regFitFromMulti j mf
  | j < length (rfmFits mf) = rfmFits mf !! j
  | otherwise = error ("regFitFromMulti: column " ++ show j ++ " out of range")

-- | 行列形式の OLS: B = X \\ Y (LAPACK 1 回)。
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

