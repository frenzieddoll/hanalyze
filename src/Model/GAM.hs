{-# LANGUAGE OverloadedStrings #-}
-- | Generalized Additive Model (GAM).
--
-- @y = β₀ + Σ_j s_j(x_j) + ε@ where each smooth term @s_j(x_j) = B_j(x_j) γ_j@
-- uses a B-spline basis.
--
-- Design:
--
--   * For each predictor @x_j@, build a B-spline basis @B_j@ (@n × m_j@).
--   * Stack into a single design matrix
--     @X = [1 | B_1 | B_2 | ... | B_p]@ (@1 + Σ m_j@ columns).
--   * Ridge-regularized OLS:
--     @β = (XᵀX + λ I)⁻¹ Xᵀ y@. The same @λ@ stabilizes every spline
--     basis (smoothness regularization).
--   * Prediction: the per-feature contribution @s_j(x_j)@ can be extracted
--     individually for visualization of each factor's effect.
--
-- 注: 識別性のため、各 spline 基底は中央化 (列平均を引く) する。
-- これで β₀ は y の平均、s_j は変動成分のみを表す。
module Model.GAM
  ( GAMFit (..)
  , fitGAM
  , predictGAM
  , predictGAMComponent
  ) where

import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import Model.Spline (bsplineBasis)

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | GAM fit result.
data GAMFit = GAMFit
  { gamDegree    :: Int                  -- ^ B-spline degree.
  , gamKnots     :: [[Double]]           -- ^ Per-feature interior knots.
  , gamBetas     :: [LA.Vector Double]   -- ^ Per-feature spline coefficients @γ_j@.
  , gamColMeans  :: [LA.Vector Double]   -- ^ Per-feature column means of @B_j@ (for centering).
  , gamIntercept :: Double               -- ^ Intercept @β₀@.
  , gamYHat      :: LA.Vector Double     -- ^ Fitted values.
  , gamResid     :: LA.Vector Double     -- ^ Residuals.
  , gamR2        :: Double               -- ^ R².
  , gamLambda    :: Double               -- ^ Ridge penalty @λ@ used.
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- フィット
-- ---------------------------------------------------------------------------

-- | Fit a GAM.
fitGAM :: Int                    -- ^ B-spline degree (3 = cubic recommended).
       -> Int                    -- ^ Number of interior knots (e.g. 5).
       -> Double                 -- ^ Ridge penalty @λ@ (0 disables regularization).
       -> [V.Vector Double]      -- ^ Predictors @[x₁, x₂, …]@.
       -> V.Vector Double        -- ^ Response @y@.
       -> GAMFit
fitGAM degree nKnots lambda xss y =
  let n         = V.length y
      -- 各列のノット (両端含めて nKnots+2 点、等間隔)
      mkKnots xs =
        let lo = V.minimum xs
            hi = V.maximum xs
            -- 両端 + 内部 nKnots 点 = nKnots + 2 個 (B-spline には clamping)
            step = (hi - lo) / fromIntegral (nKnots + 1)
        in [ lo + fromIntegral i * step | i <- [0 .. nKnots + 1] ]
      knotsList = map mkKnots xss

      -- 各 B_j (n × m_j) を構築 + 列平均で中央化
      basisRaw = zipWith (\k xs -> bsplineBasis degree k xs) knotsList xss
      colMeans = [ LA.fromList
                     [ LA.sumElements (LA.flatten (b LA.¿ [j])) / fromIntegral n
                     | j <- [0 .. LA.cols b - 1] ]
                 | b <- basisRaw ]
      basisCent = zipWith centerCols basisRaw colMeans

      -- 統合計画行列 X = [1 | B_1 | B_2 | ...]
      ones = LA.asColumn (LA.konst 1 n)
      x    = foldl1 (LA.|||) (ones : basisCent)
      yLA  = LA.fromList (V.toList y)
      p    = LA.cols x

      -- Ridge: β = (XᵀX + λ I')⁻¹ Xᵀ y  (intercept 列はペナルティ免除)
      pen  = LA.diag (LA.fromList (0 : replicate (p - 1) lambda))
      xtx  = LA.tr x LA.<> x + pen
      xty  = LA.tr x LA.#> yLA
      beta = LA.flatten (xtx LA.<\> LA.asColumn xty)

      -- intercept = β[0]、各特徴の γ_j を切り出す
      mSizes = [ LA.cols b | b <- basisRaw ]
      starts = scanl (+) 1 mSizes        -- intercept は 0
      betas  = [ LA.subVector (starts !! j) (mSizes !! j) beta
               | j <- [0 .. length xss - 1] ]
      intercept = beta LA.! 0

      yhat  = x LA.#> beta
      resid = yLA - yhat
      yMean = LA.sumElements yLA / fromIntegral n
      tss   = LA.sumElements (LA.cmap (\v -> (v - yMean) ^ (2 :: Int)) yLA)
      rss   = LA.sumElements (LA.cmap (^ (2 :: Int)) resid)
      r2    = if tss < 1e-12 then 0 else 1 - rss / tss
  in GAMFit
       { gamDegree    = degree
       , gamKnots     = knotsList
       , gamBetas     = betas
       , gamColMeans  = colMeans
       , gamIntercept = intercept
       , gamYHat      = yhat
       , gamResid     = resid
       , gamR2        = r2
       , gamLambda    = lambda
       }
  where
    -- 列平均を引いて中央化
    centerCols :: LA.Matrix Double -> LA.Vector Double -> LA.Matrix Double
    centerCols m mu =
      let cols = LA.toColumns m
          centered = zipWith (\c muVal -> LA.cmap (\v -> v - muVal) c)
                       cols (LA.toList mu)
      in LA.fromColumns centered

-- ---------------------------------------------------------------------------
-- 予測
-- ---------------------------------------------------------------------------

-- | Predict at new predictors.
predictGAM :: GAMFit -> [V.Vector Double] -> V.Vector Double
predictGAM fit xss =
  let n = if null xss then 0 else V.length (head xss)
      contributions = zipWith3 (componentVec fit)
                        [0 .. length xss - 1]
                        xss
                        (gamColMeans fit)
      total = foldl' (V.zipWith (+)) (V.replicate n (gamIntercept fit))
                contributions
  in total
  where
    foldl' f z [] = z
    foldl' f z (x:xs) = let !z' = f z x in foldl' f z' xs
    componentVec :: GAMFit -> Int -> V.Vector Double -> LA.Vector Double
                 -> V.Vector Double
    componentVec g j xs mu =
      let b      = bsplineBasis (gamDegree g) (gamKnots g !! j) xs
          gamma  = gamBetas g !! j
          n'     = LA.rows b
          ys     = b LA.#> gamma
          shiftV = LA.dot mu gamma
      in V.fromList [ ys LA.! i - shiftV | i <- [0 .. n' - 1] ]

-- | The contribution @s_j(x)@ from feature @j@ only (without the intercept).
predictGAMComponent :: GAMFit -> Int -> V.Vector Double -> V.Vector Double
predictGAMComponent fit j xs
  | j < 0 || j >= length (gamBetas fit) = V.empty
  | otherwise =
      let b      = bsplineBasis (gamDegree fit) (gamKnots fit !! j) xs
          gamma  = gamBetas fit !! j
          mu     = gamColMeans fit !! j
          ys     = b LA.#> gamma
          shiftV = LA.dot mu gamma
          n      = LA.rows b
      in V.fromList [ ys LA.! i - shiftV | i <- [0 .. n - 1] ]
