{-# LANGUAGE OverloadedStrings #-}
-- | Quantile regression.
--
-- Whereas OLS fits the conditional /mean/, quantile regression fits the
-- conditional @τ@-quantile (with @τ ∈ (0, 1)@). @τ = 0.5@ gives outlier-
-- robust median regression; @τ = 0.1 / 0.9@ estimate lower / upper
-- quantiles, useful for predictive intervals and heteroscedastic data.
--
-- Loss function (pinball / check loss):
--
-- > ρ_τ(u) = u (τ - 𝟙[u < 0])  =  τ u       if u ≥ 0
-- >                               (τ-1) u   if u < 0
--
-- Algorithm: Hunter & Lange (2000) Majorization-Minimization. Locally
-- approximate @|u|@ by a quadratic and iterate weighted least squares:
--
-- 1. β₀ = OLS 解で初期化
-- 2. 反復 k:
--    - r = y - X β_k
--    - w_i = 1 / (2 max(|r_i|, ε))
--    - y'_i = y_i + (τ - ½) / w_i
--    - β_{k+1} = (Xᵀ W X)⁻¹ Xᵀ W y'
-- 3. ||β_{k+1} - β_k|| < tol で停止 (max 100 iter)。
--
-- 評価指標 (Koenker-Machado 1999): R¹_τ = 1 - V̂_τ(model) / V̂_τ(intercept-only)
-- where V̂_τ(m) = Σ ρ_τ(r_i^m)。
module Hanalyze.Model.Quantile
  ( QRFit (..)
  , fitQuantile
  , predictQuantile
  , pinballLoss
  , pseudoR1
  ) where

import qualified Numeric.LinearAlgebra as LA
import qualified Hanalyze.Stat.Cholesky        as Chol

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | Quantile-regression fit result.
data QRFit = QRFit
  { qfTau     :: Double            -- ^ Quantile level @τ ∈ (0, 1)@.
  , qfBeta    :: LA.Vector Double  -- ^ Coefficients.
  , qfYHat    :: LA.Vector Double  -- ^ Fitted values @X β@.
  , qfResid   :: LA.Vector Double  -- ^ Residuals @y − X β@.
  , qfPinball :: Double            -- ^ Total pinball loss @V̂_τ@.
  , qfR1      :: Double            -- ^ Koenker-Machado pseudo @R¹_τ@.
  , qfIters   :: Int               -- ^ Number of iterations executed.
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- フィット
-- ---------------------------------------------------------------------------

-- | Fit a @τ@-quantile regression by Majorization-Minimization IRLS.
fitQuantile :: Double             -- ^ Quantile level @τ ∈ (0, 1)@.
            -> LA.Matrix Double   -- ^ Design matrix @X@ (must include the intercept column).
            -> LA.Vector Double   -- ^ Response @y@.
            -> QRFit
fitQuantile tau x y
  | tau <= 0 || tau >= 1 = error "fitQuantile: tau must be in (0, 1)"
  | otherwise =
      let !beta0      = x LA.<\> y         -- OLS 初期値
          !eps        = 1e-6
          !maxIter    = 100 :: Int
          !tol        = 1e-7
          !p          = LA.cols x
          !onesP      = LA.konst 1 p :: LA.Vector Double
          (betaF, k)  = loop beta0 0
          loop b iter
            | iter >= maxIter = (b, iter)
            | otherwise =
                let !r    = y - x LA.#> b
                    -- w_i = 1 / (2 max(|r_i|, eps))
                    !wVec = LA.cmap (\v -> 1 / (2 * max eps (abs v))) r
                    -- y' = y + (tau - 0.5) / w
                    !yp   = y + LA.cmap (\wi -> (tau - 0.5) / wi) wVec
                    -- W^{1/2}.
                    !sqW  = LA.cmap sqrt wVec
                    -- B10a (2026-05-06): row-scaling of X via outer
                    -- product (broadcast sqW across columns) instead
                    -- of the previous "@LA.toRows x !! i@" + "@diag@"
                    -- combination, which was @O(n² p)@ per iteration
                    -- (76× slower than statsmodels on n=10k p=20).
                    -- Now @O(n p)@ per iteration — single elementwise
                    -- multiply with a fully-allocated outer product.
                    !sqWBcast = LA.outer sqW onesP   -- n × p
                    !xScaled  = sqWBcast * x         -- n × p
                    !yScaled  = sqW * yp             -- length n
                    -- Solve the SPD normal equations
                    --   (X^T W X) β = X^T W y'
                    -- via Cholesky rather than the general LSQ path
                    -- '@LA.<\>@' (QR/dgels). For @p ≪ n@ the @p × p@
                    -- @aMat@ is tiny and dpotrf is faster than dgels
                    -- on the @n × p@ @xScaled@ matrix; this is the
                    -- same trick GLM IRLS already uses.
                    !aMat     = LA.tr xScaled LA.<> xScaled
                    !rhs      = LA.asColumn (LA.tr xScaled LA.#> yScaled)
                    !bNew     = LA.flatten (Chol.cholSolveJitter aMat rhs)
                    !delta    = LA.norm_2 (bNew - b)
                in if delta < tol then (bNew, iter + 1)
                                  else loop bNew (iter + 1)
          yhat = x LA.#> betaF
          resid = y - yhat
          loss  = pinballLoss tau (LA.toList resid)
          -- baseline: intercept-only model with τ-quantile of y
          ys    = LA.toList y
          baseQ = quantile tau ys
          baseR = [ yi - baseQ | yi <- ys ]
          baseLoss = pinballLoss tau baseR
          r1 = if baseLoss <= 1e-12 then 0
               else 1 - loss / baseLoss
      in QRFit
           { qfTau     = tau
           , qfBeta    = betaF
           , qfYHat    = yhat
           , qfResid   = resid
           , qfPinball = loss
           , qfR1      = r1
           , qfIters   = k
           }

-- | Predict at new inputs.
predictQuantile :: QRFit -> LA.Matrix Double -> LA.Vector Double
predictQuantile fit xNew = xNew LA.#> qfBeta fit

-- ---------------------------------------------------------------------------
-- 補助関数
-- ---------------------------------------------------------------------------

-- | Total pinball / check loss: @Σ ρ_τ(r_i)@.
pinballLoss :: Double -> [Double] -> Double
pinballLoss tau rs =
  sum [ if r >= 0 then tau * r else (tau - 1) * r | r <- rs ]

-- | Empirical @τ@-quantile (simple linear-interpolation style).
quantile :: Double -> [Double] -> Double
quantile p xs
  | null xs = 0
  | otherwise =
      let sorted = qSort xs
          n      = length sorted
          ix     = p * fromIntegral (n - 1)
          lo     = floor ix :: Int
          hi     = min (n - 1) (lo + 1)
          frac   = ix - fromIntegral lo
      in (1 - frac) * (sorted !! lo) + frac * (sorted !! hi)

qSort :: [Double] -> [Double]
qSort []     = []
qSort (p:xs) = qSort [x | x <- xs, x <= p]
            ++ [p]
            ++ qSort [x | x <- xs, x > p]

-- | Pseudo R¹_τ を別途計算 (model loss と baseline loss から)。
pseudoR1 :: Double            -- ^ model V̂_τ
         -> Double            -- ^ baseline (intercept-only) V̂_τ
         -> Double
pseudoR1 modelV baseV
  | baseV <= 1e-12 = 0
  | otherwise      = 1 - modelV / baseV
