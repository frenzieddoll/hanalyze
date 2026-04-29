{-# LANGUAGE OverloadedStrings #-}
module Model.GLM
  ( Family (..)
  , parseFamily
  , fitGLM
  , fitGLMWithSmooth
  ) where

import DataFrame.Core
import Model.Core
import Model.LM (polyDesignMatrix, linspace, SmoothFit (..))

import Data.Text (Text)
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import Statistics.Distribution (quantile)
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution.StudentT (studentT)

-- ---------------------------------------------------------------------------
-- Family
-- ---------------------------------------------------------------------------

-- | GLM family: encodes both the response distribution and its canonical link.
data Family = Gaussian | Binomial | Poisson
  deriving (Show, Eq)

parseFamily :: String -> Either String Family
parseFamily "gaussian" = Right Gaussian
parseFamily "binomial" = Right Binomial
parseFamily "poisson"  = Right Poisson
parseFamily s          = Left ("Unknown family '" ++ s ++ "'. Use: gaussian | binomial | poisson")

-- ---------------------------------------------------------------------------
-- Link and variance functions
-- ---------------------------------------------------------------------------

-- (g, g⁻¹, g')
type Link = (Double -> Double, Double -> Double, Double -> Double)

linkOf :: Family -> Link
linkOf Gaussian = (id, id, const 1.0)
linkOf Binomial = ( \x -> log (x / (1 - x))
                  , \x -> 1 / (1 + exp (-x))
                  , \mu -> 1.0 / (mu * (1 - mu))
                  )
linkOf Poisson  = (log, exp, recip)

varOf :: Family -> Double -> Double
varOf Gaussian _  = 1.0
varOf Binomial mu = mu * (1 - mu)
varOf Poisson  mu = mu

-- Clamp μ to avoid log(0) or division by zero
safeMu :: Family -> LA.Vector Double -> LA.Vector Double
safeMu Binomial = LA.cmap (max 1e-8 . min (1 - 1e-8))
safeMu Poisson  = LA.cmap (max 1e-8)
safeMu Gaussian = id

-- ---------------------------------------------------------------------------
-- IRLS
-- ---------------------------------------------------------------------------

maxIter :: Int
maxIter = 100

tol :: Double
tol = 1e-8

-- Initial β: intercept = g(ȳ), other terms = 0
initBeta :: Family -> LA.Vector Double -> Int -> LA.Vector Double
initBeta family y p =
  let (g, _, _) = linkOf family
      yMean = LA.sumElements y / fromIntegral (LA.size y)
      yC = case family of
             Binomial -> max 1e-6 (min (1 - 1e-6) yMean)
             Poisson  -> max 1e-6 yMean
             Gaussian -> yMean
  in LA.fromList (g yC : replicate (p - 1) 0.0)

-- One IRLS step via W^{1/2} transform (numerically stable QR solve)
irlsStep :: Link -> (Double -> Double) -> (Family -> LA.Vector Double -> LA.Vector Double)
          -> Family -> LA.Matrix Double -> LA.Vector Double -> LA.Vector Double
          -> LA.Vector Double
irlsStep (_, gInv, gDeriv) varFn clamp family x y beta =
  let eta  = x LA.#> beta
      mu   = clamp family (LA.cmap gInv eta)
      muL  = LA.toList mu
      etaL = LA.toList eta
      yL   = LA.toList y
      -- IRLS weights: wᵢ = 1 / (g'(μᵢ)² × Var(μᵢ))
      ws   = LA.fromList [ max 1e-10 (1.0 / (gDeriv m ^ (2::Int) * varFn m)) | m <- muL ]
      -- Adjusted dependent variable: zᵢ = ηᵢ + (yᵢ − μᵢ) × g'(μᵢ)
      zs   = LA.fromList [ ei + (yi - mi) * gDeriv mi | (ei,yi,mi) <- zip3 etaL yL muL ]
      sqrtW = LA.diag (LA.cmap sqrt ws)
  in LA.flatten ((sqrtW LA.<> x) LA.<\> LA.asColumn (sqrtW LA.#> zs))

-- Run IRLS until convergence; also returns (XᵀWX)⁻¹ for CI computation
runIRLS :: Family -> LA.Matrix Double -> LA.Vector Double
         -> (FitResult, LA.Matrix Double)
runIRLS family x y = (mkResult betaFinal, fisherInv betaFinal)
  where
    step  = irlsStep (linkOf family) (varOf family) safeMu family x y
    beta0 = initBeta family y (LA.cols x)

    betaFinal = converge maxIter beta0

    converge 0 beta = beta
    converge n beta =
      let betaNew = step beta
      in if LA.norm_2 (betaNew - beta) < tol || any notFinite (LA.toList betaNew)
         then betaNew
         else converge (n - 1) betaNew

    notFinite b = isNaN b || isInfinite b

    mkResult beta =
      let (_, gInv, _) = linkOf family
          mu    = safeMu family (LA.cmap gInv (x LA.#> beta))
          resid = y - mu
          r2    = pseudoR2 family y mu
      in FitResult beta mu resid r2

    -- (XᵀWX)⁻¹ from converged β — used for CI
    fisherInv beta =
      let (_, gInv, gDeriv) = linkOf family
          mu  = safeMu family (LA.cmap gInv (x LA.#> beta))
          muL = LA.toList mu
          ws  = LA.fromList [ max 1e-10 (1.0 / (gDeriv m ^ (2::Int) * varOf family m)) | m <- muL ]
          wMat = LA.diag ws
      in LA.inv (LA.tr x LA.<> wMat LA.<> x)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

fitGLM :: Family -> LA.Matrix Double -> LA.Vector Double -> FitResult
fitGLM family x y = fst (runIRLS family x y)

-- | Fit GLM and compute smooth curve + CI band on a fine grid for plotting.
-- CI is computed in linear-predictor space, then back-transformed via g⁻¹.
fitGLMWithSmooth
  :: Family
  -> Int      -- ^ polynomial degree for the linear predictor
  -> Double   -- ^ CI level (e.g. 0.95)
  -> Int      -- ^ grid resolution for smooth curve
  -> DataFrame
  -> Text     -- ^ x column
  -> Text     -- ^ y column
  -> Maybe (FitResult, SmoothFit)
fitGLMWithSmooth family degree level nGrid df xCol yCol = do
  xVec <- getNumeric xCol df
  yVec <- getNumeric yCol df
  let dm            = polyDesignMatrix degree xVec
      y             = LA.fromList (V.toList yVec)
      (res, fisher) = runIRLS family dm y
      (_, gInv, _)  = linkOf family
      beta          = coefficients res

      -- Fine grid for smooth curve
      xLa   = LA.fromList (V.toList xVec)
      xGrid = V.fromList (linspace (LA.minElement xLa) (LA.maxElement xLa) nGrid)
      dmG   = polyDesignMatrix degree xGrid
      etaG  = dmG LA.#> beta
      yGrid = map gInv (LA.toList etaG)

      -- Quantile: t-distribution for Gaussian, normal approximation otherwise
      n    = LA.rows dm
      p    = LA.cols dm
      qVal = case family of
               Gaussian -> quantile (studentT (fromIntegral (n - p))) ((1 + level) / 2)
               _        -> quantile (normalDistr 0 1) ((1 + level) / 2)

      -- CI: η* ± q × SE(η*),  SE(η*)² = x*ᵀ (XᵀWX)⁻¹ x*
      gRows  = LA.toRows dmG
      halfW xi = qVal * sqrt (max 0 (xi `LA.dot` (fisher LA.#> xi)))
      etaL   = LA.toList etaG
      lowers = zipWith (\eta xi -> gInv (eta - halfW xi)) etaL gRows
      uppers = zipWith (\eta xi -> gInv (eta + halfW xi)) etaL gRows

  return (res, SmoothFit (V.toList xGrid) yGrid lowers uppers)

-- ---------------------------------------------------------------------------
-- Goodness of fit
-- ---------------------------------------------------------------------------

-- | Deviance-based R²: 1 - D_fitted / D_null  (always in [0,1] for fitted GLMs).
-- Gaussian uses OLS R²; Binomial/Poisson use the GLM deviance ratio.
pseudoR2 :: Family -> LA.Vector Double -> LA.Vector Double -> Double
pseudoR2 Gaussian y mu =
  let resid = y - mu
      yMean = LA.sumElements y / fromIntegral (LA.size y)
      dev   = LA.cmap (subtract yMean) y
  in 1 - (resid `LA.dot` resid) / (dev `LA.dot` dev)
pseudoR2 family y mu =
  let yMean  = LA.sumElements y / fromIntegral (LA.size y)
      muNull = LA.konst yMean (LA.size y)
      dFit   = glmDeviance family y mu
      dNull  = glmDeviance family y muNull
  in if dNull == 0 then 1 else 1 - dFit / dNull

-- | GLM deviance: 2 * (logL_saturated - logL_fitted)
glmDeviance :: Family -> LA.Vector Double -> LA.Vector Double -> Double
glmDeviance Gaussian y mu =
  let r = y - mu in r `LA.dot` r
glmDeviance Binomial y mu =
  let muC  = LA.cmap (max 1e-15 . min (1 - 1e-15)) mu
      term = zipWith (\yi mui -> xlogy yi (yi / mui) + xlogy (1 - yi) ((1 - yi) / (1 - mui)))
               (LA.toList y) (LA.toList muC)
  in 2 * sum term
glmDeviance Poisson y mu =
  let muC  = LA.cmap (max 1e-15) mu
      term = zipWith (\yi mui -> xlogy yi (yi / mui) - (yi - mui))
               (LA.toList y) (LA.toList muC)
  in 2 * sum term

-- 0 * log(0) = 0 by convention
xlogy :: Double -> Double -> Double
xlogy 0 _ = 0
xlogy x y = x * log y
