{-# LANGUAGE OverloadedStrings #-}
module Model.GLM
  ( Family (..)
  , parseFamily
  , LinkFn (..)
  , parseLink
  , canonicalLink
  , fitGLM
  , fitGLMFull
  , fitGLMWithSmooth
  ) where

import qualified DataFrame.Internal.DataFrame as DXD
import DataIO.Convert (getDoubleVec)
import Model.Core
import Model.LM (multiPolyDesignMatrix, linspace, SmoothFit (..))

import Data.Text (Text)
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import Statistics.Distribution (quantile)
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution.StudentT (studentT)

-- ---------------------------------------------------------------------------
-- Family (response distribution)
-- ---------------------------------------------------------------------------

data Family = Gaussian | Binomial | Poisson
  deriving (Show, Eq)

parseFamily :: String -> Either String Family
parseFamily "gaussian" = Right Gaussian
parseFamily "binomial" = Right Binomial
parseFamily "poisson"  = Right Poisson
parseFamily s          = Left ("Unknown distribution '" ++ s ++ "'. Use: gaussian | binomial | poisson")

-- ---------------------------------------------------------------------------
-- Link function
-- ---------------------------------------------------------------------------

data LinkFn = Identity | Log | Logit | Sqrt
  deriving (Show, Eq)

parseLink :: String -> Either String LinkFn
parseLink "identity" = Right Identity
parseLink "log"      = Right Log
parseLink "logit"    = Right Logit
parseLink "sqrt"     = Right Sqrt
parseLink s          = Left ("Unknown link '" ++ s ++ "'. Use: identity | log | logit | sqrt")

canonicalLink :: Family -> LinkFn
canonicalLink Gaussian = Identity
canonicalLink Binomial = Logit
canonicalLink Poisson  = Log

-- Internal triple: (g, g⁻¹, g')
type Link = (Double -> Double, Double -> Double, Double -> Double)

linkFnOf :: LinkFn -> Link
linkFnOf Identity = (id,   id,                const 1.0)
linkFnOf Log      = (log,  exp,               recip)
linkFnOf Logit    = ( \x  -> log (x / (1 - x))
                    , \eta -> 1 / (1 + exp (-eta))
                    , \mu  -> 1.0 / (mu * (1 - mu))
                    )
linkFnOf Sqrt     = (sqrt, \eta -> eta * eta, \mu -> 0.5 / sqrt (max 1e-10 mu))

varOf :: Family -> Double -> Double
varOf Gaussian _  = 1.0
varOf Binomial mu = mu * (1 - mu)
varOf Poisson  mu = mu

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

initBeta :: Family -> LinkFn -> LA.Vector Double -> Int -> LA.Vector Double
initBeta family linkFn y p =
  let (g, _, _) = linkFnOf linkFn
      yMean = LA.sumElements y / fromIntegral (LA.size y)
      yC = case family of
             Binomial -> max 1e-6 (min (1 - 1e-6) yMean)
             Poisson  -> max 1e-6 yMean
             Gaussian -> yMean
  in LA.fromList (g yC : replicate (p - 1) 0.0)

irlsStep :: Link -> (Double -> Double)
          -> (Family -> LA.Vector Double -> LA.Vector Double)
          -> Family -> LA.Matrix Double -> LA.Vector Double -> LA.Vector Double
          -> LA.Vector Double
irlsStep (_, gInv, gDeriv) varFn clamp family x y beta =
  let eta   = x LA.#> beta
      mu    = clamp family (LA.cmap gInv eta)
      muL   = LA.toList mu
      etaL  = LA.toList eta
      yL    = LA.toList y
      ws    = LA.fromList [ max 1e-10 (1.0 / (gDeriv m ^ (2::Int) * varFn m)) | m <- muL ]
      zs    = LA.fromList [ ei + (yi - mi) * gDeriv mi | (ei,yi,mi) <- zip3 etaL yL muL ]
      sqrtW = LA.diag (LA.cmap sqrt ws)
  in LA.flatten ((sqrtW LA.<> x) LA.<\> LA.asColumn (sqrtW LA.#> zs))

runIRLS :: Family -> LinkFn -> LA.Matrix Double -> LA.Vector Double
        -> (FitResult, LA.Matrix Double)
runIRLS family linkFn x y = (mkResult betaFinal, fisherInv betaFinal)
  where
    link  = linkFnOf linkFn
    step  = irlsStep link (varOf family) safeMu family x y
    beta0 = initBeta family linkFn y (LA.cols x)

    betaFinal = converge maxIter beta0

    converge 0 beta = beta
    converge n beta =
      let betaNew = step beta
      in if LA.norm_2 (betaNew - beta) < tol || any notFinite (LA.toList betaNew)
         then betaNew
         else converge (n - 1) betaNew

    notFinite b = isNaN b || isInfinite b

    mkResult beta =
      let (_, gInv, _) = link
          mu    = safeMu family (LA.cmap gInv (x LA.#> beta))
          resid = y - mu
          r2    = pseudoR2 family y mu
      in FitResult (LA.asColumn beta)
                   (LA.asColumn mu)
                   (LA.asColumn resid)
                   (LA.fromList [r2])

    fisherInv beta =
      let (_, gInv, gDeriv) = link
          mu   = safeMu family (LA.cmap gInv (x LA.#> beta))
          muL  = LA.toList mu
          ws   = LA.fromList [ max 1e-10 (1.0 / (gDeriv m ^ (2::Int) * varOf family m)) | m <- muL ]
          wMat = LA.diag ws
      in LA.inv (LA.tr x LA.<> wMat LA.<> x)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

fitGLM :: Family -> LA.Matrix Double -> LA.Vector Double -> FitResult
fitGLM family x y = fst (runIRLS family (canonicalLink family) x y)

-- | fitGLM に加えて Fisher 情報行列の逆行列 (Laplace 近似の共分散) を返す。
-- WAIC/LOO-CV の事後サンプリングに使用。
fitGLMFull :: Family -> LinkFn -> LA.Matrix Double -> LA.Vector Double
           -> (FitResult, LA.Matrix Double)
fitGLMFull = runIRLS

-- | Fit GLM with specified distribution and link function.
-- Accepts multiple x columns with per-column polynomial degrees.
-- Returns SmoothFit only when there is exactly one x column (for scatter plot).
-- For PI with non-Gaussian families, falls back to CI (warn at call site).
fitGLMWithSmooth
  :: Family
  -> LinkFn
  -> [(Text, Int)]   -- ^ [(x column name, polynomial degree)]
  -> Band            -- ^ uncertainty band specification
  -> Int             -- ^ grid resolution for smooth curve
  -> DXD.DataFrame
  -> Text            -- ^ y column
  -> Maybe (FitResult, Maybe SmoothFit)
fitGLMWithSmooth family linkFn colDegs band nGrid df yCol = do
  xVecs <- mapM (flip getDoubleVec df . fst) colDegs
  yVec  <- getDoubleVec yCol df

  let degrees       = map snd colDegs
      dm            = multiPolyDesignMatrix (zip xVecs degrees)
      y             = LA.fromList (V.toList yVec)
      (res, fisher) = runIRLS family linkFn dm y
      (_, gInv, _)  = linkFnOf linkFn
      beta          = coefficientsV res
      n             = LA.rows dm
      p             = LA.cols dm

      -- PI falls back to CI for non-Gaussian (caller should warn)
      effectiveBand = case (band, family) of
        (PI lvl, Gaussian) -> PI lvl
        (PI lvl, _)        -> CI lvl
        (b,      _)        -> b

      mSmooth = case (xVecs, degrees) of
        ([xVec], [deg]) -> Just (makeSmoothFit xVec deg)
        _               -> Nothing

      makeSmoothFit xVec deg =
        let xLa    = LA.fromList (V.toList xVec)
            xMin   = LA.minElement xLa
            xMax   = LA.maxElement xLa
            span'  = max 1e-8 (xMax - xMin)
            xGrid  = V.fromList (linspace (xMin - 0.5*span') (xMax + 0.5*span') nGrid)
            dmG    = multiPolyDesignMatrix [(xGrid, deg)]
            etaG   = dmG LA.#> beta
            yGrid  = map gInv (LA.toList etaG)
            gRows  = LA.toRows dmG
        in case effectiveBand of
          NoBand ->
            SmoothFit (V.toList xGrid) yGrid yGrid yGrid False
          CI level ->
            let qVal  = ciQuantile level
                halfW xi = qVal * sqrt (max 0 (xi `LA.dot` (fisher LA.#> xi)))
                etaL  = LA.toList etaG
                lowers = zipWith (\eta xi -> gInv (eta - halfW xi)) etaL gRows
                uppers = zipWith (\eta xi -> gInv (eta + halfW xi)) etaL gRows
            in SmoothFit (V.toList xGrid) yGrid lowers uppers True
          PI level ->
            -- Gaussian only: add s²·1 term to CI variance
            let dfStat = fromIntegral (n - p) :: Double
                s2     = let resV = residualsV res
                         in (resV `LA.dot` resV) / dfStat
                tVal   = quantile (studentT dfStat) ((1 + level) / 2)
                xtxi   = LA.inv (LA.tr dm LA.<> dm)
                halfW xi = tVal * sqrt (s2 * (1 + xi `LA.dot` (xtxi LA.#> xi)))
                etaL  = LA.toList etaG
                lowers = zipWith (\eta xi -> gInv (eta - halfW xi)) etaL gRows
                uppers = zipWith (\eta xi -> gInv (eta + halfW xi)) etaL gRows
            in SmoothFit (V.toList xGrid) yGrid lowers uppers True

      ciQuantile level = case family of
        Gaussian -> quantile (studentT (fromIntegral (n - p))) ((1 + level) / 2)
        _        -> quantile (normalDistr 0 1) ((1 + level) / 2)

  return (res, mSmooth)

-- ---------------------------------------------------------------------------
-- Goodness of fit
-- ---------------------------------------------------------------------------

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

xlogy :: Double -> Double -> Double
xlogy 0 _ = 0
xlogy x y = x * log y
