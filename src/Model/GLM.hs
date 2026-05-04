{-# LANGUAGE OverloadedStrings #-}
-- | Generalized Linear Models fit by Iteratively Reweighted Least Squares.
--
-- Provides Gaussian, Binomial and Poisson families with identity, log,
-- logit and sqrt link functions. 'runIRLS' returns both a 'FitResult' and
-- the inverse Fisher information @(XᵀWX)⁻¹@ used for standard errors and
-- predictive intervals. The multi-output variant 'fitGLMMulti' shares the
-- family / link across response columns and runs IRLS column-wise.
module Model.GLM
  ( Family (..)
  , parseFamily
  , LinkFn (..)
  , parseLink
  , canonicalLink
  , fitGLM
  , fitGLMFull
  , fitGLMWithSmooth
    -- * Multi-output (per-column IRLS; Family/Link shared)
  , GLMFitMulti (..)
  , fitGLMMulti
  ) where

import qualified DataFrame.Internal.DataFrame as DXD
import DataIO.Convert (getDoubleVec)
import Model.Core
import Model.LM (multiPolyDesignMatrix, linspace, SmoothFit (..))

import Data.Text (Text)
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import qualified Stat.Cholesky        as Chol
import Statistics.Distribution (quantile)
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution.StudentT (studentT)

-- ---------------------------------------------------------------------------
-- Family (response distribution)
-- ---------------------------------------------------------------------------

-- | GLM exponential-family distribution.
data Family = Gaussian | Binomial | Poisson
  deriving (Show, Eq)

-- | Parse a 'Family' name (case-sensitive).
parseFamily :: String -> Either String Family
parseFamily "gaussian" = Right Gaussian
parseFamily "binomial" = Right Binomial
parseFamily "poisson"  = Right Poisson
parseFamily s          = Left ("Unknown distribution '" ++ s ++ "'. Use: gaussian | binomial | poisson")

-- ---------------------------------------------------------------------------
-- Link function
-- ---------------------------------------------------------------------------

-- | GLM link function.
data LinkFn = Identity | Log | Logit | Sqrt
  deriving (Show, Eq)

-- | Parse a 'LinkFn' name.
parseLink :: String -> Either String LinkFn
parseLink "identity" = Right Identity
parseLink "log"      = Right Log
parseLink "logit"    = Right Logit
parseLink "sqrt"     = Right Sqrt
parseLink s          = Left ("Unknown link '" ++ s ++ "'. Use: identity | log | logit | sqrt")

-- | The canonical link function for a given family.
canonicalLink :: Family -> LinkFn
canonicalLink Gaussian = Identity
canonicalLink Binomial = Logit
canonicalLink Poisson  = Log

-- Internal triple: (g, g⁻¹, g')
type Link = (Double -> Double, Double -> Double, Double -> Double)

-- | Resolve a 'LinkFn' to its triple @(g, g⁻¹, g')@.
linkFnOf :: LinkFn -> Link
linkFnOf Identity = (id,   id,                const 1.0)
linkFnOf Log      = (log,  exp,               recip)
linkFnOf Logit    = ( \x  -> log (x / (1 - x))
                    , \eta -> 1 / (1 + exp (-eta))
                    , \mu  -> 1.0 / (mu * (1 - mu))
                    )
linkFnOf Sqrt     = (sqrt, \eta -> eta * eta, \mu -> 0.5 / sqrt (max 1e-10 mu))

-- | Variance function @V(μ)@ for the given family.
varOf :: Family -> Double -> Double
varOf Gaussian _  = 1.0
varOf Binomial mu = mu * (1 - mu)
varOf Poisson  mu = mu

-- | Clamp @μ@ to its valid range, avoiding boundary singularities.
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

-- | Per-observation log-likelihood for the canonical-link GLMs we
-- support. Used for the IRLS log-likelihood-based early termination
-- (see 'runIRLS').
--
--   * Gaussian: @-½ (y − μ)²@ (constant terms dropped, harmless for the
--     ratio-based stopping rule).
--   * Binomial: @y log μ + (1 − y) log (1 − μ)@.
--   * Poisson : @y log μ − μ@ (Stirling term dropped).
glmLogLik :: Family -> LA.Vector Double -> LA.Vector Double -> Double
glmLogLik family y mu =
  let yL = LA.toList y
      mL = LA.toList mu
      f Gaussian yi mi = -0.5 * (yi - mi) ** 2
      f Binomial yi mi =
        let m' = max 1e-12 (min (1 - 1e-12) mi)
        in yi * log m' + (1 - yi) * log (1 - m')
      f Poisson  yi mi =
        let m' = max 1e-12 mi
        in yi * log m' - m'
  in sum (zipWith (f family) yL mL)

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
      -- Normal-equations form: solve (Xᵀ W X) β = Xᵀ W z via SPD Cholesky.
      -- Faster than solving (√W X) β = (√W z) with the general LSQ
      -- (dgels) when n ≫ p, which is the common GLM regime.
      wxT   = LA.tr x * LA.asRow ws            -- p × n with column scaling
      gMat  = wxT LA.<> x                       -- p × p (SPD)
      bRhs  = LA.asColumn (wxT LA.#> zs)        -- p × 1
  in LA.flatten (Chol.cholSolveJitter gMat bRhs)

-- | Run IRLS to fit a single-output GLM. Returns both the fit result
-- and the inverse Fisher information @(XᵀWX)⁻¹@ used for standard
-- errors and credible/predictive intervals.
runIRLS :: Family -> LinkFn -> LA.Matrix Double -> LA.Vector Double
        -> (FitResult, LA.Matrix Double)
runIRLS family linkFn x y = (mkResult betaFinal, fisherInv betaFinal)
  where
    link@(_, gInv, _) = linkFnOf linkFn
    step  = irlsStep link (varOf family) safeMu family x y
    beta0 = initBeta family linkFn y (LA.cols x)

    -- 'converge' tracks both β and the previous log-likelihood. We stop
    -- when |Δβ|₂ < tol (the original criterion) **or** when the relative
    -- log-likelihood change drops below 'tol'. The latter typically
    -- triggers several iterations earlier than the β-norm criterion on
    -- ill-scaled problems and is the standard sklearn / statsmodels rule.
    llOf beta =
      let mu = safeMu family (LA.cmap gInv (x LA.#> beta))
      in glmLogLik family y mu

    betaFinal = converge maxIter beta0 (llOf beta0)

    converge 0 beta _   = beta
    converge n beta llP =
      let betaNew = step beta
      in if any notFinite (LA.toList betaNew)
         then beta                       -- divergence; keep last good β
         else
           let llN  = llOf betaNew
               dLL  = abs (llN - llP) / max (abs llP) 1
               dB   = LA.norm_2 (betaNew - beta)
           in if dB < tol || dLL < tol
                then betaNew
                else converge (n - 1) betaNew llN

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
          wxT  = LA.tr x * LA.asRow ws    -- p × n
          gMat = wxT LA.<> x               -- p × p (SPD)
          p    = LA.cols x
      in Chol.cholSolveJitter gMat (LA.ident p)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Fit a GLM with the canonical link, returning just the 'FitResult'.
fitGLM :: Family -> LA.Matrix Double -> LA.Vector Double -> FitResult
fitGLM family x y = fst (runIRLS family (canonicalLink family) x y)

-- | Like 'fitGLM' but also returns the inverse Fisher information
-- (Laplace-approximate posterior covariance). Used by the WAIC / LOO-CV
-- posterior-sampling helpers.
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

-- | McFadden-style pseudo-R² for GLMs.
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

-- | GLM deviance: @D(y, μ̂) = 2 (ℓ_sat − ℓ_model)@.
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

-- ---------------------------------------------------------------------------
-- 多出力 GLM (列ごと IRLS)
-- ---------------------------------------------------------------------------

-- | Multi-output GLM result. The same family and link function are
-- used for all @q@ output columns; IRLS is run column-wise.
data GLMFitMulti = GLMFitMulti
  { gfmFamily   :: Family
  , gfmLinkFn   :: LinkFn
  , gfmFits     :: [FitResult]            -- ^ 列ごと FitResult
  , gfmFisher   :: [LA.Matrix Double]     -- ^ 列ごと (XᵀWX)⁻¹
  , gfmBeta     :: LA.Matrix Double       -- ^ 係数行列 p × q
  , gfmFitted   :: LA.Matrix Double       -- ^ 予測 n × q
  , gfmResid    :: LA.Matrix Double       -- ^ 残差 n × q
  } deriving (Show)

-- | Fit a multi-output GLM. @Y@ has shape @n × q@; family and link
-- function are shared across all columns.
fitGLMMulti :: Family -> LinkFn -> LA.Matrix Double -> LA.Matrix Double
            -> GLMFitMulti
fitGLMMulti family linkFn x y =
  let q     = LA.cols y
      perCol j = runIRLS family linkFn x (LA.flatten (y LA.¿ [j]))
      pairs = [perCol j | j <- [0 .. q - 1]]
      fits  = map fst pairs
      fishs = map snd pairs
      betaM = LA.fromColumns [LA.flatten (coefficients f) | f <- fits]
      fitM  = LA.fromColumns [LA.flatten (fitted     f) | f <- fits]
      resM  = LA.fromColumns [LA.flatten (residuals  f) | f <- fits]
  in GLMFitMulti family linkFn fits fishs betaM fitM resM
