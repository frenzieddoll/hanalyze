{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Model.GLM
-- Description : IRLS による一般化線形モデル (Generalized Linear Models)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Generalized Linear Models fit by Iteratively Reweighted Least Squares.
--
-- Provides Gaussian, Binomial and Poisson families with identity, log,
-- logit and sqrt link functions. 'runIRLS' returns both a 'FitResult' and
-- the inverse Fisher information @(XᵀWX)⁻¹@ used for standard errors and
-- predictive intervals. The multi-output variant 'fitGLMMulti' shares the
-- family / link across response columns and runs IRLS column-wise.
module Hanalyze.Model.GLM
  ( Family (..)
  , parseFamily
  , LinkFn (..)
  , parseLink
  , canonicalLink
  , GLMSolver (..)
  , fitGLM
  , fitGLMFull
  , fitGLMWith
  , fitGLMWithSmooth
  , runIRLS
  , runLBFGS_GLM
    -- * Multi-output (per-column IRLS; Family/Link shared)
  , GLMFitMulti (..)
  , fitGLMMulti
    -- * Diagnostic primitives (新規 export, request/090-CD)
  , Link
  , linkFnOf
  , glmDeviance
  , glmLogLik
  , glmVariance
    -- * Residuals + predict SE (request/090-AB)
  , glmPearsonResiduals
  , glmDevianceResiduals
  , GlmPredictCI (..)
  , predictGlmEtaWithSE
  , predictGlmMuWithCI
  ) where

import qualified DataFrame.Internal.DataFrame as DXD
import Hanalyze.DataIO.Convert (getDoubleVec)
import Hanalyze.Model.Core
import Hanalyze.Model.LM (multiPolyDesignMatrix, linspace, SmoothFit (..))

import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra as LA
import qualified Hanalyze.Stat.Cholesky        as Chol
import qualified Hanalyze.Optim.LBFGS          as LBFGS
import qualified Hanalyze.Optim.Common         as OC
import           System.IO.Unsafe     (unsafePerformIO)
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

-- | Public alias for the family variance @V(μ)@; see @varOf@. Exposed
-- so HPotfire diagnostics can compute Pearson-style standardisations
-- without re-implementing the family table.
glmVariance :: Family -> Double -> Double
glmVariance = varOf

-- | Clamp @μ@ to its valid range, avoiding boundary singularities.
safeMu :: Family -> LA.Vector Double -> LA.Vector Double
safeMu Binomial = LA.cmap (max 1e-8 . min (1 - 1e-8))
safeMu Poisson  = LA.cmap (max 1e-8)
safeMu Gaussian = id

-- | Fused @safeMu (gInv eta)@ for canonical-link GLMs — single
-- @VS.map@ pass instead of @gInv@ followed by @safeMu@.
--
-- P36 (2026-05-07): the Poisson IRLS loop did
-- @safeMu (VS.map (exp . min 500) eta)@ each iteration, which is two
-- passes over an @n@-vector and two allocations. Most iterations
-- spend the bulk of time in 'irlsStep' BLAS calls anyway, but on the
-- @n=10000@ Poisson bench this fused form trims ~10% off per-iter μ
-- compute. For non-canonical links callers fall back to the generic
-- @safeMu . VS.map gInv@ path.
--
-- Currently only used for the Poisson canonical link — Binomial
-- empirically regresses under fusion (GHC inlines the two-pass split
-- form better on the logit bench) so it stays on the
-- @safeMu . VS.map gInv@ path.
muCanonical :: Family -> LA.Vector Double -> LA.Vector Double
muCanonical Poisson =
  VS.map (\e -> max 1e-8 (exp (min 500 e)))
muCanonical f =
  -- Generic fallback: callers should normally not hit this for
  -- Binomial / Gaussian; defined for totality.
  safeMu f
{-# INLINE muCanonical #-}

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
-- Phase 11c: list-based zipWith/sum was 11.3% time + 8.3% alloc on
-- the n=10k logit profile. Replaced with vector-native zipVectorWith
-- + sumElements (no list materialization, single BLAS-friendly pass).
-- The family is dispatched once at the top-level let-binding so the
-- inner zipVectorWith sees a fully monomorphic Double -> Double ->
-- Double closure that GHC can specialize.
glmLogLik :: Family -> LA.Vector Double -> LA.Vector Double -> Double
glmLogLik family y mu = VS.sum (VS.zipWith f y mu)
  where
    f = case family of
      Gaussian -> \yi mi -> -0.5 * (yi - mi) ** 2
      Binomial -> \yi mi ->
        let m' = max 1e-12 (min (1 - 1e-12) mi)
        in yi * log m' + (1 - yi) * log (1 - m')
      Poisson  -> \yi mi ->
        let m' = max 1e-12 mi
        in yi * log m' - m'

initBeta :: Family -> LinkFn -> LA.Vector Double -> Int -> LA.Vector Double
initBeta family linkFn y p =
  let (g, _, _) = linkFnOf linkFn
      yMean = LA.sumElements y / fromIntegral (LA.size y)
      yC = case family of
             Binomial -> max 1e-6 (min (1 - 1e-6) yMean)
             Poisson  -> max 1e-6 yMean
             Gaussian -> yMean
  in LA.fromList (g yC : replicate (p - 1) 0.0)

-- | One IRLS step. Returns the updated @β@ together with the
-- corresponding @μ@ and the log-likelihood at the /input/ @β@.
--
-- Returning @μ_old@ and @ll_old@ here lets the convergence loop in
-- 'runIRLS' avoid an extra @x #> beta@ + @gInv μ@ + 'glmLogLik' pass
-- per iteration that the old API forced (see glmbench §1).
irlsStep :: Link -> (Double -> Double)
          -> (Family -> LA.Vector Double -> LA.Vector Double)
          -> Family -> LA.Matrix Double -> LA.Vector Double -> LA.Vector Double
          -> (LA.Vector Double, LA.Vector Double, Double)
irlsStep (_, gInv, gDeriv) varFn clamp family x y beta =
  -- Phase 12a (2026-05-06): replaced massiv-based map/zipWith3 with
  -- pure VS.{map,zipWith3}. Profile (Phase 11) showed
  -- @trivialScheduler_@ (massiv) consumed 9.8% of GLM IRLS time —
  -- pure overhead since 'compFor' was always 'Seq'. The replacement
  -- is single-pass, allocation-equivalent, and avoids the
  -- hmatrix↔massiv round trip.
  --
  -- P36 (2026-05-07): for Poisson canonical link, fuse @gInv@ and
  -- @safeMu@ into a single VS.map. Empirically Binomial regresses
  -- under the same fusion (GHC inlines the two-pass split better),
  -- so it stays on the generic path. The family pattern-match is
  -- hot-loop constant and gets specialized away by GHC.
  let eta    = x LA.#> beta
      mu     = case family of
                 Poisson -> muCanonical Poisson eta
                 _       -> clamp family (VS.map gInv eta)
      llHere = glmLogLik family y mu
      ws     = VS.map (\m -> max 1e-10
                               (1.0 / (gDeriv m ^ (2 :: Int) * varFn m)))
                      mu
      zs     = VS.zipWith3 (\ei yi mi -> ei + (yi - mi) * gDeriv mi)
                           eta y mu
      -- Normal-equations form: solve (Xᵀ W X) β = Xᵀ W z via SPD Cholesky.
      -- Faster than solving (√W X) β = (√W z) with the general LSQ
      -- (dgels) when n ≫ p, which is the common GLM regime.
      wxT     = LA.tr x * LA.asRow ws            -- p × n with column scaling
      gMat    = wxT LA.<> x                       -- p × p (SPD)
      bRhs    = LA.asColumn (wxT LA.#> zs)        -- p × 1
      betaNew = LA.flatten (Chol.cholSolveJitter gMat bRhs)
  in (betaNew, mu, llHere)

-- ---------------------------------------------------------------------------
-- Solver selection
-- ---------------------------------------------------------------------------

-- | GLM solver back-end.
--
--   * 'IRLS' — Iteratively Re-weighted Least Squares. Each iteration
--     builds and solves the SPD normal equations @XᵀWX β = XᵀWz@ via
--     'Hanalyze.Stat.Cholesky.cholSolveJitter'. Quadratic convergence (= a full
--     Newton step every iteration); each iteration is @O(np²)@.
--   * 'LBFGS' — direct L-BFGS minimization of the negative
--     log-likelihood with the analytic gradient @Xᵀ(μ − y)@ (canonical
--     link). Per-iteration cost is @O(np)@. This is what @sklearn@
--     uses, and is the better choice in @n ≫ p²@ regimes once the
--     'Hanalyze.Optim.LBFGS' inner loop is moved off Haskell-list operations.
--
-- Default solver: 'IRLS'. In the current bench regime (@n ≤ 10000@,
-- @p ≤ 20@), IRLS-with-Cholesky beats the pure-Haskell-list L-BFGS
-- because @O(np²)@ on small @p@ is dominated by hmatrix's BLAS calls
-- whereas the L-BFGS path pays per-step Haskell overhead. Switch to
-- 'LBFGS' for problems with @p > 50@ or when 'Hanalyze.Optim.LBFGS' itself is
-- vectorized.
data GLMSolver
  = IRLS
  | LBFGS
  deriving (Eq, Show)

defaultGLMSolver :: GLMSolver
defaultGLMSolver = IRLS

-- ---------------------------------------------------------------------------
-- L-BFGS direct GLM
-- ---------------------------------------------------------------------------

-- | Negative log-likelihood @-ℓ(β)@ for a canonical-link GLM.
glmNegLogLik :: Family -> LA.Matrix Double -> LA.Vector Double
             -> LA.Vector Double -> Double
glmNegLogLik family x y beta = negate (glmLogLik family y mu)
  where
    eta = x LA.#> beta
    mu  = case family of
            Gaussian -> eta
            Binomial -> LA.cmap (\e -> 1 / (1 + exp (-e))) eta
            Poisson  -> LA.cmap (\e -> exp (min 500 e))   eta

-- | Gradient of @-ℓ(β)@ for a canonical-link GLM:
--
-- @∇(-ℓ) = Xᵀ (μ - y)@
--
-- This identity holds for /every/ exponential-family GLM with the
-- canonical link, which is why L-BFGS is so attractive here — no
-- per-family branching is needed inside the gradient.
glmGrad :: Family -> LA.Matrix Double -> LA.Vector Double
        -> LA.Vector Double -> LA.Vector Double
glmGrad family x y beta =
  let eta = x LA.#> beta
      mu  = case family of
              Gaussian -> eta
              Binomial -> LA.cmap (\e -> 1 / (1 + exp (-e))) eta
              Poisson  -> LA.cmap (\e -> exp (min 500 e))   eta
  in LA.tr x LA.#> (mu - y)

-- | Fit a canonical-link GLM by minimizing the negative log-likelihood
-- with L-BFGS. This is the path that 'sklearn.linear_model.\*' uses
-- internally for logistic and Poisson regression and is markedly
-- faster than IRLS when @n ≫ p@ because each L-BFGS iteration costs
-- only @O(np)@ versus IRLS's @O(np²)@ for the @XᵀWX@ build.
--
-- Returns the same @(FitResult, fisherInv)@ pair as 'runIRLS'; the
-- Fisher information is computed once at the converged β via the same
-- Cholesky path used by IRLS, so downstream uses (CIs, WAIC, …) are
-- identical.
runLBFGS_GLM :: Family -> LA.Matrix Double -> LA.Vector Double
             -> (FitResult, LA.Matrix Double)
runLBFGS_GLM family x y =
  -- Only canonical-link GLMs are supported here (the simple gradient
  -- formula above relies on the canonical link). For non-canonical
  -- links (e.g. probit, sqrt link) the caller should use 'runIRLS'.
  let p     = LA.cols x
      beta0 = initBeta family (canonicalLink family) y p
      -- Vector-native objective and gradient (no list conversion per
      -- L-BFGS step, which used to dominate runtime when @p ≈ 20@).
      fV b = glmNegLogLik family x y b
      gV b = glmGrad      family x y b
      cfg  = LBFGS.defaultLBFGSConfig
               { LBFGS.lbStop = OC.defaultStopCriteria
                                  { OC.stMaxIter = 200
                                  , OC.stTolFun  = 1e-10
                                  , OC.stTolX    = 1e-10 } }
      result = unsafePerformIO $
                 LBFGS.runLBFGSWithV cfg fV gV beta0
      betaF  = LA.fromList (OC.orBest result)
      mu     = safeMu family $ case family of
                 Gaussian -> x LA.#> betaF
                 Binomial -> LA.cmap (\e -> 1 / (1 + exp (-e))) (x LA.#> betaF)
                 Poisson  -> LA.cmap (\e -> exp (min 500 e))   (x LA.#> betaF)
      resid  = y - mu
      r2     = pseudoR2 family y mu
      fitR   = FitResult (LA.asColumn betaF)
                         (LA.asColumn mu)
                         (LA.asColumn resid)
                         (LA.fromList [r2])
      -- Fisher information at convergence (same path as IRLS).
      ws     = VS.map (\m -> max 1e-10 (1.0 / (gDeriv m ^ (2::Int)
                                                * varOf family m)))
                      mu
      wxT    = LA.tr x * LA.asRow ws
      gMat   = wxT LA.<> x
      fisher = Chol.cholSolveJitter gMat (LA.ident p)
  in (fitR, fisher)
  where
    (_, _, gDeriv) = linkFnOf (canonicalLink family)

-- ---------------------------------------------------------------------------

-- | Run IRLS to fit a single-output GLM. Returns both the fit result
-- and the inverse Fisher information @(XᵀWX)⁻¹@ used for standard
-- errors and credible/predictive intervals.
runIRLS :: Family -> LinkFn -> LA.Matrix Double -> LA.Vector Double
        -> (FitResult, LA.Matrix Double)
runIRLS family linkFn x y = (mkResult betaFinal muFinal, fisherInvFromMu muFinal)
  where
    link@(_, gInv, _) = linkFnOf linkFn
    step  = irlsStep link (varOf family) safeMu family x y
    beta0 = initBeta family linkFn y (LA.cols x)
    isCanonicalLink = linkFn == canonicalLink family

    -- Mu at convergence boundary: mirror 'irlsStep' Poisson fusion
    -- when on the canonical link.
    muOf beta
      | isCanonicalLink && family == Poisson
                  = muCanonical Poisson (x LA.#> beta)
      | otherwise = safeMu family (VS.map gInv (x LA.#> beta))

    -- 'converge' tracks β and the /previous/ iteration's log-likelihood.
    -- 'irlsStep' returns @(β_{k+1}, μ_at_β_k, ll_at_β_k)@: the updated β
    -- plus the current iter's μ + ll, all free side-products of the
    -- IRLS step itself. We pass @ll_at_β_k@ forward as the next iter's
    -- @llP@, eliminating the dedicated O(np) @llOf β@ pass per iter
    -- that the previous code performed (glmbench §1).
    --
    -- Convergence is checked on β-norm or relative ll change. The ll
    -- comparison is between ll(β_k) and ll(β_{k-1}) — one iteration
    -- lagged from the standard ll(β_{k+1}) vs ll(β_k) form, which is
    -- equivalent in steady state and avoids any extra μ pass in the
    -- inner loop.
    (betaFinal, muFinal) = converge maxIter True beta0 (glmLogLik family y (muOf beta0))

    -- ★初回反復だけ dLL 判定を無効化する: 'irlsStep' が返す @llHere@ は入力 β での
    -- @ll(β_k)@ なので、 初回は seed @llP = ll(β0)@ と一致し @dLL = 0 < tol@ で
    -- IRLS が 1 ステップで早期停止してしまう (= 28d1feb7 の per-iter ll 再利用
    -- リライトで混入した回帰)。 dB (β-norm) 判定は初回も正しいので残し、 dLL は
    -- 2 反復目以降 @ll(β_k) vs ll(β_{k-1})@ が揃ってから使う。
    converge 0 _     beta _  = (beta, muOf beta)
    converge n first beta llP =
      let (betaNew, _muHere, llHere) = step beta
      in if any notFinite (LA.toList betaNew)
         then (beta, muOf beta)          -- divergence; keep last good β
         else
           let dB  = LA.norm_2 (betaNew - beta)
               dLL = abs (llHere - llP) / max (abs llP) 1
           in if dB < tol || (not first && dLL < tol)
                then (betaNew, muOf betaNew)   -- final μ pass once
                else converge (n - 1) False betaNew llHere

    notFinite b = isNaN b || isInfinite b

    mkResult beta mu =
      let resid = y - mu
          r2    = pseudoR2 family y mu
      in FitResult (LA.asColumn beta)
                   (LA.asColumn mu)
                   (LA.asColumn resid)
                   (LA.fromList [r2])

    fisherInvFromMu mu =
      let (_, _, gDeriv) = link
          ws   = VS.map (\m -> max 1e-10
                                 (1.0 / (gDeriv m ^ (2::Int) * varOf family m)))
                        mu
          wxT  = LA.tr x * LA.asRow ws    -- p × n
          gMat = wxT LA.<> x               -- p × p (SPD)
          p    = LA.cols x
      in Chol.cholSolveJitter gMat (LA.ident p)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Fit a GLM with the canonical link, returning just the 'FitResult'.
-- Uses @defaultGLMSolver@ (currently 'IRLS').
fitGLM :: Family -> LA.Matrix Double -> LA.Vector Double -> FitResult
fitGLM family x y =
  fst (fitGLMWith defaultGLMSolver family (canonicalLink family) x y)

-- | Like 'fitGLM' but also returns the inverse Fisher information
-- (Laplace-approximate posterior covariance). Used by the WAIC / LOO-CV
-- posterior-sampling helpers.
--
-- Routes through 'fitGLMWith' with @defaultGLMSolver@. When the
-- supplied 'LinkFn' is /not/ the canonical link of the family, the
-- 'LBFGS' solver is unsupported and the function silently falls back
-- to 'IRLS' so existing call sites that pass non-canonical links keep
-- working.
fitGLMFull :: Family -> LinkFn -> LA.Matrix Double -> LA.Vector Double
           -> (FitResult, LA.Matrix Double)
fitGLMFull family linkFn x y
  | linkFn == canonicalLink family = fitGLMWith defaultGLMSolver family linkFn x y
  | otherwise                      = runIRLS family linkFn x y

-- | Pick the solver explicitly. The 'LBFGS' path is only valid for the
-- canonical link of the family; non-canonical links transparently fall
-- back to 'IRLS'.
fitGLMWith
  :: GLMSolver -> Family -> LinkFn
  -> LA.Matrix Double -> LA.Vector Double
  -> (FitResult, LA.Matrix Double)
fitGLMWith IRLS  family linkFn x y = runIRLS family linkFn x y
fitGLMWith LBFGS family linkFn x y
  | linkFn == canonicalLink family = runLBFGS_GLM family x y
  | otherwise                      = runIRLS family linkFn x y

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
                etaL  = LA.toList etaG
            -- df<=0 (飽和) は s²=0/0・studentT が例外 → 帯を線に潰す (lo=hi=ĝ⁻¹(η))。
            in if dfStat <= 0
                 then SmoothFit (V.toList xGrid) yGrid (map gInv etaL) (map gInv etaL) True
                 else
                   let s2     = let resV = residualsV res
                                in (resV `LA.dot` resV) / dfStat
                       tVal   = quantile (studentT dfStat) ((1 + level) / 2)
                       xtxi   = LA.inv (LA.tr dm LA.<> dm)
                       halfW xi = tVal * sqrt (s2 * (1 + xi `LA.dot` (xtxi LA.#> xi)))
                       lowers = zipWith (\eta xi -> gInv (eta - halfW xi)) etaL gRows
                       uppers = zipWith (\eta xi -> gInv (eta + halfW xi)) etaL gRows
                   in SmoothFit (V.toList xGrid) yGrid lowers uppers True

      ciQuantile level = case family of
        -- 飽和 (df=n-p<=0) は studentT が例外 → 分位点 0 = CI 幅ゼロ (帯を線に潰す)。
        Gaussian | n - p <= 0 -> 0
                 | otherwise  -> quantile (studentT (fromIntegral (n - p))) ((1 + level) / 2)
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
      term = VS.zipWith
               (\yi mui -> xlogy yi (yi / mui)
                         + xlogy (1 - yi) ((1 - yi) / (1 - mui)))
               y muC
  in 2 * VS.sum term
glmDeviance Poisson y mu =
  let muC  = LA.cmap (max 1e-15) mu
      term = VS.zipWith
               (\yi mui -> xlogy yi (yi / mui) - (yi - mui))
               y muC
  in 2 * VS.sum term

xlogy :: Double -> Double -> Double
xlogy 0 _ = 0
xlogy x y = x * log y

-- ---------------------------------------------------------------------------
-- 090-A: Residuals (request/090-AB)
-- ---------------------------------------------------------------------------

-- | Pearson residuals @(y - μ) / sqrt(V(μ))@.
glmPearsonResiduals
  :: Family
  -> LA.Vector Double   -- ^ Observations @y@.
  -> LA.Vector Double   -- ^ Fitted means @μ@.
  -> LA.Vector Double
glmPearsonResiduals family y mu =
  VS.zipWith (\yi mui ->
                let v = varOf family mui
                in if v <= 0 then 0 else (yi - mui) / sqrt v)
             y mu

-- | Deviance residuals @sign(y - μ) · sqrt(d_i)@ where @d_i@ is the
-- per-observation contribution to the deviance @D = Σ d_i@.
glmDevianceResiduals
  :: Family
  -> LA.Vector Double
  -> LA.Vector Double
  -> LA.Vector Double
glmDevianceResiduals family y mu =
  let perObs = pointwiseDeviance family y mu
  in VS.zipWith3 (\yi mui di -> signum (yi - mui) * sqrt (max 0 di))
                 y mu perObs
  where
    pointwiseDeviance Gaussian ys ms =
      VS.zipWith (\yi mui -> let r = yi - mui in r * r) ys ms
    pointwiseDeviance Binomial ys ms =
      VS.zipWith
        (\yi mui ->
            let muC = max 1e-15 (min (1 - 1e-15) mui)
            in 2 * ( xlogy yi (yi / muC)
                   + xlogy (1 - yi) ((1 - yi) / (1 - muC)) ))
        ys ms
    pointwiseDeviance Poisson ys ms =
      VS.zipWith
        (\yi mui ->
            let muC = max 1e-15 mui
            in 2 * (xlogy yi (yi / muC) - (yi - muC)))
        ys ms

-- ---------------------------------------------------------------------------
-- 090-B: Predict + SE (request/090-AB)
-- ---------------------------------------------------------------------------

-- | Prediction with Wald confidence interval on the response (μ) scale.
data GlmPredictCI = GlmPredictCI
  { gpMu :: !Double
  , gpLo :: !Double
  , gpHi :: !Double
  } deriving (Show)

-- | Linear-predictor prediction @η = xᵀβ@ with @SE = sqrt(xᵀ Σ x)@,
-- where @Σ@ is @(XᵀWX)⁻¹@ from 'fitGLMFull'. The intercept must be
-- present in @x@.
predictGlmEtaWithSE
  :: LA.Vector Double
  -> LA.Matrix Double
  -> LA.Vector Double
  -> (Double, Double)
predictGlmEtaWithSE beta sigma x =
  let eta   = x `LA.dot` beta
      sigX  = sigma LA.#> x
      seEta = sqrt (max 0 (x `LA.dot` sigX))
  in (eta, seEta)

-- | Wald CI on the response scale: build CI in @η@ space then transform
-- both endpoints through the inverse link.
predictGlmMuWithCI
  :: LinkFn
  -> Double
  -> LA.Vector Double
  -> LA.Matrix Double
  -> LA.Vector Double
  -> GlmPredictCI
predictGlmMuWithCI link level beta sigma x =
  let (eta, se)  = predictGlmEtaWithSE beta sigma x
      z          = waldZ level
      (_, gInv, _) = linkFnOf link
      mu  = gInv eta
      lo  = gInv (eta - z * se)
      hi  = gInv (eta + z * se)
  in GlmPredictCI { gpMu = mu, gpLo = min lo hi, gpHi = max lo hi }

-- | Two-sided Wald z: @z = √2 · erf⁻¹(level)@ (so @level=0.95@ →
-- @1.95996…@). Uses Winitzki's rational approximation of @erf⁻¹@
-- (~1e-3 accuracy) to keep @statistics@ out of this module.
waldZ :: Double -> Double
waldZ lvl
  | lvl <= 0 || lvl >= 1 =
      error "predictGlmMuWithCI: confidence level must lie in (0, 1)"
  | otherwise            = sqrt 2 * inverfApprox lvl

inverfApprox :: Double -> Double
inverfApprox x =
  let a   = 0.147
      ln1 = log (1 - x * x)
      term1 = 2 / (pi * a) + ln1 / 2
  in signum x * sqrt (sqrt (term1 * term1 - ln1 / a) - term1)

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
