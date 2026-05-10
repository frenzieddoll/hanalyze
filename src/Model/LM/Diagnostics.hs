-- | Inference and residual diagnostics for ordinary linear regression.
--
-- Provides standard errors, t / p-values, F-statistic, information
-- criteria (AIC / BIC), leverage / hat-diagonal, standardised
-- residuals, and Cook's distance. All multi-output operators
-- (@q@ output columns) follow the @Matrix p × q@ canonical convention,
-- with @Vector p@ wrappers for the @q = 1@ case.
module Model.LM.Diagnostics
  ( -- * t-quantile
    ciTValue
    -- * Per-coefficient inference (Multi-output canonical)
  , CoefStats (..)
  , lmSigmaSqMulti
  , lmCovarianceMulti
  , lmStdErrorsMulti
  , lmCoefStatsMulti
    -- * 1-output convenience wrappers
  , lmStdErrors
  , lmCoefStats
    -- * Whole-model F-statistic
  , FStat (..)
  , lmFStatistic
    -- * Information criteria
  , ICs (..)
  , lmInformationCriteria
  , lmInformationCriteriaMulti
    -- * Residual diagnostics
  , hatDiagonal
  , standardizedResiduals
  , cooksDistance
    -- * Predictor utilities
  , predictorStdDevs
  ) where

import Model.Core (FitResult (..))
import qualified Numeric.LinearAlgebra as LA
import qualified Statistics.Distribution as SD
import qualified Statistics.Distribution.FDistribution as FD
import Statistics.Distribution.StudentT (studentT)

-- ---------------------------------------------------------------------------
-- t-quantile
-- ---------------------------------------------------------------------------

-- | Two-sided Student-t quantile @t_{α/2, df}@ at confidence
-- @level@ (e.g. @0.95@) and degrees of freedom @df@.
ciTValue :: Double -> Int -> Double
ciTValue level df =
  SD.quantile (studentT (fromIntegral df)) ((1.0 + level) / 2.0)

-- ---------------------------------------------------------------------------
-- Helpers shared across diagnostics
-- ---------------------------------------------------------------------------

-- | Per-output residual variance @σ²_k = RSS_k / (n − p)@. Returns a
-- length-@q@ vector.
lmSigmaSqMulti :: FitResult -> LA.Vector Double
lmSigmaSqMulti res =
  let r       = residuals res
      n       = LA.rows r
      p       = LA.rows (coefficients res)
      df      = fromIntegral (n - p) :: Double
      cols    = LA.toColumns r
      ssRes c = c `LA.dot` c
  in LA.fromList [ ssRes c / df | c <- cols ]

-- | Per-output coefficient covariance matrices. Returns a list of
-- @q@ symmetric @p × p@ matrices, one per output column:
-- @Cov_k = σ²_k × (XᵀX)⁻¹@.
lmCovarianceMulti :: LA.Matrix Double -> FitResult -> [LA.Matrix Double]
lmCovarianceMulti x res =
  let xtxi  = LA.inv (LA.tr x LA.<> x)
      sig2s = LA.toList (lmSigmaSqMulti res)
  in [ LA.scale s2 xtxi | s2 <- sig2s ]

-- ---------------------------------------------------------------------------
-- Standard errors
-- ---------------------------------------------------------------------------

-- | Per-coefficient, per-output standard errors as a @p × q@ matrix:
-- @SE_{jk} = √(diag(Cov_k)_j)@.
lmStdErrorsMulti :: LA.Matrix Double -> FitResult -> LA.Matrix Double
lmStdErrorsMulti x res =
  let covs = lmCovarianceMulti x res
      cols = [ LA.cmap sqrt (LA.takeDiag c) | c <- covs ]
  in LA.fromColumns cols

-- | 1-output convenience: standard errors as a length-@p@ vector.
lmStdErrors :: LA.Matrix Double -> FitResult -> LA.Vector Double
lmStdErrors x res = LA.flatten (lmStdErrorsMulti x res)

-- ---------------------------------------------------------------------------
-- Coefficient stats (SE / t / two-sided p)
-- ---------------------------------------------------------------------------

-- | Per-coefficient inference triple: standard error, Wald @t@ value,
-- and two-sided @p@ value @2 × (1 − F_t(|t|; df))@.
data CoefStats = CoefStats
  { csSE     :: !Double
  , csTValue :: !Double
  , csPValue :: !Double
  } deriving (Show, Eq)

-- | Per-output 'CoefStats' for every coefficient. Returns a list of
-- @q@ lists, each of length @p@.
lmCoefStatsMulti :: LA.Matrix Double -> FitResult -> [[CoefStats]]
lmCoefStatsMulti x res =
  let n      = LA.rows x
      p      = LA.cols x
      df     = fromIntegral (n - p) :: Double
      tDist  = studentT df
      betaCs = LA.toColumns (coefficients res)
      seCs   = LA.toColumns (lmStdErrorsMulti x res)
      pair beta se =
        zipWith
          (\b s ->
              let t  = if s == 0 then 0 else b / s
                  pv = 2.0 * (1.0 - SD.cumulative tDist (abs t))
              in CoefStats s t pv)
          (LA.toList beta) (LA.toList se)
  in zipWith pair betaCs seCs

-- | 1-output convenience: 'CoefStats' for every coefficient.
lmCoefStats :: LA.Matrix Double -> FitResult -> [CoefStats]
lmCoefStats x res = head (lmCoefStatsMulti x res)

-- ---------------------------------------------------------------------------
-- F-statistic (whole-model)
-- ---------------------------------------------------------------------------

-- | Whole-model F-statistic and its right-tail @p@ value:
-- @F = ((TSS − RSS)/(p − 1)) / (RSS/(n − p))@,
-- @F ~ F(p − 1, n − p)@.
data FStat = FStat
  { fsValue  :: !Double
  , fsPValue :: !Double
  , fsDf1    :: !Int
  , fsDf2    :: !Int
  } deriving (Show, Eq)

-- | Whole-model F-statistic per output column. The first design-matrix
-- column is assumed to be the intercept (so the effective number of
-- predictors is @p − 1@). For @p ≤ 1@ or @n ≤ p@ returns @F = 0@,
-- @p = 1@.
lmFStatistic :: LA.Matrix Double -> FitResult -> [FStat]
lmFStatistic x res =
  let n    = LA.rows x
      p    = LA.cols x
      df1  = p - 1
      df2  = n - p
      yMat = fitted res + residuals res
      yCs  = LA.toColumns yMat
      rCs  = LA.toColumns (residuals res)
      go yj rj =
        if df1 <= 0 || df2 <= 0
          then FStat 0 1 (max df1 0) (max df2 0)
          else
            let yMean = LA.sumElements yj / fromIntegral (LA.size yj)
                dev   = LA.cmap (subtract yMean) yj
                tss   = dev `LA.dot` dev
                rss   = rj  `LA.dot` rj
                ess   = tss - rss
                fVal  = (ess / fromIntegral df1) / (rss / fromIntegral df2)
                pVal  = if rss == 0
                          then 0
                          else SD.complCumulative
                                 (FD.fDistribution df1 df2) fVal
            in FStat fVal pVal df1 df2
  in zipWith go yCs rCs

-- ---------------------------------------------------------------------------
-- Information criteria (Gaussian LM)
-- ---------------------------------------------------------------------------

-- | Gaussian log-likelihood, AIC, and BIC under the standard
-- @ε ~ N(0, σ²)@ assumption.
data ICs = ICs
  { icLogLik :: !Double
  , icAIC    :: !Double
  , icBIC    :: !Double
  } deriving (Show, Eq)

-- | Per-output information criteria.
--
-- @
-- logLik = −n/2 × (log(2π) + log(RSS/n) + 1)
-- AIC    = 2k − 2 × logLik              (k = p + 1, σ² counted)
-- BIC    = k × log(n) − 2 × logLik
-- @
lmInformationCriteriaMulti :: FitResult -> [ICs]
lmInformationCriteriaMulti res =
  let r    = residuals res
      n    = LA.rows r
      p    = LA.rows (coefficients res)
      k    = fromIntegral (p + 1) :: Double
      nD   = fromIntegral n       :: Double
      cols = LA.toColumns r
      go c =
        let rss    = c `LA.dot` c
            logLik = -nD / 2.0 *
                       (log (2.0 * pi) + log (rss / nD) + 1.0)
            aic    = 2.0 * k - 2.0 * logLik
            bic    = k * log nD - 2.0 * logLik
        in ICs logLik aic bic
  in map go cols

-- | 1-output convenience.
lmInformationCriteria :: FitResult -> ICs
lmInformationCriteria = head . lmInformationCriteriaMulti

-- ---------------------------------------------------------------------------
-- Residual diagnostics
-- ---------------------------------------------------------------------------

-- | Hat-matrix diagonal @h_ii = xᵢᵀ (XᵀX)⁻¹ xᵢ@. Returns a length-@n@
-- vector independent of the response.
hatDiagonal :: LA.Matrix Double -> LA.Vector Double
hatDiagonal x =
  let xtxi = LA.inv (LA.tr x LA.<> x)
      rows = LA.toRows x
  in LA.fromList [ xi `LA.dot` (xtxi LA.#> xi) | xi <- rows ]

-- | Internally studentised residual @r̃_i = r_i / (σ × √(1 − h_ii))@.
-- 1-output only (multi-output leverage is the same; the standardisation
-- divides by per-column @σ@). Returns a length-@n@ vector.
standardizedResiduals :: LA.Matrix Double -> FitResult -> LA.Vector Double
standardizedResiduals x res =
  let n      = LA.rows x
      p      = LA.cols x
      rj     = LA.flatten (residuals res)        -- assumes q = 1
      rss    = rj `LA.dot` rj
      sigma  = sqrt (rss / fromIntegral (n - p))
      h      = hatDiagonal x
      one h_ = max 0.0 (1.0 - h_)
  in LA.fromList
       [ if sigma == 0 || one hi == 0
           then 0
           else ri / (sigma * sqrt (one hi))
       | (ri, hi) <- zip (LA.toList rj) (LA.toList h) ]

-- | Cook's distance @D_i = (r̃_i² / p) × (h_ii / (1 − h_ii))@.
-- 1-output only. Returns a length-@n@ vector.
cooksDistance :: LA.Matrix Double -> FitResult -> LA.Vector Double
cooksDistance x res =
  let p    = fromIntegral (LA.cols x) :: Double
      h    = hatDiagonal x
      rTil = standardizedResiduals x res
  in LA.fromList
       [ let denom = max 0.0 (1.0 - hi)
         in if denom == 0
              then 0
              else (rTi * rTi / p) * (hi / denom)
       | (rTi, hi) <- zip (LA.toList rTil) (LA.toList h) ]

-- ---------------------------------------------------------------------------
-- Predictor utilities
-- ---------------------------------------------------------------------------

-- | Per-column sample standard deviation of the design matrix
-- (length @p@). Useful for standardised contribution
-- @|β_j × sd(x_j)| / Σ|β_k × sd(x_k)|@. The intercept column is
-- typically constant, so its entry is @0@.
predictorStdDevs :: LA.Matrix Double -> LA.Vector Double
predictorStdDevs x =
  let n  = fromIntegral (LA.rows x) :: Double
      cs = LA.toColumns x
      sd c =
        let mu  = LA.sumElements c / n
            dev = LA.cmap (subtract mu) c
            v   = (dev `LA.dot` dev) / max 1.0 (n - 1.0)
        in sqrt v
  in LA.fromList (map sd cs)
