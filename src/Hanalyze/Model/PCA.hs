{-# LANGUAGE OverloadedStrings #-}
-- | Principal Component Analysis (PCA) and related dimensionality
-- reduction.
--
-- @
-- import Hanalyze.Model.PCA
--
-- let pcaRes = pca True x  -- center + scale
--     loadings = pcaComponents pcaRes
--     scores   = pcaTransform pcaRes x  -- project x onto components
-- @
--
-- * 'pca' fits PCA to a centred (and optionally scaled) feature matrix.
-- * 'pcaTransform' projects new data onto the learned components.
-- * 'pcaInverse' reconstructs from scores back to feature space.
-- * @screePlot@ / @biplot@ integration via @Viz@ (separate module).
module Hanalyze.Model.PCA
  ( -- * PCA
    PCAResult (..)
  , PCAStandardize (..)
  , pca
  , pcaTransform
  , pcaInverse
  , pcaCumExplained
    -- * Helpers
  , standardizeFeatures
  ) where

import qualified Numeric.LinearAlgebra as LA

-- | Standardisation mode for input features before SVD.
data PCAStandardize
  = NoStandardize
    -- ^ Do not center or scale (only useful when columns already have
    --   zero mean and comparable units).
  | Center
    -- ^ Subtract column means (default behaviour for PCA).
  | CenterScale
    -- ^ Subtract means and divide by sample standard deviations
    --   (= standardised PCA, AKA correlation-matrix PCA).
  deriving (Show, Eq)

-- | Result of fitting PCA. All matrices share the same number of
-- components @k@; if the user passed @k = Nothing@ then
-- @k = min(n, p)@.
data PCAResult = PCAResult
  { pcaMean       :: !(LA.Vector Double)
    -- ^ Per-column mean of the training data (length @p@).
  , pcaScale      :: !(LA.Vector Double)
    -- ^ Per-column standard deviation (length @p@). All ones when
    --   'pcaStandardize' is 'NoStandardize' / 'Center'.
  , pcaStandardize :: !PCAStandardize
  , pcaComponents :: !(LA.Matrix Double)
    -- ^ Principal axes (@loadings@), shape @k × p@. Rows are unit
    --   vectors; PC@i@ corresponds to row @i@.
  , pcaSingularValues :: !(LA.Vector Double)
    -- ^ Singular values @σ_i@, length @k@. Sorted descending.
  , pcaExplainedVar :: !(LA.Vector Double)
    -- ^ Variance of each component (= σ_i² / (n − 1)). Length @k@.
  , pcaExplainedRatio :: !(LA.Vector Double)
    -- ^ Fraction of total variance explained by each component, length
    --   @k@. Sums to ≤ 1; equals 1 when k = rank(X).
  , pcaNSamples   :: !Int
  , pcaNFeatures  :: !Int
  } deriving (Show)

-- | Center (and optionally scale) a feature matrix. Returns the
-- transformed matrix along with the column means and per-column
-- standard deviations.
standardizeFeatures
  :: PCAStandardize
  -> LA.Matrix Double  -- ^ X (n × p)
  -> (LA.Matrix Double, LA.Vector Double, LA.Vector Double)
       -- ^ (Z, μ, σ).
standardizeFeatures std x =
  let n    = LA.rows x
      p    = LA.cols x
      ones = LA.konst 1 n :: LA.Vector Double
      mu   = LA.scale (1 / fromIntegral n) (ones LA.<# x)
      xC   = x - LA.fromRows (replicate n mu)
  in case std of
       NoStandardize ->
         (x, LA.konst 0 p, LA.konst 1 p)
       Center ->
         (xC, mu, LA.konst 1 p)
       CenterScale ->
         let sd2 = LA.scale (1 / fromIntegral (n - 1))
                     (LA.konst 1 n LA.<# (xC * xC))
             sd  = LA.cmap (\v -> if v < 1e-12 then 1 else sqrt v) sd2
             z   = xC LA.<> LA.diag (LA.cmap (1 /) sd)
         in (z, mu, sd)

-- | Fit PCA on a feature matrix.
--
-- Internally uses thin SVD on the (centred / scaled) matrix so the
-- cost is @O(min(n²p, np²))@. The first @k@ rows of @Vᵀ@ are the
-- principal axes; the singular values @σ@ give component magnitudes.
pca
  :: PCAStandardize
  -> Maybe Int          -- ^ k (number of components to keep). Nothing = all.
  -> LA.Matrix Double   -- ^ X (n × p)
  -> PCAResult
pca std mK x =
  let (z, mu, sd) = standardizeFeatures std x
      n           = LA.rows z
      p           = LA.cols z
      -- Thin SVD: z = U S Vᵀ, where U is n×r, S is r-vector, V is p×r.
      (u, s, vT)  = LA.thinSVD z
      _           = u
      kMax        = min (LA.rows z) (LA.cols z)
      k           = min kMax (maybe kMax id mK)
      -- Keep first k components.
      sK          = LA.subVector 0 k s
      -- 'thinSVD' returns Vᵀ as p × min(n,p); we want first k rows of
      -- Vᵀ (= first k columns of V transposed).
      vTk         = vT LA.?? (LA.All, LA.Take k)
      components  = LA.tr vTk            -- k × p
      -- Variance per component = σ² / (n − 1).
      varK        = LA.cmap (\sv -> sv * sv / fromIntegral (max 1 (n - 1))) sK
      totalVar    = LA.sumElements
                      (LA.cmap (\sv -> sv * sv / fromIntegral (max 1 (n - 1))) s)
      ratio       = if totalVar > 0
                      then LA.scale (1 / totalVar) varK
                      else LA.konst 0 k
  in PCAResult
       { pcaMean           = mu
       , pcaScale          = sd
       , pcaStandardize    = std
       , pcaComponents     = components
       , pcaSingularValues = sK
       , pcaExplainedVar   = varK
       , pcaExplainedRatio = ratio
       , pcaNSamples       = n
       , pcaNFeatures      = p
       }

-- | Project new data onto the learned principal components.
-- Returns scores of shape @m × k@ where @m@ is the number of new
-- samples.
pcaTransform :: PCAResult -> LA.Matrix Double -> LA.Matrix Double
pcaTransform r x =
  let m  = LA.rows x
      mu = pcaMean r
      sd = pcaScale r
      xC = x - LA.fromRows (replicate m mu)
      z  = case pcaStandardize r of
             NoStandardize -> x
             Center        -> xC
             CenterScale   -> xC LA.<> LA.diag (LA.cmap (1 /) sd)
  in z LA.<> LA.tr (pcaComponents r)        -- m × k

-- | Reconstruct from scores back to feature space (approximation when
-- not all components are kept). Inverse of 'pcaTransform' modulo
-- truncation error.
pcaInverse :: PCAResult -> LA.Matrix Double -> LA.Matrix Double
pcaInverse r scores =
  let m       = LA.rows scores
      mu      = pcaMean r
      sd      = pcaScale r
      zRecon  = scores LA.<> pcaComponents r          -- m × p
      xRecon  = case pcaStandardize r of
        NoStandardize -> zRecon
        Center        -> zRecon + LA.fromRows (replicate m mu)
        CenterScale   ->
          let unscaled = zRecon LA.<> LA.diag sd
          in unscaled + LA.fromRows (replicate m mu)
  in xRecon

-- | Cumulative explained variance ratio (length k).
pcaCumExplained :: PCAResult -> LA.Vector Double
pcaCumExplained r =
  let ratio = LA.toList (pcaExplainedRatio r)
      cum   = scanl1 (+) ratio
  in LA.fromList cum
