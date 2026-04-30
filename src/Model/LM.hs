module Model.LM
  ( LinearModel (..)
  , CIBand (..)
  , SmoothFit (..)
  , fitLM
  , designMatrix
  , polyDesignMatrix
  , multiPolyDesignMatrix
  , linspace
  , fitDataFrameLM
  , confidenceBand
  , fitWithCI
  , fitPolyWithSmooth
  ) where

import DataFrame.Core
import Model.Core (FitResult (..), Model (..), Band (..), fittedList)

import Data.Text (Text)
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import Statistics.Distribution (quantile)
import Statistics.Distribution.StudentT (studentT)

data LinearModel = LinearModel
  deriving (Show)

instance Model LinearModel where
  fit     _ = fitLM
  predict _ = predictLM

-- | Ordinary Least Squares: β = (XᵀX)⁻¹Xᵀy  (via QR decomposition in hmatrix)
fitLM :: LA.Matrix Double -> LA.Vector Double -> FitResult
fitLM x y =
  let beta  = LA.flatten (x LA.<\> LA.asColumn y)
      yHat  = x LA.#> beta
      resid = y - yHat
      r2    = computeR2 y yHat
  in FitResult beta yHat resid r2

predictLM :: LA.Vector Double -> LA.Matrix Double -> LA.Vector Double
predictLM beta xNew = xNew LA.#> beta

-- | Build intercept + single predictor design matrix  [1, x].
designMatrix :: V.Vector Double -> LA.Matrix Double
designMatrix xs = LA.fromColumns
  [ LA.konst 1.0 n
  , LA.fromList (V.toList xs)
  ]
  where n = V.length xs

-- | Convenience: fit a simple LM directly from a DataFrame.
fitDataFrameLM :: DataFrame -> Text -> Text -> Maybe FitResult
fitDataFrameLM df xCol yCol = do
  xVec <- getNumeric xCol df
  yVec <- getNumeric yCol df
  let dm = designMatrix xVec
      y  = LA.fromList (V.toList yVec)
  return (fitLM dm y)

data CIBand = CIBand
  { lowerBound :: [Double]
  , upperBound :: [Double]
  , ciLevel    :: Double
  } deriving (Show)

-- | Pointwise confidence band for the mean response.
-- Formula: ŷᵢ ± t_{α/2, n−p} × sqrt(s² × xᵢᵀ (XᵀX)⁻¹ xᵢ)
confidenceBand :: LA.Matrix Double -> FitResult -> Double -> CIBand
confidenceBand x res level =
  let df   = fromIntegral (LA.rows x - LA.cols x)
      s2   = (residuals res `LA.dot` residuals res) / df
      xtxi = LA.inv (LA.tr x LA.<> x)
      tVal = quantile (studentT df) ((1.0 + level) / 2.0)
      se xi = tVal * sqrt (s2 * (xi `LA.dot` (xtxi LA.#> xi)))
      rows  = LA.toRows x
      yHats = fittedList res
      lowers = zipWith (\yh xi -> yh - se xi) yHats rows
      uppers = zipWith (\yh xi -> yh + se xi) yHats rows
  in CIBand lowers uppers level

-- | Fit LM and compute confidence band in one step.
fitWithCI :: Double -> DataFrame -> Text -> Text -> Maybe (FitResult, CIBand)
fitWithCI level df xCol yCol = do
  xVec <- getNumeric xCol df
  yVec <- getNumeric yCol df
  let dm  = designMatrix xVec
      y   = LA.fromList (V.toList yVec)
      res = fitLM dm y
  return (res, confidenceBand dm res level)

-- | Polynomial design matrix [1, x, x², …, xᵈ].
polyDesignMatrix :: Int -> V.Vector Double -> LA.Matrix Double
polyDesignMatrix degree xs = LA.fromColumns
  [ LA.fromList [ x ^ k | x <- V.toList xs ]
  | k <- [0 .. degree]
  ]

-- | Multi-column polynomial design matrix.
-- Builds [1, x1, x1², …, x1^d1, x2, …, x2^d2, …] from a list of (column, degree) pairs.
multiPolyDesignMatrix :: [(V.Vector Double, Int)] -> LA.Matrix Double
multiPolyDesignMatrix [] = error "multiPolyDesignMatrix: empty predictor list"
multiPolyDesignMatrix colDegs@((firstXs, _) : _) =
  LA.fromColumns (intercept : concatMap polyExpand colDegs)
  where
    n           = V.length firstXs
    intercept   = LA.konst 1.0 n
    polyExpand (xs, deg) =
      [ LA.fromList [ x ^ k | x <- V.toList xs ] | k <- [1 .. deg] ]

-- | Grid of evenly spaced values from lo to hi.
linspace :: Double -> Double -> Int -> [Double]
linspace lo hi n
  | n <= 1    = [lo]
  | otherwise = [ lo + fromIntegral i * (hi - lo) / fromIntegral (n - 1)
                | i <- [0 .. n - 1] ]

-- | Pre-computed smooth curve data for plotting (evaluated on a fine grid).
data SmoothFit = SmoothFit
  { sfX       :: [Double]  -- grid x values
  , sfFit     :: [Double]  -- predicted y at grid points
  , sfLower   :: [Double]  -- band lower bound (same as sfFit when sfHasBand = False)
  , sfUpper   :: [Double]  -- band upper bound (same as sfFit when sfHasBand = False)
  , sfHasBand :: Bool      -- whether to render the uncertainty band
  } deriving (Show)

-- | Fit polynomial LM of given degree and compute a smooth curve with optional band
-- on a fine grid of nGrid points for clean visualisation.
fitPolyWithSmooth
  :: Band       -- ^ uncertainty band specification
  -> Int        -- ^ grid resolution for smooth curve
  -> DataFrame
  -> Text       -- ^ x column
  -> Text       -- ^ y column
  -> Maybe (FitResult, SmoothFit)
fitPolyWithSmooth band nGrid df xCol yCol = do
  xVec <- getNumeric xCol df
  yVec <- getNumeric yCol df
  let degree = 1  -- kept for backward-compat; use fitGLMWithSmooth for poly
      dm     = polyDesignMatrix degree xVec
      y      = LA.fromList (V.toList yVec)
      res    = fitLM dm y
      beta   = coefficients res

      xLa    = LA.fromList (V.toList xVec)
      xGrid  = V.fromList (linspace (LA.minElement xLa) (LA.maxElement xLa) nGrid)
      dmG    = polyDesignMatrix degree xGrid
      yGrid  = LA.toList (dmG LA.#> beta)

      dfStat = fromIntegral (LA.rows dm - LA.cols dm) :: Double
      s2     = (residuals res `LA.dot` residuals res) / dfStat
      xtxi   = LA.inv (LA.tr dm LA.<> dm)
      gRows  = LA.toRows dmG

      computeBand level isPI =
        let tVal   = quantile (studentT dfStat) ((1.0 + level) / 2.0)
            extra  = if isPI then 1.0 else 0.0
            halfW xi = tVal * sqrt (s2 * (extra + xi `LA.dot` (xtxi LA.#> xi)))
            lowers = zipWith (\yh xi -> yh - halfW xi) yGrid gRows
            uppers = zipWith (\yh xi -> yh + halfW xi) yGrid gRows
        in (lowers, uppers)

  case band of
    NoBand ->
      return (res, SmoothFit (V.toList xGrid) yGrid yGrid yGrid False)
    CI level ->
      let (lowers, uppers) = computeBand level False
      in return (res, SmoothFit (V.toList xGrid) yGrid lowers uppers True)
    PI level ->
      let (lowers, uppers) = computeBand level True
      in return (res, SmoothFit (V.toList xGrid) yGrid lowers uppers True)

-- | R² = 1 − SS_res / SS_tot
computeR2 :: LA.Vector Double -> LA.Vector Double -> Double
computeR2 y yHat =
  let resid  = y - yHat
      yMean  = LA.sumElements y / fromIntegral (LA.size y)
      dev    = LA.cmap (subtract yMean) y
      ssRes  = resid `LA.dot` resid
      ssTot  = dev   `LA.dot` dev
  in 1.0 - ssRes / ssTot
