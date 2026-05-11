-- | Ordinary linear regression by least squares.
--
-- Solves @β = (XᵀX)⁻¹ Xᵀ y@ via hmatrix's @\\\\@ (LAPACK). Provides
-- confidence and prediction bands using
-- @t × √(s² xᵢᵀ(XᵀX)⁻¹xᵢ)@ and convenient adapters from a
-- @DataFrame@ for use from the CLI and report builder.
module Model.LM
  ( LinearModel (..)
  , CIBand (..)
  , SmoothFit (..)
    -- * Matrix-canonical fit
  , fitLM
  , predictLM
    -- * Vector wrapper (1-output convenience)
  , fitLMVec
  , predictLMVec
    -- * Design matrices
  , designMatrix
  , polyDesignMatrix
  , multiPolyDesignMatrix
  , linspace
    -- * DataFrame helpers
  , fitDataFrameLM
  , confidenceBand
  , fitWithCI
  , fitPolyWithSmooth
  ) where

import qualified DataFrame.Internal.DataFrame as DXD
import DataIO.Convert (getDoubleVec)
import Model.Core (FitResult (..), Model (..), Band (..),
                   coefficientsV, residualsV, fittedList)

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

-- | Ordinary Least Squares (Matrix canonical, 多出力対応):
-- B = (XᵀX)⁻¹ Xᵀ Y、各列を独立に解く。
fitLM :: LA.Matrix Double -> LA.Matrix Double -> FitResult
fitLM x y =
  let beta  = x LA.<\> y                   -- p × q
      yHat  = x LA.<> beta                 -- n × q
      resid = y - yHat
      r2    = computeR2Multi y yHat
  in FitResult beta yHat resid r2

predictLM :: LA.Matrix Double -> LA.Matrix Double -> LA.Matrix Double
predictLM beta xNew = xNew LA.<> beta

-- | 単一出力 (Vector y) の便利ラッパ。@asColumn@ で 1 列行列に変換。
fitLMVec :: LA.Matrix Double -> LA.Vector Double -> FitResult
fitLMVec x y = fitLM x (LA.asColumn y)

-- | 1 出力での予測 (β は Vector)。
predictLMVec :: LA.Vector Double -> LA.Matrix Double -> LA.Vector Double
predictLMVec beta xNew = xNew LA.#> beta

-- | Build intercept + single predictor design matrix  [1, x].
designMatrix :: V.Vector Double -> LA.Matrix Double
designMatrix xs = LA.fromColumns
  [ LA.konst 1.0 n
  , LA.fromList (V.toList xs)
  ]
  where n = V.length xs

-- | Convenience: fit a simple LM directly from a DataFrame.
fitDataFrameLM :: DXD.DataFrame -> Text -> Text -> Maybe FitResult
fitDataFrameLM df xCol yCol = do
  xVec <- getDoubleVec xCol df
  yVec <- getDoubleVec yCol df
  let dm = designMatrix xVec
      y  = LA.fromList (V.toList yVec)
  return (fitLMVec dm y)

data CIBand = CIBand
  { lowerBound :: [Double]
  , upperBound :: [Double]
  , ciLevel    :: Double
  } deriving (Show)

-- | Pointwise confidence band for the mean response (1 出力前提)。
-- Formula: ŷᵢ ± t_{α/2, n−p} × sqrt(s² × xᵢᵀ (XᵀX)⁻¹ xᵢ)
confidenceBand :: LA.Matrix Double -> FitResult -> Double -> CIBand
confidenceBand x res level =
  let df    = fromIntegral (LA.rows x - LA.cols x)
      resV  = residualsV res
      s2    = (resV `LA.dot` resV) / df
      xtxi  = LA.inv (LA.tr x LA.<> x)
      tVal  = quantile (studentT df) ((1.0 + level) / 2.0)
      se xi = tVal * sqrt (s2 * (xi `LA.dot` (xtxi LA.#> xi)))
      rs    = LA.toRows x
      yHats = fittedList res
      los   = zipWith (\yh xi -> yh - se xi) yHats rs
      his   = zipWith (\yh xi -> yh + se xi) yHats rs
  in CIBand los his level

-- | Fit LM and compute confidence band in one step.
fitWithCI :: Double -> DXD.DataFrame -> Text -> Text -> Maybe (FitResult, CIBand)
fitWithCI level df xCol yCol = do
  xVec <- getDoubleVec xCol df
  yVec <- getDoubleVec yCol df
  let dm  = designMatrix xVec
      y   = LA.fromList (V.toList yVec)
      res = fitLMVec dm y
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
  { sfX       :: [Double]
  , sfFit     :: [Double]
  , sfLower   :: [Double]
  , sfUpper   :: [Double]
  , sfHasBand :: Bool
  } deriving (Show)

-- | Fit polynomial LM of given degree and compute a smooth curve with optional band
-- on a fine grid of nGrid points for clean visualisation.
fitPolyWithSmooth
  :: Band
  -> Int
  -> DXD.DataFrame
  -> Text
  -> Text
  -> Maybe (FitResult, SmoothFit)
fitPolyWithSmooth band nGrid df xCol yCol = do
  xVec <- getDoubleVec xCol df
  yVec <- getDoubleVec yCol df
  let degree = 1
      dm     = polyDesignMatrix degree xVec
      y      = LA.fromList (V.toList yVec)
      res    = fitLMVec dm y
      beta   = coefficientsV res

      xLa    = LA.fromList (V.toList xVec)
      xGrid  = V.fromList (linspace (LA.minElement xLa) (LA.maxElement xLa) nGrid)
      dmG    = polyDesignMatrix degree xGrid
      yGrid  = LA.toList (dmG LA.#> beta)

      dfStat = fromIntegral (LA.rows dm - LA.cols dm) :: Double
      resV   = residualsV res
      s2     = (resV `LA.dot` resV) / dfStat
      xtxi   = LA.inv (LA.tr dm LA.<> dm)
      gRows  = LA.toRows dmG

      computeBand level isPI =
        let tVal   = quantile (studentT dfStat) ((1.0 + level) / 2.0)
            extra  = if isPI then 1.0 else 0.0
            halfW xi = tVal * sqrt (s2 * (extra + xi `LA.dot` (xtxi LA.#> xi)))
            los    = zipWith (\yh xi -> yh - halfW xi) yGrid gRows
            his    = zipWith (\yh xi -> yh + halfW xi) yGrid gRows
        in (los, his)

  case band of
    NoBand ->
      return (res, SmoothFit (V.toList xGrid) yGrid yGrid yGrid False)
    CI level ->
      let (los, his) = computeBand level False
      in return (res, SmoothFit (V.toList xGrid) yGrid los his True)
    PI level ->
      let (los, his) = computeBand level True
      in return (res, SmoothFit (V.toList xGrid) yGrid los his True)

-- | 各列ごとに R² を計算 (多出力対応)。
computeR2Multi :: LA.Matrix Double -> LA.Matrix Double -> LA.Vector Double
computeR2Multi y yHat =
  let q = LA.cols y
  in LA.fromList
       [ let yj    = LA.flatten (y    LA.¿ [j])
             yhj   = LA.flatten (yHat LA.¿ [j])
             resid = yj - yhj
             yMean = LA.sumElements yj / fromIntegral (LA.size yj)
             dev   = LA.cmap (subtract yMean) yj
             ssRes = resid `LA.dot` resid
             ssTot = dev   `LA.dot` dev
         in if ssTot == 0 then 0
              else 1.0 - ssRes / ssTot
       | j <- [0 .. q - 1] ]
