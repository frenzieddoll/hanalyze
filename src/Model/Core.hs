module Model.Core
  ( FitResult (..)
  , Model (..)
  , Band (..)
  , fittedList
  , coeffList
  ) where

import qualified Numeric.LinearAlgebra as LA

-- | Common result type for all fitted models.
data FitResult = FitResult
  { coefficients :: LA.Vector Double  -- fitted parameters β
  , fitted       :: LA.Vector Double  -- ŷ = Xβ (or link-transformed)
  , residuals    :: LA.Vector Double  -- y − ŷ
  , rSquared     :: Double            -- R² (or pseudo-R²)
  } deriving (Show)

fittedList :: FitResult -> [Double]
fittedList = LA.toList . fitted

coeffList :: FitResult -> [Double]
coeffList = LA.toList . coefficients

-- | Uncertainty band drawn around a fitted curve.
data Band
  = NoBand      -- ^ no band
  | CI Double   -- ^ confidence interval for the mean response at given level
  | PI Double   -- ^ prediction interval for individual obs at given level (Gaussian only)
  deriving (Show, Eq)

-- | Minimal interface every model must implement.
class Model m where
  fit     :: m -> LA.Matrix Double -> LA.Vector Double -> FitResult
  predict :: m -> LA.Vector Double -> LA.Matrix Double -> LA.Vector Double
