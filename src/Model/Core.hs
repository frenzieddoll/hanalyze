module Model.Core
  ( FitResult (..)
  , Model (..)
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

-- | Minimal interface every model must implement.
class Model m where
  fit     :: m -> LA.Matrix Double -> LA.Vector Double -> FitResult
  predict :: m -> LA.Vector Double -> LA.Matrix Double -> LA.Vector Double
