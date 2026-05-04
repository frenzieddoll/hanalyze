{-# LANGUAGE OverloadedStrings #-}
-- | Multi-response Response Surface Methodology.
--
-- Fits a quadratic model to each response @y_j@ and performs the extremum
-- analysis @q@ times in parallel. As a starting point for multi-objective
-- optimization, this presents the individual optimum of each response.
module Design.MultiRSM
  ( MultiQuadFit (..)
  , fitMultiQuadratic
  , optimumPointsMulti
  ) where

import qualified Numeric.LinearAlgebra as LA
import Design.RSM (QuadFit (..), fitQuadratic, optimumPoint)

-- | Aggregated multi-response quadratic fit.
data MultiQuadFit = MultiQuadFit
  { mqFits :: [QuadFit]   -- ^ Per-response quadratic fits (length @q@).
  , mqK    :: Int         -- ^ Number of factors @k@.
  , mqQ    :: Int         -- ^ Number of responses @q@.
  } deriving (Show)

-- | Multi-response quadratic regression: apply 'fitQuadratic' to each
-- response column independently.
fitMultiQuadratic :: [[Double]]            -- ^ Design matrix (@n × k@).
                  -> LA.Matrix Double      -- ^ Response @Y@ (@n × q@).
                  -> MultiQuadFit
fitMultiQuadratic design y =
  let q = LA.cols y
      k = if null design then 0 else length (head design)
      colFit j = fitQuadratic design (LA.toList (LA.flatten (y LA.¿ [j])))
      fits = [colFit j | j <- [0 .. q - 1]]
  in MultiQuadFit fits k q

-- | Compute 'optimumPoint' for each response and aggregate the
-- extremum information.
optimumPointsMulti :: MultiQuadFit -> [([Double], Double, [Double])]
optimumPointsMulti mq = map optimumPoint (mqFits mq)
