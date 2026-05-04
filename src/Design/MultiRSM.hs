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

-- | 各応答ごとの二次フィット結果を集約。
data MultiQuadFit = MultiQuadFit
  { mqFits :: [QuadFit]      -- 応答ごと (q 個)
  , mqK    :: Int            -- 因子数
  , mqQ    :: Int            -- 応答数
  } deriving (Show)

-- | 多目的二次回帰: 各応答列に独立に fitQuadratic を適用。
fitMultiQuadratic :: [[Double]]            -- 設計 (n × k)
                  -> LA.Matrix Double      -- Y (n × q)
                  -> MultiQuadFit
fitMultiQuadratic design y =
  let q = LA.cols y
      k = if null design then 0 else length (head design)
      colFit j = fitQuadratic design (LA.toList (LA.flatten (y LA.¿ [j])))
      fits = [colFit j | j <- [0 .. q - 1]]
  in MultiQuadFit fits k q

-- | 各応答について 'optimumPoint' を呼び、極値情報を集約。
optimumPointsMulti :: MultiQuadFit -> [([Double], Double, [Double])]
optimumPointsMulti mq = map optimumPoint (mqFits mq)
