{-# LANGUAGE OverloadedStrings #-}
-- | 多目的 Response Surface Methodology (Phase U1)。
--
-- 各応答 y_j に対して二次モデルを fit し、極値解析を q 個並列に行う。
-- 多目的最適化の出発点として、各応答の単独最適点を提示する。
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
