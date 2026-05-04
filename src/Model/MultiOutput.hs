-- | Common foundation for multi-output regression.
--
-- Design policy:
--
--   * Each model's /primary/ API takes the response @Y@ as
--     @LA.Matrix Double@ (@n × q@) and returns a matrix; the @q = 1@ case
--     is a specialization.
--   * The single-output API (@V.Vector Double@) is a thin wrapper that
--     promotes the response to a one-column matrix via 'asMultiY' /
--     'fromMultiY' and reuses the multi-output implementation.
--   * Per-output evaluation metrics (R² etc.) are collected here.
module Model.MultiOutput
  ( -- * 単出力 ↔ 多出力 変換
    asMultiY
  , fromMultiY
  , asMultiYV
    -- * 多出力評価指標
  , rmseMulti
  , r2Multi
  , mseMulti
  ) where

import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA

-- ---------------------------------------------------------------------------
-- 変換
-- ---------------------------------------------------------------------------

-- | 1D ベクトルを n×1 行列に昇格。
asMultiY :: V.Vector Double -> LA.Matrix Double
asMultiY = LA.asColumn . LA.fromList . V.toList

-- | hmatrix 'LA.Vector' を n×1 行列に。
asMultiYV :: LA.Vector Double -> LA.Matrix Double
asMultiYV = LA.asColumn

-- | n×1 行列から 1D ベクトルへ。q ≠ 1 のときは最初の列を返す。
fromMultiY :: LA.Matrix Double -> V.Vector Double
fromMultiY m
  | LA.cols m == 0 = V.empty
  | otherwise      = V.fromList (LA.toList (LA.flatten (m LA.¿ [0])))

-- ---------------------------------------------------------------------------
-- 評価指標
-- ---------------------------------------------------------------------------

-- | 全要素 MSE (sum-of-squares / n / q)。
mseMulti :: LA.Matrix Double -> LA.Matrix Double -> Double
mseMulti ys yhat =
  let n = LA.rows ys
      q = LA.cols ys
      r = ys - yhat
  in LA.sumElements (r * r) / fromIntegral (n * q)

-- | 全要素 RMSE。
rmseMulti :: LA.Matrix Double -> LA.Matrix Double -> Double
rmseMulti ys yhat = sqrt (mseMulti ys yhat)

-- | 列ごと R² (長さ q ベクトル)。
r2Multi :: LA.Matrix Double -> LA.Matrix Double -> V.Vector Double
r2Multi ys yhat =
  let n  = LA.rows ys
      q  = LA.cols ys
      colR2 j =
        let yc  = LA.toList (LA.flatten (ys   LA.¿ [j]))
            yhc = LA.toList (LA.flatten (yhat LA.¿ [j]))
            mu  = sum yc / fromIntegral n
            sst = sum [(y - mu)^(2::Int) | y <- yc]
            sse = sum [(y - p)^(2::Int) | (y, p) <- zip yc yhc]
        in if sst == 0 then 0 else 1 - sse / sst
  in V.fromList [ colR2 j | j <- [0 .. q - 1] ]
