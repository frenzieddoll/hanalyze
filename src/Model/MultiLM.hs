{-# LANGUAGE OverloadedStrings #-}
-- | Multivariate Linear Regression (多出力線形回帰)。
--
-- @Y = XB + E@、Y は n × q (q 出力)、X は n × p、B は p × q、E は n × q。
--
-- 各 j 列について OLS を独立に解く (= column-wise OLS)。
-- 残差の **共分散行列 Σ** も推定し、後続の多目的予測区間計算等に使う。
--
-- API は `Model.LM` と統一されており、`fitLM` をそのまま呼ぶことも可能だが、
-- このモジュールは多出力時の追加情報 (Σ、相関行列) を提供する。
module Model.MultiLM
  ( MultiFit (..)
  , fitMultiLM
  , predictMultiLM
  , residualCovariance
  , residualCorrelation
  ) where

import qualified Numeric.LinearAlgebra as LA
import Model.Core (FitResult (..))
import qualified Model.LM as LM

-- | 多出力線形回帰の追加結果。
data MultiFit = MultiFit
  { mfFit         :: FitResult              -- ^ 基本フィット結果 (Matrix-based)
  , mfResidCov    :: LA.Matrix Double       -- ^ 残差の共分散行列 Σ (q × q)
  , mfResidCor    :: LA.Matrix Double       -- ^ 残差の相関行列 (q × q)
  , mfNumOutputs  :: Int                    -- ^ q (出力次元)
  , mfNumPredict  :: Int                    -- ^ p (説明変数の次元)
  , mfNumSamples  :: Int                    -- ^ n (観測数)
  } deriving (Show)

-- | 多出力線形回帰: Y = XB + E。
-- 内部は 'LM.fitLM' をそのまま使い、追加で残差共分散を計算。
fitMultiLM :: LA.Matrix Double  -- X (n × p)
           -> LA.Matrix Double  -- Y (n × q)
           -> MultiFit
fitMultiLM x y =
  let fit = LM.fitLM x y
      res = residuals fit
      n   = LA.rows y
      q   = LA.cols y
      p   = LA.cols x
      df  = max 1 (n - p)   -- 自由度補正
      -- Σ = (1/(n-p)) * Eᵀ E
      sigma = LA.scale (1 / fromIntegral df)
                       (LA.tr res LA.<> res)
      -- 相関行列: D⁻¹ Σ D⁻¹ where D = diag(sqrt(diag(Σ)))
      diagS = [ sqrt (sigma `LA.atIndex` (i, i))
              | i <- [0 .. q - 1] ]
      corr  = LA.fromLists
        [ [ if di == 0 || dj == 0 then 0
            else (sigma `LA.atIndex` (i, j)) / (di * dj)
          | j <- [0 .. q - 1]
          , let dj = diagS !! j ]
        | i <- [0 .. q - 1]
        , let di = diagS !! i ]
  in MultiFit fit sigma corr q p n

-- | 予測。X_new (m × p) → Ŷ (m × q)。
-- LM.predictLM の薄いラッパ。
predictMultiLM :: MultiFit -> LA.Matrix Double -> LA.Matrix Double
predictMultiLM mf xNew =
  LM.predictLM (coefficients (mfFit mf)) xNew

-- | 残差共分散行列を取得 (mfResidCov のエイリアス)。
residualCovariance :: MultiFit -> LA.Matrix Double
residualCovariance = mfResidCov

-- | 残差相関行列を取得。
residualCorrelation :: MultiFit -> LA.Matrix Double
residualCorrelation = mfResidCor
