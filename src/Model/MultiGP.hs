{-# LANGUAGE OverloadedStrings #-}
-- | 多出力 Gaussian Process (Phase T6)。
--
-- 最小実装として、各出力に独立に GP を fit する **Independent GPs** を提供。
-- これは Coregionalization model の ICM (Intrinsic Coregionalization Model)
-- で B = I の特殊形に等しい。
--
-- より高度な multi-task GP (出力間の相関を学習) は将来 Phase V (Bayesian
-- Optimization) で必要になれば拡張する。現状の独立 GP でも Bayesian MOO
-- では十分機能する (各 acquisition 関数が独立に評価できる)。
module Model.MultiGP
  ( MultiGPModel (..)
  , MultiGPResult (..)
  , mgpStd
  , fitMultiGP
  , predictMultiGP
  ) where

import Model.GP (Kernel (..), GPModel (..), GPParams, GPResult (..),
                 fitGP, optimizeGP, initParamsFromData)

-- | 多出力 GP モデル: 各出力に対して個別の Kernel + Hyperparameter。
-- 簡易のため全出力で同じ Kernel タイプを使う (lengthscale 等は個別最適化)。
data MultiGPModel = MultiGPModel
  { mgpKernel :: Kernel
  , mgpParams :: [GPParams]   -- 出力ごとの hyperparams
  } deriving (Show)

-- | 各出力ごとの GP fit 結果。
data MultiGPResult = MultiGPResult
  { mgpMean   :: [[Double]]   -- 各出力の予測平均 (q 個のリスト)
  , mgpLower  :: [[Double]]   -- 各出力の 95% lower (mean - 2σ)
  , mgpUpper  :: [[Double]]   -- 各出力の 95% upper (mean + 2σ)
  , mgpModels :: [GPModel]    -- 個別モデル (lookup 用)
  } deriving (Show)

-- | mgpUpper - mgpMean から std (= σ) を計算。
mgpStd :: MultiGPResult -> [[Double]]
mgpStd r = zipWith (zipWith (\m u -> (u - m) / 2)) (mgpMean r) (mgpUpper r)

-- | 多出力 GP を fit。各出力ごとに `optimizeGP` でハイパーパラメタを学習し、
-- 予測点 testX で評価する。
--
-- 引数:
--   * kernel — 全出力共通のカーネル種類
--   * trainX — 入力点 (1D, [Double])
--   * trainYs — 出力ごとの値 ([[Double]], 長さ q)
--   * testX — 予測点
fitMultiGP :: Kernel
           -> [Double]      -- trainX
           -> [[Double]]    -- trainYs (q 出力)
           -> [Double]      -- testX
           -> MultiGPResult
fitMultiGP kern trainX trainYs testX =
  let perOutput :: [Double] -> (GPModel, GPResult)
      perOutput trainY =
        let p0   = initParamsFromData trainX trainY
            pOpt = optimizeGP kern trainX trainY p0
            mdl  = GPModel kern pOpt
            res  = fitGP mdl trainX trainY testX
        in (mdl, res)
      pairs   = map perOutput trainYs
      models  = map fst pairs
      results = map snd pairs
  in MultiGPResult
       { mgpMean   = map gpMean   results
       , mgpLower  = map gpLower  results
       , mgpUpper  = map gpUpper  results
       , mgpModels = models
       }

-- | 既存の MultiGPResult を新しい test 点で再予測 (再フィット不要)。
predictMultiGP :: MultiGPModel
               -> [Double] -> [[Double]] -> [Double]   -- trainX, trainYs, testX
               -> MultiGPResult
predictMultiGP mgp trainX trainYs testX =
  let kern    = mgpKernel mgp
      models  = zipWith (\p _ -> GPModel kern p) (mgpParams mgp) trainYs
      results = zipWith3 (\m _ ty -> fitGP m trainX ty testX)
                         models trainYs trainYs
  in MultiGPResult
       { mgpMean   = map gpMean   results
       , mgpLower  = map gpLower  results
       , mgpUpper  = map gpUpper  results
       , mgpModels = models
       }
