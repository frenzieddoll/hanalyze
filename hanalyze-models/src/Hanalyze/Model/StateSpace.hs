{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Hanalyze.Model.StateSpace
-- Description : 線形ガウス状態空間モデルの Kalman Filter / RTS Smoother
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 線形ガウス状態空間モデル (Linear Gaussian State Space Model) +
-- Kalman Filter / RTS Smoother。
--
-- モデル:
--
-- @
-- x_t = F x_{t-1} + w_t,   w_t ~ N(0, Q)
-- y_t = H x_t     + v_t,   v_t ~ N(0, R)
-- @
--
-- - 'kalmanFilter' は前向きフィルタリングで filtered mean / cov を計算し、
--   同時に innovation 系列の対数尤度 (= モデル尤度) を返す。
-- - 'kalmanSmoother' は RTS (Rauch-Tung-Striebel) で smoothed mean / cov を
--   後ろ向きに計算。 入力に既にフィルタ済の 'KalmanResult' を渡す。
--
-- すべて hmatrix Vector / Matrix で実装 (list 化禁止)。
--
-- [English]: The Linear Gaussian State Space Model plus a Kalman Filter \/
-- RTS Smoother.
--
-- Model:
--
-- @
-- x_t = F x_{t-1} + w_t,   w_t ~ N(0, Q)
-- y_t = H x_t     + v_t,   v_t ~ N(0, R)
-- @
--
-- - 'kalmanFilter' computes the filtered mean \/ covariance via forward
--   filtering, and simultaneously returns the log-likelihood of the
--   innovation sequence (= the model likelihood).
-- - 'kalmanSmoother' computes the smoothed mean \/ covariance backward via
--   RTS (Rauch-Tung-Striebel). It takes an already-filtered 'KalmanResult'
--   as input.
--
-- Implemented entirely with hmatrix Vector \/ Matrix (converting to lists is
-- forbidden).
module Hanalyze.Model.StateSpace
  ( StateSpaceModel (..)
  , KalmanResult (..)
  , kalmanFilter
  , kalmanSmoother
  ) where

import qualified Numeric.LinearAlgebra as LA

-- ===========================================================================
-- 型
-- ===========================================================================

data StateSpaceModel = StateSpaceModel
  { ssF  :: !(LA.Matrix Double)   -- ^ [日本語]: 状態遷移行列 F (n_x × n_x)。 [English]: The state transition matrix F (n_x × n_x).
  , ssH  :: !(LA.Matrix Double)   -- ^ [日本語]: 観測行列 H (n_y × n_x)。 [English]: The observation matrix H (n_y × n_x).
  , ssQ  :: !(LA.Matrix Double)   -- ^ [日本語]: プロセスノイズ共分散 Q (n_x × n_x)。 [English]: The process noise covariance Q (n_x × n_x).
  , ssR  :: !(LA.Matrix Double)   -- ^ [日本語]: 観測ノイズ共分散 R (n_y × n_y)。 [English]: The observation noise covariance R (n_y × n_y).
  , ssX0 :: !(LA.Vector Double)   -- ^ [日本語]: 初期状態 (n_x)。 [English]: The initial state (n_x).
  , ssP0 :: !(LA.Matrix Double)   -- ^ [日本語]: 初期共分散 (n_x × n_x)。 [English]: The initial covariance (n_x × n_x).
  } deriving (Show)

data KalmanResult = KalmanResult
  { krFilteredMean :: ![LA.Vector Double]
  , krFilteredCov  :: ![LA.Matrix Double]
  , krSmoothedMean :: ![LA.Vector Double]
    -- ^ [日本語]: 'kalmanFilter' のみ呼んだ場合は空。 'kalmanSmoother' を通すと埋まる。
    --   [English]: Empty if only 'kalmanFilter' was called. Populated once
    --   passed through 'kalmanSmoother'.
  , krSmoothedCov  :: ![LA.Matrix Double]
  , krLogLik       :: !Double      -- ^ [日本語]: Σ log p(y_t | y_{1:t-1})。 [English]: Σ log p(y_t | y_{1:t-1}).
  } deriving (Show)

-- ===========================================================================
-- Kalman Filter (forward pass)
-- ===========================================================================

-- | [日本語]: 観測系列 ys (各列が 1 時点の観測ベクトル) からフィルタリング。
--   ys の行 = 観測次元 n_y、 列 = 時点数 T。
--   [English]: Filters from an observation sequence ys (each column is one
--   time point's observation vector). ys's rows = observation dimension
--   n_y, columns = number of time points T.
kalmanFilter :: StateSpaceModel -> LA.Matrix Double -> KalmanResult
kalmanFilter ssm ys =
  let nY = LA.rows ys
      _  = nY :: Int
      tT = LA.cols ys
      f  = ssF ssm
      h  = ssH ssm
      q  = ssQ ssm
      r  = ssR ssm
      step (x, p, accM, accP, ll) t =
        let yt   = LA.flatten (ys LA.¿ [t])
            -- predict
            xPred = f LA.#> x
            pPred = f LA.<> p LA.<> LA.tr f + q
            -- update
            yPred = h LA.#> xPred
            sInn  = h LA.<> pPred LA.<> LA.tr h + r
            -- guard against singular S
            sInv  = LA.inv sInn
            gain  = pPred LA.<> LA.tr h LA.<> sInv
            inn   = yt - yPred
            xNew  = xPred + gain LA.#> inn
            pNew  = pPred - gain LA.<> h LA.<> pPred
            -- log-likelihood contribution
            nY_   = fromIntegral (LA.size inn) :: Double
            detS  = LA.det sInn
            quad  = inn `LA.dot` (sInv LA.#> inn)
            lt    = -0.5 * (nY_ * log (2 * pi) + log (max 1e-300 detS) + quad)
        in (xNew, pNew, accM ++ [xNew], accP ++ [pNew], ll + lt)
      (_, _, ms, ps, llTotal) =
        foldl step (ssX0 ssm, ssP0 ssm, [], [], 0) [0 .. tT - 1]
  in KalmanResult
       { krFilteredMean = ms
       , krFilteredCov  = ps
       , krSmoothedMean = []
       , krSmoothedCov  = []
       , krLogLik       = llTotal
       }

-- ===========================================================================
-- RTS Smoother (backward pass)
-- ===========================================================================

-- | [日本語]: RTS smoother。 'kalmanFilter' の出力を受け取り smoothed * を埋めて返す。
--   [English]: The RTS smoother. Takes the output of 'kalmanFilter' and
--   returns it with the smoothed * fields filled in.
kalmanSmoother :: StateSpaceModel -> KalmanResult -> KalmanResult
kalmanSmoother ssm kr =
  let f  = ssF ssm
      q  = ssQ ssm
      ms = krFilteredMean kr
      ps = krFilteredCov  kr
      tT = length ms
      -- 末尾は filtered と smoothed が同じ
      mTLast = last ms
      pTLast = last ps
      -- 後ろから前へ走査
      step (smMs, smPs) i =
        let mFilt = ms !! i
            pFilt = ps !! i
            mPred = f LA.#> mFilt
            pPred = f LA.<> pFilt LA.<> LA.tr f + q
            mNext = head smMs
            pNext = head smPs
            g     = pFilt LA.<> LA.tr f LA.<> LA.inv pPred
            mNew  = mFilt + g LA.#> (mNext - mPred)
            pNew  = pFilt + g LA.<> (pNext - pPred) LA.<> LA.tr g
        in (mNew : smMs, pNew : smPs)
      (smMsFinal, smPsFinal) =
        foldl step ([mTLast], [pTLast]) (reverse [0 .. tT - 2])
  in kr { krSmoothedMean = smMsFinal
        , krSmoothedCov  = smPsFinal
        }
