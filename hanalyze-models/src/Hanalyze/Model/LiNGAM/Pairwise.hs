{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
-- |
-- Module      : Hanalyze.Model.LiNGAM.Pairwise
-- Description : Pairwise LiNGAM (Hyvärinen-Smith 2013、2 変数間の因果方向推定)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Pairwise LiNGAM: 2 変数間の因果方向 (x → y か y → x か) 推定。
--
-- ## アルゴリズム (Hyvärinen-Smith 2013)
--
-- 標準化された (x, y) について、 非ガウシアン独立性に基づき:
--
--   R(x → y) = - Cov(x³, y) · sign(Cov(x, y)) + Cov(x, y³)
--
-- の符号で方向を決定する近似的測度 (LIM, likelihood ratio approximation)。
--
-- * R > 0 → x → y
-- * R < 0 → y → x
-- * |R| 小 → 判定不能 (ガウシアン近接 or 弱依存)
--
-- 軽量で 2 変数の方向推定に直接使える。 3 変数以上には 'DirectLiNGAM' を使う。
--
-- ## リファレンス
--
-- Hyvärinen, A. & Smith, S. M. (2013) "Pairwise likelihood ratios for
-- estimation of non-Gaussian structural equation models", JMLR 14.
-- Python 実装は cdt15/lingam の `lingam/lim.py` (LIM = Likelihood-based
-- Independence Measure)。
module Hanalyze.Model.LiNGAM.Pairwise
  ( PairwiseDirection (..)
  , PairwiseResult (..)
  , pairwiseLiNGAM
  , pairwiseScore
  ) where

import qualified Numeric.LinearAlgebra as LA

-- ===========================================================================
-- 型
-- ===========================================================================

data PairwiseDirection
  = XtoY        -- ^ x → y
  | YtoX        -- ^ y → x
  | Inconclusive  -- ^ |score| < threshold
  deriving (Show, Eq)

data PairwiseResult = PairwiseResult
  { prScore     :: !Double             -- ^ R(x → y) の値、 符号で方向決定
  , prDirection :: !PairwiseDirection
  , prMagnitude :: !Double             -- ^ |score|、 confidence の代理
  } deriving (Show)

-- ===========================================================================
-- 実装
-- ===========================================================================

-- | Pairwise LiNGAM の主関数。 threshold 未満は Inconclusive。
pairwiseLiNGAM
  :: Double               -- threshold (default 0.0 = 符号だけで判定)
  -> LA.Vector Double     -- x
  -> LA.Vector Double     -- y
  -> PairwiseResult
pairwiseLiNGAM thr x y =
  let !s = pairwiseScore x y
      !mag = abs s
      !dir
        | mag < thr = Inconclusive
        | s > 0     = XtoY
        | otherwise = YtoX
  in PairwiseResult { prScore = s, prDirection = dir, prMagnitude = mag }

-- | スコア R = -Cov(x³, y)·sign(Cov(x,y)) + Cov(x, y³)
--   x, y は内部で標準化される (zero-mean、 unit-variance)。
pairwiseScore :: LA.Vector Double -> LA.Vector Double -> Double
pairwiseScore xRaw yRaw =
  let !x = standardize xRaw
      !y = standardize yRaw
      !x3 = x * x * x
      !y3 = y * y * y
      !cov_x_y   = covar x  y
      !cov_x3_y  = covar x3 y
      !cov_x_y3  = covar x  y3
      !sgn = if cov_x_y >= 0 then 1.0 else (-1.0 :: Double)
  in - cov_x3_y * sgn + cov_x_y3

-- ===========================================================================
-- 内部
-- ===========================================================================

standardize :: LA.Vector Double -> LA.Vector Double
standardize v =
  let !n  = fromIntegral (LA.size v) :: Double
      !mu = LA.sumElements v / n
      !c  = v - LA.scalar mu
      !s  = sqrt (c `LA.dot` c / n)
      !sd = if s > 1e-12 then s else 1.0
  in LA.scale (1 / sd) c

covar :: LA.Vector Double -> LA.Vector Double -> Double
covar a b =
  let !n  = fromIntegral (LA.size a) :: Double
      !ma = LA.sumElements a / n
      !mb = LA.sumElements b / n
      !ca = a - LA.scalar ma
      !cb = b - LA.scalar mb
  in ca `LA.dot` cb / n
