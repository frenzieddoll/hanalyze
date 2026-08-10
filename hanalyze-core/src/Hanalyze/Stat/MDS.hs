{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Stat.MDS
-- Description : 多次元尺度構成法 (古典 MDS / Sammon MDS)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Multidimensional Scaling (MDS).
--
-- * Classical MDS (Torgerson) — 距離行列を二重中心化 → 固有分解 → 上位 k 成分。
-- * Sammon MDS — Sammon stress を勾配降下で最小化 (古典 MDS を初期値)。
--
-- @
-- import qualified Hanalyze.Stat.MDS as MDS
-- let d  = MDS.euclideanDist x                -- x :: Matrix Double (n × p)
--     emb = MDS.mdsClassical d 2              -- 2-D 埋め込み (n × 2)
-- @
module Hanalyze.Stat.MDS
  ( euclideanDist
  , mdsClassical
  , mdsSammon
  , sammonStress
  , SammonConfig (..)
  , defaultSammonConfig
  ) where

import qualified Numeric.LinearAlgebra as LA

-- ---------------------------------------------------------------------------
-- Distance matrix helper
-- ---------------------------------------------------------------------------

-- | n × p のデータ行列から n × n のユークリッド距離行列を作る。
euclideanDist :: LA.Matrix Double -> LA.Matrix Double
euclideanDist x =
  let !n = LA.rows x
      row i = LA.flatten (x LA.? [i])
      dij i j = LA.norm_2 (row i - row j)
  in LA.build (n, n) (\i j -> dij (round i) (round j))

-- ---------------------------------------------------------------------------
-- Classical MDS (Torgerson)
-- ---------------------------------------------------------------------------

-- | 距離行列 D (n × n) を k 次元埋め込み (n × k) に。
--
-- B = -1/2 · H · D² · H、 H = I - 1/n · 11ᵀ。 B = V Λ Vᵀ から
-- 正の上位 k 成分のみ抽出して X = V_k √Λ_k。
mdsClassical :: LA.Matrix Double  -- ^ 距離行列 D (n × n)。
             -> Int                -- ^ 目的次元 k。
             -> LA.Matrix Double  -- ^ 埋め込み (n × k)。
mdsClassical d k =
  let !n   = LA.rows d
      !d2  = d * d
      ones = LA.konst 1 (n, n) :: LA.Matrix Double
      h    = LA.ident n - LA.scale (1 / fromIntegral n) ones
      b    = LA.scale (-0.5) (h LA.<> d2 LA.<> h)
      -- 対称化 (数値誤差吸収)
      bSym = LA.scale 0.5 (b + LA.tr b)
      (eigVals, eigVecs) = LA.eigSH (LA.trustSym bSym)
      -- 降順 (hmatrix eigSH)。 正かつ上位 k を採用。
      lamList = LA.toList eigVals
      take_   = min k n
      lamPos  = [ if v > 0 then v else 0 | v <- take take_ lamList ]
      sqrtL   = LA.diag (LA.fromList (map sqrt lamPos))
      vK      = eigVecs LA.¿ [0 .. take_ - 1]
  in vK LA.<> sqrtL

-- ---------------------------------------------------------------------------
-- Sammon MDS
-- ---------------------------------------------------------------------------

data SammonConfig = SammonConfig
  { sammonMaxIter :: !Int
  , sammonLR      :: !Double   -- ^ 学習率。
  , sammonTol     :: !Double   -- ^ stress 改善の許容下限。
  } deriving (Show)

defaultSammonConfig :: SammonConfig
defaultSammonConfig = SammonConfig
  { sammonMaxIter = 300
  , sammonLR      = 0.3
  , sammonTol     = 1e-6
  }

-- | Sammon stress E = (1/c) Σ_{i<j} (δ_ij - d_ij)² / δ_ij
--   ただし δ_ij は元距離、 d_ij は埋め込み距離、 c = Σ_{i<j} δ_ij。
sammonStress :: LA.Matrix Double  -- ^ 元距離行列 (n × n)。
             -> LA.Matrix Double  -- ^ 埋め込み (n × k)。
             -> Double
sammonStress d y =
  let !n = LA.rows d
      row i = LA.flatten (y LA.? [i])
      pairs = [ (i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1] ]
      delta i j = LA.atIndex d (i, j)
      dij i j = LA.norm_2 (row i - row j)
      cTot  = sum [ delta i j | (i, j) <- pairs ]
      num   = sum [ let !del = delta i j
                        !dd  = dij i j
                    in if del > 0 then (del - dd)^(2 :: Int) / del
                                  else 0
                  | (i, j) <- pairs ]
  in if cTot > 0 then num / cTot else 0

-- | Sammon MDS。 古典 MDS を初期値にして勾配降下。
mdsSammon :: SammonConfig
          -> LA.Matrix Double  -- ^ 距離行列 D (n × n)。
          -> Int                -- ^ 目的次元 k。
          -> LA.Matrix Double  -- ^ 埋め込み (n × k)。
mdsSammon cfg d k =
  let !y0 = mdsClassical d k
      loop !y !iter !prevE
        | iter >= sammonMaxIter cfg = y
        | otherwise =
            let !grad = sammonGrad d y
                !y'   = y - LA.scale (sammonLR cfg) grad
                !e'   = sammonStress d y'
            in if abs (prevE - e') < sammonTol cfg
                 then y'
                 else loop y' (iter + 1) e'
  in loop y0 0 (sammonStress d y0)

-- | Sammon stress の勾配 (n × k)。
sammonGrad :: LA.Matrix Double -> LA.Matrix Double -> LA.Matrix Double
sammonGrad d y =
  let !n = LA.rows d
      !k = LA.cols y
      row i = LA.flatten (y LA.? [i])
      delta i j = LA.atIndex d (i, j)
      cTot = sum [ delta i j | i <- [0 .. n - 1]
                             , j <- [i + 1 .. n - 1] ]
      scl = if cTot > 0 then 2 / cTot else 0
      gradRow i =
        let yi = row i
            contribs = [ let yj = row j
                             dij = LA.norm_2 (yi - yj)
                             del = delta i j
                         in if del > 0 && dij > 0
                              then LA.scale ((del - dij) / (del * dij))
                                     (yi - yj)
                              else LA.konst 0 k
                       | j <- [0 .. n - 1], j /= i ]
        in LA.scale (negate scl) (sum contribs)
  in LA.fromRows [ gradRow i | i <- [0 .. n - 1] ]
