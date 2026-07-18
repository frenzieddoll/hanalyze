{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Design.DSD
-- Description : Definitive Screening Design (Jones-Nachtsheim 2011) の 2k+1 run 生成
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Definitive Screening Design (Jones-Nachtsheim 2011)。
--
-- k 連続因子について **2k + 1 runs** で主効果 + 二次効果 + 一部の 2 因子
-- 交互作用を識別できる効率的スクリーニング計画。
--
-- 構成:
--
--   * 1 行目: 中心点 @[0, 0, ..., 0]@
--   * 2..k+1 行目: 各 row i は position i に 0 を持ち、 他は ±1
--   * k+2..2k+1 行目: 上記の foldover (= 各行の符号反転)
--
-- 本初版は k = 4 を **Jones-Nachtsheim Table 1 の conference matrix** で
-- 構築 (verified DSD)。 他の k は Hadamard-like 構造で近似 (= 構造的 DSD)。
-- 厳密な conference-matrix DSD の追加は将来 Phase。
module Hanalyze.Design.DSD
  ( DSDResult (..)
  , dsdDesign
  ) where

import qualified Data.Bits             as B
import qualified Numeric.LinearAlgebra as LA
import           Data.Text             (Text)
import qualified Data.Text             as T

-- ===========================================================================
-- 型
-- ===========================================================================

-- | DSD の結果。
data DSDResult = DSDResult
  { dsdMatrix     :: !(LA.Matrix Double)
    -- ^ @(2k + 1) × k@ 行列。 各要素は @{-1, 0, +1}@。
  , dsdNFactors   :: !Int       -- ^ 因子数 k
  , dsdNRuns      :: !Int       -- ^ 実験数 2k + 1
  , dsdHasOptimal :: !Bool
    -- ^ @True@ = Jones-Nachtsheim Table の conference matrix 由来 (verified DSD)、
    --   @False@ = Hadamard-like 構造で近似 (structural DSD)
  } deriving (Show)

-- ===========================================================================
-- 公開関数
-- ===========================================================================

-- | DSD を生成。
--
-- k = 4 のみ verified (Jones-Nachtsheim 2011 Table 1)。
-- k ≥ 2 の他値は Hadamard-like 構造の structural DSD (`dsdHasOptimal = False`)。
-- k < 2 は @Left@。
dsdDesign :: Int -> Either Text DSDResult
dsdDesign k
  | k < 2 = Left (T.pack ("dsdDesign: need k >= 2, got k=" <> show k))
  | k == 4 = Right (verifiedDSD k confC4)
  | otherwise = Right (structuralDSD k)

-- ===========================================================================
-- 内部: verified DSD (conference matrix 由来)
-- ===========================================================================

-- | C_4: 4 次の conference matrix。 Jones-Nachtsheim 2011 Table 1 第 1 行。
--   不変条件: 対角 0、 非対角 ±1、 @C · Cᵀ = (n-1) I@。
confC4 :: [[Double]]
confC4 =
  [ [ 0,  1,  1,  1]
  , [ 1,  0,  1, -1]
  , [ 1, -1,  0,  1]
  , [ 1,  1, -1,  0]
  ]

-- | 与えた conference matrix から DSD を構築:
--   row 0 = center、 rows 1..k = C 各行、 rows k+1..2k = -C 各行。
verifiedDSD :: Int -> [[Double]] -> DSDResult
verifiedDSD k cMat =
  let center  = replicate k 0
      posRows = cMat
      negRows = map (map negate) cMat
      allRows = center : posRows ++ negRows
      mat     = LA.fromLists allRows
  in DSDResult
       { dsdMatrix     = mat
       , dsdNFactors   = k
       , dsdNRuns      = 2 * k + 1
       , dsdHasOptimal = True
       }

-- ===========================================================================
-- 内部: structural DSD (Hadamard-like、 conference matrix 無しの近似)
-- ===========================================================================

-- | k != 4 の場合の近似 DSD。 構造 (2k+1 runs、 各 row に 1 個の 0) は
-- 満たすが、 conference matrix 性質 (`C · Cᵀ = (n-1) I`) は保証しない。
--
-- ±1 パターンは Sylvester-Hadamard 風: row i の position j (j != i) について
-- @sign = (-1)^popCount(i .&. j)@。
structuralDSD :: Int -> DSDResult
structuralDSD k =
  let posRows = [ [ if j + 1 == i then 0  -- position i (1-origin in row) gets 0
                    else hadamardSign i (j + 1)
                  | j <- [0 .. k - 1]
                  ]
                | i <- [1 .. k]
                ]
      negRows = map (map negate) posRows
      center  = replicate k 0
      mat     = LA.fromLists (center : posRows ++ negRows)
  in DSDResult
       { dsdMatrix     = mat
       , dsdNFactors   = k
       , dsdNRuns      = 2 * k + 1
       , dsdHasOptimal = False
       }

-- | Sylvester-Hadamard 符号: @(-1)^popCount(i AND j)@。
hadamardSign :: Int -> Int -> Double
hadamardSign i j
  | even (B.popCount (i B..&. j)) =  1
  | otherwise                     = -1
