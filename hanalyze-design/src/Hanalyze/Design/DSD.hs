{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Design.DSD
-- Description : Definitive Screening Design (Jones-Nachtsheim 2011) の 2k+1 run 生成
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Definitive Screening Design (Jones-Nachtsheim 2011)。
--
-- k 連続因子について __2k + 1 runs__ で主効果 + 二次効果 + 一部の 2 因子
-- 交互作用を識別できる効率的スクリーニング計画。
--
-- 構成:
--
--   - 1 行目: 中心点 @[0, 0, ..., 0]@
--   - 2..k+1 行目: 各 row i は position i に 0 を持ち、 他は ±1
--   - k+2..2k+1 行目: 上記の foldover (= 各行の符号反転)
--
-- 本初版は k = 4 を __Jones-Nachtsheim Table 1 の conference matrix__ で
-- 構築 (verified DSD)。 他の k は Hadamard-like 構造で近似 (= 構造的 DSD)。
-- 厳密な conference-matrix DSD の追加は将来対応予定。
--
-- [English]: Definitive Screening Design (Jones-Nachtsheim 2011).
--
-- An efficient screening design for k continuous factors that can identify
-- main effects + quadratic effects + some two-factor interactions in
-- __2k + 1 runs__.
--
-- Construction:
--
--   - Row 1: the center point @[0, 0, ..., 0]@
--   - Rows 2..k+1: each row i has 0 at position i, and ±1 elsewhere
--   - Rows k+2..2k+1: the foldover of the above (i.e. sign-flip of each row)
--
-- This initial version builds k = 4 from the
-- __conference matrix in Jones-Nachtsheim Table 1__ (a verified DSD). Other k values are
-- approximated with a Hadamard-like structure (a structural DSD). Adding a
-- rigorous conference-matrix DSD is planned for a future phase.
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

-- | [日本語]: DSD の結果。
--   [English]: The result of a DSD.
data DSDResult = DSDResult
  { dsdMatrix     :: !(LA.Matrix Double)
    -- ^ [日本語]: @(2k + 1) × k@ 行列。 各要素は @{-1, 0, +1}@。
    --   [English]: A @(2k + 1) x k@ matrix. Each element is @{-1, 0, +1}@.
  , dsdNFactors   :: !Int       -- ^ [日本語]: 因子数 k [English]: The number of factors, k
  , dsdNRuns      :: !Int       -- ^ [日本語]: 実験数 2k + 1 [English]: The number of runs, 2k + 1
  , dsdHasOptimal :: !Bool
    -- ^ [日本語]: @True@ = Jones-Nachtsheim Table の conference matrix 由来 (verified DSD)、
    --   @False@ = Hadamard-like 構造で近似 (structural DSD)
    --   [English]: @True@ = derived from the conference matrix in the
    --   Jones-Nachtsheim Table (verified DSD); @False@ = approximated with
    --   a Hadamard-like structure (structural DSD)
  } deriving (Show)

-- ===========================================================================
-- 公開関数
-- ===========================================================================

-- | [日本語]: DSD を生成。
--
--   k = 4 のみ verified (Jones-Nachtsheim 2011 Table 1)。
--   k ≥ 2 の他値は Hadamard-like 構造の structural DSD (`dsdHasOptimal = False`)。
--   k < 2 は @Left@。
--   [English]: Generates a DSD.
--
--   Only k = 4 is verified (Jones-Nachtsheim 2011 Table 1). Other values
--   with k ≥ 2 are a structural DSD with a Hadamard-like structure
--   (`dsdHasOptimal = False`). k < 2 yields @Left@.
dsdDesign :: Int -> Either Text DSDResult
dsdDesign k
  | k < 2 = Left (T.pack ("dsdDesign: need k >= 2, got k=" <> show k))
  | k == 4 = Right (verifiedDSD k confC4)
  | otherwise = Right (structuralDSD k)

-- ===========================================================================
-- 内部: verified DSD (conference matrix 由来)
-- ===========================================================================

-- | [日本語]: C_4: 4 次の conference matrix。 Jones-Nachtsheim 2011 Table 1 第 1 行。
--   不変条件: 対角 0、 非対角 ±1、 @C · Cᵀ = (n-1) I@。
--   [English]: C_4: the order-4 conference matrix. Jones-Nachtsheim 2011
--   Table 1, row 1. Invariants: diagonal 0, off-diagonal ±1,
--   @C . Cᵀ = (n-1) I@.
confC4 :: [[Double]]
confC4 =
  [ [ 0,  1,  1,  1]
  , [ 1,  0,  1, -1]
  , [ 1, -1,  0,  1]
  , [ 1,  1, -1,  0]
  ]

-- | [日本語]: 与えた conference matrix から DSD を構築:
--   row 0 = center、 rows 1..k = C 各行、 rows k+1..2k = -C 各行。
--   [English]: Builds a DSD from the given conference matrix: row 0 =
--   center, rows 1..k = each row of C, rows k+1..2k = each row of -C.
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

-- | [日本語]: k != 4 の場合の近似 DSD。 構造 (2k+1 runs、 各 row に 1 個の 0) は
--   満たすが、 conference matrix 性質 (`C · Cᵀ = (n-1) I`) は保証しない。
--
--   ±1 パターンは Sylvester-Hadamard 風: row i の position j (j != i) について
--   @sign = (-1)^popCount(i .&. j)@。
--   [English]: The approximate DSD for k != 4. It satisfies the structure
--   (2k+1 runs, one 0 per row) but does not guarantee the conference-matrix
--   property (`C . Cᵀ = (n-1) I`).
--
--   The ±1 pattern is Sylvester-Hadamard-like: for row i, position j
--   (j != i), @sign = (-1)^popCount(i .&. j)@.
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

-- | [日本語]: Sylvester-Hadamard 符号: @(-1)^popCount(i AND j)@。
--   [English]: Sylvester-Hadamard sign: @(-1)^popCount(i AND j)@.
hadamardSign :: Int -> Int -> Double
hadamardSign i j
  | even (B.popCount (i B..&. j)) =  1
  | otherwise                     = -1
