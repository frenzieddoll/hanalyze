{-# LANGUAGE OverloadedStrings #-}
-- | Optimal designs: D-optimal and A-optimal.
--
-- Selects a subset of @n@ runs from a candidate set, maximizing /
-- minimizing a criterion based on the information matrix @XᵀX@.
--
--   * **D-optimal** — @max det(XᵀX)@ → joint estimation precision of
--     all parameters.
--   * **A-optimal** — @min trace((XᵀX)⁻¹)@ → minimum average estimation
--     variance.
--
-- Algorithm: the Fedorov exchange method (sequential exchanges). Starts
-- from a random selection of candidates and
-- 改善する交換が見つからなくなるまで繰り返す。
module Design.Optimal
  ( OptCriterion (..)
  , dOptimal
  , aOptimal
  , optimalDesign
  , candidateGrid
  , quadraticCandidates
  , pseudoShuffle
  ) where

import Data.List (foldl')
import qualified Numeric.LinearAlgebra as LA

-- | Optimality criterion.
data OptCriterion
  = DOpt   -- ^ D-optimal: maximize @det(XᵀX)@.
  | AOpt   -- ^ A-optimal: minimize @trace((XᵀX)⁻¹)@.
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- 基準値の計算
-- ---------------------------------------------------------------------------

-- | D-criterion value for a design matrix @X@: @det(XᵀX)@.
dValue :: [[Double]] -> Double
dValue rows
  | null rows = 0
  | otherwise = LA.det xtx
  where
    m   = LA.fromLists rows
    xtx = LA.tr m LA.<> m

-- | A-criterion value for a design matrix @X@: @trace((XᵀX)⁻¹)@.
-- Returns @∞@ when the inverse does not exist.
aValue :: [[Double]] -> Double
aValue rows
  | null rows = 1 / 0
  | otherwise =
      let m   = LA.fromLists rows
          xtx = LA.tr m LA.<> m
          d   = LA.det xtx
      in if abs d < 1e-12 then 1 / 0
           else
             let inv = LA.inv xtx
                 p   = LA.cols m
             in sum [ inv `LA.atIndex` (i, i) | i <- [0 .. p - 1] ]

-- | Criterion value used for optimization. Both criteria are returned
-- as quantities to /minimize/; D-optimality is encoded as
-- @-det(XᵀX)@.
critValue :: OptCriterion -> [[Double]] -> Double
critValue DOpt rows = -dValue rows  -- 最小化問題に統一
critValue AOpt rows =  aValue rows

-- ---------------------------------------------------------------------------
-- Fedorov 交換アルゴリズム
-- ---------------------------------------------------------------------------

-- | Generic optimal design: pick @n@ rows from a candidate set.
optimalDesign :: OptCriterion        -- ^ Optimization criterion.
              -> [[Double]]          -- ^ Candidate set (each row is a
                                     --   potential design row).
              -> Int                 -- ^ Number of runs to select.
              -> Int                 -- ^ Seed for the initial selection.
              -> ([Int], [[Double]]) -- ^ Selected candidate indices and
                                     --   the resulting design matrix.
optimalDesign crit cands n seed =
  let nC      = length cands
      initIdx = take n (pseudoShuffle seed [0 .. nC - 1])
      design  = map (cands !!) initIdx
      -- 改善する交換が無くなるまで反復
      improve current currentCrit =
        let pairs =
              [ (i, j)
              | i <- [0 .. n - 1]   -- 取り除く index (current の中で)
              , j <- [0 .. nC - 1]  -- 追加候補 (cands の中で)
              , j `notElem` current ]
            tryEach (bestIdx, bestC) (i, j) =
              let swapped = take i bestIdx ++ [j] ++ drop (i + 1) bestIdx
                  newDes  = map (cands !!) swapped
                  newC    = critValue crit newDes
              in if newC < bestC then (swapped, newC) else (bestIdx, bestC)
            (improved, improvedC) =
              foldl' tryEach (current, currentCrit) pairs
        in if improvedC < currentCrit
             then improve improved improvedC
             else (improved, currentCrit)
      initC = critValue crit design
      (finalIdx, _) = improve initIdx initC
  in (finalIdx, map (cands !!) finalIdx)

-- | Build a D-optimal design (specialization of 'optimalDesign').
dOptimal :: [[Double]] -> Int -> Int -> ([Int], [[Double]])
dOptimal = optimalDesign DOpt

-- | Build an A-optimal design.
aOptimal :: [[Double]] -> Int -> Int -> ([Int], [[Double]])
aOptimal = optimalDesign AOpt

-- ---------------------------------------------------------------------------
-- 候補集合の生成
-- ---------------------------------------------------------------------------

-- | Equally-spaced grid of candidates: @k@ factors, @numLevels@ values
-- per factor on @[-1, 1]@.
candidateGrid :: Int -> Int -> [[Double]]
candidateGrid k numLevels =
  let levels = if numLevels == 1 then [0]
                else [-1 + 2 * fromIntegral i / fromIntegral (numLevels - 1)
                     | i <- [0 .. numLevels - 1] :: [Int]]
      go 0 = [[]]
      go d = [v : row | v <- levels, row <- go (d - 1)]
  in go k

-- | Expand a candidate grid into the @quadraticDesign@-style row
-- representation.
--
-- @quadraticCandidates k numLevels@ — each candidate is the row
-- @[1, x_1, …, x_k, x_1², …, x_k²,
-- pairwise interactions]@.
quadraticCandidates :: Int -> Int -> [[Double]]
quadraticCandidates k numLevels =
  let baseGrid = candidateGrid k numLevels
      expand row =
        let sqE   = [x * x | x <- row]
            interE = [(row !! i) * (row !! j)
                     | i <- [0 .. k - 1], j <- [i + 1 .. k - 1]]
        in 1 : row ++ sqE ++ interE
  in map expand baseGrid

-- ---------------------------------------------------------------------------
-- ヘルパ
-- ---------------------------------------------------------------------------

-- | LCG ベースの簡易シャッフル (再現性のため seed 指定)。
pseudoShuffle :: Int -> [a] -> [a]
pseudoShuffle seed xs =
  let lcg s = (s * 1103515245 + 12345) `mod` (2 ^ (31 :: Int))
      seeds = take (length xs) (drop 1 (iterate lcg seed))
      paired = zip seeds xs
      sorted = sortByKey paired
  in map snd sorted
  where
    sortByKey [] = []
    sortByKey (p:ps) =
      sortByKey [q | q <- ps, fst q <= fst p]
      ++ [p]
      ++ sortByKey [q | q <- ps, fst q > fst p]

