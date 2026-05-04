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

-- | 最適化基準。
data OptCriterion = DOpt | AOpt deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- 基準値の計算
-- ---------------------------------------------------------------------------

-- | 設計行列 X に対する D 値 (= det(XᵀX))。
dValue :: [[Double]] -> Double
dValue rows
  | null rows = 0
  | otherwise = LA.det xtx
  where
    m   = LA.fromLists rows
    xtx = LA.tr m LA.<> m

-- | 設計行列 X に対する A 値 (= trace((XᵀX)⁻¹))。逆行列が存在しないなら ∞。
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

-- | 評価値: D-opt は最大化したいので符号反転して最小化問題に統一。
critValue :: OptCriterion -> [[Double]] -> Double
critValue DOpt rows = -dValue rows  -- 最小化問題に統一
critValue AOpt rows =  aValue rows

-- ---------------------------------------------------------------------------
-- Fedorov 交換アルゴリズム
-- ---------------------------------------------------------------------------

-- | 一般的な最適計画。候補集合から n 行を選ぶ。
--
-- 引数:
--   * @crit@   — 最適化基準
--   * @cands@  — 候補集合 (各行は計画行列の 1 行)
--   * @n@      — 選びたい試行数
--   * @seed@   — 初期選択のシード (再現性)
optimalDesign :: OptCriterion -> [[Double]] -> Int -> Int
              -> ([Int], [[Double]])  -- (選ばれた候補 index, 設計行列)
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

-- | D-optimal 設計を構築 (`optimalDesign DOpt`)。
dOptimal :: [[Double]] -> Int -> Int -> ([Int], [[Double]])
dOptimal = optimalDesign DOpt

-- | A-optimal 設計を構築。
aOptimal :: [[Double]] -> Int -> Int -> ([Int], [[Double]])
aOptimal = optimalDesign AOpt

-- ---------------------------------------------------------------------------
-- 候補集合の生成
-- ---------------------------------------------------------------------------

-- | k 因子で各因子の水準が等間隔 (numLevels 個、範囲 [-1, 1]) のグリッド。
candidateGrid :: Int -> Int -> [[Double]]
candidateGrid k numLevels =
  let levels = if numLevels == 1 then [0]
                else [-1 + 2 * fromIntegral i / fromIntegral (numLevels - 1)
                     | i <- [0 .. numLevels - 1] :: [Int]]
      go 0 = [[]]
      go d = [v : row | v <- levels, row <- go (d - 1)]
  in go k

-- | 二次モデル用の候補集合を quadraticDesign 形式に展開する。
--
-- @quadraticCandidates k numLevels@ →
--   各候補は [1, x_1, ..., x_k, x_1², ..., x_k², 交互作用...]
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

