{-# LANGUAGE OverloadedStrings #-}
-- | Pareto-front utilities for evaluating multi-objective results.
--
--   * 'isNonDominated' — is a given point non-dominated within the front?
--   * 'paretoFront'    — extract just the non-dominated points from a set.
--   * 'hypervolume'    — front volume indicator (larger is better).
--   * 'igd'            — Inverted Generational Distance (distance from the
--     true front to the approximation).
--   * 'gd'             — Generational Distance (distance from the
--     approximation to the true front).
--
-- All objectives are treated as **minimized**, matching the NSGA-II
-- convention.
module Optim.Pareto
  ( isNonDominated
  , paretoFront
  , hypervolume
  , igd
  , gd
  ) where

import Data.List (sortBy, sortOn)

-- | 点 p が集合 ps の中で非優越 (= どの ps の点にも支配されない) か。
isNonDominated :: [Double] -> [[Double]] -> Bool
isNonDominated p ps = not (any (`dominates'` p) ps)

-- | 通常の Pareto dominance (内部用、`Optim.NSGA.paretoDominates` と同形)。
dominates' :: [Double] -> [Double] -> Bool
dominates' a b =
  all (uncurry (<=)) zipped && any (uncurry (<)) zipped
  where zipped = zip a b

-- | 点集合から **非優越な点** だけ抽出する。重複点は最初の 1 つだけ残す。
paretoFront :: [[Double]] -> [[Double]]
paretoFront pts =
  [p | (i, p) <- indexed,
       not (any (\(j, q) -> j /= i && dominates' q p) indexed) ]
  where
    indexed = zip [0 :: Int ..] pts

-- | Hypervolume 指標 (HV)。
-- 参照点 r から見て Pareto front が支配する体積。
-- 大きいほど良い (収束 + 多様性 の両方を反映)。
--
-- 2D: 厳密公式 (面積)。
-- 3D 以上: HSO (Hypervolume by Slicing Objectives) で軸ごとに再帰計算。
--
-- 全目的は **最小化** を仮定 (NSGA-II の慣習)。
hypervolume :: [Double] -> [[Double]] -> Double
hypervolume ref front
  | null front = 0
  | any (\p -> length p /= dim) front = error "hypervolume: 次元不一致"
  | dim == 2 = hv2D ref front
  | otherwise = hvND ref front
  where
    dim = length ref

-- 2D: y 降順にソート → x 増加順に階段状の面積を積む
hv2D :: [Double] -> [[Double]] -> Double
hv2D [rx, ry] front =
  let valid    = [p | p <- front, head p < rx, p !! 1 < ry]
      sorted   = sortOn head valid    -- x 昇順
      go _    [] acc          = acc
      go yPrev (p:ps) acc =
        let xCur = head p
            yCur = p !! 1
        in if yCur >= yPrev   -- 支配されてる (= 重複点) → 寄与なし
             then go yPrev ps acc
             else go yCur ps (acc + (rx - xCur) * (yPrev - yCur))
  in go ry sorted 0
hv2D _ _ = 0

-- 一般 N 次元: 第 1 軸 (x_1) で降順にスライスして再帰。
--
-- HSO (Hypervolume by Slicing Objectives) アルゴリズム:
--   x_1 で降順にソートし、各点 p で:
--     width = (前のスライス境界) - p[0]
--     slice = HV(p から見える残り次元の front, 残り参照点)
--     vol += width × slice
--   前のスライス境界は ref[0] から始まり、各 p で更新。
hvND :: [Double] -> [[Double]] -> Double
hvND ref front =
  let front'   = paretoFront [p | p <- front
                                , and (zipWith (<) p ref) ]  -- ref 内のみ
      sortedDesc = sortBy (\a b -> compare (head b) (head a)) front'
                   -- x_1 降順
      r1       = head ref
      restRef  = tail ref
      go _    []     acc = acc
      go xPrev (p:ps) acc =
        let xCur  = head p
            width = xPrev - xCur
            -- 残り次元への射影: 現在の p より x_1 が小さい点 (= まだ処理してない)
            -- + p 自身
            activeRest = (tail p) :
                         [ tail q | q <- ps ]
            slice = hypervolume restRef activeRest
        in if width <= 0
             then go xPrev ps acc
             else go xCur ps (acc + width * slice)
  in go r1 sortedDesc 0

-- | Inverted Generational Distance: 真の front の各点から推定 front への
-- 最短距離の平均。**小さいほど良い**。多様性も評価できる。
--
-- IGD = (1/|R|) Σ_{r ∈ R} min_{e ∈ E} dist(r, e)
igd :: [[Double]] -> [[Double]] -> Double
igd trueF estF
  | null trueF || null estF = 1 / 0
  | otherwise =
      let n = length trueF
          minDistTo r = minimum [euclid r e | e <- estF]
      in sum (map minDistTo trueF) / fromIntegral n

-- | Generational Distance: 推定 front の各点から真の front への最短距離の平均。
-- **小さいほど良い** が、多様性は評価しない。
gd :: [[Double]] -> [[Double]] -> Double
gd trueF estF
  | null trueF || null estF = 1 / 0
  | otherwise =
      let n = length estF
          minDistTo e = minimum [euclid e t | t <- trueF]
      in sum (map minDistTo estF) / fromIntegral n

-- | ユークリッド距離。
euclid :: [Double] -> [Double] -> Double
euclid a b = sqrt (sum [(x - y) ^ (2 :: Int) | (x, y) <- zip a b])
