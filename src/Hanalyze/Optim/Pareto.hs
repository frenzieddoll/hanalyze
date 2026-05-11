{-# LANGUAGE StrictData #-}
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
module Hanalyze.Optim.Pareto
  ( isNonDominated
  , paretoFront
  , hypervolume
  , igd
  , gd
  ) where

import Data.List (sortBy, sortOn)

-- | True iff @p@ is non-dominated within the set @ps@ (no element of @ps@
-- dominates it).
isNonDominated :: [Double] -> [[Double]] -> Bool
isNonDominated p ps = not (any (`dominates'` p) ps)

-- | Plain Pareto dominance (internal helper; same definition as
-- 'Hanalyze.Optim.NSGA.paretoDominates').
dominates' :: [Double] -> [Double] -> Bool
dominates' a b =
  all (uncurry (<=)) zipped && any (uncurry (<)) zipped
  where zipped = zip a b

-- | Extract just the non-dominated points from a set. When points repeat,
-- only the first occurrence is kept.
paretoFront :: [[Double]] -> [[Double]]
paretoFront pts =
  [p | (i, p) <- indexed,
       not (any (\(j, q) -> j /= i && dominates' q p) indexed) ]
  where
    indexed = zip [0 :: Int ..] pts

-- | Hypervolume (HV) indicator: the volume dominated by the Pareto
-- front, measured from a reference point @r@. Larger is better
-- (captures both convergence and diversity).
--
-- 2D uses the exact area formula; higher dimensions use HSO
-- (Hypervolume by Slicing Objectives) recursively.
--
-- All objectives are assumed to be minimized (NSGA-II convention).
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

-- | Inverted Generational Distance: the average of, for each point in
-- the /true/ front, the minimum distance to the /estimated/ front.
-- Smaller is better; rewards diversity as well as convergence.
--
-- @IGD = (1/|R|) Σ_{r ∈ R} min_{e ∈ E} dist(r, e)@.
igd :: [[Double]] -> [[Double]] -> Double
igd trueF estF
  | null trueF || null estF = 1 / 0
  | otherwise =
      let n = length trueF
          minDistTo r = minimum [euclid r e | e <- estF]
      in sum (map minDistTo trueF) / fromIntegral n

-- | Generational Distance: the average minimum distance from each point
-- of the /estimated/ front to the /true/ front. Smaller is better, but
-- this does not penalize a lack of diversity.
gd :: [[Double]] -> [[Double]] -> Double
gd trueF estF
  | null trueF || null estF = 1 / 0
  | otherwise =
      let n = length estF
          minDistTo e = minimum [euclid e t | t <- trueF]
      in sum (map minDistTo estF) / fromIntegral n

-- | Euclidean distance.
euclid :: [Double] -> [Double] -> Double
euclid a b = sqrt (sum [(x - y) ^ (2 :: Int) | (x, y) <- zip a b])
