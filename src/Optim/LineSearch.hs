-- | One-dimensional optimization: Brent's method + golden-section search.
--
-- Both find a local minimum on a unimodal interval @[a, b]@ to high
-- precision.
--
--   * 'goldenSection' — simple and robust; linear convergence on unimodal
--     functions.
--   * 'brent' — Brent (1973): a hybrid of parabolic interpolation and
--     golden section. Superlinear convergence, robust to outliers; matches
--     @scipy.optimize.brent@ and R's @optimize@.
--
-- Both are gradient-free. They need an initial bracket
-- @a < x < b@ with @f(x) < f(a), f(b)@; use 'bracketMinimum' to find one
-- automatically.
module Optim.LineSearch
  ( BrentConfig (..)
  , defaultBrentConfig
  , brent
  , goldenSection
  , bracketMinimum
  ) where

import Optim.Common

-- | The golden ratio @φ@.
phi :: Double
phi = (1 + sqrt 5) / 2

-- | @1 − 1/φ ≈ 0.382@ — the golden-section shrink ratio.
gold :: Double
gold = (3 - sqrt 5) / 2

-- | Brent configuration.
data BrentConfig = BrentConfig
  { bcMaxIter :: !Int        -- ^ Maximum iterations.
  , bcTol     :: !Double     -- ^ Relative tolerance (target final bracket width).
  , bcDir     :: !Direction  -- ^ Optimization direction.
  } deriving (Show, Eq)

-- | Default Brent configuration: 200 iterations, tolerance 1e-8, minimization.
defaultBrentConfig :: BrentConfig
defaultBrentConfig = BrentConfig
  { bcMaxIter = 200
  , bcTol     = 1e-8
  , bcDir     = Minimize
  }

-- | Golden-section search.
--
-- Assumes @[a, b]@ is unimodal (a single interior minimum). Maintains four
-- points @a < c < d < b@ with @c = a + gold·(b-a)@, @d = b - gold·(b-a)@
-- (@gold ≈ 0.382@). Each iteration shrinks the interval by @1/φ ≈ 0.618@
-- with one new function evaluation.
goldenSection :: Direction
              -> ([Double] -> Double)    -- ^ Objective; @1D@ wrapped in a one-element list.
              -> Double                  -- ^ Bracket left @a@.
              -> Double                  -- ^ Bracket right @b@.
              -> Double                  -- ^ Tolerance.
              -> Int                     -- ^ Maximum iterations.
              -> OptimResult
goldenSection dir fUser a0 b0 tol maxIter =
  let f x = flipFor dir fUser [x]
      -- a < c < d < b を維持 (gold ≈ 0.382)
      go iter a b c d fc fd hist
        | iter >= maxIter || abs (b - a) < tol =
            let xm = if fc < fd then c else d
                fm = min fc fd
            in (xm, fm, fm : hist, iter, abs (b - a) < tol)
        | fc < fd =
            -- 最小は [a, d] にある: 区間を [a, d] に縮め、old c が new d になる
            let bN  = d
                dN  = c
                fdN = fc
                cN  = a + gold * (bN - a)
                fcN = f cN
            in go (iter + 1) a bN cN dN fcN fdN (min fcN fdN : hist)
        | otherwise =
            -- 最小は [c, b] にある: 区間を [c, b] に縮め、old d が new c になる
            let aN  = c
                cN  = d
                fcN = fd
                dN  = b - gold * (b - aN)
                fdN = f dN
            in go (iter + 1) aN b cN dN fcN fdN (min fcN fdN : hist)
      a = min a0 b0
      b = max a0 b0
      c = a + gold * (b - a)         -- 左の内点 (約 0.382 of (b-a) from a)
      d = b - gold * (b - a)         -- 右の内点 (約 0.618 of (b-a) from a)
      fc = f c
      fd = f d
      (xb, vb, hist, iters, conv) = go 0 a b c d fc fd [min fc fd]
      vUser = case dir of { Minimize -> vb; Maximize -> negate vb }
      histU = case dir of { Minimize -> reverse hist; Maximize -> map negate (reverse hist) }
  in OptimResult [xb] vUser histU iters conv

-- | Brent's method: a hybrid of parabolic interpolation and
-- golden-section search.
--
-- Compatible with the simple form found in Numerical Recipes and
-- @scipy.optimize.brent@.
brent :: BrentConfig
      -> ([Double] -> Double)
      -> Double                 -- ^ Bracket left @a@.
      -> Double                 -- ^ Bracket right @b@.
      -> OptimResult
brent cfg fUser ax bx =
  let f x = flipFor (bcDir cfg) fUser [x]
      a0 = min ax bx
      b0 = max ax bx
      x0 = a0 + gold * (b0 - a0)
      fx0 = f x0
      (xBest, vBest, hist, iters, conv) =
        loopBrent cfg f a0 b0 x0 x0 x0 fx0 fx0 fx0 0 0 [fx0]
      vUser = case bcDir cfg of { Minimize -> vBest; Maximize -> negate vBest }
      histU = case bcDir cfg of { Minimize -> reverse hist; Maximize -> map negate (reverse hist) }
  in OptimResult [xBest] vUser histU iters conv

-- | Brent 反復。Numerical Recipes "brent" の素直な移植 (簡略版)。
-- 状態: a, b (区間), x (現在最良), w (2 番目), v (3 番目), 対応する f 値。
-- e: 一つ前の `d` (放物線補間ステップの記憶)、d: 現ステップ幅。
loopBrent :: BrentConfig
          -> (Double -> Double)
          -> Double -> Double                 -- a, b
          -> Double -> Double -> Double       -- x, w, v
          -> Double -> Double -> Double       -- fx, fw, fv
          -> Int -> Double                    -- iter, e
          -> [Double]                         -- hist
          -> (Double, Double, [Double], Int, Bool)
loopBrent cfg f a b x w v fx fw fv iter e hist
  | iter >= bcMaxIter cfg = (x, fx, hist, iter, False)
  | abs (x - xm) <= tol2 - 0.5 * (b - a) = (x, fx, hist, iter, True)
  | otherwise =
      let -- 放物線補間を試み、失敗時は黄金分割
          (d, eN) = parabolicOrGolden
          u  = if abs d >= tol1 then x + d else x + signum d * tol1
          fu = f u
      in if fu <= fx
           then
             let (aN, bN) = if u >= x then (x, b) else (a, x)
                 (xN, wN, vN, fxN, fwN, fvN) = (u, x, w, fu, fx, fw)
             in loopBrent cfg f aN bN xN wN vN fxN fwN fvN (iter + 1) eN (fxN : hist)
           else
             let (aN, bN) = if u < x then (u, b) else (a, u)
                 (xN, wN, vN, fxN, fwN, fvN) =
                   if fu <= fw || w == x
                     then (x, u, w, fx, fu, fw)
                     else if fu <= fv || v == x || v == w
                            then (x, w, u, fx, fw, fu)
                            else (x, w, v, fx, fw, fv)
             in loopBrent cfg f aN bN xN wN vN fxN fwN fvN (iter + 1) eN (fxN : hist)
  where
    xm   = 0.5 * (a + b)
    tol1 = bcTol cfg * abs x + 1e-10
    tol2 = 2 * tol1
    parabolicOrGolden =
      if abs e > tol1
        then
          let r0 = (x - w) * (fx - fv)
              q0 = (x - v) * (fx - fw)
              p0 = (x - v) * q0 - (x - w) * r0
              q1 = 2 * (q0 - r0)
              p  = if q1 > 0 then -p0 else p0
              q  = abs q1
              eOld = e
              dCand = p / q
              ok = abs p < abs (0.5 * q * eOld)
                   && p > q * (a - x) && p < q * (b - x)
          in if ok then (dCand, dCand) else goldenStep
        else goldenStep
    goldenStep =
      let eG = if x >= xm then a - x else b - x
          dG = gold * eG
      in (dG, eG)

-- | Bracket search: find @(a, c, b)@ such that @f(c) < f(a)@ and
-- @f(c) < f(b)@.
--
-- A simple expanding scan (a slimmed-down @mnbrak@ from Numerical
-- Recipes). Returns 'Nothing' if no bracket is found.
bracketMinimum :: ([Double] -> Double)
               -> Double               -- ^ Initial @a@.
               -> Double               -- ^ Initial @b@.
               -> Maybe (Double, Double, Double)
                                       -- ^ @(a, c, b)@ with @f(c) < f(a), f(b)@.
bracketMinimum fUser a0 b0 =
  let f x = fUser [x]
      step = (b0 - a0) * 0.5
      go a b k
        | k > 100   = Nothing
        | f c < f a && f c < f b = Just (a, c, b)
        | otherwise = go (a - step) (b + step) (k + 1)
        where
          c = 0.5 * (a + b)
  in go a0 b0 0
