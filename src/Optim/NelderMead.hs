-- | Nelder-Mead simplex method (downhill simplex).
--
-- Nelder & Mead (1965). Gradient-free, easy to implement at low dimension
-- (1-30), and stable for local optimization. The default behind R's
-- @optim(method="Nelder-Mead")@.
--
-- Algorithm: maintain an @n+1@-vertex simplex; each iteration replaces the
-- worst vertex via reflect / expand / contract / shrink. Standard Wright
-- (1996) parameters @ρ = 1, χ = 2, γ = 1/2, σ = 1/2@. This implementation
-- follows the canonical form of Lagarias et al. (1998).
--
-- Cost: 1-2 function evaluations per iteration (@n@ on shrink). Convergence
-- becomes slow for larger @n@ — practical up to @n ≤ 10@.
module Optim.NelderMead
  ( NMConfig (..)
  , defaultNMConfig
  , runNelderMead
  , runNelderMeadWith
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Optim.Common

-- | Nelder-Mead configuration.
--
-- Standard parameters:
--
--   * Reflection      @ρ = 1.0@
--   * Expansion       @χ = 2.0@
--   * Contraction     @γ = 0.5@
--   * Shrink          @σ = 0.5@
data NMConfig = NMConfig
  { nmStop     :: !StopCriteria
  , nmInitStep :: !Double      -- ^ Initial simplex step (per axis).
  , nmRho      :: !Double      -- ^ Reflection coefficient @ρ@.
  , nmChi      :: !Double      -- ^ Expansion coefficient @χ@.
  , nmGamma    :: !Double      -- ^ Contraction coefficient @γ@.
  , nmSigma    :: !Double      -- ^ Shrink coefficient @σ@.
  , nmDir      :: !Direction
  , nmBounds   :: !(Maybe Bounds)  -- ^ Optional box constraints; when set,
                                   --   adds 'boundsPenalty' to the objective
                                   --   (soft-penalty enforcement).
  } deriving (Show, Eq)

-- | Default configuration: standard parameters, minimization, no bounds,
-- step 0.5, default 'StopCriteria'.
defaultNMConfig :: NMConfig
defaultNMConfig = NMConfig
  { nmStop     = defaultStopCriteria
  , nmInitStep = 0.5
  , nmRho      = 1.0
  , nmChi      = 2.0
  , nmGamma    = 0.5
  , nmSigma    = 0.5
  , nmDir      = Minimize
  , nmBounds   = Nothing
  }

-- | Run Nelder-Mead with the default configuration.
runNelderMead :: ([Double] -> Double)   -- ^ Objective function.
              -> [Double]                -- ^ Initial point @x₀@.
              -> IO OptimResult
runNelderMead = runNelderMeadWith defaultNMConfig

-- | Run Nelder-Mead with a user-specified configuration.
runNelderMeadWith :: NMConfig
                  -> ([Double] -> Double)
                  -> [Double]
                  -> IO OptimResult
runNelderMeadWith cfg fUser x0 =
  let n         = length x0
      fPenal xs = fUser xs + boundsPenalty (nmBounds cfg) xs
      f         = flipFor (nmDir cfg) fPenal   -- 内部は常に最小化
      step      = nmInitStep cfg
      -- 初期単体: x0 + step*e_i
      vertices0 = (x0, f x0) : [ (x, f x) | i <- [0 .. n - 1]
                                          , let x = perturb x0 i step ]
      sortedV   = sortBy (comparing snd) vertices0
      stop      = nmStop cfg
      hist0     = [ snd (head sortedV) ]
      (vEnd, hEnd, iters, conv) = loop cfg stop f 0 sortedV hist0
      (xb, vb) = head vEnd
      vbUser   = case nmDir cfg of
                   Minimize -> vb
                   Maximize -> negate vb
      histUser = case nmDir cfg of
                   Minimize -> reverse hEnd
                   Maximize -> map negate (reverse hEnd)
  in pure $ OptimResult
       { orBest      = xb
       , orValue     = vbUser
       , orHistory   = histUser
       , orIters     = iters
       , orConverged = conv
       }

-- | 軸 i 方向に step だけ動かす。
perturb :: [Double] -> Int -> Double -> [Double]
perturb xs i step =
  [ if k == i then v + (if v == 0 then step else step * (1 + abs v))
              else v
  | (k, v) <- zip [0 ..] xs ]

-- | 反復本体。引数 vertices は f 値で昇順ソート済を維持する。
loop :: NMConfig -> StopCriteria
     -> ([Double] -> Double)
     -> Int                      -- 反復カウンタ
     -> [([Double], Double)]      -- 単体頂点 ([(x, f x)] sorted ascending)
     -> [Double]                  -- best 値履歴 (逆順、新しい先頭)
     -> ([([Double], Double)], [Double], Int, Bool)
loop cfg stop f iter vertices hist
  | iter >= stMaxIter stop  = (vertices, hist, iter, False)
  | converged                = (vertices, hist, iter, True)
  | otherwise                = loop cfg stop f (iter + 1) newV newH
  where
    n        = length vertices - 1
    fBest    = snd (head vertices)
    fWorst   = snd (last vertices)
    fSecond  = snd (vertices !! (n - 1))     -- 2 番目に悪い
    -- 収束判定: f 値の幅 < tolFun または (将来) 単体の x 幅 < tolX
    converged = abs (fWorst - fBest) < stTolFun stop
                || simplexSpread vertices < stTolX stop
    -- 重心 (worst を除外して平均)
    centroid = avgVecs (map fst (init vertices))
    xWorst   = fst (last vertices)
    -- 反射点
    xR  = combine (1 + nmRho cfg) centroid (nmRho cfg) xWorst
    fR  = f xR
    (newV, newH) =
      if fR < fBest
        then -- 拡張
          let xE = combine (1 + nmRho cfg * nmChi cfg) centroid
                           (nmRho cfg * nmChi cfg) xWorst
              fE = f xE
              chosen = if fE < fR then (xE, fE) else (xR, fR)
          in update chosen vertices
      else if fR < fSecond
        then update (xR, fR) vertices
      else
        let -- 縮小
            (xC, fC) =
              if fR < fWorst
                then -- 外縮小
                  let xOC = combine (1 + nmRho cfg * nmGamma cfg) centroid
                                    (nmRho cfg * nmGamma cfg) xWorst
                  in (xOC, f xOC)
                else -- 内縮小
                  let xIC = combine (1 - nmGamma cfg) centroid
                                    (- nmGamma cfg) xWorst
                  in (xIC, f xIC)
        in if fC < fWorst
             then update (xC, fC) vertices
             else
               -- 全縮小: best を中心に他全頂点を σ 倍に縮める
               let xb = fst (head vertices)
                   shrunk = head vertices :
                            [ let xk = zipWith (\b v -> b + nmSigma cfg * (v - b)) xb x
                              in (xk, f xk)
                            | (x, _) <- tail vertices ]
                   sortedS = sortBy (comparing snd) shrunk
               in (sortedS, snd (head sortedS) : hist)
    update (xN, fN) vs =
      let replaced = init vs ++ [(xN, fN)]
          sortedR  = sortBy (comparing snd) replaced
      in (sortedR, snd (head sortedR) : hist)

-- | 単体の最大辺長 (∞-norm)。tolX 判定用。
simplexSpread :: [([Double], Double)] -> Double
simplexSpread vs =
  let xs = map fst vs
      x0 = head xs
  in maximum [ maximum (zipWith (\a b -> abs (a - b)) x0 x) | x <- tail xs ]

-- | s1 * a - s2 * b の線形結合 (純粋にベクトル演算ユーティリティ)。
combine :: Double -> [Double] -> Double -> [Double] -> [Double]
combine s1 a s2 b = zipWith (\ai bi -> s1 * ai - s2 * bi) a b

-- | 同じ長さの複数ベクトルの平均。
avgVecs :: [[Double]] -> [Double]
avgVecs xs =
  let n = fromIntegral (length xs) :: Double
  in foldr1 (zipWith (+)) (map (map (/ n)) xs)
    -- 等価: map (/n) (foldr1 (zipWith (+)) xs)、こちらの方が overflow 緩和的
