-- |
-- Module      : Hanalyze.Optim.LBFGS
-- Description : L-BFGS (限定記憶 BFGS) 準ニュートン法
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- L-BFGS (Limited-memory BFGS) quasi-Newton method.
--
-- Liu & Nocedal (1989). The standard for local optimization of large,
-- smooth objectives — practical at hundreds to tens of thousands of
-- dimensions (memory @O(mn)@ versus BFGS's @O(n²)@; @m = 10@ is typical).
--
-- Features:
--
--   * Two-loop recursion for inverse-Hessian × gradient (history size @m@).
--   * Line search: backtracking + Armijo condition (simple; not full Wolfe).
--   * Numeric-gradient variant ('runLBFGSNumeric').
--
-- Implementation note (L1, the no-list rule): the public API still
-- exchanges @[Double]@ at the boundaries (zero-cost adapter), but every
-- inner-loop arithmetic operation runs on @LA.Vector Double@ via BLAS.
-- This eliminates the per-step Haskell list overhead that previously
-- dominated the runtime (verified on the GLM bench in G2).
{-# LANGUAGE StrictData #-}

module Hanalyze.Optim.LBFGS
  ( LBFGSConfig (..)
  , defaultLBFGSConfig
  , runLBFGS
  , runLBFGSWith
  , runLBFGSWithPure
  , runLBFGSNumeric
    -- * Vector-native variants (avoid list↔Vector conversion on every step)
  , runLBFGSWithV
  , runLBFGSWithVResult
  ) where

import qualified Numeric.LinearAlgebra as LA
import           Hanalyze.Optim.Common
import qualified Hanalyze.Optim.Numeric as ON

-- | L-BFGS 設定。
data LBFGSConfig = LBFGSConfig
  { lbStop    :: !StopCriteria
  , lbMemory   :: !Int        -- ^ History size @m@ (5–20 typical).
  , lbLSMax    :: !Int        -- ^ Maximum line-search iterations.
  , lbLSC1     :: !Double     -- ^ Armijo constant @c₁@ (1e-4 typical).
  , lbLSShrink :: !Double     -- ^ Backtracking shrink rate (0.5 typical).
  , lbDir      :: !Direction
  , lbBounds   :: !(Maybe Bounds)  -- ^ Optional box constraints. When set,
                                   --   adds a quadratic 'boundsPenalty'
                                   --   (with @k = 10^6@) to both @f@ and
                                   --   @∇f@ (soft-penalty enforcement).
  } deriving (Show, Eq)

-- | Default L-BFGS configuration: history 10, Armijo c1 1e-4,
-- backtracking shrink 0.5, minimization, no bounds. Stop criteria
-- match scipy's @\"L-BFGS-B\"@ defaults (@maxiter = 1000@,
-- @ftol = 1e-12@) so smooth problems can converge to near-machine
-- precision.
defaultLBFGSConfig :: LBFGSConfig
defaultLBFGSConfig = LBFGSConfig
  { lbStop     = defaultStopCriteria { stMaxIter = 1000
                                     , stTolFun  = 1e-12
                                     , stTolX    = 1e-12 }
  , lbMemory   = 10
  , lbLSMax    = 25
  , lbLSC1     = 1e-4
  , lbLSShrink = 0.5
  , lbDir      = Minimize
  , lbBounds   = Nothing
  }

-- | Run L-BFGS with an explicit analytic gradient.
runLBFGSWith :: LBFGSConfig
             -> ([Double] -> Double)        -- ^ Objective @f@.
             -> ([Double] -> [Double])      -- ^ Gradient @∇f@.
             -> [Double]                    -- ^ Initial point @x₀@.
             -> IO OptimResult
runLBFGSWith cfg fUser gUser x0 = pure (runLBFGSWithPure cfg fUser gUser x0)

-- | 純粋版 ('runLBFGSWith' は本体が完全に純粋 = @let … in pure result@ ゆえ IO は不要)。
-- 乱数を使わない決定的最適化なので、 純粋に閉じられる ('fitSVMPure' 等が利用)。
runLBFGSWithPure :: LBFGSConfig
                 -> ([Double] -> Double)
                 -> ([Double] -> [Double])
                 -> [Double]
                 -> OptimResult
runLBFGSWithPure cfg fUser gUser x0 =
  let mbs          = lbBounds cfg
      sign         = case lbDir cfg of { Minimize -> 1; Maximize -> -1 :: Double }
      -- The internal objective and gradient operate on LA.Vector Double.
      -- They wrap the user's [Double] callbacks; the per-call list
      -- conversion is unavoidable but its cost is dominated by the user
      -- function itself, not by the optimizer.
      fV :: LA.Vector Double -> Double
      fV v = let xs = LA.toList v
             in sign * (fUser xs + boundsPenalty mbs xs)
      gV :: LA.Vector Double -> LA.Vector Double
      gV v =
        let xs = LA.toList v
            base = LA.fromList (gUser xs)
            penalty = case mbs of
              Nothing -> LA.konst 0 (LA.size v)
              Just bs ->
                let k = 1e6 :: Double
                in LA.fromList
                     [ if x <  lo then 2*k*(x - lo)
                       else if x > hi then 2*k*(x - hi)
                       else 0
                     | ((lo, hi), x) <- zip bs xs ]
        in LA.scale sign (base + penalty)
      x0v   = LA.fromList x0
      f0    = fV x0v
      g0    = gV x0v
      (xEndV, fEnd, hist, iters, conv) =
        loop cfg fV gV 0 x0v f0 g0 [] [] [f0]
      vUser = sign * fEnd     -- == fEnd for Minimize, -fEnd for Maximize
      histUser = case lbDir cfg of
                   Minimize -> reverse hist
                   Maximize -> map negate (reverse hist)
  in OptimResult
       { orBest      = LA.toList xEndV
       , orValue     = vUser
       , orHistory   = histUser
       , orIters     = iters
       , orConverged = conv
       }

-- | Run L-BFGS with the default configuration and an analytic gradient.
runLBFGS :: ([Double] -> Double)
         -> ([Double] -> [Double])
         -> [Double]
         -> IO OptimResult
runLBFGS = runLBFGSWith defaultLBFGSConfig

-- | Numeric-gradient variant: gradients are computed by central
-- differences (@h = 1e-5@).
runLBFGSNumeric :: LBFGSConfig
                -> ([Double] -> Double)
                -> [Double]
                -> IO OptimResult
runLBFGSNumeric cfg f x0 =
  runLBFGSWith cfg f (ON.numGradCentral 1e-5 f) x0

-- | Vector-native variant: avoids the @[Double] ↔ Vector Double@
-- conversion that 'runLBFGSWith' incurs on every objective and
-- gradient call. Use this when the caller already has hmatrix
-- vectors / matrices on hand (e.g. GLM, GP).
runLBFGSWithV
  :: LBFGSConfig
  -> (LA.Vector Double -> Double)
  -> (LA.Vector Double -> LA.Vector Double)
  -> LA.Vector Double
  -> IO OptimResult
runLBFGSWithV cfg fUser gUser x0v = do
  res <- runLBFGSWithVResult cfg fUser gUser x0v
  pure res

-- | Like 'runLBFGSWithV'. Provided as a longer-named alias so the
-- export list is unambiguous when both list- and Vector-native APIs
-- need to be referenced from a single import.
runLBFGSWithVResult
  :: LBFGSConfig
  -> (LA.Vector Double -> Double)
  -> (LA.Vector Double -> LA.Vector Double)
  -> LA.Vector Double
  -> IO OptimResult
runLBFGSWithVResult cfg fUser gUser x0v =
  let mbs   = lbBounds cfg
      sign  = case lbDir cfg of { Minimize -> 1; Maximize -> -1 :: Double }
      fV v = let pen = case mbs of
                   Nothing -> 0
                   Just bs -> boundsPenalty (Just bs) (LA.toList v)
             in sign * (fUser v + pen)
      gV v = case mbs of
        Nothing -> LA.scale sign (gUser v)
        Just bs ->
          let xs    = LA.toList v
              k     = 1e6 :: Double
              penG  = LA.fromList
                [ if x <  lo then 2*k*(x - lo)
                  else if x > hi then 2*k*(x - hi)
                  else 0
                | ((lo, hi), x) <- zip bs xs ]
          in LA.scale sign (gUser v + penG)
      f0       = fV x0v
      g0       = gV x0v
      (xEndV, fEnd, hist, iters, conv) =
        loop cfg fV gV 0 x0v f0 g0 [] [] [f0]
      vUser    = sign * fEnd
      histUser = case lbDir cfg of
                   Minimize -> reverse hist
                   Maximize -> map negate (reverse hist)
  in pure $ OptimResult
       { orBest      = LA.toList xEndV
       , orValue     = vUser
       , orHistory   = histUser
       , orIters     = iters
       , orConverged = conv
       }

-- ---------------------------------------------------------------------------
-- Inner loop, all Vector
-- ---------------------------------------------------------------------------

-- | Iteration body. @s_k = x_{k+1} - x_k@, @y_k = g_{k+1} - g_k@; the
-- last @m@ are kept (newest at the head).
loop :: LBFGSConfig
     -> (LA.Vector Double -> Double)
     -> (LA.Vector Double -> LA.Vector Double)
     -> Int                                       -- 反復カウンタ
     -> LA.Vector Double                          -- 現在 x
     -> Double                                    -- f(x)
     -> LA.Vector Double                          -- ∇f(x)
     -> [LA.Vector Double]                        -- s 履歴 (新しい先頭)
     -> [LA.Vector Double]                        -- y 履歴 (新しい先頭)
     -> [Double]                                  -- best 値履歴 (逆順)
     -> (LA.Vector Double, Double, [Double], Int, Bool)
loop cfg f g iter x fx gx ss ys hist
  | iter >= stMaxIter (lbStop cfg) = (x, fx, hist, iter, False)
  | gnorm < stTolFun (lbStop cfg)  = (x, fx, hist, iter, True)
  | otherwise =
      let d = twoLoop ss ys gx
          -- 初回反復 (曲率履歴なし) は方向が未スケールの最急降下 (‖d‖=‖g‖)。
          -- 勾配が大きい問題で α=1 の第1歩を打つと巨大にオーバーシュートし、
          -- 平坦な退化解に嵌って勾配消失で誤収束する (GP 周辺尤度で実測:
          -- ℓ が真の峰 105 を越えて 1e12 に飛ぶ)。Nocedal & Wright §3.5 に従い
          -- 初回のみ α₀ = min(1, 1/‖g‖₁) に抑える (2 回目以降は quasi-Newton
          -- 方向が自己スケールするので α=1 が適切)。
          alpha0 | null ss   = min 1 (1 / max 1e-16 (LA.norm_1 gx))
                 | otherwise = 1
          (xN, fN, alpha) = lineSearch cfg f x fx gx d alpha0
      in if alpha < 1e-16
           then (x, fx, hist, iter, True)
           else
             let gN  = g xN
                 sN  = xN - x
                 yN  = gN - gx
                 ssN = take (lbMemory cfg) (sN : ss)
                 ysN = take (lbMemory cfg) (yN : ys)
                 dx  = LA.norm_Inf sN
             in if dx < stTolX (lbStop cfg)
                   && abs (fx - fN) < stTolFun (lbStop cfg)
                  then (xN, fN, fN : hist, iter + 1, True)
                  else loop cfg f g (iter + 1) xN fN gN ssN ysN (fN : hist)
  where
    gnorm = LA.norm_2 gx

-- | Two-loop recursion: @r = H_k · q@, computed scale-free.
-- @ss@ / @ys@ are aligned with the newest at the head
-- (@s_{k-1}, s_{k-2}, ..., s_{k-m}@).
twoLoop :: [LA.Vector Double] -> [LA.Vector Double]
        -> LA.Vector Double -> LA.Vector Double
twoLoop [] _ q = LA.scale (-1) q                 -- 履歴なし: 単純な負勾配
twoLoop ss ys q =
  let pairs   = zip ss ys                          -- 新しい順
      rhos    = [ 1 / LA.dot y s | (s, y) <- pairs ]
      triples = zip3 ss ys rhos
      -- 第 1 ループ
      step1 (qCur, accAlphas) (s, y, rho) =
        let a  = rho * LA.dot s qCur
            qN = qCur - LA.scale a y
        in (qN, a : accAlphas)
      (qFinal, alphasNew) = foldl step1 (q, []) triples
      -- スケーリング: H_0 = γ I, γ = (s_0^T y_0) / (y_0^T y_0)
      (s0, y0) = (head ss, head ys)
      gamma    = LA.dot s0 y0 / max 1e-16 (LA.dot y0 y0)
      r0       = LA.scale gamma qFinal
      -- 第 2 ループ
      triplesAlphas = reverse (zip triples (reverse alphasNew))
      step2 rCur ((s, y, rho), alpha) =
        let beta = rho * LA.dot y rCur
            scal = alpha - beta
        in rCur + LA.scale scal s
      r        = foldl step2 r0 triplesAlphas
  in LA.scale (-1) r

-- | backtracking + Armijo 条件 @f(x + αd) ≤ f(x) + c1 α gᵀd@。
-- @alpha0@ = 初期ステップ幅 (通常 1.0、初回最急降下では 1/‖g‖₁ 等で抑える)。
lineSearch :: LBFGSConfig
           -> (LA.Vector Double -> Double)
           -> LA.Vector Double -> Double
           -> LA.Vector Double -> LA.Vector Double
           -> Double                                  -- ^ 初期ステップ幅 α₀
           -> (LA.Vector Double, Double, Double)
lineSearch cfg f x fx g d alpha0 =
  let gtd = LA.dot g d
      go alpha k
        | k >= lbLSMax cfg = (xCand, f xCand, alpha)
        | armijo           = (xCand, fxCand, alpha)
        | otherwise        = go (alpha * lbLSShrink cfg) (k + 1)
        where
          xCand  = x + LA.scale alpha d
          fxCand = f xCand
          armijo = fxCand <= fx + lbLSC1 cfg * alpha * gtd
  in go alpha0 0
