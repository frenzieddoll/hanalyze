-- | L-BFGS (Limited-memory BFGS) 準ニュートン法。
--
-- Liu & Nocedal (1989). 大規模・滑らかな目的関数の局所最適化に最適。
-- 数百〜数万次元でも実用 (BFGS の O(n²) メモリを O(mn) に削減; m=10 が標準)。
--
-- 機能:
-- - Two-loop recursion で逆 Hessian × 勾配を計算 (履歴サイズ m)
-- - 線形探索: backtracking + Armijo 条件 (簡易、Wolfe 完全準拠ではない)
-- - 数値勾配のオプション (`runLBFGSNumeric`)
--
-- 計算量: 1 反復あたり関数+勾配評価 O(数回) + メモリ O(m·n)。
module Optim.LBFGS
  ( LBFGSConfig (..)
  , defaultLBFGSConfig
  , runLBFGS
  , runLBFGSWith
  , runLBFGSNumeric
  ) where

import Optim.Common
import qualified Optim.Numeric as ON

-- | L-BFGS 設定。
data LBFGSConfig = LBFGSConfig
  { lbStop    :: !StopCriteria
  , lbMemory  :: !Int        -- ^ 履歴ベクトル数 m (典型 5〜20)
  , lbLSMax   :: !Int        -- ^ 線形探索最大反復
  , lbLSC1    :: !Double     -- ^ Armijo 定数 c1 (典型 1e-4)
  , lbLSShrink :: !Double    -- ^ backtracking 縮小率 (典型 0.5)
  , lbDir     :: !Direction
  , lbBounds  :: !(Maybe Bounds)  -- ^ box 制約 (任意)。指定時は f と ∇f に
                                   --   `boundsPenalty` の二次罰則を加算する
                                   --   soft penalty 方式 (k=1e6)
  } deriving (Show, Eq)

defaultLBFGSConfig :: LBFGSConfig
defaultLBFGSConfig = LBFGSConfig
  { lbStop     = defaultStopCriteria { stMaxIter = 200 }
  , lbMemory   = 10
  , lbLSMax    = 25
  , lbLSC1     = 1e-4
  , lbLSShrink = 0.5
  , lbDir      = Minimize
  , lbBounds   = Nothing
  }

-- | 解析勾配を渡す版。
runLBFGSWith :: LBFGSConfig
             -> ([Double] -> Double)        -- f
             -> ([Double] -> [Double])      -- ∇f
             -> [Double]                    -- x0
             -> IO OptimResult
runLBFGSWith cfg fUser gUser x0 =
  let mbs       = lbBounds cfg
      fPenal xs = fUser xs + boundsPenalty mbs xs
      gPenal xs =
        let base = gUser xs
            -- ∂/∂x_i (k * d_i^2) = 2k * d_i * (sign 反映) — clip 範囲外時のみ
            grad = case mbs of
              Nothing -> map (const 0) xs
              Just bs -> let k = 1e6 :: Double
                         in [ if x < lo then 2*k*(x - lo)
                              else if x > hi then 2*k*(x - hi)
                              else 0
                            | ((lo, hi), x) <- zip bs xs ]
        in zipWith (+) base grad
      f = flipFor (lbDir cfg) fPenal
      g = case lbDir cfg of
            Minimize -> gPenal
            Maximize -> map negate . gPenal
      f0 = f x0
      g0 = g x0
      (xEnd, fEnd, hist, iters, conv) =
        loop cfg f g 0 x0 f0 g0 [] [] [f0]
      vUser = case lbDir cfg of
                Minimize -> fEnd
                Maximize -> negate fEnd
      histUser = case lbDir cfg of
                   Minimize -> reverse hist
                   Maximize -> map negate (reverse hist)
  in pure $ OptimResult
       { orBest      = xEnd
       , orValue     = vUser
       , orHistory   = histUser
       , orIters     = iters
       , orConverged = conv
       }

-- | 既定設定 + 解析勾配。
runLBFGS :: ([Double] -> Double)
         -> ([Double] -> [Double])
         -> [Double]
         -> IO OptimResult
runLBFGS = runLBFGSWith defaultLBFGSConfig

-- | 勾配を中央差分で自動計算する版。h=1e-5 既定。
runLBFGSNumeric :: LBFGSConfig
                -> ([Double] -> Double)
                -> [Double]
                -> IO OptimResult
runLBFGSNumeric cfg f x0 =
  runLBFGSWith cfg f (ON.numGradCentral 1e-5 f) x0

-- | 反復本体。s_k = x_{k+1} - x_k、y_k = g_{k+1} - g_k の履歴を最新 m 個保持。
loop :: LBFGSConfig
     -> ([Double] -> Double)
     -> ([Double] -> [Double])
     -> Int                          -- 反復カウンタ
     -> [Double]                     -- 現在 x
     -> Double                       -- f(x)
     -> [Double]                     -- ∇f(x)
     -> [[Double]]                   -- s 履歴 (新しい先頭)
     -> [[Double]]                   -- y 履歴 (新しい先頭)
     -> [Double]                     -- best 値履歴 (逆順)
     -> ([Double], Double, [Double], Int, Bool)
loop cfg f g iter x fx gx ss ys hist
  | iter >= stMaxIter (lbStop cfg) = (x, fx, hist, iter, False)
  | gnorm < stTolFun (lbStop cfg) = (x, fx, hist, iter, True)
  | otherwise =
      let -- 探索方向 d = -H*g (Two-loop recursion で計算)
          d   = twoLoop ss ys gx
          -- 線形探索 (backtracking + Armijo)
          (xN, fN, alpha) = lineSearch cfg f x fx gx d
      in if alpha < 1e-16
           then (x, fx, hist, iter, True)  -- 進展なし → 終了
           else
             let gN  = g xN
                 sN  = zipWith (-) xN x
                 yN  = zipWith (-) gN gx
                 ssN = take (lbMemory cfg) (sN : ss)
                 ysN = take (lbMemory cfg) (yN : ys)
                 dx  = vecMaxAbs sN
             in if dx < stTolX (lbStop cfg) && abs (fx - fN) < stTolFun (lbStop cfg)
                  then (xN, fN, fN : hist, iter + 1, True)
                  else loop cfg f g (iter + 1) xN fN gN ssN ysN (fN : hist)
  where
    gnorm = sqrt (sum [v*v | v <- gx])

-- | Two-loop recursion: r = H_k · q をスケールフリーに計算。
-- ss / ys は新しい先頭で揃って並ぶ前提 (s_{k-1}, s_{k-2}, ..., s_{k-m})。
twoLoop :: [[Double]] -> [[Double]] -> [Double] -> [Double]
twoLoop [] _ q = map negate q                 -- 履歴なし: 単純な負勾配
twoLoop ss ys q =
  let pairs   = zip ss ys                       -- 新しい順
      rhos    = [ 1 / dot y s | (s, y) <- pairs ]
      triples = zip3 ss ys rhos                 -- 新しい順
      -- 第 1 ループ: 新しい順 (i = k-1, k-2, ...) に α_i, q を更新
      step1 (qCur, accAlphas) (s, y, rho) =
        let a = rho * dot s qCur
            qN = zipWith (\v yi -> v - a * yi) qCur y
        in (qN, a : accAlphas)             -- accAlphas は新しい順
      (qFinal, alphasNew) = foldl step1 (q, []) triples
      -- スケーリング: H_0 = γ I, γ = (s_0^T y_0) / (y_0^T y_0)
      (s0, y0) = (head ss, head ys)
      gamma    = dot s0 y0 / max 1e-16 (dot y0 y0)
      r0       = map (* gamma) qFinal
      -- 第 2 ループ: 古い順 (k-m, ..., k-1) で r 更新
      triplesAlphas = reverse (zip triples (reverse alphasNew))
                      -- = [((s_oldest,y,rho), alpha_oldest), ...]
      step2 rCur ((s, y, rho), alpha) =
        let beta = rho * dot y rCur
            scal = alpha - beta
        in zipWith (\rv si -> rv + scal * si) rCur s
      r        = foldl step2 r0 triplesAlphas
  in map negate r

-- | backtracking + Armijo 条件 (`f(x + αd) ≤ f(x) + c1 α gᵀd`)。
lineSearch :: LBFGSConfig
           -> ([Double] -> Double)
           -> [Double] -> Double -> [Double] -> [Double]
           -> ([Double], Double, Double)
lineSearch cfg f x fx g d =
  let gtd = dot g d
      go alpha k
        | k >= lbLSMax cfg = (xCand, f xCand, alpha)
        | armijo            = (xCand, fxCand, alpha)
        | otherwise         = go (alpha * lbLSShrink cfg) (k + 1)
        where
          xCand  = zipWith (\xi di -> xi + alpha * di) x d
          fxCand = f xCand
          armijo = fxCand <= fx + lbLSC1 cfg * alpha * gtd
  in go 1.0 0

-- | ベクトル内積。
dot :: [Double] -> [Double] -> Double
dot a b = sum (zipWith (*) a b)

-- | ‖v‖∞
vecMaxAbs :: [Double] -> Double
vecMaxAbs = maximum . map abs
