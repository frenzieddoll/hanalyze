-- | 1D 補間 (Linear / Natural cubic spline / PCHIP)。
--
-- 観測点 [(x_i, y_i)] (x 昇順、distinct) から連続関数 `Double -> Double` を構築。
-- 範囲外 (x < x_0 or x > x_{n-1}) は両端のセグメントを線形外挿。
--
-- 用途: 歯抜け wide CSV を long 化したあと、共通 grid に揃える前段補間。
-- (`DataIO.Preprocess.regridLong` から呼ばれる)
module Stat.Interpolate
  ( InterpKind (..)
  , interp1d
  ) where

import           Data.List (sortBy)
import           Data.Ord  (comparing)
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU

-- | 補間方式。
--
-- - 'Linear'         : 区間ごと線形補間。最頑健、外挿でも発散しない。
-- - 'NaturalSpline'  : 自然 3 次スプライン (端点で 2 階導関数 0)。滑らか、overshoot あり。
-- - 'PCHIP'          : Piecewise Cubic Hermite Interpolating Polynomial。
--                      局所単調性を保つ (Fritsch-Carlson 1980)。spline overshoot 回避。
data InterpKind
  = Linear
  | NaturalSpline
  | PCHIP
  deriving (Show, Eq)

-- | 観測点から補間関数を構築。x は内部で sortBy + dedupe される。
--
-- 制限: 観測点 < 2 のときは constant 関数 (1 点なら y_0、空なら 0) を返す。
interp1d :: InterpKind -> [(Double, Double)] -> (Double -> Double)
interp1d _    []         = const 0
interp1d _    [(_, y)]   = const y
interp1d kind pts0       =
  let pts = dedupe (sortBy (comparing fst) pts0)
      xs  = U.fromList (map fst pts)
      ys  = U.fromList (map snd pts)
  in case kind of
       Linear        -> linearAt xs ys
       NaturalSpline -> naturalSplineAt xs ys
       PCHIP         -> pchipAt xs ys
  where
    -- 同一 x の重複は y を平均化して 1 点にまとめる。
    dedupe :: [(Double, Double)] -> [(Double, Double)]
    dedupe []     = []
    dedupe (z:zs) = go z 1 [snd z] zs
      where
        go (x, _) n acc [] = [(x, sum acc / fromIntegral (n :: Int))]
        go (x, _) n acc ((x', y'):rest)
          | abs (x' - x) < 1e-15 = go (x, 0) (n + 1) (y' : acc) rest
          | otherwise            = (x, sum acc / fromIntegral n)
                                 : go (x', y') 1 [y'] rest

-- ---------------------------------------------------------------------------
-- 共通: x が含まれる区間 [x_i, x_{i+1}] の i を二分探索
-- ---------------------------------------------------------------------------

-- | x の挿入位置を返す。範囲外は端 (0 or n-2) にクランプ。
findSegment :: U.Vector Double -> Double -> Int
findSegment xs x =
  let n = U.length xs
      go lo hi
        | hi - lo <= 1 = lo
        | otherwise    =
            let mid = (lo + hi) `div` 2
            in if xs U.! mid > x then go lo mid else go mid hi
  in max 0 (min (n - 2) (go 0 (n - 1)))

-- ---------------------------------------------------------------------------
-- Linear
-- ---------------------------------------------------------------------------

linearAt :: U.Vector Double -> U.Vector Double -> Double -> Double
linearAt xs ys x =
  let i  = findSegment xs x
      x0 = xs U.! i
      x1 = xs U.! (i + 1)
      y0 = ys U.! i
      y1 = ys U.! (i + 1)
      t  = (x - x0) / (x1 - x0)
  in y0 + t * (y1 - y0)

-- ---------------------------------------------------------------------------
-- Natural cubic spline (端点で y'' = 0)
-- ---------------------------------------------------------------------------

-- | 端点で 2 階導関数 0 の自然スプラインの 2 階導関数 m を Thomas algorithm で解く。
naturalSplineAt :: U.Vector Double -> U.Vector Double -> Double -> Double
naturalSplineAt xs ys =
  let n = U.length xs
      h = U.generate (n - 1) (\i -> xs U.! (i + 1) - xs U.! i)
      -- 三重対角系: 内部点 i = 1 .. n-2 で
      --   h_{i-1} m_{i-1} + 2 (h_{i-1}+h_i) m_i + h_i m_{i+1}
      --     = 6 ( (y_{i+1}-y_i)/h_i - (y_i-y_{i-1})/h_{i-1} )
      -- m_0 = m_{n-1} = 0 (自然境界)
      m = solveNatural h ys
  in \x ->
       let i  = findSegment xs x
           x0 = xs U.! i
           x1 = xs U.! (i + 1)
           y0 = ys U.! i
           y1 = ys U.! (i + 1)
           hi = x1 - x0
           m0 = m U.! i
           m1 = m U.! (i + 1)
           a  = (x1 - x) / hi
           b  = (x - x0) / hi
       in a * y0 + b * y1
        + ((a*a*a - a) * m0 + (b*b*b - b) * m1) * (hi * hi) / 6

-- | n 次元 m を Thomas で解く (端 m_0 = m_{n-1} = 0)。
solveNatural :: U.Vector Double -> U.Vector Double -> U.Vector Double
solveNatural h ys =
  let n = U.length ys
  in if n < 3
       then U.replicate n 0
       else
         let -- 内部 (n-2) 元連立、行 i = 1..n-2 (1-indexed; 配列 indices 0..n-3)
             k = n - 2
             a = U.generate k (\i -> if i == 0      then 0 else h U.! i)
             b = U.generate k (\i -> 2 * (h U.! i + h U.! (i + 1)))
             c = U.generate k (\i -> if i == k - 1 then 0 else h U.! (i + 1))
             d = U.generate k (\i ->
                    let i'  = i + 1
                        hi  = h U.! i'
                        him = h U.! (i' - 1)
                    in 6 * ( (ys U.! (i' + 1) - ys U.! i') / hi
                           - (ys U.! i'       - ys U.! (i' - 1)) / him))
             mInner = thomas a b c d
         in U.fromList (0 : U.toList mInner ++ [0])

-- | 三重対角線形系 (Thomas algorithm)。
thomas :: U.Vector Double -> U.Vector Double -> U.Vector Double
       -> U.Vector Double -> U.Vector Double
thomas a b c d =
  let n = U.length b
      -- 前進消去
      go i cp dp
        | i >= n = (cp, dp)
        | otherwise =
            let cprev = if i == 0 then 0 else cp U.! (i - 1)
                dprev = if i == 0 then 0 else dp U.! (i - 1)
                ai    = a U.! i
                bi    = b U.! i
                ci    = c U.! i
                di    = d U.! i
                m     = bi - ai * cprev
                cp'   = ci / m
                dp'   = (di - ai * dprev) / m
            in go (i + 1) (cp U.// [(i, cp')]) (dp U.// [(i, dp')])
      (cFinal, dFinal) = go 0 (U.replicate n 0) (U.replicate n 0)
      -- 後退代入
      x = U.create $ do
            v <- U.thaw (U.replicate n 0)
            let backward i
                  | i < 0     = pure ()
                  | i == n - 1 = MU.unsafeWrite v i (dFinal U.! i) >> backward (i - 1)
                  | otherwise  = do
                      xn <- MU.unsafeRead v (i + 1)
                      MU.unsafeWrite v i (dFinal U.! i - cFinal U.! i * xn)
                      backward (i - 1)
            backward (n - 1)
            pure v
  in x

-- ---------------------------------------------------------------------------
-- PCHIP (Fritsch-Carlson 1980; monotone cubic Hermite)
-- ---------------------------------------------------------------------------

-- | PCHIP の傾き m_i を Fritsch-Carlson 法で計算してから区間ごとの 3 次 Hermite で評価。
pchipAt :: U.Vector Double -> U.Vector Double -> Double -> Double
pchipAt xs ys =
  let n  = U.length xs
      h  = U.generate (n - 1) (\i -> xs U.! (i + 1) - xs U.! i)
      d  = U.generate (n - 1) (\i -> (ys U.! (i + 1) - ys U.! i) / (h U.! i))
      m  = U.generate n (slopeAt h d n)
  in \x ->
       let i  = findSegment xs x
           x0 = xs U.! i
           x1 = xs U.! (i + 1)
           y0 = ys U.! i
           y1 = ys U.! (i + 1)
           hi = x1 - x0
           t  = (x - x0) / hi
           h00 = (1 + 2*t) * (1 - t) * (1 - t)
           h10 = t * (1 - t) * (1 - t)
           h01 = t * t * (3 - 2*t)
           h11 = t * t * (t - 1)
       in h00 * y0 + h10 * hi * (m U.! i)
        + h01 * y1 + h11 * hi * (m U.! (i + 1))

-- | Fritsch-Carlson 単調保存スロープ。
slopeAt :: U.Vector Double -> U.Vector Double -> Int -> Int -> Double
slopeAt h d n i
  | n < 2     = 0
  | i == 0    = endpointSlope (d U.! 0) (d U.! (min 1 (U.length d - 1)))
                              (h U.! 0) (h U.! (min 1 (U.length h - 1)))
  | i == n - 1 = endpointSlope (d U.! (n - 2)) (d U.! (max 0 (n - 3)))
                               (h U.! (n - 2)) (h U.! (max 0 (n - 3)))
  | otherwise  =
      let dPrev = d U.! (i - 1)
          dCur  = d U.! i
      in if dPrev * dCur <= 0
           then 0
           else
             let hPrev = h U.! (i - 1)
                 hCur  = h U.! i
                 w1 = 2 * hCur + hPrev
                 w2 = hCur + 2 * hPrev
             in (w1 + w2) / (w1 / dPrev + w2 / dCur)

-- | 端点の 3 点 quadratic estimate + Fritsch-Carlson の符号調整。
endpointSlope :: Double -> Double -> Double -> Double -> Double
endpointSlope d0 d1 h0 h1 =
  let m = ((2 * h0 + h1) * d0 - h0 * d1) / (h0 + h1)
  in if m * d0 <= 0
       then 0
       else if d0 * d1 < 0 && abs m > 3 * abs d0
              then 3 * d0
              else m
