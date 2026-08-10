{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | Phase 54.0 feasibility spike (計測先行・推測するな計測せよ)。
--
-- Phase 54 の本実装に入る前に、 2 つの不確実点を実測で確かめる小実験:
--
--   (Q1) AD over Vector の勾配保存:
--        `Numeric.AD.Mode.Reverse.Double.grad` が、 観測尤度を
--        ① list 内包 (現状の obsLogSum 形)・② 非ボックス Vector 上の fold・
--        ③ 十分統計量による fused 閉形式 (O(1)) で書いた log-density に対し、
--        いずれも **中心差分と一致する勾配** を返すか。
--        → 一致すれば 54.2 (観測尤度ベクトル化) の AD 前提が成立。
--
--   (Q2) ベクトル化/融合の per-grad 改善率:
--        ①②③ の 1 勾配あたり時間を `timeitIO` で計測し、 scalar 版 (①) 比の
--        改善率を出す。 ③ が桁で速ければ 54.2 の「vector-mean observe →
--        fused 配列 log-density」 の利得見込みを定量化できる。
--
-- 対象は Gaussian 線形回帰 y_i ~ Normal(a + b x_i, σ) (= m1Model 相当)。
-- σ は unconstrained u = log σ で持ち、 prior は a,b~Normal(0,10)・σ~Exp(1)
-- (HBM の logJointUnconstrained と同じ「prior + jacobian + 観測和」 構造)。
--
-- ★この spike は HBM 本体を一切いじらない。 観測和の 3 表現が AD で同値かつ
--   どれだけ速いかだけを切り出して測る独立実験。
module Main where

import           Control.Monad                  (forM_)
import qualified Data.Vector.Storable           as VS
import           Text.Printf                    (printf)

import qualified Numeric.AD.Mode.Reverse.Double as RevD

import           BenchUtil                      (timeitIO)

-- ---------------------------------------------------------------------------
-- 対数尤度 3 表現 (param = [a, b, u], σ = exp u)
-- ---------------------------------------------------------------------------

-- 共通の prior + jacobian (3 表現で同一)。
logPriorPart :: Floating a => a -> a -> a -> a
logPriorPart a b u =
  let s          = exp u
      lnNormal mu sig x = -0.5 * log (2 * pi) - log sig
                          - 0.5 * ((x - mu) / sig) ^ (2 :: Int)
      priorA     = lnNormal 0 10 a
      priorB     = lnNormal 0 10 b
      -- σ ~ Exponential 1: logDensity = log 1 - 1*σ = -σ。 jacobian dσ/du = σ → +u
      priorSigma = (-s) + u
  in priorA + priorB + priorSigma

-- 観測の 1 項 (Normal): -0.5 log(2π) - log s - 0.5 ((y - μ)/s)^2
obsTerm :: Floating a => a -> a -> a -> Double -> Double -> a
obsTerm a b s x y =
  let mu = a + b * realToFrac x
  in -0.5 * log (2 * pi) - log s
     - 0.5 * ((realToFrac y - mu) / s) ^ (2 :: Int)
{-# INLINE obsTerm #-}

-- ① scalar list 内包 (現状 obsLogSum と同じ形)。
logLikScalar :: Floating a => [Double] -> [Double] -> [a] -> a
logLikScalar xs ys ps =
  let (a : b : u : _) = ps
      s = exp u
  in logPriorPart a b u
     + sum [ obsTerm a b s x y | (x, y) <- zip xs ys ]

-- ② 非ボックス Storable Vector 上の手動 fold (list alloc を排除)。
--    データは VS.Vector Double (unboxed)、 累算器のみ AD スカラ (boxed)。
logLikVec :: Floating a => VS.Vector Double -> VS.Vector Double -> [a] -> a
logLikVec xs ys ps =
  let (a : b : u : _) = ps
      s = exp u
      n = VS.length xs
      go !acc i
        | i >= n    = acc
        | otherwise = go (acc + obsTerm a b s (xs `VS.unsafeIndex` i)
                                              (ys `VS.unsafeIndex` i)) (i + 1)
  in logPriorPart a b u + go 0 0

-- ③ 十分統計量による fused 閉形式 (O(1) per eval)。
--    Σ_i (y_i - a - b x_i)^2 = Syy - 2a Sy - 2b Sxy + n a^2 + 2ab Sx + b^2 Sxx
--    の 6 つの和は Double 定数として 1 回だけ前計算 → eval は a,b,s の多項式。
data SuffStat = SuffStat
  { ssN :: !Double, ssSx :: !Double, ssSy :: !Double
  , ssSxx :: !Double, ssSxy :: !Double, ssSyy :: !Double }

mkSuffStat :: [Double] -> [Double] -> SuffStat
mkSuffStat xs ys = SuffStat
  { ssN   = fromIntegral (length xs)
  , ssSx  = sum xs
  , ssSy  = sum ys
  , ssSxx = sum (map (\x -> x * x) xs)
  , ssSxy = sum (zipWith (*) xs ys)
  , ssSyy = sum (map (\y -> y * y) ys)
  }

logLikFused :: Floating a => SuffStat -> [a] -> a
logLikFused ss ps =
  let (a : b : u : _) = ps
      s  = exp u
      n  = realToFrac (ssN ss)
      sx = realToFrac (ssSx ss); sy = realToFrac (ssSy ss)
      sxx = realToFrac (ssSxx ss); sxy = realToFrac (ssSxy ss)
      syy = realToFrac (ssSyy ss)
      -- Σ resid^2 を展開した閉形式
      sse = syy - 2 * a * sy - 2 * b * sxy
            + n * a * a + 2 * a * b * sx + b * b * sxx
      obsSum = n * (-0.5 * log (2 * pi) - log s) - 0.5 / (s * s) * sse
  in logPriorPart a b u + obsSum

-- ---------------------------------------------------------------------------
-- 中心差分 (ground truth)
-- ---------------------------------------------------------------------------

centralDiff :: ([Double] -> Double) -> [Double] -> [Double]
centralDiff f ps =
  [ let h    = 1e-6 * (abs (ps !! j) + 1e-3)
        plus = f (bump j h)
        minu = f (bump j (-h))
    in (plus - minu) / (2 * h)
  | j <- [0 .. length ps - 1] ]
  where bump j d = [ if k == j then p + d else p | (k, p) <- zip [0 ..] ps ]

relErr :: [Double] -> [Double] -> Double
relErr g1 g2 = maximum
  [ abs (x - y) / (abs y + 1e-8) | (x, y) <- zip g1 g2 ]

-- ---------------------------------------------------------------------------
-- データ生成 (BenchHBMADModes と同形)
-- ---------------------------------------------------------------------------

genData :: Int -> ([Double], [Double])
genData n =
  let xs = [ 2.0 * sin (0.7 * fromIntegral i) | i <- [0 .. n - 1] ]
      ys = [ 2.0 + 1.5 * x + 0.3 * cos (1.3 * fromIntegral i)
           | (i, x) <- zip [0 :: Int ..] xs ]
  in (xs, ys)

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Phase 54.0 feasibility spike: 観測尤度ベクトル化 × Reverse.Double ===\n"
  let ps0 = [1.8, 1.4, log 0.35]   -- [a, b, u=log σ] (真値近傍)

  putStrLn "--- (Q1) 勾配の数値一致 (RevD.grad vs 中心差分・rel err) ---"
  forM_ [50, 200, 1000] $ \n -> do
    let (xs, ys) = genData n
        xv = VS.fromList xs; yv = VS.fromList ys
        ss = mkSuffStat xs ys
        gScalar = RevD.grad (logLikScalar xs ys) ps0
        gVec    = RevD.grad (logLikVec xv yv)    ps0
        gFused  = RevD.grad (logLikFused ss)     ps0
        gCD     = centralDiff (logLikScalar xs ys) ps0
    printf "n=%-5d | scalar=%.3e | vec=%.3e | fused=%.3e (各 vs 中心差分)\n"
      n (relErr gScalar gCD) (relErr gVec gCD) (relErr gFused gCD)
    -- AD 同士の一致も確認 (3 表現が同一勾配か)
    printf "         | vec-vs-scalar=%.3e | fused-vs-scalar=%.3e (AD 同士)\n"
      (relErr gVec gScalar) (relErr gFused gScalar)

  putStrLn "\n--- (Q2) per-grad 時間 (ms・median of 50) と scalar 比 ---"
  forM_ [50, 200, 1000, 5000] $ \n -> do
    let (xs, ys) = genData n
        xv = VS.fromList xs; yv = VS.fromList ys
        ss = mkSuffStat xs ys
        probe = sum . map abs
    (tS, _) <- timeitIO 50 probe (\_ -> pure (RevD.grad (logLikScalar xs ys) ps0))
    (tV, _) <- timeitIO 50 probe (\_ -> pure (RevD.grad (logLikVec xv yv)    ps0))
    (tF, _) <- timeitIO 50 probe (\_ -> pure (RevD.grad (logLikFused ss)     ps0))
    printf "n=%-5d | scalar=%8.4f | vec=%8.4f (×%.2f) | fused=%8.4f (×%.1f)\n"
      n tS tV (tS / tV) tF (tS / tF)

  putStrLn "\n(×N = scalar 比の速度向上。 fused は O(1) ゆえ n 増で差が拡大する想定)"
