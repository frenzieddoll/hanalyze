{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | Phase 55.3 小 bench: heteroscedastic モデルの per-call 勾配 A/B。
--
--   mHet: y_i ~ N(a, exp(g0 + g1·z_i))   (n=100, θ=3・σ が行依存の式)
--
-- 55.3 で σ 位置が「単一 latent」 → 任意 SExp に拡張され、 このモデルは
-- ベクトル式 IR に吸収されるようになった (旧 = σ 検出不能で全体 ad fallback)。
-- 比較 2 通り (同一 unconstrained 全勾配・相対誤差検証後に計測):
--
--   (a) HBM.gradADU      — 実経路 (55.3 後 = IR 吸収・NUTS が払う値)
--   (b) RevD.grad (walk) — 旧 fallback 相当 (モデル全体を ad で毎回 walk)
--
-- per-draw への波及は M 系 bench (55.5) で測る。 ここは勾配カーネル単体。
module Main where

import           Control.Monad                  (forM_)
import qualified Data.Map.Strict                as Map
import qualified Data.Text                      as T
import           Text.Printf                    (printf)

import qualified Numeric.AD.Mode.Reverse.Double as RevD

import           Hanalyze.Model.HBM             (Distribution (..), ModelP,
                                                 sample, observe, gradADU,
                                                 sampleNames, getTransforms,
                                                 logJoint, invTransformF,
                                                 logJacF)

import           BenchUtil                      (timeitIO)

nHet :: Int
nHet = 100

-- 決定的データ (DGP は等間隔 z + 線形 y・乱数不要の固定系列)。
zsHet, ysHet :: [Double]
zsHet = [ fromIntegral i / fromIntegral nHet * 2 - 1 | i <- [0 .. nHet - 1] ]
ysHet = [ 1.2 + 0.1 * z | z <- zsHet ]

mHet :: ModelP ()
mHet = do
  a  <- sample "a"  (Normal 0 10)
  g0 <- sample "g0" (Normal 0 2)
  g1 <- sample "g1" (Normal 0 2)
  forM_ (zip3 [0 :: Int ..] zsHet ysHet) $ \(i, z, y) ->
    observe (T.pack ("y_" ++ show i))
      (Normal a (exp (g0 + g1 * realToFrac z))) [y]

main :: IO ()
main = do
  let names = sampleNames mHet
      tmap  = getTransforms mHet
      trans = [ tmap Map.! nm | nm <- names ]
      uvs   = [0.9, -0.3, 0.4]
      -- (a) 実経路 (55.3 後 = IR 吸収)
      gIR = gradADU mHet names trans
      -- (b) 旧 fallback 相当: モデル全体を ad で walk (compileGradUV の
      --     synthVecIR Nothing 分岐 gradFull と同形)
      gAD uv = RevD.grad
                 (\uv' -> logJoint mHet
                            (Map.fromList
                               (zip names (zipWith invTransformF trans uv')))
                          + sum (zipWith logJacF trans uv'))
                 uv
      relErr = maximum [ abs (x - y) / (1 + abs y)
                       | (x, y) <- zip (gIR uvs) (gAD uvs) ]
  printf "relErr IR vs ad-full = %.2e\n" relErr
  -- 1 計測 = batch 回の勾配呼出 (µs 級なので)。 入力を毎回微小摂動して
  -- CSE/共有を防ぐ (1e-12 は数値に実質影響しない)。
  let batch = 1000 :: Int
      runBatch g i = pure $! sum
        [ sum (g (map (+ (1e-12 * fromIntegral (i * batch + j))) uvs))
        | j <- [1 .. batch] ]
  (msIR, _) <- timeitIO 7 id (runBatch gIR)
  (msAD, _) <- timeitIO 7 id (runBatch gAD)
  let pcIR = msIR / fromIntegral batch
      pcAD = msAD / fromIntegral batch
  printf "gradADU (IR 吸収・実経路): %.5f ms/call\n" pcIR
  printf "RevD walk (旧 fallback) : %.5f ms/call\n" pcAD
  printf "speedup x%.1f\n" (pcAD / pcIR)
