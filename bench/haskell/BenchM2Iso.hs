{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | Phase 85.5: M2 (random intercept) 単独 NUTS の A/B 計測ドライバ。
--
-- Phase 85.3 の回帰ガードで M2 の wall が Phase 84 基準比 ~1.5× 悪化して
-- 3 run 再現した (M3-M8 は非回帰・per-eval A/B は +10-18% のみ・iter 増で
-- 悪化率上昇 = GC/heap の疑い)。 本ドライバは BenchHBMScaling の M2 を
-- データ・init・config 込みで単独再現し、 `-rtsopts` 付きでビルドして
-- `+RTS -s` の alloc / GC / MUT 時間を旧 lib (worktree) と直接比較する。
--
--   cabal run bench-m2-iso -f benches -- 1600 +RTS -s
module Main where

import           Control.Monad                    (forM_)
import qualified Data.Map.Strict                  as Map
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import qualified System.Random.MWC                as MWC
import           System.Random.MWC.Distributions  (standard)
import           System.Environment               (getArgs)
import           Text.Printf                      (printf)

import           Hanalyze.Model.HBM               (ModelP, glmmRandomIntercept,
                                                   GlmmFamily (..))
import           Hanalyze.MCMC.NUTS               (NUTSConfig (..),
                                                   defaultNUTSConfig, nuts)
import           Hanalyze.MCMC.Core               (Chain, posteriorMean)

import           BenchUtil                        (timeitIO)

-- BenchHBMScaling と同一の M2 DGP (8 群 × 12 = 96 obs・決定的)
nGroups, perGroup :: Int
nGroups  = 8
perGroup = 12

normals :: Int -> Int -> IO [Double]
normals seed k = do
  g <- MWC.initialize (V.singleton (fromIntegral seed))
  mapM (const (standard g)) [1 .. k]

genM2Data :: IO ([[Double]], [Int], [Double])
genM2Data = do
  let (b0, b1, tauU, s) = (1.0, 0.8, 1.5, 1.0)
      n = nGroups * perGroup
  xz <- normals 21 n
  ez <- normals 22 n
  uz <- normals 23 nGroups
  let us   = map (* tauU) uz
      gids = [ i `div` perGroup | i <- [0 .. n - 1] ]
      xs   = map (* 2.0) xz
      ys   = [ b0 + b1 * x + (us !! g) + s * e
             | (x, g, e) <- zip3 xs gids ez ]
      xRows = [ [1.0, x] | x <- xs ]
  return (xRows, gids, ys)

mkConfig :: Int -> NUTSConfig
mkConfig iters = defaultNUTSConfig
  { nutsIterations    = iters
  , nutsBurnIn        = 500
  , nutsStepSize      = 0.1
  , nutsMaxDepth      = 10
  , nutsAdaptStepSize = True
  , nutsTargetAccept  = 0.8
  , nutsAdaptMass     = True
  }

main :: IO ()
main = do
  args <- getArgs
  let iters = case args of
        (a : _) -> read a
        _       -> 1600
  (xr, gids, ys) <- genM2Data
  let m2 :: ModelP ()
      m2 = glmmRandomIntercept GlmmGaussian xr gids ys
      names = ["beta_0", "beta_1", "tau_u", "sigma"]
              ++ [ T.pack ("u_" ++ show j) | j <- [0 .. nGroups - 1] ]
      initP = Map.fromList $
        [ ("beta_0", 1.0), ("beta_1", 0.8), ("tau_u", 1.5), ("sigma", 1.0) ]
        ++ [ (T.pack ("u_" ++ show j), 0.0) | j <- [0 .. nGroups - 1] ]
      probe ch = sum [ maybe 0 id (posteriorMean p ch) | p <- names ]
      run :: Int -> IO Chain
      run i = do
        g <- MWC.initialize (V.singleton (fromIntegral (42 + i)))
        nuts m2 (mkConfig iters) initP g
  (ms, ch) <- timeitIO 5 probe run
  printf "M2_iso iter=%d warmup=500 reps=5 median=%.1f ms  beta_1=%.4f\n"
    iters ms (maybe 0 id (posteriorMean "beta_1" ch))
