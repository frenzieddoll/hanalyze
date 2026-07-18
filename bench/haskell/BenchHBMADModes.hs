{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | HBM 勾配の AD モード比較 (Phase 53 追加調査)。
--
-- forward が低次元で速く高次元で O(p) 悪化、 generic reverse が逆 (tape
-- オーバヘッドで低次元が遅い) と判明したため、 ad の 4 モードを直接突合し
-- 「低次元も高次元も両立する単一モードが無いか」 を計測する:
--
--   * Numeric.AD.Mode.Forward        (前進・O(p))
--   * Numeric.AD.Mode.Reverse        (逆・generic・tape boxing 有)
--   * Numeric.AD.Mode.Reverse.Double (逆・Double 特化・boxing 回避)
--   * Numeric.AD.Mode.Kahn           (逆・reflection-free)
--
-- 各モードで `logJointUnconstrained` の勾配 (= gradADU と同一計算) を計時。
module Main where

import           Control.Monad                    (forM_)
import qualified Data.Map.Strict                  as Map
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import qualified System.Random.MWC                as MWC
import           System.Random.MWC.Distributions  (standard)
import           Text.Printf                      (printf)

import qualified Numeric.AD.Mode.Forward          as Fwd
import qualified Numeric.AD.Mode.Reverse          as Rev
import qualified Numeric.AD.Mode.Reverse.Double   as RevD
import qualified Numeric.AD.Mode.Kahn             as Kahn

import           Hanalyze.Model.HBM
  ( Distribution (..), ModelP, sample, observe
  , glmmRandomIntercept, GlmmFamily (..)
  , sampleNames, getTransforms, logJointUnconstrained )
import           Hanalyze.Stat.Distribution       (Transform)

import           BenchUtil

-- ---------------------------------------------------------------------------
-- モデル
-- ---------------------------------------------------------------------------

m1Model :: [Double] -> [Double] -> ModelP ()
m1Model xs ys = do
  a <- sample "a"     (Normal 0 10)
  b <- sample "b"     (Normal 0 10)
  s <- sample "sigma" (Exponential 1)
  forM_ (zip3 [0 :: Int ..] xs ys) $ \(i, x, y) ->
    observe (T.pack ("y_" ++ show i)) (Normal (a + b * realToFrac x) s) [y]

m2Model :: [[Double]] -> [Int] -> [Double] -> ModelP ()
m2Model xRows gids ys = glmmRandomIntercept GlmmGaussian xRows gids ys

normals :: Int -> Int -> IO [Double]
normals seed k = do
  g <- MWC.initialize (V.singleton (fromIntegral seed))
  mapM (const (standard g)) [1 .. k]

genM1Data :: Int -> IO ([Double], [Double])
genM1Data n = do
  xz <- normals 11 n
  ez <- normals 12 n
  let xs = map (* 2.0) xz
      ys = zipWith (\x e -> 2.0 + 1.5 * x + e) xs ez
  return (xs, ys)

genM2Data :: Int -> Int -> IO ([[Double]], [Int], [Double])
genM2Data nG perG = do
  let n = nG * perG
  xz <- normals 21 n
  ez <- normals 22 n
  uz <- normals 23 nG
  let us'  = map (* 1.5) uz
      gids = [ i `div` perG | i <- [0 .. n - 1] ]
      xs   = map (* 2.0) xz
      ys   = [ 1.0 + 0.8 * x + (us' !! g) + e | (x, g, e) <- zip3 xs gids ez ]
      xRows = [ [1.0, x] | x <- xs ]
  return (xRows, gids, ys)

-- ---------------------------------------------------------------------------
-- 各モードの勾配 (logJointUnconstrained の grad = gradADU と同一計算)
-- ---------------------------------------------------------------------------

-- f :: 多相 numeric で us → Map に詰め直し logJointUnconstrained を評価。
mkF :: (Floating a, Ord a)
    => ModelP () -> [T.Text] -> [Transform] -> [a] -> a
mkF m names trans us =
  logJointUnconstrained m names trans (Map.fromList (zip names us))

gradFwd, gradRev, gradRevD, gradKahn
  :: ModelP () -> [T.Text] -> [Transform] -> [Double] -> [Double]
gradFwd  m names trans = Fwd.grad  (mkF m names trans)
gradRev  m names trans = Rev.grad  (mkF m names trans)
gradRevD m names trans = RevD.grad (mkF m names trans)
gradKahn m names trans = Kahn.grad (mkF m names trans)

-- ---------------------------------------------------------------------------
-- 計時: 1 モデルについて 4 モードの per-grad ナノ秒を出す
-- ---------------------------------------------------------------------------

profileModel :: String -> ModelP () -> IO ()
profileModel tag m = do
  let names = sampleNames m
      p     = length names
      tmap  = getTransforms m
      trans = [ Map.findWithDefault err n tmap | n <- names ]
      err   = error "transform missing"
      us0   = take p (cycle [0.1, -0.2, 0.15, 0.05, -0.1, 0.2, 0.3, 0.0])
  (fMs,  _) <- timeitIO 30 (sum . map abs) (\_ -> pure (gradFwd  m names trans us0))
  (rMs,  _) <- timeitIO 30 (sum . map abs) (\_ -> pure (gradRev  m names trans us0))
  (rdMs, _) <- timeitIO 30 (sum . map abs) (\_ -> pure (gradRevD m names trans us0))
  (kMs,  _) <- timeitIO 30 (sum . map abs) (\_ -> pure (gradKahn m names trans us0))
  printf "%-16s p=%-3d | fwd=%8.4f | rev=%8.4f | revDouble=%8.4f | kahn=%8.4f ms\n"
    tag p fMs rMs rdMs kMs

main :: IO ()
main = do
  putStrLn "=== Phase 53: AD モード別 1 勾配時間 (ms) ==="
  putStrLn "forward=O(p) / reverse=generic / revDouble=Double特化 / kahn=reflection-free\n"

  (x1, y1) <- genM1Data 100
  profileModel "M1_pooled(100)" (m1Model x1 y1)

  putStrLn "\n--- M2 群数↑ で p↑ (obs/群=12) ---"
  forM_ [2, 4, 8, 16, 32] $ \nG -> do
    (xr, g, y) <- genM2Data nG 12
    profileModel (printf "M2_g%d" nG) (m2Model xr g y)
