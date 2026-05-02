{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | gibbsMH の動作確認テスト。
-- HBM 版 gibbsMH と HBMP 版 gibbsMH を 3 つの共役モデルで比較する。
module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Text.Printf (printf)
import System.Random.MWC (initialize)
import qualified Data.Vector as V
import Data.Word (Word32)

import MCMC.Core (Chain (..), chainVals, posteriorMean, posteriorSD, acceptanceRate)
import MCMC.Gibbs  (GibbsConfig (..), defaultGibbsConfig, gibbsMH, gibbsFromModel)
import MCMC.Gibbs (gibbsMH, gibbsFromModel)
import qualified Model.HBM  as HBM
import qualified Model.HBM as HBMP
import Stat.Distribution (Distribution (..))
import Stat.MCMC (ess)

cfg :: GibbsConfig
cfg = defaultGibbsConfig { gibbsIterations = 2000, gibbsBurnIn = 500 }

-- ---------------------------------------------------------------------------
-- Model 1: Normal-Normal (μ 共役 + σ MH)
-- ---------------------------------------------------------------------------

normalObs :: [Double]
normalObs = [1.5, 2.1, 1.8, 2.5, 1.9, 2.3, 1.7, 2.0, 2.2, 1.6]

normalHBM :: HBM.Model ()
normalHBM = do
  mu    <- HBM.sample "mu"    (Normal 0 10)
  sigma <- HBM.sample "sigma" (Exponential 1)
  HBM.observe "y" (Normal mu sigma) normalObs

normalHBMP :: HBMP.ModelP ()
normalHBMP = do
  mu    <- HBMP.sample "mu"    (HBMP.Normal 0 10)
  sigma <- HBMP.sample "sigma" (HBMP.Exponential 1)
  HBMP.observe "y" (HBMP.Normal mu sigma) normalObs

-- ---------------------------------------------------------------------------
-- Model 2: Beta-Binomial (全共役)
-- ---------------------------------------------------------------------------

binomObs :: [Double]
binomObs = [7, 8, 6, 7, 9, 7, 8, 7, 6, 7]

binomHBM :: HBM.Model ()
binomHBM = do
  p <- HBM.sample "p" (Beta 2 2)
  HBM.observe "y" (Binomial 10 p) binomObs

binomHBMP :: HBMP.ModelP ()
binomHBMP = do
  p <- HBMP.sample "p" (HBMP.Beta 2 2)
  HBMP.observe "y" (HBMP.Binomial 10 p) binomObs

-- ---------------------------------------------------------------------------
-- Model 3: Gamma-Poisson (全共役)
-- ---------------------------------------------------------------------------

poisObs :: [Double]
poisObs = [3, 5, 4, 6, 4, 3, 5, 4, 7, 4]

poisHBM :: HBM.Model ()
poisHBM = do
  lam <- HBM.sample "lambda" (Gamma 2 1)
  HBM.observe "y" (Poisson lam) poisObs

poisHBMP :: HBMP.ModelP ()
poisHBMP = do
  lam <- HBMP.sample "lambda" (HBMP.Gamma 2 1)
  HBMP.observe "y" (HBMP.Poisson lam) poisObs

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Gibbs HBMP 動作確認 ==="

  putStrLn "\n--- 1. Normal-Normal (μ 共役 + σ MH) ---"
  let (gibbsHBM1,  mhHBM1)  = gibbsFromModel  normalHBM
      (gibbsHBMP1, mhHBMP1) = gibbsFromModel normalHBMP
  printf "  HBM:  共役検出 %d 個, MH 残り: %s\n"
         (length gibbsHBM1)  (show (map T.unpack mhHBM1))
  printf "  HBMP: 共役検出 %d 個, MH 残り: %s\n"
         (length gibbsHBMP1) (show (map T.unpack mhHBMP1))
  runBoth normalHBM normalHBMP
          (Map.fromList [("mu", 0.0), ("sigma", 1.0)])
          (Map.fromList [("sigma", 0.2)])
          ["mu", "sigma"]

  putStrLn "\n--- 2. Beta-Binomial (全共役) ---"
  let (gibbsHBM2,  mhHBM2)  = gibbsFromModel  binomHBM
      (gibbsHBMP2, mhHBMP2) = gibbsFromModel binomHBMP
  printf "  HBM:  共役検出 %d 個, MH 残り: %s\n"
         (length gibbsHBM2)  (show (map T.unpack mhHBM2))
  printf "  HBMP: 共役検出 %d 個, MH 残り: %s\n"
         (length gibbsHBMP2) (show (map T.unpack mhHBMP2))
  runBoth binomHBM binomHBMP
          (Map.singleton "p" 0.5) Map.empty ["p"]

  putStrLn "\n--- 3. Gamma-Poisson (全共役) ---"
  let (gibbsHBM3,  mhHBM3)  = gibbsFromModel  poisHBM
      (gibbsHBMP3, mhHBMP3) = gibbsFromModel poisHBMP
  printf "  HBM:  共役検出 %d 個, MH 残り: %s\n"
         (length gibbsHBM3)  (show (map T.unpack mhHBM3))
  printf "  HBMP: 共役検出 %d 個, MH 残り: %s\n"
         (length gibbsHBMP3) (show (map T.unpack mhHBMP3))
  runBoth poisHBM poisHBMP
          (Map.singleton "lambda" 4.0) Map.empty ["lambda"]

runBoth
  :: HBM.Model ()
  -> HBMP.ModelP ()
  -> Map.Map T.Text Double
  -> Map.Map T.Text Double
  -> [T.Text]
  -> IO ()
runBoth mHBM mHBMP initP mhSteps names = do
  gen1 <- initialize (V.fromList [42 :: Word32, 1])
  gen2 <- initialize (V.fromList [42 :: Word32, 1])
  ch1 <- gibbsMH  mHBM  cfg mhSteps initP gen1
  ch2 <- gibbsMH mHBMP cfg mhSteps initP gen2
  putStrLn "  HBM (gibbsMH):"
  printChain ch1 names
  putStrLn "  HBMP (gibbsMH):"
  printChain ch2 names

printChain :: Chain -> [T.Text] -> IO ()
printChain ch names = do
  printf "    受容率=%.1f%% (Gibbs ステップは常に採択)\n"
    (acceptanceRate ch * 100 :: Double)
  mapM_ (\n ->
    printf "    %-8s mean=%8.4f  sd=%7.4f  ESS=%6.0f\n"
      (T.unpack n)
      (maybe 0 id (posteriorMean n ch))
      (maybe 0 id (posteriorSD   n ch))
      (ess (chainVals n ch)))
    names
