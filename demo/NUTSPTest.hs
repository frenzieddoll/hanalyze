{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | nutsP の動作確認テスト。
-- nutsP (HBMP+AD) と nuts (HBM+数値) を比較する。
module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Text.Printf (printf)
import System.Random.MWC (initialize)
import qualified Data.Vector as V
import Data.Word (Word32)

import MCMC.Core (Chain (..), chainVals, posteriorMean, posteriorSD, acceptanceRate)
import MCMC.NUTS  (NUTSConfig (..), defaultNUTSConfig, nuts)
import MCMC.NUTSP (nutsP)
import qualified Model.HBM  as HBM
import qualified Model.HBMP as HBMP
import Stat.Distribution (Distribution (..))
import Stat.MCMC (ess)

obsData :: [Double]
obsData = [-0.5, 0.3, 1.2, 2.0, 2.8, 3.5, 4.1, 1.7, 2.3, 0.9
          , 2.1, 1.4, 3.2, 2.7, 1.1, 2.5, 3.0, 1.8, 2.2, 2.6]

hbmModel :: HBM.Model ()
hbmModel = do
  mu    <- HBM.sample "mu"    (Normal 0 10)
  sigma <- HBM.sample "sigma" (Exponential 1)
  HBM.observe "y" (Normal mu sigma) obsData

hbmpModel :: HBMP.ModelP ()
hbmpModel = do
  mu    <- HBMP.sample "mu"    (HBMP.Normal 0 10)
  sigma <- HBMP.sample "sigma" (HBMP.Exponential 1)
  HBMP.observe "y" (HBMP.Normal mu sigma) obsData

cfg :: NUTSConfig
cfg = defaultNUTSConfig
  { nutsIterations = 1500
  , nutsBurnIn     = 500
  , nutsStepSize   = 0.1
  }

main :: IO ()
main = do
  putStrLn "=== NUTS for HBMP 動作確認 ==="
  let initC = Map.fromList [("mu", 0.0), ("sigma", 1.0)]

  putStrLn "\n[NUTS (HBM, 数値勾配)]"
  gen1 <- initialize (V.fromList [42 :: Word32, 1])
  ch1 <- nuts hbmModel cfg initC gen1
  printResults ch1

  putStrLn "\n[NUTS (HBMP, AD 勾配)]"
  gen2 <- initialize (V.fromList [42 :: Word32, 1])
  ch2 <- nutsP hbmpModel cfg initC gen2
  printResults ch2

  putStrLn "\n  両者の事後平均/SDが一致 → HBMP NUTS が正常動作"

printResults :: Chain -> IO ()
printResults ch = do
  printf "  受容率: %.1f%%\n" (acceptanceRate ch * 100 :: Double)
  printf "  サンプル数: %d\n" (length (chainSamples ch))
  mapM_ (\n ->
    printf "  %-8s mean=%8.4f  sd=%7.4f  ESS=%6.0f\n"
      (T.unpack n)
      (maybe 0 id (posteriorMean n ch))
      (maybe 0 id (posteriorSD   n ch))
      (ess (chainVals n ch)))
    ["mu", "sigma"]
