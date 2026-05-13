{-# LANGUAGE OverloadedStrings #-}
-- | Memory audit Q2-B: Bayesian Optimization (1D + multi-D).
--
--   ./bench-mem-bo <iters>            # 1D Forrester
--   ./bench-mem-bo <iters> <dim>      # ND sphere
module Main where

import           Data.Time.Clock       (getCurrentTime, diffUTCTime)
import           System.Environment    (getArgs)
import           System.IO             (hSetBuffering, BufferMode (..), stdout)
import           System.Random.MWC     (createSystemRandom)

import           Hanalyze.Optim.BayesOpt
                  (BayesOptConfig (..), defaultBayesOptConfig, bayesOpt,
                   bayesOptND)

-- 1D Forrester (canonical BO benchmark).
forrester :: Double -> IO Double
forrester x =
  let v = (6 * x - 2) ** 2 * sin (12 * x - 4)
  in return v

sphere :: [Double] -> IO Double
sphere xs = return $ sum [ (x - 0.3) * (x - 0.3) | x <- xs ]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  gen <- createSystemRandom
  case args of
    [it]     -> do
      let iters = read it :: Int
          cfg = defaultBayesOptConfig { boIterations = iters }
      putStrLn $ "BenchMemBO  iters=" ++ show iters ++ "  (1D Forrester)"
      t0 <- getCurrentTime
      (hist, (xb, yb)) <- bayesOpt cfg forrester (0.0, 1.0) gen
      t1 <- getCurrentTime
      putStrLn $ "  best=(" ++ show xb ++ ", " ++ show yb ++ ")"
              ++ "  histLen=" ++ show (length hist)
              ++ "  elapsed=" ++ show (diffUTCTime t1 t0)
    [it, d] -> do
      let iters = read it :: Int
          dim   = read d  :: Int
          cfg = defaultBayesOptConfig { boIterations = iters }
          bs = replicate dim (-1.0 :: Double, 1.0 :: Double)
      putStrLn $ "BenchMemBO  iters=" ++ show iters
                          ++ "  dim=" ++ show dim ++ "  (sphere)"
      t0 <- getCurrentTime
      (hist, (xb, yb)) <- bayesOptND cfg 5 sphere bs gen
      t1 <- getCurrentTime
      putStrLn $ "  best=" ++ show (xb, yb)
              ++ "  histLen=" ++ show (length hist)
              ++ "  elapsed=" ++ show (diffUTCTime t1 t0)
    _ -> putStrLn "usage: bench-mem-bo <iters> [dim]"
