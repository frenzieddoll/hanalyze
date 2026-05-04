{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | Bayesian Optimization benchmarks (B5).
--
-- Branin (2D) and Hartmann6 (6D) for 5 seeds, budget = 30 evaluations.
-- Reports median wall time and median final f(x*).

module Main where

import qualified Optim.BayesOpt          as BO
import qualified System.Random.MWC       as MWC
import           Data.List               (sort)
import           Control.Monad           (forM)

import           BenchUtil

-- ---------------------------------------------------------------------------
-- Test functions
-- ---------------------------------------------------------------------------

-- | Branin global minimum is f* = 0.397887 at three points.
branin :: [Double] -> IO Double
branin [x1, x2] =
  let a = 1
      b = 5.1 / (4 * pi * pi)
      c = 5 / pi
      r = 6
      s = 10
      t = 1 / (8 * pi)
  in return $ a * (x2 - b * x1 * x1 + c * x1 - r) ** 2
            + s * (1 - t) * cos x1 + s
branin _ = return 1e30

braninBounds :: [(Double, Double)]
braninBounds = [(-5, 10), (0, 15)]

braninStar :: Double
braninStar = 0.397887

-- | Hartmann 6D, global min f* = -3.32237 at known x*.
hartmann6 :: [Double] -> IO Double
hartmann6 xs =
  let alpha = [1.0, 1.2, 3.0, 3.2]
      a = [ [10, 3, 17, 3.5, 1.7, 8]
          , [0.05, 10, 17, 0.1, 8, 14]
          , [3, 3.5, 1.7, 10, 17, 8]
          , [17, 8, 0.05, 10, 0.1, 14] ]
      p = [ [0.1312, 0.1696, 0.5569, 0.0124, 0.8283, 0.5886]
          , [0.2329, 0.4135, 0.8307, 0.3736, 0.1004, 0.9991]
          , [0.2348, 0.1451, 0.3522, 0.2883, 0.3047, 0.6650]
          , [0.4047, 0.8828, 0.8732, 0.5743, 0.1091, 0.0381] ]
      term i =
        let aRow = a !! i
            pRow = p !! i
            inner = sum [ aRow !! j * (xs !! j - pRow !! j) ** 2 | j <- [0..5] ]
        in alpha !! i * exp (- inner)
  in return $ negate $ sum [term i | i <- [0..3]]

hartmann6Bounds :: [(Double, Double)]
hartmann6Bounds = replicate 6 (0, 1)

hartmann6Star :: Double
hartmann6Star = -3.32237

-- ---------------------------------------------------------------------------
-- Driver
-- ---------------------------------------------------------------------------

nSeeds :: Int
nSeeds = 5

mainBranin :: IO BenchRow
mainBranin = do
  let cfg = BO.defaultBayesOptConfig
              { BO.boIterations = 30, BO.boInitPoints = 5 }
  rs <- mapM (\_ -> runND cfg branin braninBounds) [1 .. nSeeds]
  let (ts, ys) = unzip rs
      medT = median ts
      medY = median ys
  return $ BenchRow "haskell" "bo" "Branin/BO" medT medY
             braninStar
             ("median over " ++ show nSeeds ++ " seeds; star=" ++ show braninStar)

mainHartmann6 :: IO BenchRow
mainHartmann6 = do
  let cfg = BO.defaultBayesOptConfig
              { BO.boIterations = 30, BO.boInitPoints = 10 }
  rs <- mapM (\_ -> runND cfg hartmann6 hartmann6Bounds) [1 .. nSeeds]
  let (ts, ys) = unzip rs
      medT = median ts
      medY = median ys
  return $ BenchRow "haskell" "bo" "Hartmann6/BO" medT medY
             hartmann6Star
             ("median over " ++ show nSeeds ++ " seeds; star=" ++ show hartmann6Star)

{-# NOINLINE runND #-}
runND :: BO.BayesOptConfig -> ([Double] -> IO Double) -> [(Double, Double)]
      -> IO (Double, Double)
runND cfg f bs = do
  gen <- MWC.createSystemRandom
  (ms, (_hist, (_xstar, ystar))) <- timeitIO 1 (\(_,(_,y)) -> y)
                                      (\_ -> BO.bayesOptND cfg 20 f bs gen)
  return (ms, ystar)

main :: IO ()
main = do
  rs <- sequence [mainBranin, mainHartmann6]
  writeRows "bench/results/haskell/bo.csv" rs
  putStrLn $ "wrote " ++ show (length rs)
          ++ " rows → bench/results/haskell/bo.csv"

median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` 2)
