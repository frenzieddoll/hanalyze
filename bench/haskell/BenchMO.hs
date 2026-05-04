{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | Multi-objective optimization benchmarks (B4).
--
-- Runs NSGA-II from 'Optim.NSGA' on ZDT1/2/3 (m=2, d=30) and DTLZ1/2
-- (m=3, d=10). Reports median wall time and the hypervolume of the
-- final approximation set against a reference point.

module Main where

import qualified Optim.NSGA              as NSGA
import qualified System.Random.MWC       as MWC
import           Data.List               (sort)
import           Control.Monad           (forM)

import           BenchUtil

-- ---------------------------------------------------------------------------
-- Test problems (m = number of objectives, d = dimension)
-- ---------------------------------------------------------------------------

data MOProblem = MOProblem
  { mpName     :: String
  , mpDim      :: Int
  , mpObjs     :: Int
  , mpFunc     :: [Double] -> [Double]
  , mpBounds   :: [(Double, Double)]
  , mpRefPoint :: [Double]                -- ^ reference for hypervolume
  }

zdt1, zdt2, zdt3 :: MOProblem
zdt1 = MOProblem "ZDT1" 30 2 fn (replicate 30 (0,1)) [1.1, 1.1]
  where
    fn xs =
      let f1 = head xs
          g  = 1 + 9 * sum (tail xs) / fromIntegral (length xs - 1)
          f2 = g * (1 - sqrt (f1 / g))
      in [f1, f2]

zdt2 = MOProblem "ZDT2" 30 2 fn (replicate 30 (0,1)) [1.1, 1.1]
  where
    fn xs =
      let f1 = head xs
          g  = 1 + 9 * sum (tail xs) / fromIntegral (length xs - 1)
          f2 = g * (1 - (f1 / g) ** 2)
      in [f1, f2]

zdt3 = MOProblem "ZDT3" 30 2 fn (replicate 30 (0,1)) [1.1, 1.1]
  where
    fn xs =
      let f1 = head xs
          g  = 1 + 9 * sum (tail xs) / fromIntegral (length xs - 1)
          f2 = g * (1 - sqrt (f1 / g) - (f1 / g) * sin (10 * pi * f1))
      in [f1, f2]

dtlz2_3 :: MOProblem
dtlz2_3 = MOProblem "DTLZ2_3" 10 3 fn (replicate 10 (0, 1)) [1.5, 1.5, 1.5]
  where
    fn xs =
      let m  = 3
          k  = length xs - m + 1
          x_m = drop (m - 1) xs
          g  = sum [(xi - 0.5) ** 2 | xi <- x_m]
          fAt i = (1 + g)
                * product [ cos (xs!!j * pi / 2) | j <- [0 .. m - i - 2] ]
                * (if i == 0 then 1 else sin (xs!!(m - i - 1) * pi / 2))
      in [fAt i | i <- [0 .. m - 1]]

problems :: [MOProblem]
problems = [zdt1, zdt2, zdt3, dtlz2_3]

-- ---------------------------------------------------------------------------
-- Driver
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  rows <- fmap concat $ forM problems $ \p -> do
    -- 1 seed: run NSGA once; export the Pareto set so that the Python
    -- aggregator can score HV/IGD via pymoo (uniform metric for both
    -- sides).
    (ms, sols) <- runOne p
    let pts = map NSGA.solObjectives sols
    writePareto p pts
    return [ BenchRow "haskell" "mo"
              (mpName p ++ "/NSGA-II") ms 0 (fromIntegral (length pts))
              ("Pareto written to bench/results/haskell/mo_pareto_"
               ++ mpName p ++ ".csv") ]
  writeRows "bench/results/haskell/mo.csv" rows
  putStrLn $ "wrote " ++ show (length rows)
          ++ " rows → bench/results/haskell/mo.csv"

writePareto :: MOProblem -> [[Double]] -> IO ()
writePareto p pts = do
  let path = "bench/results/haskell/mo_pareto_" ++ mpName p ++ ".csv"
      hdr  = unwords ["f" ++ show i | i <- [0 .. mpObjs p - 1]]
  writeFile path (replaceSpaces hdr ++ "\n"
                 ++ unlines [ commas (map show row) | row <- pts ])
  where
    replaceSpaces = map (\c -> if c == ' ' then ',' else c)
    commas []     = ""
    commas [x]    = x
    commas (x:xs) = x ++ "," ++ commas xs

{-# NOINLINE runOne #-}
runOne :: MOProblem -> IO (Double, [NSGA.Solution])
runOne p = do
  gen <- MWC.createSystemRandom
  let cfg = NSGA.defaultNSGAConfig
              { NSGA.nsgaPopSize     = 100
              , NSGA.nsgaGenerations = 100   -- pymoo と同条件 (NF1+NF3+NF4 で達成)
              }
  (ms, sols) <- timeitIO 1
                  (\xs -> sum [head (NSGA.solObjectives s) | s <- xs])
                  (\_ -> NSGA.nsga2 cfg (mpFunc p) (mpBounds p) gen)
  return (ms, sols)

-- ---------------------------------------------------------------------------

median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` 2)
