{-# LANGUAGE OverloadedStrings #-}
-- | Memory audit Q2-B: NSGA-II long generation count.
--
-- Suspected leak: 'generationLoop' recurses with a fresh 'newPop' that is
-- a thunk depending on the previous 'pop' (via 'pop ++ children' →
-- 'nonDominatedSort' → 'selectTopN'). Until forced, the entire ancestor
-- chain may be retained.
--
--   ./bench-mem-nsga2 <generations> <popSize> <dim>  +RTS -s -M256m
module Main where

import           Data.Time.Clock      (getCurrentTime, diffUTCTime)
import           System.Environment   (getArgs)
import           System.IO            (hSetBuffering, BufferMode (..), stdout)
import           System.Random.MWC    (createSystemRandom)

import           Hanalyze.Optim.NSGA  (NSGAConfig (..), defaultNSGAConfig,
                                       nsga2, Solution (..))

-- ZDT1 (m=2, decision dim d): convex Pareto front in [0,1]^d.
zdt1 :: [Double] -> [Double]
zdt1 xs =
  let f1 = head xs
      g  = 1 + 9 * (sum (tail xs) / fromIntegral (length xs - 1))
      f2 = g * (1 - sqrt (f1 / g))
  in [f1, f2]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  let (gens, pop, d) = case args of
        [a]       -> (read a :: Int, 100 :: Int, 10 :: Int)
        [a, b]    -> (read a, read b, 10)
        [a, b, c] -> (read a, read b, read c)
        _         -> (200, 100, 10)
  putStrLn $ "BenchMemNSGA  gens=" ++ show gens
                       ++ "  pop=" ++ show pop
                       ++ "  dim=" ++ show d
  gen <- createSystemRandom
  let cfg = defaultNSGAConfig
              { nsgaPopSize     = pop
              , nsgaGenerations = gens
              }
      bounds = replicate d (0.0, 1.0)
  t0 <- getCurrentTime
  front <- nsga2 cfg zdt1 bounds gen
  t1 <- getCurrentTime
  let nFront = length front
      avgF1  = sum [ head (solObjectives s) | s <- front ] / fromIntegral nFront
  putStrLn $ "  front=" ++ show nFront
          ++ "  avgF1=" ++ show avgF1
          ++ "  elapsed=" ++ show (diffUTCTime t1 t0)
