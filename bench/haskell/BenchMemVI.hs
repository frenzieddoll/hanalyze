{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- | Memory audit Q2-B-1: Stat.VI ADVI.
--
-- Looking for the chained-thunk leak in @Stat/VI.hs:176, 184@:
--
-- > writeIORef muRef (zipWith (+) mu dxMu)
--
-- After T iterations, @muRef@ holds @T@ levels of chained @zipWith@ that
-- are only forced post-loop. Expectation: alloc grows roughly linearly
-- with T, peak residency too.
--
-- Run e.g.:
--   ./bench-mem-vi 500  +RTS -s -t -M256m
--   ./bench-mem-vi 5000 +RTS -s -t -M512m
--
-- We use a synthetic flat-prior model with K parameters so the unconstrained
-- vector size is large enough to make any per-iter leak visible.
module Main where

import           Control.Monad         (forM_)
import qualified Data.Map.Strict       as Map
import qualified Data.Text             as T
import           Data.Time.Clock       (getCurrentTime, diffUTCTime)
import           System.Environment    (getArgs)
import           System.IO             (hSetBuffering, BufferMode (..), stdout)
import           System.Random.MWC     (createSystemRandom)

import           Hanalyze.Model.HBM
import           Hanalyze.Stat.Distribution ()
import           Hanalyze.Stat.VI

-- | Synthetic model: K independent Normal latents with one Normal observation
-- each (data fixed at the prior mean). Larger K = larger variational vector
-- per iteration → leak is more visible per iter.
flatModel :: Int -> ModelP ()
flatModel k = do
  forM_ [1 .. k] $ \i -> do
    let nm = T.pack ("p" ++ show i)
    pi_ <- sample nm (Normal 0 1)
    observe (T.pack ("y" ++ show i)) (Normal pi_ 1) [0.0]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  let (iters, kParams) = case args of
        [it]      -> (read it, 20)
        [it, kk]  -> (read it, read kk)
        _         -> (500, 20)
  putStrLn $ "BenchMemVI  iters=" ++ show iters
                       ++ "  K="  ++ show kParams
  gen <- createSystemRandom
  let initP = Map.fromList [ (T.pack ("p" ++ show i), 0.0)
                           | i <- [1 .. kParams] ]
      cfg   = defaultVIConfig
                { viIterations = iters
                , viSamples    = 5
                , viNumDraws   = 100
                }
  t0 <- getCurrentTime
  res <- advi (flatModel kParams) cfg initP gen
  t1 <- getCurrentTime
  let elboLast = case viElboHistory res of [] -> 0/0; xs -> last xs
      muLen    = length (viMuU res)
  putStrLn $ "  done elbo_last=" ++ show elboLast
                  ++ "  |mu|=" ++ show muLen
                  ++ "  elapsed=" ++ show (diffUTCTime t1 t0)
