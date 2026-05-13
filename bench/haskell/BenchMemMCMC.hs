{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- | Memory audit Q2-B: MCMC samplers (MH / HMC / NUTS).
--
-- Suspected: 'modifyIORef'' samplesRef (Map.Strict... :)' uses Data.Map.Strict
-- so values are WHNF; chain length T × params K should grow linearly but
-- not leak. This bench confirms.
--
--   ./bench-mem-mcmc <sampler> <iters> <K>
--   sampler ∈ {mh, hmc, nuts}
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
import           Hanalyze.MCMC.Core    (Chain (..), chainAccepted)
import           Hanalyze.MCMC.MH      (MCMCConfig (..), defaultMCMCConfig, metropolis)
import           Hanalyze.MCMC.HMC     (HMCConfig (..), defaultHMCConfig, hmc)
import           Hanalyze.MCMC.NUTS    (NUTSConfig (..), defaultNUTSConfig, nuts)

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
  let (sampler, iters, k) = case args of
        [s]         -> (s :: String, 1000 :: Int, 20 :: Int)
        [s, it]     -> (s, read it, 20)
        [s, it, kk] -> (s, read it, read kk)
        _           -> ("mh", 1000, 20)
  putStrLn $ "BenchMemMCMC  sampler=" ++ sampler
                       ++ "  iters=" ++ show iters
                       ++ "  K="     ++ show k
  gen <- createSystemRandom
  let initP = Map.fromList [ (T.pack ("p" ++ show i), 0.0) | i <- [1 .. k] ]
  t0 <- getCurrentTime
  ch <- case sampler of
          "mh"   -> metropolis (flatModel k)
                       ((defaultMCMCConfig (Map.keys initP))
                          { mcmcIterations = iters }) initP gen
          "hmc"  -> hmc  (flatModel k) (defaultHMCConfig  { hmcIterations  = iters }) initP gen
          "nuts" -> nuts (flatModel k) (defaultNUTSConfig { nutsIterations = iters
                                                          , nutsBurnIn     = iters `div` 4 }) initP gen
          _      -> error "sampler ∈ {mh, hmc, nuts}"
  t1 <- getCurrentTime
  putStrLn $ "  samples=" ++ show (length (chainSamples ch))
          ++ "  accepted=" ++ show (chainAccepted ch)
          ++ "  elapsed=" ++ show (diffUTCTime t1 t0)
