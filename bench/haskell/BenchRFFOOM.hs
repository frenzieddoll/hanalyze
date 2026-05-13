{-# LANGUAGE BangPatterns #-}
-- | OOM regression bench for Phase 11b
-- (RFF.medianPairwiseDist + rbfKernelMat).
--
-- Pre-fix: n>=768 OOM-killed WSL2 (~7 GB+) inside maximizeMarginalLikRBFMV
-- because both internals built O(n²) Haskell-list intermediates with
-- @rows !! i@ index walks. Post-fix expectation:
--
--   * n=200  : sub-second, ~10 MB alloc
--   * n=400  : ~1 s,        ~50 MB alloc
--   * n=768  : few seconds, ~200 MB alloc (grid evals dominate; was OOM)
--
-- @maximizeMarginalLikRBFMV@ exercises both fixed paths heavily:
--
--   * 'medianPairwiseDist' once for the @ℓ@ centre.
--   * 'rbfKernelMat' inside @logMarginalLikRBFMV@ for every grid point.
module Main where

import qualified Numeric.LinearAlgebra as LA
import qualified Hanalyze.Model.RFF    as RFF
import           Data.Time.Clock        (getCurrentTime, diffUTCTime)
import           System.IO              (hSetBuffering, BufferMode (..), stdout)
import           System.Environment     (getArgs)

mkX :: Int -> Int -> LA.Matrix Double
mkX n p =
  LA.reshape p
    (LA.fromList [ sin (fromIntegral (i * 31 + j * 7)) / 3
                 | i <- [0 .. n - 1], j <- [0 .. p - 1] ])

-- Tiny grid (3,2,2) so we evaluate logMarginalLikRBFMV / rbfKernelMat
-- only 24 times — enough to surface OOM behaviour but not enough to drown
-- the timing.
benchOne :: Int -> IO ()
benchOne n = do
  let x = mkX n 8
      y = LA.fromList [ sin (fromIntegral i / 5) | i <- [0 .. n - 1] ]
  t0 <- getCurrentTime
  let !r = RFF.maximizeMarginalLikRBFMV x y (Just (3, 2, 2))
  t1 <- getCurrentTime
  putStrLn $ "  n=" ++ show n
          ++ "  ml=" ++ show (RFF.mlLogMlik r)
          ++ "  ell=" ++ show (RFF.mlEll r)
          ++ "  elapsed=" ++ show (diffUTCTime t1 t0)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  let ns = case args of
             [] -> [50, 100, 200]
             _  -> map read args
  putStrLn "=== maximizeMarginalLikRBFMV (Stage1+2 with tiny grid 3*2*2) ==="
  mapM_ benchOne ns
