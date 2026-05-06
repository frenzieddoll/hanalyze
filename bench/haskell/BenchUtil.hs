{-# LANGUAGE OverloadedStrings #-}
-- | Shared helpers used by the Haskell side of the benchmarks.
-- Writes a uniform per-row CSV layout consumed by the Python aggregator.
module BenchUtil
  ( BenchRow (..)
  , writeRows
  , timeit
  , timeitIO
  , timeitTasty
  , timeitTastyIO
  , readCsvXY
  , readCsvXYG
  ) where

import           Data.IORef                 (newIORef, readIORef, writeIORef)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString            as BS
import qualified Data.Vector                as V
import qualified Numeric.LinearAlgebra      as LA
import           Data.Csv                   (decode, HasHeader (..))
import           Data.Time.Clock            (getCurrentTime, diffUTCTime)
import           Text.Printf                (printf)

import qualified Test.Tasty.Bench           as TB
import           Test.Tasty                 (Timeout (NoTimeout))
import           System.IO                  (withFile, IOMode (..), hPutStrLn,
                                             hSetBuffering, BufferMode (..))
import           Control.Exception          (evaluate)

-- | One benchmark observation. The aggregator joins (system, suite, name)
-- across @bench/results/haskell/*.csv@ and @bench/results/python/*.csv@.
data BenchRow = BenchRow
  { brSystem  :: String   -- ^ "haskell" or "python".
  , brSuite   :: String   -- ^ "regression", "kernel", "optim", "mo", "bo", ...
  , brName    :: String   -- ^ Stable benchmark name (e.g. "LM_n1000_p5").
  , brTimeMs  :: Double   -- ^ Median wall time per call in milliseconds.
  , brAccMain :: Double   -- ^ Primary accuracy metric (R², HV, |x*-x_true| etc.).
  , brAccAux  :: Double   -- ^ Secondary metric (RMSE, IGD, runs to optimum, ...).
  , brExtra   :: String   -- ^ Free-form note (e.g. "kernel=Gaussian h=0.5").
  } deriving Show

-- | Write a list of rows to a CSV file (with header).
writeRows :: FilePath -> [BenchRow] -> IO ()
writeRows path rows = withFile path WriteMode $ \h -> do
  hSetBuffering h LineBuffering
  hPutStrLn h "system,suite,name,time_ms,acc_main,acc_aux,extra"
  mapM_ (\r -> hPutStrLn h
          (printf "%s,%s,%s,%.6g,%.6g,%.6g,%s"
            (brSystem r) (brSuite r) (brName r)
            (brTimeMs r) (brAccMain r) (brAccAux r)
            (escapeCsv (brExtra r)))) rows

escapeCsv :: String -> String
escapeCsv s
  | any (`elem` (",\"\n" :: String)) s =
      '"' : concatMap (\c -> if c == '"' then "\"\"" else [c]) s ++ "\""
  | otherwise = s

-- | Run a fresh recomputation @n@ times, return the median wall-time in
-- milliseconds plus the value from the last invocation. The caller passes
-- a per-iteration builder @runIt :: Int -> IO a@ (the index defeats GHC's
-- common-subexpression elimination so the work is actually re-run each
-- time) and a probe @force :: a -> Double@ that pulls a scalar out of the
-- result, forcing the underlying Matrix / Vector computation via
-- 'evaluate'.
timeitIO :: Int -> (a -> Double) -> (Int -> IO a) -> IO (Double, a)
timeitIO n force runIt = do
  -- IORef は per-iteration の runtime 依存を作る (GHC が CSE しないように)。
  ref <- newIORef (0 :: Int)
  ts <- mapM (\i -> do
                writeIORef ref i
                _  <- readIORef  ref
                t0 <- getCurrentTime
                x  <- runIt i
                _  <- evaluate (force x)
                t1 <- getCurrentTime
                return (1000.0 * realToFrac (diffUTCTime t1 t0))) [1 .. n]
  x <- runIt 0
  _ <- evaluate (force x)
  let sorted = quickSort ts
      med    = sorted !! (length sorted `div` 2)
  return (med, x)

-- | Convenience wrapper: the action does not actually depend on the
-- iteration index. Provided for backwards compatibility — please use
-- 'timeitIO' for new code.
timeit :: Int -> (a -> Double) -> IO a -> IO (Double, a)
timeit n force action = timeitIO n force (\_ -> action)

-- | tasty-bench based timer (Phase 13).
--
-- Adaptive iteration count converges to a stable mean. Returns
-- (mean wall-time in ms, last result). The relative standard
-- deviation cap is 5% (much tighter than the default 10%).
--
-- Use this for new code; 'timeit' / 'timeitIO' kept for backwards
-- compatibility while the migration is in progress.
timeitTastyIO :: (a -> Double) -> (Int -> IO a) -> IO (Double, a)
timeitTastyIO force runIt = do
  -- Build a Benchmarkable that depends on a counter so GHC cannot
  -- common-subexpression-eliminate across iterations.
  ref <- newIORef (0 :: Int)
  let bm = TB.nfIO $ do
             i <- readIORef ref
             writeIORef ref (i + 1)
             x <- runIt i
             _ <- evaluate (force x)
             pure ()
  -- 0.05 = 5% relative stdev target. NoTimeout = run as long as
  -- needed for convergence (typical < 1 s for ms-range benchmarks).
  secs <- TB.measureCpuTime NoTimeout 0.05 bm
  -- Probe value to return alongside the timing.
  x <- runIt 0
  _ <- evaluate (force x)
  return (1000.0 * secs, x)

-- | Like 'timeitTastyIO' but the action does not depend on the
-- iteration index.
timeitTasty :: (a -> Double) -> IO a -> IO (Double, a)
timeitTasty force action = timeitTastyIO force (\_ -> action)

quickSort :: Ord a => [a] -> [a]
quickSort []     = []
quickSort (p:xs) = quickSort [y | y <- xs, y < p]
                ++ [p]
                ++ quickSort [y | y <- xs, y >= p]
  where
    quickSort []     = []
    quickSort (p:xs) = quickSort [y | y <- xs, y < p]
                    ++ [p]
                    ++ quickSort [y | y <- xs, y >= p]

-- ---------------------------------------------------------------------------
-- CSV input (small, header-fronted, all-numeric)
-- ---------------------------------------------------------------------------

-- | Read a CSV with header @x0,x1,...,x{p-1},y@ into @(X, y)@.
readCsvXY :: FilePath -> IO (LA.Matrix Double, LA.Vector Double)
readCsvXY path = do
  bytes <- BL.fromStrict <$> BS.readFile path
  case decode HasHeader bytes :: Either String (V.Vector (V.Vector Double)) of
    Left err -> error ("readCsvXY: " ++ path ++ ": " ++ err)
    Right rs ->
      let n = V.length rs
          p = V.length (rs V.! 0) - 1
          xs = LA.fromLists
                 [ [ rs V.! i V.! j | j <- [0 .. p - 1] ]
                 | i <- [0 .. n - 1] ]
          ys = LA.fromList [ rs V.! i V.! p | i <- [0 .. n - 1] ]
      in return (xs, ys)

-- | Read a CSV with header @x0,...,x{p-1},group,y@ into @(X, group_idx, y)@.
readCsvXYG :: FilePath -> IO (LA.Matrix Double, V.Vector Int, LA.Vector Double)
readCsvXYG path = do
  bytes <- BL.fromStrict <$> BS.readFile path
  case decode HasHeader bytes :: Either String (V.Vector (V.Vector Double)) of
    Left err -> error ("readCsvXYG: " ++ path ++ ": " ++ err)
    Right rs ->
      let n = V.length rs
          p = V.length (rs V.! 0) - 2
          xs = LA.fromLists
                 [ [ rs V.! i V.! j | j <- [0 .. p - 1] ]
                 | i <- [0 .. n - 1] ]
          gs = V.fromList [ round (rs V.! i V.! p)        :: Int | i <- [0 .. n - 1] ]
          ys = LA.fromList [ rs V.! i V.! (p + 1)                  | i <- [0 .. n - 1] ]
      in return (xs, gs, ys)
