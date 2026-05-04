{-# LANGUAGE OverloadedStrings #-}
-- | Shared helpers used by the Haskell side of the benchmarks.
-- Writes a uniform per-row CSV layout consumed by the Python aggregator.
module BenchUtil
  ( BenchRow (..)
  , writeRows
  , timeit
  , readCsvXY
  , readCsvXYG
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString            as BS
import qualified Data.Vector                as V
import qualified Numeric.LinearAlgebra      as LA
import           Data.Csv                   (decode, HasHeader (..))
import           Data.Time.Clock            (getCurrentTime, diffUTCTime)
import           Text.Printf                (printf)
import           System.IO                  (withFile, IOMode (..), hPutStrLn,
                                             hSetBuffering, BufferMode (..))
import           Control.DeepSeq            (NFData, deepseq)

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

-- | Run @action@ @n@ times, return the median wall-time in milliseconds plus
-- the value from the last invocation. Forces the result via NFData so lazy
-- evaluation does not skew the measurement.
timeit :: NFData a => Int -> IO a -> IO (Double, a)
timeit n action = do
  ts <- mapM (\_ -> do
                t0 <- getCurrentTime
                x  <- action
                x `deepseq` return ()
                t1 <- getCurrentTime
                return (1000.0 * realToFrac (diffUTCTime t1 t0))) [1 .. n]
  x <- action
  let sorted = quickSort ts
      med    = sorted !! (length sorted `div` 2)
  return (med, x)
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
