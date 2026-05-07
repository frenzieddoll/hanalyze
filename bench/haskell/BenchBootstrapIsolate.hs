module Main where
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWCD
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as MVS
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Word (Word64)
import qualified Data.Bits as Bits

timeit :: String -> IO a -> IO a
timeit name act = do
  t0 <- getCurrentTime
  r  <- act
  t1 <- getCurrentTime
  putStrLn $ name ++ ": " ++ show (1000.0 * realToFrac (diffUTCTime t1 t0) :: Double) ++ " ms"
  return r

main :: IO ()
main = do
  gen <- MWC.create
  let n = 1000 :: Int
      total = n * n      -- 1M ops, B=1000 × n=1000
      xs = LA.fromList [fromIntegral i | i <- [0..n-1]] :: LA.Vector Double
  -- Warmup
  buf0 <- MVS.unsafeNew total
  let warm i | i >= total = pure () | otherwise = do
                j <- MWC.uniformR (0, n - 1) gen
                MVS.unsafeWrite buf0 i (xs `LA.atIndex` j)
                warm (i+1)
  warm 0

  -- 1) uniformR Int × 1M
  buf1 <- MVS.unsafeNew total
  _ <- timeit "uniformR (0,n-1) × 1M, write Int as Double" $ do
    let go i | i >= total = pure () | otherwise = do
                j <- MWC.uniformR (0, n - 1) gen
                MVS.unsafeWrite buf1 i (fromIntegral j :: Double)
                go (i+1)
    go 0

  -- 2) gather only (deterministic indices)
  buf2 <- MVS.unsafeNew total
  _ <- timeit "gather only (det. idx) × 1M" $ do
    let go i | i >= total = pure () | otherwise = do
                let j = i `mod` n
                MVS.unsafeWrite buf2 i (xs `LA.atIndex` j)
                go (i+1)
    go 0

  -- 3) full bootstrap fill (uniformR + gather)
  buf3 <- MVS.unsafeNew total
  _ <- timeit "uniformR + gather × 1M (full bootstrap fill)" $ do
    let go i | i >= total = pure () | otherwise = do
                j <- MWC.uniformR (0, n - 1) gen
                MVS.unsafeWrite buf3 i (xs `LA.atIndex` j)
                go (i+1)
    go 0

  -- 4) raw uniform Word64 × 1M (cheaper than uniformR)
  buf4 <- MVS.unsafeNew total :: IO (MVS.IOVector Double)
  _ <- timeit "raw uniform Word64 × 1M (no range, no gather)" $ do
    let go i | i >= total = pure () | otherwise = do
                _ <- MWC.uniform gen :: IO Word64
                MVS.unsafeWrite buf4 i 0
                go (i+1)
    go 0

  -- 5) uniform Word64 + bitmask gather (assuming n is power-of-2-ish)
  buf5 <- MVS.unsafeNew total
  let mask = fromIntegral (n - 1) :: Word64   -- only valid if n is 2^k; here 1000 isn't, so this is a Lower-bound timing
  _ <- timeit "uniform Word64 + bitmask + gather × 1M (LB)" $ do
    let go i | i >= total = pure () | otherwise = do
                w <- MWC.uniform gen :: IO Word64
                let j = fromIntegral (w Bits..&. mask) `mod` n  -- still % to be safe
                MVS.unsafeWrite buf5 i (xs `LA.atIndex` j)
                go (i+1)
    go 0

  -- 6) sumElements × B=1000 (per-row mean dispatch overhead)
  let mat0 = LA.reshape n (LA.fromList [fromIntegral (i `mod` 7) | i <- [0..total-1]])
  _ <- timeit "B=1000 × LA.sumElements (per-row stat dispatch)" $ do
    let !sums = sum [LA.sumElements (mat0 LA.! r) | r <- [0..n-1]]
    print sums

  -- 7) BLAS row-sum via GEMV (mat #> ones)
  let ones = LA.konst 1 n :: LA.Vector Double
  _ <- timeit "B=1000 × n=1000 GEMV row sums (mat #> ones)" $ do
    let !s = LA.sumElements (mat0 LA.#> ones)
    print s
  return ()
