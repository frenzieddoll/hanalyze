-- | Generate the shared benchmark CSV inputs that both Haskell and Python
-- benchmarks read.
--
-- Fixed seed (mwc-random initialised from a deterministic word vector) so
-- every machine produces byte-identical CSVs. Runs all generators
-- sequentially and writes to @bench/data/@.

module Main where

import qualified Data.Vector              as V
import qualified Data.Vector.Storable     as VS
import qualified Numeric.LinearAlgebra    as LA
import           System.Random.MWC        (initialize, GenIO, uniformR)
import           System.Random.MWC.Distributions (standard)
import           Control.Monad            (replicateM, forM_)
import           System.Directory         (createDirectoryIfMissing)
import           Text.Printf              (printf, hPrintf)
import           System.IO                (withFile, IOMode (..), Handle, hPutStrLn)

main :: IO ()
main = do
  createDirectoryIfMissing True "bench/data"

  -- ---- Regression scenarios (B1) -----------------------------------------
  -- LM/Ridge: y = X β + ε, β fixed, ε ~ N(0, 0.5²)
  forM_ [(1000, 5), (10000, 50), (100000, 100)] $ \(n, p) ->
    genLM ("bench/data/lm_n" ++ show n ++ "_p" ++ show p ++ ".csv") n p

  -- GLM Logistic
  forM_ [(2000, 10), (10000, 20)] $ \(n, p) ->
    genLogistic ("bench/data/logistic_n" ++ show n ++ "_p" ++ show p ++ ".csv") n p

  -- GLM Poisson
  forM_ [(2000, 10), (10000, 20)] $ \(n, p) ->
    genPoisson ("bench/data/poisson_n" ++ show n ++ "_p" ++ show p ++ ".csv") n p

  -- GLMM (random intercept by group)
  forM_ [(2000, 5, 20), (10000, 10, 50)] $ \(n, p, g) ->
    genGLMM ("bench/data/glmm_n" ++ show n ++ "_p" ++ show p
             ++ "_g" ++ show g ++ ".csv") n p g

  -- ---- Kernel / GP scenarios (B2) ---------------------------------------
  -- y = sin(x1) + 0.5 cos(x2) + 0.3 x3 + ε  (smooth, low-noise)
  forM_ [(500, 1), (500, 5), (1000, 5), (2000, 5), (4000, 5)] $ \(n, p) ->
    genKernel ("bench/data/kernel_n" ++ show n ++ "_p" ++ show p ++ ".csv") n p

  putStrLn "All bench/data CSVs generated."

-- ---------------------------------------------------------------------------
-- LM / Ridge
-- ---------------------------------------------------------------------------

-- | y = Xβ + ε, β = sin(j+1) / (j+1), ε ~ N(0, 0.5²)
genLM :: FilePath -> Int -> Int -> IO ()
genLM path n p = do
  gen <- mkGen "lm" n p
  rows <- replicateM n (replicateM p (standard gen))
  noise <- replicateM n (standard gen)
  let beta  = [ sin (fromIntegral j + 1) / (fromIntegral j + 1)
              | j <- [0 .. p - 1 :: Int] ]
      ys    = [ sum (zipWith (*) row beta) + 0.5 * eps
              | (row, eps) <- zip rows noise ]
  writeXY path p rows ys

-- ---------------------------------------------------------------------------
-- Logistic GLM
-- ---------------------------------------------------------------------------

genLogistic :: FilePath -> Int -> Int -> IO ()
genLogistic path n p = do
  gen <- mkGen "logistic" n p
  rows <- replicateM n (replicateM p (standard gen))
  let beta = [ 0.5 * sin (fromIntegral j + 1) | j <- [0 .. p - 1 :: Int] ]
      eta  = [ sum (zipWith (*) r beta) | r <- rows ]
      mu   = map (\e -> 1 / (1 + exp (- e))) eta
  ys <- mapM (\m -> do
                u <- uniformR (0, 1) gen :: IO Double
                return (if u < m then 1.0 else 0.0)) mu
  writeXY path p rows ys

-- ---------------------------------------------------------------------------
-- Poisson GLM
-- ---------------------------------------------------------------------------

genPoisson :: FilePath -> Int -> Int -> IO ()
genPoisson path n p = do
  gen <- mkGen "poisson" n p
  rows <- replicateM n (replicateM p (uniformR (-1.0, 1.0) gen :: IO Double))
  let beta = [ 0.3 * sin (fromIntegral j + 1) | j <- [0 .. p - 1 :: Int] ]
      eta  = [ 0.5 + sum (zipWith (*) r beta) | r <- rows ]
      mu   = map exp eta
  ys <- mapM (samplePoisson gen) mu
  writeXY path p rows (map fromIntegral (ys :: [Int]))

samplePoisson :: GenIO -> Double -> IO Int
samplePoisson g lam
  | lam < 30  = sampleSmallPoisson g lam
  | otherwise = do
      -- 正規近似で十分 (ベンチデータ生成なので exact は不要)
      z <- standard g
      return (max 0 (round (lam + sqrt lam * z)))

sampleSmallPoisson :: GenIO -> Double -> IO Int
sampleSmallPoisson g lam = go 0 1.0
  where
    el = exp (- lam)
    go k pAcc = do
      u <- uniformR (0, 1) g :: IO Double
      let pNew = pAcc * u
      if pNew <= el then return k
                    else go (k + 1) pNew

-- ---------------------------------------------------------------------------
-- GLMM (Gaussian, random intercept)
-- ---------------------------------------------------------------------------

-- | n 観測 / p fixed effects / g groups。各群に N(0, σ_u² = 1) の切片。
genGLMM :: FilePath -> Int -> Int -> Int -> IO ()
genGLMM path n p g = do
  gen <- mkGen "glmm" n (p + g)
  rows  <- replicateM n (replicateM p (standard gen))
  noise <- replicateM n (standard gen)
  uVec  <- replicateM g (standard gen)
  let beta   = [ 0.5 * sin (fromIntegral j + 1) | j <- [0 .. p - 1 :: Int] ]
      groups = [ i `mod` g | i <- [0 .. n - 1 :: Int] ]
      ys     = [ sum (zipWith (*) r beta)
                 + (uVec !! (groups !! i))
                 + 0.3 * eps
               | (i, (r, eps)) <- zip [0 ..] (zip rows noise) ]
  writeXYG path p rows groups ys

-- ---------------------------------------------------------------------------
-- Kernel / GP regression target
-- ---------------------------------------------------------------------------

-- | f(x) = sin(x1) + 0.5 cos(x2) + 0.3 x3 + ... + ε ~ N(0, 0.05²)
genKernel :: FilePath -> Int -> Int -> IO ()
genKernel path n p = do
  gen <- mkGen "kernel" n p
  rows <- replicateM n (replicateM p (uniformR (-3, 3) gen :: IO Double))
  noise <- replicateM n (standard gen)
  let f r = case r of
              []        -> 0
              [a]       -> sin a
              [a, b]    -> sin a + 0.5 * cos b
              (a:b:c:_) -> sin a + 0.5 * cos b + 0.3 * c
      ys  = zipWith (\r e -> f r + 0.05 * e) rows noise
  writeXY path p rows ys

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Deterministic per-scenario seed: hash the tag + sizes into a Word32 vec.
mkGen :: String -> Int -> Int -> IO GenIO
mkGen tag n p =
  let seedInts = [ fromIntegral (length tag * 7919 + n * 31 + p)
                 , fromIntegral n
                 , fromIntegral p
                 , 0xDEADBEEF
                 ]
  in initialize (V.fromList seedInts)

writeXY :: FilePath -> Int -> [[Double]] -> [Double] -> IO ()
writeXY path p rows ys = withFile path WriteMode $ \h -> do
  let header = "x0" ++ concat [ "," ++ "x" ++ show j | j <- [1 .. p - 1] ]
                    ++ ",y"
  hPutStrLn h header
  mapM_ (\(r, y) -> do
            let cells = map dShow r ++ [dShow y]
            hPutStrLn h (intercalate1 "," cells)) (zip rows ys)
  printf "  wrote %s (%d × %d)\n" path (length rows) (p + 1)

writeXYG
  :: FilePath -> Int -> [[Double]] -> [Int] -> [Double] -> IO ()
writeXYG path p rows groups ys = withFile path WriteMode $ \h -> do
  let header = "x0" ++ concat [ "," ++ "x" ++ show j | j <- [1 .. p - 1] ]
                    ++ ",group,y"
  hPutStrLn h header
  mapM_ (\((r, g), y) -> do
            let cells = map dShow r ++ [show g, dShow y]
            hPutStrLn h (intercalate1 "," cells))
        (zip (zip rows groups) ys)
  printf "  wrote %s (%d × %d, %d groups)\n"
         path (length rows) (p + 2) (length (uniqueInts groups))

dShow :: Double -> String
dShow = printf "%.10g"

intercalate1 :: String -> [String] -> String
intercalate1 _   []     = ""
intercalate1 _   [x]    = x
intercalate1 sep (x:xs) = x ++ sep ++ intercalate1 sep xs

uniqueInts :: [Int] -> [Int]
uniqueInts = foldr (\x acc -> if x `elem` acc then acc else x : acc) []
