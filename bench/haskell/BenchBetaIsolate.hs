module Main where
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWCD
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Vector.Storable as VS
import MCMC.Gibbs (sampleBetaBB)

sampleBetaGamma :: Double -> Double -> MWC.GenIO -> IO Double
sampleBetaGamma a b gen = do
  x <- MWCD.gamma a 1 gen
  y <- MWCD.gamma b 1 gen
  return (x / (x + y))

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
  let n = 10000 :: Int
  _ <- VS.replicateM n (sampleBetaGamma 14 10 gen)  -- warmup
  _ <- timeit "10000 sampleBetaGamma (2 gamma + div)" (VS.replicateM n (sampleBetaGamma 14 10 gen))
  _ <- timeit "10000 sampleBetaBB    (Cheng BB)    " (VS.replicateM n (sampleBetaBB    14 10 gen))
  _ <- timeit "10000 gamma 14                       " (VS.replicateM n (MWCD.gamma 14 1 gen))
  _ <- timeit "10000 uniform                        " (VS.replicateM n (MWC.uniform gen :: IO Double))
  return ()
