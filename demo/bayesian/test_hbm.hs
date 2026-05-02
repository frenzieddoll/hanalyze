{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
import Model.HBM
import Stat.Distribution
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO
import Text.Printf (printf)

normalModel :: [Double] -> Model Double
normalModel yData = do
  mu    <- sample "mu"    (Normal 0 10)
  sigma <- sample "sigma" (Exponential 1)
  observe "y" (Normal mu sigma) yData
  return mu

main :: IO ()
main = do
  let yData  = [1.2, 2.3, 0.8, 1.5, 2.1]
      m      = normalModel yData
      params = Map.fromList [("mu", 1.58), ("sigma", 0.57)]

  TIO.putStr (describeModel m)
  putStrLn $ "sampleNames: " ++ show (sampleNames m)
  putStrLn ""

  printf "logJoint      = %.4f\n" (logJoint m params)
  printf "logPrior      = %.4f\n" (logPrior m params)
  printf "logLikelihood = %.4f\n" (logLikelihood m params)
  putStrLn $ "  (logPrior + logLikelihood = "
    ++ show (logPrior m params + logLikelihood m params) ++ ")"
  putStrLn ""

  let badParams = Map.fromList [("mu", 1.58), ("sigma", -1.0)]
  printf "logJoint (sigma=-1, outside support) = %.4f\n" (logJoint m badParams)

  let missingParams = Map.fromList [("mu", 1.58)]
  printf "logJoint (sigma missing)             = %.4f\n" (logJoint m missingParams)
