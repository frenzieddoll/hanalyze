{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | B12 Multi-output ベンチ。MultiLM / MultiGP を sklearn の
-- @MultiOutputRegressor@ と比較。
--
--   * MultiLM n=2000 p=10 q=5  → sklearn LinearRegression (multi-Y)
--   * MultiGP n=200 p=3  q=3   → sklearn GaussianProcessRegressor 多出力ループ
--
-- 出力: bench/results/haskell/multi_output.csv
module Main where

import qualified Numeric.LinearAlgebra as LA

import           Model.MultiLM         (fitMultiLM, predictMultiLM)
import           Model.MultiGP         (fitMultiGPMV, MultiGPResultMV (..))
import           Model.GP              (Kernel (..))

import           BenchUtil

-- ---------------------------------------------------------------------------
-- Deterministic data generators (must match Python side).
-- ---------------------------------------------------------------------------

designX :: Int -> Int -> LA.Matrix Double
designX n p =
  LA.fromLists
    [ [ sin (fromIntegral i * 0.1 + fromIntegral j * 0.7)
        + 0.3 * cos (fromIntegral i * 0.05 + fromIntegral j)
      | j <- [0 .. p - 1] ]
    | i <- [0 .. n - 1] ]

multiY :: LA.Matrix Double -> Int -> LA.Matrix Double
multiY x q =
  let n = LA.rows x
      p = LA.cols x
      coefs = LA.fromLists
                [ [ sin (fromIntegral (j * (k + 1)))
                  | j <- [0 .. p - 1] ]
                | k <- [0 .. q - 1] ]
      y = x LA.<> LA.tr coefs
      bump = LA.fromLists
        [ [ 0.05 * sin (fromIntegral i * 0.3 + fromIntegral k)
          | k <- [0 .. q - 1] ]
        | i <- [0 .. n - 1] ]
  in y + bump

-- ---------------------------------------------------------------------------

benchMultiLM :: IO [BenchRow]
benchMultiLM = do
  let !n = 2000
      !p = 10
      !q = 5
      !x = designX n p
      !y = multiY x q
      run :: Int -> IO Double
      run _ = do
        let mf   = fitMultiLM x y
            yhat = predictMultiLM mf x
            r    = yhat - y
        return (LA.sumElements (LA.cmap (\d -> d * d) r))
      probe = id
  (ms, sse) <- timeitTastyIO probe run
  let rmse = sqrt (sse / fromIntegral (n * q))
  return [ BenchRow "haskell" "multi_output"
            "MultiLM_n2000_p10_q5" ms rmse 0
            ("MultiLM n=2000 p=10 q=5; RMSE=" ++ show rmse) ]

benchMultiGP :: IO [BenchRow]
benchMultiGP = do
  let !n = 200
      !p = 3
      !q = 3
      !x = designX n p
      !y = multiY x q
      -- yCols: list of length q, each an LA.Vector of length n.
      yCols = [ LA.flatten (y LA.?? (LA.All, LA.Pos (LA.idxs [k])))
              | k <- [0 .. q - 1] ]
      run :: Int -> IO Double
      run _ = do
        let r = fitMultiGPMV RBF x yCols x
            -- Sum of all per-output predicted means (forces full computation).
            s = sum [ LA.sumElements m | m <- mgpmvMean r ]
        return s
      probe = id
  (ms, _) <- timeitTastyIO probe run
  return [ BenchRow "haskell" "multi_output"
            "MultiGP_n200_p3_q3" ms 0 0
            "MultiGP RBF n=200 p=3 q=3 (independent GPs, MV API)" ]

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  rows <- mconcat <$> sequence
    [ benchMultiLM
    , benchMultiGP
    ]
  writeRows "bench/results/haskell/multi_output.csv" rows
  putStrLn $ "wrote " ++ show (length rows)
          ++ " rows → bench/results/haskell/multi_output.csv"
