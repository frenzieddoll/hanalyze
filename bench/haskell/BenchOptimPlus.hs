{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | B9 Optim+: Constrained / Adam / CMAESFull のベンチ。
--
--   * Constrained: 2D 問題 minimise (x-1)^2 + (y-2)^2 s.t. x+y=1
--     → Augmented Lagrangian (Optim.Constrained)、scipy SLSQP / trust-constr
--   * Adam: 50D quadratic min ‖x‖^2 を 1000 step
--     → Optim.Adam.runAdamMinimize、torch / scipy 自前
--   * CMAESFull: Rosenbrock 5D (full-rank covariance)
--     → Optim.CMAESFull、cma library full-rank
--
-- 出力: bench/results/haskell/optim_plus.csv
module Main where

import qualified System.Random.MWC      as MWC

import qualified Optim.Common           as OC
import qualified Optim.Constrained      as Co
import           Optim.Adam             (defaultAdamConfig, AdamConfig (..),
                                         runAdamMinimize)
import           Optim.CMAESFull        (defaultCMAESFConfig, CMAESFConfig (..),
                                         runCMAESFullWith)

import           BenchUtil

-- ---------------------------------------------------------------------------
-- Constrained: Augmented Lagrangian on a quadratic with linear equality.
-- ---------------------------------------------------------------------------

-- minimise (x-1)^2 + (y-2)^2 subject to x + y = 1.
-- Closed-form optimum: x* = 0, y* = 1, f* = 2.
benchConstrained :: IO [BenchRow]
benchConstrained = do
  let f xs = case xs of
        [x, y] -> (x - 1)^(2::Int) + (y - 2)^(2::Int)
        _      -> error "expected 2D"
      cs = Co.ConstraintSet
            { Co.csEq   = [ \[x, y] -> x + y - 1 ]
            , Co.csIneq = []
            }
      cfg = Co.defaultConstrainedConfig
              { Co.ccOuterIter = 25
              }
      run :: Int -> IO ([Double], Double)
      run _ = do
        (r, _v) <- Co.runAugmentedLagrangian cfg f cs [0, 0]
        return (OC.orBest r, OC.orValue r)
      probe (xs, val) = case xs of
        [_, _] -> val
        _      -> 0
  (ms, (xs, val)) <- timeitTastyIO probe run
  let [x_, y_] = take 2 (xs ++ [0, 0])
      err = sqrt ((x_ - 0)^(2::Int) + (y_ - 1)^(2::Int))
  return [ BenchRow "haskell" "optim_plus"
            "Constrained_Quad2D_eq" ms err val
            ("x=" ++ show x_ ++ " y=" ++ show y_
             ++ " f=" ++ show val ++ " err_to_opt=" ++ show err) ]

-- ---------------------------------------------------------------------------
-- Adam: minimise ‖x‖² in 50D, 1000 iterations, lr=0.05.
-- ---------------------------------------------------------------------------

benchAdam :: IO [BenchRow]
benchAdam = do
  let n     = 50
      x0    = replicate n 1.0   -- f(x0) = 50
      grad  = map (* 2)         -- ∇‖x‖² = 2x
      cfg   = defaultAdamConfig
                { adamIterations   = 1000
                , adamLearningRate = 0.05
                }
      run :: Int -> IO ([Double], Double)
      run _ = do
        let (xFinal, _hist) = runAdamMinimize cfg grad x0
            f x = sum (map (\v -> v * v) x)
        return (xFinal, f xFinal)
      probe = snd
  (ms, (_xFinal, fVal)) <- timeitTastyIO probe run
  return [ BenchRow "haskell" "optim_plus"
            "Adam_quad50D_iter1000" ms fVal 0
            ("‖x‖² minimization 50D from x0=1; f_final=" ++ show fVal) ]

-- ---------------------------------------------------------------------------
-- CMAESFull: Rosenbrock 5D, 200 iterations.
-- ---------------------------------------------------------------------------

rosenbrock :: [Double] -> Double
rosenbrock xs =
  sum [ 100 * (xs !! (i + 1) - (xs !! i)^(2::Int))^(2::Int)
        + (1 - xs !! i)^(2::Int)
      | i <- [0 .. length xs - 2] ]

benchCMAESFull :: IO [BenchRow]
benchCMAESFull = do
  -- P3 fairness: give both sides the same convergence criterion
  -- (tolfun = 1e-10) and a generous iter cap (1000), so both run "to
  -- convergence" rather than getting cut off at an artificial maxiter.
  -- Previously hanalyze stopped at 200 iter with f = 0.031 while cma
  -- effectively converged in <200 iter to f ~ 5e-7; the unfair part
  -- was hanalyze's tolfun never had a chance to fire.
  let cfg = defaultCMAESFConfig
              { cmfStop   = (cmfStop defaultCMAESFConfig)
                              { OC.stMaxIter = 1000
                              , OC.stTolFun  = 1e-10
                              }
              , cmfSigma0 = 0.5
              }
      x0  = replicate 5 (-1.5)
      run :: Int -> IO Double
      run _ = do
        gen <- MWC.create
        r <- runCMAESFullWith cfg rosenbrock x0 gen
        return (OC.orValue r)
      probe = id
  (ms, fVal) <- timeitTastyIO probe run
  return [ BenchRow "haskell" "optim_plus"
            "CMAESFull_Rosenbrock5D_converge" ms fVal 0
            ("CMAESFull σ₀=0.5 tolfun=1e-10 maxIter=1000 from x0=-1.5; "
             ++ "f_final=" ++ show fVal) ]

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  rows <- mconcat <$> sequence
    [ benchConstrained
    , benchAdam
    , benchCMAESFull
    ]
  writeRows "bench/results/haskell/optim_plus.csv" rows
  putStrLn $ "wrote " ++ show (length rows)
          ++ " rows → bench/results/haskell/optim_plus.csv"
