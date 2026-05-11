{-# LANGUAGE OverloadedStrings #-}
-- | 単目的最適化ベンチマーク。
--
-- 5 アルゴリズム × 3 ベンチ関数で収束履歴を比較し、HTML レポートを出力。
--
-- アルゴリズム: Nelder-Mead / L-BFGS / Brent (1D 専用) / DE / CMA-ES
-- ベンチ:       Sphere (凸 5D) / Rosenbrock (2D) / Rastrigin (5D 多峰)
--
-- 出力: trash/single_opt_bench.html
module Main where

import qualified Data.Text as T
import Text.Printf (printf)
import qualified System.Random.MWC as MWC

import qualified Hanalyze.Optim.Common              as OC
import qualified Hanalyze.Optim.NelderMead          as NM
import qualified Hanalyze.Optim.LBFGS               as LBFGS
import qualified Hanalyze.Optim.LineSearch          as LS
import qualified Hanalyze.Optim.DifferentialEvolution as DE
import qualified Hanalyze.Optim.CMAES               as CMAES
import qualified Hanalyze.Viz.ReportBuilder         as RB
import Graphics.Vega.VegaLite hiding (filter, name, sphere)

-- ベンチ関数
sphere, rosen, rastrigin :: [Double] -> Double
sphere     xs = sum [x*x | x <- xs]
rosen      [x, y] = (1-x)^(2::Int) + 100 * (y - x*x)^(2::Int)
rosen      _      = error "rosen: 2D"
rastrigin  xs =
  10 * fromIntegral (length xs) +
  sum [x*x - 10 * cos (2 * pi * x) | x <- xs]

-- L2 距離
l2 :: [Double] -> [Double] -> Double
l2 a b = sqrt (sum (zipWith (\x y -> (x-y)^(2::Int)) a b))

-- 1 アルゴリズム実行結果
data Run = Run
  { runName    :: T.Text
  , runValue   :: Double
  , runDist    :: Double           -- 最適点との距離
  , runIters   :: Int
  , runHistory :: [Double]
  } deriving Show

main :: IO ()
main = do
  gen <- MWC.createSystemRandom

  -- ===== Sphere 5D =====
  putStrLn "=== Sphere 5D (truth = origin) ==="
  let x0_5 = [3, -2, 1, 0.5, -1.5]
      truth5 = [0, 0, 0, 0, 0]
  rNM <- NM.runNelderMeadWith
           (NM.defaultNMConfig { NM.nmStop = OC.defaultStopCriteria { OC.stMaxIter = 800 } })
           sphere x0_5
  rLB <- LBFGS.runLBFGSNumeric LBFGS.defaultLBFGSConfig sphere x0_5
  rDE <- DE.runDEWith
           ((DE.defaultDEConfig (replicate 5 (-5, 5)))
              { DE.deStop = OC.defaultStopCriteria { OC.stMaxIter = 200 } })
           sphere gen
  rCM <- CMAES.runCMAESWith
           (CMAES.defaultCMAESConfig { CMAES.cmStop = OC.defaultStopCriteria { OC.stMaxIter = 200 } })
           sphere x0_5 gen
  let sphereRuns =
        [ runOf "Nelder-Mead" rNM truth5
        , runOf "L-BFGS"      rLB truth5
        , runOf "DE"          rDE truth5
        , runOf "CMA-ES"      rCM truth5
        ]
  mapM_ printRun sphereRuns

  -- ===== Rosenbrock 2D =====
  putStrLn "\n=== Rosenbrock 2D (truth = (1,1)) ==="
  let x0_2 = [-1.2, 1.0]
      truth2 = [1, 1]
  rNM2 <- NM.runNelderMeadWith
            (NM.defaultNMConfig { NM.nmStop = OC.defaultStopCriteria { OC.stMaxIter = 5000 } })
            rosen x0_2
  rLB2 <- LBFGS.runLBFGSNumeric
            (LBFGS.defaultLBFGSConfig { LBFGS.lbStop = OC.defaultStopCriteria { OC.stMaxIter = 500 } })
            rosen x0_2
  rDE2 <- DE.runDEWith
            ((DE.defaultDEConfig (replicate 2 (-3, 3)))
              { DE.deStop = OC.defaultStopCriteria { OC.stMaxIter = 300 } })
            rosen gen
  rCM2 <- CMAES.runCMAESWith
            (CMAES.defaultCMAESConfig { CMAES.cmStop = OC.defaultStopCriteria { OC.stMaxIter = 300 } })
            rosen x0_2 gen
  let rosenRuns =
        [ runOf "Nelder-Mead" rNM2 truth2
        , runOf "L-BFGS"      rLB2 truth2
        , runOf "DE"          rDE2 truth2
        , runOf "CMA-ES"      rCM2 truth2
        ]
  mapM_ printRun rosenRuns

  -- ===== Rastrigin 5D =====
  putStrLn "\n=== Rastrigin 5D (truth = origin, multimodal) ==="
  rNM3 <- NM.runNelderMeadWith
            (NM.defaultNMConfig { NM.nmStop = OC.defaultStopCriteria { OC.stMaxIter = 2000 } })
            rastrigin x0_5
  rDE3 <- DE.runDEWith
            ((DE.defaultDEConfig (replicate 5 (-5.12, 5.12)))
              { DE.deStop = OC.defaultStopCriteria { OC.stMaxIter = 500 } })
            rastrigin gen
  rCM3 <- CMAES.runCMAESWith
            (CMAES.defaultCMAESConfig { CMAES.cmStop = OC.defaultStopCriteria { OC.stMaxIter = 500 } })
            rastrigin x0_5 gen
  let rastriginRuns =
        [ runOf "Nelder-Mead" rNM3 truth5
        , runOf "DE"          rDE3 truth5
        , runOf "CMA-ES"      rCM3 truth5
        ]
  mapM_ printRun rastriginRuns

  -- ===== Brent 1D =====
  putStrLn "\n=== Brent 1D (parabola, truth = 2.5) ==="
  let pf = LS.brent LS.defaultBrentConfig (\[x] -> (x - 2.5)^(2::Int) + 1) 0 5
  printf "  Brent: x=%.6f  f=%.6f  iters=%d\n"
    (head (OC.orBest pf)) (OC.orValue pf) (OC.orIters pf)

  -- ===== HTML レポート =====
  let cfg = RB.defaultReportConfig "Single-objective optimizer benchmark"
      sections =
        [ RB.secMarkdown "Overview"
            "5 アルゴリズム × 3 ベンチで収束履歴を比較。各表で best value と truth との距離を表示。"
        , RB.secTable "Sphere 5D (truth = origin)"
            ["Algorithm", "Value", "‖x* - truth‖", "Iterations", "Converged"]
            (map runRow sphereRuns)
        , RB.secVega "Sphere 5D 収束履歴" (convergenceSpec sphereRuns)
        , RB.secTable "Rosenbrock 2D (truth = (1,1))"
            ["Algorithm", "Value", "‖x* - truth‖", "Iterations", "Converged"]
            (map runRow rosenRuns)
        , RB.secVega "Rosenbrock 2D 収束履歴" (convergenceSpec rosenRuns)
        , RB.secTable "Rastrigin 5D (truth = origin, multimodal)"
            ["Algorithm", "Value", "‖x* - truth‖", "Iterations", "Converged"]
            (map runRow rastriginRuns)
        , RB.secVega "Rastrigin 5D 収束履歴" (convergenceSpec rastriginRuns)
        , RB.secMarkdown "Brent 1D"
            (T.pack (printf "x* = %.6f, f(x*) = %.6f, iterations = %d"
                       (head (OC.orBest pf)) (OC.orValue pf) (OC.orIters pf)))
        ]
  RB.renderReport "trash/single_opt_bench.html" cfg sections
  putStrLn "\nWrote trash/single_opt_bench.html"

runOf :: T.Text -> OC.OptimResult -> [Double] -> Run
runOf nm r truth = Run nm (OC.orValue r) (l2 (OC.orBest r) truth) (OC.orIters r) (OC.orHistory r)

printRun :: Run -> IO ()
printRun r =
  printf "  %-12s  value=%10.4g  dist=%8.4g  iters=%d\n"
    (T.unpack (runName r)) (runValue r) (runDist r) (runIters r)

runRow :: Run -> [T.Text]
runRow r =
  [ runName r
  , T.pack (printf "%.4g" (runValue r))
  , T.pack (printf "%.4g" (runDist r))
  , T.pack (show (runIters r))
  , T.pack (show (runIters r > 0))   -- 雑な印
  ]

-- | 各アルゴリズムの best 値推移をライン重ね描き。
convergenceSpec :: [Run] -> VegaLite
convergenceSpec runs =
  let rows = concat
        [ [ dataRow [ ("alg",   Str (runName r))
                    , ("iter",  Number (fromIntegral i))
                    , ("value", Number v)
                    ] []
          | (i, v) <- zip [0::Int ..] (runHistory r) ]
        | r <- runs ]
      dat = dataFromRows [] (concat rows)
  in toVegaLite
       [ title "Best value vs iteration" []
       , dat
       , mark Line [MStrokeWidth 2]
       , encoding
           . position X [PName "iter",  PmType Quantitative, PTitle "iteration"]
           . position Y [PName "value", PmType Quantitative, PTitle "best value (log)", PScale [SType ScLog]]
           . color    [MName "alg", MmType Nominal]
           $ []
       , width 700
       , height 350
       ]
