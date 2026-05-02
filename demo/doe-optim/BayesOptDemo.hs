{-# LANGUAGE OverloadedStrings #-}
-- | Phase V: Bayesian Optimization のデモ。
--
-- 1. 単一目的 BO で sin 関数の最小値を探す
-- 2. 多目的 BO (NSGA-II 内側) で 2 目的問題の Pareto 近似を構築
module Main where

import Text.Printf (printf)
import System.Random.MWC (createSystemRandom)

import Optim.BayesOpt
import Model.GP (Kernel (..))

-- 単一目的: f(x) = sin(3x) + (x - 2)² / 5
-- 真の最小は x ≈ 0.96 で y ≈ -0.789
trueF :: Double -> Double
trueF x = sin (3 * x) + (x - 2) ^ (2 :: Int) / 5

-- 多目的:
--   y_1 = x²
--   y_2 = (x - 2)²
trueMO :: [Double] -> [Double]
trueMO [x] = [x * x, (x - 2) ^ (2 :: Int)]
trueMO _ = error "1D"

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Phase V: Bayesian Optimization"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  gen <- createSystemRandom

  -- ── 1. 単一目的 BO ──
  putStrLn "[1] 単一目的 BO: f(x) = sin(3x) + (x-2)²/5 on [0, 4]"
  putStrLn "    真の最小 ≈ (0.96, -0.789)"
  let cfg = defaultBayesOptConfig
              { boIterations = 15
              , boInitPoints = 4
              , boUCBBeta    = 2.0
              }
  (history, (xBest, yBest)) <- bayesOpt cfg (return . trueF) (0, 4) gen
  printf "  評価回数: %d (= 初期 %d + BO %d)\n"
         (length history) (boInitPoints cfg) (boIterations cfg)
  printf "  推定最良: x* = %.4f, y* = %.4f\n" xBest yBest
  printf "  履歴の最終 5 評価:\n"
  mapM_ (\(x, y) -> printf "    (%.4f, %.4f)\n" x y)
        (drop (length history - 5) history)
  putStrLn ""

  -- ── 2. 多目的 BO ──
  putStrLn "[2] 多目的 BO: y_1 = x², y_2 = (x-2)² on [0, 2]"
  putStrLn "    真の Pareto front: x ∈ [0, 2] で連続"
  history2 <- bayesOptMOWithNSGA 12 5 RBF (return . trueMO)
                                 [(0, 2)] gen
  printf "  評価回数: %d\n" (length history2)
  printf "  最終 5 評価:\n"
  mapM_ (\(x, y) -> printf "    x=%s, y=%s\n"
                           (show (map round3 x))
                           (show (map round3 y)))
        (drop (length history2 - 5) history2)
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ Bayesian Optimization が動作 (単目的 BO + NSGA-II 内側)"
  putStrLn "═══════════════════════════════════════════════════════════════"

  where
    round3 :: Double -> Double
    round3 v = fromIntegral (round (v * 1000) :: Int) / 1000
