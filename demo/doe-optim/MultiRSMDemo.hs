{-# LANGUAGE OverloadedStrings #-}
-- | Phase U: 多目的 RSM + Desirability の動作確認。
module Main where

import qualified Numeric.LinearAlgebra as LA
import Text.Printf (printf)

import Hanalyze.Design.RSM (centralCompositeRotatable)
import Hanalyze.Design.MultiRSM
import Hanalyze.Optim.Desirability

-- 真の関数 (3 応答):
--   y_1 = (x_1 - 0.5)² + (x_2)² + 1                      (最小化したい、極小は (0.5, 0))
--   y_2 = -x_1² - (x_2 - 0.5)² + 5                       (最大化したい、極大は (0, 0.5))
--   y_3 = (x_1 + x_2 - 1)²                              (target = 0、x_1 + x_2 = 1 で達成)
trueY :: [Double] -> [Double]
trueY [x1, x2] =
  [ (x1 - 0.5)^(2::Int) + x2^(2::Int) + 1
  , - x1^(2::Int) - (x2 - 0.5)^(2::Int) + 5
  , (x1 + x2 - 1)^(2::Int)
  ]
trueY _ = error "2D"

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  Phase U: 多目的 RSM + Desirability"
  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn ""

  -- CCD k=2、3 応答を生成
  let design = centralCompositeRotatable 2 3   -- 11 試行
      ys     = LA.fromLists [trueY r | r <- design]

  printf "計画サイズ: %d 試行 (CCD rotatable)\n" (length design)
  printf "応答数: %d\n" (LA.cols ys)
  putStrLn ""

  -- 多目的二次回帰
  let mqFit = fitMultiQuadratic design ys
      opts  = optimumPointsMulti mqFit
  putStrLn "[1] 各応答の個別最適点 (二次回帰の解析解)"
  mapM_ (\(j, (x, y, eigs)) -> do
            printf "  y_%d: x* = %s, y* = %.3f\n"
                   (j :: Int) (show (map (round3) x)) y
            printf "        Hessian eigs = %s\n"
                   (show (map round3 eigs)))
        (zip [1..] opts)
  putStrLn ""

  -- Desirability 設計
  putStrLn "[2] Desirability 設計"
  putStrLn "  y_1 を最小化 (1 ≤ y ≤ 2 で desirable)"
  putStrLn "  y_2 を最大化 (4 ≤ y ≤ 5 で desirable)"
  putStrLn "  y_3 を target=0 (許容 -1 ≤ y ≤ 1)"
  let dts = [ Minimize 2 1
            , Maximize 4 5
            , Target 0 (-1) 1 ]
  -- 既存の test 点で D を評価
  let testPoints = [[0.5, 0.5], [0.0, 0.5], [0.5, 0.0], [1.0, 0.0]]
  putStrLn "  各点での総合 desirability D:"
  mapM_ (\xp -> do
            let yp = trueY xp
                ds = zipWith individualDesirability dts yp
                d  = overallDesirability dts yp
            printf "    x=%s, y=%s, d=%s, D=%.3f\n"
                   (show (map round3 xp))
                   (show (map round3 yp))
                   (show (map round3 ds))
                   d)
        testPoints
  putStrLn ""

  putStrLn "═══════════════════════════════════════════════════════════════"
  putStrLn "  ✓ MultiRSM (q 応答の個別二次解析) + Desirability 集約 動作"
  putStrLn "═══════════════════════════════════════════════════════════════"

  where
    round3 :: Double -> Double
    round3 v = fromIntegral (round (v * 1000) :: Int) / 1000
