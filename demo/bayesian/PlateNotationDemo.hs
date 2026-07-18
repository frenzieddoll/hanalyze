{-# LANGUAGE OverloadedStrings #-}
-- | Phase 40 plate 記法のデモ。 8-schools + nested 多レベルモデルの
-- mermaid HTML と graphviz DOT を出力する。
--
-- 実行:
--
-- > cabal run plate-notation-demo
--
-- 生成物 (demo-output/ 下):
--
-- - @8schools.html@   ブラウザで開くと mermaid plate (subgraph 囲い) で表示
-- - @8schools.dot@    @dot -Tpng 8schools.dot -o 8schools.png@ で PNG 化
-- - @multilevel.html@ nested plate (school × student)
-- - @multilevel.dot@  nested cluster
module Main where

import Control.Monad (forM_, forM)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)

import qualified Hanalyze.Model.HBM as HBM
import qualified Hanalyze.Viz.ModelGraph as VMG
import qualified Hanalyze.Viz.ModelGraphDot as VMGD

-- ---------------------------------------------------------------------------
-- モデル 1: 8-schools (Gelman et al.)
-- ---------------------------------------------------------------------------

eightSchools :: HBM.ModelP ()
eightSchools = do
  mu  <- HBM.sample "mu"  (HBM.Normal 0 5)
  tau <- HBM.sample "tau" (HBM.HalfCauchy 5)
  _ <- HBM.plate "school" 8 $ forM [0..7 :: Int] $ \j -> do
    eta <- HBM.sample ("eta_" <> T.pack (show j)) (HBM.Normal 0 1)
    HBM.observe ("y_" <> T.pack (show j))
                (HBM.Normal (mu + tau * eta) 1)
                [realToFrac j]
  return ()

-- ---------------------------------------------------------------------------
-- モデル 2: nested multi-level (school × student)
-- ---------------------------------------------------------------------------

multilevel :: HBM.ModelP ()
multilevel = do
  mu  <- HBM.sample "mu" (HBM.Normal 0 5)
  tau <- HBM.sample "tau" (HBM.HalfNormal 1)
  _ <- HBM.plate "school" 3 $ forM_ [0..2 :: Int] $ \j -> do
    theta <- HBM.sample ("theta_" <> T.pack (show j))
                        (HBM.Normal mu tau)
    _ <- HBM.plate "student" 2 $ forM_ [0..1 :: Int] $ \i ->
           HBM.observe ("y_" <> T.pack (show j) <> "_" <> T.pack (show i))
                       (HBM.Normal theta 1)
                       [realToFrac (j * 2 + i)]
    return ()
  return ()

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  createDirectoryIfMissing True "demo-output"
  -- 1. 8-schools (expanded = N 個列挙)
  let g1   = HBM.buildModelGraph eightSchools
      g1c  = HBM.collapseIndexedPlateNodes g1   -- PyMC 同等の集約
  VMG.renderModelGraph "demo-output/8schools-expanded.html"
    "8 schools - expanded (8 個列挙)" g1
  VMG.renderModelGraph "demo-output/8schools-collapsed.html"
    "8 schools - collapsed (PyMC 同等)" g1c
  VMGD.writeModelGraphDot "demo-output/8schools-expanded.dot"  g1
  VMGD.writeModelGraphDot "demo-output/8schools-collapsed.dot" g1c
  TIO.putStrLn "[1] 8-schools:"
  TIO.putStrLn "    展開 (Phase 40 旧):"
  TIO.putStrLn "    - demo-output/8schools-expanded.html / .dot"
  TIO.putStrLn "    集約 (Phase 40-A8 = PyMC 同等):"
  TIO.putStrLn "    - demo-output/8schools-collapsed.html / .dot"
  TIO.putStrLn $ "    mgPlates = " <> T.pack (show (HBM.mgPlates g1))
  TIO.putStrLn $ "    集約後ノード数 = " <> T.pack (show (length (HBM.mgNodes g1c)))
  -- 2. nested multilevel
  let g2  = HBM.buildModelGraph multilevel
      g2c = HBM.collapseIndexedPlateNodes g2
  VMG.renderModelGraph "demo-output/multilevel-expanded.html"
    "school × student - expanded" g2
  VMG.renderModelGraph "demo-output/multilevel-collapsed.html"
    "school × student - collapsed (PyMC 同等)" g2c
  VMGD.writeModelGraphDot "demo-output/multilevel-expanded.dot"  g2
  VMGD.writeModelGraphDot "demo-output/multilevel-collapsed.dot" g2c
  TIO.putStrLn "[2] nested multi-level:"
  TIO.putStrLn "    展開:    demo-output/multilevel-expanded.html / .dot"
  TIO.putStrLn "    集約:    demo-output/multilevel-collapsed.html / .dot"
  TIO.putStrLn $ "    mgPlates = " <> T.pack (show (HBM.mgPlates g2))
  TIO.putStrLn $ "    集約後ノード数 = " <> T.pack (show (length (HBM.mgNodes g2c)))
