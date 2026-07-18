{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Optim.NSGASpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Hanalyze.Model.Formula
import Hanalyze.Model.Formula.Frame
import Hanalyze.Model.Formula.Design
import Hanalyze.Model.Formula.RFormula
import Hanalyze.Model.Formula.Nonlinear
import Hanalyze.Model.Formula.Mixed
import Hanalyze.Model.GLMM
import Hanalyze.Model.GLM (Family (..), LinkFn (..))
import Hanalyze.Stat.Distribution (Transform)
import Data.List (sort, nub)
import Control.Monad (forM, forM_)
import System.IO.Temp (withSystemTempFile)
import System.IO     (hPutStr, hClose)
import           Hanalyze.Model.HBM.Ast (Expr (..), Lit (..), DoStmt (..), Err)
import           Data.IORef         (newIORef, readIORef, modifyIORef')
import qualified Numeric.LinearAlgebra as LA
import qualified Hanalyze.Optim.NSGA           as NSGAP3
import qualified Hanalyze.Optim.NSGA        as NSGA
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC as MWC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Optim.NSGA building blocks" $ do
    let mkSol obj = NSGA.Solution
                      { NSGA.solDecision   = obj   -- decision unused here
                      , NSGA.solObjectives = obj
                      , NSGA.solViolation  = 0
                      }
        s00 = mkSol [0.0, 0.0]   -- dominated by no one
        s11 = mkSol [1.0, 1.0]   -- dominated by s00
        s05 = mkSol [0.5, 0.5]
        sa  = mkSol [0.0, 1.0]   -- non-comparable with sb
        sb  = mkSol [1.0, 0.0]

    it "dominates: zero-violation pair uses paretoDominates" $ do
      NSGA.dominates s00 s11 `shouldBe` True
      NSGA.dominates s11 s00 `shouldBe` False
      NSGA.dominates sa  sb  `shouldBe` False
      NSGA.dominates sb  sa  `shouldBe` False

    it "nonDominatedSort: 3-point chain returns 3 fronts" $ do
      -- s00 ≻ s05 ≻ s11
      let fronts = NSGA.nonDominatedSort [s11, s05, s00]
      length fronts `shouldBe` 3
      length (head fronts) `shouldBe` 1   -- F_1 = {s00}

    it "crowdingDistance: 2-point front keeps both (≤2 → ∞)" $
      length (NSGA.crowdingDistance [sa, sb]) `shouldBe` 2

    it "crowdingDistance: 3-point front returns all 3 (∞ endpoints + 1 mid)" $ do
      let f3     = [mkSol [0.0, 1.0], mkSol [0.5, 0.5], mkSol [1.0, 0.0]]
          sorted = NSGA.crowdingDistance f3
      length sorted `shouldBe` 3

    it "dominationMatrix matches per-pair dominates on a 3-individual pop" $ do
      let s00 = NSGA.Solution [0, 0] [0.0, 0.0] 0   -- best
          s11 = NSGA.Solution [1, 1] [1.0, 1.0] 0   -- worst
          s05 = NSGA.Solution [0.5, 0.5] [0.5, 0.5] 0  -- middle
          pop  = [s00, s05, s11]
          pm   = NSGA.fromSolutions pop
          mDom = NSGA.dominationMatrix pm
          n    = length pop
          ref  = LA.fromLists
                   [ [ if i == j then 0
                       else if NSGA.dominates (pop !! i) (pop !! j) then 1
                       else if NSGA.dominates (pop !! j) (pop !! i) then -1
                       else 0
                     | j <- [0 .. n - 1] ]
                   | i <- [0 .. n - 1] ]
                   :: LA.Matrix Double
      LA.norm_Inf (mDom - ref) `shouldBe` 0

    it "polynomialMutation: respects bounds and pMut=0 keeps x" $ do
      gen <- MWC.createSystemRandom
      let bs = [(0, 1), (0, 1), (0, 1)] :: [(Double, Double)]
      x' <- NSGA.polynomialMutation 20 0 bs [0.5, 0.5, 0.5] gen
      x' `shouldBe` [0.5, 0.5, 0.5]
      y' <- NSGA.polynomialMutation 20 1.0 bs [0.5, 0.5, 0.5] gen
      all (\(z, (lo, hi)) -> z >= lo && z <= hi) (zip y' bs)
        `shouldBe` True

  describe "Hanalyze.Optim.NSGA.nsga2AllFronts (Phase 3.2)" $ do
    -- ZDT1 (2D decision): f1 = x1、 g = 1 + 9·x2、 f2 = g·(1 − √(f1/g))。
    -- x2 > 0 が dominated 解を生む → rank ≥ 1 を確実に残せる。
    let zdt1 :: [Double] -> [Double]
        zdt1 xs = case xs of
          [x1, x2] ->
            let g  = 1 + 9 * x2
                f1 = x1
                f2 = g * (1 - sqrt (max 1e-12 (f1 / g)))
            in [f1, f2]
          _ -> [0, 0]
        bounds2 = [(0.0, 1.0), (0.0, 1.0)]
        cfg = NSGAP3.defaultNSGAConfig
          { NSGAP3.nsgaPopSize     = 40
          , NSGAP3.nsgaGenerations = 5     -- 少世代で dominated 解を残す
          }

    it "nsga2AllFronts は最低 1 front (rank 0) を返す" $ do
      gen <- MWC.create
      fronts <- NSGAP3.nsga2AllFronts cfg zdt1 bounds2 gen
      length fronts `shouldSatisfy` (>= 1)
      head fronts `shouldSatisfy` (not . null)

    it "front の総 Solution 数 = 母集団サイズ" $ do
      gen <- MWC.create
      fronts <- NSGAP3.nsga2AllFronts cfg zdt1 bounds2 gen
      sum (map length fronts) `shouldBe` NSGAP3.nsgaPopSize cfg

    it "既存 nsga2 の結果と nsga2AllFronts の rank 0 が同一 (同じ seed)" $ do
      gen1 <- MWC.create
      front0_legacy <- NSGAP3.nsga2 cfg zdt1 bounds2 gen1
      gen2 <- MWC.create
      fronts_new   <- NSGAP3.nsga2AllFronts cfg zdt1 bounds2 gen2
      map NSGAP3.solDecision front0_legacy
        `shouldBe` map NSGAP3.solDecision (head fronts_new)

    it "rank ≥ 1 にアクセスできる (ZDT1 で dominated 解が必ず残る)" $ do
      gen <- MWC.create
      fronts <- NSGAP3.nsga2AllFronts cfg zdt1 bounds2 gen
      length fronts `shouldSatisfy` (>= 2)

    it "take (k+1) fronts で rank フィルタ" $ do
      gen <- MWC.create
      fronts <- NSGAP3.nsga2AllFronts cfg zdt1 bounds2 gen
      let top2 = take 2 fronts
      length top2 `shouldSatisfy` (<= 2)
      length top2 `shouldSatisfy` (>= 1)

    it "constrained 版 nsga2AllFrontsWithConstraints も動作" $ do
      gen <- MWC.create
      let constraint xs = case xs of
            [a, b] -> max 0 (a + b - 1.5)
            _      -> 0
      fronts <- NSGAP3.nsga2AllFrontsWithConstraints cfg zdt1 constraint bounds2 gen
      length fronts `shouldSatisfy` (>= 1)
      head fronts `shouldSatisfy` (not . null)

  describe "Hanalyze.Optim.NSGA.nsga2WithProgress (Phase 3.3)" $ do
    let zdt1 :: [Double] -> [Double]
        zdt1 xs = case xs of
          [x1, x2] ->
            let g  = 1 + 9 * x2
                f1 = x1
                f2 = g * (1 - sqrt (max 1e-12 (f1 / g)))
            in [f1, f2]
          _ -> [0, 0]
        bounds2 = [(0.0, 1.0), (0.0, 1.0)]
        cfg = NSGAP3.defaultNSGAConfig
          { NSGAP3.nsgaPopSize     = 30
          , NSGAP3.nsgaGenerations = 8
          }

    it "callback が ngpTotal 回呼ばれる" $ do
      eventsRef <- newIORef []
      gen <- MWC.create
      _ <- NSGAP3.nsga2WithProgress cfg zdt1 bounds2
             (\p -> modifyIORef' eventsRef (p :)) gen
      events <- reverse <$> readIORef eventsRef
      length events `shouldBe` NSGAP3.nsgaGenerations cfg

    it "ngpGeneration は 0..total-1 で順番に並ぶ" $ do
      eventsRef <- newIORef []
      gen <- MWC.create
      _ <- NSGAP3.nsga2WithProgress cfg zdt1 bounds2
             (\p -> modifyIORef' eventsRef (p :)) gen
      events <- reverse <$> readIORef eventsRef
      map NSGAP3.ngpGeneration events
        `shouldBe` [0 .. NSGAP3.nsgaGenerations cfg - 1]

    it "ngpTotal は全 event で同値" $ do
      eventsRef <- newIORef []
      gen <- MWC.create
      _ <- NSGAP3.nsga2WithProgress cfg zdt1 bounds2
             (\p -> modifyIORef' eventsRef (p :)) gen
      events <- readIORef eventsRef
      length (nub (map NSGAP3.ngpTotal events)) `shouldBe` 1
      head (map NSGAP3.ngpTotal events) `shouldBe` NSGAP3.nsgaGenerations cfg

    it "ngpParetoSize > 0 (= rank 0 が常に非空)" $ do
      eventsRef <- newIORef []
      gen <- MWC.create
      _ <- NSGAP3.nsga2WithProgress cfg zdt1 bounds2
             (\p -> modifyIORef' eventsRef (p :)) gen
      events <- readIORef eventsRef
      all ((> 0) . NSGAP3.ngpParetoSize) events `shouldBe` True

    it "ngpBestObjs の length = 目的次元数 (= 2 for ZDT1)" $ do
      eventsRef <- newIORef []
      gen <- MWC.create
      _ <- NSGAP3.nsga2WithProgress cfg zdt1 bounds2
             (\p -> modifyIORef' eventsRef (p :)) gen
      events <- readIORef eventsRef
      all ((== 2) . length . NSGAP3.ngpBestObjs) events `shouldBe` True

    it "既存 nsga2 と nsga2WithProgress (no-op callback) は同じ Pareto を返す" $ do
      gen1 <- MWC.create
      front_legacy <- NSGAP3.nsga2 cfg zdt1 bounds2 gen1
      gen2 <- MWC.create
      front_prog   <- NSGAP3.nsga2WithProgress cfg zdt1 bounds2 (\_ -> pure ()) gen2
      map NSGAP3.solDecision front_legacy
        `shouldBe` map NSGAP3.solDecision front_prog
