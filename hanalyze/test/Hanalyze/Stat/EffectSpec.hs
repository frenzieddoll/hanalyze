{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.EffectSpec (spec) where

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
import qualified Hanalyze.Stat.Effect          as Eff
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.Effect" $ do
    it "cohenD: 同じ分布で 0、平均差 1 SD で ~1" $ do
      let xs = LA.fromList [1, 2, 3, 4, 5] :: LA.Vector Double
          ys = LA.fromList [1, 2, 3, 4, 5] :: LA.Vector Double
      Eff.cohenD xs ys `shouldBe` 0.0

      let zs = LA.fromList [2, 3, 4, 5, 6] :: LA.Vector Double  -- shifted
          d2 = Eff.cohenD zs xs
      d2 `shouldSatisfy` (> 0.5)

    it "hedgesG ≤ |cohenD| (small-sample correction)" $ do
      let xs = LA.fromList [1, 2, 3, 4, 5] :: LA.Vector Double
          ys = LA.fromList [3, 4, 5, 6, 7] :: LA.Vector Double
          d  = Eff.cohenD xs ys
          g  = Eff.hedgesG xs ys
      abs g `shouldSatisfy` (<= abs d + 1e-9)

    it "eta2: 完全分離で大、同分布で 0" $ do
      let g1 = LA.fromList [1, 2, 3] :: LA.Vector Double
          g2 = LA.fromList [10, 11, 12] :: LA.Vector Double
          g3 = LA.fromList [20, 21, 22] :: LA.Vector Double
      Eff.eta2 [g1, g2, g3] `shouldSatisfy` (> 0.9)

    it "powerTTest: d = 0 で α 付近 (= ~0.05)" $ do
      let p = Eff.powerTTest 30 0.05 0.0
      p `shouldSatisfy` (\x -> abs (x - 0.05) < 0.01)

    it "powerTTest: 大きい d で高い power" $ do
      let p = Eff.powerTTest 30 0.05 0.8
      p `shouldSatisfy` (> 0.85)

    it "sampleSizeTTest: 小 d ほど大 n が必要" $ do
      let n1 = Eff.sampleSizeTTest 0.80 0.05 0.2  -- small effect
          n2 = Eff.sampleSizeTTest 0.80 0.05 0.5  -- medium
          n3 = Eff.sampleSizeTTest 0.80 0.05 0.8  -- large
      n1 `shouldSatisfy` (> n2)
      n2 `shouldSatisfy` (> n3)

    it "cramerV: 2×2 完全独立で ~0、強従属で大" $ do
      Eff.cramerV 0 100 2 2 `shouldBe` 0.0
      Eff.cramerV 50 100 2 2 `shouldSatisfy` (> 0.5)

  -- ===========================================================================
  -- Hanalyze.Model.DecisionTree (Phase 10)
  -- ===========================================================================

  describe "Hanalyze.Stat.Effect CI (Phase 13.2)" $ do
    it "cohenDCI: 大効果 d ≈ 2 で CI が 0 を含まない" $ do
      let xs = LA.fromList [0, 0.1, -0.1, 0.2, -0.2, 0.05, -0.05, 0.15, -0.15, 0.0]
          ys = LA.fromList [2, 2.1, 1.9, 2.2, 1.8, 2.05, 1.95, 2.15, 1.85, 2.0]
          (d, (lo, hi)) = Eff.cohenDCI xs ys 0.05
      d `shouldSatisfy` (< 0)
      hi `shouldSatisfy` (< 0)
      lo `shouldSatisfy` (< hi)
    it "cohenDCI: 同分布で CI が 0 を含む" $ do
      let xs = LA.fromList [1, 2, 3, 2, 1, 2, 3, 2, 1, 2]
          ys = LA.fromList [1.1, 2.1, 2.9, 2.05, 1.05, 1.95, 3.05, 2.1, 0.95, 2.0]
          (_, (lo, hi)) = Eff.cohenDCI xs ys 0.05
      lo `shouldSatisfy` (< 0)
      hi `shouldSatisfy` (> 0)
    it "eta2CI: 大 F で点推定が 0 より十分大きい" $ do
      let (eta, _) = Eff.eta2CI 50.0 (2, 27) 0.05
      eta `shouldSatisfy` (> 0.5)
    it "eta2CI: 小 F で CI 下端が 0 に近い" $ do
      let (_, (lo, _)) = Eff.eta2CI 0.1 (2, 27) 0.05
      lo `shouldSatisfy` (< 0.01)
