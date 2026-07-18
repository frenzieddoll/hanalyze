{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.ReliabilityBlockDiagramSpec (spec) where

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
import qualified Hanalyze.Model.ReliabilityBlockDiagram as RBD
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.ReliabilityBlockDiagram (Phase 35-A4)" $ do
    it "Leaf: そのまま返す" $ do
      RBD.reliabilityOf (RBD.Leaf 0.9) `shouldBe` 0.9
    it "Series: 2 個直列 = 積" $ do
      let r = RBD.reliabilityOf (RBD.Series [RBD.Leaf 0.9, RBD.Leaf 0.8])
      abs (r - 0.72) `shouldSatisfy` (< 1e-12)
    it "Parallel: 2 個並列 = 1 - (1-p₁)(1-p₂)" $ do
      let r = RBD.reliabilityOf (RBD.Parallel [RBD.Leaf 0.9, RBD.Leaf 0.8])
      abs (r - (1 - 0.1 * 0.2)) `shouldSatisfy` (< 1e-12)
    it "KofN k=1 ≡ Parallel" $ do
      let bs = [RBD.Leaf 0.7, RBD.Leaf 0.6, RBD.Leaf 0.5]
          rK = RBD.reliabilityOf (RBD.KofN 1 bs)
          rP = RBD.reliabilityOf (RBD.Parallel bs)
      abs (rK - rP) `shouldSatisfy` (< 1e-12)
    it "KofN k=n ≡ Series" $ do
      let bs = [RBD.Leaf 0.7, RBD.Leaf 0.6, RBD.Leaf 0.5]
          rK = RBD.reliabilityOf (RBD.KofN 3 bs)
          rS = RBD.reliabilityOf (RBD.Series bs)
      abs (rK - rS) `shouldSatisfy` (< 1e-12)
    it "KofN 2-of-3 (同質 p=0.9): C(3,2)p²(1-p) + p³ = 0.972" $ do
      let r = RBD.reliabilityOf
                (RBD.KofN 2 [RBD.Leaf 0.9, RBD.Leaf 0.9, RBD.Leaf 0.9])
      abs (r - 0.972) `shouldSatisfy` (< 1e-12)
    it "ネスト: Parallel[Series[…], Series[…]]" $ do
      let s1 = RBD.Series [RBD.Leaf 0.9, RBD.Leaf 0.95]
          s2 = RBD.Series [RBD.Leaf 0.8, RBD.Leaf 0.85]
          r  = RBD.reliabilityOf (RBD.Parallel [s1, s2])
          r1 = 0.9 * 0.95
          r2 = 0.8 * 0.85
      abs (r - (1 - (1 - r1) * (1 - r2))) `shouldSatisfy` (< 1e-12)
    it "KofN k > n: 0、 k ≤ 0: 1" $ do
      RBD.reliabilityOf (RBD.KofN 5 [RBD.Leaf 0.9, RBD.Leaf 0.9]) `shouldBe` 0
      RBD.reliabilityOf (RBD.KofN 0 [RBD.Leaf 0.9, RBD.Leaf 0.9]) `shouldBe` 1
