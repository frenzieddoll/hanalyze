{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Design.Custom.PowerSpec (spec) where

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
import qualified Hanalyze.Stat.ClassMetrics as CM
import qualified Hanalyze.Design.Optimal       as OPT
import qualified Hanalyze.Design.Custom.Factor     as CF
import qualified Hanalyze.Design.Custom.Model      as CM
import qualified Hanalyze.Design.Custom.Coordinate as CX
import qualified Hanalyze.Design.Custom.Power      as CPW
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Design.Custom.Power (Phase 24-8 designPower)" $ do
    -- Phase 24-7 で使った 2² factorial 設計 cdFact を再利用
    let f1p = CF.Factor "x1" (CF.Continuous (-1) 1) CF.Controllable
        f2p = CF.Factor "x2" (CF.Continuous (-1) 1) CF.Controllable
        mp  = CM.Model
          [CM.TIntercept, CM.TMain "x1", CM.TMain "x2"
          , CM.TInter ["x1","x2"]] CM.NCoded
        rawFact2 = LA.fromLists
          [[-1,-1],[1,-1],[-1,1],[1,1]
          ,[-1,-1],[1,-1],[-1,1],[1,1]]  -- 8 行 (n - p = 8 - 4 = 4 で df2 > 0)
        cdPow = CX.CustomDesign
          { CX.cdMatrix  = rawFact2
          , CX.cdFactors = [f1p, f2p]
          , CX.cdModel   = mp
          , CX.cdReport  = CX.CustomDesignReport
              { CX.crCriterion = OPT.DOpt
              , CX.crCriterionValue = -1, CX.crIterations = 0
              , CX.crRestarts = 0, CX.crConverged = True, CX.crSeed = Nothing
              }
          }
    it "termName: 各 ADT が canonical 名に変換される" $ do
      CPW.termName CM.TIntercept            `shouldBe` "(Intercept)"
      CPW.termName (CM.TMain "x1")          `shouldBe` "x1"
      CPW.termName (CM.TInter ["x1","x2"])  `shouldBe` "x1:x2"
      CPW.termName (CM.TPower "x1" 2)       `shouldBe` "x1^2"
    it "termColumnIndices: 連続のみで各 term が単一 column" $ do
      let m = CM.Model [CM.TIntercept, CM.TMain "x1", CM.TMain "x2"] CM.NCoded
      CPW.termColumnIndices [f1p, f2p] m
        `shouldBe` [("(Intercept)", [0]), ("x1", [1]), ("x2", [2])]
    it "designPower: effect 大 → power 増加 (x1 main)" $ do
      let p1 = CPW.designPower cdPow 1.0 [("x1", 0.5)] 0.05
          p2 = CPW.designPower cdPow 1.0 [("x1", 2.0)] 0.05
      length p1 `shouldBe` 1
      length p2 `shouldBe` 1
      CPW.dpPower (head p2) `shouldSatisfy` (> CPW.dpPower (head p1))
    it "designPower: effect 0 で power ≈ α" $ do
      let p = CPW.designPower cdPow 1.0 [("x1", 0)] 0.05
      CPW.dpPower (head p) `shouldSatisfy` (\v -> v >= 0 && v <= 0.10)
    it "designPower: 未知 term は power = 0" $ do
      let p = CPW.designPower cdPow 1.0 [("xnope", 1.0)] 0.05
      CPW.dpPower (head p) `shouldBe` 0
    it "designPower: alpha / effect / term が結果に保存される" $ do
      let [r] = CPW.designPower cdPow 1.0 [("x1", 1.5)] 0.01
      CPW.dpTerm   r `shouldBe` "x1"
      CPW.dpEffect r `shouldBe` 1.5
      CPW.dpAlpha  r `shouldBe` 0.01
