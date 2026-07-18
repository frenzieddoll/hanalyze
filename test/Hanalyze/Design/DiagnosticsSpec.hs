{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Design.DiagnosticsSpec (spec) where

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
import qualified Hanalyze.Design.Diagnostics   as DDiag
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Design.Diagnostics (Phase 14)" $ do
    let -- 直交設計 (full factorial 2² + intercept)
        xOrth = LA.fromLists
          [ [1, -1, -1]
          , [1,  1, -1]
          , [1, -1,  1]
          , [1,  1,  1]
          ]
        -- multicollinear: 列 2 = 列 1 + 微小ノイズ
        xCollin = LA.fromLists
          [ [1, -1, -0.999]
          , [1,  1,  1.001]
          , [1, -1, -1.001]
          , [1,  1,  0.999]
          ]
    it "diagnostics: 直交設計で全 efficiency が >= 0" $ do
      let dd = DDiag.diagnostics xOrth
      DDiag.ddDEff dd `shouldSatisfy` (> 0)
      DDiag.ddAEff dd `shouldSatisfy` (> 0)
      DDiag.ddGEff dd `shouldSatisfy` (> 0)
    it "VIF: 直交設計で全列の VIF ≈ 1" $ do
      let dd = DDiag.diagnostics xOrth
          v  = DDiag.ddVIF dd
      LA.atIndex v 0 `shouldSatisfy` (\x -> abs (x - 1) < 1e-6)
      LA.atIndex v 1 `shouldSatisfy` (\x -> abs (x - 1) < 1e-6)
      LA.atIndex v 2 `shouldSatisfy` (\x -> abs (x - 1) < 1e-6)
    it "VIF: 多重共線設計で列 1/2 の VIF が大きい" $ do
      let dd = DDiag.diagnostics xCollin
          v  = DDiag.ddVIF dd
      LA.atIndex v 1 `shouldSatisfy` (> 10)
    it "diagnostics: D-efficiency は直交 > collinear" $ do
      let dOrth = DDiag.ddDEff (DDiag.diagnostics xOrth)
          dCol  = DDiag.ddDEff (DDiag.diagnostics xCollin)
      dOrth `shouldSatisfy` (> dCol)
    it "aliasMatrix: 直交設計 vs 平方項で形状確認" $ do
      let z = LA.fromLists [[1], [1], [1], [1]]   -- 例: 全 1 (intercept alias)
          a = DDiag.aliasMatrix xOrth z
      LA.rows a `shouldBe` 3
      LA.cols a `shouldBe` 1
