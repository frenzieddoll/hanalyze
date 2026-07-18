{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.NumberFormatSpec (spec) where

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
import qualified Hanalyze.Stat.NumberFormat as NF
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.NumberFormat" $ do
    it "0 → '0.00'"            $ NF.fmtNum 0       `shouldBe` "0.00"
    it "中域 (0.01..999) は固定小数点 2 桁" $ do
      NF.fmtNum 0.91   `shouldBe` "0.91"
      NF.fmtNum 12.34  `shouldBe` "12.34"
      NF.fmtNum 998.7  `shouldBe` "998.70"
    it "巨大値は指数表記" $ do
      NF.fmtNum 1.10e13 `shouldBe` "1.10E+13"
      NF.fmtNum 1234.5  `shouldBe` "1.23E+3"
    it "極小値は指数表記" $ do
      NF.fmtNum 3.057e-24 `shouldBe` "3.06E-24"
      NF.fmtNum 0.0099    `shouldBe` "9.90E-3"
    it "負の値" $ do
      NF.fmtNum (-12.34)  `shouldBe` "-12.34"
      NF.fmtNum (-1.5e10) `shouldBe` "-1.50E+10"
    it "非有限値" $ do
      NF.fmtNum (0/0)        `shouldBe` "NaN"
      NF.fmtNum (1/0)        `shouldBe` "+Inf"
      NF.fmtNum (-1/0)       `shouldBe` "-Inf"
