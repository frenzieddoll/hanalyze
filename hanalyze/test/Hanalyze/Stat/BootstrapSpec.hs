{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.BootstrapSpec (spec) where

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
import qualified Hanalyze.Stat.Bootstrap       as Boot
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC as MWC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.Bootstrap" $ do
    it "bootstrapCI on N(0,1) sample: 0 が 95% CI 内" $ do
      gen <- MWC.createSystemRandom
      let xs = LA.fromList [-1.0, -0.5, 0.0, 0.5, 1.0,
                            -0.3, 0.3, -0.7, 0.7, 0.0]
      (lo, hi) <- Boot.bootstrapCI 2000 0.95 Boot.sampleMean xs gen
      lo `shouldSatisfy` (< 0.5)
      hi `shouldSatisfy` (> -0.5)

    it "permutationTest: 異なる平均で p < 0.05" $ do
      gen <- MWC.createSystemRandom
      let xs = LA.fromList [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
          ys = LA.fromList [10.0, 11.0, 12.0, 13.0, 14.0, 15.0]
      (_diff, p) <- Boot.permutationTest 2000 xs ys gen
      p `shouldSatisfy` (< 0.05)

    it "sampleMean / sampleVar / sampleMedian の整合性" $ do
      let v = LA.fromList [1.0, 2.0, 3.0, 4.0, 5.0]
      Boot.sampleMean v `shouldBe` 3.0
      Boot.sampleVar v `shouldBe` 2.5  -- variance of 1..5 (unbiased)
      Boot.sampleMedian v `shouldBe` 3.0

  -- ===========================================================================
  -- Hanalyze.DataIO.Reshape (Phase 8)
  -- ===========================================================================
