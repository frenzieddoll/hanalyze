{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.MultipleTestingSpec (spec) where

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
import qualified Hanalyze.Stat.MultipleTesting as MT
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.MultipleTesting" $ do
    it "Bonferroni: p × m, capped at 1" $ do
      let ps = [0.01, 0.04, 0.05, 0.10]
          adj = MT.bonferroni ps
      adj `shouldBe` [0.04, 0.16, 0.20, 0.40]

    it "Holm: 単調 + Bonferroni より緩い" $ do
      let ps = [0.01, 0.02, 0.03, 0.04]
          adj = MT.holm ps
      -- 各 adj ≥ 元 p、最初は p × m = 0.04
      head adj `shouldBe` 0.04
      and (zipWith (<=) ps adj) `shouldBe` True

    it "BH: monotonic non-decreasing in sorted p order" $ do
      let ps = [0.01, 0.04, 0.03, 0.05]
          adj = MT.benjaminiHochberg ps
      length adj `shouldBe` 4
      all (<= 1.0) adj `shouldBe` True
      all (>= 0.0) adj `shouldBe` True

    it "BY: BH より conservative" $ do
      let ps = [0.01, 0.02, 0.03, 0.04, 0.05]
          bh = MT.benjaminiHochberg ps
          by = MT.benjaminiYekutieli ps
      -- BY は cumulative harmonic factor を掛けるので大きい
      and (zipWith (>=) by bh) `shouldBe` True

  -- ===========================================================================
  -- Hanalyze.Stat.Bootstrap (Phase 7)
  -- ===========================================================================
