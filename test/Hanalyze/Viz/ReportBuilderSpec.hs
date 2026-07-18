{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Viz.ReportBuilderSpec (spec) where

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
import qualified Hanalyze.Viz.ReportBuilder as RB
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Viz.ReportBuilder.secInterpolation" $ do
    it "defaultInterpReport で renderReport まで通る (smoke)" $
      withSystemTempFile "ha-interp.html" $ \fp h -> do
        hClose h
        let ir = RB.defaultInterpReport "test"
        RB.renderReport fp (RB.defaultReportConfig "T") [RB.secInterpolation ir]
        out <- readFile fp
        length out `shouldSatisfy` (> 100)

    it "InterpReport 全フィールド + extra で HTML に主要要素が含まれる" $
      withSystemTempFile "ha-interp2.html" $ \fp h -> do
        hClose h
        let ir = (RB.defaultInterpReport "regrid")
                   { RB.irInterpKind    = "PCHIP"
                   , RB.irGridKind      = "Adaptive"
                   , RB.irN             = 3
                   , RB.irPerIdObserved = [("a", [(0, 0), (1, 1)])]
                   , RB.irPerIdInterpY  = [("a", [(0, 0), (0.5, 0.5), (1, 1)])]
                   , RB.irGrid          = [0, 0.5, 1]
                   , RB.irDensity       = [(0, 1), (0.5, 2), (1, 1)]
                   , RB.irPerIdSummary  = [("a", 2, 0, 1, 0, 0, 0)]
                   , RB.irExtraEnabled  = True
                   , RB.irPerIdYRange   = [("a", 0, 1, 0, 1)]
                   }
        RB.renderReport fp (RB.defaultReportConfig "T") [RB.secInterpolation ir]
        out <- readFile fp
        out `shouldContain` "Parameters"
        out `shouldContain` "PCHIP"

  -- ===========================================================================
  -- Hanalyze.Stat.Test (Phase 1: hypothesis tests)
  -- ===========================================================================
