{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Optim.LineSearchSpec (spec) where

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
import qualified Data.Vector as V
import qualified Hanalyze.Model.KernelRegression      as K
import qualified Hanalyze.Optim.LineSearch  as LS
import qualified Hanalyze.Optim.Common      as OC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Optim.LineSearch" $ do
    let parabola [x] = (x - 2.5)^(2::Int) + 1.0
        parabola _   = error "1D"
        cosBowl [x] = cos x + 0.1 * x * x   -- 単峰、最小 ≈ 1.428 付近
        cosBowl _   = error "1D"

    it "Brent: parabola minimum at x = 2.5" $ do
      let r = LS.brent LS.defaultBrentConfig parabola 0 5
      abs (head (OC.orBest r) - 2.5) `shouldSatisfy` (< 1e-5)
      OC.orValue r `shouldSatisfy` (\v -> abs (v - 1) < 1e-8)

    it "Brent: cos x + 0.1 x² minimum (verified by GS)" $ do
      let rB = LS.brent LS.defaultBrentConfig cosBowl 0 4
          rG = LS.goldenSection OC.Minimize cosBowl 0 4 1e-8 200
      abs (head (OC.orBest rB) - head (OC.orBest rG)) `shouldSatisfy` (< 1e-3)

    it "GoldenSection: parabola minimum at x = 2.5" $ do
      let r = LS.goldenSection OC.Minimize parabola 0 5 1e-7 200
      abs (head (OC.orBest r) - 2.5) `shouldSatisfy` (< 1e-3)

    it "Kernel.autoBandwidthBrent: 同じ最適 h を grid 法とほぼ一致" $ do
      let xs = V.fromList [0.0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
          ys = V.map (\x -> sin x + 0.1 * x) xs
          (hG, _) = K.gridSearchBandwidth K.Gaussian xs ys [0.5, 0.8, 1.0, 1.5, 2.0, 3.0]
          (hB, _) = K.autoBandwidthBrent K.Gaussian xs ys 0.3 4.0
      abs (hB - hG) `shouldSatisfy` (< 1.0)   -- グリッドと近い領域

  -- ===========================================================================
  -- 大域オプティマイザ (Hanalyze.Optim.DifferentialEvolution)
  -- ===========================================================================
