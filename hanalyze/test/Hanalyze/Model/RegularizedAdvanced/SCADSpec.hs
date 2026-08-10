{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.RegularizedAdvanced.SCADSpec (spec) where

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
import qualified Data.Vector.Storable              as VS
import qualified System.Random.MWC as MWC
import qualified Hanalyze.Model.Regularized as Reg
import qualified System.Random.MWC as MWC
import qualified Hanalyze.Model.RegularizedAdvanced   as RegA
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.RegularizedAdvanced.SCAD (Phase 31-A3)" $ do
    let mkSparseSCAD :: MWC.GenIO -> IO (LA.Matrix Double, LA.Vector Double, [Double])
        mkSparseSCAD gen = do
          let nS = 200
              pS = 8
              betaTrue = [3.0, 1.5, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0] :: [Double]
          xVals <- VS.replicateM (nS * pS) (do
                     u1 <- MWC.uniformR (1e-9, 1.0 :: Double) gen
                     u2 <- MWC.uniformR (0.0, 1.0 :: Double) gen
                     pure (sqrt (-2 * log u1) * cos (2 * pi * u2)))
          noisesS <- VS.replicateM nS (do
                       u1 <- MWC.uniformR (1e-9, 1.0 :: Double) gen
                       u2 <- MWC.uniformR (0.0, 1.0 :: Double) gen
                       pure (0.1 * sqrt (-2 * log u1) * cos (2 * pi * u2)))
          let xMat = LA.reshape pS (LA.fromList (VS.toList xVals))
              bt   = LA.fromList betaTrue
              yVec = xMat LA.#> bt + LA.fromList (VS.toList noisesS)
          pure (xMat, yVec, betaTrue)
    it "fitSCAD: a=3.7、 中 λ で sparse 回復 + non-zero 真値の 20% 以内" $ do
      gen <- MWC.create
      (x, y, betaTrue) <- mkSparseSCAD gen
      let fit  = RegA.fitSCAD 0.1 3.7 x y 1000 1e-5
          beta = LA.toList (Reg.rfBeta fit)
      length beta `shouldBe` 8
      [beta !! i | i <- [0, 1, 4]] `shouldSatisfy` all (\b -> abs b > 0.5)
      [beta !! i | i <- [2, 3, 5, 6, 7]] `shouldSatisfy` all (\b -> abs b < 0.2)
      and [ abs (beta !! i - betaTrue !! i) < 0.2 * abs (betaTrue !! i)
          | i <- [0, 1, 4] ] `shouldBe` True
    it "fitSCAD: 3 領域 thresholding が連続 (λ → 0 で OLS、 λ 大で 0)" $ do
      gen <- MWC.create
      (x, y, _) <- mkSparseSCAD gen
      let smallFit = RegA.fitSCAD 1e-4 3.7 x y 1000 1e-5
          bigFit   = RegA.fitSCAD 100.0 3.7 x y 1000 1e-5
      LA.toList (Reg.rfBeta bigFit) `shouldSatisfy` all (\b -> abs b < 0.05)
      let sb = LA.toList (Reg.rfBeta smallFit)
      [sb !! i | i <- [0, 1, 4]] `shouldSatisfy` all (\b -> abs b > 1.0)
