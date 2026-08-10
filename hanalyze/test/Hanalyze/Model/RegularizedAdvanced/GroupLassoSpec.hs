{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.RegularizedAdvanced.GroupLassoSpec (spec) where

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
  describe "Hanalyze.Model.RegularizedAdvanced.GroupLasso (Phase 31-A4)" $ do
    -- 3 group: G1 = [0,1,2] (active)、 G2 = [3,4,5] (inactive)、 G3 = [6,7] (active)
    -- β_true = [3, 1, 2, 0, 0, 0, 1.5, -1]
    -- Y = X β + N(0, 0.1)
    it "fitGroupLasso: 不活性 group の全係数を 0、 活性 group は回復" $ do
      gen <- MWC.create
      let nG = 300
          pG = 8
          betaTrue = [3.0, 1.0, 2.0, 0.0, 0.0, 0.0, 1.5, -1.0] :: [Double]
          groups   = [[0, 1, 2], [3, 4, 5], [6, 7]]
      xVals <- VS.replicateM (nG * pG) (do
                 u1 <- MWC.uniformR (1e-9, 1.0 :: Double) gen
                 u2 <- MWC.uniformR (0.0, 1.0 :: Double) gen
                 pure (sqrt (-2 * log u1) * cos (2 * pi * u2)))
      noisesG <- VS.replicateM nG (do
                   u1 <- MWC.uniformR (1e-9, 1.0 :: Double) gen
                   u2 <- MWC.uniformR (0.0, 1.0 :: Double) gen
                   pure (0.1 * sqrt (-2 * log u1) * cos (2 * pi * u2)))
      let xMat = LA.reshape pG (LA.fromList (VS.toList xVals))
          bt   = LA.fromList betaTrue
          yVec = xMat LA.#> bt + LA.fromList (VS.toList noisesG)
          fit  = RegA.fitGroupLasso 0.05 groups xMat yVec 1000 1e-5
          beta = LA.toList (Reg.rfBeta fit)
      length beta `shouldBe` pG
      -- G2 (index 3,4,5) は全て 0 近く
      [beta !! i | i <- [3, 4, 5]] `shouldSatisfy` all (\b -> abs b < 0.1)
      -- G1 / G3 は活性
      [beta !! i | i <- [0, 1, 2, 6, 7]] `shouldSatisfy` any (\b -> abs b > 0.5)
    it "fitGroupLasso: 大 λ で全 group を 0" $ do
      gen <- MWC.create
      let nG = 100
          pG = 4
          groups = [[0, 1], [2, 3]]
      xVals <- VS.replicateM (nG * pG) (do
                 u1 <- MWC.uniformR (1e-9, 1.0 :: Double) gen
                 u2 <- MWC.uniformR (0.0, 1.0 :: Double) gen
                 pure (sqrt (-2 * log u1) * cos (2 * pi * u2)))
      let xMat = LA.reshape pG (LA.fromList (VS.toList xVals))
          yVec = LA.fromList [fromIntegral (i `mod` 3) | i <- [0 .. nG - 1]]
          fit  = RegA.fitGroupLasso 100.0 groups xMat yVec 1000 1e-5
      LA.toList (Reg.rfBeta fit) `shouldSatisfy` all (\b -> abs b < 0.05)
