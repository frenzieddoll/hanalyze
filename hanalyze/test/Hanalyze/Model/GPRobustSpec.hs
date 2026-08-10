{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.GPRobustSpec (spec) where

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
import qualified Hanalyze.Model.GP          as GP
import qualified Hanalyze.Model.GPRobust    as GPR
import qualified Hanalyze.Model.GP        as GP
import qualified Hanalyze.Model.GPRobust  as GPR
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.GPRobust" $ do
    it "Cauchy GP is more accurate than Gaussian GP under outliers" $ do
      let trueF x = sin x
          xs = [0.0, 0.5 .. 6.0]
          cleanY = map trueF xs
          -- Inject outlier at index 5
          ys = zipWith (\i y -> if i == 5 then y + 5 else y)
                       [0::Int ..] cleanY
          hp = GP.GPParams 1.0 1.0 0.05 1.0 Nothing
          gpRes  = GP.fitGP (GP.GPModel GP.RBF hp) xs ys xs
          gaussRMSE = sqrt (sum [ (a - b) ^ (2::Int)
                                | (a, b) <- zip cleanY (GP.gpMean gpRes) ]
                            / fromIntegral (length xs))
          cauchyFit = GPR.fitGPRobust GP.RBF hp (GPR.RCauchy 0.5) xs ys
          cauchyPred = GPR.predictGPRobust cauchyFit xs
          cauchyRMSE = sqrt (sum [ (a - b) ^ (2::Int)
                                 | (a, (b, _)) <- zip cleanY cauchyPred ]
                             / fromIntegral (length xs))
      cauchyRMSE `shouldSatisfy` (< gaussRMSE)

    it "IRLS converges in finite iterations" $ do
      let xs = [0.0, 1.0, 2.0, 3.0, 4.0]
          ys = [0.1, 1.05, 1.95, 2.9, 4.1]
          hp = GP.GPParams 1.0 1.0 0.1 1.0 Nothing
          fit = GPR.fitGPRobust GP.RBF hp (GPR.RStudentT 4 0.5) xs ys
      GPR.rgpIters fit `shouldSatisfy` (\n -> n > 0 && n <= 50)
