{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.AFTSpec (spec) where

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
import qualified Data.Text   as T
import qualified Numeric.LinearAlgebra as LA
import qualified Hanalyze.Model.AFT         as AFT
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.AFT (Phase 12)" $ do
    let ts = LA.fromList [exp 1.5, exp 1.8, exp 2.0, exp 2.1, exp 2.3,
                          exp 1.9, exp 2.05, exp 2.15, exp 1.95, exp 2.2]
        delta = V.replicate 10 True
        x1 = LA.fromColumns [LA.fromList (replicate 10 1.0)]
    it "fitAFT LogNormal intercept-only: β₀ ≈ mean(log t)" $ do
      r <- AFT.fitAFT AFT.AFTLogNormal x1 ts delta
      case r of
        Left e -> expectationFailure (T.unpack e)
        Right fit -> do
          let b0   = LA.atIndex (AFT.aftBeta fit) 0
              meanLogT = LA.sumElements (LA.cmap log ts) / fromIntegral (LA.size ts)
          abs (b0 - meanLogT) `shouldSatisfy` (< 0.05)
    it "fitAFT Weibull: 打ち切り混在で動く" $ do
      let delta2 = V.fromList [True, True, False, True, True,
                               True, False, True, True, True]
      r <- AFT.fitAFT AFT.AFTWeibull x1 ts delta2
      case r of
        Left e -> expectationFailure (T.unpack e)
        Right fit -> AFT.aftScale fit `shouldSatisfy` (> 0)
    it "fitAFT 入力長 mismatch は Left" $ do
      r <- AFT.fitAFT AFT.AFTLogNormal x1 (LA.fromList [1, 2]) delta
      case r of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Left"
    it "predictAFT Exponential: E[T] = exp(X β)" $ do
      r <- AFT.fitAFT AFT.AFTExponential x1 ts delta
      case r of
        Left e -> expectationFailure (T.unpack e)
        Right fit -> do
          let preds = AFT.predictAFT fit x1
              b0    = LA.atIndex (AFT.aftBeta fit) 0
          abs (LA.atIndex preds 0 - exp b0) `shouldSatisfy` (< 1e-6)
