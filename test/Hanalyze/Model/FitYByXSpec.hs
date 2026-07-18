{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.FitYByXSpec (spec) where

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
import qualified Data.Text   as T
import qualified Numeric.LinearAlgebra as LA
import qualified Hanalyze.Stat.Test         as ST
import qualified Hanalyze.Model.FitYByX     as FXY
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.FitYByX (Phase 13.4)" $ do
    let xCont = LA.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        yCont = LA.fromList [2.1, 4.0, 5.9, 8.1, 10.0, 11.9, 14.1, 16.0, 18.0, 20.1]
        xCat  = LA.fromList [0, 0, 0, 1, 1, 1, 2, 2, 2]
        yByCat = LA.fromList [1, 2, 1.5, 10, 11, 10.5, 20, 21, 20.5]
        xCat2 = LA.fromList [0, 0, 1, 1, 0, 1]
        yCat2 = LA.fromList [0, 0, 1, 1, 0, 1]
        xBin  = LA.fromList [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]
        yBin  = LA.fromList [0, 0, 0, 0, 1, 1, 1, 1]
    it "FitContCont: y ≈ 2x で線形" $ do
      case FXY.fitYByX FXY.Continuous FXY.Continuous xCont yCont of
        Left e -> expectationFailure (T.unpack e)
        Right (FXY.FitContCont _) -> pure ()
        Right _ -> expectationFailure "wrong variant"
    it "FitCatCont: 3 group ANOVA で強く有意" $ do
      case FXY.fitYByX FXY.Categorical FXY.Continuous xCat yByCat of
        Left e -> expectationFailure (T.unpack e)
        Right (FXY.FitCatCont tr means) -> do
          ST.trPValue tr `shouldSatisfy` (< 0.001)
          length means `shouldBe` 3
        Right _ -> expectationFailure "wrong variant"
    it "FitCatCat: 完全相関で chi-square 棄却" $ do
      case FXY.fitYByX FXY.Categorical FXY.Categorical xCat2 yCat2 of
        Left e -> expectationFailure (T.unpack e)
        Right (FXY.FitCatCat tr) -> ST.trPValue tr `shouldSatisfy` (< 0.05)
        Right _ -> expectationFailure "wrong variant"
    it "FitContCat: binary y で logistic GLM 動作" $ do
      case FXY.fitYByX FXY.Continuous FXY.Categorical xBin yBin of
        Left e -> expectationFailure (T.unpack e)
        Right (FXY.FitContCat _) -> pure ()
        Right _ -> expectationFailure "wrong variant"
    it "FitContCat: y が 0/1 でないと Left" $ do
      case FXY.fitYByX FXY.Continuous FXY.Categorical xBin yByCat of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left"
    it "長さ mismatch は Left" $ do
      case FXY.fitYByX FXY.Continuous FXY.Continuous xCont (LA.fromList [1, 2]) of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left"
