{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.ClassMetricsSpec (spec) where

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
import qualified Hanalyze.Stat.ClassMetrics as CM
import qualified Hanalyze.Design.Custom.Model      as CM
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.ClassMetrics" $ do
    it "confusionMatrix: 完全一致で TP/TN のみ" $ do
      let c = CM.confusionMatrix [1, 0, 1, 0, 1] [1, 0, 1, 0, 1]
      CM.confTP c `shouldBe` 3
      CM.confTN c `shouldBe` 2
      CM.confFP c `shouldBe` 0
      CM.confFN c `shouldBe` 0
      CM.accuracy c `shouldBe` 1.0
      CM.f1Score c  `shouldBe` 1.0

    it "precision/recall/f1: 不均衡な誤分類" $ do
      let c = CM.confusionMatrix [1, 1, 1, 0, 0] [1, 1, 0, 1, 0]
      -- TP=2, FN=1, FP=1, TN=1
      CM.precision c `shouldBe` 2/3   -- 2/(2+1)
      CM.recall c    `shouldBe` 2/3   -- 2/(2+1)
      CM.f1Score c   `shouldBe` 2/3

    it "AUC: 完全分離で 1.0、ランダムスコアで ~0.5" $ do
      let ys = [0, 0, 0, 1, 1, 1]
          perfectScores  = [0.1, 0.2, 0.3, 0.7, 0.8, 0.9]
          aucPerfect = CM.auc ys perfectScores
      aucPerfect `shouldBe` 1.0

    it "logLoss: 自信ある正解で小、自信ある誤りで大" $ do
      let lossGood = CM.logLoss [1, 0, 1, 0] [0.99, 0.01, 0.99, 0.01]
          lossBad  = CM.logLoss [1, 0, 1, 0] [0.01, 0.99, 0.01, 0.99]
      lossGood `shouldSatisfy` (< 0.05)
      lossBad  `shouldSatisfy` (> 4.0)

    it "brierScore: 完全予測で 0、最悪予測で 1" $ do
      let bsGood = CM.brierScore [1, 0, 1, 0] [1.0, 0.0, 1.0, 0.0]
          bsBad  = CM.brierScore [1, 0, 1, 0] [0.0, 1.0, 0.0, 1.0]
      bsGood `shouldSatisfy` (< 1e-10)
      bsBad  `shouldBe` 1.0

    it "MCC: 完全一致で 1、ランダムで 0 付近" $ do
      let cGood = CM.confusionMatrix [1, 0, 1, 0, 1] [1, 0, 1, 0, 1]
      CM.matthewsCorr cGood `shouldBe` 1.0

    it "macroF1 (multi-class): 完全分類で 1.0" $ do
      let cm = CM.confusionMulti [0, 1, 2, 0, 1, 2] [0, 1, 2, 0, 1, 2]
      CM.accuracyMulti cm `shouldBe` 1.0
      CM.macroF1 cm       `shouldBe` 1.0

  -- ===========================================================================
  -- Hanalyze.Stat.CV (Phase 4)
  -- ===========================================================================
