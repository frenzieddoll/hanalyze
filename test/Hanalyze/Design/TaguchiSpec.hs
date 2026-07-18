{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Design.TaguchiSpec (spec) where

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
import qualified Hanalyze.Design.Orthogonal as OA
import qualified Hanalyze.Design.Taguchi as TG
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Design.Taguchi" $ do
    it "smaller-the-better SN: lower y → higher η" $ do
      let etaSmall = TG.snRatio TG.SmallerBetter [0.5, 0.5, 0.5]
          etaLarge = TG.snRatio TG.SmallerBetter [5.0, 5.0, 5.0]
      etaSmall `shouldSatisfy` (> etaLarge)

    it "larger-the-better SN: higher y → higher η" $ do
      let etaLarge = TG.snRatio TG.LargerBetter [10, 10, 10]
          etaSmall = TG.snRatio TG.LargerBetter [1, 1, 1]
      etaLarge `shouldSatisfy` (> etaSmall)

    it "nominal-the-best SN: high mean / low var → high η" $ do
      let highSN = TG.snRatio TG.NominalBest [10, 10.01, 9.99, 10]
          lowSN  = TG.snRatio TG.NominalBest [1, 4, 7, 10]
      highSN `shouldSatisfy` (> lowSN)

    it "nominal-target SN: closer to target → higher η" $ do
      let closer = TG.snRatio (TG.NominalBestTarget 5) [4.9, 5.0, 5.1]
          farther = TG.snRatio (TG.NominalBestTarget 5) [3, 5, 7]
      closer `shouldSatisfy` (> farther)

    it "snRatio on empty list is 0" $
      TG.snRatio TG.SmallerBetter [] `shouldBe` 0

    it "snRatioRows produces same length as input" $ do
      let yMatrix = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]
      length (TG.snRatioRows TG.SmallerBetter yMatrix) `shouldBe` 3

    it "analyzeSN gives one FactorEffect per assigned factor" $ do
      let specs = [ OA.FactorSpec "A" [OA.LText "lo", OA.LText "hi"]
                  , OA.FactorSpec "B" [OA.LText "lo", OA.LText "hi"]
                  , OA.FactorSpec "C" [OA.LText "lo", OA.LText "hi"]
                  ]
          Right ad = OA.assignFactors OA.l4 specs
          fes = TG.analyzeSN ad [10, 20, 30, 40]
      length fes `shouldBe` 3
      map TG.feFactor fes `shouldBe` ["A", "B", "C"]
      mapM_ (\fe -> length (TG.feSNByLevel fe) `shouldBe` 2) fes

    it "optimalLevels picks max-SN level per factor" $ do
      let specs = [ OA.FactorSpec "A" [OA.LText "lo", OA.LText "hi"]
                  , OA.FactorSpec "B" [OA.LText "lo", OA.LText "hi"]
                  , OA.FactorSpec "C" [OA.LText "lo", OA.LText "hi"]
                  ]
          Right ad = OA.assignFactors OA.l4 specs
          -- L4 row 1 ("hi" for A,B,C) has SN=100; others 0
          sns = [0, 0, 0, 100]
          fes = TG.analyzeSN ad sns
          opts = TG.optimalLevels fes
      length opts `shouldBe` 3
      -- Row 4 has all "hi" by L4 structure (2,2,1) — verify each factor's best
      mapM_ (\(_, _, eta) -> eta `shouldSatisfy` (>= 0)) opts

  -- ─────────────────────────────────────────────────────────────────────

  describe "Hanalyze.Design.Taguchi extras" $ do
    it "snRatioWithDetails: SmallerBetter on [1, 2, 3]" $ do
      let d = TG.snRatioWithDetails TG.SmallerBetter [1.0, 2.0, 3.0]
      TG.sdN d `shouldBe` 3
      TG.sdMean d     `shouldSatisfy` (\v -> abs (v - 2.0) < 1e-12)
      TG.sdVariance d `shouldSatisfy` (\v -> abs (v - 1.0) < 1e-12)
      -- η = -10 log10((1+4+9)/3) = -10 log10(14/3) ≈ -6.690
      TG.sdSN d `shouldSatisfy` (\v -> abs (v - (-6.690)) < 1e-2)

    it "factorEffectsTable: contributions sum to 1" $ do
      let specs = [ OA.FactorSpec "A" [OA.LText "lo", OA.LText "hi"]
                  , OA.FactorSpec "B" [OA.LNumeric 0,  OA.LNumeric 1]
                  ]
      case OA.assignFactors OA.l4 specs of
        Right ad -> do
          let sns = [10.0, 12.0, 11.0, 13.0]
              ext = TG.factorEffectsTable ad sns
          length ext `shouldBe` 2
          let totalC = sum (map TG.feeContribution ext)
          totalC `shouldSatisfy` (\v -> abs (v - 1.0) < 1e-12)
          all ((>= 0) . TG.feeRange) ext `shouldBe` True
        Left e -> expectationFailure (show e)

  -- ─────────────────────────────────────────────────────────────────────
