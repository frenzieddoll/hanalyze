{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.CompetingRisksSpec (spec) where

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
import qualified Hanalyze.Model.CompetingRisks as CR
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.CompetingRisks (Phase 35-A3)" $ do
    -- 簡単な手計算例: 5 サンプル、 2 causes、 censoring 含む
    -- t=1 (cause 1), t=2 (cause 2), t=3 (cause 1), t=4 (censored), t=5 (cause 2)
    let samples = [ CR.CRSample 1.0 1, CR.CRSample 2.0 2, CR.CRSample 3.0 1
                  , CR.CRSample 4.0 0, CR.CRSample 5.0 2 ]
        fit     = CR.fitCompetingRisks samples
    it "fitCompetingRisks: 検出された causes" $ do
      CR.crfCauses fit `shouldBe` [1, 2]
    it "fitCompetingRisks: event times (censored 除外)" $ do
      LA.toList (CR.crfTimes fit) `shouldBe` [1.0, 2.0, 3.0, 5.0]
    it "fitCompetingRisks: 全 CIF の和 + 全生存 ≈ 1 (各 event time で)" $ do
      let s    = LA.toList (CR.crfOverallSurvival fit)
          cifs = [ LA.toList v | (_, v) <- CR.crfCIF fit ]
          n    = length s
          totAt i = sum [ cif !! i | cif <- cifs ] + (s !! i)
      and [ abs (totAt i - 1.0) < 1e-9 | i <- [0 .. n - 1] ]
        `shouldBe` True
    it "fitCompetingRisks: CIF は単調非減少" $ do
      let nonDec xs = and (zipWith (<=) xs (tail xs))
      and [ nonDec (LA.toList v) | (_, v) <- CR.crfCIF fit ]
        `shouldBe` True
    it "fitCompetingRisks: t=1 で F̂_1 = 1/5 (cause 1、 n=5, d=1)" $ do
      let Just cif1 = lookup 1 (CR.crfCIF fit)
      abs (LA.atIndex cif1 0 - 0.2) `shouldSatisfy` (< 1e-9)
    it "fitCompetingRisks: cause 2 だけのケースは KM 1-S と一致" $ do
      -- 単一 cause なら CIF = 1 - S (overall) と一致するはず
      let s2 = [ CR.CRSample 1.0 1, CR.CRSample 2.0 1, CR.CRSample 3.0 0
               , CR.CRSample 4.0 1 ]
          fit2 = CR.fitCompetingRisks s2
          Just cif = lookup 1 (CR.crfCIF fit2)
          sv = CR.crfOverallSurvival fit2
          oneMinusS = [ 1 - x | x <- LA.toList sv ]
      and [ abs (a - b) < 1e-9
          | (a, b) <- zip (LA.toList cif) oneMinusS ]
        `shouldBe` True
