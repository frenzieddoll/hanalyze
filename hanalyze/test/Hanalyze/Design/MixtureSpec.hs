{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Design.MixtureSpec (spec) where

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
import qualified Hanalyze.Design.Mixture       as Mix
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Design.Mixture (Phase 7.1)" $ do
    let sumsToOne row = abs (sum row - 1.0) < 1e-12

    it "sanity: m < 2 は Left" $ do
      case Mix.mixtureDesign Mix.SimplexCentroid 1 of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Left for m=1"
      case Mix.mixtureDesign (Mix.SimplexLattice 2) 0 of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Left for m=0"

    it "sanity: SimplexLattice d < 1 は Left" $
      case Mix.mixtureDesign (Mix.SimplexLattice 0) 3 of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Left for d=0"

    it "SimplexLattice {3, 2}: 6 点、 全行の合計が 1" $
      case Mix.mixtureDesign (Mix.SimplexLattice 2) 3 of
        Left e -> expectationFailure (T.unpack e)
        Right r -> do
          Mix.mdNComponents r `shouldBe` 3
          Mix.mdNRuns r       `shouldBe` 6  -- C(3+2-1, 2) = C(4, 2) = 6
          LA.rows (Mix.mdMatrix r) `shouldBe` 6
          LA.cols (Mix.mdMatrix r) `shouldBe` 3
          let rows = LA.toLists (Mix.mdMatrix r)
          all sumsToOne rows `shouldBe` True
          -- 各点の値は {0, 0.5, 1.0} のみ
          all (\v -> v == 0.0 || v == 0.5 || v == 1.0)
              (LA.toList (LA.flatten (Mix.mdMatrix r))) `shouldBe` True

    it "SimplexLattice {3, 3}: 10 点 (C(5,3)=10)、 値は {0, 1/3, 2/3, 1}" $
      case Mix.mixtureDesign (Mix.SimplexLattice 3) 3 of
        Left e -> expectationFailure (T.unpack e)
        Right r -> do
          Mix.mdNRuns r `shouldBe` 10
          let rows = LA.toLists (Mix.mdMatrix r)
          all sumsToOne rows `shouldBe` True
          -- 各値は {0, 1/3, 2/3, 1} に近い
          let expectedVals = [0.0, 1/3, 2/3, 1.0]
              isClose v = any (\e -> abs (v - e) < 1e-12) expectedVals
          all isClose (LA.toList (LA.flatten (Mix.mdMatrix r))) `shouldBe` True

    it "SimplexCentroid (3): 7 点 (= 2^3 - 1)" $
      case Mix.mixtureDesign Mix.SimplexCentroid 3 of
        Left e -> expectationFailure (T.unpack e)
        Right r -> do
          Mix.mdNRuns r `shouldBe` 7
          let rows = LA.toLists (Mix.mdMatrix r)
          all sumsToOne rows `shouldBe` True
          -- 単体頂点 (1, 0, 0), (0, 1, 0), (0, 0, 1) 全て含む
          [1, 0, 0] `elem` rows `shouldBe` True
          [0, 1, 0] `elem` rows `shouldBe` True
          [0, 0, 1] `elem` rows `shouldBe` True

    it "SimplexCentroid (3) は全体重心 (1/3, 1/3, 1/3) を含む" $
      case Mix.mixtureDesign Mix.SimplexCentroid 3 of
        Left e -> expectationFailure (T.unpack e)
        Right r -> do
          let centroid = [1/3, 1/3, 1/3]
              rows = LA.toLists (Mix.mdMatrix r)
              isClose pt = abs (sum (zipWith (\a b -> abs (a - b)) pt centroid)) < 1e-12
          any isClose rows `shouldBe` True

    it "SimplexCentroid (4): 15 点 (= 2^4 - 1)" $
      case Mix.mixtureDesign Mix.SimplexCentroid 4 of
        Left e -> expectationFailure (T.unpack e)
        Right r -> Mix.mdNRuns r `shouldBe` 15

    it "SimplexLattice {3, 1}: 3 点 (= 単体頂点のみ)" $
      case Mix.mixtureDesign (Mix.SimplexLattice 1) 3 of
        Left e -> expectationFailure (T.unpack e)
        Right r -> do
          Mix.mdNRuns r `shouldBe` 3
          let rows = LA.toLists (Mix.mdMatrix r)
          rows `shouldMatchList` [[1,0,0],[0,1,0],[0,0,1]]
