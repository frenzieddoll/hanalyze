{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.DataIO.PreprocessSpec (spec) where

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
import qualified DataFrame.Internal.Column    as DX
import qualified DataFrame.Internal.DataFrame  as DX
import qualified DataFrame.Operations.Core     as DX
import qualified DataFrame.Operators           as DX
import qualified Hanalyze.DataIO.Preprocess as Pp
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.DataIO.Preprocess" $ do
    let dfNA = DX.fromNamedColumns
                 [ ("group", DX.fromList (["A","B","A","B","C"] :: [T.Text]))
                 , ("x",     DX.fromList (["1","NA","3","","5"]   :: [T.Text]))
                 , ("y",     DX.fromList ([10, 20, 30, 40, 50]    :: [Double]))
                 ]

    it "isNAString detects standard NA strings" $ do
      Pp.isNAString "NA"    `shouldBe` True
      Pp.isNAString "N/A"   `shouldBe` True
      Pp.isNAString "null"  `shouldBe` True
      Pp.isNAString ""      `shouldBe` True
      Pp.isNAString "  "    `shouldBe` True
      Pp.isNAString "valid" `shouldBe` False

    it "countMissing counts NAs in Text columns; numeric is 0" $ do
      let counts = Pp.countMissing dfNA
      lookup "x"     counts `shouldBe` Just 2
      lookup "y"     counts `shouldBe` Just 0
      lookup "group" counts `shouldBe` Just 0

    it "dropMissingRows removes rows with NA in target columns" $ do
      let df' = Pp.dropMissingRows ["x"] dfNA
          (n, _) = DX.dimensions df'
      n `shouldBe` 3   -- only rows with x ∈ {"1","3","5"} remain

    it "imputeMean converts Text/NA column to Double with mean fill" $ do
      case Pp.imputeMean "x" dfNA of
        Just df' -> do
          let xs = DX.columnAsList (DX.col @Double "x") df'
          length xs `shouldBe` 5
          -- mean of [1, 3, 5] = 3
          (xs !! 1) `shouldBe` 3.0   -- was "NA"
          (xs !! 3) `shouldBe` 3.0   -- was ""
        Nothing -> expectationFailure "imputeMean failed"

    it "selectColumns retains only listed columns" $ do
      let df' = Pp.selectColumns ["y", "group"] dfNA
      DX.columnNames df' `shouldMatchList` ["y", "group"]

    it "filterRowsByNumeric filters numeric column" $ do
      let df' = Pp.filterRowsByNumeric "y" (>= 30) dfNA
          (n, _) = DX.dimensions df'
      n `shouldBe` 3

    it "mapNumeric applies a unary function" $ do
      let df' = Pp.mapNumeric "y" (* 2) dfNA
          xs = DX.columnAsList (DX.col @Double "y") df'
      xs `shouldBe` [20, 40, 60, 80, 100]

  -- ─────────────────────────────────────────────────────────────────────

  describe "Hanalyze.DataIO.Preprocess (groupBy)" $ do
    let dfGrp = DX.fromNamedColumns
                  [ ("group", DX.fromList (["A","B","A","B","A","C"] :: [T.Text]))
                  , ("y",     DX.fromList ([1, 4, 3, 6, 5, 10]       :: [Double]))
                  ]

    it "groupByMean computes per-group mean" $ do
      case Pp.groupByMean "group" "y" dfGrp of
        Just df' -> do
          let (n, _) = DX.dimensions df'
          n `shouldBe` 3
          let gs = DX.columnAsList (DX.col @T.Text "group") df'
              vs = DX.columnAsList (DX.col @Double "y")    df'
              pairs = zip gs vs
          lookup "A" pairs `shouldBe` Just 3.0       -- (1+3+5)/3
          lookup "B" pairs `shouldBe` Just 5.0       -- (4+6)/2
          lookup "C" pairs `shouldBe` Just 10.0
        Nothing -> expectationFailure "groupByMean failed"

    it "groupBySum computes per-group sum" $ do
      case Pp.groupBySum "group" "y" dfGrp of
        Just df' -> do
          let gs = DX.columnAsList (DX.col @T.Text "group") df'
              vs = DX.columnAsList (DX.col @Double "y")    df'
              pairs = zip gs vs
          lookup "A" pairs `shouldBe` Just 9.0
          lookup "B" pairs `shouldBe` Just 10.0
        Nothing -> expectationFailure "groupBySum failed"

    it "groupByCount counts rows per group" $ do
      case Pp.groupByCount "group" dfGrp of
        Just df' -> do
          let gs = DX.columnAsList (DX.col @T.Text "group") df'
              vs = DX.columnAsList (DX.col @Double "count") df'
              pairs = zip gs vs
          lookup "A" pairs `shouldBe` Just 3.0
          lookup "B" pairs `shouldBe` Just 2.0
          lookup "C" pairs `shouldBe` Just 1.0
        Nothing -> expectationFailure "groupByCount failed"

    it "groupByMin/Max return correct extremes" $ do
      case Pp.groupByMin "group" "y" dfGrp of
        Just dfMin -> do
          let gs = DX.columnAsList (DX.col @T.Text "group") dfMin
              vs = DX.columnAsList (DX.col @Double "y")    dfMin
              pairs = zip gs vs
          lookup "A" pairs `shouldBe` Just 1.0
          lookup "B" pairs `shouldBe` Just 4.0
        Nothing -> expectationFailure "groupByMin failed"

      case Pp.groupByMax "group" "y" dfGrp of
        Just dfMax -> do
          let gs = DX.columnAsList (DX.col @T.Text "group") dfMax
              vs = DX.columnAsList (DX.col @Double "y")    dfMax
              pairs = zip gs vs
          lookup "A" pairs `shouldBe` Just 5.0
          lookup "B" pairs `shouldBe` Just 6.0
        Nothing -> expectationFailure "groupByMax failed"

  -- ─────────────────────────────────────────────────────────────────────
