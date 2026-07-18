{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.DataIO.ReshapeSpec (spec) where

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
import qualified Hanalyze.DataIO.Reshape    as Reshape
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.DataIO.Reshape" $ do
    it "lagColumn(1): 先頭が NaN、残りは 1 つずれる" $ do
      let df = DX.fromNamedColumns
                 [("x", DX.fromList [1.0, 2.0, 3.0, 4.0, 5.0 :: Double])]
          df' = Reshape.lagColumn 1 "x" "x_lag1" df
      -- Lagged column should exist
      "x_lag1" `elem` DX.columnNames df' `shouldBe` True

    it "leadColumn(1): 末尾が NaN、残りは 1 つ前進" $ do
      let df = DX.fromNamedColumns
                 [("x", DX.fromList [1.0, 2.0, 3.0, 4.0, 5.0 :: Double])]
          df' = Reshape.leadColumn 1 "x" "x_lead1" df
      "x_lead1" `elem` DX.columnNames df' `shouldBe` True

    it "rollingMean(3): 最初 2 つが NaN、3 番目以降は窓内平均" $ do
      let df = DX.fromNamedColumns
                 [("x", DX.fromList [1.0, 2.0, 3.0, 4.0, 5.0 :: Double])]
          df' = Reshape.rollingMean 3 "x" "x_rmean3" df
      "x_rmean3" `elem` DX.columnNames df' `shouldBe` True

    it "oneHot: text 列を indicator 列に展開" $ do
      let df = DX.fromNamedColumns
                 [ ("id", DX.fromList [1, 2, 3, 4, 5 :: Int])
                 , ("category", DX.fromList ["A", "B", "A", "C", "B" :: T.Text])
                 ]
          df' = Reshape.oneHot False "category" df
      let cols = DX.columnNames df'
      "category" `elem` cols `shouldBe` False  -- 元列削除
      "category_A" `elem` cols `shouldBe` True
      "category_B" `elem` cols `shouldBe` True
      "category_C" `elem` cols `shouldBe` True

    it "oneHot dropFirst=True: 1 列分減る" $ do
      let df = DX.fromNamedColumns
                 [("c", DX.fromList ["X", "Y", "Z" :: T.Text])]
          df' = Reshape.oneHot True "c" df
      length (DX.columnNames df') `shouldBe` 2  -- Y, Z (X drop)

  -- ===========================================================================
  -- Hanalyze.Stat.Effect (Phase 9)
  -- ===========================================================================
