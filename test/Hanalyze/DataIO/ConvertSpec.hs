{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.DataIO.ConvertSpec (spec) where

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
import qualified Hanalyze.DataIO.CSV        as CSV
import qualified Hanalyze.DataIO.Convert    as Conv
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.DataIO.Convert deep-eval" $ do
    it "getMaybeTextVec on text column with mixed NA strings returns Just" $
      withSystemTempFile "ha-na.csv" $ \fp h -> do
        -- 複数 NA 表現を混ぜると Hackage は Maybe Text 列として保持する。
        -- ヘッダ判定で n/a / null は欠損扱い → null bitmap が立つ。
        hPutStr h "id,score\n1,A\n2,n/a\n3,null\n4,B\n5,-\n"
        hClose h
        r <- CSV.loadAutoSafe fp
        case r of
          Right (df, _) -> case Conv.getMaybeTextVec "score" df of
            Just v  -> length (V.toList v) `shouldBe` 5
            Nothing -> expectationFailure "getMaybeTextVec returned Nothing"
          Left msg -> expectationFailure ("load failed: " ++ msg)
    it "getDoubleVec returns Nothing without crashing on NA-mixed numeric column" $
      withSystemTempFile "ha-na2.csv" $ \fp h -> do
        hPutStr h "id,score\n1,85\n2,NA\n3,92\n"
        hClose h
        r <- CSV.loadAutoSafe fp
        case r of
          Right (df, _) -> Conv.getDoubleVec "score" df `shouldBe` Nothing
          Left msg      -> expectationFailure ("load failed: " ++ msg)
