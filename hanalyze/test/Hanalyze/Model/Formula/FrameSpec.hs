{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.Formula.FrameSpec (spec) where

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
import qualified DataFrame.Internal.Column    as DX
import qualified DataFrame.Internal.DataFrame  as DX
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.Formula.Frame (A16 ModelFrame)" $ do
    let dfLM = DX.fromNamedColumns
          [ ("x",     DX.fromList ([1,2,3,4, 1,2,3,4, 1,2,3,4] :: [Double]))
          , ("y",     DX.fromList ([7,6,7,7, 5,4,5,5, 3,2,3,3]  :: [Double]))
          , ("group", DX.fromList (["A","A","A","A","B","B","B","B","C","C","C","C"] :: [T.Text])) ]
        Right f1 = parseFormula "y x group = b0 + b1*x + bg ! group"
        Right mf1 = modelFrame f1 dfLM

    it "連続 + factor: パラメータ分離" $
      mfParams mf1 `shouldBe` ["b0", "b1", "bg"]
    it "行数 = 応答列長" $
      mfNRows mf1 `shouldBe` 12
    it "factor は使われ方 (! の右) で検出・水準は昇順・index 付き" $
      lookup "group" (mfRoles mf1)
        `shouldBe` Just (RoleFactor ["A", "B", "C"]
                          (V.fromList [0,0,0,0, 1,1,1,1, 2,2,2,2]))
    it "算術中の変数は連続" $
      lookup "x" (mfRoles mf1)
        `shouldBe` Just (RoleContinuous (V.fromList [1,2,3,4, 1,2,3,4, 1,2,3,4]))

    describe "factor×factor (検証点① の前段)" $ do
      let dfGT = DX.fromNamedColumns
            [ ("y", DX.fromList ([1,2,3,4,5,6] :: [Double]))
            , ("g", DX.fromList (["A","A","B","B","C","C"] :: [T.Text]))
            , ("t", DX.fromList (["P","Q","P","Q","P","Q"] :: [T.Text])) ]
          Right fGT  = parseFormula "y g t = b0 + bg!g + bt!t + bgt!g!t"
          Right mfGT = modelFrame fGT dfGT
      it "g, t とも factor・パラメータは b0/bg/bt/bgt" $ do
        mfParams mfGT `shouldBe` ["b0", "bg", "bt", "bgt"]
        indexedVars (formRHS fGT) `shouldBe` ["g", "t"]

    describe "error" $ do
      it "応答列が無ければ Left" $
        modelFrame f1 (DX.fromNamedColumns
          [ ("x", DX.fromList ([1,2] :: [Double])) ]) `shouldSatisfy` isLeftE
      it "連続変数が無ければ Left" $ do
        let Right fNo = parseFormula "y q = b0 + b1*q"
        modelFrame fNo dfLM `shouldSatisfy` isLeftE

    describe "MissingPolicy (Phase 47 A1)" $ do
      let dfNA = DX.fromNamedColumns
            [ ("x", DX.fromList ([Just 1, Just 2, Nothing, Just 4] :: [Maybe Double]))
            , ("y", DX.fromList ([10,20,30,40] :: [Double]))
            , ("g", DX.fromList (["A","B","A","B"] :: [T.Text])) ]
          Right fNA = parseFormula "y x g = b0 + b1*x + bg ! g"
          contOf mf = case lookup "x" (mfRoles mf) of
                        Just (RoleContinuous v) -> V.toList v
                        _                       -> []

      it "DropRows: NA 行を除外" $ do
        let Right mf = modelFrameWith DropRows fNA dfNA
        mfNRows mf `shouldBe` 3
        contOf mf `shouldBe` [1,2,4]
      it "modelFrame は DropRows 既定 (後方互換)" $
        modelFrame fNA dfNA `shouldBe` modelFrameWith DropRows fNA dfNA
      it "ErrorOnMissing: NA があれば Left" $
        modelFrameWith ErrorOnMissing fNA dfNA `shouldSatisfy` isLeftE
      it "Impute ImputeMean: 連続説明変数を平均補完 (行数不変)" $ do
        let Right mf = modelFrameWith (Impute ImputeMean) fNA dfNA
        mfNRows mf `shouldBe` 4
        (contOf mf !! 2) `shouldSatisfy` (\v -> abs (v - 7/3) < 1e-9)  -- mean [1,2,4]
      it "Impute: 応答の欠損は Left に誘導" $ do
        let dfYNA = DX.fromNamedColumns
              [ ("x", DX.fromList ([1,2,3,4] :: [Double]))
              , ("y", DX.fromList ([Just 10, Nothing, Just 30, Just 40] :: [Maybe Double]))
              , ("g", DX.fromList (["A","B","A","B"] :: [T.Text])) ]
        modelFrameWith (Impute ImputeMean) fNA dfYNA `shouldSatisfy` isLeftE

      describe "TreatAsCategory" $ do
        let dfFNA = DX.fromNamedColumns
              [ ("y", DX.fromList ([10,20,30,40] :: [Double]))
              , ("x", DX.fromList ([1,2,3,4]    :: [Double]))
              , ("g", DX.fromList (["A","B","NA","B"] :: [T.Text])) ]  -- "NA" = 欠損文字列
        it "factor の NA を独立水準 <NA> として保持 (行数不変・水準は昇順)" $ do
          let Right mf = modelFrameWith TreatAsCategory fNA dfFNA
          mfNRows mf `shouldBe` 4
          lookup "g" (mfRoles mf)
            `shouldBe` Just (RoleFactor ["<NA>","A","B"] (V.fromList [1,2,0,2]))
        it "連続列の欠損は TreatAsCategory では埋めず Left に誘導" $ do
          let dfXFNA = DX.fromNamedColumns
                [ ("y", DX.fromList ([10,20,30,40] :: [Double]))
                , ("x", DX.fromList ([Just 1, Nothing, Just 3, Just 4] :: [Maybe Double]))
                , ("g", DX.fromList (["A","B","A","B"] :: [T.Text])) ]
          modelFrameWith TreatAsCategory fNA dfXFNA `shouldSatisfy` isLeftE

  -- ----------------------------------------------------------------------------
  -- A17 designMatrixF / fitLMF — 基底展開 (factor) + 線形性検出 + 識別性
  -- ----------------------------------------------------------------------------
