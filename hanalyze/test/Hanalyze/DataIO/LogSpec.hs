{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.DataIO.LogSpec (spec) where

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
import qualified Hanalyze.DataIO.Log        as Log
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.DataIO.Log" $ do
    it "Monoid: noLog <> r == r" $ do
      let r = Log.logReport (Log.mkWarn "W001" "msg" Nothing)
      Log.entries (Log.noLog <> r) `shouldBe` Log.entries r
      Log.entries (r <> Log.noLog) `shouldBe` Log.entries r
    it "addEntry appends" $ do
      let r0 = Log.logReport (Log.mkInfo "I001" "first" Nothing)
          r1 = Log.addEntry (Log.mkWarn "W001" "second" (Just "ヒント")) r0
      length (Log.entries r1) `shouldBe` 2
      Log.lgSev  (last (Log.entries r1)) `shouldBe` Log.Warn
      Log.lgHint (last (Log.entries r1)) `shouldBe` Just "ヒント"
    it "hasErrors / hasWarnings detect severity" $ do
      let rW = Log.logReport (Log.mkWarn "W"  "w"  Nothing)
          rE = Log.logReport (Log.mkErr  "E"  "e"  Nothing)
      Log.hasWarnings rW         `shouldBe` True
      Log.hasErrors   rW         `shouldBe` False
      Log.hasErrors   (rW <> rE) `shouldBe` True
    it "severityCount counts each level" $ do
      let r = Log.logReport (Log.mkInfo "I" "i" Nothing)
            <> Log.logReport (Log.mkWarn "W1" "w" Nothing)
            <> Log.logReport (Log.mkWarn "W2" "w" Nothing)
            <> Log.logReport (Log.mkErr  "E"  "e" Nothing)
      Log.severityCount Log.Info r `shouldBe` 1
      Log.severityCount Log.Warn r `shouldBe` 2
      Log.severityCount Log.Err  r `shouldBe` 1
    it "prettyEntry: includes code, message, hint" $ do
      let s = Log.prettyEntry (Log.mkWarn "W042" "壊れている" (Just "助言"))
      T.isInfixOf "[WARN]" s   `shouldBe` True
      T.isInfixOf "W042"   s   `shouldBe` True
      T.isInfixOf "壊れている" s `shouldBe` True
      T.isInfixOf "助言"   s   `shouldBe` True
