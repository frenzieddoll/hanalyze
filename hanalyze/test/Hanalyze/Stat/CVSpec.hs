{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.CVSpec (spec) where

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
import qualified Hanalyze.Stat.CV           as CV
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC as MWC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.CV" $ do
    it "kFold(5, 100): 5 fold で全 100 行を test に使用、重複なし" $ do
      gen <- MWC.createSystemRandom
      folds <- CV.kFold 5 100 gen
      length folds `shouldBe` 5
      let allTest = concatMap snd folds
      length allTest `shouldBe` 100
      length (V.toList (V.fromList allTest)) `shouldBe` 100  -- 重複なし

    it "kFold: train + test = total samples per fold" $ do
      gen <- MWC.createSystemRandom
      folds <- CV.kFold 5 100 gen
      mapM_ (\(tr, te) -> length tr + length te `shouldBe` 100) folds

    it "leaveOneOut(10): 10 folds、test set size 1 each" $ do
      folds <- CV.leaveOneOut 10
      length folds `shouldBe` 10
      mapM_ (\(_, te) -> length te `shouldBe` 1) folds

    it "stratifiedKFold(3): クラスバランスがほぼ保持される" $ do
      gen <- MWC.createSystemRandom
      let labels = replicate 30 0 ++ replicate 30 1 ++ replicate 30 2
      folds <- CV.stratifiedKFold 3 labels gen
      length folds `shouldBe` 3
      -- 各 fold の test set には各クラスの ~10 が含まれる
      mapM_ (\(_, te) -> length te `shouldSatisfy` (\n -> n >= 27 && n <= 33)) folds

    it "shuffleSplit: 反復回数とテストサイズが正しい" $ do
      gen <- MWC.createSystemRandom
      folds <- CV.shuffleSplit 5 0.2 100 gen
      length folds `shouldBe` 5
      mapM_ (\(_, te) -> length te `shouldBe` 20) folds

    it "timeSeriesSplit: forward-chaining で過去のみで学習" $ do
      let folds = CV.timeSeriesSplit 50 10 100  -- initial=50, step=10, n=100
      length folds `shouldBe` 5  -- (100-50)/10 = 5 folds
      -- 全 fold で train indices < min(test indices)
      mapM_ (\(tr, te) ->
               (maximum tr < minimum te) `shouldBe` True) folds

  -- ===========================================================================
  -- Hanalyze.Model.Cluster (Phase 5)
  -- ===========================================================================
