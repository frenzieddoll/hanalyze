{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.DiscriminantSpec (spec) where

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
import qualified Numeric.LinearAlgebra as LA
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC as MWC
import qualified Hanalyze.Model.Discriminant as LDA
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.Discriminant (Phase 10 LDA/QDA)" $ do
    -- 2 クラス × 2 特徴量、 明確に分離
    let classData :: IO (LA.Matrix Double, V.Vector Int)
        classData = do
          gen <- MWC.create
          xs0 <- sequence
                   [ sequence [MWC.uniformR (0, 1) gen, MWC.uniformR (0, 1) gen]
                   | _ <- [1..20 :: Int] ]
          xs1 <- sequence
                   [ sequence [MWC.uniformR (3, 4) gen, MWC.uniformR (3, 4) gen]
                   | _ <- [1..20 :: Int] ]
          pure ( LA.fromLists (xs0 ++ xs1)
               , V.fromList (replicate 20 0 ++ replicate 20 1)
               )

    it "fitLDA: 2 クラスで fit、 shape 整合" $ do
      (xs, ys) <- classData
      case LDA.fitLDA xs ys of
        Left e  -> expectationFailure (T.unpack e)
        Right f -> do
          LA.rows (LDA.dfMeans f) `shouldBe` 2
          LA.cols (LDA.dfMeans f) `shouldBe` 2
          LDA.dfMethod f `shouldBe` LDA.LDA
          LA.size (LDA.dfPriors f) `shouldBe` 2

    it "fitLDA + predict: 訓練データで自己判別精度 = 100%" $ do
      (xs, ys) <- classData
      case LDA.fitLDA xs ys of
        Left e  -> expectationFailure (T.unpack e)
        Right f -> do
          let (preds, _) = LDA.predictDiscriminant f xs
              matches = V.length (V.filter id (V.zipWith (==) preds ys))
          matches `shouldBe` V.length ys

    it "fitQDA: 2 クラスで fit、 dfCovariances 長さ = K" $ do
      (xs, ys) <- classData
      case LDA.fitQDA xs ys of
        Left e  -> expectationFailure (T.unpack e)
        Right f -> do
          LDA.dfMethod f `shouldBe` LDA.QDA
          length (LDA.dfCovariances f) `shouldBe` 2

    it "predictDiscriminant: posterior は行ごとに sum = 1" $ do
      (xs, ys) <- classData
      case LDA.fitLDA xs ys of
        Left e -> expectationFailure (T.unpack e)
        Right f -> do
          let (_, posts) = LDA.predictDiscriminant f xs
              rowSums = [LA.sumElements (posts LA.! i) | i <- [0 .. LA.rows posts - 1]]
          all (\s -> abs (s - 1) < 1e-9) rowSums `shouldBe` True

    it "fitLDA: 単一クラスは Left" $ do
      let xs = LA.fromLists [[1,1],[2,2],[3,3]]
          ys = V.fromList [0, 0, 0 :: Int]
      case LDA.fitLDA xs ys of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Left for single class"
