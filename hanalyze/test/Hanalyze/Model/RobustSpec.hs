{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.RobustSpec (spec) where

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
import qualified Data.Vector.Storable              as VS
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC as MWC
import qualified Hanalyze.Model.Robust                as Rob
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.Robust (Phase 31-A5 Huber/Tukey IRLS)" $ do
    -- 合成データ: y = 2 + 1.5·x + ε、 ε ~ N(0, 0.5)
    -- 5% を巨大 outlier に置換 (y += 30)。 OLS は biased、 Huber/Tukey が回復
    let mkOutlierData :: MWC.GenIO -> IO (LA.Matrix Double, LA.Vector Double)
        mkOutlierData gen = do
          let nR = 200
          xs <- VS.replicateM nR (MWC.uniformR (-3.0, 3.0 :: Double) gen)
          noisesR <- VS.replicateM nR (do
                       u1 <- MWC.uniformR (1e-9, 1.0 :: Double) gen
                       u2 <- MWC.uniformR (0.0, 1.0 :: Double) gen
                       pure (0.5 * sqrt (-2 * log u1) * cos (2 * pi * u2)))
          let yClean = VS.zipWith (\x e -> 2.0 + 1.5 * x + e) xs noisesR
              -- 先頭 5% を outlier (y += 30)
              nOut   = nR `div` 20
              yWithOut = VS.imap
                (\i v -> if i < nOut then v + 30.0 else v) yClean
              xMat = LA.fromColumns
                       [ LA.fromList (replicate nR 1.0)
                       , LA.fromList (VS.toList xs) ]
              yVec = LA.fromList (VS.toList yWithOut)
          pure (xMat, yVec)
    it "Huber: 5% outlier 混入で intercept / slope を真値の 15% 以内回復" $ do
      gen <- MWC.create
      (x, y) <- mkOutlierData gen
      let fit  = Rob.fitRobustLM (Rob.Huber Rob.defaultHuberK) x y 50 1e-6
          [b0, b1] = LA.toList (Rob.rfCoef fit)
      abs (b0 - 2.0) `shouldSatisfy` (< 0.15 * 2.0)
      abs (b1 - 1.5) `shouldSatisfy` (< 0.15 * 1.5)
      Rob.rfConverged fit `shouldBe` True
    it "Tukey biweight: 同 outlier で Huber 同等以上の精度 (5% 以内)" $ do
      gen <- MWC.create
      (x, y) <- mkOutlierData gen
      let fit  = Rob.fitRobustLM (Rob.Tukey Rob.defaultTukeyC) x y 50 1e-6
          [b0, b1] = LA.toList (Rob.rfCoef fit)
      abs (b0 - 2.0) `shouldSatisfy` (< 0.05 * 2.0)
      abs (b1 - 1.5) `shouldSatisfy` (< 0.05 * 1.5)
      -- outlier に重み 0 が付く (Tukey の特徴)
      let ws = LA.toList (Rob.rfWeights fit)
          nOut = length ws `div` 20
      take nOut ws `shouldSatisfy` all (< 0.1)
    it "huberWeight / tukeyWeight: 単体関数の境界挙動" $ do
      -- |u| < k で w = 1
      Rob.huberWeight 1.345 0.5 `shouldBe` 1.0
      -- |u| > k で w = k/|u|
      abs (Rob.huberWeight 1.345 3.0 - 1.345 / 3.0) `shouldSatisfy` (< 1e-12)
      -- Tukey: u = 0 で w = 1、 |u| ≥ c で w = 0
      Rob.tukeyWeight 4.685 0.0 `shouldBe` 1.0
      Rob.tukeyWeight 4.685 5.0 `shouldBe` 0.0
      Rob.tukeyWeight 4.685 (-5.0) `shouldBe` 0.0
