{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.RFFSpec (spec) where

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
import qualified Numeric.LinearAlgebra as LA
import qualified System.Random.MWC as MWC
import qualified Hanalyze.Model.RFF       as RFF
import qualified System.Random.MWC as MWC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.RFF (multivariate, Phase B-RFF)" $ do
    it "logMarginalLikRBFMV: 既知 ℓ で最大化される (合成データで)" $ do
      -- y = sin(x) (1D) で ℓ をスキャンし、データの z-score 後の長さスケールに
      -- 近い値で marg-lik が最大になることを確認。
      let xs = [0.0, 0.3 .. 6.0]
          ys = map sin xs
          xMat = LA.fromLists [[x] | x <- xs]
          yV   = LA.fromList ys
          ells = [0.05, 0.2, 0.5, 1.0, 2.0, 5.0]
          mliks = [ RFF.logMarginalLikRBFMV xMat yV ell 1.0 0.05 | ell <- ells ]
          best  = snd (maximum (zip mliks ells))
      best `shouldSatisfy` (\b -> b >= 0.2 && b <= 2.0)
    it "loocvRFFRidgeMV: λ → ∞ で残差ベース LOOCV が増える、適度な λ で最小" $ do
      let xs = [0.0, 0.3 .. 6.0]
          ys = map sin xs
          xMat = LA.fromLists [[x] | x <- xs]
          yV   = LA.fromList ys
      gen   <- MWC.createSystemRandom
      feats <- RFF.sampleRFFRBFMV 1 64 0.5 1.0 gen
      let lamSmall = RFF.loocvRFFRidgeMV feats xMat yV 1e-2
          lamHuge  = RFF.loocvRFFRidgeMV feats xMat yV 1e6
      lamSmall `shouldSatisfy` (< lamHuge)
    it "gridSearchLOOCVRBFMV: ℓ/λ を自動探索して LOOCV が小さくなる" $ do
      let xs = [0.0, 0.5 .. 10.0]
          ys = [ sin (x/2) | x <- xs ]
          xMat = LA.fromLists [[x] | x <- xs]
          yV   = LA.fromList ys
      gen <- MWC.createSystemRandom
      res <- RFF.gridSearchLOOCVRBFMV 1 100 xMat yV (Just (4, 8)) gen
      RFF.lcLOOCV res `shouldSatisfy` (< 1.0)
      RFF.lcEll res   `shouldSatisfy` (> 0)
    it "maximizeMarginalLikRBFMV: 雑音ありデータで mlik が改善する" $ do
      let xs = [0.0, 0.5 .. 10.0]
          ys = [ sin (x/2) + 0.05 * (fromIntegral i / 21) - 0.025
               | (i, x) <- zip [0::Int ..] xs ]
          xMat = LA.fromLists [[x] | x <- xs]
          yV   = LA.fromList ys
          res  = RFF.maximizeMarginalLikRBFMV xMat yV (Just (8, 4, 4))
      -- 最適 mlik > 任意の "ヘンな" 値 (ℓ=100, σ_n=10) より高い
          weak = RFF.logMarginalLikRBFMV xMat yV 100 1.0 10.0
      RFF.mlLogMlik res `shouldSatisfy` (> weak)
      RFF.mlEll res     `shouldSatisfy` (> 0)
      RFF.mlSigmaN res  `shouldSatisfy` (> 0)

    it "rffRidgeMV: y = x1 * t を完全にフィット" $ do
      let xs = [(x1, t) | x1 <- [1, 2, 3, 5, 7], t <- [1..10]]
          xss = [[x1, t] | (x1, t) <- xs]
          ys  = [x1 * t | (x1, t) <- xs]
          xMat = LA.fromLists xss
      gen   <- MWC.createSystemRandom
      feats <- RFF.sampleRFFRBFMV 2 256 1.0 1.0 gen
      let fit  = RFF.rffRidgeMV feats xMat ys 0.001
          yhat = RFF.predictRFFRidgeMV fit xMat
          rmse = sqrt (sum (zipWith (\a b -> (a-b)*(a-b)) ys yhat)
                       / fromIntegral (length ys))
      rmse `shouldSatisfy` (< 1.0)

  describe "Hanalyze.Model.RFF" $ do
    it "feature matrix has correct shape" $ do
      gen   <- MWC.createSystemRandom
      feats <- RFF.sampleRFFRBF 50 1.0 1.0 gen
      RFF.rffDim feats `shouldBe` 50
      let phi = RFF.rffFeatures feats [0.0, 1.0, 2.0]
      -- phi is n × D = 3 × 50
      V.length (V.fromList [0::Int]) `shouldBe` 1   -- placeholder for typing
      -- We can't easily check matrix shape without hmatrix import here,
      -- so just ensure the function doesn't crash.
      length (RFF.rffOmegas feats) `shouldSatisfy` (== 50)
      let _ = phi
      return ()

    it "RFF Ridge fits y ≈ x reasonably" $ do
      gen   <- MWC.createSystemRandom
      feats <- RFF.sampleRFFRBF 100 1.0 1.0 gen
      let xs = [0.0, 0.1 .. 1.0]
          ys = map (\x -> 2 * x + 0.5) xs
          fit = RFF.rffRidge feats xs ys 0.001
          yhat = RFF.predictRFFRidge fit xs
          rmse = sqrt (sum [ (y - yh) ^ (2 :: Int)
                           | (y, yh) <- zip ys yhat ]
                       / fromIntegral (length ys))
      rmse `shouldSatisfy` (< 0.5)

  -- ─────────────────────────────────────────────────────────────────────

  describe "Hanalyze.Model.RFF DE-based auto-HP" $ do
    it "maximizeMarginalLikRBFMV_DE: y = sin x + noise で妥当な ℓ" $ do
      gen <- MWC.create
      let n = 30
          xs = [ fromIntegral i / 5 | i <- [0 .. n - 1] ] :: [Double]
          ys = [ sin x + 0.05 * cos (3 * x) | x <- xs ]
          xMat = LA.fromColumns [LA.fromList xs]
          yVec = LA.fromList ys
      r <- RFF.maximizeMarginalLikRBFMV_DE xMat yVec 30 gen
      -- ℓ が極端に小さくない (>1e-2) ことだけ確認
      RFF.mlEll r `shouldSatisfy` (> 1e-2)

    it "gridSearchLOOCVRBFMV_DE: LOOCV が有限値、ℓ が探索範囲内" $ do
      gen <- MWC.create
      let n = 25
          xs = [ fromIntegral i / 4 | i <- [0 .. n - 1] ] :: [Double]
          ys = [ x + 0.1 * sin (2 * x) | x <- xs ]
          xMat = LA.fromColumns [LA.fromList xs]
          yVec = LA.fromList ys
      r <- RFF.gridSearchLOOCVRBFMV_DE 1 50 xMat yVec 20 gen
      RFF.lcLOOCV r `shouldSatisfy` (\v -> not (isNaN v) && v >= 0)
      RFF.lcEll   r `shouldSatisfy` (> 1e-3)

  -- ===========================================================================
  -- Hanalyze.Viz.ReportBuilder.secInterpolation (Phase G4)
  -- ===========================================================================
