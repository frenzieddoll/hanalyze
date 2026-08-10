{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.RegularizedAdvanced.AdaptiveLassoSpec (spec) where

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
import qualified Hanalyze.Model.Regularized as Reg
import qualified System.Random.MWC as MWC
import qualified Hanalyze.Model.RegularizedAdvanced   as RegA
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.RegularizedAdvanced.AdaptiveLasso (Phase 31-A1)" $ do
    -- Sparse 合成: β_true = [3, 1.5, 0, 0, 2, 0, 0, 0]、 n=200、 p=8
    -- Y = X β + N(0, 0.1)
    -- Adaptive Lasso (OLS pilot 重み + γ=1) は zero 係数を完全 0 に潰す傾向
    -- が Lasso より強い。
    it "fitAdaptiveLasso: zero 係数を Lasso 同等以上に潰せる + non-zero は回復" $ do
      gen <- MWC.create
      let nA = 200
          pA = 8
          betaTrue = [3.0, 1.5, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0] :: [Double]
      xVals <- VS.replicateM (nA * pA) (do
                 u1 <- MWC.uniformR (1e-9, 1.0 :: Double) gen
                 u2 <- MWC.uniformR (0.0, 1.0 :: Double) gen
                 pure (sqrt (-2 * log u1) * cos (2 * pi * u2)))
      noisesA <- VS.replicateM nA (do
                   u1 <- MWC.uniformR (1e-9, 1.0 :: Double) gen
                   u2 <- MWC.uniformR (0.0, 1.0 :: Double) gen
                   pure (0.1 * sqrt (-2 * log u1) * cos (2 * pi * u2)))
      let xMat = LA.reshape pA (LA.fromList (VS.toList xVals))
          bt   = LA.fromList betaTrue
          yClean = xMat LA.#> bt
          yVec   = yClean + LA.fromList (VS.toList noisesA)
          -- OLS pilot 重み (γ=1)
          w     = RegA.adaptiveWeightsFromOLS 1.0 xMat yVec
          fit   = RegA.fitAdaptiveLasso 0.1 w xMat yVec 1000 1e-5
          beta  = LA.toList (Reg.rfBeta fit)
      -- True zero (index 2,3,5,6,7) はほぼ 0、 non-zero (0,1,4) は |β| > 0.5
      length beta `shouldBe` pA
      [beta !! i | i <- [0, 1, 4]] `shouldSatisfy` all (\b -> abs b > 0.5)
      [beta !! i | i <- [2, 3, 5, 6, 7]] `shouldSatisfy` all (\b -> abs b < 0.3)
      -- 推定 β が真値の 30% 以内 (non-zero に限る)
      let nonZeroOK = and
            [ abs (beta !! i - betaTrue !! i) < 0.3 * abs (betaTrue !! i)
            | i <- [0, 1, 4] ]
      nonZeroOK `shouldBe` True
    it "adaptiveWeightsFromOLS: OLS 推定値が大きい列は重み小、 小は重み大" $ do
      -- 2 column X、 β_true = [5, 0]、 noise 0 → OLS β̂ ≈ [5, 0]
      -- → w ≈ [1/5, 1/1e-8] = [0.2, 1e8] (列 2 を強く罰)
      let x = LA.fromLists [[1, 0], [1, 0], [0, 1], [0, 1]]
          y = LA.fromList [5, 5, 0, 0]
          w = RegA.adaptiveWeightsFromOLS 1.0 x y
          ws = LA.toList w
      ws !! 0 `shouldSatisfy` (\v -> v > 0.15 && v < 0.25)  -- ≈ 1/5
      ws !! 1 `shouldSatisfy` (> 1e6)                       -- floor 1e-8 経由
