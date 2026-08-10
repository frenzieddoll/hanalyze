{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.GLMSpec (spec) where

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
import qualified Hanalyze.Model.Core        as Core
import qualified Hanalyze.Model.GLM         as GLM
import qualified Hanalyze.MCMC.Core as Core
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.GLM IRLS 収束 (28d1feb7 回帰の防止)" $ do
    -- ★かつて runIRLS の converge が初回 dLL=0 (seed llP=ll(β0) と llHere=ll(β0) が一致)
    -- で IRLS を 1 ステップで早期停止し、 μ̂ が系統的に過大だった (28d1feb7 perf リライトの
    -- 回帰)。 真の MLE はスコア方程式 Xᵀ(y-μ̂)=0 を満たすので、 それを外部非依存オラクル
    -- として検証する (statsmodels の β とも突合)。
    let xsP   = [1,2,3,4,5,6,7,8] :: [Double]
        ysP   = [1,1,3,4,7,10,15,22] :: [Double]
        xMatP = LA.matrix 2 (concatMap (\x -> [1, x]) xsP)
        yVecP = LA.fromList ysP
        (frP, _) = GLM.fitGLMFull GLM.Poisson GLM.Log xMatP yVecP
        betaP = LA.toList (head (LA.toColumns (Core.coefficients frP)))
        muP   = head (LA.toColumns (Core.fitted frP))

    it "Poisson/Log: スコア方程式 Xᵀ(y-μ̂)=0 (真の MLE に収束)" $ do
      let score = LA.tr xMatP LA.#> (yVecP - muP)
      LA.norm_Inf score `shouldSatisfy` (< 1e-6)

    it "Poisson/Log: 係数が statsmodels GLM と一致 (-0.33398, 0.43288)" $ do
      abs (betaP !! 0 - (-0.33397763)) `shouldSatisfy` (< 1e-4)
      abs (betaP !! 1 -   0.43287516)  `shouldSatisfy` (< 1e-4)

    it "Binomial/Logit: スコア方程式 Xᵀ(y-μ̂)=0 (分離しない data で MLE)" $ do
      let xsB   = [0,1,2,3,4,5,6,7,8,9] :: [Double]
          ysB   = [0,0,0,0,1,0,1,1,1,1] :: [Double]
          xMatB = LA.matrix 2 (concatMap (\x -> [1, x]) xsB)
          yVecB = LA.fromList ysB
          (frB, _) = GLM.fitGLMFull GLM.Binomial GLM.Logit xMatB yVecB
          muB   = head (LA.toColumns (Core.fitted frB))
          score = LA.tr xMatB LA.#> (yVecB - muB)
      LA.norm_Inf score `shouldSatisfy` (< 1e-6)

  describe "Hanalyze.Model.GLM diagnostics (request/090-AB)" $ do
    let xsG = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] :: [Double]
        ysG = [1.1, 2.9, 5.2, 6.8, 9.1, 11.0, 13.2, 14.8, 17.0, 19.1] :: [Double]
        xMatG = LA.matrix 2 (concatMap (\x -> [1, x]) xsG)
        yVecG = LA.fromList ysG
        (frG, sigmaG) = GLM.fitGLMFull GLM.Gaussian GLM.Identity xMatG yVecG
        betaG = head (LA.toColumns (Core.coefficients frG))
        muVG  = head (LA.toColumns (Core.fitted frG))

    it "glmPearsonResiduals (Gaussian, V=1) == raw residuals" $ do
      let pr = GLM.glmPearsonResiduals GLM.Gaussian yVecG muVG
          rr = yVecG - muVG
      LA.norm_2 (pr - rr) `shouldSatisfy` (< 1e-9)

    it "glmDevianceResiduals (Gaussian) == sign(y-μ)·|y-μ|" $ do
      let dr  = GLM.glmDevianceResiduals GLM.Gaussian yVecG muVG
          ref = LA.fromList
                  [ signum (y - m) * abs (y - m)
                  | (y, m) <- zip ysG (LA.toList muVG) ]
      LA.norm_2 (dr - ref) `shouldSatisfy` (< 1e-9)

    it "glmVariance: Binomial μ(1-μ); Poisson μ" $ do
      GLM.glmVariance GLM.Binomial 0.3 `shouldBe` 0.3 * 0.7
      GLM.glmVariance GLM.Poisson  4.0 `shouldBe` 4.0

    it "predictGlmEtaWithSE: η = xᵀβ, SE > 0" $ do
      let xNew = LA.fromList [1, 5.0]
          (eta, se) = GLM.predictGlmEtaWithSE betaG sigmaG xNew
      eta `shouldSatisfy` (\e -> abs (e - (1 + 2 * 5.0)) < 0.5)
      se  `shouldSatisfy` (> 0)
      se  `shouldSatisfy` (< 1)

    it "predictGlmMuWithCI (Identity): half-width ≈ 1.96·SE" $ do
      let xNew    = LA.fromList [1, 4.5]
          ci      = GLM.predictGlmMuWithCI GLM.Identity 0.95 betaG sigmaG xNew
          (_, se) = GLM.predictGlmEtaWithSE betaG sigmaG xNew
          halfW   = (GLM.gpHi ci - GLM.gpLo ci) / 2
      abs (halfW - 1.96 * se) `shouldSatisfy` (< 1e-2)

    it "predictGlmMuWithCI (Logit): CI stays in (0,1)" $ do
      let xs2  = LA.matrix 2 (concatMap (\i -> [1, fromIntegral (i :: Int)]) [0..9])
          ys2  = LA.fromList [0,1,0,1,0,1,0,1,0,1]
          (_, sigma2) = GLM.fitGLMFull GLM.Binomial GLM.Logit xs2 ys2
          beta2 = LA.fromList [0.0, 0.05]
          xNew  = LA.fromList [1, 5.0]
          ci    = GLM.predictGlmMuWithCI GLM.Logit 0.95 beta2 sigma2 xNew
      GLM.gpMu ci `shouldSatisfy` (\v -> v > 0 && v < 1)
      GLM.gpLo ci `shouldSatisfy` (\v -> v >= 0)
      GLM.gpHi ci `shouldSatisfy` (\v -> v <= 1)
      GLM.gpLo ci `shouldSatisfy` (< GLM.gpHi ci)
