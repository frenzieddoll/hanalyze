{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.GARCHSpec (spec) where

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
import qualified Hanalyze.Model.GARCH          as GARCH
import qualified System.Random.MWC.Distributions as MWCD
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC as MWC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.GARCH (Phase 35-A1)" $ do
    -- GARCH(1,1) からのサンプル生成: y_t = σ_t · z_t, z_t ~ N(0,1)
    -- σ²_t = ω + α ε²_{t-1} + β σ²_{t-1}
    let simulateGARCH gen omega alpha beta n = do
          let unc = omega / (1 - alpha - beta)
              loop !i !s2Prev !ePrev acc
                | i >= n = pure (reverse acc)
                | otherwise = do
                    z <- MWCD.standard gen
                    let !s2 = if i == 0 then unc
                              else omega + alpha * ePrev * ePrev + beta * s2Prev
                        !sigma = sqrt s2
                        !e     = sigma * z
                    loop (i + 1) s2 e (e : acc)
          es <- loop 0 0 0 []
          pure (LA.fromList es)
    it "fitGARCH: ω/α/β > 0、 α + β < 1" $ do
      gen <- MWC.create
      ys <- simulateGARCH gen (0.05 :: Double) 0.10 0.85 1000
      let fit = GARCH.fitGARCH ys
      GARCH.gOmega fit `shouldSatisfy` (> 0)
      GARCH.gAlpha fit `shouldSatisfy` (>= 0)
      GARCH.gBeta  fit `shouldSatisfy` (>= 0)
      (GARCH.gAlpha fit + GARCH.gBeta fit) `shouldSatisfy` (< 1)
    it "fitGARCH: 真値 (ω=0.05, α=0.10, β=0.85) を概ね回復" $ do
      gen <- MWC.create
      ys <- simulateGARCH gen (0.05 :: Double) 0.10 0.85 2000
      let fit = GARCH.fitGARCH ys
          ab  = GARCH.gAlpha fit + GARCH.gBeta fit
      -- α+β (persistence) は推定が安定しやすい
      ab `shouldSatisfy` (> 0.80)
      ab `shouldSatisfy` (< 1.00)
      -- 無条件分散 ω/(1-α-β) はサンプル分散に近い
      let n     = LA.size ys
          var_y = LA.dot ys ys / fromIntegral n
          uncV  = GARCH.gOmega fit / (1 - ab)
      abs (uncV - var_y) / var_y `shouldSatisfy` (< 0.5)
    it "fitGARCH: gSigma2 の長さ = 入力長" $ do
      gen <- MWC.create
      ys <- simulateGARCH gen (0.1 :: Double) 0.05 0.90 500
      let fit = GARCH.fitGARCH ys
      LA.size (GARCH.gSigma2 fit) `shouldBe` 500
    it "forecastGARCH: 長期予測が無条件分散 ω/(1-α-β) に収束" $ do
      gen <- MWC.create
      ys <- simulateGARCH gen (0.05 :: Double) 0.10 0.85 1000
      let fit = GARCH.fitGARCH ys
          fc  = GARCH.forecastGARCH fit 200
          ab  = GARCH.gAlpha fit + GARCH.gBeta fit
          unc = GARCH.gOmega fit / (1 - ab)
          fLast = LA.atIndex fc 199
      abs (fLast - unc) / unc `shouldSatisfy` (< 0.05)
    it "forecastGARCH: 長さ = h" $ do
      gen <- MWC.create
      ys <- simulateGARCH gen (0.05 :: Double) 0.10 0.85 200
      let fit = GARCH.fitGARCH ys
          fc  = GARCH.forecastGARCH fit 12
      LA.size fc `shouldBe` 12
