{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.VARSpec (spec) where

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
import qualified Hanalyze.Model.VAR            as VAR
import qualified System.Random.MWC.Distributions as MWCD
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC as MWC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.VAR (Phase 35-A2)" $ do
    -- 既知の VAR(1) からシミュレーション。 真の A₁ = [[0.5, 0.1], [0.2, 0.4]]
    let simulateVAR1 gen a11 a12 a21 a22 c1 c2 n = do
          let loop !i !y1Prev !y2Prev acc
                | i >= n = pure (reverse acc)
                | otherwise = do
                    z1 <- MWCD.standard gen
                    z2 <- MWCD.standard gen
                    let !y1 = c1 + a11 * y1Prev + a12 * y2Prev + 0.1 * z1
                        !y2 = c2 + a21 * y1Prev + a22 * y2Prev + 0.1 * z2
                    loop (i + 1) y1 y2 ([y1, y2] : acc)
          rows <- loop 0 0 0 []
          pure (LA.fromLists rows)
    it "fitVAR: 真の係数 (A₁) を概ね回復" $ do
      gen <- MWC.create
      yMat <- simulateVAR1 gen (0.5 :: Double) 0.1 0.2 0.4 0.05 (-0.03) 500
      let fit = VAR.fitVAR 1 yMat
          a1  = head (VAR.varCoefs fit)
      -- 各要素 ±0.05 以内
      abs (LA.atIndex a1 (0, 0) - 0.5) `shouldSatisfy` (< 0.1)
      abs (LA.atIndex a1 (0, 1) - 0.1) `shouldSatisfy` (< 0.1)
      abs (LA.atIndex a1 (1, 0) - 0.2) `shouldSatisfy` (< 0.1)
      abs (LA.atIndex a1 (1, 1) - 0.4) `shouldSatisfy` (< 0.1)
    it "fitVAR: varP / varK / 係数行列の数とサイズ" $ do
      gen <- MWC.create
      yMat <- simulateVAR1 gen (0.5 :: Double) 0.1 0.2 0.4 0 0 300
      let fit = VAR.fitVAR 2 yMat
      VAR.varP fit `shouldBe` 2
      VAR.varK fit `shouldBe` 2
      length (VAR.varCoefs fit) `shouldBe` 2
      LA.size (VAR.varConst fit) `shouldBe` 2
      mapM_ (\m -> LA.size m `shouldBe` (2, 2)) (VAR.varCoefs fit)
    it "fitVAR: 残差行列 = (n - p) × K" $ do
      gen <- MWC.create
      yMat <- simulateVAR1 gen (0.5 :: Double) 0.1 0.2 0.4 0 0 200
      let fit = VAR.fitVAR 3 yMat
      LA.size (VAR.varResiduals fit) `shouldBe` (200 - 3, 2)
    it "forecastVAR: 長さ × 次元" $ do
      gen <- MWC.create
      yMat <- simulateVAR1 gen (0.5 :: Double) 0.1 0.2 0.4 0 0 200
      let fit = VAR.fitVAR 2 yMat
          fc  = VAR.forecastVAR fit yMat 5
      LA.size fc `shouldBe` (5, 2)
    it "forecastVAR: 定常 VAR(1) で長期予測が平均 (≈ (I - A)⁻¹·c) に収束" $ do
      gen <- MWC.create
      yMat <- simulateVAR1 gen (0.5 :: Double) 0.1 0.2 0.4 0.05 (-0.03) 1000
      let fit = VAR.fitVAR 1 yMat
          fc  = VAR.forecastVAR fit yMat 100
          a1  = head (VAR.varCoefs fit)
          iK  = LA.ident 2
          mu  = (iK - a1) LA.<\> VAR.varConst fit
          fLast = LA.flatten (fc LA.? [99])
      abs (LA.atIndex fLast 0 - LA.atIndex mu 0) `shouldSatisfy` (< 0.05)
      abs (LA.atIndex fLast 1 - LA.atIndex mu 1) `shouldSatisfy` (< 0.05)
