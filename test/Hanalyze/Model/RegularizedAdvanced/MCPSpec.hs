{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.RegularizedAdvanced.MCPSpec (spec) where

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
  describe "Hanalyze.Model.RegularizedAdvanced.MCP (Phase 31-A2)" $ do
    -- 同 sparse DGP を共有: β_true = [3, 1.5, 0, 0, 2, 0, 0, 0]、 n=200、 p=8
    -- 標準化済 X (各列 N(0,1)) なので γ ≥ 3 で凸性条件満たす
    let mkSparseData :: MWC.GenIO -> IO (LA.Matrix Double, LA.Vector Double, [Double])
        mkSparseData gen = do
          let nMCP = 200
              pMCP = 8
              betaTrue = [3.0, 1.5, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0] :: [Double]
          xVals <- VS.replicateM (nMCP * pMCP) (do
                     u1 <- MWC.uniformR (1e-9, 1.0 :: Double) gen
                     u2 <- MWC.uniformR (0.0, 1.0 :: Double) gen
                     pure (sqrt (-2 * log u1) * cos (2 * pi * u2)))
          noisesMCP <- VS.replicateM nMCP (do
                         u1 <- MWC.uniformR (1e-9, 1.0 :: Double) gen
                         u2 <- MWC.uniformR (0.0, 1.0 :: Double) gen
                         pure (0.1 * sqrt (-2 * log u1) * cos (2 * pi * u2)))
          let xMat = LA.reshape pMCP (LA.fromList (VS.toList xVals))
              bt   = LA.fromList betaTrue
              yVec = xMat LA.#> bt + LA.fromList (VS.toList noisesMCP)
          pure (xMat, yVec, betaTrue)
    it "fitMCP: γ=3、 中 λ で sparse 回復 + zero 係数を Lasso より強く潰す" $ do
      gen <- MWC.create
      (x, y, betaTrue) <- mkSparseData gen
      let fit  = RegA.fitMCP 0.1 3.0 x y 1000 1e-5
          beta = LA.toList (Reg.rfBeta fit)
      length beta `shouldBe` 8
      [beta !! i | i <- [0, 1, 4]] `shouldSatisfy` all (\b -> abs b > 0.5)
      [beta !! i | i <- [2, 3, 5, 6, 7]] `shouldSatisfy` all (\b -> abs b < 0.2)
      -- non-zero は真値の 20% 以内 (MCP は Lasso より bias 小)
      and [ abs (beta !! i - betaTrue !! i) < 0.2 * abs (betaTrue !! i)
          | i <- [0, 1, 4] ] `shouldBe` True
    it "fitMCP: 大 λ で全 β を 0、 小 λ で OLS 近似 (連続スペクトル)" $ do
      gen <- MWC.create
      (x, y, _) <- mkSparseData gen
      let bigFit  = RegA.fitMCP 100.0 3.0 x y 1000 1e-5
          smallFit = RegA.fitMCP 1e-4 3.0 x y 1000 1e-5
      -- 大 λ: 全係数 ≈ 0
      LA.toList (Reg.rfBeta bigFit) `shouldSatisfy` all (\b -> abs b < 0.05)
      -- 小 λ: 真の non-zero 係数を回復
      let smallB = LA.toList (Reg.rfBeta smallFit)
      [smallB !! i | i <- [0, 1, 4]] `shouldSatisfy` all (\b -> abs b > 1.0)
