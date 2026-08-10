{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.MCMC.BayesianTestSpec (spec) where

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
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC as MWC
import qualified Hanalyze.MCMC.NUTS as NUTS
import qualified Hanalyze.MCMC.BayesianTest as BAB
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.MCMC.BayesianTest (Phase 8)" $ do
    -- HDI helper の単体テスト
    it "highestDensityInterval: 単純な uniform で expected width" $ do
      let xs = [fromIntegral i / 99 | i <- [0..99 :: Int]]  -- 0, 1/99, ..., 1
          (lo, hi) = BAB.highestDensityInterval 0.90 xs
      -- 90% HDI は約 [0, 0.9]、 width ≈ 0.9
      (hi - lo) `shouldSatisfy` (\w -> w >= 0.85 && w <= 0.95)

    it "highestDensityInterval: 0 元素で (0, 0)" $
      BAB.highestDensityInterval 0.95 [] `shouldBe` (0, 0)

    -- classifyROPE の単体テスト (HDI 形状で)
    it "ROPE 完全外: RejectH0" $ do
      let cfg = BAB.defaultBayesianABConfig
            { BAB.babCredible = 0.95
            , BAB.babRule = BAB.ROPEDecision (-0.1) 0.1
            , BAB.babNUTS = (BAB.babNUTS BAB.defaultBayesianABConfig)
                { NUTS.nutsIterations = 300, NUTS.nutsBurnIn = 200 }
            }
          -- 明確に異なる 2 群
          ysA = [10 + 0.1 * fromIntegral i | i <- [1..30 :: Int]]
          ysB = [20 + 0.1 * fromIntegral i | i <- [1..30 :: Int]]
      gen <- MWC.create
      res <- BAB.bayesianAB cfg ysA ysB gen
      BAB.babDecision res `shouldBe` BAB.RejectH0
      BAB.babMeanDiff res `shouldSatisfy` (> 5)  -- 強く正
      BAB.babProbDiffPos res `shouldSatisfy` (> 0.95)

    it "HDIOnly: NoRuleApplied" $ do
      let cfg = BAB.defaultBayesianABConfig
            { BAB.babNUTS = (BAB.babNUTS BAB.defaultBayesianABConfig)
                { NUTS.nutsIterations = 200, NUTS.nutsBurnIn = 100 }
            }
          ysA = [1, 2, 3, 4, 5 :: Double]
          ysB = [3, 4, 5, 6, 7 :: Double]
      gen <- MWC.create
      res <- BAB.bayesianAB cfg ysA ysB gen
      BAB.babDecision res `shouldBe` BAB.NoRuleApplied
      -- HDI は finite + lo ≤ hi
      let (lo, hi) = BAB.babHDI res
      lo `shouldSatisfy` (<= hi)
      -- posterior サンプルが期待数
      length (BAB.babPosteriorDiff res) `shouldBe` 200
