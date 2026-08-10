{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.BayesFactorSpec (spec) where

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
import qualified Data.Map.Strict as M
import qualified System.Random.MWC as MWC
import qualified Data.ByteString   as BS
import qualified System.Random.MWC as MWC
import qualified Hanalyze.MCMC.NUTS as NUTS
import qualified Hanalyze.Stat.BridgeSampling as BS
import qualified Hanalyze.Stat.BayesFactor    as BF
import qualified Hanalyze.Model.HBM as HBM
import qualified Data.Map.Strict    as M
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.BayesFactor (Phase 29-A3 via Bridge Sampling)" $ do
    -- M_0: μ ~ N(0, 1)    (= 強い prior、 データ y=5 から離れている)
    -- M_1: μ ~ N(0, 10)   (= 弱い prior、 データに近い μ が許容される)
    -- データ y_i = 5 (n=10)
    -- → M_1 が事実上 favored、 BF_{10} > 1 が期待
    let m0 :: HBM.ModelP ()
        m0 = do
          mu <- HBM.sample "mu" (HBM.Normal 0 1)
          HBM.observe "y" (HBM.Normal mu 1) (replicate 10 5.0)
        m1 :: HBM.ModelP ()
        m1 = do
          mu <- HBM.sample "mu" (HBM.Normal 0 10)
          HBM.observe "y" (HBM.Normal mu 1) (replicate 10 5.0)
        nutsCfg = NUTS.defaultNUTSConfig
          { NUTS.nutsIterations = 2000
          , NUTS.nutsBurnIn     = 500
          , NUTS.nutsAdaptStepSize = True
          }
    it "Bayes Factor: BF_{10} > 1 (= log_e BF > 0)、 M_1 が favored" $ do
      gen0 <- MWC.create
      ch0 <- NUTS.nuts m0 nutsCfg (M.fromList [("mu", 5)]) gen0
      gen1 <- MWC.create
      ch1 <- NUTS.nuts m1 nutsCfg (M.fromList [("mu", 5)]) gen1
      gen2 <- MWC.create
      r <- BF.bayesFactor m0 ch0 m1 ch1 BS.defaultBridgeConfig gen2
      BF.bfLogE r `shouldSatisfy` (> 0)
      -- 解釈: log_e BF > 5 (very strong) を期待 (= 強 prior vs 弱 prior の差)
      BF.bfLogE r `shouldSatisfy` (> 5)
      BF.interpretBF (BF.bfLogE r) `shouldBe` BF.BFVeryStrong
      BF.bfConverged0 r `shouldBe` True
      BF.bfConverged1 r `shouldBe` True
    it "interpretBF: Kass-Raftery 解釈閾値" $ do
      BF.interpretBF 0.5 `shouldBe` BF.BFNegligible
      BF.interpretBF 2.0 `shouldBe` BF.BFPositive
      BF.interpretBF 4.0 `shouldBe` BF.BFStrong
      BF.interpretBF 6.0 `shouldBe` BF.BFVeryStrong
      BF.interpretBF (-4.0) `shouldBe` BF.BFStrong   -- 符号反転にも対応
