{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.MCMC.SMCSpec (spec) where

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
import qualified Hanalyze.Model.Core        as Core
import qualified System.Random.MWC as MWC
import qualified Hanalyze.MCMC.SMC  as SMC
import qualified Hanalyze.MCMC.Core as Core
import qualified Hanalyze.Model.HBM as HBM
import qualified Data.Map.Strict    as M
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.MCMC.SMC (Phase 29-A1 tempered Sequential Monte Carlo)" $ do
    -- Simple Gaussian model: μ ~ N(0, 10)、 y ~ N(μ, 1)、 obs y = 5.0 (n=10)
    -- Analytic posterior: μ | y ~ N(μ_post, σ_post²)、
    --   σ_post² = 1 / (1/100 + 10/1) = 1/10.01 ≈ 0.0999
    --   μ_post = σ_post² · (0/100 + 10·5/1) = 0.0999·50 ≈ 4.995
    -- 解析解との比較 (5% tolerance)
    let modelSMC :: HBM.ModelP ()
        modelSMC = do
          mu <- HBM.sample "mu" (HBM.Normal 0 10)
          HBM.observe "y" (HBM.Normal mu 1) (replicate 10 5.0)
        cfg = (SMC.defaultSMCConfig ["mu"])
          { SMC.smcNParticles   = 500
          , SMC.smcNSteps       = 15
          , SMC.smcMHIterations = 8
          , SMC.smcMHStepSize   = M.fromList [("mu", 0.5)]
          , SMC.smcInitJitter   = 5.0
          }
    it "SMC: 粒子数 = N、 log marginal が finite" $ do
      gen <- MWC.create
      res <- SMC.smc modelSMC cfg (M.fromList [("mu", 0)]) gen
      length (Core.chainSamples (SMC.smcChain res)) `shouldBe` 500
      SMC.smcLogMarginal res `shouldSatisfy` (not . isInfinite)
      SMC.smcLogMarginal res `shouldSatisfy` (not . isNaN)
    it "SMC: posterior mean が解析解 (4.995) と 5% 以内一致" $ do
      gen <- MWC.create
      res <- SMC.smc modelSMC cfg (M.fromList [("mu", 0)]) gen
      case Core.posteriorMean "mu" (SMC.smcChain res) of
        Just mu -> abs (mu - 4.995) `shouldSatisfy` (< 0.25)  -- 5% of 5
        Nothing -> expectationFailure "posterior mean not available"
    it "SMC: posterior SD が解析解 (0.316) と妥当範囲内" $ do
      gen <- MWC.create
      res <- SMC.smc modelSMC cfg (M.fromList [("mu", 0)]) gen
      case Core.posteriorSD "mu" (SMC.smcChain res) of
        -- 真値 sqrt(0.0999) ≈ 0.316、 SMC の N=500 サンプリング誤差で 50% tol
        Just sd -> sd `shouldSatisfy` (\s -> s > 0.15 && s < 0.6)
        Nothing -> expectationFailure "posterior SD not available"
