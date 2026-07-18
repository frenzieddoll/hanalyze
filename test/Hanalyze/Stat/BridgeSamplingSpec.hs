{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.BridgeSamplingSpec (spec) where

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
import qualified Hanalyze.Model.HBM as HBM
import qualified Data.Map.Strict    as M
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.BridgeSampling (Phase 29-A2 Meng-Wong)" $ do
    -- Gaussian × Gaussian model: μ ~ N(0, σ_p=10)、 y ~ N(μ, σ_y=1)、 n=10、 y=5.0
    -- 解析的 log marginal (Python で検算): log p(y) ≈ -12.7686
    -- 算出法: log p(y) = log p(y|μ*) + log p(μ*) - log p(μ*|y) at μ* = posterior mean
    --   μ_post = 4.99500、 σ_post² = 0.0999、 ȳ = 5.0
    let modelBS :: HBM.ModelP ()
        modelBS = do
          mu <- HBM.sample "mu" (HBM.Normal 0 10)
          HBM.observe "y" (HBM.Normal mu 1) (replicate 10 5.0)
        nutsCfg = NUTS.defaultNUTSConfig
          { NUTS.nutsIterations = 2000
          , NUTS.nutsBurnIn     = 500
          , NUTS.nutsAdaptStepSize = True
          }
    it "Bridge Sampling: log marginal が解析解 (-12.77) と 0.5 以内一致" $ do
      gen <- MWC.create
      chain <- NUTS.nuts modelBS nutsCfg (M.fromList [("mu", 5)]) gen
      gen2 <- MWC.create
      r <- BS.bridgeSampling modelBS BS.defaultBridgeConfig chain gen2
      let analytic = (-12.7686)
      BS.brLogMarginal r `shouldSatisfy` (\x -> abs (x - analytic) < 0.5)
      BS.brConverged r `shouldBe` True
    it "Bridge Sampling: 反復が tolerance 内で収束 (< 50 iter)" $ do
      gen <- MWC.create
      chain <- NUTS.nuts modelBS nutsCfg (M.fromList [("mu", 5)]) gen
      gen2 <- MWC.create
      r <- BS.bridgeSampling modelBS BS.defaultBridgeConfig chain gen2
      BS.brIterations r `shouldSatisfy` (< 50)
