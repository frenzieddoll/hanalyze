{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Optim.CMAESSpec (spec) where

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
import qualified Hanalyze.Optim.CMAES       as CMAES
import qualified Hanalyze.Optim.CMAESFull   as CMAESF
import qualified Hanalyze.Optim.Common      as OC
import qualified System.Random.MWC as MWC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Optim.CMAES" $ do
    let sphere xs = sum [x*x | x <- xs]
        l2 a b = sqrt (sum (zipWith (\x y -> (x-y)^(2::Int)) a b))

    it "CMA-ES: sphere 5D を最小化、原点近傍に到達" $ do
      gen <- MWC.create
      let cfg = CMAES.defaultCMAESConfig
                  { CMAES.cmStop = OC.defaultStopCriteria { OC.stMaxIter = 200 }
                  , CMAES.cmSigma0 = 1.0 }
      r <- CMAES.runCMAESWith cfg sphere [3, -2, 1, 0.5, -1.5] gen
      l2 (OC.orBest r) [0,0,0,0,0] `shouldSatisfy` (< 0.5)

    it "CMA-ES Full: sphere 5D で 1e-3 以内に到達" $ do
      gen <- MWC.create
      let cfg = CMAESF.defaultCMAESFConfig
                  { CMAESF.cmfStop = OC.defaultStopCriteria { OC.stMaxIter = 300 }
                  , CMAESF.cmfSigma0 = 1.0 }
      r <- CMAESF.runCMAESFullWith cfg sphere [3, -2, 1, 0.5, -1.5] gen
      OC.orValue r `shouldSatisfy` (< 1e-3)

    it "CMA-ES Full: Rosenbrock 2D で (1,1) に 0.1 以内" $ do
      gen <- MWC.create
      let cfg = CMAESF.defaultCMAESFConfig
                  { CMAESF.cmfStop   = OC.defaultStopCriteria { OC.stMaxIter = 500 }
                  , CMAESF.cmfSigma0 = 0.5
                  , CMAESF.cmfLambda = Just 20 }
          rosen [x, y] = (1-x)^(2::Int) + 100 * (y - x*x)^(2::Int)
          rosen _      = error "2D"
      r <- CMAESF.runCMAESFullWith cfg rosen [-1.2, 1.0] gen
      l2 (OC.orBest r) [1, 1] `shouldSatisfy` (< 0.1)

  -- ===========================================================================
  -- メタヒューリスティック (Tier 2: Simulated Annealing, PSO)
  -- ===========================================================================
