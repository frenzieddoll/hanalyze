{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Optim.ParticleSwarmSpec (spec) where

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
import qualified Hanalyze.Optim.ParticleSwarm as PSO
import qualified Hanalyze.Optim.Common      as OC
import qualified System.Random.MWC as MWC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Optim.ParticleSwarm" $ do
    it "PSO: sphere 5D で原点近傍 (0.5 以内)" $ do
      gen <- MWC.create
      let bs = replicate 5 (-5, 5)
          cfg = (PSO.defaultPSOConfig bs)
                  { PSO.psoStop = OC.defaultStopCriteria { OC.stMaxIter = 200 }
                  , PSO.psoNum  = 30 }
          sphere xs = sum [x*x | x <- xs]
      r <- PSO.runPSOWith cfg sphere gen
      OC.orValue r `shouldSatisfy` (< 0.5)

    it "PSO: Rastrigin 3D の大域最小に近い" $ do
      gen <- MWC.create
      let bs = replicate 3 (-5.12, 5.12)
          cfg = (PSO.defaultPSOConfig bs)
                  { PSO.psoStop = OC.defaultStopCriteria { OC.stMaxIter = 300 }
                  , PSO.psoNum  = 40 }
          rastrigin xs =
            10 * fromIntegral (length xs) +
            sum [x*x - 10 * cos (2 * pi * x) | x <- xs]
      r <- PSO.runPSOWith cfg rastrigin gen
      OC.orValue r `shouldSatisfy` (< 5.0)   -- 大域近傍 (10 程度の局所有り)

  -- ===========================================================================
  -- 制約付き最適化 (Augmented Lagrangian)
  -- ===========================================================================
