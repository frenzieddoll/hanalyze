{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Optim.SimulatedAnnealingSpec (spec) where

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
import qualified Hanalyze.Optim.SimulatedAnnealing as SA
import qualified Hanalyze.Optim.Common      as OC
import qualified System.Random.MWC as MWC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Optim.SimulatedAnnealing" $ do
    it "SA: sphere 5D で sufficient annealing" $ do
      gen <- MWC.create
      let bs = replicate 5 (-3, 3)
          cfg = (SA.defaultSAConfig bs)
                  { SA.saStop = OC.defaultStopCriteria { OC.stMaxIter = 5000 }
                  , SA.saInitTemp = 2.0
                  , SA.saSchedule = SA.Geometric 0.997 }
          sphere xs = sum [x*x | x <- xs]
      r <- SA.runSAWith cfg sphere [2, -1.5, 1, 0.5, -0.7] gen
      OC.orValue r `shouldSatisfy` (< 0.5)
