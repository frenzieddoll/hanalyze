{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Optim.LBFGSSpec (spec) where

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
import qualified Hanalyze.Optim.LBFGS       as LBFGS
import qualified Hanalyze.Optim.Common      as OC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Optim.LBFGS" $ do
    let l2 :: [Double] -> [Double] -> Double
        l2 a b = sqrt (sum (zipWith (\x y -> (x-y)^(2::Int)) a b))
        sphere xs = sum [x*x | x <- xs]
        sphereGrad xs = [2*x | x <- xs]
        rosen [x, y] = (1-x)^(2::Int) + 100*(y - x*x)^(2::Int)
        rosen _ = error "rosen: 2D"
        rosenGrad [x, y] =
          [ -2*(1-x) - 400*x*(y - x*x), 200*(y - x*x) ]
        rosenGrad _ = error "rosenGrad: 2D"

    it "minimises sphere 5D with analytic grad to ~0" $ do
      r <- LBFGS.runLBFGS sphere sphereGrad [3, -2, 1, 0.5, -1.5]
      OC.orValue r `shouldSatisfy` (< 1e-8)

    it "minimises Rosenbrock 2D within 0.01 of (1,1)" $ do
      let cfg = LBFGS.defaultLBFGSConfig
                  { LBFGS.lbStop = OC.defaultStopCriteria { OC.stMaxIter = 500 } }
      r <- LBFGS.runLBFGSWith cfg rosen rosenGrad [-1.2, 1.0]
      l2 (OC.orBest r) [1, 1] `shouldSatisfy` (< 0.01)

    it "numeric gradient: sphere 30D converges" $ do
      let x0 = take 30 (cycle [1.5, -2.0, 0.5])
      r <- LBFGS.runLBFGSNumeric LBFGS.defaultLBFGSConfig sphere x0
      OC.orValue r `shouldSatisfy` (< 1e-4)

  -- ===========================================================================
  -- 1D オプティマイザ (Hanalyze.Optim.LineSearch)
  -- ===========================================================================
