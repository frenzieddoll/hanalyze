{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Optim.NelderMeadSpec (spec) where

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
import qualified Hanalyze.Optim.NelderMead  as NM
import qualified Hanalyze.Optim.Common      as OC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Optim.NelderMead" $ do
    let l2 :: [Double] -> [Double] -> Double
        l2 a b = sqrt (sum (zipWith (\x y -> (x-y)^(2::Int)) a b))
        sphere xs = sum [x*x | x <- xs]
        rosenbrock [x, y] = (1 - x)^(2::Int) + 100 * (y - x*x)^(2::Int)
        rosenbrock _ = error "rosenbrock: 2D only"

    it "minimises sphere f(x)=Σx² to ~0 from x0=[3,-2,1]" $ do
      r <- NM.runNelderMead sphere [3, -2, 1]
      OC.orValue r `shouldSatisfy` (< 1e-6)

    it "minimises Rosenbrock 2D to (1,1) within 0.05" $ do
      let cfg = NM.defaultNMConfig
                  { NM.nmStop = OC.defaultStopCriteria { OC.stMaxIter = 5000 } }
      r <- NM.runNelderMeadWith cfg rosenbrock [-1.2, 1.0]
      l2 (OC.orBest r) [1, 1] `shouldSatisfy` (< 0.05)

    it "Maximize: -sphere has optimum 0 at origin" $ do
      let cfg = NM.defaultNMConfig { NM.nmDir = OC.Maximize }
      r <- NM.runNelderMeadWith cfg (\xs -> negate (sphere xs)) [3, -2, 1]
      -- Maximize: orValue は元尺度 (= negate sphere の最大値、すなわち 0 に近い)
      OC.orValue r `shouldSatisfy` (\v -> v > -1e-6)
      -- 最良点は原点近傍
      l2 (OC.orBest r) [0, 0, 0] `shouldSatisfy` (< 1e-2)

  -- ===========================================================================
  -- 単目的オプティマイザ (Hanalyze.Optim.LBFGS)
  -- ===========================================================================
