{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Optim.DifferentialEvolutionSpec (spec) where

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
import qualified Hanalyze.Optim.DifferentialEvolution as DE
import qualified Hanalyze.Optim.Common      as OC
import qualified System.Random.MWC as MWC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Optim.DifferentialEvolution" $ do
    let sphere xs = sum [x*x | x <- xs]
        rastrigin xs =
          10 * fromIntegral (length xs) +
          sum [x*x - 10 * cos (2 * pi * x) | x <- xs]
        l2 a b = sqrt (sum (zipWith (\x y -> (x-y)^(2::Int)) a b))

    it "DE: sphere 5D が原点付近に到達" $ do
      gen <- MWC.create
      let bs = replicate 5 (-5, 5)
          cfg = (DE.defaultDEConfig bs)
                  { DE.deStop = OC.defaultStopCriteria { OC.stMaxIter = 200 } }
      r <- DE.runDEWith cfg sphere gen
      OC.orValue r `shouldSatisfy` (< 1e-3)

    it "DE: Rastrigin 3D の大域最小 (原点) を見つける" $ do
      gen <- MWC.create
      let bs = replicate 3 (-5.12, 5.12)
          cfg = (DE.defaultDEConfig bs)
                  { DE.deStop = OC.defaultStopCriteria { OC.stMaxIter = 400 } }
      r <- DE.runDEWith cfg rastrigin gen
      l2 (OC.orBest r) [0, 0, 0] `shouldSatisfy` (< 0.5)

  -- ===========================================================================
  -- 大域オプティマイザ (Hanalyze.Optim.CMAES、簡易対角版)
  -- ===========================================================================
