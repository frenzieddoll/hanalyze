{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Optim.BayesOptSpec (spec) where

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
import qualified Hanalyze.Optim.BayesOpt    as BO
import qualified System.Random.MWC as MWC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Optim.BayesOpt (acquisition optimizer swap)" $ do
    it "bayesOpt 1D: Brent 内側で簡単な凸関数 (x-1.5)^2 を見つける" $ do
      gen <- MWC.create
      let cfg = BO.defaultBayesOptConfig
                  { BO.boIterations = 8
                  , BO.boInitPoints = 4
                  , BO.boGridSize   = 32
                  }
          target x = pure ((x - 1.5)^(2::Int) :: Double)
      (_, (xb, _)) <- BO.bayesOpt cfg target (0, 3) gen
      -- 8 反復では精度は緩めに。3.0 範囲のうち 0.5 以内に収束を期待
      abs (xb - 1.5) `shouldSatisfy` (< 0.5)

  -- ===========================================================================
  -- RFF HP 自動チューニングの DE 版 (Phase O9)
  -- ===========================================================================
