{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Optim.CommonSpec (spec) where

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
import qualified Hanalyze.Optim.NelderMead  as NM
import qualified Hanalyze.Optim.LBFGS       as LBFGS
import qualified Hanalyze.Optim.CMAES       as CMAES
import qualified Hanalyze.Optim.CMAESFull   as CMAESF
import qualified Hanalyze.Optim.Common      as OC
import qualified System.Random.MWC as MWC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Optim.Common box constraints" $ do
    it "clipToBounds: 範囲外を反射、範囲内はそのまま" $ do
      OC.clipToBounds [(0,10)] [5]    `shouldBe` [5]
      OC.clipToBounds [(0,10)] [-2]   `shouldBe` [2]    -- 反射
      OC.clipToBounds [(0,10)] [12]   `shouldBe` [8]

    it "boundsPenalty: 範囲内 0、範囲外で大きい正値" $ do
      OC.boundsPenalty (Just [(0,10)]) [5]    `shouldBe` 0
      OC.boundsPenalty Nothing         [-99]  `shouldBe` 0
      OC.boundsPenalty (Just [(0,10)]) [-1]   `shouldSatisfy` (> 1e5)

    it "NelderMead: nmBounds で box 内に解が収まる" $ do
      let f xs = (head xs - 5)^(2::Int)   -- 真の最小は x=5
          cfg = NM.defaultNMConfig { NM.nmBounds = Just [(0, 2)] }
      r <- NM.runNelderMeadWith cfg f [1.0]
      head (OC.orBest r) `shouldSatisfy` (\v -> v >= -0.1 && v <= 2.1)

    it "LBFGS: lbBounds で box 内に解が収まる" $ do
      let f xs = (head xs - 5)^(2::Int)
          cfg = LBFGS.defaultLBFGSConfig { LBFGS.lbBounds = Just [(0, 2)] }
      r <- LBFGS.runLBFGSNumeric cfg f [1.0]
      head (OC.orBest r) `shouldSatisfy` (\v -> v >= -0.1 && v <= 2.1)

    it "CMAES (簡易版): cmBounds で box 内サンプル" $ do
      gen <- MWC.create
      let f xs = sum [x*x | x <- xs]
          cfg = CMAES.defaultCMAESConfig
                  { CMAES.cmBounds = Just [(-1, 1), (-1, 1)]
                  , CMAES.cmStop   = (CMAES.cmStop CMAES.defaultCMAESConfig)
                                       { OC.stMaxIter = 80 }
                  }
      r <- CMAES.runCMAESWith cfg f [0.5, 0.5] gen
      all (\v -> v >= -1.05 && v <= 1.05) (OC.orBest r) `shouldBe` True

    it "CMAESFull: cmfBounds で box 内サンプル" $ do
      gen <- MWC.create
      let f xs = sum [x*x | x <- xs]
          cfg = CMAESF.defaultCMAESFConfig
                  { CMAESF.cmfBounds = Just [(-1, 1), (-1, 1)]
                  , CMAESF.cmfStop   = (CMAESF.cmfStop CMAESF.defaultCMAESFConfig)
                                         { OC.stMaxIter = 80 }
                  }
      r <- CMAESF.runCMAESFullWith cfg f [0.5, 0.5] gen
      all (\v -> v >= -1.05 && v <= 1.05) (OC.orBest r) `shouldBe` True

  -- ===========================================================================
  -- Bayesian Optimization 内部最適化の差し替え (Hanalyze.Optim.BayesOpt)
  -- ===========================================================================
