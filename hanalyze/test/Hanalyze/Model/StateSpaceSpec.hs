{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.StateSpaceSpec (spec) where

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
import qualified Numeric.LinearAlgebra as LA
import qualified Hanalyze.Model.StateSpace     as SS
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.StateSpace (Phase 15)" $ do
    let -- 1次元 random walk: x_t = x_{t-1} + w, y_t = x_t + v
        rwModel = SS.StateSpaceModel
          { SS.ssF  = LA.fromLists [[1]]
          , SS.ssH  = LA.fromLists [[1]]
          , SS.ssQ  = LA.fromLists [[0.1]]
          , SS.ssR  = LA.fromLists [[1.0]]
          , SS.ssX0 = LA.fromList  [0]
          , SS.ssP0 = LA.fromLists [[1.0]]
          }
        -- 真の状態 0, 0.1, 0.3, 0.5, ... + ノイズ込み観測
        obs = LA.fromLists [[0.1, 0.2, 0.4, 0.6, 0.7, 0.9, 1.1]]
    it "kalmanFilter: T 時点で filtered list 長 = T" $ do
      let kr = SS.kalmanFilter rwModel obs
      length (SS.krFilteredMean kr) `shouldBe` 7
      length (SS.krFilteredCov kr)  `shouldBe` 7
    it "kalmanFilter: 共分散が正定値 (対角 > 0)" $ do
      let kr = SS.kalmanFilter rwModel obs
      all (\p -> LA.atIndex p (0, 0) > 0) (SS.krFilteredCov kr) `shouldBe` True
    it "kalmanFilter: 観測列が長くなるほど 共分散は減少 (情報蓄積)" $ do
      let kr = SS.kalmanFilter rwModel obs
          covs = SS.krFilteredCov kr
          c0 = LA.atIndex (head covs) (0, 0)
          cT = LA.atIndex (last covs) (0, 0)
      cT `shouldSatisfy` (< c0)
    it "kalmanFilter: 観測トレンドに状態が追随" $ do
      let kr = SS.kalmanFilter rwModel obs
          last_x = LA.atIndex (last (SS.krFilteredMean kr)) 0
      last_x `shouldSatisfy` (> 0.3)   -- 観測が 1 近辺に上がっている
    it "kalmanFilter: 対数尤度が有限" $ do
      let kr = SS.kalmanFilter rwModel obs
      SS.krLogLik kr `shouldSatisfy` (\v -> not (isNaN v) && not (isInfinite v))
    it "kalmanSmoother: smoothed list 長 = T" $ do
      let kr = SS.kalmanSmoother rwModel (SS.kalmanFilter rwModel obs)
      length (SS.krSmoothedMean kr) `shouldBe` 7
      length (SS.krSmoothedCov kr)  `shouldBe` 7
    it "kalmanSmoother: 末尾は filtered と一致" $ do
      let kr = SS.kalmanFilter rwModel obs
          ks = SS.kalmanSmoother rwModel kr
      abs (LA.atIndex (last (SS.krSmoothedMean ks)) 0
           - LA.atIndex (last (SS.krFilteredMean ks)) 0)
        `shouldSatisfy` (< 1e-9)
    it "kalmanSmoother: 中間点 smoothed 共分散 ≤ filtered" $ do
      let kr = SS.kalmanFilter rwModel obs
          ks = SS.kalmanSmoother rwModel kr
          mid = 3
          covF = LA.atIndex (SS.krFilteredCov ks !! mid) (0, 0)
          covS = LA.atIndex (SS.krSmoothedCov ks !! mid) (0, 0)
      covS `shouldSatisfy` (<= covF + 1e-9)
