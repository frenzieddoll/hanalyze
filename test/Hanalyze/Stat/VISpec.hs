{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.VISpec (spec) where

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
import qualified System.Random.MWC as MWC
import qualified Hanalyze.Model.HBM as HBM
import qualified Hanalyze.Stat.VI as VI
import qualified Data.Map.Strict    as M
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.VI.fullRankAdvi (Phase 37-A5)" $ do
    -- 相関のある事後を持つ簡単な 2 次元モデル
    -- mu1 ~ Normal(0, 5), mu2 ~ Normal(0, 5)
    -- y_i ~ Normal(mu1 + mu2, 0.3) を 10 件観測 → mu1 + mu2 の事後はタイトだが
    -- mu1 単独 / mu2 単独 は逆相関 (sum が決まると個別は trade-off)
    let model :: HBM.ModelP ()
        model = do
          mu1 <- HBM.sample "mu1" (HBM.Normal 0 5)
          mu2 <- HBM.sample "mu2" (HBM.Normal 0 5)
          HBM.observe "y" (HBM.Normal (mu1 + mu2) 0.3)
            (replicate 10 6.0)
        cfg = VI.defaultVIConfig
                { VI.viIterations = 300
                , VI.viSamples    = 5
                , VI.viLearningRate = 0.1
                , VI.viNumDraws   = 200
                }
        initP = M.fromList [("mu1", 3.0), ("mu2", 3.0)]
    --
    it "fullRankAdvi は viCovU = Just と viMethod = FullRank を返す" $ do
      gen <- MWC.create
      res <- VI.fullRankAdvi model cfg initP gen
      VI.viMethod res `shouldBe` VI.FullRank
      (case VI.viCovU res of Just _ -> True; Nothing -> False) `shouldBe` True
    it "mean-field advi は viCovU = Nothing と viMethod = MeanField" $ do
      gen <- MWC.create
      res <- VI.advi model cfg initP gen
      VI.viMethod res `shouldBe` VI.MeanField
      (case VI.viCovU res of Just _ -> False; Nothing -> True) `shouldBe` True
    it "fullRankAdvi の L は下三角 (上三角は 0)" $ do
      gen <- MWC.create
      res <- VI.fullRankAdvi model cfg initP gen
      case VI.viCovU res of
        Just l ->
          let n = length l
              upperZero = and [ (l !! i !! j) == 0
                              | i <- [0 .. n-1]
                              , j <- [i+1 .. n-1] ]
          in upperZero `shouldBe` True
        Nothing -> expectationFailure "viCovU should be Just"
    it "fullRankAdvi は相関を学習 (L の off-diagonal が非ゼロ)" $ do
      gen <- MWC.create
      res <- VI.fullRankAdvi model cfg initP gen
      case VI.viCovU res of
        Just l | length l >= 2 ->
          -- L[1][0] が非ゼロ = 相関を捉えている
          (abs (l !! 1 !! 0) > 0.01) `shouldBe` True
        _ -> expectationFailure "viCovU must be at least 2×2"
    it "fullRankAdvi 事後平均が mu1+mu2 ≈ 6 を回復 (合計が観測値)" $ do
      gen <- MWC.create
      res <- VI.fullRankAdvi model cfg initP gen
      let m1 = M.findWithDefault 0 "mu1" (VI.viPostMeans res)
          m2 = M.findWithDefault 0 "mu2" (VI.viPostMeans res)
      (abs (m1 + m2 - 6) < 0.5) `shouldBe` True
