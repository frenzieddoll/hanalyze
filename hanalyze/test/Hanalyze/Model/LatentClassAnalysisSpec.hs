{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.LatentClassAnalysisSpec (spec) where

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
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC as MWC
import qualified Hanalyze.Model.LatentClassAnalysis   as LCA
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.LatentClassAnalysis (Phase 32-A2 EM)" $ do
    -- DGP: 2 クラス、 4 二値特徴、 各 n=400
    --   class 0: 各特徴 ρ_{0,j,1} = 0.9 (= "1" 多)
    --   class 1: 各特徴 ρ_{1,j,1} = 0.1 (= "0" 多)
    --   π = [0.5, 0.5]
    it "fitLCA: 2 クラス分離 DGP で π / ρ を回復 (label switch 許容)" $ do
      gen <- MWC.create
      let nL = 400
          jL = 4
          drawObs :: Int -> IO [Int]
          drawObs cls = mapM (\_ -> do
              u <- MWC.uniformR (0.0, 1.0 :: Double) gen
              let pOne = if cls == 0 then 0.9 else 0.1
              pure (if u < pOne then 1 else 0))
              [0 .. jL - 1]
      rows <- mapM (\i -> do
                cls <- if i < nL `div` 2 then pure 0 else pure 1
                drawObs cls)
              [0 .. nL - 1]
      fit <- LCA.fitLCA 2 2 rows 100 1e-4 gen
      let pis = LA.toList (LCA.lcaPi fit)
      length pis `shouldBe` 2
      -- 両クラスとも 0.5 ± 0.1
      all (\p -> abs (p - 0.5) < 0.1) pis `shouldBe` True
      -- ρ: 各特徴のクラス間で「1 を取る確率」 が極端 (0.1 / 0.9 付近)
      -- label switch 対応: クラス 0 / 1 の (1 を取る確率) を取り出し sort
      let p1OfClass k =
            map (\rho -> LA.atIndex rho (k, 1)) (LCA.lcaRho fit)
          c0 = p1OfClass 0
          c1 = p1OfClass 1
          (hiSet, loSet) = if sum c0 > sum c1 then (c0, c1) else (c1, c0)
      all (> 0.7) hiSet `shouldBe` True
      all (< 0.3) loSet `shouldBe` True
    it "fitLCA: 単一クラス (K=1) は trivial、 ρ がデータ周辺分布と一致" $ do
      gen <- MWC.create
      let rows = replicate 100 [0, 1] ++ replicate 100 [1, 0]
      fit <- LCA.fitLCA 1 2 rows 50 1e-6 gen
      LA.toList (LCA.lcaPi fit) `shouldBe` [1.0]
      -- K=1 で feature 0: P(0)=0.5, P(1)=0.5
      let rho0_row0 = LA.toList (LA.flatten ((head (LCA.lcaRho fit)) LA.? [0]))
      rho0_row0 `shouldSatisfy`
        (\xs -> length xs == 2
             && abs ((xs !! 0) - 0.5) < 0.01
             && abs ((xs !! 1) - 0.5) < 0.01)
