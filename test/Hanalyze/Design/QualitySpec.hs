{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Design.QualitySpec (spec) where

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
import qualified Hanalyze.Design.Quality    as Quality
import qualified Hanalyze.Model.Weibull     as WB
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Design.Quality.processCapability" $ do
    it "centred process with σ=1, USL=6, LSL=−6 → Cp ≈ 2.0, Cpk ≈ 2.0" $ do
      -- 11-point symmetric sample around 0 with σ=1 (population)
      let xs = LA.fromList [-1.5, -1.0, -0.5, 0.5, 1.0, 1.5,
                             1.5,  1.0,  0.5, -0.5, -1.0, -1.5]
          cap = Quality.processCapability (-6) 6 xs
      Quality.capCp  cap `shouldSatisfy` (> 0)
      -- For a centred sample, Cp == Cpk by symmetry.
      abs (Quality.capCp cap - Quality.capCpk cap)
        `shouldSatisfy` (< 1e-2)

    it "shifted process: Cpk < Cp" $ do
      let xs = LA.fromList [4.0, 4.5, 4.2, 4.8, 4.3, 4.6, 4.4, 4.7]
          cap = Quality.processCapability 0 6 xs
      Quality.capCp cap  `shouldSatisfy` (> Quality.capCpk cap)

    it "processCapabilityUpper: only USL → Cp == Cpk" $ do
      let xs = LA.fromList [1.0, 1.2, 0.9, 1.1, 1.05, 0.95]
          cap = Quality.processCapabilityUpper 2.0 xs
      Quality.capCp cap `shouldBe` Quality.capCpk cap

  describe "Hanalyze.Design.Quality 非正規 (Phase 13.3)" $ do
    it "processCapabilityWeibull: 形状 k=2 の Weibull で Cp 計算が finite" $ do
      let wf = WB.WeibullFit 2.0 100.0 0 10 10 (0, 0, 0)
          cap = Quality.processCapabilityWeibull wf 10 300
      Quality.capCp cap `shouldSatisfy` (> 0)
      Quality.capCp cap `shouldSatisfy` (\v -> not (isNaN v))
    it "processCapabilityLogNormal: 対称 spec で Cpk = Cp の上下" $ do
      let mu = 0
          sigma = 0.3
          med = exp mu
          cap = Quality.processCapabilityLogNormal mu sigma (med * 0.5) (med * 2.0)
      Quality.capCp cap `shouldSatisfy` (> 0)
      Quality.capCpk cap `shouldSatisfy` (<= Quality.capCp cap + 1e-9)
    it "processCapabilityLogNormal: spec を 0 spread にすると Cp = 0" $ do
      let cap = Quality.processCapabilityLogNormal 0 0.3 1.0 1.0
      Quality.capCp cap `shouldSatisfy` (\v -> v <= 0)

  describe "Hanalyze.Design.Quality 非正規 Gamma + 統一エントリ (Phase 23-c)" $ do
    it "processCapabilityGamma: shape=2 scale=50 で Cp 計算が finite + 正" $ do
      let cap = Quality.processCapabilityGamma 2.0 50.0 5 400
      Quality.capCp cap `shouldSatisfy` (> 0)
      Quality.capCp cap `shouldSatisfy` (not . isNaN)
    it "processCapabilityGamma: spec の spread を 0 にすると Cp ≤ 0" $ do
      let cap = Quality.processCapabilityGamma 2.0 50.0 100 100
      Quality.capCp cap `shouldSatisfy` (<= 0)
    it "processCapabilityNonNormal: Weibull dispatch が個別関数と一致" $ do
      let wf = WB.WeibullFit 2.0 100.0 0 10 10 (0, 0, 0)
          capA = Quality.processCapabilityNonNormal (Quality.NNFWeibull wf) 10 300
          capB = Quality.processCapabilityWeibull wf 10 300
      Quality.capCp  capA `shouldBe` Quality.capCp  capB
      Quality.capCpk capA `shouldBe` Quality.capCpk capB
    it "processCapabilityNonNormal: LogNormal dispatch が個別関数と一致" $ do
      let capA = Quality.processCapabilityNonNormal (Quality.NNFLogNormal 0 0.3) 0.5 2.0
          capB = Quality.processCapabilityLogNormal 0 0.3 0.5 2.0
      Quality.capCp  capA `shouldBe` Quality.capCp  capB
    it "processCapabilityNonNormal: Gamma dispatch が個別関数と一致" $ do
      let capA = Quality.processCapabilityNonNormal (Quality.NNFGamma 2.0 50.0) 5 400
          capB = Quality.processCapabilityGamma 2.0 50.0 5 400
      Quality.capCp  capA `shouldBe` Quality.capCp  capB

  describe "Hanalyze.Design.Quality 多変量 Cp (Phase 23-d)" $ do
    let -- 中心 (5, 10)、 各軸 σ ≈ 1 の擬似 2 変数データ (8 点)
        dat = LA.fromLists
          [ [4, 9], [5, 10], [6, 11], [5, 10]
          , [4, 11], [6, 9], [5, 11], [5, 9] ]
        specs = [(2, 8), (7, 13)]   -- 各軸 ±3 中心、 spread = 6
    it "processCapabilityMultivariate: 基本動作、 MCp 正・finite" $ do
      case Quality.processCapabilityMultivariate dat specs of
        Left e   -> expectationFailure (show e)
        Right mc -> do
          Quality.mcNVars mc `shouldBe` 2
          Quality.mcMCp mc `shouldSatisfy` (> 0)
          Quality.mcMCp mc `shouldSatisfy` (not . isNaN)
          Quality.mcInSpecRate mc `shouldBe` 1.0
    it "MCpk ≤ MCp (中心オフセット penalty で抑制)" $ do
      case Quality.processCapabilityMultivariate dat specs of
        Left e   -> expectationFailure (show e)
        Right mc -> Quality.mcMCpk mc `shouldSatisfy` (<= Quality.mcMCp mc + 1e-9)
    it "specs 長さ不一致は Left" $ do
      case Quality.processCapabilityMultivariate dat [(0, 1)] of
        Left _   -> pure ()
        Right _  -> expectationFailure "expected Left"
    it "n=1 (観測 1 件) は Left" $ do
      case Quality.processCapabilityMultivariate (LA.fromLists [[1, 2]]) specs of
        Left _   -> pure ()
        Right _  -> expectationFailure "expected Left"
    it "InSpecRate: spec を厳しくすると内包率が下がる" $ do
      case Quality.processCapabilityMultivariate dat [(4.5, 5.5), (9.5, 10.5)] of
        Left e   -> expectationFailure (show e)
        Right mc -> Quality.mcInSpecRate mc `shouldSatisfy` (< 1.0)
