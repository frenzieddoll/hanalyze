{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.Causal.DoublyRobustSpec (spec) where

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
import qualified Data.Vector.Storable              as VS
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC as MWC
import qualified Hanalyze.Stat.Causal.PropensityScore as PS
import qualified Hanalyze.Stat.Causal.DoublyRobust    as CDR
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.Causal.DoublyRobust (Phase 30-A3 AIPW)" $ do
    -- IPW と同じ合成データ (真の ATE = 2.0)、 共通 helper にすると
    -- IO の流れが複雑になるので 1 つの it 内で 4 ケースまとめて検証。
    it "AIPW: 二重ロバスト性 (正しい outcome / 誤った PS でも 15% 以内回復)" $ do
      gen <- MWC.create
      let nC = 2000
          trueTau = 2.0 :: Double
      x1s <- VS.replicateM nC (do
              u1 <- MWC.uniformR (1e-9, 1.0 :: Double) gen
              u2 <- MWC.uniformR (0.0, 1.0 :: Double) gen
              pure (sqrt (-2 * log u1) * cos (2 * pi * u2)))
      x2s <- VS.replicateM nC (do
              u1 <- MWC.uniformR (1e-9, 1.0 :: Double) gen
              u2 <- MWC.uniformR (0.0, 1.0 :: Double) gen
              pure (sqrt (-2 * log u1) * cos (2 * pi * u2)))
      noises <- VS.replicateM nC (do
              u1 <- MWC.uniformR (1e-9, 1.0 :: Double) gen
              u2 <- MWC.uniformR (0.0, 1.0 :: Double) gen
              pure (0.5 * sqrt (-2 * log u1) * cos (2 * pi * u2)))
      let sigC z = 1.0 / (1.0 + exp (-z))
      us <- VS.replicateM nC (MWC.uniformR (0.0, 1.0 :: Double) gen)
      let ps0  = VS.zipWith (\a b -> sigC (0.8 * a + 0.4 * b)) x1s x2s
          tsV  = VS.zipWith (\p u -> if u < p then 1.0 else 0.0) ps0 us
          ysV  = VS.zipWith4 (\x1 x2 tt e ->
                    1.0 + 0.5 * x1 + 0.3 * x2 + trueTau * tt + e)
                  x1s x2s tsV noises
          xMatC = LA.fromColumns
                    [ LA.fromList (replicate nC 1)
                    , LA.fromList (VS.toList x1s)
                    , LA.fromList (VS.toList x2s) ]
          tVecC = LA.fromList (VS.toList tsV)
          yVecC = LA.fromList (VS.toList ysV)
      -- (a) 両方正しい: DR は τ=2 を 10% 以内
      let drR = CDR.doublyRobust xMatC tVecC yVecC
      CDR.drATE drR `shouldSatisfy` (\v -> abs (v - trueTau) < 0.1 * trueTau)
      LA.size (CDR.drMu1Predicted drR) `shouldBe` nC
      LA.size (CDR.drMu0Predicted drR) `shouldBe` nC
      -- (b) PS misspecified (= 全て 0.5、 = "RCT 想定"): outcome model が正しい
      --     ので DR は 15% 以内に補正される (二重ロバスト性の片側)
      let psBad = PS.PropensityScore
                    (LA.fromList (replicate nC 0.5))
                    (LA.fromList [0]) nC
          drBadPS = CDR.doublyRobustWith psBad xMatC tVecC yVecC
      CDR.drATE drBadPS `shouldSatisfy` (\v -> abs (v - trueTau) < 0.15 * trueTau)
      -- (c) PS は正しく、 outcome model は intercept-only (= misspecified):
      --     DR は IPW 相当に縮退し、 PS が正しいので 15% 以内
      let xIntOnly = LA.fromColumns [LA.fromList (replicate nC 1)]
          psGood   = PS.trimPropensity 0.01 0.99 (PS.propensityScore xMatC tVecC)
          drBadOut = CDR.doublyRobustWith psGood xIntOnly tVecC yVecC
      CDR.drATE drBadOut `shouldSatisfy` (\v -> abs (v - trueTau) < 0.15 * trueTau)
