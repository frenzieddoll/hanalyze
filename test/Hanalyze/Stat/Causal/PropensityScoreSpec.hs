{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.Causal.PropensityScoreSpec (spec) where

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
import qualified Hanalyze.Stat.Causal.PropensityScore as PS
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.Causal.PropensityScore (Phase 30-A1)" $ do
    -- 合成データ: x ~ uniform[-2, 2]、 T ~ Bernoulli(σ(x))
    -- 真の logit: logit(p) = x、 intercept ≈ 0、 slope ≈ 1
    let n = 500
        xs = [(-2.0) + 4.0 * fromIntegral i / fromIntegral (n - 1) | i <- [0 .. n - 1]]
        sigmoid z = 1.0 / (1.0 + exp (-z))
        -- 決定的 0/1 割り当て (= 並べ替え逆数で疑似 Bernoulli): σ(x) > 閾値
        -- → 安定したテストのため、 各 x で T = 1 if σ(x) > i_norm else 0
        -- i_norm を [0,1] 等差にすることで rate ≈ σ(x) を実現
        -- 閾値は i に対し x と無相関になるよう golden ratio low-discrepancy で散らす
        frac z = z - fromIntegral (floor z :: Int)
        thrs = [ frac (fromIntegral i * 0.6180339887498949) | i <- [0 .. n - 1] ]
        ts   = [ if sigmoid x > thr then 1.0 else 0.0
               | (x, thr) <- zip xs thrs ]
        xMat = LA.fromColumns [LA.fromList (replicate n 1), LA.fromList xs]  -- intercept + x
        tVec = LA.fromList ts
        psR  = PS.propensityScore xMat tVec
    it "propensityScore: psN がサンプル数と一致" $
      PS.psN psR `shouldBe` n
    it "propensityScore: psScores が [0,1] に収まる + ベクトル長一致" $ do
      LA.size (PS.psScores psR) `shouldBe` n
      LA.toList (PS.psScores psR) `shouldSatisfy`
        all (\p -> p >= 0 && p <= 1)
    it "propensityScore: logistic slope > 0 (真値 +1 の符号と一致)" $ do
      let beta = LA.toList (PS.psBeta psR)
      length beta `shouldBe` 2
      (beta !! 1) `shouldSatisfy` (> 0.3)   -- slope 正、 強く回復しなくても符号は必須
    it "trimPropensity: [0.01, 0.99] にクリップ" $ do
      let ps0 = PS.PropensityScore (LA.fromList [0.0, 0.5, 1.0])
                                   (LA.fromList [0, 0]) 3
          ps1 = PS.trimPropensity 0.01 0.99 ps0
      LA.toList (PS.psScores ps1) `shouldBe` [0.01, 0.5, 0.99]
    it "ipwWeights: t/p + (1-t)/(1-p) を要素ごとに計算" $ do
      let ps0 = PS.PropensityScore (LA.fromList [0.2, 0.8])
                                   (LA.fromList [0]) 2
          t0  = LA.fromList [1.0, 0.0]
          ws  = LA.toList (PS.ipwWeights ps0 t0)
      -- 期待: [1/0.2, 1/0.2] = [5.0, 5.0]
      ws `shouldSatisfy` (\xs -> length xs == 2
                              && abs (xs !! 0 - 5.0) < 1e-9
                              && abs (xs !! 1 - 5.0) < 1e-9)
    it "attWeights: t + (1-t)·p/(1-p) を要素ごとに計算" $ do
      let ps0 = PS.PropensityScore (LA.fromList [0.25, 0.75])
                                   (LA.fromList [0]) 2
          t0  = LA.fromList [1.0, 0.0]
          ws  = LA.toList (PS.attWeights ps0 t0)
      -- 期待: [1.0, 0.75/0.25] = [1.0, 3.0]
      ws `shouldSatisfy` (\xs -> length xs == 2
                              && abs (xs !! 0 - 1.0) < 1e-9
                              && abs (xs !! 1 - 3.0) < 1e-9)
    it "trim 後の重みが有限 (発散しない)" $ do
      -- p = 0 / 1 を含むケースを作り、 trim 前後で max weight を比較
      let ps0  = PS.PropensityScore (LA.fromList [0.001, 0.5, 0.999])
                                    (LA.fromList [0]) 3
          ps1  = PS.trimPropensity 0.01 0.99 ps0
          t0   = LA.fromList [1.0, 1.0, 0.0]
          ws1  = LA.toList (PS.ipwWeights ps1 t0)
      ws1 `shouldSatisfy` all (\w -> not (isInfinite w) && w < 200)
