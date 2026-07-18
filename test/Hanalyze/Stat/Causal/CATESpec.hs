{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.Causal.CATESpec (spec) where

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
import qualified Hanalyze.Model.RandomForest           as RF
import qualified Data.Vector.Storable              as VS
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC as MWC
import qualified Hanalyze.Stat.Causal.CATE            as CCATE
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.Causal.CATE (Phase 30-A4 meta-learners)" $ do
    -- 異質 treatment effect DGP:
    --   X1, X2 ~ N(0,1)、 T ~ Bern(σ(0.5·X1))
    --   Y = 1 + 0.5·X1 + 0.3·X2 + (1 + X1)·T + N(0, 0.5)
    -- 真の τ(X) = 1 + X1、 ATE = E[τ] = 1.0
    -- LM base learner では: T-learner / X-learner は τ(X) を線形回復、
    -- S-learner は単純な intercept-shift で ATE のみ近似 (interaction 無し)。
    it "CATE: T-learner と X-learner が ATE を 20% 以内回復、 X1 と単調" $ do
      gen <- MWC.create
      let nC = 2000
          trueATE = 1.0 :: Double
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
      let ps0  = VS.map (\a -> sigC (0.5 * a)) x1s
          tsV  = VS.zipWith (\p u -> if u < p then 1.0 else 0.0) ps0 us
          ysV  = VS.zipWith4 (\x1 x2 tt e ->
                    1.0 + 0.5 * x1 + 0.3 * x2 + (1.0 + x1) * tt + e)
                  x1s x2s tsV noises
          x1List = VS.toList x1s
          xMatC = LA.fromColumns
                    [ LA.fromList (replicate nC 1)
                    , LA.fromList x1List
                    , LA.fromList (VS.toList x2s) ]
          tVecC = LA.fromList (VS.toList tsV)
          yVecC = LA.fromList (VS.toList ysV)
      tR <- CCATE.fitCATE CCATE.TLearner CCATE.CATELM xMatC tVecC yVecC gen
      sR <- CCATE.fitCATE CCATE.SLearner CCATE.CATELM xMatC tVecC yVecC gen
      xR <- CCATE.fitCATE CCATE.XLearner CCATE.CATELM xMatC tVecC yVecC gen
      -- ATE 回復: T / X-learner は 20% 以内
      CCATE.cateATE tR `shouldSatisfy` (\v -> abs (v - trueATE) < 0.2)
      CCATE.cateATE xR `shouldSatisfy` (\v -> abs (v - trueATE) < 0.2)
      -- S-learner は constant CATE のため ATE ≈ 1 を 30% 以内
      CCATE.cateATE sR `shouldSatisfy` (\v -> abs (v - trueATE) < 0.3)
      -- 長さ一致
      LA.size (CCATE.cateEstimates tR) `shouldBe` nC
      LA.size (CCATE.cateEstimates sR) `shouldBe` nC
      LA.size (CCATE.cateEstimates xR) `shouldBe` nC
      -- T-learner τ̂(X) は X1 と単調 (= rank correlation > 0.5)
      let tauT = LA.toList (CCATE.cateEstimates tR)
          rankCorr as bs =
            let pairs = zip as bs
                concCount = sum
                  [ 1 :: Int
                  | (i, (ai, bi)) <- zip [0..] pairs
                  , (j, (aj, bj)) <- zip [0..] pairs
                  , i < j
                  , (ai - aj) * (bi - bj) > 0 ]
                discCount = sum
                  [ 1 :: Int
                  | (i, (ai, bi)) <- zip [0..] pairs
                  , (j, (aj, bj)) <- zip [0..] pairs
                  , i < j
                  , (ai - aj) * (bi - bj) < 0 ]
                tot = concCount + discCount
            in if tot == 0
                 then 0.0
                 else fromIntegral (concCount - discCount)
                       / fromIntegral tot
          -- 速度のため先頭 200 サンプルだけで rank correlation を概算
          rc = rankCorr (take 200 tauT) (take 200 x1List)
      rc `shouldSatisfy` (> 0.5)
    it "CATE: RF base learner で T-learner が ATE を 30% 以内回復 (smoke test)" $ do
      gen <- MWC.create
      let nSmall = 400
          trueATE = 1.0 :: Double
      x1s <- VS.replicateM nSmall (MWC.uniformR (-2.0, 2.0 :: Double) gen)
      us  <- VS.replicateM nSmall (MWC.uniformR (0.0, 1.0 :: Double) gen)
      let sigC z = 1.0 / (1.0 + exp (-z))
          ps0  = VS.map (\a -> sigC (0.5 * a)) x1s
          tsV  = VS.zipWith (\p u -> if u < p then 1.0 else 0.0) ps0 us
          ysV  = VS.zipWith (\x1 tt -> 1.0 + 0.5 * x1 + (1.0 + x1) * tt)
                            x1s tsV
          xMatS = LA.fromColumns [LA.fromList (VS.toList x1s)]
          tVecS = LA.fromList (VS.toList tsV)
          yVecS = LA.fromList (VS.toList ysV)
          rfCfg = RF.defaultRandomForest { RF.rfTrees = 30, RF.rfMaxDepth = 6 }
      r <- CCATE.fitCATE CCATE.TLearner (CCATE.CATERF rfCfg)
                         xMatS tVecS yVecS gen
      CCATE.cateATE r `shouldSatisfy` (\v -> abs (v - trueATE) < 0.3)
      LA.size (CCATE.cateEstimates r) `shouldBe` nSmall
