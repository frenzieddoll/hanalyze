{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.Causal.IPWSpec (spec) where

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
import qualified Hanalyze.Stat.Causal.IPW             as CIPW
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.Causal.IPW (Phase 30-A2)" $ do
    -- 合成データ (真の ATE = 2.0):
    --   X1 ~ N(0, 1)、 X2 ~ N(0, 1)、 T ~ Bern(σ(0.8·X1 + 0.4·X2))
    --   Y = 1.0 + 0.5·X1 + 0.3·X2 + 2.0·T + N(0, 0.5)
    -- → naive E[Y|T=1] - E[Y|T=0] は X1/X2 経由の交絡で真値より上、
    --   IPW で正しく τ=2 に補正されるはず。
    let trueTau = 2.0 :: Double
    it "ipw: ATE が真値 2.0 の 10% 以内 (MWC 合成、 n=2000)" $ do
      gen <- MWC.createSystemRandom >> MWC.create  -- 固定 seed
      let nC = 2000
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
          ipwR  = CIPW.ipw xMatC tVecC yVecC
      CIPW.ipwATE ipwR `shouldSatisfy` (\v -> abs (v - trueTau) < 0.1 * trueTau)
      -- 定数効果なので ATT も同程度の精度
      CIPW.ipwATT ipwR `shouldSatisfy` (\v -> abs (v - trueTau) < 0.15 * trueTau)
      -- 重み長さ + 非負
      LA.size (CIPW.ipwWeightsATE ipwR) `shouldBe` nC
      LA.toList (CIPW.ipwWeightsATE ipwR) `shouldSatisfy` all (>= 0)
      -- defaultPSTrim 適用済
      LA.toList (PS.psScores (CIPW.ipwPropensity ipwR)) `shouldSatisfy`
        all (\p -> p >= 0.01 && p <= 0.99)
    it "ipwWith: 既存 PS 再利用、 別 trim と組み合わせ可能" $ do
      -- 簡単な手書きデータ: T=1 → Y=12, T=0 → Y=10、 PS = 0.5
      -- → ATE = 2、 重み均等
      let ps  = PS.PropensityScore (LA.fromList [0.5, 0.5, 0.5, 0.5])
                                   (LA.fromList [0]) 4
          tv  = LA.fromList [1, 0, 1, 0]
          yv  = LA.fromList [12, 10, 12, 10]
          r   = CIPW.ipwWith ps tv yv
      CIPW.ipwATE r `shouldSatisfy` (\v -> abs (v - 2) < 1e-9)
      CIPW.ipwATT r `shouldSatisfy` (\v -> abs (v - 2) < 1e-9)
