{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.LiNGAM.DirectSpec (spec) where

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
import qualified Hanalyze.Stat.QuasiRandom  as QR
import qualified Hanalyze.Model.LiNGAM.Direct         as LNG
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.LiNGAM.Direct (Phase 36 A1)" $ do
    -- 既知 DAG: x0 → x1 → x2, x0 → x2、 非ガウシアン noise (uniform)
    -- 合成データを deterministic に作って causal order と B 行列を確認
    let mkSyntheticData :: Int -> LA.Matrix Double
        mkSyntheticData n =
          let -- low-discrepancy 列由来の非ガウシアン noise (uniform-like)
              halton b k = QR.radicalInverse b k - 0.5  -- 平均 0、 ~[-0.5,0.5]
              e0s = [ halton 2 i | i <- [1..n] ]
              e1s = [ halton 3 i | i <- [1..n] ]
              e2s = [ halton 5 i | i <- [1..n] ]
              -- 真の SEM: x0 = e0、 x1 = 0.8 x0 + e1、 x2 = 0.4 x0 + 0.6 x1 + e2
              x0s = e0s
              x1s = zipWith (\x0 e1 -> 0.8 * x0 + e1) x0s e1s
              x2s = zipWith3 (\x0 x1 e2 -> 0.4 * x0 + 0.6 * x1 + e2)
                             x0s x1s e2s
          in LA.fromColumns [LA.fromList x0s, LA.fromList x1s, LA.fromList x2s]
    it "fitDirectLiNGAM 3 変数 SEM: 因果順序を [0,1,2] と推定" $ do
      let xs  = mkSyntheticData 500
          fit = LNG.fitDirectLiNGAM LNG.defaultDirectLiNGAMConfig xs
      LNG.dlOrder fit `shouldBe` [0, 1, 2]
    it "B 行列の非ゼロ要素が真の構造と一致 (x1←x0, x2←x0, x2←x1)" $ do
      let xs  = mkSyntheticData 500
          fit = LNG.fitDirectLiNGAM LNG.defaultDirectLiNGAMConfig xs
          b   = LNG.dlB fit
      -- B[1,0] ≈ 0.8 (x1 ← 0.8 x0)
      abs (LA.atIndex b (1, 0) - 0.8) `shouldSatisfy` (< 0.1)
      -- B[2,1] ≈ 0.6 (x2 ← 0.6 x1)
      abs (LA.atIndex b (2, 1) - 0.6) `shouldSatisfy` (< 0.1)
      -- B[0,1] = 0 (x0 は x1 から影響受けない、 acyclic 構造のため)
      abs (LA.atIndex b (0, 1)) `shouldSatisfy` (< 0.1)
    it "Adjacency: 因果関係エッジ 3 本のみ True (threshold 0.05)" $ do
      let xs  = mkSyntheticData 500
          fit = LNG.fitDirectLiNGAM LNG.defaultDirectLiNGAMConfig xs
          adj = LNG.dlAdjacency fit
          -- |B| > 0.05 のエッジ数
          edges = sum [ round (LA.atIndex adj (i, j)) :: Int
                      | i <- [0..2], j <- [0..2], i /= j ]
      edges `shouldBe` 3
    it "entropyApprox: 標準正規分布の entropy ≈ (1+log 2π)/2 ≈ 1.419" $ do
      -- u が標準ガウシアン近似 (大数 + 中心極限) なら H ≈ 1.4189
      let n = 1000
          us = LA.fromList
                 [ let u = QR.radicalInverse 2 i
                       v = QR.radicalInverse 3 i
                   in sqrt (-2 * log (u + 1e-12)) * cos (2 * pi * v)
                 | i <- [1..n] ]
          uStd = LNG.standardize us
          h = LNG.entropyApprox uStd
      -- ガウシアンの真値は 1.4189385、 サンプル + 近似誤差で多少ズレ
      abs (h - 1.4189) `shouldSatisfy` (< 0.05)
    it "olsResidual: 完全相関時に残差 ≈ 0" $ do
      let x = LA.fromList [1..10 :: Double]
          y = LA.scale 2 x + LA.scalar 1  -- y = 2x + 1
          r = LNG.olsResidual y x   -- y を x で回帰した残差は constant 部分のみ
          -- residual の標準偏差 ≈ 0 (定数 1 は intercept 込み回帰で吸収されるが
          -- 本実装は no-intercept なので constant 1 が残る → variance 0 のみ)
          v = LA.norm_2 (r - LA.scalar (LA.sumElements r / 10))
      v `shouldSatisfy` (< 1e-9)
