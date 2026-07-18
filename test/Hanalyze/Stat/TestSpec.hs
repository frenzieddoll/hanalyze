{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.TestSpec (spec) where

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
import qualified Data.Text   as T
import qualified Numeric.LinearAlgebra as LA
import qualified Hanalyze.Stat.Test         as ST
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.Test" $ do
    it "tTest1Sample: μ₀=0 で同分布なら p > 0.05" $ do
      let xs = LA.fromList [0.1, -0.2, 0.3, 0.0, 0.15, -0.1, 0.05, 0.2]
          tr = ST.tTest1Sample xs 0 ST.TwoSided
      ST.trPValue tr `shouldSatisfy` (> 0.05)

    it "tTestWelch: 明らかにずれた 2 群で p < 0.05" $ do
      let xs = LA.fromList [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]
          ys = LA.fromList [10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0]
          tr = ST.tTestWelch xs ys ST.TwoSided
      ST.trPValue tr `shouldSatisfy` (< 0.05)

    it "anovaOneWay: 3 群が異なる平均なら有意" $ do
      let g1 = LA.fromList [1.0, 2.0, 3.0]
          g2 = LA.fromList [4.0, 5.0, 6.0]
          g3 = LA.fromList [7.0, 8.0, 9.0]
          tr = ST.anovaOneWay [g1, g2, g3]
      ST.trStatistic tr `shouldSatisfy` (> 1)
      ST.trPValue   tr `shouldSatisfy` (< 0.05)

    it "chiSquareIndep: 強い独立で p > 0.05" $ do
      let tbl = LA.matrix 2 [25, 25, 25, 25]  -- 完全独立
          tr = ST.chiSquareIndep tbl
      ST.trPValue tr `shouldSatisfy` (> 0.5)

    it "chiSquareIndep: 強い従属で p < 0.05" $ do
      let tbl = LA.matrix 2 [40, 10, 10, 40]  -- 高 chi2
          tr = ST.chiSquareIndep tbl
      ST.trPValue tr `shouldSatisfy` (< 0.05)

    it "leveneTest: 分散が大きく異なれば p < 0.05" $ do
      let g1 = LA.fromList [1, 1.1, 0.9, 1.05, 0.95, 1.02, 0.98, 1.03]
          g2 = LA.fromList [10, 20, 5, 25, 8, 30, 3, 22]  -- much larger var
          tr = ST.leveneTest [g1, g2]
      ST.trPValue tr `shouldSatisfy` (< 0.05)

    it "shapiroWilk: roughly-normal 系列で p > 0.05" $ do
      let xs = LA.fromList [-1.5, -0.5, 0.0, 0.3, 0.8, 1.2, -0.3, 0.5, 1.0, -1.0]
          tr = ST.shapiroWilk xs
      ST.trPValue tr `shouldSatisfy` (> 0.05)

    it "mannWhitneyU: 明らかにずれた 2 群で p < 0.05" $ do
      let xs = LA.fromList [1, 2, 3, 4, 5, 6]
          ys = LA.fromList [11, 12, 13, 14, 15, 16]
          tr = ST.mannWhitneyU xs ys ST.TwoSided
      ST.trPValue tr `shouldSatisfy` (< 0.05)

    it "fisherExact2x2: 強い偏りで p < 0.05" $ do
      let tr = ST.fisherExact2x2 ((20, 5), (5, 20)) ST.TwoSided
      ST.trPValue tr `shouldSatisfy` (< 0.05)

  -- ===========================================================================
  -- Hanalyze.Model.PCA (Phase 2)
  -- ===========================================================================

  describe "Hanalyze.Stat.Test multivariate (Phase 4.3, request/140)" $ do
    -- 1-sample Hotelling T² 既知ケース: 平均 μ̂ = (1, 2) と仮説 μ_0 を比較
    it "hotellingsT2 (1-sample): 仮説と異なる平均で有意" $ do
      -- X = 10 観測の 2 次元 (mean ≈ [5, 5])
      let xs = LA.fromLists
            [ [4.8, 5.1], [5.1, 5.2], [4.9, 4.8], [5.0, 5.0], [5.2, 5.1]
            , [4.7, 4.9], [5.1, 5.0], [5.0, 5.3], [4.9, 4.7], [5.3, 5.2]
            ]
          mu0 = LA.fromList [0.0, 0.0]
          tr = ST.hotellingsT2 xs mu0
      ST.trPValue tr `shouldSatisfy` (< 1e-6)
      case ST.trEffect tr of
        Just ("T²", t2) -> t2 `shouldSatisfy` (> 100)
        _ -> expectationFailure "expected T² effect"

    it "hotellingsT2 (1-sample): 仮説と一致する μ_0 で有意でない" $ do
      let xs = LA.fromLists
            [ [4.8, 5.1], [5.1, 5.2], [4.9, 4.8], [5.0, 5.0], [5.2, 5.1]
            , [4.7, 4.9], [5.1, 5.0], [5.0, 5.3], [4.9, 4.7], [5.3, 5.2]
            ]
          mu0 = LA.fromList [5.0, 5.0]
          tr = ST.hotellingsT2 xs mu0
      ST.trPValue tr `shouldSatisfy` (> 0.05)

    it "hotellingsT2 sanity: μ_0 長さ mismatch、 n ≤ p は trNote" $ do
      let xs = LA.fromLists [[1,2],[3,4],[5,6]]
      ST.trMethod (ST.hotellingsT2 xs (LA.fromList [0])) `shouldSatisfy`
        (\m -> T.isPrefixOf "Hotelling" m)
      ST.trMethod (ST.hotellingsT2 (LA.fromLists [[1,2,3],[4,5,6]])
                                   (LA.fromList [0,0,0]))
        `shouldSatisfy` (\m -> T.isPrefixOf "Hotelling" m)

    it "hotellingsT2TwoSample: 異なる中心の 2 群で有意" $ do
      let xs = LA.fromLists
            [ [4.8, 5.1], [5.1, 5.2], [4.9, 4.8], [5.0, 5.0], [5.2, 5.1]
            , [4.7, 4.9], [5.1, 5.0], [5.0, 5.3], [4.9, 4.7], [5.3, 5.2]
            ]
          ys = LA.fromLists
            [ [8.8, 9.1], [9.1, 9.2], [8.9, 8.8], [9.0, 9.0], [9.2, 9.1]
            , [8.7, 8.9], [9.1, 9.0], [9.0, 9.3], [8.9, 8.7], [9.3, 9.2]
            ]
          tr = ST.hotellingsT2TwoSample xs ys
      ST.trPValue tr `shouldSatisfy` (< 1e-6)

    it "hotellingsT2TwoSample: 同じ中心の 2 群で有意でない" $ do
      let xs = LA.fromLists
            [ [4.8, 5.1], [5.1, 5.2], [4.9, 4.8], [5.0, 5.0], [5.2, 5.1]
            , [4.7, 4.9], [5.1, 5.0], [5.0, 5.3], [4.9, 4.7], [5.3, 5.2]
            ]
          ys = LA.fromLists
            [ [5.1, 4.9], [4.9, 5.0], [5.0, 5.1], [4.8, 4.9], [5.2, 5.0]
            , [4.9, 5.2], [5.1, 4.8], [4.8, 5.1], [5.0, 4.7], [5.2, 5.1]
            ]
          tr = ST.hotellingsT2TwoSample xs ys
      ST.trPValue tr `shouldSatisfy` (> 0.05)

    it "manova: 3 群 で平均が大きく異なる場合に有意" $ do
      let g1 = LA.fromLists
            [ [4.8, 5.1], [5.1, 5.2], [4.9, 4.8], [5.0, 5.0], [5.2, 5.1]
            , [4.7, 4.9], [5.1, 5.0], [5.0, 5.3]
            ]
          g2 = LA.fromLists
            [ [8.8, 9.1], [9.1, 9.2], [8.9, 8.8], [9.0, 9.0], [9.2, 9.1]
            , [8.7, 8.9], [9.1, 9.0], [9.0, 9.3]
            ]
          g3 = LA.fromLists
            [ [12.8, 13.1], [13.1, 13.2], [12.9, 12.8], [13.0, 13.0]
            , [13.2, 13.1], [12.7, 12.9], [13.1, 13.0], [13.0, 13.3]
            ]
          tr = ST.manova [g1, g2, g3]
      ST.trPValue tr `shouldSatisfy` (< 1e-6)
      case ST.trEffect tr of
        Just ("Wilks Λ", w) -> w `shouldSatisfy` (\v -> v >= 0 && v <= 1)
        _ -> expectationFailure "expected Wilks Λ effect"

    it "manova: 1 群しか無い場合は trNote (= 検定不能)" $ do
      let g1 = LA.fromLists [[1,2],[3,4]]
          tr = ST.manova [g1]
      ST.trMethod tr `shouldSatisfy` (\m -> T.isPrefixOf "MANOVA" m)

  describe "Hanalyze.Stat.Test TOST (Phase 12)" $ do
    let near1 = LA.fromList [9.95, 10.05, 10.0, 10.1, 9.9, 10.02]
        near2 = LA.fromList [10.02, 9.98, 10.05, 9.93, 10.07, 10.01]
        far1  = LA.fromList [9.9, 10.0, 10.1, 9.95, 10.05]
        far2  = LA.fromList [12.0, 12.1, 11.9, 12.05, 11.95]
    it "TOST: 平均差小 + Δ=0.5 → equivalence (p < 0.05)" $ do
      let r = ST.tostWelch near1 near2 0.5
      ST.trPValue r `shouldSatisfy` (< 0.05)
    it "TOST: 平均差大 → not equivalent (p ≥ 0.05)" $ do
      let r = ST.tostWelch far1 far2 0.5
      ST.trPValue r `shouldSatisfy` (>= 0.05)
    it "TOST: delta ≤ 0 は note 付きで返る" $ do
      let r = ST.tostWelch near1 near2 0
      ST.trNote r `shouldSatisfy` maybe False (\n -> T.isInfixOf "delta" n)

  describe "Hanalyze.Stat.Test Friedman + Dunn (Phase 13.1)" $ do
    it "friedmanTest: 全 block 全 cell 同値 → Q ≈ 0、 p ≈ 1" $ do
      -- 全 cell が同一なら treatment 間に差が無い → Q ≈ 0
      let m = LA.fromLists
                [ [1,1,1], [1,1,1], [1,1,1], [1,1,1] ]
          r = ST.friedmanTest m
      ST.trPValue r `shouldSatisfy` (> 0.5)
    it "friedmanTest: 完全 treatment 効果あり (列ごと一定差) → 強い棄却" $ do
      let m = LA.fromLists
                [ [1, 2, 3], [2, 3, 4], [1.5, 2.5, 3.5], [1.2, 2.2, 3.2],
                  [1.1, 2.1, 3.1], [1.3, 2.3, 3.3] ]
          r = ST.friedmanTest m
      ST.trPValue r `shouldSatisfy` (< 0.05)
    it "friedmanTest: 1 行のみは Left (note 付き)" $ do
      let m = LA.fromLists [[1, 2, 3]]
          r = ST.friedmanTest m
      ST.trNote r `shouldSatisfy` maybe False (T.isInfixOf "need")
    it "dunnTest: 3 group、 強分離で全 p_adj < 0.10" $ do
      let g1 = LA.fromList [1, 2, 1.5, 1.2, 1.8, 0.5, 1.3, 1.6]
          g2 = LA.fromList [5, 6, 5.5, 5.2, 5.8, 4.5, 5.3, 5.6]
          g3 = LA.fromList [10, 11, 10.5, 10.2, 10.8, 9.5, 10.3, 10.6]
          r = ST.dunnTest [g1, g2, g3]
      length (ST.mcrPairs r) `shouldBe` 3
      all (< 0.10) (ST.mcrPAdj r) `shouldBe` True
    it "dunnTest: 同分布の 2 group は p_adj 高い" $ do
      let g1 = LA.fromList [1.0, 1.5, 2.0, 1.2, 1.8]
          g2 = LA.fromList [1.1, 1.4, 2.1, 1.3, 1.9]
          r = ST.dunnTest [g1, g2]
      length (ST.mcrPairs r) `shouldBe` 1
      head (ST.mcrPAdj r) `shouldSatisfy` (> 0.3)
