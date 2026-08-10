{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.DecisionTreeSpec (spec) where

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
import qualified Hanalyze.Model.DecisionTree as DT
import qualified Data.Text as T
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.DecisionTree" $ do
    it "fitDT: 線形分離可能なデータで perfect train accuracy" $ do
      -- y = 0 if x[0] < 5, else 1
      let xs = [[fromIntegral x] | x <- [1..10::Int]]
          ys = [if x < 5 then 0 else 1 | x <- [1..10::Int]]
          tree = DT.fitDT DT.defaultDecisionTree xs ys
          preds = map (DT.predictDT tree) xs
      preds `shouldBe` ys

    it "fitDT: 2D XOR-like パターン" $ do
      let xs = [[0, 0], [0, 1], [1, 0], [1, 1]]
          ys = [0, 1, 1, 0]  -- XOR
          tree = DT.fitDT DT.defaultDecisionTree xs ys
          preds = map (DT.predictDT tree) xs
      preds `shouldBe` ys

    it "predictDT: 混在葉は多数派 (argmax) を予測する (argmin 回帰バグ防止)" $ do
      -- depth=0 に制限し root 自身を混在葉にする。 x に依らずクラス 1 が多数 (7:3)。
      -- 旧バグ (argmin) だと少数派クラス 0 を返していた。
      let cfg  = DT.defaultDecisionTree { DT.dtMaxDepth = Just 0 }
          xs   = [[fromIntegral i] | i <- [1..10::Int]]
          ys   = [0,0,0, 1,1,1,1,1,1,1]   -- class 1 が 7、 class 0 が 3。
          tree = DT.fitDT cfg xs ys
      map (DT.predictDT tree) xs `shouldBe` replicate 10 1
      let xs = [[1.0], [2.0], [3.0]]
          ys = [0, 0, 1]
          tree = DT.fitDT DT.defaultDecisionTree xs ys
          probs = DT.predictDTProbs tree [1.5]
      -- x=1.5 should reach a leaf where most samples are class 0
      probs `shouldSatisfy` (\m -> length m >= 1)

    it "giniImpurity: 純粋クラスで 0、均等で 0.5 (2 クラス)" $ do
      DT.giniImpurity [0, 0, 0, 0] `shouldBe` 0.0
      DT.giniImpurity [1, 1, 1, 1] `shouldBe` 0.0
      DT.giniImpurity [0, 0, 1, 1] `shouldBe` 0.5

    it "maxDepth=1: shallow tree、underfit に近い" $ do
      let cfg = DT.defaultDecisionTree { DT.dtMaxDepth = Just 1 }
          xs = [[fromIntegral x, fromIntegral y]
               | x <- [1..5::Int], y <- [1..5::Int]]
          ys = [if x + y > 5 then 1 else 0
               | x <- [1..5::Int], y <- [1..5::Int]]
          tree = DT.fitDT cfg xs ys
      -- maxDepth=1 → 1 split, root = decision node
      case tree of
        DT.DNode {} -> True `shouldBe` True
        _           -> expectationFailure "Expected DNode at root"

    it "printRpart: R print.rpart 形式 (node#/split/n/loss/yval/yprob/*)" $ do
      -- 3 クラス・特徴 1 (petal_width 風) で完全分離する木。
      let xs = [[1.4,0.2],[1.3,0.2],[1.5,0.2],[1.4,0.3]
               ,[4.5,1.5],[4.7,1.4],[4.9,1.5],[4.0,1.3]
               ,[6.0,2.5],[5.8,2.2],[6.3,1.8],[5.5,2.1]]
          ys = [0,0,0,0, 1,1,1,1, 2,2,2,2]
          tree = DT.fitDT DT.defaultDecisionTree xs ys
          out  = DT.printRpartRaw ["petal_length","petal_width"]
                               ["setosa","versicolor","virginica"] tree
          ls   = lines (T.unpack out)
      -- ヘッダ (n= / legend / * denotes)。
      ls `shouldContain` ["n= 12"]
      ls `shouldContain` ["node), split, n, loss, yval, (yprob)"]
      -- root は node# 1・split=root・n=12・loss=8・yval=setosa (全クラス同数の tie)。
      ls `shouldContain` ["1) root 12 8 setosa (0.3333 0.3333 0.3333)"]
      -- 左枝 (≤・条件成立) = "name< thr"・終端 * 付き・純粋クラス確率。
      ls `shouldContain`
        ["  2) petal_width< 0.80 4 0 setosa (1.0000 0.0000 0.0000) *"]
      -- 右枝 = "name>=thr"・内部ノード (* 無し)。
      ls `shouldContain` ["  3) petal_width>=0.80 8 4 versicolor (0.0000 0.5000 0.5000)"]
      -- node# は R 慣例 root=1・子 2k/2k+1 → node 3 の子は 6/7。
      ls `shouldContain` ["    6) petal_width< 1.65 4 0 versicolor (0.0000 1.0000 0.0000) *"]
      ls `shouldContain` ["    7) petal_width>=1.65 4 0 virginica (0.0000 0.0000 1.0000) *"]

    it "printRpart: 列名・クラス名なしは f{i}/整数へフォールバック" $ do
      let xs = [[1.4,0.2],[1.3,0.2],[4.5,1.5],[4.7,1.4]]
          ys = [0,0,1,1]
          tree = DT.fitDT DT.defaultDecisionTree xs ys
          out  = DT.printRpartRaw [] [] tree
      -- 特徴 index 1 → f1、 クラス → 整数のまま。
      out `shouldSatisfy` \t -> "f" `T.isInfixOf` t
                             && "root 4 2 0" `T.isInfixOf` t

  -- ===========================================================================
  -- Hanalyze.Model.TimeSeries (Phase 11)
  -- ===========================================================================
