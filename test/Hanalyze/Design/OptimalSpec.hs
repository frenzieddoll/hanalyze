{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Design.OptimalSpec (spec) where

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
import qualified Hanalyze.Design.Optimal       as OPT
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Design.Optimal.augmentDesign (Phase 5)" $ do
    -- 2 因子の二次応答曲面候補 (intercept + x1 + x2 + x1² + x2² + x1x2)
    -- = quadraticCandidates 2 3 → 9 候補 (3×3 grid)
    let cands2 = OPT.quadraticCandidates 2 3
        -- 既存 4 行 design (適当に factorial corner を 3 個 + 中心)
        existing4 =
          [ [1.0, -1.0, -1.0, 1.0, 1.0,  1.0]
          , [1.0,  1.0, -1.0, 1.0, 1.0, -1.0]
          , [1.0, -1.0,  1.0, 1.0, 1.0, -1.0]
          , [1.0,  0.0,  0.0, 0.0, 0.0,  0.0]
          ]

    it "N=0 や 候補不足は空の new で返す" $ do
      let r0 = OPT.augmentDesign OPT.DOpt existing4 0    cands2 42
          r1 = OPT.augmentDesign OPT.DOpt existing4 1000 (take 1 cands2) 42
      OPT.arNewIndices r0 `shouldBe` []
      OPT.arNewIndices r1 `shouldBe` []
      OPT.arFullDesign r0 `shouldBe` existing4
      OPT.arFullDesign r1 `shouldBe` existing4

    it "N=2 追加で arNewRows の長さは 2、 arFullDesign は existing ++ new" $ do
      let r = OPT.augmentDesign OPT.DOpt existing4 2 cands2 42
      length (OPT.arNewIndices r) `shouldBe` 2
      length (OPT.arNewRows r)    `shouldBe` 2
      length (OPT.arFullDesign r) `shouldBe` length existing4 + 2
      take 4 (OPT.arFullDesign r) `shouldBe` existing4  -- 順序保存

    it "augment 後の |XᵀX| が augment 前 以上 (実質単調増加)" $ do
      let r = OPT.augmentDesign OPT.DOpt existing4 3 cands2 42
      -- arInitialCrit は existing 単独の |XᵀX|、 arFinalCrit は augment 後
      -- D-opt なので大きい方が良い
      OPT.arFinalCrit r `shouldSatisfy` (>= OPT.arInitialCrit r)

    it "異なる seed でも arNewRows 長さは一定、 |XᵀX| 改善は保つ" $ do
      let r1 = OPT.augmentDesign OPT.DOpt existing4 3 cands2 1
          r2 = OPT.augmentDesign OPT.DOpt existing4 3 cands2 99
      length (OPT.arNewRows r1) `shouldBe` 3
      length (OPT.arNewRows r2) `shouldBe` 3
      OPT.arFinalCrit r1 `shouldSatisfy` (>= OPT.arInitialCrit r1)
      OPT.arFinalCrit r2 `shouldSatisfy` (>= OPT.arInitialCrit r2)

    it "空 existing でも augment は機能 (= 通常の optimal design 相当)" $ do
      let r = OPT.augmentDesign OPT.DOpt [] 5 cands2 42
      length (OPT.arNewRows r) `shouldBe` 5
      OPT.arInitialCrit r `shouldBe` 0   -- 空 existing は singular扱い
      OPT.arFinalCrit r `shouldSatisfy` (> 0)

    it "選ばれた追加点は候補集合の subset" $ do
      let r = OPT.augmentDesign OPT.DOpt existing4 3 cands2 42
          newSet  = OPT.arNewRows r
          candSet = cands2
      all (`elem` candSet) newSet `shouldBe` True

  describe "Hanalyze.Design.Optimal I/E-optimal (Phase 14)" $ do
    let candidates =
          [ [1, x1, x2] | x1 <- [-1, 0, 1], x2 <- [-1, 0, 1] ]
    it "iOptimal: 9 候補から 5 設計を抽出、 fullrank" $ do
      let (idxs, des) = OPT.iOptimal candidates 5 42
      length idxs `shouldBe` 5
      length des `shouldBe` 5
    it "eOptimal: 9 候補から 5 設計を抽出" $ do
      let (idxs, _) = OPT.eOptimal candidates 5 42
      length idxs `shouldBe` 5

  describe "Hanalyze.Design.Optimal GOpt / Compound (Phase 23-a)" $ do
    let candidates =
          [ [1, x1, x2] | x1 <- [-1, 0, 1], x2 <- [-1, 0, 1] ]
    it "gOptimal: 9 候補から 5 設計を抽出 (max leverage 最小化)" $ do
      let (idxs, des) = OPT.gOptimal candidates 5 42
      length idxs `shouldBe` 5
      length des `shouldBe` 5
    it "Compound [(1, DOpt)] は DOpt と同じ design を返す" $ do
      let (idxsD, _) = OPT.dOptimal candidates 5 42
          (idxsC, _) = OPT.optimalDesign (OPT.Compound [(1.0, OPT.DOpt)]) candidates 5 42
      idxsC `shouldBe` idxsD
    it "Compound [(0.5, DOpt), (0.5, AOpt)] が n 行を選択して動作" $ do
      let (idxs, _) =
            OPT.optimalDesign
              (OPT.Compound [(0.5, OPT.DOpt), (0.5, OPT.AOpt)])
              candidates 5 42
      length idxs `shouldBe` 5

    -- Phase 78.I: n > 候補点数 のとき点の反復を許し、 きっちり n 点を返す (頭打ちしない)。
    it "optimalDesign: n > 候補点数 は点を反復して n 点を返す" $ do
      let (idxs, des) = OPT.optimalDesign OPT.DOpt candidates 12 42  -- 候補 9・n=12
      length idxs `shouldBe` 12                       -- 頭打ちせず 12 点
      length des  `shouldBe` 12
      length (nub idxs) < length idxs `shouldBe` True -- 少なくとも 1 点は反復
      all (`elem` [0 .. length candidates - 1]) idxs `shouldBe` True  -- 全て候補 index
    -- distinct が一意に D-最適な場合 (linear 2 因子・n=4 = 4 隅) は反復しない。
    -- ※真の exact D-最適ゆえ、 反復が D を上げる場合 (linear の n=5 等) は n<=nC でも反復し得る。
    it "optimalDesign: distinct が最適なら反復しない (linear n=4 = 4 隅)" $ do
      let (idxs, _) = OPT.optimalDesign OPT.DOpt candidates 4 42
      length idxs `shouldBe` 4
      length (nub idxs) `shouldBe` 4                   -- 4 隅・反復なし
