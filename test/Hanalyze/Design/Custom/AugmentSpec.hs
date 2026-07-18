{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Design.Custom.AugmentSpec (spec) where

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
import qualified Hanalyze.Stat.ClassMetrics as CM
import qualified Hanalyze.Design.Optimal       as OPT
import qualified Hanalyze.Design.Custom.Factor     as CF
import qualified Hanalyze.Design.Custom.Model      as CM
import qualified Hanalyze.Design.Custom.Coordinate as CX
import qualified Hanalyze.Design.Custom.Augment    as CAUG
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Design.Custom.Augment (Phase 25-6/7/8)" $ do
    let f1a = CF.Factor "x1" (CF.Continuous (-1) 1) CF.Controllable
        f2a = CF.Factor "x2" (CF.Continuous (-1) 1) CF.Controllable
        fcA = CF.Factor "cat" (CF.Categorical ["A","B"]) CF.Controllable
        modelA = CM.Model
          [CM.TIntercept, CM.TMain "x1", CM.TMain "x2"] CM.NCoded
        rawExisting = LA.fromLists [[-1,-1],[1,-1],[-1,1],[1,1]]
        baseSpec = CX.CustomDesignSpec
          { CX.cdsFactors = [f1a, f2a]
          , CX.cdsModel   = modelA
          , CX.cdsConstraints = []
          , CX.cdsNRuns   = 4
          , CX.cdsCriterion = OPT.DOpt
          , CX.cdsBudget    = CX.defaultBudget
          , CX.cdsSeed      = Just 0
          , CX.cdsInitial   = Just rawExisting

          , CX.cdsDJConvention = False
          }
    it "cdsInitial = Nothing で Left" $ do
      let s = baseSpec { CX.cdsInitial = Nothing }
      r <- CAUG.augmentMenu s (CAUG.Replicate 1)
      case r of Left _ -> pure (); Right _ -> expectationFailure "expected Left"
    it "Replicate 2: 既存 4 行を 2 回複製、 合計 12 行" $ do
      Right r <- CAUG.augmentMenu baseSpec (CAUG.Replicate 2)
      LA.rows (CAUG.amrMatrix r) `shouldBe` 12
      CAUG.amrAdded r `shouldBe` 8
      CAUG.amrMethod r `shouldBe` "Replicate"
    it "AddCenter 3: 中心 3 行追加 (全 0)" $ do
      Right r <- CAUG.augmentMenu baseSpec (CAUG.AddCenter 3)
      LA.rows (CAUG.amrMatrix r) `shouldBe` 7
      let lastRows = drop 4 (LA.toLists (CAUG.amrMatrix r))
      all (all (== 0)) lastRows `shouldBe` True
    it "AddAxial α=1.5: 連続 2 因子 → 4 axial 点追加 (2 * 2 factors)" $ do
      Right r <- CAUG.augmentMenu baseSpec (CAUG.AddAxial 1.5 False)
      LA.rows (CAUG.amrMatrix r) `shouldBe` 8
      CAUG.amrAdded r `shouldBe` 4
      let axial = drop 4 (LA.toLists (CAUG.amrMatrix r))
      -- 各 axial 行は 1 因子だけ ±1.5、 残り 0
      all (\row -> length (filter (\v -> abs v > 1e-9) row) == 1) axial `shouldBe` True
    it "Phase 28-10 AddAxial rawUnits=True: Continuous (500, 560) で center 530 ± α·30" $ do
      let fT = CF.Factor "T" (CF.Continuous 500 560) CF.Controllable
          rawExisting' = LA.fromLists [[500], [560]]
          spec' = CX.CustomDesignSpec
            { CX.cdsFactors = [fT]
            , CX.cdsModel   = CM.Model [CM.TIntercept, CM.TMain "T"] CM.NCoded
            , CX.cdsConstraints = []
            , CX.cdsNRuns   = 2
            , CX.cdsCriterion = OPT.DOpt
            , CX.cdsBudget    = CX.defaultBudget
            , CX.cdsSeed      = Just 1
            , CX.cdsInitial   = Just rawExisting'
            , CX.cdsDJConvention = False
            }
      Right r <- CAUG.augmentMenu spec' (CAUG.AddAxial 1.4 True)
      let axial = drop 2 (LA.toLists (CAUG.amrMatrix r))
      -- center=530、 half-range=30、 axial = 530 ± 1.4·30 = 530 ± 42 = [488, 572]
      axial `shouldBe` [[572], [488]]
    it "AddRuns 2: 候補集合から 2 行追加" $ do
      Right r <- CAUG.augmentMenu baseSpec (CAUG.AddRuns 2)
      LA.rows (CAUG.amrMatrix r) `shouldBe` 6
      CAUG.amrAdded r `shouldBe` 2
    it "Phase 28-7 Foldover CategoricalSwap: cat 因子 A→B / B→A を swap した行追加" $ do
      let fc = CF.Factor "c" (CF.Categorical ["A","B","C"]) CF.Controllable
          fx = CF.Factor "x" (CF.Continuous (-1) 1) CF.Controllable
          existing = LA.fromLists [[0, -1], [1, 1], [2, 0]]  -- c=A/B/C、 x=-1/1/0
          spec' = CX.CustomDesignSpec
            { CX.cdsFactors = [fc, fx]
            , CX.cdsModel   = CM.Model [CM.TIntercept] CM.NCoded
            , CX.cdsConstraints = []
            , CX.cdsNRuns   = 3
            , CX.cdsCriterion = OPT.DOpt
            , CX.cdsBudget    = CX.defaultBudget
            , CX.cdsSeed      = Just 1
            , CX.cdsInitial   = Just existing
            , CX.cdsDJConvention = False
            }
      Right r <- CAUG.augmentMenu spec'
        (CAUG.Foldover (CAUG.CategoricalSwap [("c", [("A","B"),("B","A")])]))
      let swapped = drop 3 (LA.toLists (CAUG.amrMatrix r))
      -- 期待: A(0) → B(1)、 B(1) → A(0)、 C(2) はそのまま
      -- x 列はそのまま
      swapped `shouldBe` [[1, -1], [0, 1], [2, 0]]
    it "Foldover Full: 連続 2 因子 → 全部の符号 flip した 4 行追加" $ do
      Right r <- CAUG.augmentMenu baseSpec (CAUG.Foldover CAUG.FullFoldover)
      LA.rows (CAUG.amrMatrix r) `shouldBe` 8
      let flipped = drop 4 (LA.toLists (CAUG.amrMatrix r))
      flipped `shouldBe` [[1,1],[-1,1],[1,-1],[-1,-1]]
    it "Foldover Partial [x1]: x1 のみ flip" $ do
      Right r <- CAUG.augmentMenu baseSpec
        (CAUG.Foldover (CAUG.PartialFoldover ["x1"]))
      let flipped = drop 4 (LA.toLists (CAUG.amrMatrix r))
      flipped `shouldBe` [[1,-1],[-1,-1],[1,1],[-1,1]]
    it "Replicate 0 は Left" $ do
      r <- CAUG.augmentMenu baseSpec (CAUG.Replicate 0)
      case r of Left _ -> pure (); Right _ -> expectationFailure "expected Left"
