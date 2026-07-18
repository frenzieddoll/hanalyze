{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Design.Custom.CompareSpec (spec) where

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
import qualified Hanalyze.Stat.ClassMetrics as CM
import qualified Hanalyze.Design.Optimal       as OPT
import qualified Hanalyze.Design.Custom.Factor     as CF
import qualified Hanalyze.Design.Custom.Model      as CM
import qualified Hanalyze.Design.Custom.Constraint as CC
import qualified Hanalyze.Design.Custom.RegionMoment as RM
import qualified Hanalyze.Design.Custom.Coordinate as CX
import qualified Hanalyze.Design.Custom.Compare    as CCMP
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Design.Custom.Compare (Phase 24-7 Design Evaluation)" $ do
    -- 2² factorial 設計 (4 行) を手動で構築、 main effects model で評価
    let f1' = CF.Factor "x1" (CF.Continuous (-1) 1) CF.Controllable
        f2' = CF.Factor "x2" (CF.Continuous (-1) 1) CF.Controllable
        modelMain = CM.Model
          [CM.TIntercept, CM.TMain "x1", CM.TMain "x2"] CM.NCoded
        modelFull2fi = CM.Model
          [CM.TIntercept, CM.TMain "x1", CM.TMain "x2"
          , CM.TInter ["x1","x2"]] CM.NCoded
        rawFact = LA.fromLists
          [[-1,-1],[1,-1],[-1,1],[1,1]]
        cdFact = CX.CustomDesign
          { CX.cdMatrix  = rawFact
          , CX.cdFactors = [f1', f2']
          , CX.cdModel   = modelMain
          , CX.cdReport  = CX.CustomDesignReport
              { CX.crCriterion = OPT.DOpt
              , CX.crCriterionValue = -64
              , CX.crIterations = 0
              , CX.crRestarts   = 0
              , CX.crConverged  = True
              , CX.crSeed       = Nothing
              }
          }
        cdFull = cdFact { CX.cdModel = modelFull2fi }
        -- 非直交な 3 行設計 (主効果と 2fi が confound): aliasNormOf > 0 を出すための材料
        rawUnbal = LA.fromLists [[-1,-1],[1,-1],[-1,1]]
        cdUnbalMain = cdFact { CX.cdMatrix = rawUnbal, CX.cdModel = modelMain }
        cdUnbalFull = cdFact { CX.cdMatrix = rawUnbal, CX.cdModel = modelFull2fi }
    it "compareDesigns: 2² factorial で D-eff ≈ 1.0 / 4 列 (D/A/G/I)" $ do
      let dc = CCMP.compareDesigns [("F2", cdFact)]
      LA.rows (CCMP.dcEffTable dc) `shouldBe` 1
      LA.cols (CCMP.dcEffTable dc) `shouldBe` 4
      (CCMP.dcEffTable dc `LA.atIndex` (0, 0)) `shouldSatisfy` (> 0.99)
    it "fdsVector: 長さ 500 ・昇順 sort" $ do
      let v = CCMP.fdsVector cdFact
      LA.size v `shouldBe` 500
      let vs = LA.toList v
      and (zipWith (<=) vs (tail vs)) `shouldBe` True
    it "aliasNormOf: 完全直交 2² factorial + main-only モデルで 2fi 部分 alias = 0、 二乗項 部分は > 0 (Phase 28-6 で Z 拡張)" $ do
      -- 2² factorial では 2fi (x1·x2) と main は orthogonal だが、
      -- x1² = 1 (constant) で intercept と完全 alias、 Phase 28-6 拡張 Z で
      -- alias_norm > 0 が正しい新動作
      CCMP.aliasNormOf cdFact `shouldSatisfy` (> 0)
    it "aliasNormOf: 非直交 3 行設計 + main-only で alias norm > 0" $ do
      CCMP.aliasNormOf cdUnbalMain `shouldSatisfy` (> 0)
    it "aliasNormOf: 2fi 含むモデル + Phase 28-6 で Z は二乗項のみ。 2² factorial で x1²=1 alias > 0" $ do
      -- Phase 28-6 拡張で Z に TPower x1 2 / TPower x2 2 が残る (model に
      -- 含まれない)、 2² factorial で x1² = 1 = intercept で完全 alias
      CCMP.aliasNormOf cdFull `shouldSatisfy` (> 0)
      -- cdUnbalFull は 3 行 × 4 列 full2fi で singular → NaN
      CCMP.aliasNormOf cdUnbalFull `shouldSatisfy` isNaN
    it "Phase 28-6 aliasNormOf: 完全 quadratic モデル (Z = 空) で alias = 0" $ do
      -- Intercept + main + 2fi + 二乗項 全部入れれば Z = 空 → alias = 0
      -- 3² 完全 factorial 9 行で X (9×6) を full rank に
      let modelFullQuad = CM.Model
            [CM.TIntercept, CM.TMain "x1", CM.TMain "x2"
            , CM.TInter ["x1","x2"]
            , CM.TPower "x1" 2, CM.TPower "x2" 2]
            CM.NCoded
          raw3by3 = LA.fromLists [[a, b] | a <- [-1, 0, 1], b <- [-1, 0, 1]]
          cdQuad = cdFact { CX.cdMatrix = raw3by3, CX.cdModel = modelFullQuad }
      CCMP.aliasNormOf cdQuad `shouldBe` 0
    it "compareDesigns: main-only vs full2fi で main 側は alias > 0、 full 側は singular (NaN) (Phase 28-6 拡張 Z で挙動変化)" $ do
      let dc = CCMP.compareDesigns
            [("unbalMain", cdUnbalMain), ("unbalFull", cdUnbalFull)]
      map fst (CCMP.dcFDS dc) `shouldBe` ["unbalMain", "unbalFull"]
      map fst (CCMP.dcAliasNorm dc) `shouldBe` ["unbalMain", "unbalFull"]
      -- 3 行 main-only: x1²/x2² と直交化されない → alias > 0
      snd (CCMP.dcAliasNorm dc !! 0) `shouldSatisfy` (> 0)
      -- 3 行 full2fi (4 列): X は singular (rank 3) → 0/0 = NaN
      snd (CCMP.dcAliasNorm dc !! 1) `shouldSatisfy` isNaN
    it "Phase 28-8 compareDesignsWithResponses: design + response から MCp/MCpk 計算" $ do
      let f1' = CF.Factor "x1" (CF.Continuous (-1) 1) CF.Controllable
          mFull = CM.Model [CM.TIntercept, CM.TMain "x1"] CM.NCoded
          raw = LA.fromLists [[-1],[1],[-1],[1]]
          cd = CX.CustomDesign
            { CX.cdMatrix = raw, CX.cdFactors = [f1'], CX.cdModel = mFull
            , CX.cdReport = CX.CustomDesignReport
                { CX.crCriterion = OPT.DOpt, CX.crCriterionValue = 0
                , CX.crIterations = 0, CX.crRestarts = 0
                , CX.crConverged = True, CX.crSeed = Nothing }
            }
          -- 2 応答変数 (y1, y2) 4 観測、 仕様 (LSL=-3, USL=3) で十分余裕あり
          y = LA.fromLists [[0.1, 0.5], [-0.2, 0.4], [0.3, -0.1], [-0.4, 0.2]]
          specs = [(-3, 3), (-3, 3)]
      let res = CCMP.compareDesignsWithResponses [("d1", cd, y, specs)]
      length (CCMP.dceMCp res) `shouldBe` 1
      case snd (head (CCMP.dceMCp res)) of
        Right v -> v `shouldSatisfy` (> 0)
        Left e  -> expectationFailure (T.unpack e)
    it "Phase 28-9 compoundGeometric: 重み 0.5/0.5 で 2 つの efficiency の幾何平均" $ do
      let comp = CCMP.compoundGeometric [(1.0, 0.8), (1.0, 0.5)]
      -- 期待値: sqrt(0.8 · 0.5) = sqrt(0.4) ≈ 0.6324555
      abs (comp - sqrt 0.4) < 1e-9 `shouldBe` True
    it "Phase 28-9 compoundGeometric: efficiency が 0 を含めば 0" $ do
      CCMP.compoundGeometric [(1.0, 0.8), (1.0, 0.0)] `shouldBe` 0
    it "Phase 28-9 dEfficiency / aEfficiency: 2² factorial main-only" $ do
      -- X = [[1,-1,-1],[1,1,-1],[1,-1,1],[1,1,1]] expand → 4×3、 X'X = 4·I_3
      -- D-eff = (det(4·I_3) / 4^3)^(1/3) = (64/64)^(1/3) = 1.0
      -- A-eff = 3 / (4 · trace((4·I_3)⁻¹)) = 3 / (4 · 3/4) = 1.0
      let x = LA.fromLists [[1,-1,-1],[1,1,-1],[1,-1,1],[1,1,1]]
      abs (CCMP.dEfficiency x - 1.0) < 1e-9 `shouldBe` True
      abs (CCMP.aEfficiency x - 1.0) < 1e-9 `shouldBe` True
    it "Phase 28-5 designEffs: BayesianD criterion 時に D 列が Bayesian D-eff に切替" $ do
      -- 同じ設計行列に対し、 cdReport.crCriterion を DOpt vs BayesianD(K) で
      -- 切り替えると、 D 列 (eff[0]) が変わる
      let f1' = CF.Factor "x1" (CF.Continuous (-1) 1) CF.Controllable
          f2' = CF.Factor "x2" (CF.Continuous (-1) 1) CF.Controllable
          mFull = CM.Model
            [CM.TIntercept, CM.TMain "x1", CM.TMain "x2", CM.TInter ["x1","x2"]]
            CM.NCoded
          raw = LA.fromLists [[-1,-1],[1,-1],[-1,1],[1,1]]
          cdBase = CX.CustomDesign
            { CX.cdMatrix  = raw
            , CX.cdFactors = [f1', f2']
            , CX.cdModel   = mFull
            , CX.cdReport  = CX.CustomDesignReport
                { CX.crCriterion = OPT.DOpt
                , CX.crCriterionValue = 0
                , CX.crIterations = 0
                , CX.crRestarts   = 0
                , CX.crConverged  = True
                , CX.crSeed       = Nothing
                }
            }
          kBayes = [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,1.0]]  -- K_jj=1 on 2fi only
          cdBayes = cdBase
            { CX.cdReport = (CX.cdReport cdBase)
                { CX.crCriterion = OPT.BayesianD kBayes } }
      let dcD = CCMP.compareDesigns [("d", cdBase)]
          dcB = CCMP.compareDesigns [("b", cdBayes)]
          dEffD = CCMP.dcEffTable dcD `LA.atIndex` (0, 0)
          dEffB = CCMP.dcEffTable dcB `LA.atIndex` (0, 0)
      -- DOpt 側 = 古典 D-eff (= 1.0 for 2² factorial)
      -- BayesianD 側 = (det(X'X + K)/n^p)^(1/p) > 古典 D-eff (K_jj=1 を足したので det 増)
      dEffB `shouldSatisfy` (> dEffD)

  describe "Hanalyze.Design.Custom.Compare (Phase 28-4a regionMomentMatrixAnalytic)" $ do
    -- 連続因子 z ∈ U[-1, 1]、 categorical K 水準等確率の解析積分
    let fc1 = CF.Factor "x1" (CF.Continuous (-1) 1) CF.Controllable
        fc2 = CF.Factor "x2" (CF.Continuous (-1) 1) CF.Controllable
        fcat = CF.Factor "c" (CF.Categorical ["A","B","C"]) CF.Controllable
        nearly a b = abs (a - b) < 1e-12
    it "TIntercept のみ: M_R = [[1]]" $ do
      let m = CM.Model [CM.TIntercept] CM.NCoded
      case CCMP.regionMomentMatrixAnalytic [fc1] m of
        Right mr -> do
          LA.rows mr `shouldBe` 1
          LA.cols mr `shouldBe` 1
          (mr `LA.atIndex` (0,0)) `shouldSatisfy` nearly 1
        Left e -> expectationFailure (show e)
    it "Intercept + 連続 main: E[1]=1, E[x]=0, E[x²]=1/3" $ do
      let m = CM.Model [CM.TIntercept, CM.TMain "x1", CM.TMain "x2"] CM.NCoded
      case CCMP.regionMomentMatrixAnalytic [fc1, fc2] m of
        Right mr -> do
          (mr `LA.atIndex` (0,0)) `shouldSatisfy` nearly 1
          (mr `LA.atIndex` (0,1)) `shouldSatisfy` nearly 0
          (mr `LA.atIndex` (0,2)) `shouldSatisfy` nearly 0
          (mr `LA.atIndex` (1,1)) `shouldSatisfy` nearly (1/3)
          (mr `LA.atIndex` (2,2)) `shouldSatisfy` nearly (1/3)
          (mr `LA.atIndex` (1,2)) `shouldSatisfy` nearly 0
        Left e -> expectationFailure (show e)
    it "TInter [x1,x2]: E[x1·x2]=0, E[(x1·x2)²]=1/9" $ do
      let m = CM.Model [CM.TIntercept, CM.TInter ["x1","x2"]] CM.NCoded
      case CCMP.regionMomentMatrixAnalytic [fc1, fc2] m of
        Right mr -> do
          (mr `LA.atIndex` (0,1)) `shouldSatisfy` nearly 0
          (mr `LA.atIndex` (1,1)) `shouldSatisfy` nearly (1/9)
        Left e -> expectationFailure (show e)
    it "TPower x1 2: E[x1²]=1/3, E[(x1²)²]=1/5" $ do
      let m = CM.Model [CM.TIntercept, CM.TPower "x1" 2] CM.NCoded
      case CCMP.regionMomentMatrixAnalytic [fc1] m of
        Right mr -> do
          (mr `LA.atIndex` (0,1)) `shouldSatisfy` nearly (1/3)
          (mr `LA.atIndex` (1,1)) `shouldSatisfy` nearly (1/5)
        Left e -> expectationFailure (show e)
    it "Categorical 3 水準 main: E[I_l]=1/3, E[I_l²]=1/3, E[I_l·I_m]=0 (l≠m)" $ do
      let m = CM.Model [CM.TIntercept, CM.TMain "c"] CM.NCoded
      case CCMP.regionMomentMatrixAnalytic [fcat] m of
        Right mr -> do
          LA.rows mr `shouldBe` 3   -- Intercept + 2 indicator cols
          (mr `LA.atIndex` (0,1)) `shouldSatisfy` nearly (1/3)
          (mr `LA.atIndex` (1,1)) `shouldSatisfy` nearly (1/3)
          (mr `LA.atIndex` (2,2)) `shouldSatisfy` nearly (1/3)
          (mr `LA.atIndex` (1,2)) `shouldSatisfy` nearly 0
        Left e -> expectationFailure (show e)
    it "Mixture 因子は Left (Phase 28-4a 非対応)" $ do
      let fm = CF.Factor "w" (CF.Mixture 0 1) CF.Controllable
          m  = CM.Model [CM.TIntercept, CM.TMain "w"] CM.NCoded
      case CCMP.regionMomentMatrixAnalytic [fm] m of
        Left _  -> pure ()
        Right _ -> expectationFailure "Mixture should be rejected"
    it "Phase 28-4c regionMomentMatrixMC: 制約無しなら analytic とほぼ一致" $ do
      let m = CM.Model [CM.TIntercept, CM.TMain "x1", CM.TPower "x1" 2] CM.NCoded
      case (CCMP.regionMomentMatrixAnalytic [fc1] m,
            RM.regionMomentMatrixMC 5000 [fc1] m []) of
        (Right mA, Right mC) -> do
          let diff i j = abs (mA `LA.atIndex` (i, j) - mC `LA.atIndex` (i, j))
          maximum [diff i j | i <- [0..2], j <- [0..2]] `shouldSatisfy` (< 0.05)
        _ -> expectationFailure "analytic or MC failed"
    it "Phase 28-4c regionMomentMatrixMC: 制約 (x1 ≥ 0) で M_R が変化" $ do
      let m    = CM.Model [CM.TIntercept, CM.TMain "x1"] CM.NCoded
          cons = [CC.LinearIneq [("x1", 1)] CC.CGeq 0]
      case RM.regionMomentMatrixMC 5000 [fc1] m cons of
        Right mC -> do
          -- 制約 x1 ≥ 0 下で coded [0, 1] uniform → E[x1] = 0.5
          (mC `LA.atIndex` (0, 1)) `shouldSatisfy` (> 0.4)
          (mC `LA.atIndex` (0, 1)) `shouldSatisfy` (< 0.6)
        Left e -> expectationFailure (T.unpack e)
    it "iValueRegionM: 2² factorial main-only で p/n と異なる値 (= 縮退脱却)" $ do
      let m = CM.Model [CM.TIntercept, CM.TMain "x1", CM.TMain "x2"] CM.NCoded
          raw = LA.fromLists [[-1,-1],[1,-1],[-1,1],[1,1]]
      case (CCMP.regionMomentMatrixAnalytic [fc1, fc2] m,
            CM.expandDesignMatrix [fc1, fc2] m raw) of
        (Right mr, Right x) -> do
          let v = CCMP.iValueRegionM mr x
          -- self-moment 版は p/n = 3/4 = 0.75。 region 版は trace(I) where
          -- 期待される値: I = (1/n) · trace((X'X)⁻¹ · M_R)? 違う、 Iは trace((X'X)⁻¹ M_R)
          -- 2² factorial で X'X = 4·I_3 → (X'X)⁻¹ = (1/4) I_3
          -- M_R = diag(1, 1/3, 1/3) → trace = (1/4)·(1 + 1/3 + 1/3) = (1/4)·(5/3) = 5/12 ≈ 0.4167
          v `shouldSatisfy` nearly (5/12)
        _ -> expectationFailure "expand or moment failed"

  describe "Hanalyze.Design.Custom.Compare (Phase 28-4b designEffs I-eff region)" $ do
    let f1' = CF.Factor "x1" (CF.Continuous (-1) 1) CF.Controllable
        f2' = CF.Factor "x2" (CF.Continuous (-1) 1) CF.Controllable
        mMain = CM.Model
          [CM.TIntercept, CM.TMain "x1", CM.TMain "x2"] CM.NCoded
        rawFact = LA.fromLists [[-1,-1],[1,-1],[-1,1],[1,1]]
        cdFact = CX.CustomDesign
          { CX.cdMatrix  = rawFact
          , CX.cdFactors = [f1', f2']
          , CX.cdModel   = mMain
          , CX.cdReport  = CX.CustomDesignReport
              { CX.crCriterion = OPT.IOpt
              , CX.crCriterionValue = 0
              , CX.crIterations = 0
              , CX.crRestarts   = 0
              , CX.crConverged  = True
              , CX.crSeed       = Nothing
              }
          }
    it "compareDesigns: I-eff (4列目) = 1/(n·trace) = 1/(4·5/12) = 0.6 (region 版)" $ do
      let dc  = CCMP.compareDesigns [("F2", cdFact)]
          iE = CCMP.dcEffTable dc `LA.atIndex` (0, 3)
      -- 旧 self-moment 版なら 1/p = 1/3 ≈ 0.333、 region 版は 0.6
      abs (iE - 0.6) < 1e-9 `shouldBe` True
