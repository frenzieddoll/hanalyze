{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Design.Custom.SplitPlotSpec (spec) where

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
import qualified Hanalyze.Design.Custom.Coordinate as CX
import qualified Hanalyze.Design.Custom.SplitPlot  as CSP
import qualified Data.Vector.Storable              as VS
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Design.Custom.SplitPlot (Phase 25-3/4)" $ do
    let fWP = CF.Factor "temp" (CF.Continuous (-1) 1) CF.HardToChange
        fSP = CF.Factor "rate" (CF.Continuous (-1) 1) CF.Controllable
        modelSP = CM.Model
          [CM.TIntercept, CM.TMain "temp", CM.TMain "rate"
          , CM.TInter ["temp","rate"]] CM.NCoded
        spec = CX.CustomDesignSpec
          { CX.cdsFactors = [fWP, fSP]
          , CX.cdsModel   = modelSP
          , CX.cdsConstraints = []
          , CX.cdsNRuns   = 8
          , CX.cdsCriterion = OPT.DOpt
          , CX.cdsBudget    = CX.defaultBudget
              { CX.dbRestarts = 3, CX.dbMaxIter = 30 }
          , CX.cdsSeed      = Just 50
          , CX.cdsInitial   = Nothing

          , CX.cdsDJConvention = False
          }
        cfg = CSP.defaultSplitPlotConfig 4   -- 4 WP × 2 runs = 8
    it "wholePlotIndicator: 8 行 / 4 WP で [0,0,1,1,2,2,3,3]" $ do
      VS.toList (CSP.wholePlotIndicator 8 4) `shouldBe` [0,0,1,1,2,2,3,3]
    it "whichRoleIsWP: HardToChange 因子の index を返す" $ do
      CSP.whichRoleIsWP [fWP, fSP] `shouldBe` [0]
      CSP.whichRoleIsWP [fSP, fWP] `shouldBe` [1]
    it "HardToChange 因子なしで Left" $ do
      let s = spec { CX.cdsFactors = [fSP, fSP] }
      r <- CSP.generateSplitPlot s cfg
      case r of Left _ -> pure (); Right _ -> expectationFailure "expected Left"
    it "spcNWhole < 1 で Left" $ do
      r <- CSP.generateSplitPlot spec (CSP.SplitPlotConfig 0 1.0 Nothing)
      case r of Left _ -> pure (); Right _ -> expectationFailure "expected Left"
    it "Phase 28-2 generateSplitPlot: strip-plot (VeryHardToChange) 因子は strip 内で constant" $ do
      let fWP    = CF.Factor "wp"    (CF.Continuous (-1) 1) CF.HardToChange
          fStrip = CF.Factor "strip" (CF.Continuous (-1) 1) CF.VeryHardToChange
          fSP    = CF.Factor "sp"    (CF.Continuous (-1) 1) CF.Controllable
          specStrip = CX.CustomDesignSpec
            { CX.cdsFactors     = [fWP, fStrip, fSP]
            , CX.cdsModel       = CM.Model
                [CM.TIntercept, CM.TMain "wp", CM.TMain "strip", CM.TMain "sp"] CM.NCoded
            , CX.cdsConstraints = []
            , CX.cdsNRuns       = 12  -- 4 WP × 3 strip
            , CX.cdsCriterion   = OPT.DOpt
            , CX.cdsBudget      = (CX.defaultBudget)
                { CX.dbRestarts = 1, CX.dbMaxIter = 5 }
            , CX.cdsSeed        = Just 3
            , CX.cdsInitial     = Nothing
            , CX.cdsDJConvention = False
            }
          cfgStrip = CSP.SplitPlotConfig 4 1.0 (Just 3)
      r <- CSP.generateSplitPlot specStrip cfgStrip
      case r of
        Left e -> expectationFailure (T.unpack e)
        Right spd -> do
          let n = 12
              wpId    = CSP.spdWholePlotId spd
              stripId = case CSP.spdSubPlotId spd of
                Just s -> s
                Nothing -> error "stripId should be Just in 28-2 strip-plot"
              wpCol    = LA.toList (LA.flatten (LA.subMatrix (0, 0) (n, 1) (CSP.spdMatrix spd)))
              stripCol = LA.toList (LA.flatten (LA.subMatrix (0, 1) (n, 1) (CSP.spdMatrix spd)))
              wpConstantInWP w =
                let vs = [wpCol !! i | i <- [0 .. n-1], wpId VS.! i == w]
                in all (\x -> abs (x - head vs) < 1e-9) vs
              stripConstantInStrip s =
                let vs = [stripCol !! i | i <- [0 .. n-1], stripId VS.! i == s]
                in all (\x -> abs (x - head vs) < 1e-9) vs
          all wpConstantInWP [0..3] `shouldBe` True
          all stripConstantInStrip [0..2] `shouldBe` True
    it "Phase 28-3 generateSplitPlot: Categorical WP 因子 (machine) も WP 内で constant" $ do
      let fMachine = CF.Factor "machine" (CF.Categorical ["A","B"]) CF.HardToChange
          fX = CF.Factor "x" (CF.Continuous (-1) 1) CF.Controllable
          specCat = CX.CustomDesignSpec
            { CX.cdsFactors     = [fMachine, fX]
            , CX.cdsModel       = CM.Model
                [CM.TIntercept, CM.TMain "machine", CM.TMain "x"] CM.NCoded
            , CX.cdsConstraints = []
            , CX.cdsNRuns       = 8
            , CX.cdsCriterion   = OPT.DOpt
            , CX.cdsBudget      = (CX.defaultBudget)
                { CX.dbRestarts = 1, CX.dbMaxIter = 5 }
            , CX.cdsSeed        = Just 7
            , CX.cdsInitial     = Nothing
            , CX.cdsDJConvention = False
            }
          cfgCat = CSP.SplitPlotConfig 4 1.0 Nothing
      r <- CSP.generateSplitPlot specCat cfgCat
      case r of
        Left e -> expectationFailure (T.unpack e)
        Right spd -> do
          let wpId = CSP.spdWholePlotId spd
              machineCol = LA.toList (LA.flatten (LA.subMatrix (0, 0) (8, 1) (CSP.spdMatrix spd)))
              perWP w = [ machineCol !! i | i <- [0 .. 7], wpId VS.! i == w ]
              constant xs = all (\x -> abs (x - head xs) < 1e-9) xs
          all constant [perWP w | w <- [0..3]] `shouldBe` True
    it "generateSplitPlot: WP 因子 (temp) は WP 内で constant" $ do
      r <- CSP.generateSplitPlot spec cfg
      case r of
        Left e -> expectationFailure (T.unpack e)
        Right spd -> do
          LA.rows (CSP.spdMatrix spd) `shouldBe` 8
          let wpId = CSP.spdWholePlotId spd
              wpCol = LA.toList (LA.flatten (LA.subMatrix (0, 0) (8, 1) (CSP.spdMatrix spd)))
          -- 各 WP 内で temp 値が一致しているか
          let perWP w = [ wpCol !! i | i <- [0 .. 7], wpId VS.! i == w ]
              constant xs = all (\x -> abs (x - head xs) < 1e-9) xs
          all constant [perWP w | w <- [0..3]] `shouldBe` True

  describe "Phase 27-2 pinned: Jones-Goos (2012) Table 2 D-criterion regression" $ do
    -- 一次根拠: Jones & Goos (2012) "I-optimal versus D-optimal split-plot
    -- response surface designs" (U Antwerp Research Paper 2012-002) Table 2、
    -- 20-run split-plot D-Optimal design (4 WP × 5 SP、 1 WP w + 1 SP s、
    -- full quadratic、 η=1)。 Phase 27-2 で hanalyze の SplitPlot
    -- D-opt 結果が同じ D-criterion (= 2684.444...) に到達することを確認済。
    -- 本テストは regression 防止 (CI 常時検証)。 bench/custom-design/REPORT.md
    -- §Phase 27-2 参照。
    let factorsJG =
          [ CF.Factor "w" (CF.Continuous (-1) 1) CF.HardToChange
          , CF.Factor "s" (CF.Continuous (-1) 1) CF.Controllable
          ]
        modelJG = CM.Model
          [ CM.TIntercept
          , CM.TMain "w", CM.TMain "s"
          , CM.TInter ["w","s"]
          , CM.TPower "w" 2, CM.TPower "s" 2
          ] CM.NCoded
        specJG = CX.CustomDesignSpec
          { CX.cdsFactors     = factorsJG
          , CX.cdsModel       = modelJG
          , CX.cdsConstraints = []
          , CX.cdsNRuns       = 20
          , CX.cdsCriterion   = OPT.DOpt
          , CX.cdsBudget      = CX.defaultBudget
          , CX.cdsSeed        = Just 42
          , CX.cdsInitial     = Nothing

          , CX.cdsDJConvention = False
          }
        cfgJG = CSP.SplitPlotConfig
          { CSP.spcNWhole = 4, CSP.spcVarRatio = 1.0, CSP.spcNStrip = Nothing }
    it "Jones-Goos golden: SplitPlot D-opt の det(X' M⁻¹ X) ≥ 2684.0" $ do
      r <- CSP.generateSplitPlot specJG cfgJG
      case r of
        Left e   -> expectationFailure (T.unpack e)
        Right sp -> do
          -- spdGEFFEst = -det(X' M⁻¹ X)、 文献値 2684.4444...
          let dval = - CSP.spdGEFFEst sp
          dval `shouldSatisfy` (>= 2684.0)
          -- 0.05% 以内で文献値と一致 (現状実測 1.0000、 retry 余地 0.0005)
          abs (dval - 2684.4444444444) `shouldSatisfy` (< 1.5)

  -- Phase 78.M M1: IO→ST pure 化。純粋版が IO 版とビット一致 (アルゴリズム不変)。
  describe "Phase 78.M M1: generateSplitPlotPure (seed 決定的 pure)" $ do
    let fWP = CF.Factor "temp" (CF.Continuous (-1) 1) CF.HardToChange
        fSP = CF.Factor "rate" (CF.Continuous (-1) 1) CF.Controllable
        modelSP = CM.Model
          [CM.TIntercept, CM.TMain "temp", CM.TMain "rate"
          , CM.TInter ["temp","rate"]] CM.NCoded
        specSP = CX.CustomDesignSpec
          { CX.cdsFactors = [fWP, fSP], CX.cdsModel = modelSP
          , CX.cdsConstraints = [], CX.cdsNRuns = 8, CX.cdsCriterion = OPT.DOpt
          , CX.cdsBudget = CX.defaultBudget { CX.dbRestarts = 3, CX.dbMaxIter = 30 }
          , CX.cdsSeed = Just 50, CX.cdsInitial = Nothing, CX.cdsDJConvention = False }
        cfgSP = CSP.defaultSplitPlotConfig 4

    it "純粋版は IO 版と同 seed でビット一致 (split-plot)" $ do
      io <- CSP.generateSplitPlot specSP cfgSP
      case (io, CSP.generateSplitPlotPure specSP cfgSP) of
        (Right a, Right b) -> do
          LA.toLists (CSP.spdMatrix a) `shouldBe` LA.toLists (CSP.spdMatrix b)
          CSP.spdGEFFEst a `shouldBe` CSP.spdGEFFEst b
          VS.toList (CSP.spdWholePlotId a) `shouldBe` VS.toList (CSP.spdWholePlotId b)
        _ -> expectationFailure "expected Right from both IO and pure"

    it "純粋版は参照透過 (同 spec を 2 回で同結果)" $ do
      case (CSP.generateSplitPlotPure specSP cfgSP, CSP.generateSplitPlotPure specSP cfgSP) of
        (Right a, Right b) ->
          LA.toLists (CSP.spdMatrix a) `shouldBe` LA.toLists (CSP.spdMatrix b)
        _ -> expectationFailure "expected Right from both pure calls"
