{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Stat.SPCSpec (spec) where

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
import qualified Data.Vector as V
import qualified Data.Text   as T
import qualified Hanalyze.Stat.SPC             as SPC
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Stat.SPC X-bar / R chart (Phase 1.2)" $ do
    -- Montgomery 9th ed. Example 6.1 (Piston Ring): n=5, k=25
    -- Here we use a small synthetic but consistent set so we can check
    -- the algebra exactly.
    let -- 5 subgroups, each of size 4
        subs :: V.Vector (V.Vector Double)
        subs = V.fromList $ map V.fromList
          [ [10.0, 10.4, 10.2, 9.8]   -- mean 10.10  range 0.6
          , [9.9,  10.1, 10.3, 10.0]  -- mean 10.075 range 0.4
          , [10.2, 9.9,  10.0, 10.3]  -- mean 10.10  range 0.4
          , [9.8,  10.0, 10.1, 9.9]   -- mean 9.95   range 0.3
          , [10.1, 10.2, 10.0, 10.4]  -- mean 10.175 range 0.4
          ]
        -- Hand-computed: X̿ = mean of means; R̄ = mean of ranges
        expectedXBarBar = (10.10 + 10.075 + 10.10 + 9.95 + 10.175) / 5
        expectedRBar    = (0.6  + 0.4   + 0.4  + 0.3 + 0.4)   / 5
        -- For n=4: A2 = 0.729, D3 = 0, D4 = 2.282, d2 = 2.059
        a2_4 = 0.729 :: Double
        d3_4 = 0.000 :: Double
        d4_4 = 2.282 :: Double
        d2_4 = 2.059 :: Double

    it "fitSPC XR returns 2 charts (X-bar then R)" $
      case SPC.fitSPC SPC.XR (SPC.VarSubgroups subs) of
        Left  e  -> expectationFailure (T.unpack e)
        Right xs -> do
          length xs `shouldBe` 2
          SPC.spcChartName (xs !! 0) `shouldBe` "X-bar"
          SPC.spcChartName (xs !! 1) `shouldBe` "R"

    it "X-bar chart の center, UCL, LCL が Montgomery 公式と一致" $
      case SPC.fitSPC SPC.XR (SPC.VarSubgroups subs) of
        Left  e        -> expectationFailure (T.unpack e)
        Right (x:_:_)  -> do
          SPC.spcCenter x  `shouldSatisfy` (\v -> abs (v - expectedXBarBar) < 1e-9)
          let uclX = expectedXBarBar + a2_4 * expectedRBar
              lclX = expectedXBarBar - a2_4 * expectedRBar
          V.head (SPC.spcUCL x) `shouldSatisfy` (\v -> abs (v - uclX) < 1e-9)
          V.head (SPC.spcLCL x) `shouldSatisfy` (\v -> abs (v - lclX) < 1e-9)
        Right _ -> expectationFailure "expected 2 charts"

    it "R chart の center, UCL, LCL が Montgomery 公式と一致" $
      case SPC.fitSPC SPC.XR (SPC.VarSubgroups subs) of
        Left  e          -> expectationFailure (T.unpack e)
        Right (_:r:_)    -> do
          SPC.spcCenter r `shouldSatisfy` (\v -> abs (v - expectedRBar) < 1e-9)
          let uclR = d4_4 * expectedRBar
              lclR = d3_4 * expectedRBar
          V.head (SPC.spcUCL r) `shouldSatisfy` (\v -> abs (v - uclR) < 1e-9)
          V.head (SPC.spcLCL r) `shouldSatisfy` (\v -> abs (v - lclR) < 1e-9)
        Right _ -> expectationFailure "expected 2 charts"

    it "推定 σ = R̄ / d2(n) (両 chart で同値)" $
      case SPC.fitSPC SPC.XR (SPC.VarSubgroups subs) of
        Left e            -> expectationFailure (T.unpack e)
        Right (x:r:_)     -> do
          let expectedSigma = expectedRBar / d2_4
          SPC.spcSigma x `shouldSatisfy` (\v -> abs (v - expectedSigma) < 1e-9)
          SPC.spcSigma r `shouldBe` SPC.spcSigma x
        Right _ -> expectationFailure "expected 2 charts"

    it "subgroup size が不揃いだと Left" $
      case SPC.fitSPC SPC.XR (SPC.VarSubgroups (V.fromList
             [ V.fromList [1, 2, 3]
             , V.fromList [4, 5]  -- size mismatch
             ])) of
        Left  _ -> pure ()
        Right _ -> expectationFailure "expected Left for mismatched subgroup sizes"

    it "subgroup size が 範囲外 (1 or >15) だと Left" $ do
      case SPC.fitSPC SPC.XR (SPC.VarSubgroups (V.fromList
             [ V.fromList [1]
             , V.fromList [2]
             ])) of
        Left  _ -> pure ()
        Right _ -> expectationFailure "expected Left for n=1"
      case SPC.fitSPC SPC.XR (SPC.VarSubgroups (V.fromList
             [ V.fromList (replicate 16 1)
             , V.fromList (replicate 16 2)
             ])) of
        Left  _ -> pure ()
        Right _ -> expectationFailure "expected Left for n=16"

    it "chart kind と入力の不一致は Left" $
      case SPC.fitSPC SPC.XR (SPC.VarIndividual (V.fromList [1, 2, 3])) of
        Left  _ -> pure ()
        Right _ -> expectationFailure "expected Left for XR + VarIndividual"

  describe "Hanalyze.Stat.SPC I-MR chart (Phase 1.2)" $ do
    let xs = V.fromList [10.0, 10.2, 9.8, 10.1, 9.9, 10.3, 10.0, 9.7, 10.2, 10.1]
        -- moving range:
        -- |0.2|, |0.4|, |0.3|, |0.2|, |0.4|, |0.3|, |0.3|, |0.5|, |0.1|
        -- MR̄ = 2.7 / 9 = 0.3
        -- x̄ = sum(xs)/10 = 100.3 / 10 = 10.03
        expectedXBar = 10.03
        expectedMRBar = 0.3
        -- For n=2: d2 = 1.128, D4 = 3.267, D3 = 0
        d2_2 = 1.128 :: Double
        d4_2 = 3.267 :: Double
        expectedSigma = expectedMRBar / d2_2
        expectedUCLI = expectedXBar + 3 * expectedSigma
        expectedLCLI = expectedXBar - 3 * expectedSigma
        expectedUCLMR = d4_2 * expectedMRBar

    it "fitSPC IMR は I chart と MR chart を返す" $
      case SPC.fitSPC SPC.IMR (SPC.VarIndividual xs) of
        Left e         -> expectationFailure (T.unpack e)
        Right [iCh, mr] -> do
          SPC.spcChartName iCh `shouldBe` "I"
          SPC.spcChartName mr  `shouldBe` "MR"
        Right _ -> expectationFailure "expected 2 charts"

    it "I chart の CL / UCL / LCL が MR̄/d2(2)·3 公式と一致" $
      case SPC.fitSPC SPC.IMR (SPC.VarIndividual xs) of
        Left e            -> expectationFailure (T.unpack e)
        Right (iCh:_)     -> do
          SPC.spcCenter iCh `shouldSatisfy` (\v -> abs (v - expectedXBar) < 1e-9)
          V.head (SPC.spcUCL iCh) `shouldSatisfy` (\v -> abs (v - expectedUCLI) < 1e-9)
          V.head (SPC.spcLCL iCh) `shouldSatisfy` (\v -> abs (v - expectedLCLI) < 1e-9)
          V.length (SPC.spcPoints iCh) `shouldBe` V.length xs
        Right _ -> expectationFailure "expected at least 1 chart"

    it "MR chart の CL / UCL / LCL" $
      case SPC.fitSPC SPC.IMR (SPC.VarIndividual xs) of
        Left e            -> expectationFailure (T.unpack e)
        Right (_:mr:_)    -> do
          SPC.spcCenter mr `shouldSatisfy` (\v -> abs (v - expectedMRBar) < 1e-9)
          V.head (SPC.spcUCL mr) `shouldSatisfy` (\v -> abs (v - expectedUCLMR) < 1e-9)
          V.head (SPC.spcLCL mr) `shouldBe` 0
          V.length (SPC.spcPoints mr) `shouldBe` V.length xs - 1
        Right _ -> expectationFailure "expected 2 charts"

    it "観測 1 個以下だと Left" $ do
      case SPC.fitSPC SPC.IMR (SPC.VarIndividual (V.fromList [1.0])) of
        Left  _ -> pure ()
        Right _ -> expectationFailure "expected Left for n=1"
      case SPC.fitSPC SPC.IMR (SPC.VarIndividual V.empty) of
        Left  _ -> pure ()
        Right _ -> expectationFailure "expected Left for empty"

  describe "Hanalyze.Stat.SPC attribute charts: p / np / c / u (Phase 1.3)" $ do

    --------------------------------------------------------------------- p
    it "p chart: 一定 n では p̂ = d/n、 p̄ = Σd/Σn、 UCL/LCL は ±3·sqrt(p̄q̄/n)" $ do
      -- 全 subgroup n=100、 不良数 [5, 6, 4, 7, 3]
      let ds  = V.fromList [5,6,4,7,3]
          ns  = V.fromList [100,100,100,100,100]
          pBar = (5+6+4+7+3) / 500.0 :: Double  -- = 0.05
          se   = sqrt (pBar*(1-pBar)/100)
          ucl0 = pBar + 3*se
          lcl0 = max 0 (pBar - 3*se)
      case SPC.fitSPC SPC.P (SPC.AttrProportion ds ns) of
        Left e        -> expectationFailure (T.unpack e)
        Right [ch]    -> do
          SPC.spcChartName ch `shouldBe` "p"
          SPC.spcCenter ch `shouldSatisfy` (\v -> abs (v - pBar) < 1e-12)
          V.head (SPC.spcUCL ch) `shouldSatisfy` (\v -> abs (v - ucl0) < 1e-12)
          V.head (SPC.spcLCL ch) `shouldSatisfy` (\v -> abs (v - lcl0) < 1e-12)
          -- p̂_0 = 5/100 = 0.05
          V.head (SPC.spcPoints ch) `shouldSatisfy` (\v -> abs (v - 0.05) < 1e-12)
        Right _ -> expectationFailure "expected single chart"

    it "p chart: 可変 n では UCL_i が point ごとに違う" $
      case SPC.fitSPC SPC.P
             (SPC.AttrProportion (V.fromList [5,5,5]) (V.fromList [100,200,400])) of
        Left e        -> expectationFailure (T.unpack e)
        Right [ch]    -> do
          let ucls = V.toList (SPC.spcUCL ch)
          -- n が増えるほど UCL は CL に近づく (狭まる)
          (ucls !! 0) `shouldSatisfy` (> (ucls !! 1))
          (ucls !! 1) `shouldSatisfy` (> (ucls !! 2))
        Right _ -> expectationFailure "expected single chart"

    it "p chart: 不良数 > sample size は Left" $
      case SPC.fitSPC SPC.P
             (SPC.AttrProportion (V.fromList [10,3]) (V.fromList [5,5])) of
        Left  _ -> pure ()
        Right _ -> expectationFailure "expected Left"

    --------------------------------------------------------------------- np
    it "np chart: CL = n·p̄, σ̂ = sqrt(n·p̄·(1−p̄)), UCL/LCL = ±3σ̂" $ do
      let ds = V.fromList [5,6,4,7,3]
          n  = 100
          pBar = 25 / 500.0 :: Double  -- = 0.05
          cl   = fromIntegral n * pBar  -- = 5.0
          sigma = sqrt (fromIntegral n * pBar * (1 - pBar))
          ucl   = cl + 3 * sigma
          lcl   = max 0 (cl - 3 * sigma)
      case SPC.fitSPC SPC.NP (SPC.AttrCount ds n) of
        Left e        -> expectationFailure (T.unpack e)
        Right [ch]    -> do
          SPC.spcChartName ch `shouldBe` "np"
          SPC.spcCenter ch     `shouldSatisfy` (\v -> abs (v - cl) < 1e-12)
          V.head (SPC.spcUCL ch) `shouldSatisfy` (\v -> abs (v - ucl) < 1e-12)
          V.head (SPC.spcLCL ch) `shouldSatisfy` (\v -> abs (v - lcl) < 1e-12)
          SPC.spcSigma ch       `shouldSatisfy` (\v -> abs (v - sigma) < 1e-12)
        Right _ -> expectationFailure "expected single chart"

    it "np chart: d > n は Left" $
      case SPC.fitSPC SPC.NP (SPC.AttrCount (V.fromList [3, 11, 2]) 10) of
        Left  _ -> pure ()
        Right _ -> expectationFailure "expected Left"

    --------------------------------------------------------------------- c
    it "c chart: CL = c̄, σ̂ = sqrt(c̄), UCL = c̄ + 3·sqrt(c̄)" $ do
      let ds = V.fromList [4,5,3,6,2,5,4]
          cBar = (4+5+3+6+2+5+4) / 7.0 :: Double  -- = 29/7 ≈ 4.142857
          sigma = sqrt cBar
          ucl   = cBar + 3 * sigma
      case SPC.fitSPC SPC.C (SPC.AttrDefects ds) of
        Left e        -> expectationFailure (T.unpack e)
        Right [ch]    -> do
          SPC.spcChartName ch `shouldBe` "c"
          SPC.spcCenter ch     `shouldSatisfy` (\v -> abs (v - cBar) < 1e-12)
          V.head (SPC.spcUCL ch) `shouldSatisfy` (\v -> abs (v - ucl) < 1e-12)
          SPC.spcSigma ch       `shouldSatisfy` (\v -> abs (v - sigma) < 1e-12)
        Right _ -> expectationFailure "expected single chart"

    it "c chart: c̄ が小さければ LCL = 0 にクリップ" $
      case SPC.fitSPC SPC.C (SPC.AttrDefects (V.fromList [0,1,0,1,0])) of
        Left e        -> expectationFailure (T.unpack e)
        Right [ch]    -> V.head (SPC.spcLCL ch) `shouldBe` 0
        Right _       -> expectationFailure "expected single chart"

    --------------------------------------------------------------------- u
    it "u chart: u_i = d_i/n_i, ū = Σd/Σn, UCL_i 可変" $ do
      let ds  = V.fromList [10, 8, 12, 6]
          ns  = V.fromList [50, 40, 60, 30]
          uBar = (10+8+12+6) / fromIntegral (50+40+60+30) :: Double  -- = 36/180 = 0.2
      case SPC.fitSPC SPC.U (SPC.AttrDefectRate ds ns) of
        Left e        -> expectationFailure (T.unpack e)
        Right [ch]    -> do
          SPC.spcChartName ch `shouldBe` "u"
          SPC.spcCenter ch    `shouldSatisfy` (\v -> abs (v - uBar) < 1e-12)
          -- u_0 = 10/50 = 0.2
          V.head (SPC.spcPoints ch) `shouldSatisfy` (\v -> abs (v - 0.2) < 1e-12)
          -- UCL_i が異なる n に対して異なる
          let ucls = V.toList (SPC.spcUCL ch)
          length (filter (== head ucls) ucls) `shouldSatisfy` (< length ucls)
        Right _ -> expectationFailure "expected single chart"

    it "u chart: 系列長 mismatch は Left" $
      case SPC.fitSPC SPC.U
             (SPC.AttrDefectRate (V.fromList [1,2,3]) (V.fromList [10,20])) of
        Left  _ -> pure ()
        Right _ -> expectationFailure "expected Left"

  describe "Hanalyze.Stat.SPC Western Electric rules (Phase 1.4)" $ do
    -- 8 rules を持っているか
    it "westernElectricRules は 8 rules" $
      length SPC.westernElectricRules `shouldBe` 8
    it "rule 番号は 1..8" $
      map SPC.ruleNumber SPC.westernElectricRules `shouldBe` [1..8]

    -- 合成 chart 結果でルール個別検証
    let mkChart :: [Double] -> SPC.SPCChartResult
        mkChart pts = SPC.SPCChartResult
          { SPC.spcPoints    = V.fromList pts
          , SPC.spcCenter    = 0
          , SPC.spcUCL       = V.fromList (map (const 3.0)  pts)
          , SPC.spcLCL       = V.fromList (map (const (-3.0)) pts)
          , SPC.spcSigma     = 1
          , SPC.spcChartName = "test"
          }
        weRule n = SPC.westernElectricRules !! (n - 1)

    it "Rule 1 (>3σ): 4.0 だけ違反、 2.0 や 0 は違反しない" $
      SPC.ruleCheck (weRule 1) (mkChart [0, 1, 2.9, 4.0, -3.5, 2.0])
        `shouldBe` [3, 4]

    it "Rule 2 (3点中2点が同側>2σ): window 末尾で違反 (overlap window 各回検出)" $ do
      -- [0, +1, +1, 0]: window i=0..2 (2 positive) → index 2、
      --                  window i=1..3 (2 positive) → index 3
      SPC.ruleCheck (weRule 2) (mkChart [0, 2.5, 2.5, 0])
        `shouldBe` [2, 3]
      -- 隔てパターン: window 1..3 のみ hit
      SPC.ruleCheck (weRule 2) (mkChart [0, 2.5, 0, 2.5])
        `shouldBe` [3]

    it "Rule 3 (5点中4点が同側>1σ): 1.5,1.5,0,1.5,1.5 で末尾違反" $
      SPC.ruleCheck (weRule 3) (mkChart [1.5, 1.5, 0, 1.5, 1.5])
        `shouldBe` [4]

    it "Rule 4 (8点連続同側): 8 連続 +1 → index 7 から違反、 4 連続では無し" $ do
      SPC.ruleCheck (weRule 4) (mkChart (replicate 8 1.0))
        `shouldBe` [7]
      SPC.ruleCheck (weRule 4) (mkChart (replicate 4 1.0))
        `shouldBe` []

    it "Rule 5 (6点連続単調): 0..5 → index 5 から違反" $
      SPC.ruleCheck (weRule 5) (mkChart [0,1,2,3,4,5])
        `shouldBe` [5]

    it "Rule 6 (15点連続 1σ 以内): すべて 0.5 → index 14 以降違反" $
      take 1 (SPC.ruleCheck (weRule 6) (mkChart (replicate 15 0.5)))
        `shouldBe` [14]

    it "Rule 7 (8点連続 1σ 外): 1.5,1.5,...,1.5 8 点 → index 7" $
      SPC.ruleCheck (weRule 7) (mkChart (replicate 8 1.5))
        `shouldBe` [7]

    it "Rule 8 (14点連続交互): 交互系列で末尾違反" $
      SPC.ruleCheck (weRule 8)
        (mkChart [0,1,0,1,0,1,0,1,0,1,0,1,0,1])
        `shouldBe` [13]

    it "checkRules: 全ルール適用で SPCViolation list を返す" $ do
      -- 4σ 単発点 + 8 連続正側 → Rule 1 と Rule 4 と (1σ 外連続なので) Rule 7 が hit
      let ch = mkChart (4.0 : replicate 8 1.5)
          vs = SPC.checkRules SPC.westernElectricRules ch
      -- Rule 1 (index 0) は確実に含まれる
      (1 `elem` map SPC.vRuleNumber vs) `shouldBe` True
      -- Rule 4 (index 8, 8 連続同側 +1.5) も
      (4 `elem` map SPC.vRuleNumber vs) `shouldBe` True
      -- vChartName は元の chart の名前を保持
      all ((== "test") . SPC.vChartName) vs `shouldBe` True

    it "全 in-control データには違反が出ない" $ do
      let ch = mkChart [0.1, -0.2, 0.3, -0.1, 0.2, -0.3, 0.1, 0.0, 0.2, -0.2]
      SPC.checkRules SPC.westernElectricRules ch `shouldBe` []

  describe "Hanalyze.Stat.SPC Nelson rules (Phase 1.5)" $ do
    let mkChart' :: [Double] -> SPC.SPCChartResult
        mkChart' pts = SPC.SPCChartResult
          { SPC.spcPoints    = V.fromList pts
          , SPC.spcCenter    = 0
          , SPC.spcUCL       = V.fromList (map (const  3.0) pts)
          , SPC.spcLCL       = V.fromList (map (const (-3.0)) pts)
          , SPC.spcSigma     = 1
          , SPC.spcChartName = "test"
          }
        nelsonRule n = SPC.nelsonRules !! (n - 1)

    it "nelsonRules は 8 rules、 番号 1..8" $ do
      length SPC.nelsonRules `shouldBe` 8
      map SPC.ruleNumber SPC.nelsonRules `shouldBe` [1..8]

    it "Rule 1 (>3σ) = WE 1 と同じ検出" $
      SPC.ruleCheck (nelsonRule 1) (mkChart' [0, 4.0, 2.0, -3.5])
        `shouldBe` [1, 3]

    it "Rule 2 (9点連続同側): 8 点では未検出、 9 点目で検出" $ do
      SPC.ruleCheck (nelsonRule 2) (mkChart' (replicate 8 1.0))
        `shouldBe` []
      SPC.ruleCheck (nelsonRule 2) (mkChart' (replicate 9 1.0))
        `shouldBe` [8]

    it "Rule 2 ≠ WE 4 (8 vs 9 連続)" $ do
      -- 同じデータ (8 点連続同側) で WE 4 は hit、 Nelson 2 は miss
      let ch = mkChart' (replicate 8 1.0)
      SPC.ruleCheck (SPC.westernElectricRules !! 3) ch `shouldBe` [7]
      SPC.ruleCheck (nelsonRule 2) ch `shouldBe` []

    it "Rule 3 (6点連続単調) = WE 5" $
      SPC.ruleCheck (nelsonRule 3) (mkChart' [0,1,2,3,4,5])
        `shouldBe` [5]

    it "Rule 4 (14点連続交互) = WE 8" $
      SPC.ruleCheck (nelsonRule 4)
        (mkChart' [0,1,0,1,0,1,0,1,0,1,0,1,0,1])
        `shouldBe` [13]

    it "Rule 5 (3点中2点>2σ同側) = WE 2" $
      SPC.ruleCheck (nelsonRule 5) (mkChart' [0, 2.5, 0, 2.5])
        `shouldBe` [3]

    it "Rule 6 (5点中4点>1σ同側) = WE 3" $
      SPC.ruleCheck (nelsonRule 6) (mkChart' [1.5, 1.5, 0, 1.5, 1.5])
        `shouldBe` [4]

    it "Rule 7 (15点連続 1σ 以内) = WE 6" $
      take 1 (SPC.ruleCheck (nelsonRule 7) (mkChart' (replicate 15 0.5)))
        `shouldBe` [14]

    it "Rule 8 (8点連続 1σ 外) = WE 7" $
      SPC.ruleCheck (nelsonRule 8) (mkChart' (replicate 8 1.5))
        `shouldBe` [7]

    it "in-control データに対しては Nelson も violation 0" $ do
      let ch = mkChart' [0.1, -0.2, 0.3, -0.1, 0.2, -0.3, 0.1, 0.0, 0.2, -0.2]
      SPC.checkRules SPC.nelsonRules ch `shouldBe` []

    it "checkRules: Nelson rule で違反した点はルール名 \"Nelson N\"" $ do
      let ch = mkChart' (replicate 9 1.0)  -- Rule 2 確実 hit (9 点同側)
          vs = SPC.checkRules SPC.nelsonRules ch
      (2 `elem` map SPC.vRuleNumber vs) `shouldBe` True
      -- 違反 record の名前が "Nelson 2" 含む
      any (\v -> SPC.vRuleName v == "Nelson 2") vs `shouldBe` True

  describe "Hanalyze.Stat.SPC EWMA / CUSUM (Phase 11)" $ do
    let inControl = V.fromList [10.1, 9.9, 10.05, 9.95, 10.02, 9.98, 10.0, 10.03, 9.97, 10.01]
        shifted   = V.fromList [10.0, 10.1, 9.9, 10.05, 11.5, 11.6, 11.8, 11.7, 11.9, 12.0]
    it "EWMA: in-control 系列で λ=0.2 L=3 ですべての点が管理限界内" $
      case SPC.fitSPC SPC.EWMAChart (SPC.EWMAInput inControl 0.2 3.0 10.0 0.1) of
        Left e -> expectationFailure (T.unpack e)
        Right [c] -> do
          V.length (SPC.spcPoints c) `shouldBe` V.length inControl
          let zs   = SPC.spcPoints c
              ucls = SPC.spcUCL c
              lcls = SPC.spcLCL c
              inLim = V.izipWith
                (\i z _ ->
                   z <= V.unsafeIndex ucls i + 1e-9
                   && z >= V.unsafeIndex lcls i - 1e-9)
                zs zs
          V.and inLim `shouldBe` True
        Right _ -> expectationFailure "expected single chart"
    it "EWMA: 平均シフト系列で後半に管理限界超過点あり" $
      case SPC.fitSPC SPC.EWMAChart (SPC.EWMAInput shifted 0.2 3.0 10.0 0.1) of
        Left _ -> expectationFailure "expected Right"
        Right [c] -> do
          let zs   = SPC.spcPoints c
              ucls = SPC.spcUCL c
              anyOut = V.any id $ V.izipWith
                (\i z _ -> z > V.unsafeIndex ucls i) zs zs
          anyOut `shouldBe` True
        Right _ -> expectationFailure "expected single chart"
    it "EWMA: λ ∉ (0,1] は Left" $
      case SPC.fitSPC SPC.EWMAChart (SPC.EWMAInput inControl 1.5 3.0 10.0 0.1) of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left"
    it "EWMA: 漸近的に limit が μ₀ ± L σ √(λ/(2−λ)) に収束" $
      case SPC.fitSPC SPC.EWMAChart
             (SPC.EWMAInput (V.replicate 200 10.0) 0.2 3.0 10.0 0.1) of
        Right [c] -> do
          let lambda = 0.2
              ll     = 3.0
              s      = 0.1
              asym   = ll * s * sqrt (lambda / (2 - lambda))
              uclLast = V.last (SPC.spcUCL c)
          abs (uclLast - (10 + asym)) `shouldSatisfy` (< 1e-6)
        _ -> expectationFailure "expected single chart"

    it "CUSUM: in-control で両側とも限界内" $
      case SPC.fitSPC SPC.CUSUMChart (SPC.CUSUMInput inControl 10.0 0.1 0.5 4.0) of
        Left e -> expectationFailure (T.unpack e)
        Right [cp, cn] -> do
          SPC.spcChartName cp `shouldBe` "CUSUM+"
          SPC.spcChartName cn `shouldBe` "CUSUM-"
          let hAbs = 4.0 * 0.1
              cpPts = SPC.spcPoints cp
              cnPts = SPC.spcPoints cn  -- already negated
          V.all (<= hAbs + 1e-9)         cpPts `shouldBe` True
          V.all (>= negate hAbs - 1e-9)  cnPts `shouldBe` True
        Right _ -> expectationFailure "expected 2 charts"
    it "CUSUM: 上方シフトで C+ が h σ を超える点あり" $
      case SPC.fitSPC SPC.CUSUMChart (SPC.CUSUMInput shifted 10.0 0.1 0.5 4.0) of
        Right [cp, _] -> do
          let hAbs = 4.0 * 0.1
          V.any (> hAbs) (SPC.spcPoints cp) `shouldBe` True
        _ -> expectationFailure "expected 2 charts"
    it "CUSUM: h ≤ 0 は Left" $
      case SPC.fitSPC SPC.CUSUMChart (SPC.CUSUMInput inControl 10.0 0.1 0.5 0) of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected Left"
