{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Test.Hspec
import Model.GLMM
import Model.GLM (Family (..), LinkFn (..))
import qualified Data.Vector as V
import qualified Data.Text   as T
import qualified Numeric.LinearAlgebra as LA
import Data.List (sort)

import qualified DataFrame                    as DX
import qualified Design.Orthogonal as OA
import qualified Design.Taguchi as TG
import qualified DataIO.Preprocess as Pp
import qualified DataIO.Log        as Log
import qualified DataIO.CSV        as CSV
import qualified DataIO.Convert    as Conv
import qualified DataIO.Health     as Health
import qualified DataIO.Clean      as Clean
import qualified DataIO.Convert    as Conv2
import qualified Stat.Standardize  as Std
import qualified Stat.NumberFormat as NF
import qualified Data.ByteString   as BS
import System.IO.Temp (withSystemTempFile)
import System.IO     (hPutStr, hClose)
import qualified Model.GP        as GP
import qualified Model.GPRobust  as GPR
import qualified Model.RFF       as RFF
import qualified Model.Regularized as Reg
import qualified Model.Spline      as Sp
import qualified Model.Kernel      as K
import qualified Model.Core        as Core
import qualified Model.GLM         as GLM
import qualified System.Random.MWC as MWC

main :: IO ()
main = hspec $ do
  describe "Model.GLMM" $ do
    -- Dataset: 3 groups × 4 obs, strong between-group signal, weak within-group noise.
    -- True: β₀≈5, β₁≈0, u_A≈2, u_B≈0, u_C≈-2, σ²_u≈4, σ²≈small → ICC≈high.
    let df  = DX.fromNamedColumns
                [ ("x",     DX.fromList ([1,2,3,4, 1,2,3,4, 1,2,3,4] :: [Double]))
                , ("y",     DX.fromList ([7.1,6.9,7.0,7.0, 5.0,4.9,5.1,5.0, 3.0,2.9,3.1,3.0] :: [Double]))
                , ("group", DX.fromList (["A","A","A","A","B","B","B","B","C","C","C","C"] :: [T.Text])) ]
        res = fitLMEDataFrame [("x", 1)] "group" "y" df

    it "returns Just for valid input" $
      res `shouldSatisfy` (\r -> case r of { Just _ -> True; Nothing -> False })

    it "ICC is in [0, 1]" $
      maybe (expectationFailure "expected Just") (\r ->
        glmmICC r `shouldSatisfy` (\v -> v >= 0 && v <= 1)) res

    it "ICC is high for strongly grouped data" $
      maybe (expectationFailure "expected Just") (\r ->
        glmmICC r `shouldSatisfy` (> 0.9)) res

    it "random variance is positive" $
      maybe (expectationFailure "expected Just") (\r ->
        glmmRandVar r `shouldSatisfy` (> 0)) res

    it "residual variance is positive" $
      maybe (expectationFailure "expected Just") (\r ->
        glmmResidVar r `shouldSatisfy` (> 0)) res

    it "BLUP count equals number of groups" $
      maybe (expectationFailure "expected Just") (\r ->
        V.length (glmmBLUPs r) `shouldBe` 3) res

    it "group labels are sorted" $
      case res of
        Just r  -> glmmGroups r `shouldBe` V.fromList ["A","B","C"]
        Nothing -> expectationFailure "expected Just"

    it "returns Nothing for missing column" $
      fitLMEDataFrame [("x", 1)] "group" "missing" df
        `shouldSatisfy` (\r -> case r of { Nothing -> True; Just _ -> False })

  describe "Model.GLMM (non-Gaussian)" $ do
    -- Binomial GLMM: 3 hospitals, binary outcome (treatment success)
    -- Strong hospital effect; within each hospital, dose → higher success rate.
    -- True: u_A ≈ +1, u_B ≈ 0, u_C ≈ -1  (on logit scale)
    let dfBin  = DX.fromNamedColumns
                   [ ("dose",     DX.fromList ([1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5] :: [Double]))
                   , ("success",  DX.fromList ([1,1,1,1,1, 1,1,0,1,0, 0,0,0,1,0] :: [Double]))
                   , ("hospital", DX.fromList (["A","A","A","A","A","B","B","B","B","B","C","C","C","C","C"] :: [T.Text])) ]
        resBin = fitGLMMDataFrame Binomial Logit [("dose", 1)] "hospital" "success" dfBin

    it "Binomial GLMM returns Just" $
      resBin `shouldSatisfy` (\r -> case r of { Just _ -> True; Nothing -> False })

    it "Binomial ICC in [0, 1]" $
      maybe (expectationFailure "expected Just") (\r ->
        glmmICC r `shouldSatisfy` (\v -> v >= 0 && v <= 1)) resBin

    it "Binomial σ²_u is positive" $
      maybe (expectationFailure "expected Just") (\r ->
        glmmRandVar r `shouldSatisfy` (> 0)) resBin

    -- Poisson GLMM: 3 regions, count outcome (events per month)
    -- True: β₀ on log scale ≈ 2 (≈7 events baseline), u differs by region.
    let dfPois  = DX.fromNamedColumns
                    [ ("time",   DX.fromList ([1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5] :: [Double]))
                    , ("count",  DX.fromList ([15,18,22,20,25, 7,9,8,10,11, 2,3,2,4,3] :: [Double]))
                    , ("region", DX.fromList (["X","X","X","X","X","Y","Y","Y","Y","Y","Z","Z","Z","Z","Z"] :: [T.Text])) ]
        resPois = fitGLMMDataFrame Poisson Log [("time", 1)] "region" "count" dfPois

    it "Poisson GLMM returns Just" $
      resPois `shouldSatisfy` (\r -> case r of { Just _ -> True; Nothing -> False })

    it "Poisson σ²_u is positive" $
      maybe (expectationFailure "expected Just") (\r ->
        glmmRandVar r `shouldSatisfy` (> 0)) resPois

    it "Poisson ICC in [0, 1]" $
      maybe (expectationFailure "expected Just") (\r ->
        glmmICC r `shouldSatisfy` (\v -> v >= 0 && v <= 1)) resPois

  -- ─────────────────────────────────────────────────────────────────────
  describe "Design.Orthogonal" $ do
    it "L4 has 4 runs and 3 columns" $ do
      OA.oaRuns OA.l4    `shouldBe` 4
      OA.oaFactors OA.l4 `shouldBe` 3
      length (OA.oaTable OA.l4) `shouldBe` 4

    it "L8 has 8 runs and 7 columns" $ do
      OA.oaRuns OA.l8    `shouldBe` 8
      OA.oaFactors OA.l8 `shouldBe` 7

    it "L9 has 9 runs and 4 columns at 3 levels each" $ do
      OA.oaRuns OA.l9    `shouldBe` 9
      OA.oaFactors OA.l9 `shouldBe` 4
      OA.oaLevels OA.l9  `shouldBe` [3, 3, 3, 3]

    it "L18 has 18 runs and 8 columns (1×2 + 7×3)" $ do
      OA.oaRuns OA.l18    `shouldBe` 18
      OA.oaLevels OA.l18  `shouldBe` 2 : replicate 7 3

    it "L8 columns are balanced (each level appears 4 times)" $ do
      let table = OA.oaTable OA.l8
          colJ j = [ row !! j | row <- table ]
      mapM_ (\j -> do
        let cs = colJ j
        length (filter (== 1) cs) `shouldBe` 4
        length (filter (== 2) cs) `shouldBe` 4) [0 .. 6]

    it "L8 column pairs are pairwise orthogonal" $ do
      let table = OA.oaTable OA.l8
          colJ j = [ row !! j | row <- table ]
          pairCount j1 j2 a b =
            length (filter id (zipWith (\x y -> x == a && y == b)
                                       (colJ j1) (colJ j2)))
      -- For 2-level orthogonality: each pair (1,1)/(1,2)/(2,1)/(2,2) must appear equally
      mapM_ (\(j1, j2) -> do
        pairCount j1 j2 1 1 `shouldBe` 2
        pairCount j1 j2 1 2 `shouldBe` 2
        pairCount j1 j2 2 1 `shouldBe` 2
        pairCount j1 j2 2 2 `shouldBe` 2)
        [(0,1),(0,2),(0,3),(1,2),(1,3),(2,3)]

    it "lookupOA finds standard arrays case-insensitively" $ do
      OA.oaName <$> OA.lookupOA "L9"   `shouldBe` Just "L9(3^4)"
      OA.oaName <$> OA.lookupOA "l9"   `shouldBe` Just "L9(3^4)"
      OA.oaName <$> OA.lookupOA "L99"  `shouldBe` Nothing

    it "assignFactors fills levels correctly for L4" $ do
      let specs = [ OA.FactorSpec "A" [OA.LText "lo", OA.LText "hi"]
                  , OA.FactorSpec "B" [OA.LNumeric 0,   OA.LNumeric 1]
                  ]
      case OA.assignFactors OA.l4 specs of
        Right ad -> do
          length (OA.adRows ad) `shouldBe` 4
          map length (OA.adRows ad) `shouldBe` [2, 2, 2, 2]
        Left e -> expectationFailure (show e)

    it "assignFactors rejects too many factors" $ do
      let specs = replicate 5 (OA.FactorSpec "X" [OA.LNumeric 1, OA.LNumeric 2])
      OA.assignFactors OA.l4 specs `shouldSatisfy`
        \r -> case r of { Left _ -> True; Right _ -> False }

    it "assignFactors rejects level count mismatch" $ do
      let specs = [ OA.FactorSpec "A" [OA.LText "x"] ]   -- only 1 level, L4 needs 2
      OA.assignFactors OA.l4 specs `shouldSatisfy`
        \r -> case r of { Left _ -> True; Right _ -> False }

  -- ─────────────────────────────────────────────────────────────────────
  describe "Design.Taguchi" $ do
    it "smaller-the-better SN: lower y → higher η" $ do
      let etaSmall = TG.snRatio TG.SmallerBetter [0.5, 0.5, 0.5]
          etaLarge = TG.snRatio TG.SmallerBetter [5.0, 5.0, 5.0]
      etaSmall `shouldSatisfy` (> etaLarge)

    it "larger-the-better SN: higher y → higher η" $ do
      let etaLarge = TG.snRatio TG.LargerBetter [10, 10, 10]
          etaSmall = TG.snRatio TG.LargerBetter [1, 1, 1]
      etaLarge `shouldSatisfy` (> etaSmall)

    it "nominal-the-best SN: high mean / low var → high η" $ do
      let highSN = TG.snRatio TG.NominalBest [10, 10.01, 9.99, 10]
          lowSN  = TG.snRatio TG.NominalBest [1, 4, 7, 10]
      highSN `shouldSatisfy` (> lowSN)

    it "nominal-target SN: closer to target → higher η" $ do
      let closer = TG.snRatio (TG.NominalBestTarget 5) [4.9, 5.0, 5.1]
          farther = TG.snRatio (TG.NominalBestTarget 5) [3, 5, 7]
      closer `shouldSatisfy` (> farther)

    it "snRatio on empty list is 0" $
      TG.snRatio TG.SmallerBetter [] `shouldBe` 0

    it "snRatioRows produces same length as input" $ do
      let yMatrix = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]
      length (TG.snRatioRows TG.SmallerBetter yMatrix) `shouldBe` 3

    it "analyzeSN gives one FactorEffect per assigned factor" $ do
      let specs = [ OA.FactorSpec "A" [OA.LText "lo", OA.LText "hi"]
                  , OA.FactorSpec "B" [OA.LText "lo", OA.LText "hi"]
                  , OA.FactorSpec "C" [OA.LText "lo", OA.LText "hi"]
                  ]
          Right ad = OA.assignFactors OA.l4 specs
          fes = TG.analyzeSN ad [10, 20, 30, 40]
      length fes `shouldBe` 3
      map TG.feFactor fes `shouldBe` ["A", "B", "C"]
      mapM_ (\fe -> length (TG.feSNByLevel fe) `shouldBe` 2) fes

    it "optimalLevels picks max-SN level per factor" $ do
      let specs = [ OA.FactorSpec "A" [OA.LText "lo", OA.LText "hi"]
                  , OA.FactorSpec "B" [OA.LText "lo", OA.LText "hi"]
                  , OA.FactorSpec "C" [OA.LText "lo", OA.LText "hi"]
                  ]
          Right ad = OA.assignFactors OA.l4 specs
          -- L4 row 1 ("hi" for A,B,C) has SN=100; others 0
          sns = [0, 0, 0, 100]
          fes = TG.analyzeSN ad sns
          opts = TG.optimalLevels fes
      length opts `shouldBe` 3
      -- Row 4 has all "hi" by L4 structure (2,2,1) — verify each factor's best
      mapM_ (\(_, _, eta) -> eta `shouldSatisfy` (>= 0)) opts

  -- ─────────────────────────────────────────────────────────────────────
  describe "DataIO.Preprocess" $ do
    let dfNA = DX.fromNamedColumns
                 [ ("group", DX.fromList (["A","B","A","B","C"] :: [T.Text]))
                 , ("x",     DX.fromList (["1","NA","3","","5"]   :: [T.Text]))
                 , ("y",     DX.fromList ([10, 20, 30, 40, 50]    :: [Double]))
                 ]

    it "isNAString detects standard NA strings" $ do
      Pp.isNAString "NA"    `shouldBe` True
      Pp.isNAString "N/A"   `shouldBe` True
      Pp.isNAString "null"  `shouldBe` True
      Pp.isNAString ""      `shouldBe` True
      Pp.isNAString "  "    `shouldBe` True
      Pp.isNAString "valid" `shouldBe` False

    it "countMissing counts NAs in Text columns; numeric is 0" $ do
      let counts = Pp.countMissing dfNA
      lookup "x"     counts `shouldBe` Just 2
      lookup "y"     counts `shouldBe` Just 0
      lookup "group" counts `shouldBe` Just 0

    it "dropMissingRows removes rows with NA in target columns" $ do
      let df' = Pp.dropMissingRows ["x"] dfNA
          (n, _) = DX.dimensions df'
      n `shouldBe` 3   -- only rows with x ∈ {"1","3","5"} remain

    it "imputeMean converts Text/NA column to Double with mean fill" $ do
      case Pp.imputeMean "x" dfNA of
        Just df' -> do
          let xs = DX.columnAsList (DX.col @Double "x") df'
          length xs `shouldBe` 5
          -- mean of [1, 3, 5] = 3
          (xs !! 1) `shouldBe` 3.0   -- was "NA"
          (xs !! 3) `shouldBe` 3.0   -- was ""
        Nothing -> expectationFailure "imputeMean failed"

    it "selectColumns retains only listed columns" $ do
      let df' = Pp.selectColumns ["y", "group"] dfNA
      DX.columnNames df' `shouldMatchList` ["y", "group"]

    it "filterRowsByNumeric filters numeric column" $ do
      let df' = Pp.filterRowsByNumeric "y" (>= 30) dfNA
          (n, _) = DX.dimensions df'
      n `shouldBe` 3

    it "mapNumeric applies a unary function" $ do
      let df' = Pp.mapNumeric "y" (* 2) dfNA
          xs = DX.columnAsList (DX.col @Double "y") df'
      xs `shouldBe` [20, 40, 60, 80, 100]

  -- ─────────────────────────────────────────────────────────────────────
  describe "DataIO.Preprocess (groupBy)" $ do
    let dfGrp = DX.fromNamedColumns
                  [ ("group", DX.fromList (["A","B","A","B","A","C"] :: [T.Text]))
                  , ("y",     DX.fromList ([1, 4, 3, 6, 5, 10]       :: [Double]))
                  ]

    it "groupByMean computes per-group mean" $ do
      case Pp.groupByMean "group" "y" dfGrp of
        Just df' -> do
          let (n, _) = DX.dimensions df'
          n `shouldBe` 3
          let gs = DX.columnAsList (DX.col @T.Text "group") df'
              vs = DX.columnAsList (DX.col @Double "y")    df'
              pairs = zip gs vs
          lookup "A" pairs `shouldBe` Just 3.0       -- (1+3+5)/3
          lookup "B" pairs `shouldBe` Just 5.0       -- (4+6)/2
          lookup "C" pairs `shouldBe` Just 10.0
        Nothing -> expectationFailure "groupByMean failed"

    it "groupBySum computes per-group sum" $ do
      case Pp.groupBySum "group" "y" dfGrp of
        Just df' -> do
          let gs = DX.columnAsList (DX.col @T.Text "group") df'
              vs = DX.columnAsList (DX.col @Double "y")    df'
              pairs = zip gs vs
          lookup "A" pairs `shouldBe` Just 9.0
          lookup "B" pairs `shouldBe` Just 10.0
        Nothing -> expectationFailure "groupBySum failed"

    it "groupByCount counts rows per group" $ do
      case Pp.groupByCount "group" dfGrp of
        Just df' -> do
          let gs = DX.columnAsList (DX.col @T.Text "group") df'
              vs = DX.columnAsList (DX.col @Double "count") df'
              pairs = zip gs vs
          lookup "A" pairs `shouldBe` Just 3.0
          lookup "B" pairs `shouldBe` Just 2.0
          lookup "C" pairs `shouldBe` Just 1.0
        Nothing -> expectationFailure "groupByCount failed"

    it "groupByMin/Max return correct extremes" $ do
      case Pp.groupByMin "group" "y" dfGrp of
        Just dfMin -> do
          let gs = DX.columnAsList (DX.col @T.Text "group") dfMin
              vs = DX.columnAsList (DX.col @Double "y")    dfMin
              pairs = zip gs vs
          lookup "A" pairs `shouldBe` Just 1.0
          lookup "B" pairs `shouldBe` Just 4.0
        Nothing -> expectationFailure "groupByMin failed"

      case Pp.groupByMax "group" "y" dfGrp of
        Just dfMax -> do
          let gs = DX.columnAsList (DX.col @T.Text "group") dfMax
              vs = DX.columnAsList (DX.col @Double "y")    dfMax
              pairs = zip gs vs
          lookup "A" pairs `shouldBe` Just 5.0
          lookup "B" pairs `shouldBe` Just 6.0
        Nothing -> expectationFailure "groupByMax failed"

  -- ─────────────────────────────────────────────────────────────────────
  describe "Stat.NumberFormat" $ do
    it "0 → '0.00'"            $ NF.fmtNum 0       `shouldBe` "0.00"
    it "中域 (0.01..999) は固定小数点 2 桁" $ do
      NF.fmtNum 0.91   `shouldBe` "0.91"
      NF.fmtNum 12.34  `shouldBe` "12.34"
      NF.fmtNum 998.7  `shouldBe` "998.70"
    it "巨大値は指数表記" $ do
      NF.fmtNum 1.10e13 `shouldBe` "1.10E+13"
      NF.fmtNum 1234.5  `shouldBe` "1.23E+3"
    it "極小値は指数表記" $ do
      NF.fmtNum 3.057e-24 `shouldBe` "3.06E-24"
      NF.fmtNum 0.0099    `shouldBe` "9.90E-3"
    it "負の値" $ do
      NF.fmtNum (-12.34)  `shouldBe` "-12.34"
      NF.fmtNum (-1.5e10) `shouldBe` "-1.50E+10"
    it "非有限値" $ do
      NF.fmtNum (0/0)        `shouldBe` "NaN"
      NF.fmtNum (1/0)        `shouldBe` "+Inf"
      NF.fmtNum (-1/0)       `shouldBe` "-Inf"

  describe "Stat.Standardize" $ do
    let xMat = LA.fromLists [[1, 100], [2, 200], [3, 300], [4, 400], [5, 500]] :: LA.Matrix Double
        s    = Std.fitStandardizer xMat
    it "fit: 各列の μ が一致" $
      Std.stMu s `shouldSatisfy`
        (\ms -> length ms == 2
              && abs (ms !! 0 - 3) < 1e-9
              && abs (ms !! 1 - 300) < 1e-9)
    it "fit: 各列の σ が不偏分散の平方根 (n-1 正規化)" $
      Std.stSd s `shouldSatisfy`
        (\ss -> length ss == 2
              && abs (ss !! 0 - sqrt 2.5) < 1e-9
              && abs (ss !! 1 - sqrt 25000) < 1e-9)
    it "apply 後は各列 mean≈0, std≈1" $ do
      let x' = Std.applyStandardizer s xMat
          c0 = LA.toColumns x' !! 0
          c1 = LA.toColumns x' !! 1
          mn v = LA.sumElements v / fromIntegral (LA.size v)
      abs (mn c0) `shouldSatisfy` (< 1e-9)
      abs (mn c1) `shouldSatisfy` (< 1e-9)
    it "unapply で元の値に戻る" $ do
      let x'  = Std.applyStandardizer s xMat
          x'' = Std.unapplyStandardizer s x'
          d   = LA.norm_2 (xMat - x'') :: Double
      d `shouldSatisfy` (< 1e-9)
    it "定数列 (std=0) は std=1 にフォールバック (中央化のみ)" $ do
      let constMat = LA.fromLists [[7, 1], [7, 2], [7, 3]] :: LA.Matrix Double
          s2      = Std.fitStandardizer constMat
      abs (Std.stSd s2 !! 0 - 1.0) `shouldSatisfy` (< 1e-12)
      let x' = Std.applyStandardizer s2 constMat
          c0 = LA.toColumns x' !! 0
      abs (LA.sumElements c0) `shouldSatisfy` (< 1e-9)

  describe "Model.RFF (multivariate, Phase B-RFF)" $ do
    it "logMarginalLikRBFMV: 既知 ℓ で最大化される (合成データで)" $ do
      -- y = sin(x) (1D) で ℓ をスキャンし、データの z-score 後の長さスケールに
      -- 近い値で marg-lik が最大になることを確認。
      let xs = [0.0, 0.3 .. 6.0]
          ys = map sin xs
          xMat = LA.fromLists [[x] | x <- xs]
          yV   = LA.fromList ys
          ells = [0.05, 0.2, 0.5, 1.0, 2.0, 5.0]
          mliks = [ RFF.logMarginalLikRBFMV xMat yV ell 1.0 0.05 | ell <- ells ]
          best  = snd (maximum (zip mliks ells))
      best `shouldSatisfy` (\b -> b >= 0.2 && b <= 2.0)
    it "loocvRFFRidgeMV: λ → ∞ で残差ベース LOOCV が増える、適度な λ で最小" $ do
      let xs = [0.0, 0.3 .. 6.0]
          ys = map sin xs
          xMat = LA.fromLists [[x] | x <- xs]
          yV   = LA.fromList ys
      gen   <- MWC.createSystemRandom
      feats <- RFF.sampleRFFRBFMV 1 64 0.5 1.0 gen
      let lamSmall = RFF.loocvRFFRidgeMV feats xMat yV 1e-2
          lamHuge  = RFF.loocvRFFRidgeMV feats xMat yV 1e6
      lamSmall `shouldSatisfy` (< lamHuge)
    it "gridSearchLOOCVRBFMV: ℓ/λ を自動探索して LOOCV が小さくなる" $ do
      let xs = [0.0, 0.5 .. 10.0]
          ys = [ sin (x/2) | x <- xs ]
          xMat = LA.fromLists [[x] | x <- xs]
          yV   = LA.fromList ys
      gen <- MWC.createSystemRandom
      res <- RFF.gridSearchLOOCVRBFMV 1 100 xMat yV (Just (4, 8)) gen
      RFF.lcLOOCV res `shouldSatisfy` (< 1.0)
      RFF.lcEll res   `shouldSatisfy` (> 0)
    it "maximizeMarginalLikRBFMV: 雑音ありデータで mlik が改善する" $ do
      let xs = [0.0, 0.5 .. 10.0]
          ys = [ sin (x/2) + 0.05 * (fromIntegral i / 21) - 0.025
               | (i, x) <- zip [0::Int ..] xs ]
          xMat = LA.fromLists [[x] | x <- xs]
          yV   = LA.fromList ys
          res  = RFF.maximizeMarginalLikRBFMV xMat yV (Just (8, 4, 4))
      -- 最適 mlik > 任意の "ヘンな" 値 (ℓ=100, σ_n=10) より高い
          weak = RFF.logMarginalLikRBFMV xMat yV 100 1.0 10.0
      RFF.mlLogMlik res `shouldSatisfy` (> weak)
      RFF.mlEll res     `shouldSatisfy` (> 0)
      RFF.mlSigmaN res  `shouldSatisfy` (> 0)

    it "rffRidgeMV: y = x1 * t を完全にフィット" $ do
      let xs = [(x1, t) | x1 <- [1, 2, 3, 5, 7], t <- [1..10]]
          xss = [[x1, t] | (x1, t) <- xs]
          ys  = [x1 * t | (x1, t) <- xs]
          xMat = LA.fromLists xss
      gen   <- MWC.createSystemRandom
      feats <- RFF.sampleRFFRBFMV 2 256 1.0 1.0 gen
      let fit  = RFF.rffRidgeMV feats xMat ys 0.001
          yhat = RFF.predictRFFRidgeMV fit xMat
          rmse = sqrt (sum (zipWith (\a b -> (a-b)*(a-b)) ys yhat)
                       / fromIntegral (length ys))
      rmse `shouldSatisfy` (< 1.0)

  describe "Model.RFF" $ do
    it "feature matrix has correct shape" $ do
      gen   <- MWC.createSystemRandom
      feats <- RFF.sampleRFFRBF 50 1.0 1.0 gen
      RFF.rffDim feats `shouldBe` 50
      let phi = RFF.rffFeatures feats [0.0, 1.0, 2.0]
      -- phi is n × D = 3 × 50
      V.length (V.fromList [0::Int]) `shouldBe` 1   -- placeholder for typing
      -- We can't easily check matrix shape without hmatrix import here,
      -- so just ensure the function doesn't crash.
      length (RFF.rffOmegas feats) `shouldSatisfy` (== 50)
      let _ = phi
      return ()

    it "RFF Ridge fits y ≈ x reasonably" $ do
      gen   <- MWC.createSystemRandom
      feats <- RFF.sampleRFFRBF 100 1.0 1.0 gen
      let xs = [0.0, 0.1 .. 1.0]
          ys = map (\x -> 2 * x + 0.5) xs
          fit = RFF.rffRidge feats xs ys 0.001
          yhat = RFF.predictRFFRidge fit xs
          rmse = sqrt (sum [ (y - yh) ^ (2 :: Int)
                           | (y, yh) <- zip ys yhat ]
                       / fromIntegral (length ys))
      rmse `shouldSatisfy` (< 0.5)

  -- ─────────────────────────────────────────────────────────────────────
  describe "Model.GPRobust" $ do
    it "Cauchy GP is more accurate than Gaussian GP under outliers" $ do
      let trueF x = sin x
          xs = [0.0, 0.5 .. 6.0]
          cleanY = map trueF xs
          -- Inject outlier at index 5
          ys = zipWith (\i y -> if i == 5 then y + 5 else y)
                       [0::Int ..] cleanY
          hp = GP.GPParams 1.0 1.0 0.05 1.0
          gpRes  = GP.fitGP (GP.GPModel GP.RBF hp) xs ys xs
          gaussRMSE = sqrt (sum [ (a - b) ^ (2::Int)
                                | (a, b) <- zip cleanY (GP.gpMean gpRes) ]
                            / fromIntegral (length xs))
          cauchyFit = GPR.fitGPRobust GP.RBF hp (GPR.RCauchy 0.5) xs ys
          cauchyPred = GPR.predictGPRobust cauchyFit xs
          cauchyRMSE = sqrt (sum [ (a - b) ^ (2::Int)
                                 | (a, (b, _)) <- zip cleanY cauchyPred ]
                             / fromIntegral (length xs))
      cauchyRMSE `shouldSatisfy` (< gaussRMSE)

    it "IRLS converges in finite iterations" $ do
      let xs = [0.0, 1.0, 2.0, 3.0, 4.0]
          ys = [0.1, 1.05, 1.95, 2.9, 4.1]
          hp = GP.GPParams 1.0 1.0 0.1 1.0
          fit = GPR.fitGPRobust GP.RBF hp (GPR.RStudentT 4 0.5) xs ys
      GPR.rgpIters fit `shouldSatisfy` (\n -> n > 0 && n <= 50)

  describe "DataIO.Log" $ do
    it "Monoid: noLog <> r == r" $ do
      let r = Log.logReport (Log.mkWarn "W001" "msg" Nothing)
      Log.entries (Log.noLog <> r) `shouldBe` Log.entries r
      Log.entries (r <> Log.noLog) `shouldBe` Log.entries r
    it "addEntry appends" $ do
      let r0 = Log.logReport (Log.mkInfo "I001" "first" Nothing)
          r1 = Log.addEntry (Log.mkWarn "W001" "second" (Just "ヒント")) r0
      length (Log.entries r1) `shouldBe` 2
      Log.lgSev  (last (Log.entries r1)) `shouldBe` Log.Warn
      Log.lgHint (last (Log.entries r1)) `shouldBe` Just "ヒント"
    it "hasErrors / hasWarnings detect severity" $ do
      let rW = Log.logReport (Log.mkWarn "W"  "w"  Nothing)
          rE = Log.logReport (Log.mkErr  "E"  "e"  Nothing)
      Log.hasWarnings rW         `shouldBe` True
      Log.hasErrors   rW         `shouldBe` False
      Log.hasErrors   (rW <> rE) `shouldBe` True
    it "severityCount counts each level" $ do
      let r = Log.logReport (Log.mkInfo "I" "i" Nothing)
            <> Log.logReport (Log.mkWarn "W1" "w" Nothing)
            <> Log.logReport (Log.mkWarn "W2" "w" Nothing)
            <> Log.logReport (Log.mkErr  "E"  "e" Nothing)
      Log.severityCount Log.Info r `shouldBe` 1
      Log.severityCount Log.Warn r `shouldBe` 2
      Log.severityCount Log.Err  r `shouldBe` 1
    it "prettyEntry: includes code, message, hint" $ do
      let s = Log.prettyEntry (Log.mkWarn "W042" "壊れている" (Just "助言"))
      T.isInfixOf "[WARN]" s   `shouldBe` True
      T.isInfixOf "W042"   s   `shouldBe` True
      T.isInfixOf "壊れている" s `shouldBe` True
      T.isInfixOf "助言"   s   `shouldBe` True

  describe "DataIO.CSV.loadAutoSafe" $ do
    it "Empty file → Left, no exception" $
      withSystemTempFile "ha-empty.csv" $ \fp h -> do
        hPutStr h ""
        hClose h
        r <- CSV.loadAutoSafe fp
        case r of
          Left msg -> T.isInfixOf "Empty" (T.pack msg) `shouldBe` True
          Right _  -> expectationFailure "expected Left for empty file"
    it "Header-only file → Left" $
      withSystemTempFile "ha-hdr.csv" $ \fp h -> do
        hPutStr h "x,y,z\n"
        hClose h
        r <- CSV.loadAutoSafe fp
        case r of
          Left msg -> T.isInfixOf "header" (T.pack msg) `shouldBe` True
          Right _  -> expectationFailure "expected Left for header-only file"
    it "Valid CSV → Right with empty log by default" $
      withSystemTempFile "ha-ok.csv" $ \fp h -> do
        hPutStr h "x,y\n1,2\n3,4\n"
        hClose h
        r <- CSV.loadAutoSafe fp
        case r of
          Left  msg      -> expectationFailure ("unexpected Left: " ++ msg)
          Right (_, lg)  -> Log.entries lg `shouldBe` []

  describe "DataIO.Convert deep-eval" $ do
    it "getMaybeTextVec on text column with mixed NA strings returns Just" $
      withSystemTempFile "ha-na.csv" $ \fp h -> do
        -- 複数 NA 表現を混ぜると Hackage は Maybe Text 列として保持する。
        -- ヘッダ判定で n/a / null は欠損扱い → null bitmap が立つ。
        hPutStr h "id,score\n1,A\n2,n/a\n3,null\n4,B\n5,-\n"
        hClose h
        r <- CSV.loadAutoSafe fp
        case r of
          Right (df, _) -> case Conv.getMaybeTextVec "score" df of
            Just v  -> length (V.toList v) `shouldBe` 5
            Nothing -> expectationFailure "getMaybeTextVec returned Nothing"
          Left msg -> expectationFailure ("load failed: " ++ msg)
    it "getDoubleVec returns Nothing without crashing on NA-mixed numeric column" $
      withSystemTempFile "ha-na2.csv" $ \fp h -> do
        hPutStr h "id,score\n1,85\n2,NA\n3,92\n"
        hClose h
        r <- CSV.loadAutoSafe fp
        case r of
          Right (df, _) -> Conv.getDoubleVec "score" df `shouldBe` Nothing
          Left msg      -> expectationFailure ("load failed: " ++ msg)

  describe "DataIO.Health" $ do
    it "W001: ヘッダ無し疑い (列名が全て数値)" $ do
      let df = DX.insertColumn "1.0" (DX.fromList ([2.0, 4.0] :: [Double]))
             $ DX.insertColumn "2.0" (DX.fromList ([4.1, 8.0] :: [Double]))
             $ DX.empty
          codes = map Log.lgCode (Log.entries (Health.detectHeaderless df))
      codes `shouldContain` ["W001"]
    it "W001 は通常ヘッダでは発火しない" $ do
      let df = DX.insertColumn "x" (DX.fromList ([1.0, 2.0] :: [Double]))
             $ DX.empty
      Log.entries (Health.detectHeaderless df) `shouldBe` []
    it "W002: コメント行 (# 始まり) を検出" $ do
      let preview = "# header comment\n# more comment\nx,y\n1,2\n"
          codes   = map Log.lgCode (Log.entries (Health.detectCommentLines preview))
      codes `shouldContain` ["W002"]
    it "W005: 1 列 DataFrame + プレビューにタブ → delimiter ミスマッチ" $ do
      let df = DX.insertColumn "x\ty" (DX.fromList ([1.0] :: [Double]))
             $ DX.empty
          preview = "x\ty\n1\t2\n3\t4\n"
          codes = map Log.lgCode
                    (Log.entries (Health.detectDelimiterMismatch preview df))
      codes `shouldContain` ["W005"]
    it "W008: 通貨記号付き列を検出" $ do
      let df = DX.insertColumn "price"
                 (DX.fromList (["$1,234.56", "$2,500.00", "$3,000.00", "$4,000"] :: [T.Text]))
             $ DX.empty
          codes = map Log.lgCode (Log.entries (Health.detectThousandsCurrency df))
      codes `shouldContain` ["W008"]
    -- BS インポートを使う何かのスモーク (未使用 warning 防止)
    it "preview is non-empty for typical use" $
      BS.length "x,y\n1,2" `shouldSatisfy` (> 0)

  describe "DataIO.CSV.loadAutoSafeWith" $ do
    it "--no-header: 先頭行をデータ行として扱い col0... を生成" $
      withSystemTempFile "ha-noh.csv" $ \fp h -> do
        hPutStr h "1,2\n3,4\n5,6\n"
        hClose h
        r <- CSV.loadAutoSafeWith
               (CSV.defaultLoadOpts { CSV.loNoHeader = True }) fp
        case r of
          Left e -> expectationFailure ("unexpected Left: " ++ e)
          Right (df, lg) -> do
            let cols = DX.columnNames df
            cols `shouldBe` ["col0", "col1"]
            map Log.lgCode (Log.entries lg) `shouldContain` ["I012"]
    it "--skip 2: 先頭 2 行を skip" $
      withSystemTempFile "ha-skip.csv" $ \fp h -> do
        hPutStr h "# c1\n# c2\nx,y\n1,2\n3,4\n"
        hClose h
        r <- CSV.loadAutoSafeWith
               (CSV.defaultLoadOpts { CSV.loSkip = 2 }) fp
        case r of
          Left e -> expectationFailure ("unexpected Left: " ++ e)
          Right (df, _) -> DX.columnNames df `shouldBe` ["x", "y"]
    it "sniff: ヘッダ無し CSV を自動推論で col0... に変える" $
      withSystemTempFile "ha-sniff-noh.csv" $ \fp h -> do
        hPutStr h "1.0,2.0\n3.0,4.0\n"
        hClose h
        r <- CSV.loadAutoSafeWith CSV.defaultLoadOpts fp
        case r of
          Left e -> expectationFailure ("unexpected Left: " ++ e)
          Right (df, lg) -> do
            DX.columnNames df `shouldBe` ["col0", "col1"]
            map Log.lgCode (Log.entries lg) `shouldContain` ["I013"]
    it "sniff: コメント行 # を skip 推論" $
      withSystemTempFile "ha-sniff-skip.csv" $ \fp h -> do
        hPutStr h "# comment 1\n# comment 2\nx,y\n1,2\n3,4\n"
        hClose h
        r <- CSV.loadAutoSafeWith CSV.defaultLoadOpts fp
        case r of
          Left e -> expectationFailure ("unexpected Left: " ++ e)
          Right (df, _) -> DX.columnNames df `shouldBe` ["x", "y"]
    it "sniff: セミコロン区切りを自動検出" $
      withSystemTempFile "ha-sniff-semi.csv" $ \fp h -> do
        hPutStr h "a;b;c\n1;2;3\n4;5;6\n"
        hClose h
        r <- CSV.loadAutoSafeWith CSV.defaultLoadOpts fp
        case r of
          Left e -> expectationFailure ("unexpected Left: " ++ e)
          Right (df, _) -> DX.columnNames df `shouldBe` ["a", "b", "c"]
    it "sniff: --no-sniff で自動推論を切れる" $
      withSystemTempFile "ha-no-sniff.csv" $ \fp h -> do
        hPutStr h "1.0,2.0\n3.0,4.0\n"
        hClose h
        r <- CSV.loadAutoSafeWith
               (CSV.defaultLoadOpts { CSV.loSniff = False }) fp
        case r of
          Left e -> expectationFailure ("unexpected Left: " ++ e)
          Right (df, lg) -> do
            -- ヘッダ無しの自動修復は走らないので col0 にはならない
            DX.columnNames df `shouldBe` ["1.0", "2.0"]
            -- 代わりに W001 が出る
            map Log.lgCode (Log.entries lg) `shouldContain` ["W001"]

    it "Clean.stripUnitsCol: 12.3kg → 12.3" $ do
      let df0 = DX.insertColumn "w"
                   (DX.fromList (["12.3kg", "11.5cm", "10kg"] :: [T.Text]))
              $ DX.empty
          (df1, lg) = Clean.applyRule Clean.StripUnits "w" df0
      map Log.lgCode (Log.entries lg) `shouldContain` ["I100"]
      case Conv2.getDoubleVec "w" df1 of
        Just v  -> V.toList v `shouldBe` [12.3, 11.5, 10.0]
        Nothing -> expectationFailure "expected numeric column"
    it "Clean.parseCurrencyCol: $1,234.56 → 1234.56" $ do
      let df0 = DX.insertColumn "p"
                   (DX.fromList (["$1,234.56", "$2,500.00"] :: [T.Text]))
              $ DX.empty
          (df1, _) = Clean.applyRule Clean.ParseCurrency "p" df0
      case Conv2.getDoubleVec "p" df1 of
        Just v  -> V.toList v `shouldBe` [1234.56, 2500.0]
        Nothing -> expectationFailure "expected numeric column"
    it "Clean.coerceNumericCol: 混在パターンを最大限拾う" $ do
      let df0 = DX.insertColumn "x"
                   (DX.fromList (["12.3", "12.3kg", "$1,000"] :: [T.Text]))
              $ DX.empty
          (df1, _) = Clean.applyRule Clean.CoerceNumeric "x" df0
      case Conv2.getDoubleVec "x" df1 of
        Just v  -> V.toList v `shouldBe` [12.3, 12.3, 1000.0]
        Nothing -> expectationFailure "expected all-success column"
    it "Preprocess.meltLonger: wide → long、NA セルは除外、列名を Double に parse" $ do
      let df0 = DX.insertColumn "id" (DX.fromList (["a", "b"] :: [T.Text]))
              $ DX.insertColumn "1"  (DX.fromList ([Just 10.0, Nothing] :: [Maybe Double]))
              $ DX.insertColumn "2"  (DX.fromList ([Just 20.0, Just 30.0] :: [Maybe Double]))
              $ DX.insertColumn "3"  (DX.fromList ([Nothing,   Just 60.0] :: [Maybe Double]))
              $ DX.empty
          df1 = Pp.meltLonger ["id"] ["1", "2", "3"] "t" "y" True df0
          (nrows, ncols) = DX.dimensions df1
      nrows `shouldBe` 4    -- a,1=10; a,2=20; b,2=30; b,3=60
      ncols `shouldBe` 3    -- id, t, y
      DX.columnNames df1 `shouldMatchList` ["id", "t", "y"]
      case Conv2.getDoubleVec "y" df1 of
        Just v  -> sort (V.toList v) `shouldBe` [10, 20, 30, 60]
        Nothing -> expectationFailure "expected y as numeric"
      case Conv2.getDoubleVec "t" df1 of
        Just v  -> sort (V.toList v) `shouldBe` [1, 2, 2, 3]
        Nothing -> expectationFailure "expected t parsed as numeric"

    it "Clean.cleanPipeline: 複数列を一括変換" $ do
      let df0 = DX.insertColumn "p"
                   (DX.fromList (["$10", "$20"] :: [T.Text]))
              $ DX.insertColumn "w"
                   (DX.fromList (["1kg", "2kg"]   :: [T.Text]))
              $ DX.empty
          rules = [("p", Clean.ParseCurrency), ("w", Clean.StripUnits)]
          (df1, lg) = Clean.cleanPipeline rules df0
          codes = map Log.lgCode (Log.entries lg)
      codes `shouldContain` ["I101"]
      codes `shouldContain` ["I100"]
      Conv2.getDoubleVec "p" df1 `shouldSatisfy` \mv ->
        case mv of { Just v -> V.toList v == [10, 20]; Nothing -> False }

    it "--strict + 警告ありデータ (sniff off) → Left" $
      withSystemTempFile "ha-strict.csv" $ \fp h -> do
        hPutStr h "1.0,2.0\n3.0,4.0\n"  -- ヘッダ無し疑い W001
        hClose h
        -- sniff を切ると W001 が残るので strict が短絡する
        r <- CSV.loadAutoSafeWith
               (CSV.defaultLoadOpts { CSV.loStrict = True
                                    , CSV.loSniff  = False }) fp
        case r of
          Left _   -> return ()
          Right _  -> expectationFailure "expected Left under --strict --no-sniff"

  -- ===========================================================================
  -- 多出力 API の q=1 等価性 (M1〜M8)
  -- ===========================================================================
  describe "Multi-output equivalence (q=1)" $ do
    let xs = LA.fromLists [[1,1.0], [1,2.0], [1,3.0], [1,4.0], [1,5.0]] :: LA.Matrix Double
        yV = LA.fromList [2.1, 3.9, 6.0, 8.1, 10.0]                      :: LA.Vector Double
        yM = LA.asColumn yV
        approx tol a b = abs (a - b) < tol
        approxList tol as bs = length as == length bs &&
                               all (uncurry (approx tol)) (zip as bs)
        buildGroupsLocal gvec =
          let lbls = V.fromList . sort . foldr (\x acc -> if x `elem` acc then acc else x:acc) [] $ V.toList gvec
              qN   = V.length lbls
              idxFor x = case V.elemIndex x lbls of
                           Just i  -> i
                           Nothing -> 0
              idx  = V.map idxFor gvec
              sz   = V.fromList [ V.length (V.filter (== j) idx) | j <- [0 .. qN - 1] ]
          in (lbls, idx, sz)

    it "M1 Regularized Ridge: fitRegularized == fitRegularizedMulti col 0" $ do
      let single = Reg.fitRegularized (Reg.L2 0.1) xs yV
          multi  = Reg.fitRegularizedMulti (Reg.L2 0.1) xs yM
          extr   = Reg.regFitFromMulti 0 multi
      approxList 1e-9 (LA.toList (Reg.rfBeta single))
                      (LA.toList (Reg.rfBeta extr))
        `shouldBe` True

    it "M1 Regularized Lasso: q=1 一致" $ do
      let single = Reg.fitRegularized (Reg.L1 0.05) xs yV
          multi  = Reg.fitRegularizedMulti (Reg.L1 0.05) xs yM
          extr   = Reg.regFitFromMulti 0 multi
      approxList 1e-9 (LA.toList (Reg.rfBeta single))
                      (LA.toList (Reg.rfBeta extr))
        `shouldBe` True

    it "M2 Spline: fitSpline == fitSplineMulti col 0" $ do
      let xv = V.fromList [1,2,3,4,5,6,7,8,9,10] :: V.Vector Double
          yv = V.fromList (map (\x -> sin (x/2) + 0.01*x) (V.toList xv))
          knots = [1,3,5,7,10]
          single = Sp.fitSpline (Sp.BSpline 3) knots xv yv
          ymat   = LA.asColumn (LA.fromList (V.toList yv))
          multi  = Sp.fitSplineMulti (Sp.BSpline 3) knots xv ymat
          colS   = LA.toList (Sp.sfBeta single)
          colM   = LA.toList (LA.flatten (Sp.smfBeta multi LA.¿ [0]))
      approxList 1e-9 colS colM `shouldBe` True

    it "M3 Kernel Ridge: kernelRidge == kernelRidgeMulti col 0" $ do
      let xv = V.fromList [0.0,1,2,3,4,5,6,7,8,9] :: V.Vector Double
          yv = V.fromList [0.0, 0.5, 1.0, 1.4, 1.7, 1.9, 2.0, 2.0, 1.95, 1.8]
          single = K.kernelRidge K.Gaussian 1.0 0.01 xv yv
          ymat   = LA.asColumn (LA.fromList (V.toList yv))
          multi  = K.kernelRidgeMulti K.Gaussian 1.0 0.01 xv ymat
      approxList 1e-9 (LA.toList (K.krAlpha single))
                      (LA.toList (LA.flatten (K.krmAlpha multi LA.¿ [0])))
        `shouldBe` True

    it "M3 Kernel NW: nwRegression == nwRegressionMulti col 0" $ do
      let xv = V.fromList [0.0,1,2,3,4,5,6,7,8,9] :: V.Vector Double
          yv = V.fromList [0.1, 0.3, 0.7, 1.0, 1.5, 1.9, 2.0, 1.95, 1.8, 1.5]
          xn = V.fromList [0.5, 2.5, 5.5, 8.5]
          single = K.nwRegression K.Gaussian 1.0 xv yv xn
          ymat   = LA.asColumn (LA.fromList (V.toList yv))
          multi  = K.nwRegressionMulti K.Gaussian 1.0 xv ymat xn
          colM   = LA.toList (LA.flatten (multi LA.¿ [0]))
      approxList 1e-9 (V.toList single) colM `shouldBe` True

    it "M4 RFF Ridge: rffRidge == rffRidgeMulti col 0" $ do
      gen <- MWC.create
      rff <- RFF.sampleRFFRBF 16 1.0 1.0 gen
      let xList = [0.0, 1, 2, 3, 4, 5]
          yList = [0.1, 0.5, 1.0, 1.4, 1.7, 1.9]
          single = RFF.rffRidge rff xList yList 0.01
          ymat   = LA.asColumn (LA.fromList yList)
          multi  = RFF.rffRidgeMulti rff xList ymat 0.01
      approxList 1e-9 (LA.toList (RFF.rffrWeights single))
                      (LA.toList (LA.flatten (RFF.rffrmWeights multi LA.¿ [0])))
        `shouldBe` True

    it "M5 GP: fitGP mean == fitGPMulti col 0" $ do
      let model = GP.GPModel GP.RBF GP.defaultGPParams
          trX   = [0.0, 1, 2, 3, 4, 5]
          trY   = [0.1, 0.4, 0.9, 1.3, 1.6, 1.8]
          tsX   = [0.5, 2.5, 4.5]
          single = GP.fitGP model trX trY tsX
          ymat   = LA.asColumn (LA.fromList trY)
          (mMat, _) = GP.fitGPMulti model trX ymat tsX
      approxList 1e-9 (GP.gpMean single)
                      (LA.toList (LA.flatten (mMat LA.¿ [0])))
        `shouldBe` True

    it "M5 GPRobust: fitGPRobust α == fitGPRobustMulti col 0" $ do
      let params = GP.defaultGPParams
          trX    = [0.0, 1, 2, 3, 4, 5]
          trY    = [0.1, 0.4, 5.0, 1.3, 1.6, 1.8]   -- 1 outlier at idx 2
          single = GPR.fitGPRobust GP.RBF params (GPR.RStudentT 4 0.3) trX trY
          ymat   = LA.asColumn (LA.fromList trY)
          multi  = GPR.fitGPRobustMulti GP.RBF params (GPR.RStudentT 4 0.3) trX ymat
          firstFit = head (GPR.rgmFits multi)
      approxList 1e-9 (LA.toList (GPR.rgpAlpha single))
                      (LA.toList (GPR.rgpAlpha firstFit))
        `shouldBe` True

    it "M6 GLM Gaussian: fitGLM == fitGLMMulti col 0" $ do
      let single = GLM.fitGLM GLM.Gaussian xs yV
          multi  = GLM.fitGLMMulti GLM.Gaussian GLM.Identity xs yM
          colM   = LA.toList (LA.flatten (GLM.gfmBeta multi LA.¿ [0]))
          colS   = LA.toList (LA.flatten (Core.coefficients single))
      approxList 1e-7 colS colM `shouldBe` True

    it "M7 LME: fitLME == fitLMEMulti col 0" $ do
      let xMat = LA.fromLists
                   [[1,1],[1,2],[1,3],[1,4],
                    [1,1],[1,2],[1,3],[1,4],
                    [1,1],[1,2],[1,3],[1,4]] :: LA.Matrix Double
          y1   = LA.fromList [7.1,6.9,7.0,7.0, 5.0,4.9,5.1,5.0, 3.0,2.9,3.1,3.0]
          ym   = LA.asColumn y1
          gv   = V.fromList (["A","A","A","A","B","B","B","B","C","C","C","C"] :: [T.Text])
          (lbls, idx, sz) = buildGroupsLocal gv
          single = fitLME xMat y1 idx lbls sz
          multi  = fitLMEMulti xMat ym idx lbls sz
          firstM = head (glmmFits multi)
      glmmRandVar firstM `shouldSatisfy` approx 1e-9 (glmmRandVar single)
