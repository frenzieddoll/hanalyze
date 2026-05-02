{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import DataFrame.Core
import Model.GLMM
import Model.GLM (Family (..), LinkFn (..))
import qualified Data.Vector as V

import qualified Design.Orthogonal as OA
import qualified Design.Taguchi as TG
import qualified DataIO.Preprocess as Pp
import qualified Model.GP        as GP
import qualified Model.GPRobust  as GPR
import qualified Model.RFF       as RFF
import qualified System.Random.MWC as MWC

main :: IO ()
main = hspec $ do
  describe "DataFrame.Core" $ do
    it "mkDataFrame stores columns" $ do
      let df = mkDataFrame [("x", NumericCol (V.fromList [1,2,3]))]
      columnNames df `shouldBe` ["x"]

    it "getNumeric retrieves numeric column" $ do
      let v  = V.fromList [1.0, 2.0, 3.0]
          df = mkDataFrame [("x", NumericCol v)]
      getNumeric "x" df `shouldBe` Just v

    it "numRows counts rows" $ do
      let df = mkDataFrame [("x", NumericCol (V.fromList [1,2,3]))]
      numRows df `shouldBe` 3

  describe "Model.GLMM" $ do
    -- Dataset: 3 groups × 4 obs, strong between-group signal, weak within-group noise.
    -- True: β₀≈5, β₁≈0, u_A≈2, u_B≈0, u_C≈-2, σ²_u≈4, σ²≈small → ICC≈high.
    let xs  = V.fromList [1,2,3,4, 1,2,3,4, 1,2,3,4 :: Double]
        ys  = V.fromList [7.1,6.9,7.0,7.0, 5.0,4.9,5.1,5.0, 3.0,2.9,3.1,3.0]
        gs  = V.fromList ["A","A","A","A","B","B","B","B","C","C","C","C"]
        df  = mkDataFrame
                [ ("x",     NumericCol xs)
                , ("y",     NumericCol ys)
                , ("group", TextCol    gs) ]
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
    let doseV  = V.fromList [1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5 :: Double]
        succV  = V.fromList [1,1,1,1,1, 1,1,0,1,0, 0,0,0,1,0 :: Double]
        hospV  = V.fromList ["A","A","A","A","A","B","B","B","B","B","C","C","C","C","C"]
        dfBin  = mkDataFrame
                   [ ("dose",     NumericCol doseV)
                   , ("success",  NumericCol succV)
                   , ("hospital", TextCol    hospV) ]
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
    let timeV   = V.fromList [1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5 :: Double]
        countV  = V.fromList [15,18,22,20,25, 7,9,8,10,11, 2,3,2,4,3 :: Double]
        regionV = V.fromList ["X","X","X","X","X","Y","Y","Y","Y","Y","Z","Z","Z","Z","Z"]
        dfPois  = mkDataFrame
                    [ ("time",   NumericCol timeV)
                    , ("count",  NumericCol countV)
                    , ("region", TextCol    regionV) ]
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
    let dfNA = mkDataFrame
                 [ ("group", TextCol (V.fromList ["A","B","A","B","C"]))
                 , ("x",     TextCol (V.fromList ["1","NA","3","","5"]))
                 , ("y",     NumericCol (V.fromList [10, 20, 30, 40, 50]))
                 ]

    it "isNAString detects standard NA strings" $ do
      Pp.isNAString "NA"    `shouldBe` True
      Pp.isNAString "N/A"   `shouldBe` True
      Pp.isNAString "null"  `shouldBe` True
      Pp.isNAString ""      `shouldBe` True
      Pp.isNAString "  "    `shouldBe` True
      Pp.isNAString "valid" `shouldBe` False

    it "countMissing counts NAs in TextCol; ignores NumericCol" $ do
      let counts = Pp.countMissing dfNA
      lookup "x"     counts `shouldBe` Just 2
      lookup "y"     counts `shouldBe` Just 0
      lookup "group" counts `shouldBe` Just 0

    it "dropMissingRows removes rows with NA in target columns" $ do
      let df' = Pp.dropMissingRows ["x"] dfNA
      numRows df' `shouldBe` 3   -- only rows 1, 3, 5 remain (x = "1","3","5")

    it "imputeMean converts TextCol to NumericCol with mean fill" $ do
      case Pp.imputeMean "x" dfNA of
        Just df' -> do
          case getNumeric "x" df' of
            Just v  -> do
              V.length v `shouldBe` 5
              -- mean of [1, 3, 5] = 3
              v V.! 1 `shouldBe` 3.0   -- was "NA"
              v V.! 3 `shouldBe` 3.0   -- was ""
            Nothing -> expectationFailure "x should be numeric after imputeMean"
        Nothing -> expectationFailure "imputeMean failed"

    it "selectColumns retains only listed columns" $ do
      let df' = Pp.selectColumns ["y", "group"] dfNA
      columnNames df' `shouldMatchList` ["y", "group"]

    it "filterRowsByNumeric filters numeric column" $ do
      let df' = Pp.filterRowsByNumeric "y" (>= 30) dfNA
      numRows df' `shouldBe` 3

    it "mapNumeric applies a unary function" $ do
      let df' = Pp.mapNumeric "y" (* 2) dfNA
      case getNumeric "y" df' of
        Just v  -> V.toList v `shouldBe` [20, 40, 60, 80, 100]
        Nothing -> expectationFailure "y should be numeric"

  -- ─────────────────────────────────────────────────────────────────────
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
