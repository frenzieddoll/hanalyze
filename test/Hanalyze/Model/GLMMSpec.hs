{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.GLMMSpec (spec) where

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
import qualified Numeric.LinearAlgebra as LA
import qualified DataFrame.Internal.Column    as DX
import qualified DataFrame.Internal.DataFrame  as DX
import qualified Hanalyze.Model.Core        as Core
import qualified Hanalyze.MCMC.Core as Core
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.GLMM" $ do
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

  describe "Hanalyze.Model.GLMM general random effects (Phase 48)" $ do
    let approxG tol a b = abs (a - b) < tol
        buildG gvec =
          let lbls = V.fromList . sort
                       . foldr (\x acc -> if x `elem` acc then acc else x:acc) []
                       $ V.toList gvec
              idxF x = maybe 0 id (V.elemIndex x lbls)
          in (lbls, V.map idxF gvec)

    -- (1) r=1, intercept-only Z must reproduce scalar fitLME exactly.
    describe "reduces to fitLME for intercept-only random effect (r=1)" $ do
      let xMat = LA.fromLists
                   [[1,1],[1,2],[1,3],[1,4],
                    [1,1],[1,2],[1,3],[1,4],
                    [1,1],[1,2],[1,3],[1,4]] :: LA.Matrix Double
          zOne = LA.fromLists (replicate 12 [1.0])          :: LA.Matrix Double
          y    = LA.fromList [7.1,6.9,7.0,7.0, 5.0,4.9,5.1,5.0, 3.0,2.9,3.1,3.0]
          gv   = V.fromList (["A","A","A","A","B","B","B","B","C","C","C","C"] :: [T.Text])
          (lbls, idx) = buildG gv
          sizes = V.fromList [ V.length (V.filter (== j) idx) | j <- [0..V.length lbls - 1] ]
          scal = fitLME xMat y idx lbls sizes
          gen  = fitLMEGeneral xMat zOne y idx lbls

      it "random variance matches glmmRandVar" $
        LA.atIndex (reRandCov gen) (0,0) `shouldSatisfy` approxG 1e-6 (glmmRandVar scal)
      it "residual variance matches glmmResidVar" $
        reResidVar gen `shouldSatisfy` approxG 1e-6 (glmmResidVar scal)
      it "fixed-effect β matches glmmFixed" $
        LA.toList (Core.coefficientsV (reFixed gen))
          `shouldSatisfy` (\bs -> and (zipWith (approxG 1e-6) bs
                                        (LA.toList (Core.coefficientsV (glmmFixed scal)))))
      it "BLUPs match glmmBLUPs (q×1 column)" $
        LA.toList (LA.flatten (reBLUPs gen))
          `shouldSatisfy` (\bs -> and (zipWith (approxG 1e-6) bs (V.toList (glmmBLUPs scal))))
      it "group labels preserved" $
        reGroups gen `shouldBe` V.fromList ["A","B","C"]

    -- (2) random slope: 4 groups with known per-group intercept+slope deviations.
    --     Fixed mean β=[2,3]; group lines differ in both intercept and slope.
    describe "recovers random slope structure (r=2)" $ do
      let oneGrp = [[1,0],[1,1],[1,2],[1,3],[1,4]] :: [[Double]]
          xMat = LA.fromLists (concat (replicate 4 oneGrp))   :: LA.Matrix Double
          zMat = xMat                                          -- (1 + x | g)
          -- A: 3+3.5x  B: 1+2.5x  C: 2.5+2.7x  D: 1.5+3.3x  (+tiny noise)
          y = LA.fromList
                [ 3.01, 6.49, 10.01, 13.49, 17.00      -- A
                , 1.01, 3.49,  6.01,  8.49, 11.00      -- B
                , 2.51, 5.19,  7.91, 10.59, 13.30      -- C
                , 1.51, 4.79,  8.11, 11.39, 14.70 ]    -- D
          gv = V.fromList (concatMap (replicate 5) (["A","B","C","D"] :: [T.Text]))
          (lbls, idx) = buildG gv
          gen = fitLMEGeneral xMat zMat y idx lbls
          beta = LA.toList (Core.coefficientsV (reFixed gen))

      it "covariance G is 2×2" $
        LA.size (reRandCov gen) `shouldBe` (2, 2)
      it "BLUP matrix is q×2 (4 groups × intercept+slope)" $
        LA.size (reBLUPs gen) `shouldBe` (4, 2)
      it "residual variance positive" $
        reResidVar gen `shouldSatisfy` (> 0)
      it "G diagonal (variances) positive" $
        (LA.atIndex (reRandCov gen) (0,0) > 0 && LA.atIndex (reRandCov gen) (1,1) > 0)
          `shouldBe` True
      it "fixed-effect β recovers population mean [2, 3]" $
        beta `shouldSatisfy` (\b -> approxG 0.3 (b !! 0) 2.0 && approxG 0.3 (b !! 1) 3.0)
      it "BLUP intercept ordering: A > B" $
        (LA.atIndex (reBLUPs gen) (0,0) > LA.atIndex (reBLUPs gen) (1,0)) `shouldBe` True
      it "BLUP slope ordering: A > B" $
        (LA.atIndex (reBLUPs gen) (0,1) > LA.atIndex (reBLUPs gen) (1,1)) `shouldBe` True

  describe "Hanalyze.Model.GLMM general random effects non-Gaussian (Phase 48 A2)" $ do
    let approxG tol a b = abs (a - b) < tol
        buildG gvec =
          let lbls = V.fromList . sort
                       . foldr (\x acc -> if x `elem` acc then acc else x:acc) []
                       $ V.toList gvec
              idxF x = maybe 0 id (V.elemIndex x lbls)
          in (lbls, V.map idxF gvec)

    -- (1) r=1, intercept-only Z reproduces scalar fitGLMM (same MLE fixed point).
    describe "reduces to fitGLMM for intercept-only random effect (r=1)" $ do
      let xMat = LA.fromLists (concatMap (\d -> [[1,d]]) ([1,2,3,4,5] ++ [1,2,3,4,5] ++ [1,2,3,4,5]))
                   :: LA.Matrix Double
          zOne = LA.fromLists (replicate 15 [1.0])  :: LA.Matrix Double
          gv   = V.fromList (["A","A","A","A","A","B","B","B","B","B","C","C","C","C","C"] :: [T.Text])
          (lbls, idx) = buildG gv
          sizes = V.fromList [ V.length (V.filter (== j) idx) | j <- [0..V.length lbls - 1] ]

      describe "Binomial / Logit" $ do
        let y    = LA.fromList [1,1,1,1,1, 1,1,0,1,0, 0,0,0,1,0]
            scal = fitGLMM Binomial Logit xMat y idx lbls sizes
            gen  = fitGLMMGeneral Binomial Logit xMat zOne y idx lbls
        it "random variance matches glmmRandVar" $
          LA.atIndex (reRandCov gen) (0,0) `shouldSatisfy` approxG 1e-4 (glmmRandVar scal)
        it "fixed-effect β matches glmmFixed" $
          LA.toList (Core.coefficientsV (reFixed gen))
            `shouldSatisfy` (\bs -> and (zipWith (approxG 1e-4) bs
                                          (LA.toList (Core.coefficientsV (glmmFixed scal)))))
        it "BLUPs match glmmBLUPs" $
          LA.toList (LA.flatten (reBLUPs gen))
            `shouldSatisfy` (\bs -> and (zipWith (approxG 1e-4) bs (V.toList (glmmBLUPs scal))))
        it "residual variance is 1.0 (non-Gaussian convention)" $
          reResidVar gen `shouldSatisfy` approxG 1e-12 1.0

      describe "Poisson / Log" $ do
        let y    = LA.fromList [15,18,22,20,25, 7,9,8,10,11, 2,3,2,4,3]
            scal = fitGLMM Poisson Log xMat y idx lbls sizes
            gen  = fitGLMMGeneral Poisson Log xMat zOne y idx lbls
        it "random variance matches glmmRandVar" $
          LA.atIndex (reRandCov gen) (0,0) `shouldSatisfy` approxG 1e-4 (glmmRandVar scal)
        it "fixed-effect β matches glmmFixed" $
          LA.toList (Core.coefficientsV (reFixed gen))
            `shouldSatisfy` (\bs -> and (zipWith (approxG 1e-4) bs
                                          (LA.toList (Core.coefficientsV (glmmFixed scal)))))

    -- (2) random slope (r=2): structure + finiteness sanity for a Poisson GLMM.
    describe "random slope structure (r=2, Poisson)" $ do
      let oneGrp = [[1,0],[1,1],[1,2],[1,3],[1,4]] :: [[Double]]
          xMat = LA.fromLists (concat (replicate 3 oneGrp)) :: LA.Matrix Double
          zMat = xMat
          -- region X: steep growth, Y: moderate, Z: flat (slope differs by group)
          y = LA.fromList [ 3, 5, 9, 16, 28      -- X (steep)
                          , 4, 6, 8, 11, 15      -- Y (moderate)
                          , 5, 5, 6, 6,  7 ]     -- Z (flat)
          gv = V.fromList (concatMap (replicate 5) (["X","Y","Z"] :: [T.Text]))
          (lbls, idx) = buildG gv
          gen = fitGLMMGeneral Poisson Log xMat zMat y idx lbls
      it "covariance G is 2×2" $
        LA.size (reRandCov gen) `shouldBe` (2, 2)
      it "BLUP matrix is q×2 (3 groups × intercept+slope)" $
        LA.size (reBLUPs gen) `shouldBe` (3, 2)
      it "G diagonal variances positive" $
        (LA.atIndex (reRandCov gen) (0,0) > 0 && LA.atIndex (reRandCov gen) (1,1) > 0)
          `shouldBe` True
      it "fixed-effect β finite" $
        LA.toList (Core.coefficientsV (reFixed gen))
          `shouldSatisfy` all (not . isNaN)
      it "BLUP slope ordering: X (steep) > Z (flat)" $
        (LA.atIndex (reBLUPs gen) (0,1) > LA.atIndex (reBLUPs gen) (2,1)) `shouldBe` True

  describe "Hanalyze.Model.GLMM (non-Gaussian)" $ do
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

  describe "Hanalyze.Model.GLMM SE (request/100)" $ do
    -- Same fixture as the GLMM tests above (3 groups × 4 obs).
    -- design X = [1, x] over the same 12 rows.
    let xMat12 = LA.matrix 2
                   ( concatMap (\v -> [1, v])
                       [1,2,3,4,1,2,3,4,1,2,3,4 :: Double] )
        yVec12 = LA.fromList
                   [7.1,6.9,7.0,7.0, 5.0,4.9,5.1,5.0, 3.0,2.9,3.1,3.0]
        gVec12 = V.fromList
                   ["A","A","A","A","B","B","B","B","C","C","C","C" :: T.Text]
        -- Inline group construction (mirrors Hanalyze.Model.GLMM.buildGroups
        -- which is currently internal).
        gLabels12 = V.fromList ["A", "B", "C"] :: V.Vector T.Text
        gIdx12    = V.map
                      (\g -> case V.elemIndex g gLabels12 of
                               Just i  -> i
                               Nothing -> 0)
                      gVec12
        gSizes12  = V.fromList [4, 4, 4]
        glmmRes   = fitLME xMat12 yVec12 gIdx12 gLabels12 gSizes12
        idx12     = gIdx12

    it "glmmFixedSE: returns one SE per coefficient (length p)" $ do
      let ses = glmmFixedSE xMat12 idx12 glmmRes
      LA.size ses `shouldBe` LA.cols xMat12

    it "glmmFixedSE: all SEs are positive" $ do
      let ses = glmmFixedSE xMat12 idx12 glmmRes
      mapM_ (\v -> v `shouldSatisfy` (> 0)) (LA.toList ses)

    it "glmmFixedSE: σ_u → 0 reduces to OLS SE within tolerance" $ do
      -- Force σ²_u to 0 (no random effects → OLS).
      let resOLS = glmmRes { glmmRandVar = 0 }
          sesG   = glmmFixedSE xMat12 idx12 resOLS
          -- Reference OLS SE from σ² (XᵀX)⁻¹.
          xtx   = LA.tr xMat12 LA.<> xMat12
          covOLS = LA.scale (glmmResidVar resOLS) (LA.inv xtx)
          sesOLS = LA.fromList
                     [ sqrt (LA.atIndex covOLS (i, i))
                     | i <- [0 .. LA.cols xMat12 - 1] ]
      LA.norm_Inf (sesG - sesOLS) `shouldSatisfy` (< 1e-9)

    it "glmmBLUPSE: one entry per group, all positive" $ do
      let ses = glmmBLUPSE idx12 glmmRes
      V.length ses `shouldBe` V.length (glmmGroups glmmRes)
      mapM_ (\v -> v `shouldSatisfy` (> 0)) (V.toList ses)

    it "glmmBLUPSE: shrinkage formula (1/σ²_u + n_j/σ²)⁻¹^½" $ do
      let ses    = glmmBLUPSE idx12 glmmRes
          sig2u  = glmmRandVar  glmmRes
          sig2   = glmmResidVar glmmRes
          -- Group sizes: A=4, B=4, C=4 (balanced design)
          expected = sqrt (1.0 / (1.0 / sig2u + 4.0 / sig2))
      mapM_ (\(_, v) -> abs (v - expected) `shouldSatisfy` (< 1e-9))
            (zip [0 :: Int ..] (V.toList ses))

  -- ========================================================================
  -- NUTS streaming callback (Phase 9.1a)
  -- ========================================================================
