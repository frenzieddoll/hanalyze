{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.MultiOutputSpec (spec) where

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
import qualified System.Random.MWC as MWC
import qualified Hanalyze.Model.GP          as GP
import qualified Hanalyze.Model.GPRobust    as GPR
import qualified Hanalyze.Model.GP        as GP
import qualified Hanalyze.Model.GPRobust  as GPR
import qualified Hanalyze.Model.RFF       as RFF
import qualified Hanalyze.Model.Regularized as Reg
import qualified Hanalyze.Model.Spline      as Sp
import qualified Hanalyze.Model.KernelRegression      as K
import qualified Hanalyze.Model.Core        as Core
import qualified Hanalyze.Model.GLM         as GLM
import qualified System.Random.MWC as MWC
import qualified Hanalyze.MCMC.Core as Core
import SpecHelper

spec :: Spec
spec = do
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

    it "bsplineBasis: partition of unity が右端 x=hi でも成立 (= 1)" $ do
      -- clamped ノットで hi が重複するため、 退化区間 [hi,hi] を右閉にすると
      -- 高次再帰で基底全ゼロ化する欠陥があった (計測で確認・修正済)。 内点と
      -- 端点の両方で行和 = 1 を要求する回帰テスト。
      let knots = [0,2,4,6,8] :: [Double]
          xs    = V.fromList [0, 4, 7.99, 8.0]   -- 左端 / 内点 / 端近傍 / 右端
          basis = Sp.bsplineBasis 3 knots xs
          sums  = map sum (LA.toLists basis)
      approxList 1e-9 sums [1,1,1,1] `shouldBe` True

    it "fitSpline: 右端 x=hi のフィット値が崩れない (基底全ゼロ化の回帰)" $ do
      -- y=x² を 3 次 B-spline で fit。 右端の fitted が 0 に落ちず原値に近いこと。
      let xv = V.fromList [0,1,2,3,4,5,6,7,8] :: V.Vector Double
          yv = V.map (\x -> x*x) xv
          fit = Sp.fitSpline (Sp.BSpline 3) [0,2,4,6,8] xv yv
          yhatLast = V.last (Sp.predictSpline fit (V.fromList [8.0]))
      yhatLast `shouldSatisfy` (\v -> abs (v - 64) < 5)  -- 64 = 8² 近傍 (0 でない)

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

  -- ===========================================================================
  -- 単目的オプティマイザ (Hanalyze.Optim.NelderMead)
  -- ===========================================================================
