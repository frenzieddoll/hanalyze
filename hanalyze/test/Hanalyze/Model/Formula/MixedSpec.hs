{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.Formula.MixedSpec (spec) where

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
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.Formula.Mixed (Phase 48 A3/A4)" $ do
    let approxG tol a b = abs (a - b) < tol

    describe "extractRandom: (…|g) ブロック抽出" $ do
      it "(1|g) = random intercept" $
        extractRandom "y ~ x + (1|g)"
          `shouldBe` Right ("y ~ x", [RandomSpec True [] "g"])
      it "(1+x|g) = random intercept + slope" $
        extractRandom "y ~ x + (1+x|g)"
          `shouldBe` Right ("y ~ x", [RandomSpec True ["x"] "g"])
      it "(0+x|g) = random slope のみ (intercept 抑制)" $
        extractRandom "y ~ x + (0+x|g)"
          `shouldBe` Right ("y ~ x", [RandomSpec False ["x"] "g"])
      it "(1|g) のみ → 固定は intercept 補完" $
        extractRandom "y ~ (1|g)"
          `shouldBe` Right ("y ~ 1", [RandomSpec True [] "g"])
      it "random 項なし → 空リスト" $
        extractRandom "y ~ x"
          `shouldBe` Right ("y ~ x", [])
      it "独自構文 y x = b0 + b1*x + (1|g)" $
        extractRandom "y x = b0 + b1*x + (1|g)"
          `shouldBe` Right ("y x = b0 + b1*x", [RandomSpec True [] "g"])

    -- e2e: 3 群 × 4 obs (GLMM describe と同データ)。 (1|group) は
    -- fitLMEDataFrame の random intercept と同一結果になるはず。
    let dfL = DX.fromNamedColumns
                [ ("x",     DX.fromList ([1,2,3,4, 1,2,3,4, 1,2,3,4] :: [Double]))
                , ("y",     DX.fromList ([7.1,6.9,7.0,7.0, 5.0,4.9,5.1,5.0, 3.0,2.9,3.1,3.0] :: [Double]))
                , ("group", DX.fromList (["A","A","A","A","B","B","B","B","C","C","C","C"] :: [T.Text])) ]

    describe "fitMixedLME: random intercept (1|group)" $ do
      let resE = fitMixedLME "y ~ x + (1|group)" dfL
          ref  = fitLMEDataFrame [("x", 1)] "group" "y" dfL
      it "returns Right" $
        resE `shouldSatisfy` either (const False) (const True)
      it "G is 1×1 and matches fitLMEDataFrame σ²_u" $
        case (resE, ref) of
          (Right (r, _), Just rd) ->
            LA.atIndex (reRandCov r) (0,0) `shouldSatisfy` approxG 1e-6 (glmmRandVar rd)
          _ -> expectationFailure "expected Right + Just"
      it "BLUPs match fitLMEDataFrame (random intercept equivalence)" $
        case (resE, ref) of
          (Right (r, _), Just rd) ->
            LA.toList (LA.flatten (reBLUPs r))
              `shouldSatisfy` (\bs -> and (zipWith (approxG 1e-6) bs (V.toList (glmmBLUPs rd))))
          _ -> expectationFailure "expected Right + Just"
      it "fixed coef labels include intercept + x" $
        case resE of
          Right (_, labels) -> length labels `shouldBe` 2
          _                 -> expectationFailure "expected Right"

    describe "fitMixedLME: random slope (1+x|group)" $ do
      let resE = fitMixedLME "y ~ x + (1+x|group)" dfL
      it "returns Right with 2×2 G and q×2 BLUPs" $
        case resE of
          Right (r, _) -> do
            LA.size (reRandCov r) `shouldBe` (2, 2)
            LA.size (reBLUPs r)   `shouldBe` (3, 2)
          _ -> expectationFailure "expected Right"

    describe "error paths" $ do
      it "random 項なしは Left" $
        fitMixedLME "y ~ x" dfL `shouldSatisfy` either (const True) (const False)
      it "存在しない grouping 列は Left" $
        fitMixedLME "y ~ x + (1|nosuch)" dfL `shouldSatisfy` either (const True) (const False)

    describe "fitMixedGLMM: Binomial (1|group)" $ do
      let dfB = DX.fromNamedColumns
                  [ ("dose",     DX.fromList ([1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5] :: [Double]))
                  , ("success",  DX.fromList ([1,1,1,1,1, 1,1,0,1,0, 0,0,0,1,0] :: [Double]))
                  , ("hospital", DX.fromList (["A","A","A","A","A","B","B","B","B","B","C","C","C","C","C"] :: [T.Text])) ]
      it "returns Right" $
        fitMixedGLMM Binomial Logit "success ~ dose + (1|hospital)" dfB
          `shouldSatisfy` either (const False) (const True)
