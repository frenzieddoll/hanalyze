{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.Formula.DesignSpec (spec) where

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
import qualified Hanalyze.Model.Spline      as Sp
import qualified Hanalyze.Model.Core        as Core
import qualified Hanalyze.MCMC.Core as Core
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.Formula.Design (A17 designMatrixF/fitLMF)" $ do
    let frm s = case parseFormula s of Right f -> f; Left e -> error e

    describe "検証点①: factor×factor 飽和モデル (ŷ = セル平均・満ランク)" $ do
      let gs = ["A","A","B","B","C","C","A","A","B","B","C","C"] :: [T.Text]
          ts = ["P","Q","P","Q","P","Q","P","Q","P","Q","P","Q"] :: [T.Text]
          ys = [10,20,30,40,50,60,12,22,34,44,52,62] :: [Double]
          df = DX.fromNamedColumns
                 [ ("y", DX.fromList ys)
                 , ("g", DX.fromList gs)
                 , ("t", DX.fromList ts) ]
          Right (fr, lbls) = fitLMF (frm "y g t = b0 + bg!g + bt!t + bgt!g!t") df
          cellMean a b = let zs = [v | (gi,ti,v) <- zip3 gs ts ys, gi==a, ti==b]
                         in sum zs / fromIntegral (length zs)
          expected = [cellMean a b | (a,b) <- zip gs ts]
          Right mf = modelFrame (frm "y g t = b0 + bg!g + bt!t + bgt!g!t") df
          Right (x, _) = designMatrixF (frm "y g t = b0 + bg!g + bt!t + bgt!g!t") mf
      it "列数 = Kg*Kt = 6 (treatment contrast)" $
        length lbls `shouldBe` 6
      it "設計行列が満ランク (制約後)" $
        LA.rank x `shouldBe` 6
      it "ŷ = セル平均" $
        and (zipWith (\p q -> abs (p - q) < 1e-9) (Core.fittedList fr) expected)
          `shouldBe` True

    describe "線形連続 (既存 OLS と一致)" $ do
      let df = DX.fromNamedColumns
                 [ ("y", DX.fromList ([2.1,3.9,6.1,7.9,10.1] :: [Double]))
                 , ("x", DX.fromList ([1,2,3,4,5] :: [Double])) ]
          Right (fr, _) = fitLMF (frm "y x = b0 + b1*x") df
          coefs = LA.toList (Core.coefficientsV fr)
      it "切片≈0・傾き≈2" $ do
        abs (coefs !! 0 - 0.02) `shouldSatisfy` (< 1e-6)
        abs (coefs !! 1 - 2.0)  `shouldSatisfy` (< 1e-6)

    describe "factor×連続 (水準別傾き) は列が水準分" $ do
      let df = DX.fromNamedColumns
                 [ ("y", DX.fromList ([1,2,3,4,5,6] :: [Double]))
                 , ("x", DX.fromList ([1,2,3,4,5,6] :: [Double]))
                 , ("g", DX.fromList (["A","A","B","B","C","C"] :: [T.Text])) ]
          Right (_, lbls) = fitLMF (frm "y g x = b0 + bg!g + bs!g*x") df
      it "切片 + g 主効果2 + g×x 傾き3 (連続 interaction は全水準保持) = 6 列" $
        length lbls `shouldBe` 6

    describe "線形性検出" $ do
      let df = DX.fromNamedColumns
                 [ ("y", DX.fromList ([1,2,3] :: [Double]))
                 , ("x", DX.fromList ([1,2,3] :: [Double])) ]
      it "a*exp(-b*x) は非線形ゆえ Left" $
        fitLMF (frm "y x = a*exp(-b*x)") df `shouldSatisfy` isLeftE
      it "b0 + b1*x + b2*log x は線形ゆえ Right" $
        linearityCheck (frm "y x = b0 + b1*x + b2*log x") df `shouldSatisfy` isRightE

    describe "検証点②: 基底展開 (Python 非依存オラクル)" $ do
      let xs = [0.2,0.5,0.9,1.4,2.0,2.6,3.1,3.7,4.2,4.8] :: [Double]
          ys = map (\x -> sin x + 0.1*x) xs
          df = DX.fromNamedColumns
                 [ ("y", DX.fromList ys), ("x", DX.fromList xs) ]
      it "poly(x,2) は二次を厳密再現 (y=1+2x+3x² で R²≈1)" $ do
        let yq = map (\x -> 1 + 2*x + 3*x*x) xs
            dfq = DX.fromNamedColumns [ ("y", DX.fromList yq), ("x", DX.fromList xs) ]
            Right (fr, _) = fitLMF (frm "y x = b0 + bp ! poly(x,2)") dfq
        and (zipWith (\p q -> abs (p - q) < 1e-9) (Core.fittedList fr) yq)
          `shouldBe` True
      it "opoly(x,2) は不等間隔でも設計列が直交 (linear ⊥ quadratic・raw poly と別)" $ do
        -- 不等間隔 3 点。 raw poly(x,2) なら x·x² = 1·1+2·4+5·25 = 134 ≠ 0 で共線だが、
        -- opoly は実測値を QR 直交化するので linear 列 ⊥ quadratic 列 (内積 ≈ 0)。
        let xsu = [1.0, 2.0, 5.0] :: [Double]
            dfu = DX.fromNamedColumns
                    [ ("y", DX.fromList [0,0,0 :: Double]), ("x", DX.fromList xsu) ]
            frmR s = case parseRFormula s of Right f -> f; Left e -> error e
            fR  = frmR "y ~ opoly(x,2)"
            Right mfu       = modelFrame fR dfu
            Right (xmat, _) = designMatrixF fR mfu
            cols            = LA.toColumns xmat
            [c1, c2]        = drop (length cols - 2) cols   -- opoly の 2 列 (切片を除く)
        abs (LA.dot c1 c2) `shouldSatisfy` (< 1e-9)
      it "opoly(x,2) は二次を厳密再現 (raw poly と同 span・ŷ 一致)" $ do
        let xsu = [1.0, 2.0, 5.0, 7.0, 8.0] :: [Double]
            yq  = map (\x -> 1 + 2*x + 3*x*x) xsu
            dfq2 = DX.fromNamedColumns [ ("y", DX.fromList yq), ("x", DX.fromList xsu) ]
            frmR s = case parseRFormula s of Right f -> f; Left e -> error e
            Right (fr, _) = fitLMF (frmR "y ~ opoly(x,2)") dfq2
        and (zipWith (\p q -> abs (p - q) < 1e-9) (Core.fittedList fr) yq)
          `shouldBe` True
      it "bspline(x,4) (切片なし) の ŷ = fitSpline (BSpline 3, quantileKnots 4)" $ do
        let Right (fr, _) = fitLMF (frm "y x = bs ! bspline(x,4)") df
            sf = Sp.fitSpline (Sp.BSpline 3) (Sp.quantileKnots 4 (V.fromList xs))
                              (V.fromList xs) (V.fromList ys)
            yhatSpline = V.toList (Sp.predictSpline sf (V.fromList xs))
        and (zipWith (\p q -> abs (p - q) < 1e-9) (Core.fittedList fr) yhatSpline)
          `shouldBe` True
