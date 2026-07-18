{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.Formula.RFormulaSpec (spec) where

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
import qualified DataFrame.Internal.Column    as DX
import qualified DataFrame.Internal.DataFrame  as DX
import qualified Hanalyze.Model.Core        as Core
import qualified Hanalyze.MCMC.Core as Core
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.Formula.RFormula (A18 R front-end)" $ do
    let gs = ["A","A","B","B","C","C","A","A","B","B","C","C"] :: [T.Text]
        ts = ["P","Q","P","Q","P","Q","P","Q","P","Q","P","Q"] :: [T.Text]
        ys = [10,20,30,40,50,60,12,22,34,44,52,62] :: [Double]
        xs = [1,2,3,4,5,6,1,2,3,4,5,6] :: [Double]
        df = DX.fromNamedColumns
               [ ("y", DX.fromList ys), ("g", DX.fromList gs)
               , ("t", DX.fromList ts), ("x", DX.fromList xs) ]
        yhat s = case parseModel s >>= \f -> fitLMF f df of
                   Right (fr, _) -> Right (Core.fittedList fr)
                   Left e        -> Left e
        equiv rsyn usyn = case (yhat rsyn, yhat usyn) of
          (Right a, Right b) -> and (zipWith (\p q -> abs (p - q) < 1e-9) a b)
          _                  -> False

    describe "dispatch: ~ を含めば R・無ければ独自" $ do
      it "y ~ x → 応答 y / データ変数 x" $
        case parseModel "y ~ x" of
          Right (Formula r dv _) -> (r, dv) `shouldBe` ("y", ["x"])
          Left e                 -> expectationFailure e
      it "独自構文 (= 区切り) も同 dispatch で通る" $
        parseModel "y x = b0 + b1*x" `shouldSatisfy` isRightE

    describe "クロス front-end 等価 (R 構文 vs 独自構文 → 同 ŷ)" $ do
      it "linear: y~x ≡ y x = b0+b1*x" $
        equiv "y ~ x" "y x = b0 + b1*x" `shouldBe` True
      it "factor×factor: y~C(g)*C(t) ≡ 飽和" $
        equiv "y ~ C(g)*C(t)" "y g t = b0 + bg!g + bt!t + bgt!g!t" `shouldBe` True
      it "factor×連続: y~C(g)+C(g):x ≡ 水準別傾き" $
        equiv "y ~ C(g) + C(g):x" "y g x = b0 + bg!g + bs!g*x" `shouldBe` True
      it "I(x**2): y~x+I(x**2) ≡ b0+b1*x+b2*x^2" $
        equiv "y ~ x + I(x**2)" "y x = b0 + b1*x + b2*x^2" `shouldBe` True
      it "poly: y~poly(x,2) ≡ b0+bp!poly(x,2)" $
        equiv "y ~ poly(x,2)" "y x = b0 + bp ! poly(x,2)" `shouldBe` True

  -- round-trip 用の任意 Formula 生成 (識別子は固定プール・Lit は非負整数)。
