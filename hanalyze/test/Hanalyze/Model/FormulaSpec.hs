{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Model.FormulaSpec (spec) where

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
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Model.Formula (A15 parser)" $ do
    describe "golden: 入力 → 期待 AST (優先順位・結合の固定)" $ do
      let g src expect = it (T.unpack src) $ parseFormula src `shouldBe` Right expect
      g "y x = b1*x"
        (Formula "y" ["x"] (Bin Mul (Ref "b1") (Ref "x")))
      g "y x = b0 + b1*x"
        (Formula "y" ["x"] (Bin Add (Ref "b0") (Bin Mul (Ref "b1") (Ref "x"))))
      g "y g t = b ! g ! t"
        (Formula "y" ["g", "t"] (Index (Index (Ref "b") (Ref "g")) (Ref "t")))
      g "y g x = bg ! g * x"  -- ! は * より高優先 (factor×連続 = 水準別傾き)
        (Formula "y" ["g", "x"] (Bin Mul (Index (Ref "bg") (Ref "g")) (Ref "x")))
      g "y x = a*exp(-b*x)"  -- 単項 - は * より高優先 ゆえ -b*x = (-b)*x
        (Formula "y" ["x"] (Bin Mul (Ref "a") (App "exp" [Bin Mul (Neg (Ref "b")) (Ref "x")])))
      g "y x = b0 + b1*x^2"  -- ^ は * より高優先
        (Formula "y" ["x"] (Bin Add (Ref "b0") (Bin Mul (Ref "b1") (Bin Pow (Ref "x") (Lit 2)))))
      g "y x = log (x+1)"    -- 空白並置適用
        (Formula "y" ["x"] (App "log" [Bin Add (Ref "x") (Lit 1)]))

    describe "error: 不正入力は Left" $ do
      let bad src = it (T.unpack src) $ parseFormula src `shouldSatisfy` isLeftE
      bad "y x = "       -- 右辺なし
      bad "y x b1*x"     -- = なし
      bad "= b0 + b1*x"  -- 左辺なし

    describe "round-trip: parse . pretty == id (QuickCheck)" $
      prop "任意 Formula で round-trip" $ \f ->
        parseFormula (prettyFormula f) === Right f
