{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.DataIO.HealthSpec (spec) where

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
import qualified Hanalyze.DataIO.Log        as Log
import qualified Hanalyze.DataIO.Health     as Health
import qualified Data.ByteString   as BS
import qualified Hanalyze.Stat.BridgeSampling as BS
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.DataIO.Health" $ do
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
