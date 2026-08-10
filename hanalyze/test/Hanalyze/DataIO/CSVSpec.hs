{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.DataIO.CSVSpec (spec) where

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
import qualified DataFrame.Internal.Column    as DX
import qualified DataFrame.Internal.DataFrame  as DX
import qualified DataFrame.Operations.Core     as DX
import qualified Hanalyze.DataIO.Preprocess as Pp
import qualified Hanalyze.DataIO.Log        as Log
import qualified Hanalyze.DataIO.CSV        as CSV
import qualified Hanalyze.DataIO.Clean      as Clean
import qualified Hanalyze.DataIO.Convert    as Conv2
import qualified Hanalyze.Stat.AdaptiveGrid as AG
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.DataIO.CSV.loadAutoSafe" $ do
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

  describe "Hanalyze.DataIO.CSV.loadAutoSafeWith" $ do
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

    it "Preprocess.regridLong: ZIntersection モードで全 id が共通範囲に収まる" $ do
      -- id=a: z=0..3, id=b: z=1..4 → intersection は (1, 3)
      let df0 = DX.insertColumn "id" (DX.fromList (["a","a","a","a","b","b","b","b"] :: [T.Text]))
              $ DX.insertColumn "z"  (DX.fromList ([0,1,2,3,1,2,3,4] :: [Double]))
              $ DX.insertColumn "y"  (DX.fromList ([0,1,4,9,1,4,9,16] :: [Double]))
              $ DX.empty
          opts = Pp.defaultRegridOpts
                   { Pp.roN = 5, Pp.roZBoundsMode = Pp.ZIntersection
                   , Pp.roGridKind = AG.Uniform }
          rr = Pp.regridLong "id" "z" "y" opts df0
      Pp.rrZMin rr `shouldBe` 1.0
      Pp.rrZMax rr `shouldBe` 3.0
      length (Pp.rrZGrid rr) `shouldBe` 5
      length (Pp.rrIds rr) `shouldBe` 2

    it "Preprocess.regridLong: ZUnion モードで [min,max] が和集合になる" $ do
      let df0 = DX.insertColumn "id" (DX.fromList (["a","a","b","b"] :: [T.Text]))
              $ DX.insertColumn "z"  (DX.fromList ([0,2,1,3] :: [Double]))
              $ DX.insertColumn "y"  (DX.fromList ([0,4,1,9] :: [Double]))
              $ DX.empty
          opts = Pp.defaultRegridOpts
                   { Pp.roN = 4, Pp.roZBoundsMode = Pp.ZUnion
                   , Pp.roGridKind = AG.Uniform }
          rr = Pp.regridLong "id" "z" "y" opts df0
      Pp.rrZMin rr `shouldBe` 0.0
      Pp.rrZMax rr `shouldBe` 3.0
      -- 外挿が記録される (id=a の上端 0..2、共通 0..3 → above=1)
      let stat_a = head [s | s <- Pp.rrPerIdStats rr, Pp.piId s == "a"]
      Pp.piExtrapAbove stat_a `shouldBe` 1.0

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
