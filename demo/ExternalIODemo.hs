{-# LANGUAGE OverloadedStrings #-}
-- | DataIO.External のデモ。
--
-- Hackage 'dataframe' ライブラリ経由で CSV を読み込み:
-- - 既存 'DataIO.CSV.loadAuto' との比較 (型推論精度)
-- - 欠損値が "NA" 文字列として保持されることを確認
-- - imputeMean で欠損補完して NumericCol 化
import qualified Data.Text as T
import qualified Data.Vector as V

import DataIO.CSV         (loadAuto)
import DataIO.External    (loadCSVExt)
import DataIO.Preprocess  (countMissing, imputeMean)
import DataFrame.Core     (DataFrame, Column (..), columnNames, getColumn,
                           numRows)
import Text.Printf        (printf)

testCSV :: String
testCSV = unlines
  [ "name,age,score,group"
  , "Alice,30,95.5,A"
  , "Bob,25,88.0,B"
  , "Carol,35,,A"            -- score 欠損
  , "Dave,,77.2,B"            -- age 欠損
  , "Eve,42,NA,C"             -- score "NA"
  ]

main :: IO ()
main = do
  let path = "/tmp/external_demo.csv"
  writeFile path testCSV

  putStrLn "=================================="
  putStrLn " DataIO.External Demo"
  putStrLn "=================================="
  putStrLn ""

  -- 1. 既存 loadAuto (cassava ベース)
  putStrLn "--- 既存 loadAuto (DataIO.CSV) ---"
  Right df1 <- loadAuto path
  printDFTypes df1
  putStrLn ""

  -- 2. loadCSVExt (Hackage dataframe ベース)
  putStrLn "--- 新規 loadCSVExt (DataIO.External) ---"
  Right df2 <- loadCSVExt path
  printDFTypes df2
  putStrLn ""

  -- 3. countMissing で欠損を確認 (External 経由は欠損が NA として残る)
  putStrLn "--- countMissing on External-loaded df ---"
  mapM_ (\(c, m) ->
    if m > 0 then printf "  %s: %d missing\n" (T.unpack c) m
             else printf "  %s: complete\n"   (T.unpack c))
    (countMissing df2)
  putStrLn ""

  -- 4. imputeMean で score 列を補完 (TextCol → NumericCol)
  putStrLn "--- imputeMean \"score\" on External-loaded df ---"
  case imputeMean "score" df2 of
    Just df3 -> do
      printDFTypes df3
      printf "  → score is now numeric (mean-imputed for NA rows)\n"
    Nothing -> putStrLn "  imputeMean failed"
  putStrLn ""

  putStrLn "Done."

printDFTypes :: DataFrame -> IO ()
printDFTypes df = do
  printf "  Rows: %d, Columns: %d\n" (numRows df) (length (columnNames df))
  mapM_ (\n -> case getColumn n df of
           Just (NumericCol v) ->
             printf "    %-10s : NumericCol (n=%d)\n" (T.unpack n) (V.length v)
           Just (TextCol v)    ->
             printf "    %-10s : TextCol    (n=%d) values=%s\n"
               (T.unpack n) (V.length v)
               (show (V.toList v))
           Nothing -> printf "    %-10s : <missing>\n" (T.unpack n))
        (columnNames df)
