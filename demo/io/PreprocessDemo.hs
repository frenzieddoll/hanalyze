{-# LANGUAGE OverloadedStrings #-}
-- | DataIO.Preprocess の総合デモ。
--
-- - NA 文字列を含む CSV をロード
-- - countMissing で欠損列を確認
-- - dropMissingRows / imputeMean / imputeMedian / imputeConstant の比較
-- - filterRowsByNumeric / mapNumeric / deriveNumeric の使用例
module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V

import DataIO.CSV          (loadAuto)
import DataFrame.Core      (DataFrame, Column (..), columnNames, getColumn,
                            getNumeric, numRows)
import DataIO.Preprocess

import System.IO          (hPutStrLn, stderr)
import System.Exit        (exitFailure)
import Text.Printf        (printf)

testCSV :: String
testCSV = unlines
  [ "group,age,income"
  , "A,25,40000"
  , "A,NA,42000"
  , "B,32,"
  , "B,28,55000"
  , "C,,38000"
  , "A,45,NA"
  , "B,30,48000"
  , "C,55,72000"
  ]

main :: IO ()
main = do
  -- write a test CSV in /tmp
  let path = "/tmp/preprocess_demo.csv"
  writeFile path testCSV

  result <- loadAuto path
  case result of
    Left err -> do
      hPutStrLn stderr ("Parse error: " ++ err)
      exitFailure
    Right df -> runDemo df

runDemo :: DataFrame -> IO ()
runDemo df = do
  putStrLn "=================================="
  putStrLn " DataIO.Preprocess Demo"
  putStrLn "=================================="
  putStrLn ""
  printf "Loaded %d rows, columns: %s\n"
         (numRows df) (T.unpack (T.intercalate ", " (columnNames df)))
  putStrLn ""

  -- 1. 欠損カウント
  putStrLn "--- countMissing ---"
  mapM_ (\(c, m) ->
    if m > 0 then printf "  %s: %d missing\n" (T.unpack c) m
             else printf "  %s: complete\n"   (T.unpack c))
    (countMissing df)
  putStrLn ""

  -- 2. dropMissingRows
  putStrLn "--- dropMissingRows [\"age\", \"income\"] ---"
  let df1 = dropMissingRows ["age", "income"] df
  printf "  After: %d rows (was %d)\n" (numRows df1) (numRows df)
  putStrLn ""

  -- 3. parseNumericColumn (after dropping NA)
  putStrLn "--- parseNumericColumn (TextCol → NumericCol) ---"
  case parseNumericColumn "age" df1 >>= parseNumericColumn "income" of
    Nothing -> putStrLn "  failed (still has unparseable cells)"
    Just df2 -> do
      printf "  Both age/income are now numeric\n"
      showNumericStats df2 "age"
      showNumericStats df2 "income"
  putStrLn ""

  -- 4. imputeMean / imputeMedian on raw df (before drop)
  putStrLn "--- imputeMean / imputeMedian on age ---"
  case imputeMean "age" df of
    Just df3 -> do
      printf "  imputeMean produces %d numeric rows\n" (numRows df3)
      showNumericStats df3 "age"
    Nothing -> putStrLn "  imputeMean failed"
  case imputeMedian "income" df of
    Just df4 -> do
      printf "  imputeMedian produces %d numeric rows\n" (numRows df4)
      showNumericStats df4 "income"
    Nothing -> putStrLn "  imputeMedian failed"
  putStrLn ""

  -- 5. filterRowsByNumeric
  putStrLn "--- filterRowsByNumeric (age >= 30) ---"
  let dfNum = case imputeMean "age" df >>= imputeMean "income" of
                Just d -> d
                Nothing -> df
      dfFilt = filterRowsByNumeric "age" (>= 30) dfNum
  printf "  After: %d rows (was %d)\n" (numRows dfFilt) (numRows dfNum)
  putStrLn ""

  -- 6. mapNumeric
  putStrLn "--- mapNumeric \"income\" (/1000) ---"
  let dfMap = mapNumeric "income" (/ 1000) dfNum
  showNumericStats dfMap "income"
  putStrLn ""

  -- 7. deriveNumeric (income / age = income-per-year)
  putStrLn "--- deriveNumeric \"ratio\" = income / age ---"
  let dfDeriv = deriveNumeric "ratio"
                  (\row -> case (Map.lookup "income" row, Map.lookup "age" row) of
                             (Just (VNum i), Just (VNum a)) | a > 0 -> i / a
                             _ -> 0)
                  dfNum
  showNumericStats dfDeriv "ratio"
  putStrLn ""

  -- 8. selectColumns
  putStrLn "--- selectColumns [\"group\", \"age\"] ---"
  let dfSel = selectColumns ["group", "age"] dfNum
  printf "  columns: %s\n" (T.unpack (T.intercalate ", " (columnNames dfSel)))
  putStrLn ""

  putStrLn "Done."

showNumericStats :: DataFrame -> T.Text -> IO ()
showNumericStats df name = case getNumeric name df of
  Nothing -> printf "  %s: not numeric\n" (T.unpack name)
  Just v  -> do
    let xs = V.toList v
        m  = length xs
        mean = sum xs / fromIntegral m
        mn = minimum xs
        mx = maximum xs
    printf "  %-10s n=%d  min=%.2f  max=%.2f  mean=%.2f\n"
           (T.unpack name) m mn mx mean
