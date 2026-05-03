{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- | DataIO.Preprocess の総合デモ。
--
-- - NA 文字列を含む CSV をロード (Hackage dataframe 経由)
-- - countMissing で欠損列を確認
-- - dropMissingRows / imputeMean / imputeMedian / imputeConstant の比較
-- - filterRowsByNumeric / mapNumeric / deriveNumeric の使用例
module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import qualified DataFrame                    as DX
import qualified DataFrame.Internal.DataFrame as DXD
import DataIO.CSV         (loadCSV)
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
  let path = "/tmp/preprocess_demo.csv"
  writeFile path testCSV

  result <- loadCSV path
  case result of
    Left err -> do
      hPutStrLn stderr ("Parse error: " ++ err)
      exitFailure
    Right df -> runDemo df

runDemo :: DXD.DataFrame -> IO ()
runDemo df = do
  putStrLn "=================================="
  putStrLn " DataIO.Preprocess Demo"
  putStrLn "=================================="
  putStrLn ""
  let (nrows, _) = DX.dimensions df
  printf "Loaded %d rows, columns: %s\n"
         nrows (T.unpack (T.intercalate ", " (DX.columnNames df)))
  putStrLn ""

  putStrLn "--- countMissing ---"
  mapM_ (\(c, m) ->
    if m > 0 then printf "  %s: %d missing\n" (T.unpack c) m
             else printf "  %s: complete\n"   (T.unpack c))
    (countMissing df)
  putStrLn ""

  putStrLn "--- dropMissingRows [\"age\", \"income\"] ---"
  let df1 = dropMissingRows ["age", "income"] df
      (nrows1, _) = DX.dimensions df1
  printf "  After: %d rows (was %d)\n" nrows1 nrows
  putStrLn ""

  putStrLn "--- parseNumericColumn ---"
  case parseNumericColumn "age" df1 >>= parseNumericColumn "income" of
    Nothing -> putStrLn "  (already numeric or parse failed; OK if Hackage parsed it)"
    Just df2 -> do
      printf "  Both age/income are now numeric\n"
      showNumericStats df2 "age"
      showNumericStats df2 "income"
  putStrLn ""

  putStrLn "--- imputeMean / imputeMedian on age ---"
  case imputeMean "age" df of
    Just df3 -> do
      let (n3, _) = DX.dimensions df3
      printf "  imputeMean produces %d numeric rows\n" n3
      showNumericStats df3 "age"
    Nothing -> putStrLn "  imputeMean failed"
  case imputeMedian "income" df of
    Just df4 -> do
      let (n4, _) = DX.dimensions df4
      printf "  imputeMedian produces %d numeric rows\n" n4
      showNumericStats df4 "income"
    Nothing -> putStrLn "  imputeMedian failed"
  putStrLn ""

  putStrLn "--- filterRowsByNumeric (age >= 30) ---"
  let dfNum = case imputeMean "age" df >>= imputeMean "income" of
                Just d  -> d
                Nothing -> df
      dfFilt = filterRowsByNumeric "age" (>= 30) dfNum
      (nNum, _)  = DX.dimensions dfNum
      (nFilt, _) = DX.dimensions dfFilt
  printf "  After: %d rows (was %d)\n" nFilt nNum
  putStrLn ""

  putStrLn "--- mapNumeric \"income\" (/1000) ---"
  let dfMap = mapNumeric "income" (/ 1000) dfNum
  showNumericStats dfMap "income"
  putStrLn ""

  putStrLn "--- deriveNumeric \"ratio\" = income / age ---"
  let dfDeriv = deriveNumeric "ratio"
                  (\row -> case (Map.lookup "income" row, Map.lookup "age" row) of
                             (Just (VNum i), Just (VNum a)) | a > 0 -> i / a
                             _ -> 0)
                  dfNum
  showNumericStats dfDeriv "ratio"
  putStrLn ""

  putStrLn "--- selectColumns [\"group\", \"age\"] ---"
  let dfSel = selectColumns ["group", "age"] dfNum
  printf "  columns: %s\n" (T.unpack (T.intercalate ", " (DX.columnNames dfSel)))
  putStrLn ""

  putStrLn "Done."

showNumericStats :: DXD.DataFrame -> T.Text -> IO ()
showNumericStats df name =
  case readNum name df of
    Nothing -> printf "  %s: not numeric\n" (T.unpack name)
    Just xs -> do
      let m  = length xs
          mean = sum xs / fromIntegral m
          mn = minimum xs
          mx = maximum xs
      printf "  %-10s n=%d  min=%.2f  max=%.2f  mean=%.2f\n"
             (T.unpack name) m mn mx mean

readNum :: T.Text -> DXD.DataFrame -> Maybe [Double]
readNum name df =
  case DXD.getColumn name df of
    Nothing -> Nothing
    Just _  ->
      case tryReadDouble name df of
        Just xs -> Just xs
        Nothing -> tryReadIntAsDouble name df

tryReadDouble :: T.Text -> DXD.DataFrame -> Maybe [Double]
tryReadDouble name df = either (const Nothing) Just $
  fmap (map (id :: Double -> Double)) $
    Right (DX.columnAsList (DX.col @Double name) df)

tryReadIntAsDouble :: T.Text -> DXD.DataFrame -> Maybe [Double]
tryReadIntAsDouble name df = either (const Nothing) Just $
  fmap (map (fromIntegral :: Int -> Double)) $
    Right (DX.columnAsList (DX.col @Int name) df)
