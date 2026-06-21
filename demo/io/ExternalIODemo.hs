{-# LANGUAGE OverloadedStrings #-}
-- | Hanalyze.DataIO.External のデモ。
--
-- Hackage 'dataframe' ライブラリ経由で CSV を読み込み:
-- - 列ごとの自動型推論結果
-- - 欠損値の検出
-- - imputeMean で欠損補完
import qualified Data.Text as T

import Hanalyze.DataIO.CSV         (loadCSV)
import Hanalyze.DataIO.Preprocess  (countMissing, imputeMean)
import qualified DataFrame.Internal.DataFrame  as DX
import qualified DataFrame.Operations.Core     as DX
import qualified DataFrame.Internal.DataFrame as DXD
import qualified DataFrame.Internal.Column as DXC
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
  putStrLn " Hanalyze.DataIO.External Demo"
  putStrLn "=================================="
  putStrLn ""

  putStrLn "--- loadCSV (Hackage dataframe) ---"
  Right df <- loadCSV path
  printDFTypes df
  putStrLn ""

  putStrLn "--- countMissing ---"
  mapM_ (\(c, m) ->
    if m > 0 then printf "  %s: %d missing\n" (T.unpack c) m
             else printf "  %s: complete\n"   (T.unpack c))
    (countMissing df)
  putStrLn ""

  putStrLn "--- imputeMean \"score\" ---"
  case imputeMean "score" df of
    Just df3 -> do
      printDFTypes df3
      printf "  → score is now numeric (mean-imputed for NA rows)\n"
    Nothing -> putStrLn "  imputeMean failed"
  putStrLn ""

  putStrLn "Done."

printDFTypes :: DXD.DataFrame -> IO ()
printDFTypes df = do
  let (rows, ncols) = DX.dimensions df
  printf "  Rows: %d, Columns: %d\n" rows ncols
  mapM_ (\n -> case DXD.getColumn n df of
           Just c  -> printf "    %-10s : %s (len=%d)\n"
                        (T.unpack n)
                        (DXC.columnTypeString c)
                        (DXC.columnLength c)
           Nothing -> printf "    %-10s : <missing>\n" (T.unpack n))
        (DX.columnNames df)
