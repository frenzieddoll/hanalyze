{-# LANGUAGE OverloadedStrings #-}
-- | 汚いデータを @data\/dirty\/@ から読み込み、'DataIO.CSV.loadAutoSafeWith'
-- がどんな警告コード (W001…W008) や情報コード (I010…I012) を出すかを
-- 19 ファイル分一覧表示するショーケース demo。
--
-- 実行方法:
--
-- @
-- cabal run dirty-data-demo
-- @
--
-- 出力例:
--
-- @
-- ───────────────────────────────────────────────────────────────────
--   data/dirty/02_no_header.csv
-- ───────────────────────────────────────────────────────────────────
--   [WARN]  W001: 列名が全て数値です: 1.0, 2.0 — ヘッダ行が無いファイル
--                  の可能性。ヒント: --no-header を指定してください。
--   → 同ファイルを LoadOpts { loNoHeader = True } で再読込:
--   [INFO]  I012: --no-header: ヘッダ 2 列 (col0...) を生成しました。
-- @
--
-- 19 ファイル全件処理し、最後にまとめテーブルを出力する。
module Main where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Control.Monad (forM_, forM)
import Data.List (sort)
import Text.Printf (printf)

import qualified DataFrame     as DX
import qualified DataIO.CSV    as CSV
import qualified DataIO.Log    as Log

dataDir :: FilePath
dataDir = "data/dirty"

-- | (ファイル名, 期待される W コード, 「修復策」のオプション)。
fixtures :: [(FilePath, [T.Text], Maybe CSV.LoadOpts)]
fixtures =
  -- 期待 W コードは Sniff (loSniff=True デフォルト) 適用後の値。
  -- Sniff で自動修復される ([] になる) ものはコメントで元の警告を残す。
  [ ("01_clean.csv",              [],            Nothing)
  , ("02_no_header.csv",          [], -- sniff: header off で W001 → 0
       Just (CSV.defaultLoadOpts { CSV.loNoHeader = True }))
  , ("03_preamble.csv",           [], -- sniff: skip=3 で W002 → 0
       Just (CSV.defaultLoadOpts { CSV.loSkip = 3 }))
  , ("04_ragged.csv",             [],            Nothing)
  , ("05_dup_header.csv",         ["W004", "W004"],                       Nothing)
  , ("06_blank_unnamed.csv",      ["W004", "W004", "W004", "W004"],       Nothing)
  , ("07_mixed_na.csv",           ["W003", "W006"],                       Nothing)
  , ("08_thousands_currency.csv", ["W008"],      Nothing)
  , ("09_quotes_commas.csv",      [],            Nothing)
  , ("10_bom.csv",                [],            Nothing)
  , ("11_semicolon_eu.csv",       ["W008", "W008", "W008"], -- sniff で ;、ただし "1,5" が桁区切りに誤検出 (Phase C 課題)
       Nothing)
  , ("12_real.tsv",               [],            Nothing)
  , ("13_crlf.csv",               [], -- sniff で tab → 0
       Nothing)
  , ("14_wrong_ext.csv",          [], -- sniff で tab → 0
       Nothing)
  , ("15_trailing_blank.csv",     [],            Nothing)
  , ("16_dates_units.csv",        ["W007"],      Nothing)
  , ("17_empty.csv",              ["LeftError"], Nothing)
  , ("18_header_only.csv",        ["LeftError"], Nothing)
  , ("19_whitespace.csv",         [],            Nothing)
  ]

main :: IO ()
main = do
  putStrLn "==========================================================="
  putStrLn " Dirty Data Demo — DataIO.CSV.loadAutoSafeWith showcase"
  putStrLn "==========================================================="
  putStrLn ""

  results <- forM fixtures $ \(name, expectCodes, mFix) -> do
    let path = dataDir <> "/" <> name
    sep
    putStrLn $ "  " ++ path
    sep
    actualCodes <- describeOne path CSV.defaultLoadOpts
    -- 期待コードと突き合わせて簡易判定
    let expectedSet = sort expectCodes
        actualSet   = sort actualCodes
        ok = expectedSet == actualSet
    printf "  期待 W コード: %s\n" (showCodes expectedSet)
    printf "  実測 W コード: %s\n"
           (showCodes actualSet ++ if ok then "  [OK]" else "  [DIFF]")

    -- 修復策があれば再ロードして I コードを出す
    case mFix of
      Just lo -> do
        putStrLn ""
        putStrLn "  → 修復案で再読込:"
        _ <- describeOne path lo
        return ()
      Nothing -> return ()

    putStrLn ""
    return (name, ok)

  -- 集計
  putStrLn "==========================================================="
  putStrLn " Summary"
  putStrLn "==========================================================="
  let nOK = length (filter snd results)
  printf "  期待コード一致: %d / %d\n" nOK (length results)
  forM_ results $ \(name, ok) ->
    printf "    %-32s %s\n" name ((if ok then "OK" else "DIFF") :: String)

sep :: IO ()
sep = putStrLn "-----------------------------------------------------------"

-- | 1 ファイルを読み、ログを stdout に出して、得られた W/I コードのリストを返す。
-- Left の場合は ["LeftError"] を返してテストの突き合わせに使う。
describeOne :: FilePath -> CSV.LoadOpts -> IO [T.Text]
describeOne path lo = do
  r <- CSV.loadAutoSafeWith lo path
  case r of
    Left err -> do
      printf "  [Parse error] %s\n" err
      return ["LeftError"]
    Right (df, lg) -> do
      let (nrows, _) = DX.dimensions df
          ncols      = length (DX.columnNames df)
      printf "  Rows / Cols : %d × %d\n" nrows ncols
      let es = Log.entries lg
      if null es
        then putStrLn "  (no warnings)"
        else mapM_ (TIO.putStrLn . ("  " <>) . Log.prettyEntry) es
      return [ Log.lgCode e | e <- es, sevWarn (Log.lgSev e) ]

sevWarn :: Log.Severity -> Bool
sevWarn Log.Warn = True
sevWarn _        = False

showCodes :: [T.Text] -> String
showCodes [] = "(none)"
showCodes xs = T.unpack (T.intercalate ", " xs)
