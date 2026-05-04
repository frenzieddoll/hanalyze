{-# LANGUAGE OverloadedStrings #-}
-- | Structured warning / informational messaging shared by data loaders
-- and preprocessing.
--
--   * 'LogEntry'        — a single message (severity / code / body / hint).
--   * 'LogReport'       — a 'Monoid' wrapper around @[LogEntry]@.
--   * 'Loaded'          — the @(value, log)@ pair returned by every loader.
--   * 'printLogReport'  — stdout pretty printer.
--   * 'logEntriesAsHtml' — adapter for 'Viz.ReportBuilder'.
--
-- 利用シナリオ:
--
-- @
-- (df, lg) <- loadCsvSafe path  -- :: IO (Either ParseError (Loaded DataFrame))
-- printLogReport lg              -- 警告を端末に出す
-- when (isStrict opts && hasErrors lg) $ exitFailure
-- @
module DataIO.Log
  ( -- * 型
    Severity (..)
  , LogEntry (..)
  , LogReport
  , Loaded
    -- * 構築
  , mkInfo
  , mkWarn
  , mkErr
  , addEntry
  , logReport
  , noLog
    -- * 集約
  , entries
  , hasErrors
  , hasWarnings
  , severityCount
    -- * 出力
  , printLogReport
  , prettyEntry
  ) where

import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | メッセージ重要度。
data Severity = Info | Warn | Err
  deriving (Eq, Ord, Show)

-- | 1 件のログエントリ。
--
-- @lgCode@ は @W001@ / @E002@ 形式の安定した識別子。出力 grep やテストで使う。
data LogEntry = LogEntry
  { lgSev  :: !Severity
  , lgCode :: !Text
  , lgMsg  :: !Text
  , lgHint :: !(Maybe Text)
  } deriving (Eq, Show)

-- | エントリのリストラッパ。'Monoid' で連結できる。
newtype LogReport = LogReport { entries :: [LogEntry] }
  deriving (Eq, Show)

instance Semigroup LogReport where
  LogReport a <> LogReport b = LogReport (a ++ b)

instance Monoid LogReport where
  mempty = LogReport []

-- | 値とログのペア。loader / cleaner が共通に返す形。
type Loaded a = (a, LogReport)

-- ---------------------------------------------------------------------------
-- 構築
-- ---------------------------------------------------------------------------

mkInfo :: Text -> Text -> Maybe Text -> LogEntry
mkInfo c m h = LogEntry Info c m h

mkWarn :: Text -> Text -> Maybe Text -> LogEntry
mkWarn c m h = LogEntry Warn c m h

mkErr :: Text -> Text -> Maybe Text -> LogEntry
mkErr c m h = LogEntry Err c m h

-- | エントリを末尾追加。
addEntry :: LogEntry -> LogReport -> LogReport
addEntry e (LogReport xs) = LogReport (xs ++ [e])

-- | 単一エントリから 'LogReport' を作る。
logReport :: LogEntry -> LogReport
logReport e = LogReport [e]

-- | 空のログ ('mempty' のエイリアス)。
noLog :: LogReport
noLog = mempty

-- ---------------------------------------------------------------------------
-- 集約
-- ---------------------------------------------------------------------------

hasErrors :: LogReport -> Bool
hasErrors (LogReport xs) = any ((== Err) . lgSev) xs

hasWarnings :: LogReport -> Bool
hasWarnings (LogReport xs) = any ((== Warn) . lgSev) xs

severityCount :: Severity -> LogReport -> Int
severityCount s (LogReport xs) = length (filter ((== s) . lgSev) xs)

-- ---------------------------------------------------------------------------
-- 出力
-- ---------------------------------------------------------------------------

prettyEntry :: LogEntry -> Text
prettyEntry e =
  let prefix = case lgSev e of
        Info -> "[INFO]  "
        Warn -> "[WARN]  "
        Err  -> "[ERROR] "
      hint = case lgHint e of
        Nothing -> ""
        Just h  -> "\n        ヒント: " <> h
  in prefix <> lgCode e <> ": " <> lgMsg e <> hint

-- | ログを stdout に出す。空ログは何も書かない。
printLogReport :: LogReport -> IO ()
printLogReport (LogReport []) = return ()
printLogReport (LogReport xs) = do
  let nW = length (filter ((== Warn) . lgSev) xs)
      nE = length (filter ((== Err)  . lgSev) xs)
      nI = length (filter ((== Info) . lgSev) xs)
      summary = T.concat
        [ "(" , T.pack (show (length xs)), " entries"
        , if nE > 0 then ", " <> T.pack (show nE) <> " error"   else ""
        , if nW > 0 then ", " <> T.pack (show nW) <> " warning" else ""
        , if nI > 0 then ", " <> T.pack (show nI) <> " info"    else ""
        , ")"
        ]
  TIO.putStrLn ("--- DataIO log " <> summary <> " ---")
  mapM_ (TIO.putStrLn . prettyEntry) xs
  TIO.putStrLn "----------------------"
