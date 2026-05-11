{-# LANGUAGE OverloadedStrings #-}
-- | Structured warning / informational messaging shared by data loaders
-- and preprocessing.
--
--   * 'LogEntry'        — a single message (severity / code / body / hint).
--   * @LogReport@       — a 'Monoid' wrapper around @[LogEntry]@.
--   * 'Loaded'          — the @(value, log)@ pair returned by every loader.
--   * 'printLogReport'  — stdout pretty printer.
--   * @logEntriesAsHtml@ — adapter for 'Hanalyze.Viz.ReportBuilder'.
--
-- 利用シナリオ:
--
-- @
-- (df, lg) <- loadCsvSafe path  -- :: IO (Either ParseError (Loaded DataFrame))
-- printLogReport lg              -- 警告を端末に出す
-- when (isStrict opts && hasErrors lg) $ exitFailure
-- @
module Hanalyze.DataIO.Log
  ( -- * 型
    Severity (..)
  , LogEntry (..)
  , LogReport
  , Loaded
    -- * Construction
  , mkInfo
  , mkWarn
  , mkErr
  , addEntry
  , logReport
  , noLog
    -- * Aggregation
  , entries
  , hasErrors
  , hasWarnings
  , severityCount
    -- * Output
  , printLogReport
  , prettyEntry
  ) where

import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | Message severity.
data Severity = Info | Warn | Err
  deriving (Eq, Ord, Show)

-- | A single log entry.
--
-- 'lgCode' is a stable identifier of the form @W001@ / @E002@ used for
-- grepping output and writing tests against the log.
data LogEntry = LogEntry
  { lgSev  :: !Severity
  , lgCode :: !Text
  , lgMsg  :: !Text
  , lgHint :: !(Maybe Text)
  } deriving (Eq, Show)

-- | A 'Monoid' list-wrapper of 'LogEntry'.
newtype LogReport = LogReport { entries :: [LogEntry] }
  deriving (Eq, Show)

instance Semigroup LogReport where
  LogReport a <> LogReport b = LogReport (a ++ b)

instance Monoid LogReport where
  mempty = LogReport []

-- | A value paired with its log. Loaders and cleaners return this shape.
type Loaded a = (a, LogReport)

-- ---------------------------------------------------------------------------
-- 構築
-- ---------------------------------------------------------------------------

-- | Build an 'Info' entry from @(code, message, optional hint)@.
mkInfo :: Text -> Text -> Maybe Text -> LogEntry
mkInfo c m h = LogEntry Info c m h

-- | Build a @Warn@ entry.
mkWarn :: Text -> Text -> Maybe Text -> LogEntry
mkWarn c m h = LogEntry Warn c m h

-- | Build an 'Err' entry.
mkErr :: Text -> Text -> Maybe Text -> LogEntry
mkErr c m h = LogEntry Err c m h

-- | Append an entry to the end of a report.
addEntry :: LogEntry -> LogReport -> LogReport
addEntry e (LogReport xs) = LogReport (xs ++ [e])

-- | Make a @LogReport@ that contains a single entry.
logReport :: LogEntry -> LogReport
logReport e = LogReport [e]

-- | The empty log (alias for 'mempty').
noLog :: LogReport
noLog = mempty

-- ---------------------------------------------------------------------------
-- 集約
-- ---------------------------------------------------------------------------

-- | True if the report contains any 'Err' entries.
--
-- >>> hasErrors noLog
-- False
-- >>> hasErrors (logReport (mkErr "E001" "boom" Nothing))
-- True
hasErrors :: LogReport -> Bool
hasErrors (LogReport xs) = any ((== Err) . lgSev) xs

-- | True if the report contains any @Warn@ entries.
hasWarnings :: LogReport -> Bool
hasWarnings (LogReport xs) = any ((== Warn) . lgSev) xs

-- | Number of entries with the given severity.
severityCount :: Severity -> LogReport -> Int
severityCount s (LogReport xs) = length (filter ((== s) . lgSev) xs)

-- ---------------------------------------------------------------------------
-- 出力
-- ---------------------------------------------------------------------------

-- | Pretty-print a single 'LogEntry' (severity tag + code + message,
-- and optionally the hint on a second line).
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

-- | Print the log to stdout. Empty logs print nothing.
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
