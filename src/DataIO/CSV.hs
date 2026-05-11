{-# LANGUAGE OverloadedStrings #-}
-- | CSV / TSV / SSV loaders that return Hackage @dataframe@'s
-- 'DataFrame.Internal.DataFrame.DataFrame' directly.
--
--   * CSV / TSV — delegated to Hackage's 'DX.readCsv' / 'DX.readTsv'
--     (improved type inference, missing-bitmap support).
--   * SSV       — Hackage has no dedicated loader, so we read with
--     @cassava@ and assemble columns via 'DX.fromList' /
--     'DX.insertColumn'.
module DataIO.CSV
  ( loadCSV
  , loadTSV
  , loadSSV
  , loadAuto
    -- * Safe loaders (return Either + LogReport)
  , loadCsvSafe
  , loadTsvSafe
  , loadSsvSafe
  , loadAutoSafe
    -- * Loader options (Phase A4)
  , LoadOpts (..)
  , defaultLoadOpts
  , loadAutoSafeWith
  , ParseError
  ) where

import qualified DataFrame                    as DX
import qualified DataFrame.IO.CSV             as DXIO
import qualified DataFrame.Internal.DataFrame as DXD

import Control.Exception (SomeException, try, evaluate)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.List (isSuffixOf)
import Data.Csv (NamedRecord, Header, defaultDecodeOptions, DecodeOptions(..), decodeByNameWith)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import System.IO.Error (tryIOError)
import Text.Read (readMaybe)

import DataIO.Log (Loaded, LogReport, mkInfo, hasWarnings, logReport, entries)
import DataIO.Health (inspectWithPreview)
import qualified DataIO.Sniff as Sniff
import qualified System.IO.Temp as Tmp
import System.IO (hClose)

-- | Parse-error message (a plain 'String').
type ParseError = String

-- ---------------------------------------------------------------------------
-- CSV / TSV: Hackage に直接委譲
-- ---------------------------------------------------------------------------

-- | Load a CSV file via Hackage's @readCsv@.
loadCSV :: FilePath -> IO (Either ParseError DXD.DataFrame)
loadCSV = loadHackage DX.readCsv

-- | Load a TSV file via Hackage's @readTsv@.
loadTSV :: FilePath -> IO (Either ParseError DXD.DataFrame)
loadTSV = loadHackage DX.readTsv

loadHackage :: (FilePath -> IO DXD.DataFrame)
            -> FilePath -> IO (Either ParseError DXD.DataFrame)
loadHackage reader path = do
  r <- try (reader path) :: IO (Either SomeException DXD.DataFrame)
  return $ case r of
    Left  e  -> Left ("CSV/TSV loader failed: " ++ show e)
    Right df -> Right df

-- ---------------------------------------------------------------------------
-- SSV: cassava で読み、Hackage 'DataFrame' に詰め替える
-- ---------------------------------------------------------------------------

-- | Load a space-separated value file via @cassava@; the result is
-- repackaged into a Hackage 'DXD.DataFrame'.
loadSSV :: FilePath -> IO (Either ParseError DXD.DataFrame)
loadSSV path = do
  content <- BL.readFile path
  let opts = defaultDecodeOptions { decDelimiter = fromIntegral (ord ' ') }
  case decodeByNameWith opts content of
    Left err          -> return (Left err)
    Right (hdr, rows) -> return (Right (toHackageDF hdr rows))

toHackageDF :: Header -> V.Vector NamedRecord -> DXD.DataFrame
toHackageDF hdr rows =
  foldl insert DX.empty
    [ (TE.decodeUtf8 key, classifyCells key rows) | key <- V.toList hdr ]
  where
    insert df (name, col) = DX.insertColumn name col df

-- | 列の値を全て読み 'Double' として parse できれば数値列、そうでなければ Text 列。
classifyCells :: BS.ByteString -> V.Vector NamedRecord -> DX.Column
classifyCells key rows =
  let cells = V.map (TE.decodeUtf8 . HM.lookupDefault "" key) rows
      texts = V.toList cells
  in case mapM (readMaybe . T.unpack) texts of
       Just nums -> DX.fromList (nums :: [Double])
       Nothing   -> DX.fromList (texts :: [Text])

-- ---------------------------------------------------------------------------
-- 拡張子による自動振り分け
-- ---------------------------------------------------------------------------

-- | Auto-dispatch by file extension: @.tsv@ → @loadTSV@, @.ssv@ →
-- 'loadSSV', otherwise 'loadCSV'.
loadAuto :: FilePath -> IO (Either ParseError DXD.DataFrame)
loadAuto path
  | ".tsv" `isSuffixOf` path = loadTSV path
  | ".ssv" `isSuffixOf` path = loadSSV path
  | otherwise                = loadCSV path

-- ---------------------------------------------------------------------------
-- Safe loaders (Phase A2)
--
-- 空ファイル / ヘッダのみ / Hackage の internal 'error' を全て 'Either' に
-- 押し込め、call stack を端末に出さないようにする。'Loaded' で副次的な
-- ログを返せる (現状はパス情報のみ、A3 で W コードが付き始める)。
-- ---------------------------------------------------------------------------

-- | ファイルを行ベクトルで先読みして、空 / ヘッダのみを検出する。
-- Right に返るのは「行リスト (改行で split, 空行は除く)」。
preflight :: FilePath -> IO (Either ParseError [BS.ByteString])
preflight path = do
  e <- tryIOError (BS.readFile path)
  case e of
    Left ioe  -> return (Left ("Cannot read file: " ++ show ioe))
    Right bs0 ->
      let bs   = stripBOM bs0
          rows = filter (not . BS.null) (BS.split (fromIntegral (ord '\n')) bs)
          rs   = map stripCR rows
      in case rs of
           []      -> return (Left "Empty file (no rows).")
           [_only] -> return (Left "File has only a header row (no data).")
           _       -> return (Right rs)

stripCR :: BS.ByteString -> BS.ByteString
stripCR bs
  | BS.null bs                  = bs
  | BS.last bs == fromIntegral (ord '\r') = BS.init bs
  | otherwise                   = bs

-- | UTF-8 BOM (EF BB BF) を取り除く。
stripBOM :: BS.ByteString -> BS.ByteString
stripBOM bs
  | BS.length bs >= 3
  , BS.index bs 0 == 0xEF
  , BS.index bs 1 == 0xBB
  , BS.index bs 2 == 0xBF = BS.drop 3 bs
  | otherwise             = bs

-- | Hackage @readCsv@ / @readTsv@ を例外捕捉付きで呼ぶ。
runHackageSafe
  :: (FilePath -> IO DXD.DataFrame)
  -> FilePath
  -> IO (Either ParseError DXD.DataFrame)
runHackageSafe reader path = do
  r <- try (reader path >>= evaluate) :: IO (Either SomeException DXD.DataFrame)
  return $ case r of
    Right df -> Right df
    Left  e  -> Left (cleanError (show e))

-- | call-stack 行を取り除き、ユーザに見せる 1 行メッセージに整形する。
cleanError :: String -> String
cleanError = takeWhile (/= '\n')

-- | Safe CSV loader. Returns 'Left' on empty input, header-only files,
-- or Hackage internal errors instead of bubbling them up as exceptions.
loadCsvSafe :: FilePath -> IO (Either ParseError (Loaded DXD.DataFrame))
loadCsvSafe = loadHackageSafe DX.readCsv

-- | Safe TSV loader (TSV analogue of 'loadCsvSafe').
loadTsvSafe :: FilePath -> IO (Either ParseError (Loaded DXD.DataFrame))
loadTsvSafe = loadHackageSafe DX.readTsv

loadHackageSafe
  :: (FilePath -> IO DXD.DataFrame) -> FilePath
  -> IO (Either ParseError (Loaded DXD.DataFrame))
loadHackageSafe reader path = do
  pre <- preflight path
  case pre of
    Left e   -> return (Left e)
    Right rs -> do
      r <- runHackageSafe reader path
      return $ case r of
        Left  e  -> Left e
        Right df -> Right (df, inspectWithPreview (previewBytes rs) df)

-- | Safe SSV loader (SSV analogue of 'loadCsvSafe').
loadSsvSafe :: FilePath -> IO (Either ParseError (Loaded DXD.DataFrame))
loadSsvSafe path = do
  pre <- preflight path
  case pre of
    Left e  -> return (Left e)
    Right rs -> do
      r <- loadSSV path
      return $ case r of
        Left  e  -> Left e
        Right df -> Right (df, inspectWithPreview (previewBytes rs) df)

-- | 先頭 8 KB 程度を健全性検査のプレビュー用に切り出す。
previewBytes :: [BS.ByteString] -> BS.ByteString
previewBytes rs =
  let joined = BS.intercalate "\n" rs
  in BS.take 8192 joined

-- | Auto-dispatch safe loader: picks 'loadCsvSafe' / 'loadTsvSafe' /
-- 'loadSsvSafe' from the file extension.
loadAutoSafe :: FilePath -> IO (Either ParseError (Loaded DXD.DataFrame))
loadAutoSafe path
  | ".tsv" `isSuffixOf` path = loadTsvSafe path
  | ".ssv" `isSuffixOf` path = loadSsvSafe path
  | otherwise                = loadCsvSafe path

-- ---------------------------------------------------------------------------
-- Phase A4: ロードオプション
-- ---------------------------------------------------------------------------

-- | Loading options that can be supplied from the CLI.
data LoadOpts = LoadOpts
  { loSkip     :: !Int            -- ^ Skip the first @N@ rows.
  , loComment  :: !(Maybe Char)   -- ^ Skip rows starting with this character (e.g. @\'#\'@).
  , loNoHeader :: !Bool           -- ^ Treat the file as header-less and generate @col0, col1, …@.
  , loStrict   :: !Bool           -- ^ Short-circuit to 'Left' if the
                                  --   @LogReport@ contains a @Warn@ entry.
  , loSniff    :: !Bool           -- ^ Enable auto-inference (default 'True').
  , loDelim    :: !(Maybe Char)   -- ^ Override the delimiter ('Nothing'
                                  --   uses the file extension and sniff result).
  } deriving (Eq, Show)

-- | Default loading options: no skip, no comment char, header expected,
-- non-strict, sniff enabled, no delimiter override.
defaultLoadOpts :: LoadOpts
defaultLoadOpts = LoadOpts 0 Nothing False False True Nothing

-- | Run 'loadAutoSafe' with the given @LoadOpts@. When @skip@,
-- @comment@ and @noHeader@ are all unset the file is read directly;
-- otherwise the request is realized by writing to a temporary file
-- 前処理結果を書き出してから読む。
--
-- 'loSniff' が True (デフォルト) のときは、ユーザ未指定の項目に限り
-- 'DataIO.Sniff.sniffBytes' の結果で自動補完する:
--
-- * 'loSkip == 0' なら sniff の skip 値で上書き
-- * 'loComment == Nothing' なら sniff のコメント文字で上書き
-- * 'loNoHeader == False' で sniff が「ヘッダ無し」を強く示唆したら上書き
--
-- 自動推論で値が変わったときは I013 (Info コード) として LogReport に残す。
loadAutoSafeWith
  :: LoadOpts -> FilePath
  -> IO (Either ParseError (Loaded DXD.DataFrame))
loadAutoSafeWith opts0 path = do
  -- Sniff: 必要なら冒頭バイト列を読んでオプションを補完する
  (opts, sniffLog) <- if loSniff opts0
    then do
      eRaw <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      case eRaw of
        Left _    -> return (opts0, mempty)
        Right raw -> return (applySniff opts0 (Sniff.sniffBytes (BS.take 8192 raw)))
    else return (opts0, mempty)
  if needRewrite opts
    then withRewritten opts path (\p extra -> go opts p (sniffLog <> extra))
    else go opts path sniffLog
  where
    go effOpts p extraLog = do
      -- delimiter 指定があれば Hackage の readCsvWithOpts を使う
      r <- case loDelim effOpts of
        Nothing -> loadAutoSafe p
        Just c  -> loadCsvWithDelim c p
      case r of
        Left  e        -> return (Left e)
        Right (df, lg) ->
          let lg' = extraLog <> lg
          in if loStrict opts0 && hasWarnings lg'
               then return $ Left
                      ("strict: 警告が発生しました ("
                         <> show (length (entries lg'))
                         <> " 件)。--strict を外すか、--skip / --comment / --no-header / --no-sniff で対処してください。")
               else return (Right (df, lg'))

-- | 指定 delimiter で CSV を読み、loadAutoSafe 同等の Loaded を返す。
loadCsvWithDelim
  :: Char -> FilePath -> IO (Either ParseError (Loaded DXD.DataFrame))
loadCsvWithDelim c path = do
  pre <- preflight path
  case pre of
    Left e  -> return (Left e)
    Right rs -> do
      let opts = DXIO.defaultReadOptions { DXIO.columnSeparator = c }
      r <- try (DXIO.readCsvWithOpts opts path >>= evaluate)
             :: IO (Either SomeException DXD.DataFrame)
      return $ case r of
        Left  e  -> Left (cleanError (show e))
        Right df -> Right (df, inspectWithPreview (previewBytes rs) df)

-- | sniff 結果を @LoadOpts@ に反映する。ユーザ指定がある項目 (>0 / Just /
-- True) は尊重し、未指定のところだけ書き換える。書き換えた項目は
-- I013 ログに残す。
applySniff :: LoadOpts -> Sniff.Sniff -> (LoadOpts, LogReport)
applySniff o s =
  let (skip', noteSkip)   =
        if loSkip o == 0 && Sniff.sfSkip s > 0
          then (Sniff.sfSkip s,
                Just $ "先頭 " <> tShow (Sniff.sfSkip s) <> " 行を skip (sniff)")
          else (loSkip o, Nothing)
      (comm', noteComm)  =
        case (loComment o, Sniff.sfCommentChar s) of
          (Nothing, Just c) -> (Just c,
                                Just $ "コメント文字 '" <> T.singleton c <> "' を採用 (sniff)")
          _                 -> (loComment o, Nothing)
      (nohd', noteHdr)   =
        if not (loNoHeader o) && not (Sniff.sfHasHeader s)
          then (True,
                Just "ヘッダ無しと推論 (sniff): col0... を生成")
          else (loNoHeader o, Nothing)
      (delim', noteDelim) =
        case (loDelim o, Sniff.sfDelim s) of
          (Nothing, c) | c /= ',' ->
            (Just c, Just $ "delimiter '" <> T.singleton c <> "' を採用 (sniff)")
          _                       -> (loDelim o, Nothing)
      lg = mconcat
             [ logReport (mkInfo "I013" m Nothing)
             | Just m <- [noteSkip, noteComm, noteHdr, noteDelim]
             ]
  in (o { loSkip = skip', loComment = comm', loNoHeader = nohd'
        , loDelim = delim' }, lg)

needRewrite :: LoadOpts -> Bool
needRewrite o = loSkip o > 0
             || loComment o /= Nothing
             || loNoHeader o

-- | 前処理 (skip / comment / no-header) を施した一時ファイルを作って
-- アクションに渡す。withSystemTempFile で自動クリーンアップ。
withRewritten
  :: LoadOpts -> FilePath
  -> (FilePath -> LogReport -> IO (Either ParseError (Loaded DXD.DataFrame)))
  -> IO (Either ParseError (Loaded DXD.DataFrame))
withRewritten opts path act = do
  raw <- BS.readFile path
  let (rewritten, plog) = rewriteContent opts raw
  Tmp.withSystemTempFile "ha-rewrite-.csv" $ \tmp h -> do
    BS.hPut h rewritten
    hClose h
    act tmp plog

-- | LoadOpts に従ってバイト列を変換し、変換ログを返す。
rewriteContent :: LoadOpts -> BS.ByteString -> (BS.ByteString, LogReport)
rewriteContent opts bs0 =
  let nl       = fromIntegral (ord '\n')
      bs       = stripBOM bs0
      rawLines = BS.split nl bs
      lines0   = map stripCR rawLines
      (afterSkip, skippedNote) =
        if loSkip opts > 0
          then ( drop (loSkip opts) lines0
               , logReport (mkInfo "I010"
                   ("先頭 " <> tShow (loSkip opts) <> " 行を skip しました。")
                   Nothing))
          else (lines0, mempty)
      (afterComment, commentNote) = case loComment opts of
        Just ch ->
          let chBy = fromIntegral (ord ch)
              isC l = case BS.uncons (BS.dropWhile (== fromIntegral (ord ' ')) l) of
                        Just (c, _) -> c == chBy
                        Nothing     -> False
              kept   = filter (not . isC) afterSkip
              dropped = length afterSkip - length kept
          in if dropped > 0
               then ( kept
                    , logReport (mkInfo "I011"
                        ("コメント文字 '" <> T.singleton ch
                            <> "' で始まる行を " <> tShow dropped <> " 件 skip しました。")
                        Nothing))
               else (kept, mempty)
        Nothing -> (afterSkip, mempty)
      (afterHeader, headerNote) =
        if loNoHeader opts
          then case dropWhile BS.null afterComment of
                 [] -> (afterComment, mempty)
                 (firstRow:_) ->
                   let nCols = length (BS.split (fromIntegral (ord ',')) firstRow)
                       hdr   = BS.intercalate ","
                                 [ TE.encodeUtf8 (T.pack ("col" ++ show i))
                                 | i <- [0 .. nCols - 1] ]
                       lg = logReport (mkInfo "I012"
                              ("--no-header: ヘッダ "
                                 <> tShow nCols
                                 <> " 列 (col0...) を生成しました。")
                              Nothing)
                   in (hdr : afterComment, lg)
          else (afterComment, mempty)
      out      = BS.intercalate (BS.singleton nl) afterHeader
      logTotal = skippedNote <> commentNote <> headerNote
  in (out, logTotal)

tShow :: Show a => a -> Text
tShow = T.pack . show
