{-# LANGUAGE OverloadedStrings #-}
-- | CSV / TSV / SSV ローダ。Phase 2 以降は Hackage @dataframe@ の
-- 'DataFrame.Internal.DataFrame.DataFrame' を直接返す。
--
-- - CSV / TSV: Hackage の 'DX.readCsv' / 'DX.readTsv' に委譲 (型推論強化、
--   欠損ビットマップ対応)。
-- - SSV: Hackage に専用ローダが無いため cassava で読みつつ、各列を
--   'DX.fromList' で 'DX.Column' 化、'DX.insertColumn' で組み立てる。
module DataIO.CSV
  ( loadCSV
  , loadTSV
  , loadSSV
  , loadAuto
    -- * Safe loaders (Phase A2 — Either + LogReport を返す)
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
import qualified System.IO.Temp as Tmp
import System.IO (hClose)

type ParseError = String

-- ---------------------------------------------------------------------------
-- CSV / TSV: Hackage に直接委譲
-- ---------------------------------------------------------------------------

loadCSV :: FilePath -> IO (Either ParseError DXD.DataFrame)
loadCSV = loadHackage DX.readCsv

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

-- | Hackage 'readCsv' / 'readTsv' を例外捕捉付きで呼ぶ。
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

-- | CSV の安全版。空 / ヘッダのみ / Hackage の internal error を 'Left' で返す。
loadCsvSafe :: FilePath -> IO (Either ParseError (Loaded DXD.DataFrame))
loadCsvSafe = loadHackageSafe DX.readCsv

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

-- | SSV の安全版。
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

-- | 拡張子で自動振り分けする安全版。
loadAutoSafe :: FilePath -> IO (Either ParseError (Loaded DXD.DataFrame))
loadAutoSafe path
  | ".tsv" `isSuffixOf` path = loadTsvSafe path
  | ".ssv" `isSuffixOf` path = loadSsvSafe path
  | otherwise                = loadCsvSafe path

-- ---------------------------------------------------------------------------
-- Phase A4: ロードオプション
-- ---------------------------------------------------------------------------

-- | CLI から渡せる読込オプション。
--
-- * 'loSkip'      — 先頭 N 行を読み飛ばす
-- * 'loComment'   — 指定文字で始まる行を読み飛ばす (例: @\'#\'@)
-- * 'loNoHeader'  — ヘッダ無しと見なし、@col0, col1, ...@ を生成
-- * 'loStrict'    — 'LogReport' に Warn が含まれていたら 'Left' で短絡する
data LoadOpts = LoadOpts
  { loSkip     :: !Int
  , loComment  :: !(Maybe Char)
  , loNoHeader :: !Bool
  , loStrict   :: !Bool
  } deriving (Eq, Show)

defaultLoadOpts :: LoadOpts
defaultLoadOpts = LoadOpts 0 Nothing False False

-- | 'LoadOpts' を反映して 'loadAutoSafe' を呼ぶ。skip/comment/noHeader が
-- いずれも無指定のときは元ファイルを直接読む。指定があれば一時ファイルに
-- 前処理結果を書き出してから読む。
loadAutoSafeWith
  :: LoadOpts -> FilePath
  -> IO (Either ParseError (Loaded DXD.DataFrame))
loadAutoSafeWith opts path
  | needRewrite opts = withRewritten opts path go
  | otherwise        = go path mempty
  where
    go p extraLog = do
      r <- loadAutoSafe p
      case r of
        Left  e        -> return (Left e)
        Right (df, lg) ->
          let lg' = extraLog <> lg
          in if loStrict opts && hasWarnings lg'
               then return $ Left
                      ("strict: 警告が発生しました ("
                         <> show (length (entries lg'))
                         <> " 件)。--strict を外すか、--skip / --comment / --no-header で対処してください。")
               else return (Right (df, lg'))

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
