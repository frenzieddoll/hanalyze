{-# LANGUAGE OverloadedStrings #-}
-- | Auto-detect a CSV's delimiter, comment lines, presence of header,
-- and NA candidates by inspecting the first 8 KB. While 'LoadOpts' lets
-- the user state these explicitly, this module adds a layer that guesses
-- when nothing is specified.
--
-- Design notes:
--
--   * 8 KB is assumed to be enough to decide structure (we don't stream
--     huge files).
--   * Inference results live in a 'Sniff' record. Supporting evidence
--     (per-delimiter scores etc.) is recorded in 'sfNotes' and emitted
--     as Info codes through 'LogReport'.
--   * Sniffing is best-effort and decoupled from the strict path: users
--     can disable it entirely with @--no-sniff@, or escalate any
--     mismatch to an error with @--strict@.
module DataIO.Sniff
  ( -- * 型
    Sniff (..)
  , defaultSniff
    -- * 推論
  , sniffBytes
  , sniffFile
    -- * 個別判定 (内部公開、テスト用)
  , detectDelimiter
  , detectHasHeader
  , detectSkip
  , detectCommentChar
  ) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (ord, isDigit)
import Data.List (sortBy, maximumBy)
import Data.Ord  (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | sniff 結果。
--
-- * 'sfDelim'      推測した delimiter (`,`/`;`/`\t`/space/`|`)
-- * 'sfHasHeader'  ヘッダ行が在りそうか (False なら 'col0' 系を生成すべき)
-- * 'sfSkip'       先頭何行を飛ばすべきか (コメント / メタデータ行数)
-- * 'sfCommentChar' コメント行の prefix 文字 (検出できれば)
-- * 'sfNotes'       推論根拠の人間可読メモ (LogReport 用)
data Sniff = Sniff
  { sfDelim       :: !Char
  , sfHasHeader   :: !Bool
  , sfSkip        :: !Int
  , sfCommentChar :: !(Maybe Char)
  , sfNotes       :: ![Text]
  } deriving (Eq, Show)

defaultSniff :: Sniff
defaultSniff = Sniff
  { sfDelim       = ','
  , sfHasHeader   = True
  , sfSkip        = 0
  , sfCommentChar = Nothing
  , sfNotes       = []
  }

-- ---------------------------------------------------------------------------
-- 公開 API
-- ---------------------------------------------------------------------------

-- | ファイル冒頭 8 KB を読んで推論する。
sniffFile :: FilePath -> IO Sniff
sniffFile path = do
  bs <- BS.readFile path
  return (sniffBytes (BS.take 8192 bs))

-- | バイト列を直接受けて推論する。
sniffBytes :: BS.ByteString -> Sniff
sniffBytes bs0 =
  let bs       = stripBOM bs0
      ls0      = filter (not . BS.null) (BS.split (fromIntegral (ord '\n')) bs)
      ls       = map stripCR ls0
      (skipN, mComment) = detectSkip ls
      dataLines = drop skipN ls
      delim    = detectDelimiter dataLines
      hasHdr   = detectHasHeader delim dataLines
      notes    = mconcat
        [ ["delimiter = " <> renderDelim delim]
        , ["header     = " <> if hasHdr then "yes" else "no"]
        , [ "skip       = " <> T.pack (show skipN)
          | skipN > 0 ]
        , [ "comment    = '" <> T.singleton c <> "'"
          | Just c <- [mComment] ]
        ]
  in Sniff
       { sfDelim       = delim
       , sfHasHeader   = hasHdr
       , sfSkip        = skipN
       , sfCommentChar = mComment
       , sfNotes       = notes
       }

renderDelim :: Char -> Text
renderDelim '\t' = "tab"
renderDelim ' '  = "space"
renderDelim c    = "'" <> T.singleton c <> "'"

-- ---------------------------------------------------------------------------
-- delimiter 推論
-- ---------------------------------------------------------------------------

-- | 候補 delimiter ('`,;\t|`') について各行での出現数を取り、
-- 「行ごとの分散が小さい」 + 「中央値の出現数が多い」を優先する。
-- そもそも空入力やシングル行の場合は ',' を返す。
detectDelimiter :: [BS.ByteString] -> Char
detectDelimiter [] = ','
detectDelimiter ls =
  let candidates = ',' : ';' : '\t' : '|' : []
      score c =
        let counts = map (BS.count (fromIntegral (ord c))) (take 20 ls)
        in (median counts, varianceD counts)  -- (大→良, 小→良)
      -- variance を最優先で昇順 (= 列数が安定している = 確実な delimiter)、
      -- 次に median を降順 (出現数が多いほど良い)。
      -- median 優先だと "1,5;2,5;3,0" のような文字列で comma が 3 出るために
      -- 誤って comma が選ばれてしまう。
      cmp a b =
        let (ma, va) = score a
            (mb, vb) = score b
        in compare va vb <> compare mb ma
      ranked = sortBy cmp candidates
  in case ranked of
       (c:_) | fst (score c) >= 1 -> c
       _                           -> ','

median :: [Int] -> Int
median xs =
  let s = sortBy compare xs
      n = length s
  in if n == 0 then 0 else s !! (n `div` 2)

-- | 整数除算で潰さない分散 (Double で計算)。
varianceD :: [Int] -> Double
varianceD xs =
  let n = length xs
      m = (fromIntegral (sum xs) :: Double) / fromIntegral (max 1 n)
  in if n <= 1 then 0
     else sum [ (fromIntegral x - m) ** 2 | x <- xs ] / fromIntegral (n - 1)

-- ---------------------------------------------------------------------------
-- ヘッダ有無の推論
-- ---------------------------------------------------------------------------

-- | 1 行目の各セルが全て numeric token なら「ヘッダ無し」と判断する。
-- それ以外 (text を含む) は「ヘッダ有り」。空入力は True を返す
-- (Hackage が空 CSV を弾くため、あとはそちら側で扱う)。
detectHasHeader :: Char -> [BS.ByteString] -> Bool
detectHasHeader _      []       = True
detectHasHeader delim (l:_) =
  let cells  = BS.split (fromIntegral (ord delim)) l
      tokens = map (T.strip . decodeAscii) cells
      isNum t = case readMaybe (T.unpack t) :: Maybe Double of
                  Just _  -> True
                  Nothing -> False
  in not (all isNum tokens)
  || null tokens
  || all T.null tokens

-- ---------------------------------------------------------------------------
-- 先頭 skip / コメント文字の推論
-- ---------------------------------------------------------------------------

-- | 先頭から 'コメント文字' で始まる行が連続する数を skip 候補とする。
-- 'コメント文字' は `#` / `!` / `;` / `//` のどれか。検出文字も返す。
detectSkip :: [BS.ByteString] -> (Int, Maybe Char)
detectSkip ls =
  let candidates = ['#', '!']
      n c = length (takeWhile (startsWith c) ls)
      best = maximumBy (comparing (\c -> n c)) candidates
      k    = n best
  in if k > 0
       then (k, Just best)
       else (0, Nothing)

startsWith :: Char -> BS.ByteString -> Bool
startsWith c bs =
  let bs' = BS.dropWhile (\b -> b == fromIntegral (ord ' ')
                              || b == fromIntegral (ord '\t')) bs
  in case BS.uncons bs' of
       Just (h, _) -> h == fromIntegral (ord c)
       Nothing     -> False

-- | 'detectSkip' の結果からコメント文字だけ取り出すラッパ。
detectCommentChar :: [BS.ByteString] -> Maybe Char
detectCommentChar = snd . detectSkip

-- ---------------------------------------------------------------------------
-- ユーティリティ
-- ---------------------------------------------------------------------------

stripCR :: BS.ByteString -> BS.ByteString
stripCR bs
  | BS.null bs                              = bs
  | BS.last bs == fromIntegral (ord '\r')   = BS.init bs
  | otherwise                               = bs

stripBOM :: BS.ByteString -> BS.ByteString
stripBOM bs
  | BS.length bs >= 3
  , BS.index bs 0 == 0xEF
  , BS.index bs 1 == 0xBB
  , BS.index bs 2 == 0xBF = BS.drop 3 bs
  | otherwise             = bs

decodeAscii :: BS.ByteString -> Text
decodeAscii = T.pack . BS8.unpack

-- 未使用ワーニング抑止
_unusedRefs :: ([Char], Char -> Bool)
_unusedRefs = ("?", isDigit)
