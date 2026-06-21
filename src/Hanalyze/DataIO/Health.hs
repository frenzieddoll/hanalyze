{-# LANGUAGE OverloadedStrings #-}
-- | DataFrame health check. Surfaces the "looks suspicious" patterns that
-- can hide in a successfully-loaded DataFrame, as warning codes.
--
-- Codes detected:
--
--   * @W001@ — header is suspect (all column names parse as numbers).
--   * @W003@ — ragged: per-column lengths differ (Hackage normally pads,
--     but we double-check).
--   * @W004@ — duplicate / empty / surrounding-whitespace column names.
--   * @W005@ — delimiter mismatch: single-column DataFrame whose values
--     contain another delimiter candidate.
--   * @W006@ — heterogeneous mix of NA strings.
--   * @W007@ — unit suffix inferred (most cells in a Text column match
--     @^\\d+\\.?\\d*[a-zA-Z]+$@).
--   * @W008@ — currency or thousand-separator suspect.
--
-- Auxiliary checks that need a raw-byte preview are in
-- 'inspectWithPreview'.
-- それ以外は 'inspectDataFrame' で DataFrame だけから判定可能。
--
-- 利用シナリオ:
--
-- @
-- (df, lg0) <- loadAutoSafe path
-- let lg = lg0 <> inspectDataFrame df
-- printLogReport lg
-- @
module Hanalyze.DataIO.Health
  ( inspectDataFrame
  , inspectWithPreview
  , detectHeaderless
  , detectDuplicateBlankNames
  , detectMixedNAStrings
  , detectUnitSuffix
  , detectThousandsCurrency
  , detectDelimiterMismatch
  , detectCommentLines
  , detectRagged
  ) where

import qualified DataFrame.Internal.DataFrame  as DX
import qualified DataFrame.Operations.Core     as DX
import qualified DataFrame.Internal.Column    as DXC
import qualified DataFrame.Internal.DataFrame as DXD

import qualified Data.ByteString      as BS
import qualified Data.Map.Strict      as Map
import Data.Char (isDigit, isAlpha, ord)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Read (readMaybe)

import Hanalyze.DataIO.Log (LogEntry, LogReport, mkWarn, logReport, noLog)
import Hanalyze.DataIO.Convert (getMaybeTextVec)
import Hanalyze.DataIO.Preprocess (isNAString)

-- ---------------------------------------------------------------------------
-- 公開エントリポイント
-- ---------------------------------------------------------------------------

-- | Aggregate every W-code that can be checked from the DataFrame
-- alone (without the source bytes).
inspectDataFrame :: DXD.DataFrame -> LogReport
inspectDataFrame df = mconcat
  [ detectHeaderless df
  , detectDuplicateBlankNames df
  , detectRagged df
  , detectMixedNAStrings df
  , detectUnitSuffix df
  , detectThousandsCurrency df
  ]

-- | DataFrame plus a leading raw-byte preview, used for the W-codes
-- that need both inputs (e.g. W005 delimiter
-- ミスマッチ / W004 ヘッダ行レベルの重複) も合わせて返す。
inspectWithPreview :: BS.ByteString -> DXD.DataFrame -> LogReport
inspectWithPreview preview df = mconcat
  [ inspectDataFrame df
  , detectDelimiterMismatch preview df
  , detectRawHeaderIssues preview df
  , detectCommentLines preview
  ]

-- ---------------------------------------------------------------------------
-- W002 コメント行 (#/!/// 等で始まる先頭行)
-- ---------------------------------------------------------------------------

detectCommentLines :: BS.ByteString -> LogReport
detectCommentLines preview =
  let ls = take 8 (BS.split (fromIntegral (ord '\n')) preview)
      isComment l =
        case BS.uncons (BS.dropWhile (\c -> c == fromIntegral (ord ' ')
                                          || c == fromIntegral (ord '\t')) l) of
          Just (c, _) -> c `elem` map (fromIntegral . ord) (['#', '!'] :: String)
          Nothing     -> False
      n = length (filter isComment ls)
  in if n > 0
       then logReport
              (mkWarn "W002"
                 ("先頭付近に "
                    <> T.pack (show n)
                    <> " 件のコメント風行 (# / ! 始まり) を検出。")
                 (Just "--skip N でコメント行数を読み飛ばすか、--comment '#' を指定してください。"))
       else noLog

-- | 原本ヘッダ行 (先頭行) を見て、列数 / 重複 / 空セルが DataFrame と
-- 食い違っていないかをチェックする。Hackage は読込時に重複列を後勝ちで
-- 黙ってマージするため、ここで原本側を走査して気付く必要がある。
detectRawHeaderIssues :: BS.ByteString -> DXD.DataFrame -> LogReport
detectRawHeaderIssues preview df =
  case takeFirstLine preview of
    Nothing -> noLog
    Just hdrLine ->
      let -- まずは comma 区切りで素朴に分割 (TSV/SSV では別 delimiter だが、
          -- W005 で別途検出されるので OK)
          rawCells = T.splitOn "," (decodeAscii hdrLine)
          rawTrim  = map T.strip rawCells
          dups     = findDups rawTrim
          blanks   = filter T.null rawTrim
          dfCols   = DX.columnNames df
          missing  = length rawTrim - length dfCols
      in mconcat
           [ if null dups then noLog
               else logReport
                      (mkWarn "W004"
                         ("原本ヘッダに重複列名: "
                            <> T.intercalate ", " dups
                            <> " — 後勝ちでマージされ、データの一部が消失している恐れがあります。")
                         (Just "重複を解消した CSV を渡すか、コピー前の原本を確認してください。"))
           , if null blanks then noLog
               else logReport
                      (mkWarn "W004"
                         ("原本ヘッダに空セルが "
                            <> T.pack (show (length blanks))
                            <> " 件。匿名列として扱われます。")
                         (Just "ヘッダ行のフォーマットを見直してください。"))
           , if missing > 0 && not (null dfCols)
               then logReport
                      (mkWarn "W004"
                         ("原本ヘッダ列数 (" <> T.pack (show (length rawTrim))
                            <> ") と DataFrame 列数 (" <> T.pack (show (length dfCols))
                            <> ") が不一致 — 列がマージ/欠落している可能性。")
                         (Just "列名のフォーマットを確認してください。"))
               else noLog
           ]

takeFirstLine :: BS.ByteString -> Maybe BS.ByteString
takeFirstLine bs =
  case BS.split (fromIntegral (ord '\n')) bs of
    (l:_) | not (BS.null l) -> Just l
    _                       -> Nothing

decodeAscii :: BS.ByteString -> Text
decodeAscii = T.pack . map (toEnum . fromIntegral) . BS.unpack

findDups :: Ord a => [a] -> [a]
findDups xs =
  let cnt = Map.fromListWith (+) [(x, 1 :: Int) | x <- xs]
  in [ x | (x, k) <- Map.toList cnt, k > 1 ]

-- ---------------------------------------------------------------------------
-- W001 ヘッダ無し疑い
-- ---------------------------------------------------------------------------

-- | 全列名が Double として parse できるなら、先頭行が data 行だった可能性が高い。
detectHeaderless :: DXD.DataFrame -> LogReport
detectHeaderless df =
  let names = DX.columnNames df
      allNumeric = not (null names)
                && all (\n -> case readMaybe (T.unpack n) :: Maybe Double of
                                Just _  -> True
                                Nothing -> False) names
  in if allNumeric
       then logReport
              (mkWarn "W001"
                 ("列名が全て数値です: "
                    <> T.intercalate ", " names
                    <> " — ヘッダ行が無いファイルの可能性。")
                 (Just "ヘッダ無しなら --no-header を指定してください。"))
       else noLog

-- ---------------------------------------------------------------------------
-- W003 ragged (列ごとに非 null セル数が大きく異なる)
-- ---------------------------------------------------------------------------

-- | DataFrame の各列について、null 以外のセル数を求め、最大と最小の差が
-- 全行数の 1/3 を超えていたら警告。Hackage は ragged 行を null bitmap で
-- 補うため、この差で間接的に検出できる。
detectRagged :: DXD.DataFrame -> LogReport
detectRagged df =
  let names    = DX.columnNames df
      (nrows, _) = DX.dimensions df
      -- 列内の null bitmap を直接走査して非 null セル数を求める。
      -- これにより数値 / Text を問わず使える。
      nonNullN n = case DXD.getColumn n df of
        Nothing -> nrows
        Just c  ->
          let len = DXC.columnLength c
          in length [ () | i <- [0 .. len - 1]
                         , not (DXC.columnElemIsNull c i) ]
      counts = [ (n, nonNullN n) | n <- names ]
  in case counts of
       [] -> noLog
       _  ->
         let mx = maximum (map snd counts)
             mn = minimum (map snd counts)
             gap = mx - mn
             worst = [ n | (n, k) <- counts, k == mn ]
         in if nrows >= 6 && gap > 0 && gap * 3 >= nrows
              then logReport
                     (mkWarn "W003"
                        ("列ごとの非 null セル数に乖離: "
                           <> T.pack (show mn) <> "..." <> T.pack (show mx)
                           <> " (差 " <> T.pack (show gap) <> "); "
                           <> "短い列: " <> T.intercalate ", " worst)
                        (Just "ragged な行 (列数が揃っていない) の可能性。CSV を整形してください。"))
              else noLog

-- ---------------------------------------------------------------------------
-- W004 重複 / 空 / 前後空白の列名
-- ---------------------------------------------------------------------------

detectDuplicateBlankNames :: DXD.DataFrame -> LogReport
detectDuplicateBlankNames df =
  let names = DX.columnNames df
      blanks = [ n | n <- names, T.null (T.strip n) ]
      trimmedDiffer = [ n | n <- names, n /= T.strip n ]
      grouped = Map.fromListWith (+) [(n, 1 :: Int) | n <- names]
      dups = [ n | (n, k) <- Map.toList grouped, k > 1 ]
      mk code msg hint = logReport (mkWarn code msg hint)
  in mconcat
       [ if null blanks then noLog
           else mk "W004"
                  ("空または空白のみの列名が "
                     <> T.pack (show (length blanks))
                     <> " 件あります。")
                  (Just "ヘッダ行に空セルがある可能性。--skip N で読み飛ばすか、--no-header をお試しください。")
       , if null trimmedDiffer then noLog
           else mk "W004"
                  ("前後に空白を持つ列名: "
                     <> T.intercalate ", " (map (T.pack . show) trimmedDiffer))
                  (Just "Hanalyze.DataIO.Preprocess.renameColumn でリネームできます。")
       , if null dups then noLog
           else mk "W004"
                  ("重複した列名: "
                     <> T.intercalate ", " dups
                     <> " — 後勝ちで一方が消失している恐れがあります。")
                  (Just "事前に列名を変更するか、CSV を見直してください。")
       ]

-- ---------------------------------------------------------------------------
-- W006 NA 文字列の多型混在
-- ---------------------------------------------------------------------------

-- | NA とみなしうる広めの文字列セット。'isNAString' (defaultNAStrings) に
-- 加えて単独の @-@ / @--@ / @.@ も対象にする (検出限定の判定であり、
-- 既存の補完 API の挙動は変えない)。
isNALike :: Text -> Bool
isNALike t =
  isNAString t
  || (let s = T.strip t in s `elem` ["-", "--", ".", "—"])

-- | 1 列の中に異なる NA 表現が 2 種以上混じっていたら警告。
-- DataFrame の null bitmap (= 既に欠損として処理されたセル) と、文字列上に
-- 残っている NA-like トークンを別カウントとして扱う。
detectMixedNAStrings :: DXD.DataFrame -> LogReport
detectMixedNAStrings df = mconcat
  [ checkColumn n
  | n <- DX.columnNames df
  ]
  where
    checkColumn n = case getMaybeTextVec n df of
      Nothing -> noLog
      Just v  ->
        let cells = V.toList v
            -- "<null>" を 1 つの形として扱う
            tokens = [ case mx of
                         Nothing -> "<null>"
                         Just x  -> T.toLower (T.strip x)
                     | mx <- cells
                     , case mx of
                         Nothing -> True
                         Just x  -> isNALike x
                     ]
            naSet = Map.fromListWith (+) [ (k, 1 :: Int) | k <- tokens ]
        in if Map.size naSet >= 2
             then logReport
                    (mkWarn "W006"
                       ("列 " <> T.pack (show n)
                          <> " に NA 表現が複数種類混在: "
                          <> T.intercalate ", "
                              [ k <> "(" <> T.pack (show v') <> ")"
                              | (k, v') <- Map.toList naSet ])
                       (Just "Hanalyze.DataIO.Preprocess.imputeMean / dropMissingRows で正規化できます。"))
             else noLog

-- ---------------------------------------------------------------------------
-- W007 単位混入
-- ---------------------------------------------------------------------------

-- | text 列で「数字 + 英字サフィックス」のセルが過半なら、単位付きの数値とみなす。
detectUnitSuffix :: DXD.DataFrame -> LogReport
detectUnitSuffix df = mconcat
  [ checkColumn n | n <- DX.columnNames df ]
  where
    checkColumn n = case getMaybeTextVec n df of
      Nothing -> noLog
      Just v  ->
        let xs = [ x | Just x <- V.toList v, not (isNAString x) ]
            n0 = length xs
            hits = length (filter looksLikeUnitNumber xs)
        in if n0 >= 2 && hits * 2 >= n0
             then logReport
                    (mkWarn "W007"
                       ("列 " <> T.pack (show n)
                          <> " は単位付きの数値が混入している可能性 ("
                          <> T.pack (show hits) <> "/"
                          <> T.pack (show n0) <> " セル)。")
                       (Just "Phase C で stripUnits を実装予定。当面は手動で数値化してください。"))
             else noLog

-- | "12.3kg" / "11cm" 等のパターン判定。
looksLikeUnitNumber :: Text -> Bool
looksLikeUnitNumber t =
  let s = T.strip t
      (digits, rest) = T.span (\c -> isDigit c || c == '.' || c == '-') s
      suffix = T.takeWhile isAlpha rest
  in not (T.null digits)
     && not (T.null suffix)
     && T.length suffix <= 4
     && case readMaybe (T.unpack digits) :: Maybe Double of
          Just _  -> True
          Nothing -> False

-- ---------------------------------------------------------------------------
-- W008 通貨 / 桁区切り
-- ---------------------------------------------------------------------------

-- | "$1,234.56" / "1,234" / "¥10,000" 等のパターンを検出。
detectThousandsCurrency :: DXD.DataFrame -> LogReport
detectThousandsCurrency df = mconcat
  [ checkColumn n | n <- DX.columnNames df ]
  where
    checkColumn n = case getMaybeTextVec n df of
      Nothing -> noLog
      Just v  ->
        let xs = [ x | Just x <- V.toList v, not (isNAString x) ]
            n0 = length xs
            hits = length (filter looksLikeThousands xs)
        in if n0 >= 2 && hits * 2 >= n0
             then logReport
                    (mkWarn "W008"
                       ("列 " <> T.pack (show n)
                          <> " に通貨記号 / 桁区切りつき数値の可能性 ("
                          <> T.pack (show hits) <> "/"
                          <> T.pack (show n0) <> " セル)。")
                       (Just "Phase C で parseCurrency を実装予定。"))
             else noLog

looksLikeThousands :: Text -> Bool
looksLikeThousands t0 =
  let t1 = T.strip t0
      t2 = T.dropWhile (`elem` ("$¥€£" :: String)) t1
      hasComma = T.any (== ',') t2
      onlyMoney = T.all (\c -> isDigit c || c == ',' || c == '.' || c == '-') t2
  in hasComma && onlyMoney

-- ---------------------------------------------------------------------------
-- W005 delimiter ミスマッチ
-- ---------------------------------------------------------------------------

-- | DataFrame が 1 列だけで、その値に @;@ / @\t@ / @|@ が頻出するなら delimiter
-- 判定がずれた可能性が高い。preview として渡された生バイト列も確認材料にする。
detectDelimiterMismatch :: BS.ByteString -> DXD.DataFrame -> LogReport
detectDelimiterMismatch preview df =
  let nCols = length (DX.columnNames df)
      candidates = [(';', "セミコロン"), ('\t', "タブ"), ('|', "縦棒")]
      counts =
        [ (c, n, ja)
        | (c, ja) <- candidates
        , let n = BS.count (fromIntegral (ord c)) preview
        , n > 0
        ]
      heavy = [ (c, n, ja) | (c, n, ja) <- counts, n >= 2 ]
  in if nCols == 1 && not (null heavy)
       then logReport
              (mkWarn "W005"
                 ("DataFrame が 1 列のみで、生データに "
                    <> T.intercalate "/" [ ja <> "(" <> T.pack (show n) <> ")"
                                         | (_,n,ja) <- heavy ]
                    <> " が含まれます。delimiter が違う可能性。")
                 (Just "--delim ';'/'\\t'/'|' を試してください。"))
       else noLog

-- 未使用ワーニングを抑える (将来 LogEntry を直接構築する箇所で使う)
_unused :: LogEntry
_unused = mkWarn "" "" Nothing
