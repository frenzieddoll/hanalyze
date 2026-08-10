{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.DataIO.Health
-- Description : 読み込み済み DataFrame の疑わしいパターンを警告コード (W001〜W008) として検出
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: DataFrame の健全性チェック。 読み込みに成功した DataFrame の中に
-- 潜みうる「怪しい」パターンを、警告コードとして洗い出す。
--
-- 検出されるコード:
--
--   * @W001@ — ヘッダが疑わしい (全列名が数値として parse できる)。
--   * @W003@ — ragged: 列ごとの長さが異なる (Hackage 側で通常はパディングされるが、
--     念のため二重チェックする)。
--   * @W004@ — 列名の重複 / 空 / 前後空白。
--   * @W005@ — delimiter ミスマッチ: 1 列だけの DataFrame で、値に別の
--     delimiter 候補が含まれている。
--   * @W006@ — NA 文字列の多型混在。
--   * @W007@ — 単位サフィックスを推測 (Text 列の大半のセルが
--     @^\\d+\\.?\\d*[a-zA-Z]+$@ に一致)。
--   * @W008@ — 通貨記号または桁区切りの疑い。
--
-- 生バイト列のプレビューが必要な補助チェックは 'inspectWithPreview' にある。
-- それ以外は 'inspectDataFrame' で DataFrame だけから判定可能。
--
-- 利用シナリオ:
--
-- @
-- (df, lg0) <- loadAutoSafe path
-- let lg = lg0 <> inspectDataFrame df
-- printLogReport lg
-- @
--
-- [English]: DataFrame health check. Surfaces the "looks suspicious" patterns that
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
-- 'inspectWithPreview'. Everything else can be judged from the
-- DataFrame alone via 'inspectDataFrame'.
--
-- Usage scenario:
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

-- | [日本語]: DataFrame と先頭の生バイト列プレビューの両方を必要とする W コード
--   (例: W005 delimiter ミスマッチ / W004 ヘッダ行レベルの重複) も合わせて返す。
--   [English]: Also returns the W-codes that need both the DataFrame
--   and a leading raw-byte preview (e.g. W005 delimiter mismatch / W004
--   header-row-level duplicates).
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

-- | [日本語]: 原本ヘッダ行 (先頭行) を見て、列数 / 重複 / 空セルが DataFrame と
--   食い違っていないかをチェックする。Hackage は読込時に重複列を後勝ちで
--   黙ってマージするため、ここで原本側を走査して気付く必要がある。
--   [English]: Looks at the original header row (first line) and checks
--   whether the column count / duplicates / blank cells disagree with
--   the DataFrame. Hackage silently merges duplicate columns
--   last-write-wins on load, so we need to scan the original side here
--   to notice it.
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

-- | [日本語]: 全列名が Double として parse できるなら、先頭行が data 行だった可能性が高い。
--   [English]: If every column name can be parsed as 'Double', the
--   first row was likely a data row, not a header.
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

-- | [日本語]: DataFrame の各列について、null 以外のセル数を求め、最大と最小の差が
--   全行数の 1/3 を超えていたら警告。Hackage は ragged 行を null bitmap で
--   補うため、この差で間接的に検出できる。
--   [English]: For each column of the DataFrame, computes the
--   non-null cell count; warns if the gap between the max and min
--   exceeds 1/3 of the total row count. Hackage pads ragged rows via
--   the null bitmap, so this gap lets us detect it indirectly.
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

-- | [日本語]: NA とみなしうる広めの文字列セット。'isNAString' (defaultNAStrings) に
--   加えて単独の @-@ / @--@ / @.@ も対象にする (検出限定の判定であり、
--   既存の補完 API の挙動は変えない)。
--   [English]: A broader set of strings treated as NA-like. In
--   addition to 'isNAString' (defaultNAStrings), also targets lone
--   @-@ / @--@ / @.@ (this is a detection-only judgment; it doesn't
--   change the behavior of the existing imputation API).
isNALike :: Text -> Bool
isNALike t =
  isNAString t
  || (let s = T.strip t in s `elem` ["-", "--", ".", "—"])

-- | [日本語]: 1 列の中に異なる NA 表現が 2 種以上混じっていたら警告。
--   DataFrame の null bitmap (= 既に欠損として処理されたセル) と、文字列上に
--   残っている NA-like トークンを別カウントとして扱う。
--   [English]: Warns if a single column mixes 2 or more distinct NA
--   representations. The DataFrame's null bitmap (= cells already
--   treated as missing) and NA-like tokens still remaining as strings
--   are counted separately.
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

-- | [日本語]: text 列で「数字 + 英字サフィックス」のセルが過半なら、単位付きの数値とみなす。
--   [English]: If more than half the cells in a Text column are
--   "number + alphabetic suffix", treats it as a unit-suffixed number.
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

-- | [日本語]: "12.3kg" / "11cm" 等のパターン判定。
--   [English]: Judges patterns like "12.3kg" / "11cm".
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

-- | [日本語]: "$1,234.56" / "1,234" / "¥10,000" 等のパターンを検出。
--   [English]: Detects patterns like "$1,234.56" / "1,234" / "¥10,000".
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

-- | [日本語]: DataFrame が 1 列だけで、その値に @;@ / @\t@ / @|@ が頻出するなら delimiter
--   判定がずれた可能性が高い。preview として渡された生バイト列も確認材料にする。
--   [English]: If the DataFrame has only a single column and its
--   values frequently contain @;@ / @\t@ / @|@, the delimiter
--   detection was likely wrong. The raw byte string passed as the
--   preview is also used as supporting evidence.
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
