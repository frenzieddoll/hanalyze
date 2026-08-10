{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Hanalyze.Data.Strings
-- Description : stringr 流の Text 純粋操作 (str_* 相当・行/列展開含む)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: stringr 流の文字列操作 (Ch14 "Strings")。
--
-- R4DS Ch14 で扱う `str_*` 関数を __純粋な `Text` 操作__として公開する
-- ('Data.Transform' と同列の `Data/` 純粋抽象)。 DataFrame 行/列展開を伴う
-- `separate_*` は別途 (本モジュール下部・要 DataFrame)。
--
-- === recycling / NA
-- @str_c@ 相当は tidyverse の __recycling 規則__ (長さ 1 か n) に従う。 NA 伝播は
-- 'Maybe' 版 ('strCMaybe') で表す (R の @NA@ は `Nothing`)。
--
-- === locale
-- 'strToUpper' / 'strSort' は __既定 locale__ (Unicode コードポイント順・en 相当)。
-- R4DS §14.6.3 の locale 依存 (Czech の "ch"・Turkish の dotless i 等) は ICU が要るため
-- 本モジュールでは扱わず、 tutorial 側で「概念のみ」 honest に注記する。
--
-- [English]: stringr-style string operations (Ch14 "Strings").
--
-- Exposes the `str_*` functions from R4DS Ch14 as __pure `Text` operations__
-- (a pure `Data/` abstraction alongside 'Data.Transform'). The `separate_*`
-- functions, which expand DataFrame rows/columns, live separately (further
-- down in this module; they require a DataFrame).
--
-- === Recycling \/ NA
-- @str_c@ and friends follow tidyverse's __recycling rule__ (length 1 or n).
-- NA propagation is expressed via the 'Maybe' variant ('strCMaybe') (R's @NA@
-- corresponds to `Nothing`).
--
-- === Locale
-- 'strToUpper' \/ 'strSort' use the __default locale__ (Unicode code point
-- order, roughly equivalent to en). The locale-dependent behavior from R4DS
-- §14.6.3 (e.g. Czech's "ch", Turkish's dotless i) would require ICU, so this
-- module does not handle it; the tutorial honestly notes it as "concept
-- only".
module Hanalyze.Data.Strings
  ( -- * 長さ / 部分取り出し (str_length / str_sub)
    strLength
  , strSub
    -- * 連結 (str_c / str_flatten / str_glue)
  , strC
  , strCMaybe
  , strFlatten
  , strGlue
    -- * 大文字化 / ソート (str_to_upper / str_sort)
  , strToUpper
  , strSort
    -- * 文字比較 / encoding (str_equal / charToRaw・§14.6)
  , strEqual
  , charToRaw
    -- * 行展開 (separate_longer・DataFrame)
  , separateLongerDelim
  , separateLongerPosition
    -- * 列分割 (separate_wider・DataFrame)
  , TooFew (..)
  , TooMany (..)
  , separateWiderDelim
  , separateWiderDelimWith
  , separateWiderPosition
  , separateWiderPositionWith
    -- * 正規表現 (§15 Regular expressions・regex-tdfa)
  , strDetect
  , strDetectWith
  , strCount
  , strSubset
  , strWhich
  , strExtract
  , strExtractAll
  , strMatch
  , strReplace
  , strReplaceAll
  , strRemove
  , strRemoveAll
  , strSplit
  , strLocate
  , strEscape
  , separateWiderRegex
  ) where

import           Data.Array  (elems)
import           Data.List   (sort, transpose)
import           Data.Maybe  (isJust)
import           Data.Word   (Word8)
import qualified Data.ByteString as BS
import           Data.Text   (Text)
import qualified Data.Text   as T
import qualified Data.Text.Encoding as TE
import           Data.Text.Normalize (NormalizationMode (NFC), normalize)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified DataFrame.Internal.Column    as DF
import qualified DataFrame.Internal.DataFrame  as DF
import qualified DataFrame.Operations.Subset   as DF
import qualified DataFrame.Internal.Column  as DFC
import qualified DataFrame.Operations.Subset as DFS (rowsAtIndices)
import           Text.Regex.TDFA            (Regex, CompOption (..), ExecOption (..))
import qualified Text.Regex.TDFA            as RE

import           Hanalyze.DataIO.Convert (getMaybeTextVec)

-- ===========================================================================
-- 長さ / 部分取り出し
-- ===========================================================================

-- | [日本語]: 文字数 (= stringr @str_length@・@T.length@)。 コードポイント単位。
--   [English]: Character count (= stringr @str_length@ \/ @T.length@); counted
--   in code points.
strLength :: Text -> Int
strLength = T.length

-- | [日本語]: 部分文字列 (= @str_sub(string, start, end)@)。 __1 始まり・両端含む__。
--   負の index は末尾から (@-1@ = 最終文字)。 範囲外は内側にクリップ。
--   例: @strSub 1 3 "Apple" == "App"@・@strSub (-3) (-1) "Apple" == "ple"@。
--   [English]: A substring (= @str_sub(string, start, end)@),
--   __1-indexed and inclusive of both ends__. Negative indices count from the end (@-1@ = the
--   last character). Out-of-range indices are clipped inward.
--   Example: @strSub 1 3 "Apple" == "App"@ \/ @strSub (-3) (-1) "Apple" == "ple"@.
strSub :: Int -> Int -> Text -> Text
strSub start end t =
  let n = T.length t
      norm i | i < 0     = n + i + 1   -- -1 → n (1 始まり)
             | otherwise = i
      s = max 1 (norm start)
      e = min n (norm end)
  in if s > e then "" else T.take (e - s + 1) (T.drop (s - 1) t)

-- ===========================================================================
-- 連結
-- ===========================================================================

-- | [日本語]: ベクトル連結 (= @str_c(...)@)。 各列を __recycling 規則__ (長さ 1 か n) で
--   揃え、 行ごとに連結する。 リテラルは長さ 1 の列 (@["x"]@) として渡す。
--   例: @strC [["Hello "], names, ["!"]]@。 NA 伝播版は 'strCMaybe'。
--   [English]: Vectorized concatenation (= @str_c(...)@). Aligns each column
--   using the __recycling rule__ (length 1 or n), then concatenates row by
--   row. Pass a literal as a length-1 column (@["x"]@).
--   Example: @strC [["Hello "], names, ["!"]]@. See 'strCMaybe' for the
--   NA-propagating variant.
strC :: [[Text]] -> [Text]
strC [] = []
strC cols = map T.concat (recycleCols cols)

-- | [日本語]: 'strC' の NA 伝播版 (= @str_c@ の R 既定)。 行内に 'Nothing' があれば結果も
--   'Nothing' (R の @NA@ 伝播)。 リテラルは @[Just "x"]@。
--   [English]: The NA-propagating variant of 'strC' (= R's default @str_c@
--   behavior). If any element of a row is 'Nothing', the result for that row
--   is also 'Nothing' (mirroring R's @NA@ propagation). Pass a literal as
--   @[Just "x"]@.
strCMaybe :: [[Maybe Text]] -> [Maybe Text]
strCMaybe [] = []
strCMaybe cols =
  [ if any (== Nothing) row then Nothing else Just (T.concat [x | Just x <- row])
  | row <- recycleCols cols ]

-- | [日本語]: 文字ベクトル→単一文字列 (= @str_flatten(x, collapse)@)。 @T.intercalate@。
--   例: @strFlatten ", " ["a","b","c"] == "a, b, c"@。
--   [English]: Collapses a character vector into a single string
--   (= @str_flatten(x, collapse)@); implemented via @T.intercalate@.
--   Example: @strFlatten ", " ["a","b","c"] == "a, b, c"@.
strFlatten :: Text -> [Text] -> Text
strFlatten = T.intercalate

-- | [日本語]: テンプレート補間 (= @str_glue@)。 @"{key}"@ を @env@ の列で置換 (recycling)。
--   例: @strGlue "Hello {name}!" [("name", names)]@。 未知 key は error。
--   [English]: Template interpolation (= @str_glue@). Substitutes @"{key}"@
--   with the corresponding column from @env@ (with recycling).
--   Example: @strGlue "Hello {name}!" [("name", names)]@. An unknown key
--   raises an error.
strGlue :: Text -> [(Text, [Text])] -> [Text]
strGlue tmpl env =
  let n    = maximum (1 : map (length . snd) env)
      segs = parseGlue tmpl
      val key i = case lookup key env of
        Just col
          | length col == 1 -> head col
          | i < length col  -> col !! i
          | otherwise       -> error "strGlue: 列長が不揃い (recycling 不能)"
        Nothing -> error ("strGlue: 未知の {" ++ T.unpack key ++ "}")
  in [ T.concat [ either id (\k -> val k i) s | s <- segs ] | i <- [0 .. n - 1] ]

-- ===========================================================================
-- 大文字化 / ソート
-- ===========================================================================

-- | [日本語]: 大文字化 (= @str_to_upper@・既定 locale)。 @T.toUpper@。
--   [English]: Converts to upper case (= @str_to_upper@, default locale);
--   implemented via @T.toUpper@.
strToUpper :: Text -> Text
strToUpper = T.toUpper

-- | [日本語]: 昇順ソート (= @str_sort@・既定 locale = Unicode コードポイント順)。
--   [English]: Sorts in ascending order (= @str_sort@, default locale =
--   Unicode code point order).
strSort :: [Text] -> [Text]
strSort = sort

-- ===========================================================================
-- 文字比較 / encoding (§14.6 Non-English Text)
-- ===========================================================================

-- | [日本語]: 見た目が同じ文字の等価判定 (= @str_equal@・§14.6.2)。 アクセント付き文字は
--   合成済 (@"\xfc"@ = ü) と 基底+結合 (@"u\x308"@) で__符号列が違っても見た目は同じ__。
--   両者を __NFC 正規化__してから比較するため等価になる
--   (例: @strEqual "\xfc" "u\x308" == True@・素の @==@ では False)。
--   [English]: Equality of visually-identical characters (= @str_equal@,
--   §14.6.2). An accented character can be represented either precomposed
--   (@"\xfc"@ = ü) or as base + combining mark (@"u\x308"@) —
--   __the code sequences differ but they look the same__. Both are compared after
--   __NFC normalization__, so they compare equal
--   (e.g. @strEqual "\xfc" "u\x308" == True@, whereas plain @==@ gives False).
strEqual :: Text -> Text -> Bool
strEqual a b = normalize NFC a == normalize NFC b

-- | [日本語]: 文字列の __UTF-8 バイト列__ (= R @charToRaw@・§14.6.1)。 各バイトを 'Word8' で
--   返す (R は 16 進表示)。 例: @charToRaw "Hadley" == [0x48,0x61,0x64,0x6c,0x65,0x79]@。
--   [English]: The string's __UTF-8 byte sequence__ (= R's @charToRaw@,
--   §14.6.1). Returns each byte as a 'Word8' (R displays them in hex).
--   Example: @charToRaw "Hadley" == [0x48,0x61,0x64,0x6c,0x65,0x79]@.
charToRaw :: Text -> [Word8]
charToRaw = BS.unpack . TE.encodeUtf8

-- ===========================================================================
-- 行展開 (separate_longer・DataFrame)
-- ===========================================================================
--
-- @separate_longer_*@ は 1 行を **複数行**に展開する (tidyr §14.4.1)。 対象列の
-- 各セルを分割し、 piece 数だけその行を複製 (他列はそのまま複製) して、 対象列を
-- flatten した piece で差し替える。 NA (Nothing) は分割せず 1 行のまま保持。

-- | [日本語]: 区切り文字で行展開 (= @separate_longer_delim(df, col, delim)@)。
--   例: 列 @x@ の @"a,b,c"@ → 3 行 (@"a"@/@"b"@/@"c"@)、 他列は複製。
--   [English]: Expands rows by a delimiter (= @separate_longer_delim(df, col,
--   delim)@). Example: column @x@'s @"a,b,c"@ becomes 3 rows
--   (@"a"@\/@"b"@\/@"c"@), with the other columns duplicated.
separateLongerDelim :: Text -> Text -> DF.DataFrame -> DF.DataFrame
separateLongerDelim col delim =
  separateLongerWith col $ \mv -> case mv of
    Nothing -> [Nothing]
    Just t  -> map Just (T.splitOn delim t)

-- | [日本語]: 固定幅で行展開 (= @separate_longer_position(df, col, width)@)。
--   各セルを先頭から @width@ 文字ずつの塊に分割して行展開する。
--   例: @width=1@ で @"131"@ → 3 行 (@"1"@/@"3"@/@"1"@)。
--   [English]: Expands rows by a fixed width (= @separate_longer_position(df,
--   col, width)@). Splits each cell into chunks of @width@ characters from
--   the start. Example: with @width=1@, @"131"@ becomes 3 rows
--   (@"1"@\/@"3"@\/@"1"@).
separateLongerPosition :: Text -> Int -> DF.DataFrame -> DF.DataFrame
separateLongerPosition col width
  | width <= 0 = error "separateLongerPosition: width は正でなければならない"
  | otherwise  =
      separateLongerWith col $ \mv -> case mv of
        Nothing -> [Nothing]
        Just t  | T.null t  -> [Just ""]            -- 空セルは 1 空行を保持
                | otherwise -> map Just (T.chunksOf width t)

-- | [日本語]: 行展開の核: 対象列を Text 読みし、 各行を @split@ で pieces 化。 piece 数で
--   全列を @rowsAtIndices@ 複製し、 対象列を flatten した piece に差し替える。
--   [English]: The core of row expansion: reads the target column as Text
--   and splits each row into pieces via @split@. Duplicates all columns via
--   @rowsAtIndices@ according to the piece count, then replaces the target
--   column with the flattened pieces.
separateLongerWith
  :: Text -> (Maybe Text -> [Maybe Text]) -> DF.DataFrame -> DF.DataFrame
separateLongerWith col split df =
  case getMaybeTextVec col df of
    Nothing  -> error ("separateLonger: 列 " ++ T.unpack col
                        ++ " が見つからない、 または Text 列でない")
    Just vec ->
      let piecesPerRow = map split (V.toList vec)                 -- [[Maybe Text]]
          idxs = concat [ replicate (length ps) i
                        | (i, ps) <- zip [0 ..] piecesPerRow ]
          flat = concat piecesPerRow
          expanded = DFS.rowsAtIndices (VU.fromList idxs) df
      in DF.insertColumn col (buildTextCol flat) expanded

-- | [日本語]: @[Maybe Text]@ → Column。 全要素 Just なら素の Text 列、 NA 混在なら Maybe 列。
--   [English]: @[Maybe Text]@ → Column. A plain Text column if all elements
--   are Just; a Maybe column if any NA is present.
buildTextCol :: [Maybe Text] -> DFC.Column
buildTextCol xs
  | all isJust xs = DF.fromList [ t | Just t <- xs ]
  | otherwise     = DF.fromList xs

-- ===========================================================================
-- 列分割 (separate_wider・DataFrame)
-- ===========================================================================
--
-- @separate_wider_*@ は 1 セルを **複数列**に分割する (tidyr §14.4.2・行数不変)。
-- piece 数と新列名の数が合わないときの方針を 'TooFew' / 'TooMany' で指定する
-- (§14.4.3 の @too_few@ / @too_many@)。

-- | [日本語]: piece が __足りない__ときの方針 (= @too_few@)。
--   [English]: Policy for when there are __too few__ pieces (= @too_few@).
data TooFew
  = AlignStart    -- ^ [日本語]: 不足分を右側に NA で埋める (= @"align_start"@)。 [English]: Pads the shortfall with NA on the right (= @"align_start"@).
  | AlignEnd      -- ^ [日本語]: 不足分を左側に NA で埋める (= @"align_end"@)。 [English]: Pads the shortfall with NA on the left (= @"align_end"@).
  | TooFewError   -- ^ [日本語]: 不足があれば error (= @"error"@・既定)。 [English]: Errors if there is a shortfall (= @"error"@, the default).
  | TooFewDebug   -- ^ [日本語]: align_start で埋めつつ診断列を付与 (= @"debug"@)。 [English]: Pads like align_start while adding diagnostic columns (= @"debug"@).
  deriving (Eq, Show)

-- | [日本語]: piece が __多すぎる__ときの方針 (= @too_many@)。
--   [English]: Policy for when there are __too many__ pieces (= @too_many@).
data TooMany
  = DropExtra     -- ^ [日本語]: 余剰 piece を捨てる (= @"drop"@)。 [English]: Drops the extra pieces (= @"drop"@).
  | MergeExtra    -- ^ [日本語]: 余剰を最終列に区切り文字で再結合 (= @"merge"@)。 [English]: Rejoins the extras into the final column using the delimiter (= @"merge"@).
  | TooManyError  -- ^ [日本語]: 余剰があれば error (= @"error"@・既定)。 [English]: Errors if there is a surplus (= @"error"@, the default).
  | TooManyDebug  -- ^ [日本語]: drop で埋めつつ診断列 (余剰を remainder) を付与 (= @"debug"@)。 [English]: Behaves like drop while adding a diagnostic column holding the surplus as a remainder (= @"debug"@).
  deriving (Eq, Show)

-- | [日本語]: 区切り文字で列分割 (= @separate_wider_delim(df, col, delim, names)@・厳密)。
--   piece 数と @names@ 数が不一致なら error。 @names@ の 'Nothing' はその piece を
--   捨てる (= R の @NA@・§14.4.2)。
--   [English]: Splits a column by a delimiter (= @separate_wider_delim(df,
--   col, delim, names)@; strict). Errors if the piece count doesn't match
--   the number of @names@. A 'Nothing' in @names@ drops that piece
--   (= R's @NA@, §14.4.2).
separateWiderDelim
  :: Text -> Text -> [Maybe Text] -> DF.DataFrame -> DF.DataFrame
separateWiderDelim col delim names =
  separateWiderDelimWith col delim names TooFewError TooManyError

-- | [日本語]: 'separateWiderDelim' の方針指定版 (§14.4.3 の @too_few@ / @too_many@)。
--   [English]: The policy-configurable variant of 'separateWiderDelim'
--   (the @too_few@ \/ @too_many@ of §14.4.3).
separateWiderDelimWith
  :: Text -> Text -> [Maybe Text] -> TooFew -> TooMany -> DF.DataFrame -> DF.DataFrame
separateWiderDelimWith col delim names tf tm =
  separateWiderImpl col names tf tm (T.splitOn delim) (T.intercalate delim)

-- | [日本語]: 固定幅で列分割 (= @separate_wider_position(df, col, widths)@・厳密)。
--   @widths@ = @[(列名, 文字数)]@。 文字列長が総幅と一致しなければ error。
--   [English]: Splits a column by fixed widths (= @separate_wider_position(df,
--   col, widths)@; strict). @widths@ is @[(column name, character count)]@.
--   Errors if the string length doesn't match the total width.
separateWiderPosition
  :: Text -> [(Text, Int)] -> DF.DataFrame -> DF.DataFrame
separateWiderPosition col widths =
  separateWiderPositionWith col widths TooFewError TooManyError

-- | [日本語]: 'separateWiderPosition' の方針指定版。 文字列が総幅より短ければ @too_few@、
--   長ければ余り (remainder) を @too_many@ で処理する。
--   [English]: The policy-configurable variant of 'separateWiderPosition'.
--   If the string is shorter than the total width, @too_few@ applies; if
--   longer, the remainder is handled via @too_many@.
separateWiderPositionWith
  :: Text -> [(Text, Int)] -> TooFew -> TooMany -> DF.DataFrame -> DF.DataFrame
separateWiderPositionWith col widths tf tm df =
  let names = map (Just . fst) widths
      -- 幅順に切り出す。 文字列が尽きたら打ち切り (→ piece 不足 = too_few)。
      -- 総幅を超える余りは最後に「余剰 piece」として 1 個付ける (→ too_many)。
      chop t =
        let go [] rest = ([], rest)
            go (w : ws) rest
              | T.null rest = ([], rest)
              | otherwise   = let (h, r)   = T.splitAt w rest
                                  (hs, r') = go ws r
                              in (h : hs, r')
            (pieces, remainder) = go (map snd widths) t
        in if T.null remainder then pieces else pieces ++ [remainder]
  in separateWiderImpl col names tf tm chop T.concat df

-- | [日本語]: 列分割の核実装。 @split@ で各セルを piece 化し、 'TooFew' / 'TooMany' で
--   ちょうど @length names@ スロットに整える。 'TooFewDebug' / 'TooManyDebug' の
--   とき診断列 @{col}_ok@ / @{col}_pieces@ / @{col}_remainder@ を付ける。
--   [English]: The core implementation of column splitting. Splits each cell
--   into pieces via @split@, then reconciles them to exactly @length names@
--   slots using 'TooFew' \/ 'TooMany'. When 'TooFewDebug' \/ 'TooManyDebug'
--   is set, adds diagnostic columns @{col}_ok@ \/ @{col}_pieces@ \/
--   @{col}_remainder@.
separateWiderImpl
  :: Text -> [Maybe Text] -> TooFew -> TooMany
  -> (Text -> [Text]) -> ([Text] -> Text)
  -> DF.DataFrame -> DF.DataFrame
separateWiderImpl col names tf tm split rejoin df =
  case getMaybeTextVec col df of
    Nothing  -> error ("separateWider: 列 " ++ T.unpack col
                        ++ " が見つからない、 または Text 列でない")
    Just vec ->
      let cells    = V.toList vec
          rows     = map (resolveRow . fmap split) cells
          -- スロット行列を列向きに転置 (各新列 = 全行のその位置)。
          slotCols = transpose [ s | (s, _, _) <- rows ]            -- [[Maybe Text]] (列×行)
          oks      = [ ok | (_, ok, _) <- rows ]
          rems     = [ r  | (_, _,  r) <- rows ]
          pieceCnt = map (maybe 0 (length . split)) cells
          debug    = tf == TooFewDebug || tm == TooManyDebug
          -- names と slotCols を突き合わせ、 Just name の列だけ採用。
          namedCols = [ (nm, buildTextCol scol)
                      | (Just nm, scol) <- zip names slotCols ]
          diagCols  = [ (col <> "_ok",        DF.fromList oks)
                      , (col <> "_pieces",    DF.fromList pieceCnt)
                      , (col <> "_remainder", DF.fromList rems) ]
          base      = DF.exclude [col] df
          insertAll = foldl (\d (nm, c) -> DF.insertColumn nm c d)
          withNamed = insertAll base namedCols
      in if debug then insertAll withNamed diagCols else withNamed
  where
    n0 = length names
    -- 1 行を (スロット [Maybe Text]・ok・remainder) に解決。
    -- remainder は診断列用で、 余剰なし行は "" (R の @{col}_remainder@ 準拠)。
    resolveRow :: Maybe [Text] -> ([Maybe Text], Bool, Text)
    resolveRow Nothing       = (replicate n0 Nothing, True, "")  -- NA 入力は全 NA
    resolveRow (Just pieces) =
      let k = length pieces
      in if k == n0
           then (map Just pieces, True, "")
           else if k < n0
             then case tf of
               TooFewError -> error ("separateWider: piece が不足 (" ++ show k
                                      ++ " < " ++ show n0 ++ ") col=" ++ T.unpack col)
               AlignEnd    -> (replicate (n0 - k) Nothing ++ map Just pieces, False, "")
               _           -> (map Just pieces ++ replicate (n0 - k) Nothing, False, "")
                              -- AlignStart / TooFewDebug
             else case tm of   -- k > n0
               TooManyError -> error ("separateWider: piece が過多 (" ++ show k
                                       ++ " > " ++ show n0 ++ ") col=" ++ T.unpack col)
               MergeExtra   ->
                 let (keep, extra) = splitAt (n0 - 1) pieces
                 in (map Just keep ++ [Just (rejoin extra)], False, "")
               _            ->  -- DropExtra / TooManyDebug
                 let (keep, extra) = splitAt n0 pieces
                 in (map Just keep, False, rejoin extra)

-- ===========================================================================
-- 内部
-- ===========================================================================

-- | [日本語]: 列群を recycling 規則 (長さ 1 か n) で n 行に揃え、 行ごとの列リストに転置。
--   [English]: Aligns a set of columns to n rows via the recycling rule
--   (length 1 or n), then transposes into a per-row list of columns.
recycleCols :: [[a]] -> [[a]]
recycleCols cols =
  let n = maximum (map length cols)
      recy c
        | length c == n = c
        | length c == 1 = replicate n (head c)
        | otherwise     = error "str_c/glue: 列長は 1 か n でなければならない (recycling)"
  in if n == 0 then [] else transpose (map recy cols)

-- | [日本語]: @"a {x} b {y}"@ → @[Left "a ", Right "x", Left " b ", Right "y"]@。
--   @{{@ / @}}@ はリテラルの @{@ / @}@ にエスケープ (glue 同様)。
--   [English]: @"a {x} b {y}"@ → @[Left "a ", Right "x", Left " b ", Right "y"]@.
--   @{{@ \/ @}}@ escape to literal @{@ \/ @}@ (same as glue).
parseGlue :: Text -> [Either Text Text]
parseGlue = go
  where
    go t
      | T.null t  = []
      | otherwise =
          let (lit, rest) = T.break (== '{') t
          in case T.uncons rest of
               Nothing -> prependLit lit []
               Just ('{', r1)
                 | Just ('{', r2) <- T.uncons r1 ->   -- "{{" → リテラル '{'
                     prependLit (lit <> "{") (go r2)
                 | otherwise ->
                     let (key, r2) = T.break (== '}') r1
                     in case T.uncons r2 of
                          Just ('}', r3) ->
                            prependLit lit (Right (T.strip key) : go r3)
                          _ -> error "strGlue: 閉じない { がある"
               _ -> [Left lit]

    -- リテラル segment では @}}@ を @}@ に畳む (glue の閉じ波括弧エスケープ。
    -- @{{@ → @{@ は break 側で処理済み)。
    prependLit l0 xs = let l = T.replace "}}" "}" l0 in [Left l | not (T.null l)] ++ xs

-- ===========================================================================
-- 正規表現 (§15 Regular expressions・regex-tdfa POSIX ERE)
-- ===========================================================================
--
-- regex-tdfa は **POSIX ERE** ゆえ PCRE ショートハンド @\\d@ @\\s@ @\\w@ を解さない
-- (実測 2026-06-19)。 本モジュールは R(stringr) 流のパターンをそのまま使えるよう、
-- @\\d \\D \\s \\S \\w \\W@ を 'translateShorthand' で **POSIX クラスに変換**してから
-- tdfa に渡す。 単語境界 @\\b@ は tdfa が直接対応。 ★後方参照 @\\1@ は POSIX に無く
-- **非対応** (tutorial 側で「概念のみ」 honest 注記)。
--
-- 引数順は **pattern 先・string 後** (stringr は string 先だが、 Haskell では
-- @map (strDetect pat) xs@ / @filter (strDetect pat) xs@ と部分適用しやすいため)。

-- | [日本語]: PCRE ショートハンド (@\\d \\D \\s \\S \\w \\W@) を POSIX クラスに変換する。
--   文字クラス @[...]@ の内外で展開形が違う (外: @[[:digit:]]@・内: @[:digit:]@)。
--   @\\\\@ (literal backslash) や他のエスケープ (@\\.@ @\\b@ @\\1@ 等) はそのまま通す。
--   [English]: Translates PCRE shorthands (@\\d \\D \\s \\S \\w \\W@) into
--   POSIX classes. The expansion differs inside vs. outside a character
--   class @[...]@ (outside: @[[:digit:]]@; inside: @[:digit:]@). A literal
--   backslash (@\\\\@) and other escapes (@\\.@ @\\b@ @\\1@ etc.) pass
--   through unchanged.
translateShorthand :: Text -> Text
translateShorthand = T.pack . go False . T.unpack
  where
    go _     []            = []
    go inCls ('\\':c:rest)
      | Just body <- lookup c shorthands = wrap inCls body ++ go inCls rest
      | otherwise                        = '\\' : c : go inCls rest   -- \. \\ \b \1 等はそのまま
    go _     ('[':rest)    = '[' : go True  rest
    go _     (']':rest)    = ']' : go False rest
    go inCls (c:rest)      = c   : go inCls rest

    shorthands =
      [ ('d', "[:digit:]"),  ('D', "^[:digit:]")
      , ('s', "[:space:]"),  ('S', "^[:space:]")
      , ('w', "[:alnum:]_"), ('W', "^[:alnum:]_") ]
    -- クラス外は @[ ... ]@ で囲む (否定 ^... はクラス否定 [^...] に)。 クラス内は
    -- 中身だけ ([:digit:] 等)。 クラス内の否定 (\D 等) は POSIX で表現不能ゆえ近似 (caret 落とし)。
    wrap True  body          = stripCaret body
    wrap False ('^':body)    = "[^" ++ body ++ "]"
    wrap False body          = "[" ++ body ++ "]"
    stripCaret ('^':b) = b
    stripCaret b       = b

-- | [日本語]: パターン (ショートハンド変換済) を tdfa 'Regex' に compile。
--   @ci@ = ignore_case (§15.5 の @regex(ignore_case = TRUE)@)。
--   @^@ @$@ は __文字列全体__の先頭/末尾 (R 既定・single line = multiline False)。
--   [English]: Compiles a pattern (after shorthand translation) into a tdfa
--   'Regex'. @ci@ is ignore_case (§15.5's @regex(ignore_case = TRUE)@).
--   @^@ \/ @$@ anchor to the start\/end of the __whole string__ (R's
--   default, single line = multiline False).
mkRegex :: Bool -> Text -> Regex
mkRegex ci pat =
  RE.makeRegexOpts comp RE.defaultExecOpt (T.unpack (translateShorthand pat))
  where
    comp = RE.defaultCompOpt { caseSensitive = not ci, multiline = False }

-- | [日本語]: マッチ配列 (whole + groups) を @[(text, offset, len)]@ に。 offset<0 = 不参加グループ。
--   [English]: Converts a match array (whole + groups) into
--   @[(text, offset, len)]@. offset < 0 means the group didn't participate.
matchElems :: RE.MatchText String -> [(String, Int, Int)]
matchElems arr = [ (g, o, l) | (g, (o, l)) <- elems arr ]

-- | [日本語]: パターンにマッチするか (= @str_detect(string, pattern)@・§15.3.1)。
--   [English]: Whether the pattern matches (= @str_detect(string, pattern)@,
--   §15.3.1).
strDetect :: Text -> Text -> Bool
strDetect = strDetectWith False

-- | [日本語]: 'strDetect' の ignore_case 指定版 (§15.5)。 @strDetectWith True pat s@ で大小無視。
--   [English]: The ignore_case variant of 'strDetect' (§15.5).
--   @strDetectWith True pat s@ ignores case.
strDetectWith :: Bool -> Text -> Text -> Bool
strDetectWith ci pat s = RE.matchTest (mkRegex ci pat) (T.unpack s)

-- | [日本語]: マッチ回数 (= @str_count(string, pattern)@・§15.3.2)。
--   [English]: The number of matches (= @str_count(string, pattern)@,
--   §15.3.2).
strCount :: Text -> Text -> Int
strCount pat s = RE.matchCount (mkRegex False pat) (T.unpack s)

-- | [日本語]: マッチした要素だけ残す (= @str_subset(x, pattern)@)。
--   [English]: Keeps only the elements that match (= @str_subset(x,
--   pattern)@).
strSubset :: Text -> [Text] -> [Text]
strSubset pat = filter (strDetect pat)

-- | [日本語]: マッチした要素の位置 (= @str_which@・__1 始まり__)。
--   [English]: The positions of matching elements (= @str_which@,
--   __1-indexed__).
strWhich :: Text -> [Text] -> [Int]
strWhich pat xs = [ i | (i, x) <- zip [1 ..] xs, strDetect pat x ]

-- | [日本語]: 最初のマッチを取り出す (= @str_extract(string, pattern)@)。 無マッチは 'Nothing'。
--   [English]: Extracts the first match (= @str_extract(string, pattern)@).
--   Returns 'Nothing' if there is no match.
strExtract :: Text -> Text -> Maybe Text
strExtract pat s =
  case RE.matchOnceText (mkRegex False pat) (T.unpack s) of
    Just (_, arr, _) | ((m, _, _) : _) <- matchElems arr -> Just (T.pack m)
    _                                                    -> Nothing

-- | [日本語]: すべてのマッチを取り出す (= @str_extract_all@)。
--   [English]: Extracts all matches (= @str_extract_all@).
strExtractAll :: Text -> Text -> [Text]
strExtractAll pat s =
  [ T.pack m
  | arr <- RE.matchAllText (mkRegex False pat) (T.unpack s)
  , ((m, _, _) : _) <- [matchElems arr] ]

-- | [日本語]: 最初のマッチの __whole + capture groups__ (= @str_match@)。 不参加グループ = 'Nothing'。
--   先頭が whole match、 以降が @()@ グループ。 無マッチは @[]@。
--   [English]: The __whole match + capture groups__ of the first match
--   (= @str_match@); a non-participating group is 'Nothing'. The first
--   element is the whole match, followed by the @()@ groups. No match
--   yields @[]@.
strMatch :: Text -> Text -> [Maybe Text]
strMatch pat s =
  case RE.matchOnceText (mkRegex False pat) (T.unpack s) of
    Just (_, arr, _) -> [ if o < 0 then Nothing else Just (T.pack g)
                        | (g, o, _) <- matchElems arr ]
    Nothing          -> []

-- | [日本語]: 最初のマッチを置換 (= @str_replace(string, pattern, replacement)@・§15.3.3)。
--   replacement 内の @\\1@..@\\9@ は capture group 参照、 @\\\\@ はリテラル @\\@。
--   [English]: Replaces the first match (= @str_replace(string, pattern,
--   replacement)@, §15.3.3). Within the replacement, @\\1@..@\\9@ refer to
--   capture groups and @\\\\@ is a literal @\\@.
strReplace :: Text -> Text -> Text -> Text
strReplace = replaceImpl False

-- | [日本語]: すべてのマッチを置換 (= @str_replace_all@)。
--   [English]: Replaces all matches (= @str_replace_all@).
strReplaceAll :: Text -> Text -> Text -> Text
strReplaceAll = replaceImpl True

replaceImpl :: Bool -> Text -> Text -> Text -> Text
replaceImpl global pat rep s =
  let rx      = mkRegex False pat
      str     = T.unpack s
      ms      = (if global then id else take 1) (RE.matchAllText rx str)
      repl    = T.unpack rep
      go cur [] = drop cur str
      go cur (arr : rest) =
        case matchElems arr of
          gs@((_, o, l) : _) ->
            take (o - cur) (drop cur str) ++ expandRep repl gs ++ go (o + l) rest
          [] -> go cur rest
  in T.pack (go 0 ms)

-- | [日本語]: replacement の @\\n@ を group n に展開 (@\\0@=whole・@\\\\@=リテラル @\\@)。
--   [English]: Expands @\\n@ in the replacement to group n (@\\0@ = whole
--   match, @\\\\@ = a literal @\\@).
expandRep :: String -> [(String, Int, Int)] -> String
expandRep r groups = ex r
  where
    ex []                = []
    ex ('\\' : d : rest)
      | d >= '0' && d <= '9' = grp (fromEnum d - fromEnum '0') ++ ex rest
      | d == '\\'            = '\\' : ex rest
      | otherwise           = d : ex rest
    ex (c : rest)          = c : ex rest
    grp i = case drop i groups of
              ((g, o, _) : _) | o >= 0 -> g
              _                        -> ""

-- | [日本語]: 最初のマッチを削除 (= @str_remove@ = @str_replace(., pattern, "")@)。
--   [English]: Removes the first match (= @str_remove@ =
--   @str_replace(., pattern, "")@).
strRemove :: Text -> Text -> Text
strRemove pat = strReplace pat ""

-- | [日本語]: すべてのマッチを削除 (= @str_remove_all@)。
--   [English]: Removes all matches (= @str_remove_all@).
strRemoveAll :: Text -> Text -> Text
strRemoveAll pat = strReplaceAll pat ""

-- | [日本語]: パターンで分割 (= @str_split(string, pattern)@)。 マッチ部分を区切りとして除く。
--   [English]: Splits by a pattern (= @str_split(string, pattern)@); the
--   matched portions are removed as delimiters.
strSplit :: Text -> Text -> [Text]
strSplit pat s =
  let rx  = mkRegex False pat
      str = T.unpack s
      ms  = RE.matchAllText rx str
      go cur []          = [drop cur str]
      go cur (arr : rest) =
        case matchElems arr of
          ((_, o, l) : _) -> take (o - cur) (drop cur str) : go (o + l) rest
          []              -> go cur rest
  in map T.pack (go 0 ms)

-- | [日本語]: 最初のマッチの位置 @(start, end)@ (= @str_locate@・__1 始まり・両端含む__)。 無マッチ = 'Nothing'。
--   [English]: The position @(start, end)@ of the first match (=
--   @str_locate@, __1-indexed and inclusive of both ends__). No match
--   yields 'Nothing'.
strLocate :: Text -> Text -> Maybe (Int, Int)
strLocate pat s =
  case RE.matchOnceText (mkRegex False pat) (T.unpack s) of
    Just (_, arr, _) | ((_, o, l) : _) <- matchElems arr, o >= 0 -> Just (o + 1, o + l)
    _                                                            -> Nothing

-- | [日本語]: 正規表現メタ文字をエスケープ (= @str_escape@・§15.6・リテラル文字列からパターンを作る用)。
--   [English]: Escapes regex metacharacters (= @str_escape@, §15.6; for
--   building a pattern from a literal string).
strEscape :: Text -> Text
strEscape = T.concatMap esc
  where
    esc c | c `elem` metas = T.pack ['\\', c]
          | otherwise      = T.singleton c
    metas = ".^$|()[]{}*+?\\" :: String

-- | [日本語]: 名前付きグループで列に分割 (= @separate_wider_regex(df, col, patterns)@・§15.3.4)。
--   @specs@ = @[(Just 列名 | Nothing, 部分パターン)]@。 各部分パターンを順に capture group 化し、
--   セルを文字列全体マッチ (@^...$@) して各 group を対応列へ。 'Nothing' の group は捨てる
--   (= R の名無し)。 各部分パターンは __内部に capturing group を持たない前提__
--   (持つと group index がずれる・R4DS の例は単純パターンのみ)。
--   [English]: Splits into columns using named groups (=
--   @separate_wider_regex(df, col, patterns)@, §15.3.4). @specs@ is
--   @[(Just column name | Nothing, sub-pattern)]@. Each sub-pattern is
--   turned into a capture group in order, the cell is matched against the
--   whole string (@^...$@), and each group is assigned to its column. A
--   'Nothing' group is dropped (= R's unnamed). Each sub-pattern is assumed
--   __not to contain a capturing group of its own__ (doing so would shift
--   the group indices; the R4DS examples use only simple patterns).
separateWiderRegex :: Text -> [(Maybe Text, Text)] -> DF.DataFrame -> DF.DataFrame
separateWiderRegex col specs df =
  case getMaybeTextVec col df of
    Nothing  -> error ("separateWiderRegex: 列 " ++ T.unpack col
                        ++ " が見つからない、 または Text 列でない")
    Just vec ->
      let pat   = "^" <> T.concat [ "(" <> p <> ")" | (_, p) <- specs ] <> "$"
          rx    = mkRegex False pat
          names = map fst specs
          nslot = length specs
          rowGroups mv = case mv of
            Nothing -> replicate nslot Nothing
            Just t  -> case RE.matchOnceText rx (T.unpack t) of
              Just (_, arr, _) ->
                let grps = drop 1 (matchElems arr)   -- whole match を除く
                in take nslot
                     ([ if o < 0 then Nothing else Just (T.pack g) | (g, o, _) <- grps ]
                       ++ repeat Nothing)
              Nothing -> error ("separateWiderRegex: パターン不一致 col=" ++ T.unpack col
                                 ++ " value=" ++ show t)
          rows     = map rowGroups (V.toList vec)
          slotCols = transpose rows
          named    = [ (nm, buildTextCol scol) | (Just nm, scol) <- zip names slotCols ]
          base     = DF.exclude [col] df
      in foldl (\d (nm, c) -> DF.insertColumn nm c d) base named
