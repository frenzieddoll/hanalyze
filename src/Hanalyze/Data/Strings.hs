{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Hanalyze.Data.Strings
-- Description : stringr 流の Text 純粋操作 (str_* 相当・行/列展開含む)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- stringr 流の文字列操作 (Phase 28 Ch14 "Strings")。
--
-- R4DS Ch14 で扱う `str_*` 関数を **純粋な `Text` 操作**として公開する
-- ('Data.Transform' と同列の `Data/` 純粋抽象)。 DataFrame 行/列展開を伴う
-- `separate_*` は別途 (本モジュール下部・要 DataFrame)。
--
-- === recycling / NA
-- `str_c` 相当は tidyverse の **recycling 規則** (長さ 1 か n) に従う。 NA 伝播は
-- 'Maybe' 版 ('strCMaybe') で表す (R の `NA` は `Nothing`)。
--
-- === locale
-- 'strToUpper' / 'strSort' は **既定 locale** (Unicode コードポイント順・en 相当)。
-- R4DS §14.6.3 の locale 依存 (Czech の "ch"・Turkish の dotless i 等) は ICU が要るため
-- 本モジュールでは扱わず、 tutorial 側で「概念のみ」 honest に注記する。
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

-- | 文字数 (= stringr @str_length@・@T.length@)。 コードポイント単位。
strLength :: Text -> Int
strLength = T.length

-- | 部分文字列 (= @str_sub(string, start, end)@)。 **1 始まり・両端含む**。
--   負の index は末尾から (@-1@ = 最終文字)。 範囲外は内側にクリップ。
--   例: @strSub 1 3 "Apple" == "App"@・@strSub (-3) (-1) "Apple" == "ple"@。
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

-- | ベクトル連結 (= @str_c(...)@)。 各列を **recycling 規則** (長さ 1 か n) で
--   揃え、 行ごとに連結する。 リテラルは長さ 1 の列 (@["x"]@) として渡す。
--   例: @strC [["Hello "], names, ["!"]]@。 NA 伝播版は 'strCMaybe'。
strC :: [[Text]] -> [Text]
strC [] = []
strC cols = map T.concat (recycleCols cols)

-- | 'strC' の NA 伝播版 (= @str_c@ の R 既定)。 行内に 'Nothing' があれば結果も
--   'Nothing' (R の `NA` 伝播)。 リテラルは @[Just "x"]@。
strCMaybe :: [[Maybe Text]] -> [Maybe Text]
strCMaybe [] = []
strCMaybe cols =
  [ if any (== Nothing) row then Nothing else Just (T.concat [x | Just x <- row])
  | row <- recycleCols cols ]

-- | 文字ベクトル→単一文字列 (= @str_flatten(x, collapse)@)。 @T.intercalate@。
--   例: @strFlatten ", " ["a","b","c"] == "a, b, c"@。
strFlatten :: Text -> [Text] -> Text
strFlatten = T.intercalate

-- | テンプレート補間 (= @str_glue@)。 @"{key}"@ を @env@ の列で置換 (recycling)。
--   例: @strGlue "Hello {name}!" [("name", names)]@。 未知 key は error。
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

-- | 大文字化 (= @str_to_upper@・既定 locale)。 @T.toUpper@。
strToUpper :: Text -> Text
strToUpper = T.toUpper

-- | 昇順ソート (= @str_sort@・既定 locale = Unicode コードポイント順)。
strSort :: [Text] -> [Text]
strSort = sort

-- ===========================================================================
-- 文字比較 / encoding (§14.6 Non-English Text)
-- ===========================================================================

-- | 見た目が同じ文字の等価判定 (= @str_equal@・§14.6.2)。 アクセント付き文字は
--   合成済 (@"\xfc"@ = ü) と 基底+結合 (@"u\x308"@) で**符号列が違っても見た目は同じ**。
--   両者を **NFC 正規化**してから比較するため等価になる
--   (例: @strEqual "\xfc" "u\x308" == True@・素の @==@ では False)。
strEqual :: Text -> Text -> Bool
strEqual a b = normalize NFC a == normalize NFC b

-- | 文字列の **UTF-8 バイト列** (= R @charToRaw@・§14.6.1)。 各バイトを 'Word8' で
--   返す (R は 16 進表示)。 例: @charToRaw "Hadley" == [0x48,0x61,0x64,0x6c,0x65,0x79]@。
charToRaw :: Text -> [Word8]
charToRaw = BS.unpack . TE.encodeUtf8

-- ===========================================================================
-- 行展開 (separate_longer・DataFrame)
-- ===========================================================================
--
-- @separate_longer_*@ は 1 行を **複数行**に展開する (tidyr §14.4.1)。 対象列の
-- 各セルを分割し、 piece 数だけその行を複製 (他列はそのまま複製) して、 対象列を
-- flatten した piece で差し替える。 NA (Nothing) は分割せず 1 行のまま保持。

-- | 区切り文字で行展開 (= @separate_longer_delim(df, col, delim)@)。
--   例: 列 @x@ の @"a,b,c"@ → 3 行 (@"a"@/@"b"@/@"c"@)、 他列は複製。
separateLongerDelim :: Text -> Text -> DF.DataFrame -> DF.DataFrame
separateLongerDelim col delim =
  separateLongerWith col $ \mv -> case mv of
    Nothing -> [Nothing]
    Just t  -> map Just (T.splitOn delim t)

-- | 固定幅で行展開 (= @separate_longer_position(df, col, width)@)。
--   各セルを先頭から @width@ 文字ずつの塊に分割して行展開する。
--   例: @width=1@ で @"131"@ → 3 行 (@"1"@/@"3"@/@"1"@)。
separateLongerPosition :: Text -> Int -> DF.DataFrame -> DF.DataFrame
separateLongerPosition col width
  | width <= 0 = error "separateLongerPosition: width は正でなければならない"
  | otherwise  =
      separateLongerWith col $ \mv -> case mv of
        Nothing -> [Nothing]
        Just t  | T.null t  -> [Just ""]            -- 空セルは 1 空行を保持
                | otherwise -> map Just (T.chunksOf width t)

-- | 行展開の核: 対象列を Text 読みし、 各行を @split@ で pieces 化。 piece 数で
--   全列を 'rowsAtIndices' 複製し、 対象列を flatten した piece に差し替える。
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

-- | @[Maybe Text]@ → Column。 全要素 Just なら素の Text 列、 NA 混在なら Maybe 列。
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

-- | piece が **足りない**ときの方針 (= @too_few@)。
data TooFew
  = AlignStart    -- ^ 不足分を右側に NA で埋める (= @"align_start"@)。
  | AlignEnd      -- ^ 不足分を左側に NA で埋める (= @"align_end"@)。
  | TooFewError   -- ^ 不足があれば error (= @"error"@・既定)。
  | TooFewDebug   -- ^ align_start で埋めつつ診断列を付与 (= @"debug"@)。
  deriving (Eq, Show)

-- | piece が **多すぎる**ときの方針 (= @too_many@)。
data TooMany
  = DropExtra     -- ^ 余剰 piece を捨てる (= @"drop"@)。
  | MergeExtra    -- ^ 余剰を最終列に区切り文字で再結合 (= @"merge"@)。
  | TooManyError  -- ^ 余剰があれば error (= @"error"@・既定)。
  | TooManyDebug  -- ^ drop で埋めつつ診断列 (余剰を remainder) を付与 (= @"debug"@)。
  deriving (Eq, Show)

-- | 区切り文字で列分割 (= @separate_wider_delim(df, col, delim, names)@・厳密)。
--   piece 数と @names@ 数が不一致なら error。 @names@ の 'Nothing' はその piece を
--   捨てる (= R の @NA@・§14.4.2)。
separateWiderDelim
  :: Text -> Text -> [Maybe Text] -> DF.DataFrame -> DF.DataFrame
separateWiderDelim col delim names =
  separateWiderDelimWith col delim names TooFewError TooManyError

-- | 'separateWiderDelim' の方針指定版 (§14.4.3 の @too_few@ / @too_many@)。
separateWiderDelimWith
  :: Text -> Text -> [Maybe Text] -> TooFew -> TooMany -> DF.DataFrame -> DF.DataFrame
separateWiderDelimWith col delim names tf tm =
  separateWiderImpl col names tf tm (T.splitOn delim) (T.intercalate delim)

-- | 固定幅で列分割 (= @separate_wider_position(df, col, widths)@・厳密)。
--   @widths@ = @[(列名, 文字数)]@。 文字列長が総幅と一致しなければ error。
separateWiderPosition
  :: Text -> [(Text, Int)] -> DF.DataFrame -> DF.DataFrame
separateWiderPosition col widths =
  separateWiderPositionWith col widths TooFewError TooManyError

-- | 'separateWiderPosition' の方針指定版。 文字列が総幅より短ければ @too_few@、
--   長ければ余り (remainder) を @too_many@ で処理する。
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

-- | 列分割の核実装。 @split@ で各セルを piece 化し、 'TooFew' / 'TooMany' で
--   ちょうど @length names@ スロットに整える。 'TooFewDebug' / 'TooManyDebug' の
--   とき診断列 @{col}_ok@ / @{col}_pieces@ / @{col}_remainder@ を付ける。
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

-- | 列群を recycling 規則 (長さ 1 か n) で n 行に揃え、 行ごとの列リストに転置。
recycleCols :: [[a]] -> [[a]]
recycleCols cols =
  let n = maximum (map length cols)
      recy c
        | length c == n = c
        | length c == 1 = replicate n (head c)
        | otherwise     = error "str_c/glue: 列長は 1 か n でなければならない (recycling)"
  in if n == 0 then [] else transpose (map recy cols)

-- | @"a {x} b {y}"@ → @[Left "a ", Right "x", Left " b ", Right "y"]@。
--   @{{@ / @}}@ はリテラルの @{@ / @}@ にエスケープ (glue 同様)。
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

-- | PCRE ショートハンド (@\\d \\D \\s \\S \\w \\W@) を POSIX クラスに変換する。
--   文字クラス @[...]@ の内外で展開形が違う (外: @[[:digit:]]@・内: @[:digit:]@)。
--   @\\\\@ (literal backslash) や他のエスケープ (@\\.@ @\\b@ @\\1@ 等) はそのまま通す。
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

-- | パターン (ショートハンド変換済) を tdfa 'Regex' に compile。
--   @ci@ = ignore_case (§15.5 の @regex(ignore_case = TRUE)@)。
--   @^@ @$@ は **文字列全体**の先頭/末尾 (R 既定・single line = multiline False)。
mkRegex :: Bool -> Text -> Regex
mkRegex ci pat =
  RE.makeRegexOpts comp RE.defaultExecOpt (T.unpack (translateShorthand pat))
  where
    comp = RE.defaultCompOpt { caseSensitive = not ci, multiline = False }

-- | マッチ配列 (whole + groups) を @[(text, offset, len)]@ に。 offset<0 = 不参加グループ。
matchElems :: RE.MatchText String -> [(String, Int, Int)]
matchElems arr = [ (g, o, l) | (g, (o, l)) <- elems arr ]

-- | パターンにマッチするか (= @str_detect(string, pattern)@・§15.3.1)。
strDetect :: Text -> Text -> Bool
strDetect = strDetectWith False

-- | 'strDetect' の ignore_case 指定版 (§15.5)。 @strDetectWith True pat s@ で大小無視。
strDetectWith :: Bool -> Text -> Text -> Bool
strDetectWith ci pat s = RE.matchTest (mkRegex ci pat) (T.unpack s)

-- | マッチ回数 (= @str_count(string, pattern)@・§15.3.2)。
strCount :: Text -> Text -> Int
strCount pat s = RE.matchCount (mkRegex False pat) (T.unpack s)

-- | マッチした要素だけ残す (= @str_subset(x, pattern)@)。
strSubset :: Text -> [Text] -> [Text]
strSubset pat = filter (strDetect pat)

-- | マッチした要素の位置 (= @str_which@・**1 始まり**)。
strWhich :: Text -> [Text] -> [Int]
strWhich pat xs = [ i | (i, x) <- zip [1 ..] xs, strDetect pat x ]

-- | 最初のマッチを取り出す (= @str_extract(string, pattern)@)。 無マッチは 'Nothing'。
strExtract :: Text -> Text -> Maybe Text
strExtract pat s =
  case RE.matchOnceText (mkRegex False pat) (T.unpack s) of
    Just (_, arr, _) | ((m, _, _) : _) <- matchElems arr -> Just (T.pack m)
    _                                                    -> Nothing

-- | すべてのマッチを取り出す (= @str_extract_all@)。
strExtractAll :: Text -> Text -> [Text]
strExtractAll pat s =
  [ T.pack m
  | arr <- RE.matchAllText (mkRegex False pat) (T.unpack s)
  , ((m, _, _) : _) <- [matchElems arr] ]

-- | 最初のマッチの **whole + capture groups** (= @str_match@)。 不参加グループ = 'Nothing'。
--   先頭が whole match、 以降が @()@ グループ。 無マッチは @[]@。
strMatch :: Text -> Text -> [Maybe Text]
strMatch pat s =
  case RE.matchOnceText (mkRegex False pat) (T.unpack s) of
    Just (_, arr, _) -> [ if o < 0 then Nothing else Just (T.pack g)
                        | (g, o, _) <- matchElems arr ]
    Nothing          -> []

-- | 最初のマッチを置換 (= @str_replace(string, pattern, replacement)@・§15.3.3)。
--   replacement 内の @\\1@..@\\9@ は capture group 参照、 @\\\\@ はリテラル @\\@。
strReplace :: Text -> Text -> Text -> Text
strReplace = replaceImpl False

-- | すべてのマッチを置換 (= @str_replace_all@)。
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

-- | replacement の @\\n@ を group n に展開 (@\\0@=whole・@\\\\@=リテラル @\\@)。
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

-- | 最初のマッチを削除 (= @str_remove@ = @str_replace(., pattern, "")@)。
strRemove :: Text -> Text -> Text
strRemove pat = strReplace pat ""

-- | すべてのマッチを削除 (= @str_remove_all@)。
strRemoveAll :: Text -> Text -> Text
strRemoveAll pat = strReplaceAll pat ""

-- | パターンで分割 (= @str_split(string, pattern)@)。 マッチ部分を区切りとして除く。
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

-- | 最初のマッチの位置 @(start, end)@ (= @str_locate@・**1 始まり・両端含む**)。 無マッチ = 'Nothing'。
strLocate :: Text -> Text -> Maybe (Int, Int)
strLocate pat s =
  case RE.matchOnceText (mkRegex False pat) (T.unpack s) of
    Just (_, arr, _) | ((_, o, l) : _) <- matchElems arr, o >= 0 -> Just (o + 1, o + l)
    _                                                            -> Nothing

-- | 正規表現メタ文字をエスケープ (= @str_escape@・§15.6・リテラル文字列からパターンを作る用)。
strEscape :: Text -> Text
strEscape = T.concatMap esc
  where
    esc c | c `elem` metas = T.pack ['\\', c]
          | otherwise      = T.singleton c
    metas = ".^$|()[]{}*+?\\" :: String

-- | 名前付きグループで列に分割 (= @separate_wider_regex(df, col, patterns)@・§15.3.4)。
--   @specs@ = @[(Just 列名 | Nothing, 部分パターン)]@。 各部分パターンを順に capture group 化し、
--   セルを文字列全体マッチ (@^...$@) して各 group を対応列へ。 'Nothing' の group は捨てる
--   (= R の名無し)。 ★各部分パターンは **内部に capturing group を持たない前提**
--   (持つと group index がずれる・R4DS の例は単純パターンのみ)。
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
