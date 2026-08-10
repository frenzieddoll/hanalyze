{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Hanalyze.Data.Factor
-- Description : forcats 流の因子 (factor) 型と水準操作 (fct_* 相当)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: forcats 流の因子 (factor) 型操作 (Ch16 "Factors")。
--
-- R の `factor` を __水準 (levels) の順序付きリスト + 各観測の水準コード__として
-- 表す 'Factor' 型と、 forcats の `fct_*` 相当を純粋関数として公開する
-- ('Data.Strings' / 'Data.Transform' と同列の `Data/` 純粋抽象)。
--
-- === なぜ専用型か
-- ただの @[Text]@ と違い factor は (1) 水準の__意味的順序__ (アルファベット順とは別)、
-- (2) データに現れない水準の保持、 (3) 整数コード化、 を持つ。 forcats の `fct_*` は
-- この水準順序や中身を操作する関数群で、 順序概念のない @[Text]@ には無い。
--
-- === HBM の Column.Factor との違い
-- 'Hanalyze.Model.HBM' の内部 @Column = Numeric | Factor@ は NUTS に渡す
-- 観測列の内部表現。 本 'Factor' は __データ整形ドメイン__の公開型で責務が別 (独立実装)。
--
-- === コード規約
-- 'facCodes' は __0 始まり__ (@facLevels !! code@ で復元)。 欠損 (R の @\<NA\>@) は
-- コード @-1@ で表す ('naCode')。
--
-- [English]: forcats-style factor type operations (Ch16 "Factors").
--
-- Provides the 'Factor' type, which represents R's `factor` as
-- __an ordered list of levels plus each observation's level code__, along with pure
-- functions for the `fct_*` equivalents from forcats (a pure `Data/`
-- abstraction alongside 'Data.Strings' \/ 'Data.Transform').
--
-- === Why a dedicated type
-- Unlike a plain @[Text]@, a factor carries (1) a __semantic ordering__ of
-- levels (distinct from alphabetical order), (2) levels that persist even
-- when absent from the data, and (3) integer coding. forcats's `fct_*`
-- functions manipulate this level ordering and content, which has no
-- counterpart for an order-less @[Text]@.
--
-- === Difference from HBM's Column.Factor
-- The internal @Column = Numeric | Factor@ in 'Hanalyze.Model.HBM' is
-- the internal representation of an observation column passed to NUTS. This
-- 'Factor' is a public type for the __data-wrangling domain__ with a
-- separate responsibility (an independent implementation).
--
-- === Coding convention
-- 'facCodes' is __0-indexed__ (recovered via @facLevels !! code@). Missing
-- values (R's @\<NA\>@) are represented by code @-1@ ('naCode').
module Hanalyze.Data.Factor
  ( -- * 型
    Factor (..)
  , naCode
    -- * 生成 (factor / fct / ordered)
  , factor
  , factorWith
  , fct
  , ordered
    -- * 参照 (levels / as.character / count)
  , levels
  , isOrdered
  , asTexts
  , asTextsMaybe
  , fctCount
    -- * 順序操作 (16.4 forcats fct_reorder 系)
  , fctReorder
  , fctRelevel
  , fctReorder2
  , fctInfreq
  , fctRev
    -- * 水準操作 (16.5 forcats fct_recode / fct_collapse / fct_lump 系)
  , fctRecode
  , fctCollapse
  , fctLumpN
  , fctLumpMin
  , fctLumpProp
  , fctLumpLowfreq
  ) where

import           Data.List (foldl', sort, sortBy, elemIndex)
import           Data.Maybe (mapMaybe)
import           Data.Ord (comparing, Down (..))
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-- === 型 =====================================================================

-- | [日本語]: 因子。 水準ラベル + 各観測の水準コード (0 始まり・NA は 'naCode')。
--   [English]: A factor. Level labels + each observation's level code
--   (0-indexed; NA is 'naCode').
data Factor = Factor
  { facLevels  :: [Text]        -- ^ [日本語]: 水準ラベル (定義順) [English]: The level labels, in definition order.
  , facCodes   :: VU.Vector Int -- ^ [日本語]: 各観測の水準コード (0 始まり・NA = 'naCode') [English]: Each observation's level code (0-indexed; NA = 'naCode').
  , facOrdered :: Bool          -- ^ [日本語]: 順序付き因子か (R `ordered()`) [English]: Whether this is an ordered factor (R's `ordered()`).
  } deriving (Eq, Show)

-- | [日本語]: 欠損コード (R の @NA_integer_@ 相当)。
--   [English]: The missing-value code (equivalent to R's @NA_integer_@).
naCode :: Int
naCode = -1

-- === 生成 ===================================================================

-- | [日本語]: @factor xs@ : R の `factor()` 既定。 水準 = 値の __ソート済 unique__。
--   [English]: @factor xs@: R's default `factor()`. The levels are the
--   __sorted unique__ values.
factor :: [Text] -> Factor
factor xs = factorWith (sortUnique xs) xs

-- | [日本語]: @factorWith lvls xs@ : 水準を明示。 @lvls@ に無い値は NA ('naCode')。
--   [English]: @factorWith lvls xs@: Levels are given explicitly. Values not
--   present in @lvls@ become NA ('naCode').
factorWith :: [Text] -> [Text] -> Factor
factorWith lvls xs = Factor lvls (VU.fromList (map enc xs)) False
  where
    idx   = M.fromList (zip lvls [0 ..])
    enc x = M.findWithDefault naCode x idx

-- | [日本語]: @fct xs@ : forcats の `fct()`。 水準 = 値の __出現順 unique__ (factor() と違い sort しない)。
--   [English]: @fct xs@: forcats's `fct()`. The levels are the values'
--   __unique values in order of first appearance__ (unlike factor(), it
--   does not sort).
fct :: [Text] -> Factor
fct xs = factorWith (nubKeepOrder xs) xs

-- | [日本語]: @ordered lvls xs@ : 順序付き因子 (R `ordered()`)。 水準間に @<@ 順序を持つ。
--   [English]: @ordered lvls xs@: An ordered factor (R's `ordered()`), with
--   a @<@ ordering between levels.
ordered :: [Text] -> [Text] -> Factor
ordered lvls xs = (factorWith lvls xs) { facOrdered = True }

-- === 参照 ===================================================================

-- | [日本語]: 水準ラベル (定義順)。
--   [English]: The level labels, in definition order.
levels :: Factor -> [Text]
levels = facLevels

-- | [日本語]: 順序付き因子か。
--   [English]: Whether this is an ordered factor.
isOrdered :: Factor -> Bool
isOrdered = facOrdered

-- | [日本語]: @as.character()@ 相当。 各観測をラベルへ。 NA は @""@。
--   [English]: Equivalent to @as.character()@. Converts each observation to
--   its label; NA becomes @""@.
asTexts :: Factor -> [Text]
asTexts f = map (maybe "" id) (asTextsMaybe f)

-- | [日本語]: NA を 'Nothing' で残す版。
--   [English]: The variant that keeps NA as 'Nothing'.
asTextsMaybe :: Factor -> [Maybe Text]
asTextsMaybe f = map lab (VU.toList (facCodes f))
  where
    v = V.fromList (facLevels f)
    lab c | c < 0 || c >= V.length v = Nothing
          | otherwise                = Just (v V.! c)

-- | [日本語]: dplyr `count()` 相当。 __水準順__に (ラベル, 出現数)。 0 件水準も含む。 NA は除外。
--   [English]: Equivalent to dplyr's `count()`. Returns (label, count) pairs
--   __in level order__, including levels with zero occurrences. NA is
--   excluded.
fctCount :: Factor -> [(Text, Int)]
fctCount f = [ (lv, M.findWithDefault 0 i cmap) | (lv, i) <- zip (facLevels f) [0 ..] ]
  where
    cmap = foldl' (\m c -> if c < 0 then m else M.insertWith (+) c 1 m)
                  M.empty (VU.toList (facCodes f))

-- === 順序操作 (16.4) ========================================================
--
-- いずれも **facLevels の順序を入れ替え、 facCodes を旧 code → 新 code に
-- 再マッピング**して実装する ('applyLevelOrder')。 観測の中身 (どの水準か) は
-- 不変で、 水準の**並び順**だけが変わる (forcats `fct_reorder` 系と同義)。

-- | [日本語]: forcats `fct_reorder(f, x, .fun)`。 各水準に属する @x@ 値へ集約関数 @fun@
-- (R4DS の既定は @median@) を適用し、 その値の__昇順__に水準を並べ替える。
-- @x@ 中の @NaN@ は NA とみなし集約前に除去。 値が無い水準は末尾。
--   [English]: forcats's `fct_reorder(f, x, .fun)`. Applies the aggregation
--   function @fun@ (R4DS's default is @median@) to the @x@ values belonging
--   to each level, then reorders the levels by that value's __ascending__
--   order. @NaN@ in @x@ is treated as NA and removed before aggregation.
--   Levels with no values go last.
fctReorder :: ([Double] -> Double) -> Factor -> [Double] -> Factor
fctReorder fun f xs = applyLevelOrder order f
  where
    n     = length (facLevels f)
    grp   = groupByCode n (facCodes f) xs
    key i = case filter (not . isNaN) (grp V.! i) of
              [] -> 1 / 0          -- 値なし → +Inf で末尾へ
              vs -> fun vs
    order = sortBy (comparing key) [0 .. n - 1]

-- | [日本語]: forcats `fct_relevel(f, ...)`。 指定した水準を (指定順で) __先頭__へ移し、
-- 残りは元の相対順序を保つ。 存在しない水準名は無視する。
--   [English]: forcats's `fct_relevel(f, ...)`. Moves the given levels
--   (in the given order) to the __front__, keeping the rest in their
--   original relative order. Nonexistent level names are ignored.
fctRelevel :: [Text] -> Factor -> Factor
fctRelevel front f = applyLevelOrder order f
  where
    lvs      = facLevels f
    frontIx  = mapMaybe (`elemIndex` lvs) front
    rest     = [ i | i <- [0 .. length lvs - 1], i `notElem` frontIx ]
    order    = frontIx ++ rest

-- | [日本語]: forcats `fct_reorder2(f, x, y)` (既定 @last2@)。 各水準で
-- __最大の @x@ に対応する @y@__ を取り、 その値の__降順__に水準を並べ替える (凡例順を線の
-- 右端の高さに合わせる用途)。 @x@ 同値は後着 (入力順で後ろ) を採用。
--   [English]: forcats's `fct_reorder2(f, x, y)` (default @last2@). Takes,
--   for each level, __the @y@ corresponding to the largest @x@__, and
--   reorders levels by that value's __descending__ order (used to align
--   legend order with the height at the line's right edge). Ties in @x@
--   favor the later-occurring input.
fctReorder2 :: Factor -> [Double] -> [Double] -> Factor
fctReorder2 f xs ys = applyLevelOrder order f
  where
    n     = length (facLevels f)
    grp   = groupPairs n (facCodes f) xs ys
    key i = case grp V.! i of
              [] -> -1 / 0         -- 値なし → -Inf で降順末尾へ
              ps -> snd (foldl1 (\a b -> if fst b >= fst a then b else a) ps)
    order = sortBy (comparing (Down . key)) [0 .. n - 1]

-- | [日本語]: forcats `fct_infreq(f)`。 出現__頻度の降順__に水準を並べ替える。
-- 同頻度は元の水準順を保つ (安定)。 NA は計数対象外。
--   [English]: forcats's `fct_infreq(f)`. Reorders levels by
--   __descending frequency of occurrence__. Ties keep the original level order (stable);
--   NA is excluded from counting.
fctInfreq :: Factor -> Factor
fctInfreq f = applyLevelOrder order f
  where
    n      = length (facLevels f)
    counts = [ M.findWithDefault 0 i cmap | i <- [0 .. n - 1] ] :: [Int]
    cmap   = foldl' (\m c -> if c < 0 then m else M.insertWith (+) c 1 m)
                    M.empty (VU.toList (facCodes f))
    order  = sortBy (comparing (Down . (countArr V.!))) [0 .. n - 1]
    countArr = V.fromList counts

-- | [日本語]: forcats `fct_rev(f)`。 水準を__逆順__にする。
--   [English]: forcats's `fct_rev(f)`. Reverses the order of the levels.
fctRev :: Factor -> Factor
fctRev f = applyLevelOrder (reverse [0 .. length (facLevels f) - 1]) f

-- === 水準操作 (16.5) ========================================================
--
-- 水準**ラベルそのもの**を付け替える/併合する操作。 ラベルを付け替えた結果
-- 同名になった水準は 1 つに畳む ('relabelMerge')。 lump 系は余りを @"Other"@ に
-- まとめ、 forcats と同じく @"Other"@ を**末尾水準**に置く。

-- | [日本語]: 余り水準のまとめ先ラベル (forcats @other_level@ 既定)。
--   [English]: The label that leftover levels are merged into (forcats's
--   default @other_level@).
otherLevel :: Text
otherLevel = "Other"

-- | [日本語]: forcats `fct_recode(f, new = "old", ...)`。 水準ラベルを改名する。
-- 引数は @(新, 旧)@ の対。 複数の旧を同じ新に向ければ__併合__される。
-- 言及されない水準はそのまま。 水準の相対順序は保つ。
--   [English]: forcats's `fct_recode(f, new = "old", ...)`. Renames level
--   labels. Arguments are @(new, old)@ pairs; pointing multiple olds at the
--   same new __merges__ them. Unmentioned levels are left unchanged, and
--   the levels' relative order is preserved.
fctRecode :: [(Text, Text)] -> Factor -> Factor
fctRecode pairs f = relabelMerge newLabels f
  where
    m         = M.fromList [ (old, new) | (new, old) <- pairs ]
    newLabels = [ M.findWithDefault lv lv m | lv <- facLevels f ]

-- | [日本語]: forcats `fct_collapse(f, new = c("o1","o2"), ...)`。 複数水準を 1 つへ併合。
-- 引数は @(新, [旧...])@。 言及されない水準はそのまま (@other_level@ は非対応)。
--   [English]: forcats's `fct_collapse(f, new = c("o1","o2"), ...)`. Merges
--   multiple levels into one. Arguments are @(new, [old...])@. Unmentioned
--   levels are left unchanged (@other_level@ is not supported).
fctCollapse :: [(Text, [Text])] -> Factor -> Factor
fctCollapse groups f = relabelMerge newLabels f
  where
    m         = M.fromList [ (old, new) | (new, olds) <- groups, old <- olds ]
    newLabels = [ M.findWithDefault lv lv m | lv <- facLevels f ]

-- | [日本語]: forcats `fct_lump_n(f, n)`。 頻度上位 @n@ 水準を残し、 他を @"Other"@ へ。
-- @n@ 負なら頻度__下位__ @|n|@ を残す。 同頻度のタイは両方残す (上位 n 側)。
--   [English]: forcats's `fct_lump_n(f, n)`. Keeps the top-@n@ levels by
--   frequency, lumping the rest into @"Other"@. If @n@ is negative, keeps
--   the bottom @|n|@ by frequency. Ties at the frequency threshold are both
--   kept (on the top-n side).
fctLumpN :: Int -> Factor -> Factor
fctLumpN n f
  | m == 0    = f
  | n == 0    = lumpBy (const True) f
  | n > 0     = lumpBy (\i -> cnts !! i < thrTop) f
  | otherwise = lumpBy (\i -> cnts !! i > thrBot) f
  where
    cnts   = levelCounts f
    m      = length cnts
    thrTop | n >= m    = minimum cnts                      -- 全水準保持
           | otherwise = sortBy (comparing Down) cnts !! (n - 1)
    k      = negate n
    thrBot | k >= m    = maximum cnts                      -- 全水準保持
           | otherwise = sort cnts !! (k - 1)

-- | [日本語]: forcats `fct_lump_min(f, min)`。 出現回数 @< min@ の水準を @"Other"@ へ (strict)。
--   [English]: forcats's `fct_lump_min(f, min)`. Lumps levels with a count
--   @< min@ into @"Other"@ (strict).
fctLumpMin :: Int -> Factor -> Factor
fctLumpMin k f = lumpBy (\i -> cnts !! i < k) f
  where cnts = levelCounts f

-- | [日本語]: forcats `fct_lump_prop(f, prop)`。 出現割合 @< prop@ の水準を @"Other"@ へ。
--   [English]: forcats's `fct_lump_prop(f, prop)`. Lumps levels whose
--   proportion of occurrences is @< prop@ into @"Other"@.
fctLumpProp :: Double -> Factor -> Factor
fctLumpProp p f = lumpBy (\i -> fromIntegral (cnts !! i) < p * fromIntegral total) f
  where
    cnts  = levelCounts f
    total = sum cnts

-- | [日本語]: forcats `fct_lump_lowfreq(f)`。 低頻度水準を、 @"Other"@ が__最小水準のまま__で
-- いられる範囲で併合する。 頻度降順に並べ、 ある水準が「それより低頻度な水準の
-- 合計」 を上回った時点で、 以降を @"Other"@ へまとめる (forcats @lump_cutoff@ 同型)。
--   [English]: forcats's `fct_lump_lowfreq(f)`. Merges low-frequency levels
--   to the extent that @"Other"@ can remain __the smallest level__. Levels
--   are ordered by descending frequency, and once a level's count exceeds
--   "the sum of all less-frequent levels", everything from there on is
--   lumped into @"Other"@ (isomorphic to forcats's @lump_cutoff@).
fctLumpLowfreq :: Factor -> Factor
fctLumpLowfreq f
  | m == 0    = f
  | otherwise = lumpBy (\i -> (cnts !! i, i) `elem` lumpedKeys) f
  where
    cnts       = levelCounts f
    m          = length cnts
    -- (頻度, 水準index) を頻度降順 (タイは index 昇順) に
    descKeys   = sortBy (comparing (\(c, i) -> (Down c, i))) (zip cnts [0 ..])
    cut        = cutoff (map fst descKeys)          -- 保持する個数 (cut..末尾を lump)
    lumpedKeys = drop cut descKeys

-- | [日本語]: forcats @lump_cutoff@: 降順 count 列で、 「自分 > 残り (より低頻度) の合計」 と
-- なる最初の位置 @i@ を見つけ、 @i+1@ (= 0 始まりで @i@ 番目まで保持) を返す。
-- 該当無しなら全保持 (= 長さ)。
--   [English]: forcats's @lump_cutoff@. In a descending-count list, finds
--   the first position @i@ where "this count > the sum of the remaining
--   (less-frequent) counts", and returns @i+1@ (i.e. keep through 0-indexed
--   position @i@). If no such position exists, keeps everything (= the
--   length).
cutoff :: [Int] -> Int
cutoff xs = go 0 (sum xs) xs
  where
    go i _    []       = i
    go i left (c : cs)
      | c > left - c   = i + 1
      | otherwise      = go (i + 1) (left - c) cs

-- === 内部 helper ============================================================

-- | [日本語]: 水準ラベルを @newLabels@ (旧水準 index 順・長さ = 旧水準数) に付け替え、
-- 同名になった水準を 1 つに畳む。 新水準順は新ラベルの__初出順__。 NA は不変。
--   [English]: Relabels the levels using @newLabels@ (in old-level index
--   order; length = number of old levels), folding levels that become
--   identically named into one. The new level order follows the new
--   labels' __order of first appearance__. NA is unchanged.
relabelMerge :: [Text] -> Factor -> Factor
relabelMerge newLabels f =
  Factor newLevels (VU.map remap (facCodes f)) (facOrdered f)
  where
    newLevels = nubKeepOrder newLabels
    pos       = M.fromList (zip newLevels [0 ..])
    o2nVec    = V.fromList [ pos M.! lab | lab <- newLabels ]
    remap c | c < 0 || c >= V.length o2nVec = c
            | otherwise                     = o2nVec V.! c

-- | [日本語]: 指定水準を__末尾__へ移す (存在しなければ不変)。 'applyLevelOrder' を再利用。
--   [English]: Moves the given level to the __end__ (a no-op if it doesn't
--   exist). Reuses 'applyLevelOrder'.
moveLevelToEnd :: Text -> Factor -> Factor
moveLevelToEnd lv f = case elemIndex lv (facLevels f) of
  Nothing -> f
  Just i  -> applyLevelOrder ([ j | j <- [0 .. n - 1], j /= i ] ++ [i]) f
  where n = length (facLevels f)

-- | [日本語]: 水準 index で lump 判定し、 余りを 'otherLevel' へ併合 (末尾に置く)。
-- lump 対象が無ければ不変。
--   [English]: Decides lumping by level index, merging the leftovers into
-- 'otherLevel' (placed at the end). A no-op if nothing qualifies for
-- lumping.
lumpBy :: (Int -> Bool) -> Factor -> Factor
lumpBy isLump f
  | not (or lumpFlags) = f
  | otherwise          = moveLevelToEnd otherLevel (relabelMerge newLabels f)
  where
    lumpFlags = map isLump [0 .. length (facLevels f) - 1]
    newLabels = [ if isLump i then otherLevel else lv
                | (i, lv) <- zip [0 ..] (facLevels f) ]

-- | [日本語]: 各水準 (定義順) の出現回数。 NA は除外。
--   [English]: The occurrence count of each level, in definition order.
--   NA is excluded.
levelCounts :: Factor -> [Int]
levelCounts f = [ M.findWithDefault 0 i cmap | i <- [0 .. length (facLevels f) - 1] ]
  where
    cmap = foldl' (\m c -> if c < 0 then m else M.insertWith (+) c 1 m)
                  M.empty (VU.toList (facCodes f))

-- | [日本語]: @applyLevelOrder order f@ : @order@ は新しい並びを表す__旧 index のリスト__
-- (@order !! p@ = 新位置 @p@ に来る旧水準 index)。 facLevels を並べ替え、
-- facCodes を旧 code → 新 code に再マップする。 NA ('naCode') はそのまま。
--   [English]: @applyLevelOrder order f@: @order@ is
--   __a list of old indices__ representing the new arrangement (@order !! p@ = the old
--   level index that lands at new position @p@). Reorders facLevels and
--   remaps facCodes from old code to new code. NA ('naCode') is left
--   unchanged.
applyLevelOrder :: [Int] -> Factor -> Factor
applyLevelOrder order f = f
  { facLevels = map (lvs V.!) order
  , facCodes  = VU.map remap (facCodes f)
  }
  where
    lvs   = V.fromList (facLevels f)
    n     = length (facLevels f)
    -- 旧 index → 新 index
    o2n   = VU.replicate n (-1) VU.// [ (old, new) | (new, old) <- zip [0 ..] order ]
    remap c | c < 0 || c >= n = c
            | otherwise       = o2n VU.! c

-- | [日本語]: 水準コードで @x@ 値をグループ化 (長さ n・入力順保持・NA/範囲外は除外)。
--   [English]: Groups @x@ values by level code (length n, preserving input
--   order; NA \/ out-of-range are excluded).
groupByCode :: Int -> VU.Vector Int -> [Double] -> V.Vector [Double]
groupByCode n codes xs =
  V.map reverse $ V.accum (flip (:)) (V.replicate n [])
    [ (c, x) | (c, x) <- zip (VU.toList codes) xs, c >= 0, c < n ]

-- | [日本語]: 水準コードで @(x, y)@ 対をグループ化 (長さ n・入力順保持・NA/範囲外は除外)。
--   [English]: Groups @(x, y)@ pairs by level code (length n, preserving
--   input order; NA \/ out-of-range are excluded).
groupPairs :: Int -> VU.Vector Int -> [Double] -> [Double] -> V.Vector [(Double, Double)]
groupPairs n codes xs ys =
  V.map reverse $ V.accum (flip (:)) (V.replicate n [])
    [ (c, (x, y)) | (c, x, y) <- zip3 (VU.toList codes) xs ys, c >= 0, c < n ]

-- | [日本語]: ソート済 unique (Map のキー集合 = 昇順 unique)。
--   [English]: Sorted unique values (a Map's key set = ascending unique).
sortUnique :: [Text] -> [Text]
sortUnique = M.keys . M.fromList . map (\x -> (x, ()))

-- | [日本語]: 出現順を保った unique。
--   [English]: Unique values that preserve order of appearance.
nubKeepOrder :: [Text] -> [Text]
nubKeepOrder = go M.empty
  where
    go _ [] = []
    go seen (x : xs)
      | x `M.member` seen = go seen xs
      | otherwise         = x : go (M.insert x () seen) xs
