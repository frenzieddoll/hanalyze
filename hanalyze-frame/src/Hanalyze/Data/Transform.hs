{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Hanalyze.Data.Transform
-- Description : dplyr 流の順位・オフセット・累積・区間化を純粋な [a] -> [b] として提供
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: dplyr 流の順序付き/窓関数的なベクトル変換。
--
-- R4DS Ch13 "Numbers" で扱う順位・オフセット・累積・区間化・連続識別子を、
-- __純粋な `[a] -> [b]`__ として公開する。 統計 ('Stat.Descriptive') でも IO/DataFrame
-- (@DataIO@) でもなく、 純粋データ抽象の `Data/` 名前空間に置く (@Data.ColumnSource@
-- の隣)。 DataFrame 直結解析 API の @mutate@ もこれを呼ぶ。
--
-- === NA
-- 順位関数は NA を含むベクトル用に @*NA@ 変種 (@[Maybe a] -> [Maybe b]@・dplyr の
-- @na.last="keep"@ と同じく Nothing は Nothing を保ち、 残りを順位付け) を併設する。
--
-- === desc
-- 降順順位は別関数を設けず 'Data.Ord.Down' を被せる (@minRank (map Down xs)@・
-- NA 付きは @minRankNA (map (fmap Down) xs)@)。
--
-- [English]: dplyr-style ordered\/window-function-like vector
-- transformations.
--
-- Exposes the ranking, offset, cumulative, binning, and consecutive-id
-- operations from R4DS Ch13 "Numbers" as __pure `[a] -> [b]` functions__.
-- Rather than statistics ('Stat.Descriptive') or IO\/DataFrame (@DataIO@),
-- this lives in the pure data abstraction `Data/` namespace (alongside
-- @Data.ColumnSource@). The DataFrame-integrated analysis API's @mutate@
-- also calls into this module.
--
-- === NA
-- The ranking functions have @*NA@ variants for vectors containing NA
-- (@[Maybe a] -> [Maybe b]@; like dplyr's @na.last="keep"@, Nothing stays
-- Nothing while the rest are ranked).
--
-- === desc
-- Descending rank has no separate function; instead, wrap with
-- 'Data.Ord.Down' (@minRank (map Down xs)@; with NA,
-- @minRankNA (map (fmap Down) xs)@).
module Hanalyze.Data.Transform
  ( -- * 順位 (dplyr ranking)
    minRank, denseRank, rowNumber
  , percentRank, cumeDist
    -- * 順位 (NA 保持変種)
  , minRankNA, denseRankNA, rowNumberNA
  , percentRankNA, cumeDistNA
    -- * オフセット
  , lag, lead
    -- * 累積
  , cumsum, cumprod, cummin, cummax, cummean
    -- * 区間化
  , cut, cutLabels
    -- * 連続識別子
  , consecutiveId
  ) where

import           Data.List       (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

-- ===========================================================================
-- 順位
-- ===========================================================================

-- | [日本語]: 最小順位法 (dplyr @min_rank@・tie は最小を共有し次を飛ばす: 1,2,2,4)。
--   各値の順位 = 1 + (厳密に小さい要素数)。
--   [English]: Minimum-rank method (dplyr's @min_rank@; ties share the
--   smallest rank and the next rank is skipped: 1,2,2,4). Each value's rank
--   = 1 + (the number of strictly smaller elements).
minRank :: Ord a => [a] -> [Int]
minRank xs = map ((rm M.!) ) xs
  where
    sorted = sortOn id xs
    -- 値 → 最初の出現 index (= 厳密に小さい要素数)。1 始まりにして格納。
    rm = M.fromListWith min [ (v, i + 1) | (i, v) <- zip [0 ..] sorted ]

-- | [日本語]: 密順位法 (dplyr @dense_rank@・tie で番号を飛ばさない: 1,2,2,3)。
--   各値の順位 = その値以下の __相異なる値の個数__。
--   [English]: Dense-rank method (dplyr's @dense_rank@; ties don't skip a
--   number: 1,2,2,3). Each value's rank = __the count of distinct values__
--   less than or equal to it.
denseRank :: Ord a => [a] -> [Int]
denseRank xs = map (dm M.!) xs
  where
    distinct = S.toAscList (S.fromList xs)
    dm = M.fromList (zip distinct [1 :: Int ..])

-- | [日本語]: 行番号 (dplyr @row_number@・tie も出現順で一意: 1,2,3,4)。
--   [English]: Row number (dplyr's @row_number@; even ties get a unique
--   value based on order of appearance: 1,2,3,4).
rowNumber :: Ord a => [a] -> [Int]
rowNumber xs = map (rm M.!) [0 .. n - 1]
  where
    n     = length xs
    order = map fst (sortOn snd (zip [0 :: Int ..] xs))  -- (origIdx, value) を value, origIdx 昇順
    rm    = M.fromList (zip order [1 :: Int ..])

-- | [日本語]: パーセント順位 (dplyr @percent_rank@ = (minRank - 1)/(n - 1))。
--   [English]: Percent rank (dplyr's @percent_rank@ = (minRank - 1)/(n - 1)).
percentRank :: Ord a => [a] -> [Double]
percentRank xs
  | n <= 1    = map (const 0) xs
  | otherwise = [ fromIntegral (r - 1) / fromIntegral (n - 1) | r <- minRank xs ]
  where n = length xs

-- | [日本語]: 累積分布 (dplyr @cume_dist@ = (≤x の個数)/n)。
--   [English]: Cumulative distribution (dplyr's @cume_dist@ = (count ≤ x)/n).
cumeDist :: Ord a => [a] -> [Double]
cumeDist xs = map (\x -> fromIntegral (cm M.! x) / fromIntegral n) xs
  where
    n      = length xs
    sorted = sortOn id xs
    -- 値 → その値の最後の出現 index + 1 (= ≤ その値の個数)。
    cm = M.fromListWith max [ (v, i + 1) | (i, v) <- zip [0 ..] sorted ]

-- --- NA 保持変種 -----------------------------------------------------------

-- | [日本語]: 非 NA だけを @f@ で順位付けし、 NA (Nothing) は Nothing のまま位置を保つ。
--   [English]: Ranks only the non-NA values via @f@, keeping NA (Nothing)
--   as Nothing in place.
onJusts :: forall a b. ([a] -> [b]) -> [Maybe a] -> [Maybe b]
onJusts f xs =
  let idxVals = [ (i, a) | (i, Just a) <- zip [0 ..] xs ]
      ranked  = f (map snd idxVals)
      m       = M.fromList (zip (map fst idxVals) ranked)
  in [ M.lookup i m | i <- [0 .. length xs - 1] ]

minRankNA     :: Ord a => [Maybe a] -> [Maybe Int]
minRankNA      = onJusts minRank
denseRankNA   :: Ord a => [Maybe a] -> [Maybe Int]
denseRankNA    = onJusts denseRank
rowNumberNA   :: Ord a => [Maybe a] -> [Maybe Int]
rowNumberNA    = onJusts rowNumber
percentRankNA :: Ord a => [Maybe a] -> [Maybe Double]
percentRankNA  = onJusts percentRank
cumeDistNA    :: Ord a => [Maybe a] -> [Maybe Double]
cumeDistNA     = onJusts cumeDist

-- ===========================================================================
-- オフセット
-- ===========================================================================

-- | [日本語]: @lag n d xs@: 各値を n 個後ろへずらし、 先頭 n 個を default @d@ で埋める
--   (dplyr @lag(x, n, default)@・既定 R は NA)。 入力と同長。
--   [English]: @lag n d xs@: Shifts each value back by n positions, filling
--   the first n with the default @d@ (dplyr's @lag(x, n, default)@; R's
--   default is NA). Same length as the input.
lag :: Int -> a -> [a] -> [a]
lag n d xs = take (length xs) (replicate n d ++ xs)

-- | [日本語]: @lead n d xs@: 各値を n 個前へずらし、 末尾 n 個を default @d@ で埋める。
--   [English]: @lead n d xs@: Shifts each value forward by n positions,
--   filling the last n with the default @d@.
lead :: Int -> a -> [a] -> [a]
lead n d xs = take (length xs) (drop n xs ++ repeat d)

-- ===========================================================================
-- 累積
-- ===========================================================================

cumsum  :: Num a => [a] -> [a]
cumsum   = scanl1 (+)
cumprod :: Num a => [a] -> [a]
cumprod  = scanl1 (*)
cummin  :: Ord a => [a] -> [a]
cummin   = scanl1 min
cummax  :: Ord a => [a] -> [a]
cummax   = scanl1 max

-- | [日本語]: 累積平均 (dplyr @cummean@・i 番目 = 先頭 i 個の平均)。
--   [English]: Cumulative mean (dplyr's @cummean@; the i-th element = the
--   mean of the first i elements).
cummean :: [Double] -> [Double]
cummean xs = zipWith (\s i -> s / fromIntegral i) (scanl1 (+) xs) [1 :: Int ..]

-- ===========================================================================
-- 区間化 (base R cut・既定 right = TRUE → (a, b])
-- ===========================================================================

-- | [日本語]: @cut breaks xs@: 各値が属する bin の index (1 始まり) を返す。 境界は昇順前提・
--   区間は @(lo, hi]@ (right=TRUE)・範囲外は Nothing (= R の NA)。
--   [English]: @cut breaks xs@: Returns the (1-indexed) bin index each value
--   belongs to. Breaks are assumed ascending; intervals are @(lo, hi]@
--   (right=TRUE); out-of-range values yield Nothing (= R's NA).
cut :: [Double] -> [Double] -> [Maybe Int]
cut breaks = map binOf
  where
    intervals = zip [1 :: Int ..] (zip breaks (drop 1 breaks))
    binOf x = case [ i | (i, (lo, hi)) <- intervals, x > lo, x <= hi ] of
                (i : _) -> Just i
                []      -> Nothing

-- | [日本語]: ラベル付き 'cut' (@labels@ は @breaks - 1@ 個)。
--   [English]: A labeled variant of 'cut' (@labels@ has @breaks - 1@
--   elements).
cutLabels :: [b] -> [Double] -> [Double] -> [Maybe b]
cutLabels labels breaks = map (fmap (labels !!) . fmap (subtract 1)) . cut breaks

-- ===========================================================================
-- 連続識別子 (dplyr consecutive_id・値が変わるたび +1)
-- ===========================================================================

consecutiveId :: Eq a => [a] -> [Int]
consecutiveId = go Nothing 0
  where
    go _ _ [] = []
    go prev cur (y : ys) =
      let cur' = case prev of
                   Just p | p == y -> cur
                   _               -> cur + 1
      in cur' : go (Just y) cur' ys
