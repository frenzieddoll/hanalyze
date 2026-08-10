{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Hanalyze.Data.Transform
-- Description : dplyr 流の順位・オフセット・累積・区間化を純粋な [a] -> [b] として提供
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- dplyr 流の順序付き/窓関数的なベクトル変換 (Phase 66)。
--
-- R4DS Ch13 "Numbers" で扱う順位・オフセット・累積・区間化・連続識別子を、
-- **純粋な `[a] -> [b]`** として公開する。 統計 ('Stat.Descriptive') でも IO/DataFrame
-- ('DataIO') でもなく、 純粋データ抽象の `Data/` 名前空間に置く ('Data.ColumnSource'
-- の隣)。 DataFrame 直結解析 API (Phase 67) の @mutate@ もこれを呼ぶ。
--
-- === NA
-- 順位関数は NA を含むベクトル用に @*NA@ 変種 (@[Maybe a] -> [Maybe b]@・dplyr の
-- @na.last="keep"@ と同じく Nothing は Nothing を保ち、 残りを順位付け) を併設する。
--
-- === desc
-- 降順順位は別関数を設けず 'Data.Ord.Down' を被せる (@minRank (map Down xs)@・
-- NA 付きは @minRankNA (map (fmap Down) xs)@)。
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

-- | 最小順位法 (dplyr @min_rank@・tie は最小を共有し次を飛ばす: 1,2,2,4)。
--   各値の順位 = 1 + (厳密に小さい要素数)。
minRank :: Ord a => [a] -> [Int]
minRank xs = map ((rm M.!) ) xs
  where
    sorted = sortOn id xs
    -- 値 → 最初の出現 index (= 厳密に小さい要素数)。1 始まりにして格納。
    rm = M.fromListWith min [ (v, i + 1) | (i, v) <- zip [0 ..] sorted ]

-- | 密順位法 (dplyr @dense_rank@・tie で番号を飛ばさない: 1,2,2,3)。
--   各値の順位 = その値以下の **相異なる値の個数**。
denseRank :: Ord a => [a] -> [Int]
denseRank xs = map (dm M.!) xs
  where
    distinct = S.toAscList (S.fromList xs)
    dm = M.fromList (zip distinct [1 :: Int ..])

-- | 行番号 (dplyr @row_number@・tie も出現順で一意: 1,2,3,4)。
rowNumber :: Ord a => [a] -> [Int]
rowNumber xs = map (rm M.!) [0 .. n - 1]
  where
    n     = length xs
    order = map fst (sortOn snd (zip [0 :: Int ..] xs))  -- (origIdx, value) を value, origIdx 昇順
    rm    = M.fromList (zip order [1 :: Int ..])

-- | パーセント順位 (dplyr @percent_rank@ = (minRank - 1)/(n - 1))。
percentRank :: Ord a => [a] -> [Double]
percentRank xs
  | n <= 1    = map (const 0) xs
  | otherwise = [ fromIntegral (r - 1) / fromIntegral (n - 1) | r <- minRank xs ]
  where n = length xs

-- | 累積分布 (dplyr @cume_dist@ = (≤x の個数)/n)。
cumeDist :: Ord a => [a] -> [Double]
cumeDist xs = map (\x -> fromIntegral (cm M.! x) / fromIntegral n) xs
  where
    n      = length xs
    sorted = sortOn id xs
    -- 値 → その値の最後の出現 index + 1 (= ≤ その値の個数)。
    cm = M.fromListWith max [ (v, i + 1) | (i, v) <- zip [0 ..] sorted ]

-- --- NA 保持変種 -----------------------------------------------------------

-- | 非 NA だけを @f@ で順位付けし、 NA (Nothing) は Nothing のまま位置を保つ。
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

-- | @lag n d xs@: 各値を n 個後ろへずらし、 先頭 n 個を default @d@ で埋める
--   (dplyr @lag(x, n, default)@・既定 R は NA)。 入力と同長。
lag :: Int -> a -> [a] -> [a]
lag n d xs = take (length xs) (replicate n d ++ xs)

-- | @lead n d xs@: 各値を n 個前へずらし、 末尾 n 個を default @d@ で埋める。
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

-- | 累積平均 (dplyr @cummean@・i 番目 = 先頭 i 個の平均)。
cummean :: [Double] -> [Double]
cummean xs = zipWith (\s i -> s / fromIntegral i) (scanl1 (+) xs) [1 :: Int ..]

-- ===========================================================================
-- 区間化 (base R cut・既定 right = TRUE → (a, b])
-- ===========================================================================

-- | @cut breaks xs@: 各値が属する bin の index (1 始まり) を返す。 境界は昇順前提・
--   区間は @(lo, hi]@ (right=TRUE)・範囲外は Nothing (= R の NA)。
cut :: [Double] -> [Double] -> [Maybe Int]
cut breaks = map binOf
  where
    intervals = zip [1 :: Int ..] (zip breaks (drop 1 breaks))
    binOf x = case [ i | (i, (lo, hi)) <- intervals, x > lo, x <= hi ] of
                (i : _) -> Just i
                []      -> Nothing

-- | ラベル付き 'cut' (@labels@ は @breaks - 1@ 個)。
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
