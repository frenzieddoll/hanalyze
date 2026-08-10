{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
-- |
-- Module      : Hanalyze.Data.Wrangle
-- Description : DataFrame 直結の dplyr 風 summarise/mutate/groupBy 動詞
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- DataFrame 直結の解析動詞 (Phase 67)。
--
-- hgg の @df |>> layer (scatter "x" "y")@ と対称に、 DataFrame を直接
-- データ源として dplyr の @summarise@ / @mutate@ / @group_by@ 相当を
-- 「列名参照 + パイプライン + DataFrame in/out」 で書ける薄層。
--
-- 数値ロジックは 'Hanalyze.Stat.Descriptive' (Phase 65) と
-- 'Hanalyze.Data.Transform' (Phase 66) に委譲し、 本モジュールは
-- **列名解決 + 群化 + DataFrame 組み立て + NA 処理**のみを担う。
--
-- @
-- import DataFrame.Operators ((|>))
-- df |> summarise [ "mean" =: meanOf "dep_delay", "q95" =: quantileOf 0.95 "dep_delay", "n" =: nOf ]
-- df |> mutate    [ "z" =: zscoreOf "x", "rank" =: minRankOf "dep_delay" ]
-- df |> groupBy ["year","month","day"] |> summarise [ "mean" =: meanOf "dep_delay" ]
-- @
--
-- 集約子は既定で **NA 除去 (na.rm=TRUE 相当)**。 数値列のみ対象 (factor は群キー
-- としてのみ扱う)。 群の並びは dplyr 同様キー昇順。
--
-- v1 の制限 (判断申し送り): 入力は Hackage @DataFrame@ 限定 ('ColumnSource' 全多相化は
-- 後続)。 grouped @mutate@ は未対応 (ungrouped のみ)。
module Hanalyze.Data.Wrangle
  ( -- * 動詞
    summarise, Summarisable
  , mutate
  , groupBy, Grouped
    -- * 命名
  , (=:)
    -- * 集約子 (列名 → スカラ)
  , Agg
  , meanOf, medianOf, sdOf, varOf, iqrOf, quantileOf, sumOf, minOf, maxOf
  , nOf, nDistinctOf
    -- * 列式 (mutate・列名 → 列)
  , ColExpr
  , zscoreOf, minRankOf, denseRankOf, lagOf, leadOf, cumsumOf
  ) where

import           Data.Maybe               (catMaybes, isJust)
import           Data.List                (nub, transpose)
import qualified Data.Map.Strict          as M
import qualified Data.Vector              as V
import           Data.Text                (Text)
import qualified DataFrame.Internal.Column    as DF
import qualified DataFrame.Internal.DataFrame  as DF
import qualified DataFrame.Operations.Core     as DF
import qualified DataFrame.Internal.Column as DFC

import           Hanalyze.DataIO.Convert    (getMaybeTextVec)
import           Hanalyze.DataIO.Preprocess (readMaybeDoubleColumn)
import qualified Hanalyze.Stat.Descriptive  as D
import qualified Hanalyze.Data.Transform    as T

-- ===========================================================================
-- 内部表現
-- ===========================================================================

-- | 1 群分のデータ (行数 + 数値列の name→値・NA 保持・行整列)。
data Group = Group
  { gSize :: !Int
  , gNum  :: !(M.Map Text [Maybe Double])
  }

-- | 集約結果のセル (数値列 or 整数列)。
data Cell = CD !Double | CI !Int

-- | 群キーの 1 要素 (数値 or 文字列)。Ord は KNum<KTxt・KNum は数値順。
data KeyVal = KNum !Double | KTxt !Text deriving (Eq, Ord)

-- | 集約子: 群 → セル。
newtype Agg = Agg (Group -> Cell)

-- | 列式: 群 → 新しい列 (NA 保持)。
newtype ColExpr = ColExpr (Group -> [Maybe Double])

-- | 結果列名 + 操作 を結ぶ。
(=:) :: Text -> a -> (Text, a)
(=:) = (,)
infixr 0 =:

-- ===========================================================================
-- 集約子 (na.rm = TRUE 既定)
-- ===========================================================================

col1 :: Text -> ([Double] -> Double) -> Agg
col1 c f = Agg (\g -> CD (f (catMaybes (M.findWithDefault [] c (gNum g)))))

meanOf, medianOf, sdOf, varOf, iqrOf, sumOf, minOf, maxOf :: Text -> Agg
meanOf   c = col1 c D.meanL
medianOf c = col1 c D.medianL
sdOf     c = col1 c D.sdL
varOf    c = col1 c D.varianceL
iqrOf    c = col1 c D.iqrL
sumOf    c = col1 c sum
minOf    c = col1 c minimum
maxOf    c = col1 c maximum

quantileOf :: Double -> Text -> Agg
quantileOf p c = col1 c (D.quantileL p)

-- | 行数 (= R @n()@)。
nOf :: Agg
nOf = Agg (CI . gSize)

-- | 相異なる非 NA 値の個数 (= R @n_distinct()@)。
nDistinctOf :: Text -> Agg
nDistinctOf c = Agg (\g -> CI (length (nub (catMaybes (M.findWithDefault [] c (gNum g))))))

-- ===========================================================================
-- 列式 (mutate)
-- ===========================================================================

colE :: Text -> ([Maybe Double] -> [Maybe Double]) -> ColExpr
colE c f = ColExpr (\g -> f (M.findWithDefault [] c (gNum g)))

-- | Z スコア (x - mean) / sd。 NA は NA のまま。
zscoreOf :: Text -> ColExpr
zscoreOf c = colE c $ \xs ->
  let ys = catMaybes xs; m = D.meanL ys; s = D.sdL ys
  in map (fmap (\x -> (x - m) / s)) xs

-- | 最小順位 (dplyr min_rank・NA 保持)。
minRankOf :: Text -> ColExpr
minRankOf c = colE c (map (fmap fromIntegral) . T.minRankNA)

-- | 密順位 (dplyr dense_rank・NA 保持)。
denseRankOf :: Text -> ColExpr
denseRankOf c = colE c (map (fmap fromIntegral) . T.denseRankNA)

-- | n 個ラグ (先頭を NA 埋め)。
lagOf :: Int -> Text -> ColExpr
lagOf n c = colE c (T.lag n Nothing)

-- | n 個リード (末尾を NA 埋め)。
leadOf :: Int -> Text -> ColExpr
leadOf n c = colE c (T.lead n Nothing)

-- | 累積和 (NA は以降へ伝播 = R cumsum)。
cumsumOf :: Text -> ColExpr
cumsumOf c = colE c scan
  where scan []       = []
        scan (x : xs) = scanl (\acc y -> (+) <$> acc <*> y) x xs

-- ===========================================================================
-- 列抽出・群化
-- ===========================================================================

nrows :: DF.DataFrame -> Int
nrows = fst . DF.dimensions

-- | 数値列をすべて NA 保持・行整列で取り出す。
numColumns :: DF.DataFrame -> [(Text, V.Vector (Maybe Double))]
numColumns df =
  [ (n, V.fromList ms) | n <- DF.columnNames df, Just ms <- [readMaybeDoubleColumn n df] ]

-- | 群キー列を 'KeyVal' 列として取り出す (数値優先・無理なら Text)。
keyColumn :: Text -> DF.DataFrame -> [KeyVal]
keyColumn name df =
  -- 数値読みが「全 NA」 なら実体は Text 列ゆえ Text 抽出に倒す
  -- (readMaybeDoubleColumn は Text 列を Just [Nothing..] で返す罠の回避)。
  case readMaybeDoubleColumn name df of
    Just ms | any isJust ms -> map (maybe (KNum (0/0)) KNum) ms
    _ -> case getMaybeTextVec name df of
      Just v  -> map (maybe (KTxt "NA") KTxt) (V.toList v)
      Nothing -> replicate (nrows df) (KTxt "NA")

-- | 各行の群キー tuple。
rowKeys :: [Text] -> DF.DataFrame -> [[KeyVal]]
rowKeys keys df = transpose (map (`keyColumn` df) keys)

-- | キー昇順の群 (キー tuple, 行 index 群)。
groupRows :: [Text] -> DF.DataFrame -> [([KeyVal], [Int])]
groupRows keys df =
  M.toAscList (M.fromListWith (flip (++)) (zip (rowKeys keys df) (map (: []) [0 ..])))

mkGroup :: [(Text, V.Vector (Maybe Double))] -> [Int] -> Group
mkGroup ncols idxs = Group
  { gSize = length idxs
  , gNum  = M.fromList [ (n, [ v V.! i | i <- idxs ]) | (n, v) <- ncols ]
  }

-- ===========================================================================
-- セル / キー列 → DataFrame Column
-- ===========================================================================

buildCol :: [Cell] -> DFC.Column
buildCol cells
  | all isCI cells = DF.fromList [ i | CI i <- cells ]
  | otherwise      = DF.fromList (map toD cells)
  where
    isCI (CI _) = True
    isCI _      = False
    toD (CD x)  = x
    toD (CI i)  = fromIntegral i

buildKeyCol :: [KeyVal] -> DFC.Column
buildKeyCol ks
  | all isTxt ks                  = DF.fromList [ t | KTxt t <- ks ]
  | all isWholeNum ks             = DF.fromList [ round d :: Int | KNum d <- ks ]
  | otherwise                     = DF.fromList [ d | KNum d <- ks ]
  where
    isTxt (KTxt _) = True
    isTxt _        = False
    isWholeNum (KNum d) = not (isNaN d) && d == fromIntegral (round d :: Int)
    isWholeNum _        = False

-- ===========================================================================
-- 動詞
-- ===========================================================================

-- | 群化された DataFrame (キー列名を保持)。
data Grouped = Grouped ![Text] !DF.DataFrame

-- | dplyr @group_by@。
groupBy :: [Text] -> DF.DataFrame -> Grouped
groupBy = Grouped

-- | @summarise@ は DataFrame (= 1 群) でも 'Grouped' でも使える。
class Summarisable g where
  summarise :: [(Text, Agg)] -> g -> DF.DataFrame

instance Summarisable DF.DataFrame where
  summarise aggs df =
    let g = Group { gSize = nrows df
                  , gNum  = M.fromList (map (fmap V.toList) (numColumns df)) }
    in DF.fromNamedColumns [ (rn, buildCol [runA a g]) | (rn, a) <- aggs ]

instance Summarisable Grouped where
  summarise aggs (Grouped keys df) =
    let ncols = numColumns df
        grps  = [ (kt, mkGroup ncols idxs) | (kt, idxs) <- groupRows keys df ]
        keyCols = [ (kn, buildKeyCol [ kt !! j | (kt, _) <- grps ])
                  | (j, kn) <- zip [0 ..] keys ]
        resCols = [ (rn, buildCol [ runA a g | (_, g) <- grps ])
                  | (rn, a) <- aggs ]
    in DF.fromNamedColumns (keyCols ++ resCols)

runA :: Agg -> Group -> Cell
runA (Agg f) = f

-- | dplyr @mutate@ (ungrouped・元列を温存して新列を右端に足す)。
mutate :: [(Text, ColExpr)] -> DF.DataFrame -> DF.DataFrame
mutate exprs df =
  let g = Group { gSize = nrows df
                , gNum  = M.fromList (map (fmap V.toList) (numColumns df)) }
  in foldl (\d (nm, ColExpr f) -> DF.insertVector nm (V.fromList (f g)) d) df exprs
