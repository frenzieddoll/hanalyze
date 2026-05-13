{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Data-preprocessing helpers built on Hackage's @dataframe@.
--
-- All operations consume and produce 'DXD.DataFrame'.
--
--   * Missing-value detection, removal, and imputation
--     (mean / median / constant).
-- - 列の選択 / 削除 / リネーム
-- - 行のフィルタリング
-- - 派生列の計算 (mapNumeric / deriveNumeric / deriveText)
-- - Text 列を数値化 (NA 除去 + parse)
--
-- すべて純粋に新しい 'DXD.DataFrame' を返す。
module Hanalyze.DataIO.Preprocess
  ( -- * 値・行の表現
    Value (..)
  , DataRow
  , isVMissing
    -- * NA detection
  , isNAString
  , defaultNAStrings
    -- * Column select / drop / rename
  , selectColumns
  , dropColumns
  , renameColumn
    -- * Missing-value handling
  , countMissing
  , dropMissingRows
  , imputeConstant
  , imputeMean
  , imputeMedian
  , parseNumericColumn
  , readMaybeDoubleColumn
    -- * Row filters
  , rowsOf
  , filterRows
  , filterRowsByNumeric
    -- * Derived columns
  , mapNumeric
  , deriveNumeric
  , deriveText
  , replaceColumn
  , addColumn
    -- * groupBy and aggregate
  , groupByAggregate
  , groupByMean
  , groupBySum
  , groupByMin
  , groupByMax
  , groupByMedian
  , groupByCount
    -- * Wide ↔ long transformation (melt)
  , meltLonger
    -- * Long-form regrid (resample jagged data onto a common grid)
  , ZBoundsMode (..)
  , RegridOpts (..)
  , defaultRegridOpts
  , RegridResult (..)
  , PerIdStat (..)
  , regridLong
  ) where

import qualified DataFrame                    as DX
import qualified DataFrame.Internal.Column    as DXC
import qualified DataFrame.Internal.DataFrame as DXD
import qualified DataFrame.Internal.Types     as DXT

import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, try, evaluate)
import Data.List (sort)
import qualified Data.List
import qualified Data.Ord
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
import qualified Hanalyze.Stat.Interpolate
import qualified Hanalyze.Stat.AdaptiveGrid

-- ---------------------------------------------------------------------------
-- 値 / 行の表現 (deriveNumeric/deriveText 用の述語インタフェース)
-- ---------------------------------------------------------------------------

-- | A typed cell value used by 'deriveNumeric' / 'deriveText'-style
-- predicates. Missing values become 'VMissing'.
data Value = VNum Double | VText Text | VMissing
  deriving (Show, Eq)

-- | True for 'VMissing'; useful inside row predicates.
isVMissing :: Value -> Bool
isVMissing VMissing = True
isVMissing _        = False

-- | A single row keyed by column name.
type DataRow = Map.Map Text Value

-- ---------------------------------------------------------------------------
-- NA 検出 (Text レベル)
-- ---------------------------------------------------------------------------

-- | Strings recognised as missing values (case-sensitive on the trimmed
-- text): @\"\"@, @\"NA\"@, @\"N/A\"@, @\"n/a\"@, @\"null\"@, @\"NULL\"@,
-- @\"NaN\"@, @\"nan\"@, @\"?\"@.
defaultNAStrings :: [Text]
defaultNAStrings = ["", "NA", "N/A", "n/a", "null", "NULL", "NaN", "nan", "?"]

-- | True when the trimmed input text is in 'defaultNAStrings'.
isNAString :: Text -> Bool
isNAString t = T.strip t `elem` defaultNAStrings

-- ---------------------------------------------------------------------------
-- 列の選択 / 削除 / リネーム
-- ---------------------------------------------------------------------------

-- | Keep only the named columns (silently ignoring names that are not
-- present).
selectColumns :: [Text] -> DXD.DataFrame -> DXD.DataFrame
selectColumns names df =
  let present = filter (`elem` DX.columnNames df) names
  in DX.select present df

-- | Drop the named columns (silently ignoring names that are not present).
dropColumns :: [Text] -> DXD.DataFrame -> DXD.DataFrame
dropColumns names df =
  let present = filter (`elem` DX.columnNames df) names
  in DX.exclude present df

-- | Rename @old@ to @new@. No-op if @old@ is missing.
renameColumn :: Text -> Text -> DXD.DataFrame -> DXD.DataFrame
renameColumn old new df
  | old `elem` DX.columnNames df = DX.rename old new df
  | otherwise                    = df

-- ---------------------------------------------------------------------------
-- 内部: 列値の安全な取得 (型不一致時 Nothing)
-- ---------------------------------------------------------------------------

-- | Length of a named column (0 if absent).
colLength :: Text -> DXD.DataFrame -> Int
colLength name df = case DXD.getColumn name df of
  Just c  -> DXC.columnLength c
  Nothing -> 0

-- | Is the @i@-th cell null? Returns 'True' for missing columns.
isNullAt :: Text -> Int -> DXD.DataFrame -> Bool
isNullAt name i df = case DXD.getColumn name df of
  Just c  -> DXC.columnElemIsNull c i
  Nothing -> True

-- | 列を @[a]@ として安全に取り出す。型不一致や例外 (Hackage が
-- @error "fromMaybeVec: Nothing slot"@ 等を投げるケース) も 'Nothing' で吸収。
-- 'force' でリスト要素まで NF にしてから捕捉する。
tryColumnAsList
  :: forall a. (DXC.Columnable a, NFData a)
  => Text -> DXD.DataFrame -> Maybe [a]
tryColumnAsList name df = unsafePerformIO $ do
  r <- try (evaluate (force (DX.columnAsList (DX.col @a name) df)))
         :: IO (Either SomeException [a])
  return $ case r of
    Right xs -> Just xs
    Left _   -> Nothing

-- ---------------------------------------------------------------------------
-- 欠損値処理
-- ---------------------------------------------------------------------------

-- | Per-column missing count. Columns without a null bitmap contribute
-- 0; columns with a bitmap contribute their null count. Text columns
-- additionally count cells whose value is in 'defaultNAStrings' (for
-- CSV-source compatibility).
countMissing :: DXD.DataFrame -> [(Text, Int)]
countMissing df =
  [ (n, countOne n) | n <- DX.columnNames df ]
  where
    countOne n =
      let len    = colLength n df
          nulls  = length [ () | i <- [0 .. len - 1], isNullAt n i df ]
          texts  = case tryColumnAsList @Text n df of
                     Just xs -> length (filter isNAString xs)
                     Nothing -> 0
      in nulls + texts

-- | Drop rows where any of the listed columns is null. NA strings in
-- Text columns are also treated as missing.
--
-- Phase 11b (2026-05-14): cache per-column Text 'Vector' once instead
-- of calling 'tryColumnAsList' + @xs !! i@ inside the inner row loop.
-- The previous version was O(rows² × cols); the cached version is
-- O(rows × cols).
dropMissingRows :: [Text] -> DXD.DataFrame -> DXD.DataFrame
dropMissingRows targets df =
  let cols = targets
      n    = if null cols then 0 else maximum (map (`colLength` df) cols)
      -- One pass per column: Maybe (Vector Text) of NA-eligible entries.
      textCache :: [(Text, Maybe (V.Vector Text))]
      textCache =
        [ (c, fmap V.fromList (tryColumnAsList @Text c df))
        | c <- cols ]
      isTextNAVec mv i = case mv of
        Just v  -> i < V.length v && isNAString (V.unsafeIndex v i)
        Nothing -> False
      rowMissing i =
        any (\(c, mv) -> isNullAt c i df || isTextNAVec mv i) textCache
      keep = [ i | i <- [0 .. n - 1], not (rowMissing i) ]
  in selectRows keep df

-- | インデックス集合で全列を縦スライス。
selectRows :: [Int] -> DXD.DataFrame -> DXD.DataFrame
selectRows idxs df = foldr ins DX.empty (DX.columnNames df)
  where
    ins name acc =
      case sliceColumn name df idxs of
        Just c  -> DX.insertColumn name c acc
        Nothing -> acc

-- | 列を indices で取り出して新しい Column を作る。
-- BoxedColumn / UnboxedColumn のどちらでも columnAsList 経由で安全に処理する。
sliceColumn :: Text -> DXD.DataFrame -> [Int] -> Maybe DX.Column
sliceColumn name df idxs = case DXD.getColumn name df of
  Nothing -> Nothing
  Just _  ->
    -- 型を順に試す。Maybe Double → Double → Maybe Int → Int → Text の順。
    tryAs @(Maybe Double)
      (tryAs @Double
        (tryAs @(Maybe Int)
          (tryAs @Int
            (tryAs @Text Nothing))))
  where
    -- Phase 11b (2026-05-14): convert the column to a 'Vector' once and
    -- use 'unsafeIndex'. The previous @xs !! i@ in a list-comprehension
    -- was O(i) per index, so 'sliceColumn' on n indices was O(n²).
    tryAs
      :: forall a. (DXC.Columnable a, NFData a,
                    DXC.ColumnifyRep (DXT.KindOf a) a)
      => Maybe DX.Column -> Maybe DX.Column
    tryAs fallback = case tryColumnAsList @a name df of
      Just xs ->
        let v   = V.fromList xs
            len = V.length v
        in Just (DX.fromList
                   [ V.unsafeIndex v i | i <- idxs, i < len ])
      Nothing -> fallback

-- | Impute missing values with a constant and homogenize to a 'Double'
-- column.
imputeConstant :: Text -> Double -> DXD.DataFrame -> Maybe DXD.DataFrame
imputeConstant name fill df = case readMaybeDoubleColumn name df of
  Nothing -> Nothing
  Just xs ->
    let filled = map (maybe fill id) xs
    in Just (DX.insertColumn name (DX.fromList filled) df)

-- | Impute missing values with the mean of the present cells. Returns
-- 'Nothing' when the column has no non-missing cells.
imputeMean :: Text -> DXD.DataFrame -> Maybe DXD.DataFrame
imputeMean name df = case readMaybeDoubleColumn name df of
  Nothing -> Nothing
  Just xs ->
    let nums = [ x | Just x <- xs ]
    in if null nums
         then Nothing
         else
           let m = sum nums / fromIntegral (length nums)
           in imputeConstant name m df

-- | Impute missing values with the median. Returns 'Nothing' when the
-- column has no non-missing cells.
imputeMedian :: Text -> DXD.DataFrame -> Maybe DXD.DataFrame
imputeMedian name df = case readMaybeDoubleColumn name df of
  Nothing -> Nothing
  Just xs ->
    let s = sort [ x | Just x <- xs ]
    in if null s
         then Nothing
         else imputeConstant name (s !! (length s `div` 2)) df

-- | Read any of Text / Double / Maybe Double / Int / Maybe Int as
-- @[Maybe Double]@.
-- に正規化して取り出す。Text 列の NA 文字列・parse 失敗は Nothing として扱う。
--
-- 注意: Hackage 'DX.columnAsList' は @Maybe a@ 列に対して @col @a@ を要求しても
-- 例外を投げず、null セルを 0 などのデフォルト値で埋めて返す。そのため null は
-- 必ず @isNullAt@ (= columnElemIsNull) で別途マスクする。
readMaybeDoubleColumn :: Text -> DXD.DataFrame -> Maybe [Maybe Double]
readMaybeDoubleColumn name df = fmap (maskNulls . zip [0..]) raw
  where
    maskNulls = map (\(i, x) -> if isNullAt name i df then Nothing else x)
    raw =
      case tryColumnAsList @(Maybe Double) name df of
        Just xs -> Just xs
        Nothing -> case tryColumnAsList @(Maybe Int) name df of
          Just xs -> Just (map (fmap fromIntegral) xs)
          Nothing -> case tryColumnAsList @Double name df of
            Just xs -> Just (map Just xs)
            Nothing -> case tryColumnAsList @Int name df of
              Just xs -> Just (map (Just . fromIntegral) xs)
              Nothing -> case tryColumnAsList @Text name df of
                Just xs -> Just
                  [ if isNAString t
                      then Nothing
                      else readMaybe (T.unpack t)
                  | t <- xs ]
                Nothing -> Nothing

-- | Convert a Text column into a Double column. Returns 'Nothing' if
-- any cell is missing or fails to parse.
parseNumericColumn :: Text -> DXD.DataFrame -> Maybe DXD.DataFrame
parseNumericColumn name df =
  case tryColumnAsList @Double name df of
    Just _  -> Just df
    Nothing -> case tryColumnAsList @Text name df of
      Nothing -> Nothing
      Just xs -> do
        ds <- mapM (readMaybe . T.unpack) xs
        return (DX.insertColumn name (DX.fromList (ds :: [Double])) df)

-- ---------------------------------------------------------------------------
-- 行フィルタ (DataRow ベース、レガシー API)
-- ---------------------------------------------------------------------------

-- | Expand a DataFrame into a list of 'DataRow'. NA strings become
-- 'VMissing'.
rowsOf :: DXD.DataFrame -> [DataRow]
rowsOf df =
  let cols = DX.columnNames df
      n    = if null cols then 0 else maximum (map (`colLength` df) cols)
  in [ Map.fromList [ (c, cellAt c i) | c <- cols ] | i <- [0 .. n - 1] ]
  where
    cellAt c i
      | isNullAt c i df = VMissing
      | otherwise = case readMaybeDoubleColumn c df of
          Just xs | i < length xs ->
            case xs !! i of
              Just d  -> VNum d
              Nothing ->
                case tryColumnAsList @Text c df of
                  Just ts | i < length ts ->
                    let t = ts !! i
                    in if isNAString t then VMissing else VText t
                  _ -> VMissing
          _ -> case tryColumnAsList @Text c df of
                 Just ts | i < length ts ->
                   let t = ts !! i
                   in if isNAString t then VMissing else VText t
                 _ -> VMissing

-- | Keep only the rows for which the predicate evaluates to 'True'.
filterRows :: (DataRow -> Bool) -> DXD.DataFrame -> DXD.DataFrame
filterRows p df =
  let keep = [ i | (i, r) <- zip [0..] (rowsOf df), p r ]
  in selectRows keep df

-- | Keep only the rows for which a numeric column satisfies the
-- predicate.
filterRowsByNumeric :: Text -> (Double -> Bool) -> DXD.DataFrame -> DXD.DataFrame
filterRowsByNumeric name p df =
  case readMaybeDoubleColumn name df of
    Nothing -> df
    Just xs ->
      let keep = [ i | (i, Just x) <- zip [0..] xs, p x ]
      in selectRows keep df

-- ---------------------------------------------------------------------------
-- 派生列
-- ---------------------------------------------------------------------------

-- | Apply @f@ element-wise to a numeric column. The column is left
-- unchanged when its type is not @Double@.
mapNumeric :: Text -> (Double -> Double) -> DXD.DataFrame -> DXD.DataFrame
mapNumeric name f df = case tryColumnAsList @Double name df of
  Just xs -> DX.insertColumn name (DX.fromList (map f xs)) df
  Nothing -> df

-- | Derive a new numeric column from each row.
deriveNumeric :: Text -> (DataRow -> Double) -> DXD.DataFrame -> DXD.DataFrame
deriveNumeric newName f df =
  let vals = map f (rowsOf df)
  in DX.insertColumn newName (DX.fromList (vals :: [Double])) df

-- | Derive a new text column from each row.
deriveText :: Text -> (DataRow -> Text) -> DXD.DataFrame -> DXD.DataFrame
deriveText newName f df =
  let vals = map f (rowsOf df)
  in DX.insertColumn newName (DX.fromList (vals :: [Text])) df

-- | Replace or insert a column (Hackage's 'DX.insertColumn' replaces an
-- existing column).
replaceColumn :: Text -> DX.Column -> DXD.DataFrame -> DXD.DataFrame
replaceColumn = DX.insertColumn

-- | Append a new column (or replace if the name already exists).
addColumn :: Text -> DX.Column -> DXD.DataFrame -> DXD.DataFrame
addColumn = DX.insertColumn

-- ---------------------------------------------------------------------------
-- groupBy / aggregate
-- ---------------------------------------------------------------------------

-- | Aggregate a numeric column with the given function, grouped by a
-- text key column.
-- カスタム集約 (任意の @[Double] -> Double@) を扱うため、Hackage の
-- @groupBy + aggregate@ ではなく独自バケット実装。決まった集約は
-- 'groupByMean' 等を経由した方が高速。
groupByAggregate
  :: Text                          -- ^ グループ列
  -> Text                          -- ^ 集約対象列
  -> ([Double] -> Double)          -- ^ 集約関数
  -> DXD.DataFrame
  -> Maybe DXD.DataFrame
groupByAggregate gCol nCol agg df =
  case (tryColumnAsList @Text gCol df, readMaybeDoubleColumn nCol df) of
    (Just gs, Just nsM) ->
      let pairs = [ (g, x) | (g, Just x) <- zip gs nsM ]
          buckets = collectInOrder pairs
          groups   = map fst buckets
          aggVals  = map (agg . snd) buckets
      in Just $
           DX.insertColumn nCol (DX.fromList (aggVals :: [Double])) $
           DX.insertColumn gCol (DX.fromList (groups  :: [Text]))
             DX.empty
    _ -> Nothing

-- | 順序保持の group→[value] 蓄積。
collectInOrder :: Eq k => [(k, v)] -> [(k, [v])]
collectInOrder = foldl step []
  where
    step acc (k, v) = case lookup k acc of
      Just _  -> [ if k' == k then (k', vs ++ [v]) else (k', vs) | (k', vs) <- acc ]
      Nothing -> acc ++ [(k, [v])]

-- | Group-by aggregation with the per-group mean.
groupByMean   :: Text -> Text -> DXD.DataFrame -> Maybe DXD.DataFrame
groupByMean g n = groupByAggregate g n meanD

-- | Group-by aggregation with the per-group sum.
groupBySum    :: Text -> Text -> DXD.DataFrame -> Maybe DXD.DataFrame
groupBySum g n = groupByAggregate g n sum

-- | Group-by aggregation with the per-group minimum.
groupByMin    :: Text -> Text -> DXD.DataFrame -> Maybe DXD.DataFrame
groupByMin g n = groupByAggregate g n minimum

-- | Group-by aggregation with the per-group maximum.
groupByMax    :: Text -> Text -> DXD.DataFrame -> Maybe DXD.DataFrame
groupByMax g n = groupByAggregate g n maximum

-- | Group-by aggregation with the per-group median.
groupByMedian :: Text -> Text -> DXD.DataFrame -> Maybe DXD.DataFrame
groupByMedian g n = groupByAggregate g n medianD

-- | Per-group row count. The output column is named @\"count\"@.
groupByCount :: Text -> DXD.DataFrame -> Maybe DXD.DataFrame
groupByCount gCol df = case tryColumnAsList @Text gCol df of
  Nothing -> Nothing
  Just gs ->
    let buckets = collectInOrder [ (g, ()) | g <- gs ]
        keys    = map fst buckets
        counts  = map (fromIntegral . length . snd) buckets
    in Just $
         DX.insertColumn "count" (DX.fromList (counts :: [Double])) $
         DX.insertColumn gCol    (DX.fromList (keys   :: [Text]))
           DX.empty

meanD :: [Double] -> Double
meanD [] = 0
meanD xs = sum xs / fromIntegral (length xs)

medianD :: [Double] -> Double
medianD [] = 0
medianD xs = let s = sort xs in s !! (length s `div` 2)

-- ---------------------------------------------------------------------------
-- Wide → Long 変形 (melt / pivot_longer)
-- ---------------------------------------------------------------------------

-- | Wide-form の DataFrame を long-form に展開する (R/pandas の pivot_longer
-- / melt 相当)。
--
-- @meltLonger idCols valueCols varName valueName parseVarAsDouble df@:
--
-- * @idCols@         そのまま残す (繰返しコピー) 列。
-- * @valueCols@      縦方向に展開する列。これらの列名が新しい @varName@ 列の値になる。
-- * @varName@        新しい variable 列の名前 (例: \"t\")。
-- * @valueName@      新しい value 列の名前 (例: \"y\")。
-- * @parseVarAsDouble@
--                    True なら variable 列の中身 (= 元 wide 列名) を Double として
--                    parse して数値列に。Parse 失敗時は Text 列のまま。
--
-- 元セルが NA (null bitmap or NA 文字列) の行は出力から除外される。
--
-- 例:
--
-- @
-- name x1 1   2   3       --      name x1 t y
-- a    1  10  20  -    →          a    1  1 10
-- b    2  -   30  60              a    1  2 20
--                                 b    2  2 30
--                                 b    2  3 60
-- @
meltLonger
  :: [Text]      -- ^ id 列 (そのまま残す)
  -> [Text]      -- ^ wide 列 (縦展開する)
  -> Text        -- ^ 新しい variable 列名
  -> Text        -- ^ 新しい value 列名
  -> Bool        -- ^ True: variable 列を Double に parse
  -> DXD.DataFrame
  -> DXD.DataFrame
meltLonger idCols valueCols varName valueName parseVar df =
  let nrows = fst (DX.dimensions df)
      -- 各 id 列を [Maybe Text] / [Maybe Double] として取り出す
      idTexts =
        [ (n, idColAsText n) | n <- idCols ]
      -- id 列を [Text] として取り出す: Maybe Text → Text → Maybe Double → Double → Maybe Int → Int の順に試行
      idColAsText n =
        case tryColumnAsList @(Maybe Text) n df of
          Just xs -> Just (map (maybe "" id) xs)
          Nothing -> case tryColumnAsList @Text n df of
            Just xs -> Just xs
            Nothing -> case tryColumnAsList @(Maybe Double) n df of
              Just xs -> Just (map showMaybeD xs)
              Nothing -> case tryColumnAsList @Double n df of
                Just xs -> Just (map showD xs)
                Nothing -> case tryColumnAsList @(Maybe Int) n df of
                  Just xs -> Just (map showMaybeI xs)
                  Nothing -> case tryColumnAsList @Int n df of
                    Just xs -> Just (map (T.pack . show) xs)
                    Nothing -> Nothing
      showMaybeD Nothing  = ""
      showMaybeD (Just d) = showD d
      showD d
        | d == fromInteger (round d) = T.pack (show (round d :: Integer))
        | otherwise                   = T.pack (show d)
      showMaybeI Nothing  = ""
      showMaybeI (Just i) = T.pack (show i)
      -- valueCols のセル値を [Maybe Double] で取り出す
      valData =
        [ (n, valueAsMaybeDouble n df, varValue n)
        | n <- valueCols ]
      varValue n
        | parseVar  = case readMaybe (T.unpack n) :: Maybe Double of
                        Just d  -> Right d
                        Nothing -> Left n
        | otherwise = Left n
      -- 全 (id行 × value列) ペアから NA でない物だけ残す
      indices = [(i, j) | i <- [0 .. nrows - 1], j <- [0 .. length valueCols - 1]]
      keep = [ (i, j, v)
             | (i, j) <- indices
             , let (_, vs, _) = valData !! j
             , Just v <- [vs !! i]
             ]
      -- id 列を keep 行数ぶん展開
      mkIdCol (n, mxs) =
        let xs = case mxs of
                   Just xs0 -> xs0
                   Nothing  -> replicate nrows ""
            ys = [ xs !! i | (i, _, _) <- keep ]
        in (n, ys)
      -- variable 列
      varValues = [ thd ((valData !! j)) | (_, j, _) <- keep ]
        where thd (_,_,c) = c
      -- value 列 (Double)
      valValues = [ v | (_, _, v) <- keep ]
      idColsOut = map mkIdCol idTexts
      df0       = foldl insertText DX.empty idColsOut
      df1       = case (parseVar, sequence (map varEither varValues)) of
                    (True, Just ds) ->
                      DX.insertColumn varName (DX.fromList (ds :: [Double])) df0
                    _               ->
                      let texts = map (either id (T.pack . show)) varValues
                      in DX.insertColumn varName (DX.fromList texts) df0
      df2       = DX.insertColumn valueName (DX.fromList (valValues :: [Double])) df1
  in df2
  where
    insertText d (n, xs) = DX.insertColumn n (DX.fromList (xs :: [Text])) d
    varEither (Right d) = Just d
    varEither (Left  _) = Nothing
    showCell Nothing  = ""
    showCell (Just t) = t

-- | 列を 'Maybe Double' のリストとして取り出すヘルパ (内部用)。
-- 数値 / Maybe Double / Int / Maybe Int / Text 列のいずれでも対応。
valueAsMaybeDouble :: Text -> DXD.DataFrame -> [Maybe Double]
valueAsMaybeDouble name df = case readMaybeDoubleColumn name df of
  Just xs -> xs
  Nothing -> replicate (fst (DX.dimensions df)) Nothing

-- ---------------------------------------------------------------------------
-- Long-form regrid (Phase G3): 歯抜けの long-form データを共通 grid に揃える
-- ---------------------------------------------------------------------------

-- | 共通 z 範囲の決定方式。
data ZBoundsMode
  = ZIntersection  -- ^ 全 id で観測がある区間: (max_id min_z, min_id max_z) — 外挿なし
  | ZUnion         -- ^ 全 id をカバー: (min_id min_z, max_id max_z) — 外挿あり
  deriving (Show, Eq)

-- | 'regridLong' の設定。
data RegridOpts = RegridOpts
  { roInterp     :: !Hanalyze.Stat.Interpolate.InterpKind
  , roGridKind   :: !Hanalyze.Stat.AdaptiveGrid.GridKind
  , roN          :: !Int
  , roZBoundsMode :: !ZBoundsMode
  , roCoarseN    :: !Int     -- ^ adaptive 用粗 grid サイズ (default 200)
  , roEpsRatio   :: !Double  -- ^ adaptive 用平坦部最低密度比 (default 0.05)
  } deriving (Show, Eq)

-- | 推奨デフォルト (PCHIP / Adaptive / N=30 / Intersection / coarse=200 / ε=0.05)。
defaultRegridOpts :: RegridOpts
defaultRegridOpts = RegridOpts
  { roInterp      = Hanalyze.Stat.Interpolate.PCHIP
  , roGridKind    = Hanalyze.Stat.AdaptiveGrid.Adaptive
  , roN           = 30
  , roZBoundsMode = ZIntersection
  , roCoarseN     = 200
  , roEpsRatio    = 0.05
  }

-- | id ごとの統計 (G4 のレポートで使用)。
data PerIdStat = PerIdStat
  { piId          :: !Text
  , piNObserved   :: !Int        -- ^ 元観測点数
  , piZMin        :: !Double     -- ^ 観測 z 最小
  , piZMax        :: !Double     -- ^ 観測 z 最大
  , piExtrapBelow :: !Double     -- ^ 共通 grid zmin が観測 zmin より小さい量 (>0 なら外挿)
  , piExtrapAbove :: !Double     -- ^ 共通 grid zmax が観測 zmax より大きい量 (>0 なら外挿)
  , piResidualMax :: !Double     -- ^ 補間関数を観測 z に再投入したときの最大残差
  } deriving (Show, Eq)

-- | regridLong の戻り値。data + レポート用統計。
data RegridResult = RegridResult
  { rrDataFrame   :: !DXD.DataFrame
  , rrZGrid       :: ![Double]
  , rrZMin        :: !Double
  , rrZMax        :: !Double
  , rrPerIdStats  :: ![PerIdStat]
  , rrIds         :: ![Text]
  , rrPerIdInterp :: ![(Text, [(Double, Double)], Double -> Double)]
                       -- ^ id ごとに (id, 元観測点, 補間関数)。レポートのオーバーレイ用
  , rrDensity     :: ![(Double, Double)]   -- ^ adaptive 時の (z, density) ペア (空: uniform 時)
  }

-- | 歯抜けの long-form @[idCol, zCol, yCol]@ を共通 grid に揃える。
--
-- 1. idCol で groupBy → id ごとに (z, y) ペア取得 (NA は除外)
-- 2. ZBoundsMode に従って共通 (zmin, zmax) を決定
-- 3. 'Hanalyze.Stat.AdaptiveGrid.makeGrid' で N 点 grid を生成
-- 4. 各 id を 'Hanalyze.Stat.Interpolate.interp1d' で補間し grid 上で評価
-- 5. id × grid の long-form DataFrame を返す
--
-- 観測点が < 2 の id は補間できないため除外され、レポートに記録される。
regridLong
  :: Text          -- ^ id 列名
  -> Text          -- ^ z 列名
  -> Text          -- ^ y 列名
  -> RegridOpts
  -> DXD.DataFrame
  -> RegridResult
regridLong idCol zCol yCol opts df =
  let -- 列を取り出す
      ids   = case tryColumnAsList @Text idCol df of
                Just xs -> xs
                Nothing -> case tryColumnAsList @(Maybe Text) idCol df of
                  Just xs -> map (maybe "" id) xs
                  Nothing -> case tryColumnAsList @Double idCol df of
                    Just xs -> map (T.pack . show) xs
                    Nothing -> case tryColumnAsList @Int idCol df of
                      Just xs -> map (T.pack . show) xs
                      Nothing -> []
      zs    = valueAsMaybeDouble zCol df
      ys    = valueAsMaybeDouble yCol df
      -- (id, [(z, y)]) にグループ化、NA 行は除外
      triples = [ (i, z, y)
                | (i, mz, my) <- zip3 ids zs ys
                , Just z <- [mz]
                , Just y <- [my] ]
      grouped =
        let m = foldl (\acc (i, z, y) -> Map.insertWith (++) i [(z, y)] acc)
                      Map.empty triples
        in [ (i, sortBy (Data.Ord.comparing fst) pts)
           | (i, pts) <- Map.toList m
           , length pts >= 2 ]
      idsKept = map fst grouped
      perIdPts = map snd grouped
      -- z 範囲
      ranges = [ (minimum (map fst pts), maximum (map fst pts)) | pts <- perIdPts ]
      (zmin, zmax) = case roZBoundsMode opts of
        ZIntersection ->
          if null ranges
            then (0, 1)
            else (maximum (map fst ranges), minimum (map snd ranges))
        ZUnion        ->
          if null ranges
            then (0, 1)
            else (minimum (map fst ranges), maximum (map snd ranges))
      -- 共通 grid
      gridSpec = Hanalyze.Stat.AdaptiveGrid.GridSpec
        { Hanalyze.Stat.AdaptiveGrid.gsKind       = roGridKind opts
        , Hanalyze.Stat.AdaptiveGrid.gsN          = roN opts
        , Hanalyze.Stat.AdaptiveGrid.gsInterpKind = roInterp opts
        , Hanalyze.Stat.AdaptiveGrid.gsCoarseN    = roCoarseN opts
        , Hanalyze.Stat.AdaptiveGrid.gsEpsRatio   = roEpsRatio opts
        }
      grid = Hanalyze.Stat.AdaptiveGrid.makeGrid perIdPts (zmin, zmax) gridSpec
      -- id ごとに補間関数 + grid 上の y を評価
      interpFns = [ (i, pts, Hanalyze.Stat.Interpolate.interp1d (roInterp opts) pts)
                  | (i, pts) <- grouped ]
      perIdY    = [ map f grid | (_, _, f) <- interpFns ]
      -- 統計
      stats = [ let zMn = fst rg
                    zMx = snd rg
                    extL = max 0 (zMn - zmin)
                    extU = max 0 (zmax - zMx)
                    residMax = if null pts then 0
                               else maximum [ abs (f z - y) | (z, y) <- pts ]
                in PerIdStat
                     { piId          = i
                     , piNObserved   = length pts
                     , piZMin        = zMn
                     , piZMax        = zMx
                     , piExtrapBelow = extL
                     , piExtrapAbove = extU
                     , piResidualMax = residMax
                     }
              | ((i, pts, f), rg) <- zip interpFns ranges
              ]
      -- 出力 long DataFrame: 行数 = nIds × len grid
      n = length grid
      idsOut    = concat [ replicate n i | i <- idsKept ]
      zsOut     = concat (replicate (length idsKept) grid)
      ysOut     = concat perIdY
      dfOut     = DX.insertColumn yCol  (DX.fromList ysOut)
                $ DX.insertColumn zCol  (DX.fromList zsOut)
                $ DX.insertColumn idCol (DX.fromList idsOut)
                $ DX.empty
      -- adaptive density (レポート用): coarse grid 上の (z, density)
      density = case roGridKind opts of
        Hanalyze.Stat.AdaptiveGrid.Uniform  -> []
        Hanalyze.Stat.AdaptiveGrid.Adaptive -> computeDensity perIdPts (roInterp opts)
                                                    (roCoarseN opts) zmin zmax
  in RegridResult
       { rrDataFrame   = dfOut
       , rrZGrid       = grid
       , rrZMin        = zmin
       , rrZMax        = zmax
       , rrPerIdStats  = stats
       , rrIds         = idsKept
       , rrPerIdInterp = interpFns
       , rrDensity     = density
       }
  where
    sortBy = Data.List.sortBy

-- | 内部: adaptive レポート用の (z, max_id |dy/dz|) 列を再計算 (G4 の R3 で表示)。
computeDensity
  :: [[(Double, Double)]] -> Hanalyze.Stat.Interpolate.InterpKind -> Int
  -> Double -> Double
  -> [(Double, Double)]
computeDensity perIdPts kind coarseN zmin zmax =
  let coarse = Hanalyze.Stat.AdaptiveGrid.uniformGrid coarseN zmin zmax
      ysPerId = [ map (Hanalyze.Stat.Interpolate.interp1d kind pts) coarse
                | pts <- perIdPts, length pts >= 2 ]
      slopeAbsLocal zs ys =
        let n = length zs
            zarr = zs
            yarr = ys
        in [ if n < 2 then 0
             else if i == 0 then abs ((yarr !! 1 - yarr !! 0) /
                                      (zarr !! 1 - zarr !! 0))
             else if i == n - 1 then abs ((yarr !! (n-1) - yarr !! (n-2)) /
                                          (zarr !! (n-1) - zarr !! (n-2)))
             else abs ((yarr !! (i+1) - yarr !! (i-1)) /
                       (zarr !! (i+1) - zarr !! (i-1)))
           | i <- [0 .. n-1] ]
      slopes = map (slopeAbsLocal coarse) ysPerId
      peak = if null slopes
               then replicate coarseN 0
               else [ maximum [ s !! i | s <- slopes ] | i <- [0 .. coarseN - 1] ]
  in zip coarse peak

-- 既存モジュールでの import 追加
-- (tryColumnAsList などは元々 import 済み、Map.insertWith / Data.List.sortBy /
--  Data.Ord.comparing も import 済み)

-- 補助 import (修飾名で参照するため)
{-# NOINLINE _placeholderRegridImports #-}
_placeholderRegridImports :: ()
_placeholderRegridImports = ()
