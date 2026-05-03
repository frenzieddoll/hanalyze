{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Hackage @dataframe@ ベースのデータ前処理ヘルパ。
--
-- Phase 1 で独自 'DataFrame.Core' から Hackage 'DataFrame.DataFrame'
-- (本モジュールでは @DXD.DataFrame@) に全面切替された。
--
-- - 欠損値の検出 / 除去 / 補完 (mean / median / 定数)
-- - 列の選択 / 削除 / リネーム
-- - 行のフィルタリング
-- - 派生列の計算 (mapNumeric / deriveNumeric / deriveText)
-- - Text 列を数値化 (NA 除去 + parse)
--
-- すべて純粋に新しい 'DXD.DataFrame' を返す。
module DataIO.Preprocess
  ( -- * 値・行の表現
    Value (..)
  , DataRow
  , isVMissing
    -- * NA 検出
  , isNAString
  , defaultNAStrings
    -- * 列の選択 / 削除 / リネーム
  , selectColumns
  , dropColumns
  , renameColumn
    -- * 欠損値の処理
  , countMissing
  , dropMissingRows
  , imputeConstant
  , imputeMean
  , imputeMedian
  , parseNumericColumn
  , readMaybeDoubleColumn
    -- * 行フィルタ
  , rowsOf
  , filterRows
  , filterRowsByNumeric
    -- * 派生列
  , mapNumeric
  , deriveNumeric
  , deriveText
  , replaceColumn
  , addColumn
    -- * groupBy / aggregate
  , groupByAggregate
  , groupByMean
  , groupBySum
  , groupByMin
  , groupByMax
  , groupByMedian
  , groupByCount
    -- * Wide ↔ Long 変形 (Phase B/C — melt)
  , meltLonger
  ) where

import qualified DataFrame                    as DX
import qualified DataFrame.Internal.Column    as DXC
import qualified DataFrame.Internal.DataFrame as DXD
import qualified DataFrame.Internal.Types     as DXT

import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, try, evaluate)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

-- ---------------------------------------------------------------------------
-- 値 / 行の表現 (deriveNumeric/deriveText 用の述語インタフェース)
-- ---------------------------------------------------------------------------

data Value = VNum Double | VText Text | VMissing
  deriving (Show, Eq)

isVMissing :: Value -> Bool
isVMissing VMissing = True
isVMissing _        = False

type DataRow = Map.Map Text Value

-- ---------------------------------------------------------------------------
-- NA 検出 (Text レベル)
-- ---------------------------------------------------------------------------

defaultNAStrings :: [Text]
defaultNAStrings = ["", "NA", "N/A", "n/a", "null", "NULL", "NaN", "nan", "?"]

isNAString :: Text -> Bool
isNAString t = T.strip t `elem` defaultNAStrings

-- ---------------------------------------------------------------------------
-- 列の選択 / 削除 / リネーム
-- ---------------------------------------------------------------------------

selectColumns :: [Text] -> DXD.DataFrame -> DXD.DataFrame
selectColumns names df =
  let present = filter (`elem` DX.columnNames df) names
  in DX.select present df

dropColumns :: [Text] -> DXD.DataFrame -> DXD.DataFrame
dropColumns names df =
  let present = filter (`elem` DX.columnNames df) names
  in DX.exclude present df

renameColumn :: Text -> Text -> DXD.DataFrame -> DXD.DataFrame
renameColumn old new df
  | old `elem` DX.columnNames df = DX.rename old new df
  | otherwise                    = df

-- ---------------------------------------------------------------------------
-- 内部: 列値の安全な取得 (型不一致時 Nothing)
-- ---------------------------------------------------------------------------

-- | 列の長さ (列が無ければ 0)。
colLength :: Text -> DXD.DataFrame -> Int
colLength name df = case DXD.getColumn name df of
  Just c  -> DXC.columnLength c
  Nothing -> 0

-- | 「i 番目の要素が null か」。列が無ければ True。
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

-- | 列ごとの欠損数。null bitmap が無い列は 0、ある列は null 数を数える。
-- 加えて Text 列に含まれる NA 文字列も欠損として数える (CSV 由来互換)。
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

-- | 指定した列のいずれかが null である行を削除。
-- Text 列の NA 文字列もまとめて欠損として扱う。
dropMissingRows :: [Text] -> DXD.DataFrame -> DXD.DataFrame
dropMissingRows targets df =
  let n = if null cols then 0 else maximum (map (`colLength` df) cols)
      cols = targets
      rowMissing i =
        any (\c -> isNullAt c i df || isTextNA c i) cols
      isTextNA c i = case tryColumnAsList @Text c df of
        Just xs -> i < length xs && isNAString (xs !! i)
        Nothing -> False
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
    tryAs
      :: forall a. (DXC.Columnable a, NFData a,
                    DXC.ColumnifyRep (DXT.KindOf a) a)
      => Maybe DX.Column -> Maybe DX.Column
    tryAs fallback = case tryColumnAsList @a name df of
      Just xs -> Just (DX.fromList [ xs !! i | i <- idxs, i < length xs ])
      Nothing -> fallback

-- | 定数で欠損補完して Double 列に統一する。
imputeConstant :: Text -> Double -> DXD.DataFrame -> Maybe DXD.DataFrame
imputeConstant name fill df = case readMaybeDoubleColumn name df of
  Nothing -> Nothing
  Just xs ->
    let filled = map (maybe fill id) xs
    in Just (DX.insertColumn name (DX.fromList filled) df)

-- | 平均値で補完。非欠損が 0 件なら Nothing。
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

-- | 中央値で補完。非欠損が 0 件なら Nothing。
imputeMedian :: Text -> DXD.DataFrame -> Maybe DXD.DataFrame
imputeMedian name df = case readMaybeDoubleColumn name df of
  Nothing -> Nothing
  Just xs ->
    let s = sort [ x | Just x <- xs ]
    in if null s
         then Nothing
         else imputeConstant name (s !! (length s `div` 2)) df

-- | Text/Double/Maybe Double/Int/Maybe Int いずれの列でも @[Maybe Double]@
-- に正規化して取り出す。Text 列の NA 文字列・parse 失敗は Nothing として扱う。
--
-- 注意: Hackage 'DX.columnAsList' は @Maybe a@ 列に対して @col @a@ を要求しても
-- 例外を投げず、null セルを 0 などのデフォルト値で埋めて返す。そのため null は
-- 必ず 'isNullAt' (= columnElemIsNull) で別途マスクする。
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

-- | Text 列を Double 列に変換。NA / parse 不能セルがあれば Nothing。
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

-- | 行を 'DataRow' のリストに展開する。NA 文字列は VMissing。
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

filterRows :: (DataRow -> Bool) -> DXD.DataFrame -> DXD.DataFrame
filterRows p df =
  let keep = [ i | (i, r) <- zip [0..] (rowsOf df), p r ]
  in selectRows keep df

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

mapNumeric :: Text -> (Double -> Double) -> DXD.DataFrame -> DXD.DataFrame
mapNumeric name f df = case tryColumnAsList @Double name df of
  Just xs -> DX.insertColumn name (DX.fromList (map f xs)) df
  Nothing -> df

deriveNumeric :: Text -> (DataRow -> Double) -> DXD.DataFrame -> DXD.DataFrame
deriveNumeric newName f df =
  let vals = map f (rowsOf df)
  in DX.insertColumn newName (DX.fromList (vals :: [Double])) df

deriveText :: Text -> (DataRow -> Text) -> DXD.DataFrame -> DXD.DataFrame
deriveText newName f df =
  let vals = map f (rowsOf df)
  in DX.insertColumn newName (DX.fromList (vals :: [Text])) df

-- | 列の上書き / 追加 (Hackage 'DX.insertColumn' は既存列を置換する)。
replaceColumn :: Text -> DX.Column -> DXD.DataFrame -> DXD.DataFrame
replaceColumn = DX.insertColumn

addColumn :: Text -> DX.Column -> DXD.DataFrame -> DXD.DataFrame
addColumn = DX.insertColumn

-- ---------------------------------------------------------------------------
-- groupBy / aggregate
-- ---------------------------------------------------------------------------

-- | グループ列 (Text) ごとに数値列に集約関数を適用する。
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

groupByMean   :: Text -> Text -> DXD.DataFrame -> Maybe DXD.DataFrame
groupByMean g n = groupByAggregate g n meanD

groupBySum    :: Text -> Text -> DXD.DataFrame -> Maybe DXD.DataFrame
groupBySum g n = groupByAggregate g n sum

groupByMin    :: Text -> Text -> DXD.DataFrame -> Maybe DXD.DataFrame
groupByMin g n = groupByAggregate g n minimum

groupByMax    :: Text -> Text -> DXD.DataFrame -> Maybe DXD.DataFrame
groupByMax g n = groupByAggregate g n maximum

groupByMedian :: Text -> Text -> DXD.DataFrame -> Maybe DXD.DataFrame
groupByMedian g n = groupByAggregate g n medianD

-- | グループごとの行数。count 列名は @"count"@ 固定。
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
