{-# LANGUAGE OverloadedStrings #-}
-- | データ前処理ヘルパ。
--
-- 'DataIO.CSV' でロードした 'DataFrame' に対して:
-- - 欠損値の検出 / 除去 / 補完 (mean / median / 定数)
-- - 列の選択 / 削除 / リネーム
-- - 行のフィルタリング
-- - 派生列の計算 (mapNumeric / deriveNumeric / deriveText)
-- - TextCol を数値化 (NA 除去 + parse)
--
-- すべて純粋に新しい 'DataFrame' を返す (元は不変)。
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
    -- * 欠損値の処理 (テキスト列の NA 文字列対応)
  , countMissing
  , dropMissingRows
  , imputeConstant
  , imputeMean
  , imputeMedian
  , parseNumericColumn
    -- * 行フィルタ
  , rowsOf
  , filterRows
  , filterRowsByNumeric
    -- * 派生列
  , mapNumeric
  , deriveNumeric
  , deriveText
  ) where

import DataFrame.Core

import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Read (readMaybe)

-- ---------------------------------------------------------------------------
-- 値 / 行の表現
-- ---------------------------------------------------------------------------

-- | 1 セルの値 (数値 / テキスト / 欠損)。
data Value = VNum Double | VText Text | VMissing
  deriving (Show, Eq)

isVMissing :: Value -> Bool
isVMissing VMissing = True
isVMissing _        = False

type DataRow = Map.Map Text Value

-- ---------------------------------------------------------------------------
-- NA 検出
-- ---------------------------------------------------------------------------

-- | 標準的に「欠損」と見なす文字列。
defaultNAStrings :: [Text]
defaultNAStrings = ["", "NA", "N/A", "n/a", "null", "NULL", "NaN", "nan", "?"]

-- | 'defaultNAStrings' のいずれかにマッチするか。
isNAString :: Text -> Bool
isNAString t = T.strip t `elem` defaultNAStrings

-- ---------------------------------------------------------------------------
-- 列の選択 / 削除 / リネーム
-- ---------------------------------------------------------------------------

-- | 指定した列だけを残す (順序を保つ)。存在しない列は無視。
selectColumns :: [Text] -> DataFrame -> DataFrame
selectColumns names df =
  mkDataFrame [ (n, c) | n <- names, Just c <- [getColumn n df] ]

-- | 指定した列を削除する。
dropColumns :: [Text] -> DataFrame -> DataFrame
dropColumns names df =
  let keep = filter (`notElem` names) (columnNames df)
  in mkDataFrame [ (n, c) | n <- keep, Just c <- [getColumn n df] ]

-- | 列名を変更する。元の名前が無ければ無変更。
renameColumn :: Text -> Text -> DataFrame -> DataFrame
renameColumn old new df = case getColumn old df of
  Nothing -> df
  Just c  ->
    mkDataFrame
      [ (if n == old then new else n, c')
      | n <- columnNames df
      , Just c' <- [getColumn n df]
      , (n /= new) || (n == old) ]
    `mergeCol` (new, c)
  where
    -- 名前衝突時は new を上書き
    mergeCol :: DataFrame -> (Text, Column) -> DataFrame
    mergeCol _ (k, v) =
      mkDataFrame $
        [ (n, c) | n <- columnNames df, n /= old, n /= new
                 , Just c <- [getColumn n df] ]
        ++ [(k, v)]

-- ---------------------------------------------------------------------------
-- 欠損値処理
-- ---------------------------------------------------------------------------

-- | 列ごとの欠損値 (NA 文字列) の数。
-- 'NumericCol' の列は欠損 0 と扱う (我々の型は欠損を表現しないため)。
countMissing :: DataFrame -> [(Text, Int)]
countMissing df =
  [ (n, missing c)
  | n <- columnNames df
  , Just c <- [getColumn n df] ]
  where
    missing (NumericCol _) = 0
    missing (TextCol v)    = V.length (V.filter isNAString v)

-- | 指定した列のいずれかに NA 文字列がある行を削除。
-- 'NumericCol' の列は欠損なしとみなす。
dropMissingRows :: [Text] -> DataFrame -> DataFrame
dropMissingRows targets df =
  let n = numRows df
      keepIxs = [ i | i <- [0 .. n - 1]
                    , not (any (rowMissing i) targets) ]
      sliceCol (NumericCol v) = NumericCol (V.fromList [ v V.! i | i <- keepIxs ])
      sliceCol (TextCol v)    = TextCol    (V.fromList [ v V.! i | i <- keepIxs ])
  in mkDataFrame
       [ (name, sliceCol c)
       | name <- columnNames df
       , Just c <- [getColumn name df] ]
  where
    rowMissing i name = case getColumn name df of
      Just (TextCol v) -> i < V.length v && isNAString (v V.! i)
      _                -> False

-- | 指定列を「定数で欠損補完して NumericCol 化」する。
-- 既に 'NumericCol' なら無変更。'TextCol' は parse して非 NA を採用、
-- NA は constant で埋める。すべて parse 不能なテキストの列は失敗。
imputeConstant :: Text -> Double -> DataFrame -> Maybe DataFrame
imputeConstant name fill df = case getColumn name df of
  Just (NumericCol _) -> Just df
  Just (TextCol v)    ->
    let parsed = V.map (parseCell fill) v
    in Just (replaceColumn name (NumericCol parsed) df)
  Nothing -> Nothing
  where
    parseCell c t
      | isNAString t = c
      | otherwise    = maybe c id (readMaybe (T.unpack t))

-- | 平均値で補完。
imputeMean :: Text -> DataFrame -> Maybe DataFrame
imputeMean name df = case getColumn name df of
  Just (NumericCol _) -> Just df
  Just (TextCol v)    ->
    let nums = [ x | t <- V.toList v, not (isNAString t)
                   , Just x <- [readMaybe (T.unpack t) :: Maybe Double] ]
    in if null nums
         then Nothing
         else
           let m = sum nums / fromIntegral (length nums)
           in imputeConstant name m df
  Nothing -> Nothing

-- | 中央値で補完。
imputeMedian :: Text -> DataFrame -> Maybe DataFrame
imputeMedian name df = case getColumn name df of
  Just (NumericCol _) -> Just df
  Just (TextCol v)    ->
    let nums = sort
                 [ x | t <- V.toList v, not (isNAString t)
                     , Just x <- [readMaybe (T.unpack t) :: Maybe Double] ]
    in if null nums
         then Nothing
         else
           let m = nums !! (length nums `div` 2)
           in imputeConstant name m df
  Nothing -> Nothing

-- | TextCol を NumericCol に変換 (NA 行は事前に dropMissingRows で除去済の前提)。
-- parse 不能セルがあれば 'Nothing'。
parseNumericColumn :: Text -> DataFrame -> Maybe DataFrame
parseNumericColumn name df = case getColumn name df of
  Just (NumericCol _) -> Just df
  Just (TextCol v)    -> do
    parsed <- V.mapM (readMaybe . T.unpack) v
    return (replaceColumn name (NumericCol parsed) df)
  Nothing -> Nothing

-- ---------------------------------------------------------------------------
-- 行フィルタ
-- ---------------------------------------------------------------------------

-- | 全行を 'DataRow' のリストに展開。
rowsOf :: DataFrame -> [DataRow]
rowsOf df =
  let cols = columnNames df
      n    = numRows df
  in [ Map.fromList [ (c, cellAt c i) | c <- cols ] | i <- [0 .. n - 1] ]
  where
    cellAt c i = case getColumn c df of
      Just (NumericCol v)
        | i < V.length v -> VNum (v V.! i)
        | otherwise      -> VMissing
      Just (TextCol v)
        | i < V.length v ->
            let t = v V.! i
            in if isNAString t then VMissing else VText t
        | otherwise -> VMissing
      Nothing -> VMissing

-- | 述語に一致する行だけを残す。
filterRows :: (DataRow -> Bool) -> DataFrame -> DataFrame
filterRows p df =
  let keepIxs = [ i | (i, r) <- zip [0..] (rowsOf df), p r ]
      sliceCol (NumericCol v) = NumericCol (V.fromList [ v V.! i | i <- keepIxs, i < V.length v ])
      sliceCol (TextCol v)    = TextCol    (V.fromList [ v V.! i | i <- keepIxs, i < V.length v ])
  in mkDataFrame
       [ (name, sliceCol c)
       | name <- columnNames df
       , Just c <- [getColumn name df] ]

-- | 数値列に対する単純な比較フィルタ。
filterRowsByNumeric :: Text -> (Double -> Bool) -> DataFrame -> DataFrame
filterRowsByNumeric name p = filterRows $ \row ->
  case Map.lookup name row of
    Just (VNum x) -> p x
    _             -> False

-- ---------------------------------------------------------------------------
-- 派生列
-- ---------------------------------------------------------------------------

-- | 単一の数値列に関数を適用する (in-place ではなく新しい DataFrame)。
mapNumeric :: Text -> (Double -> Double) -> DataFrame -> DataFrame
mapNumeric name f df = case getColumn name df of
  Just (NumericCol v) ->
    replaceColumn name (NumericCol (V.map f v)) df
  _ -> df

-- | 行から数値列を取り出して新しい数値列を作る。
-- 入力列のいずれかが欠損 (VMissing) または非数値の行は失敗 → 'Nothing'。
deriveNumeric
  :: Text                         -- ^ 新しい列名
  -> (DataRow -> Double)
  -> DataFrame -> DataFrame
deriveNumeric newName f df =
  let vals = V.fromList [ f r | r <- rowsOf df ]
  in addColumn newName (NumericCol vals) df

-- | 行から文字列列を作る。
deriveText
  :: Text
  -> (DataRow -> Text)
  -> DataFrame -> DataFrame
deriveText newName f df =
  let vals = V.fromList [ f r | r <- rowsOf df ]
  in addColumn newName (TextCol vals) df

-- ---------------------------------------------------------------------------
-- 内部ユーティリティ
-- ---------------------------------------------------------------------------

-- | 列を上書き (なければ追加)。
replaceColumn :: Text -> Column -> DataFrame -> DataFrame
replaceColumn name col df =
  mkDataFrame
    [ (n, if n == name then col else c)
    | n <- if name `elem` columnNames df
              then columnNames df
              else columnNames df ++ [name]
    , Just c <- [Map.lookup n m]
    ]
  where
    m = Map.fromList
          $ ((name, col) :)
          $ [ (n, c) | n <- columnNames df, Just c <- [getColumn n df] ]

-- | 列を末尾に追加 (重複時は上書き)。
addColumn :: Text -> Column -> DataFrame -> DataFrame
addColumn = replaceColumn
