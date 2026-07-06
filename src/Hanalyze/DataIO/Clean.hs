{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- | Column-level cleaning DSL.
--
-- Health checks ('Hanalyze.DataIO.Health') only emit warnings for columns
-- containing currency symbols, thousands separators, units, or alternate
-- decimal points. This module turns each warning into an explicit rule
-- ('ColumnRule') that converts the column into a numeric column.
--
-- Design notes:
--
--   * Each rule has the shape "extract a Text column → transform →
--     write back into the DataFrame", and always returns a transformation
--     log (@LogReport@).
--   * Cells that fail to convert are stored as 'Nothing' (the null
--     bitmap). The number of failures is recorded as I100-series Info
--     codes in the log.
--   * 'cleanPipeline' applies multiple rules in sequence.
-- * Phase B の自動推論との二段構え: sniff で読み込みは通るがセル値が
--   text のままになる #08 / #16 を、Clean で数値化して回帰可能にする。
--
-- 主要ルール
--
-- * 'StripUnits'      末尾の英字を取り除いて Double 化 (\"12.3kg\" → 12.3)
-- * 'ParseCurrency'   通貨記号 / 桁区切り (@$@/@¥@/@€@/@,@) を除去して数値化
-- * 'ParseDecimalEU'  decimal separator が ',' (EU style) のセルを Double 化
-- * 'TrimText'        前後の空白を除く
-- * 'CoerceNumeric'   上記 3 種を順に試して最初に成功した変換を採用
-- * @DedupeColumns@   重複列名に @_2@ などのサフィックスを付ける
-- * @FillBlankNames@  空列名を @col0@ 等で埋める
module Hanalyze.DataIO.Clean
  ( -- * 型
    ColumnRule (..)
    -- * Single-rule operators
  , applyRule
  , stripUnitsCol
  , parseCurrencyCol
  , parseDecimalEUCol
  , trimTextCol
  , coerceNumericCol
    -- * Pipeline
  , cleanPipeline
    -- * DataFrame-level operations
  , dedupeColumns
  , fillBlankNames
  ) where

import qualified DataFrame.Internal.Column    as DX
import qualified DataFrame.Internal.DataFrame  as DX
import qualified DataFrame.Operations.Core     as DX
import qualified DataFrame.Internal.DataFrame as DXD
import qualified DataFrame.Internal.Column    as DXC

import Data.Char (isAlpha, isDigit)
import qualified Data.Map.Strict      as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Read (readMaybe)

import Hanalyze.DataIO.Convert (getMaybeTextVec)
import Hanalyze.DataIO.Log     (LogReport, mkInfo, mkWarn, logReport, noLog)

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | A single column-cleaning rule.
data ColumnRule
  = StripUnits     -- ^ Strip trailing alphabetic suffix and parse
                   --   (@\"12.3kg\" → 12.3@).
  | ParseCurrency  -- ^ Parse currency-like strings such as @\"$1,234.56\"@
                   --   or @\"¥10,000\"@ into 'Double'.
  | ParseDecimalEU -- ^ Decimal point as @\",\"@ (@\"3,14\" → 3.14@).
  | TrimText       -- ^ Strip surrounding whitespace; column stays as 'Text'.
  | CoerceNumeric  -- ^ Try @StripUnits@, then @ParseCurrency@, then
                   --   @ParseDecimalEU@ in that order.
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- 個別ルール (列名指定)
-- ---------------------------------------------------------------------------

-- | Apply a single 'ColumnRule' to a single named column.
applyRule :: ColumnRule -> Text -> DXD.DataFrame -> (DXD.DataFrame, LogReport)
applyRule r name df = case r of
  StripUnits     -> stripUnitsCol     name df
  ParseCurrency  -> parseCurrencyCol  name df
  ParseDecimalEU -> parseDecimalEUCol name df
  TrimText       -> trimTextCol       name df
  CoerceNumeric  -> coerceNumericCol  name df

-- | Drop a trailing alphabetic unit suffix and parse the prefix as
-- 'Double' (e.g. @\"12.3kg\"@, @\"11.5cm\"@).
stripUnitsCol :: Text -> DXD.DataFrame -> (DXD.DataFrame, LogReport)
stripUnitsCol = liftCellRule "I100" "stripUnits" $ \t ->
  let s = T.strip t
      (digits, rest) = T.span (\c -> isDigit c || c == '.' || c == '-') s
      suffix = T.takeWhile isAlpha rest
  in if T.null digits
       then Nothing
       else if T.null suffix
              then readMaybe (T.unpack digits)
              else readMaybe (T.unpack digits)

-- | Parse a currency-formatted string (with currency symbol and
-- thousands separators) into 'Double': @\"$1,234.56\" → 1234.56@.
parseCurrencyCol :: Text -> DXD.DataFrame -> (DXD.DataFrame, LogReport)
parseCurrencyCol = liftCellRule "I101" "parseCurrency" $ \t ->
  let s1 = T.strip t
      s2 = T.dropWhile (`elem` ("$¥€£" :: String)) s1
      s3 = T.replace "," "" s2
  in readMaybe (T.unpack s3)

-- | EU-style decimal separator @\",\"@: @\"3,14\" → 3.14@.
parseDecimalEUCol :: Text -> DXD.DataFrame -> (DXD.DataFrame, LogReport)
parseDecimalEUCol = liftCellRule "I102" "parseDecimalEU" $ \t ->
  let s = T.replace "," "." (T.strip t)
  in readMaybe (T.unpack s)

-- | Strip surrounding whitespace and write back as a 'Text' column.
trimTextCol :: Text -> DXD.DataFrame -> (DXD.DataFrame, LogReport)
trimTextCol name df = case getMaybeTextVec name df of
  Nothing -> (df, logReport (mkWarn "I103W"
                  ("trimText: 列 '" <> name <> "' を text として取り出せません。")
                  Nothing))
  Just v  ->
    let trimmed = V.map (fmap T.strip) v
        out     = V.toList trimmed
    in ( DX.insertColumn name (DX.fromList (out :: [Maybe Text])) df
       , logReport (mkInfo "I103" ("trimText 適用: 列 '" <> name <> "'") Nothing))

-- | Catch-all numeric coercion: try @StripUnits@, then
-- @ParseCurrency@, then @ParseDecimalEU@ in order. The first successful
-- conversion wins; a cell that fails every rule is stored as a null
-- (via the null bitmap).
coerceNumericCol :: Text -> DXD.DataFrame -> (DXD.DataFrame, LogReport)
coerceNumericCol = liftCellRule "I104" "coerceNumeric" $ \t ->
  let candidates =
        [ \s -> let s' = T.strip s
                in readMaybe (T.unpack s') :: Maybe Double
        , \s -> -- StripUnits 風
            let s' = T.strip s
                (digits, _) = T.span (\c -> isDigit c || c == '.' || c == '-') s'
            in if T.null digits then Nothing
               else readMaybe (T.unpack digits)
        , \s -> -- ParseCurrency 風
            let s1 = T.strip s
                s2 = T.dropWhile (`elem` ("$¥€£" :: String)) s1
                s3 = T.replace "," "" s2
            in readMaybe (T.unpack s3)
        , \s -> -- ParseDecimalEU 風
            let s' = T.replace "," "." (T.strip s)
            in readMaybe (T.unpack s')
        ]
      tryAll [] = Nothing
      tryAll (f : fs) = case f t of
        Just x  -> Just x
        Nothing -> tryAll fs
  in tryAll candidates

-- ---------------------------------------------------------------------------
-- 共通ヘルパ: text → Maybe Double 変換を 1 列に適用
-- ---------------------------------------------------------------------------

-- | Helper: apply an arbitrary @text → 'Maybe Double'@ converter to a
-- single column. If the column cannot be read as Text, the DataFrame
-- is returned unchanged with a warning log entry.
liftCellRule
  :: Text                           -- ^ Info code.
  -> Text                           -- ^ Rule name (for the log).
  -> (Text -> Maybe Double)         -- ^ Cell converter.
  -> Text                           -- ^ 列名
  -> DXD.DataFrame
  -> (DXD.DataFrame, LogReport)
liftCellRule code rule fn name df = case getMaybeTextVec name df of
  Nothing ->
    ( df
    , logReport (mkWarn (code <> "W")
        (rule <> ": 列 '" <> name <> "' を text として取り出せません。")
        (Just "数値列に対しては不要かもしれません。"))
    )
  Just v  ->
    let raw       = V.toList v
            -- raw :: [Maybe Text]
        processed = [ mt >>= (\t -> if isMissing t then Nothing else fn t)
                    | mt <- raw ]
        nIn  = length raw
        nOk  = length [ () | Just _ <- processed ]
        nMis = length [ () | Just t <- raw, isMissing t ]
        df'  = DX.insertColumn name
                   (DX.fromList (processed :: [Maybe Double])) df
        msg  = rule <> " 適用: 列 '" <> name <> "' "
                  <> tShow nOk <> "/" <> tShow nIn <> " 成功"
                  <> (if nMis > 0 then " (NA " <> tShow nMis <> ")" else "")
        lg   = logReport (mkInfo code msg Nothing)
        warnLog = if nOk * 2 < nIn  -- 半数未満しか成功していない場合
                    then logReport (mkWarn (code <> "L")
                            (rule <> ": 列 '" <> name <> "' は変換成功率が低いです (" <> tShow nOk <> "/" <> tShow nIn <> ")")
                            (Just "別ルールを試すか、データを確認してください。"))
                    else noLog
    in (df', lg <> warnLog)

isMissing :: Text -> Bool
isMissing t = T.null (T.strip t)

tShow :: Show a => a -> Text
tShow = T.pack . show

-- ---------------------------------------------------------------------------
-- パイプライン
-- ---------------------------------------------------------------------------

-- | Apply several rules in order, concatenating the per-rule logs.
cleanPipeline
  :: [(Text, ColumnRule)]
  -> DXD.DataFrame
  -> (DXD.DataFrame, LogReport)
cleanPipeline []           df = (df, noLog)
cleanPipeline ((n, r):rs) df0 =
  let (df1, lg1) = applyRule r n df0
      (df2, lg2) = cleanPipeline rs df1
  in (df2, lg1 <> lg2)

-- ---------------------------------------------------------------------------
-- DataFrame レベル操作
-- ---------------------------------------------------------------------------

-- | Disambiguate duplicate column names by appending @_2@, @_3@, ...
-- (Hackage の DataFrame は重複列を後勝ちでマージするため、ロード前に
-- 行いたい場合は CSV テキスト側で。本関数はロード後の DataFrame に対して
-- 行う suffix 付与で、新しい DataFrame を返す。)
dedupeColumns :: DXD.DataFrame -> (DXD.DataFrame, LogReport)
dedupeColumns df =
  let names = DX.columnNames df
      go acc [] = reverse acc
      go acc (x:xs) =
        let used = Map.fromListWith (+) [(y, 1 :: Int) | y <- acc]
            n0   = Map.findWithDefault 0 x used
        in if n0 == 0 then go (x:acc) xs
                      else go ((x <> "_" <> tShow (n0 + 1)):acc) xs
      newNames = go [] names
      changed  = [ (a, b) | (a, b) <- zip names newNames, a /= b ]
  in if null changed
       then (df, noLog)
       else
         let df' = foldl rename df (zip names newNames)
             rename d (old, new)
               | old == new = d
               | otherwise  = DX.rename old new d
         in ( df'
            , logReport (mkInfo "I105"
                ("重複列名に suffix を付与: "
                   <> T.intercalate ", "
                       [ a <> " → " <> b | (a, b) <- changed ])
                Nothing))

-- | 空列名を col0 / col1 / ... で埋める。
fillBlankNames :: DXD.DataFrame -> (DXD.DataFrame, LogReport)
fillBlankNames df =
  let names = DX.columnNames df
      replaceBlank i n
        | T.null (T.strip n) = "col" <> tShow i
        | otherwise          = n
      newNames = zipWith replaceBlank [0 :: Int ..] names
      changed  = [ (a, b) | (a, b) <- zip names newNames, a /= b ]
  in if null changed
       then (df, noLog)
       else
         let df' = foldl rn df (zip names newNames)
             rn d (old, new) | old == new = d | otherwise = DX.rename old new d
         in ( df'
            , logReport (mkInfo "I106"
                ("空列名を埋めました: " <> tShow (length changed) <> " 列")
                Nothing))

-- 未使用 warning 抑止
_unused :: DXC.Column -> ()
_unused _ = ()
