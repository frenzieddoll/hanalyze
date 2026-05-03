{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- | 列単位のクリーニング DSL (Phase C)。
--
-- 通貨記号 / 桁区切り / 単位 / decimal point 違いなど、Phase A の Health
-- 検査では「警告止まり」だった列を、明示的なルール ('ColumnRule') で
-- 数値化する。
--
-- 設計方針
--
-- * 各ルールは「Text 列を取り出す → 変換 → DXD.DataFrame に書き戻す」の
--   形を取り、変換ログ ('LogReport') を必ず返す。
-- * 変換に失敗したセルは 'Nothing' (= null bitmap) として保存。
--   失敗件数は I100 系の Info コードでログに残す。
-- * パイプライン ('cleanPipeline') で複数ルールを順次適用できる。
-- * Phase B の自動推論との二段構え: sniff で読み込みは通るがセル値が
--   text のままになる #08 / #16 を、Clean で数値化して回帰可能にする。
--
-- 主要ルール
--
-- * 'StripUnits'      末尾の英字を取り除いて Double 化 (\"12.3kg\" → 12.3)
-- * 'ParseCurrency'   通貨記号 / 桁区切り (`$`/`¥`/`€`/`,`) を除去して数値化
-- * 'ParseDecimalEU'  decimal separator が ',' (EU style) のセルを Double 化
-- * 'TrimText'        前後の空白を除く
-- * 'CoerceNumeric'   上記 3 種を順に試して最初に成功した変換を採用
-- * 'DedupeColumns'   重複列名に @_2@ などのサフィックスを付ける
-- * 'FillBlankNames'  空列名を @col0@ 等で埋める
module DataIO.Clean
  ( -- * 型
    ColumnRule (..)
    -- * 個別ルール
  , applyRule
  , stripUnitsCol
  , parseCurrencyCol
  , parseDecimalEUCol
  , trimTextCol
  , coerceNumericCol
    -- * パイプライン
  , cleanPipeline
    -- * DataFrame レベル操作
  , dedupeColumns
  , fillBlankNames
  ) where

import qualified DataFrame                    as DX
import qualified DataFrame.Internal.DataFrame as DXD
import qualified DataFrame.Internal.Column    as DXC

import Data.Char (isAlpha, isDigit)
import qualified Data.Map.Strict      as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Read (readMaybe)

import DataIO.Convert (getMaybeTextVec)
import DataIO.Log     (LogReport, mkInfo, mkWarn, logReport, noLog)

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | 列単位の変換ルール。
data ColumnRule
  = StripUnits        -- ^ 末尾の英字を取り除く ("12.3kg"→12.3)
  | ParseCurrency     -- ^ "$1,234.56" / "¥10,000" 等を Double 化
  | ParseDecimalEU    -- ^ decimal point が ',' のセル ("3,14" → 3.14)
  | TrimText          -- ^ 前後の空白除去 (text 列のまま)
  | CoerceNumeric     -- ^ StripUnits → ParseCurrency → ParseDecimalEU を順に試す
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- 個別ルール (列名指定)
-- ---------------------------------------------------------------------------

-- | 単一ルールを単一列に適用する dispatch。
applyRule :: ColumnRule -> Text -> DXD.DataFrame -> (DXD.DataFrame, LogReport)
applyRule r name df = case r of
  StripUnits     -> stripUnitsCol     name df
  ParseCurrency  -> parseCurrencyCol  name df
  ParseDecimalEU -> parseDecimalEUCol name df
  TrimText       -> trimTextCol       name df
  CoerceNumeric  -> coerceNumericCol  name df

-- | "12.3kg" / "11.5cm" のように末尾の英字 (単位) を取り除いて Double 化。
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

-- | 通貨記号 / 桁区切りの数値を Double 化。"$1,234.56" → 1234.56。
parseCurrencyCol :: Text -> DXD.DataFrame -> (DXD.DataFrame, LogReport)
parseCurrencyCol = liftCellRule "I101" "parseCurrency" $ \t ->
  let s1 = T.strip t
      s2 = T.dropWhile (`elem` ("$¥€£" :: String)) s1
      s3 = T.replace "," "" s2
  in readMaybe (T.unpack s3)

-- | decimal separator が ',' (EU style)。"3,14" → 3.14。
parseDecimalEUCol :: Text -> DXD.DataFrame -> (DXD.DataFrame, LogReport)
parseDecimalEUCol = liftCellRule "I102" "parseDecimalEU" $ \t ->
  let s = T.replace "," "." (T.strip t)
  in readMaybe (T.unpack s)

-- | 前後空白を除いた Text 列に書き戻す。
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

-- | StripUnits → ParseCurrency → ParseDecimalEU を順に試す万能変換。
-- 最初に成功した変換を採用、どれも失敗なら null bitmap で詰める。
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

-- | 任意の text → 'Maybe Double' 変換関数を 1 列に適用するヘルパ。
-- 列が text として取れない場合は警告のみ出して df を返す。
liftCellRule
  :: Text                           -- ^ Info コード
  -> Text                           -- ^ ルール名 (ログ用)
  -> (Text -> Maybe Double)         -- ^ セル変換関数
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

-- | 複数のルールを順次適用する。各ルールのログを連結して返す。
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

-- | 重複列名に @_2@, @_3@ ... のサフィックスを付けて衝突回避する。
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
