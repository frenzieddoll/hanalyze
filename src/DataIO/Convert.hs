{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Safe extraction of numeric / Text vectors from a Hackage @dataframe@
-- ('DXD.DataFrame'). Used widely across @Model.*@ and @Viz.*@.
--
--   * 'getDoubleVec' — normalize Double / Int / Maybe Double / Maybe Int /
--     Text columns to @V.Vector Double@. Text values are parsed; if any
--     missing slot is present (null bitmap or NA string), returns
--     'Nothing' so model fits cannot crash on missing data.
--   * 'getTextVec'   — extract a Text column. Returns 'Nothing' on type
--     mismatch.
module DataIO.Convert
  ( getDoubleVec
  , getTextVec
  , getMaybeTextVec
  ) where

import qualified DataFrame                    as DX
import qualified DataFrame.Internal.Column    as DXC
import qualified DataFrame.Internal.DataFrame as DXD

import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, try, evaluate)
import Data.Text (Text)
import qualified Data.Vector as V
import System.IO.Unsafe (unsafePerformIO)

import DataIO.Preprocess (readMaybeDoubleColumn)

-- | DXD.DataFrame から数値列を 'V.Vector Double' として取り出す。
-- 欠損 / parse 失敗が含まれていれば 'Nothing'。
getDoubleVec :: Text -> DXD.DataFrame -> Maybe (V.Vector Double)
getDoubleVec name df = do
  xs <- readMaybeDoubleColumn name df
  vs <- sequence xs
  return (V.fromList vs)

-- | DXD.DataFrame から Text 列を 'V.Vector Text' として取り出す。
-- null bitmap が立った要素を含む列は 'Nothing' を返す
-- (純粋に文字列だけが詰まっていることが保証される)。
getTextVec :: Text -> DXD.DataFrame -> Maybe (V.Vector Text)
getTextVec name df = case tryColumnAsList @Text name df of
  Just xs -> Just (V.fromList xs)
  Nothing -> Nothing

-- | DXD.DataFrame から Text 列を 'V.Vector (Maybe Text)' として取り出す。
-- null セルが含まれていても 'Nothing' で表現したものを返す。
-- 'getTextVec' で 'Nothing' になった列の検査用に用いる (info 表示など)。
getMaybeTextVec :: Text -> DXD.DataFrame -> Maybe (V.Vector (Maybe Text))
getMaybeTextVec name df =
  case tryColumnAsList @(Maybe Text) name df of
    Just xs -> Just (V.fromList xs)
    Nothing -> case tryColumnAsList @Text name df of
      Just xs -> Just (V.fromList (map Just xs))
      Nothing -> Nothing

-- | 'DX.columnAsList' を例外セーフに呼び出す。型不一致 / null 要素アクセス
-- (Hackage が内部で 'error "fromMaybeVec: Nothing slot"' を投げるケース等)
-- でも 'Nothing' を返す。
--
-- 重要: 'evaluate' は WHNF までしか評価しないので、リスト要素に潜む 'error'
-- が逃げてくる。'force' を挟んで NF まで詰めてから捕捉する。
tryColumnAsList
  :: forall a. (DXC.Columnable a, NFData a)
  => Text -> DXD.DataFrame -> Maybe [a]
tryColumnAsList name df = unsafePerformIO $ do
  r <- try (evaluate (force (DX.columnAsList (DX.col @a name) df)))
         :: IO (Either SomeException [a])
  return $ case r of
    Right xs -> Just xs
    Left _   -> Nothing
