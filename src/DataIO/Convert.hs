{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Hackage @dataframe@ ('DXD.DataFrame') から数値 / Text ベクタを安全に
-- 取り出す共通ヘルパ。Model.* / Viz.* 各層から widely 利用される。
--
-- - 'getDoubleVec': Double / Int / Maybe Double / Maybe Int / Text 列のいずれでも
--   @V.Vector Double@ に正規化。Text 列は parse、欠損 (null bitmap or NA 文字列)
--   が 1 つでもあれば 'Nothing' を返す (モデル fit が落ちないようにするため)。
-- - 'getTextVec': Text 列の取り出し。型不一致時は 'Nothing'。
module DataIO.Convert
  ( getDoubleVec
  , getTextVec
  ) where

import qualified DataFrame                    as DX
import qualified DataFrame.Internal.Column    as DXC
import qualified DataFrame.Internal.DataFrame as DXD

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
getTextVec :: Text -> DXD.DataFrame -> Maybe (V.Vector Text)
getTextVec name df = case tryColumnAsList @Text name df of
  Just xs -> Just (V.fromList xs)
  Nothing -> Nothing

-- | 'DX.columnAsList' を例外セーフに呼び出す。型不一致時は 'Nothing'。
tryColumnAsList
  :: forall a. DXC.Columnable a
  => Text -> DXD.DataFrame -> Maybe [a]
tryColumnAsList name df = unsafePerformIO $ do
  r <- try (evaluate (DX.columnAsList (DX.col @a name) df))
         :: IO (Either SomeException [a])
  return $ case r of
    Right xs -> Just xs
    Left _   -> Nothing
