{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Hanalyze.DataIO.Convert
-- Description : Hackage dataframe から数値/Text 列を安全に Vector へ抽出する変換層
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Safe extraction of numeric / Text vectors from a Hackage @dataframe@
-- ('DXD.DataFrame'). Used widely across @Model.*@ and @Viz.*@.
--
--   * 'getDoubleVec' — normalize Double / Int / Integer / Maybe Double /
--     Maybe Int / Maybe Integer / Text columns to @V.Vector Double@. Text values are parsed; if any
--     missing slot is present (null bitmap or NA string), returns
--     'Nothing' so model fits cannot crash on missing data.
--   * 'getTextVec'   — extract a Text column. Returns 'Nothing' on type
--     mismatch.
module Hanalyze.DataIO.Convert
  ( getDoubleVec
  , getTextVec
  , getMaybeTextVec
  ) where

import qualified DataFrame.Operators           as DX
import qualified DataFrame.Operations.Core     as DX
import qualified DataFrame.Internal.Column    as DXC
import qualified DataFrame.Internal.DataFrame as DXD

import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, try, evaluate)
import Data.Text (Text)
import qualified Data.Vector as V
import System.IO.Unsafe (unsafePerformIO)

import Hanalyze.DataIO.Preprocess (readMaybeDoubleColumn)

-- | Extract a numeric column as 'V.Vector Double'. Returns 'Nothing'
-- when any cell is missing or fails to parse.
getDoubleVec :: Text -> DXD.DataFrame -> Maybe (V.Vector Double)
getDoubleVec name df = do
  xs <- readMaybeDoubleColumn name df
  vs <- sequence xs
  return (V.fromList vs)

-- | Extract a Text column as 'V.Vector Text'. Returns 'Nothing' if any
-- slot has its null bit set, guaranteeing the result holds only proper
-- strings.
getTextVec :: Text -> DXD.DataFrame -> Maybe (V.Vector Text)
getTextVec name df = case tryColumnAsList @Text name df of
  Just xs -> Just (V.fromList xs)
  Nothing -> Nothing

-- | Extract a Text column as 'V.Vector (Maybe Text)'. Null cells become
-- 'Nothing' instead of failing. Useful for inspecting columns where
-- 'getTextVec' would return 'Nothing' (e.g. for @info@ display).
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
