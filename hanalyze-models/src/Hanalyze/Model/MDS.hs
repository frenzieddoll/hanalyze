{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Model.MDS
-- Description : MDS (多次元尺度構成法) の高レベルモデル型 (Phase 75.21)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- MDS の高レベルモデル型 (Phase 75.21)。
--
-- 低レベルの行列カーネル ('mdsClassical' / 'mdsSammon' / 'euclideanDist') は
-- 'Hanalyze.Stat.MDS' に置き、 ここは @df |-> mds cfg cols@ で使う
-- **モデル型** 'MDSResult' (= 'Hanalyze.Model.PCA.PCAResult' と同格) と
-- その設定 'MDSConfig' を提供する。
--
-- MDS (多次元尺度構成法) = サンプル間の **距離 (非類似度) を保ったまま** 高次元
-- データを 2D へ配置する可視化・次元圧縮。 'MDSClassical' (Torgerson・ユークリッド
-- 距離なら PCA と等価) と 'MDSSammon' (小距離重視の非線形版) を選べる。 結果は
-- 埋め込み (MDS1/MDS2) に加え **元データ (群色付け用の列を含む)** を保持し、
-- plot 側で @toPlot m@ (単色散布) / @toPlot (mdsView m <> mdsGroupBy \"g\")@ (群色) に使う。
module Hanalyze.Model.MDS
  ( -- * 手法と設定
    MDSMethod (..)
  , MDSConfig (..)
  , defaultMDS
    -- ** 再 export (Sammon パラメータ)
  , SammonConfig (..)
  , defaultSammonConfig
    -- * モデル型
  , MDSResult (..)
  , runMDS
  ) where

import           Data.Text (Text)
import qualified Data.Text             as T
import qualified Data.Vector           as V
import qualified Numeric.LinearAlgebra as LA
import qualified DataFrame.Internal.DataFrame  as DX

import           Hanalyze.DataIO.Convert (getDoubleVec)
import qualified Hanalyze.Stat.MDS       as S
import           Hanalyze.Stat.MDS       (SammonConfig (..), defaultSammonConfig)

-- ===========================================================================
-- 手法と設定
-- ===========================================================================

-- | MDS の手法選択。 'MDSClassical' = 古典 MDS (Torgerson・固有分解)、
-- 'MDSSammon' = Sammon 写像 (小距離重視の非線形・勾配降下)。
data MDSMethod = MDSClassical | MDSSammon
  deriving (Show, Eq)

-- | MDS の設定。 手法 ('mdsMethod') と、 'MDSSammon' 選択時に使う Sammon
-- パラメータ ('mdsSammon') を持つ (他の config 同様レコード型・裸の直和を
-- spec 引数にしない)。 k=2 固定・距離はユークリッドのみ (現状実装どおり)。
data MDSConfig = MDSConfig
  { mdsMethod :: !MDSMethod      -- ^ 古典 / Sammon。
  , mdsSammon :: !SammonConfig   -- ^ 'MDSSammon' 選択時の勾配降下パラメータ。
  } deriving (Show)

-- | 既定設定: 古典 MDS・Sammon パラメータは既定。
defaultMDS :: MDSConfig
defaultMDS = MDSConfig MDSClassical defaultSammonConfig

-- ===========================================================================
-- モデル型
-- ===========================================================================

-- | 学習済 MDS。 2D 埋め込み (MDS1/MDS2) に加え、 **元データ ('mdsSourceFrame')** を
-- 保持して plot 側の群色付け ('mdsGroupBy') に使う。 'Hanalyze.Model.PCA.PCAResult'
-- と同格のモデル型 (df 型ではない)。
data MDSResult = MDSResult
  { mdsMethodUsed  :: !MDSMethod          -- ^ 使った手法。
  , mdsEmbedding   :: !(LA.Matrix Double) -- ^ 埋め込み (n × 2)。
  , mdsFeatures    :: ![Text]             -- ^ 入力に使った特徴列名。
  , mdsSourceFrame :: !DX.DataFrame       -- ^ 元データ (群色付け用に保持)。
  }

-- | @runMDS cfg frame cols@ — frame の特徴列 @cols@ を行列化し、 ユークリッド
-- 距離 → 古典 / Sammon MDS で 2D 埋め込みを得る。 列が無い / 長さ不揃いなら 'Left'。
runMDS :: MDSConfig -> DX.DataFrame -> [Text] -> Either String MDSResult
runMDS _   _     []   = Left "MDS: 特徴列が空です (1 列以上必要)"
runMDS cfg frame cols = do
  colVecs <- mapM getCol cols
  let lens = map length colVecs
  if not (allEq lens)
    then Left ("MDS: 特徴列の長さが不揃いです: " <> show lens)
    else do
      let n    = head lens
          xMat = LA.fromLists [ [ v !! i | v <- colVecs ] | i <- [0 .. n - 1] ]
          d    = S.euclideanDist xMat
          emb  = case mdsMethod cfg of
                   MDSClassical -> S.mdsClassical d 2
                   MDSSammon    -> S.mdsSammon (mdsSammon cfg) d 2
      Right MDSResult
        { mdsMethodUsed  = mdsMethod cfg
        , mdsEmbedding   = emb
        , mdsFeatures    = cols
        , mdsSourceFrame = frame
        }
  where
    getCol c = case V.toList <$> getDoubleVec c frame of
      Just vs -> Right vs
      Nothing -> Left ("MDS: 数値列が見つかりません: " <> T.unpack c)
    allEq []     = True
    allEq (x:xs) = all (== x) xs
