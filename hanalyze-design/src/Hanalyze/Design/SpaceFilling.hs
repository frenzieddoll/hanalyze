{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Design.SpaceFilling
-- Description : 空間充填計画 (Latin Hypercube / Maximin LHS / Halton) — コンピュータ実験・surrogate モデル用 DoE
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 空間充填計画 (Space-Filling Designs) — コンピュータ実験 / surrogate
-- モデル用の DoE。
--
-- 提供する方式:
--
--   * 'latinHypercube' — Latin Hypercube Sampling (stratified random)
--   * 'latinHypercubeMaximin' — Maximin LHS (点間最小距離を最大化する局所探索)
--   * 'haltonDesign' — Halton 低偏差列 (決定的、 再現性高)
--
-- 出力は全て @[0, 1)^d@ 上の点。 ユーザは bounds スケーリングを後で行う
-- (`Hanalyze.Stat.QuasiRandom.lhsSamplesIn` 等を参考に)。
module Hanalyze.Design.SpaceFilling
  ( SpaceFillingDesign (..)
  , latinHypercube
  , latinHypercubeMaximin
  , haltonDesign
    -- * 品質指標
  , designMinDistance
  ) where

import           Control.Monad             (forM_, when)
import           Data.IORef                (newIORef, readIORef, writeIORef, modifyIORef')
import qualified Numeric.LinearAlgebra     as LA
import           Data.Text                 (Text)
import qualified System.Random.MWC         as MWC

import qualified Hanalyze.Stat.QuasiRandom as QR

-- ===========================================================================
-- 型
-- ===========================================================================

-- | 空間充填計画の結果。
data SpaceFillingDesign = SpaceFillingDesign
  { sfdMatrix  :: !(LA.Matrix Double)  -- ^ n × d、 @[0, 1)^d@ 上の点
  , sfdNPoints :: !Int                 -- ^ 行数 n
  , sfdNDims   :: !Int                 -- ^ 列数 d
  , sfdMinDist :: !Double              -- ^ 点間最小ユークリッド距離 (大きい方が良い)
  , sfdMethod  :: !Text                -- ^ "LHS" / "MaximinLHS" / "Halton"
  } deriving (Show)

-- ===========================================================================
-- 公開関数
-- ===========================================================================

-- | Latin Hypercube Sampling — 各次元のセル @[i/n, (i+1)/n)@ を 1 度ずつ
-- ランダム順序で埋める。 iid uniform より初期被覆良。
latinHypercube :: Int            -- ^ 点数 n
               -> Int            -- ^ 次元 d
               -> MWC.GenIO
               -> IO SpaceFillingDesign
latinHypercube n d gen
  | n < 1 || d < 1 = pure SpaceFillingDesign
      { sfdMatrix  = (0 LA.>< 0) []
      , sfdNPoints = 0
      , sfdNDims   = 0
      , sfdMinDist = 0
      , sfdMethod  = "LHS"
      }
  | otherwise = do
      pts <- QR.lhsSamples n d gen
      let mat = LA.fromLists pts
      pure SpaceFillingDesign
        { sfdMatrix  = mat
        , sfdNPoints = n
        , sfdNDims   = d
        , sfdMinDist = designMinDistance mat
        , sfdMethod  = "LHS"
        }

-- | Maximin LHS — 初期 LHS から始めて、 ランダム (列, 行ペア) で値交換を試行、
-- 点間最小距離が改善するなら採用、 を @nTries@ 回 (= 全試行回数) 反復。
--
-- 結果は **LHS の stratification 性質を保ったまま** 距離を最大化したもの。
-- @nTries = 1000@ 程度で実用的な改善が得られる (n, d による)。
latinHypercubeMaximin :: Int            -- ^ 点数 n
                     -> Int            -- ^ 次元 d
                     -> Int            -- ^ 試行回数 (= swap 候補数の上限)
                     -> MWC.GenIO
                     -> IO SpaceFillingDesign
latinHypercubeMaximin n d nTries gen
  | n < 2 || d < 1 = do
      -- 1 点しかなければ swap 不能、 通常 LHS を返す
      lhs <- latinHypercube n d gen
      pure lhs { sfdMethod = "MaximinLHS" }
  | otherwise = do
      initPts  <- QR.lhsSamples n d gen
      matRef   <- newIORef (LA.fromLists initPts)
      distRef  <- do
        let m0 = LA.fromLists initPts
        newIORef (designMinDistance m0)
      forM_ [1 .. nTries] $ \_ -> do
        -- ランダムに 1 列 k 選び、 その列の 2 行 i, j を swap
        k <- MWC.uniformR (0, d - 1) gen
        i <- MWC.uniformR (0, n - 1) gen
        j <- MWC.uniformR (0, n - 1) gen
        when (i /= j) $ do
          curMat   <- readIORef matRef
          let newMat  = swapEntries curMat i j k
              newDist = designMinDistance newMat
          curDist <- readIORef distRef
          when (newDist > curDist) $ do
            writeIORef matRef  newMat
            writeIORef distRef newDist
      finalMat  <- readIORef matRef
      finalDist <- readIORef distRef
      pure SpaceFillingDesign
        { sfdMatrix  = finalMat
        , sfdNPoints = n
        , sfdNDims   = d
        , sfdMinDist = finalDist
        , sfdMethod  = "MaximinLHS"
        }

-- | Halton 低偏差列ベースの決定的 design。 同じ @(n, d)@ で必ず同じ点集合を
-- 返す (再現性目的)。
haltonDesign :: Int          -- ^ 点数 n
             -> Int          -- ^ 次元 d
             -> SpaceFillingDesign
haltonDesign n d
  | n < 1 || d < 1 = SpaceFillingDesign
      { sfdMatrix  = (0 LA.>< 0) []
      , sfdNPoints = 0
      , sfdNDims   = 0
      , sfdMinDist = 0
      , sfdMethod  = "Halton"
      }
  | otherwise =
      let mat = QR.haltonMatrix n d
      in SpaceFillingDesign
           { sfdMatrix  = mat
           , sfdNPoints = n
           , sfdNDims   = d
           , sfdMinDist = designMinDistance mat
           , sfdMethod  = "Halton"
           }

-- ===========================================================================
-- 品質指標
-- ===========================================================================

-- | 点間ユークリッド距離の最小値。 空 design (行数 < 2) では 0。
designMinDistance :: LA.Matrix Double -> Double
designMinDistance mat
  | LA.rows mat < 2 = 0
  | otherwise =
      let n   = LA.rows mat
          rs  = LA.toRows mat
          pairs = [ (i, j) | i <- [0 .. n - 2], j <- [i + 1 .. n - 1] ]
          dist (i, j) =
            let di = rs !! i
                dj = rs !! j
                v  = di - dj
            in sqrt (LA.sumElements (v * v))
      in minimum (map dist pairs)

-- ===========================================================================
-- 内部 helper
-- ===========================================================================

-- | Matrix の (i, k) 要素と (j, k) 要素を入れ替えた新しい Matrix。
swapEntries :: LA.Matrix Double -> Int -> Int -> Int -> LA.Matrix Double
swapEntries mat i j k =
  let nR = LA.rows mat
      nC = LA.cols mat
      a  = LA.atIndex mat (i, k)
      b  = LA.atIndex mat (j, k)
      rows = LA.toLists mat
      update r idx newVal =
        take k r ++ [newVal] ++ drop (k + 1) r
      _ = (nR, nC)  -- silence
  in LA.fromLists
       [ if rIdx == i then update (rows !! rIdx) k b
         else if rIdx == j then update (rows !! rIdx) k a
         else rows !! rIdx
       | rIdx <- [0 .. nR - 1]
       ]
