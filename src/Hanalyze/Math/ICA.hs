{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
-- |
-- Module      : Hanalyze.Math.ICA
-- Description : FastICA (Hyvärinen 1999) による独立成分分析 (whitening + fixed-point iteration)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- FastICA (Hyvärinen 1999) による独立成分分析。
--
-- 観測 X = A · S (n_samples × p)、 S が互いに独立な非ガウシアン成分のとき、
-- A を推定して S = A⁻¹ · X を抽出する。 ICA-LiNGAM (Shimizu 2006) の前段
-- および信号分離一般に使う。
--
-- ## アルゴリズム
--
-- 1. **Centering**: X の各列を中心化
-- 2. **Whitening**: X の covariance を eigen 分解して
--    @Z = E · D^(-1/2) · Eᵀ · X@ を作る (Z の cov = I)
-- 3. **Fixed-point iteration** (per component): 任意の w から始めて
--    @w⁺ = E[Z · g(wᵀZ)] - E[g'(wᵀZ)] · w@、 正規化、 直交化 (デフレーション)、
--    収束 (|wᵀwᵒˡᵈ| ≈ 1) まで繰返し
-- 4. **回収**: 全成分の row 構成 W に対し、 S = W · Z、 A = pinv(W) (whitened
--    座標から元座標への戻し変換は別途)
--
-- non-linearity g としては logcosh (Hyvärinen 標準) を採用:
-- g(u) = tanh(a·u)、 g'(u) = a·(1 - tanh²(a·u))、 a = 1.0
--
-- ## 出力
--
-- 'ICAResult' は分離行列 W (p × p, whitened 座標)、 mixing 行列 A (元座標、
-- W · whiten から逆算)、 推定独立成分 S (n × p)、 収束情報を持つ。
module Hanalyze.Math.ICA
  ( ICAConfig (..)
  , ICAResult (..)
  , defaultICAConfig
  , fitICA
  , fitICAGen
  , fitICAPure
  ) where

import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector           as V
import qualified System.Random.MWC     as MWC
import           Control.Monad         (forM_, when)
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad.ST      (runST)
import           Data.Primitive.MutVar (newMutVar, readMutVar, writeMutVar)
import           System.Random.MWC.Distributions (standard)

-- ===========================================================================
-- 設定
-- ===========================================================================

data ICAConfig = ICAConfig
  { icaMaxIter   :: !Int
  , icaTol       :: !Double
  , icaNumComp   :: !(Maybe Int)
    -- ^ 抽出する成分数。 'Nothing' で全成分 (= p)
  , icaSeed      :: !(Maybe Int)
  } deriving (Show)

defaultICAConfig :: ICAConfig
defaultICAConfig = ICAConfig
  { icaMaxIter = 200
  , icaTol     = 1e-4
  , icaNumComp = Nothing
  , icaSeed    = Just 12345
  }

data ICAResult = ICAResult
  { icaW           :: !(LA.Matrix Double)
    -- ^ whitened 空間での分離行列 (p × p)
  , icaA           :: !(LA.Matrix Double)
    -- ^ 元 X 空間における推定 mixing 行列。 X ≈ S · Aᵀ + mean
  , icaUnmixing    :: !(LA.Matrix Double)
    -- ^ 元 X 空間における分離行列 (S = (X - mean) · unmixingᵀ)
  , icaS           :: !(LA.Matrix Double)
    -- ^ 推定独立成分 (n × k)
  , icaMean        :: !(LA.Vector Double)
    -- ^ 列平均 (centering 用)
  , icaConverged   :: !Bool
  , icaIterations  :: !Int
  } deriving (Show)

-- ===========================================================================
-- 主実装
-- ===========================================================================

-- | FastICA 本体 (Phase 77.C で PrimMonad 一般化)。 Gen を受け取り ST/IO いずれでも動く
--   (IORef→MutVar)。 'fitICA' (IO) / 'fitICAPure' (ST・seed) が gen を作って呼ぶ。
fitICAGen :: PrimMonad m => ICAConfig -> LA.Matrix Double -> MWC.Gen (PrimState m) -> m ICAResult
fitICAGen cfg x gen = do
  let !n  = LA.rows x
      !p  = LA.cols x
      !k  = maybe p id (icaNumComp cfg)
      -- centering
      means = LA.fromList
                [ LA.sumElements (x LA.¿ [j]) / fromIntegral n
                | j <- [0 .. p - 1] ]
      meanMat = LA.fromRows (replicate n means)
      xc      = x - meanMat
      -- whitening: Z = E D^(-1/2) Eᵀ · Xᵀ をしたいが、 hmatrix は行ベクトル
      -- 規約なので、 共分散行列を求めて eigen 分解する
      cov     = (LA.tr xc LA.<> xc) / fromIntegral n
      (d, e)  = LA.eigSH (LA.trustSym cov)
      -- d : Vector Double, e : Matrix Double (columns are eigenvectors)
      dInvSqrt = LA.cmap (\v -> if v > 1e-12 then 1 / sqrt v else 0) d
      whitenMat = e LA.<> LA.diag dInvSqrt LA.<> LA.tr e   -- (p × p)
      z         = xc LA.<> LA.tr whitenMat                -- (n × p)
  -- FastICA loop (deflation) — p × p の分離行列 W を 1 行ずつ確定。 gen は引数。
  wRowsRef <- newMutVar ([] :: [LA.Vector Double])
  itersRef <- newMutVar (0 :: Int)
  convRef  <- newMutVar True
  forM_ [0 .. k - 1] $ \_compIdx -> do
    -- 初期 w を gauss 乱数で
    w0Raw <- V.replicateM p (standard gen)
    let w0 = LA.fromList (V.toList w0Raw)
    wsExisting <- readMutVar wRowsRef
    -- 既存成分への直交化
    let w0Ortho = deflate wsExisting w0
        w0Norm  = LA.scale (1 / LA.norm_2 w0Ortho) w0Ortho
    -- fixed point iteration
    wRef <- newMutVar w0Norm
    convergedThisRef <- newMutVar False
    forM_ [1 .. icaMaxIter cfg] $ \iter -> do
      wOld <- readMutVar wRef
      isC  <- readMutVar convergedThisRef
      when (not isC) $ do
        let wu     = z LA.#> wOld          -- (n,)
            gWu    = LA.cmap tanh wu
            gpWu   = LA.cmap (\v -> 1 - tanh v ** 2) wu
            wNew0  = LA.tr z LA.#> gWu / LA.scalar (fromIntegral n)
                       - LA.scale (LA.sumElements gpWu / fromIntegral n) wOld
            wDef   = deflate wsExisting wNew0
            wNew   = LA.scale (1 / LA.norm_2 wDef) wDef
            !diff  = abs (abs (wNew `LA.dot` wOld) - 1)
        writeMutVar wRef wNew
        writeMutVar itersRef iter
        when (diff < icaTol cfg) $ writeMutVar convergedThisRef True
    finalConv <- readMutVar convergedThisRef
    when (not finalConv) $ writeMutVar convRef False
    wFinal <- readMutVar wRef
    writeMutVar wRowsRef (wsExisting ++ [wFinal])
  ws <- readMutVar wRowsRef
  let !wMat = LA.fromRows ws                    -- (k × p)、 whitened 空間
      !sMat = z LA.<> LA.tr wMat                -- (n × k)、 独立成分
      -- 元 X 空間: unmixing = wMat · whitenMat (k × p)
      !unmixing = wMat LA.<> whitenMat
      -- mixing = pseudo-inverse of unmixing  (p × k)
      !mixing   = LA.pinv unmixing
  iters <- readMutVar itersRef
  conv  <- readMutVar convRef
  pure ICAResult
    { icaW           = wMat
    , icaA           = mixing
    , icaUnmixing    = unmixing
    , icaS           = sMat
    , icaMean        = means
    , icaConverged   = conv
    , icaIterations  = iters
    }
  where
    deflate :: [LA.Vector Double] -> LA.Vector Double -> LA.Vector Double
    deflate ws w = foldl (\acc wi -> acc - LA.scale (acc `LA.dot` wi) wi) w ws

-- | FastICA (IO)。 'icaSeed' が 'Just' なら決定的、 'Nothing' で system random。
fitICA :: ICAConfig -> LA.Matrix Double -> IO ICAResult
fitICA cfg x = do
  gen <- case icaSeed cfg of
    Just s  -> MWC.initialize (V.fromList [fromIntegral s])
    Nothing -> MWC.createSystemRandom
  fitICAGen cfg x gen

-- | FastICA の **seed 純粋版** (Phase 77.C・@df |->@ 用)。 'icaSeed' (既定 12345・'Nothing' は
--   12345 fallback) で 'runST'+MWC。 同 seed で IO 版とビット一致 (乱数列は monad 非依存)。
fitICAPure :: ICAConfig -> LA.Matrix Double -> ICAResult
fitICAPure cfg x = runST $ do
  gen <- MWC.initialize (V.fromList [fromIntegral (maybe 12345 id (icaSeed cfg))])
  fitICAGen cfg x gen
