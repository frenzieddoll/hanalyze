{-# LANGUAGE OverloadedStrings #-}
-- | 入力特徴の標準化 (z-score 化) ユーティリティ。
--
-- 用途:
--
-- * RFF / Kernel 系で複数特徴のスケール差が大きいときに、共通長さスケール
--   ℓ で破綻するのを防ぐ。'fitStandardizer' で μ, σ を学習し、
--   'applyStandardizer' で X を z-score 化、モデルが返した予測点に対しては
--   'unapplyStandardizer' で元単位に戻す。
-- * インタラクティブ予測 (JS) で、ユーザーが元単位 (例: energy=80 keV)
--   を slider で入力したものを、JS 側で μ, σ を使って標準化空間に変換し
--   モデルに渡す。そのために 'stMu' / 'stSd' をそのまま JSON 出力できる。
--
-- 約束事:
--
-- * y は標準化しない (回帰の出力スケールを保つ)。
-- * std=0 の定数列は、std=1.0 として扱い (x - μ)/1 = x - μ を返す。
--   実質的には「中央化のみ」になる。
-- * n=1 の列も std=1.0 扱い。
module Stat.Standardize
  ( Standardizer (..)
  , fitStandardizer
  , applyStandardizer
  , unapplyStandardizer
  , applyStandardizerCol
  , identityStandardizer
  ) where

import qualified Numeric.LinearAlgebra as LA

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | 各特徴の (μ, σ)。length は特徴数 p。
data Standardizer = Standardizer
  { stMu :: ![Double]
  , stSd :: ![Double]
  } deriving (Eq, Show)

-- | 「何もしない」標準化 (μ=0, σ=1)。p 次元。
identityStandardizer :: Int -> Standardizer
identityStandardizer p = Standardizer (replicate p 0) (replicate p 1)

-- ---------------------------------------------------------------------------
-- 学習 (fit)
-- ---------------------------------------------------------------------------

-- | n × p の行列から各列の (mean, std) を学習。
-- * std は不偏分散の平方根 (n-1 で正規化)。
-- * std が極小 (< 1e-12) の列は std=1.0 に置換 (定数列対策)。
fitStandardizer :: LA.Matrix Double -> Standardizer
fitStandardizer x =
  let cols = LA.toColumns x
      mus  = map mean cols
      sds  = zipWith (\c m -> robustSd c m) cols mus
  in Standardizer mus sds
  where
    mean v
      | LA.size v == 0 = 0
      | otherwise      = LA.sumElements v / fromIntegral (LA.size v)
    robustSd v m =
      let n = LA.size v
      in if n <= 1
           then 1.0
           else
             let xs   = LA.toList v
                 ss   = sum [ (x' - m) * (x' - m) | x' <- xs ]
                 var  = ss / fromIntegral (n - 1)
                 sd0  = sqrt var
             in if sd0 < 1e-12 then 1.0 else sd0

-- ---------------------------------------------------------------------------
-- 適用 / 復元
-- ---------------------------------------------------------------------------

-- | (x - μ) / σ を全行に適用。
applyStandardizer :: Standardizer -> LA.Matrix Double -> LA.Matrix Double
applyStandardizer s x =
  let cols  = LA.toColumns x
      cols' = zipWith3 transformCol cols (stMu s) (stSd s)
  in LA.fromColumns cols'
  where
    transformCol c m sd = LA.cmap (\v -> (v - m) / sd) c

-- | x · σ + μ を全行に適用 (標準化空間 → 元単位)。
unapplyStandardizer :: Standardizer -> LA.Matrix Double -> LA.Matrix Double
unapplyStandardizer s x =
  let cols  = LA.toColumns x
      cols' = zipWith3 untransformCol cols (stMu s) (stSd s)
  in LA.fromColumns cols'
  where
    untransformCol c m sd = LA.cmap (\v -> v * sd + m) c

-- | 1 列・1 セルの標準化 (JS 側 / スライダ用)。インデックスが範囲外なら値そのまま。
applyStandardizerCol :: Standardizer -> Int -> Double -> Double
applyStandardizerCol s k v
  | k < 0 || k >= length (stMu s) = v
  | otherwise =
      let m  = stMu s !! k
          sd = stSd s !! k
      in (v - m) / sd
