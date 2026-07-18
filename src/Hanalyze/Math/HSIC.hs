{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Math.HSIC
-- Description : Hilbert-Schmidt Independence Criterion による kernel 法ベースの独立性検定統計量
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Hilbert-Schmidt Independence Criterion (HSIC、 Gretton et al. 2005)。
--
-- ## モチベーション
--
-- 確率変数 X, Y の独立性を測る kernel 法ベースの統計量。 線形相関や
-- partial correlation と違い、 非線形依存も検出できる。 LiNGAM 系統
-- (特に ParceLiNGAM bottom-up 探索) で「残差と他変数の独立性」 を判定する
-- 中核ツール。
--
-- ## 統計量 (biased empirical estimator)
--
-- > HSIC_b(X, Y) = (1 / n²) · tr(K_X · H · K_Y · H)
--
-- ここで K_X[i,j] = k(x_i, x_j) は RBF kernel、 H = I − (1/n) · 1 1ᵀ は
-- 中心化行列。 X ⊥ Y の下で HSIC_b → 0、 強依存で正値。
--
-- ## bandwidth の決め方
--
-- median heuristic: σ = median(‖x_i − x_j‖) (i ≠ j、 サンプル間距離の中央値)。
-- cdt15/lingam を含む慣用設定で、 サンプル数のオーダー依存が小さく robust。
--
-- ## 集約 (ParceLiNGAM での使い方)
--
-- 多次元 X (列が変数) と単変量残差 R の依存判定は、 各列 X_i ごとに
-- HSIC(X_i, R) を計算して **総和 (= aggregate)** を取る。 cdt15/lingam の
-- 内部実装は Fisher 法で p 値を合成するが、 v0.2 では p 値を使わず統計量の
-- 総和で相対比較する (実用上は relative scoring が機能する)。
--
-- ## リファレンス
--
-- Gretton et al. (2005) "Measuring statistical dependence with Hilbert-Schmidt
-- norms", ALT 2005. cdt15/lingam の `lingam/hsic.py`。
module Hanalyze.Math.HSIC
  ( hsicBiased
  , hsicRBF
  , medianBandwidth
  , hsicAggregate
  ) where

import qualified Numeric.LinearAlgebra      as LA
import qualified Hanalyze.Stat.KernelDist   as KD
import           Data.List                  (sort)

-- ===========================================================================
-- カーネル行列構築
-- ===========================================================================

-- | RBF (Gaussian) カーネル行列 K[i, j] = exp(−‖x_i − x_j‖² / (2σ²))。
--   入力 @x@ は @n × p@ (行がサンプル、 列が変数)。
rbfKernelMatrix :: Double -> LA.Matrix Double -> LA.Matrix Double
rbfKernelMatrix sigma x =
  let !twoSig2 = 2 * sigma * sigma
      !d2      = KD.pairwiseSqDist x
  in LA.cmap (\v -> exp (negate v / twoSig2)) d2

-- | サンプル間距離の中央値 (median heuristic for kernel bandwidth)。
--   対角 (距離 0) は除外し、 上三角の値だけを集めて中央値を取る。
--   退化 (median = 0) の場合は 1.0 にフォールバック。
medianBandwidth :: LA.Matrix Double -> Double
medianBandwidth x =
  let !d2    = KD.pairwiseSqDist x
      !n     = LA.rows d2
      vals   = [ LA.atIndex d2 (i, j)
               | i <- [0 .. n - 1], j <- [i + 1 .. n - 1] ]
      sorted = sort vals
      med    = case sorted of
                 [] -> 1.0
                 _  -> let !m = length sorted `div` 2
                       in sorted !! m
      sig    = sqrt (max med 1.0e-12)
  in if sig > 0 then sig else 1.0

-- ===========================================================================
-- HSIC 統計量
-- ===========================================================================

-- | biased empirical HSIC を K, L から計算: (1/n²) · tr(K_c · L_c)。
--   K_c = H K H、 L_c = H L H、 H = I − (1/n) · 1 1ᵀ。
--   ※ tr(K_c L_c) = tr(K_c L) (中心化の冪等性により) なので片側中心化で済む。
hsicWithKernels :: LA.Matrix Double -> LA.Matrix Double -> Double
hsicWithKernels k l =
  let !n     = LA.rows k
      !nD    = fromIntegral n
      !h     = LA.ident n - LA.scale (1.0 / nD)
                   (LA.konst 1.0 (n, n))
      !kc    = h LA.<> k LA.<> h
      !prod  = kc LA.<> l
      !tr    = sum [ LA.atIndex prod (i, i) | i <- [0 .. n - 1] ]
  in tr / (nD * nD)

-- | RBF kernel + median bandwidth で biased HSIC を計算。
--   入力 @x@, @y@ は @n × p@ / @n × q@ (行が共通サンプル、 列が変数)。
hsicRBF :: LA.Matrix Double -> LA.Matrix Double -> Double
hsicRBF x y =
  let !sx = medianBandwidth x
      !sy = medianBandwidth y
      !k  = rbfKernelMatrix sx x
      !l  = rbfKernelMatrix sy y
  in hsicWithKernels k l

-- | bias HSIC を @hsicRBF@ で計算する公開エイリアス。
hsicBiased :: LA.Matrix Double -> LA.Matrix Double -> Double
hsicBiased = hsicRBF

-- | 多次元 @X@ (n × p) と単変量 @r@ (長さ n) の依存度を、
--   各列ごとの HSIC を **総和** して集約する。 ParceLiNGAM bottom-up の
--   exogenous 判定に使う (cdt15/lingam の Fisher 法と同趣旨、 ただし p 値
--   合成ではなく統計量の総和)。
hsicAggregate :: LA.Matrix Double -> LA.Vector Double -> Double
hsicAggregate x r =
  let !p    = LA.cols x
      !rMat = LA.asColumn r
  in sum [ hsicRBF (LA.asColumn (LA.flatten (x LA.¿ [j]))) rMat
         | j <- [0 .. p - 1] ]
