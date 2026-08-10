{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Design.Diagnostics
-- Description : DoE 設計診断 (Alias Matrix / VIF / D-A-G-I efficiency の一括算出)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- DoE 設計診断: Alias Matrix / VIF / D-A-G-I efficiency。
--
-- 設計行列 @X@ (n × p) に対して、 multicollinearity と最適性指標を一括算出。
--
-- ===  efficiency 指標
--
--   * D-efficiency = (|XᵀX| / n^p)^{1/p}
--   * A-efficiency = p / trace((XᵀX/n)⁻¹)
--   * G-efficiency = p / max_i (n · x_iᵀ (XᵀX)⁻¹ x_i)
--   * I-efficiency = 1 / (n · trace((XᵀX)⁻¹ · M))、
--     M = 1/n · XᵀX (= self-moment 近似版)
--
-- ===  VIF
--
-- 各列 j について、 「j 以外の列で j を回帰」 した R² を用いて
-- @VIF_j = 1 / (1 − R²_j)@。 切片を含む X を想定し、 切片列 (= 全 1) は
-- スキップ。
--
-- ===  Alias Matrix
--
-- @A = (XᵀX)⁻¹ Xᵀ Z@、 ここで Z は 「設計に入れていない交絡項」 のモデル
-- 行列。 ここでは Z を Optional 引数として取り、 未指定の場合は
-- アライアス対象が無いとして空行列を返す。
module Hanalyze.Design.Diagnostics
  ( DesignDiagnostics (..)
  , diagnostics
  , diagnosticsWithAlias
  , vifVector
  , aliasMatrix
  ) where

import qualified Numeric.LinearAlgebra as LA

-- ===========================================================================
-- 型
-- ===========================================================================

data DesignDiagnostics = DesignDiagnostics
  { ddVIF         :: !(LA.Vector Double)
  , ddDEff        :: !Double
  , ddAEff        :: !Double
  , ddGEff        :: !Double
  , ddIEff        :: !Double
  , ddAliasMatrix :: !(LA.Matrix Double)
  } deriving (Show)

-- ===========================================================================
-- 公開 API
-- ===========================================================================

-- | Alias を含めない簡易版 (Z = 空)。
diagnostics :: LA.Matrix Double -> DesignDiagnostics
diagnostics x =
  let dd = computeDiagnostics x
  in dd { ddAliasMatrix = LA.fromLists [[]] }

-- | Z (交絡対象モデル行列) 込みの完全版。 Z の行数は X と一致する必要がある。
diagnosticsWithAlias :: LA.Matrix Double -> LA.Matrix Double -> DesignDiagnostics
diagnosticsWithAlias x z =
  let dd = computeDiagnostics x
      a  = aliasMatrix x z
  in dd { ddAliasMatrix = a }

-- | VIF を各列について返す (全 1 列は VIF = 1)。
vifVector :: LA.Matrix Double -> LA.Vector Double
vifVector x =
  let p = LA.cols x
  in LA.fromList [ vifForCol x j | j <- [0 .. p - 1] ]

-- | Alias matrix A = (XᵀX)⁻¹ Xᵀ Z。
aliasMatrix :: LA.Matrix Double -> LA.Matrix Double -> LA.Matrix Double
aliasMatrix x z =
  let xtx = LA.tr x LA.<> x
      d   = LA.det xtx
  in if abs d < 1e-12
       then LA.fromLists [[]]
       else LA.inv xtx LA.<> LA.tr x LA.<> z

-- ===========================================================================
-- 内部
-- ===========================================================================

computeDiagnostics :: LA.Matrix Double -> DesignDiagnostics
computeDiagnostics x =
  let n   = LA.rows x
      p   = LA.cols x
      nD  = fromIntegral n :: Double
      pD  = fromIntegral p :: Double
      xtx = LA.tr x LA.<> x
      d   = LA.det xtx
      singular = abs d < 1e-12
      inv = if singular then LA.ident p else LA.inv xtx
      -- D-efficiency: (|XᵀX| / n^p)^{1/p}  (clamped to ≥ 0)
      dEff = if singular || d <= 0 then 0
                else (d / (nD ** pD)) ** (1 / pD)
      -- A-efficiency: p / trace((XᵀX/n)⁻¹) = p · n / trace((XᵀX)⁻¹)... wait
      -- (XᵀX / n)⁻¹ = n · (XᵀX)⁻¹  なので trace((XᵀX/n)⁻¹) = n · trace((XᵀX)⁻¹)
      -- → A-eff = p / (n · trace((XᵀX)⁻¹))
      trInv = sum [ inv `LA.atIndex` (i, i) | i <- [0 .. p - 1] ]
      aEff  = if singular || trInv == 0 then 0
                else pD / (nD * trInv)
      -- G-efficiency: p / max_i (n · h_ii)、 h_ii = x_iᵀ (XᵀX)⁻¹ x_i
      hMax = if singular then 1
               else maximum
                      [ let xi = LA.flatten (x LA.? [i])
                            v  = inv LA.#> xi
                        in xi `LA.dot` v
                      | i <- [0 .. n - 1] ]
      gEff = if hMax == 0 then 0 else pD / (nD * hMax)
      -- I-efficiency 近似 (self-moment)
      iEff = if singular then 0
               else
                 let m = LA.scale (1 / nD) xtx
                     t = LA.sumElements (LA.takeDiag (inv LA.<> m))
                 in if t == 0 then 0 else 1 / (nD * t)
  in DesignDiagnostics
       { ddVIF         = vifVector x
       , ddDEff        = dEff
       , ddAEff        = aEff
       , ddGEff        = gEff
       , ddIEff        = iEff
       , ddAliasMatrix = LA.fromLists [[]]
       }

-- | 列 j の VIF。 全 1 列 (切片) は 1 を返す。
vifForCol :: LA.Matrix Double -> Int -> Double
vifForCol x j =
  let col = LA.flatten (x LA.¿ [j])
      isConst = let c0 = LA.atIndex col 0
                in LA.maxElement (LA.cmap (\v -> abs (v - c0)) col) < 1e-12
  in if isConst then 1
       else
         let p       = LA.cols x
             others  = [ k | k <- [0 .. p - 1], k /= j ]
             xOthers = x LA.¿ others
             yj      = col
             xtx     = LA.tr xOthers LA.<> xOthers
             d       = LA.det xtx
         in if abs d < 1e-12 then 1 / 0
              else
                let beta = LA.inv xtx LA.#> (LA.tr xOthers LA.#> yj)
                    yhat = xOthers LA.#> beta
                    yBar = LA.sumElements yj / fromIntegral (LA.size yj)
                    ssR  = LA.sumElements ((yj - yhat) ^ (2 :: Int))
                    ssT  = LA.sumElements ((yj - LA.scalar yBar) ^ (2 :: Int))
                    r2   = if ssT == 0 then 0 else 1 - ssR / ssT
                in if r2 >= 1 then 1 / 0 else 1 / (1 - r2)
