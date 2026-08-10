{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
-- |
-- Module      : Hanalyze.Model.LiNGAM.VAR
-- Description : VAR-LiNGAM (Hyvärinen et al. 2010) — 時系列データに対する LiNGAM 拡張
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: VAR-LiNGAM (Hyvärinen et al. 2010): 時系列データに対する LiNGAM 拡張。
--
-- ## モデル
--
-- 観測時系列 Y_t ∈ ℝ^K は以下の SVAR (構造 VAR) に従う:
--
-- > Y_t = Σ_{l=1..p} A_l^* · Y_{t-l} + B_0 · Y_t + e_t
--
-- ここで B_0 は同時刻因果 (contemporaneous causal effect、 acyclic + LiNGAM)、
-- e_t は非ガウシアン独立 noise。 通常の reduced-form VAR(p) と関係:
--
-- > Y_t = Σ_l A_l · Y_{t-l} + u_t,   u_t = (I - B_0)⁻¹ · e_t
--
-- なので u_t に LiNGAM を適用すれば B_0 が求まり、 A_l^* も A_l と B_0 から
-- 回収できる。
--
-- ## アルゴリズム
--
-- 1. 'Hanalyze.Model.VAR.fitVAR' で reduced-form VAR(p) を fit
-- 2. 残差 u_t (= @varResiduals@) に @fitDirectLiNGAM@ を適用 → B_0 と
--    causal order を取得
-- 3. 構造 lag 行列を A_l^* = (I - B_0) · A_l で復元 (l=1..p)
--
-- ## リファレンス
--
-- Hyvärinen et al. (2010) "Estimation of a Structural Vector Autoregression
-- Model Using Non-Gaussianity", JMLR 11. Python 実装は cdt15/lingam の
-- `lingam/var_lingam.py`。
--
-- [English]: VAR-LiNGAM (Hyvärinen et al. 2010): a LiNGAM extension for
-- time-series data.
--
-- ## Model
--
-- The observed time series Y_t ∈ ℝ^K follows the SVAR (structural VAR)
-- below:
--
-- > Y_t = Σ_{l=1..p} A_l^* · Y_{t-l} + B_0 · Y_t + e_t
--
-- where B_0 is the contemporaneous causal effect (acyclic + LiNGAM),
-- and e_t is non-Gaussian independent noise. Related to the ordinary
-- reduced-form VAR(p):
--
-- > Y_t = Σ_l A_l · Y_{t-l} + u_t,   u_t = (I - B_0)⁻¹ · e_t
--
-- so applying LiNGAM to u_t recovers B_0, and A_l^* can also be
-- recovered from A_l and B_0.
--
-- ## Algorithm
--
-- 1. Fit the reduced-form VAR(p) with 'Hanalyze.Model.VAR.fitVAR'
-- 2. Apply @fitDirectLiNGAM@ to the residuals u_t (= @varResiduals@) →
--    obtain B_0 and the causal order
-- 3. Recover the structural lag matrices as A_l^* = (I - B_0) · A_l
--    (l=1..p)
--
-- ## References
--
-- Hyvärinen et al. (2010) "Estimation of a Structural Vector
-- Autoregression Model Using Non-Gaussianity", JMLR 11. The Python
-- implementation is cdt15/lingam's `lingam/var_lingam.py`.
module Hanalyze.Model.LiNGAM.VAR
  ( VARLiNGAMConfig (..)
  , VARLiNGAMFit (..)
  , defaultVARLiNGAMConfig
  , fitVARLiNGAM
  , vlDAG
  ) where

import qualified Numeric.LinearAlgebra as LA

import qualified Hanalyze.Model.VAR           as V
import qualified Hanalyze.Model.LiNGAM.Direct as DL
import qualified Hanalyze.Model.DAG           as DAG

-- ===========================================================================
-- 設定 / 結果
-- ===========================================================================

data VARLiNGAMConfig = VARLiNGAMConfig
  { vlcLagOrder  :: !Int
    -- ^ [日本語]: VAR の lag 数 p (≥ 1)。 [English]: The VAR lag count p (≥ 1).
  , vlcDirectCfg :: !DL.DirectLiNGAMConfig
  } deriving (Show)

defaultVARLiNGAMConfig :: VARLiNGAMConfig
defaultVARLiNGAMConfig = VARLiNGAMConfig
  { vlcLagOrder  = 1
  , vlcDirectCfg = DL.defaultDirectLiNGAMConfig
  }

data VARLiNGAMFit = VARLiNGAMFit
  { vlVARFit          :: !V.VARFit
    -- ^ [日本語]: reduced-form VAR(p) fit 結果。 [English]: The reduced-form VAR(p) fit result.
  , vlContempLiNGAM   :: !DL.DirectLiNGAMFit
    -- ^ [日本語]: 残差 u_t に対する DirectLiNGAM 結果 (= 同時刻因果 B_0)。 [English]: The DirectLiNGAM result on the residuals u_t (= the contemporaneous causal effect B_0).
  , vlB0              :: !(LA.Matrix Double)
    -- ^ [日本語]: 同時刻因果係数 (K × K)、 = vlContempLiNGAM の dlB。 [English]: The contemporaneous causal coefficients (K × K), = vlContempLiNGAM's dlB.
  , vlStructuralLags  :: ![LA.Matrix Double]
    -- ^ [日本語]: 構造 lag 行列 A_l^* = (I - B_0) · A_l (length = p)。 [English]: The structural lag matrices A_l^* = (I - B_0) · A_l (length = p).
  , vlContempOrder    :: ![Int]
  } deriving (Show)

-- ===========================================================================
-- 主実装
-- ===========================================================================

fitVARLiNGAM :: VARLiNGAMConfig -> LA.Matrix Double -> VARLiNGAMFit
fitVARLiNGAM cfg y =
  let !varFit = V.fitVAR (vlcLagOrder cfg) y
      !resid  = V.varResiduals varFit
      !lgFit  = DL.fitDirectLiNGAM (vlcDirectCfg cfg) resid
      !b0     = DL.dlB lgFit
      !k      = V.varK varFit
      !iMinusB0 = LA.ident k - b0
      !structLags =
        [ iMinusB0 LA.<> al | al <- V.varCoefs varFit ]
  in VARLiNGAMFit
       { vlVARFit         = varFit
       , vlContempLiNGAM  = lgFit
       , vlB0             = b0
       , vlStructuralLags = structLags
       , vlContempOrder   = DL.dlOrder lgFit
       }

-- | [日本語]: 同時刻因果 (B_0) の DAG 表現を返す。 lag 部分は含まない (時間方向は別の
--   表現が必要、 v0.1 では同時刻のみ DAG 化)。
--   [English]: Returns the DAG representation of the contemporaneous
--   causal effect (B_0). Does not include the lag part (the time
--   direction needs a separate representation; v0.1 only DAG-izes the
--   contemporaneous part).
vlDAG :: VARLiNGAMConfig -> VARLiNGAMFit -> DAG.DAG
vlDAG cfg fit = DAG.fromBMatrix (DL.dlcPruneThr (vlcDirectCfg cfg)) (vlB0 fit)
