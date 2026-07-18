{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Design.Custom.Bayesian
-- Description : Bayesian D-optimality (DuMouchel-Jones 1994) の事前精度行列ヘルパ
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Bayesian D-optimality (DuMouchel-Jones 1994) のヘルパ (Phase 26)。
--
-- spec: doe-custom-design-spec v0.1.1 §2.7。
-- 参考: DuMouchel & Jones (1994) "A Simple Bayesian Modification of D-Optimal
-- Designs to Reduce Dependence on an Assumed Model", Technometrics 36:37-47。
--
-- ## 概念
--
-- 通常の D-opt は @det(XᵀX)@ を最大化する。 Bayesian D-opt は事前情報 (= 興味の
-- 薄い高次項に対する事前分布) を K (prior precision matrix) で表現し、
-- @det(XᵀX + K)@ を最大化する。
--
-- K の典型構造 (DuMouchel-Jones):
--
--   * 主効果 / intercept: 興味あり → K_jj = 0 (= 事前情報無し)
--   * 2 因子交互作用 / 二乗項: 興味薄 → K_jj = τ² (= τ² の事前精度で「ほぼ 0」 と仮定)
--   * 非対角は 0
--
-- τ² は 「effect が 1σ_error 程度になる確信度」 から決まる、 既定 1.0 で開始して
-- 設計者が調整する慣例。
--
-- ## 使い方
--
-- @
-- import Hanalyze.Design.Custom.Bayesian
-- import Hanalyze.Design.Optimal (OptCriterion (..))
--
-- let k = priorPrecisionDefault factors model 1.0
--     spec = ... { cdsCriterion = BayesianD (precisionToMatrix k) }
-- @
module Hanalyze.Design.Custom.Bayesian
  ( PriorPrecision (..)
  , precisionToMatrix
  , priorPrecisionDefault
  , priorPrecisionFromTerms
  , bayesianDValueM
    -- * DuMouchel-Jones §2.2 規約 (Phase 28-12、 RegionMoment 再export)
  , DJTransform (..)
  , djFitTransform
  , djApplyTransform
  , djTransformColumns
  ) where

import qualified Numeric.LinearAlgebra    as LA

import           Hanalyze.Design.Custom.Factor
import           Hanalyze.Design.Custom.Model
import           Hanalyze.Design.Custom.Power (termColumnIndices, termName)
import           Hanalyze.Design.Custom.RegionMoment
                   ( DJTransform (..), djFitTransform, djApplyTransform
                   , djTransformColumns )

-- | Prior precision matrix のラッパ。 対角優位を想定するが、 一般 p × p 行列を
-- 受け入れる (DuMouchel-Jones 1994 の対角構造は最も一般的だが、 ユーザが
-- 任意の K を持ち込むのも妨げない)。
newtype PriorPrecision = PriorPrecision (LA.Matrix Double)
  deriving (Show)

-- | 内部 matrix を [[Double]] として取得 (OptCriterion.BayesianD への引き渡し用)。
precisionToMatrix :: PriorPrecision -> [[Double]]
precisionToMatrix (PriorPrecision m) = LA.toLists m

-- | DuMouchel-Jones の既定プリセット:
--
--   * intercept / 主効果: K_jj = 0
--   * 2fi (`TInter` len 2) / 二乗 (`TPower`) / nested: K_jj = τ²
--   * categorical 主効果 (K-1 列): K_jj = 0 (主効果扱い)
--
-- 非対角は全て 0。 expand 後の列順 = `expandDesignMatrix` の出力順 と一致。
priorPrecisionDefault :: [Factor] -> Model -> Double -> PriorPrecision
priorPrecisionDefault factors model tau2 =
  priorPrecisionFromTerms factors model (defaultClassifier tau2)

-- | 各 term に対する K_jj 値を返す classifier 経由で K を構築する一般版。
-- ユーザが「自分の問題では二乗だけ τ²、 2fi は 0」 などのカスタム classifier を
-- 渡せる。
priorPrecisionFromTerms
  :: [Factor]
  -> Model
  -> (ModelTerm -> Double)  -- ^ term ごとの K_jj 値
  -> PriorPrecision
priorPrecisionFromTerms factors model classifyKjj =
  let pairs   = termColumnIndices factors model
      nameMap = [ (termName t, classifyKjj t) | t <- mTerms model ]
      pTotal  = case pairs of
        [] -> 0
        _  -> 1 + maximum (concatMap snd pairs)
      diag = [ kjjForCol pairs nameMap j | j <- [0 .. pTotal - 1] ]
  in PriorPrecision (LA.diagl diag)

kjjForCol :: [(t, [Int])] -> [(t, Double)] -> Int -> Double
kjjForCol pairs nameMap col =
  case [ v | ((_, cols), (_, v)) <- zip pairs nameMap, col `elem` cols ] of
    (v:_) -> v
    []    -> 0

-- | DuMouchel-Jones 既定の classifier。
defaultClassifier :: Double -> ModelTerm -> Double
defaultClassifier _    TIntercept     = 0
defaultClassifier _    (TMain _)      = 0
defaultClassifier tau2 (TInter ns)
  | length ns >= 2 = tau2
  | otherwise      = 0
defaultClassifier tau2 (TPower _ k)
  | k >= 2 = tau2
  | otherwise = 0
defaultClassifier tau2 (TNested _ _) = tau2

-- | Bayesian D-criterion (Matrix-native): @det(XᵀX + K)@ そのもの (符号なし)。
-- K の次元が X の列数と不一致なら 0。
bayesianDValueM :: PriorPrecision -> LA.Matrix Double -> Double
bayesianDValueM (PriorPrecision km) x =
  let p = LA.cols x
  in if LA.rows km /= p || LA.cols km /= p
       then 0
       else LA.det (LA.tr x LA.<> x + km)

-- ---------------------------------------------------------------------------
-- DuMouchel-Jones §2.2 規約 (Phase 28-12) — 実装は Custom.RegionMoment.hs に
-- 移動 (Coordinate ↔ Bayesian の module cycle 回避のため)。 本 module からは
-- 再 export のみ。
-- ---------------------------------------------------------------------------
--
-- DJ (1994) §2.2 (Technometrics 36:39) は、 prior τ² が「effect size 1σ_error」
-- と等価に解釈されるよう、 potential terms (TInter len≥2 / TPower k≥2 /
-- TNested) に以下の変換を要求する:
--
--   1. **centering**: 候補集合上で平均を引く (subtract mean over candidate)
--   2. **primary との直交化**: candidate 上の primary 列 (TIntercept / TMain /
--      TInter len 1) に LS regress して残差を取る
--   3. **range = 1 への正規化**: 直交化後の値の (max - min) で割る
--
-- paper §2.2 末尾の例 (primary {1, x}、 candidate {-1, -0.5, 0, 0.5, 1} 5 水準):
--
--   * x² → z₁ = x² − 0.5 (mean(x²)=0.5、 primary 直交、 range=1)
--   * x³ → z₂ = (x³ − 0.85x)/0.6 (E[x⁴]/E[x²]=0.85、 range=0.6)
--
-- 同一 K = diag(0..0, τ²..τ²) を当てた det(X'X + K) が paper の値と一致する
-- ためにはこの規約が必要。 'priorPrecisionDefault' は K のみを構築するので、
-- ユーザは expand 後に 'djTransformColumns' で列変換を適用してから
-- 'bayesianDValueM' を呼ぶ。 coordinateExchange への自動適用は未対応 (Phase 28-12
-- 範囲外)。

-- (実装は Custom.RegionMoment.hs を参照。 本 module からは re-export のみ)
