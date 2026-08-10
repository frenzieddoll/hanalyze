-- |
-- Module      : Hanalyze.Model.HBM
-- Description : 多相階層ベイズモデル (Hierarchical Bayesian Model, HBM) DSL の facade
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Polymorphic Hierarchical Bayesian Model (HBM) DSL.
--
-- Phase 58 で責務別 submodule に分割済み。 本モジュールは **facade**:
-- 下位 8 module (Util/Distribution/Sampling/Model/Track/Eval/IR/Gradient) を
-- import し、 従来の公開 API を export list 経由でそのまま再公開する。
-- 既存 importer (18 src module + test) は無改修で従来通り使える。
--
-- A free-monad embedded language for probabilistic programs. The
-- continuation type is left polymorphic so that a single model term can
-- be reinterpreted as:
--
--   * a structural inspector (parameter / observation graph),
--   * a log-joint density,
--   * an automatically-differentiated log-joint
--     (via @Numeric.AD.Mode.Reverse.Double@ — Double 特化の reverse モードゆえ
--      勾配は latent 数 p に依らず ~1 sweep。 Phase 53 で forward から切替:
--      forward は勾配 1 本に p 回評価が要り階層モデルで線形悪化していた。
--      generic Reverse は tape boxing で低次元が遅く、 Reverse.Double が全 p で
--      forward/generic を上回ると実測),
--   * a dependency tracker (the 'Track' interpretation, used by
--     @Hanalyze.Viz.ModelGraph@ to build a Mermaid DAG).
--
-- See @docs/bayesian/02-probabilistic-model.md@ for an extended
-- introduction.
--
-- @
-- data ModelF a next
--   = Sample  Text (Distribution a) (a -> next)
--   | Observe Text (Distribution a) [Double] next
--   deriving Functor
-- @
--
-- ユーザーは @forall a. (Floating a, Ord a) => Model a r@ という
-- 「型に多相なモデル」を一度だけ書き、解釈時に @a@ を選ぶことで
-- 同じモデルから複数の解釈 (サンプリング・log joint・AD 勾配・依存抽出)
-- を取り出せる。
--
-- == 使い方
--
-- @
-- import Hanalyze.Model.HBM
--
-- myModel :: ModelP ()
-- myModel = do
--   mu    <- sample "mu"    (Normal 0 10)
--   sigma <- sample "sigma" (Exponential 1)
--   observe "y" (Normal mu sigma) [1.5, 2.0, 1.8]
--
-- -- 異なる解釈:
-- logVal = logJoint myModel (Map.fromList [("mu",1),("sigma",2)])  -- 数値評価
-- gVec   = gradAD myModel ["mu","sigma"] [1, 2]                    -- AD 勾配
-- deps   = extractDeps myModel                                      -- 依存関係
-- @
module Hanalyze.Model.HBM
  ( -- * Polymorphic distributions
    Distribution (..)
  , distName
  , logDensity
  , logDensityObs
  , sampleDist
  , sampleMvDist
  , distCDF
  , logCDF
  , logSF
    -- * Polymorphic model DSL
  , Free (..)
  , liftF
  , ModelF (..)
  , Model
  , ModelP
  , sample
  , observe
  , observeMV
  , observeColumns
  , observeLM
  , observeLMR
  , observeNormalLM
  , LMFamily (..)
  , REff (..)
  , REffect (..)
  , reffNames
  , reNormal
  , at
  , indexed
  , (.#)
  , potential
  , deterministic
  , runDeterministics
  , deterministicNames
  , augmentChainWithDeterministic
  , nonCenteredNormal
  , dirichlet
  , orderedCuts
  , dpStickBreaking
  , hmmLatent
  -- ** Phase 40 plate notation
  , plate
  , plateI
  , plateI_
  , plateForM
  , plateForM_
  , withPlate
  , hmmForwardLogLik
  , GlmmFamily (..)
  , glmmRandomIntercept
  , dataNamed
  , dataNamedX
  , dataNamedIx
  , dataNamedObs
  , Ix (..)
  , TrackTag (..)
  , (!!!)
  , atIx
  , withData
  , withDataIx
  , mvNormalLatent
  , mvNormalLogDensity
  , mvNormalCholLogDensity
  , multinomialLogDensity
  , mvStudentTLogDensity
  , dirichletMultinomialLogDensity
  , wishartLogDensity
  , obsLogSum
  , lkjCorrCholesky
  , gpExpQuadCov
  , gpLatent
  , ar1Latent
    -- * Structural inspection
  , Node (..)
  , NodeKind (..)
  , collectNodes
  , sampleNames
  , dataSlots
  , dataIxSlots
  , extractDeps
    -- * Type aliases
  , Params
    -- * Interpreters
  , logJoint
  , logPrior
  , logLikelihood
  , perObsLogLiks
  , runObserveDists
  , priorList
  , describeModel
    -- * Model graph (visualization)
  , ModelGraph (..)
  , buildModelGraph
  , collapseIndexedPlateNodes
    -- * AD gradient
  , gradAD
  , gradADU
  , compileGradU
  , compileGradUV
  , compileGradValUV
  , compileGradValUVM
  , compileLogPU
  , compileLogPUV
  , synthGaussLMBlocks
  , synthVecIR
  , gradPathLabel
    -- * Numeric utilities (test 用・Phase 56.1)
  , lgammaApprox
  , digamma
    -- * Constraint transforms (for HMC)
  , getTransforms
  , logJointUnconstrained
  , invTransformF
  , logJacF
    -- * Dependency-tracking interpretation
  , Track (..)
  , trackVar
  , trackConst
  ) where

-- Phase 58.2: 純粋な数値・線形代数 leaf util を分離。 internal 利用に加え
-- 'lgammaApprox' / 'digamma' は export list 経由でそのまま再エクスポートされる。
import Hanalyze.Model.HBM.Util
-- Phase 58.3/58.6a: 多相分布 ADT + 密度 + CDF を分離 (Util の上層)。 公開 API
-- (Distribution(..)/distName/logDensity/logDensityObs/obsLogSum/distCDF/logCDF/
-- logSF/MV密度群) は export list 経由でそのまま再エクスポート。 ★58.6a で事前
-- logDensity と観測 logDensityObs/obsLogSum を本体から Distribution へ集約
-- (Eval の logJoint/logPrior が logDensity を参照する back-edge を解消・密度は
-- 本来 Distribution の責務。 INLINABLE は AD cross-module inlining 維持で保持)。
import Hanalyze.Model.HBM.Distribution
-- Phase 58.4: 分布からのサンプリング (sampleDist/sampleMvDist) を分離。
-- export list 経由でそのまま再エクスポート。 PrimMonad/mwc-random 依存・非ホット。
import Hanalyze.Model.HBM.Sampling
-- Phase 58.5: 多相モデル DSL (Free monad + ModelF + plate + 構造検査) を分離。
-- 公開 API (Free/liftF/ModelF/Model/ModelP/sample/observe/plate/collectNodes 等)
-- は export list 経由でそのまま再エクスポート。
import Hanalyze.Model.HBM.Model
-- Phase 58.6b: 依存追跡型 Track (Track/trackVar/trackConst/extractDeps) を分離。
-- Model/Distribution の上層・非ホット (DAG 抽出のみ・NUTS per-draw 非経路)。
-- export list 経由でそのまま再エクスポート。
import Hanalyze.Model.HBM.Track
-- Phase 58.6c: 評価層 (ObserveLM 評価 + logJoint/logPrior/logLikelihood interp +
-- 互換 API runDeterministics/buildModelGraph 等 + runTrack) を分離。 Track の上層。
-- ★ホット (logJoint は AD 勾配経路)。 AD 勾配・IR (本体残置) は本モジュールを
-- forward import する。 公開 API は export list 経由でそのまま再エクスポート。
import Hanalyze.Model.HBM.Eval
-- Phase 58.7: IR (中間表現) 層 (affine/非線形/密度 IR) を分離。 最ホット (gradVecIR)。
import Hanalyze.Model.HBM.IR
-- Phase 58.8: AD 勾配コンパイラ層 (compileGradUV/hybridGradClosure/gaussLMBlocks/
-- 定数 prior 解析勾配/制約変換) を分離。 IR の上層・最ホット (NUTS per-draw 本経路)。
-- 公開 API (gradAD/gradADU/compileGradU/compileGradUV/compileLogPU/compileLogPUV/
-- getTransforms/logJointUnconstrained/invTransformF/logJacF) は export list 経由で再公開。
import Hanalyze.Model.HBM.Gradient
