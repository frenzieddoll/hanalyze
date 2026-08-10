{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Hanalyze.Model.HBM.Eval
-- Description : HBM のモデル評価層 (log-joint/尤度インタープリタ + DAG 構築)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: モデル評価層を 'Hanalyze.Model.HBM' から分離したもの。
--
--   PPL の __評価層__ (記述層 'Hanalyze.Model.HBM.Model' の上):
--
--   - 構造化線形予測子 observe ('ObserveLM') の評価 (lmObsLogSum 等)
--   - log-joint / log-prior / log-likelihood の多相インタープリタ
--   - Gibbs 共役検出向けの runObserveDists / priorList
--   - 派生量評価 (runDeterministics / augmentChainWithDeterministic) と
--     DAG 構築 (buildModelGraph / collapseIndexedPlateNodes)
--
--   依存は下層 Model / Distribution (密度) / Track (extractDeps) / Util /
--   MCMC.Core のみ。 AD 勾配・IR は __上層__ に置かれ本モジュールへ依存する
--   (一方向)。
-- [English]: The model evaluation layer, split out from
--   'Hanalyze.Model.HBM'.
--
--   The PPL's __evaluation layer__ (built on top of the description layer
--   'Hanalyze.Model.HBM.Model'):
--
--   - Evaluation of the structured linear-predictor observe ('ObserveLM')
--     (lmObsLogSum, etc.)
--   - Polymorphic interpreters for log-joint / log-prior / log-likelihood
--   - runObserveDists / priorList for Gibbs conjugacy detection
--   - Derived-quantity evaluation (runDeterministics /
--     augmentChainWithDeterministic) and DAG construction
--     (buildModelGraph / collapseIndexedPlateNodes)
--
--   Depends only on the lower layers Model / Distribution (densities) /
--   Track (extractDeps) / Util / MCMC.Core. The AD gradient / IR live in
--   the __upper__ layer and depend on this module (one direction only).
module Hanalyze.Model.HBM.Eval
  ( -- * ObserveLM 評価
    lmObsLogSum
    -- * Interpreters
  , logJoint
  , logPrior
  , logPriorWith
  , logLikelihood
  , perObsLogLiks
  , runObserveDists
  , mvNormalObserveOf
  , priorList
  , describeModel
    -- * Type aliases
  , Params
    -- * 派生量
  , runDeterministics
  , deterministicNames
  , augmentChainWithDeterministic
    -- * Model graph (visualization)
  , ModelGraph (..)
  , buildModelGraph
  , collapseIndexedPlateNodes
  ) where

import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Hanalyze.MCMC.Core (Chain (..))
import Hanalyze.Model.HBM.Util (negInf, chunksOf)
import Hanalyze.Model.HBM.Distribution
import Hanalyze.Model.HBM.Model
import Hanalyze.Model.HBM.Track (Track, extractDeps)

-- ---------------------------------------------------------------------------
-- ObserveLM (構造化線形予測子 observe) の評価 (Phase 54.1)
-- ---------------------------------------------------------------------------

-- | [日本語]: 線形予測子 η_i = Σ_j β_j·X_ij。
--   synthGaussLMBlocks (本体) / IR が AD で微分しながら呼ぶホット経路。
--   monolith では同一モジュール inline されていた。 境界跨ぎで失われると M1/M2 が
--   約 +25% 劣化する (bench で実測) ため INLINABLE で cross-module inline を維持。
--   [English]: The linear predictor η_i = Σ_j β_j·X_ij.
--
--   A hot path called by synthGaussLMBlocks (the main body) / IR while
--   differentiating via AD. It used to be inlined in the same module in
--   the monolith. Losing it across a module boundary degrades M1/M2 by
--   about +25% (measured by bench), so INLINABLE keeps the cross-module
--   inline.
{-# INLINABLE lmEta #-}
lmEta :: Fractional a => [a] -> [Double] -> a
lmEta betas xrow = sum (zipWith (\b x -> b * realToFrac x) betas xrow)

-- | [日本語]: ランダム効果項の per-obs 寄与 @Σ_re w_i·u^{re}[gid_i]@ (長さ n)。
--   重み @Nothing@ = 全 1。
--   [English]: The per-observation contribution of the random-effect term
--   @Σ_re w_i·u^{re}[gid_i]@ (length n). Weight @Nothing@ = all 1s.
{-# INLINABLE lmReffEta #-}
lmReffEta :: forall a. Fractional a => [REff] -> Int -> Map Text a -> [a]
lmReffEta reffs n params =
  foldr (zipWith (+)) (replicate n 0)
    [ let uvals = [ Map.findWithDefault 0 nm params | nm <- uNames ]
          base  = [ uvals !! g | g <- gids ]
      in case mw of
           Nothing -> base
           Just ws -> zipWith (\v w -> v * realToFrac w) base ws
    | REff uNames gids _ mw _ <- reffs ]

-- | [日本語]: 'ObserveLM' ブロックの各観測の log-density (per-obs)。 param Map から
--   β / u / (Gaussian の) σ を名前で引く。 η_i = Σ_j β_j X_ij + Σ_re u^{re}[gid_i]
--   を scalar 経路と同じ式で評価する。
--   [English]: The per-observation log-density of an 'ObserveLM' block.
--   Looks up β / u / (Gaussian's) σ by name from the param Map, and
--   evaluates η_i = Σ_j β_j X_ij + Σ_re u^{re}[gid_i] with the same
--   formula as the scalar path.
{-# INLINABLE lmObsLogLiks #-}
lmObsLogLiks :: forall a. (Floating a, Ord a)
             => [Text] -> [[Double]] -> [REff] -> LMFamily -> [Double] -> Map Text a -> [a]
lmObsLogLiks betaNames designX reffs fam ys params =
  let betas = [ Map.findWithDefault 0 n params | n <- betaNames ]
      reEta = lmReffEta reffs (length ys) params
      etas  = zipWith (\xr re -> lmEta betas xr + re) designX reEta
      rows  = zip etas ys
  in case fam of
       LMGaussian sName ->
         let sigma = Map.findWithDefault 0 sName params
         in [ logDensityObs (Normal eta sigma) y | (eta, y) <- rows ]
       LMPoisson ->
         [ logDensityObs (Poisson (exp eta)) y | (eta, y) <- rows ]
       LMBernoulli ->
         [ logDensityObs (Bernoulli (1 / (1 + exp (negate eta)))) y
         | (eta, y) <- rows ]

-- | [日本語]: 'ObserveLM' ブロックの log-likelihood 和。
--   [English]: The sum of the log-likelihood of an 'ObserveLM' block.
{-# INLINABLE lmObsLogSum #-}
lmObsLogSum :: (Floating a, Ord a)
            => [Text] -> [[Double]] -> [REff] -> LMFamily -> [Double] -> Map Text a -> a
lmObsLogSum betaNames designX reffs fam ys params =
  sum (lmObsLogLiks betaNames designX reffs fam ys params)

-- ---------------------------------------------------------------------------
-- 評価インタープリタ
-- ---------------------------------------------------------------------------

-- | [日本語]: log-joint @log p(θ, y)@ を計算する多相インタープリタ。
--   引数 @a@ を @Double@ にすると数値評価、@Reverse s Double@ にすると AD 評価が可能。
--   [English]: Polymorphic interpreter that computes the log-joint
--   @log p(θ, y)@. Instantiating the argument @a@ with @Double@ gives
--   numeric evaluation; with @Reverse s Double@, AD evaluation.
logJoint :: (Floating a, Ord a) => Model a r -> Map Text a -> a
logJoint model params = go model 0
  where
    go (Pure _) acc = acc
    go (Free (Sample n d k)) acc =
      case Map.lookup n params of
        Nothing  -> negInf
        Just v   ->
          let lp = logDensity d v
          in go (k v) (acc + lp)
    go (Free (Observe _ d ys next)) acc =
      let ll = obsLogSum d ys
      in go next (acc + ll)
    go (Free (ObserveLM _ bs xs re fam ys next)) acc =
      go next (acc + lmObsLogSum bs xs re fam ys params)
    go (Free (Potential _ v next)) acc = go next (acc + v)
    go (Free (Deterministic _ v k)) acc = go (k v) acc
    -- Phase 60.2: Data 継続は [a] を受ける (lazy list・消費 1 回で O(n)/eval)
    go (Free (Data _ ys k)) acc = go (k (map realToFrac ys, ys)) acc
    go (Free (DataIx _ is k)) acc = go (k is) acc
    go (Free (PlateBegin _ _ next)) acc = go next acc
    go (Free (PlateEnd next))       acc = go next acc

-- | [日本語]: log p(θ) のみ (prior 部分)。
--   [English]: Just log p(θ) (the prior part).
logPrior :: (Floating a, Ord a) => Model a r -> Map Text a -> a
logPrior = logPriorWith logDensity

-- | [日本語]: 'logPrior' の密度関数注入版。 AD 経路が定数 hyperparameter の
--   lgamma 正規化項を Double へ畳み込む 'logDensityRD' を差し込むために使う
--   (@Gradient@ の fRest 参照)。 @logPriorWith logDensity@ = 従来の 'logPrior'。
--   [English]: A density-function-injectable version of 'logPrior'. Used
--   to plug in 'logDensityRD', which folds the lgamma normalization term
--   of constant hyperparameters into a @Double@ on the AD path
--   (referenced by @Gradient@'s fRest). @logPriorWith logDensity@ is
--   equivalent to the plain 'logPrior'.
logPriorWith :: (Floating a, Ord a)
             => (Distribution a -> a -> a) -> Model a r -> Map Text a -> a
logPriorWith density model params = go model 0
  where
    go (Pure _) acc = acc
    go (Free (Sample n d k)) acc =
      case Map.lookup n params of
        Nothing -> negInf
        Just v  -> go (k v) (acc + density d v)
    go (Free (Observe _ _ _ next)) acc = go next acc
    go (Free (ObserveLM _ _ _ _ _ _ next)) acc = go next acc  -- prior 部分には寄与しない
    go (Free (Potential _ v next)) acc = go next (acc + v)
    go (Free (Deterministic _ v k)) acc = go (k v) acc
    go (Free (Data _ ys k)) acc = go (k (map realToFrac ys, ys)) acc
    go (Free (DataIx _ is k)) acc = go (k is) acc
    go (Free (PlateBegin _ _ next)) acc = go next acc
    go (Free (PlateEnd next))       acc = go next acc

-- | [日本語]: log p(y | θ) のみ (likelihood 部分)。
--   [English]: Just log p(y | θ) (the likelihood part).
logLikelihood :: (Floating a, Ord a) => Model a r -> Map Text a -> a
logLikelihood model params = go model 0
  where
    go (Pure _) acc = acc
    go (Free (Sample n _ k)) acc =
      case Map.lookup n params of
        Nothing -> go (k 0) acc
        Just v  -> go (k v) acc
    go (Free (Observe _ d ys next)) acc =
      let ll = obsLogSum d ys
      in go next (acc + ll)
    go (Free (ObserveLM _ bs xs re fam ys next)) acc =
      go next (acc + lmObsLogSum bs xs re fam ys params)
    go (Free (Potential _ _ next)) acc = go next acc   -- Potential は事前項とみなす
    go (Free (Deterministic _ v k)) acc = go (k v) acc
    go (Free (Data _ ys k)) acc = go (k (map realToFrac ys, ys)) acc
    go (Free (DataIx _ is k)) acc = go (k is) acc
    go (Free (PlateBegin _ _ next)) acc = go next acc
    go (Free (PlateEnd next))       acc = go next acc

-- | [日本語]: 各 observe ノードについて、 現在のパラメータ値で評価した分布を
--   観測データと共に返す。 Gibbs サンプラーが共役構造を検出する際に、
--   潜在変数の現在値に対する観測分布のパラメータを得るために使う
--   (Double 特殊化版)。
--
--   例: @y ~ Normal(mu, sigma)@ で @ps = {mu=2, sigma=0.5}@ を渡すと
--   @[(\"y\", Normal 2 0.5, [...])]@ を返す。
--   [English]: For each observe node, return its distribution evaluated at
--   the current parameter values together with the observed data. Used
--   by the Gibbs sampler when detecting conjugate structure, to obtain the
--   observation distribution's parameters at the latent variables' current
--   values (the @Double@-specialized form).
--
--   Example: given @y ~ Normal(mu, sigma)@ and @ps = {mu=2, sigma=0.5}@,
--   returns @[(\"y\", Normal 2 0.5, [...])]@.
runObserveDists :: Model Double r
                -> Map Text Double
                -> [(Text, Distribution Double, [Double])]
runObserveDists (Pure _) _ = []
runObserveDists (Free (Sample n _ k)) ps =
  runObserveDists (k (Map.findWithDefault 0 n ps)) ps
runObserveDists (Free (Observe n d ys next)) ps =
  (n, d, ys) : runObserveDists next ps
runObserveDists (Free (ObserveLM _ _ _ _ _ _ next)) ps =
  -- ObserveLM は per-obs で μ が異なり単一 Distribution に収まらない。
  -- Gibbs 共役検出 (この関数の用途) の対象外ゆえスキップ。
  runObserveDists next ps
runObserveDists (Free (Potential _ _ next)) ps =
  runObserveDists next ps
runObserveDists (Free (Deterministic _ v k)) ps =
  runObserveDists (k v) ps
runObserveDists (Free (Data _ ys k)) ps =
  runObserveDists (k (ys, ys)) ps
runObserveDists (Free (DataIx _ is k)) ps =
  runObserveDists (k is) ps
runObserveDists (Free (PlateBegin _ _ next)) ps = runObserveDists next ps
runObserveDists (Free (PlateEnd next))       ps = runObserveDists next ps

-- | [日本語]: 解析随伴 (detach) パスの適格判定 + 抽出。
--   モデルの尤度項が __ちょうど 1 個の 'MvNormal' observe__ のみ (他 'Observe' /
--   'ObserveLM' 無し) のとき、その @(μ, Σ, ys)@ を __現在の param 値で評価__ して
--   返す。 それ以外は 'Nothing' (= 呼び出し側は従来の walk+ad / vecIR 経路へ)。
--
--   多相 (@Floating a@) ゆえ Double でも AD 型でも走らせられる: Double 版で
--   LAPACK 用の Σ⁻¹/logdet を作り (G,h 定数化)、 AD 版で surrogate @<G,Σ(θ)>@ を
--   微分する (@Gradient.compileGradUV@ の解析枝)。 walk は 'logJoint' 等と同一
--   (Sample 継続に @params Map.! name@ を流す)。 μ/Σ は Observe ノードの
--   'Distribution' に格納された式ゆえ、 現在の param 値で lazy に具体化される。
--
--   適格条件を __1 個の MvNormal に限定__するのは正しさのため: 尤度が MvNormal
--   単独なら @grad(logPrior+logJac) + detach(observe)@ で厳密に総勾配を再構成できる
--   (@logJoint = logPrior + logLikelihood@・@logLikelihood = obsLogSum(MvNormal)@)。
--   [English]: Eligibility check + extraction for the analytic adjoint
--   (detach) path.
--
--   When the model's likelihood term consists of
--   __exactly one 'MvNormal' observe__ (no other 'Observe' / 'ObserveLM'),
--   returns its @(μ, Σ, ys)@ __evaluated at the current param values__.
--   Otherwise 'Nothing' (the caller then falls back to the usual
--   walk+ad / vecIR path).
--
--   Being polymorphic (@Floating a@) lets it run with either Double or an
--   AD type: the Double version builds Σ⁻¹/logdet for LAPACK (fixing G, h
--   as constants), and the AD version differentiates the surrogate
--   @<G,Σ(θ)>@ (the analytic branch of @Gradient.compileGradUV@). The walk
--   is the same as 'logJoint' etc. (feeding @params Map.! name@ into the
--   Sample continuation). μ/Σ come from the expression stored in the
--   Observe node's 'Distribution', so they are lazily materialized at the
--   current param values.
--
--   The eligibility condition is __restricted to exactly 1 MvNormal__ for
--   correctness: only when the likelihood is a single MvNormal can the
--   total gradient be exactly reconstructed as
--   @grad(logPrior+logJac) + detach(observe)@ (since
--   @logJoint = logPrior + logLikelihood@ and
--   @logLikelihood = obsLogSum(MvNormal)@).
mvNormalObserveOf :: (Floating a, Ord a)
                  => Model a r -> Map Text a -> Maybe ([a], [[a]], [Double])
mvNormalObserveOf model params =
  case go model of
    Just [(MvNormal mu cov, ys)] -> Just (mu, cov, ys)
    _                            -> Nothing
  where
    -- Observe ノードの (dist, ys) を集める。 ObserveLM が在れば失格 (Nothing)。
    go (Pure _) = Just []
    go (Free (Sample n _ k)) =
      case Map.lookup n params of
        Nothing -> Nothing              -- param 欠落 = 失格 (通常起きない)
        Just v  -> go (k v)
    go (Free (Observe _ d ys next)) = ((d, ys) :) <$> go next
    go (Free (ObserveLM {}))        = Nothing          -- 構造化尤度は対象外
    go (Free (Potential _ _ next))  = go next          -- prior 側 (logPrior が処理)
    go (Free (Deterministic _ v k)) = go (k v)
    go (Free (Data _ ys k))         = go (k (map realToFrac ys, ys))
    go (Free (DataIx _ is k))       = go (k is)
    go (Free (PlateBegin _ _ next)) = go next
    go (Free (PlateEnd next))       = go next

-- | [日本語]: 各 sample ノードについて @(name, prior distribution)@ を
--   @Double@ 特殊化形式で返す。
--   Gibbs サンプラーの共役検出で「この潜在変数の事前は Gamma か Beta か」を
--   判定するために使う。継続値はプレースホルダ 0 を流す。
--   [English]: For each sample node, return @(name, prior distribution)@ in
--   the @Double@-specialized form. Used by the Gibbs sampler's conjugacy
--   detection to determine "is this latent variable's prior a Gamma or a
--   Beta?". Feeds the placeholder 0 into the continuation.
priorList :: Model Double r -> [(Text, Distribution Double)]
priorList (Pure _) = []
priorList (Free (Sample n d k)) = (n, d) : priorList (k 0)
priorList (Free (Observe _ _ _ next)) = priorList next
priorList (Free (ObserveLM _ _ _ _ _ _ next)) = priorList next
priorList (Free (Potential _ _ next)) = priorList next
priorList (Free (Deterministic _ v k)) = priorList (k v)
priorList (Free (Data _ ys k)) = priorList (k (ys, ys))
priorList (Free (DataIx _ is k)) = priorList (k is)
priorList (Free (PlateBegin _ _ next)) = priorList next
priorList (Free (PlateEnd next))       = priorList next

-- ---------------------------------------------------------------------------
-- 互換 API
-- ---------------------------------------------------------------------------

-- | [日本語]: パラメータ名 → 値 のマップ (constrained 空間)。
--   [English]: A map from parameter name to value (in the constrained space).
type Params = Map Text Double

-- | [日本語]: Per-observation log-likelihood (WAIC / LOO-CV で使用)。
--   各 Observe ノードのすべての観測値の logDensity を平坦リストで返す。
--   [English]: Per-observation log-likelihood (used by WAIC / LOO-CV).
--   Returns the logDensity of every observation of every Observe node as
--   a flat list.
perObsLogLiks :: forall r. ModelP r -> Params -> [Double]
perObsLogLiks m params = go m []
  where
    go :: Model Double r -> [Double] -> [Double]
    go (Pure _) acc = reverse acc
    go (Free (Sample n _ k)) acc =
      go (k (Map.findWithDefault 0 n params)) acc
    go (Free (Observe _ d ys next)) acc =
      let lls = case d of
            MvNormal mu cov ->
              let k = length mu
              in [ mvNormalLogDensity mu cov (map realToFrac yv :: [Double])
                 | yv <- chunksOf k ys ]
            Multinomial nn pp ->
              let k = length pp
              in [ multinomialLogDensity nn pp yv | yv <- chunksOf k ys ]
            _ -> [ logDensityObs d y | y <- ys ]
      in go next (reverse lls ++ acc)
    go (Free (ObserveLM _ bs xs re fam ys next)) acc =
      let lls = lmObsLogLiks bs xs re fam ys params
      in go next (reverse lls ++ acc)
    go (Free (Potential _ _ next)) acc = go next acc
    go (Free (Deterministic _ v k)) acc = go (k v) acc
    go (Free (Data _ ys k)) acc = go (k (ys, ys)) acc
    go (Free (DataIx _ is k)) acc = go (k is) acc
    go (Free (PlateBegin _ _ next)) acc = go next acc
    go (Free (PlateEnd next))       acc = go next acc

-- | [日本語]: 全ての 'Deterministic' ノードを評価し、 導出量の @Map@ を返す。
--
--   @params@ は latent 変数 (sample) の値を表す Map。Deterministic は
--   それらから導出される量で、ここでは Double 特殊化で評価する。
--   [English]: Evaluate every 'Deterministic' node and return the
--   resulting derived-quantity @Map@.
--
--   @params@ is a Map representing the values of the latent variables
--   (sample). Deterministic quantities are derived from them, evaluated
--   here in the @Double@-specialized form.
runDeterministics :: forall r. ModelP r -> Params -> Map Text Double
runDeterministics m params = go m Map.empty
  where
    go :: Model Double r -> Map Text Double -> Map Text Double
    go (Pure _) acc = acc
    go (Free (Sample n _ k)) acc =
      go (k (Map.findWithDefault 0 n params)) acc
    go (Free (Observe _ _ _ next)) acc = go next acc
    go (Free (ObserveLM _ _ _ _ _ _ next)) acc = go next acc
    go (Free (Potential _ _ next)) acc = go next acc
    go (Free (Deterministic n v k)) acc =
      go (k v) (Map.insert n v acc)
    go (Free (Data _ ys k)) acc = go (k (ys, ys)) acc
    go (Free (DataIx _ is k)) acc = go (k is) acc
    go (Free (PlateBegin _ _ next)) acc = go next acc
    go (Free (PlateEnd next))       acc = go next acc

-- | [日本語]: モデル中の 'Deterministic' 宣言名を宣言順で列挙する。
--   同名の重複宣言 (plate 内反復等) は初出のみ残す。'collectNodes' は
--   Deterministic を素通しして 'Node' 化しないため専用 walker で拾う。
--   'runDeterministics' の返す Map の key 集合と一致する (順序のみ異なる)。
--   [English]: Enumerates the model's 'Deterministic' declaration names in
--   declaration order. Duplicate declarations of the same name (e.g. plate
--   iteration) keep only the first occurrence. Since 'collectNodes' passes
--   Deterministic through without turning it into a 'Node', a dedicated
--   walker collects it here. Matches the key set of the Map returned by
--   'runDeterministics' (only the order differs).
deterministicNames :: forall r. ModelP r -> [Text]
deterministicNames m = nub (go m [])
  where
    go :: Model Double r -> [Text] -> [Text]
    go (Pure _) acc = reverse acc
    go (Free (Sample n _ k)) acc = go (k 0) acc   -- placeholder 0 (collectNodes 同型)
    go (Free (Observe _ _ _ next)) acc = go next acc
    go (Free (ObserveLM _ _ _ _ _ _ next)) acc = go next acc
    go (Free (Potential _ _ next)) acc = go next acc
    go (Free (Deterministic n v k)) acc = go (k v) (n : acc)
    go (Free (Data _ ys k)) acc = go (k (ys, ys)) acc
    go (Free (DataIx _ is k)) acc = go (k is) acc
    go (Free (PlateBegin _ _ next)) acc = go next acc
    go (Free (PlateEnd next))       acc = go next acc

-- | [日本語]: 全 posterior サンプルに対して 'runDeterministics' を評価し、
--   結果を 'chainSamples' の Map にマージした新しい Chain を返す。
--   これにより @chainVals@ / @posteriorSummary@ などのヘルパで派生量を
--   そのまま参照できる。
--   [English]: Evaluate 'runDeterministics' on every posterior sample and
--   return a new Chain with the results merged into 'chainSamples's Map.
--   This lets helpers such as @chainVals@ / @posteriorSummary@ reference
--   derived quantities directly.
augmentChainWithDeterministic :: ModelP r -> Chain -> Chain
augmentChainWithDeterministic m ch =
  let aug ps = Map.union (runDeterministics m ps) ps
  in ch { chainSamples = map aug (chainSamples ch) }

-- | Human-readable summary of the model structure (no inference is run).
describeModel :: ModelP r -> Text
describeModel m = T.unlines (header : map fmtNode (collectNodes m))
  where
    header = "Model nodes:"
    fmtNode n = case nodeKind n of
      LatentN       -> "  [latent]   " <> nodeName n <> " ~ " <> nodeDist n
      ObservedN k   -> "  [observed] " <> nodeName n <> " ~ " <> nodeDist n
                    <> "  (n=" <> T.pack (show k) <> ")"
      DeterministicN -> "  [determ]   " <> nodeName n <> " = " <> nodeDist n
      DataN k        -> "  [data]     " <> nodeName n
                    <> "  (n=" <> T.pack (show k) <> ")"

-- | DAG representation of the model. Edges are derived automatically by
-- 'extractDeps'.
data ModelGraph = ModelGraph
  { mgNodes  :: [Node]
  , mgEdges  :: [(Text, Text)]   -- (parent, child)
  , mgPlates :: Map Text Int     -- Phase 40: plate 名 → サイズ N
  } deriving (Show)

-- | [日本語]: Plate 内の indexed RV (`eta_0, eta_1, …, eta_{n-1}`) を
--   __代表 1 ノードに集約__して、 PyMC `pm.model_to_graphviz` 流の true plate
--   描画用に変換する。
--
--   集約条件 (heuristic):
--
--   - 同じ `nodePlates` (= plate スタック) に属する
--   - 名前が @\<prefix\>_\<digit+\>$@ パターン (末尾が _ + 数字)
--   - 同じ @prefix@ を持つノード群が 2 個以上
--   - 同じ `nodeDist` (= 分布名が一致)
--
--   集約結果:
--
--   - 代表ノード名は @prefix@ (例: @eta_0..eta_7@ → @eta@)
--   - `nodeKind`: 元の集合内で最初の出現を維持 (LatentN / ObservedN)。
--     ObservedN の場合は観測数を全集約 (Σ)
--   - `nodeDeps`: 全集合の親集合の和 (ただし、 同じ集合内のメンバ間 deps は
--     削除 — 自己集約のため)
--   - edges: 集約後の名前で dedupe
--
--   plate 文脈外で起きる「同じ命名規則の名前衝突」 (e.g. @beta_0@ 固定効果 vs
--   @u_0@ 群効果) はこの heuristic で誤って集約されない (plate 制約)。
--
--   元 graph をそのまま渡せば不変 (idempotent)。 plate に属さない / 単独
--   のノードは触らない。
--   [English]: Converts the indexed RVs within a plate (`eta_0, eta_1, …,
--   eta_{n-1}`) by __collapsing them into a single representative node__,
--   for PyMC `pm.model_to_graphviz`-style true plate rendering.
--
--   Collapse conditions (heuristic):
--
--   - Belong to the same `nodePlates` (= plate stack)
--   - Name matches the @\<prefix\>_\<digit+\>$@ pattern (ends in _ + digits)
--   - 2 or more nodes share the same @prefix@
--   - Share the same `nodeDist` (= same distribution name)
--
--   Collapse result:
--
--   - The representative node's name is @prefix@ (e.g. @eta_0..eta_7@ →
--     @eta@)
--   - `nodeKind`: keeps the first occurrence within the original set
--     (LatentN / ObservedN). For ObservedN, the observation counts are
--     summed (Σ)
--   - `nodeDeps`: the union of the parent sets of the whole set (deps
--     between members of the same set are removed, since that would be
--     self-collapse)
--   - edges: deduped under the collapsed names
--
--   A "name collision under the same naming convention" occurring outside
--   a plate context (e.g. @beta_0@ a fixed effect vs @u_0@ a group effect)
--   is not mistakenly collapsed by this heuristic (the plate constraint).
--
--   Passing the original graph through unchanged is a no-op (idempotent).
--   Nodes that don't belong to a plate, or stand alone, are untouched.
collapseIndexedPlateNodes :: ModelGraph -> ModelGraph
collapseIndexedPlateNodes mg0 =
  -- 不動点: 1 回の集約で取りこぼした多段 plate (e.g. y_0_0..y_2_1 → y_0..y_2 →
  -- 残り index suffix を持つ → y) を順次潰す。 mgNodes 数が減らなくなれば終了。
  let step g = collapseIndexedPlateNodesOnce g
      iter g = let g' = step g in if length (mgNodes g') == length (mgNodes g)
                                    then g else iter g'
  in iter mg0

-- | [日本語]: `collapseIndexedPlateNodes` の 1 段集約 (内部、 不動点を作る材料)。
--   [English]: One step of `collapseIndexedPlateNodes`'s collapsing
--   (internal; the building block used to reach the fixed point).
collapseIndexedPlateNodesOnce :: ModelGraph -> ModelGraph
collapseIndexedPlateNodesOnce mg =
  let ns        = mgNodes mg
      es        = mgEdges mg
      -- 1. 各ノードについて (plate path, prefix) または Nothing を計算
      keyOf n = case T.breakOnEnd "_" (nodeName n) of
        (pre, digits)
          | not (T.null pre) && not (T.null digits)
            && T.all (`elem` ("0123456789" :: String)) digits ->
              Just (nodePlates n, T.init pre)  -- _ を除いた prefix
        _ -> Nothing
      -- 2. キー単位で groupings
      keyed = [(keyOf n, n) | n <- ns]
      -- 3. グループ化 (Just key) のみ、 Nothing は単独
      grouped :: Map.Map ([Text], Text) [Node]
      grouped = Map.fromListWith (flip (++))
        [ (k, [n]) | (Just k, n) <- keyed ]
      -- 4. 集約候補: size ≥ 2 かつ全 nodeDist 一致
      collapsible = Map.filter
        (\g -> length g >= 2
            && all (\n -> nodeDist n == nodeDist (head g)) g)
        grouped
      -- 5. name → 代表名 のマップ
      nameMap :: Map.Map Text Text
      nameMap = Map.fromList
        [ (nodeName n, prefix)
        | ((_plates, prefix), grp) <- Map.toList collapsible
        , n <- grp
        ]
      mapName n = Map.findWithDefault n n nameMap
      -- 6. 集約後ノード作成
      mkRepresentative (_, prefix) grp =
        let first = head grp
            kind  = case nodeKind first of
              ObservedN _ ->
                ObservedN (sum [k | n <- grp,
                                    let ObservedN k = nodeKind n])
              LatentN        -> LatentN
              DeterministicN -> DeterministicN
              dk@(DataN _)   -> dk
            -- 自己集約 (同じ集合のメンバへの deps) を除外
            memberNames = Set.fromList (map nodeName grp)
            externalDeps = Set.unions (map nodeDeps grp)
              `Set.difference` memberNames
            -- 親側の名前も mapName で remap (e.g. mu_0..mu_K-1 集約済の場合)
            remappedDeps = Set.map mapName externalDeps
        in first { nodeName = prefix
                 , nodeKind = kind
                 , nodeDeps = remappedDeps
                 }
      -- 7. ノードリスト再構築: 集約対象は代表 1 個、 非対象はそのまま
      isInGroup n = case keyOf n of
        Just k -> Map.member k collapsible
        Nothing -> False
      seenGroups :: [([Text], Text)]
      seenGroups = []
      walk [] _ acc = reverse acc
      walk (n:rest) seen acc
        | isInGroup n =
            let Just k = keyOf n
            in if k `elem` seen
                 then walk rest seen acc
                 else let rep = mkRepresentative k (collapsible Map.! k)
                      in walk rest (k : seen) (rep : acc)
        | otherwise = walk rest seen
            (n { nodeDeps = Set.map mapName (nodeDeps n) } : acc)
      newNodes = walk ns seenGroups []
      -- 8. edges を remap + dedupe + 自己ループ除去
      newEdges = Set.toList $ Set.fromList
        [ (s', t')
        | (s, t) <- es
        , let s' = mapName s
        , let t' = mapName t
        , s' /= t'   -- 自己ループ除外
        ]
  in mg { mgNodes = newNodes, mgEdges = newEdges }

-- | [日本語]: 多相モデルから DAG を自動構築する (Track 型による依存追跡)。
--
--   同じ名前で複数登場する Observe ノード (例: 回帰モデルで観測点ごとに
--   @observe \"y\"@ を発行する場合) は 1 つに統合される。観測数の合計と
--   親変数集合の和をマージし、エッジも重複排除する。
--   [English]: Automatically builds a DAG from a polymorphic model
--   (dependency tracking via the Track type).
--
--   Observe nodes that appear repeatedly under the same name (e.g. issuing
--   @observe \"y\"@ per observation in a regression model) are merged into
--   one. The observation counts are summed and the parent-variable sets
--   are unioned; edges are deduped as well.
buildModelGraph :: ModelP r -> ModelGraph
buildModelGraph m =
  let (rawNodes, plates) = extractDeps m
      merged   = assignDataPlates plates (mergeByName rawNodes)
      edges    = Set.toList $ Set.fromList
                   [ (parent, nodeName n)
                   | n <- merged
                   , parent <- Set.toList (nodeDeps n) ]
  in ModelGraph merged edges plates
  where
    -- Phase 60.6 追補: 宣言位置が plate 外 (nodePlates = []) の DataN を、
    -- PyMC の dims 同様「データ長 = plate サイズ」 の一意 match で plate に
    -- 割り当てる (典型 = モデル冒頭で宣言した dataNamedX n=150 が obs(150)
    -- cluster 内に描かれる)。 一致 plate が複数 / なし は据え置き (外に描く)。
    -- 入れ子 plate の full path は、 既にその plate に居る他ノードの
    -- nodePlates から逆引きする (plate 内ノードが無い場合は単独 path)。
    assignDataPlates plates ns =
      let paths = [ nodePlates n | n <- ns, not (null (nodePlates n)) ]
          pathFor nm = case [ p | p <- paths, last p == nm ] of
                         (p : _) -> p
                         []      -> [nm]
          assign n = case nodeKind n of
            DataN k | null (nodePlates n) ->
              case [ nm | (nm, sz) <- Map.toList plates, sz == k ] of
                [nm] -> n { nodePlates = pathFor nm }
                _    -> n
            _ -> n
      in map assign ns
    -- 同名ノードを統合: ObservedN n1 + ObservedN n2 → ObservedN (n1+n2)
    -- LatentN は最初の出現を残す。deps は和集合。
    -- nodePlates は最初の出現のものを維持 (同名は同 plate 前提)。
    mergeByName ns = mergeGo ns Map.empty []
    mergeGo [] _ acc = reverse acc
    mergeGo (n:ns) seen acc =
      let nm = nodeName n
      in case Map.lookup nm seen of
           Nothing -> mergeGo ns (Map.insert nm n seen) (n : acc)
           Just prev ->
             -- Phase 60.4: DataN は最弱 — 同名の非 DataN ノード (典型 =
             -- dataNamedObs "y" + observe "y" の docs 慣例) があれば吸収される
             -- (PyMC で observed RV が data 容器を内包して表示されるのと同型)。
             let (kind', dist', plates') =
                   case (nodeKind prev, nodeKind n) of
                     (ObservedN a, ObservedN b) ->
                       (ObservedN (a + b), nodeDist prev, nodePlates prev)
                     (DataN _, k2) -> (k2, nodeDist n, nodePlates n)
                     (k1, _)       -> (k1, nodeDist prev, nodePlates prev)
                 merged' = Node
                   { nodeName = nm
                   , nodeKind = kind'
                   , nodeDist   = dist'
                   , nodeDeps   = nodeDeps prev <> nodeDeps n
                   , nodePlates = plates'
                   }
                 acc' = map (\x -> if nodeName x == nm then merged' else x) acc
             in mergeGo ns (Map.insert nm merged' seen) acc'


-- ---------------------------------------------------------------------------
-- Track 評価 (logJoint の Track 特殊化)
-- ---------------------------------------------------------------------------

-- | [日本語]: Track でモデルを評価する (log joint も依存集合付きで計算)。
--   [English]: Evaluates the model with Track (computes the log joint
--   along with its dependency set as well).
runTrack :: forall r. ModelP r -> Map Text Track -> Track
runTrack m params = logJoint (m :: Model Track r) params
