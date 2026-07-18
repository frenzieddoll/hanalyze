{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- |
-- Module      : Hanalyze.Model.HBM.Gradient
-- Description : HBM の AD 勾配コンパイラ層 (NUTS per-draw のホット経路)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Phase 58.8: AD 勾配コンパイラ層を 'Hanalyze.Model.HBM' 本体から分離。
-- IR (中間表現) 層の **上層** であり、 NUTS per-draw の本経路 (compileGradUV →
-- gradVecIR / hybridGradClosure) を担う最ホット モジュール。 unconstrained 空間の
-- log-joint・解析閉形式勾配 (Gaussian LM ブロック)・ハイブリッド勾配クロージャ・
-- 定数 prior 解析勾配・制約変換 (invTransformF/logJacF) を含む。
--
-- 全 top-level を export し ('module ... where' = 暗黙全公開)、 公開 API
-- (gradAD/gradADU/compileGradU/compileGradUV/compileLogPU/compileLogPUV/
-- getTransforms/logJointUnconstrained/invTransformF/logJacF) は facade
-- 'Hanalyze.Model.HBM' の export list 経由で再エクスポートされる。
module Hanalyze.Model.HBM.Gradient where

import Control.DeepSeq (NFData (..), force)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, forM_, replicateM, when)
import Data.List (foldl', zip4)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Reflection (Reifies)
import Numeric.AD.Mode.Reverse.Double (grad, grad')
import qualified Numeric.AD.Internal.Reverse.Double as ADRD
import qualified System.Random.MWC as MWCBase
import qualified System.Random.MWC.Distributions as MWC
import System.Random.MWC (Gen)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)

import Control.Monad.ST (ST, runST)
import qualified Data.Vector          as BV
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed  as VU

-- Phase 95 A6: dense MvNormal observe の解析随伴 (detach) で Σ⁻¹/logdet を LAPACK
-- で 1 度だけ作る (cholesky を AD tape に載せない)。
import qualified Numeric.LinearAlgebra as LA

import Hanalyze.Stat.Distribution (Transform (..))
import Hanalyze.MCMC.Core (Chain (..))

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

-- Phase 60.7: '!!!' の依存タグは AD 勾配には無関係 (既定 id = サンプリング
-- ビット不変)。 ReverseDouble は ad の internal 型ゆえ orphan instance だが、
-- AD 経路の instantiate はこのモジュールに閉じている。
instance TrackTag (ADRD.ReverseDouble s)

-- ---------------------------------------------------------------------------
-- AD 勾配
-- ---------------------------------------------------------------------------

-- | AD で勾配を計算する。@names@ の順で各パラメータに対する偏微分を返す。
gradAD :: ModelP r -> [Text] -> [Double] -> [Double]
gradAD m names xs0 = grad f xs0
  where
    f xs =
      let params = Map.fromList (zip names xs)
      in logJoint m params

-- | unconstrained 空間で AD 勾配を計算する (HMC 用)。
-- 各パラメータに制約変換を適用し、Jacobian 補正項込みの log-joint を微分する。
--
-- Phase 54.4a-54.6: モデルが **Gaussian-恒等リンクの 'ObserveLM' ブロック** を
-- 含む場合、 そのブロックの観測尤度勾配 (= 規模 n に比例する支配項) を解析
-- 閉形式 (∂β=Xᵀr/σ² 等) で計算し、 prior / jacobian / scalar observe /
-- 非 Gaussian LM は従来 `ad` で計算してから成分加算する (ハイブリッド)。
-- Gaussian LM を含まないモデルは従来通り全体を `ad` で微分 (後方互換)。
gradADU :: ModelP r -> [Text] -> [Transform] -> [Double] -> [Double]
gradADU m names trans = compileGradU m names trans

-- | 'compileGradUV' の list wrapper (後方互換 API)。 NUTS は vector-native の
-- 'compileGradUV' を直接使う。
compileGradU :: forall r. ModelP r -> [Text] -> [Transform] -> ([Double] -> [Double])
compileGradU m names trans =
  let gv = compileGradUV m names trans
  in VS.toList . gv . VS.fromList

-- | 'compileGradUV' が実際に選ぶ勾配経路のラベル (診断表示用・Phase 91 A4)。
-- 'compileGradUV' 本体の分岐順 (gaussLMBlocksAuto → synthVecIR → 全体 ad) を
-- そのまま反映する唯一の分類子。
--
-- ★**束縛済モデル** ('hbmModelSpec' 経由) を渡すこと。 生の未束縛モデル
-- ('dataNamed*' の既定 @[]@ のまま) を渡すと data 行が空になり、 Gaussian LM
-- 合成も 'collectSymRows' も 0 行となって経路判定が狂う (Phase 91 A4 実測:
-- 17-nes/12-ark は実際は Gaussian LM 閉形式経路なのに、 生モデルを渡した
-- 診断が @synthVecIR = Nothing@ と誤表示していた)。
gradPathLabel :: ModelP r -> String
gradPathLabel m = case gaussLMBlocksAuto m of
  ([], _) -> case synthVecIR m of
    Nothing | hasHmmObserve m       -> "HMM forward-backward 閉形式随伴 (Phase 92)"
            | hasArmaObserve m      -> "ARMA(1,1) 逆向き随伴の閉形式 (Phase 101)"
            | hasGradedIrtObserve m -> "graded response IRT 解析勾配 (Phase 101)"
            | otherwise             -> "legacy walk+ad (全体 ad)"
    Just _  -> "vecIR (ベクトル式 IR 高速経路)"
  _       -> "Gaussian LM 閉形式ブロック (解析勾配)"

-- | Phase 92: 'gradPathLabel' 用の軽量構造判定 — 尤度が単一 'HmmForwardNormal'
-- observe か。 param 値は不要 (latent へ 0 を給餌・分布値は強制しない)。
-- 実際の経路選択は 'gradValPlan' 内の 'hmmAnalyticVG' (probe 同型) が行う。
hasHmmObserve :: ModelP r -> Bool
hasHmmObserve m = case go m of
    Just [HmmForwardNormal {}] -> True
    _                          -> False
  where
    go :: Model Double r' -> Maybe [Distribution Double]
    go (Pure _) = Just []
    go (Free (Sample _ _ k))        = go (k 0)
    go (Free (Observe _ d _ next))  = (d :) <$> go next
    go (Free (ObserveLM {}))        = Nothing
    go (Free (Potential _ _ next))  = go next
    go (Free (Deterministic _ v k)) = go (k v)
    go (Free (Data _ ys k))         = go (k (ys, ys))
    go (Free (DataIx _ is k))       = go (k is)
    go (Free (PlateBegin _ _ next)) = go next
    go (Free (PlateEnd next))       = go next

-- | Phase 101: 'gradPathLabel' 用の軽量構造判定 — 尤度が単一 'ArmaNormal'
-- observe か。 実際の経路選択は 'gradValPlan' 内の 'armaAnalyticVG' が行う。
hasArmaObserve :: ModelP r -> Bool
hasArmaObserve m = case go m of
    Just [ArmaNormal {}] -> True
    _                    -> False
  where
    go :: Model Double r' -> Maybe [Distribution Double]
    go (Pure _) = Just []
    go (Free (Sample _ _ k))        = go (k 0)
    go (Free (Observe _ d _ next))  = (d :) <$> go next
    go (Free (ObserveLM {}))        = Nothing
    go (Free (Potential _ _ next))  = go next
    go (Free (Deterministic _ v k)) = go (k v)
    go (Free (Data _ ys k))         = go (k (ys, ys))
    go (Free (DataIx _ is k))       = go (k is)
    go (Free (PlateBegin _ _ next)) = go next
    go (Free (PlateEnd next))       = go next

-- | Phase 101: 'gradPathLabel' 用の軽量構造判定 — 尤度が単一
-- 'GradedResponseIrt' observe か。 実際の経路選択は 'gradValPlan' 内の
-- 'gradedIrtAnalyticVG' が行う。
hasGradedIrtObserve :: ModelP r -> Bool
hasGradedIrtObserve m = case go m of
    Just [GradedResponseIrt {}] -> True
    _                           -> False
  where
    go :: Model Double r' -> Maybe [Distribution Double]
    go (Pure _) = Just []
    go (Free (Sample _ _ k))        = go (k 0)
    go (Free (Observe _ d _ next))  = (d :) <$> go next
    go (Free (ObserveLM {}))        = Nothing
    go (Free (Potential _ _ next))  = go next
    go (Free (Deterministic _ v k)) = go (k v)
    go (Free (Data _ ys k))         = go (k (ys, ys))
    go (Free (DataIx _ is k))       = go (k is)
    go (Free (PlateBegin _ _ next)) = go next
    go (Free (PlateEnd next))       = go next

-- ---------------------------------------------------------------------------
-- Phase 95 A6: dense MvNormal observe の解析随伴 (detach) 経路
-- ---------------------------------------------------------------------------

-- | Phase 95 A6: 尤度が **単一 'MvNormal' observe** のモデル (GP 回帰・
-- dense-MvNormal) の value+grad を **解析随伴 (detach トリック)** で計算する
-- クロージャを構築する。 'mvNormalObserveOf' が 'Just' のとき (= 適格) のみ
-- 'Just' を返し、 非適格なら 'Nothing' (呼び出し側は従来 walk+ad へ)。
--
-- === なぜ速いか
-- 現行 walk+ad は N×N Cholesky + solve + logdet を **毎 leapfrog で reverse-AD
-- tape 上に丸ごと展開**する (O(N³) のスカラー演算が各々 boxed AD ノードを alloc)。
-- 大 N で壊滅的 (§A4: N=50 で対 PyMC 62×)。 本経路は PyMC/Stan と同じく
-- **Cholesky を AD tape に載せず** Σ⁻¹/logdet を LAPACK で 1 度だけ Double 計算し、
-- @G = ∂logp/∂Σ@・@h = ∂logp/∂μ@ を定数化して surrogate @<G,Σ(θ)>+<h,μ(θ)>@ の
-- 軽量 ad (O(N²)・cholesky 無し) だけを微分する。
--
-- === 数学 (detach)
-- 1 観測 (k-vector) の @logp = -k/2·log2π - 0.5·log|Σ| - 0.5·(y-μ)ᵀΣ⁻¹(y-μ)@ に対し
-- @α = Σ⁻¹(y-μ)@、 @∂logp/∂Σ = 0.5(ααᵀ - Σ⁻¹)@、 @∂logp/∂μ = α@。 複数 chunk では
-- @G = 0.5(Σ_m α_mα_mᵀ - nCh·Σ⁻¹)@、 @h = Σ_m α_m@。 surrogate @<G,Σ(θ)>+<h,μ(θ)>@ の
-- θ 勾配は連鎖律で @∂logp/∂θ@ に厳密一致 (proto: 有限差分 2.8e-9・ad-through 2.2e-15)。
-- θ=invTransform(u) を surrogate 内に組むので ∂/∂u が直接得られる。
--
-- 総勾配 = @grad(logPrior+logJac)@ (scalar・cheap) + @detach(observe)@。
-- 値 = @(logPrior+logJac)@ + @logp_MvNormal@ (同じ LAPACK 分解から)。
--
-- === 常時 ON (次元しきい値なし)
-- 解析随伴は近似でなく厳密ゆえ正しさゲート不要。 PyMC/Stan も Cholesky Op の
-- 解析随伴を **常時**使う (次元しきい値を持たない)。 小 N (N≲40) では list 往復の
-- 定数倍で ad-through と拮抗〜僅遅だが (§A3-proto crossover ≈N40-50)、 該当する
-- shipping モデルは無い。 その帯の overhead 除去 (surrogate 配列化) は TODO (§A6)。
mvNormalAnalyticVG
  :: forall r. ModelP r -> [Text] -> [Transform]
  -> Maybe (VS.Vector Double -> (Double, VS.Vector Double))
mvNormalAnalyticVG m names trans =
  -- 適格判定は構造のみ (param 値に依らない) — ダミー 0 で walk。
  case mvNormalObserveOf m probeParams of
    Nothing -> Nothing
    Just _  -> Just closure
  where
    probeParams :: Map Text Double
    probeParams = Map.fromList [ (n, 0) | n <- names ]

    closure :: VS.Vector Double -> (Double, VS.Vector Double)
    closure uv =
      let us     = VS.toList uv
          thetaD = [ invTransformF t u | (t, u) <- zip trans us ]
          paramD = Map.fromList (zip names thetaD)
      in case mvNormalObserveOf m paramD of
           Nothing -> (fFullA us, VS.fromList (gradFullA us))  -- 適格判定と矛盾: fallback
           Just (muD, covD, ys)
             | k == 0 || null chunks ->                        -- 退化: fallback
                 (fFullA us, VS.fromList (gradFullA us))
             | otherwise ->
                 let -- 片道 flat 化のみ (fromLists 不使用): [[Double]] を concat して
                     -- row-major で Matrix に積む。 戻りの toLists は一切しない。
                     sig       = LA.matrix k (concat covD)            -- N×N
                     (inv, (lndet, _sgn)) = LA.invlndet sig
                     muV       = LA.fromList muD
                     ds        = [ LA.fromList (map realToFrac ym) - muV | ym <- chunks ]
                     alphas    = [ inv LA.#> d | d <- ds ]            -- α_m = Σ⁻¹(y_m-μ)
                     quadSum   = sum [ d LA.<.> a | (d, a) <- zip ds alphas ]
                     kA        = fromIntegral k :: Double
                     nChA      = fromIntegral nCh :: Double
                     logpObs   = nChA * (negate 0.5 * kA * log (2 * pi) - 0.5 * lndet)
                                 - 0.5 * quadSum
                     -- G = 0.5(Σ_m α_mα_mᵀ - nCh·Σ⁻¹), h = Σ_m α_m
                     gMat      = LA.scale 0.5
                                   (foldl1 (+) [ LA.outer a a | a <- alphas ]
                                    - LA.scale nChA inv)
                     -- G/h は **flat Storable Vector のまま** (row-major)。 surrogate 側で
                     -- index して realToFrac lift = nested list 化 (toLists) を回避。
                     gFlat     = LA.flatten gMat :: VS.Vector Double  -- length k*k, row-major
                     hVec      = foldl1 (+) alphas :: VS.Vector Double -- length k
                     -- detach surrogate: ∂/∂u <G,Σ(θ(u))> + <h,μ(θ(u))>。 Σ(θ) は model が
                     -- [[a]] を吐くので concat で 1 回 flat 化し gFlat と flat×flat 内積。
                     surrogate :: forall a. (Floating a, Ord a, TrackTag a) => [a] -> a
                     surrogate uu =
                       let thetaA = [ invTransformF t u | (t, u) <- zip trans uu ]
                           paramA = Map.fromList (zip names thetaA)
                       in case mvNormalObserveOf m paramA of
                            Just (muA, covA, _) ->
                              dotFlatL gFlat (concat covA) + dotFlatL hVec muA
                            Nothing -> 0
                     gObs      = grad surrogate us                    -- ∂obs/∂u
                     gRest     = gradRestA us                         -- ∂(logPrior+logJac)/∂u
                     vRest     = fRestA us                            -- (logPrior+logJac)(u)
                 in ( vRest + logpObs
                    , VS.fromList (zipWith (+) gRest gObs) )
             where k      = length muD
                   chunks = chunksOf k ys
                   nCh    = length chunks

    -- flat Double Vector · [a] リスト内積: Double 側 (G/h) を list 化せず index して
    -- realToFrac で lift。 xs (concat covA / muA) だけを 1 回舐める (toLists 往復回避)。
    dotFlatL :: forall a. Floating a => VS.Vector Double -> [a] -> a
    dotFlatL gv = go 0 0
      where go !i !acc (x : xs) = go (i + 1) (acc + realToFrac (VS.unsafeIndex gv i) * x) xs
            go _  !acc []       = acc

    -- prior + logJac (尤度を除く) — scalar・dense 行列を含まない。
    fRestA :: (Floating a, Ord a, TrackTag a) => [a] -> a
    fRestA us =
      let paramsC = Map.fromList (zip names [ invTransformF t u | (t, u) <- zip trans us ])
          logJac  = sum [ logJacF t u | (t, u) <- zip trans us ]
      in logPrior m paramsC + logJac
    -- AD 側は 'logDensityRD' 注入版 (Phase 92 B3・値/勾配とも fRestA と bit 一致)
    fRestRD :: forall s. Reifies s ADRD.Tape
            => [ADRD.ReverseDouble s] -> ADRD.ReverseDouble s
    fRestRD us =
      let paramsC = Map.fromList (zip names [ invTransformF t u | (t, u) <- zip trans us ])
          logJac  = sum [ logJacF t u | (t, u) <- zip trans us ]
      in logPriorWith logDensityRD m paramsC + logJac
    gradRestA :: [Double] -> [Double]
    gradRestA = grad fRestRD

    -- 適格判定が崩れた稀ケース用の完全 walk+ad fallback (従来経路と同一)。
    fFullA :: (Floating a, Ord a, TrackTag a) => [a] -> a
    fFullA us =
      let paramsC = Map.fromList (zip names [ invTransformF t u | (t, u) <- zip trans us ])
          logJac  = sum [ logJacF t u | (t, u) <- zip trans us ]
      in logJoint m paramsC + logJac
    gradFullA :: [Double] -> [Double]
    gradFullA = grad fFullA

-- ---------------------------------------------------------------------------
-- Phase 95 B-dsl: GP (RBF) 尤度の閉形式随伴 (Cholesky を AD tape に載せない)
-- ---------------------------------------------------------------------------

-- | Phase 95 B-dsl: 尤度が **単一 'MvNormalGpRBF' observe** のモデルから
-- @(x, α, ρ, σ, ys)@ を現在の param 値で抽出する。 それ以外 (他 Observe/
-- ObserveLM 混在・非 GpRBF) は 'Nothing'。 walk は 'mvNormalObserveOf' と同型。
gpRBFObserveOf :: (Floating a, Ord a)
               => Model a r -> Map Text a -> Maybe ([a], a, a, a, [Double])
gpRBFObserveOf model params =
  case go model of
    Just [(MvNormalGpRBF xs al rh sg, ys)] -> Just (xs, al, rh, sg, ys)
    _                                      -> Nothing
  where
    go (Pure _) = Just []
    go (Free (Sample n _ k)) =
      case Map.lookup n params of
        Nothing -> Nothing
        Just v  -> go (k v)
    go (Free (Observe _ d ys next)) = ((d, ys) :) <$> go next
    go (Free (ObserveLM {}))        = Nothing
    go (Free (Potential _ _ next))  = go next
    go (Free (Deterministic _ v k)) = go (k v)
    go (Free (Data _ ys k))         = go (k (map realToFrac ys, ys))
    go (Free (DataIx _ is k))       = go (k is)
    go (Free (PlateBegin _ _ next)) = go next
    go (Free (PlateEnd next))       = go next

-- | Phase 95 B-dsl: 'MvNormalGpRBF' 尤度の value+grad を **閉形式随伴**で計算する
-- クロージャを構築する。 適格 (単一 GpRBF observe) のときのみ 'Just'。
--
-- === A案 (汎用 detach) との差 = 84% の除去
-- A案 (§mvNormalAnalyticVG) は surrogate @<G,Σ(θ)>@ を **AD で微分**するため、
-- 毎 leaf で Σ(θ) を @[[a]]@ で組み直し (profile: gpExpQuadCov 55%) その全 N²
-- ノードを reverse-AD tape に載せていた (reifyTypeable+partials 29%)。 本経路は:
--
--   1. カーネル役割 (x/α/ρ/σ) が 'MvNormalGpRBF' の型で明示されているので、
--      **∂Σ/∂θ を閉形式** (@∂Σ/∂α=2K'/α@・@∂Σ/∂ρ=K'∘d²/ρ³@・@∂Σ/∂σ=I@) で書ける。
--   2. G=∂logp/∂Σ・K'・d² は **hmatrix Matrix** (脱リスト) で計算し、
--      @g_θ = <G,∂Σ/∂θ>@ を要素積 + trace で Double 算出 (**AD tape ゼロ**)。
--   3. u への連鎖律は **軽量 surrogate** @g_α·α(u)+g_ρ·ρ(u)+g_σ·σ(u)@ を ad。
--      ad は α/ρ/σ の **3 scalar 抽出のみ** (cov は非展開・lazy)。 これで
--      「どの u が α/ρ/σ か」の対応付けを AD が自動処理する (名前直書き不要)。
--   4. 値 logp と Σ⁻¹/logdet は同じ LAPACK 分解 (@invlndet@) から。
--
-- 距離行列 d² は x (data・定数) から **build 時に 1 度だけ**作り、全 leaf で再利用。
gpRBFAnalyticVG
  :: forall r. ModelP r -> [Text] -> [Transform]
  -> Maybe (VS.Vector Double -> (Double, VS.Vector Double))
gpRBFAnalyticVG m names trans =
  case gpRBFObserveOf m probeParams of
    Just (xs0, _, _, _, _) | not (null xs0) -> Just (closure (buildD2 xs0))
    _                                       -> Nothing
  where
    probeParams :: Map Text Double
    probeParams = Map.fromList [ (nm, 0) | nm <- names ]

    -- x (data・定数) から距離² 行列 D2_ij=(x_i-x_j)² を build 時 1 回だけ。
    buildD2 :: [Double] -> LA.Matrix Double
    buildD2 xs =
      let nn = length xs
          xv = VS.fromList xs
      in LA.matrix nn [ let d = VS.unsafeIndex xv i - VS.unsafeIndex xv j in d * d
                      | i <- [0 .. nn - 1], j <- [0 .. nn - 1] ]

    paramMapOf :: forall a. Floating a => [a] -> Map Text a
    paramMapOf us = Map.fromList (zip names [ invTransformF t u | (t, u) <- zip trans us ])

    closure :: LA.Matrix Double -> VS.Vector Double -> (Double, VS.Vector Double)
    closure d2 uv =
      let us = VS.toList uv
      in case gpRBFObserveOf m (paramMapOf us) of
           Nothing -> (fFullA us, VS.fromList (gradFullA us))       -- 適格崩れ: fallback
           Just (xs, alphaD, rhoD, sigmaD, ys)
             | k == 0 || null chunks -> (fFullA us, VS.fromList (gradFullA us))
             | otherwise ->
                 let -- 純カーネル K'_ij = α² exp(-0.5 D2/ρ²)。exp は hmatrix の
                     -- element-wise Floating instance (C ベクトル化) を使い、cmap の
                     -- Haskell ラムダ per-element boxing (2500 boxed Double/leaf) を回避。
                     kMat    = LA.scale (alphaD * alphaD)
                                 (exp (LA.scale (negate 0.5 / (rhoD * rhoD)) d2))
                     -- Σ = K' + (jitter+σ)I。対角に直接加算 (脱 ident: N×N を 3 パス→1)。
                     covM    = LA.accum kMat (+) [ ((i, i), 1e-10 + sigmaD) | i <- [0 .. k - 1] ]
                 -- covM = K'(PSD) + (1e-10+σ>0)I ゆえ本来 PD。Cholesky は LU 全逆行列より
                 -- O(N³) 定数が軽い (PyMC/Stan と同じ経路)。ただし σ→0⁺ の悪条件で LAPACK が
                 -- 非 PD 判定する稀ケースに備え mbChol で受け、崩れたら full-AD へ安全退避。
                 in case LA.mbChol (LA.trustSym covM) of
                      Nothing    -> (fFullA us, VS.fromList (gradFullA us))  -- 非 PD (稀): fallback
                      Just uchol ->
                       let inv     = LA.cholSolve uchol (LA.ident k)         -- Σ⁻¹ (SPD solve)
                           lndet   = 2 * sum (map log (LA.toList (LA.takeDiag uchol)))  -- log|Σ|=2Σlog U_ii
                           ds      = [ LA.fromList (map realToFrac yv) | yv <- chunks ]  -- (y - μ), μ=0
                           alphas  = [ inv LA.#> d | d <- ds ]                 -- Σ⁻¹(y-μ)
                           quadSum = sum [ d LA.<.> a | (d, a) <- zip ds alphas ]
                           kA      = fromIntegral k :: Double
                           nChA    = fromIntegral nCh :: Double
                           logpObs = nChA * (negate 0.5 * kA * log (2 * pi) - 0.5 * lndet)
                                     - 0.5 * quadSum
                           -- 閉形式随伴 g_θ = <G, ∂Σ/∂θ>, G = 0.5(Σ_m α_mα_mᵀ - nCh·Σ⁻¹)。
                           -- Frobenius 恒等式で G を materialize せず算出 (N×N 一時行列を全廃):
                           --   <α_mα_mᵀ, M> = α_mᵀ M α_m  (BLAS mat-vec + dot・N² alloc なし)、
                           --   <Σ⁻¹, M>     = <flatten Σ⁻¹, flatten M>  (BLAS ddot・temp なし)。
                           -- ∂Σ/∂α=2K'/α・∂Σ/∂σ=I・∂Σ/∂ρ=K'∘d²/ρ³。
                           kd2      = kMat * d2                              -- ∂Σ/∂ρ 用 Hadamard (1 回だけ)
                           invFlat  = LA.flatten inv
                           quadK    = sum [ a LA.<.> (kMat LA.#> a) | a <- alphas ]  -- Σ α_mᵀK'α_m
                           quadKd2  = sum [ a LA.<.> (kd2  LA.#> a) | a <- alphas ]  -- Σ α_mᵀ(K'∘d²)α_m
                           aaSum    = sum [ a LA.<.> a | a <- alphas ]              -- Σ α_mᵀα_m
                           frobIK   = invFlat LA.<.> LA.flatten kMat               -- <Σ⁻¹, K'>
                           frobIKd2 = invFlat LA.<.> LA.flatten kd2                -- <Σ⁻¹, K'∘d²>
                           trInv    = LA.sumElements (LA.takeDiag inv)             -- tr(Σ⁻¹)
                           gAlpha   = (quadK - nChA * frobIK) / alphaD             -- 2·<G,K'>/α
                           gSigma   = 0.5 * (aaSum - nChA * trInv)                 -- <G,I> = tr(G)
                           gRho     = 0.5 * (quadKd2 - nChA * frobIKd2) / (rhoD ** 3) -- <G,K'∘d²>/ρ³
                           -- 軽量 scatter: g_θ を u へ連鎖 (ad は α/ρ/σ の抽出のみ・cov 非展開)
                           surrogate :: forall a. (Floating a, Ord a, TrackTag a) => [a] -> a
                           surrogate uu =
                             case gpRBFObserveOf m (paramMapOf uu) of
                               Just (_, al, rh, sg, _) ->
                                 realToFrac gAlpha * al + realToFrac gRho * rh
                                   + realToFrac gSigma * sg
                               Nothing -> 0
                           gObs    = grad surrogate us                        -- ∂obs/∂u
                           gRest   = gradRestA us                             -- ∂(logPrior+logJac)/∂u
                           vRest   = fRestA us                                -- (logPrior+logJac)(u)
                       in ( vRest + logpObs
                          , VS.fromList (zipWith (+) gRest gObs) )
             where k      = length xs                                  -- GP 次元
                   chunks = chunksOf k ys                              -- 通常 1 chunk
                   nCh    = length chunks

    -- prior + logJac (尤度除く) — scalar・dense 行列なし。
    fRestA :: (Floating a, Ord a, TrackTag a) => [a] -> a
    fRestA us = logPrior m (paramMapOf us)
                  + sum [ logJacF t u | (t, u) <- zip trans us ]
    -- AD 側は 'logDensityRD' 注入版 (Phase 92 B3・値/勾配とも fRestA と bit 一致)
    fRestRD :: forall s. Reifies s ADRD.Tape
            => [ADRD.ReverseDouble s] -> ADRD.ReverseDouble s
    fRestRD us = logPriorWith logDensityRD m (paramMapOf us)
                   + sum [ logJacF t u | (t, u) <- zip trans us ]
    gradRestA :: [Double] -> [Double]
    gradRestA = grad fRestRD

    -- 適格崩れ時の完全 walk+ad fallback。
    fFullA :: (Floating a, Ord a, TrackTag a) => [a] -> a
    fFullA us = logJoint m (paramMapOf us)
                  + sum [ logJacF t u | (t, u) <- zip trans us ]
    gradFullA :: [Double] -> [Double]
    gradFullA = grad fFullA

-- ---------------------------------------------------------------------------
-- Phase 92 A2: HMM forward 尤度の閉形式随伴 (forward-backward・AD tape ゼロ)
-- ---------------------------------------------------------------------------

-- | Phase 92 A2: 尤度が **単一 'HmmForwardNormal' observe** のモデルから
-- @(π_0, trans, μs, σ, ys)@ を現在の param 値で抽出する。 それ以外 (他 Observe/
-- ObserveLM 混在・非 HMM) は 'Nothing'。 walk は 'gpRBFObserveOf' と同型。
hmmObserveOf :: (Floating a, Ord a)
             => Model a r -> Map Text a -> Maybe ([a], [[a]], [a], a, [Double])
hmmObserveOf model params =
  case go model of
    Just [(HmmForwardNormal pi0 tr mus sg, ys)] -> Just (pi0, tr, mus, sg, ys)
    _                                           -> Nothing
  where
    go (Pure _) = Just []
    go (Free (Sample n _ k)) =
      case Map.lookup n params of
        Nothing -> Nothing
        Just v  -> go (k v)
    go (Free (Observe _ d ys next)) = ((d, ys) :) <$> go next
    go (Free (ObserveLM {}))        = Nothing
    go (Free (Potential _ _ next))  = go next
    go (Free (Deterministic _ v k)) = go (k v)
    go (Free (Data _ ys k))         = go (k (map realToFrac ys, ys))
    go (Free (DataIx _ is k))       = go (k is)
    go (Free (PlateBegin _ _ next)) = go next
    go (Free (PlateEnd next))       = go next

-- | Phase 92 A2: 'HmmForwardNormal' 尤度の value+grad を **forward-backward の
-- 閉形式随伴**で計算するクロージャを構築する。 適格 (単一 HMM observe) のとき
-- のみ 'Just'。 構成は 'gpRBFAnalyticVG' と同じ 3 段:
--
--   1. forward α / backward β を **Double 空間** (AD tape 外) で 1 回ずつ回し、
--      値 @logL = logSumExp_k α_T[k]@ と閉形式随伴
--      @∂logL/∂μ_k = Σ_t γ_t[k]·(y_t-μ_k)/σ²@ (γ_t[k]=exp(α_t+β_t-logL))・
--      @∂logL/∂T_ij = Σ_t exp(α_t[i]+emit_{t+1}[j]+β_{t+1}[j]-logL)@ (ξ 集計)・
--      @∂logL/∂π_k = γ_0[k]/π_k@・σ も同様、 を Double で算出する。
--   2. u への連鎖律は **軽量 surrogate** @Σ g_θ·θ(u)@ を ad。 ad は
--      π/T/μ/σ の **O(K²) scalar 抽出のみ** (T 長の forward loop は非展開)。
--      dirichlet の棒折り deterministic 等の合成は AD が自動処理する。
--   3. prior + logJac は 'logPrior' ベースの fRest (走査対象から尤度を除外)。
--
-- 従来 walk+ad は T×K² 個の logSumExp/logDensity を毎 leapfrog boxed AD で
-- 再評価していた (Phase 92 A1d: 数値密度系 84% + AD 6%・alloc 23.7GB/5s)。
-- 本経路は同じ O(TK²) を unboxed Double で 2 パス回すだけで tape に載せない。
hmmAnalyticVG
  :: forall r. ModelP r -> [Text] -> [Transform]
  -> Maybe (VS.Vector Double -> (Double, VS.Vector Double))
hmmAnalyticVG m names trans =
  case hmmObserveOf m probeParams of
    Just (pi0, tr, mus, _, ys)
      | kDim > 0, length tr == kDim, all ((== kDim) . length) tr
      , length mus == kDim, not (null ys) -> Just closure
      where kDim = length pi0
    _ -> Nothing
  where
    probeParams :: Map Text Double
    probeParams = Map.fromList [ (nm, 0) | nm <- names ]

    paramMapOf :: forall a. Floating a => [a] -> Map Text a
    paramMapOf us = Map.fromList (zip names [ invTransformF t u | (t, u) <- zip trans us ])

    closure :: VS.Vector Double -> (Double, VS.Vector Double)
    closure uv =
      let us = VS.toList uv
      in case hmmObserveOf m (paramMapOf us) of
           Nothing -> (fFullA us, VS.fromList (gradFullA us))       -- 適格崩れ: fallback
           Just (pi0D, transD, musD, sgD, ys)
             | sgD <= 0 || null ys -> (fFullA us, VS.fromList (gradFullA us))
             | otherwise ->
                 -- B2-① (2026-07-17): 脱リスト化 — α/β/emit を unboxed 行 vector で持ち、
                 -- 内側 K ループは lseK (list 非 alloc の 2 パス logSumExp)。γ は非実体化。
                 let kk  = length pi0D
                     tT  = length ys
                     ixs = [0 .. kk - 1]
                     ysV  = VU.fromList ys
                     musV = VU.fromList musD
                     lPi0 = VU.fromList (map safeLog pi0D)
                     lTr  = VU.fromList (map safeLog (concat transD))   -- 行優先 K×K flat
                     lsg  = log sgD
                     c2pi = 0.5 * log (2 * pi)
                     emitAt t k' = let z = (VU.unsafeIndex ysV t - VU.unsafeIndex musV k') / sgD
                                   in -0.5 * z * z - lsg - c2pi
                     emitRows = [ VU.generate kk (emitAt t) | t <- [0 .. tT - 1] ]
                     -- K 要素 logSumExp (max → sumexp の 2 パス・中間 list 無し)
                     lseK f = let mx = foldl' (\acc i -> max acc (f i)) negInf ixs
                              in if mx == negInf then negInf
                                 else mx + log (foldl' (\acc i -> acc + exp (f i - mx)) 0 ixs)
                     -- forward: α_t 全行を保持 (随伴の γ/ξ に使う)
                     alpha0 = VU.zipWith (+) lPi0 (head emitRows)
                     stepF aPrev emT = VU.generate kk $ \j ->
                       lseK (\i -> VU.unsafeIndex aPrev i + VU.unsafeIndex lTr (i * kk + j))
                         + VU.unsafeIndex emT j
                     alphaRows = scanl stepF alpha0 (tail emitRows)   -- 長さ T
                     logL = lseK (VU.unsafeIndex (last alphaRows))
                     -- backward: β_{T-1}=0・β_t[i] = lse_j (lT_ij + emit_{t+1}[j] + β_{t+1}[j])
                     stepB emNext bNext = VU.generate kk $ \i ->
                       lseK (\j -> VU.unsafeIndex lTr (i * kk + j)
                                     + VU.unsafeIndex emNext j + VU.unsafeIndex bNext j)
                     betaRows = scanr stepB (VU.replicate kk 0) (tail emitRows)  -- 長さ T
                     -- 閉形式随伴 (全て Double・tape ゼロ・γ_t[k] = exp(α+β-logL) は都度計算)
                     gammaAt aR bR k' = exp (VU.unsafeIndex aR k' + VU.unsafeIndex bR k' - logL)
                     abRows = zip3 alphaRows betaRows [0 ..]
                     gMu = [ foldl' (\acc (aR, bR, t) ->
                                       acc + gammaAt aR bR k'
                                             * (VU.unsafeIndex ysV t - VU.unsafeIndex musV k')
                                             / (sgD * sgD))
                                    0 abRows
                           | k' <- ixs ]
                     gSg = foldl' (\acc (aR, bR, t) ->
                                     foldl' (\a2 k' ->
                                               let z = (VU.unsafeIndex ysV t
                                                          - VU.unsafeIndex musV k') / sgD
                                               in a2 + gammaAt aR bR k' * (z * z - 1) / sgD)
                                            acc ixs)
                                  0 abRows
                     gPi0 = [ if p > 0 then gammaAt (head alphaRows) (head betaRows) k' / p else 0
                            | (k', p) <- zip ixs pi0D ]
                     -- ξ 集計: ∂logL/∂T_ij = Σ_{t<T-1} exp(α_t[i]+emit_{t+1}[j]+β_{t+1}[j]-logL)
                     xiRows = zip3 alphaRows (tail emitRows) (tail betaRows)
                     gTr = [ [ foldl' (\acc (aR, emN, bN) ->
                                         acc + exp (VU.unsafeIndex aR i + VU.unsafeIndex emN j
                                                      + VU.unsafeIndex bN j - logL))
                                      0 xiRows
                             | j <- ixs ]
                           | i <- ixs ]
                     -- 軽量 scatter: g_θ を u へ連鎖 (ad は π/T/μ/σ の抽出のみ・T loop 非展開)
                     surrogate :: forall a. (Floating a, Ord a, TrackTag a) => [a] -> a
                     surrogate uu =
                       case hmmObserveOf m (paramMapOf uu) of
                         Just (p0, tr', ms, s, _) ->
                           sum (zipWith (\g v -> realToFrac g * v) gPi0 p0)
                             + sum (zipWith (\gr r -> sum (zipWith (\g v -> realToFrac g * v) gr r))
                                            gTr tr')
                             + sum (zipWith (\g v -> realToFrac g * v) gMu ms)
                             + realToFrac gSg * s
                         Nothing -> 0
                     -- B2-② (2026-07-17): prior+logJac と surrogate を 1 本の AD tape に合流し
                     -- grad' で値+勾配を同時取得 (walk 4 回/eval → Double 1 + AD 1)。
                     -- fRest の値は vComb から surrogate の Double 値を引いて復元する。
                     -- B3: prior 密度は 'logDensityRD' 注入 (fRestRD) = 定数
                     -- hyperparam の lgamma 正規化項を Double へ畳み込み (bit 一致)。
                     fCombRD :: forall s. Reifies s ADRD.Tape
                             => [ADRD.ReverseDouble s] -> ADRD.ReverseDouble s
                     fCombRD uu = fRestRD uu + surrogate uu
                     (vComb, gComb) = grad' fCombRD us
                     surrAtUs = sum (zipWith (*) gPi0 pi0D)
                                  + sum (zipWith (\gr r -> sum (zipWith (*) gr r)) gTr transD)
                                  + sum (zipWith (*) gMu musD)
                                  + gSg * sgD
                 in ( (vComb - surrAtUs) + logL
                    , VS.fromList gComb )

    safeLog :: Double -> Double
    safeLog x = if x <= 0 then negInf else log x

    -- prior + logJac (尤度除く) — 'gpRBFAnalyticVG' と同じ。
    fRestA :: (Floating a, Ord a, TrackTag a) => [a] -> a
    fRestA us = logPrior m (paramMapOf us)
                  + sum [ logJacF t u | (t, u) <- zip trans us ]
    gradRestA :: [Double] -> [Double]
    gradRestA = grad fRestA

    -- fRestA の AD 特化 (Phase 92 B3): 'logDensityRD' 注入で定数 hyperparam の
    -- lgamma 正規化項を Double へ畳み込む。 値・勾配とも fRestA と bit 一致
    -- ('logDensityRD' の注釈参照)。
    fRestRD :: forall s. Reifies s ADRD.Tape
            => [ADRD.ReverseDouble s] -> ADRD.ReverseDouble s
    fRestRD us = logPriorWith logDensityRD m (paramMapOf us)
                   + sum [ logJacF t u | (t, u) <- zip trans us ]

    -- 適格崩れ時の完全 walk+ad fallback。
    fFullA :: (Floating a, Ord a, TrackTag a) => [a] -> a
    fFullA us = logJoint m (paramMapOf us)
                  + sum [ logJacF t u | (t, u) <- zip trans us ]
    gradFullA :: [Double] -> [Double]
    gradFullA = grad fFullA

-- ---------------------------------------------------------------------------
-- Phase 101 A2: ARMA(1,1) 尤度の閉形式随伴 (逆向き随伴再帰・AD tape ゼロ)
-- ---------------------------------------------------------------------------

-- | Phase 101 A2: 尤度が **単一 'ArmaNormal' observe** のモデルから
-- @(μ, φ, θ, σ, ys)@ を現在の param 値で抽出する。 それ以外 (他 Observe 混在・
-- 非 ARMA) は 'Nothing'。 walk は 'hmmObserveOf' と同型。
armaObserveOf :: (Floating a, Ord a)
              => Model a r -> Map Text a -> Maybe (a, a, a, a, [Double])
armaObserveOf model params =
  case go model of
    Just [(ArmaNormal mu phi theta sg, ys)] -> Just (mu, phi, theta, sg, ys)
    _                                       -> Nothing
  where
    go (Pure _) = Just []
    go (Free (Sample n _ k)) =
      case Map.lookup n params of
        Nothing -> Nothing
        Just v  -> go (k v)
    go (Free (Observe _ d ys next)) = ((d, ys) :) <$> go next
    go (Free (ObserveLM {}))        = Nothing
    go (Free (Potential _ _ next))  = go next
    go (Free (Deterministic _ v k)) = go (k v)
    go (Free (Data _ ys k))         = go (k (map realToFrac ys, ys))
    go (Free (DataIx _ is k))       = go (k is)
    go (Free (PlateBegin _ _ next)) = go next
    go (Free (PlateEnd next))       = go next

-- | Phase 101 A2: 'ArmaNormal' 尤度の value+grad を **逆向き 1 パスの閉形式
-- 随伴**で計算するクロージャを構築する。 適格 (単一 ArmaNormal observe) の
-- ときのみ 'Just'。 構成は 'hmmAnalyticVG' と同じ 3 段:
--
--   1. err 前向き再帰 (@e_1 = y_1 − (μ+φμ)@・@e_t = y_t − μ − φ·y_{t−1} −
--      θ·e_{t−1}@) と随伴の逆向き再帰 (@ē_t = −e_t/σ² − θ·ē_{t+1}@・
--      @ē_T = −e_T/σ²@) を **Double 空間** (AD tape 外) で 1 回ずつ回し、
--      値 @logL = Σ_t log N(e_t; 0, σ)@ と閉形式随伴
--      @∂logL/∂μ = ē_1·(−(1+φ)) − Σ_{t≥2} ē_t@・
--      @∂logL/∂φ = ē_1·(−μ) − Σ_{t≥2} ē_t·y_{t−1}@・
--      @∂logL/∂θ = −Σ_{t≥2} ē_t·e_{t−1}@・
--      @∂logL/∂σ = −T/σ + (Σ_t e_t²)/σ³@ を算出する。
--   2. u への連鎖律は **軽量 surrogate** @Σ g_θ·θ(u)@ を ad (μ/φ/θ/σ の
--      4 scalar 抽出のみ・T 長の再帰は非展開)。
--   3. prior + logJac は 'logDensityRD' 注入の fRestRD (Phase 92 B3 と同じ)。
--
-- 従来 walk+ad は T 本の 'logDensity' + mapAccumL 再帰を毎 leapfrog boxed AD
-- で再評価していた (Phase 101 A1: logDensity 31.2% + armaModel 20.9%・
-- alloc 72%)。 本経路は同じ O(T) を unboxed Double で 2 パス回すだけ。
armaAnalyticVG
  :: forall r. ModelP r -> [Text] -> [Transform]
  -> Maybe (VS.Vector Double -> (Double, VS.Vector Double))
armaAnalyticVG m names trans =
  case armaObserveOf m probeParams of
    Just (_, _, _, _, ys) | not (null ys) -> Just closure
    _                                     -> Nothing
  where
    probeParams :: Map Text Double
    probeParams = Map.fromList [ (nm, 0) | nm <- names ]

    paramMapOf :: forall a. Floating a => [a] -> Map Text a
    paramMapOf us = Map.fromList (zip names [ invTransformF t u | (t, u) <- zip trans us ])

    closure :: VS.Vector Double -> (Double, VS.Vector Double)
    closure uv =
      let us = VS.toList uv
      in case armaObserveOf m (paramMapOf us) of
           Nothing -> (fFullA us, VS.fromList (gradFullA us))       -- 適格崩れ: fallback
           Just (muD, phiD, thD, sgD, ys)
             | sgD <= 0 || null ys -> (fFullA us, VS.fromList (gradFullA us))
             | otherwise ->
                 let ysV = VU.fromList ys
                     tT  = VU.length ysV
                     s2  = sgD * sgD
                     -- forward: err 列 (unboxed・prefix 参照の constructN)
                     errsV = VU.constructN tT $ \pre ->
                       let i = VU.length pre
                       in if i == 0
                            then VU.unsafeIndex ysV 0 - (muD + phiD * muD)
                            else VU.unsafeIndex ysV i
                                   - (muD + phiD * VU.unsafeIndex ysV (i - 1)
                                        + thD * VU.unsafeIndex pre (i - 1))
                     sumE2 = VU.foldl' (\acc e -> acc + e * e) 0 errsV
                     logL  = fromIntegral tT * (-0.5 * log (2 * pi) - log sgD)
                               - sumE2 / (2 * s2)
                     -- backward: 随伴 ē (suffix 参照の constructrN・ē_T = −e_T/σ²)
                     ebarV = VU.constructrN tT $ \suf ->
                       let t = tT - 1 - VU.length suf
                           direct = negate (VU.unsafeIndex errsV t) / s2
                       in if VU.null suf
                            then direct
                            else direct - thD * VU.unsafeIndex suf 0
                     -- 閉形式随伴 (全て Double・tape ゼロ)
                     gMu = VU.unsafeIndex ebarV 0 * negate (1 + phiD)
                             - VU.ifoldl' (\acc t eb -> if t == 0 then acc else acc + eb)
                                          0 ebarV
                     gPhi = VU.unsafeIndex ebarV 0 * negate muD
                              - VU.ifoldl' (\acc t eb ->
                                              if t == 0 then acc
                                              else acc + eb * VU.unsafeIndex ysV (t - 1))
                                           0 ebarV
                     gTh = negate (VU.ifoldl' (\acc t eb ->
                                                 if t == 0 then acc
                                                 else acc + eb * VU.unsafeIndex errsV (t - 1))
                                              0 ebarV)
                     gSg = negate (fromIntegral tT) / sgD + sumE2 / (s2 * sgD)
                     -- 軽量 scatter: g_θ を u へ連鎖 (ad は μ/φ/θ/σ の 4 scalar 抽出のみ)
                     surrogate :: forall a. (Floating a, Ord a, TrackTag a) => [a] -> a
                     surrogate uu =
                       case armaObserveOf m (paramMapOf uu) of
                         Just (mu', phi', th', sg', _) ->
                           realToFrac gMu * mu' + realToFrac gPhi * phi'
                             + realToFrac gTh * th' + realToFrac gSg * sg'
                         Nothing -> 0
                     fCombRD :: forall s. Reifies s ADRD.Tape
                             => [ADRD.ReverseDouble s] -> ADRD.ReverseDouble s
                     fCombRD uu = fRestRD uu + surrogate uu
                     (vComb, gComb) = grad' fCombRD us
                     surrAtUs = gMu * muD + gPhi * phiD + gTh * thD + gSg * sgD
                 in ( (vComb - surrAtUs) + logL
                    , VS.fromList gComb )

    -- prior + logJac (尤度除く)・'logDensityRD' 注入 — 'hmmAnalyticVG' と同じ。
    fRestRD :: forall s. Reifies s ADRD.Tape
            => [ADRD.ReverseDouble s] -> ADRD.ReverseDouble s
    fRestRD us = logPriorWith logDensityRD m (paramMapOf us)
                   + sum [ logJacF t u | (t, u) <- zip trans us ]

    -- 適格崩れ時の完全 walk+ad fallback。
    fFullA :: (Floating a, Ord a, TrackTag a) => [a] -> a
    fFullA us = logJoint m (paramMapOf us)
                  + sum [ logJacF t u | (t, u) <- zip trans us ]
    gradFullA :: [Double] -> [Double]
    gradFullA = grad fFullA

-- ---------------------------------------------------------------------------
-- Phase 101 A3: graded response IRT 尤度の解析勾配 (AD tape ゼロ)
-- ---------------------------------------------------------------------------

-- | Phase 101 A3: 尤度が **単一 'GradedResponseIrt' observe** のモデルから
-- @(θs, ncats, δs, γs, ys)@ を現在の param 値で抽出する。 walk は
-- 'armaObserveOf' と同型。
gradedIrtObserveOf :: (Floating a, Ord a)
                   => Model a r -> Map Text a
                   -> Maybe ([a], [Int], [Double], [[Double]], [Double])
gradedIrtObserveOf model params =
  case go model of
    Just [(GradedResponseIrt ths ncats dls gms, ys)] -> Just (ths, ncats, dls, gms, ys)
    _                                                -> Nothing
  where
    go (Pure _) = Just []
    go (Free (Sample n _ k)) =
      case Map.lookup n params of
        Nothing -> Nothing
        Just v  -> go (k v)
    go (Free (Observe _ d ys next)) = ((d, ys) :) <$> go next
    go (Free (ObserveLM {}))        = Nothing
    go (Free (Potential _ _ next))  = go next
    go (Free (Deterministic _ v k)) = go (k v)
    go (Free (Data _ ys k))         = go (k (map realToFrac ys, ys))
    go (Free (DataIx _ is k))       = go (k is)
    go (Free (PlateBegin _ _ next)) = go next
    go (Free (PlateEnd next))       = go next

-- | Phase 101 A3: 'GradedResponseIrt' 尤度の value+grad を **解析勾配**で
-- 計算するクロージャを構築する。 適格 (単一 GradedResponseIrt observe) の
-- ときのみ 'Just'。 構成は 'armaAnalyticVG' と同じ 3 段:
--
--   1. 各 (child i, item j, grade≠−1) の @Q_k = invlogit(δ_j(θ_i−γ_jk))@ と
--      カテゴリ確率 p (隣接差) を **Double 空間**で評価し、 値
--      @logL = Σ log p@ と解析勾配 @∂logL/∂θ_i = Σ_j (dp/dθ)/p@
--      (@dQ/dθ = δ·Q(1−Q)@ の隣接差) を算出する。
--   2. u への連鎖律は **軽量 surrogate** @Σ g_i·θ_i(u)@ を ad
--      (θs の nChild scalar 抽出のみ)。
--   3. prior + logJac は 'logDensityRD' 注入の fRestRD。
--
-- 従来 walk+ad は nChild×nItem×ncat の Q/p リスト構築 (`!!` 索引込) を毎
-- leapfrog boxed AD で再評価していた (Phase 101 A1: logCatProb 64.8% time /
-- 73.2% alloc)。 本経路は同じ O(Σ ncat) を Double で 1 パス回すだけ。
gradedIrtAnalyticVG
  :: forall r. ModelP r -> [Text] -> [Transform]
  -> Maybe (VS.Vector Double -> (Double, VS.Vector Double))
gradedIrtAnalyticVG m names trans =
  case gradedIrtObserveOf m probeParams of
    Just (ths, ncats, dls, gms, ys)
      | not (null ths), not (null ys)
      , length ncats == length dls, length ncats == length gms -> Just closure
    _ -> Nothing
  where
    probeParams :: Map Text Double
    probeParams = Map.fromList [ (nm, 0) | nm <- names ]

    paramMapOf :: forall a. Floating a => [a] -> Map Text a
    paramMapOf us = Map.fromList (zip names [ invTransformF t u | (t, u) <- zip trans us ])

    closure :: VS.Vector Double -> (Double, VS.Vector Double)
    closure uv =
      let us = VS.toList uv
      in case gradedIrtObserveOf m (paramMapOf us) of
           Nothing -> (fFullA us, VS.fromList (gradFullA us))       -- 適格崩れ: fallback
           Just (thsD, ncats, dls, gms, ys)
             | null ys -> (fFullA us, VS.fromList (gradFullA us))
             | otherwise ->
                 let nItem = length ncats
                     rows  = chunksOf nItem ys
                     -- (logL, g) を child i 毎に Double で 1 パス集計
                     childLG th row = foldl' step (0, 0) (zip4 ncats dls gms row)
                       where
                         step (accL, accG) (nc, dl, gm, grD)
                           | grD == -1 = (accL, accG)
                           | otherwise =
                               let gr   = round grD :: Int
                                   kMax = nc - 1
                                   q kk = 1 / (1 + exp (negate (dl * (th - gm !! (kk - 1)))))
                                   dq kk = let qv = q kk in dl * qv * (1 - qv)
                                   (p, dp)
                                     | gr == 1   = (1 - q 1, negate (dq 1))
                                     | gr == nc  = (q kMax, dq kMax)
                                     | otherwise = (q (gr - 1) - q gr, dq (gr - 1) - dq gr)
                               in (accL + log p, accG + dp / p)
                     lgs  = [ childLG th row | (th, row) <- zip thsD rows ]
                     logL = sum (map fst lgs)
                     gThs = map snd lgs
                     -- 軽量 scatter: g_i を u へ連鎖 (ad は θs の scalar 抽出のみ)
                     surrogate :: forall a. (Floating a, Ord a, TrackTag a) => [a] -> a
                     surrogate uu =
                       case gradedIrtObserveOf m (paramMapOf uu) of
                         Just (ths', _, _, _, _) ->
                           sum (zipWith (\g v -> realToFrac g * v) gThs ths')
                         Nothing -> 0
                     fCombRD :: forall s. Reifies s ADRD.Tape
                             => [ADRD.ReverseDouble s] -> ADRD.ReverseDouble s
                     fCombRD uu = fRestRD uu + surrogate uu
                     (vComb, gComb) = grad' fCombRD us
                     surrAtUs = sum (zipWith (*) gThs thsD)
                 in ( (vComb - surrAtUs) + logL
                    , VS.fromList gComb )

    -- prior + logJac (尤度除く)・'logDensityRD' 注入 — 'armaAnalyticVG' と同じ。
    fRestRD :: forall s. Reifies s ADRD.Tape
            => [ADRD.ReverseDouble s] -> ADRD.ReverseDouble s
    fRestRD us = logPriorWith logDensityRD m (paramMapOf us)
                   + sum [ logJacF t u | (t, u) <- zip trans us ]

    -- 適格崩れ時の完全 walk+ad fallback。
    fFullA :: (Floating a, Ord a, TrackTag a) => [a] -> a
    fFullA us = logJoint m (paramMapOf us)
                  + sum [ logJacF t u | (t, u) <- zip trans us ]
    gradFullA :: [Double] -> [Double]
    gradFullA = grad fFullA

-- | Phase 54.4b/54.6: 'gradADU' の **静的部分** (Gaussian LM ブロック抽出・
-- 設計列のベクトル化・名前→index 解決・`ad` クロージャ構築) を 1 度だけ行い、
-- unconstrained ベクトルを受けて勾配ベクトルを返すクロージャを構築する。
-- NUTS / HMC は draw ループの**外**で 1 度呼び、 全 leapfrog で再利用する。
--
-- Phase 54.6: per-op 計測 (prof-nuts-54.4e.prof) で per-call の Text-key
-- `Map.fromList` 組立 + `Map.fromListWith` 勾配集約 (compileGradU self 17.9%) と
-- vec-tape の演算毎ベクトル割当 (~52%) が残ボトルネックと確定 → 名前は compile
-- 時に index へ解決し、 勾配は ST mutable vector に解析閉形式で直接集約する
-- (Gaussian LM の勾配は ∂β_k=X_kᵀr/σ²・∂u_j=Σ_{i∈g_j}r_i/σ²・∂σ=-n/σ+sumR2/σ³
-- の閉形式ゆえ汎用 tape 不要)。
compileGradUV :: forall r. ModelP r -> [Text] -> [Transform]
              -> (VS.Vector Double -> VS.Vector Double)
compileGradUV m names trans =
  case gaussLMBlocksAuto m of
    ([], _) -> case synthVecIR m of
      -- Phase 95: 尤度が単一 dense MvNormal observe なら解析随伴。 Gp-RBF (B-dsl・
      -- 閉形式随伴) を最優先、 次に汎用 MvNormal (A案・flat detach)、 いずれも
      -- 非適格なら従来の全体 ad (後方互換)。
      Nothing -> case gpRBFAnalyticVG m names trans of
        Just vg -> \uv -> snd (vg uv)
        Nothing -> case mvNormalAnalyticVG m names trans of
          Just vg -> \uv -> snd (vg uv)
          Nothing -> \uv -> VS.fromList (gradFull (VS.toList uv))
      Just (gs, fams, sObs) ->                  -- 54.11: ベクトル式 IR (非線形 μ)
        let ixOf   = Map.fromList (zip names [0 ..])
            nP     = length names
            transB = BV.fromList trans
            cvi    = compileVecIR ixOf gs fams
            famSet = Set.fromList (concat [ ms | (ms, _, _) <- fams ])
            cps    = constPriorsOf m famSet
            lnGroups = collectLogNormalGroups m        -- Phase 98 A3: LogNormal 群
            lnUNames = concat [ us | (us, _, _) <- lnGroups ]
            lnIx   = map (resolveLogNormal ixOf) lnGroups
            exclNames = sObs `Set.union` famSet
                        `Set.union` Set.fromList (map fst cps)
                        `Set.union` Set.fromList lnUNames
            noResid = residualFreeOfDensity exclNames m
            cpIx   = [ (ixOf Map.! n, d) | (n, d) <- cps ]
            mPriorGrad
              | noResid   = Nothing
              | otherwise = Just (grad (fExcl (compileResidual exclNames m) exclNames))
        in \uv ->
             let pc = VS.generate nP $ \i ->
                        invTransformF (transB BV.! i) (uv `VS.unsafeIndex` i)
                 mgc = runST $ do
                   mg <- VSM.replicate nP 0
                   ok <- gradVecIR cvi pc mg
                   if ok
                     then do
                       mapM_ (\(i, d) ->
                                case constPriorGradD d (pc `VS.unsafeIndex` i) of
                                  Just g  -> VSM.modify mg (+ g) i
                                  Nothing -> pure ()) cpIx
                       mapM_ (\ln -> gradLogNormalIx ln pc mg) lnIx  -- A3
                       Just <$> VS.unsafeFreeze mg
                     else pure Nothing
             in case mgc of
                  -- guard 違反 (観測項が定数 -∞ の境界領域・例 invLogit の FP
                  -- 飽和 p==1): walk+ad に per-call fallback し従来経路と同一の
                  -- 勾配 (違反行 = 定数 → 勾配 0・他の行は有効) を返す。
                  -- ★旧 tape (54.11-55.4) は unguarded で NaN が全勾配を汚染し
                  -- NUTS が max-depth 迷走する潜在バグだった (56.2 で修正・
                  -- 'constPriorGradD' の「guard 違反 = 勾配 0 で ad と一致」 と
                  -- 同じ原則)。 境界点のみの稀ケースゆえ per-draw 影響なし。
                  Nothing -> VS.fromList (gradFull (VS.toList uv))
                  Just gC -> case mPriorGrad of
                    Nothing ->
                      VS.generate nP $ \i ->
                        let t = transB BV.! i
                            u = uv `VS.unsafeIndex` i
                        in gC `VS.unsafeIndex` i * dInvTransform t u
                           + dLogJacU t u
                    Just priorGrad ->
                      let pg = priorGrad (VS.toList uv)
                      in VS.fromList
                           [ pg_i + gC `VS.unsafeIndex` i * dInvTransform t u
                           | (i, (pg_i, (t, u))) <- zip [0 :: Int ..]
                               (zip pg (zip trans (VS.toList uv))) ]
    (gbs, synthObs) ->                                    -- ハイブリッド (静的 hoist)
      let -- 54.4c: REff (Just scale) の u-prior は解析勾配・u_j を ad から除外。
          -- 54.4e: 定数パラメタ prior も解析勾配・ad から除外。 密度項が残らな
          -- ければ ad クロージャを丸ごと省略 (logJac 勾配も解析式)。
          (priorREs, cps, exclNames, cblocks, hierGroups, noResid) = analyzeGaussModel m gbs synthObs
          ixOf   = Map.fromList (zip names [0 ..])
          nP     = length names
          transB = BV.fromList trans                      -- boxed (Storable 不可)
          cbIx   = map (resolveLMBlock ixOf) cblocks
          reIx   = [ ReffPriorIx (VU.fromList (map (ixOf Map.!) uNames))
                                 (ixOf Map.! scaleName)
                   | (uNames, scaleName) <- priorREs ]
          hniIx  = map (resolveHierNormal ixOf) hierGroups
          cpIx   = [ (ixOf Map.! n, d) | (n, d) <- cps ]
          mPriorGrad                                      -- 残 prior 等の ad (fallback)
            | noResid   = Nothing
            | otherwise = Just (grad (fExcl (compileResidual exclNames m) exclNames))
      in hybridGradClosure nP transB trans
           (\pc mg -> do
              mapM_ (\cb -> gradLMBlockIx cb pc mg) cbIx
              mapM_ (\ri -> gradReffPriorIx ri pc mg) reIx
              mapM_ (\hn -> gradHierNormalIx hn pc mg) hniIx
              mapM_ (\(i, d) ->
                       case constPriorGradD d (pc `VS.unsafeIndex` i) of
                         Just g  -> VSM.modify mg (+ g) i
                         Nothing -> pure ()) cpIx)
           mPriorGrad
  where
    gradFull = grad fFull
    fFull us =
      let paramsC = Map.fromList
            [ (n, invTransformF t u) | (n, t, u) <- zip3 names trans us ]
          logJac  = sum [ logJacF t u | (t, u) <- zip trans us ]
      in logJoint m paramsC + logJac
    fExcl mcr excl us =
      let paramsC = Map.fromList
            [ (n, invTransformF t u) | (n, t, u) <- zip3 names trans us ]
          logJac  = sum [ logJacF t u | (t, u) <- zip trans us ]
      in residualExcl mcr excl m paramsC + logJac

-- | Phase 87.2b: 'compileGradUV' の value-and-grad 融合版 (JAX @value_and_grad@
-- 相当)。 返り値 = (logπ(u) (logJac 込・'compileLogPUV' と同値)・∇logπ(u)
-- ('compileGradUV' と同値))。 NUTS の葉は leapfrog 最終勾配とエネルギー (logπ)
-- を同一点で別々に評価していた (prof 実測で葉 logPi が全体の 19%) — 本閉包は
-- forward pass を 1 度だけ走らせて両方を返す。 経路分岐・fallback 意味論は
-- 'compileGradUV' / 'compileLogPUV' と対 (vecIR guard 違反 = 値 -∞ +
-- 勾配 walk+ad fallback)。
compileGradValUV :: forall r. ModelP r -> [Text] -> [Transform]
                 -> (VS.Vector Double -> (Double, VS.Vector Double))
compileGradValUV m names trans =
  case gradValPlan m names trans of
    GVPure f -> f
    GVVecIR sz prep core finish -> \uv ->
      let pc  = prep uv
          mvg = runST $ do
            ar  <- VSM.unsafeNew sz
            adj <- VSM.unsafeNew sz
            core ar adj pc
      in finish uv pc mvg

-- | Phase 90 A11-4①: 'compileGradValUV' の monadic 版。 vecIR 経路の作業
-- バッファ (forward arena + 随伴 arena・13-traffic 実測で 34k セル × 2 =
-- 葉勾配 0.139ms 中の確保 0.031ms + GC churn) を **閉包生成時に 1 度だけ**
-- 確保し、 全 leapfrog 呼出で再利用する。 閉包は chain ごとに生成される
-- ('nutsStream' 内) ため、 chain 横断 spark 並列 ('nutsChainsPure') とも
-- 干渉しない。 返る値/勾配は毎回 fresh に freeze されるので alias しない。
-- 非 vecIR 経路 (walk+ad / ハイブリッド) は従来 pure 閉包をそのまま包む。
compileGradValUVM :: forall m r. PrimMonad m
                  => ModelP r -> [Text] -> [Transform]
                  -> m (VS.Vector Double -> m (Double, VS.Vector Double))
compileGradValUVM m names trans =
  case gradValPlan m names trans of
    GVPure f -> pure (\uv -> pure (f uv))
    GVVecIR sz prep core finish -> do
      ar  <- VSM.unsafeNew sz
      adj <- VSM.unsafeNew sz
      pure $ \uv -> do
        let pc = prep uv
        mvg <- stToPrim (core ar adj pc)
        pure (finish uv pc mvg)

-- | Phase 90 A11-4①: 'compileGradValUV' / 'compileGradValUVM' が共有する
-- 静的解析結果。 vecIR 経路のみ per-call の arena/adj 確保をバッファ注入
-- (prep / core / finish の 3 分割) に分離し、 pure 版 (毎回確保・従来意味論)
-- と monadic 版 (chain 閉包で 1 回確保) が同一 per-call コードを共有する。
data GradValPlan
  = GVPure (VS.Vector Double -> (Double, VS.Vector Double))
    -- ^ walk+ad fallback / ハイブリッド経路 (arena 非使用・従来 pure 閉包)。
  | GVVecIR
      !Int                                    -- ^ arena/adj サイズ ('vpSize')
      (VS.Vector Double -> VS.Vector Double)  -- ^ prep: uv → pc (invTransform)
      (forall s. VSM.MVector s Double -> VSM.MVector s Double
                 -> VS.Vector Double
                 -> ST s (Maybe (Double, VS.Vector Double)))
        -- ^ core: ar adj pc → (値, constrained 勾配)。 guard 違反 = Nothing。
      (VS.Vector Double -> VS.Vector Double
                 -> Maybe (Double, VS.Vector Double)
                 -> (Double, VS.Vector Double))
        -- ^ finish: uv pc mvg → 最終 (logπ, ∇logπ) (chain rule + fallback)。

gradValPlan :: forall r. ModelP r -> [Text] -> [Transform] -> GradValPlan
gradValPlan m names trans =
  case gaussLMBlocksAuto m of
    ([], _) -> case synthVecIR m of
      -- Phase 95: 尤度が単一 dense MvNormal observe なら解析随伴 (pure 閉包 = GVPure)。
      -- Phase 92: 単一 HmmForwardNormal observe も同様 (forward-backward 閉形式)。
      -- Phase 101: 単一 ArmaNormal observe も同様 (逆向き随伴再帰の閉形式)。
      -- HMM → ARMA → Gp-RBF (B-dsl・閉形式) → 汎用 MvNormal (A案) → 従来の全体 walk+ad。
      Nothing -> case hmmAnalyticVG m names trans of
        Just vg -> GVPure vg
        Nothing -> case armaAnalyticVG m names trans of
          Just vg -> GVPure vg
          Nothing -> case gradedIrtAnalyticVG m names trans of
           Just vg -> GVPure vg
           Nothing -> case gpRBFAnalyticVG m names trans of
            Just vg -> GVPure vg
            Nothing -> case mvNormalAnalyticVG m names trans of
              Just vg -> GVPure vg
              Nothing -> GVPure $ \uv ->        -- 後方互換: 全体を walk + ad (融合なし)
                let us = VS.toList uv
                in (fFull us, VS.fromList (gradFull us))
      Just (gs, fams, sObs) ->                -- ベクトル式 IR (compileGradUV と同静的)
        let ixOf   = Map.fromList (zip names [0 ..])
            nP     = length names
            transB = BV.fromList trans
            cvi    = compileVecIR ixOf gs fams
            famSet = Set.fromList (concat [ ms | (ms, _, _) <- fams ])
            cps    = constPriorsOf m famSet
            lnGroups = collectLogNormalGroups m        -- Phase 98 A3: LogNormal 群
            lnUNames = concat [ us | (us, _, _) <- lnGroups ]
            lnIx   = map (resolveLogNormal ixOf) lnGroups
            exclNames = sObs `Set.union` famSet
                        `Set.union` Set.fromList (map fst cps)
                        `Set.union` Set.fromList lnUNames
            noResid = residualFreeOfDensity exclNames m
            cpIx   = [ (ixOf Map.! n, d) | (n, d) <- cps ]
            mPrior                             -- (勾配, 値) の対 (fExcl は logJac 込)
              | noResid   = Nothing
              | otherwise = let mcr = compileResidual exclNames m
                            in Just (grad (fExcl mcr exclNames), fExcl mcr exclNames)
            prep uv = VS.generate nP $ \i ->
                        invTransformF (transB BV.! i) (uv `VS.unsafeIndex` i)
            core :: forall s. VSM.MVector s Double -> VSM.MVector s Double
                 -> VS.Vector Double -> ST s (Maybe (Double, VS.Vector Double))
            core ar adj pc = do
              mg <- VSM.replicate nP 0
              mv <- gradVecIRValWith cvi ar adj pc mg
              case mv of
                Nothing -> pure Nothing
                Just v  -> do
                  mapM_ (\(i, d) ->
                           case constPriorGradD d (pc `VS.unsafeIndex` i) of
                             Just g  -> VSM.modify mg (+ g) i
                             Nothing -> pure ()) cpIx
                  mapM_ (\ln -> gradLogNormalIx ln pc mg) lnIx  -- A3
                  gv <- VS.unsafeFreeze mg
                  pure (Just (v, gv))
            finish uv pc mvg = case mvg of
              -- guard 違反: 値 = -∞ ('vecIRValue' と同一)・勾配 = walk+ad
              -- fallback ('compileGradUV' と同一)。
              Nothing -> ((-1) / 0, VS.fromList (gradFull (VS.toList uv)))
              Just (vIR, gC) ->
                let cpVal = sum [ logDensity d (pc `VS.unsafeIndex` i)
                                | (i, d) <- cpIx ]
                          + sum [ valueLogNormalIx ln pc | ln <- lnIx ]  -- A3
                in case mPrior of
                  Nothing ->
                    let logJac = sum [ logJacF (transB BV.! i)
                                               (uv `VS.unsafeIndex` i)
                                     | i <- [0 .. nP - 1] ]
                        g = VS.generate nP $ \i ->
                              let t = transB BV.! i
                                  u = uv `VS.unsafeIndex` i
                              in gC `VS.unsafeIndex` i * dInvTransform t u
                                 + dLogJacU t u
                    in (vIR + cpVal + logJac, g)
                  Just (priorGrad, priorVal) ->
                    let us = VS.toList uv
                        pg = priorGrad us
                        g = VS.fromList
                              [ pg_i + gC `VS.unsafeIndex` i * dInvTransform t u
                              | (i, (pg_i, (t, u))) <- zip [0 :: Int ..]
                                  (zip pg (zip trans us)) ]
                    in (vIR + cpVal + priorVal us, g)
        in GVVecIR (vpSize (cvProg cvi)) prep core finish
    (gbs, synthObs) -> GVPure $               -- ハイブリッド (compileGradUV と同静的)
      let (priorREs, cps, exclNames, cblocks, hierGroups, noResid) = analyzeGaussModel m gbs synthObs
          ixOf   = Map.fromList (zip names [0 ..])
          nP     = length names
          transB = BV.fromList trans
          cbIx   = map (resolveLMBlock ixOf) cblocks
          reIx   = [ ReffPriorIx (VU.fromList (map (ixOf Map.!) uNames))
                                 (ixOf Map.! scaleName)
                   | (uNames, scaleName) <- priorREs ]
          hniIx  = map (resolveHierNormal ixOf) hierGroups
          cpIx   = [ (ixOf Map.! n, d) | (n, d) <- cps ]
          mPrior
            | noResid   = Nothing
            | otherwise = let mcr = compileResidual exclNames m
                          in Just (grad (fExcl mcr exclNames), fExcl mcr exclNames)
      in hybridGradValClosure nP transB trans
           (\pc mg -> do
              mapM_ (\cb -> gradLMBlockIx cb pc mg) cbIx
              mapM_ (\ri -> gradReffPriorIx ri pc mg) reIx
              mapM_ (\hn -> gradHierNormalIx hn pc mg) hniIx
              mapM_ (\(i, d) ->
                       case constPriorGradD d (pc `VS.unsafeIndex` i) of
                         Just g  -> VSM.modify mg (+ g) i
                         Nothing -> pure ()) cpIx)
           (\pc -> sum [ valueLMBlockIx cb pc | cb <- cbIx ]
                   + sum [ valueReffPriorIx ri pc | ri <- reIx ]
                   + sum [ valueHierNormalIx hn pc | hn <- hniIx ]
                   + sum [ logDensity d (pc `VS.unsafeIndex` i)
                         | (i, d) <- cpIx ])
           mPrior
  where
    gradFull = grad fFull
    fFull us =
      let paramsC = Map.fromList
            [ (n, invTransformF t u) | (n, t, u) <- zip3 names trans us ]
          logJac  = sum [ logJacF t u | (t, u) <- zip trans us ]
      in logJoint m paramsC + logJac
    fExcl mcr excl us =
      let paramsC = Map.fromList
            [ (n, invTransformF t u) | (n, t, u) <- zip3 names trans us ]
          logJac  = sum [ logJacF t u | (t, u) <- zip trans us ]
      in residualExcl mcr excl m paramsC + logJac

-- | Phase 87.2b: 'hybridGradClosure' の value-and-grad 融合版。 解析勾配
-- (@gradC@) に加えて解析**値** (@valC@ = 'compileLogPUV' ハイブリッド経路の
-- analytic と同一) を計算し、 (値 (logJac 込)・勾配) を返す。
hybridGradValClosure
  :: Int -> BV.Vector Transform -> [Transform]
  -> (forall s. VS.Vector Double -> VSM.MVector s Double -> ST s ())
  -> (VS.Vector Double -> Double)
  -> Maybe ([Double] -> [Double], [Double] -> Double)
  -> (VS.Vector Double -> (Double, VS.Vector Double))
hybridGradValClosure nP transB trans gradC valC mPrior = \uv ->
  let pc = VS.generate nP $ \i ->
             invTransformF (transB BV.! i) (uv `VS.unsafeIndex` i)
      gC = runST $ do
        mg <- VSM.replicate nP 0
        gradC pc mg
        VS.unsafeFreeze mg
      aVal = valC pc
  in case mPrior of
       Nothing ->
         let logJac = sum [ logJacF (transB BV.! i) (uv `VS.unsafeIndex` i)
                          | i <- [0 .. nP - 1] ]
             g = VS.generate nP $ \i ->
                   let t = transB BV.! i
                       u = uv `VS.unsafeIndex` i
                   in gC `VS.unsafeIndex` i * dInvTransform t u + dLogJacU t u
         in (aVal + logJac, g)
       Just (priorGrad, priorVal) ->            -- fExcl は logJac 込
         let us = VS.toList uv
             pg = priorGrad us
             g = VS.fromList
                   [ p + gC `VS.unsafeIndex` i * dInvTransform t u
                   | (i, (p, (t, u))) <- zip [0 :: Int ..]
                       (zip pg (zip trans us)) ]
         in (aVal + priorVal us, g)

-- | 'compileGradUV' の per-call 本体 (Phase 54.11 で affine 経路と IR 経路の
-- 共有部を関数化): unconstrained ベクトル → constrained 値 → 解析/ベクトル
-- 経路の constrained 勾配 (@gradC@ が mutable ベクトルへ加算) → chain rule。
-- @mPriorGrad@ = 残差 ad クロージャ ('Nothing' = 密度項が残らず logJac も解析)。
hybridGradClosure
  :: Int -> BV.Vector Transform -> [Transform]
  -> (forall s. VS.Vector Double -> VSM.MVector s Double -> ST s ())
  -> Maybe ([Double] -> [Double])
  -> (VS.Vector Double -> VS.Vector Double)
hybridGradClosure nP transB trans gradC mPriorGrad = \uv ->
  let pc = VS.generate nP $ \i ->
             invTransformF (transB BV.! i) (uv `VS.unsafeIndex` i)
      gC = runST $ do                            -- constrained 空間の解析勾配
        mg <- VSM.replicate nP 0
        gradC pc mg
        VS.unsafeFreeze mg
  in case mPriorGrad of
       Nothing ->                                -- ad 完全省略 (logJac 解析)
         VS.generate nP $ \i ->
           let t = transB BV.! i
               u = uv `VS.unsafeIndex` i
           in gC `VS.unsafeIndex` i * dInvTransform t u + dLogJacU t u
       Just priorGrad ->                         -- 残りは ad (chain は ad 内)
         let pg = priorGrad (VS.toList uv)
         in VS.fromList
              [ p + gC `VS.unsafeIndex` i * dInvTransform t u
              | (i, (p, (t, u))) <- zip [0 :: Int ..]
                  (zip pg (zip trans (VS.toList uv))) ]

-- | Phase 54.4e: **定数パラメタ prior** の解析勾配 @d logDensity(d, θ)/dθ@
-- (constrained 空間)。 @Nothing@ = 未対応分布 (従来 `ad` に fallback)。
--
-- prior のパラメタが他 latent に依存しない (extractDeps で deps ∅) latent に
-- のみ使う前提 (パラメタを定数として θ でだけ微分する)。 各分岐は 'logDensity'
-- の実装・ガードと対にしてある: ガード違反域では 'logDensity' が定数 negInf を
-- 返し `ad` の勾配は 0 になるので、 ここでも 0 を返して一致させる。
constPriorGradD :: Distribution Double -> Double -> Maybe Double
constPriorGradD d x = case d of
  Normal mu sig
    | sig <= 0           -> Just 0
    | otherwise          -> Just (negate (x - mu) / (sig * sig))
  Exponential rate
    | x < 0 || rate <= 0 -> Just 0
    | otherwise          -> Just (negate rate)
  Gamma shape rate
    | x <= 0 || shape <= 0 || rate <= 0 -> Just 0
    | otherwise          -> Just ((shape - 1) / x - rate)
  Beta alpha beta
    | x <= 0 || x >= 1 || alpha <= 0 || beta <= 0 -> Just 0
    | otherwise          -> Just ((alpha - 1) / x - (beta - 1) / (1 - x))
  Uniform lo hi
    | hi <= lo || x < lo || x > hi -> Just 0
    | otherwise          -> Just 0
  StudentT df mu sig
    | df <= 0 || sig <= 0 -> Just 0
    | otherwise          ->
        let z = x - mu
        in Just (negate ((df + 1) * z) / (df * sig * sig + z * z))
  Cauchy loc sc
    | sc <= 0            -> Just 0
    | otherwise          ->
        let z = x - loc
        in Just (negate (2 * z) / (sc * sc + z * z))
  HalfNormal sig
    | sig <= 0 || x < 0  -> Just 0
    | otherwise          -> Just (negate x / (sig * sig))
  HalfCauchy sc
    | sc <= 0 || x < 0   -> Just 0
    | otherwise          -> Just (negate (2 * x) / (sc * sc + x * x))
  LogNormal mu sig
    | sig <= 0 || x <= 0 -> Just 0
    | otherwise          ->
        Just (negate (1 + (log x - mu) / (sig * sig)) / x)
  InverseGamma alpha beta
    | alpha <= 0 || beta <= 0 || x <= 0 -> Just 0
    | otherwise          -> Just (negate (alpha + 1) / x + beta / (x * x))
  Weibull kShape lam
    | kShape <= 0 || lam <= 0 || x <= 0 -> Just 0
    | otherwise          ->
        Just ((kShape - 1) / x - (kShape / lam) * (x / lam) ** (kShape - 1))
  Pareto alpha xm
    | alpha <= 0 || xm <= 0 || x < xm -> Just 0
    | otherwise          -> Just (negate (alpha + 1) / x)
  _ -> Nothing

-- | 'logJacF' の u 微分 (Phase 54.4e: ad 省略時に解析で加算)。 'logJacF' と対。
dLogJacU :: Transform -> Double -> Double
dLogJacU UnconstrainedT _ = 0
dLogJacU PositiveT      _ = 1
dLogJacU UnitIntervalT  u = let s = 1 / (1 + exp (-u)) in 1 - 2 * s

-- | Phase 54.4e: @excl@ 除外後の walk に log-density 寄与が残らないか。
-- 残らなければ 'compileGradU' は `ad` クロージャを丸ごと省略でき
-- (reflection tape 生成 = profile の 18.9% がゼロに)、 'compileLogPU' は
-- Free walk 自体を省略できる。 scalar 'Observe' は名前が @excl@ になければ
-- False (Phase 54.8: 自動合成で吸収済みの Observe は除外扱い)。
-- 'Potential' があれば常に False (従来 ad / walk 経路に fallback・正しさ担保)。
residualFreeOfDensity :: Set Text -> Model Double r -> Bool
residualFreeOfDensity excl = go
  where
    go (Pure _) = True
    go (Free (Sample n _ k)) = n `Set.member` excl && go (k 0)
    go (Free (Observe n _ _ next)) = n `Set.member` excl && go next
    go (Free (ObserveLM nm _ _ _ _ _ next)) = nm `Set.member` excl && go next
    -- Phase 90 A10: vecIR ('VGPot') に吸収済みの potential は残差に数えない。
    go (Free (Potential n _ next)) = n `Set.member` excl && go next
    go (Free (Deterministic _ v k)) = go (k v)
    go (Free (Data _ ys k)) = go (k (ys, ys))
    go (Free (DataIx _ is k)) = go (k is)
    go (Free (PlateBegin _ _ next)) = go next
    go (Free (PlateEnd next)) = go next

-- | Phase 54.4e: 'compileGradU' / 'compileLogPU' 共通の静的解析。
-- Gaussian LM ブロック群から (ブロック名, 解析 u-prior, 定数パラメタ prior,
-- 除外集合, 前処理済みブロック, residual 空フラグ) を 1 度だけ求める。
-- @synthObs@ (Phase 54.8) = 自動合成ブロックに吸収済みの scalar 'Observe' 名
-- (除外集合に合流させ、 residual walk で二重加算しない)。
analyzeGaussModel
  :: ModelP r
  -> [(Text, [Text], [[Double]], [REff], Text, [Double])]
  -> Set Text                              -- synthObs (吸収済 scalar Observe 名)
  -> ( [([Text], Text)]                    -- priorREs (uNames, scaleName)
     , [(Text, Distribution Double)]       -- constPriors
     , Set Text                            -- exclNames
     , [CompiledLMBlock]
     , [([Text], Text, Text)]              -- Phase 93: 階層 Normal 群 (uNames, μ名, τ名)
     , Bool )                              -- residual に密度項が残らないか
analyzeGaussModel m gbs synthObs =
  let blockNames = [ bn | (bn, _, _, _, _, _) <- gbs ]
      priorREs   = [ (uNames, scaleName)
                   | (_, _, _, res, _, _) <- gbs
                   , REff uNames _ (Just scaleName) _ _ <- res ]
      exclUNames = concat [ uNames | (uNames, _) <- priorREs ]
      -- Phase 93: 非ゼロ latent 平均の階層 Normal prior 群 (mean-0 reff とは disjoint)。
      -- u_i の prior を解析勾配 ('gradHierNormalIx') で扱い残差 ad から外す。
      -- μ・τ は自身の prior を持つので cps 側に残す (u のみ除外)。
      hierGroups = collectHierNormalGroups m
      hierUNames = concat [ us | (us, _, _) <- hierGroups ]
      -- 定数パラメタ prior。 u_j (REff / 階層群 経由で解析済) は除く。
      cps = constPriorsOf m (Set.fromList (exclUNames ++ hierUNames))
      exclNames = Set.fromList (blockNames ++ exclUNames ++ hierUNames ++ map fst cps)
                  `Set.union` synthObs
      cblocks   = [ compileLMBlock (bs, xs, re, sn, ys)
                  | (_, bs, xs, re, sn, ys) <- gbs ]
      noResid   = residualFreeOfDensity exclNames m
  in (priorREs, cps, exclNames, cblocks, hierGroups, noResid)

-- | 定数パラメタ prior の抽出 (Phase 54.4e): extractDeps で親 latent 無し
-- (deps ∅) かつ解析勾配対応分布の latent。 @exclSet@ = 別経路 (REff 族 /
-- 54.11 IR 族) で扱う latent は除く。 54.4e/54.11 で共有。
constPriorsOf :: ModelP r -> Set Text -> [(Text, Distribution Double)]
constPriorsOf m exclSet =
  let (depNodes, _) = extractDeps m
      latentDeps = Map.fromList [ (nodeName nd, nodeDeps nd)
                                | nd <- depNodes, nodeKind nd == LatentN ]
  in [ (n, dist)
     | (n, dist) <- priorList m
     , not (Set.member n exclSet)
     , Just deps <- [Map.lookup n latentDeps], Set.null deps
     , Just _ <- [constPriorGradD dist 0.5] ]

-- | Phase 54.4d: logp **値** 評価のコンパイル ('compileGradU' の値版)。
--
-- NUTS は tree node ごとにエネルギー (logp の値) を評価する。 54.4c 時点の
-- cost-centre profile で、 勾配は vec 化済みなのに値評価が Free walk +
-- per-obs スカラ 'logDensityObs' のままで per-draw の 46% を占めると判明
-- (`prof-nuts-54.4c.prof`)。 本関数は 'compileGradU' と同じ静的前処理
-- ('CompiledLMBlock') を 1 度だけ行い、 unconstrained ベクトルを受けて
-- log-joint + log-jacobian を返すクロージャを構築する:
--
--   * Gaussian-恒等リンク 'ObserveLM' ブロックの観測尤度値 → 素な Double
--     ベクトル演算 ('valueCompiledLMBlock'・tape 不要)
--   * @REff (Just scale)@ の u-prior 値 → 解析式 ('reffPriorValue')
--   * 残り (他 prior / scalar observe / 非 Gauss LM / jacobian)
--     → 'logJointExclBlocks' の Double walk
--
-- Gaussian LM を含まないモデルは従来 'logJointUnconstrained' 相当に fallback
-- (後方互換)。 数値は 'logJointUnconstrained' と一致 (test で担保)。
compileLogPU :: forall r. ModelP r -> [Text] -> [Transform] -> ([Double] -> Double)
compileLogPU m names trans =
  let lv = compileLogPUV m names trans
  in lv . VS.fromList

-- | 'compileLogPU' の vector-native 版 (Phase 54.6)。 NUTS のエネルギー評価が
-- 直接使う。 名前は compile 時に index へ解決し、 per-call は Storable vector
-- 上の素な Double 演算のみ (Text-key Map 組立なし)。
compileLogPUV :: forall r. ModelP r -> [Text] -> [Transform]
              -> (VS.Vector Double -> Double)
compileLogPUV m names trans =
  case gaussLMBlocksAuto m of
    ([], _) -> case synthVecIR m of
      Nothing -> fFull . VS.toList                     -- 後方互換: 従来の walk 評価
      Just (gs, fams, sObs) ->                 -- 54.11: ベクトル式 IR (非線形 μ)
        let ixOf   = Map.fromList (zip names [0 ..])
            nP     = length names
            transB = BV.fromList trans
            cvi    = compileVecIR ixOf gs fams
            famSet = Set.fromList (concat [ ms | (ms, _, _) <- fams ])
            cps    = constPriorsOf m famSet
            lnGroups = collectLogNormalGroups m        -- Phase 98 A3: LogNormal 群
            lnUNames = concat [ us | (us, _, _) <- lnGroups ]
            lnIx   = map (resolveLogNormal ixOf) lnGroups
            exclNames = sObs `Set.union` famSet
                        `Set.union` Set.fromList (map fst cps)
                        `Set.union` Set.fromList lnUNames
            noResid = residualFreeOfDensity exclNames m
            cpIx   = [ (ixOf Map.! n, d) | (n, d) <- cps ]
            mResid
              | noResid   = Nothing
              | otherwise = Just (residualExcl (compileResidual exclNames m) exclNames m)
        in hybridLogPClosure nP transB names
             (\pc -> vecIRValue cvi pc
                     + sum [ logDensity d (pc `VS.unsafeIndex` i)
                           | (i, d) <- cpIx ]
                     + sum [ valueLogNormalIx ln pc | ln <- lnIx ])  -- A3
             mResid
    (gbs, synthObs) ->
      let -- 54.4c/54.4e と同じ静的解析: u-prior は解析値・定数パラメタ prior は
          -- 直接 logDensity・残りだけ walk。 密度項が残らなければ Free walk 自体を
          -- 省略する (モデル再構築 = reNormal の Text 名生成等も消える)。
          (priorREs, cps, exclNames, cblocks, hierGroups, noResid) = analyzeGaussModel m gbs synthObs
          ixOf   = Map.fromList (zip names [0 ..])
          nP     = length names
          transB = BV.fromList trans
          cbIx   = map (resolveLMBlock ixOf) cblocks
          reIx   = [ ReffPriorIx (VU.fromList (map (ixOf Map.!) uNames))
                                 (ixOf Map.! scaleName)
                   | (uNames, scaleName) <- priorREs ]
          hniIx  = map (resolveHierNormal ixOf) hierGroups
          cpIx   = [ (ixOf Map.! n, d) | (n, d) <- cps ]
          mResid                                       -- 残 walk (fallback のみ)
            | noResid   = Nothing
            | otherwise = Just (residualExcl (compileResidual exclNames m) exclNames m)
      in hybridLogPClosure nP transB names
           (\pc -> sum [ valueLMBlockIx cb pc | cb <- cbIx ]
                   + sum [ valueReffPriorIx ri pc | ri <- reIx ]
                   + sum [ valueHierNormalIx hn pc | hn <- hniIx ]
                   + sum [ logDensity d (pc `VS.unsafeIndex` i)
                         | (i, d) <- cpIx ])
           mResid
  where
    fFull us =
      let paramsC = Map.fromList
            [ (n, invTransformF t u) | (n, t, u) <- zip3 names trans us ]
          logJac  = sum [ logJacF t u | (t, u) <- zip trans us ]
      in logJoint m paramsC + logJac

-- | 'compileLogPUV' の per-call 本体 (Phase 54.11 で affine 経路と IR 経路の
-- 共有部を関数化): unconstrained ベクトル → constrained 値 → 解析/ベクトル
-- 経路の log-density 値 (@analytic@) + 残差 walk (@mResid@) + log-jacobian。
hybridLogPClosure
  :: Int -> BV.Vector Transform -> [Text]
  -> (VS.Vector Double -> Double)
  -> Maybe (Map Text Double -> Double)
  -> (VS.Vector Double -> Double)
hybridLogPClosure nP transB names analytic mResid = \uv ->
  let pc = VS.generate nP $ \i ->
             invTransformF (transB BV.! i) (uv `VS.unsafeIndex` i)
      logJac = sum [ logJacF (transB BV.! i) (uv `VS.unsafeIndex` i)
                   | i <- [0 .. nP - 1] ]
      residV = case mResid of
        Nothing -> 0
        Just rv -> rv (Map.fromList (zip names (VS.toList pc)))
  in residV + analytic pc + logJac

-- | invTransform の導関数 dθ/du (chain rule 用)。 'invTransformF' と対。
dInvTransform :: Transform -> Double -> Double
dInvTransform UnconstrainedT _ = 1
dInvTransform PositiveT      u = exp u
dInvTransform UnitIntervalT  u = let s = 1 / (1 + exp (-u)) in s * (1 - s)

-- | モデル中の Gaussian-恒等リンク 'ObserveLM' ブロックを収集する
-- (ブロック名 / β 名 / 設計行列 / ランダム効果 / σ 名 / 観測 ys)。 非 Gaussian は除外。
gaussLMBlocks :: ModelP r -> [(Text, [Text], [[Double]], [REff], Text, [Double])]
gaussLMBlocks m = go m []
  where
    go (Pure _) acc = reverse acc
    go (Free f) acc = case f of
      Sample _ _ k        -> go (k 0) acc
      Observe _ _ _ next  -> go next acc
      ObserveLM nm bs xs re fam ys next ->
        case fam of
          LMGaussian sn -> go next ((nm, bs, xs, re, sn, ys) : acc)
          _             -> go next acc
      Potential _ _ next  -> go next acc
      Deterministic _ v k -> go (k v) acc
      Data _ ys k         -> go (k (ys, ys)) acc
      DataIx _ is k       -> go (k is) acc
      PlateBegin _ _ next -> go next acc
      PlateEnd next       -> go next acc

-- | 'gaussLMBlocks' + Phase 54.8 自動合成。 明示 'ObserveLM' ブロックに、
-- per-obs scalar 'Observe' から自動合成したブロックを連結して返す
-- (合成に吸収した scalar Observe 名集合も返す → 'analyzeGaussModel' で除外)。
gaussLMBlocksAuto
  :: ModelP r
  -> ([(Text, [Text], [[Double]], [REff], Text, [Double])], Set Text)
gaussLMBlocksAuto m =
  let (sblocks, sObs) = synthGaussLMBlocks m
  in (gaussLMBlocks m ++ sblocks, sObs)


-- | 'logJoint' と同じだが、 名前が @excl@ に含まれる項を **加算しない**:
--
--   * @excl@ に含まれる 'ObserveLM' ブロックの観測尤度 (vec-tape 経路で別計算)
--   * @excl@ に含まれる scalar 'Observe' の観測尤度
--     (Phase 54.8: 'synthGaussLMBlocks' が合成ブロックへ吸収済みのもの)
--   * @excl@ に含まれる 'Sample' ノードの prior log-density
--     (Phase 54.4c: 群効果 @u_j@ の prior を解析勾配経路で別計算するため)。
--     値は継続に必要なので 'Sample' 自体は walk するが log-density は足さない。
logJointExclBlocks :: (Floating a, Ord a)
                   => Set Text -> Model a r -> Map Text a -> a
logJointExclBlocks excl model params = go model 0
  where
    go (Pure _) acc = acc
    go (Free (Sample n d k)) acc =
      case Map.lookup n params of
        Nothing -> negInf
        Just v
          | n `Set.member` excl -> go (k v) acc
          | otherwise           -> go (k v) (acc + logDensity d v)
    go (Free (Observe n d ys next)) acc
      | n `Set.member` excl = go next acc
      | otherwise           = go next (acc + obsLogSum d ys)
    go (Free (ObserveLM nm bs xs re fam ys next)) acc
      | nm `Set.member` excl = go next acc
      | otherwise            = go next (acc + lmObsLogSum bs xs re fam ys params)
    -- Phase 90 A10: vecIR ('VGPot') に吸収済みの potential は二重加算しない。
    go (Free (Potential n v next)) acc
      | n `Set.member` excl = go next acc
      | otherwise           = go next (acc + v)
    go (Free (Deterministic _ v k)) acc = go (k v) acc
    go (Free (Data _ ys k)) acc = go (k (map realToFrac ys, ys)) acc
    go (Free (DataIx _ is k)) acc = go (k is) acc
    go (Free (PlateBegin _ _ next)) acc = go next acc
    go (Free (PlateEnd next))       acc = go next acc

-- | Phase 98 A2: excl 吸収後の残余 log-density。 'compileResidual' が成功すれば
-- flat 畳み込み ('residualValueA'・Free walk 無し)、 失敗すれば従来の
-- 'logJointExclBlocks' walk に fallback する。 呼び出し側は @mcr@ を 1 度だけ
-- ('compileResidual' で) 構築して値/勾配の両クロージャに渡す ('CompiledResidual'
-- は 'SExp' 保持の純データなので型非依存で共有できる)。
residualExcl :: (Floating a, Ord a)
             => Maybe CompiledResidual -> Set Text -> Model a r -> Map Text a -> a
residualExcl (Just cr) _    _ params = residualValueA cr params
residualExcl Nothing   excl m params = logJointExclBlocks excl m params

-- | Phase 54.4b: Gaussian-恒等リンク 'ObserveLM' ブロックの **静的部分**を 1 度
-- だけ前処理した中間表現。 NUTS の draw ループの外で構築し全 leapfrog で再利用する
-- ことで、 設計列のベクトル化 (@row !! k@ = O(n·p²)) や群 id の unbox 変換・ys の
-- Storable 化といった「値に依らず draw 間で不変な仕事」 を毎勾配評価から外す。
data CompiledLMBlock = CompiledLMBlock
  { clbBetas :: ![Text]                          -- ^ β パラメタ名 (列順)
  , clbCols  :: ![VS.Vector Double]              -- ^ 設計列 (p 本・各 length n)
  , clbReff  :: ![([Text], Int, VU.Vector Int, Maybe (VS.Vector Double))]
    -- ^ (u 名, nG, gids, per-row 重み) のランダム効果 (重み Nothing = 全 1)
  , clbSname :: !Text                            -- ^ σ パラメタ名
  , clbYs    :: !(VS.Vector Double)              -- ^ 観測 (length n)
  , clbN     :: !Int
  , clbP     :: !Int
  }

-- | 'gaussLMBlocks' の 1 ブロックを 'CompiledLMBlock' に前処理する (静的・1 回)。
compileLMBlock :: ([Text], [[Double]], [REff], Text, [Double]) -> CompiledLMBlock
compileLMBlock (betaNames, designX, reffs, sName, ys) =
  let p    = length betaNames
      n    = length ys
      cols = [ VS.fromList [ row !! k | row <- designX ] | k <- [0 .. p - 1] ]
      reff = [ (uNames, length uNames, VU.fromList gids, fmap VS.fromList mw)
             | REff uNames gids _ mw _ <- reffs ]
  in CompiledLMBlock betaNames cols reff sName (VS.fromList ys) n p

-- | Phase 54.6: 'CompiledLMBlock' の名前参照を param index に解決した形。
-- compile 時に 1 度だけ作り、 per-call は Storable vector への index 参照のみ
-- (Text-key Map lookup なし)。
data CompiledLMBlockIx = CompiledLMBlockIx
  { cliBetaIx :: !(VU.Vector Int)                       -- ^ β の param index (列順)
  , cliXMat   :: !(VS.Vector Double)                    -- ^ 設計行列 row-major (n×p・X[i*p+k])
  , cliCols   :: !(BV.Vector (VS.Vector Double))        -- ^ 設計列 (∂β dot 用・O(1) 添字)
  , cliReff   :: ![(VU.Vector Int, Int, VU.Vector Int, Maybe (VS.Vector Double))]
    -- ^ (u indices, nG, gids, per-row 重み)。 重み Nothing = 全 1 (Phase 54.10)
  , cliSIx    :: !Int                                   -- ^ σ の param index
  , cliYs     :: !(VS.Vector Double)                    -- ^ 観測 (length n)
  , cliN      :: !Int
  , cliP      :: !Int
  }

-- | 'CompiledLMBlock' の名前を index に解決する (静的・1 回)。 Phase 54.7a で
-- row-major 設計行列も前計算 (残差ループのキャッシュ局所性 + リスト走査排除)。
resolveLMBlock :: Map Text Int -> CompiledLMBlock -> CompiledLMBlockIx
resolveLMBlock ixOf clb =
  let n = clbN clb
      p = clbP clb
      cols = clbCols clb
  in CompiledLMBlockIx
    { cliBetaIx = VU.fromList [ ixOf Map.! nm | nm <- clbBetas clb ]
    , cliXMat   = VS.generate (n * p) $ \ix ->
                    let (i, k) = ix `divMod` p
                    in (cols !! k) `VS.unsafeIndex` i
    , cliCols   = BV.fromList cols
    , cliReff   = [ (VU.fromList [ ixOf Map.! nm | nm <- uNames ], nG, gids, mw)
                  | (uNames, nG, gids, mw) <- clbReff clb ]
    , cliSIx    = ixOf Map.! clbSname clb
    , cliYs     = clbYs clb
    , cliN      = n
    , cliP      = p
    }

-- | Phase 93: 階層 Normal 群 (uNames, μ名, τ名) の名前を param index に解決する
-- ('resolveLMBlock' と同様に compile 時 1 回)。
resolveHierNormal :: Map Text Int -> ([Text], Text, Text) -> HierNormalIx
resolveHierNormal ixOf (uNames, meanName, scaleName) = HierNormalIx
  { hniUIx     = VU.fromList [ ixOf Map.! nm | nm <- uNames ]
  , hniMeanIx  = ixOf Map.! meanName
  , hniScaleIx = ixOf Map.! scaleName
  }

-- | 残差 @r_i = y_i - Σ_k β_k X_ik - Σ_re u^{re}[gid_i]@ と @Σr²@ を
-- **1 パスの手動ループ** で計算する (Phase 54.7a: (a)-0 実測で per-call
-- ~48-82KB の割当が本物と確定 — `VS.generate` 内のリスト fold・`zip`/`toList`
-- の毎回再構築・dot/sumR2 の中間ベクトルが原因。 unboxed アキュムレータの
-- 明示ループ + row-major X で割当を r 1 本に削減)。
lmResidualS :: CompiledLMBlockIx -> VS.Vector Double -> (VS.Vector Double, Double)
lmResidualS blk pc = runST $ do
  let n   = cliN blk
      p   = cliP blk
      xm  = cliXMat blk
      ys  = cliYs blk
      res = cliReff blk
      bv  = VS.generate p (\k -> pc `VS.unsafeIndex` (cliBetaIx blk `VU.unsafeIndex` k))
  mr <- VSM.unsafeNew n
  let goObs !i !acc
        | i >= n    = pure acc
        | otherwise = do
            let base = i * p
                goK !k !s
                  | k >= p    = s
                  | otherwise = goK (k + 1)
                      (s + bv `VS.unsafeIndex` k * (xm `VS.unsafeIndex` (base + k)))
                reS = foldl' (\ !a (uix, _, gids, mw) ->
                                let u = pc `VS.unsafeIndex` (uix `VU.unsafeIndex`
                                          (gids `VU.unsafeIndex` i))
                                in a + case mw of
                                         Nothing -> u
                                         Just w  -> w `VS.unsafeIndex` i * u) 0 res
                ri  = ys `VS.unsafeIndex` i - goK 0 0 - reS
            VSM.unsafeWrite mr i ri
            goObs (i + 1) (acc + ri * ri)
  sumR2 <- goObs 0 0
  r <- VS.unsafeFreeze mr
  pure (r, sumR2)

-- | 前処理済みブロックの観測尤度 @Σ_i logDensityObs(Normal η_i σ) y_i@ の
-- **constrained 空間**での勾配を解析閉形式で mutable 勾配ベクトルに加算する
-- (Phase 54.6: Gaussian-恒等リンクは閉形式が書けるので汎用 tape 不要):
--
-- > ∂/∂β_k = X_kᵀ r / σ²
-- > ∂/∂u_j = (Σ_{i: gid_i=j} w_i·r_i) / σ²   (scatter・O(n)・重み無しは w_i=1)
-- > ∂/∂σ   = -n/σ + (Σ r²)/σ³
--
-- Phase 54.7a: dot / scatter とも unboxed アキュムレータの明示ループ
-- (中間ベクトル・`VU.convert`・`accumulate` 割当なし)。
gradLMBlockIx :: CompiledLMBlockIx -> VS.Vector Double
              -> VSM.MVector s Double -> ST s ()
gradLMBlockIx blk pc mg = do
  let sigma = pc `VS.unsafeIndex` cliSIx blk
      s2    = sigma * sigma
      n     = cliN blk
      (r, sumR2) = lmResidualS blk pc
      n'    = fromIntegral n
  forM_ [0 .. cliP blk - 1] $ \k -> do
    let c = cliCols blk `BV.unsafeIndex` k
        dot !i !acc
          | i >= n    = acc
          | otherwise = dot (i + 1)
              (acc + c `VS.unsafeIndex` i * r `VS.unsafeIndex` i)
    VSM.modify mg (+ (dot 0 0 / s2)) (cliBetaIx blk `VU.unsafeIndex` k)
  forM_ (cliReff blk) $ \(uix, nG, gids, mw) -> do
    macc <- VSM.replicate nG 0
    let scat !i
          | i >= n    = pure ()
          | otherwise = do
              let g  = gids `VU.unsafeIndex` i
                  ri = r `VS.unsafeIndex` i
                  wr = case mw of
                         Nothing -> ri
                         Just w  -> w `VS.unsafeIndex` i * ri
              v <- VSM.unsafeRead macc g
              VSM.unsafeWrite macc g (v + wr)
              scat (i + 1)
    scat 0
    forM_ [0 .. nG - 1] $ \j -> do
      gj <- VSM.unsafeRead macc j
      VSM.modify mg (+ (gj / s2)) (uix `VU.unsafeIndex` j)
  VSM.modify mg (+ (negate n' / sigma + sumR2 / (s2 * sigma))) (cliSIx blk)

-- | 前処理済みブロックの観測尤度の **値**
-- @-n/2·log2π - n·logσ - Σr²/(2σ²)@。 Phase 54.7a: r を materialize せず
-- sumR2 だけを 1 パスの明示ループで累積 (割当ゼロ)。
-- guard (σ≤0 → -∞) は 'logDensityObs' の Normal 分岐と一致させる。
valueLMBlockIx :: CompiledLMBlockIx -> VS.Vector Double -> Double
valueLMBlockIx blk pc
  | sigma <= 0 = negInf
  | otherwise  =
      negate (0.5 * n' * log (2 * pi)) - n' * log sigma
        - sumR2 / (2 * sigma * sigma)
  where
    sigma = pc `VS.unsafeIndex` cliSIx blk
    n     = cliN blk
    p     = cliP blk
    n'    = fromIntegral n
    xm    = cliXMat blk
    ys    = cliYs blk
    res   = cliReff blk
    bv    = VS.generate p (\k -> pc `VS.unsafeIndex` (cliBetaIx blk `VU.unsafeIndex` k))
    sumR2 = goObs 0 0
    goObs !i !acc
      | i >= n    = acc
      | otherwise =
          let base = i * p
              goK !k !s
                | k >= p    = s
                | otherwise = goK (k + 1)
                    (s + bv `VS.unsafeIndex` k * (xm `VS.unsafeIndex` (base + k)))
              reS = foldl' (\ !a (uix, _, gids, mw) ->
                              let u = pc `VS.unsafeIndex` (uix `VU.unsafeIndex`
                                        (gids `VU.unsafeIndex` i))
                              in a + case mw of
                                       Nothing -> u
                                       Just w  -> w `VS.unsafeIndex` i * u) 0 res
              ri  = ys `VS.unsafeIndex` i - goK 0 0 - reS
          in goObs (i + 1) (acc + ri * ri)

-- | Phase 54.4c/54.6: 群効果 prior @u_j ~ Normal(0, τ)@ の index 解決形。
data ReffPriorIx = ReffPriorIx
  { rpiUIx     :: !(VU.Vector Int)   -- ^ u_j の param index (長さ nG)
  , rpiScaleIx :: !Int               -- ^ τ の param index
  }

-- | 群効果 prior の **constrained 空間**での解析勾配を mutable 勾配ベクトルに
-- 加算する (`ad` のスカラ tape を回避):
--
-- > log p(u | τ) = -nG/2·log(2π) - nG·log τ - (Σ u_j²)/(2τ²)
-- > ∂/∂u_j = -u_j / τ²
-- > ∂/∂τ   = -nG/τ + (Σ u_j²)/τ³
--
-- τ 成分は τ 自身の prior (解析 or `ad` 経路) と加算合流する。 unconstrained への
-- chain rule ('dInvTransform') は呼出側で適用する。
gradReffPriorIx :: ReffPriorIx -> VS.Vector Double -> VSM.MVector s Double -> ST s ()
gradReffPriorIx (ReffPriorIx uix six) pc mg = do
  let tau  = pc `VS.unsafeIndex` six
      tau2 = tau * tau
      nG   = VU.length uix
  sumU2 <- VU.foldM' (\ !acc i -> do
                        let u = pc `VS.unsafeIndex` i
                        VSM.modify mg (+ (negate u / tau2)) i
                        pure (acc + u * u)) 0 uix
  VSM.modify mg (+ (negate (fromIntegral nG) / tau + sumU2 / (tau2 * tau))) six

-- | 群効果 prior の log-density 和の **値** ('gradReffPriorIx' の値版)。
-- guard (τ≤0 → -∞) は 'logDensity' の Normal 分岐と一致させる。
valueReffPriorIx :: ReffPriorIx -> VS.Vector Double -> Double
valueReffPriorIx (ReffPriorIx uix six) pc
  | tau <= 0  = negInf
  | otherwise =
      negate (0.5 * nG' * log (2 * pi)) - nG' * log tau
        - sumU2 / (2 * tau * tau)
  where
    tau   = pc `VS.unsafeIndex` six
    nG'   = fromIntegral (VU.length uix)
    sumU2 = VU.foldl' (\ !acc i -> let u = pc `VS.unsafeIndex` i
                                   in acc + u * u) 0 uix

-- | Phase 93: **非ゼロ latent 平均**の階層 Normal prior の解析勾配経路。
-- 'ReffPriorIx' (mean-0 専用) の一般化で、 平均 μ・スケール τ とも latent の
-- @u_i ~ Normal(μ, τ)@ 群を扱う (rats の @alpha[i]~Normal(muAlpha,sigmaAlpha)@ 等)。
data HierNormalIx = HierNormalIx
  { hniUIx     :: !(VU.Vector Int)   -- ^ u_i の param index (長さ nG)
  , hniMeanIx  :: !Int               -- ^ μ の param index
  , hniScaleIx :: !Int               -- ^ τ の param index
  }

-- | 'HierNormalIx' の **constrained 空間**での解析勾配を mutable 勾配ベクトルに
-- 加算する (`ad` のスカラ tape を回避):
--
-- > log p(u | μ, τ) = -nG/2·log(2π) - nG·log τ - (Σ (u_i-μ)²)/(2τ²)
-- > ∂/∂u_i = -(u_i - μ) / τ²
-- > ∂/∂μ   =  (Σ (u_i - μ)) / τ²
-- > ∂/∂τ   = -nG/τ + (Σ (u_i-μ)²)/τ³
--
-- μ・τ 成分は各自の prior (解析 or `ad` 経路) と加算合流する。 unconstrained への
-- chain rule ('dInvTransform') は呼出側で適用する。
gradHierNormalIx :: HierNormalIx -> VS.Vector Double -> VSM.MVector s Double -> ST s ()
gradHierNormalIx (HierNormalIx uix mIx sIx) pc mg = do
  let mu   = pc `VS.unsafeIndex` mIx
      tau  = pc `VS.unsafeIndex` sIx
      tau2 = tau * tau
      nG   = VU.length uix
  (sumD, sumD2) <-
    VU.foldM' (\ (!accD, !accD2) i -> do
                 let u = pc `VS.unsafeIndex` i
                     d = u - mu
                 VSM.modify mg (+ (negate d / tau2)) i
                 pure (accD + d, accD2 + d * d)) (0, 0) uix
  VSM.modify mg (+ (sumD / tau2)) mIx
  VSM.modify mg (+ (negate (fromIntegral nG) / tau + sumD2 / (tau2 * tau))) sIx

-- | 'HierNormalIx' の log-density 和の **値** ('gradHierNormalIx' の値版)。
-- guard (τ≤0 → -∞) は 'logDensity' の Normal 分岐と一致させる。
valueHierNormalIx :: HierNormalIx -> VS.Vector Double -> Double
valueHierNormalIx (HierNormalIx uix mIx sIx) pc
  | tau <= 0  = negInf
  | otherwise =
      negate (0.5 * nG' * log (2 * pi)) - nG' * log tau
        - sumD2 / (2 * tau * tau)
  where
    mu    = pc `VS.unsafeIndex` mIx
    tau   = pc `VS.unsafeIndex` sIx
    nG'   = fromIntegral (VU.length uix)
    sumD2 = VU.foldl' (\ !acc i -> let d = pc `VS.unsafeIndex` i - mu
                                   in acc + d * d) 0 uix

-- ---------------------------------------------------------------------------
-- Phase 98 A3: LogNormal 群 prior の解析勾配 ('HierNormalIx' の LogNormal 版)
-- ---------------------------------------------------------------------------
-- @a_i ~ LogNormal(μ, σ)@ 群 (μ = 定数 or 単一 latent・σ = 単一 latent) の値/勾配を
-- 解析式で扱い、 vecIR 経路の残余 reverse-AD tape (irt-2pl で ~30%time/~85%alloc) を消す。

-- | 'collectLogNormalGroups' の結果を param index へ解決した中間表現。
-- μ が定数なら @hlnMeanIx = Left c@、 latent なら @Right ix@。
data LogNormalIx = LogNormalIx
  { hlnUIx     :: !(VU.Vector Int)     -- ^ a_i の param index (長さ nG)
  , hlnMeanIx  :: !(Either Double Int) -- ^ μ (定数 or param index)
  , hlnScaleIx :: !Int                 -- ^ σ の param index
  }

resolveLogNormal :: Map Text Int -> ([Text], Either Double Text, Text) -> LogNormalIx
resolveLogNormal ixOf (uNames, mean, scaleName) = LogNormalIx
  { hlnUIx     = VU.fromList [ ixOf Map.! nm | nm <- uNames ]
  , hlnMeanIx  = either Left (Right . (ixOf Map.!)) mean
  , hlnScaleIx = ixOf Map.! scaleName
  }

-- | 'LogNormalIx' の **constrained 空間**での解析勾配を mutable 勾配ベクトルに
-- 加算する (`ad` のスカラ tape を回避)。 L_i = log a_i, d_i = L_i - μ として:
--
-- > log p(a | μ, σ) = -nG/2·log(2π) - nG·log σ - Σ L_i - (Σ d_i²)/(2σ²)
-- > ∂/∂a_i = -(1 + d_i/σ²) / a_i
-- > ∂/∂μ   =  (Σ d_i) / σ²          (μ が latent のときのみ)
-- > ∂/∂σ   = -nG/σ + (Σ d_i²)/σ³
--
-- unconstrained への chain rule ('dInvTransform') は呼出側で適用する。
gradLogNormalIx :: LogNormalIx -> VS.Vector Double -> VSM.MVector s Double -> ST s ()
gradLogNormalIx (LogNormalIx uix meanIx sIx) pc mg = do
  let mu   = either id (pc `VS.unsafeIndex`) meanIx
      sig  = pc `VS.unsafeIndex` sIx
      sig2 = sig * sig
      nG   = VU.length uix
  (sumD, sumD2) <-
    VU.foldM' (\ (!accD, !accD2) i -> do
                 let a = pc `VS.unsafeIndex` i
                     d = log a - mu
                 VSM.modify mg (+ (negate (1 + d / sig2) / a)) i
                 pure (accD + d, accD2 + d * d)) (0, 0) uix
  case meanIx of
    Right mIx -> VSM.modify mg (+ (sumD / sig2)) mIx
    Left _    -> pure ()
  VSM.modify mg (+ (negate (fromIntegral nG) / sig + sumD2 / (sig2 * sig))) sIx

-- | 'LogNormalIx' の log-density 和の **値** ('gradLogNormalIx' の値版)。
-- guard (σ≤0 / a_i≤0 → -∞) は 'logDensity' の LogNormal 分岐と一致させる
-- (a は PositiveT 変換で a>0 だが安全のため一致させる)。
valueLogNormalIx :: LogNormalIx -> VS.Vector Double -> Double
valueLogNormalIx (LogNormalIx uix meanIx sIx) pc
  | sig <= 0                      = negInf
  | VU.any (\i -> pc `VS.unsafeIndex` i <= 0) uix = negInf
  | otherwise =
      negate (0.5 * nG' * log (2 * pi)) - nG' * log sig - sumL
        - sumD2 / (2 * sig * sig)
  where
    mu    = either id (pc `VS.unsafeIndex`) meanIx
    sig   = pc `VS.unsafeIndex` sIx
    nG'   = fromIntegral (VU.length uix)
    (sumL, sumD2) =
      VU.foldl' (\ (!aL, !aD2) i ->
                   let l = log (pc `VS.unsafeIndex` i)
                       d = l - mu
                   in (aL + l, aD2 + d * d)) (0, 0) uix

-- ---------------------------------------------------------------------------
-- 制約変換 (Floating 多相版)
-- ---------------------------------------------------------------------------

-- | unconstrained → constrained 変換 (Floating 多相)。
--
-- > UnconstrainedT: θ = u
-- > PositiveT:      θ = exp(u)
-- > UnitIntervalT:  θ = sigmoid(u) = 1/(1+exp(-u))
invTransformF :: Floating a => Transform -> a -> a
invTransformF UnconstrainedT u = u
invTransformF PositiveT      u = exp u
invTransformF UnitIntervalT  u = 1 / (1 + exp (-u))

-- | log |∂θ/∂u| — Jacobian 行列式の対数 (Floating 多相)。
logJacF :: Floating a => Transform -> a -> a
logJacF UnconstrainedT _ = 0
logJacF PositiveT      u = u                       -- log(exp u) = u
logJacF UnitIntervalT  u =
  let p = 1 / (1 + exp (-u))
  in log p + log (1 - p)                           -- log σ(u)(1-σ(u))

-- | 各 latent 変数の事前分布から制約変換を自動検出する。 分布名→変換の表は
-- 'nameToTransform' (@HBM.Distribution@) に一元化 (probe 側 'vecIRProbeOK' と
-- 同一 source)。
getTransforms :: ModelP r -> Map Text Transform
getTransforms m = Map.fromList
  [ (nodeName n, nameToTransform (nodeDist n))
  | n <- collectNodes m
  , nodeKind n == LatentN
  ]

-- | unconstrained 空間における log-joint (Jacobian 補正込み)。
-- Jacobian 補正で確率密度の積分を保存する。
logJointUnconstrained :: forall a r. (Floating a, Ord a)
                      => Model a r
                      -> [Text]      -- ^ パラメータ順序
                      -> [Transform] -- ^ 各パラメータの変換種別
                      -> Map Text a  -- ^ unconstrained パラメータ値
                      -> a
logJointUnconstrained m names trans paramsU =
  let paramsC = Map.fromList
        [ (n, invTransformF t (Map.findWithDefault 0 n paramsU))
        | (n, t) <- zip names trans ]
      logJac  = sum
        [ logJacF t (Map.findWithDefault 0 n paramsU)
        | (n, t) <- zip names trans ]
  in logJoint m paramsC + logJac
