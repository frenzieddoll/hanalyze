{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Hanalyze.Model.HBM.IR
-- Description : HBM の中間表現 (IR) 層 (affine 追跡・SExp/UExp コンパイル)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: IR (中間表現) 層を 'Hanalyze.Model.HBM' から分離。
--
-- AD 勾配の高速経路で使う __中間表現__ (記述層 Model / 評価層 Eval の上層):
--
--   - affine 追跡 ('AffV') による per-obs 手書き Gaussian モデルの自動
--     ObserveLM 化 ('synthGaussLMBlocks')
--   - 非線形 μ の「スカラ式 IR」 ('SExp') → 「ベクトル式 IR」 ('UExp') 合成
--     ('synthVecIR' / 'compileVecIR')
--   - 観測密度の IR 式化 ('VecObsIR' → 'CompiledVecIR') と arena 上の値/勾配
--     評価 ('vecIRValue' / 'gradVecIR')
--
-- ★__最ホット__: NUTS per-draw の勾配本経路 ('gradVecIR')。 monolith では AD 勾配
-- コンパイラ (@compileGradUV@ 本体残置) と同一モジュールで inline されていた。
-- 境界跨ぎ inline 喪失を防ぐため定義と一緒に INLINABLE/SPECIALIZE を移送。 依存は
-- 下層 Model / Distribution / Eval (lmObsLogSum) / Util のみ (一方向)。
--
-- export list は省略 (内部実装層)。 公開 surface (synthGaussLMBlocks / synthVecIR)
-- は facade 'Hanalyze.Model.HBM' の export list が制御する。
--
-- [English]: The IR (intermediate representation) layer, separated out from
-- 'Hanalyze.Model.HBM'.
--
-- The __intermediate representation__ used on the fast path for AD
-- gradients (a layer above the description layer Model / evaluation layer
-- Eval):
--
--   - Automatic conversion of hand-written per-observation Gaussian models
--     into ObserveLM form via affine tracking ('AffV')
--     ('synthGaussLMBlocks')
--   - Composing a "scalar-expression IR" ('SExp') for nonlinear μ into a
--     "vector-expression IR" ('UExp') ('synthVecIR' \/ 'compileVecIR')
--   - Turning observation densities into IR expressions ('VecObsIR' ->
--     'CompiledVecIR') and evaluating values\/gradients over the arena
--     ('vecIRValue' \/ 'gradVecIR')
--
-- ★__The single hottest path__: the main gradient path for NUTS per-draw
-- ('gradVecIR'). In the monolith, this was inlined into the same module as
-- the AD gradient compiler (@compileGradUV@, whose body remains there). The
-- INLINABLE\/SPECIALIZE pragmas were moved together with the definitions to
-- avoid losing cross-module inlining. Dependencies flow one way only, on
-- the lower layers Model \/ Distribution \/ Eval (lmObsLogSum) \/ Util.
--
-- The export list is omitted (this is an internal implementation layer).
-- The public surface (synthGaussLMBlocks \/ synthVecIR) is controlled by
-- the facade 'Hanalyze.Model.HBM' export list.
module Hanalyze.Model.HBM.IR where

import Control.DeepSeq (NFData (..), force)
import Control.Exception (SomeAsyncException (..), SomeException, evaluate,
                          fromException, throwIO, try)
import Control.Monad (forM, forM_, replicateM, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (foldl')
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, hashStableName, makeStableName)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric.AD.Mode.Reverse.Double (grad)
import Control.Monad.Primitive (PrimMonad, PrimState)

import Control.Monad.ST (ST, runST)
import qualified Data.Vector          as BV
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed  as VU

import Hanalyze.Stat.Distribution (Transform (..), fromUnconstrained)
import Hanalyze.MCMC.Core (Chain (..))

import Hanalyze.Model.HBM.Util
import Hanalyze.Model.HBM.Distribution
import Hanalyze.Model.HBM.Sampling
import Hanalyze.Model.HBM.Model
import Hanalyze.Model.HBM.Track
import Hanalyze.Model.HBM.Eval

-- ---------------------------------------------------------------------------
-- Phase 54.8: per-obs 手書きモデルの自動 ObserveLM 化 (M1 救済)
-- ---------------------------------------------------------------------------

-- | [日本語]: affine 追跡値。 latent 値の場所に流して、 式が
--   @Σ coeff_i · latent_i + offset@ (係数は定数) の形に留まるかを追跡する。
--   非線形演算 (latent 同士の積・exp 等) が掛かった時点で @NA@ に落ちる。
--   [English]: An affine-tracking value. Fed in place of a latent value, it
--   tracks whether the expression stays in the form @Σ coeff_i · latent_i +
--   offset@ (with constant coefficients). Once a nonlinear operation hits it
--   (product of latents, exp, etc.), it collapses to @NA@.
data AffV
  = AffC !Double               -- ^ [日本語]: 定数。 [English]: A constant.
  | AffL !(Map Text Double) !Double  -- ^ [日本語]: Σ coeff·latent + offset (affine)。 [English]: Σ coeff·latent + offset (affine).
  | NA                         -- ^ [日本語]: 非 affine (追跡断念)。 [English]: Non-affine (tracking gave up).

-- Phase 60.7: '!!!' の依存タグは IR 抽出には無関係 (既定 id)。
instance TrackTag AffV

-- | [日本語]: 非定数値の比較 = 値依存分岐。 構造抽出は分岐の片側しか見られないため
--   誤抽出になる → error poison で walk 全体を失敗させ、 呼出側の
--   @try/evaluate/force@ で捕捉して fallback する (安全網①)。
--   [English]: Comparing non-constant values means a value-dependent
--   branch. Structural extraction can only see one side of a branch, which
--   would cause a mis-extraction, so we error-poison the whole walk, and
--   the caller catches it with @try/evaluate/force@ and falls back (safety
--   net (1)).
affPoison :: a
affPoison = error "AffV: non-constant comparison (value-dependent branch)"

instance Eq AffV where
  AffC a == AffC b = a == b
  _      == _      = affPoison

instance Ord AffV where
  compare (AffC a) (AffC b) = compare a b
  compare _        _        = affPoison

instance Num AffV where
  AffC a   + AffC b   = AffC (a + b)
  AffC a   + AffL m c = AffL m (a + c)
  AffL m c + AffC a   = AffL m (c + a)
  AffL m1 c1 + AffL m2 c2 = AffL (Map.unionWith (+) m1 m2) (c1 + c2)
  _ + _ = NA
  AffC a   * AffC b   = AffC (a * b)
  AffC a   * AffL m c = scaleAffV a m c
  AffL m c * AffC a   = scaleAffV a m c
  _ * _ = NA
  negate (AffC a)   = AffC (negate a)
  negate (AffL m c) = AffL (Map.map negate m) (negate c)
  negate NA         = NA
  abs (AffC a) = AffC (abs a)
  abs _        = NA
  signum (AffC a) = AffC (signum a)
  signum _        = NA
  fromInteger = AffC . fromInteger

-- | [日本語]: 定数倍。 0 倍は affine 情報ごと消えて定数 0 (係数 0 の死に列を作らない)。
--   [English]: Scale by a constant. Multiplying by 0 wipes out the affine
--   info entirely, giving the constant 0 (so we never build a dead column
--   of zero coefficients).
scaleAffV :: Double -> Map Text Double -> Double -> AffV
scaleAffV a m c
  | a == 0    = AffC 0
  | otherwise = AffL (Map.map (a *) m) (a * c)

instance Fractional AffV where
  AffC a   / AffC b = AffC (a / b)
  AffL m c / AffC b = scaleAffV (recip b) m c
  _        / _      = NA
  recip (AffC a) = AffC (recip a)
  recip _        = NA
  fromRational = AffC . fromRational

instance Floating AffV where
  pi = AffC pi
  exp   = affLift1 exp
  log   = affLift1 log
  sqrt  = affLift1 sqrt
  sin   = affLift1 sin
  cos   = affLift1 cos
  tan   = affLift1 tan
  asin  = affLift1 asin
  acos  = affLift1 acos
  atan  = affLift1 atan
  sinh  = affLift1 sinh
  cosh  = affLift1 cosh
  tanh  = affLift1 tanh
  asinh = affLift1 asinh
  acosh = affLift1 acosh
  atanh = affLift1 atanh

-- | [日本語]: 超越関数: 定数には適用、 latent が絡んだら非 affine。
--   [English]: A transcendental function: applied to constants directly,
--   but treated as non-affine once a latent is involved.
affLift1 :: (Double -> Double) -> AffV -> AffV
affLift1 f (AffC a) = AffC (f a)
affLift1 _ _        = NA

-- | [日本語]: per-obs 手書き scalar 'Observe' 群から Gaussian LM ブロックを
--   __自動合成__する。 返り値は (合成ブロック群, 吸収した Observe ノード名集合)。
--   検出できない / 安全網に掛かった場合は @([], ∅)@ (従来経路に fallback)。
--
--   仕組み: 'Sample' の継続に @AffL {name:1} 0@ を給餌して model を walk し、
--   @Observe nm (Normal μ σ) ys@ の μ が affine・σ が単一 latent (係数 1・offset 0)
--   の行を収集する。 定数 offset は ys 側に畳む (Normal は y−μ のみに依存)。
--   σ 名ごとに 1 ブロックへまとめ、 prior が @Normal(0, τ)@ (τ 単一 latent) を
--   共有し __各行にちょうど 1 つ__現れる latent 族を 'REff' gather に昇格する
--   (dense one-hot は O(nG·n) で階層に逆効果)。 係数は任意で、
--   per-row 重みとして 'REff' に載せる (random slope @v_g·x_i@ も
--   gather 化。 全 1 なら重みスロットは @Nothing@ = 従来の random intercept)。
--   族抽出に失敗した latent は dense β 列のまま (正しいが遅い・安全方向)。
--
--   安全網 2 段: ① 'AffV' の Eq/Ord は非定数比較で error poison →
--   'unsafePerformIO' + 'try' + 'force' で捕捉し全体 fallback (値依存分岐モデルの
--   誤抽出防止・Nonlinear 系の前例に同じ)。 ② 合成ブロックの観測尤度を probe
--   2 点で walk 評価 ('obsOnlySum') と突合し、 不一致なら fallback。
--   [English]: __Automatically synthesizes__ Gaussian LM blocks from a group
--   of hand-written per-observation scalar 'Observe' nodes. Returns
--   (synthesized blocks, the set of absorbed Observe node names). If
--   nothing can be detected, or a safety net trips, returns @([], ∅)@ (fall
--   back to the previous path).
--
--   How it works: walks the model, feeding @AffL {name:1} 0@ into the
--   continuation of each 'Sample', and collects the rows where
--   @Observe nm (Normal μ σ) ys@ has an affine μ and a σ that is a single
--   latent (coefficient 1, offset 0). Constant offsets are folded into
--   @ys@ (Normal depends only on y−μ). Rows are grouped into one block per
--   σ name; when the prior is @Normal(0, τ)@ (a single latent τ) shared
--   across the group, and a latent family appears __exactly once per row__,
--   that family is promoted to an 'REff' gather (dense one-hot would be
--   O(nG·n), which hurts hierarchical models). Coefficients are arbitrary
--   and are carried in 'REff' as per-row weights (random slopes
--   @v_g·x_i@ are also gathered this way; if all weights are 1 the weight
--   slot is @Nothing@, i.e. an ordinary random intercept). Any latent whose
--   family extraction fails stays as a dense β column (correct but slower
--   — the safe direction).
--
--   Two safety nets: (1) 'AffV'\'s Eq\/Ord poisons on non-constant
--   comparisons -> caught via 'unsafePerformIO' + 'try' + 'force' and the
--   whole thing falls back (this prevents mis-extraction on
--   value-dependent-branch models, following the precedent set for the
--   nonlinear family). (2) The synthesized blocks' observation likelihood
--   is cross-checked at two probe points against the walked evaluation
--   ('obsOnlySum'); a mismatch triggers fallback.
synthGaussLMBlocks
  :: ModelP r
  -> ([(Text, [Text], [[Double]], [REff], Text, [Double])], Set Text)
synthGaussLMBlocks m = unsafePerformIO $ do
  r <- try (evaluate (force (synthGaussLMWalk m)))
  pure $ case r :: Either SomeException
                    ([(Text, [Text], [[Double]], [REff], Text, [Double])], Set Text) of
    Left _  -> ([], Set.empty)
    Right v@(blocks, obsNames)
      | null blocks          -> ([], Set.empty)
      | synthProbeOK m blocks obsNames -> v
      | otherwise            -> ([], Set.empty)
{-# NOINLINE synthGaussLMBlocks #-}

-- | [日本語]: 'synthGaussLMBlocks' の純粋部 (walk + 族抽出)。 poison は遅延に潜むので
--   呼出側が force してから使う。
--   [English]: The pure part of 'synthGaussLMBlocks' (walking + family
--   extraction). The poison hides in laziness, so the caller must force
--   before use.
synthGaussLMWalk
  :: ModelP r
  -> ([(Text, [Text], [[Double]], [REff], Text, [Double])], Set Text)
synthGaussLMWalk m =
  let (rows, priors) = collectAffRows m
      sigmas = ordNubT [ sn | (_, _, _, sn, _) <- rows ]
      blocks = [ synthBlock priors sn [ r | r@(_, _, _, sn', _) <- rows, sn' == sn ]
               | sn <- sigmas ]
      obsNames = Set.fromList [ nm | (nm, _, _, _, _) <- rows ]
  in (blocks, obsNames)
  where
    ordNubT = go Set.empty
      where go _ [] = []
            go seen (x:xs)
              | x `Set.member` seen = go seen xs
              | otherwise           = x : go (Set.insert x seen) xs

-- | [日本語]: model を 'AffV' で walk し、 合成可能な行 (Observe 名, μ 係数, μ offset,
--   σ 名, 観測値) と latent prior のスケール検出 (@Normal(0, τ)@ → @Just τ@) を集める。
--   [English]: Walks the model with 'AffV' and collects the rows that can
--   be synthesized (Observe name, μ coefficients, μ offset, σ name,
--   observed value) along with scale detection for the latent prior
--   (@Normal(0, τ)@ -> @Just τ@).
collectAffRows
  :: Model AffV r
  -> ([(Text, Map Text Double, Double, Text, Double)], Map Text (Maybe Text))
collectAffRows = go [] Map.empty
  where
    go rows priors (Pure _) = (reverse rows, priors)
    go rows priors (Free f) = case f of
      Sample n d k ->
        let sc = case d of
                   Normal (AffC 0) (AffL tm 0)
                     | [(tn, 1)] <- Map.toList tm -> Just tn
                   _ -> Nothing
        in go rows (Map.insert n sc priors) (k (AffL (Map.singleton n 1) 0))
      Observe nm (Normal mu sg) ys next
        | AffL sm 0 <- sg, [(sn, 1)] <- Map.toList sm
        , Just (cs, off) <- affParts mu ->
            go ([ (nm, cs, off, sn, y) | y <- ys ] ++ rows) priors next
      Observe _ _ _ next  -> go rows priors next
      ObserveLM _ _ _ _ _ _ next -> go rows priors next
      Potential _ _ next  -> go rows priors next
      Deterministic _ v k -> go rows priors (k v)
      Data _ ys k         -> go rows priors (k (map realToFrac ys, ys))
      DataIx _ is k       -> go rows priors (k is)
      PlateBegin _ _ next -> go rows priors next
      PlateEnd next       -> go rows priors next
    affParts (AffC c)   = Just (Map.empty, c)
    affParts (AffL m c) = Just (m, c)
    affParts NA         = Nothing

-- | [日本語]: 非ゼロ __latent 平均__ の階層 Normal prior を検出する。
--   @u_i ~ Normal(μ, τ)@ で μ・τ __ともに単一 latent__ (係数 1・offset 0) の
--   'Sample' を集め、 (μ, τ) の組ごとに出現順を保って群化する。 返り値の各要素は
--   @(u 名の群, μ 名, τ 名)@。
--
--   平均が定数 (@AffC 0@) の reff は既存の mean-0 解析経路 (@ReffPriorIx@) が
--   扱うのでここでは検出しない (μ が 'AffL' でないと不一致)。 係数≠1・多項・
--   offset≠0 の平均や非 affine な μ/τ も対象外 (残差 ad に残す安全側)。
--   rats の @alpha[i]~Normal(muAlpha,sigmaAlpha)@ / @beta[i]~Normal(muBeta,sigmaBeta)@
--   のような varying-intercept/slope の中心化階層 prior を解析勾配へ載せるための検出器。
--   [English]: Detects hierarchical Normal priors with a __nonzero latent mean__.
--   Collects 'Sample' nodes of the form @u_i ~ Normal(μ, τ)@ where
--   __both μ and τ are a single latent__ (coefficient 1, offset 0), and
--   groups them by the (μ, τ) pair, preserving order of appearance. Each
--   returned element is @(the group of u names, μ name, τ name)@.
--
--   Random effects whose mean is a constant (@AffC 0@) are already handled
--   by the existing mean-0 analytic path (@ReffPriorIx@), so they are not
--   detected here (they wouldn't match anyway, since μ isn't an 'AffL').
--   Means with coefficient ≠ 1, polynomial terms, offset ≠ 0, or non-affine
--   μ\/τ are also excluded (left on the residual AD path, the safe
--   direction). This is the detector that puts centered hierarchical
--   priors for varying-intercept\/slope models — such as rats's
--   @alpha[i]~Normal(muAlpha,sigmaAlpha)@ \/
--   @beta[i]~Normal(muBeta,sigmaBeta)@ — onto the analytic gradient.
collectHierNormalGroups :: Model AffV r -> [([Text], Text, Text)]
collectHierNormalGroups = regroup . go
  where
    go (Pure _) = []
    go (Free f) = case f of
      Sample n d k ->
        let hit = case d of
                    Normal (AffL mm 0) (AffL tm 0)
                      | [(mn, 1)] <- Map.toList mm
                      , [(tn, 1)] <- Map.toList tm -> Just (n, mn, tn)
                    _ -> Nothing
            rest = go (k (AffL (Map.singleton n 1) 0))
        in maybe rest (: rest) hit
      Observe _ _ _ next          -> go next
      ObserveLM _ _ _ _ _ _ next  -> go next
      Potential _ _ next          -> go next
      Deterministic _ v k         -> go (k v)
      Data _ ys k                 -> go (k (map realToFrac ys, ys))
      DataIx _ is k               -> go (k is)
      PlateBegin _ _ next         -> go next
      PlateEnd next               -> go next
    -- (μ,τ) ごとに、u の出現順を保って群化する。
    regroup hits =
      [ ([ u | (u, mn', tn') <- hits, mn' == mn, tn' == tn ], mn, tn)
      | (mn, tn) <- ordNub [ (mn, tn) | (_, mn, tn) <- hits ] ]
    ordNub = goN Set.empty
      where goN _ [] = []
            goN s (x:xs) | x `Set.member` s = goN s xs
                         | otherwise        = x : goN (Set.insert x s) xs

-- | [日本語]: @a_i ~ LogNormal(μ, σ)@ 群を検出する ('collectHierNormalGroups' の
--   LogNormal 版)。μ は定数 (@Left c@・例 irt-2pl の 0) か単一 latent (@Right mn@)、
--   σ は単一 latent (@AffL {sn:1} 0@)。σ 定数は @constPriorsOf@ が拾うのでここでは対象外。
--   返り値 = [(u 名, μ, σ 名)]。vecIR 経路で解析勾配 (@gradLogNormalIx@) に載せ残差 ad から
--   外す (irt-2pl の 20-項 LogNormal prior が reverse-AD tape を張っていたのを解消)。
--   [English]: Detects @a_i ~ LogNormal(μ, σ)@ groups (the LogNormal
--   analogue of 'collectHierNormalGroups'). μ is either a constant
--   (@Left c@; e.g. 0 in irt-2pl) or a single latent (@Right mn@); σ is a
--   single latent (@AffL {sn:1} 0@). Constant σ is picked up by
--   @constPriorsOf@, so it's excluded here. Returns [(u names, μ, σ name)].
--   Putting these onto the analytic gradient (@gradLogNormalIx@) on the
--   vecIR path removes them from the residual AD path (this fixed the
--   20-term LogNormal prior in irt-2pl that was building a reverse-AD
--   tape).
collectLogNormalGroups :: Model AffV r -> [([Text], Either Double Text, Text)]
collectLogNormalGroups = regroup . go
  where
    go (Pure _) = []
    go (Free f) = case f of
      Sample n d k ->
        let hit = case d of
                    LogNormal muA (AffL tm 0)
                      | [(tn, 1)] <- Map.toList tm
                      , Just mean <- affMean muA -> Just (n, mean, tn)
                    _ -> Nothing
            rest = go (k (AffL (Map.singleton n 1) 0))
        in maybe rest (: rest) hit
      Observe _ _ _ next          -> go next
      ObserveLM _ _ _ _ _ _ next  -> go next
      Potential _ _ next          -> go next
      Deterministic _ v k         -> go (k v)
      Data _ ys k                 -> go (k (map realToFrac ys, ys))
      DataIx _ is k               -> go (k is)
      PlateBegin _ _ next         -> go next
      PlateEnd next               -> go next
    -- μ = 定数 (AffC c / offset のみの AffL) か単一 latent (係数 1・offset 0)。
    affMean (AffC c)                        = Just (Left c)
    affMean (AffL mm c)
      | Map.null mm                         = Just (Left c)
      | [(mn, 1)] <- Map.toList mm, c == 0  = Just (Right mn)
    affMean _                               = Nothing
    -- (μ, σ) ごとに u の出現順を保って群化する。
    regroup hits =
      [ ([ u | (u, mean', sn') <- hits, mean' == mean, sn' == sn ], mean, sn)
      | (mean, sn) <- ordNub [ (mean, sn) | (_, mean, sn) <- hits ] ]
    ordNub = goN Set.empty
      where goN _ [] = []
            goN s (x:xs) | x `Set.member` s = goN s xs
                         | otherwise        = x : goN (Set.insert x s) xs

-- | [日本語]: 1 つの σ 名グループから (ブロック名, β 名, X, REff 族, σ 名, ys') を合成する。
--   [English]: Synthesizes (block name, β names, X, REff families, σ name,
--   ys') from a single σ-name group.
synthBlock
  :: Map Text (Maybe Text)
  -> Text
  -> [(Text, Map Text Double, Double, Text, Double)]
  -> (Text, [Text], [[Double]], [REff], Text, [Double])
synthBlock priors sn rows =
  let coeffs   = [ cs | (_, cs, _, _, _) <- rows ]
      latents  = Set.toAscList (Set.unions (map Map.keysSet coeffs))
      -- 族候補 (Phase 54.10 で「係数常 1」 を撤廃): prior = Normal(0, τ) 検出済
      -- なら係数任意。 係数は per-row 重みとして 'REff' に載せる (random slope)。
      isCand l = maybe False (/= Nothing) (Map.lookup l priors)
      -- スケール τ ごとに族を貪欲抽出: 全行にちょうど 1 つ現れる族のみ採用。
      famsByTau = Map.fromListWith (++)
        [ (tn, [l]) | l <- latents, isCand l
        , Just (Just tn) <- [Map.lookup l priors] ]
      accepted = [ (tn, Set.toAscList (Set.fromList ls))
                 | (tn, ls) <- Map.toList famsByTau
                 , let fam = Set.fromList ls
                 , all (\cs -> length (filter (`Map.member` cs) (Set.toList fam)) == 1)
                       coeffs ]
      famSet   = Set.fromList (concatMap snd accepted)
      reffs    = [ let gws = [ gwOf fam cs | cs <- coeffs ]
                       ws  = map snd gws
                       mw  = if all (== 1) ws then Nothing else Just ws
                   in REff fam (map fst gws) (Just tn) mw Nothing
                 | (tn, fam) <- accepted ]
      -- 各行で族中ちょうど 1 つ現れる latent の (族内 index, 係数 = 重み)。
      gwOf fam cs = head [ (j, cs Map.! l) | (j, l) <- zip [0 ..] fam
                         , l `Map.member` cs ]
      betas    = [ l | l <- latents, not (l `Set.member` famSet) ]
      xs       = [ [ Map.findWithDefault 0 b cs | b <- betas ] | cs <- coeffs ]
      ys'      = [ y - off | (_, _, off, _, y) <- rows ]
  in ("__synth_lm_" <> sn, betas, xs, reffs, sn, ys')

-- | [日本語]: 安全網②: 合成ブロックの観測尤度を、 元 model の walk 評価
--   ('obsOnlySum' = 吸収した scalar Observe だけ足す) と probe 2 点で突合する。
--   prior は足さないので guard 起因の ±∞ で比較が壊れない。 probe 値は
--   per-param に変えて係数の取り違えも検出する (全 latent 正値 → σ guard 安全)。
--   [English]: Safety net (2): cross-checks the synthesized blocks'
--   observation likelihood against the original model's walked evaluation
--   ('obsOnlySum', which sums only the absorbed scalar Observes) at two
--   probe points. Since priors aren't added, ±∞ from guards doesn't break
--   the comparison. The probe values vary per parameter so that swapped
--   coefficients are also detected (all latents positive, so σ guards are
--   safe).
synthProbeOK
  :: ModelP r
  -> [(Text, [Text], [[Double]], [REff], Text, [Double])]
  -> Set Text -> Bool
synthProbeOK m blocks obsNames = all check [(0.5, 0.07), (1.3, 0.11)]
  where
    names = sampleNames m
    check (base, step) =
      let pm = Map.fromList [ (n, base + step * fromIntegral i)
                            | (n, i) <- zip names [0 :: Int ..] ]
          ref = obsOnlySum obsNames m pm
          syn = sum [ lmObsLogSum bs xs re (LMGaussian sn) ys pm
                    | (_, bs, xs, re, sn, ys) <- blocks ]
      in abs (ref - syn) <= 1e-9 * (1 + abs ref)

-- | [日本語]: 名前が @sel@ に含まれる scalar 'Observe' の log-likelihood __だけ__を足す
--   walk (probe 用)。
--   [English]: A walk that sums __only__ the log-likelihood of scalar
--   'Observe' nodes whose name is in @sel@ (used for probing).
obsOnlySum :: Set Text -> Model Double r -> Map Text Double -> Double
obsOnlySum sel model params = go model 0
  where
    go (Pure _) acc = acc
    go (Free (Sample n _ k)) acc = go (k (Map.findWithDefault 0 n params)) acc
    go (Free (Observe n d ys next)) acc
      | n `Set.member` sel = go next (acc + obsLogSum d ys)
      | otherwise          = go next acc
    go (Free (ObserveLM _ _ _ _ _ _ next)) acc = go next acc
    go (Free (Potential _ _ next)) acc = go next acc
    go (Free (Deterministic _ v k)) acc = go (k v) acc
    go (Free (Data _ ys k)) acc = go (k (ys, ys)) acc
    go (Free (DataIx _ is k)) acc = go (k is) acc
    go (Free (PlateBegin _ _ next)) acc = go next acc
    go (Free (PlateEnd next)) acc = go next acc

-- ---------------------------------------------------------------------------
-- Phase 54.11: 非線形 μ のベクトル式 IR (M5/M6 救済)
-- ---------------------------------------------------------------------------
--
-- 54.8 の AffV (affine 限定) の代役として、 latent 値の場所に **スカラ式ノード**
-- ('SExp') を給餌して model を walk し、 per-obs scalar @Observe (Normal μ σ)@ の
-- μ 式 (非線形可) を行ごとに収集する。 行間で式の形が同型 (定数 leaf だけが行
-- ごとに違う) なら定数列をベクトル leaf に束ねて「ベクトル式 IR」 ('UExp') へ
-- 持ち上げ (μ⃗ = f(θ, x⃗))、 評価は VecAD の vector-op tape で行う (勾配) /
-- 素な Double ベクトル演算で行う (値)。 階層 prior (a_g ~ Normal(m, τ) の族) の
-- スカラ密度も同 IR に乗せる (M6 要件・54.9)。
--
-- IR 持ち上げ + 静的解析は compile 時 1 回・draw 間で再利用する (54.4b 前例)。
-- VecAD tape 自体は per-call 構築 (spike `bench-hbm-vecir` の実測はこの構築込み)。

-- | [日本語]: スカラ単項演算子 ('SExp' の節)。 導関数は 'sUnD' と対。
--   [English]: A scalar unary operator ('SExp' node). Its derivative is
--   'sUnD'.
data SUn
  = SNegO | SAbsO | SSignumO | SExpO | SLogO | SSqrtO | SRecipO
  | SSinO | SCosO | STanO | SAsinO | SAcosO | SAtanO
  | SSinhO | SCoshO | STanhO | SAsinhO | SAcoshO | SAtanhO
  | SLgammaO   -- ^ [日本語]: log Γ (密度 IR 用。 'Floating' 経由では現れない)。 [English]: log Γ (used by the density IR; never appears via 'Floating').
  deriving (Eq, Ord, Show)

-- | [日本語]: 'SUn' の評価関数を known-function として継続に渡す CPS dispatcher。
--   arena 実行ループが @f = sUnF op@ で closure を束縛してから
--   要素毎に間接呼出すると GHC が unbox できず per-element boxing が出る
--   (irt-2pl prof で sUnF/sBinF/sUnD 計 30.5% time・38.9% alloc)。 call site を
--   INLINE 展開して op の case をループの外に出し、 各分岐を known-function の
--   特殊化 unboxed ループに落とす。 演算内容・FP 順序は不変 (= posterior bit 一致)。
--   [English]: A CPS dispatcher that hands 'SUn'\'s evaluation function to
--   the continuation as a known function. If the arena execution loop
--   binds a closure via @f = sUnF op@ and calls it indirectly per element,
--   GHC can't unbox it, producing per-element boxing (in the irt-2pl
--   profile, sUnF\/sBinF\/sUnD accounted for 30.5% of time and 38.9% of
--   allocation combined). By INLINE-expanding the call site, the case on
--   @op@ is hoisted out of the loop, and each branch falls into a
--   specialized unboxed loop for its known function. The operation content
--   and floating-point order are unchanged (i.e. the posterior is
--   bit-identical).
withSUnF :: SUn -> ((Double -> Double) -> r) -> r
withSUnF o k = case o of
  SNegO    -> k negate
  SAbsO    -> k abs
  SSignumO -> k signum
  SExpO    -> k exp
  SLogO    -> k log
  SSqrtO   -> k sqrt
  SRecipO  -> k recip
  SSinO    -> k sin
  SCosO    -> k cos
  STanO    -> k tan
  SAsinO   -> k asin
  SAcosO   -> k acos
  SAtanO   -> k atan
  SSinhO   -> k sinh
  SCoshO   -> k cosh
  STanhO   -> k tanh
  SAsinhO  -> k asinh
  SAcoshO  -> k acosh
  SAtanhO  -> k atanh
  SLgammaO -> k lgammaApprox
{-# INLINE withSUnF #-}

sUnF :: SUn -> Double -> Double
sUnF o = withSUnF o id

-- | [日本語]: 'sUnF' の導関数の CPS dispatcher ('withSUnF' と同じ意図)。
--   [English]: A CPS dispatcher for 'sUnF'\'s derivative (same intent as
--   'withSUnF').
withSUnD :: SUn -> ((Double -> Double) -> r) -> r
withSUnD o k = case o of
  SNegO    -> k (const (-1))
  SAbsO    -> k signum
  SSignumO -> k (const 0)
  SExpO    -> k exp
  SLogO    -> k recip
  SSqrtO   -> k (\x -> 0.5 / sqrt x)
  SRecipO  -> k (\x -> negate (recip (x * x)))
  SSinO    -> k cos
  SCosO    -> k (negate . sin)
  STanO    -> k (\x -> let t = tan x in 1 + t * t)
  SAsinO   -> k (\x -> 1 / sqrt (1 - x * x))
  SAcosO   -> k (\x -> negate (1 / sqrt (1 - x * x)))
  SAtanO   -> k (\x -> 1 / (1 + x * x))
  SSinhO   -> k cosh
  SCoshO   -> k sinh
  STanhO   -> k (\x -> let t = tanh x in 1 - t * t)
  SAsinhO  -> k (\x -> 1 / sqrt (x * x + 1))
  SAcoshO  -> k (\x -> 1 / sqrt (x * x - 1))
  SAtanhO  -> k (\x -> 1 / (1 - x * x))
  -- digamma でなく項別微分: 評価関数 lgammaApprox の AD 微分 (walk+ad fallback /
  -- 参照勾配) とビット近傍一致させる (digamma だと z=12 境界で ~1.3e-9 ズレ・56.4)
  SLgammaO -> k lgammaApproxDeriv
{-# INLINE withSUnD #-}

-- | [日本語]: 'sUnF' の導関数。
--   [English]: 'sUnF'\'s derivative.
sUnD :: SUn -> Double -> Double
sUnD o = withSUnD o id

-- | [日本語]: スカラ二項演算子 ('SExp' の節)。 'SMaxO' は Mixture/ZeroInflatedBinomial の
--   log-sum-exp を数値安定に組むための elementwise max (勾配は winner-take-all の
--   subgradient・'gradVecIRGo' 参照)。 'SExp' の 'Num' インスタンス経由では
--   構築しない (Num に max が無い) — 'logSumExp2' からのみ直接 'RU2 SMaxO' で
--   使う。
--   [English]: A scalar binary operator ('SExp' node). 'SMaxO' is the
--   elementwise max used to build a numerically stable log-sum-exp for
--   Mixture\/ZeroInflatedBinomial (its gradient is a winner-take-all
--   subgradient; see 'gradVecIRGo'). It is never built via 'SExp'\'s 'Num'
--   instance (Num has no max) — only 'logSumExp2' constructs it directly
--   as 'RU2 SMaxO'.
data SBin = SAddO | SSubO | SMulO | SDivO | SMaxO
  deriving (Eq, Ord, Show)

-- | [日本語]: 二項演算子の CPS dispatcher ('withSUnF' と同じ意図)。
--   [English]: A CPS dispatcher for binary operators (same intent as
--   'withSUnF').
withSBinF :: SBin -> ((Double -> Double -> Double) -> r) -> r
withSBinF o k = case o of
  SAddO -> k (+)
  SSubO -> k (-)
  SMulO -> k (*)
  SDivO -> k (/)
  SMaxO -> k max
{-# INLINE withSBinF #-}

sBinF :: SBin -> Double -> Double -> Double
sBinF o = withSBinF o id

-- | [日本語]: スカラ式 IR。 latent 値の場所に流して式の木を構築する (AffV と違い
--   非線形演算も leaf に潜らず木に残る)。 定数同士は即畳み込む ('sc1'/'sc2') ので
--   データ由来の値は常に 'SC' leaf に正規化され、 行間の形状照合が成立する。
--   [English]: The scalar-expression IR. Fed in place of a latent value, it
--   builds an expression tree (unlike AffV, nonlinear operations don't sink
--   into a leaf but stay in the tree). Constants fold together immediately
--   ('sc1'\/'sc2'), so data-derived values always normalize to an 'SC'
--   leaf, which makes shape matching across rows work.
data SExp
  = SC !Double          -- ^ [日本語]: 定数 (データ・リテラル)。 [English]: A constant (data or literal).
  | SV !Text            -- ^ [日本語]: latent 参照。 [English]: A latent reference.
  | S1 !SUn SExp
  | S2 !SBin SExp SExp

instance NFData SExp where
  rnf (SC x)     = rnf x
  rnf (SV n)     = rnf n
  rnf (S1 o e)   = o `seq` rnf e
  rnf (S2 o a b) = o `seq` rnf a `seq` rnf b

-- Phase 60.7: '!!!' の依存タグは IR 抽出には無関係 (既定 id)。
instance TrackTag SExp

-- | [日本語]: 非定数値の比較 = 値依存分岐 → error poison (AffV と同じ安全網①)。
--   [English]: Comparing non-constant values means a value-dependent branch
--   -> error poison (the same safety net (1) as for AffV).
symPoison :: a
symPoison = error "SExp: non-constant comparison (value-dependent branch)"

instance Eq SExp where
  SC a == SC b = a == b
  _    == _    = symPoison

instance Ord SExp where
  compare (SC a) (SC b) = compare a b
  compare _        _    = symPoison

-- | [日本語]: 定数畳み込み付きノード構築。
--   [English]: Node construction with constant folding.
sc1 :: SUn -> SExp -> SExp
sc1 o (SC a) = SC (sUnF o a)
sc1 o e      = S1 o e

sc2 :: SBin -> SExp -> SExp -> SExp
sc2 o (SC a) (SC b) = SC (sBinF o a b)
sc2 o a b           = S2 o a b

instance Num SExp where
  (+) = sc2 SAddO
  (-) = sc2 SSubO
  (*) = sc2 SMulO
  negate = sc1 SNegO
  abs    = sc1 SAbsO
  signum = sc1 SSignumO
  fromInteger = SC . fromInteger

instance Fractional SExp where
  (/) = sc2 SDivO
  recip = sc1 SRecipO
  fromRational = SC . fromRational

instance Floating SExp where
  pi    = SC pi
  exp   = sc1 SExpO
  log   = sc1 SLogO
  sqrt  = sc1 SSqrtO
  sin   = sc1 SSinO
  cos   = sc1 SCosO
  tan   = sc1 STanO
  asin  = sc1 SAsinO
  acos  = sc1 SAcosO
  atan  = sc1 SAtanO
  sinh  = sc1 SSinhO
  cosh  = sc1 SCoshO
  tanh  = sc1 STanhO
  asinh = sc1 SAsinhO
  acosh = sc1 SAcoshO
  atanh = sc1 SAtanhO

-- | [日本語]: 構造一致 (total・poison しない。 族 prior の同型判定用)。
--   [English]: Structural equality (total, never poisons; used to judge
--   whether family priors are isomorphic).
sexpEq :: SExp -> SExp -> Bool
sexpEq (SC a)     (SC b)     = a == b
sexpEq (SV a)     (SV b)     = a == b
sexpEq (S1 o a)   (S1 p b)   = o == p && sexpEq a b
sexpEq (S2 o a c) (S2 p b d) = o == p && sexpEq a b && sexpEq c d
sexpEq _          _          = False

-- | [日本語]: 式中の latent 参照名。
--   [English]: The names of latent references appearing in the expression.
sexpVars :: SExp -> Set Text
sexpVars (SC _)     = Set.empty
sexpVars (SV n)     = Set.singleton n
sexpVars (S1 _ e)   = sexpVars e
sexpVars (S2 _ a b) = sexpVars a `Set.union` sexpVars b

-- | [日本語]: μ 式の「形の指紋」。 演算子木の形と leaf の SC/SV 区別のみで、
--   値・名前は含めない。 同一 σ 下で式形が混在しても指紋ごとに独立のグループとして
--   'unifyMany' に掛けるためのキー (形違いで σ グループ丸ごと drop しない)。
--   「全行同一 SV」 と「行で異なる SV (族 gather)」 の区別は従来どおり unify 側の仕事。
--   [English]: The "shape fingerprint" of a μ expression. Includes only the
--   operator tree's shape and the SC\/SV distinction at leaves — no values
--   or names. Used as a key so that, even when expression shapes mix under
--   the same σ, each fingerprint forms an independent group fed into
--   'unifyMany' (so a σ group with mixed shapes isn't dropped entirely).
--   Distinguishing "same SV across all rows" from "different SV per row (a
--   family gather)" remains the job of the unify step, as before.
sexpShape :: SExp -> String
sexpShape (SC _)     = "c"
sexpShape (SV _)     = "v"
sexpShape (S1 o e)   = show o ++ '(' : sexpShape e ++ ")"
sexpShape (S2 o a b) = show o ++ '(' : sexpShape a ++ ',' : sexpShape b ++ ")"

-- | [日本語]: σ 式の「名前付き指紋」。 'sexpShape' と違い SV は latent 名を
--   含める: σ 側は名前が違えば別グループに分ける (σ leaf を行で混ぜて族 gather に
--   持ち上げると、 族 prior 条件を満たさない σ 同士の合流でグループ全体が drop する
--   退行が起き得るため、 σ は保守的に「同一式 (定数値のみ行依存可)」 でキーする)。
--   heteroscedastic (例 @exp(g0 + g1·z_i)@) は名前が全行同一・データ定数だけ行で
--   違う形なので、 このキーで 1 グループに揃い 'unifyMany' が UC 列に持ち上げる。
--   [English]: The "named fingerprint" of a σ expression. Unlike
--   'sexpShape', SV includes the latent name here: on the σ side, rows
--   with different names go into different groups (mixing σ leaves across
--   rows into a family gather could regress by merging σ's that don't
--   satisfy the family-prior condition and dropping the whole group, so σ
--   is keyed conservatively by "the same expression, only constant data
--   values may vary by row"). A heteroscedastic case (e.g.
--   @exp(g0 + g1·z_i)@) has the same name across all rows with only the
--   data constant differing by row, so it lines up into one group under
--   this key and 'unifyMany' lifts it to a UC column.
sexpKeyNamed :: SExp -> String
sexpKeyNamed (SC _)     = "c"
sexpKeyNamed (SV n)     = "v:" ++ T.unpack n
sexpKeyNamed (S1 o e)   = show o ++ '(' : sexpKeyNamed e ++ ")"
sexpKeyNamed (S2 o a b) =
  show o ++ '(' : sexpKeyNamed a ++ ',' : sexpKeyNamed b ++ ")"

-- | [日本語]: scalar 'Observe' 行の分布部。 IR 化対象の分布のみ。
--
--   ★分布追加チェックリスト (1 分布 = 6 箇所・1 commit):
--     1. 'collectSymRows' に Observe 分岐 (+観測値定義域チェック → 域外行を含む
--        グループは収集時に弾く = walk の -∞ 縮退を残す安全方向)
--     2. @keyOf@ に family タグ (位置-尺度系は scale 側を 'sexpKeyNamed')
--     3. @tryGroup@ の unify 分岐
--     4. 'VecGroupSrc' / 'VecObsIR' ctor (+NFData) + 観測値定数の compile 時前計算
--     5. 密度式 + 値 guard ('logDensityObs' の該当分岐と完全一致。 densityIR の
--        式のみ・勾配は記号微分で自動)
--     6. test: 吸収確認 + 値 1e-9 + 勾配 ad 1e-9 + 中心差分 1e-4 + fallback 確認。
--        probe 点 (0.5/1.3) の定義域を分布別に確認 (link 経由は構造上域内・
--        パラメタ latent 直で域外なら fallback = 既知制限)
--   [English]: The distribution part of a scalar 'Observe' row. Only
--   distributions targeted for IR-ification.
--
--   ★Checklist for adding a distribution (one distribution = 6 spots, 1
--   commit):
--     1. An Observe branch in 'collectSymRows' (+ an observed-value domain
--        check -> groups containing out-of-domain rows are rejected at
--        collection time, the safe direction that preserves the walk's -∞
--        degeneration)
--     2. A family tag in @keyOf@ (for location-scale families, key the
--        scale side with 'sexpKeyNamed')
--     3. A unify branch in @tryGroup@
--     4. A 'VecGroupSrc' \/ 'VecObsIR' constructor (+ NFData) and
--        precomputing the observed-value constants at compile time
--     5. The density expression + value guard (must exactly match the
--        corresponding branch of 'logDensityObs'; only the densityIR
--        expression is needed — the gradient is automatic via symbolic
--        differentiation)
--     6. A test: confirm absorption + value within 1e-9 + AD gradient
--        within 1e-9 + central-difference within 1e-4 + confirm fallback.
--        Check the probe points' (0.5\/1.3) domain per distribution (via a
--        link function they're structurally in-domain; with a parameter
--        directly on a latent, out-of-domain triggers fallback — a known
--        limitation)
data SymDist
  = SDGauss SExp SExp   -- ^ [日本語]: Normal μ σ (σ は任意式)。 [English]: Normal μ σ (σ may be any expression).
  | SDPois  SExp        -- ^ [日本語]: Poisson λ (λ は任意式・GLM log link は exp が式に入る)。 [English]: Poisson λ (λ may be any expression; a GLM log link puts exp in the expression).
  | SDBern  SExp        -- ^ [日本語]: Bernoulli p (同・invLogit が式に入る)。 [English]: Bernoulli p (likewise; invLogit appears in the expression).
  | SDStudT !Double SExp SExp
    -- ^ [日本語]: StudentT ν μ σ (ν は SC 定数のみ吸収 = lgamma 項が定数化。
    --   ν latent は fallback・計画の scope どおり)。
    --   [English]: StudentT ν μ σ (only an SC constant ν is absorbed, i.e.
    --   the lgamma term becomes a constant; a latent ν falls back, as
    --   planned).
  | SDCauchy SExp SExp  -- ^ [日本語]: Cauchy x₀ γ。 [English]: Cauchy x₀ γ.
  | SDLogis SExp SExp   -- ^ [日本語]: Logistic μ s。 [English]: Logistic μ s.
  | SDGumbel SExp SExp  -- ^ [日本語]: Gumbel μ β。 [English]: Gumbel μ β.
  | SDExpo SExp         -- ^ [日本語]: Exponential rate (y ≥ 0 は収集時に確認)。 [English]: Exponential rate (y ≥ 0 is checked at collection time).
  | SDWeib SExp SExp    -- ^ [日本語]: Weibull k λ (y > 0 は収集時に確認)。 [English]: Weibull k λ (y > 0 is checked at collection time).
  | SDLogN SExp SExp    -- ^ [日本語]: LogNormal μ σ (y > 0 は収集時に確認)。 [English]: LogNormal μ σ (y > 0 is checked at collection time).
  | SDGamma SExp SExp   -- ^ [日本語]: Gamma α rate (y > 0 は収集時に確認)。 [English]: Gamma α rate (y > 0 is checked at collection time).
  | SDBeta SExp SExp    -- ^ [日本語]: Beta α β (0 < y < 1 は収集時に確認)。 [English]: Beta α β (0 < y < 1 is checked at collection time).
  | SDBinom !Int SExp   -- ^ [日本語]: Binomial n p (n は ctor 定数・
                        --   0 ≤ round y ≤ n は収集時に確認)。
                        --   [English]: Binomial n p (n is a constructor
                        --   constant; 0 ≤ round y ≤ n is checked at
                        --   collection time).
  | SDGeom SExp         -- ^ [日本語]: Geometric p (round y ≥ 1 は収集時に確認)。 [English]: Geometric p (round y ≥ 1 is checked at collection time).
  | SDNegBin SExp SExp  -- ^ [日本語]: NegativeBinomial μ α (y ≥ 0 は収集時に確認)。 [English]: NegativeBinomial μ α (y ≥ 0 is checked at collection time).
  | SDMixNorm2 SExp SExp SExp SExp SExp SExp
    -- ^ [日本語]: Mixture [w1,w2] [Normal μ1 σ1, Normal μ2 σ2] (2 成分
    --   Normal 混合限定 — 任意分布族・K 成分への一般化は対象外。 w1 w2 は
    --   'Distribution.hs' の Mixture 定義どおり Σw で自動正規化するので
    --   w1+w2=1 を仮定しない (w1 w2 μ1 σ1 μ2 σ2)。
    --   [English]: Mixture [w1,w2] [Normal μ1 σ1, Normal μ2 σ2] (limited to
    --   a 2-component Normal mixture — generalizing to arbitrary
    --   distribution families or K components is out of scope. w1 and w2
    --   are automatically normalized by Σw, per the Mixture definition in
    --   'Distribution.hs', so w1+w2=1 is not assumed (w1 w2 μ1 σ1 μ2 σ2)).
  | SDZIBinom !Int SExp SExp
    -- ^ [日本語]: ZeroInflatedBinomial n ψ p (n は ctor 定数・
    --   0 ≤ round y ≤ n は収集時に確認)。
    --   [English]: ZeroInflatedBinomial n ψ p (n is a constructor constant;
    --   0 ≤ round y ≤ n is checked at collection time).

-- | [日本語]: model を 'SExp' で walk し、 scalar @Observe@ 行 (Observe 名, 分布部, 観測値)
--   と latent prior を集める ('collectAffRows' の後継版)。 σ を任意式にし、
--   対象分布を Normal 限定から Poisson / Bernoulli 等にも拡張している。 他のノードは
--   素通し (residual walk に残す)。
--   [English]: Walks the model with 'SExp' and collects scalar @Observe@
--   rows (Observe name, distribution part, observed value) and latent
--   priors (the successor to 'collectAffRows'). σ may be any expression,
--   and the target distributions are extended from Normal-only to also
--   include Poisson \/ Bernoulli and others. Other nodes pass through
--   unchanged (left on the residual walk).
collectSymRows
  :: Model SExp r
  -> ([(Text, SymDist, Double)], Map Text (Distribution SExp))
collectSymRows = go [] Map.empty
  where
    go rows priors (Pure _) = (reverse rows, priors)
    go rows priors (Free f) = case f of
      Sample n d k -> go rows (Map.insert n d priors) (k (SV n))
      Observe nm (Normal mu sg) ys next ->
        go ([ (nm, SDGauss mu sg, y) | y <- ys ] ++ rows) priors next
      Observe nm (Poisson lam) ys next ->
        go ([ (nm, SDPois lam, y) | y <- ys ] ++ rows) priors next
      Observe nm (Bernoulli p) ys next ->
        go ([ (nm, SDBern p, y) | y <- ys ] ++ rows) priors next
      -- 56.3 位置-尺度系 (support = ℝ → 観測値定義域チェック不要)。
      -- StudentT は ν=SC かつ ν>0 のみ (ν≤0 は walk の -∞ を残す安全方向)。
      Observe nm (StudentT (SC nu) mu sg) ys next | nu > 0 ->
        go ([ (nm, SDStudT nu mu sg, y) | y <- ys ] ++ rows) priors next
      Observe nm (Cauchy loc sc) ys next ->
        go ([ (nm, SDCauchy loc sc, y) | y <- ys ] ++ rows) priors next
      Observe nm (Logistic mu s) ys next ->
        go ([ (nm, SDLogis mu s, y) | y <- ys ] ++ rows) priors next
      Observe nm (Gumbel mu be) ys next ->
        go ([ (nm, SDGumbel mu be, y) | y <- ys ] ++ rows) priors next
      -- 56.4 正値・区間系 (観測値定義域チェックは tryGroup の ysV 検査で)。
      Observe nm (Exponential rate) ys next ->
        go ([ (nm, SDExpo rate, y) | y <- ys ] ++ rows) priors next
      Observe nm (Weibull k lam) ys next ->
        go ([ (nm, SDWeib k lam, y) | y <- ys ] ++ rows) priors next
      Observe nm (LogNormal mu sg) ys next ->
        go ([ (nm, SDLogN mu sg, y) | y <- ys ] ++ rows) priors next
      Observe nm (Gamma sh rt) ys next ->
        go ([ (nm, SDGamma sh rt, y) | y <- ys ] ++ rows) priors next
      Observe nm (Beta al be) ys next ->
        go ([ (nm, SDBeta al be, y) | y <- ys ] ++ rows) priors next
      -- 56.5 離散系。
      Observe nm (Binomial n p) ys next ->
        go ([ (nm, SDBinom n p, y) | y <- ys ] ++ rows) priors next
      Observe nm (Geometric p) ys next ->
        go ([ (nm, SDGeom p, y) | y <- ys ] ++ rows) priors next
      Observe nm (NegativeBinomial mu al) ys next ->
        go ([ (nm, SDNegBin mu al, y) | y <- ys ] ++ rows) priors next
      -- Phase 90 A3: 2成分 Normal 混合限定 (04-low-dim-gauss-mix の
      -- log_mix(θ, normal_lpdf(μ1,σ1), normal_lpdf(μ2,σ2)) と同型)。
      -- 任意分布族・3成分以上は対象外 (素通し → residual walk+ad)。
      Observe nm (Mixture [w1, w2] [Normal mu1 sg1, Normal mu2 sg2]) ys next ->
        go ([ (nm, SDMixNorm2 w1 w2 mu1 sg1 mu2 sg2, y) | y <- ys ] ++ rows) priors next
      Observe nm (ZeroInflatedBinomial n psi p) ys next ->
        go ([ (nm, SDZIBinom n psi p, y) | y <- ys ] ++ rows) priors next
      Observe _ _ _ next  -> go rows priors next
      ObserveLM _ _ _ _ _ _ next -> go rows priors next
      Potential _ _ next  -> go rows priors next
      Deterministic _ v k -> go rows priors (k v)
      Data _ ys k         -> go rows priors (k (map realToFrac ys, ys))
      DataIx _ is k       -> go rows priors (k is)
      PlateBegin _ _ next -> go rows priors next
      PlateEnd next       -> go rows priors next

-- | [日本語]: model を 'SExp' で walk し、 raw 'Potential' の (名前, 式) を出現順に
--   集める。 'collectSymRows' と同じ給餌 (latent = @SV n@)。
--   [English]: Walks the model with 'SExp' and collects raw 'Potential'
--   (name, expression) pairs in order of appearance. Uses the same feed as
--   'collectSymRows' (latent = @SV n@).
collectSymPots :: Model SExp r -> [(Text, SExp)]
collectSymPots = go []
  where
    go acc (Pure _) = reverse acc
    go acc (Free f) = case f of
      Sample n _ k        -> go acc (k (SV n))
      Observe _ _ _ next  -> go acc next
      ObserveLM _ _ _ _ _ _ next -> go acc next
      Potential nm v next -> go ((nm, v) : acc) next
      Deterministic _ v k -> go acc (k v)
      Data _ ys k         -> go acc (k (map realToFrac ys, ys))
      DataIx _ is k       -> go acc (k is)
      PlateBegin _ _ next -> go acc next
      PlateEnd next       -> go acc next

-- ===========================================================================
-- Phase 98 A2: 残余 log-joint の flat compile (Free AST 再解釈の廃止)
-- ===========================================================================
-- @logJointExclBlocks@ (Gradient.hs) は excl 吸収後の残余 log-density を求める
-- ため 'Model a r' の Free 構造を毎勾配評価で頭から walk する。 vecIR arena に
-- 吸収し切れない項 (例 06-irt-2pl: `a` の LogNormal 事前分布) が残るモデルでは、
-- 大量の吸収済み Observe plate まで「継続のため素通り walk」する純オーバーヘッド
-- が支配する (Phase 98 A1c prof: logJointExclBlocks = 31.5% time / 41.7% alloc・
-- Free monad `>>=`/`fmap` が十数億 entry)。
--
-- 本 IR は残余を **1 度の symbolic walk で flat 化**し ('CompiledResidual')、 全
-- leapfrog で「非吸収項の畳み込み」だけを行う (Free walk 廃止)。 @CompiledLMBlock@
-- の残余版に相当。 'SExp' 保持の純データなので値 (Double) と勾配 (AD 型) の双方で
-- 共有できる ('residualValueA' が多相)。

-- | [日本語]: 残余 log-joint の非吸収項を出現順に flat 化した中間表現。
--   [English]: An intermediate representation that flattens the
--   non-absorbed terms of the residual log-joint, in order of appearance.
data CompiledResidual = CompiledResidual
  { crPriors :: ![(Text, Distribution SExp)]     -- ^ [日本語]: 非吸収 Sample: logDensity d (params!n)。 [English]: A non-absorbed Sample: logDensity d (params!n).
  , crObs    :: ![(Distribution SExp, [Double])] -- ^ [日本語]: 非吸収 Observe: obsLogSum d ys。 [English]: A non-absorbed Observe: obsLogSum d ys.
  , crPots   :: ![SExp]                          -- ^ [日本語]: 非吸収 Potential: 式値。 [English]: A non-absorbed Potential: the expression value.
  }

-- | [日本語]: 'sUnF' の 'Floating' 一般化 (density IR 専用の @SLgammaO@ を除く —
--   SLgammaO は 'Floating SExp' インスタンス経由では現れず 'Distribution SExp' に
--   入らない)。
--   [English]: A 'Floating' generalization of 'sUnF' (excluding @SLgammaO@,
--   which is specific to the density IR — it never shows up via the
--   'Floating SExp' instance, so it never ends up inside 'Distribution SExp').
sUnG :: Floating a => SUn -> a -> a
sUnG SNegO    = negate
sUnG SAbsO    = abs
sUnG SSignumO = signum
sUnG SExpO    = exp
sUnG SLogO    = log
sUnG SSqrtO   = sqrt
sUnG SRecipO  = recip
sUnG SSinO    = sin
sUnG SCosO    = cos
sUnG STanO    = tan
sUnG SAsinO   = asin
sUnG SAcosO   = acos
sUnG SAtanO   = atan
sUnG SSinhO   = sinh
sUnG SCoshO   = cosh
sUnG STanhO   = tanh
sUnG SAsinhO  = asinh
sUnG SAcoshO  = acosh
sUnG SAtanhO  = atanh
sUnG SLgammaO = error "sUnG: SLgammaO は残余 SExp には現れない (compileResidual の不変条件)"

-- | [日本語]: 'sBinF' の 'Floating'+'Ord' 一般化。
--   [English]: A 'Floating'+'Ord' generalization of 'sBinF'.
sBinG :: (Floating a, Ord a) => SBin -> a -> a -> a
sBinG SAddO = (+)
sBinG SSubO = (-)
sBinG SMulO = (*)
sBinG SDivO = (/)
sBinG SMaxO = max

-- | [日本語]: 'SExp' を任意の 'Floating' 型で評価する (latent 参照は @lookupVar@
--   経由)。 'CompiledResidual' の per-eval 評価に使う (SExp 木の畳み込み・Free
--   walk 無し)。
--   [English]: Evaluates an 'SExp' at any 'Floating' type (latent references
--   go through @lookupVar@). Used for the per-eval evaluation of
--   'CompiledResidual' (folding the SExp tree, with no Free-monad walk).
evalSExpA :: (Floating a, Ord a) => (Text -> a) -> SExp -> a
evalSExpA lookupVar = ev
  where
    ev (SC x)     = realToFrac x
    ev (SV n)     = lookupVar n
    ev (S1 o e)   = sUnG o (ev e)
    ev (S2 o a b) = sBinG o (ev a) (ev b)

-- | [日本語]: 残余 (excl 吸収後) を 1 度の symbolic walk で 'CompiledResidual' に
--   flat 化する。 compiled 経路で忠実再現できない残余 (非吸収 'ObserveLM') が
--   あれば 'Nothing' を返し、 呼び出し側は従来の @logJointExclBlocks@ walk に
--   fallback する。 'Deterministic'\/'Data' は walk 時に 'SExp' へインライン
--   展開されるので収集式の 'SV' は必ず sampled latent を指す (per-eval の params
--   に存在)。
--   [English]: Flattens the residual (after excl absorption) into a
--   'CompiledResidual' with a single symbolic walk. Returns 'Nothing' if the
--   residual contains something the compiled path cannot faithfully
--   reproduce (a non-absorbed 'ObserveLM'), and the caller falls back to the
--   previous @logJointExclBlocks@ walk. 'Deterministic'\/'Data' get inlined
--   into 'SExp' during the walk, so any 'SV' in the collected expression
--   always refers to a sampled latent (present in the per-eval params).
compileResidual :: Set Text -> Model SExp r -> Maybe CompiledResidual
compileResidual excl = go [] [] []
  where
    go ps os pots (Pure _) =
      Just (CompiledResidual (reverse ps) (reverse os) (reverse pots))
    go ps os pots (Free f) = case f of
      Sample n d k
        | n `Set.member` excl -> go ps os pots (k (SV n))
        | otherwise           -> go ((n, d) : ps) os pots (k (SV n))
      Observe n d ys next
        | n `Set.member` excl -> go ps os pots next
        | otherwise           -> go ps ((d, ys) : os) pots next
      ObserveLM nm _ _ _ _ _ next
        | nm `Set.member` excl -> go ps os pots next
        | otherwise            -> Nothing   -- 非吸収 ObserveLM は flat 化不可 → fallback
      Potential n v next
        | n `Set.member` excl -> go ps os pots next
        | otherwise           -> go ps os (v : pots) next
      Deterministic _ v k -> go ps os pots (k v)
      Data _ ys k         -> go ps os pots (k (map realToFrac ys, ys))
      DataIx _ is k       -> go ps os pots (k is)
      PlateBegin _ _ next -> go ps os pots next
      PlateEnd next       -> go ps os pots next

-- | [日本語]: 'CompiledResidual' の per-eval 評価 (Free walk 無し・flat list の
--   畳み込み)。 'logJointExclBlocks excl m params' と同値 (同じ
--   @logDensity@\/'obsLogSum'・同じ params)。 sampled latent が params に無い
--   場合は @logJointExclBlocks@ と同じく -∞ (安全網)。
--   [English]: Per-eval evaluation of a 'CompiledResidual' (no Free-monad
--   walk, just folding the flat list). Equivalent to
--   'logJointExclBlocks excl m params' (same @logDensity@\/'obsLogSum', same
--   params). If a sampled latent is missing from params, returns -∞ just
--   like @logJointExclBlocks@ does (a safety net).
residualValueA :: (Floating a, Ord a) => CompiledResidual -> Map Text a -> a
residualValueA cr params = priorSum + obsSum + potSum
  where
    ev = evalSExpA (\n -> Map.findWithDefault 0 n params)
    priorSum = sum [ case Map.lookup n params of
                       Nothing -> negInf
                       Just v  -> logDensity (fmap ev d) v
                   | (n, d) <- crPriors cr ]
    obsSum   = sum [ obsLogSum (fmap ev d) ys | (d, ys) <- crObs cr ]
    potSum   = sum [ ev v | v <- crPots cr ]

-- | [日本語]: ベクトル式 IR。 'unifyMany' が行ごとの 'SExp' を束ねた結果で、
--   leaf はスカラ (行に依らない) かベクトル (行ごとに値が違う) のいずれか。
--   [English]: The vector-expression IR. The result of 'unifyMany' bundling
--   together the per-row 'SExp' values; each leaf is either a scalar
--   (row-independent) or a vector (differs per row).
data UExp
  = UK !Double                  -- ^ [日本語]: 全行同一の定数 (スカラ)。 [English]: A constant shared by all rows (scalar).
  | UC !(VS.Vector Double)      -- ^ [日本語]: 行ごとの定数列 (データ列・長さ n)。 [English]: A per-row constant column (a data column, length n).
  | UV !Text                    -- ^ [日本語]: 全行同一の latent (スカラ・broadcast)。 [English]: A latent shared by all rows (scalar, broadcast).
  | UG ![Text] !(VU.Vector Int) -- ^ [日本語]: 族 gather: 行 i は member[gids_i] (長さ n)。 [English]: A family gather: row i is member[gids_i] (length n).
  | U1 !SUn UExp
  | U2 !SBin UExp UExp
  | USum UExp                   -- ^ [日本語]: Σ (ベクトル → スカラ)。 raw potential 内の
                                --   同型 Σ チェーンのベクトル化に使う ('absorbPot')。
                                --   中身は行依存 ('uexpIsVec') であること。
                                --   [English]: A Σ (vector -> scalar). Used to
                                --   vectorize isomorphic Σ chains inside a raw
                                --   potential ('absorbPot'). The contents must be
                                --   row-dependent ('uexpIsVec').

instance NFData UExp where
  rnf (UK v)     = rnf v
  rnf (UC v)     = v `seq` ()
  rnf (UV n)     = rnf n
  rnf (UG ms g)  = rnf ms `seq` g `seq` ()
  rnf (U1 o e)   = o `seq` rnf e
  rnf (U2 o a b) = o `seq` rnf a `seq` rnf b
  rnf (USum e)   = rnf e

-- | [日本語]: 行ごとのスカラ式を 1 本のベクトル式に持ち上げる (形状照合)。
--   演算子木が全行同型で、 leaf が「全行 SC」「全行同一 SV」「行ごとに違う SV
--   (→ 族 gather 候補)」 のいずれかに揃う場合のみ成功。
--   [English]: Lifts per-row scalar expressions into a single vector
--   expression (shape matching). Succeeds only if the operator tree is
--   isomorphic across all rows and the leaves are uniformly one of: "SC in
--   every row", "the same SV in every row", or "a different SV per row"
--   (a candidate for a family gather).
unifyMany :: [SExp] -> Maybe UExp
unifyMany []          = Nothing
unifyMany es@(e0 : _) = case e0 of
  SC _ -> do
    vs <- mapM (\e -> case e of SC v -> Just v; _ -> Nothing) es
    Just $ case vs of
      (v : rest) | all (== v) rest -> UK v
      _                            -> UC (VS.fromList vs)
  SV _ -> do
    ns <- mapM (\e -> case e of SV n -> Just n; _ -> Nothing) es
    Just $ case ns of
      (n0 : rest) | all (== n0) rest -> UV n0
      _ ->
        let mems = Set.toAscList (Set.fromList ns)
            ixm  = Map.fromList (zip mems [0 :: Int ..])
        in UG mems (VU.fromList [ ixm Map.! n | n <- ns ])
  S1 o _ -> do
    cs <- mapM (\e -> case e of S1 o' c | o' == o -> Just c; _ -> Nothing) es
    U1 o <$> unifyMany cs
  S2 o _ _ -> do
    ps <- mapM (\e -> case e of S2 o' a b | o' == o -> Just (a, b); _ -> Nothing) es
    U2 o <$> unifyMany (map fst ps) <*> unifyMany (map snd ps)

-- | [日本語]: IR 中のスカラ latent 参照 (出現順・重複あり)。
--   [English]: Scalar latent references inside the IR (in order of
--   appearance, duplicates included).
uexpScalNames :: UExp -> [Text]
uexpScalNames (UV n)     = [n]
uexpScalNames (U1 _ e)   = uexpScalNames e
uexpScalNames (U2 _ a b) = uexpScalNames a ++ uexpScalNames b
uexpScalNames (USum e)   = uexpScalNames e
uexpScalNames _          = []

-- | [日本語]: IR 中の族 gather の member リスト (出現順・重複あり)。
--   [English]: The member lists of family gathers inside the IR (in order
--   of appearance, duplicates included).
uexpFamilies :: UExp -> [[Text]]
uexpFamilies (UG ms _)   = [ms]
uexpFamilies (U1 _ e)    = uexpFamilies e
uexpFamilies (U2 _ a b)  = uexpFamilies a ++ uexpFamilies b
uexpFamilies (USum e)    = uexpFamilies e
uexpFamilies _           = []

-- | [日本語]: 式が行依存 (ベクトル形) か ('ruIsVec' の 'UExp' 版)。
--   'USum' は Σ 済みなのでスカラ。 'ruIsVec' と同じ共有無視走査の残党
--   (absorbPot 経路で同じ指数爆発があり得る) のため、 同時に StableName memo
--   walk 化 (詳細は 'ruIsVec')。
--   [English]: Whether an expression is row-dependent (vector-shaped); the
--   'UExp' counterpart of 'ruIsVec'. 'USum' has already summed, so it's a
--   scalar. Left over from the same share-ignoring traversal as 'ruIsVec'
--   (the same exponential blowup can happen on the absorbPot path), so it
--   is likewise turned into a StableName memo walk (see 'ruIsVec' for
--   details).
uexpIsVec :: UExp -> Bool
uexpIsVec e0 = unsafePerformIO $ do
  memo <- newIORef IM.empty
  let go x0 = do
        x  <- evaluate x0
        sn <- makeStableName x
        let h = hashStableName sn
        mm <- readIORef memo
        case lookup sn =<< IM.lookup h mm of
          Just r  -> pure r
          Nothing -> do
            r <- case x of
              UC _     -> pure True
              UG _ _   -> pure True
              U1 _ e   -> go e
              U2 _ a b -> (||) <$> go a <*> go b
              _        -> pure False
            modifyIORef' memo (IM.insertWith (++) h [(sn, r)])
            pure r
  go e0
{-# NOINLINE uexpIsVec #-}

-- | [日本語]: 出現順を保つ重複排除。
--   [English]: Deduplication that preserves order of appearance.
ordNubO :: Ord a => [a] -> [a]
ordNubO = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
      | x `Set.member` seen = go seen xs
      | otherwise           = x : go (Set.insert x seen) xs

-- | [日本語]: IR グループ (unify 後・compile 前)。 family ごとに観測密度の
--   組み方が違う (Gaussian 限定から Poisson / Bernoulli を追加していった経緯あり)。
--   [English]: An IR group (after unify, before compile). The way the
--   observation density is assembled differs per family (originally
--   Gaussian-only, with Poisson / Bernoulli added later).
data VecGroupSrc
  = VGGauss !UExp !UExp !(VS.Vector Double)  -- ^ [日本語]: μ IR, σ IR, ys。 [English]: μ IR, σ IR, ys.
  | VGPois  !UExp !(VS.Vector Double)        -- ^ [日本語]: λ IR, ys (全行 y ≥ 0 を確認済)。 [English]: λ IR, ys (confirmed y ≥ 0 for every row).
  | VGBern  !UExp !(VS.Vector Double)        -- ^ [日本語]: p IR, ys (全行 round y ∈ {0,1})。 [English]: p IR, ys (round y ∈ {0,1} for every row).
  | VGStudT !Double !UExp !UExp !(VS.Vector Double)
    -- ^ [日本語]: ν (SC 定数), μ IR, σ IR, ys。 [English]: ν (an SC constant), μ IR, σ IR, ys.
  | VGCauchy !UExp !UExp !(VS.Vector Double)  -- ^ [日本語]: x₀ IR, γ IR, ys。 [English]: x₀ IR, γ IR, ys.
  | VGLogis !UExp !UExp !(VS.Vector Double)   -- ^ [日本語]: μ IR, s IR, ys。 [English]: μ IR, s IR, ys.
  | VGGumbel !UExp !UExp !(VS.Vector Double)  -- ^ [日本語]: μ IR, β IR, ys。 [English]: μ IR, β IR, ys.
  | VGExpo !UExp !(VS.Vector Double)          -- ^ [日本語]: rate IR, ys (全行 y ≥ 0)。 [English]: rate IR, ys (y ≥ 0 for every row).
  | VGWeib !UExp !UExp !(VS.Vector Double)    -- ^ [日本語]: k IR, λ IR, ys (全行 y > 0)。 [English]: k IR, λ IR, ys (y > 0 for every row).
  | VGLogN !UExp !UExp !(VS.Vector Double)    -- ^ [日本語]: μ IR, σ IR, ys (全行 y > 0)。 [English]: μ IR, σ IR, ys (y > 0 for every row).
  | VGGamma !UExp !UExp !(VS.Vector Double)   -- ^ [日本語]: α IR, rate IR, ys (全行 y > 0)。 [English]: α IR, rate IR, ys (y > 0 for every row).
  | VGBeta !UExp !UExp !(VS.Vector Double)    -- ^ [日本語]: α IR, β IR, ys (全行 0<y<1)。 [English]: α IR, β IR, ys (0<y<1 for every row).
  | VGBinom !(VS.Vector Double) !UExp !(VS.Vector Double)
    -- ^ [日本語]: n 列 (行対応), p IR, ys (0≤k≤n。 n を行対応 Vector 化する
    --   ことで n 別の group 分裂を解消し 1 group にまとめてある)。
    --   [English]: an n column (per row), p IR, ys (0≤k≤n). Making n a
    --   per-row vector avoids splitting into separate groups per distinct
    --   n, keeping everything in a single group.
  | VGGeom !UExp !(VS.Vector Double)          -- ^ [日本語]: p IR, ys (round y ≥ 1)。 [English]: p IR, ys (round y ≥ 1).
  | VGNegBin !UExp !UExp !(VS.Vector Double)  -- ^ [日本語]: μ IR, α IR, ys (y ≥ 0)。 [English]: μ IR, α IR, ys (y ≥ 0).
  | VGMixNorm2 !UExp !UExp !UExp !UExp !UExp !UExp !(VS.Vector Double)
    -- ^ [日本語]: w1 IR, w2 IR, μ1 IR, σ1 IR, μ2 IR, σ2 IR, ys (2成分限定)。
    --   [English]: w1 IR, w2 IR, μ1 IR, σ1 IR, μ2 IR, σ2 IR, ys
    --   (two-component mixtures only).
  | VGZIBinom !(VS.Vector Double) !UExp !UExp !(VS.Vector Double)
    -- ^ [日本語]: n 列 (行対応), ψ IR, p IR, ys (0≤k≤n。 n は行対応 Vector 化
    --   済)。 [English]: an n column (per row), ψ IR, p IR, ys (0≤k≤n; n is
    --   already a per-row vector).
  | VGPot !UExp
    -- ^ [日本語]: raw `potential` 項。 scalar 形の UExp (内部の同型
    --   Σ チェーンは 'USum' でベクトル化済・'absorbPot')。 値 = 式そのもの
    --   (ys なし・guard なし = walk の 'Potential' 加算と同値)。
    --   [English]: A raw `potential` term: a scalar-shaped UExp (any
    --   isomorphic Σ chain inside it has already been vectorized as 'USum'
    --   by 'absorbPot'). Its value is the expression itself (no ys, no
    --   guard — equivalent to the 'Potential' addition performed by the
    --   walk).

instance NFData VecGroupSrc where
  rnf (VGGauss u sg ys)    = rnf u `seq` rnf sg `seq` ys `seq` ()
  rnf (VGPois u ys)        = rnf u `seq` ys `seq` ()
  rnf (VGBern u ys)        = rnf u `seq` ys `seq` ()
  rnf (VGStudT nu u sg ys) = nu `seq` rnf u `seq` rnf sg `seq` ys `seq` ()
  rnf (VGCauchy u sc ys)   = rnf u `seq` rnf sc `seq` ys `seq` ()
  rnf (VGLogis u s ys)     = rnf u `seq` rnf s `seq` ys `seq` ()
  rnf (VGGumbel u be ys)   = rnf u `seq` rnf be `seq` ys `seq` ()
  rnf (VGExpo u ys)        = rnf u `seq` ys `seq` ()
  rnf (VGWeib k u ys)      = rnf k `seq` rnf u `seq` ys `seq` ()
  rnf (VGLogN u sg ys)     = rnf u `seq` rnf sg `seq` ys `seq` ()
  rnf (VGGamma sh u ys)    = rnf sh `seq` rnf u `seq` ys `seq` ()
  rnf (VGBeta al u ys)     = rnf al `seq` rnf u `seq` ys `seq` ()
  rnf (VGBinom nv u ys)    = nv `seq` rnf u `seq` ys `seq` ()
  rnf (VGGeom u ys)        = rnf u `seq` ys `seq` ()
  rnf (VGNegBin u al ys)   = rnf u `seq` rnf al `seq` ys `seq` ()
  rnf (VGMixNorm2 w1 w2 m1 s1 m2 s2 ys) =
    rnf w1 `seq` rnf w2 `seq` rnf m1 `seq` rnf s1 `seq` rnf m2 `seq`
    rnf s2 `seq` ys `seq` ()
  rnf (VGZIBinom nv psi p ys) = nv `seq` rnf psi `seq` rnf p `seq` ys `seq` ()
  rnf (VGPot u)              = rnf u

-- | [日本語]: 'synthVecIR' の結果: (グループ列, 族 prior (members, m, τ),
--   吸収した scalar Observe / raw potential 名集合 = residual walk から
--   除外すべき名前)。 σ は 'UExp' 型 (スカラ式なら値はスカラ・UC を含む
--   行依存式なら heteroscedastic ベクトル密度)。
--   [English]: The result of 'synthVecIR': (the group list, family priors
--   (members, m, τ), the set of absorbed scalar-Observe \/ raw-potential
--   names — i.e. names to exclude from the residual walk). σ is a 'UExp'
--   (a scalar expression gives a scalar value; a row-dependent expression
--   containing UC gives a heteroscedastic vector density).
type VecIRSrc =
  ( [VecGroupSrc]
  , [([Text], SExp, SExp)]
  , Set Text )

-- ===========================================================================
-- Phase 90 A8: 式 DAG 化 (共有保存 hash-consing) — synthVecIR 指数ハングの根治
-- ===========================================================================
--
-- 従来の合成解析 ('sexpShape'/'sexpVars'/'unifyMany'/'rnf' 等) は 'SExp' を
-- 素朴な木として walk していたが、 ユーザコードの let 共有 (RK4 等の逐次再帰で
-- 前状態を複数回参照する形) を無視すると訪問回数が「経路数」 (深さに対し指数)
-- に比例して爆発する (A6 実測: RK4 深さ5で DAG 352 ノード vs 経路 6.8×10¹¹)。
-- ここでは StableName (heap 同一性) + 構造 intern (hash-consing) で式を一度
-- だけ明示的 DAG (ノード表 + ID) に変換し、 以後の解析を全て ID ベース
-- O(distinct ノード数) で行う。 形状クラス・自由変数集合はノード生成時に
-- bottom-up で確定する (子 ID は常に親より先に intern 済み)。

-- | [日本語]: 'SExp' の DAG ノード (子は intern 済み ID)。 構造 intern のキー =
--   「構造が等しい ⇔ ID が等しい」 が成立する ('sexpEq'/'sexpKeyNamed' の代替)。
--   [English]: A DAG node for 'SExp' (children are already-interned IDs).
--   The key for structural interning: "structurally equal ⇔ same ID" holds
--   (replaces 'sexpEq'\/'sexpKeyNamed').
data SNode = NC !Double | NV !Text | N1 !SUn !Int | N2 !SBin !Int !Int
  deriving (Eq, Ord)

-- | [日本語]: latent 名を消した形状クラスのキー ('sexpShape' の代替。 SV は全て
--   KV に潰れる = 名前違いの行が同一形状クラスに揃い族 gather 候補になる)。
--   [English]: The key for the shape class with latent names erased
--   (replaces 'sexpShape'; every SV collapses to KV, so rows differing
--   only by name line up under the same shape class and become family
--   gather candidates).
data ShapeKey = KC | KV | K1 !SUn !Int | K2 !SBin !Int !Int
  deriving (Eq, Ord)

-- | [日本語]: 名前付き指紋のキー ('sexpKeyNamed' の代替)。 SV は latent 名を
--   保持・__SC は値を無視して同一クラスに潰す__ (行ごとに違うデータ定数だけの
--   σ 式を 1 グループに束ね、 unify が UC 列へ持ち上げる仕様)。 構造
--   intern ID ('sexpEq' 相当・定数値まで厳密) とは役割が違う点に注意。
--   [English]: The key for the named fingerprint (replaces
--   'sexpKeyNamed'). SV keeps the latent name; __SC ignores its value__
--   and collapses into the same class (this bundles per-row σ expressions
--   that differ only by a data constant into one group, which unify then
--   lifts into a UC column). Note this plays a different role from the
--   structural intern ID ('sexpEq'-equivalent, strict down to constant
--   values).
data NamedKey = MC | MV !Text | M1 !SUn !Int | M2 !SBin !Int !Int
  deriving (Eq, Ord)

-- | [日本語]: intern 状態。 memo は 2 段: heap 同一性 (StableName・共有 thunk
--   の再走査防止) と構造 ('SNode'・等価だが別 heap の部分式を同一 ID に合流)。
--   [English]: The interning state. The memo has two levels: heap identity
--   (StableName, preventing re-traversal of shared thunks) and structure
--   ('SNode', merging equal-but-different-heap subexpressions into the
--   same ID).
data SDagSt = SDagSt
  { sdStable :: !(IM.IntMap [(StableName SExp, Int)])
  , sdStruct :: !(Map SNode Int)
  , sdNodes  :: !(IM.IntMap SNode)             -- ^ [日本語]: ID → ノード。 [English]: ID -> node.
  , sdShapes :: !(Map ShapeKey Int)            -- ^ [日本語]: 形状 intern。 [English]: Shape interning.
  , sdShape  :: !(IM.IntMap Int)               -- ^ [日本語]: ID → 形状クラス ID。 [English]: ID -> shape-class ID.
  , sdNamedKs :: !(Map NamedKey Int)           -- ^ [日本語]: 名前付き指紋 intern。 [English]: Named-fingerprint interning.
  , sdNamed  :: !(IM.IntMap Int)               -- ^ [日本語]: ID → 名前付き指紋 ID。 [English]: ID -> named-fingerprint ID.
  , sdVars   :: !(IM.IntMap (Set Text))        -- ^ [日本語]: ID → 自由 latent 集合。 [English]: ID -> the set of free latents.
  , sdNext   :: !Int
  , sdUnify  :: !(Map [Int] UExp)
    -- ^ [日本語]: unify memo (ID 列 → 'UExp')。 __全 group 共有__ = 同一部分式列は
    --   同一 'UExp' heap オブジェクトに合流し、 出力も共有付き DAG になる
    --   (garch11 のような group 跨ぎ共有が下流 'compileVecIR' の identity
    --   memo で 1 回だけコンパイルされるために必須)。
    --   [English]: The unify memo (ID list -> 'UExp').
    --   __Shared across all groups__: identical subexpression sequences
    --   merge into the same 'UExp' heap object, so the output is also a
    --   shared DAG (this is required so that cross-group sharing, e.g. in
    --   garch11, is compiled only once by the identity memo in the
    --   downstream 'compileVecIR').
  }

newSDag :: IO (IORef SDagSt)
newSDag = newIORef (SDagSt IM.empty Map.empty IM.empty Map.empty
                            IM.empty Map.empty IM.empty IM.empty 0 Map.empty)

sdagNodeOf :: SDagSt -> Int -> SNode
sdagNodeOf st i = sdNodes st IM.! i

sdagShapeOf :: SDagSt -> Int -> Int
sdagShapeOf st i = sdShape st IM.! i

sdagNamedOf :: SDagSt -> Int -> Int
sdagNamedOf st i = sdNamed st IM.! i

sdagVarsOf :: SDagSt -> Int -> Set Text
sdagVarsOf st i = sdVars st IM.! i

-- | [日本語]: 'SExp' を DAG に intern して ID を返す。 各 heap ノードの訪問は
--   1 回 (StableName memo)・poison ('symPoison' 等の error thunk) はここで
--   顕在化する ('synthVecIR' の try が捕捉する範囲内で呼ぶこと)。
--   [English]: Interns an 'SExp' into the DAG and returns its ID. Each heap
--   node is visited exactly once (via the StableName memo); a poison (an
--   error thunk such as 'symPoison') surfaces here (call this only within
--   the scope that 'synthVecIR''s try catches).
internS :: IORef SDagSt -> SExp -> IO Int
internS ref = go
  where
    go e0 = do
      e  <- evaluate e0
      sn <- makeStableName e
      let h = hashStableName sn
      st <- readIORef ref
      case lookup sn =<< IM.lookup h (sdStable st) of
        Just i  -> pure i
        Nothing -> do
          nd <- case e of
            SC v     -> pure (NC v)
            SV n     -> pure (NV n)
            S1 o a   -> N1 o <$> go a
            S2 o a b -> N2 o <$> go a <*> go b
          st1 <- readIORef ref
          i <- case Map.lookup nd (sdStruct st1) of
            Just j  -> pure j
            Nothing -> do
              let j     = sdNext st1
                  shKey = case nd of
                    NC _     -> KC
                    NV _     -> KV
                    N1 o a   -> K1 o (sdShape st1 IM.! a)
                    N2 o a b -> K2 o (sdShape st1 IM.! a) (sdShape st1 IM.! b)
                  (shId, shapes') = case Map.lookup shKey (sdShapes st1) of
                    Just s  -> (s, sdShapes st1)
                    Nothing -> let s = Map.size (sdShapes st1)
                               in (s, Map.insert shKey s (sdShapes st1))
                  nmKey = case nd of
                    NC _     -> MC
                    NV n     -> MV n
                    N1 o a   -> M1 o (sdNamed st1 IM.! a)
                    N2 o a b -> M2 o (sdNamed st1 IM.! a) (sdNamed st1 IM.! b)
                  (nmId, nameds') = case Map.lookup nmKey (sdNamedKs st1) of
                    Just s  -> (s, sdNamedKs st1)
                    Nothing -> let s = Map.size (sdNamedKs st1)
                               in (s, Map.insert nmKey s (sdNamedKs st1))
                  vs = case nd of
                    NC _     -> Set.empty
                    NV n     -> Set.singleton n
                    N1 _ a   -> sdVars st1 IM.! a
                    N2 _ a b -> (sdVars st1 IM.! a) `Set.union` (sdVars st1 IM.! b)
              writeIORef ref st1
                { sdStruct = Map.insert nd j (sdStruct st1)
                , sdNodes  = IM.insert j nd (sdNodes st1)
                , sdShapes = shapes'
                , sdShape  = IM.insert j shId (sdShape st1)
                , sdNamedKs = nameds'
                , sdNamed  = IM.insert j nmId (sdNamed st1)
                , sdVars   = IM.insert j vs (sdVars st1)
                , sdNext   = j + 1 }
              pure j
          modifyIORef' ref $ \s ->
            s { sdStable = IM.insertWith (++) h [(sn, i)] (sdStable s) }
          pure i

-- | [日本語]: 'unifyMany' の DAG 版: 行ごとの ID で lockstep 再帰し、 位置
--   (= ID 列) ごとに結果 'UExp' を memo する。 同一 ID 列は同一 'UExp' オブジェクトに
--   合流するので出力も共有付き DAG (leaf 判定・失敗条件は 'unifyMany' と同一)。
--   [English]: The DAG version of 'unifyMany': recurses in lockstep over
--   per-row IDs, memoizing the resulting 'UExp' by position (= the ID
--   list). Identical ID lists merge into the same 'UExp' object, so the
--   output is also a shared DAG (leaf detection and failure conditions are
--   the same as in 'unifyMany').
unifyManyD :: IORef SDagSt -> [SExp] -> IO (Maybe UExp)
unifyManyD ref es = mapM (internS ref) es >>= goIds
  where
    goIds [] = pure Nothing
    goIds is = do
      st <- readIORef ref
      case Map.lookup is (sdUnify st) of
        Just u  -> pure (Just u)
        Nothing -> do
          mu <- case map (sdagNodeOf st) is of
            nds@(NC _ : _) -> pure $ do
              vs <- mapM (\n -> case n of NC v -> Just v; _ -> Nothing) nds
              Just $ case vs of
                (v : rest) | all (== v) rest -> UK v
                _                            -> UC (VS.fromList vs)
            nds@(NV _ : _) -> pure $ do
              ns <- mapM (\n -> case n of NV nm -> Just nm; _ -> Nothing) nds
              Just $ case ns of
                (n0 : rest) | all (== n0) rest -> UV n0
                _ ->
                  let mems = Set.toAscList (Set.fromList ns)
                      ixm  = Map.fromList (zip mems [0 :: Int ..])
                  in UG mems (VU.fromList [ ixm Map.! n | n <- ns ])
            nds@(N1 o _ : _) ->
              case mapM (\n -> case n of N1 o' c | o' == o -> Just c
                                         _                 -> Nothing) nds of
                Nothing -> pure Nothing
                Just cs -> fmap (U1 o) <$> goIds cs
            nds@(N2 o _ _ : _) ->
              case mapM (\n -> case n of N2 o' a b | o' == o -> Just (a, b)
                                         _                   -> Nothing) nds of
                Nothing -> pure Nothing
                Just ps -> do
                  ma <- goIds (map fst ps)
                  case ma of
                    Nothing -> pure Nothing
                    Just ua -> fmap (U2 o ua) <$> goIds (map snd ps)
            [] -> pure Nothing
          case mu of
            Nothing -> pure Nothing
            Just u  -> do
              u' <- evaluate u
              modifyIORef' ref $ \s -> s { sdUnify = Map.insert is u' (sdUnify s) }
              pure (Just u')

-- | [日本語]: 'UExp' の scalar leaf 名と族 gather member リストを
--   __初出順__で収集する ('uexpScalNames'/'uexpFamilies' の共有保存版)。
--   memo (visited 集合) を IORef で外から渡し、 複数式・複数 group を跨いで
--   1 本の memo で走る = 共有部分式は 1 回だけ訪問。 収集結果を 'ordNubO' に
--   掛ける用途ではスキップされた再訪問分は重複除去されるだけなので結果は
--   木 walk と一致する。
--   [English]: Collects an 'UExp''s scalar leaf names and family-gather
--   member lists __in order of first appearance__ (the share-preserving
--   version of 'uexpScalNames'\/'uexpFamilies'). The memo (the visited
--   set) is passed in from outside via an IORef, so it runs as a single
--   memo across multiple expressions and groups — shared subexpressions
--   are visited only once. When the collected result is fed into
--   'ordNubO', skipped re-visits are simply deduplicated away, so the
--   result matches a plain tree walk.
uexpLeavesIO :: IORef (IM.IntMap [StableName UExp]) -> UExp
             -> IO ([Text], [[Text]])
uexpLeavesIO seenRef = go
  where
    go u0 = do
      u  <- evaluate u0
      sn <- makeStableName u
      let h = hashStableName sn
      seen <- readIORef seenRef
      if maybe False (elem sn) (IM.lookup h seen)
        then pure ([], [])
        else do
          modifyIORef' seenRef (IM.insertWith (++) h [sn])
          case u of
            UK _     -> pure ([], [])
            UC _     -> pure ([], [])
            UV n     -> pure ([n], [])
            UG ms _  -> pure ([], [ms])
            U1 _ e   -> go e
            U2 _ a b -> do
              (s1, f1) <- go a
              (s2, f2) <- go b
              pure (s1 ++ s2, f1 ++ f2)
            USum e   -> go e

-- | [日本語]: 'absorbPot' が 'USum' 化を試みる加算チェーンの最小項数。
--   これ未満の和はスカラ 'U2' 連鎖のまま持つ (コスト無視できる規模)。
--   [English]: The minimum number of terms in an addition chain before
--   'absorbPot' attempts to turn it into a 'USum'. Sums with fewer terms
--   than this stay as a scalar 'U2' chain (the cost is negligible at that
--   size).
potSumThreshold :: Int
potSumThreshold = 8

-- | [日本語]: raw `potential` 式を scalar 'UExp' へ吸収する。 大きな同型
--   加算チェーン (項数 ≥ 'potSumThreshold') は 'unifyManyD' でベクトル化
--   して 'USum' へ落とす (チェーン中の定数項は畳んで加算)。 吸収できない
--   構造 (unify 失敗・行依存にならない縮退 Σ 等) は Nothing = その
--   potential ごと残差 ad に残す (安全方向・値は walk と同値のまま)。
--   走査は StableName memo で共有保存 (式 DAG 化での教訓: 素朴な木 walk は
--   共有式で指数爆発する)。
--   [English]: Absorbs a raw `potential` expression into a scalar 'UExp'.
--   Large isomorphic addition chains (number of terms ≥
--   'potSumThreshold') are vectorized via 'unifyManyD' and folded down
--   into a 'USum' (constant terms in the chain are collapsed and added
--   in). Structures that can't be absorbed (unify failure, a degenerate
--   Σ that doesn't come out row-dependent, etc.) yield Nothing — that
--   potential is left on the residual AD path (the safe direction; the
--   value stays equivalent to the walk). The traversal preserves sharing
--   via a StableName memo (a lesson from the expression-DAG work: a naive
--   tree walk blows up exponentially on shared subexpressions).
absorbPot :: IORef SDagSt
          -> IORef (IM.IntMap [(StableName SExp, Maybe UExp)])
          -> SExp -> IO (Maybe UExp)
absorbPot ref memoRef = go
  where
    go e0 = do
      e  <- evaluate e0
      sn <- makeStableName e
      let h = hashStableName sn
      mm <- readIORef memoRef
      case lookup sn =<< IM.lookup h mm of
        Just r  -> pure r
        Nothing -> do
          r <- build e
          modifyIORef' memoRef (IM.insertWith (++) h [(sn, r)])
          pure r
    build e = case e of
      SC v -> pure (Just (UK v))
      SV n -> pure (Just (UV n))
      S2 SAddO _ _ -> do
        terms <- flat e []
        let (cs, ts) = foldr part (0, []) terms
            part t (c, acc) = case t of
              SC v -> (c + v, acc)
              _    -> (c, t : acc)
        if length ts >= potSumThreshold
          then do
            mu <- unifyManyD ref ts
            case mu of
              Just u | uexpIsVec u ->
                pure (Just (if cs == 0 then USum u
                            else U2 SAddO (USum u) (UK cs)))
              -- 巨大チェーンをスカラ連鎖のまま素通しすると compile 側が
              -- 肥大するため、 unify 不能なら吸収ごと断念 (残差 ad へ)。
              _ -> pure Nothing
          else bin e
      S1 o a -> fmap (U1 o) <$> go a
      S2 {}  -> bin e
    bin (S2 o a b) = do
      ma <- go a
      case ma of
        Nothing -> pure Nothing
        Just ua -> fmap (U2 o ua) <$> go b
    bin _ = pure Nothing
    -- 加算 spine の平坦化 (foldl 'sum' 由来の深い左スパイン・O(項数))。
    flat e0 acc = do
      e <- evaluate e0
      case e of
        S2 SAddO a b -> flat a =<< flat b acc
        _            -> pure (e : acc)

-- | [日本語]: per-obs 手書き scalar 'Observe' 群から「ベクトル式 IR」 を
--   __自動合成__する ('synthGaussLMBlocks' の非線形版)。 検出できない / 安全網に
--   掛かった場合は 'Nothing' (従来経路に fallback)。
--
--   安全網 2 段 ('synthGaussLMBlocks' と同じ): ① 'SExp' の Eq/Ord は非定数
--   比較で error poison → 'unsafePerformIO' + 'try' で捕捉し全体 fallback
--   (poison は 'internS' の走査中に顕在化する)。 async 例外 (timeout /
--   Ctrl-C 等) は fallback にせず__透過__する (飲み込むとハングの中断が
--   「fallback」に誤報告されることを実測確認した)。 ② IR の値評価 (観測尤度 +
--   族 prior) を probe 2 点で walk 評価 ('obsOnlySum' + 'priorOnlySum') と
--   突合し、 不一致なら fallback。
--   [English]: __Automatically synthesizes__ a "vector-expression IR" from
--   a group of hand-written per-observation scalar 'Observe' nodes (the
--   nonlinear counterpart of 'synthGaussLMBlocks'). Returns 'Nothing' if
--   nothing can be detected, or a safety net trips (falls back to the
--   previous path).
--
--   Two safety nets (same as in 'synthGaussLMBlocks'): (1) 'SExp''s Eq\/Ord
--   poisons on non-constant comparisons -> caught via 'unsafePerformIO' +
--   'try' and the whole thing falls back (the poison surfaces during
--   'internS''s traversal). Async exceptions (timeout, Ctrl-C, etc.) are
--   __passed through__ rather than turned into a fallback (measurement
--   confirmed that swallowing them misreports a hang's interruption as a
--   "fallback"). (2) The IR's value evaluation (observation likelihood +
--   family prior) is cross-checked at two probe points against the walked
--   evaluation ('obsOnlySum' + 'priorOnlySum'); a mismatch triggers
--   fallback.
synthVecIR :: ModelP r -> Maybe VecIRSrc
synthVecIR m = unsafePerformIO $ do
  r <- try (synthVecIRWalkIO m)
  case r :: Either SomeException VecIRSrc of
    Left e
      | Just (SomeAsyncException _) <- fromException e -> throwIO e
      | otherwise -> pure Nothing
    Right v@(gs, _, _)
      | null gs          -> pure Nothing
      | vecIRProbeOK m v -> pure (Just v)
      | otherwise        -> pure Nothing
{-# NOINLINE synthVecIR #-}

-- | [日本語]: 互換 wrapper (旧 pure 版と同じ表面)。 内部は 'synthVecIRWalkIO'。
--   [English]: A compatibility wrapper (same surface as the old pure
--   version). Internally delegates to 'synthVecIRWalkIO'.
synthVecIRWalk :: ModelP r -> VecIRSrc
synthVecIRWalk = unsafePerformIO . synthVecIRWalkIO
{-# NOINLINE synthVecIRWalk #-}

-- | [日本語]: 'synthVecIR' の合成部 (walk + 形状照合 + 族抽出)。 共有保存
--   DAG ('internS'/'unifyManyD') ベースで実装しており、 解析は全て ID 経由
--   O(distinct ノード数) で、 RK4 のような深い自己参照式でも指数爆発しない。
--   照合に失敗した σ グループは丸ごと残す (residual ad に fallback・安全方向)。
--   結果の式部分は構築時に正格化済み (旧実装の「呼出側が force」 は不要 —
--   共有 DAG に rnf を掛けると経路数比例で逆に爆発するため__禁止__)。
--   [English]: The synthesis part of 'synthVecIR' (walking + shape
--   matching + family extraction). Built on the sharing-preserving DAG
--   ('internS'\/'unifyManyD'), so all analysis goes through IDs in
--   O(distinct nodes), and even deeply self-referential expressions like
--   RK4 don't blow up exponentially. σ groups that fail to match are left
--   whole (falls back to residual AD — the safe direction). The
--   expression part of the result is already strict by construction (the
--   old implementation's "caller must force" is unnecessary — and
--   __forbidden__, since running rnf over the shared DAG would itself blow
--   up in proportion to the path count).
synthVecIRWalkIO :: ModelP r -> IO VecIRSrc
synthVecIRWalkIO m = do
  let (rows, priors) = collectSymRows m
  ref      <- newSDag
  leafSeen <- newIORef IM.empty
      -- 族条件: 全 member の prior が構造同一の Normal(m, τ) で、 m/τ が member
      -- 自身を参照しない (ベクトル化密度 -nG·logτ - Σ(a_j-m)²/(2τ²) が成立する形)。
      -- 構造同一判定 ('sexpEq' 相当) は intern ID の等値。
  let famOf ms = case mapM (`Map.lookup` priors) ms of
        Just ds@(Normal m0 t0 : _) -> do
          i0 <- internS ref m0
          j0 <- internS ref t0
          oks <- forM ds $ \d -> case d of
            Normal mm tt -> do
              im <- internS ref mm
              jt <- internS ref tt
              pure (im == i0 && jt == j0)
            _ -> pure False
          st <- readIORef ref
          pure $ if and oks
                    && Set.null ((sdagVarsOf st i0 `Set.union` sdagVarsOf st j0)
                                 `Set.intersection` Set.fromList ms)
                 then Just (ms, m0, t0) else Nothing
        _ -> pure Nothing
      -- IO 上の Maybe 連結 (MaybeT 相当の局所定義・unify 失敗の短絡用)。
      mIO >>=? k = mIO >>= maybe (pure Nothing) k
      okIf cond g = pure (if cond then Just g else Nothing)
      -- family 別の unify + 観測値の妥当性 (値 guard を walk と一致させるため、
      -- 観測値側の guard に掛かる行を含むグループは吸収しない = walk が -∞ を
      -- 返す縮退ケースをそのまま残す安全方向)。
      tryGroup grows = do
        let ysV = VS.fromList [ y | (_, _, y) <- grows ]
        mg <- case [ d | (_, d, _) <- grows ] of
          ds@(SDGauss{} : _) ->
            unifyManyD ref [ mu | SDGauss mu _ <- ds ] >>=? \u ->
            unifyManyD ref [ sg | SDGauss _ sg <- ds ] >>=? \sgU ->
            pure (Just (VGGauss u sgU ysV))
          ds@(SDPois{} : _) ->
            unifyManyD ref [ lam | SDPois lam <- ds ] >>=? \u ->
            okIf (VS.all (>= 0) ysV) (VGPois u ysV)
          ds@(SDBern{} : _) ->
            unifyManyD ref [ p | SDBern p <- ds ] >>=? \u ->
            okIf (VS.all (\y -> let k = round y :: Int in k == 0 || k == 1) ysV)
                 (VGBern u ysV)
          ds@(SDStudT nu _ _ : _) ->
            unifyManyD ref [ mu | SDStudT _ mu _ <- ds ] >>=? \u ->
            unifyManyD ref [ sg | SDStudT _ _ sg <- ds ] >>=? \sgU ->
            pure (Just (VGStudT nu u sgU ysV))
          ds@(SDCauchy{} : _) ->
            unifyManyD ref [ loc | SDCauchy loc _ <- ds ] >>=? \u ->
            unifyManyD ref [ sc | SDCauchy _ sc <- ds ] >>=? \scU ->
            pure (Just (VGCauchy u scU ysV))
          ds@(SDLogis{} : _) ->
            unifyManyD ref [ mu | SDLogis mu _ <- ds ] >>=? \u ->
            unifyManyD ref [ s | SDLogis _ s <- ds ] >>=? \sU ->
            pure (Just (VGLogis u sU ysV))
          ds@(SDGumbel{} : _) ->
            unifyManyD ref [ mu | SDGumbel mu _ <- ds ] >>=? \u ->
            unifyManyD ref [ be | SDGumbel _ be <- ds ] >>=? \beU ->
            pure (Just (VGGumbel u beU ysV))
          ds@(SDExpo{} : _) ->
            unifyManyD ref [ rate | SDExpo rate <- ds ] >>=? \u ->
            okIf (VS.all (>= 0) ysV) (VGExpo u ysV)
          ds@(SDWeib{} : _) ->
            unifyManyD ref [ k | SDWeib k _ <- ds ] >>=? \kU ->
            unifyManyD ref [ lam | SDWeib _ lam <- ds ] >>=? \u ->
            okIf (VS.all (> 0) ysV) (VGWeib kU u ysV)
          ds@(SDLogN{} : _) ->
            unifyManyD ref [ mu | SDLogN mu _ <- ds ] >>=? \u ->
            unifyManyD ref [ sg | SDLogN _ sg <- ds ] >>=? \sgU ->
            okIf (VS.all (> 0) ysV) (VGLogN u sgU ysV)
          ds@(SDGamma{} : _) ->
            unifyManyD ref [ sh | SDGamma sh _ <- ds ] >>=? \shU ->
            unifyManyD ref [ rt | SDGamma _ rt <- ds ] >>=? \u ->
            okIf (VS.all (> 0) ysV) (VGGamma shU u ysV)
          ds@(SDBeta{} : _) ->
            unifyManyD ref [ al | SDBeta al _ <- ds ] >>=? \alU ->
            unifyManyD ref [ be | SDBeta _ be <- ds ] >>=? \u ->
            okIf (VS.all (\y -> y > 0 && y < 1) ysV) (VGBeta alU u ysV)
          ds@(SDBinom{} : _) ->
            unifyManyD ref [ p | SDBinom _ p <- ds ] >>=? \u ->
            let nsV = VS.fromList [ fromIntegral n | SDBinom n _ <- ds ]
                -- Phase 94: n を行対応化したので、 各行を自分の n で域内判定
                -- (旧: 先頭行の n を全行に流用 = merge 前提が単一 n だった)。
                domOk = and [ let k = round y :: Int in k >= 0 && k <= round nn
                            | (nn, y) <- zip (VS.toList nsV) (VS.toList ysV) ]
            in okIf domOk (VGBinom nsV u ysV)
          ds@(SDGeom{} : _) ->
            unifyManyD ref [ p | SDGeom p <- ds ] >>=? \u ->
            okIf (VS.all (\y -> (round y :: Int) >= 1) ysV) (VGGeom u ysV)
          ds@(SDNegBin{} : _) ->
            unifyManyD ref [ mu | SDNegBin mu _ <- ds ] >>=? \u ->
            unifyManyD ref [ al | SDNegBin _ al <- ds ] >>=? \alU ->
            okIf (VS.all (>= 0) ysV) (VGNegBin u alU ysV)
          ds@(SDMixNorm2{} : _) ->
            unifyManyD ref [ w1 | SDMixNorm2 w1 _ _ _ _ _ <- ds ] >>=? \w1U ->
            unifyManyD ref [ w2 | SDMixNorm2 _ w2 _ _ _ _ <- ds ] >>=? \w2U ->
            unifyManyD ref [ m1 | SDMixNorm2 _ _ m1 _ _ _ <- ds ] >>=? \m1U ->
            unifyManyD ref [ s1 | SDMixNorm2 _ _ _ s1 _ _ <- ds ] >>=? \s1U ->
            unifyManyD ref [ m2 | SDMixNorm2 _ _ _ _ m2 _ <- ds ] >>=? \m2U ->
            unifyManyD ref [ s2 | SDMixNorm2 _ _ _ _ _ s2 <- ds ] >>=? \s2U ->
            pure (Just (VGMixNorm2 w1U w2U m1U s1U m2U s2U ysV))
          ds@(SDZIBinom{} : _) ->
            unifyManyD ref [ psi | SDZIBinom _ psi _ <- ds ] >>=? \psiU ->
            unifyManyD ref [ p | SDZIBinom _ _ p <- ds ] >>=? \pU ->
            let nsV = VS.fromList [ fromIntegral n | SDZIBinom n _ _ <- ds ]
                domOk = and [ let k = round y :: Int in k >= 0 && k <= round nn
                            | (nn, y) <- zip (VS.toList nsV) (VS.toList ysV) ]
            in okIf domOk (VGZIBinom nsV psiU pU ysV)
          [] -> pure Nothing
        case mg of
          Nothing -> pure Nothing
          Just g  -> do
            -- Phase 90 A5: family absorb (prior のベクトル化) は likelihood 側の
            -- vecIR 吸収と独立の最適化。 famOf に失敗した family は fams から
            -- 単に除外し (absorb しない)、 その prior は既存の `constPriorsOf`
            -- (`Gradient.hs`) 経由で扱わせる。 leaf 収集の memo (leafSeen) は
            -- group 跨ぎ共有 — スキップされた再訪問分の family は前の group が
            -- 同一 (ms, m0, τ0) を famsAll に登録済みなので結果は不変。
            famLs <- concatMap snd <$> mapM (uexpLeavesIO leafSeen) (vgExprAll g)
            famRs <- mapM famOf (ordNubO famLs)
            let fams = [ f | Just f <- famRs ]
            pure (Just (g, fams, Set.fromList [ nm | (nm, _, _) <- grows ]))
      -- Phase 55.2-56.3 のグループキー (family タグ + σ 名前付き指紋 + μ 形状)
      -- を DAG の ID で表現: 名前付き指紋 = 構造 intern ID ('sexpKeyNamed' と
      -- 同値)、 形状 = 形状クラス ID ('sexpShape' と同値)。 String 指紋は
      -- 長さが式の展開サイズ (= 経路数) 比例で指数爆発するため廃止 (A6)。
      key tag named shaped = do
        nids <- mapM (internS ref) named
        si   <- internS ref shaped
        st   <- readIORef ref
        pure (tag :: String, map (sdagNamedOf st) nids, sdagShapeOf st si)
      keyOf d = case d of
        SDGauss mu sg    -> key "g" [sg] mu
        SDPois  lam      -> key "p" [] lam
        SDBern  p        -> key "b" [] p
        SDStudT nu mu sg -> key ("t:" ++ show nu) [sg] mu
        SDCauchy loc sc  -> key "cy" [sc] loc
        SDLogis mu s     -> key "lg" [s] mu
        SDGumbel mu be   -> key "gb" [be] mu
        SDExpo rate      -> key "e" [] rate
        SDWeib k lam     -> key "w" [k] lam
        SDLogN mu sg     -> key "ln" [sg] mu
        SDGamma sh rt    -> key "ga" [sh] rt
        SDBeta al be     -> key "be" [al] be
        SDBinom _ p      -> key "bi:" [] p   -- Phase 94: n を key から除外 (行対応 Vector 化で 1 group に merge)
        SDGeom p         -> key "ge" [] p
        SDNegBin mu al   -> key "nb" [al] mu
        SDMixNorm2 w1 w2 m1 s1 m2 s2 -> key "mx2" [w1, w2, s1, m2, s2] m1
        SDZIBinom _ psi p -> key "zb:" [psi] p   -- Phase 94: n を key から除外
  rowsK <- forM rows $ \r@(_, d, _) -> (,) r <$> keyOf d
  let gkeys = ordNubO (map snd rowsK)
  cands <- concat <$> forM gkeys (\gk -> do
             mc <- tryGroup [ r | (r, gk') <- rowsK, gk' == gk ]
             pure (maybe [] (: []) mc))
  -- Phase 90 A10: raw potential の吸収。 吸収成功した potential は
  -- 'VGPot' グループ + 吸収名集合 (第3成分) に合流する。 吸収できない
  -- potential はここに現れない = 従来どおり残差 ad が担う。
  -- ★potential の gather は族の**部分集合** member list になり得る
  -- (icar の node1/node2 等) ため、 leaf family を famOf に掛けると
  -- Observe 群由来の全体族と member が重複し disjoint チェックで全体
  -- fallback してしまう。 potential 側では族 prior 吸収を行わない —
  -- 族に吸収されなかった latent の prior は @constPriorsOf@
  -- (`Gradient.hs`) が per-scalar 解析勾配で拾うので残差ゼロは保たれる。
  potMemo <- newIORef IM.empty
  potRs <- forM (collectSymPots m) $ \(nm, e) -> do
    mu <- absorbPot ref potMemo e
    pure (fmap ((,) nm) mu)
  let pots = [ p | Just p <- potRs ]
  let famsAll = Map.toList (Map.fromList
                  [ (ms, (mx, tx)) | (_, fs, _) <- cands, (ms, mx, tx) <- fs ])
      famNames = concatMap fst famsAll
      disjoint = length famNames == Set.size (Set.fromList famNames)
  evaluate $ if not disjoint
    then ([], [], Set.empty)   -- 族 member が重複 (二重計上の危険) → 全体 fallback
    else ( [ g | (g, _, _) <- cands ] ++ [ VGPot u | (_, u) <- pots ]
         , [ (ms, mx, tx) | (ms, (mx, tx)) <- famsAll ]
         , Set.unions [ obs | (_, _, obs) <- cands ]
           `Set.union` Set.fromList (map fst pots) )

-- | [日本語]: グループ中の全 'UExp' フィールド (出現順)。 従来の
--   @vgExpr1@/@vgExpr2@ (最大2フィールド限定) を、 Mixture (6フィールド) 等
--   任意個数のフィールドを持つ family にも対応できる形に一般化した
--   (呼び出し側 'compileVecIR' の scalNames/vecLists 収集は 1 パスに統合)。
--   [English]: All the 'UExp' fields in a group (in order of appearance).
--   Generalizes the old @vgExpr1@\/@vgExpr2@ (limited to 2 fields) so it
--   can also handle families with an arbitrary number of fields, such as
--   Mixture (6 fields). (The caller-side 'compileVecIR' scalNames\/vecLists
--   collection is unified into a single pass.)
vgExprAll :: VecGroupSrc -> [UExp]
vgExprAll (VGGauss u sg _)      = [u, sg]
vgExprAll (VGPois u _)          = [u]
vgExprAll (VGBern u _)          = [u]
vgExprAll (VGStudT _ u sg _)    = [u, sg]
vgExprAll (VGCauchy u sc _)     = [u, sc]
vgExprAll (VGLogis u s _)       = [u, s]
vgExprAll (VGGumbel u be _)     = [u, be]
vgExprAll (VGExpo u _)          = [u]
vgExprAll (VGWeib k u _)        = [u, k]
vgExprAll (VGLogN u sg _)       = [u, sg]
vgExprAll (VGGamma sh u _)      = [u, sh]
vgExprAll (VGBeta al u _)       = [u, al]
vgExprAll (VGBinom _ u _)       = [u]
vgExprAll (VGGeom u _)          = [u]
vgExprAll (VGNegBin u al _)     = [u, al]
vgExprAll (VGMixNorm2 w1 w2 m1 s1 m2 s2 _) = [w1, w2, m1, s1, m2, s2]
vgExprAll (VGZIBinom _ psi p _) = [psi, p]
vgExprAll (VGPot u)             = [u]

-- | [日本語]: グループ中の族 gather member リスト (全 'UExp' フィールドから)。
--   [English]: The family-gather member lists in a group (gathered from
--   all its 'UExp' fields).
vgFamilies :: VecGroupSrc -> [[Text]]
vgFamilies = concatMap uexpFamilies . vgExprAll

-- | [日本語]: 名前が @sel@ に含まれる raw 'Potential' の値__だけ__を足す walk
--   ('obsOnlySum' の potential 版・probe 用)。
--   [English]: A walk that sums __only__ the values of raw 'Potential'
--   nodes whose name is in @sel@ (the potential counterpart of
--   'obsOnlySum', used for probing).
potOnlySum :: Set Text -> Model Double r -> Map Text Double -> Double
potOnlySum sel model params = go model 0
  where
    go (Pure _) acc = acc
    go (Free (Sample n _ k)) acc = go (k (Map.findWithDefault 0 n params)) acc
    go (Free (Observe _ _ _ next)) acc = go next acc
    go (Free (ObserveLM _ _ _ _ _ _ next)) acc = go next acc
    go (Free (Potential n v next)) acc
      | n `Set.member` sel = go next (acc + v)
      | otherwise          = go next acc
    go (Free (Deterministic _ v k)) acc = go (k v) acc
    go (Free (Data _ ys k)) acc = go (k (ys, ys)) acc
    go (Free (DataIx _ is k)) acc = go (k is) acc
    go (Free (PlateBegin _ _ next)) acc = go next acc
    go (Free (PlateEnd next)) acc = go next acc

-- | [日本語]: 名前が @sel@ に含まれる 'Sample' の prior log-density
--   __だけ__を足す walk ('obsOnlySum' の prior 版・probe 用)。
--   [English]: A walk that sums __only__ the prior log-density of
--   'Sample' nodes whose name is in @sel@ (the prior counterpart of
--   'obsOnlySum', used for probing).
priorOnlySum :: Set Text -> Model Double r -> Map Text Double -> Double
priorOnlySum sel model params = go model 0
  where
    go (Pure _) acc = acc
    go (Free (Sample n d k)) acc =
      let v = Map.findWithDefault 0 n params
      in go (k v) (if n `Set.member` sel then acc + logDensity d v else acc)
    go (Free (Observe _ _ _ next)) acc = go next acc
    go (Free (ObserveLM _ _ _ _ _ _ next)) acc = go next acc
    go (Free (Potential _ _ next)) acc = go next acc
    go (Free (Deterministic _ v k)) acc = go (k v) acc
    go (Free (Data _ ys k)) acc = go (k (ys, ys)) acc
    go (Free (DataIx _ is k)) acc = go (k is) acc
    go (Free (PlateBegin _ _ next)) acc = go next acc
    go (Free (PlateEnd next)) acc = go next acc

-- | [日本語]: 安全網②: IR の値 (観測尤度 + 族 prior) を、 元 model の
--   walk 評価と probe 2 点で突合する ('synthProbeOK' と同じ流儀。
--   probe 値は per-param に変えて係数取り違えも検出・全 latent 正値で guard 安全)。
--   [English]: Safety net (2): cross-checks the IR's value (observation
--   likelihood + family prior) against the original model's walked
--   evaluation at two probe points (the same approach as
--   'synthProbeOK'; probe values vary per-parameter so a mixed-up
--   coefficient can also be caught, and are kept positive for every
--   latent so any domain guard stays safe).
vecIRProbeOK :: ModelP r -> VecIRSrc -> Bool
vecIRProbeOK m (gs, fams, obsNames) = all check [(0.5, 0.07), (1.3, 0.11)]
  where
    names  = sampleNames m
    -- 各 latent の prior 分布 → 制約変換種別。 probe 点を **その latent の台**
    -- (正値 / 単位区間) に写すため。 素の base+step は有界台の latent
    -- (Beta 等) で域外になり sqrt(1-pc²) 等が NaN 化 → 誤 fallback していた
    -- (Phase 80.2)。 'fromUnconstrained' で恒等 / exp / sigmoid を通し常に域内。
    --
    -- Phase 90 A3: @base + step·i@ の @i@ は全 latent 通し番号なので、
    -- 個体ごとの random effect 等で latent 数が多い階層モデル (M=385 等) では
    -- 添字の大きい latent の probe 値が発散する (例: i=387 → 27.6)。
    -- unconstrained (Normal 等・変換なし) latent は exp/log の非線形演算を
    -- 経由すると `exp(-eps)` アンダーフロー → `log(1-p) = -Infinity` →
    -- `ref - syn = -Inf - (-Inf) = NaN` で誤って probe 不一致 (05-mh で実測
    -- 発覚)。 通し番号を法 16 で折り返し、 添字が多くても probe 値の広がりを
    -- 一定に保つ (元の「係数取り違え検出のため異なる値を使う」意図は
    -- 16 通りの相異なる値で十分に保たれる)。
    (_, priors) = collectSymRows m
    trOf n = maybe UnconstrainedT distToTransform (Map.lookup n priors)
    ixOf   = Map.fromList (zip names [0 ..])
    cvi    = compileVecIR ixOf gs fams
    famSet = Set.fromList (concat [ ms | (ms, _, _) <- fams ])
    check (base, step) =
      let pm = Map.fromList
                 [ (n, fromUnconstrained (trOf n) (base + step * fromIntegral (i `mod` 16)))
                 | (n, i) <- zip names [0 :: Int ..] ]
          pc = VS.fromList [ pm Map.! n | n <- names ]
          ref = obsOnlySum obsNames m pm + priorOnlySum famSet m pm
                + potOnlySum obsNames m pm   -- 吸収済み raw potential (A10)
          syn = vecIRValue cvi pc
      in abs (ref - syn) <= 1e-9 * (1 + abs ref)

-- | [日本語]: index 解決済みのベクトル式 IR ノード。 latent 参照は leaf
--   __位置__ ('cvScalIx' \/ 'cvVecIxs' の添字) に解決済み (per-call の Text
--   lookup なし)。
--   [English]: An index-resolved vector-expression IR node. Latent
--   references are already resolved to leaf __positions__ (indices into
--   'cvScalIx' \/ 'cvVecIxs'), so there is no per-call Text lookup.
data RUExp
  = RUK !Double
  | RUC !(VS.Vector Double)
  | RUV !Int                    -- ^ [日本語]: scalar leaf 位置。 [English]: A scalar-leaf position.
  | RUG !Int !(VU.Vector Int)   -- ^ [日本語]: vector leaf 位置 + gids (gather・長さ = 行数)。 [English]: A vector-leaf position plus gids (a gather, length = number of rows).
  | RUVec !Int                  -- ^ [日本語]: vector leaf そのもの (族 prior 用)。 [English]: The vector leaf itself (used for family priors).
  | RU1 !SUn RUExp
  | RU2 !SBin RUExp RUExp
  | RUSum RUExp                 -- ^ [日本語]: Σ (ベクトル → スカラ)。 [English]: A Σ (vector -> scalar).
  deriving (Eq, Ord)            -- ^ [日本語]: compile 時 hash-consing (CSE) 用。 [English]: Used for hash-consing (CSE) at compile time.

-- | [日本語]: 式が行依存 (ベクトル形) か (compile 時に静的に決まる)。
--   素朴な構造再帰は共有 DAG 上で__経路数__に比例して走り、 garch11 の σ
--   逐次再帰 (sPrev² = 2 参照 × T 段 → Σ2^t 経路) で指数ハングした
--   (prof 99.8% time・entries 2^30)。 StableName memo walk で
--   O(distinct)/呼出に是正 ('compileVecIR' と同流儀・引数決定的なので参照透過)。
--   [English]: Whether an expression is row-dependent (vector-shaped);
--   this is decided statically at compile time. A naive structural
--   recursion runs in time proportional to the __path count__ over the
--   shared DAG, and hung exponentially on garch11's sequential σ
--   recursion (sPrev² referenced twice per step over T steps -> Σ2^t
--   paths; profiling showed 99.8% of time, 2^30 entries). Fixed to
--   O(distinct nodes) per call with a StableName memo walk (the same
--   approach as 'compileVecIR'; referentially transparent since the
--   arguments are deterministic).
ruIsVec :: RUExp -> Bool
ruIsVec e0 = unsafePerformIO $ do
  memo <- newIORef IM.empty
  let go x0 = do
        x  <- evaluate x0
        sn <- makeStableName x
        let h = hashStableName sn
        mm <- readIORef memo
        case lookup sn =<< IM.lookup h mm of
          Just r  -> pure r
          Nothing -> do
            r <- case x of
              RUC{}     -> pure True
              RUG{}     -> pure True
              RUVec{}   -> pure True
              RU1 _ e   -> go e
              RU2 _ a b -> (||) <$> go a <*> go b
              _         -> pure False
            modifyIORef' memo (IM.insertWith (++) h [(sn, r)])
            pure r
  go e0
{-# NOINLINE ruIsVec #-}

infixl 6 .+#, .-#
infixl 7 .*#, ./#
-- | [日本語]: 密度 IR 構築用の局所演算子 (export しない)。 恒等演算
--   (x·1 / x+0 / x-0 / x÷1) は構築時に畳む = 'ru2Smart'。 μ 合成
--   (designHBMProgram) が汎用に作る @0 + coef·x@ 連鎖が radon で 919 セル級
--   ベクトル命令 20 本中 6 本 (~29%) を占めると命令列 dump のプロファイルで
--   実測されたため。 x·0→0 は IEEE 非保存 (x=Inf/NaN で NaN) ゆえ畳まない。
--   [English]: Local operators for building the density IR (not
--   exported). Identity operations (x·1 \/ x+0 \/ x-0 \/ x÷1) are folded at
--   construction time via 'ru2Smart'. This is because a profiler
--   instruction-stream dump measured that the @0 + coef·x@ chains
--   generically produced by μ composition (designHBMProgram) accounted for
--   6 of 20 (~29%) vector instructions at radon's ~919-cell scale. x·0→0
--   is not folded, since it isn't IEEE-safe (x=Inf\/NaN gives NaN).
(.+#), (.-#), (.*#), (./#) :: RUExp -> RUExp -> RUExp
(.+#) = ru2Smart SAddO
(.-#) = ru2Smart SSubO
(.*#) = ru2Smart SMulO
(./#) = ru2Smart SDivO

-- | [日本語]: 'RU2' の恒等演算畳み込み smart constructor。 定数同士は
--   即値化 (SExp の 'sc2' と同じ流儀)。
--   [English]: The identity-folding smart constructor for 'RU2'. Two
--   constants are folded into a literal value immediately (the same
--   approach as SExp's 'sc2').
ru2Smart :: SBin -> RUExp -> RUExp -> RUExp
ru2Smart o (RUK a) (RUK b) = RUK (sBinF o a b)
ru2Smart SAddO (RUK 0) b = b
ru2Smart SAddO a (RUK 0) = a
ru2Smart SSubO a (RUK 0) = a
ru2Smart SMulO (RUK 1) b = b
ru2Smart SMulO a (RUK 1) = a
ru2Smart SDivO a (RUK 1) = a
ru2Smart o a b = RU2 o a b

-- | [日本語]: 'RU1' の smart constructor (命令融合)。
--
--   - 定数は即値化 ('ru2Smart' と同流儀)。
--   - __@log(exp x) → x@ の代数畳み込み__: GLM log-link で観測密度が
--     @Σ y·log(λ)@・@λ = exp(η)@ を組むと @log(exp η)@ の往復が 1 命令
--     (観測長ぶんの `SLogO` pass + その backward) として残る。 これを恒等に
--     畳んで η を直接使う (`Σ y·η − exp η` = Poisson の log-link 標準形)。
--     数学的に厳密な恒等 (exp は常に正・log(exp x)=x)。 FP では ulp 差が出る
--     ため __draws は変わる__ (回帰判定は PyMC 事後突合による別 gate)。
--   ⚠ @exp(log x) → x@ は x>0 でしか成立せず (log(負)=NaN) 一般には不正 =
--     畳まない。 log∘exp のみ。
--
--   [English]: The smart constructor for 'RU1' (instruction fusion).
--
--   - Constants are folded into a literal value immediately (same approach
--     as 'ru2Smart').
--   - __Algebraic folding of @log(exp x) → x@__: when a GLM log-link
--     observation density builds @Σ y·log(λ)@ with @λ = exp(η)@, the
--     round trip @log(exp η)@ remains as one instruction (an `SLogO` pass
--     over the observation length, plus its backward pass). This folds
--     that away as an identity, using η directly (giving
--     `Σ y·η − exp η`, the standard Poisson log-link form). Mathematically
--     an exact identity (exp is always positive, so log(exp x)=x), but
--     floating point produces ulp-level differences, so
--     __draws do change__ (regression checking is a separate gate, done
--     by comparing posteriors against PyMC).
--   ⚠ @exp(log x) → x@ only holds for x>0 (log of a negative is NaN), so
--     it is not valid in general and is not folded. Only log∘exp is.
ru1Smart :: SUn -> RUExp -> RUExp
ru1Smart o (RUK a)          = RUK (sUnF o a)
ru1Smart SLogO (RU1 SExpO x) = x
ru1Smart o e                = RU1 o e

-- | [日本語]: 前処理済みの IR (@CompiledLMBlock@ の IR 版)。 compile 時に
--   1 度だけ作り、 per-call は leaf 値の差し替え + tape/値評価のみ。
--   index 解決 + 観測値由来の定数前計算済みのグループ。
--   [English]: A pre-processed IR (the IR counterpart of
--   @CompiledLMBlock@). Built once at compile time; each call only swaps
--   in leaf values and evaluates the tape\/value. A group with indices
--   already resolved and observation-derived constants already
--   precomputed.
data VecObsIR
  = VOGauss !RUExp !RUExp !(VS.Vector Double)
    -- ^ [日本語]: (μ IR, σ IR, ys)。 σ IR が行依存 (RUC を含む) なら
    --   heteroscedastic ベクトル密度。
    --   [English]: (μ IR, σ IR, ys). If σ IR is row-dependent (contains
    --   RUC), this is a heteroscedastic vector density.
  | VOPois !RUExp !(VS.Vector Double) !Double
    -- ^ [日本語]: (λ IR, ys, Σ log y_i! 前計算)。
    --   logp = Σ(y_i·logλ_i - λ_i) - Σlog y_i! (y は定数なので factorial 項は
    --   compile 時前計算・勾配に寄与しない)。
    --   [English]: (λ IR, ys, precomputed Σ log y_i!). logp =
    --   Σ(y_i·logλ_i - λ_i) - Σlog y_i! (since y is a constant, the
    --   factorial term is precomputed at compile time and contributes
    --   nothing to the gradient).
  | VOBern !RUExp !(VS.Vector Double)
    -- ^ [日本語]: (p IR, yb)。 yb = round 済 0/1 列。
    --   logp = Σ(yb_i·log p_i + (1-yb_i)·log(1-p_i))
    --   ('logDensityObs' の round 分岐を係数化)。
    --   [English]: (p IR, yb). yb is the already-rounded 0\/1 column.
    --   logp = Σ(yb_i·log p_i + (1-yb_i)·log(1-p_i)) (turns
    --   'logDensityObs''s round branch into coefficients).
  | VOStudT !Double !RUExp !RUExp !(VS.Vector Double)
    -- ^ [日本語]: (ν 定数, μ IR, σ IR, ys)。 lgamma 項は ν=SC なので
    --   compile 時定数 ('lgammaApprox' で walk と完全一致)。
    --   [English]: (a constant ν, μ IR, σ IR, ys). The lgamma term is a
    --   compile-time constant since ν=SC (matches the walk exactly via
    --   'lgammaApprox').
  | VOCauchy !RUExp !RUExp !(VS.Vector Double)
    -- ^ [日本語]: (x₀ IR, γ IR, ys)。 logp = -n·logπ - Σlogγ - Σlog(1+z_i²)。
    --   [English]: (x₀ IR, γ IR, ys). logp = -n·logπ - Σlogγ - Σlog(1+z_i²).
  | VOLogis !RUExp !RUExp !(VS.Vector Double)
    -- ^ [日本語]: (μ IR, s IR, ys)。 logp = -Σz_i - Σlog s - 2·Σlog(1+exp(-z_i))。
    --   [English]: (μ IR, s IR, ys). logp = -Σz_i - Σlog s - 2·Σlog(1+exp(-z_i)).
  | VOGumbel !RUExp !RUExp !(VS.Vector Double)
    -- ^ [日本語]: (μ IR, β IR, ys)。 logp = -Σlog β - Σz_i - Σexp(-z_i)。
    --   [English]: (μ IR, β IR, ys). logp = -Σlog β - Σz_i - Σexp(-z_i).
  | VOExpo !RUExp !(VS.Vector Double)
    -- ^ [日本語]: (rate IR, ys)。 logp = Σlog rate_i - Σ rate_i·y_i。
    --   [English]: (rate IR, ys). logp = Σlog rate_i - Σ rate_i·y_i.
  | VOWeib !RUExp !RUExp !(VS.Vector Double)
    -- ^ [日本語]: (k IR, λ IR, log ys 前計算)。 (y/λ)^k は
    --   exp(k·(log y - log λ)) で初等化 (@**@ と ulp 差のみ)。
    --   [English]: (k IR, λ IR, precomputed log ys). (y/λ)^k is reduced to
    --   exp(k·(log y - log λ)) (differs from @**@ only at the ulp level).
  | VOLogN !RUExp !RUExp !(VS.Vector Double) !Double
    -- ^ [日本語]: (μ IR, σ IR, log ys 前計算, -Σlog y 定数)。 密度 =
    --   Gaussian ノード ('VOGauss' の densityIR) 再利用 + 定数。
    --   [English]: (μ IR, σ IR, precomputed log ys, the constant -Σlog y).
    --   The density reuses the Gaussian node ('VOGauss''s densityIR) plus
    --   a constant.
  | VOGamma !RUExp !RUExp !(VS.Vector Double) !(VS.Vector Double)
    -- ^ [日本語]: (α IR, rate IR, ys, log ys 前計算)。 lgammaΓ(α) は
    --   @SLgammaO@ (値 lgammaApprox / 導関数 lgammaApproxDeriv)。
    --   [English]: (α IR, rate IR, ys, precomputed log ys). lgammaΓ(α) is
    --   @SLgammaO@ (value via lgammaApprox, derivative via
    --   lgammaApproxDeriv).
  | VOBeta !RUExp !RUExp !(VS.Vector Double) !(VS.Vector Double)
    -- ^ [日本語]: (α IR, β IR, log ys, log (1-ys) 前計算)。
    --   [English]: (α IR, β IR, precomputed log ys, precomputed log (1-ys)).
  | VOBinom !RUExp !(VS.Vector Double) !(VS.Vector Double) !Double
    -- ^ [日本語]: (p IR, k 列 (raw y・walk の kA と一致), n-k 列,
    --   Σ logC(n,round y) 定数)。 Bernoulli 式の係数一般化。
    --   [English]: (p IR, a k column (raw y, matching the walk's kA),
    --   an n-k column, the constant Σ logC(n,round y)). A coefficient
    --   generalization of the Bernoulli form.
  | VOGeom !RUExp !(VS.Vector Double)
    -- ^ [日本語]: (p IR, k 列 (raw y))。
    --   logp = Σ(k_i-1)·log(1-p_i) + Σlog p_i。
    --   [English]: (p IR, a k column (raw y)).
    --   logp = Σ(k_i-1)·log(1-p_i) + Σlog p_i.
  | VONegBin !RUExp !RUExp !(VS.Vector Double) !Double
    -- ^ [日本語]: (μ IR, α IR, k 列 (raw y), Σ lgammaΓ(k_i+1) 定数)。
    --   lgammaΓ(k_i+α) は @SLgammaO@ の elementwise 適用。
    --   [English]: (μ IR, α IR, a k column (raw y), the constant
    --   Σ lgammaΓ(k_i+1)). lgammaΓ(k_i+α) is an elementwise application of
    --   @SLgammaO@.
  | VOPot !RUExp
    -- ^ [日本語]: raw `potential` 項。 scalar 形 (内部 Σ は 'RUSum')。
    --   logp 寄与 = 式の値そのもの・guard なし (walk の 'Potential' 加算と
    --   同値)。
    --   [English]: A raw `potential` term. Scalar-shaped (any internal Σ
    --   is an 'RUSum'). Its contribution to logp is the expression's
    --   value itself, with no guard (equivalent to the 'Potential'
    --   addition performed by the walk).
  | VOMixNorm2 !RUExp !RUExp !RUExp !RUExp !RUExp !RUExp !(VS.Vector Double)
    -- ^ [日本語]: (w1 IR, w2 IR, μ1 IR, σ1 IR, μ2 IR, σ2 IR, ys)。
    --   2成分 Normal 混合限定。 logp_i = logsumexp(logw1-logtotal+lpdf1_i,
    --   logw2-logtotal+lpdf2_i) ('Distribution.hs' の Mixture と数式一致)。
    --   [English]: (w1 IR, w2 IR, μ1 IR, σ1 IR, μ2 IR, σ2 IR, ys).
    --   Two-component Normal mixtures only. logp_i =
    --   logsumexp(logw1-logtotal+lpdf1_i, logw2-logtotal+lpdf2_i) (matches
    --   the formula for Mixture in 'Distribution.hs').
  | VOZIBinom !(VS.Vector Double) !RUExp !RUExp !(VS.Vector Double) !(VS.Vector Double)
              !(VS.Vector Double) !(VS.Vector Double)
    -- ^ [日本語]: (n 列 (行対応), ψ IR, p IR, mask0 列 (y=0なら1), y 列 (raw),
    --   n-y 列, logC(n,y) 列)。 y==0/y>0 の分岐は compile 時に mask 列へ
    --   落とし、 両方の分岐式を全行 elementwise に計算してから mask で選択
    --   (group 分割はしない — family gather の disjoint 検査を壊さない
    --   安全設計)。
    --   [English]: (an n column (per row), ψ IR, p IR, a mask0 column (1
    --   if y=0), a y column (raw), an n-y column, a logC(n,y) column).
    --   The y==0\/y>0 branch is lowered to a mask column at compile time:
    --   both branch expressions are computed elementwise for every row,
    --   then selected via the mask (no group splitting — a safety design
    --   that avoids breaking the family-gather disjointness check).

data CompiledVecIR = CompiledVecIR
  { cvProg   :: !VecProgram
    -- ^ [日本語]: 値 + 勾配の静的命令列 (compile 時 1 回生成 = per-call の
    --   tape 構築を撤去し「tape を compile 時に固定」)。
    --   [English]: The static value+gradient instruction stream (generated
    --   once at compile time — removes per-call tape construction by
    --   "fixing the tape at compile time").
  , cvScalIx :: !(VU.Vector Int)
    -- ^ [日本語]: scalar leaf → param index。 [English]: scalar leaf -> param index.
  , cvVecIxs :: ![VU.Vector Int]
    -- ^ [日本語]: vector leaf → member param indices。 [English]: vector leaf -> member param indices.
  }


-- | [日本語]: 'VecIRSrc' を param index に解決する (静的・1 回)。
--   'UExp'\/'SExp' → 'RUExp' の変換と leaf 収集を identity memo
--   (StableName) で共有保存し、 命令列生成は intern 済み DAG ('RUNode') 上で
--   行う (旧実装は全て共有無視の木 walk + 'Map' 構造キーの CSE = 深い共有式
--   で指数)。 表面は pure のまま ('Gradient.hs' の呼出互換・引数に対し
--   決定的なので参照透過)。
--   [English]: Resolves a 'VecIRSrc' into param indices (statically, once).
--   The 'UExp'\/'SExp' -> 'RUExp' conversion and leaf collection preserve
--   sharing via an identity memo (StableName), and instruction-stream
--   generation runs over the already-interned DAG ('RUNode') (the old
--   implementation used a share-ignoring tree walk plus 'Map'-structural-key
--   CSE throughout, which was exponential on deeply shared expressions).
--   The surface stays pure (compatible with how 'Gradient.hs' calls it;
--   referentially transparent since it's deterministic in its arguments).
compileVecIR
  :: Map Text Int
  -> [VecGroupSrc] -> [([Text], SExp, SExp)]
  -> CompiledVecIR
compileVecIR ixOf gs fams = unsafePerformIO (compileVecIRIO ixOf gs fams)
{-# NOINLINE compileVecIR #-}

compileVecIRIO
  :: Map Text Int
  -> [VecGroupSrc] -> [([Text], SExp, SExp)]
  -> IO CompiledVecIR
compileVecIRIO ixOf gs fams = do
  -- leaf 収集 (初出順・memo は全式共有 = ordNubO 後の結果は旧木 walk と同一)
  seenRef   <- newIORef IM.empty
  leafPairs <- mapM (uexpLeavesIO seenRef) [ e | g <- gs, e <- vgExprAll g ]
  sdag <- newSDag
  famVars <- forM fams $ \(_, mx, tx) -> do
    im <- internS sdag mx
    it <- internS sdag tx
    st <- readIORef sdag
    pure (Set.toList (sdagVarsOf st im `Set.union` sdagVarsOf st it))
  let scalNames = ordNubO (concatMap fst leafPairs ++ concat famVars)
      vecLists  = ordNubO (concatMap snd leafPairs
                           ++ [ ms | (ms, _, _) <- fams ])
      sPos = Map.fromList (zip scalNames [0 :: Int ..])
      vPos = Map.fromList (zip vecLists [0 :: Int ..])
  -- UExp/SExp → RUExp (identity memo で共有保存・'ru2Smart' の畳みは従来どおり)
  rUMemo <- newIORef IM.empty
  rSMemo <- newIORef IM.empty
  let rU u0 = do
        u  <- evaluate u0
        sn <- makeStableName u
        let h = hashStableName sn
        mm <- readIORef rUMemo
        case lookup sn =<< IM.lookup h mm of
          Just r  -> pure r
          Nothing -> do
            r <- case u of
              UK v       -> pure (RUK v)
              UC v       -> pure (RUC v)
              UV n       -> pure (RUV (sPos Map.! n))
              UG ms gids -> pure (RUG (vPos Map.! ms) gids)
              U1 o e     -> RU1 o <$> rU e
              U2 o a b   -> ru2Smart o <$> rU a <*> rU b
              USum e     -> RUSum <$> rU e
            r' <- evaluate r
            modifyIORef' rUMemo (IM.insertWith (++) h [(sn, r')])
            pure r'
      rS e0 = do
        e  <- evaluate e0
        sn <- makeStableName e
        let h = hashStableName sn
        mm <- readIORef rSMemo
        case lookup sn =<< IM.lookup h mm of
          Just r  -> pure r
          Nothing -> do
            r <- case e of
              SC v     -> pure (RUK v)
              SV n     -> pure (RUV (sPos Map.! n))
              S1 o x   -> RU1 o <$> rS x
              S2 o a b -> ru2Smart o <$> rS a <*> rS b
            r' <- evaluate r
            modifyIORef' rSMemo (IM.insertWith (++) h [(sn, r')])
            pure r'
      cgOf g = case g of
        VGGauss u sg ys -> VOGauss <$> rU u <*> rU sg <*> pure ys
        VGPois u ys ->
          (\r -> VOPois r ys
             (VS.sum (VS.map (logFactorial . (round :: Double -> Int)) ys)))
          <$> rU u
        VGBern u ys ->
          (\r -> VOBern r (VS.map (\y -> fromIntegral (round y :: Int)) ys))
          <$> rU u
        VGStudT nu u sg ys -> VOStudT nu <$> rU u <*> rU sg <*> pure ys
        VGCauchy u sc ys -> VOCauchy <$> rU u <*> rU sc <*> pure ys
        VGLogis u s ys -> VOLogis <$> rU u <*> rU s <*> pure ys
        VGGumbel u be ys -> VOGumbel <$> rU u <*> rU be <*> pure ys
        VGExpo u ys -> (\r -> VOExpo r ys) <$> rU u
        VGWeib k u ys ->
          (\rk r -> VOWeib rk r (VS.map log ys)) <$> rU k <*> rU u
        VGLogN u sg ys ->
          let lys = VS.map log ys
          in (\r rsg -> VOLogN r rsg lys (negate (VS.sum lys)))
             <$> rU u <*> rU sg
        VGGamma sh u ys ->
          (\rsh r -> VOGamma rsh r ys (VS.map log ys)) <$> rU sh <*> rU u
        VGBeta al u ys ->
          (\ral r -> VOBeta ral r (VS.map log ys)
                       (VS.map (\y -> log (1 - y)) ys))
          <$> rU al <*> rU u
        VGBinom nv u ys ->
          (\r -> VOBinom r ys (VS.zipWith (-) nv ys)
             (VS.sum (VS.zipWith (\nn y -> logBinomCoeff (round nn) (round y)) nv ys)))
          <$> rU u
        VGGeom u ys -> (\r -> VOGeom r ys) <$> rU u
        VGNegBin u al ys ->
          (\r ral -> VONegBin r ral ys
             (VS.sum (VS.map (\y -> lgammaApprox (y + 1)) ys)))
          <$> rU u <*> rU al
        VGMixNorm2 w1 w2 m1 s1 m2 s2 ys ->
          (\a b c d e f -> VOMixNorm2 a b c d e f ys)
          <$> rU w1 <*> rU w2 <*> rU m1 <*> rU s1 <*> rU m2 <*> rU s2
        VGZIBinom nv psi p ys ->
          (\rpsi rp -> VOZIBinom nv rpsi rp
             (VS.map (\y -> if round y == (0 :: Int) then 1 else 0) ys)
             ys
             (VS.zipWith (-) nv ys)
             (VS.zipWith (\nn y -> logBinomCoeff (round nn) (round y)) nv ys))
          <$> rU psi <*> rU p
        VGPot u -> VOPot <$> rU u
  gd <- map densityIR <$> mapM cgOf gs
  fd <- forM fams $ \(ms, mx, tx) ->
          famDensityIR (vPos Map.! ms) (length ms) <$> rS mx <*> rS tx
  let obj  = foldl1 (RU2 SAddO) (map fst gd ++ map fst fd)
      grds = concatMap snd gd ++ concatMap snd fd
  -- RUExp → intern 済み DAG → 命令列 (構造 intern が旧 CSE cache と同じ
  -- 重複排除を構造キー比較なしで与える)
  rud  <- newRUDag
  k0   <- internRU rud (RUK 0)
  objI <- internRU rud obj
  gIs  <- forM grds $ \(k, ge) -> (,) k <$> internRU rud ge
  st   <- readIORef rud
  pure CompiledVecIR
    { cvProg   = compileVecProgramD (map length vecLists) (rudNodes st)
                                    k0 objI gIs
    , cvScalIx = VU.fromList [ ixOf Map.! n | n <- scalNames ]
    , cvVecIxs = [ VU.fromList [ ixOf Map.! n | n <- ms ] | ms <- vecLists ]
    }

-- ---------------------------------------------------------------------------
-- Phase 56.2: 観測密度の IR 式化 + 静的命令列 (記号 reverse-mode・arena 実行)
-- ---------------------------------------------------------------------------

-- | [日本語]: 値側 guard の種別 (勾配側は unguarded・従来の前例どおり)。
--   [English]: The kind of guard applied on the value side (the gradient
--   side stays unguarded, following past precedent).
data GuardKind = GPos | GUnit

-- | [日本語]: 数値安定な 2 項 log-sum-exp:
--   log(exp a + exp b) = max(a,b) + log(1+exp(-|a-b|))。 Mixture (log_mix)・
--   ZeroInflatedBinomial の x=0 分岐で使う。 'SMaxO' (勾配は winner-take-all
--   subgradient) を経由するので、 常に有限差分 (|a-b| は必ず ≥0) のみ exp
--   する = オーバーフロー安全。
--   [English]: A numerically stable pairwise log-sum-exp:
--   log(exp a + exp b) = max(a,b) + log(1+exp(-|a-b|)). Used for the
--   Mixture (log_mix) and ZeroInflatedBinomial x=0 branches. Since it goes
--   through 'SMaxO' (whose gradient is a winner-take-all subgradient), it
--   only ever exponentiates a finite difference (|a-b| is always ≥0),
--   making it overflow-safe.
logSumExp2 :: RUExp -> RUExp -> RUExp
logSumExp2 a b =
  RU2 SMaxO a b .+# RU1 SLogO (RUK 1 .+# RU1 SExpO (RU1 SNegO (RU1 SAbsO (a .-# b))))

-- | [日本語]: family 別の観測密度を IR 式として組む。 式・guard とも
--   'logDensityObs' の該当分岐と値一致 (test/probe で担保)。 旧 groupVal /
--   groupNode (手書き tape ノード) の置換 — 勾配は記号微分で自動。
--   [English]: Assembles the observation density for each family as an IR
--   expression. Both the expression and the guard match the corresponding
--   branch of 'logDensityObs' (guaranteed by tests\/probes). Replaces the
--   old groupVal\/groupNode (hand-written tape nodes) — the gradient is
--   now automatic via symbolic differentiation.
densityIR :: VecObsIR -> (RUExp, [(GuardKind, RUExp)])
densityIR g = case g of
  -- raw potential (Phase 90 A10): 式値そのまま・guard なし。
  VOPot re -> (re, [])
  -- Gaussian: -n/2·log2π - (n·logσ + Σr²/(2σ²)) (σ スカラ) /
  --           -n/2·log2π - Σlogσ_i - Σ(r_i/σ_i)²/2 (σ 行依存・55.3)
  VOGauss mu sge ys ->
    let n' = fromIntegral (VS.length ys) :: Double
        c0 = RUK (negate (0.5 * n' * log (2 * pi)))
        r  = RUC ys .-# mu
    in if ruIsVec sge
       then ( c0 .-# RUSum (RU1 SLogO sge)
                 .-# (RUSum (let t = r ./# sge in t .*# t) ./# RUK 2)
            , [(GPos, sge)] )
       else ( c0 .-# (RUK n' .*# RU1 SLogO sge)
                 .-# (RUSum (r .*# r) ./# (RUK 2 .*# sge .*# sge))
            , [(GPos, sge)] )
  -- Poisson: Σ(y_i·logλ_i - λ_i) - Σlog y_i! (lfk 前計算・y は raw = kA 一致)
  VOPois lam ys lfk ->
    let n' = fromIntegral (VS.length ys) :: Double
        -- Phase 90 A11-4② (F3b): log(exp η) を η に畳む (log-link 標準形)。
        logLam = ru1Smart SLogO lam
    in if ruIsVec lam
       then ( RUSum (RUC ys .*# logLam) .-# RUSum lam .-# RUK lfk
            , [(GPos, lam)] )
       else ( (RUK (VS.sum ys) .*# logLam)
                .-# (RUK n' .*# lam) .-# RUK lfk
            , [(GPos, lam)] )
  -- Bernoulli: Σ(yb·log p + (1-yb)·log(1-p)) (yb = round 済 0/1 定数)
  VOBern p yb ->
    let n' = fromIntegral (VS.length yb) :: Double
        c1 = VS.sum yb
        omp = RUK 1 .-# p
    in if ruIsVec p
       then ( RUSum (RUC yb .*# RU1 SLogO p)
                .+# RUSum (RUC (VS.map (1 -) yb) .*# RU1 SLogO omp)
            , [(GUnit, p)] )
       else ( (RUK c1 .*# RU1 SLogO p) .+# (RUK (n' - c1) .*# RU1 SLogO omp)
            , [(GUnit, p)] )
  -- StudentT (ν=SC・56.3): n·[lgamma((ν+1)/2) - lgamma(ν/2) - ½log(νπ)]
  --   - Σlogσ - ((ν+1)/2)·Σ log(1 + z_i²/ν)。 ν≤0 は収集時に排除済。
  VOStudT nu mu sge ys ->
    let n' = fromIntegral (VS.length ys) :: Double
        c0 = RUK (n' * (lgammaApprox ((nu + 1) / 2) - lgammaApprox (nu / 2)
                        - 0.5 * log (nu * pi)))
        z  = zOf mu sge ys
    in ( c0 .-# sumLogScale (VS.length ys) sge
            .-# (RUK ((nu + 1) / 2)
                   .*# RUSum (RU1 SLogO (RUK 1 .+# (z .*# z ./# RUK nu))))
       , [(GPos, sge)] )
  -- Cauchy (56.3): -n·logπ - Σlogγ - Σ log(1 + z_i²)
  VOCauchy loc sce ys ->
    let n' = fromIntegral (VS.length ys) :: Double
        z  = zOf loc sce ys
    in ( RUK (negate (n' * log pi)) .-# sumLogScale (VS.length ys) sce
            .-# RUSum (RU1 SLogO (RUK 1 .+# z .*# z))
       , [(GPos, sce)] )
  -- Logistic (56.3): -Σz_i - Σlog s - 2·Σ log(1 + exp(-z_i))
  VOLogis mu se ys ->
    let z = zOf mu se ys
    in ( RU1 SNegO (RUSum z) .-# sumLogScale (VS.length ys) se
            .-# (RUK 2
                   .*# RUSum (RU1 SLogO (RUK 1 .+# RU1 SExpO (RU1 SNegO z))))
       , [(GPos, se)] )
  -- Gumbel (56.3): -Σlog β - Σz_i - Σ exp(-z_i)
  VOGumbel mu bee ys ->
    let z = zOf mu bee ys
    in ( RU1 SNegO (sumLogScale (VS.length ys) bee)
            .-# RUSum z .-# RUSum (RU1 SExpO (RU1 SNegO z))
       , [(GPos, bee)] )
  -- Exponential (56.4): Σ log rate_i - Σ rate_i·y_i (y ≥ 0 は収集時確認済)
  VOExpo rate ys ->
    ( sumLogScale (VS.length ys) rate
        .-# (if ruIsVec rate then RUSum (rate .*# RUC ys)
             else rate .*# RUK (VS.sum ys))
    , [(GPos, rate)] )
  -- Weibull (56.4): Σlog k - Σlog λ + Σ(k-1)·z_i - Σ exp(k·z_i)
  -- (z_i = log y_i - log λ_i。 walk の (y/λ)**k と ulp 差のみ)
  VOWeib k lam lys ->
    let n = VS.length lys
        z = RUC lys .-# RU1 SLogO lam
    in ( sumLogScale n k .-# sumLogScale n lam
            .+# RUSum ((k .-# RUK 1) .*# z)
            .-# RUSum (RU1 SExpO (k .*# z))
       , [(GPos, k), (GPos, lam)] )
  -- LogNormal (56.4): N(log y⃗ | μ, σ) - Σlog y (Gaussian ノード再利用・guard も流用)
  VOLogN mu sge lys c ->
    let (e, gds) = densityIR (VOGauss mu sge lys)
    in (RUK c .+# e, gds)
  -- Gamma (56.4): Σ(α-1)·log y - Σ rate·y + Σ α·log rate - Σ lgammaΓ(α)
  VOGamma al rate ys lys ->
    let n = VS.length ys
    in ( RUSum ((al .-# RUK 1) .*# RUC lys)
            .-# (if ruIsVec rate then RUSum (rate .*# RUC ys)
                 else rate .*# RUK (VS.sum ys))
            .+# sumOf n (al .*# RU1 SLogO rate)
            .-# sumOf n (RU1 SLgammaO al)
       , [(GPos, al), (GPos, rate)] )
  -- Beta (56.4): Σ(α-1)·log y + Σ(β-1)·log(1-y) - Σ[lgΓα + lgΓβ - lgΓ(α+β)]
  VOBeta al be lys l1ys ->
    let n = VS.length lys
    in ( RUSum ((al .-# RUK 1) .*# RUC lys)
            .+# RUSum ((be .-# RUK 1) .*# RUC l1ys)
            .-# sumOf n (RU1 SLgammaO al .+# RU1 SLgammaO be
                           .-# RU1 SLgammaO (al .+# be))
       , [(GPos, al), (GPos, be)] )
  -- Binomial (56.5): ΣlogC + Σk_i·log p_i + Σ(n-k_i)·log(1-p_i)
  -- (Bernoulli の yb/1-yb 係数を k/n-k に一般化・logC は compile 時定数)
  VOBinom p kv nkv lc ->
    let omp = RUK 1 .-# p
    in if ruIsVec p
       then ( RUK lc .+# RUSum (RUC kv .*# RU1 SLogO p)
                .+# RUSum (RUC nkv .*# RU1 SLogO omp)
            , [(GUnit, p)] )
       else ( RUK lc .+# (RUK (VS.sum kv) .*# RU1 SLogO p)
                .+# (RUK (VS.sum nkv) .*# RU1 SLogO omp)
            , [(GUnit, p)] )
  -- Geometric (56.5): Σ(k_i-1)·log(1-p_i) + Σlog p_i (round y ≥ 1 は収集時確認済)
  VOGeom p kv ->
    let n' = fromIntegral (VS.length kv) :: Double
        omp = RUK 1 .-# p
    in if ruIsVec p
       then ( RUSum ((RUC kv .-# RUK 1) .*# RU1 SLogO omp)
                .+# RUSum (RU1 SLogO p)
            , [(GUnit, p)] )
       else ( (RUK (VS.sum kv - n') .*# RU1 SLogO omp)
                .+# (RUK n' .*# RU1 SLogO p)
            , [(GUnit, p)] )
  -- NegativeBinomial (56.5 本命): p = α/(α+μ) として
  -- Σ lgΓ(k_i+α) - Σ lgΓ(α) - Σ lgΓ(k_i+1) + Σ α·log p_i + Σ k_i·log(1-p_i)
  -- (lgΓ(k_i+α) は SLgammaO の elementwise・lgΓ(k_i+1) は compile 時定数)
  VONegBin mu al kv lgk1 ->
    let n = VS.length kv
        p = al ./# (al .+# mu)
    in ( RUSum (RU1 SLgammaO (RUC kv .+# al))
            .-# sumOf n (RU1 SLgammaO al)
            .-# RUK lgk1
            .+# sumOf n (al .*# RU1 SLogO p)
            .+# RUSum (RUC kv .*# RU1 SLogO (RUK 1 .-# p))
       , [(GPos, mu), (GPos, al)] )
  -- Mixture (Phase 90 A3・2成分 Normal 限定): 'Distribution.hs' の
  -- @logDensity (Mixture ws comps) x = logSumExpA [log(w_k/Σw)+logDensity d_k x]@
  -- と数式一致 (w1+w2=1 を仮定せず Σw で正規化)。 各成分の Gaussian 対数密度
  -- (per-row) を 'gaussLpdfElem' で作り、 'logSumExp2' で数値安定に合成。
  VOMixNorm2 w1 w2 m1 s1 m2 s2 ys ->
    let total  = w1 .+# w2
        logw1  = RU1 SLogO w1 .-# RU1 SLogO total
        logw2  = RU1 SLogO w2 .-# RU1 SLogO total
        lpdf1  = gaussLpdfElem m1 s1 ys
        lpdf2  = gaussLpdfElem m2 s2 ys
        perRow = logSumExp2 (logw1 .+# lpdf1) (logw2 .+# lpdf2)
    in ( RUSum perRow
       , [(GPos, w1), (GPos, w2), (GPos, s1), (GPos, s2)] )
  -- ZeroInflatedBinomial (Phase 90 A3): 'Distribution.hs' の
  -- @logDensity (ZeroInflatedBinomial n psi p) x@ と数式一致。 y==0/y>0 の
  -- データ分岐は group を分けず mask 列で elementwise に選択する (family
  -- gather の disjoint 検査を壊さない安全設計・A1 調査で確定した方針)。
  -- branch0 は「もしこの行が y=0 だったら」の仮想値を **行ごとの n** (nv) で
  -- 計算する (nmy=n-y_i は y=0 行以外で n と異なるため使えず、 専用の n 列 nv を
  -- 使う。 Phase 94 で n を行対応化 = n 別 group 分裂を解消)。
  VOZIBinom nv psi p mask0 yv nmy logc ->
    let omp     = RUK 1 .-# p
        ompsi   = RUK 1 .-# psi
        branch0 = logSumExp2 (RU1 SLogO psi)
                    (RU1 SLogO ompsi .+# (RUC nv .*# RU1 SLogO omp))
        branch1 = RU1 SLogO ompsi .+# RUC logc
                    .+# (RUC yv .*# RU1 SLogO p) .+# (RUC nmy .*# RU1 SLogO omp)
        perRow  = (RUC mask0 .*# branch0) .+# ((RUK 1 .-# RUC mask0) .*# branch1)
    in ( RUSum perRow, [(GUnit, psi), (GUnit, p)] )
  where
    -- 位置-尺度系の共通形 (56.3): z⃗ = (y⃗ - μ)/s (y⃗ は RUC 定数なので常にベクトル形)
    zOf mu sge ys = (RUC ys .-# mu) ./# sge
    -- Σ e: e がスカラ式なら n·e に畳む (走査回避・56.4 で一般化)
    sumOf n e
      | ruIsVec e = RUSum e
      | otherwise = RUK (fromIntegral n) .*# e
    -- Σ log s (スカラ時 n·log s・Gauss の 55.3 と同型)
    sumLogScale n sge = sumOf n (RU1 SLogO sge)
    -- Phase 90 A3 (Mixture 用): Gaussian 対数密度の **行ごとの値**
    -- (-0.5·log2π - logσ - (y-μ)²/(2σ²))。 'VOGauss' の densityIR と違い
    -- ここでは Σ を取らずベクトルのまま返す (logSumExp2 で行ごとに混合してから
    -- 最後に 1 回だけ Σ する必要があるため)。
    gaussLpdfElem mu sge ys =
      let r = RUC ys .-# mu
      in RUK (negate (0.5 * log (2 * pi))) .-# RU1 SLogO sge
             .-# ((r .*# r) ./# (RUK 2 .*# sge .*# sge))

-- | [日本語]: 族 prior 密度の IR 式: -nG/2·log2π - nG·logτ - Σ(a_j-m)²/(2τ²)。
--   [English]: The IR expression for a family prior density:
--   -nG/2·log2π - nG·logτ - Σ(a_j-m)²/(2τ²).
famDensityIR :: Int -> Int -> RUExp -> RUExp -> (RUExp, [(GuardKind, RUExp)])
famDensityIR vp nG mx tx =
  let nG' = fromIntegral nG :: Double
      c0  = RUK (negate (0.5 * nG' * log (2 * pi)))
      ra  = RUVec vp .-# mx
  in ( c0 .-# (RUK nG' .*# RU1 SLogO tx)
          .-# (RUSum (ra .*# ra) ./# (RUK 2 .*# tx .*# tx))
     , [(GPos, tx)] )

-- | [日本語]: 静的命令列の 1 命令。 slot i = 命令 i の結果 (SSA/ANF・共有保存)。
--   全 slot の形 (スカラ / 長さ n) は compile 時に確定し、 1 本の unboxed
--   arena にオフセット解決して敷き詰める (boxed 中間表現なし)。
--   [English]: A single instruction in the static instruction stream. Slot
--   i holds the result of instruction i (SSA\/ANF, sharing preserved). The
--   shape of every slot (scalar, or length n) is fixed at compile time,
--   and all slots are laid out with resolved offsets into a single
--   unboxed arena (no boxed intermediate representation).
data VInstr
  = VIK !Double                          -- ^ [日本語]: スカラ定数。 [English]: A scalar constant.
  | VIKV !(VS.Vector Double)             -- ^ [日本語]: ベクトル定数 (データ列)。 [English]: A vector constant (a data column).
  | VILeafS !Int                         -- ^ [日本語]: scalar leaf p の値。 [English]: The value of scalar leaf p.
  | VILeafV !Int                         -- ^ [日本語]: vector leaf p (member 列そのもの)。 [English]: Vector leaf p (the member column itself).
  | VIGath !Int !(VU.Vector Int) !Int    -- ^ [日本語]: gather (vector leaf p, gids, 行数)。 [English]: A gather (vector leaf p, gids, row count).
  | VIUn !SUn !Int
  | VIBin !SBin !Int !Int                -- ^ [日本語]: broadcast は形 (静的) で解決。 [English]: Broadcasting is resolved via the (static) shape.
  | VISum !Int                           -- ^ [日本語]: Σ (ベクトル → スカラ)。 [English]: A Σ (vector -> scalar).

  -- Phase 85.3-ii: superinstruction (radon 命令列 dump 由来の頻出パターンを
  -- compile 時に融合。 pass 数と中間 slot を削減 = 85.3a spike の融合利得)
  | VIAxpy !Int !Int !Int                -- ^ [日本語]: out = a + s·v (a: slot・len 0 は
                                         --   broadcast、 s: スカラ slot、 v: ベクトル slot)。
                                         --   [English]: out = a + s·v (a: a slot,
                                         --   length 0 means broadcast; s: a scalar
                                         --   slot; v: a vector slot).
  | VIAxpyC !Int !Int !(VS.Vector Double)
                                         -- ^ [日本語]: 同上・v がデータ列定数
                                         --   (VIKV copy 消滅)。
                                         --   [English]: Same as above, but v is a
                                         --   data-column constant (eliminates a
                                         --   VIKV copy).
  | VISumSqD !Int !Int                   -- ^ [日本語]: out(スカラ) = Σ (x_j − m_j)²
                                         --   (x/m: slot・スカラ側は broadcast)。
                                         --   [English]: out (scalar) = Σ (x_j − m_j)²
                                         --   (x\/m: slots; the scalar side broadcasts).
  | VISumSqC !(VS.Vector Double) !Int    -- ^ [日本語]: 同上・x がデータ列定数。
                                         --   [English]: Same as above, but x is a
                                         --   data-column constant.

  -- Phase 85.3-iv: RE 連鎖の gather 内蔵化 + 3 項融合 (radon 残 pass の削減)
  | VIMulG !Int !Int !(VU.Vector Int) !Int
                                         -- ^ [日本語]: out = s·gather(p) (s: スカラ
                                         --   slot・gather は VIGath と同形で命令内蔵
                                         --   = gather の実体化 pass 消滅)。
                                         --   [English]: out = s·gather(p) (s: a
                                         --   scalar slot; the gather has the same
                                         --   shape as VIGath but is folded into the
                                         --   instruction, eliminating the pass that
                                         --   would materialize the gather).
  | VIAxpyG !Int !Int !Int !(VU.Vector Int) !Int
                                         -- ^ [日本語]: out = a + s·gather(p)。
                                         --   [English]: out = a + s·gather(p).
  | VIMulVC !Int !Int !(VS.Vector Double)
                                         -- ^ [日本語]: out = s·v⊙c
                                         --   (スカラ×ベクトル×データ列定数)。
                                         --   [English]: out = s·v⊙c (scalar times
                                         --   vector times data-column constant).
  | VISumSqC2 !(VS.Vector Double) !Int !Int
                                         -- ^ [日本語]: out(スカラ) = Σ (c_j − m1_j − m2_j)²
                                         --   (m1/m2: ベクトル slot・和の実体化
                                         --   pass 消滅)。
                                         --   [English]: out (scalar) =
                                         --   Σ (c_j − m1_j − m2_j)² (m1\/m2: vector
                                         --   slots; eliminates the pass that would
                                         --   materialize the sum).
  | VISumSqDGG !Int !(VU.Vector Int) !Int !(VU.Vector Int) !Int
                                         -- ^ [日本語]: out(スカラ) =
                                         --   Σ (φ[px·gx_j] − φ[pm·gm_j])²。 gather 2 本を
                                         --   SumSqD に内蔵 (ICAR ペア差分・5461 セルの
                                         --   gather 実体化 2 本を消す)。 gather 値は pc
                                         --   から直読み・随伴は param へ直 scatter。
                                         --   [English]: out (scalar) =
                                         --   Σ (φ[px·gx_j] − φ[pm·gm_j])². Folds two
                                         --   gathers into SumSqD (an ICAR pairwise
                                         --   difference; eliminates the two gather
                                         --   materialization passes at a 5461-cell
                                         --   scale). Gather values are read
                                         --   directly from pc; adjoints are
                                         --   scattered directly into param.

-- | [日本語]: compile 済みの値+勾配プログラム。 生成は 1 回・per-call は
--   forward (値) / forward+backward (勾配) の実行のみ = per-call の tape
--   構築を撤去し「tape を compile 時に固定」。 arena は per-call 確保
--   (共有 mutable なし = @nutsChainsPure@ の spark 並列と整合)。
--   [English]: The compiled value+gradient program. Generated once; each
--   call only runs forward (for the value) or forward+backward (for the
--   gradient) — this removes per-call tape construction by "fixing the
--   tape at compile time". The arena is allocated per call (no shared
--   mutable state, consistent with 'nutsChainsPure''s spark-based
--   parallelism).
data VecProgram = VecProgram
  { vpInstrs  :: !(BV.Vector VInstr)
  , vpOff     :: !(VU.Vector Int)        -- ^ [日本語]: slot → arena オフセット。 [English]: slot -> arena offset.
  , vpLen     :: !(VU.Vector Int)        -- ^ [日本語]: slot → 0 (スカラ) / n (ベクトル)。 [English]: slot -> 0 (scalar) \/ n (vector).
  , vpSize    :: !Int                    -- ^ [日本語]: arena 総長。 [English]: The total arena length.
  , vpObj     :: !Int                    -- ^ [日本語]: 目的 (log-density 和) の slot。 [English]: The slot for the objective (the summed log-density).
  , vpGuards  :: ![(GuardKind, Int)]     -- ^ [日本語]: 値側 guard (slot 参照)。 [English]: Value-side guards (slot references).
  }

-- ===========================================================================
-- Phase 90 A8: 'RUExp' の DAG intern + 共有保存の命令列生成
-- ===========================================================================

-- | [日本語]: 'RUExp' の DAG ノード (子は intern 済み ID)。
--   [English]: A DAG node for 'RUExp' (children are already-interned IDs).
data RUNode
  = RNK !Double
  | RNC !(VS.Vector Double)
  | RNV !Int
  | RNG !Int !(VU.Vector Int)
  | RNVec !Int
  | RN1 !SUn !Int
  | RN2 !SBin !Int !Int
  | RNSum !Int
  deriving (Eq, Ord)

data RUDagSt = RUDagSt
  { rudStable :: !(IM.IntMap [(StableName RUExp, Int)])
  , rudStruct :: !(Map RUNode Int)
  , rudNodes  :: !(IM.IntMap RUNode)
  , rudNext   :: !Int
  }

newRUDag :: IO (IORef RUDagSt)
newRUDag = newIORef (RUDagSt IM.empty Map.empty IM.empty 0)

-- | [日本語]: 'RUExp' を DAG に intern して ID を返す ('internS' の RUExp
--   版)。 構造 intern が旧 'compileVecProgram' の @Map RUExp Int@ CSE と同じ
--   重複排除を与える (旧実装はキー比較が構造 walk = 共有木で経路数比例、
--   こちらは子 ID 比較のみで O(ノード数 · log))。
--   [English]: Interns an 'RUExp' into the DAG and returns its ID (the
--   'RUExp' counterpart of 'internS'). Structural interning gives the same
--   deduplication as the old 'compileVecProgram''s @Map RUExp Int@ CSE
--   (the old implementation's key comparison was a structural walk,
--   proportional to path count over a shared tree; this one only compares
--   child IDs, giving O(number of nodes · log)).
internRU :: IORef RUDagSt -> RUExp -> IO Int
internRU ref = go
  where
    go e0 = do
      e  <- evaluate e0
      sn <- makeStableName e
      let h = hashStableName sn
      st <- readIORef ref
      case lookup sn =<< IM.lookup h (rudStable st) of
        Just i  -> pure i
        Nothing -> do
          nd <- case e of
            RUK v      -> pure (RNK v)
            RUC v      -> pure (RNC v)
            RUV p      -> pure (RNV p)
            RUG p gids -> pure (RNG p gids)
            RUVec p    -> pure (RNVec p)
            RU1 o x    -> RN1 o <$> go x
            RU2 o a b  -> RN2 o <$> go a <*> go b
            RUSum x    -> RNSum <$> go x
          st1 <- readIORef ref
          i <- case Map.lookup nd (rudStruct st1) of
            Just j  -> pure j
            Nothing -> do
              let j = rudNext st1
              writeIORef ref st1
                { rudStruct = Map.insert nd j (rudStruct st1)
                , rudNodes  = IM.insert j nd (rudNodes st1)
                , rudNext   = j + 1 }
              pure j
          modifyIORef' ref $ \s ->
            s { rudStable = IM.insertWith (++) h [(sn, i)] (rudStable s) }
          pure i

-- | [日本語]: 'compileVecProgram' の DAG 版。 ノードは intern 済み ID で
--   参照し、 CSE cache は ID → slot の 'IM.IntMap'。 superinstruction 融合
--   (85.3-ii/iv) の構造判定は ID 経由の 1 段 lookup。 意味は旧実装と同一 —
--   「構造等値 ⇔ ID 等値」 が intern で保証されるため、 Σ(x−m)² 融合の
--   @r1 == r2@ も ID 比較で厳密に旧構造比較と一致する。
--   [English]: The DAG version of 'compileVecProgram'. Nodes are
--   referenced by their interned IDs, and the CSE cache is an
--   'IM.IntMap' from ID to slot. The structural checks for
--   superinstruction fusion (85.3-ii\/iv) are a single ID-based lookup.
--   The semantics match the old implementation exactly — since interning
--   guarantees "structurally equal ⇔ same ID", the @r1 == r2@ check in the
--   Σ(x−m)² fusion, done via ID comparison, matches the old structural
--   comparison precisely.
compileVecProgramD
  :: [Int]              -- ^ [日本語]: vector leaf 長。 [English]: Vector-leaf lengths.
  -> IM.IntMap RUNode   -- ^ [日本語]: ID → ノード (子 ID < 親 ID)。 [English]: ID -> node (child IDs < parent ID).
  -> Int                -- ^ [日本語]: @RUK 0@ の ID (Σx² 融合で m 側が無い時の代用)。 [English]: The ID of @RUK 0@ (a stand-in for when the Σx² fusion has no m side).
  -> Int                -- ^ [日本語]: 目的 (log-density 和) root ID。 [English]: The root ID of the objective (the summed log-density).
  -> [(GuardKind, Int)] -- ^ [日本語]: guard root ID。 [English]: Guard root IDs.
  -> VecProgram
compileVecProgramD vecLens nodes k0 objI guardIs =
  let nodeOf i = nodes IM.! i
      isVecA = IM.foldlWithKey'
        (\mp i nd -> IM.insert i
           (case nd of
              RNC{}     -> True
              RNG{}     -> True
              RNVec{}   -> True
              RN1 _ a   -> mp IM.! a
              RN2 _ a b -> (mp IM.! a) || (mp IM.! b)
              _         -> False) mp)
        IM.empty nodes
      isVec i = isVecA IM.! i
      emit ins l i (cache, acc, lens, n) =
        (n, (IM.insert i n cache, ins : acc, BV.snoc lens l, n + 1 :: Int))
      -- Phase 85.3-ii: a + s·v (AXPY) の融合対象判定。
      mulSV i = case nodeOf i of
        RN2 SMulO p q
          | not (isVec p), isVec q -> Just (p, q)
          | isVec p, not (isVec q) -> Just (q, p)
        _ -> Nothing
      axpyMatch a b = case mulSV b of
        Just (se, ve) -> Just (a, se, ve)
        Nothing       -> case mulSV a of
          Just (se, ve) -> Just (b, se, ve)
          Nothing       -> Nothing
      -- Phase 85.3-iv: スカラ × gather (VIMulG 判定)
      mulSG x y = case (nodeOf x, nodeOf y) of
        (_, RNG p gids) | not (isVec x) -> Just (x, p, gids)
        (RNG p gids, _) | not (isVec y) -> Just (y, p, gids)
        _ -> Nothing
      -- Phase 85.3-iv: (スカラ×ベクトル) ⊙ データ列定数 (VIMulVC 判定)
      mulVC x y = case (nodeOf x, nodeOf y) of
        (RNC c, _) -> goVC c y
        (_, RNC c) -> goVC c x
        _          -> Nothing
        where
          goVC c mi = case nodeOf mi of
            RN2 SMulO p q
              | not (isVec p), isVec q -> Just (p, q, c)
              | isVec p, not (isVec q) -> Just (q, p, c)
            _ -> Nothing
      comp i st@(cache, _, _, _) = case IM.lookup i cache of
        Just sl -> (sl, st)
        Nothing -> case nodeOf i of
          RNK v      -> emit (VIK v) 0 i st
          RNC v      -> emit (VIKV v) (VS.length v) i st
          RNV p      -> emit (VILeafS p) 0 i st
          RNVec p    -> emit (VILeafV p) (vecLens !! p) i st
          RNG p gids ->
            emit (VIGath p gids (VU.length gids)) (VU.length gids) i st
          RN1 o x    ->
            let (sx, st1@(_, _, lens1, _)) = comp x st
            in emit (VIUn o sx) (lens1 BV.! sx) i st1
          -- Phase 85.3-ii: Σ(x−m)² / Σx² を 1 命令に融合。
          RNSum mI | RN2 SMulO r1 r2 <- nodeOf mI, r1 == r2, isVec r1 ->
            let (xe, me) = case nodeOf r1 of
                  RN2 SSubO x mm -> (x, mm)
                  _              -> (r1, k0)
            in case nodeOf xe of
                 RNC c | RN2 SAddO m1 m2 <- nodeOf me, isVec m1, isVec m2 ->
                   let (s1, st1) = comp m1 st
                       (s2, st2) = comp m2 st1
                   in emit (VISumSqC2 c s1 s2) 0 i st2
                 RNC c ->
                   let (sm, st1) = comp me st
                   in emit (VISumSqC c sm) 0 i st1
                 -- Phase 90 A11-4② (F2): Σ(gather − gather)² は gather 2 本を
                 -- SumSqD 命令に内蔵し 5461 セルの arena 実体化を消す (ICAR)。
                 _ | RNG px gx <- nodeOf xe, RNG pm gm <- nodeOf me
                   , VU.length gx == VU.length gm ->
                     emit (VISumSqDGG px gx pm gm (VU.length gx)) 0 i st
                 _ ->
                   let (sx, st1) = comp xe st
                       (sm, st2) = comp me st1
                   in emit (VISumSqD sx sm) 0 i st2
          -- Phase 85.3-ii: a + s·v → VIAxpy。 85.3-iv: v が gather なら VIAxpyG。
          RN2 SAddO a b | Just (ae, se, ve) <- axpyMatch a b ->
            let (sa, st1) = comp ae st
                (ss, st2) = comp se st1
            in case nodeOf ve of
                 RNC c -> emit (VIAxpyC sa ss c) (VS.length c) i st2
                 RNG p gids ->
                   emit (VIAxpyG sa ss p gids (VU.length gids))
                        (VU.length gids) i st2
                 _ ->
                   let (sv, st3@(_, _, lens3, _)) = comp ve st2
                   in emit (VIAxpy sa ss sv) (lens3 BV.! sv) i st3
          -- Phase 85.3-iv: スカラ×gather → VIMulG。
          RN2 SMulO a b | Just (se, p, gids) <- mulSG a b ->
            let (ss, st1) = comp se st
            in emit (VIMulG ss p gids (VU.length gids)) (VU.length gids) i st1
          -- Phase 85.3-iv: (スカラ×ベクトル)⊙データ列定数 → VIMulVC。
          RN2 SMulO a b | Just (se, ve, c) <- mulVC a b ->
            let (ss, st1) = comp se st
                (sv, st2) = comp ve st1
            in emit (VIMulVC ss sv c) (VS.length c) i st2
          RN2 o a b  ->
            let (sa, st1) = comp a st
                (sb, st2@(_, _, lens2, _)) = comp b st1
            in emit (VIBin o sa sb) (max (lens2 BV.! sa) (lens2 BV.! sb)) i st2
          RNSum x    -> let (sx, st1) = comp x st in emit (VISum sx) 0 i st1
      st0 = (IM.empty :: IM.IntMap Int, [], BV.empty, 0)
      (sObj, st1) = comp objI st0
      (gss, (_, accF, lensF, _)) =
        foldl (\(gacc, st) (k, gi) ->
                 let (sl, st') = comp gi st in (gacc ++ [(k, sl)], st'))
              ([], st1) guardIs
      lensL = BV.toList lensF
      offs  = scanl (+) 0 (map (max 1) lensL)   -- スカラ slot は 1 セル
  in VecProgram
    { vpInstrs  = BV.fromList (reverse accF)
    , vpOff     = VU.fromList (init offs)
    , vpLen     = VU.fromList lensL
    , vpSize    = last offs
    , vpObj     = sObj
    , vpGuards  = gss
    }

-- | [日本語]: 'RUExp' (目的 + guard 式) を命令列へ。 leaf は重複排除・
--   それ以外は木のまま (密度式は小さいので CSE なしで十分。 随伴は slot
--   単位で共有されるため記号微分でも式膨張しない)。
--   [English]: Lowers an 'RUExp' (the objective plus guard expressions)
--   into an instruction stream. Leaves are deduplicated; everything else
--   stays a tree (density expressions are small enough that CSE isn't
--   needed. Adjoints are shared per slot, so symbolic differentiation
--   doesn't blow up the expression either).
compileVecProgram :: [Int] -> RUExp -> [(GuardKind, RUExp)] -> VecProgram
compileVecProgram vecLens obj guards =
  let emit ins l e (cache, acc, lens, n) =
        (n, (Map.insert e n cache, ins : acc, BV.snoc lens l, n + 1 :: Int))
      -- Phase 85.3-ii: a + s·v (AXPY) の融合対象判定。 加数のどちらかが
      -- (スカラ × ベクトル) 積なら Just (残りの加数, スカラ式, ベクトル式)。
      mulSV (RU2 SMulO p q)
        | not (ruIsVec p), ruIsVec q = Just (p, q)
        | ruIsVec p, not (ruIsVec q) = Just (q, p)
      mulSV _ = Nothing
      axpyMatch a b = case mulSV b of
        Just (se, ve) -> Just (a, se, ve)
        Nothing       -> case mulSV a of
          Just (se, ve) -> Just (b, se, ve)
          Nothing       -> Nothing
      -- Phase 85.3-iv: スカラ × gather (VIMulG 判定)
      mulSG x y = case (x, y) of
        (s, RUG p gids) | not (ruIsVec s) -> Just (s, p, gids)
        (RUG p gids, s) | not (ruIsVec s) -> Just (s, p, gids)
        _ -> Nothing
      -- Phase 85.3-iv: (スカラ×ベクトル) ⊙ データ列定数 (VIMulVC 判定)
      mulVC x y = case (x, y) of
        (RUC c, m) -> goVC c m
        (m, RUC c) -> goVC c m
        _          -> Nothing
        where goVC c (RU2 SMulO p q)
                | not (ruIsVec p), ruIsVec q = Just (p, q, c)
                | ruIsVec p, not (ruIsVec q) = Just (q, p, c)
              goVC _ _ = Nothing
      comp e st@(cache, _, _, _) = case Map.lookup e cache of
        Just sl -> (sl, st)
        Nothing -> case e of
          RUK v      -> emit (VIK v) 0 e st
          RUC v      -> emit (VIKV v) (VS.length v) e st
          RUV p      -> emit (VILeafS p) 0 e st
          RUVec p    -> emit (VILeafV p) (vecLens !! p) e st
          RUG p gids ->
            emit (VIGath p gids (VU.length gids)) (VU.length gids) e st
          RU1 o x    ->
            let (sx, st1@(_, _, lens1, _)) = comp x st
            in emit (VIUn o sx) (lens1 BV.! sx) e st1
          -- Phase 85.3-ii: Σ(x−m)² / Σx² を 1 命令に融合 (residual→二乗→Σ の
          -- 3 pass → 1 pass・中間 slot 消滅)。 x がデータ列定数なら VIKV copy
          -- ごと消す。 ruIsVec 条件はスカラ RUSum の従来意味 (0) を保存。
          RUSum (RU2 SMulO r1 r2) | r1 == r2, ruIsVec r1 ->
            let (xe, me) = case r1 of
                  RU2 SSubO x m -> (x, m)
                  x             -> (x, RUK 0)
            in case xe of
                 -- 85.3-iv: Σ(c − (m1+m2))² は和の実体化も畳む (radon の
                 -- 固定効果 μ + RE 項の和がここに来る)
                 RUC c | RU2 SAddO m1 m2 <- me, ruIsVec m1, ruIsVec m2 ->
                   let (s1, st1) = comp m1 st
                       (s2, st2) = comp m2 st1
                   in emit (VISumSqC2 c s1 s2) 0 e st2
                 RUC c ->
                   let (sm, st1) = comp me st
                   in emit (VISumSqC c sm) 0 e st1
                 _ ->
                   let (sx, st1) = comp xe st
                       (sm, st2) = comp me st1
                   in emit (VISumSqD sx sm) 0 e st2
          -- Phase 85.3-ii: a + s·v → VIAxpy (2 pass → 1 pass)。
          -- 85.3-iv: v が gather ならそれも内蔵 (VIAxpyG)。
          RU2 SAddO a b | Just (ae, se, ve) <- axpyMatch a b ->
            let (sa, st1) = comp ae st
                (ss, st2) = comp se st1
            in case ve of
                 RUC c -> emit (VIAxpyC sa ss c) (VS.length c) e st2
                 RUG p gids ->
                   emit (VIAxpyG sa ss p gids (VU.length gids))
                        (VU.length gids) e st2
                 _     ->
                   let (sv, st3@(_, _, lens3, _)) = comp ve st2
                   in emit (VIAxpy sa ss sv) (lens3 BV.! sv) e st3
          -- Phase 85.3-iv: スカラ×gather → VIMulG (gather 実体化の消滅)。
          RU2 SMulO a b | Just (se, p, gids) <- mulSG a b ->
            let (ss, st1) = comp se st
            in emit (VIMulG ss p gids (VU.length gids)) (VU.length gids) e st1
          -- Phase 85.3-iv: (スカラ×ベクトル)⊙データ列定数 → VIMulVC。
          RU2 SMulO a b | Just (se, ve, c) <- mulVC a b ->
            let (ss, st1) = comp se st
                (sv, st2) = comp ve st1
            in emit (VIMulVC ss sv c) (VS.length c) e st2
          RU2 o a b  ->
            let (sa, st1) = comp a st
                (sb, st2@(_, _, lens2, _)) = comp b st1
            in emit (VIBin o sa sb) (max (lens2 BV.! sa) (lens2 BV.! sb)) e st2
          RUSum x    -> let (sx, st1) = comp x st in emit (VISum sx) 0 e st1
      st0 = (Map.empty :: Map RUExp Int, [], BV.empty, 0)
      (sObj, st1) = comp obj st0
      (gss, (_, accF, lensF, _)) =
        foldl (\(gacc, st) (k, ge) ->
                 let (sl, st') = comp ge st in (gacc ++ [(k, sl)], st'))
              ([], st1) guards
      lensL = BV.toList lensF
      offs  = scanl (+) 0 (map (max 1) lensL)   -- スカラ slot は 1 セル
  in VecProgram
    { vpInstrs  = BV.fromList (reverse accF)
    , vpOff     = VU.fromList (init offs)
    , vpLen     = VU.fromList lensL
    , vpSize    = last offs
    , vpObj     = sObj
    , vpGuards  = gss
    }

-- | [日本語]: forward 実行: 全 slot の値を 1 本の arena に書く
--   (ST・per-call 確保)。
--   [English]: The forward pass: writes every slot's value into a single
--   arena (in ST, allocated per call).
forwardArena
  :: CompiledVecIR -> VS.Vector Double -> ST s (VSM.MVector s Double)
forwardArena cvi pc = do
  ar <- VSM.unsafeNew (vpSize (cvProg cvi))
  forwardArenaInto cvi pc ar
  pure ar

-- | [日本語]: 'forwardArena' の呼出側バッファ版 (NUTS 葉勾配の per-call
--   arena 確保 (34k セル級) を chain 閉包での 1 回確保 + 再利用に変える)。
--   全 slot を毎回上書きするため zero-fill 不要。
--   [English]: A caller-supplied-buffer version of 'forwardArena' (turns
--   NUTS leaf-gradient per-call arena allocation at the ~34k-cell scale
--   into a single allocation in the chain closure, reused thereafter).
--   Every slot is overwritten on each call, so no zero-fill is needed.
forwardArenaInto
  :: CompiledVecIR -> VS.Vector Double -> VSM.MVector s Double -> ST s ()
forwardArenaInto cvi pc ar = do
  let prog   = cvProg cvi
      instrs = vpInstrs prog
      offV   = vpOff prog
      lenV   = vpLen prog
      misB   = BV.fromList (cvVecIxs cvi)
      scal p = pc `VS.unsafeIndex` (cvScalIx cvi `VU.unsafeIndex` p)
  let off i = offV `VU.unsafeIndex` i
      len i = lenV `VU.unsafeIndex` i
      rd  = VSM.unsafeRead ar
      wr  = VSM.unsafeWrite ar
      step i = do
        let o = off i
        case instrs BV.! i of
          VIK v     -> wr o v
          VIKV v    ->
            let go !j | j >= VS.length v = pure ()
                      | otherwise = do
                          wr (o + j) (v `VS.unsafeIndex` j)
                          go (j + 1)
            in go 0
          VILeafS p -> wr o (scal p)
          VILeafV p ->
            let mis = misB BV.! p
                go !j | j >= VU.length mis = pure ()
                      | otherwise = do
                          wr (o + j)
                            (pc `VS.unsafeIndex` (mis `VU.unsafeIndex` j))
                          go (j + 1)
            in go 0
          VIGath p gids n ->
            let mis = misB BV.! p
                go !r | r >= n = pure ()
                      | otherwise = do
                          wr (o + r) (pc `VS.unsafeIndex`
                            (mis `VU.unsafeIndex` (gids `VU.unsafeIndex` r)))
                          go (r + 1)
            in go 0
          -- Phase 105 A3: withSUnF/withSBinF (INLINE CPS) で op の case を
          -- ループ外に出し、 known-function の特殊化 unboxed ループに落とす
          -- (closure 間接呼出の per-element boxing 排除。 FP 順序不変)。
          VIUn op x -> withSUnF op $ \f -> do
            let xo = off x
            case len i of
              0 -> rd xo >>= wr o . f
              n ->
                let go !j | j >= n = pure ()
                          | otherwise = do
                              v <- rd (xo + j)
                              wr (o + j) (f v)
                              go (j + 1)
                in go 0
          VIBin op x y -> withSBinF op $ \f -> do
            let xo = off x
                yo = off y
            case (len x, len y) of
              (0, 0) -> do
                a <- rd xo
                b <- rd yo
                wr o (f a b)
              (0, n) -> do
                a <- rd xo
                let go !j | j >= n = pure ()
                          | otherwise = do
                              b <- rd (yo + j)
                              wr (o + j) (f a b)
                              go (j + 1)
                go 0
              (n, 0) -> do
                b <- rd yo
                let go !j | j >= n = pure ()
                          | otherwise = do
                              a <- rd (xo + j)
                              wr (o + j) (f a b)
                              go (j + 1)
                go 0
              (n, _) ->
                let go !j | j >= n = pure ()
                          | otherwise = do
                              a <- rd (xo + j)
                              b <- rd (yo + j)
                              wr (o + j) (f a b)
                              go (j + 1)
                in go 0
          VISum x -> do
            let xo = off x
                n  = len x
                go !acc !j | j >= n    = wr o acc
                           | otherwise = do
                               v <- rd (xo + j)
                               go (acc + v) (j + 1)
            go 0 0
          -- Phase 85.3-ii superinstruction
          VIAxpy a s v -> do
            sv <- rd (off s)
            let vo = off v
                n  = len i
            case len a of
              0 -> do
                av <- rd (off a)
                let go !j | j >= n = pure ()
                          | otherwise = do
                              b <- rd (vo + j)
                              wr (o + j) (av + sv * b)
                              go (j + 1)
                go 0
              _ -> do
                let ao = off a
                    go !j | j >= n = pure ()
                          | otherwise = do
                              av <- rd (ao + j)
                              b  <- rd (vo + j)
                              wr (o + j) (av + sv * b)
                              go (j + 1)
                go 0
          VIAxpyC a s c -> do
            sv <- rd (off s)
            let n = len i
            case len a of
              0 -> do
                av <- rd (off a)
                let go !j | j >= n = pure ()
                          | otherwise = do
                              wr (o + j) (av + sv * (c `VS.unsafeIndex` j))
                              go (j + 1)
                go 0
              _ -> do
                let ao = off a
                    go !j | j >= n = pure ()
                          | otherwise = do
                              av <- rd (ao + j)
                              wr (o + j) (av + sv * (c `VS.unsafeIndex` j))
                              go (j + 1)
                go 0
          VISumSqD x m -> do
            let xo = off x
                mo = off m
                bx = len x /= 0
                bm = len m /= 0
                n  = max (len x) (len m)
                go !acc !j
                  | j >= n = wr o acc
                  | otherwise = do
                      a <- rd (if bx then xo + j else xo)
                      b <- rd (if bm then mo + j else mo)
                      let d = a - b
                      go (acc + d * d) (j + 1)
            go 0 0
          VISumSqC c m -> do
            let mo = off m
                bm = len m /= 0
                n  = VS.length c
                go !acc !j
                  | j >= n = wr o acc
                  | otherwise = do
                      b <- rd (if bm then mo + j else mo)
                      let d = c `VS.unsafeIndex` j - b
                      go (acc + d * d) (j + 1)
            go 0 0
          -- Phase 85.3-iv superinstruction
          VIMulG s p gids n -> do
            sv <- rd (off s)
            let mis = misB BV.! p
                go !j | j >= n = pure ()
                      | otherwise = do
                          wr (o + j) (sv * (pc `VS.unsafeIndex`
                            (mis `VU.unsafeIndex` (gids `VU.unsafeIndex` j))))
                          go (j + 1)
            go 0
          VIAxpyG a s p gids n -> do
            sv <- rd (off s)
            let mis = misB BV.! p
                gv j = pc `VS.unsafeIndex`
                         (mis `VU.unsafeIndex` (gids `VU.unsafeIndex` j))
            case len a of
              0 -> do
                av <- rd (off a)
                let go !j | j >= n = pure ()
                          | otherwise = do
                              wr (o + j) (av + sv * gv j)
                              go (j + 1)
                go 0
              _ -> do
                let ao = off a
                    go !j | j >= n = pure ()
                          | otherwise = do
                              av <- rd (ao + j)
                              wr (o + j) (av + sv * gv j)
                              go (j + 1)
                go 0
          VIMulVC s v c -> do
            sv <- rd (off s)
            let vo = off v
                n  = VS.length c
                go !j | j >= n = pure ()
                      | otherwise = do
                          b <- rd (vo + j)
                          wr (o + j) (sv * b * (c `VS.unsafeIndex` j))
                          go (j + 1)
            go 0
          VISumSqC2 c m1 m2 -> do
            let m1o = off m1
                m2o = off m2
                n   = VS.length c
                go !acc !j
                  | j >= n = wr o acc
                  | otherwise = do
                      b1 <- rd (m1o + j)
                      b2 <- rd (m2o + j)
                      let d = c `VS.unsafeIndex` j - b1 - b2
                      go (acc + d * d) (j + 1)
            go 0 0
          -- Phase 90 A11-4② (F2): gather 2 本内蔵の Σ(φ_a − φ_b)²。 gather 値は
          -- pc から直読み (VIGath forward と同経路)・arena 実体化なし。
          VISumSqDGG px gx pm gm n -> do
            let misx = misB BV.! px
                mism = misB BV.! pm
                go !acc !j
                  | j >= n = wr o acc
                  | otherwise = do
                      let a = pc `VS.unsafeIndex`
                                (misx `VU.unsafeIndex` (gx `VU.unsafeIndex` j))
                          b = pc `VS.unsafeIndex`
                                (mism `VU.unsafeIndex` (gm `VU.unsafeIndex` j))
                          d = a - b
                      go (acc + d * d) (j + 1)
            go 0 0
      loop !i | i >= BV.length instrs = pure ()
              | otherwise = step i >> loop (i + 1)
  loop 0

-- | [日本語]: IR の log-density __値__ (観測尤度 + 族 prior)。 guard
--   (σ/τ/λ ≤ 0・p ∉ (0,1) → -∞) は 'logDensityObs' / @logDensity@ の
--   該当分岐と一致。
--   [English]: The IR's log-density __value__ (observation likelihood +
--   family prior). Guards (σ\/τ\/λ ≤ 0, or p ∉ (0,1), give -∞) match the
--   corresponding branches of 'logDensityObs' \/ @logDensity@.
vecIRValue :: CompiledVecIR -> VS.Vector Double -> Double
vecIRValue cvi pc = runST $ do
  let prog = cvProg cvi
  ar <- forwardArena cvi pc
  ok <- arenaGuardsOK prog ar
  if ok then VSM.unsafeRead ar (vpOff prog `VU.unsafeIndex` vpObj prog)
        else pure negInf

-- | [日本語]: 'gradVecIR' の value-and-grad 融合版。 forward arena を 1 度
--   だけ構築し、 log-density __値__ (objective slot・'vecIRValue' と同一) と
--   constrained 勾配 (mg へ加算・'gradVecIR' と同一) を同時に返す。 NUTS の葉が
--   leapfrog 最終勾配と同一点でエネルギー (logπ) を別途評価していた重複
--   (プロファイル実測 19%) を除去するためのエントリポイント。 guard 違反 =
--   Nothing (呼出側が 値 -∞ / 勾配 walk+ad fallback で従来意味論と一致させる)。
--   [English]: The value-and-gradient fused version of 'gradVecIR'.
--   Builds the forward arena only once, and returns both the log-density
--   __value__ (the objective slot, identical to 'vecIRValue') and the
--   constrained gradient (added into mg, identical to 'gradVecIR') at the
--   same time. This entry point removes the duplicated work where a NUTS
--   leaf separately evaluated the energy (logπ) at the same point as the
--   final leapfrog gradient (measured at 19% of profiled time). A guard
--   violation gives Nothing (the caller matches the previous semantics
--   by treating that as value -∞ \/ falling back to the walk+AD gradient).
gradVecIRVal :: CompiledVecIR -> VS.Vector Double -> VSM.MVector s Double
             -> ST s (Maybe Double)
gradVecIRVal cvi pc mg = do
  let sz = vpSize (cvProg cvi)
  ar  <- VSM.unsafeNew sz
  adj <- VSM.unsafeNew sz
  gradVecIRValWith cvi ar adj pc mg

-- | [日本語]: 'gradVecIRVal' の呼出側バッファ版。 @ar@ / @adj@ は
--   長さ 'vpSize' の作業バッファで、 呼出間で再利用してよい (初期化不要・
--   毎回全上書き / zero-fill される)。 NUTS の葉勾配 closure が chain ごとに
--   1 度だけ確保して全 leapfrog で使い回すためのエントリポイント。
--   [English]: A caller-supplied-buffer version of 'gradVecIRVal'. @ar@ \/
--   @adj@ are working buffers of length 'vpSize' that may be reused
--   across calls (no initialization needed — everything is overwritten
--   or zero-filled on each call). This entry point lets a NUTS leaf's
--   gradient closure allocate once per chain and reuse the buffers for
--   every leapfrog step.
gradVecIRValWith :: CompiledVecIR
                 -> VSM.MVector s Double -> VSM.MVector s Double
                 -> VS.Vector Double -> VSM.MVector s Double
                 -> ST s (Maybe Double)
gradVecIRValWith cvi ar adj pc mg = do
  let prog = cvProg cvi
  forwardArenaInto cvi pc ar
  ok <- arenaGuardsOK prog ar
  if not ok
    then pure Nothing
    else do
      v <- VSM.unsafeRead ar (vpOff prog `VU.unsafeIndex` vpObj prog)
      gradVecIRGoWith cvi pc ar adj mg
      pure (Just v)

-- | [日本語]: forward arena 上で値側 guard を検査 (vecIRValue / gradVecIR 共有)。
--   [English]: Checks the value-side guards over the forward arena
--   (shared by vecIRValue \/ gradVecIR).
arenaGuardsOK :: VecProgram -> VSM.MVector s Double -> ST s Bool
arenaGuardsOK prog ar = fmap and (mapM gOK (vpGuards prog))
  where
    gOK (k, sl) = do
      let o = vpOff prog `VU.unsafeIndex` sl
          n = max 1 (vpLen prog `VU.unsafeIndex` sl)
          chk = case k of
            GPos  -> (> 0)
            GUnit -> \pv -> pv > 0 && pv < 1
          go !j | j >= n    = pure True
                | otherwise = do
                    v <- VSM.unsafeRead ar (o + j)
                    if chk v then go (j + 1) else pure False
      go 0

-- | [日本語]: IR の constrained 勾配を mutable 勾配ベクトルへ__直接__加算
--   する (記号 reverse-mode・arena backward)。 forward arena と同形の随伴
--   arena に逆順伝播し、 leaf 随伴は param 位置へその場で scatter。
--   命令列・形・オフセットは compile 時に固定済み = per-call の tape
--   構築なし。 勾配側は unguarded (従来の前例どおり・-∞ 状態は NUTS が
--   値側で棄却する)。 unconstrained への chain rule は呼出側。
--   [English]: Adds the IR's constrained gradient __directly__ into a
--   mutable gradient vector (symbolic reverse-mode, arena-based
--   backward pass). Propagates in reverse through an adjoint arena
--   shaped like the forward arena, scattering leaf adjoints straight
--   into their param positions as it goes. The instruction stream,
--   shapes, and offsets are already fixed at compile time, so there is
--   no per-call tape construction. The gradient side is unguarded
--   (following past precedent — -∞ states are rejected by NUTS on the
--   value side). The chain rule to unconstrained space is the caller's
--   responsibility.
gradVecIR :: CompiledVecIR -> VS.Vector Double -> VSM.MVector s Double
          -> ST s Bool
gradVecIR cvi pc mg = do
  let prog = cvProg cvi
  ar <- forwardArena cvi pc
  ok <- arenaGuardsOK prog ar
  if not ok then pure False
            else gradVecIRGo cvi pc ar mg >> pure True

-- | [日本語]: 'gradVecIR' の backward 本体 (guard 通過後)。 gather 内蔵
--   命令 (VIMulG/VIAxpyG) が gather 値を読むため pc (constrained params) を取る。
--   [English]: The backward-pass body of 'gradVecIR' (after guards pass).
--   Takes pc (the constrained params) because the gather-fused
--   instructions (VIMulG\/VIAxpyG) read gather values from it.
gradVecIRGo
  :: CompiledVecIR -> VS.Vector Double -> VSM.MVector s Double
  -> VSM.MVector s Double -> ST s ()
gradVecIRGo cvi pc ar mg = do
  adj <- VSM.unsafeNew (vpSize (cvProg cvi))
  gradVecIRGoWith cvi pc ar adj mg

-- | [日本語]: 'gradVecIRGo' の呼出側 adj バッファ版。 zero-fill は
--   本関数が行う (旧 @VSM.replicate (vpSize prog) 0@ と同値) ため、
--   呼出側は確保のみで初期化不要。
--   [English]: A caller-supplied-adj-buffer version of 'gradVecIRGo'.
--   Zero-filling is done by this function itself (equivalent to the old
--   @VSM.replicate (vpSize prog) 0@), so the caller only needs to
--   allocate the buffer, not initialize it.
gradVecIRGoWith
  :: CompiledVecIR -> VS.Vector Double -> VSM.MVector s Double
  -> VSM.MVector s Double -> VSM.MVector s Double -> ST s ()
gradVecIRGoWith cvi pc ar adj mg = do
  let prog   = cvProg cvi
      instrs = vpInstrs prog
      offV   = vpOff prog
      lenV   = vpLen prog
      misB   = BV.fromList (cvVecIxs cvi)
      nSlots = BV.length instrs
  VSM.set adj 0
  let off i = offV `VU.unsafeIndex` i
      len i = lenV `VU.unsafeIndex` i
      rdV = VSM.unsafeRead ar
      rdA = VSM.unsafeRead adj
      addA o d = VSM.unsafeModify adj (+ d) o
      addG ix d = VSM.unsafeModify mg (+ d) ix
      step i = do
        let o = off i
        case instrs BV.! i of
          VIK _  -> pure ()
          VIKV _ -> pure ()
          VILeafS p -> do
            d <- rdA o
            addG (cvScalIx cvi `VU.unsafeIndex` p) d
          VILeafV p ->
            let mis = misB BV.! p
                go !j | j >= len i = pure ()
                      | otherwise = do
                          d <- rdA (o + j)
                          addG (mis `VU.unsafeIndex` j) d
                          go (j + 1)
            in go 0
          VIGath p gids n ->
            let mis = misB BV.! p
                go !r | r >= n = pure ()
                      | otherwise = do
                          d <- rdA (o + r)
                          addG (mis `VU.unsafeIndex`
                                  (gids `VU.unsafeIndex` r)) d
                          go (r + 1)
            in go 0
          -- Phase 105 A3: withSUnD (INLINE CPS) で特殊化 (forward 側と同じ意図)。
          VIUn op x -> withSUnD op $ \df -> do
            let xo = off x
            case len i of
              0 -> do
                a <- rdA o
                v <- rdV xo
                addA xo (df v * a)
              n ->
                let go !j | j >= n = pure ()
                          | otherwise = do
                              a <- rdA (o + j)
                              v <- rdV (xo + j)
                              addA (xo + j) (df v * a)
                              go (j + 1)
                in go 0
          VIBin op x y -> do
            let xo = off x
                yo = off y
                n  = max 1 (len i)
                bx = len x /= 0   -- x がベクトルか
                by = len y /= 0
                xi j = if bx then xo + j else xo
                yi j = if by then yo + j else yo
            case op of
              SAddO ->
                let go !j | j >= n = pure ()
                          | otherwise = do
                              a <- rdA (o + j)
                              addA (xi j) a
                              addA (yi j) a
                              go (j + 1)
                in go 0
              SSubO ->
                let go !j | j >= n = pure ()
                          | otherwise = do
                              a <- rdA (o + j)
                              addA (xi j) a
                              addA (yi j) (negate a)
                              go (j + 1)
                in go 0
              SMulO ->
                let go !j | j >= n = pure ()
                          | otherwise = do
                              a  <- rdA (o + j)
                              vx <- rdV (xi j)
                              vy <- rdV (yi j)
                              addA (xi j) (a * vy)
                              addA (yi j) (a * vx)
                              go (j + 1)
                in go 0
              SDivO ->
                let go !j | j >= n = pure ()
                          | otherwise = do
                              a  <- rdA (o + j)
                              vx <- rdV (xi j)
                              vy <- rdV (yi j)
                              addA (xi j) (a / vy)
                              addA (yi j) (negate (a * vx / (vy * vy)))
                              go (j + 1)
                in go 0
              -- winner-take-all subgradient (tie は測度0・x側に付与で十分)。
              SMaxO ->
                let go !j | j >= n = pure ()
                          | otherwise = do
                              a  <- rdA (o + j)
                              vx <- rdV (xi j)
                              vy <- rdV (yi j)
                              if vx >= vy
                                then addA (xi j) a
                                else addA (yi j) a
                              go (j + 1)
                in go 0
          VISum x -> do
            a <- rdA o
            let xo = off x
                n  = len x
                go !j | j >= n = pure ()
                      | otherwise = addA (xo + j) a >> go (j + 1)
            go 0
          -- Phase 85.3-ii superinstruction: out = a + s·v の随伴 =
          -- adj a += g (スカラ a は Σg)・adj s += Σ g·v・adj v += g·s。
          VIAxpy a s v -> do
            sv <- rdV (off s)
            let vo = off v
                ao = off a
                n  = len i
            case len a of
              0 ->
                let go !ga !gs !j
                      | j >= n = addA ao ga >> addA (off s) gs
                      | otherwise = do
                          g  <- rdA (o + j)
                          bv <- rdV (vo + j)
                          addA (vo + j) (g * sv)
                          go (ga + g) (gs + g * bv) (j + 1)
                in go 0 0 0
              _ ->
                let go !gs !j
                      | j >= n = addA (off s) gs
                      | otherwise = do
                          g  <- rdA (o + j)
                          bv <- rdV (vo + j)
                          addA (ao + j) g
                          addA (vo + j) (g * sv)
                          go (gs + g * bv) (j + 1)
                in go 0 0
          VIAxpyC a s c -> do
            let ao = off a
                n  = len i
            case len a of
              0 ->
                let go !ga !gs !j
                      | j >= n = addA ao ga >> addA (off s) gs
                      | otherwise = do
                          g <- rdA (o + j)
                          go (ga + g) (gs + g * (c `VS.unsafeIndex` j)) (j + 1)
                in go 0 0 0
              _ ->
                let go !gs !j
                      | j >= n = addA (off s) gs
                      | otherwise = do
                          g <- rdA (o + j)
                          addA (ao + j) g
                          go (gs + g * (c `VS.unsafeIndex` j)) (j + 1)
                in go 0 0
          -- out = Σ(x−m)² の随伴 = adj x_j += 2(x_j−m_j)·g・adj m_j −= 同
          -- (スカラ側は Σ を単発加算)。 2(x−m)g は旧 (r·r 同一 slot 2 加算 +
          -- SSubO 伝播) と IEEE 同値 (x+x ≡ 2x)。
          VISumSqD x m -> do
            g <- rdA o
            let xo = off x
                mo = off m
                bx = len x /= 0
                bm = len m /= 0
                n  = max (len x) (len m)
                go !sx !sm !j
                  | j >= n = do
                      if bx then pure () else addA xo sx
                      if bm then pure () else addA mo sm
                  | otherwise = do
                      a <- rdV (if bx then xo + j else xo)
                      b <- rdV (if bm then mo + j else mo)
                      let d = 2 * (a - b) * g
                      if bx then addA (xo + j) d          else pure ()
                      if bm then addA (mo + j) (negate d) else pure ()
                      go (if bx then sx else sx + d)
                         (if bm then sm else sm - d) (j + 1)
            go 0 0 0
          VISumSqC c m -> do
            g <- rdA o
            let mo = off m
                bm = len m /= 0
                n  = VS.length c
                go !sm !j
                  | j >= n = if bm then pure () else addA mo sm
                  | otherwise = do
                      b <- rdV (if bm then mo + j else mo)
                      let d = 2 * (c `VS.unsafeIndex` j - b) * g
                      if bm then addA (mo + j) (negate d) >> go sm (j + 1)
                            else go (sm - d) (j + 1)
            go 0 0
          -- Phase 85.3-iv superinstruction: gather 内蔵命令の随伴は leaf
          -- (param) へ直接 scatter ('VIGath' backward と同じ) + gather 値は
          -- pc から読む。
          VIMulG s p gids n -> do
            sv <- rdV (off s)
            let mis = misB BV.! p
                go !gs !j
                  | j >= n = addA (off s) gs
                  | otherwise = do
                      g <- rdA (o + j)
                      let ix = mis `VU.unsafeIndex` (gids `VU.unsafeIndex` j)
                      addG ix (g * sv)
                      go (gs + g * (pc `VS.unsafeIndex` ix)) (j + 1)
            go 0 0
          VIAxpyG a s p gids n -> do
            sv <- rdV (off s)
            let mis = misB BV.! p
                ao  = off a
            case len a of
              0 ->
                let go !ga !gs !j
                      | j >= n = addA ao ga >> addA (off s) gs
                      | otherwise = do
                          g <- rdA (o + j)
                          let ix = mis `VU.unsafeIndex` (gids `VU.unsafeIndex` j)
                          addG ix (g * sv)
                          go (ga + g) (gs + g * (pc `VS.unsafeIndex` ix)) (j + 1)
                in go 0 0 0
              _ ->
                let go !gs !j
                      | j >= n = addA (off s) gs
                      | otherwise = do
                          g <- rdA (o + j)
                          let ix = mis `VU.unsafeIndex` (gids `VU.unsafeIndex` j)
                          addA (ao + j) g
                          addG ix (g * sv)
                          go (gs + g * (pc `VS.unsafeIndex` ix)) (j + 1)
                in go 0 0
          VIMulVC s v c -> do
            sv <- rdV (off s)
            let vo = off v
                n  = VS.length c
                go !gs !j
                  | j >= n = addA (off s) gs
                  | otherwise = do
                      g <- rdA (o + j)
                      b <- rdV (vo + j)
                      let cj = c `VS.unsafeIndex` j
                      addA (vo + j) (g * sv * cj)
                      go (gs + g * b * cj) (j + 1)
            go 0 0
          VISumSqC2 c m1 m2 -> do
            g <- rdA o
            let m1o = off m1
                m2o = off m2
                n   = VS.length c
                go !j | j >= n = pure ()
                      | otherwise = do
                          b1 <- rdV (m1o + j)
                          b2 <- rdV (m2o + j)
                          let d = 2 * (c `VS.unsafeIndex` j - b1 - b2) * g
                          addA (m1o + j) (negate d)
                          addA (m2o + j) (negate d)
                          go (j + 1)
            go 0
          -- Phase 90 A11-4② (F2): 随伴 = ∂/∂φ_a[Σ(φ_a−φ_b)²] = 2(φ_a−φ_b)·g を
          -- param へ直 scatter (φ_b は −同)。 gather 値は pc から直読み。
          VISumSqDGG px gx pm gm n -> do
            g <- rdA o
            let misx = misB BV.! px
                mism = misB BV.! pm
                go !j | j >= n = pure ()
                      | otherwise = do
                          let ixa = misx `VU.unsafeIndex` (gx `VU.unsafeIndex` j)
                              ixb = mism `VU.unsafeIndex` (gm `VU.unsafeIndex` j)
                              d = 2 * (pc `VS.unsafeIndex` ixa
                                       - pc `VS.unsafeIndex` ixb) * g
                          addG ixa d
                          addG ixb (negate d)
                          go (j + 1)
            go 0
      loop !i | i < 0     = pure ()
              | otherwise = step i >> loop (i - 1)
  VSM.unsafeWrite adj (off (vpObj prog)) 1
  loop (nSlots - 1)
