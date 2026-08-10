{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- |
-- Module      : Hanalyze.Model.HBM.Model
-- Description : HBM の多相モデル DSL (Free monad) 記述層
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 多相モデル DSL (Free monad) を 'Hanalyze.Model.HBM' から分離した。
--
-- 本モジュールは PPL の __記述層__ を担う:
--
--   - @Free@ monad 再実装 (型は 'Hanalyze.Model.HBM' 公開のものと別個)
--   - 'ModelF' プリミティブ (sample / observe / observeLM / deterministic /
--     plate / Data / Potential) と 'Model' / 'ModelP' 型エイリアス
--   - 第一級ランダム効果値 'REffect' / 'REff' と階層モデル helper 群
--     (reNormal / mvNormalLatent / lkjCorrCholesky / ar1Latent / dirichlet /
--      orderedCuts / dpStickBreaking / hmmLatent / glmmRandomIntercept 等)
--   - Plate notation と構造検査 ('collectNodes' / 'sampleNames')
--
-- 評価 (logJoint 等)・AD 勾配・IR は __上層__ に置かれ、 本モジュールは
-- それらに依存しない (leaf-first・facade 非 import の規律。 計画参照)。
-- 依存は下層 'Hanalyze.Model.HBM.Util' / '...Distribution' のみ。
--
-- [English]: The polymorphic model DSL (Free monad) was split out of
-- 'Hanalyze.Model.HBM'.
--
-- This module owns the PPL's __description layer__:
--
--   - The reimplemented @Free@ monad (a type distinct from the one
--     publicly exposed by 'Hanalyze.Model.HBM')
--   - The 'ModelF' primitives (sample \/ observe \/ observeLM \/
--     deterministic \/ plate \/ Data \/ Potential) and the 'Model' \/
--     'ModelP' type aliases
--   - First-class random-effect values 'REffect' \/ 'REff' and the
--     hierarchical-model helper family (reNormal \/ mvNormalLatent \/
--     lkjCorrCholesky \/ ar1Latent \/ dirichlet \/ orderedCuts \/
--     dpStickBreaking \/ hmmLatent \/ glmmRandomIntercept, etc.)
--   - Plate notation and structural inspection ('collectNodes' \/
--     'sampleNames')
--
-- Evaluation (logJoint etc.), AD gradients, and the IR live in the
-- __upper layer__; this module does not depend on them (leaf-first,
-- no-facade-import discipline; see the plan). Its only dependencies
-- are the lower-layer 'Hanalyze.Model.HBM.Util' \/
-- '...Distribution'.
module Hanalyze.Model.HBM.Model
  ( -- * Free monad
    Free (..)
  , liftF
    -- * Polymorphic model DSL
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
  , lmFamilyName
  , lmParents
  , REff (..)
  , REffect (..)
  , reffNames
  , reNormal
  , at
  , indexed
  , (.#)
  , potential
  , deterministic
  , nonCenteredNormal
  , dirichlet
  , orderedCuts
  , dpStickBreaking
  , hmmLatent
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
  , lkjCorrCholesky
  , gpExpQuadCov
  , gpLatent
  , ar1Latent
    -- ** plate notation
  , plate
  , plateI
  , plateI_
  , plateForM
  , plateForM_
  , withPlate
    -- * Structural inspection
  , Node (..)
  , NodeKind (..)
  , collectNodes
  , sampleNames
  , dataSlots
  , dataIxSlots
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad (forM, forM_)
import Data.List (foldl', nub)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T

import Hanalyze.Model.HBM.Util (negInf, logSumExpA, choleskyL, hmmForwardLogLik)
import Hanalyze.Model.HBM.Distribution

-- ---------------------------------------------------------------------------
-- @Free@ monad (再実装。Hanalyze.Model.HBM のものとは型が違うので別途定義)
-- ---------------------------------------------------------------------------

data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap g (Pure a) = Pure (g a)
  fmap g (Free x) = Free (fmap (fmap g) x)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure g <*> x  = fmap g x
  Free fg <*> x = Free (fmap (<*> x) fg)

instance Functor f => Monad (Free f) where
  return = pure
  Pure a >>= g = g a
  Free x >>= g = Free (fmap (>>= g) x)

liftF :: Functor f => f a -> Free f a
liftF fa = Free (fmap Pure fa)

-- ---------------------------------------------------------------------------
-- 多相モデル (@Free@ monad)
-- ---------------------------------------------------------------------------

-- | [日本語]: DSL のプリミティブ。継続が @a -> next@ なので任意の @a@ を流せる。
--
--   'Potential' は PyMC の @pm.Potential@ 相当で、任意の log-prob 項を
--   log-joint に加える。ソフト制約・カスタム尤度・正則化項などに使える。
--
--   [English]: The DSL's primitives. Since the continuation is
--   @a -> next@, any @a@ can be threaded through.
--
--   'Potential' is the equivalent of PyMC's @pm.Potential@: it adds an
--   arbitrary log-prob term to the log-joint. Useful for soft
--   constraints, custom likelihoods, regularization terms, and the like.
-- | [日本語]: 構造化線形予測子 observe の family / link。
--
--   通常の 'Observe' は平均が不透明な AD 値ゆえ「β に線形」 という構造を
--   保持できない。 'ObserveLM' は設計行列 X (Double) と β パラメタ名を __分離__
--   して持つことで線形構造をライブラリが知り、 Gaussian-恒等リンクの
--   十分統計量 collapse (観測和を tape O(p²) に畳む) を可能にする。
--
--   [English]: The family \/ link for the structured linear-predictor
--   observe.
--
--   The regular 'Observe' cannot preserve the "linear in β" structure,
--   since its mean is an opaque AD value. 'ObserveLM' keeps the design
--   matrix X (Double) and the β parameter names __separate__, so the
--   library knows the linear structure, enabling the collapse of the
--   sufficient statistic for the Gaussian-identity link (folding the
--   observation sum into an O(p²) tape).
data LMFamily
  = LMGaussian Text   -- ^ [日本語]: identity link。 引数 = σ (誤差 SD) パラメタ名。 [English]: identity link; the argument is the σ (residual SD) parameter name.
  | LMPoisson         -- ^ [日本語]: log link (μ = exp η)。 [English]: log link (μ = exp η).
  | LMBernoulli       -- ^ [日本語]: logit link (p = 1/(1+e^{-η}))。 [English]: logit link (p = 1/(1+e^{-η})).
  deriving (Show, Eq)

-- | [日本語]: 'ObserveLM' のランダム効果項。 線形予測子に
-- @η_i += u^{re}[gid_i]@ を gather で加える。 設計行列の one-hot 指示列として
-- 密に展開する代わりに、 群 id ベクトルで疎に保持することで vec-tape の
-- 観測尤度勾配が群効果に対しても O(n) で済む (密展開は O(nG·n) で階層モデルで
-- 逆効果になる・計測で確認済)。
--
-- フィールド: u パラメタ名 (長さ nG・既に 'sample' 済の latent を参照) /
-- 各観測の群 id (長さ n・0..nG-1) /
-- prior スケール名: @Just τName@ なら各 u_j が
-- @u_j ~ Normal(0, τ)@ という標準的な階層 prior を持つことを宣言する。
-- これがあると @compileGradU@ は u-prior 勾配を __解析的に__ (ベクトル化して)
-- 計算し、 対応する @u_j@ 'Sample' ノードを @ad@ walk から除外できる
-- (per-grad の支配項だった O(nG) スカラ @ad@ を排除)。 @Nothing@ なら
-- prior は従来通り @ad@ 経路で扱う (後方互換)。 通常は 'reNormal'/'at' で
-- 自動的に @Just@ が載るので、 ユーザがこの構築子を直接書く必要はない。
--
-- per-row 重み: @Just ws@ (長さ n) なら @η_i += w_i·u^{re}[gid_i]@
-- (random slope = 群別係数 × 共変量)。 @Nothing@ = 全 1 (random intercept・
-- 後方互換)。 prior 解析勾配 (@u_j ~ Normal(0,τ)@) は重みと無関係に同形。
-- 由来 slot 名: 5 番目 field は gids がどのデータ slot
-- ('dataNamedIx') 由来かの静的属性。 @Just slot@ なら 'lmParents' が slot 名を
-- 親集合に加え、 DAG に slot (DataN)→観測ノードのエッジが出る (PyMC
-- @b0[gid]@ 同型)。 'atIx' が自動で載せる。 'at' / IR 合成経路は @Nothing@
-- (従来挙動)。 hot closure (@CompiledLMBlock@) には乗らない = per-draw 無影響。
--
-- [English]: The random-effect term of 'ObserveLM'. Adds
-- @η_i += u^{re}[gid_i]@ to the linear predictor via a gather. Instead
-- of densely expanding the design matrix into one-hot indicator
-- columns, keeping the group-id vector sparse means the vec-tape's
-- observation-likelihood gradient stays O(n) even for group effects
-- (dense expansion is O(nG·n), which backfires for hierarchical
-- models, as confirmed by measurement).
--
-- Fields: the u parameter names (length nG; refer to latents already
-- declared via 'sample') \/ each observation's group id (length n,
-- 0..nG-1) \/ the prior scale name: @Just τName@ declares that each
-- u_j has the standard hierarchical prior @u_j ~ Normal(0, τ)@. When
-- present, @compileGradU@ computes the u-prior gradient __analytically__
-- (vectorized) and can exclude the corresponding @u_j@ 'Sample' nodes
-- from the @ad@ walk (removing the O(nG) scalar @ad@ calls that used
-- to dominate the per-gradient cost). @Nothing@ means the prior is
-- still handled via the @ad@ path as before (backward compatible).
-- 'reNormal'\/'at' normally attach @Just@ automatically, so users
-- should not need to construct this directly.
--
-- Per-row weights: @Just ws@ (length n) means
-- @η_i += w_i·u^{re}[gid_i]@ (random slope = per-group coefficient ×
-- covariate). @Nothing@ = all 1s (random intercept, backward
-- compatible). The analytic prior gradient (@u_j ~ Normal(0,τ)@) has
-- the same shape regardless of the weights.
--
-- Origin slot name: the 5th field is a static attribute recording
-- which data slot ('dataNamedIx') the gids came from. @Just slot@
-- makes 'lmParents' add the slot name to the parent set, so the DAG
-- gets an edge from the slot (DataN) to the observation node (the
-- same shape as PyMC's @b0[gid]@). 'atIx' attaches this automatically.
-- 'at' \/ the IR-composition path leave it @Nothing@ (legacy behavior).
-- It is not carried by the hot closure (@CompiledLMBlock@), so it has
-- no per-draw impact.
data REff = REff [Text] [Int] (Maybe Text) (Maybe [Double]) !(Maybe Text)
  deriving (Show, Eq)

-- Phase 54.8: synthGaussLMBlocks の安全網 (force で全評価し poison を捕捉) 用。
instance NFData REff where
  rnf (REff us gids sc mw ms) =
    rnf us `seq` rnf gids `seq` rnf sc `seq` rnf mw `seq` rnf ms

data ModelF a next
  = Sample  Text (Distribution a) (a -> next)
  | Observe Text (Distribution a) [Double] next
  | ObserveLM Text [Text] [[Double]] [REff] LMFamily [Double] next
    -- ^ [日本語]: 構造化線形予測子 observe。
    --   フィールド: ブロック名 / β パラメタ名 (順序 = X の列) /
    --   設計行列 X (n 行 × p 列、 Double) / ランダム効果項 (gather) /
    --   family-link / 観測 ys (長さ n)。
    --   各 i について η_i = Σ_j β_j·X_ij + Σ_re u^{re}[gid^{re}_i]、
    --   μ_i = link⁻¹(η_i)、 log-lik = Σ_i logDensityObs(family μ_i) y_i。
    --   β / u / 分散パラメタは別途 'sample' で宣言された latent を
    --   __名前参照__する (prior は持たない)。
    --   DAG 上は 1 観測ノード (親 = β + u + 分散パラメタ名)。
    --
    --   [English]: The structured linear-predictor observe.
    --   Fields: block name \/ β parameter names (order = X's columns) \/
    --   design matrix X (n rows × p columns, Double) \/ random-effect
    --   terms (gather) \/ family-link \/ observations ys (length n).
    --   For each i, η_i = Σ_j β_j·X_ij + Σ_re u^{re}[gid^{re}_i],
    --   μ_i = link⁻¹(η_i), log-lik = Σ_i logDensityObs(family μ_i) y_i.
    --   β \/ u \/ the variance parameter are __referenced by name__ from
    --   latents declared separately via 'sample' (they carry no prior
    --   here). On the DAG this is a single observation node (parents =
    --   β + u + the variance parameter name).
  | Potential Text a next
    -- ^ [日本語]: 名前付きの ad-hoc な log-prob 項。値 @a@ がそのまま log-joint に加算される。
    --   [English]: A named ad-hoc log-prob term. The value @a@ is added
    --   directly to the log-joint.
  | Deterministic Text a (a -> next)
    -- ^ [日本語]: 名前付きの派生量 (PyMC `pm.Deterministic`)。log-joint には寄与せず、
    --   サンプルごとに値を保存する。継続には値そのものを通すので、その後の
    --   モデル中でも参照可能。
    --
    --   [English]: A named derived quantity (PyMC's `pm.Deterministic`).
    --   Does not contribute to the log-joint; its value is saved for
    --   each sample. Since the value itself is threaded through the
    --   continuation, it can also be referenced later in the model.
  | Data Text [Double] (([a], [Double]) -> next)
    -- ^ [日本語]: 名前付き観測データプレースホルダ (PyMC `pm.Data`)。
    --   モデル内でデータを保持し、`withData` で外部から差し替え可能。
    --   観測値を直接 `observe` に渡す代わりに、`dataNamed` で受け取って
    --   `observe` に渡すと、後でデータ差し替えができる。
    --   ★破壊的変更 (旧バージョン比): 継続は ([a], [Double]) の 2 view を受ける
    --   (格納は [Double] のまま・各 interpreter が lift)。 fst = モデル数値型
    --   ('dataNamed'、 covariate 用・realToFrac 不要)、 snd = 生 [Double]
    --   ('dataNamedObs'、 'observe' の観測値用)。 tuple は lazy なので
    --   未使用側の lift コストは掛からない。
    --
    --   [English]: A named observed-data placeholder (PyMC's `pm.Data`).
    --   Holds data inside the model, replaceable from the outside via
    --   `withData`. Instead of passing observed values directly to
    --   `observe`, receiving them via `dataNamed` and passing that to
    --   `observe` allows the data to be swapped out later.
    --   ★Breaking change (vs. an earlier version): the continuation now
    --   receives two views, ([a], [Double]) (storage stays [Double];
    --   each interpreter lifts as needed). fst = the model's numeric
    --   type ('dataNamed', for covariates, no realToFrac needed), snd =
    --   raw [Double] ('dataNamedObs', for 'observe''s observed values).
    --   The tuple is lazy, so the unused side incurs no lifting cost.
  | DataIx Text [Int] ([Int] -> next)
    -- ^ [日本語]: 離散 index 専用のデータプレースホルダ。 群 index 等の
    --   名義尺度を [Int] のまま運ぶ (= AD 型に持ち上げない・round 罠の根治)。
    --   継続型は @a@ に依らず [Int] なので interpreter の lift も不要。
    --
    --   [English]: A data placeholder dedicated to discrete indices.
    --   Carries nominal-scale values such as group indices as-is, as
    --   [Int] (i.e. never lifted to the AD type — the fix for the
    --   rounding trap at its root). Since the continuation type is
    --   [Int] regardless of @a@, no interpreter lifting is needed either.
  | PlateBegin Text Int next
    -- ^ [日本語]: Plate 開始マーカー (Pyro/NumPyro 流の plate-block 糖衣)。
    --   名前 + サイズ N を持つ plate スコープの開始。 直後から 'PlateEnd'
    --   までに登録される 'Sample' / 'Observe' / 'Deterministic' は
    --   buildModelGraph で「plate メンバ」 として描画される。
    --   nested plate は LIFO スタックで対応。 log eval interpreter (logJoint
    --   等) は __透過__ に処理する (何もしない)。
    --
    --   [English]: A plate-begin marker (sugar for a Pyro\/NumPyro-style
    --   plate block). Opens a plate scope with a name and size N. Every
    --   'Sample' \/ 'Observe' \/ 'Deterministic' registered from here up
    --   to the matching 'PlateEnd' is drawn as a "plate member" by
    --   buildModelGraph. Nested plates are handled with a LIFO stack.
    --   Log-eval interpreters (logJoint etc.) treat this __transparently__
    --   (i.e. do nothing).
  | PlateEnd next
    -- ^ [日本語]: Plate 終了マーカー。 最新の PlateBegin スコープを閉じる。
    --   [English]: A plate-end marker. Closes the most recent
    --   'PlateBegin' scope.
  deriving Functor

type Model a = Free (ModelF a)

-- | [日本語]: 多相モデル DSL の型エイリアス。
-- @ModelP r = forall a. (Floating a, Ord a, TrackTag a) => Model a r@
-- ('TrackTag' は '!!!' の依存タグ注入用。 数値解釈は既定 id)。
--
-- [English]: Type alias for the polymorphic model DSL.
-- @ModelP r = forall a. (Floating a, Ord a, TrackTag a) => Model a r@
-- ('TrackTag' is used to inject the dependency tag for '!!!'; the
-- default numeric interpretation is the identity).
type ModelP r = forall a. (Floating a, Ord a, TrackTag a) => Model a r

sample :: Text -> Distribution a -> Model a a
sample n d = liftF (Sample n d id)

observe :: Text -> Distribution a -> [Double] -> Model a ()
observe n d ys = liftF (Observe n d ys ())

-- | [日本語]: 構造化線形予測子 observe。
--
-- @observeLM name betaNames designX family ys@ は、 設計行列 @designX@
-- (n 行 × p 列) と β パラメタ名 @betaNames@ (長さ p・既に 'sample' で宣言済の
-- latent を参照) を __分離して__保持する観測ブロック。 各観測 i について
-- η_i = Σ_j β_j·X_ij を作り、 @family@ のリンク逆関数で μ_i に写して
-- 観測 @ys !! i@ の log-density を加算する。
--
-- 通常の per-obs @observe@ を N 回呼ぶのと数値的に等価だが、 線形構造を
-- 保持するので Gaussian-恒等リンクの十分統計量 collapse に乗せられる。
--
-- [English]: The structured linear-predictor observe.
--
-- @observeLM name betaNames designX family ys@ is an observation block
-- that keeps the design matrix @designX@ (n rows × p columns) and the
-- β parameter names @betaNames@ (length p; referencing latents already
-- declared via 'sample') __separate__. For each observation i, it forms
-- η_i = Σ_j β_j·X_ij, maps it to μ_i via @family@'s inverse link, and
-- adds the log-density of the observation @ys !! i@.
--
-- Numerically equivalent to calling per-obs @observe@ N times, but
-- since it preserves the linear structure it can be put through the
-- sufficient-statistic collapse for the Gaussian-identity link.
observeLM :: Text -> [Text] -> [[Double]] -> LMFamily -> [Double] -> Model a ()
observeLM n betas designX fam ys = liftF (ObserveLM n betas designX [] fam ys ())

-- | [日本語]: ランダム効果付き 'observeLM'。
--
-- @observeLMR name betaNames designX reffs family ys@ は 'observeLM' に
-- ランダム効果項 @reffs@ を加えたもの。 各 'REff' は (u パラメタ名, 群 id) で
-- @η_i += u^{re}[gid_i]@ を __gather__ で寄与する。 群効果を設計行列の one-hot
-- 指示列に密展開すると vec-tape 勾配が O(nG·n) になり階層モデルで逆効果になる
-- (計測済) ため、 群構造は疎に保持して gather で O(n) に保つ。
--
-- [English]: 'observeLM' with random effects.
--
-- @observeLMR name betaNames designX reffs family ys@ is 'observeLM'
-- with the random-effect terms @reffs@ added. Each 'REff' contributes
-- @η_i += u^{re}[gid_i]@ via a __gather__, from a (u parameter name,
-- group id) pair. Densely expanding group effects into one-hot
-- indicator columns of the design matrix would make the vec-tape
-- gradient O(nG·n), which backfires for hierarchical models (as
-- measured), so the group structure is kept sparse and applied via
-- gather to stay O(n).
observeLMR :: Text -> [Text] -> [[Double]] -> [REff] -> LMFamily -> [Double]
           -> Model a ()
observeLMR n betas designX reffs fam ys =
  liftF (ObserveLM n betas designX reffs fam ys ())

-- ---------------------------------------------------------------------------
-- 第一級ランダム効果値 (Phase 54.4c)
-- ---------------------------------------------------------------------------

-- | [日本語]: 第一級ランダム効果値。 'reNormal' で宣言した nG 個の
-- iid @Normal(0, τ)@ latent を、 構造 (基底名・群数・スケール名・値) ごと
-- ひとつの値に載せて持ち運ぶ。 これにより観測の線形予測子に効果を載せるとき
-- 文字列添字 (@"u_" <> show j@) も @us !! g@ も書かずに 'at' で gather でき
-- (Haskell 王道の「構造を値に載せて流す」)、 さらにスケール名が構造として
-- 保持されるので @compileGradU@ が u-prior 勾配を解析的にベクトル化できる。
--
-- [English]: A first-class random-effect value. Carries the nG iid
-- @Normal(0, τ)@ latents declared via 'reNormal' as a single value,
-- together with their structure (base name, group count, scale name,
-- values). This lets an observation's linear predictor gather the
-- effect via 'at' without writing string subscripts (@"u_" <> show j@)
-- or @us !! g@ (the idiomatic Haskell approach of "threading structure
-- through as a value"), and since the scale name is preserved as part
-- of the structure, @compileGradU@ can vectorize the u-prior gradient
-- analytically.
data REffect a = REffect
  { reffBase   :: !Text   -- ^ [日本語]: 基底名 (例 @"u"@)。 latent 名は @base_<j>@。 [English]: The base name (e.g. @"u"@); latent names are @base_<j>@.
  , reffNG     :: !Int    -- ^ [日本語]: 群数 nG [English]: The number of groups, nG.
  , reffScale  :: !Text   -- ^ [日本語]: スケール latent の名前 (@u_j ~ Normal(0, scale)@) [English]: The name of the scale latent (@u_j ~ Normal(0, scale)@).
  , reffValues :: [a]     -- ^ [日本語]: サンプル済 nG 個の値 (forward 評価・deterministic 用) [English]: The nG already-sampled values (for forward evaluation \/ deterministic).
  }

-- | [日本語]: 'REffect' の latent 名 (@base_0 .. base_{nG-1}@)。
--   [English]: 'REffect'\'s latent names (@base_0 .. base_{nG-1}@).
reffNames :: REffect a -> [Text]
reffNames re = [ indexed (reffBase re) j | j <- [0 .. reffNG re - 1] ]

-- | [日本語]: 群別ランダム効果を第一級値として宣言する。
--
-- @reNormal base nG scaleName scaleVal@ は @base_0 .. base_{nG-1}@ という
-- nG 個の latent を各々 @Normal(0, scaleVal)@ として 'sample' し、 その構造
-- (基底名 / nG / スケール名 / 値) を 'REffect' にまとめて返す。 @scaleName@ は
-- @scaleVal@ を生んだスケール latent の名前 (例 @"tau_u"@) で、 解析 prior 勾配
-- 経路 (@compileGradU@) がスケール変数を引くために構造として保持する
-- (値は名前を覚えていないため明示的に渡す)。
--
-- @
-- tau <- sample "tau_u" (HalfNormal 5)
-- u   <- reNormal "u" nG "tau_u" tau
-- observeNormalLM "y" xRows betaNames [u \`at\` gids] "sigma" ys
-- @
--
-- [English]: Declares per-group random effects as a first-class value.
--
-- @reNormal base nG scaleName scaleVal@ samples nG latents,
-- @base_0 .. base_{nG-1}@, each as @Normal(0, scaleVal)@ via 'sample',
-- and returns their structure (base name \/ nG \/ scale name \/ values)
-- bundled into an 'REffect'. @scaleName@ is the name of the scale
-- latent that produced @scaleVal@ (e.g. @"tau_u"@), kept as part of
-- the structure so that the analytic prior-gradient path
-- (@compileGradU@) can look up the scale variable (the value alone
-- doesn't remember its name, so it must be passed explicitly).
--
-- @
-- tau <- sample "tau_u" (HalfNormal 5)
-- u   <- reNormal "u" nG "tau_u" tau
-- observeNormalLM "y" xRows betaNames [u \`at\` gids] "sigma" ys
-- @
reNormal :: Num a => Text -> Int -> Text -> a -> Model a (REffect a)
reNormal base nG scaleName scaleVal = do
  vals <- forM [0 .. nG - 1] $ \j ->
            sample (indexed base j) (Normal 0 scaleVal)
  pure (REffect base nG scaleName vals)

-- | [日本語]: 'REffect' を観測の群 id 列に対して gather し 'REff' (観測ブロック用) に変換する。
-- @η_i += u^{re}[gid_i]@。 スケール名を 'REff' に載せるので、 これ経由で観測に
-- 入った効果は @compileGradU@ の解析 prior 勾配経路に乗る。
--
-- [English]: Gathers an 'REffect' over an observation's group-id list
-- and converts it to an 'REff' (for use in observation blocks):
-- @η_i += u^{re}[gid_i]@. Since the scale name is carried on the
-- 'REff', an effect entered this way is put through @compileGradU@\'s
-- analytic prior-gradient path.
at :: REffect a -> [Int] -> REff
at re gids = REff (reffNames re) gids (Just (reffScale re)) Nothing Nothing

-- | [日本語]: Gaussian-恒等リンク版の構造化 observe。 'observeLMR' の
-- @LMGaussian@ 特化で、 'at' で作った 'REff' をそのまま渡せる薄いラッパ。
--
-- @observeNormalLM name designX betaNames reffs sigmaName ys@。
--
-- [English]: The structured observe specialized to the
-- Gaussian-identity link. A thin wrapper around 'observeLMR'\'s
-- @LMGaussian@ case that lets you pass an 'REff' built with 'at'
-- directly.
--
-- @observeNormalLM name designX betaNames reffs sigmaName ys@.
observeNormalLM :: Text -> [[Double]] -> [Text] -> [REff] -> Text -> [Double]
                -> Model a ()
observeNormalLM name designX betaNames reffs sName ys =
  observeLMR name betaNames designX reffs (LMGaussian sName) ys

-- | [日本語]: 多変量観測 ('MvNormal' 用)。 各観測は長さ @k@ のベクトルで、
--   リストとして @[[Double]]@ で渡す。 内部的には @concat@ で flatten され、
--   評価時に Distribution の次元 k で chunk される。
--
--   [English]: Multivariate observation (for 'MvNormal'). Each
--   observation is a length-@k@ vector; pass them as a list
--   @[[Double]]@. Internally it is flattened via @concat@, then
--   re-chunked into groups of the Distribution's dimension k at
--   evaluation time.
observeMV :: Text -> Distribution a -> [[Double]] -> Model a ()
observeMV n d obss = liftF (Observe n d (concat obss) ())

-- | [日本語]: 多出力観測 helper。 @q@ 組の
--   @observe (prefix <> \"_\" <> j) dist_j ys_j@ を順に発行する。
--
--   多出力回帰の尤度を 1 行で書きたいときに使う:
--
--   @
--   observeColumns \"y\" [(Normal mu_j sigma_j, ysCol j) | j <- [0 .. q - 1]]
--   @
--
--   [English]: Multi-output observation helper. Emits @q@ pairs of
--   @observe (prefix <> \"_\" <> j) dist_j ys_j@ in order.
--
--   Useful when you want to write a multi-output regression's
--   likelihood in one line:
--
--   @
--   observeColumns \"y\" [(Normal mu_j sigma_j, ysCol j) | j <- [0 .. q - 1]]
--   @
observeColumns :: Text -> [(Distribution a, [Double])] -> Model a ()
observeColumns prefix pairs =
  mapM_ (\(j, (d, ys)) ->
           observe (prefix <> "_" <> T.pack (show (j :: Int))) d ys)
        (zip [0..] pairs)

-- | [日本語]: インデックス付きノード名を作る: @indexed "theta" 1 == "theta_1"@。
--
--   階層モデルで群ごとの 'sample' / 'observe' 名を作るときに頻出する
--   @T.pack ("theta_" ++ show j)@ ボイラープレートを畳む。 アンダースコアは
--   自動付与 (= 'observeColumns' / 'nonCenteredNormal' 等の命名規約に一致)。
--
--   [English]: Builds an indexed node name:
--   @indexed "theta" 1 == "theta_1"@.
--
--   Folds the common boilerplate @T.pack ("theta_" ++ show j)@ used to
--   name per-group 'sample' / 'observe' calls in hierarchical models. The
--   underscore is added automatically (matching the naming convention of
--   'observeColumns' / 'nonCenteredNormal', etc.).
--
-- > forM_ (zip [1..] groupData) $ \(j, ys) -> do
-- >   theta <- sample (indexed "theta" j) (Normal mu tau)   -- "theta_1" …
-- >   observe (indexed "y" j) (Normal theta 1) ys
indexed :: Text -> Int -> Text
indexed pre i = pre <> "_" <> T.pack (show i)

-- | [日本語]: 'indexed' の中置演算子版: @"theta" .# j == "theta_1"@。
--   (Haskell の演算子記号に @_@ は使えないため @.#@ を採用。)
--   [English]: The infix-operator form of 'indexed':
--   @"theta" .# j == "theta_1"@. (@.#@ is used because Haskell operator
--   symbols cannot contain @_@.)
infixl 9 .#
(.#) :: Text -> Int -> Text
(.#) = indexed

-- | Add an arbitrary log-probability term to the model (analogous to
-- PyMC's @pm.Potential@).
--
-- [日本語]: 通常のサンプリング/観測では表せない log-density 寄与を入れるのに
--   使う。 典型用途:
--
--   - __ソフト制約__: @potential \"order\" (if mu1 < mu2 then 0 else (-1e10))@
--   - __カスタム尤度__: 既存 'Distribution' で表せない尤度項
--   - __正則化__: ベイズ的な正則化 (e.g. ridge: @-0.5 * lambda * sum (map (^2) betas)@)
--
--   @Potential@ の値は @logJoint@ と @logPrior@ に加算される
--   (@logLikelihood@ には含まれない — これらは @observe@ 専用)。
--
--   [English]: Used to add log-density contributions that ordinary
--   sampling/observation cannot express. Typical uses:
--
--   - __Soft constraints__: @potential \"order\" (if mu1 < mu2 then 0 else (-1e10))@
--   - __Custom likelihoods__: likelihood terms not expressible with an existing 'Distribution'
--   - __Regularization__: Bayesian regularization (e.g. ridge: @-0.5 * lambda * sum (map (^2) betas)@)
--
--   @Potential@'s value is added to @logJoint@ and @logPrior@ (it is not
--   included in @logLikelihood@ — those are @observe@-only).
potential :: Text -> a -> Model a ()
potential nm v = liftF (Potential nm v ())

-- | [日本語]: 派生量を名前付きで保存する (PyMC `pm.Deterministic` 相当)。
--   log-joint には寄与しないが、 各 posterior サンプルごとに値が記録され
--   @augmentChainWithDeterministic@ で Chain に注入できる。
--   [English]: Saves a derived quantity under a name (equivalent to
--   PyMC's @pm.Deterministic@). It does not contribute to the log-joint,
--   but its value is recorded for each posterior draw and can be injected
--   into the Chain via @augmentChainWithDeterministic@.
--
-- 例 / Example:
--
-- > tau <- deterministic "tau" (1 / (sigma * sigma))
deterministic :: Text -> a -> Model a a
deterministic nm v = liftF (Deterministic nm v id)

-- | [日本語]: DAG / Node 表示用の分布名 (リンク逆関数を適用した観測分布の名前)。 [English]: The distribution name for DAG / Node display (the observation distribution's name after applying the inverse link).
lmFamilyName :: LMFamily -> Text
lmFamilyName (LMGaussian _) = "Normal"
lmFamilyName LMPoisson      = "Poisson"
lmFamilyName LMBernoulli    = "Bernoulli"

-- | [日本語]: 'ObserveLM' が参照する latent パラメタ名の集合 (DAG の親)。
--   β + ランダム効果 u + (Gaussian の) σ。
--   [English]: The set of latent parameter names 'ObserveLM' references
--   (the DAG's parents): β + random effects u + (for Gaussian) σ.
lmParents :: [Text] -> [REff] -> LMFamily -> Set Text
lmParents betaNames reffs fam =
  Set.fromList betaNames
  <> Set.fromList (concat [ uNames | REff uNames _ _ _ _ <- reffs ])
  -- Phase 62: gids の由来 slot 名 ('atIx' 経由) も親に = slot→観測ノードのエッジ
  <> Set.fromList [ s | REff _ _ _ _ (Just s) <- reffs ]
  <> case fam of
       LMGaussian sName -> Set.singleton sName
       LMPoisson        -> Set.empty
       LMBernoulli      -> Set.empty

-- ---------------------------------------------------------------------------
-- Phase 40-A1: Plate notation
-- ---------------------------------------------------------------------------

-- | [日本語]: Pyro / NumPyro 流の plate-block。
--   [English]: A Pyro-/NumPyro-style plate block.
--
-- [日本語]: @plate name n body@ は、 do-block 内で繰り返し作られる indexed RV 群
--   (e.g. @eta_0, eta_1, …, eta_{n-1}@) を __同じ plate に属する__ と
--   マークする bracket。 @buildModelGraph@ で plate 集約描画される。
--   [English]: @plate name n body@ is a bracket that marks the indexed RVs
--   repeatedly created inside a do-block (e.g. @eta_0, eta_1, …,
--   eta_{n-1}@) as __belonging to the same plate__. @buildModelGraph@
--   renders plates aggregated.
--
-- 例 (8-schools) / Example (8-schools):
--
-- > mu  <- sample "mu" (Normal 0 5)
-- > tau <- sample "tau" (HalfCauchy 5)
-- > etas <- plate "school" 8 $ forM [0..7] $ \j ->
-- >           sample ("eta_" <> T.pack (show j)) (Normal 0 1)
-- > _ <- plate "school" 8 $ forM_ [0..7] $ \j ->
-- >        observe ("y_" <> T.pack (show j))
-- >                (Normal (mu + tau * (etas !! j)) 1) [ys !! j]
--
-- [日本語]: 内部: 'PlateBegin' / 'PlateEnd' マーカーで囲む。 log eval (logJoint
--   / logPrior 等) は __透過__ に動作し、 plate は描画レイヤーでのみ
--   意味を持つ。 NUTS / Gibbs / VI への影響なし。
--   [English]: Internally, this wraps the body with 'PlateBegin' /
--   'PlateEnd' markers. Log evaluation (logJoint / logPrior, etc.) works
--   __transparently__ through it — plates only carry meaning at the
--   rendering layer, and have no effect on NUTS / Gibbs / VI.
plate :: Text -> Int -> Model a r -> Model a r
plate name n body = do
  liftF (PlateBegin name n ())
  r <- body
  liftF (PlateEnd ())
  return r

-- | [日本語]: 'plate' の利便 helper: @plateI name n f@ =
--   @plate name n (forM [0..n-1] f)@。 「N 個の indexed RV を作る」 という
--   最頻パターン向け糖衣。
--   [English]: A convenience helper over 'plate': @plateI name n f@ =
--   @plate name n (forM [0..n-1] f)@. Sugar for the most common pattern,
--   "create N indexed RVs."
--
-- 例 / Example:
--
-- > etas <- plateI "school" 8 $ \j ->
-- >           sample ("eta_" <> T.pack (show j)) (Normal 0 1)
plateI :: Text -> Int -> (Int -> Model a r) -> Model a [r]
plateI name n action = plate name n (forM [0 .. n - 1] action)

-- | [日本語]: 'plateI' の返り値を捨てる版 (@forM_@ の plate 版・index 反復)。
--   @plateI_ name n f = plate name n (forM_ [0..n-1] f)@。 観測のみの index
--   ループ向け (@plateForM_ name [0..n-1] f@ と同義だが index 反復の意図が明示的・
--   'plateForM' / 'plateForM_' の対称に合わせ index 版にも破棄形を用意)。
--   [English]: The value-discarding version of 'plateI' (the plate
--   version of @forM_@, index-driven). @plateI_ name n f = plate name n
--   (forM_ [0..n-1] f)@. For observation-only index loops (equivalent to
--   @plateForM_ name [0..n-1] f@, but makes index-driven iteration
--   explicit; provided so the index-based variant has a discarding form
--   symmetric with 'plateForM' / 'plateForM_').
--
-- 例 (8-schools の観測) / Example (8-schools observations):
--
-- > plateI_ "school" 8 $ \j ->
-- >   observe ("y" .# j) (Normal (mu + tau * etas !! j) 1) [ys !! j]
plateI_ :: Text -> Int -> (Int -> Model a r) -> Model a ()
plateI_ name n action = plate name n (forM_ [0 .. n - 1] action)

-- | [日本語]: データ行リストを plate で囲んで反復する糖衣 (@forM@ の plate
--   版・引数順も @forM@ 形)。 @plateForM name rows f = plate name (length rows)
--   (forM rows f)@。 plate サイズは行数から自動。 観測ループの定番
--   @plate name (length rows) $ forM_ … rows@ を畳む。
--   [English]: Sugar for iterating over a list of data rows wrapped in a
--   plate (the plate version of @forM@, argument order matches @forM@
--   too). @plateForM name rows f = plate name (length rows) (forM rows
--   f)@. The plate size is derived automatically from the row count,
--   folding the common observation-loop pattern
--   @plate name (length rows) $ forM_ … rows@.
--
-- 例 (ベイズ線形回帰の観測) / Example (Bayesian linear regression observations):
--
-- > plateForM_ "obs" (zip x y) $ \(xi, yi) -> do
-- >   mu <- deterministic "mu" (a + b * realToFrac xi)
-- >   observe "obs" (Normal mu s) [yi]
plateForM :: Text -> [b] -> (b -> Model a r) -> Model a [r]
plateForM name rows f = plate name (length rows) (forM rows f)

-- | [日本語]: 返り値を捨てる版 (@forM_@ の plate 版)。 観測のみのループに。 [English]: The value-discarding version (the plate version of @forM_@), for observation-only loops.
plateForM_ :: Text -> [b] -> (b -> Model a r) -> Model a ()
plateForM_ name rows f = plate name (length rows) (forM_ rows f)

-- | [日本語]: 低レベル plate API: 任意の Model action を plate スコープで包む。
--   'plate' は @withPlate name n@ + body の組合せに分解される。 nested
--   plate を独自構築する際の primitive。
--   [English]: The low-level plate API: wraps an arbitrary Model action in
--   a plate scope. 'plate' decomposes into @withPlate name n@ + body; this
--   is the primitive for building custom nested plates.
withPlate :: Text -> Int -> Model a r -> Model a r
withPlate = plate

-- | [日本語]: 名前付きデータプレースホルダを宣言する (PyMC `pm.Data` 相当)。
--   既定値 @ys@ を持ち、 後で 'withData' により差し替え可能。
--   [English]: Declares a named data placeholder (equivalent to PyMC's
--   @pm.Data@). Has a default value @ys@, later swappable via 'withData'.
--
-- 典型的な使い方 / Typical usage:
--
-- > model = do
-- >   y <- dataNamed "y" trainData
-- >   mu <- sample "mu" (Normal 0 5)
-- >   observe "y" (Normal mu 1) y
--
-- [日本語]: そして @withData \"y\" testData model@ で同じ構造で別データを使う。
--   [English]: Then @withData \"y\" testData model@ reuses the same
--   structure with different data.
--
-- [日本語]: ★破壊的変更: 戻り値は @[a]@ (モデルの数値型)。 受け取った値は
--   そのまま式に入る (@realToFrac@ 不要)。 @a@ には @Real@ 制約が無いので、
--   旧コードの @realToFrac xi@ は型エラーになる (= 無言の挙動変化が起きない
--   壊れ方)。 機械的に @realToFrac@ を消せば移行完了。
--   観測値として 'observe' に渡す側 (@[Double]@ が要る) は 'dataNamedObs' を使う。
--   [English]: ★A breaking change: the return type is @[a]@ (the model's
--   numeric type). The received value flows directly into expressions (no
--   @realToFrac@ needed). Since @a@ carries no @Real@ constraint, old code
--   with @realToFrac xi@ now fails to type-check (a loud failure, not a
--   silent behavior change). Migration is complete once @realToFrac@ is
--   mechanically removed. Use 'dataNamedObs' for the side that passes
--   observed values to 'observe' (which needs @[Double]@).
dataNamed :: Text -> [Double] -> Model a [a]
dataNamed n ys = liftF (Data n ys fst)

-- | [日本語]: 'dataNamed' の同義。 役割 suffix 三点セットの正書き:
--   [English]: A synonym for 'dataNamed'. The canonical spelling of the
--   three role-suffixed variants:
--
-- > x  <- dataNamedX   "x" []   -- 説明変数 / covariate: モデル数値型 [a]
-- > ys <- dataNamedObs "y" []   -- 目的変数 / response: 生 [Double] ('observe' へ)
-- > gs <- dataNamedIx  "g" []   -- 群 index / group index: [Int]
--
-- [日本語]: 既存コードの 'dataNamed' もそのまま使える (削除予定なし)。
-- [English]: Existing code's 'dataNamed' also still works as-is (no plan
-- to remove it).
dataNamedX :: Text -> [Double] -> Model a [a]
dataNamedX = dataNamed

-- | [日本語]: 'dataNamed' と同じ slot の __観測値 view__ (生 @[Double]@)。
--   'observe' / 'observeLM' の観測値引数は AD に持ち上げない @[Double]@ 固定
--   なので、 y 側のデータ slot はこちらで受ける:
--   [English]: The __observed-value view__ (raw @[Double]@) of the same
--   slot as 'dataNamed'. Since 'observe' / 'observeLM''s observed-value
--   argument is always @[Double]@ and never lifted to AD, y-side data
--   slots should be received this way:
--
-- > x  <- dataNamed    "x" []   -- covariate: モデル数値型 [a]
-- > ys <- dataNamedObs "y" []   -- 観測値 / observed value: 生 [Double]
-- > ...
-- > observe "y" (Normal mu s) ys
--
-- [日本語]: 同名 slot を 'dataNamed' と 'dataNamedObs' の両 view で読んでもよい
--   (差し替えは 'withData' / 列 bind が slot 名単位で行うため一貫する)。
--   [English]: The same-named slot may be read through both the
--   'dataNamed' and 'dataNamedObs' views — this stays consistent because
--   swapping (via 'withData' / column binding) operates per slot name.
dataNamedObs :: Text -> [Double] -> Model a [Double]
dataNamedObs n ys = liftF (Data n ys snd)

-- | [日本語]: 離散 index 専用のデータプレースホルダ (後に 'Ix' 戻りへ刷新)。
--   群 index 等を slot 名タグ付き index 'Ix' で運ぶ。 @bs '!!!' g@ で引くと
--   DAG に slot→利用先のエッジが自動で出る (PyMC の @b0[gid]@ 同型)。
--   'Ix' は Num でないので誤って算術に混ぜると型エラーで止まる
--   (= 連続値経路の round 罠を根治し、 以後も維持)。
--   [English]: A data placeholder dedicated to discrete indices (later
--   revised to return 'Ix'). Carries group indices, etc. as a slot-name-
--   tagged index 'Ix'. Indexing with @bs '!!!' g@ automatically emits a
--   slot→use-site edge in the DAG (matching PyMC's @b0[gid]@). Since 'Ix'
--   is not a @Num@, accidentally mixing it into arithmetic fails at
--   compile time (this permanently closes the continuous-value rounding
--   trap).
--
-- > gs <- dataNamedIx "g" [0,0,1,1,2]
-- > let mu_i = b0s !!! g   -- round 不要 / no round needed・DAG に g→mu エッジ
dataNamedIx :: Text -> [Int] -> Model a [Ix]
dataNamedIx n is = liftF (DataIx n is (map (\i -> Ix i (Just n))))

-- | [日本語]: slot 名タグ付き離散 index。 'dataNamedIx' が返し、 '!!!' で
--   使う。 由来 slot 名 ('ixSlot') は DAG 抽出 (Track 解釈) のエッジ生成にだけ
--   使われ、 数値評価では 'ixVal' のみが意味を持つ。
--   [English]: A slot-name-tagged discrete index, returned by
--   'dataNamedIx' and consumed by '!!!'. The originating slot name
--   ('ixSlot') is used only for edge generation in DAG extraction (the
--   @Track@ interpretation); numeric evaluation looks only at 'ixVal'.
data Ix = Ix
  { ixVal  :: !Int          -- ^ [日本語]: index 本体 (0..nG-1)。 [English]: the index value itself (0..nG-1).
  , ixSlot :: !(Maybe Text) -- ^ [日本語]: 由来 slot 名 ('dataNamedIx' なら Just)。 [English]: the originating slot name (@Just@ when from 'dataNamedIx').
  } deriving (Show, Eq)

-- | [日本語]: 解釈ごとの依存タグ注入。 既定 = 何もしない (数値解釈は
--   ゼロコスト・サンプリングはビット不変)。 @Track@ 解釈だけが override して
--   依存集合に slot 名を足し、 DAG にエッジを出す。
--   [English]: Injects a dependency tag, per interpretation. The default
--   does nothing (numeric evaluation pays zero cost; sampling is bit-
--   identical). Only the @Track@ interpretation overrides it, adding the
--   slot name to the dependency set and emitting a DAG edge.
class TrackTag a where
  tagDep :: Text -> a -> a
  tagDep _ = id
  {-# INLINE tagDep #-}

instance TrackTag Double

-- dogfood 典型 (群別係数のタプル) 用: 成分ごとに伝播
instance (TrackTag a, TrackTag b) => TrackTag (a, b) where
  tagDep nm (a, b) = (tagDep nm a, tagDep nm b)
instance (TrackTag a, TrackTag b, TrackTag c) => TrackTag (a, b, c) where
  tagDep nm (a, b, c) = (tagDep nm a, tagDep nm b, tagDep nm c)
instance (TrackTag a, TrackTag b, TrackTag c, TrackTag d)
      => TrackTag (a, b, c, d) where
  tagDep nm (a, b, c, d) = (tagDep nm a, tagDep nm b, tagDep nm c, tagDep nm d)

-- | [日本語]: slot 名タグ付き索引。 @bs '!!!' g@ = @bs !! ixVal g@ に、
--   Track 解釈でのみ g の由来 slot 名を依存タグとして注入する
--   (= DAG に slot→利用先エッジ。 数値解釈は '!!' と同コスト)。
--   [English]: Slot-name-tagged indexing. @bs '!!!' g@ is @bs !! ixVal g@,
--   with g's originating slot name injected as a dependency tag only
--   under the @Track@ interpretation (emitting a slot→use-site edge in
--   the DAG; numeric evaluation costs the same as '!!').
(!!!) :: TrackTag b => [b] -> Ix -> b
xs !!! Ix i ms = maybe id tagDep ms (xs !! i)
infixl 9 !!!
{-# INLINE (!!!) #-}

-- | [日本語]: 'at' の 'Ix' 版。 'dataNamedIx' の gids を random effect の
--   gather に渡す。 先頭 'Ix' の由来 slot 名 ('ixSlot') を 'REff' に
--   載せるので、 DAG に slot→観測ノードのエッジが出る (gather の gids は単一
--   slot 由来が通常形ゆえ先頭で代表)。 '!!!' (deterministic μ 経路) と並ぶ
--   PyMC @b0[gid]@ 同型の両経路対応。
--   [English]: The 'Ix' version of 'at'. Passes 'dataNamedIx''s gids to a
--   random effect's gather. Carries the first 'Ix''s originating slot name
--   ('ixSlot') onto 'REff', so the DAG gets a slot→observation-node edge
--   (since a gather's gids typically come from a single slot, the first
--   one is taken as representative). Covers both paths matching PyMC's
--   @b0[gid]@, alongside '!!!' (the deterministic-μ path).
atIx :: REffect a -> [Ix] -> REff
atIx re gids =
  REff (reffNames re) (map ixVal gids) (Just (reffScale re)) Nothing
       (case gids of { Ix _ ms : _ -> ms; [] -> Nothing })

-- | Replace a named data block in the model. If no match exists the
-- model is returned unchanged.
--
-- [日本語]: 同じ名前が複数回出現する場合は全箇所で差し替わる。
--   型シグネチャは @Model a r@ なので、 ユーザーが @ModelP r@ から呼ぶ場合
--   そのまま多相的に使える (各 @a@ で個別に適用される)。
--   [English]: If the same name occurs multiple times, all occurrences are
--   replaced. Since the type signature is @Model a r@, users calling from
--   @ModelP r@ can use it polymorphically as-is (applied separately for
--   each @a@).
withData :: forall r. Text -> [Double] -> ModelP r -> ModelP r
withData n new m = mPoly
  where
    -- 戻り値を多相モデルとして再構築。各 @a@ 個別に元の m を走査する。
    mPoly :: forall a. (Floating a, Ord a, TrackTag a) => Model a r
    mPoly = go m
      where
        go :: Model a r -> Model a r
        go (Pure r) = Pure r
        go (Free f) = Free (case f of
          Data n' ys k
            | n == n'   -> Data n' new (\d -> go (k d))
            | otherwise -> Data n' ys  (\d -> go (k d))
          DataIx n' is k       -> DataIx n' is (\d -> go (k d))
          Sample nm d k        -> Sample nm d (\v -> go (k v))
          Observe nm d ys nx   -> Observe nm d ys (go nx)
          ObserveLM nm bs xs re fam ys nx -> ObserveLM nm bs xs re fam ys (go nx)
          Potential nm v nx    -> Potential nm v (go nx)
          Deterministic nm v k -> Deterministic nm v (\v' -> go (k v'))
          PlateBegin nm sz nx  -> PlateBegin nm sz (go nx)
          PlateEnd nx          -> PlateEnd (go nx))

-- | [日本語]: 'withData' の離散 index 版: 名前付き @DataIx@ ブロックを
--   外部から差し替える。 一致しなければモデルは不変。
--   [English]: The discrete-index version of 'withData': externally
--   replaces a named @DataIx@ block. If nothing matches, the model is
--   returned unchanged.
withDataIx :: forall r. Text -> [Int] -> ModelP r -> ModelP r
withDataIx n new m = mPoly
  where
    mPoly :: forall a. (Floating a, Ord a, TrackTag a) => Model a r
    mPoly = go m
      where
        go :: Model a r -> Model a r
        go (Pure r) = Pure r
        go (Free f) = Free (case f of
          DataIx n' is k
            | n == n'   -> DataIx n' new (\d -> go (k d))
            | otherwise -> DataIx n' is  (\d -> go (k d))
          Data n' ys k         -> Data n' ys (\d -> go (k d))
          Sample nm d k        -> Sample nm d (\v -> go (k v))
          Observe nm d ys nx   -> Observe nm d ys (go nx)
          ObserveLM nm bs xs re fam ys nx -> ObserveLM nm bs xs re fam ys (go nx)
          Potential nm v nx    -> Potential nm v (go nx)
          Deterministic nm v k -> Deterministic nm v (\v' -> go (k v'))
          PlateBegin nm sz nx  -> PlateBegin nm sz (go nx)
          PlateEnd nx          -> PlateEnd (go nx))

-- | Latent multivariate-normal vector (analogous to PyMC's
-- @pm.MvNormal@ used as a latent).
--
-- [日本語]: 非中心化パラメタ化 + Cholesky 分解で実装:
--   [English]: Implemented via non-centered parameterization + Cholesky
--   decomposition:
--
--   z_i ~ Normal(0, 1)  (i = 0..K-1, 独立な latent / independent latents)
--   x   = μ + L z       (L = Cholesky(Σ))
--
-- [日本語]: 各 z_i は通常の latent として NUTS が探索し、 x は派生量として
--   Chain に記録される。 共分散行列が他の latent に依存する形でも
--   動作する (choleskyL は @(Floating a, Ord a)@ 多相)。
--   [English]: Each z_i is explored by NUTS as an ordinary latent, and x
--   is recorded in the Chain as a derived quantity. This also works when
--   the covariance matrix depends on other latents (@choleskyL@ is
--   polymorphic over @(Floating a, Ord a)@).
--
-- [日本語]: 共分散が非正定値のときは μ をそのまま返す (NUTS 探索中の不正領域
--   に対する graceful fallback)。
--   [English]: When the covariance is not positive-definite, μ is returned
--   as-is (a graceful fallback for invalid regions visited during NUTS
--   exploration).
--
-- [日本語]: 戻り値: K 次元 latent ベクトル @[a]@ (μ + L z)。 Chain には
--   @<name>_z<i>@ (raw latent) と @<name>_<i>@ (派生量) を保存。
--   [English]: Returns: the K-dimensional latent vector @[a]@ (μ + L z).
--   The Chain stores @<name>_z<i>@ (the raw latent) and @<name>_<i>@ (the
--   derived quantity).
mvNormalLatent :: forall a. (Floating a, Ord a)
               => Text -> [a] -> [[a]] -> Model a [a]
mvNormalLatent name muVec covMatrix = do
  let k = length muVec
  zs <- mapM (\i -> sample (name <> "_z" <> T.pack (show i)) (Normal 0 1))
             [0 .. k - 1]
  let xs = case choleskyL covMatrix of
        Just l  -> [ (muVec !! i) +
                       sum [ ((l !! i) !! j) * (zs !! j)
                           | j <- [0 .. i] ]
                   | i <- [0 .. k - 1] ]
        Nothing -> muVec      -- non-PD のフォールバック
  mapM
    (\(i, x) -> deterministic (name <> "_" <> T.pack (show i)) x)
    (zip [0 :: Int ..] xs)

-- | [日本語]: LKJ 相関行列の Cholesky factor (PyMC @LKJCholeskyCov@ 相当)。
--   [English]: The Cholesky factor of an LKJ correlation matrix
--   (equivalent to PyMC's @LKJCholeskyCov@).
--
-- [日本語]: LKJ(η) 事前分布: p(R) ∝ |R|^(η-1)。 η = 1 で uniform、
--   η > 1 で I に集中。
--   [English]: LKJ(η) prior: p(R) ∝ |R|^(η-1). Uniform at η = 1,
--   concentrating toward I as η > 1.
--
-- [日本語]: 実装は canonical partial correlations (CPC) 法:
--   [English]: Implemented via the canonical partial correlations (CPC)
--   method:
--
--   z_ij ~ scaled Beta(α_i, α_i) on (-1, 1),  α_i = η + (K - i - 1) / 2
--     (i = 1..K-1, j = 0..i-1)
--
-- [日本語]: 各 z_ij は @<name>_pc<i>_<j>@ (Beta latent in (0,1)、内部で
--   2u-1 に変換) として保存。 Cholesky factor の各要素は派生量
--   @<name>_L<i>_<j>@。
--   [English]: Each z_ij is stored as @<name>_pc<i>_<j>@ (a Beta latent in
--   (0,1), converted internally to 2u-1). Each Cholesky factor element is
--   the derived quantity @<name>_L<i>_<j>@.
--
-- [日本語]: 戻り値: K×K 下三角行列 L (R = L Lᵀ となる相関の Cholesky)。
--   対角は √(1 - Σ z_{i,k}²)、対角下は z_ij × √(Π_{k<j}(1-z_{i,k}²))。
--   [English]: Returns: the K×K lower-triangular matrix L (the Cholesky
--   factor of the correlation, R = L Lᵀ). The diagonal is
--   √(1 - Σ z_{i,k}²), and below-diagonal entries are
--   z_ij × √(Π_{k<j}(1-z_{i,k}²)).
lkjCorrCholesky :: forall a. (Floating a, Ord a)
                => Text -> Int -> a -> Model a [[a]]
lkjCorrCholesky name k eta
  | k < 2     = error "lkjCorrCholesky: dimension must be >= 2"
  | otherwise = do
      -- 各 (i, j) で 1 <= j < i <= K-1 の partial correlation を sample
      let pcIndices = [(i, j) | i <- [1 .. k - 1], j <- [0 .. i - 1]]
      pcs <- mapM
        (\(i, j) -> do
            let alpha = eta + fromIntegral (k - i - 1) / 2
                tag   = T.pack (show i) <> "_" <> T.pack (show j)
            u <- sample (name <> "_u" <> tag) (Beta alpha alpha)
            deterministic (name <> "_pc" <> tag) (2 * u - 1))
        pcIndices
      -- (i,j) → z_ij マップ
      let pcMap = zip pcIndices pcs
          lookupPC i j = head [v | ((ii, jj), v) <- pcMap, ii == i, jj == j]
      -- Cholesky factor を構築 (下三角)
      let lRow i =
            [ if j > i then 0
              else if i == 0 && j == 0 then 1
              else if j == i  -- 対角
                   then sqrt (1 - sum [ let z = lookupPC i kk
                                        in z * z | kk <- [0 .. i - 1] ])
              else            -- 対角下 j < i
                let z       = lookupPC i j
                    factor2 = product [ let z' = lookupPC i kk
                                        in 1 - z' * z' | kk <- [0 .. j - 1] ]
                in z * sqrt factor2
            | j <- [0 .. k - 1] ]
          lMat = [lRow i | i <- [0 .. k - 1]]
      -- L 各要素を deterministic として保存
      _ <- mapM
        (\(i, j) ->
          deterministic (name <> "_L" <> T.pack (show i) <> "_" <> T.pack (show j))
                        ((lMat !! i) !! j))
        [(i, j) | i <- [0 .. k - 1], j <- [0 .. i]]
      return lMat

-- | [日本語]: RBF (exponentiated quadratic) カーネルによる GP 共分散行列
--   (Stan @gp_exp_quad_cov(x, alpha, rho)@ 相当)。
--   [English]: A GP covariance matrix via the RBF (exponentiated
--   quadratic) kernel (equivalent to Stan's
--   @gp_exp_quad_cov(x, alpha, rho)@).
--
-- [日本語]: @K[i][j] = alpha^2 * exp(-0.5 * (x_i - x_j)^2 / rho^2)@、対角には
--   数値安定化の jitter (1e-10) を加える (Stan 原典の
--   @+ diag_matrix(rep_vector(1e-10, N))@ に対応)。 @x@ は 'dataNamedX' で
--   束縛した @[a]@ をそのまま渡す (data とハイパーパラメータ alpha/rho は
--   共に @a@ 型なので realToFrac 不要)。
--   [English]: @K[i][j] = alpha^2 * exp(-0.5 * (x_i - x_j)^2 / rho^2)@,
--   with a numerical-stability jitter (1e-10) added on the diagonal
--   (corresponding to Stan's original
--   @+ diag_matrix(rep_vector(1e-10, N))@). @x@ can be passed straight
--   through as the @[a]@ bound by 'dataNamedX' (no @realToFrac@ needed
--   since both the data and the alpha/rho hyperparameters share type @a@).
--
-- [日本語]: vecIR (per-row 独立項の和が前提) には密行列が構造的に載らない
--   ため、 legacy walk+ad 経路 (@grad fFull@) で使う想定の孤立関数。
--   [English]: A dense matrix cannot structurally fit vecIR (which assumes
--   a sum of per-row-independent terms), so this is an isolated function
--   intended for use via the legacy walk+ad path (@grad fFull@).
gpExpQuadCov :: forall a. Floating a => [a] -> a -> a -> [[a]]
gpExpQuadCov xs alpha rho =
  [ [ let d = xi - xj
      in alpha * alpha * exp (negate 0.5 * d * d / (rho * rho))
           + (if i == j then 1e-10 else 0)
    | (j, xj) <- zip [0 :: Int ..] xs ]
  | (i, xi) <- zip [0 :: Int ..] xs ]

-- | [日本語]: Gaussian Process 潜在関数 (Stan の non-centered GP
--   パラメタ化相当):
--   [English]: A Gaussian Process latent function (equivalent to Stan's
--   non-centered GP parameterization):
--
-- > f_tilde ~ Normal(0, 1)     (各点独立 / independent per point)
-- > L_cov = cholesky_decompose(gp_exp_quad_cov(x, alpha, rho))
-- > f = L_cov * f_tilde
--
-- [日本語]: 既存の 'choleskyL' ('mvNormalLatent' と同じ AD 対応 Cholesky
--   分解) をそのまま流用する。 共分散が非正定値のときは全ゼロに
--   フォールバックする ('mvNormalLatent' と同型の graceful fallback)。
--   [English]: Reuses the existing 'choleskyL' (the same AD-compatible
--   Cholesky decomposition as 'mvNormalLatent'). When the covariance is
--   not positive-definite, falls back to all zeros (a graceful fallback
--   of the same form as 'mvNormalLatent').
--
-- [日本語]: 戻り値: N 次元 latent ベクトル @[a]@ (GP 事後関数値 f)。 各要素は
--   @<name>_f<i>@ として deterministic 保存される。
--   [English]: Returns: the N-dimensional latent vector @[a]@ (the GP
--   posterior function value f). Each element is stored as a
--   deterministic under @<name>_f<i>@.
gpLatent :: forall a. (Floating a, Ord a)
         => Text -> [a] -> a -> a -> Model a [a]
gpLatent name xs alpha rho = do
  let n = length xs
  ftilde <- mapM (\i -> sample (name <> "_ftilde" <> T.pack (show i)) (Normal 0 1))
                 [0 .. n - 1]
  let cov = gpExpQuadCov xs alpha rho
      fs = case choleskyL cov of
        Just l  -> [ sum [ (l !! i !! j) * (ftilde !! j) | j <- [0 .. i] ]
                   | i <- [0 .. n - 1] ]
        Nothing -> replicate n 0    -- non-PD のフォールバック
  mapM
    (\(i, f) -> deterministic (name <> "_f" <> T.pack (show i)) f)
    (zip [0 :: Int ..] fs)

-- | [日本語]: AR(1) latent 時系列 (PyMC `pm.AR1` 相当)。
--   [English]: An AR(1) latent time series (equivalent to PyMC's
--   @pm.AR1@).
--
-- [日本語]: 状態方程式:  x_t = ϕ x_{t−1} + ε_t,   ε_t ~ Normal(0, σ)
--   初期分布:    x_0 ~ Normal(0, σ / √(1 − ϕ²))   (定常分布、 |ϕ| < 1 なら有限)
--   [English]: State equation: x_t = ϕ x_{t−1} + ε_t, ε_t ~ Normal(0, σ).
--   Initial distribution: x_0 ~ Normal(0, σ / √(1 − ϕ²)) (the stationary
--   distribution, finite when |ϕ| < 1).
--
-- [日本語]: 引数 @phi@ は AR 係数、 @sigma@ は innovation の sd。 N 個の
--   latent 状態 x_0 .. x_{N-1} を非中心化パラメタ化で sample する:
--   [English]: The @phi@ argument is the AR coefficient, and @sigma@ is
--   the innovation's sd. Samples N latent states x_0 .. x_{N-1} using
--   non-centered parameterization:
--
--   raw_t ~ Normal(0, 1)
--   x_t = phi * x_{t-1} + sigma * raw_t       (t > 0)
--   x_0 = (sigma / √(1 - ϕ²)) * raw_0
--
-- [日本語]: 戻り値: x_0 .. x_{N-1} の latent 値リスト ([a])。 各 raw_t は
--   @<name>_raw<t>@、 x_t 自体は派生量 @<name>_<t>@ として保存。
--   [English]: Returns: the list of latent values x_0 .. x_{N-1} ([a]).
--   Each raw_t is stored as @<name>_raw<t>@, and x_t itself as the derived
--   quantity @<name>_<t>@.
--
-- [日本語]: |ϕ| ≥ 1 のフォールバック: 初期 sd を sigma に置き換える。
-- [English]: Fallback for |ϕ| ≥ 1: replaces the initial sd with sigma.
ar1Latent :: forall a. (Floating a, Ord a)
          => Text -> Int -> a -> a -> Model a [a]
ar1Latent name nT phi sigma
  | nT < 1 = error "ar1Latent: length must be >= 1"
  | otherwise = do
      raws <- mapM
        (\t -> sample (name <> "_raw" <> T.pack (show t)) (Normal 0 1))
        [0 .. nT - 1]
      let phi2     = phi * phi
          stat     = if phi2 < 1
                       then sigma / sqrt (1 - phi2)
                       else sigma   -- フォールバック
      -- Phase 38: scanl で xs を先に組み立てると、 各 x_t の Track が
      -- {x_raw0, …, x_raw_t} という遠い親集合を保持してしまい、 後で
      -- deterministic 登録しても下流の親が plate-style にならない。
      -- 各 step で deterministic の戻り値 (det 名で再ラベルされた Track)
      -- を次の step に渡す monadic recursion で組む。
      x0 <- deterministic (name <> "_0") (stat * head raws)
      let chain _    []           = return []
          chain xPrev ((t, rt):rest) = do
            xt <- deterministic
                    (name <> "_" <> T.pack (show t))
                    (phi * xPrev + sigma * rt)
            xs' <- chain xt rest
            return (xt : xs')
      xs' <- chain x0 (zip [(1 :: Int) .. ] (tail raws))
      return (x0 : xs')

-- | [日本語]: 非中心化 (non-centered) 正規分布。
--   [English]: A non-centered normal distribution.
--
-- [日本語]: @x ~ Normal(loc, scale)@ を直接サンプリングする代わりに、
--   [English]: Instead of sampling @x ~ Normal(loc, scale)@ directly, it
--   expands to:
--
-- > raw <- sample (name <> "_raw") (Normal 0 1)
-- > deterministic name (loc + scale * raw)
--
-- [日本語]: loc / scale が他の latent に依存するとき、 centered
--   パラメタ化は HMC の posterior が病的になりやすいので、 それを
--   緩和するヘルパ。 Neal's funnel が代表例。
--   [English]: When loc / scale depend on other latents, centered
--   parameterization tends to make HMC's posterior pathological; this
--   helper mitigates that. Neal's funnel is the canonical example.
--
-- [日本語]: 戻り値は constrained な値 @loc + scale * raw@。 Chain には
--   @<name>_raw@ (latent) と @<name>@ (derived) の両方が保存される。
--   [English]: Returns the constrained value @loc + scale * raw@. Both
--   @<name>_raw@ (the latent) and @<name>@ (the derived quantity) are
--   stored in the Chain.
nonCenteredNormal :: Num a => Text -> a -> a -> Model a a
nonCenteredNormal name loc scale = do
  raw <- sample (name <> "_raw") (Normal 0 1)
  deterministic name (loc + scale * raw)

-- | [日本語]: 'glmmRandomIntercept' の GLMM family。 [English]: The GLMM family for 'glmmRandomIntercept'.
data GlmmFamily
  = GlmmGaussian   -- ^ [日本語]: 連続 y、 残差 SD @sigma@ も sample される。 [English]: continuous y; the residual SD @sigma@ is also sampled.
  | GlmmBinomial   -- ^ [日本語]: 0/1 y、 Bernoulli(σ(η))。 [English]: 0/1 y, Bernoulli(σ(η)).
  | GlmmPoisson    -- ^ [日本語]: 非負整数 y、 Poisson(exp η)。 [English]: non-negative integer y, Poisson(exp η).
  deriving (Show, Eq)

-- | [日本語]: Random intercept GLMM helper。
--   [English]: A random-intercept GLMM helper.
--
-- [日本語]: `y ~ X β + u_{group(i)} + (error)` を 1 関数で組み立てる:
--   [English]: Assembles `y ~ X β + u_{group(i)} + (error)` in a single
--   function:
--
-- - 固定効果 @β_k ~ Normal(0, 5)@ (p 個) / fixed effects @β_k ~ Normal(0, 5)@ (p of them)
-- - 群レベル SD @τ_u ~ HalfNormal(5)@ / group-level SD @τ_u ~ HalfNormal(5)@
-- - 群効果 @u_j ~ Normal(0, τ_u)@ (nG 個、 centered パラメタ化。
--   群数大 / 群内 N 小なら別途 'nonCenteredNormal' を直接使う) /
--   group effects @u_j ~ Normal(0, τ_u)@ (nG of them, centered
--   parameterization; for many groups / small within-group N, use
--   'nonCenteredNormal' directly instead)
-- - family に応じた観測 / observation depending on the family:
--     - Gaussian: 残差 @σ ~ Exp(1)@ を sample → @y ~ Normal(X β + u_j, σ)@ /
--       sample the residual @σ ~ Exp(1)@ → @y ~ Normal(X β + u_j, σ)@
--     - Binomial: @y ~ Bernoulli(σ(X β + u_j))@、 y は 0/1 /
--       @y ~ Bernoulli(σ(X β + u_j))@, y is 0/1
--     - Poisson:  @y ~ Poisson(exp(X β + u_j))@、 y は非負整数 /
--       @y ~ Poisson(exp(X β + u_j))@, y is a non-negative integer
--
-- [日本語]: 観測は単一の構造化ブロック @observeLMR \"y\"@ として発行される
--   (PyMC/Stan と同じく 1 ベクトル化観測ノード。 旧実装は per-obs @y_i@ を
--   n 個展開)。 固定効果は密設計行列・群効果は gather で表現するので
--   vec-tape ハイブリッド gradADU の高速経路に乗る。 chain 上の latent 名:
--   @beta_0, …, beta_{p-1}, tau_u, u_0, …, u_{nG-1}, sigma?@.
--   [English]: The observation is emitted as a single structured block
--   @observeLMR \"y\"@ (one vectorized observation node, matching
--   PyMC/Stan; the earlier implementation expanded per-obs @y_i@ into n
--   nodes). Fixed effects are represented with a dense design matrix and
--   group effects with a gather, so this rides the fast path of the
--   vec-tape hybrid gradADU. Latent names in the chain:
--   @beta_0, …, beta_{p-1}, tau_u, u_0, …, u_{nG-1}, sigma?@.
--
-- [日本語]: 個別 (random slope や non-centered) が必要ならパターン 5
--   (random slope) / 形式 C (non-centered) を直接書く方が柔軟。 本 helper
--   は最頻ユースケース 「固定効果 + 群別切片」 専用の shorthand。
--   [English]: For custom needs (random slopes, non-centered), writing
--   Pattern 5 (random slope) / Form C (non-centered) directly is more
--   flexible. This helper is shorthand dedicated to the most common use
--   case, "fixed effects + per-group intercept."
glmmRandomIntercept
  :: forall a. (Floating a, Ord a)
  => GlmmFamily   -- ^ [日本語]: 尤度の family。 [English]: the likelihood family.
  -> [[Double]]   -- ^ [日本語]: 固定効果 design X (n × p)、 切片は手で 1 列追加すること。 [English]: the fixed-effect design X (n × p); add an intercept column by hand.
  -> [Int]        -- ^ [日本語]: 各観測の group id (0..nG-1)。 [English]: each observation's group id (0..nG-1).
  -> [Double]     -- ^ [日本語]: 観測 y (length n)。 [English]: the observed y (length n).
  -> Model a ()
glmmRandomIntercept fam xRows gids ys = do
  let n  = length ys
      p  = if null xRows then 0 else length (head xRows)
      nG = if null gids then 0 else maximum gids + 1
  -- 固定効果
  betas <- forM [0 .. p - 1] $ \k ->
    sample (T.pack ("beta_" ++ show k)) (Normal 0 5)
  -- 群レベル SD
  tauU <- sample "tau_u" (HalfNormal 5)
  -- 群別切片を第一級ランダム効果値として宣言 (Phase 54.4c)。 reNormal が
  -- u_0..u_{nG-1} ~ Normal(0, tauU) を sample しつつスケール名 "tau_u" を構造に
  -- 載せるので、 観測に `at` で gather すると compileGradU の **解析 prior 勾配**
  -- 経路に乗り、 prior の O(nG) スカラ ad が排除される。
  u <- reNormal "u" nG "tau_u" tauU
  -- Gaussian のみ残差 SD
  _mSig <- case fam of
    GlmmGaussian -> Just <$> sample "sigma" (Exponential 1)
    _            -> return Nothing
  -- 観測は単一の構造化ブロック (observeLMR) として発行する (Phase 54.4a)。
  -- η_i = Σ_k β_k X_ik + u_{g(i)} を固定効果 (密設計行列) + 群効果 (gather) で
  -- 表現するので、 vec-tape ハイブリッド gradADU の高速経路に乗る。 PyMC/Stan と
  -- 同じく観測は 1 ベクトル化ノード "y" (旧: per-obs y_i を n 個展開)。
  let betaNames = [ T.pack ("beta_" ++ show k) | k <- [0 .. p - 1] ]
      reffs     = [ u `at` gids ]
      lmFam     = case fam of
        GlmmGaussian -> LMGaussian "sigma"
        GlmmBinomial -> LMBernoulli
        GlmmPoisson  -> LMPoisson
  -- betas/n は名前参照ゆえ値は使わないが、 latent 宣言として必要。
  _ <- pure (betas, n)
  observeLMR "y" betaNames xRows reffs lmFam ys

-- | Dirichlet distribution (analogous to PyMC's @pm.Dirichlet@), expanded
-- via stick-breaking into a
-- [日本語]: latent ベクトル。 [English]: latent vector.
--
-- [日本語]: 引数:
--   [English]: Arguments:
--
--   - @name@   : ベース名。 展開後は @<name>_b<i>@ (i=0..K-2) が Beta 由来の
--                棒折り変数、 @<name>_<i>@ (i=0..K-1) が deterministic で
--                記録された π 成分。 /
--                the base name. After expansion, @<name>_b<i>@ (i=0..K-2)
--                are the Beta-derived stick-breaking variables, and
--                @<name>_<i>@ (i=0..K-1) are the π components recorded as
--                deterministics.
--   - @alphas@ : 集中度ベクトル α = (α_1,...,α_K)。 長さ K ≥ 2。 /
--                the concentration vector α = (α_1,...,α_K), length K ≥ 2.
--
-- [日本語]: アルゴリズム:
--   k = 1..K-1 で β_k ~ Beta(α_k, Σ_{j>k} α_j) を sample する。
--   π_1 = β_1,  π_k = β_k Π_{j<k} (1 − β_j),  π_K = Π_{j<K} (1 − β_j)
--   [English]: Algorithm: for k = 1..K-1, sample
--   β_k ~ Beta(α_k, Σ_{j>k} α_j). Then
--   π_1 = β_1,  π_k = β_k Π_{j<k} (1 − β_j),  π_K = Π_{j<K} (1 − β_j).
--
-- [日本語]: これは π ~ Dirichlet(α) と厳密に等価なので、 追加の Jacobian
--   補正は不要。 HMC/NUTS では β_k が UnitIntervalT (logit) で自動的に
--   (0,1) ↔ ℝ 変換されるので、 シンプレックス制約は満たされる。
--   [English]: This is exactly equivalent to π ~ Dirichlet(α), so no
--   additional Jacobian correction is needed. Under HMC/NUTS, β_k is
--   automatically transformed (0,1) ↔ ℝ via UnitIntervalT (logit), so the
--   simplex constraint is satisfied.
dirichlet :: forall a. (Floating a, Ord a) => Text -> [a] -> Model a [a]
dirichlet name alphas = do
  let k = length alphas
  if k < 2
    then error "dirichlet: 長さ 2 未満のベクトルは未対応"
    else do
      let -- α_k+1..K の累積和 (右から)。長さ K (最後の要素は 0)
          tailSums = scanr (+) 0 alphas
      -- β_0..β_{K-2} を sample
      betas <- mapM
        (\i -> sample (name <> "_b" <> T.pack (show i))
                      (Beta (alphas !! i) (tailSums !! (i + 1))))
        [0 .. k - 2]
      -- 残り棒の累積積 prods[i] = Π_{j<i} (1 - β_j),  prods[0] = 1
      let prods = scanl (\acc b -> acc * (1 - b)) (1 :: a) betas
          -- π_i = β_i * prods[i] for i < K-1, π_{K-1} = prods[K-1]
          pis = [ if i < length betas
                    then (betas !! i) * (prods !! i)
                    else prods !! i
                | i <- [0 .. k - 1] ]
      -- 各 π_i を deterministic として保存し戻り値にも返す
      mapM (\(i, p) ->
              deterministic (name <> "_" <> T.pack (show i)) p)
           (zip [0 :: Int ..] pis)

-- | Increasing cuts helper for 'OrderedLogistic' / 'OrderedProbit'.
-- [日本語]: @c_1 = c_min@、 @c_k = c_{k-1} + d_k@ with
--   @d_k ~ HalfNormal(scale)@ により自動的に increasing 列を保証する。
--   [English]: @c_1 = c_min@, @c_k = c_{k-1} + d_k@ with
--   @d_k ~ HalfNormal(scale)@ automatically guarantees an increasing
--   sequence.
--
-- [日本語]: 戻り値は長さ @nCuts@ の Track が通る deterministic 値の列
--   (@name_c_0@, …, @name_c_{nCuts-1}@)。 各 @d_k@ は @name_d_k@ で
--   latent として登録される。 cuts は OrderedLogistic / OrderedProbit に
--   そのまま渡せる。
--   [English]: Returns a length-@nCuts@ list of deterministic values that
--   Track passes through (@name_c_0@, …, @name_c_{nCuts-1}@). Each @d_k@
--   is registered as a latent under @name_d_k@. The cuts can be passed
--   directly to OrderedLogistic / OrderedProbit.
--
-- [日本語]: DAG-safe pattern: monadic recursion で @deterministic@ の
--   戻り値 (det 名で relabel された Track) を次 step に渡すことで
--   plate-style の親集合を保つ。
--   [English]: A DAG-safe pattern: threads @deterministic@'s return value
--   (a Track relabeled under the deterministic's name) into the next step
--   via monadic recursion, preserving a plate-style parent set.
orderedCuts :: forall a. (Floating a, Ord a)
            => Text   -- ^ [日本語]: ベース名。 [English]: the base name.
            -> Int    -- ^ [日本語]: カット数 K-1 (≥ 1)。 [English]: the number of cuts K-1 (≥ 1).
            -> a      -- ^ [日本語]: 最小値 c_min。 [English]: the minimum value c_min.
            -> a      -- ^ [日本語]: 増分の HalfNormal スケール。 [English]: the HalfNormal scale for the increments.
            -> Model a [a]
orderedCuts name nCuts cMin scale
  | nCuts < 1 = error "orderedCuts: nCuts < 1 は未対応"
  | otherwise = do
      -- c_1 = c_min (定数を deterministic で登録、 Track 透過のため)
      c1 <- deterministic (name <> "_c_1") cMin
      -- c_2, ..., c_nCuts を monadic recursion で順に作る
      -- chain prev i: 現在の前 cut Track が prev、 次に作るのは index i (1-based)
      let chain prev i acc
            | i > nCuts = return (reverse acc)
            | otherwise = do
                d  <- sample (name <> "_d_" <> T.pack (show i))
                             (HalfNormal scale)
                ci <- deterministic (name <> "_c_" <> T.pack (show i))
                                    (prev + d)
                chain ci (i + 1) (ci : acc)
      rest <- chain c1 2 []
      return (c1 : rest)

-- | [日本語]: Dirichlet Process の有限近似 stick-breaking。
--   [English]: A finite stick-breaking approximation of a Dirichlet
--   Process.
--
-- [日本語]: @β_k ~ Beta(1, α)@ for @k = 1, …, T-1@、 重み
--   @π_k = β_k Π_{j<k}(1 - β_j)@、 @π_T = Π_{j<T}(1 - β_j)@ (残差) で
--   @Σ_k π_k = 1@ を保証。 truncation level @T@ で打ち切る (実用 T = 20-50)。
--   [English]: @β_k ~ Beta(1, α)@ for @k = 1, …, T-1@; the weights
--   @π_k = β_k Π_{j<k}(1 - β_j)@ and @π_T = Π_{j<T}(1 - β_j)@ (the
--   remainder) guarantee @Σ_k π_k = 1@. Truncated at level @T@ (in
--   practice T = 20-50).
--
-- [日本語]: 戻り値は長さ @T@ の deterministic Track 列
--   (@name_pi_1@, …, @name_pi_T@)。 @β_k@ は @name_b_k@ で latent 登録。
--   [English]: Returns a length-@T@ list of deterministic Tracks
--   (@name_pi_1@, …, @name_pi_T@). Each @β_k@ is registered as a latent
--   under @name_b_k@.
--
-- [日本語]: DAG-safe: 各 β を sample 後、 累積積を deterministic で chain して
--   π を計算する規律。
--   [English]: DAG-safe: after sampling each β, computes π by chaining the
--   cumulative product through @deterministic@.
dpStickBreaking :: forall a. (Floating a, Ord a)
                => Text   -- ^ [日本語]: ベース名。 [English]: the base name.
                -> Int    -- ^ [日本語]: truncation level T (≥ 2)。 [English]: the truncation level T (≥ 2).
                -> a      -- ^ [日本語]: concentration α (> 0)。 [English]: the concentration α (> 0).
                -> Model a [a]
dpStickBreaking name truncT alpha
  | truncT < 2 = error "dpStickBreaking: truncation level < 2 は未対応"
  | otherwise = do
      -- β_1, …, β_{T-1} を sample
      betas <- mapM
        (\i -> sample (name <> "_b_" <> T.pack (show i))
                      (Beta 1 alpha))
        [1 .. truncT - 1]
      -- 累積積 stick_k = Π_{j<k} (1 - β_j) を deterministic で chain
      -- stick_1 = 1、 stick_{k+1} = stick_k * (1 - β_k)
      stick1 <- deterministic (name <> "_stick_1") (1 :: a)
      let stickChain prev i acc
            | i > truncT = return (reverse acc)
            | otherwise = do
                let bIdx  = i - 1
                    beta  = betas !! (bIdx - 1)  -- 1-based β_{i-1}
                sNext <- deterministic
                           (name <> "_stick_" <> T.pack (show i))
                           (prev * (1 - beta))
                stickChain sNext (i + 1) (sNext : acc)
      restSticks <- stickChain stick1 2 []
      let sticks = stick1 : restSticks  -- 長さ T
      -- π_k = β_k * stick_k for k < T、 π_T = stick_T
      pis <- mapM
        (\i ->
          let stickI = sticks !! (i - 1)
              piVal  = if i < truncT
                         then (betas !! (i - 1)) * stickI
                         else stickI
          in deterministic (name <> "_pi_" <> T.pack (show i)) piVal)
        [1 .. truncT]
      return pis

-- | [日本語]: Hidden Markov Model 用の遷移行列 + 初期分布 prior helper。
--   K 状態の HMM について、 初期分布 π_0 と K×K 遷移行列の各行に
--   Dirichlet(α, …, α) prior を置く。
--   [English]: A helper for Hidden Markov Model transition-matrix + initial-
--   distribution priors. For a K-state HMM, places a Dirichlet(α, …, α)
--   prior on the initial distribution π_0 and on each row of the K×K
--   transition matrix.
--
-- [日本語]: 戻り値は @(π_0, transitions)@:
--   [English]: Returns @(π_0, transitions)@:
--
-- - @π_0@: 長さ K の確率列 (Σ = 1)、 @name_pi0_<i>@ で deterministic 登録 /
--   a length-K probability vector (Σ = 1), registered as a deterministic
--   under @name_pi0_<i>@
-- - @transitions@: 長さ K のリスト、 i 番目は遷移行列 i 行目
--   (@name_trans_i_<j>@ で deterministic) /
--   a length-K list whose i-th element is row i of the transition matrix
--   (@name_trans_i_<j>@ as a deterministic)
--
-- [日本語]: 離散状態列は __直接 latent としない__ (NUTS は離散変数を扱えない)。
--   代わりに、 ユーザは観測列 @y@ の emission log-prob 行列を計算し、
--   'hmmForwardLogLik' で状態列をマージナル化した周辺対数尤度を求め、
--   'potential' で組み込む形を取る。
--   [English]: The discrete state sequence is __never a direct latent__
--   (NUTS cannot handle discrete variables). Instead, users compute the
--   emission log-prob matrix for the observation sequence @y@, obtain the
--   marginal log-likelihood by marginalizing out the state sequence via
--   'hmmForwardLogLik', and incorporate it with 'potential'.
--
-- [日本語]: 内部実装は既存 'dirichlet' helper を K+1 回呼ぶだけ。 すべて
--   deterministic chain で DAG-safe。
--   [English]: Internally this simply calls the existing 'dirichlet'
--   helper K+1 times; everything is DAG-safe via a deterministic chain.
hmmLatent :: forall a. (Floating a, Ord a)
          => Text   -- ^ [日本語]: ベース名。 [English]: the base name.
          -> Int    -- ^ [日本語]: 状態数 K (≥ 2)。 [English]: the number of states K (≥ 2).
          -> a      -- ^ [日本語]: Dirichlet concentration α (> 0、 1 で uniform prior)。 [English]: the Dirichlet concentration α (> 0; 1 gives a uniform prior).
          -> Model a ([a], [[a]])
hmmLatent name k alpha
  | k < 2 = error "hmmLatent: K < 2 は未対応"
  | otherwise = do
      pi0 <- dirichlet (name <> "_pi0") (replicate k alpha)
      trans <- mapM
        (\i -> dirichlet (name <> "_trans_" <> T.pack (show i))
                         (replicate k alpha))
        [0 .. k - 1]
      return (pi0, trans)

-- | [日本語]: HMM forward algorithm marginal log-likelihood。
--   'Hanalyze.Model.HBM.Util' へ純粋移設済 (ここは re-export
--   のみ・API 不変)。 用法は従来の @'potential' nm (hmmForwardLogLik ...)@ に加え、
--   Normal emission の場合は 'HmmForwardNormal' + 'observeMV' が推奨
--   (勾配コンパイラが forward-backward の閉形式随伴を使えるため大幅に速い)。
--   [English]: The HMM forward algorithm's marginal log-likelihood. Purely
--   relocated to 'Hanalyze.Model.HBM.Util' (this is a re-export
--   only; the API is unchanged). Besides the traditional usage
--   @'potential' nm (hmmForwardLogLik ...)@, for Normal emissions,
--   'HmmForwardNormal' + 'observeMV' is recommended (much faster, since
--   the gradient compiler can use the closed-form forward-backward
--   adjoint).

-- ---------------------------------------------------------------------------
-- 構造検査
-- ---------------------------------------------------------------------------

data NodeKind = LatentN | ObservedN Int | DeterministicN
              | DataN Int   -- ^ [日本語]: データ slot ('dataNamed' / 'dataNamedIx')。
                            --   Int = 長さ。 PyMC の pm.Data (ConstantData) 相当。
                            --   [English]: A data slot ('dataNamed' /
                            --   'dataNamedIx'); the @Int@ is the length,
                            --   equivalent to PyMC's @pm.Data@
                            --   (@ConstantData@).
  deriving (Show, Eq)

data Node = Node
  { nodeName   :: Text
  , nodeKind   :: NodeKind
  , nodeDist   :: Text         -- 分布名 (e.g. "Normal")
  , nodeDeps   :: Set Text     -- 直接の親 (依存変数)
  , nodePlates :: [Text]       -- Phase 40: plate スタック (外側から内側、 空 = 任意の plate に属さない)
  } deriving (Show)

-- | Walk the model with placeholder zeros and collect 'Node' metadata.
-- [日本語]: 依存関係 ('nodeDeps') は @extractDeps@ を使うこと (placeholder
--   走査では取れない)。
-- [English]: For dependencies ('nodeDeps'), use @extractDeps@ instead — a
-- placeholder walk cannot recover them.
collectNodes :: forall r. ModelP r -> [Node]
collectNodes m = go m []
  where
    go :: Model Double r -> [Node] -> [Node]
    go (Pure _) acc = reverse acc
    go (Free (Sample n d k)) acc =
      go (k 0) (Node n LatentN (distName d) Set.empty [] : acc)
    go (Free (Observe n d ys next)) acc =
      go next (Node n (ObservedN (length ys)) (distName d) Set.empty [] : acc)
    go (Free (ObserveLM n _ _ _ fam ys next)) acc =
      go next (Node n (ObservedN (length ys)) (lmFamilyName fam) Set.empty [] : acc)
    go (Free (Potential _ _ next)) acc = go next acc   -- Node 表示には含めない
    go (Free (Deterministic _ v k)) acc = go (k v) acc
    go (Free (Data n ys k)) acc =
      go (k (ys, ys)) (Node n (DataN (length ys)) "Data" Set.empty [] : acc)
    go (Free (DataIx n is k)) acc =
      go (k is) (Node n (DataN (length is)) "DataIx" Set.empty [] : acc)
    go (Free (PlateBegin _ _ next)) acc = go next acc  -- Phase 40: 透過
    go (Free (PlateEnd next))       acc = go next acc

sampleNames :: ModelP r -> [Text]
sampleNames m = [nodeName n | n <- collectNodes m, nodeKind n == LatentN]

-- | [日本語]: モデル中の 'Data' slot を (名前, placeholder が空か) で列挙する。
--   同名 slot が複数回現れる場合は 1 entry に集約し、 __いずれかが空なら空扱い__
--   (束縛層の loud error 判定は保守側に倒す)。 @DataIx@ slot は 'dataIxSlots'。
--   [English]: Enumerates the model's 'Data' slots as (name, is the
--   placeholder empty). If the same-named slot occurs multiple times, they
--   are collapsed to one entry — __treated as empty if any occurrence is empty__
--   (erring conservative for the binding layer's loud-error
--   check). Use 'dataIxSlots' for @DataIx@ slots.
dataSlots :: forall r. ModelP r -> [(Text, Bool)]
dataSlots m = dedupSlots (go m [])
  where
    go :: Model Double r -> [(Text, Bool)] -> [(Text, Bool)]
    go (Pure _) acc = reverse acc
    go (Free (Sample _ _ k)) acc = go (k 0) acc
    go (Free (Observe _ _ _ next)) acc = go next acc
    go (Free (ObserveLM _ _ _ _ _ _ next)) acc = go next acc
    go (Free (Potential _ _ next)) acc = go next acc
    go (Free (Deterministic _ v k)) acc = go (k v) acc
    go (Free (Data n ys k)) acc = go (k (ys, ys)) ((n, null ys) : acc)
    go (Free (DataIx _ is k)) acc = go (k is) acc
    go (Free (PlateBegin _ _ next)) acc = go next acc
    go (Free (PlateEnd next))       acc = go next acc

-- | [日本語]: モデル中の @DataIx@ slot を (名前, placeholder が空か) で列挙する。 [English]: Enumerates the model's @DataIx@ slots as (name, is the placeholder empty).
dataIxSlots :: forall r. ModelP r -> [(Text, Bool)]
dataIxSlots m = dedupSlots (go m [])
  where
    go :: Model Double r -> [(Text, Bool)] -> [(Text, Bool)]
    go (Pure _) acc = reverse acc
    go (Free (Sample _ _ k)) acc = go (k 0) acc
    go (Free (Observe _ _ _ next)) acc = go next acc
    go (Free (ObserveLM _ _ _ _ _ _ next)) acc = go next acc
    go (Free (Potential _ _ next)) acc = go next acc
    go (Free (Deterministic _ v k)) acc = go (k v) acc
    go (Free (Data _ ys k)) acc = go (k (ys, ys)) acc
    go (Free (DataIx n is k)) acc = go (k is) ((n, null is) : acc)
    go (Free (PlateBegin _ _ next)) acc = go next acc
    go (Free (PlateEnd next))       acc = go next acc

-- | [日本語]: slot 列挙の重複集約 (先頭出現順を保ち、 空 flag は OR)。 [English]: Deduplicates a slot enumeration, preserving first-occurrence order and OR-ing the empty flag.
dedupSlots :: [(Text, Bool)] -> [(Text, Bool)]
dedupSlots xs =
  [ (n, or [ e | (n', e) <- xs, n' == n ])
  | n <- nub (map fst xs) ]

