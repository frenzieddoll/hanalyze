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
-- Phase 58.5: 多相モデル DSL (Free monad) を 'Hanalyze.Model.HBM' から分離。
--
-- 本モジュールは PPL の **記述層** を担う:
--
--   * @Free@ monad 再実装 (型は 'Hanalyze.Model.HBM' 公開のものと別個)
--   * 'ModelF' プリミティブ (sample / observe / observeLM / deterministic /
--     plate / Data / Potential) と 'Model' / 'ModelP' 型エイリアス
--   * 第一級ランダム効果値 'REffect' / 'REff' と階層モデル helper 群
--     (reNormal / mvNormalLatent / lkjCorrCholesky / ar1Latent / dirichlet /
--      orderedCuts / dpStickBreaking / hmmLatent / glmmRandomIntercept 等)
--   * Plate notation (Phase 40) と構造検査 ('collectNodes' / 'sampleNames')
--
-- 評価 (logJoint 等)・AD 勾配・IR は **上層** に置かれ、 本モジュールは
-- それらに依存しない (leaf-first・facade 非 import の規律。 Phase 58 計画参照)。
-- 依存は下層 'Hanalyze.Model.HBM.Util' / '...Distribution' のみ。
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
    -- ** Phase 40 plate notation
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

-- | DSL のプリミティブ。継続が @a -> next@ なので任意の @a@ を流せる。
--
-- 'Potential' は PyMC の @pm.Potential@ 相当で、任意の log-prob 項を
-- log-joint に加える。ソフト制約・カスタム尤度・正則化項などに使える。
-- | 構造化線形予測子 observe (Phase 54.1) の family / link。
--
-- 通常の 'Observe' は平均が不透明な AD 値ゆえ「β に線形」 という構造を
-- 保持できない。 'ObserveLM' は設計行列 X (Double) と β パラメタ名を **分離**
-- して持つことで線形構造をライブラリが知り、 54.2 で Gaussian-恒等リンクの
-- 十分統計量 collapse (観測和を tape O(p²) に畳む) を可能にする。
data LMFamily
  = LMGaussian Text   -- ^ identity link。 引数 = σ (誤差 SD) パラメタ名。
  | LMPoisson         -- ^ log link (μ = exp η)。
  | LMBernoulli       -- ^ logit link (p = 1/(1+e^{-η}))。
  deriving (Show, Eq)

-- | 'ObserveLM' のランダム効果項 (Phase 54.4a)。 線形予測子に
-- @η_i += u^{re}[gid_i]@ を gather で加える。 設計行列の one-hot 指示列として
-- 密に展開する代わりに、 群 id ベクトルで疎に保持することで vec-tape の
-- 観測尤度勾配が群効果に対しても O(n) で済む (密展開は O(nG·n) で階層モデルで
-- 逆効果になる・54.4a 計測で確認)。
--
-- フィールド: u パラメタ名 (長さ nG・既に 'sample' 済の latent を参照) /
-- 各観測の群 id (長さ n・0..nG-1) /
-- prior スケール名 (Phase 54.4c): @Just τName@ なら各 u_j が
-- @u_j ~ Normal(0, τ)@ という標準的な階層 prior を持つことを宣言する。
-- これがあると 'compileGradU' は u-prior 勾配を **解析的に** (ベクトル化して)
-- 計算し、 対応する @u_j@ 'Sample' ノードを `ad` walk から除外できる
-- (per-grad の支配項だった O(nG) スカラ `ad` を排除)。 @Nothing@ なら
-- prior は従来通り `ad` 経路で扱う (後方互換)。 通常は 'reNormal'/'at' で
-- 自動的に @Just@ が載るので、 ユーザがこの構築子を直接書く必要はない。
--
-- per-row 重み (Phase 54.10): @Just ws@ (長さ n) なら @η_i += w_i·u^{re}[gid_i]@
-- (random slope = 群別係数 × 共変量)。 @Nothing@ = 全 1 (random intercept・
-- 後方互換)。 prior 解析勾配 (@u_j ~ Normal(0,τ)@) は重みと無関係に同形。
-- 由来 slot 名 (Phase 62): 5 番目 field は gids がどのデータ slot
-- ('dataNamedIx') 由来かの静的属性。 @Just slot@ なら 'lmParents' が slot 名を
-- 親集合に加え、 DAG に slot (DataN)→観測ノードのエッジが出る (PyMC
-- @b0[gid]@ 同型)。 'atIx' が自動で載せる。 'at' / IR 合成経路は @Nothing@
-- (従来挙動)。 hot closure ('CompiledLMBlock') には乗らない = per-draw 無影響。
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
    -- ^ 構造化線形予測子 observe (Phase 54.1、 54.4a で REff 追加)。
    --   フィールド: ブロック名 / β パラメタ名 (順序 = X の列) /
    --   設計行列 X (n 行 × p 列、 Double) / ランダム効果項 (gather) /
    --   family-link / 観測 ys (長さ n)。
    --   各 i について η_i = Σ_j β_j·X_ij + Σ_re u^{re}[gid^{re}_i]、
    --   μ_i = link⁻¹(η_i)、 log-lik = Σ_i logDensityObs(family μ_i) y_i。
    --   β / u / 分散パラメタは別途 'sample' で宣言された latent を
    --   **名前参照**する (prior は持たない)。
    --   DAG 上は 1 観測ノード (親 = β + u + 分散パラメタ名)。
  | Potential Text a next
    -- ^ 名前付きの ad-hoc な log-prob 項。値 @a@ がそのまま log-joint に加算される。
  | Deterministic Text a (a -> next)
    -- ^ 名前付きの派生量 (PyMC `pm.Deterministic`)。log-joint には寄与せず、
    --   サンプルごとに値を保存する。継続には値そのものを通すので、その後の
    --   モデル中でも参照可能。
  | Data Text [Double] (([a], [Double]) -> next)
    -- ^ 名前付き観測データプレースホルダ (PyMC `pm.Data`)。
    --   モデル内でデータを保持し、`withData` で外部から差し替え可能。
    --   観測値を直接 `observe` に渡す代わりに、`dataNamed` で受け取って
    --   `observe` に渡すと、後でデータ差し替えができる。
    --   ★Phase 60.2 破壊的変更: 継続は ([a], [Double]) の 2 view を受ける
    --   (格納は [Double] のまま・各 interpreter が lift)。 fst = モデル数値型
    --   ('dataNamed'、 covariate 用・realToFrac 不要)、 snd = 生 [Double]
    --   ('dataNamedObs'、 'observe' の観測値用)。 tuple は lazy なので
    --   未使用側の lift コストは掛からない。
  | DataIx Text [Int] ([Int] -> next)
    -- ^ 離散 index 専用のデータプレースホルダ (Phase 60.2)。 群 index 等の
    --   名義尺度を [Int] のまま運ぶ (= AD 型に持ち上げない・round 罠の根治)。
    --   継続型は @a@ に依らず [Int] なので interpreter の lift も不要。
  | PlateBegin Text Int next
    -- ^ Plate 開始マーカー (Phase 40-A1、 Pyro/NumPyro 流の plate-block 糖衣)。
    --   名前 + サイズ N を持つ plate スコープの開始。 直後から 'PlateEnd'
    --   までに登録される 'Sample' / 'Observe' / 'Deterministic' は
    --   buildModelGraph で「plate メンバ」 として描画される。
    --   nested plate は LIFO スタックで対応。 log eval interpreter (logJoint
    --   等) は **透過** に処理する (何もしない)。
  | PlateEnd next
    -- ^ Plate 終了マーカー (Phase 40-A1)。 最新の PlateBegin スコープを閉じる。
  deriving Functor

type Model a = Free (ModelF a)

-- | Type alias for the polymorphic model DSL.
-- @ModelP r = forall a. (Floating a, Ord a, TrackTag a) => Model a r@
-- ('TrackTag' は Phase 60.7 '!!!' の依存タグ注入用。 数値解釈は既定 id)。
type ModelP r = forall a. (Floating a, Ord a, TrackTag a) => Model a r

sample :: Text -> Distribution a -> Model a a
sample n d = liftF (Sample n d id)

observe :: Text -> Distribution a -> [Double] -> Model a ()
observe n d ys = liftF (Observe n d ys ())

-- | 構造化線形予測子 observe (Phase 54.1)。
--
-- @observeLM name betaNames designX family ys@ は、 設計行列 @designX@
-- (n 行 × p 列) と β パラメタ名 @betaNames@ (長さ p・既に 'sample' で宣言済の
-- latent を参照) を **分離して**保持する観測ブロック。 各観測 i について
-- η_i = Σ_j β_j·X_ij を作り、 @family@ のリンク逆関数で μ_i に写して
-- 観測 @ys !! i@ の log-density を加算する。
--
-- 通常の per-obs @observe@ を N 回呼ぶのと数値的に等価だが、 線形構造を
-- 保持するので 54.2 で Gaussian-恒等リンクの十分統計量 collapse に乗せられる。
observeLM :: Text -> [Text] -> [[Double]] -> LMFamily -> [Double] -> Model a ()
observeLM n betas designX fam ys = liftF (ObserveLM n betas designX [] fam ys ())

-- | ランダム効果付き 'observeLM' (Phase 54.4a)。
--
-- @observeLMR name betaNames designX reffs family ys@ は 'observeLM' に
-- ランダム効果項 @reffs@ を加えたもの。 各 'REff' は (u パラメタ名, 群 id) で
-- @η_i += u^{re}[gid_i]@ を **gather** で寄与する。 群効果を設計行列の one-hot
-- 指示列に密展開すると vec-tape 勾配が O(nG·n) になり階層モデルで逆効果になる
-- (54.4a 計測) ため、 群構造は疎に保持して gather で O(n) に保つ。
observeLMR :: Text -> [Text] -> [[Double]] -> [REff] -> LMFamily -> [Double]
           -> Model a ()
observeLMR n betas designX reffs fam ys =
  liftF (ObserveLM n betas designX reffs fam ys ())

-- ---------------------------------------------------------------------------
-- 第一級ランダム効果値 (Phase 54.4c)
-- ---------------------------------------------------------------------------

-- | 第一級ランダム効果値 (Phase 54.4c)。 'reNormal' で宣言した nG 個の
-- iid @Normal(0, τ)@ latent を、 構造 (基底名・群数・スケール名・値) ごと
-- ひとつの値に載せて持ち運ぶ。 これにより観測の線形予測子に効果を載せるとき
-- 文字列添字 (@"u_" <> show j@) も @us !! g@ も書かずに 'at' で gather でき
-- (Haskell 王道の「構造を値に載せて流す」)、 さらにスケール名が構造として
-- 保持されるので 'compileGradU' が u-prior 勾配を解析的にベクトル化できる。
data REffect a = REffect
  { reffBase   :: !Text   -- ^ 基底名 (例 @"u"@)。 latent 名は @base_<j>@。
  , reffNG     :: !Int    -- ^ 群数 nG
  , reffScale  :: !Text   -- ^ スケール latent の名前 (@u_j ~ Normal(0, scale)@)
  , reffValues :: [a]     -- ^ サンプル済 nG 個の値 (forward 評価・deterministic 用)
  }

-- | 'REffect' の latent 名 (@base_0 .. base_{nG-1}@)。
reffNames :: REffect a -> [Text]
reffNames re = [ indexed (reffBase re) j | j <- [0 .. reffNG re - 1] ]

-- | 群別ランダム効果を第一級値として宣言する (Phase 54.4c)。
--
-- @reNormal base nG scaleName scaleVal@ は @base_0 .. base_{nG-1}@ という
-- nG 個の latent を各々 @Normal(0, scaleVal)@ として 'sample' し、 その構造
-- (基底名 / nG / スケール名 / 値) を 'REffect' にまとめて返す。 @scaleName@ は
-- @scaleVal@ を生んだスケール latent の名前 (例 @"tau_u"@) で、 解析 prior 勾配
-- 経路 ('compileGradU') がスケール変数を引くために構造として保持する
-- (値は名前を覚えていないため明示的に渡す)。
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

-- | 'REffect' を観測の群 id 列に対して gather し 'REff' (観測ブロック用) に変換する。
-- @η_i += u^{re}[gid_i]@。 スケール名を 'REff' に載せるので、 これ経由で観測に
-- 入った効果は 'compileGradU' の解析 prior 勾配経路に乗る。
at :: REffect a -> [Int] -> REff
at re gids = REff (reffNames re) gids (Just (reffScale re)) Nothing Nothing

-- | Gaussian-恒等リンク版の構造化 observe (Phase 54.4c)。 'observeLMR' の
-- @LMGaussian@ 特化で、 'at' で作った 'REff' をそのまま渡せる薄いラッパ。
--
-- @observeNormalLM name designX betaNames reffs sigmaName ys@。
observeNormalLM :: Text -> [[Double]] -> [Text] -> [REff] -> Text -> [Double]
                -> Model a ()
observeNormalLM name designX betaNames reffs sName ys =
  observeLMR name betaNames designX reffs (LMGaussian sName) ys

-- | Multivariate observation (for 'MvNormal'). Each observation is a
-- length-@k@ vector; pass them as a list @[[Double]]@.
-- 内部的には @concat@ で flatten され、評価時に Distribution の次元 k で chunk される。
observeMV :: Text -> Distribution a -> [[Double]] -> Model a ()
observeMV n d obss = liftF (Observe n d (concat obss) ())

-- | Multi-output observation helper. Takes @q@ pairs of
-- @observe (prefix <> \"_\" <> j) dist_j ys_j@ を順に発行する。
--
-- 多出力回帰の尤度を 1 行で書きたいときに使う:
--
-- @
-- observeColumns \"y\" [(Normal mu_j sigma_j, ysCol j) | j <- [0 .. q - 1]]
-- @
observeColumns :: Text -> [(Distribution a, [Double])] -> Model a ()
observeColumns prefix pairs =
  mapM_ (\(j, (d, ys)) ->
           observe (prefix <> "_" <> T.pack (show (j :: Int))) d ys)
        (zip [0..] pairs)

-- | インデックス付きノード名を作る: @indexed "theta" 1 == "theta_1"@。
--
-- 階層モデルで群ごとの 'sample' / 'observe' 名を作るときに頻出する
-- @T.pack ("theta_" ++ show j)@ ボイラープレートを畳む。 アンダースコアは
-- 自動付与 (= 'observeColumns' / 'nonCenteredNormal' 等の命名規約に一致)。
--
-- > forM_ (zip [1..] groupData) $ \(j, ys) -> do
-- >   theta <- sample (indexed "theta" j) (Normal mu tau)   -- "theta_1" …
-- >   observe (indexed "y" j) (Normal theta 1) ys
indexed :: Text -> Int -> Text
indexed pre i = pre <> "_" <> T.pack (show i)

-- | 'indexed' の中置演算子版: @"theta" .# j == "theta_1"@。
--
-- (Haskell の演算子記号に @_@ は使えないため @.#@ を採用。)
infixl 9 .#
(.#) :: Text -> Int -> Text
(.#) = indexed

-- | Add an arbitrary log-probability term to the model (analogous to
-- PyMC's @pm.Potential@).
--
-- 通常のサンプリング/観測では表せない log-density 寄与を入れるのに使う。
-- 典型用途:
--
--   * **ソフト制約**: @potential \"order\" (if mu1 < mu2 then 0 else (-1e10))@
--   * **カスタム尤度**: 既存 'Distribution' で表せない尤度項
--   * **正則化**: ベイズ的な正則化 (e.g. ridge: @-0.5 * lambda * sum (map (^2) betas)@)
--
-- @Potential@ の値は 'logJoint' と 'logPrior' に加算される
-- ('logLikelihood' には含まれない — これらは @observe@ 専用)。
potential :: Text -> a -> Model a ()
potential nm v = liftF (Potential nm v ())

-- | 派生量を名前付きで保存する (PyMC `pm.Deterministic` 相当)。
--
-- log-joint には寄与しないが、各 posterior サンプルごとに値が記録され
-- 'augmentChainWithDeterministic' で Chain に注入できる。
--
-- 例:
--
-- > tau <- deterministic "tau" (1 / (sigma * sigma))
deterministic :: Text -> a -> Model a a
deterministic nm v = liftF (Deterministic nm v id)

-- | DAG / Node 表示用の分布名 (リンク逆関数を適用した観測分布の名前)。
lmFamilyName :: LMFamily -> Text
lmFamilyName (LMGaussian _) = "Normal"
lmFamilyName LMPoisson      = "Poisson"
lmFamilyName LMBernoulli    = "Bernoulli"

-- | 'ObserveLM' が参照する latent パラメタ名の集合 (DAG の親)。
-- β + ランダム効果 u + (Gaussian の) σ。
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

-- | Pyro / NumPyro 流の plate-block (Phase 40)。
--
-- @plate name n body@ は、 do-block 内で繰り返し作られる indexed RV 群
-- (e.g. @eta_0, eta_1, …, eta_{n-1}@) を **同じ plate に属する** と
-- マークする bracket。 'buildModelGraph' で plate 集約描画される。
--
-- 例 (8-schools):
--
-- > mu  <- sample "mu" (Normal 0 5)
-- > tau <- sample "tau" (HalfCauchy 5)
-- > etas <- plate "school" 8 $ forM [0..7] $ \j ->
-- >           sample ("eta_" <> T.pack (show j)) (Normal 0 1)
-- > _ <- plate "school" 8 $ forM_ [0..7] $ \j ->
-- >        observe ("y_" <> T.pack (show j))
-- >                (Normal (mu + tau * (etas !! j)) 1) [ys !! j]
--
-- 内部: 'PlateBegin' / 'PlateEnd' マーカーで囲む。 log eval (logJoint
-- / logPrior 等) は **透過** に動作し、 plate は描画レイヤーでのみ
-- 意味を持つ。 NUTS / Gibbs / VI への影響なし。
plate :: Text -> Int -> Model a r -> Model a r
plate name n body = do
  liftF (PlateBegin name n ())
  r <- body
  liftF (PlateEnd ())
  return r

-- | 'plate' の利便 helper: @plateI name n f@ = @plate name n (forM [0..n-1] f)@。
-- 「N 個の indexed RV を作る」 という最頻パターン向け糖衣。
--
-- 例:
--
-- > etas <- plateI "school" 8 $ \j ->
-- >           sample ("eta_" <> T.pack (show j)) (Normal 0 1)
plateI :: Text -> Int -> (Int -> Model a r) -> Model a [r]
plateI name n action = plate name n (forM [0 .. n - 1] action)

-- | 'plateI' の返り値を捨てる版 (@forM_@ の plate 版・index 反復)。
-- @plateI_ name n f = plate name n (forM_ [0..n-1] f)@。 観測のみの index
-- ループ向け (@plateForM_ name [0..n-1] f@ と同義だが index 反復の意図が明示的・
-- 'plateForM' / 'plateForM_' の対称に合わせ index 版にも破棄形を用意)。
--
-- 例 (8-schools の観測):
--
-- > plateI_ "school" 8 $ \j ->
-- >   observe ("y" .# j) (Normal (mu + tau * etas !! j) 1) [ys !! j]
plateI_ :: Text -> Int -> (Int -> Model a r) -> Model a ()
plateI_ name n action = plate name n (forM_ [0 .. n - 1] action)

-- | データ行リストを plate で囲んで反復する糖衣 (@forM@ の plate 版・引数順も @forM@ 形)。
-- @plateForM name rows f = plate name (length rows) (forM rows f)@。 plate サイズは
-- 行数から自動。 観測ループの定番 @plate name (length rows) $ forM_ … rows@ を畳む。
--
-- 例 (ベイズ線形回帰の観測):
--
-- > plateForM_ "obs" (zip x y) $ \(xi, yi) -> do
-- >   mu <- deterministic "mu" (a + b * realToFrac xi)
-- >   observe "obs" (Normal mu s) [yi]
plateForM :: Text -> [b] -> (b -> Model a r) -> Model a [r]
plateForM name rows f = plate name (length rows) (forM rows f)

-- | 返り値を捨てる版 (@forM_@ の plate 版)。 観測のみのループに。
plateForM_ :: Text -> [b] -> (b -> Model a r) -> Model a ()
plateForM_ name rows f = plate name (length rows) (forM_ rows f)

-- | 低レベル plate API: 任意の Model action を plate スコープで包む。
-- 'plate' は @withPlate name n@ + body の組合せに分解される。 nested
-- plate を独自構築する際の primitive。
withPlate :: Text -> Int -> Model a r -> Model a r
withPlate = plate

-- | 名前付きデータプレースホルダを宣言する (PyMC `pm.Data` 相当)。
-- 既定値 @ys@ を持ち、後で 'withData' により差し替え可能。
--
-- 典型的な使い方:
--
-- > model = do
-- >   y <- dataNamed "y" trainData
-- >   mu <- sample "mu" (Normal 0 5)
-- >   observe "y" (Normal mu 1) y
--
-- そして @withData \"y\" testData model@ で同じ構造で別データを使う。
--
-- ★Phase 60.2 破壊的変更: 戻り値は @[a]@ (モデルの数値型)。 受け取った値は
-- そのまま式に入る (@realToFrac@ 不要)。 @a@ には @Real@ 制約が無いので、
-- 旧コードの @realToFrac xi@ は型エラーになる (= 無言の挙動変化が起きない
-- 壊れ方)。 機械的に @realToFrac@ を消せば移行完了。
-- 観測値として 'observe' に渡す側 (@[Double]@ が要る) は 'dataNamedObs' を使う。
dataNamed :: Text -> [Double] -> Model a [a]
dataNamed n ys = liftF (Data n ys fst)

-- | 'dataNamed' の同義 (Phase 60.6)。 役割 suffix 三点セットの正書き:
--
-- > x  <- dataNamedX   "x" []   -- 説明変数: モデル数値型 [a]
-- > ys <- dataNamedObs "y" []   -- 目的変数: 生 [Double] ('observe' へ)
-- > gs <- dataNamedIx  "g" []   -- 群 index: [Int]
--
-- 既存コードの 'dataNamed' もそのまま使える (削除予定なし)。
dataNamedX :: Text -> [Double] -> Model a [a]
dataNamedX = dataNamed

-- | 'dataNamed' と同じ slot の **観測値 view** (生 @[Double]@)。
-- 'observe' / 'observeLM' の観測値引数は AD に持ち上げない @[Double]@ 固定
-- なので、 y 側のデータ slot はこちらで受ける (Phase 60.2):
--
-- > x  <- dataNamed    "x" []   -- covariate: モデル数値型 [a]
-- > ys <- dataNamedObs "y" []   -- 観測値:    生 [Double]
-- > ...
-- > observe "y" (Normal mu s) ys
--
-- 同名 slot を 'dataNamed' と 'dataNamedObs' の両 view で読んでもよい
-- (差し替えは 'withData' / 列 bind が slot 名単位で行うため一貫する)。
dataNamedObs :: Text -> [Double] -> Model a [Double]
dataNamedObs n ys = liftF (Data n ys snd)

-- | 離散 index 専用のデータプレースホルダ (Phase 60.2、 60.7 で 'Ix' 戻りに刷新)。
-- 群 index 等を slot 名タグ付き index 'Ix' で運ぶ。 @bs '!!!' g@ で引くと
-- DAG に slot→利用先のエッジが自動で出る (PyMC の @b0[gid]@ 同型)。
-- 'Ix' は Num でないので誤って算術に混ぜると型エラーで止まる
-- (= 連続値経路の round 罠根治、 60.2 から継続)。
--
-- > gs <- dataNamedIx "g" [0,0,1,1,2]
-- > let mu_i = b0s !!! g   -- round 不要・DAG に g→mu エッジ
dataNamedIx :: Text -> [Int] -> Model a [Ix]
dataNamedIx n is = liftF (DataIx n is (map (\i -> Ix i (Just n))))

-- | slot 名タグ付き離散 index (Phase 60.7)。 'dataNamedIx' が返し、 '!!!' で
-- 使う。 由来 slot 名 ('ixSlot') は DAG 抽出 (Track 解釈) のエッジ生成にだけ
-- 使われ、 数値評価では 'ixVal' のみが意味を持つ。
data Ix = Ix
  { ixVal  :: !Int          -- ^ index 本体 (0..nG-1)
  , ixSlot :: !(Maybe Text) -- ^ 由来 slot 名 ('dataNamedIx' なら Just)
  } deriving (Show, Eq)

-- | 解釈ごとの依存タグ注入 (Phase 60.7)。 既定 = 何もしない (数値解釈は
-- ゼロコスト・サンプリングはビット不変)。 'Track' 解釈だけが override して
-- 依存集合に slot 名を足し、 DAG にエッジを出す。
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

-- | slot 名タグ付き索引 (Phase 60.7)。 @bs '!!!' g@ = @bs !! ixVal g@ に、
-- Track 解釈でのみ g の由来 slot 名を依存タグとして注入する
-- (= DAG に slot→利用先エッジ。 数値解釈は '!!' と同コスト)。
(!!!) :: TrackTag b => [b] -> Ix -> b
xs !!! Ix i ms = maybe id tagDep ms (xs !! i)
infixl 9 !!!
{-# INLINE (!!!) #-}

-- | 'at' の 'Ix' 版 (Phase 60.7)。 'dataNamedIx' の gids を random effect の
-- gather に渡す。 Phase 62: 先頭 'Ix' の由来 slot 名 ('ixSlot') を 'REff' に
-- 載せるので、 DAG に slot→観測ノードのエッジが出る (gather の gids は単一
-- slot 由来が通常形ゆえ先頭で代表)。 '!!!' (deterministic μ 経路) と並ぶ
-- PyMC @b0[gid]@ 同型の両経路対応。
atIx :: REffect a -> [Ix] -> REff
atIx re gids =
  REff (reffNames re) (map ixVal gids) (Just (reffScale re)) Nothing
       (case gids of { Ix _ ms : _ -> ms; [] -> Nothing })

-- | Replace a named data block in the model. If no match exists the
-- model is returned unchanged.
-- 同じ名前が複数回出現する場合は全箇所で差し替わる。
--
-- 型シグネチャは @Model a r@ なので、ユーザーが @ModelP r@ から呼ぶ場合
-- そのまま多相的に使える (各 @a@ で個別に適用される)。
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

-- | 'withData' の離散 index 版 (Phase 60.2): 名前付き 'DataIx' ブロックを
-- 外部から差し替える。 一致しなければモデルは不変。
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
-- 非中心化パラメタ化 + Cholesky 分解で実装:
--
--   z_i ~ Normal(0, 1)  (i = 0..K-1, 独立な latent)
--   x   = μ + L z       (L = Cholesky(Σ))
--
-- 各 z_i は通常の latent として NUTS が探索し、x は派生量として
-- Chain に記録される。共分散行列が他の latent に依存する形でも
-- 動作する (choleskyL は @(Floating a, Ord a)@ 多相)。
--
-- 共分散が非正定値のときは μ をそのまま返す (NUTS 探索中の不正領域
-- に対する graceful fallback)。
--
-- 戻り値: K 次元 latent ベクトル @[a]@ (μ + L z)。
-- Chain には @<name>_z<i>@ (raw latent) と @<name>_<i>@ (派生量) を保存。
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

-- | LKJ 相関行列の Cholesky factor (PyMC @LKJCholeskyCov@ 相当)。
--
-- LKJ(η) 事前: p(R) ∝ |R|^(η-1)。η = 1 で uniform、η > 1 で I に集中。
--
-- 実装は canonical partial correlations (CPC) 法:
--   z_ij ~ scaled Beta(α_i, α_i) on (-1, 1),  α_i = η + (K - i - 1) / 2
--     (i = 1..K-1, j = 0..i-1)
--
-- 各 z_ij は @<name>_pc<i>_<j>@ (Beta latent in (0,1)、内部で 2u-1 に変換)
-- として保存。Cholesky factor の各要素は派生量 @<name>_L<i>_<j>@。
--
-- 戻り値: K×K 下三角行列 L (R = L Lᵀ となる相関の Cholesky)。
-- 対角は √(1 - Σ z_{i,k}²)、対角下は z_ij × √(Π_{k<j}(1-z_{i,k}²))。
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

-- | RBF (exponentiated quadratic) カーネルによる GP 共分散行列
-- (Stan @gp_exp_quad_cov(x, alpha, rho)@ 相当)。
--
-- @K[i][j] = alpha^2 * exp(-0.5 * (x_i - x_j)^2 / rho^2)@、対角には数値安定化の
-- jitter (1e-10) を加える (Stan 原典の @+ diag_matrix(rep_vector(1e-10, N))@ に
-- 対応)。@x@ は 'dataNamedX' で束縛した @[a]@ をそのまま渡す (data と
-- ハイパーパラメータ alpha/rho は共に @a@ 型なので realToFrac 不要)。
--
-- Phase 90 A2: vecIR (per-row 独立項の和が前提) には密行列が構造的に載らない
-- ため、legacy walk+ad 経路 (`grad fFull`) で使う想定の孤立関数。
gpExpQuadCov :: forall a. Floating a => [a] -> a -> a -> [[a]]
gpExpQuadCov xs alpha rho =
  [ [ let d = xi - xj
      in alpha * alpha * exp (negate 0.5 * d * d / (rho * rho))
           + (if i == j then 1e-10 else 0)
    | (j, xj) <- zip [0 :: Int ..] xs ]
  | (i, xi) <- zip [0 :: Int ..] xs ]

-- | Gaussian Process 潜在関数 (Stan の non-centered GP パラメタ化相当):
--
-- > f_tilde ~ Normal(0, 1)     (各点独立)
-- > L_cov = cholesky_decompose(gp_exp_quad_cov(x, alpha, rho))
-- > f = L_cov * f_tilde
--
-- 既存の 'choleskyL' ('mvNormalLatent' と同じ AD 対応 Cholesky 分解) をそのまま
-- 流用する。共分散が非正定値のときは全ゼロにフォールバックする
-- ('mvNormalLatent' と同型の graceful fallback)。
--
-- 戻り値: N 次元 latent ベクトル @[a]@ (GP 事後関数値 f)。各要素は
-- @<name>_f<i>@ として deterministic 保存される。
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

-- | AR(1) latent 時系列 (PyMC `pm.AR1` 相当)。
--
-- 状態方程式:  x_t = ϕ x_{t−1} + ε_t,   ε_t ~ Normal(0, σ)
-- 初期分布:    x_0 ~ Normal(0, σ / √(1 − ϕ²))   (定常分布、|ϕ| < 1 なら有限)
--
-- 引数 @phi@ は AR 係数、@sigma@ は innovation の sd。N 個の latent
-- 状態 x_0 .. x_{N-1} を非中心化パラメタ化で sample する:
--
--   raw_t ~ Normal(0, 1)
--   x_t = phi * x_{t-1} + sigma * raw_t       (t > 0)
--   x_0 = (sigma / √(1 - ϕ²)) * raw_0
--
-- 戻り値: x_0 .. x_{N-1} の latent 値リスト ([a])。各 raw_t は
-- @<name>_raw<t>@、x_t 自体は派生量 @<name>_<t>@ として保存。
--
-- |ϕ| ≥ 1 のフォールバック: 初期 sd を sigma に置き換える。
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

-- | 非中心化 (non-centered) 正規分布。
--
-- @x ~ Normal(loc, scale)@ を直接サンプリングする代わりに、
--
-- > raw <- sample (name <> "_raw") (Normal 0 1)
-- > deterministic name (loc + scale * raw)
--
-- に展開する。loc / scale が他の latent に依存するとき、centered
-- パラメタ化は HMC の posterior が病的になりやすいので、それを
-- 緩和するヘルパ。Neal's funnel が代表例。
--
-- 戻り値は constrained な値 @loc + scale * raw@。Chain には
-- @<name>_raw@ (latent) と @<name>@ (derived) の両方が保存される。
nonCenteredNormal :: Num a => Text -> a -> a -> Model a a
nonCenteredNormal name loc scale = do
  raw <- sample (name <> "_raw") (Normal 0 1)
  deterministic name (loc + scale * raw)

-- | GLMM family for 'glmmRandomIntercept' (Phase 37-A6)。
data GlmmFamily
  = GlmmGaussian   -- ^ 連続 y、 残差 SD `sigma` も sample される
  | GlmmBinomial   -- ^ 0/1 y、 Bernoulli(σ(η))
  | GlmmPoisson    -- ^ 非負整数 y、 Poisson(exp η)
  deriving (Show, Eq)

-- | Random intercept GLMM helper (Phase 37-A6)。
--
-- `y ~ X β + u_{group(i)} + (error)` を 1 関数で組み立てる:
--
-- * 固定効果 @β_k ~ Normal(0, 5)@ (p 個)
-- * 群レベル SD @τ_u ~ HalfNormal(5)@
-- * 群効果 @u_j ~ Normal(0, τ_u)@ (nG 個、 centered パラメタ化。
--   群数大 / 群内 N 小なら別途 'nonCenteredNormal' を直接使う)
-- * family に応じた観測:
--     * Gaussian: 残差 @σ ~ Exp(1)@ を sample → @y ~ Normal(X β + u_j, σ)@
--     * Binomial: @y ~ Bernoulli(σ(X β + u_j))@、 y は 0/1
--     * Poisson:  @y ~ Poisson(exp(X β + u_j))@、 y は非負整数
--
-- 観測は単一の構造化ブロック @observeLMR \"y\"@ として発行される (Phase 54.4a・
-- PyMC/Stan と同じく 1 ベクトル化観測ノード。 旧実装は per-obs @y_i@ を n 個展開)。
-- 固定効果は密設計行列・群効果は gather で表現するので vec-tape ハイブリッド
-- gradADU の高速経路に乗る。 chain 上の latent 名:
-- @beta_0, …, beta_{p-1}, tau_u, u_0, …, u_{nG-1}, sigma?@.
--
-- 個別 (random slope や non-centered) が必要ならパターン 5 (random slope) /
-- 形式 C (non-centered) を直接書く方が柔軟。 本 helper は最頻ユースケース
-- 「固定効果 + 群別切片」 専用の shorthand。
glmmRandomIntercept
  :: forall a. (Floating a, Ord a)
  => GlmmFamily   -- ^ 尤度の family
  -> [[Double]]   -- ^ 固定効果 design X (n × p)、 切片は手で 1 列追加すること
  -> [Int]        -- ^ 各観測の group id (0..nG-1)
  -> [Double]     -- ^ 観測 y (length n)
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
-- via stick-breaking
-- latent ベクトル。
--
-- 引数:
--   * @name@   : ベース名。展開後は @<name>_b<i>@ (i=0..K-2) が Beta 由来の
--                棒折り変数、@<name>_<i>@ (i=0..K-1) が deterministic で
--                記録された π 成分。
--   * @alphas@ : 集中度ベクトル α = (α_1,...,α_K)。長さ K ≥ 2。
--
-- アルゴリズム:
--   k = 1..K-1 で β_k ~ Beta(α_k, Σ_{j>k} α_j) を sample する。
--   π_1 = β_1,  π_k = β_k Π_{j<k} (1 − β_j),  π_K = Π_{j<K} (1 − β_j)
--
-- これは π ~ Dirichlet(α) と厳密に等価なので、追加の Jacobian 補正は不要。
-- HMC/NUTS では β_k が UnitIntervalT (logit) で自動的に
-- (0,1) ↔ ℝ 変換されるので、シンプレックス制約は満たされる。
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

-- | Increasing cuts helper for 'OrderedLogistic' / 'OrderedProbit'
-- (Phase 39-A6)。 @c_1 = c_min@、 @c_k = c_{k-1} + d_k@ with
-- @d_k ~ HalfNormal(scale)@ により自動的に increasing 列を保証する。
--
-- 戻り値は長さ @nCuts@ の Track が通る deterministic 値の列
-- (@name_c_0@, …, @name_c_{nCuts-1}@)。 各 @d_k@ は @name_d_k@ で
-- latent として登録される。 cuts は OrderedLogistic / OrderedProbit に
-- そのまま渡せる。
--
-- DAG-safe pattern (Phase 38 で確立): monadic recursion で
-- @deterministic@ の戻り値 (det 名で relabel された Track) を次 step に
-- 渡すことで plate-style の親集合を保つ。
orderedCuts :: forall a. (Floating a, Ord a)
            => Text   -- ^ ベース名
            -> Int    -- ^ カット数 K-1 (≥ 1)
            -> a      -- ^ 最小値 c_min
            -> a      -- ^ 増分の HalfNormal スケール
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

-- | Dirichlet Process の有限近似 stick-breaking (Phase 39-A5)。
-- @β_k ~ Beta(1, α)@ for @k = 1, …, T-1@、 重み
-- @π_k = β_k Π_{j<k}(1 - β_j)@、 @π_T = Π_{j<T}(1 - β_j)@ (残差) で
-- @Σ_k π_k = 1@ を保証。 truncation level @T@ で打ち切る (実用 T = 20-50)。
--
-- 戻り値は長さ @T@ の deterministic Track 列
-- (@name_pi_1@, …, @name_pi_T@)。 @β_k@ は @name_b_k@ で latent 登録。
--
-- DAG-safe: 各 β を sample 後、 累積積を deterministic で chain して
-- π を計算 (Phase 38 確立の規律)。
dpStickBreaking :: forall a. (Floating a, Ord a)
                => Text   -- ^ ベース名
                -> Int    -- ^ truncation level T (≥ 2)
                -> a      -- ^ concentration α (> 0)
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

-- | Hidden Markov Model 用の遷移行列 + 初期分布 prior helper
-- (Phase 39-A4)。 K 状態の HMM について、 初期分布 π_0 と
-- K×K 遷移行列の各行に Dirichlet(α, …, α) prior を置く。
--
-- 戻り値は @(π_0, transitions)@:
-- * @π_0@: 長さ K の確率列 (Σ = 1)、 @name_pi0_<i>@ で deterministic 登録
-- * @transitions@: 長さ K のリスト、 i 番目は遷移行列 i 行目
--   (@name_trans_i_<j>@ で deterministic)
--
-- 離散状態列は **直接 latent としない** (NUTS は離散変数を扱えない)。
-- 代わりに、 ユーザは観測列 @y@ の emission log-prob 行列を計算し、
-- 'hmmForwardLogLik' で状態列をマージナル化した周辺対数尤度を求め、
-- 'potential' で組み込む形を取る。
--
-- 内部実装は既存 'dirichlet' helper を K+1 回呼ぶだけ。 すべて
-- deterministic chain で DAG-safe (Phase 38 規律)。
hmmLatent :: forall a. (Floating a, Ord a)
          => Text   -- ^ ベース名
          -> Int    -- ^ 状態数 K (≥ 2)
          -> a      -- ^ Dirichlet concentration α (> 0、 1 で uniform prior)
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

-- | HMM forward algorithm marginal log-likelihood (Phase 39-A4)。
-- Phase 92 A2 で 'Hanalyze.Model.HBM.Util' へ純粋移設 (ここは re-export
-- のみ・API 不変)。 用法は従来の @'potential' nm (hmmForwardLogLik ...)@ に加え、
-- Normal emission の場合は 'HmmForwardNormal' + 'observeMV' が推奨
-- (勾配コンパイラが forward-backward の閉形式随伴を使えるため大幅に速い)。

-- ---------------------------------------------------------------------------
-- 構造検査
-- ---------------------------------------------------------------------------

data NodeKind = LatentN | ObservedN Int | DeterministicN
              | DataN Int   -- ^ Phase 60.4: データ slot ('dataNamed' / 'dataNamedIx')。
                            --   Int = 長さ。 PyMC の pm.Data (ConstantData) 相当。
  deriving (Show, Eq)

data Node = Node
  { nodeName   :: Text
  , nodeKind   :: NodeKind
  , nodeDist   :: Text         -- 分布名 (e.g. "Normal")
  , nodeDeps   :: Set Text     -- 直接の親 (依存変数)
  , nodePlates :: [Text]       -- Phase 40: plate スタック (外側から内側、 空 = 任意の plate に属さない)
  } deriving (Show)

-- | Walk the model with placeholder zeros and collect 'Node' metadata.
-- 依存関係 ('nodeDeps') は 'extractDeps' を使うこと (placeholder 走査では取れない)。
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

-- | モデル中の 'Data' slot を (名前, placeholder が空か) で列挙する (Phase 60.3)。
-- 同名 slot が複数回現れる場合は 1 entry に集約し、 **いずれかが空なら空扱い**
-- (束縛層の loud error 判定は保守側に倒す)。 'DataIx' slot は 'dataIxSlots'。
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

-- | モデル中の 'DataIx' slot を (名前, placeholder が空か) で列挙する (Phase 60.3)。
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

-- | slot 列挙の重複集約 (先頭出現順を保ち、 空 flag は OR)。
dedupSlots :: [(Text, Bool)] -> [(Text, Bool)]
dedupSlots xs =
  [ (n, or [ e | (n', e) <- xs, n' == n ])
  | n <- nub (map fst xs) ]

