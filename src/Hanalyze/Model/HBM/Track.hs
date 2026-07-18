{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Hanalyze.Model.HBM.Track
-- Description : HBM の依存追跡型 Track (latent 変数への依存伝播)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Phase 58.6b: 依存追跡型 'Track' を 'Hanalyze.Model.HBM' から分離。
--
-- 'Track' は @Floating@ 演算を通して「この値はどの latent 変数に依存するか」を
-- 伝播する型。 'ModelP' をこの型で特殊化することで各 Observe / Deterministic
-- ノードの親集合を自動抽出する ('extractDeps')。 DAG 可視化 (buildModelGraph)
-- の基盤。
--
-- 依存は下層 'Hanalyze.Model.HBM.Model' (Node / ModelF / lmParents 等) と
-- '...Distribution' (Distribution / distName) のみ。 評価層 (logJoint 等) には
-- 依存しない (runTrack = logJoint の Track 特殊化は Eval 層に置く)。
module Hanalyze.Model.HBM.Track
  ( Track (..)
  , trackVar
  , trackConst
  , extractDeps
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Hanalyze.Model.HBM.Model
import Hanalyze.Model.HBM.Distribution

-- ---------------------------------------------------------------------------
-- 依存追跡型 Track
-- ---------------------------------------------------------------------------

-- | Floating 演算を通して「この値はどの変数に依存するか」を伝播する型。
--
-- @ModelP@ をこの型で特殊化することで、各 Observe ノードが
-- どの latent 変数に依存しているか自動抽出できる。
data Track = Track
  { trackVal  :: !Double
  , trackDeps :: !(Set Text)
  } deriving (Show, Eq)

-- | 変数として登場する Track (deps に自分の名前を入れる)。
trackVar :: Text -> Double -> Track
trackVar n v = Track v (Set.singleton n)

-- | 定数として扱う Track (deps なし)。
trackConst :: Double -> Track
trackConst v = Track v Set.empty

-- Phase 60.7: '!!!' の依存タグ注入。 Track 解釈だけが slot 名を依存集合に
-- 足し、 DAG に slot→利用先のエッジを出す (数値解釈は既定 id)。
instance TrackTag Track where
  tagDep nm (Track v ds) = Track v (Set.insert nm ds)

-- 自然な順序関係 (Double の比較を使う)
instance Ord Track where
  compare a b = compare (trackVal a) (trackVal b)

-- Floating の階段
instance Num Track where
  fromInteger n = trackConst (fromInteger n)
  Track a sa + Track b sb = Track (a + b) (sa <> sb)
  Track a sa - Track b sb = Track (a - b) (sa <> sb)
  Track a sa * Track b sb = Track (a * b) (sa <> sb)
  abs    (Track a sa) = Track (abs a) sa
  signum (Track a sa) = Track (signum a) sa
  negate (Track a sa) = Track (negate a) sa

instance Fractional Track where
  fromRational r = trackConst (fromRational r)
  Track a sa / Track b sb = Track (a / b) (sa <> sb)

instance Floating Track where
  pi             = trackConst pi
  exp   (Track a sa) = Track (exp   a) sa
  log   (Track a sa) = Track (log   a) sa
  sin   (Track a sa) = Track (sin   a) sa
  cos   (Track a sa) = Track (cos   a) sa
  tan   (Track a sa) = Track (tan   a) sa
  asin  (Track a sa) = Track (asin  a) sa
  acos  (Track a sa) = Track (acos  a) sa
  atan  (Track a sa) = Track (atan  a) sa
  sinh  (Track a sa) = Track (sinh  a) sa
  cosh  (Track a sa) = Track (cosh  a) sa
  tanh  (Track a sa) = Track (tanh  a) sa
  asinh (Track a sa) = Track (asinh a) sa
  acosh (Track a sa) = Track (acosh a) sa
  atanh (Track a sa) = Track (atanh a) sa
  sqrt  (Track a sa) = Track (sqrt  a) sa
  Track a sa ** Track b sb = Track (a ** b) (sa <> sb)
  logBase (Track a sa) (Track b sb) = Track (logBase a b) (sa <> sb)

instance Real Track where
  toRational = toRational . trackVal

instance RealFrac Track where
  properFraction (Track a sa) = let (i, f) = properFraction a in (i, Track f sa)

-- | モデルを Track 型で実行し、各ノードの依存関係を抽出する。
--
-- Sample n: その変数自体は @{n}@ に依存する (自己依存)。
-- Observe n: 分布のパラメータに含まれる latent 変数の集合を deps とする。
--
-- Phase 40: plate スタックを保持し、 各 Node に 'nodePlates' を埋める。
-- 同時に出現した plate (name, size) を 'Map Text Int' で返す。
extractDeps :: forall r. ModelP r -> ([Node], Map Text Int)
extractDeps m =
  let (ns, plates) = go m [] [] Map.empty Map.empty Map.empty in (ns, plates)
  where
    -- 引数 stack は **inner-most が head** の plate 名スタック。
    -- slots / obsAcc は Phase 63.1 の side map: slots = データ slot の生値
    -- (slot 名 → ys)、 obsAcc = observe の生 ys を obs 名ごとに chunk 蓄積
    -- (per-point loop の observe \"y\" … [y] も連結すれば slot 全列と一致する)。
    -- walk 終端 (Pure) で値一致逆引きし obs→slot エッジを張る ('linkObsSlots')。
    go :: Model Track r -> [Text] -> [Node] -> Map Text Int
       -> Map Text [Double] -> Map Text [[Double]] -> ([Node], Map Text Int)
    go (Pure _) _ acc plates slots obsAcc =
      (reverse (linkObsSlots slots obsAcc acc), plates)
    go (Free (Sample n d k)) stack acc plates slots obsAcc =
      let parentDeps = distDepsT d
          node = Node n LatentN (distName d) parentDeps (reverse stack)
          v    = trackVar n 1.0  -- 1 にすると log/exp が安全
      in go (k v) stack (node : acc) plates slots obsAcc
    go (Free (Observe n d ys next)) stack acc plates slots obsAcc =
      let parentDeps = distDepsT d
          node = Node n (ObservedN (length ys)) (distName d) parentDeps (reverse stack)
      in go next stack (node : acc) plates slots (obsChunk n ys obsAcc)
    go (Free (ObserveLM n bs _ re fam ys next)) stack acc plates slots obsAcc =
      -- 親 = β + u + 分散パラメタ名 (lmParents)。 観測ブロックは 1 ノード。
      let parentDeps = lmParents bs re fam
          node = Node n (ObservedN (length ys)) (lmFamilyName fam) parentDeps (reverse stack)
      in go next stack (node : acc) plates slots (obsChunk n ys obsAcc)
    go (Free (Potential nm v next)) stack acc plates slots obsAcc =
      -- Potential も DAG 上は「依存を持つ無形ノード」として可視化
      let parentDeps = trackDeps v
          node = Node nm LatentN "Potential" parentDeps (reverse stack)
      in go next stack (node : acc) plates slots obsAcc
    go (Free (Deterministic nm v k)) stack acc plates slots obsAcc =
      -- Deterministic ノードの親は @v@ が触れた latent 集合。
      -- 継続には deps を @{nm}@ に「再ラベル」 した Track を渡し、 下流が
      -- @v@ の遠い親 (mu, tau 等) ではなく **det 名 nm そのもの** を
      -- 親として認識するようにする (Phase 38 で plate-style DAG に修正)。
      -- 数値値は元の @trackVal v@ を保持 (下流の log/exp 等が安全)。
      let parentDeps = trackDeps v
          node = Node nm DeterministicN "Deterministic" parentDeps (reverse stack)
          v'   = Track (trackVal v) (Set.singleton nm)
      in go (k v') stack (node : acc) plates slots obsAcc
    go (Free (Data n ys k)) stack acc plates slots obsAcc =
      -- Phase 60.4: pm.Data 相当のデータノード。 値 (fst view) には slot 名の
      -- dep タグを載せ、 下流 (deterministic / observe の dist パラメタ) が
      -- x→mu のエッジを自動で張れるようにする (Phase 38 deterministic
      -- re-label と同手法)。 snd view (dataNamedObs の生 [Double]) には
      -- deps を載せられないため、 slots に生値を控えて walk 終端で
      -- 値一致逆引きの obs→slot エッジを張る (Phase 63.1)。
      let node = Node n (DataN (length ys)) "Data" Set.empty (reverse stack)
          vals = map (\v -> Track v (Set.singleton n)) ys
      in go (k (vals, ys)) stack (node : acc) plates (Map.insert n ys slots) obsAcc
    go (Free (DataIx n is k)) stack acc plates slots obsAcc =
      -- DataIx は [Int] のまま継続に渡すため dep タグは載らない (ノードのみ)。
      -- observe の ys ([Double]) と一致し得ないので slots にも入れない。
      let node = Node n (DataN (length is)) "DataIx" Set.empty (reverse stack)
      in go (k is) stack (node : acc) plates slots obsAcc
    go (Free (PlateBegin nm sz next)) stack acc plates slots obsAcc =
      -- plate を開始 = stack に push、 サイズも記録 (重複時は新値で上書き
      -- = 同名 plate は同サイズ前提)
      go next (nm : stack) acc (Map.insert nm sz plates) slots obsAcc
    go (Free (PlateEnd next)) stack acc plates slots obsAcc =
      -- plate を終了 = stack から pop。 空 stack は誤用 (PlateBegin 抜き
      -- で PlateEnd が来た等) — 黙って無視する
      let stack' = case stack of { _ : t -> t; [] -> [] }
      in go next stack' acc plates slots obsAcc

    -- obs 名ごとの ys chunk 蓄積 (新 chunk を先頭 prepend = 逆順保持。
    -- per-observe の list append による O(n²) を避ける)。
    obsChunk :: Text -> [Double] -> Map Text [[Double]] -> Map Text [[Double]]
    obsChunk n ys = Map.insertWith (++) n [ys]

    -- Phase 63.1: observe の連結 ys と値一致するデータ slot へ obs→slot エッジ
    -- (= 該当 DataN Node の nodeDeps に obs 名を追加。 nodeDeps は「直接の親」
    -- ゆえ slot は obs の子 = PyMC `make_compute_graph` の obs→y と同型)。
    --
    -- - 値一致は plate 長さ match (60.6) と同種の表示専用ヒューリスティック:
    --   偶然同値の slot にも張られる (既知 caveat・doc 明記)、 同値 slot 複数は
    --   全部に張る。 空 slot (未 bind placeholder) は対象外。
    -- - 同名 (dataNamedObs \"y\" + observe \"y\" の docs 慣例) は対象外:
    --   mergeByName で 1 ノードに統合されるため自己ループになる。
    -- - 引数 acc は逆順のまま受けて逆順のまま返す (呼び元 Pure 節で reverse)。
    linkObsSlots :: Map Text [Double] -> Map Text [[Double]] -> [Node] -> [Node]
    linkObsSlots slots obsAcc acc
      | Map.null links = acc
      | otherwise      = map upd acc
      where
        -- obs 名 → 連結 ys (chunk は新しい順 prepend 蓄積ゆえ reverse)
        obsYs = Map.map (concat . reverse) obsAcc
        -- slot 名 → 親として足す obs 名集合
        links = Map.fromListWith Set.union
          [ (slotName, Set.singleton obsName)
          | (slotName, sv) <- Map.toList slots
          , not (null sv)
          , (obsName, ys) <- Map.toList obsYs
          , obsName /= slotName
          , sv == ys ]
        upd nd = case nodeKind nd of
          DataN _ | Just parents <- Map.lookup (nodeName nd) links ->
            nd { nodeDeps = nodeDeps nd <> parents }
          _ -> nd

-- | Distribution Track に含まれる依存変数集合を取り出す。
distDepsT :: Distribution Track -> Set Text
distDepsT (Normal mu sig)    = trackDeps mu <> trackDeps sig
distDepsT (Exponential r)    = trackDeps r
distDepsT (Gamma s r)        = trackDeps s <> trackDeps r
distDepsT (Beta a b)         = trackDeps a <> trackDeps b
distDepsT (Poisson lam)      = trackDeps lam
distDepsT (Binomial _ p)     = trackDeps p
distDepsT (Uniform lo hi)    = trackDeps lo <> trackDeps hi
distDepsT (StudentT df mu s) = trackDeps df <> trackDeps mu <> trackDeps s
distDepsT (Cauchy loc s)     = trackDeps loc <> trackDeps s
distDepsT (HalfNormal s)     = trackDeps s
distDepsT (HalfCauchy s)     = trackDeps s
distDepsT (LogNormal mu s)   = trackDeps mu <> trackDeps s
distDepsT (Bernoulli p)      = trackDeps p
distDepsT (Categorical ps)   = mconcat (map trackDeps ps)
distDepsT (Mixture ws ds)    = mconcat (map trackDeps ws) <> mconcat (map distDepsT ds)
distDepsT (Truncated d mLo mHi) =
  distDepsT d <> maybe mempty trackDeps mLo <> maybe mempty trackDeps mHi
distDepsT (Censored  d mLo mHi) =
  distDepsT d <> maybe mempty trackDeps mLo <> maybe mempty trackDeps mHi
distDepsT (MvNormal mus covRows) =
  mconcat (map trackDeps mus)
    <> mconcat (concatMap (map trackDeps) covRows)
distDepsT (MvNormalChol mus sigmas lRows) =
  mconcat (map trackDeps mus)
    <> mconcat (map trackDeps sigmas)
    <> mconcat (concatMap (map trackDeps) lRows)
distDepsT (MvNormalGpRBF xs alpha rho sigma) =   -- Phase 95 B-dsl: x は data・α/ρ/σ が param
  mconcat (map trackDeps xs)
    <> trackDeps alpha <> trackDeps rho <> trackDeps sigma
distDepsT (HmmForwardNormal pi0 trans mus sg) =   -- Phase 92 A2: 全て param 側 (data は Observe に載る)
  mconcat (map trackDeps pi0)
    <> mconcat (concatMap (map trackDeps) trans)
    <> mconcat (map trackDeps mus) <> trackDeps sg
distDepsT (ArmaNormal mu phi theta sg) =   -- Phase 101 A2: 全て param 側 (data は Observe に載る)
  trackDeps mu <> trackDeps phi <> trackDeps theta <> trackDeps sg
distDepsT (GradedResponseIrt thetas _ _ _) =   -- Phase 101 A3: θs のみ param 側 (他は定数 data)
  mconcat (map trackDeps thetas)
distDepsT (NegativeBinomial mu alpha) = trackDeps mu <> trackDeps alpha
distDepsT (Multinomial _ ps) = mconcat (map trackDeps ps)
distDepsT (ZeroInflatedPoisson psi lam) = trackDeps psi <> trackDeps lam
distDepsT (ZeroInflatedBinomial _ psi p) = trackDeps psi <> trackDeps p
distDepsT (InverseGamma a b) = trackDeps a <> trackDeps b
distDepsT (Weibull k l)      = trackDeps k <> trackDeps l
distDepsT (Pareto a xm)      = trackDeps a <> trackDeps xm
distDepsT (BetaBinomial _ a b) = trackDeps a <> trackDeps b
distDepsT (VonMises mu k)    = trackDeps mu <> trackDeps k
-- Phase 37 で追加した分布 (Phase 38 補修で網羅追加)
distDepsT (SkewNormal mu sig alpha) =
  trackDeps mu <> trackDeps sig <> trackDeps alpha
distDepsT (Logistic mu s)    = trackDeps mu <> trackDeps s
distDepsT (Gumbel mu beta)   = trackDeps mu <> trackDeps beta
distDepsT (AsymmetricLaplace b kappa mu) =
  trackDeps b <> trackDeps kappa <> trackDeps mu
distDepsT (OrderedLogistic eta cuts) =
  trackDeps eta <> mconcat (map trackDeps cuts)
distDepsT DiscreteUniform{}  = mempty   -- Int 引数のみ
distDepsT (Geometric p)      = trackDeps p
distDepsT HyperGeometric{}   = mempty   -- Int 引数のみ
distDepsT (ZeroInflatedNegativeBinomial psi mu alpha) =
  trackDeps psi <> trackDeps mu <> trackDeps alpha
distDepsT (MvStudentT nu mus covRows) =
  trackDeps nu
    <> mconcat (map trackDeps mus)
    <> mconcat (concatMap (map trackDeps) covRows)
distDepsT (DirichletMultinomial _ alphas) =
  mconcat (map trackDeps alphas)
distDepsT (Triangular lo c hi) =
  trackDeps lo <> trackDeps c <> trackDeps hi
distDepsT (Kumaraswamy a b)    = trackDeps a <> trackDeps b
distDepsT (Rice nu sig)        = trackDeps nu <> trackDeps sig
distDepsT (DiscreteWeibull q beta) = trackDeps q <> trackDeps beta
distDepsT (Wishart nu vRows) =
  trackDeps nu <> mconcat (concatMap (map trackDeps) vRows)
distDepsT (Bound d mLo mHi) =
  distDepsT d
    <> maybe mempty trackDeps mLo
    <> maybe mempty trackDeps mHi
distDepsT (OrderedProbit eta cuts) =
  trackDeps eta <> mconcat (map trackDeps cuts)

