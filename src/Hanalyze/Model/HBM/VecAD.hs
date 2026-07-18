{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Hanalyze.Model.HBM.VecAD
-- Description : 自作の最小 reverse-mode AD (vector-op tape)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 自作・最小 reverse-mode AD (vector-op tape)。 Phase 54.3 第2 spike で
-- 「採用 = 案B (自前 vector-op tape)」 と判断したエンジンを本実装用に移植した
-- もの (`bench/haskell/BenchHBMVecADSpike.hs` の gradHandroll 系)。
--
-- 設計: forward で「ベクトル演算ごとにノードを発番」 し、 各ノードの随伴更新
-- クロージャを逆順リストに積む (= 自前 Wengert tape)。 backward で出力に 1 を
-- seed し、 逆位相順 (= 発番の逆順 = prepend したリストの先頭) にクロージャを
-- replay して入力 (leaf) の随伴を得る。 tape は「ベクトル演算 1 個 = 1 ノード」
-- ゆえ `ad` のスカラ tape (per-scalar-op で O(n) ノード) より桁で小さい。
--
-- スカラは長さ 1 の Storable Vector として随伴を持ち、 ノード随伴は単一の
-- mutable 配列に統一格納する。
--
-- ⚠ 値依存制御フロー (分布の台チェック等) は tape に乗らない。 本エンジンは
-- 構造が値に依らず静的な部分 (Gaussian-恒等リンクの線形予測子 + 二乗和) 専用。
-- 非対応の構造は呼出側で scalar (`ad`) 経路に fallback する。
module Hanalyze.Model.HBM.VecAD
  ( -- * 値ハンドルと文脈
    Rval (..)
  , Ctx
  , ridOf
    -- * tape の実行
  , runTape
    -- * leaf
  , inputVec
  , inputScal
  , constVec
    -- * ベクトル演算 (随伴付き)
  , idxHR
  , sliceHR
  , scaleHR
  , vaddHR
  , vsubHR
  , dotHR
  , gatherHR
  , vexpHR
  , bcastAddHR
  , hadamardHR
  , vmap1HR
    -- * スカラ演算 (随伴付き)
  , map1S
  , cstS
  , addS
  , subS
  , mulS
  , divByS
  , expS
  , logS
  , mulConstS
  , addConstS
  , foldVadd
  ) where

import           Control.Monad (when)
import           Control.Monad.ST
import           Data.Array.ST (STArray, newArray, readArray, writeArray)
import           Data.STRef
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed  as VU

-- ===========================================================================
-- 値ハンドルと tape 文脈
-- ===========================================================================

-- | reverse-mode の値ハンドル: ノード id + primal (scalar / vector)。
data Rval = RScal !Int !Double | RVec !Int !(VS.Vector Double)

ridOf :: Rval -> Int
ridOf (RScal i _) = i
ridOf (RVec  i _) = i

type Adj s = STArray s Int (VS.Vector Double)

-- | 発番カウンタ + backward クロージャ列 (prepend = 発番の逆順)。
data Ctx s = Ctx !(STRef s Int) !(STRef s [Adj s -> ST s ()])

fresh :: Ctx s -> ST s Int
fresh (Ctx cnt _) = do
  n <- readSTRef cnt
  writeSTRef cnt (n + 1)
  pure n

record :: Ctx s -> (Adj s -> ST s ()) -> ST s ()
record (Ctx _ bw) f = modifySTRef' bw (f :)

-- | 随伴の加算 (空 = ゼロ扱い)。
bumpA :: Adj s -> Int -> VS.Vector Double -> ST s ()
bumpA adj i contrib = do
  cur <- readArray adj i
  writeArray adj i (if VS.null cur then contrib else VS.zipWith (+) cur contrib)

readAdjS :: Adj s -> Int -> ST s Double
readAdjS adj i = do
  v <- readArray adj i
  pure (if VS.null v then 0 else v VS.! 0)

-- ===========================================================================
-- tape の実行 (forward build → seed → backward replay)
-- ===========================================================================

-- | tape を構築するアクション (出力ノード + 勾配を読みたい leaf 群を返す) を
-- 受け取り、 forward 評価 → 出力に 1 を seed → backward replay の上で、
-- 各 leaf の随伴 (= 出力の各 leaf に対する勾配ベクトル) を返す。
--
-- @build@ は @(出力 Rval, [leaf Rval])@ を返す。 結果は leaf ごとの随伴
-- ベクトル (RScal leaf は長さ1、 RVec leaf は元の長さ)。
runTape :: (forall s. Ctx s -> ST s (Rval, [Rval])) -> [VS.Vector Double]
runTape build = runST $ do
  cnt <- newSTRef 0
  bw  <- newSTRef []
  let ctx = Ctx cnt bw
  (out, leaves) <- build ctx
  total <- readSTRef cnt
  adj <- newArray (0, max 0 (total - 1)) VS.empty
  writeArray adj (ridOf out) (VS.singleton 1)
  closures <- readSTRef bw
  mapM_ ($ adj) closures
  mapM (\lf -> readArray adj (ridOf lf)) leaves

-- ===========================================================================
-- leaf
-- ===========================================================================

-- | ベクトル leaf (勾配を読む入力)。
inputVec :: Ctx s -> VS.Vector Double -> ST s Rval
inputVec ctx v = do
  i <- fresh ctx
  pure (RVec i v)

-- | スカラ leaf (勾配を読む入力)。
inputScal :: Ctx s -> Double -> ST s Rval
inputScal ctx x = do
  i <- fresh ctx
  pure (RScal i x)

-- | 定数ベクトルノード (backward 無し)。
constVec :: Ctx s -> VS.Vector Double -> ST s Rval
constVec ctx v = do { i <- fresh ctx; pure (RVec i v) }

-- ===========================================================================
-- ベクトル演算 (随伴付き)
-- ===========================================================================

-- | 全長 @l@ の vec から要素 @i@ を取り出す (scalar 化)。 随伴 = e_i·dy。
idxHR :: Ctx s -> Int -> Int -> Rval -> ST s Rval
idxHR ctx l i (RVec vid v) = do
  o <- fresh ctx
  record ctx $ \adj -> do
    g <- readAdjS adj o
    when (g /= 0) $ bumpA adj vid (VS.generate l (\j -> if j == i then g else 0))
  pure (RScal o (v VS.! i))
idxHR _ _ _ _ = error "idxHR: scalar input"

-- | 全長 @l@ の vec から @[off, off+len)@ を切り出す。 随伴は zeros l に散布。
sliceHR :: Ctx s -> Int -> Int -> Int -> Rval -> ST s Rval
sliceHR ctx l off len (RVec vid v) = do
  o <- fresh ctx
  record ctx $ \adj -> do
    dy <- readArray adj o
    when (not (VS.null dy)) $
      bumpA adj vid (VS.generate l (\j -> if j >= off && j < off + len then dy VS.! (j - off) else 0))
  pure (RVec o (VS.slice off len v))
sliceHR _ _ _ _ _ = error "sliceHR: scalar input"

-- | scalar * vector。 ∂scalar = dy·v、 ∂v = scalar·dy。
scaleHR :: Ctx s -> Rval -> Rval -> ST s Rval
scaleHR ctx (RScal kid k) (RVec vid v) = do
  o <- fresh ctx
  record ctx $ \adj -> do
    dy <- readArray adj o
    when (not (VS.null dy)) $ do
      bumpA adj kid (VS.singleton (VS.sum (VS.zipWith (*) dy v)))
      bumpA adj vid (VS.map (* k) dy)
  pure (RVec o (VS.map (* k) v))
scaleHR _ _ _ = error "scaleHR: shape"

-- | vector + vector。
vaddHR :: Ctx s -> Rval -> Rval -> ST s Rval
vaddHR ctx (RVec aid a) (RVec bid b) = do
  o <- fresh ctx
  record ctx $ \adj -> do
    dy <- readArray adj o
    when (not (VS.null dy)) $ do
      bumpA adj aid dy
      bumpA adj bid dy
  pure (RVec o (VS.zipWith (+) a b))
vaddHR _ _ _ = error "vaddHR: shape"

-- | vector - vector。
vsubHR :: Ctx s -> Rval -> Rval -> ST s Rval
vsubHR ctx (RVec aid a) (RVec bid b) = do
  o <- fresh ctx
  record ctx $ \adj -> do
    dy <- readArray adj o
    when (not (VS.null dy)) $ do
      bumpA adj aid dy
      bumpA adj bid (VS.map negate dy)
  pure (RVec o (VS.zipWith (-) a b))
vsubHR _ _ _ = error "vsubHR: shape"

-- | 内積。 ∂a = dy·b、 ∂b = dy·a。
dotHR :: Ctx s -> Rval -> Rval -> ST s Rval
dotHR ctx (RVec aid a) (RVec bid b) = do
  o <- fresh ctx
  record ctx $ \adj -> do
    g <- readAdjS adj o
    when (g /= 0) $ do
      bumpA adj aid (VS.map (* g) b)
      bumpA adj bid (VS.map (* g) a)
  pure (RScal o (VS.sum (VS.zipWith (*) a b)))
dotHR _ _ _ = error "dotHR: shape"

-- | @u[gids]@ gather (gids/nG は定数)。 随伴は scatter-add で O(n)。
gatherHR :: Ctx s -> VU.Vector Int -> Int -> Rval -> ST s Rval
gatherHR ctx gids nG (RVec uid u) = do
  let n = VU.length gids
  o <- fresh ctx
  record ctx $ \adj -> do
    dy <- readArray adj o
    when (not (VS.null dy)) $
      bumpA adj uid (VS.convert $
        VU.accumulate (+) (VU.replicate nG 0) (VU.zip gids (VU.convert dy :: VU.Vector Double)))
  pure (RVec o (VS.generate n (\i -> u VS.! (gids VU.! i))))
gatherHR _ _ _ _ = error "gatherHR: shape"

-- | elementwise exp (Phase 54.11 spike: 非線形 μ 用)。 ∂v = dy ⊙ exp(v)。
vexpHR :: Ctx s -> Rval -> ST s Rval
vexpHR ctx (RVec vid v) = do
  let ev = VS.map exp v
  o <- fresh ctx
  record ctx $ \adj -> do
    dy <- readArray adj o
    when (not (VS.null dy)) $
      bumpA adj vid (VS.zipWith (*) dy ev)
  pure (RVec o ev)
vexpHR _ _ = error "vexpHR: scalar input"

-- | scalar + vector の broadcast 加算 (Phase 54.11 spike)。 ∂scalar = Σ dy。
bcastAddHR :: Ctx s -> Rval -> Rval -> ST s Rval
bcastAddHR ctx (RScal kid k) (RVec vid v) = do
  o <- fresh ctx
  record ctx $ \adj -> do
    dy <- readArray adj o
    when (not (VS.null dy)) $ do
      bumpA adj kid (VS.singleton (VS.sum dy))
      bumpA adj vid dy
  pure (RVec o (VS.map (+ k) v))
bcastAddHR _ _ _ = error "bcastAddHR: shape"

-- | elementwise 積 v ⊙ w (Phase 54.11 spike: gather(a)[i]·exp(-b·x_i) 用)。
-- ∂v = dy ⊙ w、 ∂w = dy ⊙ v。
hadamardHR :: Ctx s -> Rval -> Rval -> ST s Rval
hadamardHR ctx (RVec aid a) (RVec bid b) = do
  o <- fresh ctx
  record ctx $ \adj -> do
    dy <- readArray adj o
    when (not (VS.null dy)) $ do
      bumpA adj aid (VS.zipWith (*) dy b)
      bumpA adj bid (VS.zipWith (*) dy a)
  pure (RVec o (VS.zipWith (*) a b))
hadamardHR _ _ _ = error "hadamardHR: shape"

-- | 汎用 elementwise 単項 (Phase 54.11: ベクトル式 IR の log/recip/sqrt/tanh 等)。
-- @f@ とその導関数 @f'@ を受け、 ∂v = dy ⊙ f'(v) (v は入力 primal)。
vmap1HR :: Ctx s -> (Double -> Double) -> (Double -> Double) -> Rval -> ST s Rval
vmap1HR ctx f df (RVec vid v) = do
  o <- fresh ctx
  record ctx $ \adj -> do
    dy <- readArray adj o
    when (not (VS.null dy)) $
      bumpA adj vid (VS.zipWith (\g x -> g * df x) dy v)
  pure (RVec o (VS.map f v))
vmap1HR _ _ _ _ = error "vmap1HR: scalar input"

-- | 非空ベクトルノード列を vadd で畳む。
foldVadd :: Ctx s -> [Rval] -> ST s Rval
foldVadd _   []       = error "foldVadd: empty"
foldVadd _   [x]      = pure x
foldVadd ctx (x:y:xs) = vaddHR ctx x y >>= \z -> foldVadd ctx (z : xs)

-- ===========================================================================
-- スカラ演算 (随伴付き)
-- ===========================================================================

cstS :: Ctx s -> Double -> ST s Rval
cstS ctx x = do { i <- fresh ctx; pure (RScal i x) }

binS :: Ctx s -> (Double -> Double -> Double) -> (Double -> Double -> (Double, Double))
     -> Rval -> Rval -> ST s Rval
binS ctx f df (RScal aid a) (RScal bid b) = do
  o <- fresh ctx
  record ctx $ \adj -> do
    g <- readAdjS adj o
    when (g /= 0) $ do
      let (da, db) = df a b
      bumpA adj aid (VS.singleton (g * da))
      bumpA adj bid (VS.singleton (g * db))
  pure (RScal o (f a b))
binS _ _ _ _ _ = error "binS: scalar expected"

addS, subS, mulS :: Ctx s -> Rval -> Rval -> ST s Rval
addS ctx = binS ctx (+) (\_ _ -> (1, 1))
subS ctx = binS ctx (-) (\_ _ -> (1, -1))
mulS ctx = binS ctx (*) (\a b -> (b, a))

-- | scalar 除算 (a/b)。
divByS :: Ctx s -> Rval -> Rval -> ST s Rval
divByS ctx = binS ctx (/) (\a b -> (1 / b, negate a / (b * b)))

unS :: Ctx s -> (Double -> Double) -> (Double -> Double) -> Rval -> ST s Rval
unS ctx f df (RScal aid a) = do
  o <- fresh ctx
  record ctx $ \adj -> do
    g <- readAdjS adj o
    when (g /= 0) $ bumpA adj aid (VS.singleton (g * df a))
  pure (RScal o (f a))
unS _ _ _ _ = error "unS: scalar expected"

expS, logS :: Ctx s -> Rval -> ST s Rval
expS ctx = unS ctx exp exp
logS ctx = unS ctx log (\a -> 1 / a)

-- | 汎用スカラ単項 (Phase 54.11)。 @f@ と導関数 @f'@ を受ける ('unS' の公開形)。
map1S :: Ctx s -> (Double -> Double) -> (Double -> Double) -> Rval -> ST s Rval
map1S = unS

mulConstS, addConstS :: Ctx s -> Double -> Rval -> ST s Rval
mulConstS ctx c = unS ctx (* c) (const c)
addConstS ctx c = unS ctx (+ c) (const 1)
