{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Hanalyze.Model.HBM.VecAD
-- Description : 自作の最小 reverse-mode AD (vector-op tape)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 自作・最小 reverse-mode AD (vector-op tape)。 「採用 = 案B
--   (自前 vector-op tape)」 と判断したエンジンを本実装用に移植したもの
--   (`bench/haskell/BenchHBMVecADSpike.hs` の gradHandroll 系)。
--
--   設計: forward で「ベクトル演算ごとにノードを発番」 し、 各ノードの随伴更新
--   クロージャを逆順リストに積む (= 自前 Wengert tape)。 backward で出力に 1 を
--   seed し、 逆位相順 (= 発番の逆順 = prepend したリストの先頭) にクロージャを
--   replay して入力 (leaf) の随伴を得る。 tape は「ベクトル演算 1 個 = 1 ノード」
--   ゆえ @ad@ のスカラ tape (per-scalar-op で O(n) ノード) より桁で小さい。
--
--   スカラは長さ 1 の Storable Vector として随伴を持ち、 ノード随伴は単一の
--   mutable 配列に統一格納する。
--
--   注意: 値依存制御フロー (分布の台チェック等) は tape に乗らない。 本エンジンは
--   構造が値に依らず静的な部分 (Gaussian-恒等リンクの線形予測子 + 二乗和) 専用。
--   非対応の構造は呼出側で scalar (@ad@) 経路に fallback する。
-- [English]: A hand-rolled minimal reverse-mode AD (vector-op tape). This is
--   the engine chosen ("adopt = plan B, a custom vector-op tape") and ported
--   into the production implementation from the spike
--   (`bench/haskell/BenchHBMVecADSpike.hs`'s gradHandroll family).
--
--   Design: on the forward pass, each vector operation is numbered as a node,
--   and each node's adjoint-update closure is pushed onto a reverse-order list
--   (= a custom Wengert tape). On the backward pass, the output is seeded with
--   1, and closures are replayed in reverse topological order (= reverse
--   numbering order = the head of the prepended list) to obtain the adjoints
--   of the input leaves. Because the tape records "one vector operation = one
--   node," it is orders of magnitude smaller than @ad@'s scalar tape (which
--   creates O(n) nodes per scalar op).
--
--   Scalars carry their adjoint as a length-1 Storable Vector, and all node
--   adjoints are stored uniformly in a single mutable array.
--
--   Caveat: value-dependent control flow (e.g. distribution support checks)
--   does not survive on the tape. This engine only covers the part of the
--   structure that is static regardless of value (Gaussian-identity-link
--   linear predictor + sum of squares). Unsupported structures fall back to
--   the scalar (@ad@) path at the call site.
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

-- | [日本語]: reverse-mode の値ハンドル: ノード id + primal (scalar / vector)。
--   [English]: A reverse-mode value handle: node id + primal (scalar / vector).
data Rval = RScal !Int !Double | RVec !Int !(VS.Vector Double)

ridOf :: Rval -> Int
ridOf (RScal i _) = i
ridOf (RVec  i _) = i

type Adj s = STArray s Int (VS.Vector Double)

-- | [日本語]: 発番カウンタ + backward クロージャ列 (prepend = 発番の逆順)。
--   [English]: A numbering counter plus the list of backward closures
--   (prepend order = reverse numbering order).
data Ctx s = Ctx !(STRef s Int) !(STRef s [Adj s -> ST s ()])

fresh :: Ctx s -> ST s Int
fresh (Ctx cnt _) = do
  n <- readSTRef cnt
  writeSTRef cnt (n + 1)
  pure n

record :: Ctx s -> (Adj s -> ST s ()) -> ST s ()
record (Ctx _ bw) f = modifySTRef' bw (f :)

-- | [日本語]: 随伴の加算 (空 = ゼロ扱い)。
--   [English]: Accumulate into an adjoint (an empty vector is treated as zero).
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

-- | [日本語]: tape を構築するアクション (出力ノード + 勾配を読みたい leaf 群を
--   返す) を受け取り、 forward 評価 → 出力に 1 を seed → backward replay の
--   上で、 各 leaf の随伴 (= 出力の各 leaf に対する勾配ベクトル) を返す。
--
--   @build@ は @(出力 Rval, [leaf Rval])@ を返す。 結果は leaf ごとの随伴
--   ベクトル (RScal leaf は長さ1、 RVec leaf は元の長さ)。
--   [English]: Takes an action that builds the tape (returning the output
--   node plus the leaves whose gradient we want to read), then performs a
--   forward evaluation, seeds the output with 1, replays backward, and
--   returns the adjoint of each leaf (= the gradient vector with respect to
--   that leaf).
--
--   @build@ returns @(output Rval, [leaf Rval])@. The result is one adjoint
--   vector per leaf (length 1 for an RScal leaf, original length for an
--   RVec leaf).
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

-- | [日本語]: ベクトル leaf (勾配を読む入力)。
--   [English]: A vector leaf (an input whose gradient we read).
inputVec :: Ctx s -> VS.Vector Double -> ST s Rval
inputVec ctx v = do
  i <- fresh ctx
  pure (RVec i v)

-- | [日本語]: スカラ leaf (勾配を読む入力)。
--   [English]: A scalar leaf (an input whose gradient we read).
inputScal :: Ctx s -> Double -> ST s Rval
inputScal ctx x = do
  i <- fresh ctx
  pure (RScal i x)

-- | [日本語]: 定数ベクトルノード (backward 無し)。
--   [English]: A constant vector node (no backward closure).
constVec :: Ctx s -> VS.Vector Double -> ST s Rval
constVec ctx v = do { i <- fresh ctx; pure (RVec i v) }

-- ===========================================================================
-- ベクトル演算 (随伴付き)
-- ===========================================================================

-- | [日本語]: 全長 @l@ の vec から要素 @i@ を取り出す (scalar 化)。 随伴 = e_i·dy。
--   [English]: Extracts element @i@ from a length-@l@ vec (turning it into a
--   scalar). Adjoint = e_i·dy.
idxHR :: Ctx s -> Int -> Int -> Rval -> ST s Rval
idxHR ctx l i (RVec vid v) = do
  o <- fresh ctx
  record ctx $ \adj -> do
    g <- readAdjS adj o
    when (g /= 0) $ bumpA adj vid (VS.generate l (\j -> if j == i then g else 0))
  pure (RScal o (v VS.! i))
idxHR _ _ _ _ = error "idxHR: scalar input"

-- | [日本語]: 全長 @l@ の vec から @[off, off+len)@ を切り出す。 随伴は
--   zeros l に散布。
--   [English]: Slices out @[off, off+len)@ from a length-@l@ vec. The
--   adjoint is scattered into zeros of length l.
sliceHR :: Ctx s -> Int -> Int -> Int -> Rval -> ST s Rval
sliceHR ctx l off len (RVec vid v) = do
  o <- fresh ctx
  record ctx $ \adj -> do
    dy <- readArray adj o
    when (not (VS.null dy)) $
      bumpA adj vid (VS.generate l (\j -> if j >= off && j < off + len then dy VS.! (j - off) else 0))
  pure (RVec o (VS.slice off len v))
sliceHR _ _ _ _ _ = error "sliceHR: scalar input"

-- | [日本語]: scalar * vector。 ∂scalar = dy·v、 ∂v = scalar·dy。
--   [English]: scalar * vector. ∂scalar = dy·v, ∂v = scalar·dy.
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

-- | [日本語]: vector + vector。
--   [English]: vector + vector.
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

-- | [日本語]: vector - vector。
--   [English]: vector - vector.
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

-- | [日本語]: 内積。 ∂a = dy·b、 ∂b = dy·a。
--   [English]: Dot product. ∂a = dy·b, ∂b = dy·a.
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

-- | [日本語]: @u[gids]@ gather (gids/nG は定数)。 随伴は scatter-add で O(n)。
--   [English]: A @u[gids]@ gather (gids/nG are constants). The adjoint is
--   an O(n) scatter-add.
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

-- | [日本語]: elementwise exp (非線形 μ 用)。 ∂v = dy ⊙ exp(v)。
--   [English]: Elementwise exp (for a non-linear μ). ∂v = dy ⊙ exp(v).
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

-- | [日本語]: scalar + vector の broadcast 加算。 ∂scalar = Σ dy。
--   [English]: A broadcast addition of scalar + vector. ∂scalar = Σ dy.
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

-- | [日本語]: elementwise 積 v ⊙ w (gather(a)[i]·exp(-b·x_i) 用)。
--   ∂v = dy ⊙ w、 ∂w = dy ⊙ v。
--   [English]: An elementwise product v ⊙ w (for gather(a)[i]·exp(-b·x_i)).
--   ∂v = dy ⊙ w, ∂w = dy ⊙ v.
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

-- | [日本語]: 汎用 elementwise 単項 (ベクトル式 IR の log/recip/sqrt/tanh 等)。
--   @f@ とその導関数 @f'@ を受け、 ∂v = dy ⊙ f'(v) (v は入力 primal)。
--   [English]: A generic elementwise unary op (for the vector-expression IR's
--   log/recip/sqrt/tanh, etc.). Takes @f@ and its derivative @f'@;
--   ∂v = dy ⊙ f'(v) (v is the input primal).
vmap1HR :: Ctx s -> (Double -> Double) -> (Double -> Double) -> Rval -> ST s Rval
vmap1HR ctx f df (RVec vid v) = do
  o <- fresh ctx
  record ctx $ \adj -> do
    dy <- readArray adj o
    when (not (VS.null dy)) $
      bumpA adj vid (VS.zipWith (\g x -> g * df x) dy v)
  pure (RVec o (VS.map f v))
vmap1HR _ _ _ _ = error "vmap1HR: scalar input"

-- | [日本語]: 非空ベクトルノード列を vadd で畳む。
--   [English]: Folds a non-empty list of vector nodes with vadd.
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

-- | [日本語]: scalar 除算 (a/b)。
--   [English]: Scalar division (a/b).
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

-- | [日本語]: 汎用スカラ単項。 @f@ と導関数 @f'@ を受ける ('unS' の公開形)。
--   [English]: A generic scalar unary op. Takes @f@ and its derivative @f'@
--   (the public form of 'unS').
map1S :: Ctx s -> (Double -> Double) -> (Double -> Double) -> Rval -> ST s Rval
map1S = unS

mulConstS, addConstS :: Ctx s -> Double -> Rval -> ST s Rval
mulConstS ctx c = unS ctx (* c) (const c)
addConstS ctx c = unS ctx (+ c) (const 1)
