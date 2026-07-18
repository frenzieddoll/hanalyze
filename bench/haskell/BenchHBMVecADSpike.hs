{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- GHC 9.6.7 の exitification パスが本モジュールで simplifier panic (completeCall)
-- を起こすため無効化。 4 手法とも同一フラグでコンパイルされるので比較は不変。
{-# OPTIONS_GHC -fno-exitification #-}
-- | Phase 54 専用ベクトル化 AD feasibility spike (計測先行・推測するな計測せよ)。
--
-- 「numpyro に階層モデルで追いつくには `ad` のボックススカラ tape を、 Storable
-- 配列上の tape-free なベクトル化 reverse-mode に置換する必要がある」 という
-- 仮説を、 **本実装の前に**実測で検証する小実験。
--
-- 対象 = 階層 Gaussian (random intercept GLMM、 BenchHBMADModes の M2 と同型):
--   η_i = Σ_k β_k X_ik + u_{g(i)},  y_i ~ Normal(η_i, σ)
--   prior: β_k~N(0,5)、 τ_u~HalfNormal(5)、 u_j~N(0,τ_u)、 σ~Exp(1)
--   unconstrained: β/u は identity、 τ_u/σ は log 変換 (PositiveT、 jacobian +u)。
--
-- 勾配を 2 通りで計算し per-grad 時間を比較:
--   (a) `Numeric.AD.Mode.Reverse.Double.grad` — 現状の方式 (スカラ tape)。
--   (b) 手書きベクトル化解析勾配 — Storable/Unboxed 配列上の reduction のみ。
--       これは tape を一切作らない = ベクトル化 reverse-mode AD の **時間の下限**
--       (汎用エンジンはこれより遅いが、 ここが (a) を桁で上回らなければ専用 AD を
--        作っても勝てない、 という feasibility の天井判定に使う)。
--
-- (b) の正しさは中心差分 (同じ logp) で検証してから時間を測る。
module Main where

import           Control.Monad                  (forM_, when)
import           Control.Monad.ST               (ST, runST)
import           Data.Array.ST                  (STArray, newArray, readArray,
                                                 writeArray)
import           Data.List                      (foldl')
import           Data.STRef                     (STRef, modifySTRef', newSTRef,
                                                 readSTRef, writeSTRef)
import qualified Data.Vector.Storable           as VS
import qualified Data.Vector.Unboxed            as VU
import           Text.Printf                    (printf)

import qualified Numeric.AD.Mode.Reverse.Double as RevD

import           Numeric.Backprop               (BVar, Reifies, W, auto, gradBP,
                                                 liftOp1, liftOp2, op1, op2)

import           BenchUtil                      (timeitIO)

-- ---------------------------------------------------------------------------
-- 問題サイズと合成データ
-- ---------------------------------------------------------------------------

data Prob = Prob
  { pP      :: !Int          -- 固定効果数
  , pNG     :: !Int          -- 群数
  , pXRows  :: ![[Double]]   -- design X (n × p)
  , pGids   :: ![Int]        -- group id (length n)
  , pYs     :: ![Double]     -- 観測 (length n)
  }

-- BenchHBMADModes.genM2Data と同型の決定的データ。
genProb :: Int -> Int -> Prob
genProb nG perG =
  let n    = nG * perG
      xz   = [ sin (0.7 * fromIntegral i) | i <- [0 .. n - 1] ]
      ez   = [ 0.3 * cos (1.3 * fromIntegral i) | i <- [0 .. n - 1] ]
      uz   = [ 0.9 * sin (2.1 * fromIntegral j) | j <- [0 .. nG - 1] ]
      gids = [ i `div` perG | i <- [0 .. n - 1] ]
      xs   = map (* 2.0) xz
      ys   = [ 1.0 + 0.8 * x + (uz !! g) + e
             | (x, g, e) <- zip3 xs gids ez ]
      xRows = [ [1.0, x] | x <- xs ]
  in Prob { pP = 2, pNG = nG, pXRows = xRows, pGids = gids, pYs = ys }

-- パラメタ θ のレイアウト: [β_0..β_{p-1}, logτ, u_0..u_{nG-1}, logσ]
paramLen :: Prob -> Int
paramLen pr = pP pr + 1 + pNG pr + 1

-- 真値近傍の初期 θ。
theta0 :: Prob -> [Double]
theta0 pr =
  let p  = pP pr; nG = pNG pr
  in replicate p 0.5 ++ [log 1.2] ++ replicate nG 0.1 ++ [log 0.35]

-- ---------------------------------------------------------------------------
-- (a) 多相 logp (ad で grad する対象)
-- ---------------------------------------------------------------------------

logN :: Floating a => a -> a -> a -> a
logN x m s = -0.5 * log (2 * pi) - log s - 0.5 * ((x - m) / s) ^ (2 :: Int)

logHalfNormal :: Floating a => a -> a -> a
logHalfNormal x s = 0.5 * log (2 / pi) - log s - 0.5 * (x / s) ^ (2 :: Int)

logp :: forall a. Floating a => Prob -> [a] -> a
logp pr theta =
  let p   = pP pr; nG = pNG pr
      b   = take p theta
      logTau = theta !! p
      us  = take nG (drop (p + 1) theta)
      logSig = theta !! (p + 1 + nG)
      tau = exp logTau
      sig = exp logSig
      priorB   = sum [ logN bk 0 5 | bk <- b ]
      priorTau = logHalfNormal tau 5 + logTau          -- + jacobian (log 変換)
      priorU   = sum [ logN uj 0 tau | uj <- us ]
      priorSig = negate sig + logSig                   -- logExp(σ;1)=-σ + jacobian
      etas = [ sum (zipWith (\bk x -> bk * realToFrac x) b xr) + (us !! g)
             | (xr, g) <- zip (pXRows pr) (pGids pr) ]
      loglik = sum [ logN (realToFrac y) eta sig | (eta, y) <- zip etas (pYs pr) ]
  in priorB + priorTau + priorU + priorSig + loglik

gradAD :: Prob -> [Double] -> [Double]
gradAD pr = RevD.grad (logp pr)

-- ---------------------------------------------------------------------------
-- (b) 手書きベクトル化解析勾配 (Storable/Unboxed 配列・tape なし)
-- ---------------------------------------------------------------------------

-- 事前計算: 列ごとの X (length n の VS が p 本)、 group id (VU)。
data Compiled = Compiled
  { cP     :: !Int
  , cNG    :: !Int
  , cN     :: !Int
  , cXCols :: ![VS.Vector Double]  -- p 本、 各 length n
  , cGids  :: !(VU.Vector Int)
  , cYs    :: !(VS.Vector Double)
  }

compile :: Prob -> Compiled
compile pr =
  let p = pP pr; nG = pNG pr; n = length (pYs pr)
      xcols = [ VS.fromList [ row !! k | row <- pXRows pr ] | k <- [0 .. p - 1] ]
  in Compiled p nG n xcols (VU.fromList (pGids pr)) (VS.fromList (pYs pr))

gradVec :: Compiled -> [Double] -> [Double]
gradVec c theta =
  let p = cP c; nG = cNG c; n = cN c
      b   = take p theta
      logTau = theta !! p
      us  = take nG (drop (p + 1) theta)
      logSig = theta !! (p + 1 + nG)
      tau = exp logTau
      sig = exp logSig
      uv  = VS.fromList us
      -- η = Xβ + u[g]  (length n、 VS)
      xb  = foldl' (\acc (col, bk) -> VS.zipWith (+) acc (VS.map (* bk) col))
                   (VS.replicate n 0) (zip (cXCols c) b)
      ug  = VS.generate n (\i -> uv VS.! (cGids c VU.! i))
      eta = VS.zipWith (+) xb ug
      r   = VS.zipWith (-) (cYs c) eta            -- 残差 y - η
      sig2 = sig * sig
      -- ∂/∂β_k = -β_k/25 + (1/σ²) Σ_i r_i X_ik
      gB  = [ negate bk / 25 + VS.sum (VS.zipWith (*) col r) / sig2
            | (col, bk) <- zip (cXCols c) b ]
      -- ∂/∂u_j = -u_j/τ² + (1/σ²) Σ_{i:g_i=j} r_i  (scatter-add で O(n))
      rGroup = VU.accumulate (+) (VU.replicate nG 0)
                 (VU.zip (cGids c) (VU.convert r :: VU.Vector Double))
      gU  = [ negate (us !! j) / (tau * tau) + (rGroup VU.! j) / sig2
            | j <- [0 .. nG - 1] ]
      sumU2 = VS.sum (VS.map (\x -> x * x) uv)
      sumR2 = VS.sum (VS.map (\x -> x * x) r)
      -- logHalfNormal(τ;5) の scale は定数 5 ゆえ τ 由来は -τ²/25 のみ。
      -- + log 変換 jacobian (+1) + Σ_j logN(u_j;0,τ) の -nG + Σu²/τ²。
      gLogTau = 1 - fromIntegral nG - (tau * tau) / 25 + sumU2 / (tau * tau)
      gLogSig = negate sig + 1 - fromIntegral n + sumR2 / sig2
  in gB ++ [gLogTau] ++ gU ++ [gLogSig]

-- ---------------------------------------------------------------------------
-- (案A) backprop ライブラリによる汎用ベクトル化 reverse-mode AD
--
-- theta を 1 本の Storable Vector とみなし backprop で grad する。 ベクトル演算
-- (scale/add/sub/gather/dot/sum) は liftOp で随伴を手書きするので tape は
-- 「ベクトル演算 1 個 = 1 ノード」 になる (= 狙い)。 chain rule と tape の所有は
-- backprop が担う。 スカラ演算 (exp/log/+/*) は BVar の Num/Floating で書ける。
-- ※随伴は (案B) と共有 → 案A/案B の差は「tape をライブラリが持つか自前か」に純化。
-- ---------------------------------------------------------------------------

-- 全長 L の theta から要素 i を取り出す。 随伴は e_i*dy (長さ L)。
idxV :: Reifies s W => Int -> Int -> BVar s (VS.Vector Double) -> BVar s Double
idxV l i = liftOp1 $ op1 $ \v ->
  (v VS.! i, \dy -> VS.generate l (\j -> if j == i then dy else 0))

-- 全長 L の theta から [off, off+len) を切り出す。 随伴は zeros L に散布。
sliceV :: Reifies s W
       => Int -> Int -> Int -> BVar s (VS.Vector Double) -> BVar s (VS.Vector Double)
sliceV l off len = liftOp1 $ op1 $ \v ->
  ( VS.slice off len v
  , \dy -> VS.generate l (\j -> if j >= off && j < off + len then dy VS.! (j - off) else 0) )

-- scalar * vector。 ∂scalar = dy·v、 ∂v = scalar*dy。
scaleV :: Reifies s W => BVar s Double -> BVar s (VS.Vector Double) -> BVar s (VS.Vector Double)
scaleV = liftOp2 $ op2 $ \k v ->
  (VS.map (* k) v, \dy -> (VS.sum (VS.zipWith (*) dy v), VS.map (* k) dy))

vaddV :: Reifies s W => BVar s (VS.Vector Double) -> BVar s (VS.Vector Double) -> BVar s (VS.Vector Double)
vaddV = liftOp2 $ op2 $ \a b -> (VS.zipWith (+) a b, \dy -> (dy, dy))

vsubV :: Reifies s W => BVar s (VS.Vector Double) -> BVar s (VS.Vector Double) -> BVar s (VS.Vector Double)
vsubV = liftOp2 $ op2 $ \a b -> (VS.zipWith (-) a b, \dy -> (dy, VS.map negate dy))

-- 内積。 ∂a = dy*b、 ∂b = dy*a (a·a なら勾配は 2a を backprop の和算で得る)。
dotV :: Reifies s W => BVar s (VS.Vector Double) -> BVar s (VS.Vector Double) -> BVar s Double
dotV = liftOp2 $ op2 $ \a b ->
  (VS.sum (VS.zipWith (*) a b), \dy -> (VS.map (* dy) b, VS.map (* dy) a))

-- u[gids] gather (gids/nG は定数)。 随伴は scatter-add。
gatherV :: Reifies s W => VU.Vector Int -> Int -> BVar s (VS.Vector Double) -> BVar s (VS.Vector Double)
gatherV gids nG = liftOp1 $ op1 $ \u ->
  ( VS.generate (VU.length gids) (\i -> u VS.! (gids VU.! i))
  , \dy -> VS.convert $
      VU.accumulate (+) (VU.replicate nG 0)
        (VU.zip gids (VU.convert dy :: VU.Vector Double)) )

logpBP :: forall s. Reifies s W => Compiled -> BVar s (VS.Vector Double) -> BVar s Double
logpBP c theta =
  let p = cP c; nG = cNG c; n = cN c
      l = p + 1 + nG + 1
      bVec   = sliceV l 0 p theta
      logTau = idxV l p theta
      uVec   = sliceV l (p + 1) nG theta
      logSig = idxV l (p + 1 + nG) theta
      tau = exp logTau
      sig = exp logSig
      -- Xβ = Σ_k β_k * col_k  (β_k は bVec の第 k 要素)
      colC k = auto (cXCols c !! k)
      xb = foldl' (\acc k -> vaddV acc (scaleV (idxV p k bVec) (colC k)))
                  (auto (VS.replicate n 0)) [0 .. p - 1]
      ug  = gatherV (cGids c) nG uVec
      eta = vaddV xb ug
      r   = vsubV (auto (cYs c)) eta
      nD  = fromIntegral n
      pD  = fromIntegral p
      ngD = fromIntegral nG
      sumB2 = dotV bVec bVec
      sumU2 = dotV uVec uVec
      sumR2 = dotV r r
      priorB   = negate (0.5 * pD * log (2 * pi)) - pD * log 5 - sumB2 / (2 * 25)
      priorTau = 0.5 * log (2 / pi) - log 5 - tau * tau / (2 * 25) + logTau
      priorU   = negate (0.5 * ngD * log (2 * pi)) - ngD * log tau - sumU2 / (2 * tau * tau)
      priorSig = negate sig + logSig
      loglik   = negate (0.5 * nD * log (2 * pi)) - nD * log sig - sumR2 / (2 * sig * sig)
  in priorB + priorTau + priorU + priorSig + loglik

gradBackprop :: Compiled -> [Double] -> [Double]
gradBackprop c theta = VS.toList $ gradBP (logpBP c) (VS.fromList theta)

-- ---------------------------------------------------------------------------
-- (案B) 自作・最小 reverse-mode AD (vector-op tape)
--
-- forward で「ベクトル演算ごとにノードを発番」 し、 各ノードの随伴更新クロージャを
-- 逆順リストに積む (= 自前 Wengert tape)。 backward で出力に 1 を seed し、 逆位相順
-- (= 発番の逆順 = prepend したリストの先頭から) にクロージャを replay して入力 (theta
-- leaf) の随伴を得る。 随伴の式は案A の liftOp と同一 → 差は「tape 所有が自前か否か」。
-- スカラは長さ 1 の VS で随伴を持ち、 ノード随伴は単一の mutable 配列に統一格納する。
-- ---------------------------------------------------------------------------

-- reverse-mode の値ハンドル: ノード id + primal (scalar / vector)。
data Rval = RScal !Int !Double | RVec !Int !(VS.Vector Double)

ridOf :: Rval -> Int
ridOf (RScal i _) = i
ridOf (RVec  i _) = i

type Adj s = STArray s Int (VS.Vector Double)

-- 発番カウンタ + backward クロージャ列 (prepend = 発番の逆順)。
data Ctx s = Ctx !(STRef s Int) !(STRef s [Adj s -> ST s ()])

fresh :: Ctx s -> ST s Int
fresh (Ctx cnt _) = do
  n <- readSTRef cnt
  writeSTRef cnt (n + 1)
  pure n

record :: Ctx s -> (Adj s -> ST s ()) -> ST s ()
record (Ctx _ bw) f = modifySTRef' bw (f :)

-- 随伴の加算 (空 = ゼロ扱い)。
bumpA :: Adj s -> Int -> VS.Vector Double -> ST s ()
bumpA adj i contrib = do
  cur <- readArray adj i
  writeArray adj i (if VS.null cur then contrib else VS.zipWith (+) cur contrib)

readAdjS :: Adj s -> Int -> ST s Double
readAdjS adj i = do
  v <- readArray adj i
  pure (if VS.null v then 0 else v VS.! 0)

-- leaf (theta)。 backward 無し・勾配は最終的にこの随伴を読む。
inputVec :: Ctx s -> VS.Vector Double -> ST s Rval
inputVec ctx v = do
  i <- fresh ctx
  pure (RVec i v)

-- 全長 l の vec から要素 i を取り出す (scalar 化)。
idxHR :: Ctx s -> Int -> Int -> Rval -> ST s Rval
idxHR ctx l i (RVec vid v) = do
  o <- fresh ctx
  record ctx $ \adj -> do
    g <- readAdjS adj o
    when (g /= 0) $ bumpA adj vid (VS.generate l (\j -> if j == i then g else 0))
  pure (RScal o (v VS.! i))
idxHR _ _ _ _ = error "idxHR: scalar input"

-- 全長 l の vec から [off, off+len) を切り出す。
sliceHR :: Ctx s -> Int -> Int -> Int -> Rval -> ST s Rval
sliceHR ctx l off len (RVec vid v) = do
  o <- fresh ctx
  record ctx $ \adj -> do
    dy <- readArray adj o
    when (not (VS.null dy)) $
      bumpA adj vid (VS.generate l (\j -> if j >= off && j < off + len then dy VS.! (j - off) else 0))
  pure (RVec o (VS.slice off len v))
sliceHR _ _ _ _ _ = error "sliceHR: scalar input"

-- scalar * vector。
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

-- u[gids] gather (gids/nG 定数)。
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

-- scalar 演算群。
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

addS, mulS, subS :: Ctx s -> Rval -> Rval -> ST s Rval
addS ctx = binS ctx (+) (\_ _ -> (1, 1))
subS ctx = binS ctx (-) (\_ _ -> (1, -1))
mulS ctx = binS ctx (*) (\a b -> (b, a))

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

mulConstS, addConstS :: Ctx s -> Double -> Rval -> ST s Rval
mulConstS ctx c = unS ctx (* c) (const c)
addConstS ctx c = unS ctx (+ c) (const 1)

-- 案B logp (logpBP と同じ式を B エンジンで構築)。 戻りは出力ノード。
logpHR :: Ctx s -> Compiled -> Rval -> ST s Rval
logpHR ctx c theta = do
  let p = cP c; nG = cNG c; n = cN c
      l = p + 1 + nG + 1
  bVec   <- sliceHR ctx l 0 p theta
  logTau <- idxHR ctx l p theta
  uVec   <- sliceHR ctx l (p + 1) nG theta
  logSig <- idxHR ctx l (p + 1 + nG) theta
  tau <- expS ctx logTau
  sig <- expS ctx logSig
  -- Xβ = Σ_k β_k * col_k
  xb <- do
    cols <- mapM (\k -> do
                    bk  <- idxHR ctx p k bVec
                    col <- constVecM ctx (cXCols c !! k)
                    scaleHR ctx bk col) [0 .. p - 1]
    foldM1 (vaddHR ctx) cols
  ug  <- gatherHR ctx (cGids c) nG uVec
  eta <- vaddHR ctx xb ug
  yC  <- constVecM ctx (cYs c)
  r   <- vsubHR ctx yC eta
  sumB2 <- dotHR ctx bVec bVec
  sumU2 <- dotHR ctx uVec uVec
  sumR2 <- dotHR ctx r r
  let nD = fromIntegral n; pD = fromIntegral p; ngD = fromIntegral nG
  -- priorB = constB - sumB2/50
  priorB <- do { t <- mulConstS ctx (-1 / (2 * 25)) sumB2
               ; addConstS ctx (negate (0.5 * pD * log (2 * pi)) - pD * log 5) t }
  -- priorTau = constT - tau²/50 + logTau
  priorTau <- do
    tau2  <- mulS ctx tau tau
    t1    <- mulConstS ctx (-1 / (2 * 25)) tau2
    t2    <- addS ctx t1 logTau
    addConstS ctx (0.5 * log (2 / pi) - log 5) t2
  -- priorU = constU - nG*log tau - sumU2/(2τ²)
  priorU <- do
    lt    <- logS ctx tau
    a1    <- mulConstS ctx (negate ngD) lt
    tau2  <- mulS ctx tau tau
    inv   <- mulConstS ctx (-0.5) =<< divByS ctx sumU2 tau2
    s     <- addS ctx a1 inv
    addConstS ctx (negate (0.5 * ngD * log (2 * pi))) s
  -- priorSig = -sig + logSig
  priorSig <- do { ns <- mulConstS ctx (-1) sig; addS ctx ns logSig }
  -- loglik = constL - n*log sig - sumR2/(2σ²)
  loglik <- do
    ls    <- logS ctx sig
    a1    <- mulConstS ctx (negate nD) ls
    sig2  <- mulS ctx sig sig
    inv   <- mulConstS ctx (-0.5) =<< divByS ctx sumR2 sig2
    s     <- addS ctx a1 inv
    addConstS ctx (negate (0.5 * nD * log (2 * pi))) s
  -- 総和
  s1 <- addS ctx priorB priorTau
  s2 <- addS ctx s1 priorU
  s3 <- addS ctx s2 priorSig
  addS ctx s3 loglik
  where
    foldM1 _ []       = error "foldM1: empty"
    foldM1 _ [x]      = pure x
    foldM1 g (x:y:xs) = g x y >>= \z -> foldM1 g (z : xs)

-- 定数ベクトルノード (backward 無し)。
constVecM :: Ctx s -> VS.Vector Double -> ST s Rval
constVecM ctx v = do { i <- fresh ctx; pure (RVec i v) }

-- scalar 除算 (a/b)。
divByS :: Ctx s -> Rval -> Rval -> ST s Rval
divByS ctx = binS ctx (/) (\a b -> (1 / b, negate a / (b * b)))

gradHandroll :: Compiled -> [Double] -> [Double]
gradHandroll c theta = runST $ do
  cnt <- newSTRef 0
  bw  <- newSTRef []
  let ctx = Ctx cnt bw
  th  <- inputVec ctx (VS.fromList theta)
  out <- logpHR ctx c th
  n   <- readSTRef cnt
  adj <- newArray (0, n - 1) VS.empty
  writeArray adj (ridOf out) (VS.singleton 1)
  closures <- readSTRef bw
  mapM_ ($ adj) closures
  g <- readArray adj (ridOf th)
  pure (if VS.null g then replicate (length theta) 0 else VS.toList g)

-- ---------------------------------------------------------------------------
-- 中心差分 (検証用)
-- ---------------------------------------------------------------------------

centralDiff :: ([Double] -> Double) -> [Double] -> [Double]
centralDiff f ps =
  [ let h = 1e-6 * (abs (ps !! j) + 1e-3)
    in (f (bump j h) - f (bump j (-h))) / (2 * h)
  | j <- [0 .. length ps - 1] ]
  where bump j d = [ if k == j then p + d else p | (k, p) <- zip [0 ..] ps ]

relErr :: [Double] -> [Double] -> Double
relErr a b = maximum [ abs (x - y) / (abs y + 1e-6) | (x, y) <- zip a b ]

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Phase 54 専用ベクトル化 AD feasibility spike ===\n"
  putStrLn "対象: 階層 Gaussian (random intercept、 M2 同型)。 obs/群=12。"
  putStrLn "(a) ad Reverse.Double.grad  vs  (b) 手書きベクトル化解析勾配 (tape なし)\n"

  putStrLn "--- (検証) 各勾配 vs 中心差分 (rel err) ---"
  forM_ [2, 8, 32] $ \nG -> do
    let pr  = genProb nG 12
        c   = compile pr
        t0  = theta0 pr
        cd  = centralDiff (logp pr) t0
        eV  = relErr (gradVec c t0) cd
        eBP = relErr (gradBackprop c t0) cd
        eHR = relErr (gradHandroll c t0) cd
    printf "nG=%-3d p=%-3d | vec=%.3e | backprop=%.3e | handroll=%.3e\n"
      nG (paramLen pr) eV eBP eHR

  putStrLn "\n--- (デバッグ) nG=2 の成分比較 [央差 / ad / vec] ---"
  let prD = genProb 2 12
      cD  = compile prD
      t0D = theta0 prD
      cd  = centralDiff (logp prD) t0D
      ga  = gradAD prD t0D
      gv  = gradVec cD t0D
  forM_ (zip3 [0 :: Int ..] (zip3 cd ga gv) (theta0 prD)) $ \(j, (a, b, v), _) ->
    printf "  θ%-2d | cd=%10.4f | ad=%10.4f | vec=%10.4f\n" j a b v

  putStrLn "\n--- per-grad 時間 (ms・median of 50) ---"
  putStrLn "ad=現行スカラtape / vec=解析勾配(下限) / bp=backprop(案A) / hr=自作tape(案B)\n"
  forM_ [2, 4, 8, 16, 32] $ \nG -> do
    let pr = genProb nG 12
        c  = compile pr
        t0 = theta0 pr
        probe = sum . map abs
    (tA, _)  <- timeitIO 50 probe (\_ -> pure (gradAD pr t0))
    (tB, _)  <- timeitIO 50 probe (\_ -> pure (gradVec c t0))
    (tC, _)  <- timeitIO 50 probe (\_ -> pure (gradBackprop c t0))
    (tD, _)  <- timeitIO 50 probe (\_ -> pure (gradHandroll c t0))
    printf "nG=%-3d p=%-3d n=%-4d | ad=%7.4f | vec=%7.4f | bp=%7.4f | hr=%7.4f | ad/bp ×%.1f | ad/hr ×%.1f | hr/vec ×%.1f\n"
      nG (paramLen pr) (nG * 12) tA tB tC tD (tA / tC) (tA / tD) (tD / tB)

  putStrLn "\n(vec=tape-free 解析勾配=汎用ベクトル化 AD の時間下限。"
  putStrLn " ad/bp・ad/hr = 案A・案B が現行 ad を何倍速くするか (判断ゲート: ≥5× で 54.4 本実装へ)。"
  putStrLn " hr/vec = 案B が下限からどれだけ離れているか = 自前 tape のオーバヘッド)"
