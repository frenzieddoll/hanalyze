{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | Phase 54.11 spike: 非線形 μ の「ベクトル式 IR」 feasibility (計測先行)。
--
-- 54.9 の prof で M5/M6 (非 affine μ) の負けは「per-obs スカラ AD」 帰着が
-- ~90% (logDensityObs ~52% + μ の AD 演算 ~25% + tape 管理 ~12%) と確定した。
-- 54.11 本実装 = 「ベクトル式 IR を構築する追跡 interpreter」 の前に、
-- **手組みの vec-tape (VecAD + 54.11 追加の elementwise op)** で M5/M6 の
-- 勾配カーネルがどこまで速いかを実測し、 ゲート (実経路 `gradADU` 比 ≥3×)
-- を判定する。
--
--   M5: μ_i = a·exp(-b·x_i) + c,  y_i ~ N(μ_i, σ)        (n=100, θ=4)
--   M6: μ_i = a_{g(i)}·exp(-b·x_i), a_g ~ N(μ_a, τ_a)    (n=96, nG=8, θ=12)
--
-- 比較 3 通り (全て同一の unconstrained 全勾配・中心差分/相互で検証後に計測):
--   (a)  RevD.grad (多相 logp 直書き)   — スカラ tape の下限 (walk 無し)
--   (a') HBM.gradADU (per-obs 手書き)   — 実経路 (Free walk + ad fallback) = NUTS が払う値
--   (b)  VecAD 手組み tape              — ベクトル式 IR 化の到達見込み (per-call 構築込み)
--
-- ⚠ (b) は手組み = IR 追跡 interpreter のオーバヘッドを含まない楽観側。
-- 「PyMC 同等」 とは言わない。 ゲート判定にのみ使う。
module Main where

import           Control.Monad                  (forM_)
import           Control.Monad.ST               (ST)
import           Data.List                      (foldl')
import qualified Data.Map.Strict                as Map
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Data.Vector.Storable           as VS
import qualified Data.Vector.Unboxed            as VU
import qualified System.Random.MWC              as MWC
import           System.Random.MWC.Distributions (standard)
import           Text.Printf                    (printf)

import qualified Numeric.AD.Mode.Reverse.Double as RevD

import           Hanalyze.Model.HBM             (Distribution (..), ModelP,
                                                 sample, observe, gradADU,
                                                 sampleNames, getTransforms)
import           Hanalyze.Model.HBM.VecAD

import           BenchUtil                      (timeitIO)

-- ---------------------------------------------------------------------------
-- データ (BenchHBMScaling と同一 DGP・seed)
-- ---------------------------------------------------------------------------

normals :: Int -> Int -> IO [Double]
normals seed k = do
  g <- MWC.initialize (V.singleton (fromIntegral seed))
  mapM (const (standard g)) [1 .. k]

nM5 :: Int
nM5 = 100

genM5 :: IO ([Double], [Double])
genM5 = do
  let (a, b, c, s) = (2.5, 1.2, 0.5, 0.3)
  ez <- normals 51 nM5
  let xs = [ 3.0 * (fromIntegral i + 0.5) / fromIntegral nM5
           | i <- [0 .. nM5 - 1] ]
      ys = [ a * exp (negate b * x) + c + s * e | (x, e) <- zip xs ez ]
  return (xs, ys)

nGroups, perGroup :: Int
nGroups  = 8
perGroup = 12

genM6 :: IO ([Double], [Int], [Double])
genM6 = do
  let (muA, tauA, b, s) = (2.0, 0.5, 1.0, 0.3)
      n = nGroups * perGroup
  ez <- normals 61 n
  az <- normals 62 nGroups
  let as   = [ muA + tauA * z | z <- az ]
      gids = [ i `div` perGroup | i <- [0 .. n - 1] ]
      xs   = [ 3.0 * (fromIntegral (i `mod` perGroup) + 0.5)
                   / fromIntegral perGroup
             | i <- [0 .. n - 1] ]
      ys   = [ (as !! g) * exp (negate b * x) + s * e
             | (x, g, e) <- zip3 xs gids ez ]
  return (xs, gids, ys)

-- ---------------------------------------------------------------------------
-- モデル (BenchHBMScaling と同一・(a') gradADU 用)
-- ---------------------------------------------------------------------------

m5Model :: [Double] -> [Double] -> ModelP ()
m5Model xs ys = do
  a <- sample "a" (Normal 0 10)
  b <- sample "b" (HalfNormal 2)
  c <- sample "c" (Normal 0 10)
  s <- sample "sigma" (Exponential 1)
  forM_ (zip3 [0 :: Int ..] xs ys) $ \(i, x, y) ->
    observe (T.pack ("y_" ++ show i))
      (Normal (a * exp (negate b * realToFrac x) + c) s) [y]

m6Model :: [Double] -> [Int] -> [Double] -> ModelP ()
m6Model xs gids ys = do
  let nG = if null gids then 0 else maximum gids + 1
  muA  <- sample "mu_a"  (Normal 0 10)
  tauA <- sample "tau_a" (HalfNormal 2)
  as   <- mapM (\j -> sample (T.pack ("a_" ++ show j)) (Normal muA tauA))
               [0 .. nG - 1]
  b    <- sample "b" (HalfNormal 2)
  s    <- sample "sigma" (Exponential 1)
  forM_ (zip3 [0 :: Int ..] (zip xs gids) ys) $ \(i, (x, g), y) ->
    observe (T.pack ("y_" ++ show i))
      (Normal ((as !! g) * exp (negate b * realToFrac x)) s) [y]

-- ---------------------------------------------------------------------------
-- (a) 多相 logp 直書き (RevD.grad の対象・unconstrained 全勾配)
-- ---------------------------------------------------------------------------

logN :: Floating a => a -> a -> a -> a
logN x m s = -0.5 * log (2 * pi) - log s - 0.5 * ((x - m) / s) ^ (2 :: Int)

logHalfNormal :: Floating a => a -> a -> a
logHalfNormal x s = 0.5 * log (2 / pi) - log s - 0.5 * (x / s) ^ (2 :: Int)

-- θ = [a, log b, c, log σ] (sampleNames 順・b/σ は PositiveT)。
logp5 :: forall a. Floating a => [Double] -> [Double] -> [a] -> a
logp5 xs ys [ua, ub, uc, us] =
  let b   = exp ub
      sig = exp us
      pri = logN ua 0 10 + logHalfNormal b 2 + ub
            + logN uc 0 10 + (negate sig + us)
      ll  = sum [ logN (realToFrac y) (ua * exp (negate b * realToFrac x) + uc) sig
                | (x, y) <- zip xs ys ]
  in pri + ll
logp5 _ _ _ = error "logp5: θ shape"

-- θ = [μ_a, log τ_a, a_0..a_{nG-1}, log b, log σ]。
logp6 :: forall a. Floating a => [Double] -> [Int] -> [Double] -> [a] -> a
logp6 xs gids ys theta =
  let nG  = nGroups
      ma  = theta !! 0
      ut  = theta !! 1
      as  = take nG (drop 2 theta)
      ub  = theta !! (2 + nG)
      us  = theta !! (3 + nG)
      tau = exp ut
      b   = exp ub
      sig = exp us
      pri = logN ma 0 10 + logHalfNormal tau 2 + ut
            + sum [ logN aj ma tau | aj <- as ]
            + logHalfNormal b 2 + ub + (negate sig + us)
      ll  = sum [ logN (realToFrac y) ((as !! g) * exp (negate b * realToFrac x)) sig
                | (x, g, y) <- zip3 xs gids ys ]
  in pri + ll

-- ---------------------------------------------------------------------------
-- (b) VecAD 手組み tape (per-call 構築込み)
-- ---------------------------------------------------------------------------

negS :: Ctx s -> Rval -> ST s Rval
negS ctx = mulConstS ctx (-1)

-- | M5 の unconstrained 全勾配 (θ=4)。
gradVec5 :: VS.Vector Double -> VS.Vector Double -> [Double] -> [Double]
gradVec5 xC yC [ua0, ub0, uc0, us0] =
  let n  = VS.length xC
      gs = runTape $ \ctx -> do
        ua <- inputScal ctx ua0
        ub <- inputScal ctx ub0
        uc <- inputScal ctx uc0
        us <- inputScal ctx us0
        b   <- expS ctx ub
        sig <- expS ctx us
        xs  <- constVec ctx xC
        ys  <- constVec ctx yC
        nb  <- negS ctx b
        t1  <- scaleHR ctx nb xs            -- -b·x
        t2  <- vexpHR ctx t1                -- exp(-b·x)
        t3  <- scaleHR ctx ua t2            -- a·exp(-b·x)
        mu  <- bcastAddHR ctx uc t3         -- + c
        r   <- vsubHR ctx ys mu
        sr2 <- dotHR ctx r r
        -- loglik = -n/2·log2π - n·logσ - sr2/(2σ²)   (logσ = us)
        s2  <- mulS ctx sig sig
        den <- mulConstS ctx 2 s2
        q   <- divByS ctx sr2 den
        nls <- mulConstS ctx (fromIntegral n) us
        ll0 <- addS ctx nls q               -- n·logσ + sr2/(2σ²)
        ll  <- mulConstS ctx (-1) ll0
        -- priors: a,c ~ N(0,10); b ~ HalfNormal 2 (+jac ub); σ ~ Exp 1 (+jac us)
        aa  <- mulS ctx ua ua
        pa  <- mulConstS ctx (negate (1 / 200)) aa
        cc  <- mulS ctx uc uc
        pc  <- mulConstS ctx (negate (1 / 200)) cc
        bb  <- mulS ctx b b
        pb0 <- mulConstS ctx (negate (1 / 8)) bb
        pb  <- addS ctx pb0 ub
        nsg <- negS ctx sig
        ps  <- addS ctx nsg us
        tot <- foldAddS ctx [ll, pa, pc, pb, ps]
        pure (tot, [ua, ub, uc, us])
  in map VS.head gs
gradVec5 _ _ _ = error "gradVec5: θ shape"

-- | M6 の unconstrained 全勾配 (θ = 4 + nG)。
gradVec6 :: VS.Vector Double -> VU.Vector Int -> VS.Vector Double
         -> [Double] -> [Double]
gradVec6 xC gids yC theta =
  let n   = VS.length xC
      nG  = nGroups
      ma0 = theta !! 0
      ut0 = theta !! 1
      as0 = VS.fromList (take nG (drop 2 theta))
      ub0 = theta !! (2 + nG)
      us0 = theta !! (3 + nG)
      gs = runTape $ \ctx -> do
        ma  <- inputScal ctx ma0
        ut  <- inputScal ctx ut0
        av  <- inputVec ctx as0
        ub  <- inputScal ctx ub0
        us  <- inputScal ctx us0
        tau <- expS ctx ut
        b   <- expS ctx ub
        sig <- expS ctx us
        xs  <- constVec ctx xC
        ys  <- constVec ctx yC
        nb  <- negS ctx b
        t1  <- scaleHR ctx nb xs
        t2  <- vexpHR ctx t1                -- exp(-b·x)
        ag  <- gatherHR ctx gids nG av      -- a_{g(i)}
        mu  <- hadamardHR ctx ag t2         -- a_g·exp(-b·x)
        r   <- vsubHR ctx ys mu
        sr2 <- dotHR ctx r r
        s2  <- mulS ctx sig sig
        den <- mulConstS ctx 2 s2
        q   <- divByS ctx sr2 den
        nls <- mulConstS ctx (fromIntegral n) us
        ll0 <- addS ctx nls q
        ll  <- mulConstS ctx (-1) ll0
        -- prior a_j ~ N(μ_a, τ): -nG·logτ - Σ(a_j-μ_a)²/(2τ²)   (logτ = ut)
        zsC <- constVec ctx (VS.replicate nG 0)
        mab <- bcastAddHR ctx ma zsC        -- μ_a broadcast (長さ nG)
        ra  <- vsubHR ctx av mab
        sra <- dotHR ctx ra ra
        t2a <- mulS ctx tau tau
        dna <- mulConstS ctx 2 t2a
        qa  <- divByS ctx sra dna
        nlt <- mulConstS ctx (fromIntegral nG) ut
        pa0 <- addS ctx nlt qa
        pa  <- mulConstS ctx (-1) pa0
        -- μ_a ~ N(0,10); τ_a ~ HalfNormal 2 (+jac ut); b ~ HalfNormal 2 (+jac ub);
        -- σ ~ Exp 1 (+jac us)
        mm  <- mulS ctx ma ma
        pm  <- mulConstS ctx (negate (1 / 200)) mm
        tt  <- mulS ctx tau tau
        pt0 <- mulConstS ctx (negate (1 / 8)) tt
        pt  <- addS ctx pt0 ut
        bb  <- mulS ctx b b
        pb0 <- mulConstS ctx (negate (1 / 8)) bb
        pb  <- addS ctx pb0 ub
        nsg <- negS ctx sig
        ps  <- addS ctx nsg us
        tot <- foldAddS ctx [ll, pa, pm, pt, pb, ps]
        pure (tot, [ma, ut, av, ub, us])
  in case gs of
       [gma, gut, gav, gub, gus] ->
         VS.head gma : VS.head gut : VS.toList gav ++ [VS.head gub, VS.head gus]
       _ -> error "gradVec6: leaf shape"

-- | スカラノード列を addS で畳む。
foldAddS :: Ctx s -> [Rval] -> ST s Rval
foldAddS _   []       = error "foldAddS: empty"
foldAddS _   [x]      = pure x
foldAddS ctx (x:y:xs) = addS ctx x y >>= \z -> foldAddS ctx (z : xs)

-- ---------------------------------------------------------------------------
-- 検証 + 計測
-- ---------------------------------------------------------------------------

closeVec :: Double -> [Double] -> [Double] -> Bool
closeVec tol u v =
  length u == length v
  && and [ abs (a - b) <= tol * (1 + max (abs a) (abs b)) | (a, b) <- zip u v ]

centralDiff :: ([Double] -> Double) -> [Double] -> [Double]
centralDiff f th =
  [ (f (bump i h) - f (bump i (negate h))) / (2 * h) | i <- [0 .. length th - 1] ]
  where
    h = 1e-5
    bump i d = [ if j == i then t + d else t | (j, t) <- zip [0 ..] th ]

-- K 回の勾配呼出を 1 計測にまとめる (1 call ~0.1ms 級のタイマ分解能対策)。
benchGrad :: String -> Int -> ([Double] -> [Double]) -> [Double] -> IO Double
benchGrad tag k f th0 = do
  let run i = pure $! foldl' (\ !acc j ->
                 let th = [ t + 1e-9 * fromIntegral (i + j) | t <- th0 ]
                 in acc + sum (f th)) 0 [1 .. k]
  (ms, _) <- timeitIO 7 id run
  let per = ms / fromIntegral k
  printf "  %-28s %8.4f ms/grad (%d calls median)\n" tag per k
  pure per

main :: IO ()
main = do
  putStrLn "== Phase 54.11 spike: 非線形 μ の vec-tape (手組み IR) =="
  (x5, y5)     <- genM5
  (x6, g6, y6) <- genM6

  -- ---- M5 ----
  let m5 :: ModelP ()
      m5 = m5Model x5 y5
      n5names = sampleNames m5
      n5trans = [ getTransforms m5 Map.! nm | nm <- n5names ]
      th5  = [0.8, log 0.9, 0.3, log 0.4]
      x5C  = VS.fromList x5
      y5C  = VS.fromList y5
      gAd5  = RevD.grad (logp5 x5 y5) th5
      gAdu5 = gradADU m5 n5names n5trans th5
      gVec5 = gradVec5 x5C y5C th5
      gCd5  = centralDiff (logp5 x5 y5) th5
  putStrLn "M5 検証 (RevD / gradADU / vec-tape / 中心差分):"
  printf "  RevD vs gradADU: %s\n" (show (closeVec 1e-9 gAd5 gAdu5))
  printf "  vec  vs RevD:    %s\n" (show (closeVec 1e-9 gVec5 gAd5))
  printf "  vec  vs 中心差分: %s\n" (show (closeVec 1e-4 gVec5 gCd5))
  putStrLn "M5 計測:"
  pa5  <- benchGrad "(a)  RevD.grad (logp 直書き)" 200 (RevD.grad (logp5 x5 y5)) th5
  pa5' <- benchGrad "(a') gradADU (実経路 walk+ad)" 200 (gradADU m5 n5names n5trans) th5
  pb5  <- benchGrad "(b)  vec-tape 手組み" 200 (gradVec5 x5C y5C) th5
  printf "  → (a')/(b) = %.1fx / (a)/(b) = %.1fx (ゲート ≥3×)\n\n"
    (pa5' / pb5) (pa5 / pb5)

  -- ---- M6 ----
  let m6 :: ModelP ()
      m6 = m6Model x6 g6 y6
      n6names = sampleNames m6
      n6trans = [ getTransforms m6 Map.! nm | nm <- n6names ]
      th6  = [1.5, log 0.6] ++ replicate nGroups 1.8 ++ [log 0.9, log 0.4]
      x6C  = VS.fromList x6
      y6C  = VS.fromList y6
      g6U  = VU.fromList g6
      gAd6  = RevD.grad (logp6 x6 g6 y6) th6
      gAdu6 = gradADU m6 n6names n6trans th6
      gVec6 = gradVec6 x6C g6U y6C th6
      gCd6  = centralDiff (logp6 x6 g6 y6) th6
  putStrLn "M6 検証 (RevD / gradADU / vec-tape / 中心差分):"
  printf "  RevD vs gradADU: %s\n" (show (closeVec 1e-9 gAd6 gAdu6))
  printf "  vec  vs RevD:    %s\n" (show (closeVec 1e-9 gVec6 gAd6))
  printf "  vec  vs 中心差分: %s\n" (show (closeVec 1e-4 gVec6 gCd6))
  putStrLn "M6 計測:"
  pa6  <- benchGrad "(a)  RevD.grad (logp 直書き)" 200 (RevD.grad (logp6 x6 g6 y6)) th6
  pa6' <- benchGrad "(a') gradADU (実経路 walk+ad)" 200 (gradADU m6 n6names n6trans) th6
  pb6  <- benchGrad "(b)  vec-tape 手組み" 200 (gradVec6 x6C g6U y6C) th6
  printf "  → (a')/(b) = %.1fx / (a)/(b) = %.1fx (ゲート ≥3×)\n"
    (pa6' / pb6) (pa6 / pb6)
