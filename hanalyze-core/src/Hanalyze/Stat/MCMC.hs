{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Stat.MCMC
-- Description : MCMC チェーンの純粋な後処理 (自己相関・HDI・ESS・R-hat・KDE・BFMI)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Pure post-processing utilities for MCMC chains.
--
-- Provides autocorrelation, highest-density intervals (HDI), effective
-- sample size (Geyer's initial monotone sequence estimator), split-R-hat
-- (Vehtari et al. 2021), kernel density estimation (Silverman bandwidth)
-- and BFMI. Operates on raw @Vector@ samples or on the 'Hanalyze.MCMC.Core.Chain'
-- type from the sampler layer.
module Hanalyze.Stat.MCMC
  ( autocorr
  , hdi
  , ess
  , essBulk
  , rhat
  , kde
  , bfmi
  , rankHist
  ) where

import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Function (on)
import Data.List (groupBy, minimumBy, sort, sortBy)
import Data.Ord  (comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Statistics.Distribution as SD
import Statistics.Distribution.Normal (standard)

-- | Autocorrelation at lags 0 .. min(maxLag, n-1).
-- Uses O(n × maxLag) time with Vector indexing.
autocorr :: Int -> [Double] -> [(Int, Double)]
autocorr maxLag xs =
  let v   = V.fromList xs
      n   = V.length v
      mu  = V.sum v / fromIntegral n
      var = V.sum (V.map (\x -> (x - mu) ^ (2 :: Int)) v) / fromIntegral n
      acf k
        | var == 0 || k >= n = 0
        | otherwise =
            V.sum (V.zipWith (\a b -> (a - mu) * (b - mu))
                             (V.take (n - k) v)
                             (V.drop k      v))
            / (fromIntegral (n - k) * var)
  in [(k, acf k) | k <- [0 .. min maxLag (n - 1)]]

-- | Highest density interval: shortest contiguous interval that covers
-- @level@ fraction of the (sorted) samples. Returns (lower, upper).
hdi :: Double -> [Double] -> (Double, Double)
hdi level xs
  | null xs   = (0, 0)
  | otherwise =
      let sorted  = V.fromList (sort xs)
          n       = V.length sorted
          window  = max 1 (min (n - 1) (floor (level * fromIntegral n) :: Int))
          (_, i)  = minimumBy (comparing fst)
                      [ (sorted V.! (i' + window) - sorted V.! i', i')
                      | i' <- [0 .. n - window - 1] ]
      in (sorted V.! i, sorted V.! (i + window))

-- | Effective sample size via Geyer's initial monotone sequence estimator.
-- Returns n when the chain is too short to estimate.
ess :: [Double] -> Double
ess xs
  | n < 4     = fromIntegral n
  | otherwise =
      let acs    = map snd (autocorr (n `div` 2) xs)
          -- Gamma(k) = rho(2k) + rho(2k+1)
          gammas = pairSums acs
          -- Monotone non-increasing sequence of Gamma
          monoG  = scanl1 min gammas
          posG   = takeWhile (> 0) monoG
          tau    = max 1 (-1 + 2 * sum posG)
      in fromIntegral n / tau
  where
    n = length xs
    pairSums (a : b : rest) = (a + b) : pairSums rest
    pairSums _              = []

-- | arviz / Stan 互換の rank-normalized **bulk ESS** (Vehtari et al. 2021)。
--
-- 引数は 'rhat' と同じ「パラメータ 1 つの chain ごとの sample 列」。手順は
-- arviz の @ess(method="bulk")@ と同一:
--
-- 1. 各 chain を半分に split (奇数長は中央 1 点を落とす) して 2M 本の
--    sub-chain にする
-- 2. 全値プールの平均 rank (同値は平均) を
--    @(r − 3\/8) \/ (S + 1\/4)@ で (0,1) に写し Φ⁻¹ で z 化 (rank 正規化)
-- 3. 多 chain 結合自己相関 @ρ̂_t = 1 − (W − mean acov_t) \/ var⁺@ に
--    Geyer の initial positive + monotone sequence を適用し
--    @τ̂ = −1 + 2Σρ̂@ (下限 @1\/log₁₀(MN)@)、@ESS = MN \/ τ̂@
--
-- 単 chain の 'ess' (Geyer IMSE・τ 下限 1 クランプで n 頭打ち) と異なり
-- 多 chain 情報と rank 正規化で裾の重い分布でも安定し、PyMC / arviz の
-- @ess_bulk@ と数値比較できる (Phase 92 B4: bench の指標非対称の是正)。
-- chain が短すぎるとき (split 後 4 draw 未満・arviz は NaN を返す領域) は
-- フォールバックとして元の総 draw 数を返す。
essBulk :: [[Double]] -> Double
essBulk chains
  | m < 1 || n < 4 = fromIntegral (sum (map length nonEmpty))  -- 元の総 draw 数
  | otherwise      = essMultiChain (rankNormalize sub)
  where
    nonEmpty = filter (not . null) chains
    -- arviz _split_chains: 前半 floor(n/2) + 後半 floor(n/2) (奇数長は中央落ち)
    splitOne vs = let h = length vs `div` 2
                  in [take h vs, drop (length vs - h) vs]
    sub0 = concatMap splitOne nonEmpty
    n    = if null sub0 then 0 else minimum (map length sub0)
    sub  = map (take n) sub0
    m    = length sub

-- | rank 正規化 (arviz @_z_scale@): 全 chain プールの平均 rank →
-- @(r − 3\/8)\/(S + 1\/4)@ → 標準正規の分位関数。chain 構造は保存する。
rankNormalize :: [[Double]] -> [[Double]]
rankNormalize chains = rechunk (map length chains) (map z ranks)
  where
    flat  = concat chains
    s     = fromIntegral (length flat) :: Double
    ranks = averageRanks flat
    z r   = SD.quantile standard ((r - 3 / 8) / (s + 0.25))
    rechunk []           _  = []
    rechunk (len : lens) xs = let (h, t) = splitAt len xs in h : rechunk lens t

-- | 同値を平均 rank (scipy @rankdata(method="average")@ 相当) にした
-- 1-based rank を入力順で返す。
averageRanks :: [Double] -> [Double]
averageRanks xs = map snd (sortBy (comparing fst) ranked)
  where
    byVal  = sortBy (comparing snd) (zip [0 :: Int ..] xs)
    groups = groupBy ((==) `on` snd) byVal
    ranked = go 0 groups
    go _ [] = []
    go pos (g : gs) =
      let k   = length g
          -- ranks pos+1 .. pos+k の平均
          avg = fromIntegral (2 * pos + k + 1) / 2 :: Double
      in [ (i, avg) | (i, _) <- g ] ++ go (pos + k) gs

-- | 多 chain 結合 ESS (arviz @_ess@ の忠実な移植)。入力 = z 化済み等長
-- sub-chain 群。
essMultiChain :: [[Double]] -> Double
essMultiChain sub
  | isNaN varPlus || varPlus <= 0 = sTotal
  | otherwise = runST $ do
      rhoT <- VUM.replicate n 0
      VUM.write rhoT 0 1
      let rho1 = rho 1
      VUM.write rhoT 1 rho1
      -- Geyer initial positive sequence (ペア和が正の間だけ採用)
      let goPos t rhoEven rhoOdd
            | t < n - 3 && rhoEven + rhoOdd > 0 = do
                let re = rho (t + 1)
                    ro = rho (t + 2)
                when (re + ro >= 0) $ do
                  VUM.write rhoT (t + 1) re
                  VUM.write rhoT (t + 2) ro
                goPos (t + 2) re ro
            | otherwise = pure (t, rhoEven)
      (tEnd, lastEven) <- goPos 1 1.0 rho1
      let maxT = tEnd - 2
      when (lastEven > 0 && maxT + 1 < n) $
        VUM.write rhoT (maxT + 1) lastEven
      -- Geyer initial monotone sequence (ペア和を非増加に均す)
      let goMono t
            | t <= maxT - 2 = do
                a <- VUM.read rhoT (t - 1)
                b <- VUM.read rhoT t
                c <- VUM.read rhoT (t + 1)
                d <- VUM.read rhoT (t + 2)
                when (c + d > a + b) $ do
                  VUM.write rhoT (t + 1) ((a + b) / 2)
                  VUM.write rhoT (t + 2) ((a + b) / 2)
                goMono (t + 2)
            | otherwise = pure ()
      goMono 1
      frozen <- VU.unsafeFreeze rhoT
      let tauRaw = -1 + 2 * VU.sum (VU.take (maxT + 1) frozen)
                      + (if maxT + 1 < n then frozen VU.! (maxT + 1) else 0)
          tau    = max tauRaw (1 / logBase 10 sTotal)
      pure (sTotal / tau)
  where
    m      = length sub
    n      = length (head sub)
    sTotal = fromIntegral (m * n)
    acovs  = map (autocovBiased . V.fromList) sub
    chainMeans = map (\vs -> sum vs / fromIntegral n) sub
    meanAcov t = sum (map (V.! t) acovs) / fromIntegral m
    meanVar = meanAcov 0 * fromIntegral n / fromIntegral (n - 1)
    varPlus = meanVar * fromIntegral (n - 1) / fromIntegral n
            + (if m > 1 then sampleVar chainMeans else 0)
    rho t   = 1 - (meanVar - meanAcov t) / varPlus
    sampleVar vs =
      let mu = sum vs / fromIntegral (length vs)
      in sum [ (x - mu) ^ (2 :: Int) | x <- vs ] / fromIntegral (length vs - 1)

-- | biased 自己共分散 (分母 n・arviz @_autocov@ と同じ規約) を lag 0..n-1 で。
autocovBiased :: V.Vector Double -> V.Vector Double
autocovBiased v = V.generate nn at
  where
    nn = V.length v
    mu = V.sum v / fromIntegral nn
    c  = V.map (subtract mu) v
    at t = V.sum (V.zipWith (*) (V.take (nn - t) c) (V.drop t c))
           / fromIntegral nn

-- | Split-R-hat convergence diagnostic (Vehtari et al. 2021).
--
-- Splits each chain in half to obtain @2M@ sub-chains, then computes
-- R-hat from the between-chain variance @B@ and within-chain variance
-- @W@. The conventional convergence threshold is @R-hat < 1.01@.
-- The argument is the per-chain sample list for a single parameter.
-- Returns 'Nothing' when there are fewer than 2 chains or fewer than 4
-- samples per chain.
rhat :: [[Double]] -> Maybe Double
rhat chains
  | m < 2 || n < 4 = Nothing
  | w == 0         = Nothing
  | otherwise      = Just (sqrt (varPlus / w))
  where
    allVals   = filter (not . null) chains
    splitOne vs = let half = length vs `div` 2
                  in [take half vs, drop half vs]
    subchains = concatMap splitOne allVals
    m         = length subchains
    n         = minimum (map length subchains)
    trimmed   = map (take n) subchains
    mean_ vs  = sum vs / fromIntegral (length vs)
    chainMeans = map mean_ trimmed
    grandMean  = mean_ chainMeans
    b = fromIntegral n / fromIntegral (m - 1)
        * sum (map (\mu -> (mu - grandMean) ^ (2 :: Int)) chainMeans)
    chainVars = map (\vs -> let mu = mean_ vs
                            in sum (map (\x -> (x - mu) ^ (2 :: Int)) vs)
                               / fromIntegral (n - 1)) trimmed
    w       = mean_ chainVars
    varPlus = fromIntegral (n - 1) / fromIntegral n * w + b / fromIntegral n

-- | Kernel density estimation (Gaussian kernel, Silverman bandwidth).
--
-- Returns @nPoints@ pairs of @(x, density)@. With fewer than two samples
-- the returned list is empty. The grid spans @[min - 3σ, max + 3σ]@.
kde :: Int -> [Double] -> [(Double, Double)]
kde nPoints xs
  | length xs < 2 = []
  | sig <= 0      = []
  | otherwise     = [(x, density x) | x <- grid]
  where
    n    = length xs
    mu   = sum xs / fromIntegral n
    var  = sum (map (\x -> (x - mu) ^ (2 :: Int)) xs) / fromIntegral (n - 1)
    sig  = sqrt var
    h    = 1.06 * sig * fromIntegral n ** (-0.2)   -- Silverman's rule
    lo   = minimum xs - 3 * sig
    hi   = maximum xs + 3 * sig
    step = (hi - lo) / fromIntegral (nPoints - 1)
    grid = [lo + fromIntegral i * step | i <- [0 .. nPoints - 1 :: Int]]
    kernel u = exp (-0.5 * u * u) / sqrt (2 * pi)
    density x = sum [kernel ((x - xi) / h) | xi <- xs]
                / (fromIntegral n * h)

-- | Bayesian Fraction of Missing Information (Betancourt 2016).
--
-- @
-- BFMI = E[(E_n − E_{n−1})²] / Var(E)
-- @
--
-- Computed from the energy sequence (Hamiltonian per iteration) of an
-- HMC/NUTS run. Values below 0.3 indicate that momentum resampling is
-- not exploring the posterior tails (consider reparameterization — the
-- canonical example is Neal's funnel). Values above 0.3 are healthy;
-- PyMC commonly uses 0.5 as a reference threshold.
bfmi :: [Double] -> Maybe Double
bfmi es
  | length es < 4 = Nothing
  | varE == 0     = Nothing
  | otherwise     = Just (numer / varE)
  where
    n        = length es
    mu       = sum es / fromIntegral n
    varE     = sum (map (\x -> (x - mu) ^ (2 :: Int)) es)
               / fromIntegral (n - 1)
    diffs    = zipWith (-) (drop 1 es) es
    numer    = sum (map (\d -> d * d) diffs)
               / fromIntegral (length diffs)

-- | Rank-normalized per-chain histogram counts (PyMC @plot_rank@ の素材・
-- Vehtari et al. 2021)。 全 chain をプールした値に昇順 rank (1..n) を振り、
-- chain ごとに @nBins@ 個のビンへ振り分けた **ビンごとのカウント** を返す。
-- 返り値は chain ごとの長さ @nBins@ のカウント列 (= @[[count]]@・入力 chain 順)。
-- 収束時は各 chain の rank 分布が一様 (= どのビンもほぼ同数) に近づく。
--
-- ビン境界は Viz/Plot 両経路で共有するためここに一元化する (二重実装を避ける)。
rankHist :: Int -> [[Double]] -> [[Int]]
rankHist nBins perChain =
  [ [ length (filter (== b) (chainBins c)) | b <- [0 .. nBins - 1] ]
  | c <- [0 .. nCh - 1] ]
  where
    nCh       = length perChain
    flat      = [ (cid, v) | (cid, vs) <- zip [0 :: Int ..] perChain, v <- vs ]
    n         = length flat
    -- 値昇順に rank 1..n を振り、 元 (flat) 順序へ戻す
    ranked    = zipWith (\rk (oi, _) -> (oi, rk))
                        [1 :: Int ..]
                        (sortBy (comparing (snd . snd)) (zip [0 :: Int ..] flat))
    rankByIdx = map snd (sortBy (comparing fst) ranked)   -- flat 順の rank
    binSize   = max 1 (n `div` nBins)
    binOf r   = min (nBins - 1) ((r - 1) `div` binSize)
    chainSeq  = map fst flat
    chainBins c = [ binOf r | (cid, r) <- zip chainSeq rankByIdx, cid == c ]
