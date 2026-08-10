{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Stat.SPC
-- Description : 統計的工程管理 (SPC) — 管理図 (X̄-R/I-MR/p/np/c/u) + 判定ルール
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 統計的工程管理 (Statistical Process Control) — 管理図 + 判定ルール。
--
-- 変数管理図 (X̄-R / I-MR) と属性管理図 (p / np / c / u) を共通 API で扱う。
-- 判定ルール (Western Electric / Nelson) は fit と分離した pure 関数。
--
-- ===  公開 API
--
-- * 'SPCChart' / 'SPCInput' / 'SPCChartResult'
-- * 'fitSPC'
-- * 'westernElectricRules' / 'nelsonRules' / 'checkRules'
--
-- ===  典型的な使い方
--
-- > case fitSPC XR (VarSubgroups subs) of
-- >   Left err -> ...
-- >   Right [xbar, rChart] -> do
-- >     let viols = checkRules westernElectricRules xbar
-- >     ...
module Hanalyze.Stat.SPC
  ( -- * chart 種別
    SPCChart (..)
  , SPCInput  (..)
  , SPCChartResult (..)
    -- * fit
  , fitSPC
    -- * 判定ルール
  , SPCRule (..)
  , SPCViolation (..)
  , westernElectricRules
  , nelsonRules
  , checkRules
  ) where

import qualified Data.Text     as T
import qualified Data.Vector   as V
import           Data.Text     (Text)
import           Data.Vector   (Vector)

-- ===========================================================================
-- 型定義
-- ===========================================================================

-- | 管理図の種別。
data SPCChart
  = XR    -- ^ X̄-R chart (subgroup 平均 + range)
  | IMR   -- ^ I-MR chart (individual + moving range)
  | P     -- ^ p chart (不良率、 subgroup size 可変)
  | NP    -- ^ np chart (不良数、 subgroup size 一定)
  | C     -- ^ c chart (単位あたり欠陥数、 unit size 一定)
  | U     -- ^ u chart (単位あたり欠陥率、 unit size 可変)
  | EWMAChart    -- ^ EWMA (Exponentially Weighted Moving Average) chart (Phase 11)
  | CUSUMChart   -- ^ CUSUM (Cumulative Sum) chart 両側 (Phase 11)
  deriving (Show, Eq)

-- | 管理図入力。 chart 種別に対応した構成のみ受け付ける。
data SPCInput
  = -- | 変数管理図 (X̄-R) 用。 各 subgroup の観測値ベクトル。
    --   subgroup サイズ (内側 Vector の長さ) は全 subgroup で同一であること。
    VarSubgroups   !(Vector (Vector Double))
  | -- | I-MR 用。 個別観測値の系列。
    VarIndividual  !(Vector Double)
  | -- | p chart 用。 (不良数, sample size) の系列。
    AttrProportion !(Vector Int) !(Vector Int)
  | -- | np chart 用。 (不良数の系列, 一定 sample size)。
    AttrCount      !(Vector Int) !Int
  | -- | c chart 用。 欠陥数の系列 (unit size は一定と仮定)。
    AttrDefects    !(Vector Int)
  | -- | u chart 用。 (欠陥数, unit size) の系列。
    AttrDefectRate !(Vector Int) !(Vector Int)
  | -- | EWMA 用。 (個別観測値 xs, λ ∈ (0,1], L (sigma 倍数), μ₀ target, σ₀ baseline σ)。
    --   σ₀ ≤ 0 を渡すと xs の標本標準偏差で代用。
    EWMAInput      !(Vector Double) !Double !Double !Double !Double
  | -- | CUSUM 用。 (個別観測値 xs, μ₀ target, σ₀ baseline σ, k (allowance, σ単位), h (decision interval, σ単位))。
    --   σ₀ ≤ 0 を渡すと xs の標本標準偏差で代用。 両側 CUSUM (C+, C-) を返す。
    CUSUMInput     !(Vector Double) !Double !Double !Double !Double
  deriving (Show, Eq)

-- | 1 つの管理図の fit 結果。 X̄-R / I-MR では 2 つ並んで返る。
--
-- 不変条件:
--
--   * @V.length spcPoints == V.length spcUCL == V.length spcLCL@
--   * 固定 limit chart (X̄-R / I-MR / np / c) では UCL/LCL は全要素同値
--   * 変動 limit chart (p / u) では UCL/LCL が点ごとに異なる
data SPCChartResult = SPCChartResult
  { spcPoints    :: !(Vector Double)
    -- ^ 点ごとにプロットする統計量 (X̄、 R、 個別値、 MR、 p̂、 np、 c、 u 等)
  , spcCenter    :: !Double
    -- ^ 中心線 (CL)
  , spcUCL       :: !(Vector Double)
    -- ^ 上方管理限界 (点ごと)
  , spcLCL       :: !(Vector Double)
    -- ^ 下方管理限界 (点ごと)
  , spcSigma     :: !Double
    -- ^ 推定 σ (rule 判定用、 zone A/B/C の境界を計算するのに使う)
  , spcChartName :: !Text
    -- ^ "X-bar" / "R" / "I" / "MR" / "p" / "np" / "c" / "u"
  } deriving (Show)

-- ===========================================================================
-- Montgomery 定数 (n = 2..15)
-- ===========================================================================

-- | 出典: Montgomery, "Introduction to Statistical Quality Control" 9th ed.
--   Appendix VI。 @(A2, D3, D4, d2)@。
--   subgroup size 範囲外の @n@ では 'Nothing'。
subgroupConst :: Int -> Maybe (Double, Double, Double, Double)
subgroupConst n = case n of
  2  -> Just (1.880, 0.000, 3.267, 1.128)
  3  -> Just (1.023, 0.000, 2.574, 1.693)
  4  -> Just (0.729, 0.000, 2.282, 2.059)
  5  -> Just (0.577, 0.000, 2.115, 2.326)
  6  -> Just (0.483, 0.000, 2.004, 2.534)
  7  -> Just (0.419, 0.076, 1.924, 2.704)
  8  -> Just (0.373, 0.136, 1.864, 2.847)
  9  -> Just (0.337, 0.184, 1.816, 2.970)
  10 -> Just (0.308, 0.223, 1.777, 3.078)
  11 -> Just (0.285, 0.256, 1.744, 3.173)
  12 -> Just (0.266, 0.283, 1.717, 3.258)
  13 -> Just (0.249, 0.307, 1.693, 3.336)
  14 -> Just (0.235, 0.328, 1.672, 3.407)
  15 -> Just (0.223, 0.347, 1.653, 3.472)
  _  -> Nothing

-- ===========================================================================
-- 内部ヘルパ
-- ===========================================================================

vmean :: Vector Double -> Double
vmean v
  | V.null v  = 0
  | otherwise = V.sum v / fromIntegral (V.length v)

vrange :: Vector Double -> Double
vrange v
  | V.null v  = 0
  | otherwise = V.maximum v - V.minimum v

-- | 単一値で埋めた長さ @n@ の Vector。
vconst :: Int -> Double -> Vector Double
vconst n x = V.replicate n x

tshow :: Show a => a -> Text
tshow = T.pack . show

chartTag :: SPCChart -> Text
chartTag XR  = "XR"
chartTag IMR = "IMR"
chartTag P   = "P"
chartTag NP  = "NP"
chartTag C   = "C"
chartTag U   = "U"
chartTag EWMAChart  = "EWMA"
chartTag CUSUMChart = "CUSUM"

inputTag :: SPCInput -> Text
inputTag VarSubgroups{}   = "VarSubgroups"
inputTag VarIndividual{}  = "VarIndividual"
inputTag AttrProportion{} = "AttrProportion"
inputTag AttrCount{}      = "AttrCount"
inputTag AttrDefects{}    = "AttrDefects"
inputTag AttrDefectRate{} = "AttrDefectRate"
inputTag EWMAInput{}      = "EWMAInput"
inputTag CUSUMInput{}     = "CUSUMInput"

-- ===========================================================================
-- 公開関数
-- ===========================================================================

-- | 管理図を fit する。 X̄-R / I-MR は 2 chart を返す
-- (順に X̄ chart / R chart、 I chart / MR chart)。
-- chart 種別と入力の組合せが不正な場合 'Left' を返す。
fitSPC :: SPCChart -> SPCInput -> Either Text [SPCChartResult]
fitSPC XR  (VarSubgroups subs)    = fitXR subs
fitSPC IMR (VarIndividual xs)     = fitIMR xs
fitSPC P   (AttrProportion ds ns) = fitP  ds ns
fitSPC NP  (AttrCount ds n)       = fitNP ds n
fitSPC C   (AttrDefects ds)       = fitC  ds
fitSPC U   (AttrDefectRate ds ns) = fitU  ds ns
fitSPC EWMAChart  (EWMAInput xs lam ll mu0 s0)        = fitEWMA xs lam ll mu0 s0
fitSPC CUSUMChart (CUSUMInput xs mu0 s0 k h)          = fitCUSUM xs mu0 s0 k h
fitSPC chart inp =
  Left $ "Hanalyze.Stat.SPC.fitSPC: chart kind "
       <> chartTag chart
       <> " does not match input "
       <> inputTag inp

-- ---------------------------------------------------------------------------
-- X̄-R chart
-- ---------------------------------------------------------------------------

-- | X̄-R chart:
--
--   * X̄ chart: CL = X̿、 UCL = X̿ + A2·R̄、 LCL = X̿ − A2·R̄、 σ̂ = R̄ / d2
--   * R chart: CL = R̄、 UCL = D4·R̄、 LCL = D3·R̄
fitXR :: Vector (Vector Double) -> Either Text [SPCChartResult]
fitXR subs
  | V.null subs = Left "fitSPC XR: empty subgroup list"
  | otherwise =
      let !n  = V.length (V.head subs)
          !k  = V.length subs
          sizesOk = V.all (\s -> V.length s == n) subs
      in if not sizesOk
           then Left "fitSPC XR: subgroup sizes are not uniform"
           else case subgroupConst n of
             Nothing -> Left $ "fitSPC XR: subgroup size n=" <> tshow n
                            <> " is outside supported range (2..15)"
             Just (a2, d3, d4, d2c) ->
               let means   = V.map vmean  subs
                   ranges  = V.map vrange subs
                   xBarBar = vmean means
                   rBar    = vmean ranges
                   sigma   = rBar / d2c
                   uclX    = xBarBar + a2 * rBar
                   lclX    = xBarBar - a2 * rBar
                   uclR    = d4 * rBar
                   lclR    = d3 * rBar
                   xChart  = SPCChartResult
                     { spcPoints    = means
                     , spcCenter    = xBarBar
                     , spcUCL       = vconst k uclX
                     , spcLCL       = vconst k lclX
                     , spcSigma     = sigma
                     , spcChartName = "X-bar"
                     }
                   rChart  = SPCChartResult
                     { spcPoints    = ranges
                     , spcCenter    = rBar
                     , spcUCL       = vconst k uclR
                     , spcLCL       = vconst k lclR
                     , spcSigma     = sigma
                     , spcChartName = "R"
                     }
               in Right [xChart, rChart]

-- ---------------------------------------------------------------------------
-- I-MR chart
-- ---------------------------------------------------------------------------

-- | I-MR chart:
--
--   * MR_i = |x_i − x_{i−1}|  for i = 1..N−1
--   * I chart:  CL = x̄、 σ̂ = MR̄ / d2(n=2) = MR̄ / 1.128、 UCL/LCL = x̄ ± 3σ̂
--   * MR chart: CL = MR̄、 UCL = D4(2)·MR̄ = 3.267·MR̄、 LCL = D3(2)·MR̄ = 0
fitIMR :: Vector Double -> Either Text [SPCChartResult]
fitIMR xs
  | V.length xs < 2 = Left "fitSPC IMR: need at least 2 individual observations"
  | otherwise =
      let !n      = V.length xs
          xBar    = vmean xs
          mr      = V.generate (n - 1) (\i -> abs (xs V.! (i + 1) - xs V.! i))
          mrBar   = vmean mr
          (_, d3, d4, d2c) = case subgroupConst 2 of
            Just t  -> t
            Nothing -> (0, 0, 0, 1.128)  -- 到達不能
          sigma   = mrBar / d2c
          uclI    = xBar + 3 * sigma
          lclI    = xBar - 3 * sigma
          uclMR   = d4 * mrBar
          lclMR   = d3 * mrBar
          iChart  = SPCChartResult
            { spcPoints    = xs
            , spcCenter    = xBar
            , spcUCL       = vconst n uclI
            , spcLCL       = vconst n lclI
            , spcSigma     = sigma
            , spcChartName = "I"
            }
          mrChart = SPCChartResult
            { spcPoints    = mr
            , spcCenter    = mrBar
            , spcUCL       = vconst (n - 1) uclMR
            , spcLCL       = vconst (n - 1) lclMR
            , spcSigma     = sigma
            , spcChartName = "MR"
            }
      in Right [iChart, mrChart]

-- ---------------------------------------------------------------------------
-- p chart (proportion defective, variable subgroup size)
-- ---------------------------------------------------------------------------

-- | p chart:
--
--   * p̂_i = d_i / n_i
--   * p̄   = Σ d_i / Σ n_i
--   * CL  = p̄
--   * UCL_i = p̄ + 3·sqrt(p̄(1−p̄)/n_i)、 LCL_i = max(0, …)
--
-- σ̂ は **平均 n** に基づく代表値 (rule 判定用)。
fitP :: Vector Int -> Vector Int -> Either Text [SPCChartResult]
fitP ds ns
  | V.length ds /= V.length ns
      = Left "fitSPC P: defectives and sample-size series differ in length"
  | V.null ds = Left "fitSPC P: empty series"
  | V.any (< 0) ds = Left "fitSPC P: defectives must be non-negative"
  | V.any (<= 0) ns = Left "fitSPC P: sample sizes must be positive"
  | V.or (V.zipWith (>) ds ns) = Left "fitSPC P: defectives exceed sample size"
  | otherwise =
      let k       = V.length ds
          totalD  = sum (V.toList ds) :: Int
          totalN  = sum (V.toList ns) :: Int
          pBar    = fromIntegral totalD / fromIntegral totalN
          phat    = V.zipWith (\d n -> fromIntegral d / fromIntegral n) ds ns
          ucl     = V.map (\ni -> pBar + 3 * sqrt (pBar * (1 - pBar) /
                                                   fromIntegral ni)) ns
          lcl     = V.map (\ni -> max 0 $ pBar - 3 * sqrt (pBar * (1 - pBar) /
                                                           fromIntegral ni)) ns
          nMean   = fromIntegral totalN / fromIntegral k :: Double
          sigma   = sqrt (pBar * (1 - pBar) / nMean)
      in Right [SPCChartResult
        { spcPoints    = phat
        , spcCenter    = pBar
        , spcUCL       = ucl
        , spcLCL       = lcl
        , spcSigma     = sigma
        , spcChartName = "p"
        }]

-- ---------------------------------------------------------------------------
-- np chart (count defective, constant subgroup size n)
-- ---------------------------------------------------------------------------

-- | np chart (n は全 subgroup で一定):
--
--   * CL  = n·p̄ = 平均不良数
--   * σ̂  = sqrt(n·p̄·(1−p̄))
--   * UCL = n·p̄ + 3·σ̂、 LCL = max(0, …)
fitNP :: Vector Int -> Int -> Either Text [SPCChartResult]
fitNP ds n
  | V.null ds        = Left "fitSPC NP: empty defectives series"
  | n <= 0           = Left "fitSPC NP: sample size n must be positive"
  | V.any (< 0) ds   = Left "fitSPC NP: defectives must be non-negative"
  | V.any (> n) ds   = Left "fitSPC NP: defectives exceed sample size"
  | otherwise =
      let k       = V.length ds
          totalD  = sum (V.toList ds) :: Int
          pBar    = fromIntegral totalD / fromIntegral (n * k) :: Double
          cl      = fromIntegral n * pBar
          sigma   = sqrt (fromIntegral n * pBar * (1 - pBar))
          ucl     = cl + 3 * sigma
          lcl     = max 0 (cl - 3 * sigma)
          pts     = V.map fromIntegral ds :: Vector Double
      in Right [SPCChartResult
        { spcPoints    = pts
        , spcCenter    = cl
        , spcUCL       = vconst k ucl
        , spcLCL       = vconst k lcl
        , spcSigma     = sigma
        , spcChartName = "np"
        }]

-- ---------------------------------------------------------------------------
-- c chart (count of defects, constant unit size)
-- ---------------------------------------------------------------------------

-- | c chart:
--
--   * CL  = c̄ = 平均欠陥数
--   * σ̂  = sqrt(c̄)
--   * UCL = c̄ + 3·sqrt(c̄)、 LCL = max(0, …)
fitC :: Vector Int -> Either Text [SPCChartResult]
fitC ds
  | V.null ds         = Left "fitSPC C: empty defects series"
  | V.any (< 0) ds    = Left "fitSPC C: defects must be non-negative"
  | otherwise =
      let k       = V.length ds
          cBar    = fromIntegral (sum (V.toList ds)) / fromIntegral k :: Double
          sigma   = sqrt cBar
          ucl     = cBar + 3 * sigma
          lcl     = max 0 (cBar - 3 * sigma)
          pts     = V.map fromIntegral ds :: Vector Double
      in Right [SPCChartResult
        { spcPoints    = pts
        , spcCenter    = cBar
        , spcUCL       = vconst k ucl
        , spcLCL       = vconst k lcl
        , spcSigma     = sigma
        , spcChartName = "c"
        }]

-- ---------------------------------------------------------------------------
-- u chart (defect rate, variable unit size)
-- ---------------------------------------------------------------------------

-- | u chart:
--
--   * u_i = d_i / n_i
--   * ū   = Σ d_i / Σ n_i
--   * CL  = ū
--   * UCL_i = ū + 3·sqrt(ū/n_i)、 LCL_i = max(0, …)
fitU :: Vector Int -> Vector Int -> Either Text [SPCChartResult]
fitU ds ns
  | V.length ds /= V.length ns
      = Left "fitSPC U: defects and unit-size series differ in length"
  | V.null ds       = Left "fitSPC U: empty series"
  | V.any (< 0) ds  = Left "fitSPC U: defects must be non-negative"
  | V.any (<= 0) ns = Left "fitSPC U: unit sizes must be positive"
  | otherwise =
      let k       = V.length ds
          totalD  = fromIntegral (sum (V.toList ds)) :: Double
          totalN  = fromIntegral (sum (V.toList ns)) :: Double
          uBar    = totalD / totalN
          us      = V.zipWith (\d n -> fromIntegral d / fromIntegral n) ds ns
          ucl     = V.map (\ni -> uBar + 3 * sqrt (uBar / fromIntegral ni)) ns
          lcl     = V.map (\ni -> max 0 (uBar - 3 * sqrt (uBar / fromIntegral ni))) ns
          nMean   = totalN / fromIntegral k
          sigma   = sqrt (uBar / nMean)
      in Right [SPCChartResult
        { spcPoints    = us
        , spcCenter    = uBar
        , spcUCL       = ucl
        , spcLCL       = lcl
        , spcSigma     = sigma
        , spcChartName = "u"
        }]

-- ===========================================================================
-- 判定ルール (Phase 1.4 / 1.5 で実装)
-- ===========================================================================

-- | 判定ルール 1 個。
data SPCRule = SPCRule
  { ruleName   :: !Text                       -- ^ "Western Electric 1" / "Nelson 1" 等
  , ruleNumber :: !Int                        -- ^ ルール番号 (1..8)
  , ruleCheck  :: SPCChartResult -> [Int]     -- ^ 違反点の 0-origin index list
  }

-- | ルール違反 1 件。
data SPCViolation = SPCViolation
  { vRuleName    :: !Text
  , vRuleNumber  :: !Int
  , vPointIndex  :: !Int
  , vChartName   :: !Text   -- ^ どの chart で違反したか (X-bar / R / 等)
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- 内部パターン検出 (rule 共通)
-- ---------------------------------------------------------------------------

-- $patternDetectors
-- ゾーン境界は CL ± k·σ で定義 (σ は 'spcSigma' フィールド)。
-- 可変 limit chart (p / u) では σ は代表値 (平均 n から算出) なので、
-- ゾーン判定はやや近似となる (canvas display 用途では実用上問題なし)。

-- | k·σ の絶対値を超えた点の index (0-origin)。 chart 種別非依存。
beyondSigma :: Double -> SPCChartResult -> [Int]
beyondSigma k r =
  let cl    = spcCenter r
      sigma = spcSigma r
      pts   = V.toList (spcPoints r)
  in [ i | (i, x) <- zip [0..] pts
         , abs (x - cl) > k * sigma ]

-- | k·σ を超える点について「+ なら +1、 − なら −1、 ゾーン内なら 0」。
sideAtSigma :: Double -> SPCChartResult -> [Int]
sideAtSigma k r =
  let cl    = spcCenter r
      sigma = spcSigma r
      pts   = V.toList (spcPoints r)
      classify x
        | x - cl >  k * sigma =  1
        | x - cl < -k * sigma = -1
        | otherwise           =  0
  in map classify pts

-- | CL に対する符号 (上 = +1, 下 = -1, 上 = 0)。
sideOfCenter :: SPCChartResult -> [Int]
sideOfCenter r =
  let cl    = spcCenter r
      pts   = V.toList (spcPoints r)
      classify x
        | x >  cl =  1
        | x <  cl = -1
        | otherwise = 0
  in map classify pts

-- | N 個連続で同符号 (CL の同じ側) になっている末尾点の index を返す。
--   例: 8 連続 → 連続区間の 8 点目以降を全部 violation として返す。
runSameSide :: Int -> SPCChartResult -> [Int]
runSameSide n r = go 0 0 0 (sideOfCenter r) []
  where
    go !i !curSide !runLen ss acc = case ss of
      []     -> reverse acc
      (s:xs) ->
        let (curSide', runLen')
              | s == 0           = (0, 0)
              | s == curSide     = (curSide, runLen + 1)
              | otherwise        = (s, 1)
            acc' | runLen' >= n = i : acc
                 | otherwise    = acc
        in go (i + 1) curSide' runLen' xs acc'

-- | N 個連続で単調 (全て上昇 or 全て下降) のパターンの末尾 index。
trendMono :: Int -> SPCChartResult -> [Int]
trendMono n r = go 0 0 0 (V.toList (spcPoints r)) []
  where
    -- direction: +1 = increasing, -1 = decreasing, 0 = none yet
    go _ _ _ [] acc = reverse acc
    go _ _ _ [_] acc = reverse acc
    go !i !dir !runLen (x : ys@(y : _)) acc =
      let d | y > x =  1
            | y < x = -1
            | otherwise = 0
          (dir', runLen')
            | d == 0      = (0, 0)
            | d == dir    = (dir, runLen + 1)
            | otherwise   = (d, 2)   -- 始まり: 2 点で run=2
          -- 違反 = runLen が n 以上、 i+1 (現在の y) の index を記録
          acc' | runLen' >= n = (i + 1) : acc
               | otherwise    = acc
      in go (i + 1) dir' runLen' ys acc'

-- | N 個連続で交互上下のパターンの末尾 index。
alternating :: Int -> SPCChartResult -> [Int]
alternating n r = go 0 0 0 (V.toList (spcPoints r)) []
  where
    go _ _ _ [] acc = reverse acc
    go _ _ _ [_] acc = reverse acc
    go !i !lastDir !runLen (x : ys@(y : _)) acc =
      let d | y > x =  1
            | y < x = -1
            | otherwise = 0
          (lastDir', runLen')
            | d == 0                       = (0, 0)
            | lastDir == 0                 = (d, 2)
            | d == negate lastDir          = (d, runLen + 1)
            | otherwise                    = (d, 2)
          acc' | runLen' >= n = (i + 1) : acc
               | otherwise    = acc
      in go (i + 1) lastDir' runLen' ys acc'

-- | k 個連続で σ 倍の絶対値以内 (= ゾーン C 内のみ) の末尾 index。
--   stratification (W-E rule 6 / Nelson 7)。
withinSigma :: Int -> Double -> SPCChartResult -> [Int]
withinSigma n k r =
  let cl    = spcCenter r
      sigma = spcSigma r
      pts   = V.toList (spcPoints r)
      flags = map (\x -> abs (x - cl) <= k * sigma) pts
  in collectRun n flags

-- | k 個連続で σ 倍の絶対値より外 (= ゾーン A or B、 中央線の同/異側問わず) の末尾 index。
--   mixture (W-E rule 7 / Nelson 8)。
beyondSigmaEither :: Int -> Double -> SPCChartResult -> [Int]
beyondSigmaEither n k r =
  let cl    = spcCenter r
      sigma = spcSigma r
      pts   = V.toList (spcPoints r)
      flags = map (\x -> abs (x - cl) > k * sigma) pts
  in collectRun n flags

-- | True が n 個以上連続するパターンの末尾 index 集合。
collectRun :: Int -> [Bool] -> [Int]
collectRun n = go 0 0 []
  where
    go _ _ acc [] = reverse acc
    go !i !rn acc (f : fs) =
      let rn'  = if f then rn + 1 else 0
          acc' | rn' >= n = i : acc
               | otherwise = acc
      in go (i + 1) rn' acc' fs

-- | 「直近 m 点のうち k 点以上が k·σ を **同じ側** で超えている」 末尾 index。
--   Western Electric 2 / 3 用 (m, k, σ係数)。
kOfMBeyondSameSide :: Int -> Int -> Double -> SPCChartResult -> [Int]
kOfMBeyondSameSide kth m sigK r = go 0 (sideAtSigma sigK r) []
  where
    go _ ss acc | length ss < m = reverse acc
    go !i ss acc =
      let window = take m ss
          posCount = length (filter (==  1) window)
          negCount = length (filter (== -1) window)
          hit      = posCount >= kth || negCount >= kth
          -- 違反 index は window の末尾 (= i + m - 1)
          acc' | hit       = (i + m - 1) : acc
               | otherwise = acc
      in case ss of
           []     -> reverse acc'
           (_:xs) -> go (i + 1) xs acc'

-- ---------------------------------------------------------------------------
-- Western Electric rules (WECO 8 rules)
-- ---------------------------------------------------------------------------

-- | Western Electric Company (WECO) rules。 8 rules。
--
-- (Western Electric Statistical Quality Control Handbook 1956 +
-- 一般的な 8-rule 拡張)
--
--   * Rule 1: 1 点が 3σ 超
--   * Rule 2: 3 点中 2 点が同じ側で 2σ 超
--   * Rule 3: 5 点中 4 点が同じ側で 1σ 超
--   * Rule 4: 8 点連続で CL の同じ側
--   * Rule 5: 6 点連続で単調 (上昇 or 下降)
--   * Rule 6: 15 点連続で 1σ 以内 (stratification)
--   * Rule 7: 8 点連続で 1σ 外 (mixture; どちら側でも可)
--   * Rule 8: 14 点連続で交互上下
westernElectricRules :: [SPCRule]
westernElectricRules =
  [ SPCRule "Western Electric 1" 1 (beyondSigma 3)
  , SPCRule "Western Electric 2" 2 (kOfMBeyondSameSide 2 3 2)
  , SPCRule "Western Electric 3" 3 (kOfMBeyondSameSide 4 5 1)
  , SPCRule "Western Electric 4" 4 (runSameSide 8)
  , SPCRule "Western Electric 5" 5 (trendMono 6)
  , SPCRule "Western Electric 6" 6 (withinSigma 15 1)
  , SPCRule "Western Electric 7" 7 (beyondSigmaEither 8 1)
  , SPCRule "Western Electric 8" 8 (alternating 14)
  ]

-- ---------------------------------------------------------------------------
-- Nelson rules (1984、 8 rules)
-- ---------------------------------------------------------------------------

-- | Nelson rules (Nelson, L.S. 1984, J. Qual. Tech.)。 8 rules。
--
-- WE 8 rules と多くが重複するが、 ルール番号と一部の N が異なる:
--
--   * Rule 1: 1 点が 3σ 超                                   (= WE 1)
--   * Rule 2: 9 点連続で CL の同じ側                          (WE 4 は 8 点)
--   * Rule 3: 6 点連続で単調                                  (= WE 5)
--   * Rule 4: 14 点連続で交互上下                              (= WE 8)
--   * Rule 5: 3 点中 2 点が同じ側で 2σ 超                      (= WE 2)
--   * Rule 6: 5 点中 4 点が同じ側で 1σ 超                      (= WE 3)
--   * Rule 7: 15 点連続で 1σ 以内                              (= WE 6)
--   * Rule 8: 8 点連続で 1σ 外 (どちら側でも可)                (= WE 7)
--
-- 検出ロジックは [[westernElectricRules]] と同じヘルパを再利用。
nelsonRules :: [SPCRule]
nelsonRules =
  [ SPCRule "Nelson 1" 1 (beyondSigma 3)
  , SPCRule "Nelson 2" 2 (runSameSide 9)
  , SPCRule "Nelson 3" 3 (trendMono 6)
  , SPCRule "Nelson 4" 4 (alternating 14)
  , SPCRule "Nelson 5" 5 (kOfMBeyondSameSide 2 3 2)
  , SPCRule "Nelson 6" 6 (kOfMBeyondSameSide 4 5 1)
  , SPCRule "Nelson 7" 7 (withinSigma 15 1)
  , SPCRule "Nelson 8" 8 (beyondSigmaEither 8 1)
  ]

-- | 指定したルール集合で違反点を検出する。
checkRules :: [SPCRule] -> SPCChartResult -> [SPCViolation]
checkRules rs r =
  [ SPCViolation (ruleName ru) (ruleNumber ru) i (spcChartName r)
  | ru <- rs
  , i  <- ruleCheck ru r
  ]

-- ---------------------------------------------------------------------------
-- EWMA chart (Phase 11)
-- ---------------------------------------------------------------------------

-- | EWMA chart:
--
--   * 再帰: @z_i = λ x_i + (1 − λ) z_{i−1}@, @z_0 = μ₀@
--   * 時変管理限界: @μ₀ ± L σ √(λ/(2−λ) · (1 − (1−λ)^{2i}))@
--   * σ₀ ≤ 0 のとき xs の標本標準偏差で代用。
--
-- 入力検証: 0 < λ ≤ 1, L > 0, |xs| ≥ 1。
fitEWMA :: Vector Double -> Double -> Double -> Double -> Double
        -> Either Text [SPCChartResult]
fitEWMA xs lam ll mu0 s0In
  | V.null xs                = Left "fitSPC EWMA: empty input"
  | not (lam > 0 && lam <= 1) = Left "fitSPC EWMA: λ must be in (0, 1]"
  | ll <= 0                  = Left "fitSPC EWMA: L must be > 0"
  | otherwise =
      let !n     = V.length xs
          !sigma = if s0In > 0 then s0In else sampleSD xs
          zs     = V.scanl' (\z x -> lam * x + (1 - lam) * z) mu0 xs
          -- scanl' includes initial → drop the seed
          zsTail = V.tail zs
          ucl = V.generate n (\i ->
            let i1 = fromIntegral (i + 1) :: Double
                factor = lam / (2 - lam) * (1 - (1 - lam) ** (2 * i1))
            in mu0 + ll * sigma * sqrt factor)
          lcl = V.generate n (\i ->
            let i1 = fromIntegral (i + 1) :: Double
                factor = lam / (2 - lam) * (1 - (1 - lam) ** (2 * i1))
            in mu0 - ll * sigma * sqrt factor)
      in Right [ SPCChartResult
                   { spcPoints    = zsTail
                   , spcCenter    = mu0
                   , spcUCL       = ucl
                   , spcLCL       = lcl
                   , spcSigma     = sigma
                   , spcChartName = "EWMA"
                   } ]

-- ---------------------------------------------------------------------------
-- CUSUM chart (Phase 11)
-- ---------------------------------------------------------------------------

-- | CUSUM (両側) chart:
--
--   * @C⁺_i = max(0, x_i − (μ₀ + k σ) + C⁺_{i−1})@,  @C⁺_0 = 0@
--   * @C⁻_i = max(0, (μ₀ − k σ) − x_i + C⁻_{i−1})@,  @C⁻_0 = 0@
--   * 決定限界: @H = h σ@  (上側のみ、 下側は @−H@ として描画用に @-1 × C⁻@ を返す)
--
-- 返り値: [C⁺ chart, C⁻ chart]。 C⁻ chart は points が負方向に出るよう
-- @spcPoints = − C⁻@ として表現し、 LCL = −H、 UCL = 0 とする。
fitCUSUM :: Vector Double -> Double -> Double -> Double -> Double
         -> Either Text [SPCChartResult]
fitCUSUM xs mu0 s0In k h
  | V.null xs = Left "fitSPC CUSUM: empty input"
  | k < 0     = Left "fitSPC CUSUM: k must be ≥ 0"
  | h <= 0    = Left "fitSPC CUSUM: h must be > 0"
  | otherwise =
      let !n     = V.length xs
          !sigma = if s0In > 0 then s0In else sampleSD xs
          kAbs   = k * sigma
          hAbs   = h * sigma
          cPos   = V.scanl' (\c x -> max 0 (c + (x - mu0) - kAbs)) 0 xs
          cNeg   = V.scanl' (\c x -> max 0 (c + (mu0 - x) - kAbs)) 0 xs
          cPosT  = V.tail cPos
          cNegT  = V.tail cNeg
          chartPos = SPCChartResult
            { spcPoints    = cPosT
            , spcCenter    = 0
            , spcUCL       = vconst n hAbs
            , spcLCL       = vconst n 0
            , spcSigma     = sigma
            , spcChartName = "CUSUM+"
            }
          chartNeg = SPCChartResult
            { spcPoints    = V.map negate cNegT
            , spcCenter    = 0
            , spcUCL       = vconst n 0
            , spcLCL       = vconst n (-hAbs)
            , spcSigma     = sigma
            , spcChartName = "CUSUM-"
            }
      in Right [chartPos, chartNeg]

-- | 標本標準偏差 (n-1 補正)。 EWMA/CUSUM の σ₀ デフォルト用。
sampleSD :: Vector Double -> Double
sampleSD xs
  | V.length xs < 2 = 0
  | otherwise =
      let m  = vmean xs
          ss = V.sum (V.map (\x -> (x - m) ** 2) xs)
      in sqrt (ss / fromIntegral (V.length xs - 1))
