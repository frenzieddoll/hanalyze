{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Design.Quality
-- Description : 計画評価指標 (直交性・D/A-efficiency・VIF) と工程能力指数 (Cp/Cpk 等) の算出
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Quality criteria for evaluating designs.
--
--   * 'isOrthogonal'       — are the design columns orthogonal? (i.e.
--     @XᵀX@ diagonal).
--   * 'orthogonalityScore' — numeric orthogonality score in @[0, 1]@.
--   * 'conditionNumber'    — condition number of @XᵀX@ (large values
--     indicate multicollinearity).
--   * 'dEfficiency'        — D-efficiency @det(XᵀX/n)^(1/p)@.
--   * 'aEfficiency'        — A-efficiency: reciprocal of
--     @trace((XᵀX/n)⁻¹)@.
--   * 'vifList'            — per-column Variance Inflation Factor.
module Hanalyze.Design.Quality
  ( isOrthogonal
  , orthogonalityScore
  , conditionNumber
  , dEfficiency
  , aEfficiency
  , vifList
    -- * Process capability
  , Capability (..)
  , processCapability
  , processCapabilityUpper
  , processCapabilityLower
  , processCapabilityWeibull
  , processCapabilityLogNormal
  , processCapabilityGamma
    -- * Process capability — unified non-normal entry (Phase 23-c)
  , NonNormalFit (..)
  , processCapabilityNonNormal
    -- * 多変量 Process Capability (Phase 23-d)
  , MultivariateCapability (..)
  , processCapabilityMultivariate
  ) where

import           Data.Text                       (Text)
import qualified Numeric.LinearAlgebra as LA
import qualified Statistics.Distribution         as SD
import qualified Statistics.Distribution.Normal  as Normal
import qualified Statistics.Distribution.Gamma   as Gamma
import           Hanalyze.Model.Weibull          (WeibullFit (..))

-- | True iff the design matrix @X@ is orthogonal (i.e. @XᵀX@ is
-- diagonal up to tolerance @ε@).
isOrthogonal :: Double -> [[Double]] -> Bool
isOrthogonal eps xs =
  let m   = LA.fromLists xs
      xtx = LA.tr m LA.<> m
      n   = LA.rows xtx
      offDiagSum =
        sum [ abs (xtx `LA.atIndex` (i, j))
            | i <- [0 .. n - 1]
            , j <- [0 .. n - 1]
            , i /= j ]
  in offDiagSum < eps

-- | Orthogonality score in @[0, 1]@: 0 = far from orthogonal,
-- 1 = exactly orthogonal. Compares the off-diagonal mass against the
-- diagonal mass.
orthogonalityScore :: [[Double]] -> Double
orthogonalityScore xs =
  let m   = LA.fromLists xs
      xtx = LA.tr m LA.<> m
      n   = LA.rows xtx
      diagSum =
        sum [ abs (xtx `LA.atIndex` (i, i)) | i <- [0 .. n - 1] ]
      offDiagSum =
        sum [ abs (xtx `LA.atIndex` (i, j))
            | i <- [0 .. n - 1]
            , j <- [0 .. n - 1]
            , i /= j ]
  in if diagSum == 0 then 0
       else 1 - offDiagSum / (diagSum + offDiagSum)

-- | Condition number of @XᵀX@ (@λ_max / λ_min@). Values above 30
-- typically indicate multicollinearity.
conditionNumber :: [[Double]] -> Double
conditionNumber xs =
  let m   = LA.fromLists xs
      xtx = LA.tr m LA.<> m
      svs = LA.singularValues xtx
      sList = LA.toList svs
  in if null sList || minimum sList == 0
       then 1 / 0   -- ∞
       else maximum sList / minimum sList

-- | D-efficiency @det(XᵀX/n)^(1/p)@ — to be maximized. Approaches 1 for
-- a fully orthogonal design.
dEfficiency :: [[Double]] -> Double
dEfficiency xs =
  let m   = LA.fromLists xs
      n   = fromIntegral (LA.rows m) :: Double
      p   = fromIntegral (LA.cols m) :: Double
      xtx = LA.tr m LA.<> m
      detV = LA.det (LA.scale (1/n) xtx)
  in if detV <= 0 then 0
       else detV ** (1 / p)

-- | A-efficiency: reciprocal of @trace((XᵀX/n)⁻¹)@. A smaller trace
-- means higher per-coefficient estimation precision.
aEfficiency :: [[Double]] -> Double
aEfficiency xs =
  let m   = LA.fromLists xs
      n   = fromIntegral (LA.rows m) :: Double
      p   = fromIntegral (LA.cols m) :: Double
      xtx = LA.tr m LA.<> m
      detV = LA.det xtx
  in if detV == 0 then 0
       else
         let inv = LA.inv (LA.scale (1/n) xtx)
             tr  = sum [inv `LA.atIndex` (i, i)
                       | i <- [0 .. round p - 1] :: [Int]]
         in p / tr

-- | Per-column Variance Inflation Factor.
--
-- @VIF_j = 1 / (1 - R²_j)@, where @R²_j@ is the coefficient of
-- determination from regressing column @j@ on the others.
-- @VIF > 10@ is a strong sign of multicollinearity.
vifList :: [[Double]] -> [Double]
vifList xs =
  let m   = LA.fromLists xs
      p   = LA.cols m
  in [vifFor m j | j <- [0 .. p - 1]]
  where
    vifFor mat j =
      let yCol  = LA.flatten (mat LA.¿ [j])
          xCols = [k | k <- [0 .. LA.cols mat - 1], k /= j]
          xRest = mat LA.¿ xCols
          beta  = LA.flatten (xRest LA.<\> LA.asColumn yCol)
          yHat  = xRest LA.#> beta
          ssRes = LA.sumElements ((yCol - yHat) ^ (2 :: Int))
          mu    = LA.sumElements yCol / fromIntegral (LA.size yCol)
          ssTot = LA.sumElements ((yCol - LA.scalar mu) ^ (2 :: Int))
          r2    = if ssTot == 0 then 0 else 1 - ssRes / ssTot
      in if r2 >= 1 then 1/0 else 1 / (1 - r2)

-- ---------------------------------------------------------------------------
-- Process capability (Cp / Cpk)
-- ---------------------------------------------------------------------------

-- | Process capability summary.
--
--   * @capCp  = (USL − LSL) / (6 σ)@
--   * @capCpk = min((USL − μ) / (3 σ), (μ − LSL) / (3 σ))@
--
-- For one-sided variants (no LSL or no USL) only the relevant half of
-- @Cpk@ is used; @Cp@ falls back to that half (so @Cp == Cpk@).
data Capability = Capability
  { capCp   :: !Double
  , capCpk  :: !Double
  , capMean :: !Double
  , capSd   :: !Double
  } deriving (Show, Eq)

-- | Two-sided process capability with explicit @LSL@ and @USL@.
processCapability
  :: Double            -- ^ LSL (lower spec limit)
  -> Double            -- ^ USL (upper spec limit)
  -> LA.Vector Double  -- ^ Sample observations.
  -> Capability
processCapability lsl usl xs =
  let (mu, sd) = meanSd xs
      cp       = if sd == 0 then 0 else (usl - lsl) / (6 * sd)
      cpkUpper = if sd == 0 then 0 else (usl - mu) / (3 * sd)
      cpkLower = if sd == 0 then 0 else (mu - lsl) / (3 * sd)
      cpk      = min cpkUpper cpkLower
  in Capability cp cpk mu sd

-- | One-sided upper-spec process capability (only @USL@).
processCapabilityUpper :: Double -> LA.Vector Double -> Capability
processCapabilityUpper usl xs =
  let (mu, sd) = meanSd xs
      cpk      = if sd == 0 then 0 else (usl - mu) / (3 * sd)
  in Capability cpk cpk mu sd

-- | One-sided lower-spec process capability (only @LSL@).
processCapabilityLower :: Double -> LA.Vector Double -> Capability
processCapabilityLower lsl xs =
  let (mu, sd) = meanSd xs
      cpk      = if sd == 0 then 0 else (mu - lsl) / (3 * sd)
  in Capability cpk cpk mu sd

-- | Process Capability for **Weibull-distributed** characteristics.
--
-- 非正規分布の場合、 6σ では裾を過小評価する。 ISO 22514 / AIAG 推奨の
-- パーセンタイル法:
--
-- > Cp  = (USL − LSL) / (P_{0.99865} − P_{0.00135})
-- > Cpk = min( (USL − median) / (P_{0.99865} − median),
-- >            (median − LSL) / (median − P_{0.00135}) )
--
-- Weibull quantile: @F⁻¹(p) = λ · (−log(1 − p))^{1/k}@
processCapabilityWeibull
  :: WeibullFit
  -> Double            -- ^ LSL
  -> Double            -- ^ USL
  -> Capability
processCapabilityWeibull wf lsl usl =
  let k   = wfShape wf
      lam = wfScale wf
      q p = lam * ((-log (1 - p)) ** (1 / k))
      pLo  = q 0.00135
      pHi  = q 0.99865
      med  = q 0.5
      spread = pHi - pLo
      cp   = if spread == 0 then 0 else (usl - lsl) / spread
      cpkU = if pHi == med then 0 else (usl - med) / (pHi - med)
      cpkL = if med == pLo then 0 else (med - lsl) / (med - pLo)
      cpk  = min cpkU cpkL
  in Capability cp cpk med spread

-- | Process Capability for **LogNormal-distributed** characteristics.
--   引数は log-scale の μ, σ (ln X ~ Normal(μ, σ²))。
--
-- > X_p = exp(μ + σ · z_p)
processCapabilityLogNormal
  :: Double            -- ^ μ (log scale mean)
  -> Double            -- ^ σ (log scale sd)
  -> Double            -- ^ LSL
  -> Double            -- ^ USL
  -> Capability
processCapabilityLogNormal mu sigma lsl usl =
  let zHi = SD.quantile Normal.standard 0.99865
      zLo = SD.quantile Normal.standard 0.00135
      pHi = exp (mu + sigma * zHi)
      pLo = exp (mu + sigma * zLo)
      med = exp mu
      spread = pHi - pLo
      cp   = if spread == 0 then 0 else (usl - lsl) / spread
      cpkU = if pHi == med then 0 else (usl - med) / (pHi - med)
      cpkL = if med == pLo then 0 else (med - lsl) / (med - pLo)
      cpk  = min cpkU cpkL
  in Capability cp cpk med spread

-- | Process Capability for **Gamma-distributed** characteristics (Phase 23-c)。
--   shape (= k) と scale (= θ) を引数に取る (statistics-0.16 の @gammaDistr@ と同表記)。
--   rate β = 1 / θ を使うユーザは scale = 1/β で渡す。
--
--   分位点法 (ISO 22514) で Cp / Cpk を算出:
--
--   > Cp  = (USL − LSL) / (P_{0.99865} − P_{0.00135})
--   > Cpk = min( (USL − median) / (P_{0.99865} − median),
--   >            (median − LSL) / (median − P_{0.00135}) )
processCapabilityGamma
  :: Double            -- ^ shape (k > 0)
  -> Double            -- ^ scale (θ > 0)
  -> Double            -- ^ LSL
  -> Double            -- ^ USL
  -> Capability
processCapabilityGamma shape scale lsl usl =
  let d   = Gamma.gammaDistr shape scale
      pLo = SD.quantile d 0.00135
      pHi = SD.quantile d 0.99865
      med = SD.quantile d 0.5
      spread = pHi - pLo
      cp   = if spread == 0 then 0 else (usl - lsl) / spread
      cpkU = if pHi == med then 0 else (usl - med) / (pHi - med)
      cpkL = if med == pLo then 0 else (med - lsl) / (med - pLo)
      cpk  = min cpkU cpkL
  in Capability cp cpk med spread

-- | 非正規 Cp の統一エントリ用 ADT (Phase 23-c)。 spec: doe-spec v0.2 §3.13。
data NonNormalFit
  = NNFWeibull   !WeibullFit       -- ^ Weibull MLE 結果
  | NNFLogNormal !Double !Double    -- ^ log-scale μ, σ
  | NNFGamma     !Double !Double    -- ^ shape, scale
  deriving (Show)

-- | 非正規分布 fit の type tag で Weibull / LogNormal / Gamma を dispatch。
-- 個別関数 (@processCapabilityWeibull@ 等) と等価、 ADT で取り回したいケース用。
processCapabilityNonNormal
  :: NonNormalFit
  -> Double          -- ^ LSL
  -> Double          -- ^ USL
  -> Capability
processCapabilityNonNormal (NNFWeibull   wf)         = processCapabilityWeibull   wf
processCapabilityNonNormal (NNFLogNormal mu sigma)   = processCapabilityLogNormal mu sigma
processCapabilityNonNormal (NNFGamma     k  scale)   = processCapabilityGamma     k  scale

-- ---------------------------------------------------------------------------
-- 多変量 Process Capability (Phase 23-d、 spec: doe-spec v0.2 §2.10 / §3.13)
-- ---------------------------------------------------------------------------

-- | 多変量 Process Capability の結果。
--
-- @mcMCp@ は Wang-Hubele-Lawrence (1994) 風の体積比ベース:
--
-- > MCp = (det(Σ_T) / det(Σ))^(1/(2p))
--
-- ここで Σ_T = diag(((USL_i − LSL_i) / 6)²) (= 各軸 6σ 相当の理想分散)、
-- Σ は標本共分散、 p は変数数。 単変量 Cp の自然な多変量拡張。
--
-- @mcMCpk@ は中心オフセット penalty を乗じた値:
--
-- > MCpk = MCp · max(0, 1 − sqrt(T²) / 3)
-- > T²   = (μ_data − μ_T)' Σ⁻¹ (μ_data − μ_T)
-- > μ_T  = (LSL + USL) / 2
--
-- @mcInSpecRate@ は spec box (per-variable LSL/USL) の内包率 (実測)。
data MultivariateCapability = MultivariateCapability
  { mcNVars       :: !Int
  , mcMean        :: !(LA.Vector Double)
  , mcCov         :: !(LA.Matrix Double)
  , mcMCp         :: !Double
  , mcMCpk        :: !Double
  , mcInSpecRate  :: !Double
  } deriving (Show)

-- | 多変量 Cp 計算。 入力 @data@ は n 行 × p 列の観測行列。
-- @specs@ は各変数の (LSL, USL) を **列順** に与える。
--
-- @Left@ を返すケース:
--
--   * @specs@ の長さが列数と一致しない
--   * n < 2 (共分散が定義されない)
--   * 共分散が singular (= det ≈ 0)
processCapabilityMultivariate
  :: LA.Matrix Double
  -> [(Double, Double)]
  -> Either Text MultivariateCapability
processCapabilityMultivariate dat specs
  | p == 0                = Left "processCapabilityMultivariate: empty data (0 columns)"
  | length specs /= p     = Left "processCapabilityMultivariate: specs length ≠ #columns"
  | n < 2                 = Left "processCapabilityMultivariate: need at least 2 observations"
  | any (\(lo, hi) -> hi <= lo) specs =
      Left "processCapabilityMultivariate: each USL must be > LSL"
  | abs detSigma < 1e-12  = Left "processCapabilityMultivariate: covariance is singular"
  | otherwise =
      Right MultivariateCapability
        { mcNVars      = p
        , mcMean       = mu
        , mcCov        = sigma
        , mcMCp        = mcp
        , mcMCpk       = mcpk
        , mcInSpecRate = inSpec
        }
  where
    n         = LA.rows dat
    p         = LA.cols dat
    mu        = LA.scale (1 / fromIntegral n) (LA.fromList [LA.sumElements (col j) | j <- [0 .. p - 1]])
    col j     = LA.flatten (LA.subMatrix (0, j) (n, 1) dat)
    centered  = LA.fromRows [ LA.fromList [(dat `LA.atIndex` (i, j)) - (mu `LA.atIndex` j) | j <- [0 .. p - 1]] | i <- [0 .. n - 1] ]
    sigma     = LA.scale (1 / fromIntegral (n - 1)) (LA.tr centered LA.<> centered)
    detSigma  = LA.det sigma
    sigmaT    = LA.diagl [ ((hi - lo) / 6) ** 2 | (lo, hi) <- specs ]
    detSigmaT = LA.det sigmaT
    pD        = fromIntegral p :: Double
    mcp       = (detSigmaT / detSigma) ** (1 / (2 * pD))
    muT       = LA.fromList [ (lo + hi) / 2 | (lo, hi) <- specs ]
    diff      = mu - muT
    invSigma  = LA.inv sigma
    t2        = diff LA.<.> (invSigma LA.#> diff)
    penalty   = max 0 (1 - sqrt (max 0 t2) / 3)
    mcpk      = mcp * penalty
    inSpec    =
      let rowsXs = LA.toLists dat
          inside r = and [ lo <= x && x <= hi | (x, (lo, hi)) <- zip r specs ]
          k = length (filter inside rowsXs)
      in fromIntegral k / fromIntegral n

-- | Sample mean and unbiased standard deviation.
meanSd :: LA.Vector Double -> (Double, Double)
meanSd xs =
  let n  = LA.size xs
      nD = fromIntegral n :: Double
      mu = LA.sumElements xs / nD
      d  = LA.cmap (subtract mu) xs
      v  = if n <= 1 then 0
                     else (d `LA.dot` d) / (nD - 1.0)
  in (mu, sqrt v)
