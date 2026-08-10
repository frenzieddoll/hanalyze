{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Design.Quality
-- Description : 計画評価指標 (直交性・D/A-efficiency・VIF) と工程能力指数 (Cp/Cpk 等) の算出
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 計画評価の品質規準。
--
--   - 'isOrthogonal'       — 設計の列が直交か (= @XᵀX@ が対角行列か)。
--   - 'orthogonalityScore' — @[0, 1]@ の数値的直交性スコア。
--   - 'conditionNumber'    — @XᵀX@ の条件数 (大きい値は多重共線性を示す)。
--   - 'dEfficiency'        — D-efficiency @det(XᵀX/n)^(1/p)@。
--   - 'aEfficiency'        — A-efficiency: @trace((XᵀX/n)⁻¹)@ の逆数。
--   - 'vifList'            — 列ごとの Variance Inflation Factor。
--
-- [English]: Quality criteria for evaluating designs.
--
--   - 'isOrthogonal'       — are the design columns orthogonal? (i.e.
--     @XᵀX@ diagonal).
--   - 'orthogonalityScore' — numeric orthogonality score in @[0, 1]@.
--   - 'conditionNumber'    — condition number of @XᵀX@ (large values
--     indicate multicollinearity).
--   - 'dEfficiency'        — D-efficiency @det(XᵀX/n)^(1/p)@.
--   - 'aEfficiency'        — A-efficiency: reciprocal of
--     @trace((XᵀX/n)⁻¹)@.
--   - 'vifList'            — per-column Variance Inflation Factor.
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

-- | [日本語]: 設計行列 @X@ が直交 (= @XᵀX@ が許容誤差 @ε@ の範囲で対角行列) なら True。
--   [English]: True iff the design matrix @X@ is orthogonal (i.e. @XᵀX@
--   is diagonal up to tolerance @ε@).
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

-- | [日本語]: @[0, 1]@ の直交性スコア: 0 = 直交から遠い、 1 = 完全に直交。
--   対角成分と非対角成分の質量を比較する。
--   [English]: Orthogonality score in @[0, 1]@: 0 = far from orthogonal,
--   1 = exactly orthogonal. Compares the off-diagonal mass against the
--   diagonal mass.
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

-- | [日本語]: @XᵀX@ の条件数 (@λ_max / λ_min@)。 30 を超える値は多重共線性を
--   示すことが多い。
--   [English]: Condition number of @XᵀX@ (@λ_max / λ_min@). Values above
--   30 typically indicate multicollinearity.
conditionNumber :: [[Double]] -> Double
conditionNumber xs =
  let m   = LA.fromLists xs
      xtx = LA.tr m LA.<> m
      svs = LA.singularValues xtx
      sList = LA.toList svs
  in if null sList || minimum sList == 0
       then 1 / 0   -- ∞
       else maximum sList / minimum sList

-- | [日本語]: D-efficiency @det(XᵀX/n)^(1/p)@ — 最大化すべき量。 完全直交設計で 1 に近づく。
--   [English]: D-efficiency @det(XᵀX/n)^(1/p)@ — to be maximized.
--   Approaches 1 for a fully orthogonal design.
dEfficiency :: [[Double]] -> Double
dEfficiency xs =
  let m   = LA.fromLists xs
      n   = fromIntegral (LA.rows m) :: Double
      p   = fromIntegral (LA.cols m) :: Double
      xtx = LA.tr m LA.<> m
      detV = LA.det (LA.scale (1/n) xtx)
  in if detV <= 0 then 0
       else detV ** (1 / p)

-- | [日本語]: A-efficiency: @trace((XᵀX/n)⁻¹)@ の逆数。 trace が小さいほど
--   係数ごとの推定精度が高いことを意味する。
--   [English]: A-efficiency: reciprocal of @trace((XᵀX/n)⁻¹)@. A smaller
--   trace means higher per-coefficient estimation precision.
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

-- | [日本語]: 列ごとの Variance Inflation Factor。
--
--   @VIF_j = 1 / (1 - R²_j)@。 @R²_j@ は列 @j@ を他の列に回帰したときの決定係数。
--   @VIF > 10@ は多重共線性の強い兆候。
--
--   [English]: Per-column Variance Inflation Factor.
--
--   @VIF_j = 1 / (1 - R²_j)@, where @R²_j@ is the coefficient of
--   determination from regressing column @j@ on the others.
--   @VIF > 10@ is a strong sign of multicollinearity.
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

-- | [日本語]: 工程能力サマリ。
--
--   - @capCp  = (USL − LSL) / (6 σ)@
--   - @capCpk = min((USL − μ) / (3 σ), (μ − LSL) / (3 σ))@
--
--   片側変種 (LSL 無しまたは USL 無し) では @Cpk@ の該当側だけを使い、
--   @Cp@ もその半分に fallback する (= @Cp == Cpk@)。
--
--   [English]: Process capability summary.
--
--   - @capCp  = (USL − LSL) / (6 σ)@
--   - @capCpk = min((USL − μ) / (3 σ), (μ − LSL) / (3 σ))@
--
--   For one-sided variants (no LSL or no USL) only the relevant half of
--   @Cpk@ is used; @Cp@ falls back to that half (so @Cp == Cpk@).
data Capability = Capability
  { capCp   :: !Double
  , capCpk  :: !Double
  , capMean :: !Double
  , capSd   :: !Double
  } deriving (Show, Eq)

-- | [日本語]: 明示的な @LSL@ と @USL@ による両側工程能力。
--   [English]: Two-sided process capability with explicit @LSL@ and @USL@.
processCapability
  :: Double            -- ^ [日本語]: LSL (下側規格限界) [English]: LSL (lower spec limit)
  -> Double            -- ^ [日本語]: USL (上側規格限界) [English]: USL (upper spec limit)
  -> LA.Vector Double  -- ^ [日本語]: 標本観測値。 [English]: Sample observations.
  -> Capability
processCapability lsl usl xs =
  let (mu, sd) = meanSd xs
      cp       = if sd == 0 then 0 else (usl - lsl) / (6 * sd)
      cpkUpper = if sd == 0 then 0 else (usl - mu) / (3 * sd)
      cpkLower = if sd == 0 then 0 else (mu - lsl) / (3 * sd)
      cpk      = min cpkUpper cpkLower
  in Capability cp cpk mu sd

-- | [日本語]: 片側 (上側規格のみ) 工程能力 (@USL@ のみ)。
--   [English]: One-sided upper-spec process capability (only @USL@).
processCapabilityUpper :: Double -> LA.Vector Double -> Capability
processCapabilityUpper usl xs =
  let (mu, sd) = meanSd xs
      cpk      = if sd == 0 then 0 else (usl - mu) / (3 * sd)
  in Capability cpk cpk mu sd

-- | [日本語]: 片側 (下側規格のみ) 工程能力 (@LSL@ のみ)。
--   [English]: One-sided lower-spec process capability (only @LSL@).
processCapabilityLower :: Double -> LA.Vector Double -> Capability
processCapabilityLower lsl xs =
  let (mu, sd) = meanSd xs
      cpk      = if sd == 0 then 0 else (mu - lsl) / (3 * sd)
  in Capability cpk cpk mu sd

-- | [日本語]: __Weibull 分布__ に従う特性値の工程能力。
--
--   非正規分布の場合、 6σ では裾を過小評価する。 ISO 22514 / AIAG 推奨の
--   パーセンタイル法:
--
--   > Cp  = (USL − LSL) / (P_{0.99865} − P_{0.00135})
--   > Cpk = min( (USL − median) / (P_{0.99865} − median),
--   >            (median − LSL) / (median − P_{0.00135}) )
--
--   Weibull quantile: @F⁻¹(p) = λ · (−log(1 − p))^{1/k}@
--
--   [English]: Process Capability for __Weibull-distributed__
--   characteristics.
--
--   For non-normal distributions, 6σ underestimates the tails. The
--   percentile method recommended by ISO 22514 \/ AIAG:
--
--   > Cp  = (USL − LSL) / (P_{0.99865} − P_{0.00135})
--   > Cpk = min( (USL − median) / (P_{0.99865} − median),
--   >            (median − LSL) / (median − P_{0.00135}) )
--
--   Weibull quantile: @F⁻¹(p) = λ · (−log(1 − p))^{1/k}@
processCapabilityWeibull
  :: WeibullFit
  -> Double            -- ^ [日本語]: LSL [English]: LSL
  -> Double            -- ^ [日本語]: USL [English]: USL
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

-- | [日本語]: __LogNormal 分布__ に従う特性値の工程能力。
--   引数は log-scale の μ, σ (ln X ~ Normal(μ, σ²))。
--
--   > X_p = exp(μ + σ · z_p)
--
--   [English]: Process Capability for __LogNormal-distributed__
--   characteristics. Arguments are the log-scale μ, σ
--   (ln X ~ Normal(μ, σ²)).
--
--   > X_p = exp(μ + σ · z_p)
processCapabilityLogNormal
  :: Double            -- ^ [日本語]: μ (log scale の平均) [English]: μ (log scale mean)
  -> Double            -- ^ [日本語]: σ (log scale の標準偏差) [English]: σ (log scale sd)
  -> Double            -- ^ [日本語]: LSL [English]: LSL
  -> Double            -- ^ [日本語]: USL [English]: USL
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

-- | [日本語]: __Gamma 分布__ に従う特性値の工程能力。
--   shape (= k) と scale (= θ) を引数に取る (statistics-0.16 の @gammaDistr@ と同表記)。
--   rate β = 1 / θ を使うユーザは scale = 1/β で渡す。
--
--   分位点法 (ISO 22514) で Cp / Cpk を算出:
--
--   > Cp  = (USL − LSL) / (P_{0.99865} − P_{0.00135})
--   > Cpk = min( (USL − median) / (P_{0.99865} − median),
--   >            (median − LSL) / (median − P_{0.00135}) )
--
--   [English]: Process Capability for __Gamma-distributed__
--   characteristics. Takes shape (= k) and scale (= θ) as arguments
--   (same notation as statistics-0.16's @gammaDistr@). Users working
--   with rate β = 1 / θ should pass scale = 1/β.
--
--   Cp \/ Cpk are computed via the quantile method (ISO 22514):
--
--   > Cp  = (USL − LSL) / (P_{0.99865} − P_{0.00135})
--   > Cpk = min( (USL − median) / (P_{0.99865} − median),
--   >            (median − LSL) / (median − P_{0.00135}) )
processCapabilityGamma
  :: Double            -- ^ [日本語]: shape (k > 0) [English]: shape (k > 0)
  -> Double            -- ^ [日本語]: scale (θ > 0) [English]: scale (θ > 0)
  -> Double            -- ^ [日本語]: LSL [English]: LSL
  -> Double            -- ^ [日本語]: USL [English]: USL
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

-- | [日本語]: 非正規 Cp の統一エントリ用 ADT。 spec: doe-spec v0.2 §3.13。
--   [English]: An ADT for the unified non-normal Cp entry point. spec:
--   doe-spec v0.2 §3.13.
data NonNormalFit
  = NNFWeibull   !WeibullFit       -- ^ [日本語]: Weibull MLE 結果 [English]: Weibull MLE result
  | NNFLogNormal !Double !Double    -- ^ [日本語]: log-scale μ, σ [English]: log-scale μ, σ
  | NNFGamma     !Double !Double    -- ^ [日本語]: shape, scale [English]: shape, scale
  deriving (Show)

-- | [日本語]: 非正規分布 fit の type tag で Weibull / LogNormal / Gamma を dispatch。
--   個別関数 (@processCapabilityWeibull@ 等) と等価、 ADT で取り回したいケース用。
--   [English]: Dispatches to Weibull \/ LogNormal \/ Gamma via the
--   non-normal distribution fit's type tag. Equivalent to the individual
--   functions (@processCapabilityWeibull@ etc.); for cases where you want
--   to handle it as an ADT.
processCapabilityNonNormal
  :: NonNormalFit
  -> Double          -- ^ [日本語]: LSL [English]: LSL
  -> Double          -- ^ [日本語]: USL [English]: USL
  -> Capability
processCapabilityNonNormal (NNFWeibull   wf)         = processCapabilityWeibull   wf
processCapabilityNonNormal (NNFLogNormal mu sigma)   = processCapabilityLogNormal mu sigma
processCapabilityNonNormal (NNFGamma     k  scale)   = processCapabilityGamma     k  scale

-- ---------------------------------------------------------------------------
-- 多変量 Process Capability (Phase 23-d、 spec: doe-spec v0.2 §2.10 / §3.13)
-- ---------------------------------------------------------------------------

-- | [日本語]: 多変量 Process Capability の結果。
--
--   @mcMCp@ は Wang-Hubele-Lawrence (1994) 風の体積比ベース:
--
--   > MCp = (det(Σ_T) / det(Σ))^(1/(2p))
--
--   ここで Σ_T = diag(((USL_i − LSL_i) / 6)²) (= 各軸 6σ 相当の理想分散)、
--   Σ は標本共分散、 p は変数数。 単変量 Cp の自然な多変量拡張。
--
--   @mcMCpk@ は中心オフセット penalty を乗じた値:
--
--   > MCpk = MCp · max(0, 1 − sqrt(T²) / 3)
--   > T²   = (μ_data − μ_T)' Σ⁻¹ (μ_data − μ_T)
--   > μ_T  = (LSL + USL) / 2
--
--   @mcInSpecRate@ は spec box (per-variable LSL/USL) の内包率 (実測)。
--
--   [English]: The result of multivariate Process Capability.
--
--   @mcMCp@ is based on a Wang-Hubele-Lawrence (1994)-style volume ratio:
--
--   > MCp = (det(Σ_T) / det(Σ))^(1/(2p))
--
--   where Σ_T = diag(((USL_i − LSL_i) / 6)²) (the ideal variance
--   corresponding to 6σ on each axis), Σ is the sample covariance, and p
--   is the number of variables. A natural multivariate extension of the
--   univariate Cp.
--
--   @mcMCpk@ is the value multiplied by a centering-offset penalty:
--
--   > MCpk = MCp · max(0, 1 − sqrt(T²) / 3)
--   > T²   = (μ_data − μ_T)' Σ⁻¹ (μ_data − μ_T)
--   > μ_T  = (LSL + USL) / 2
--
--   @mcInSpecRate@ is the (empirically measured) fraction contained
--   within the spec box (per-variable LSL\/USL).
data MultivariateCapability = MultivariateCapability
  { mcNVars       :: !Int
  , mcMean        :: !(LA.Vector Double)
  , mcCov         :: !(LA.Matrix Double)
  , mcMCp         :: !Double
  , mcMCpk        :: !Double
  , mcInSpecRate  :: !Double
  } deriving (Show)

-- | [日本語]: 多変量 Cp 計算。 入力 @data@ は n 行 × p 列の観測行列。
--   @specs@ は各変数の (LSL, USL) を __列順__ に与える。
--
--   @Left@ を返すケース:
--
--     - @specs@ の長さが列数と一致しない
--     - n < 2 (共分散が定義されない)
--     - 共分散が singular (= det ≈ 0)
--
--   [English]: Multivariate Cp calculation. Input @data@ is an n-row ×
--   p-column observation matrix. @specs@ gives each variable's (LSL, USL)
--   in __column order__.
--
--   Cases returning @Left@:
--
--     - the length of @specs@ doesn't match the column count
--     - n < 2 (covariance is undefined)
--     - the covariance is singular (= det ≈ 0)
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

-- | [日本語]: 標本平均と不偏標準偏差。
--   [English]: Sample mean and unbiased standard deviation.
meanSd :: LA.Vector Double -> (Double, Double)
meanSd xs =
  let n  = LA.size xs
      nD = fromIntegral n :: Double
      mu = LA.sumElements xs / nD
      d  = LA.cmap (subtract mu) xs
      v  = if n <= 1 then 0
                     else (d `LA.dot` d) / (nD - 1.0)
  in (mu, sqrt v)
