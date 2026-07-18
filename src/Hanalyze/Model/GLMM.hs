-- |
-- Module      : Hanalyze.Model.GLMM
-- Description : 線形/一般化線形混合効果モデル (random intercept/slope)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Linear and generalized linear mixed-effects models.
--
-- 'fitLME' / 'fitGLMM' fit a __random-intercept__ mixed model: a single
-- scalar random effect per group (variance @σ²_u@, scalar BLUP @û_j@).
-- 'fitLME' is Gaussian via exact EM; 'fitGLMM' is non-Gaussian via Laplace.
--
-- 'fitLMEGeneral' / 'fitGLMMGeneral' (Phase 48) generalise to __vector
-- random effects__ (random intercept + slopes): a per-group design block
-- @Z_j@ with an @r×r@ covariance matrix @G@ and a vector BLUP @b̂_j@. With
-- @r = 1@ (intercept only) they reduce exactly to 'fitLME' / 'fitGLMM'.
--
-- The multi-output variants ('fitLMEMulti', 'fitGLMMMulti') run the
-- random-intercept algorithm independently per response column.
module Hanalyze.Model.GLMM
  ( GLMMResult (..)
  , fitLME
  , fitGLMM
  , fitLMEDataFrame
  , fitGLMMDataFrame
    -- * General random effects (intercept + slope; Phase 48)
  , GLMMResultRE (..)
  , fitLMEGeneral
  , fitGLMMGeneral
    -- * Multi-output (per-column EM/Laplace; Family/Link shared)
  , GLMMResultMulti (..)
  , fitLMEMulti
  , fitGLMMMulti
    -- * Standard errors (request/100)
  , glmmFixedSE
  , glmmBLUPSE
    -- * Group helper (shared with Formula.Mixed)
  , buildGroups
  ) where

import qualified DataFrame.Internal.DataFrame as DXD
import Hanalyze.DataIO.Convert (getDoubleVec, getTextVec)
import Hanalyze.Model.Core     (FitResult (..))
import Hanalyze.Model.GLM      (Family (..), LinkFn (..))
import Hanalyze.Model.LM       (multiPolyDesignMatrix)

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import Data.Text           (Text)
import qualified Data.Vector    as V
import qualified Numeric.LinearAlgebra as LA

-- ---------------------------------------------------------------------------
-- Result type
-- ---------------------------------------------------------------------------

-- | Fit result for a random-intercept mixed model.
--
--   * LME (Gaussian):     @y = Xβ + Zu + ε@, @u_j ~ N(0, σ²_u)@,
--     @ε_i ~ N(0, σ²)@.
--   * GLMM (non-Gaussian): @g(E[y|u]) = Xβ + Zu@, @u_j ~ N(0, σ²_u)@.
data GLMMResult = GLMMResult
  { glmmFixed    :: FitResult        -- ^ Fixed-effect fit (β, conditional
                                     --   fitted values, residuals, R²).
  , glmmRandVar  :: Double           -- ^ Random-intercept variance @σ²_u@.
  , glmmResidVar :: Double           -- ^ Residual variance @σ²@ (1.0 for non-Gaussian families).
  , glmmBLUPs    :: V.Vector Double  -- ^ Best linear unbiased predictions
                                     --   @û_j@, aligned with 'glmmGroups'.
  , glmmGroups   :: V.Vector Text    -- ^ Sorted unique group labels.
  , glmmICC      :: Double           -- ^ Intraclass correlation (exact
                                     --   for Gaussian; link-scale
                                     --   approximation otherwise).
  } deriving (Show)

-- | Fit result for a __general__ mixed model with vector random effects
--   (random intercept + slopes), Phase 48.
--
--   * LME (Gaussian):     @y_j = X_j β + Z_j b_j + ε_j@, @b_j ~ N(0, G)@,
--     @ε_i ~ N(0, σ²)@, where @Z_j@ is the per-group random-effect design
--     block (@n_j × r@) and @G@ is the @r×r@ random-effect covariance.
--   * GLMM (non-Gaussian): @g(E[y|b]) = X_j β + Z_j b_j@, @b_j ~ N(0, G)@.
--
--   With @r = 1@ and an intercept-only @Z@ this reduces exactly to the
--   scalar 'GLMMResult' (@reRandCov = [[σ²_u]]@, @reBLUPs@ a single column).
data GLMMResultRE = GLMMResultRE
  { reFixed    :: FitResult        -- ^ Fixed-effect fit (β, conditional
                                   --   fitted values, residuals, R²).
  , reRandCov  :: LA.Matrix Double -- ^ Random-effect covariance @G@ (@r×r@).
  , reResidVar :: Double           -- ^ Residual variance @σ²@ (1.0 for
                                   --   non-Gaussian families).
  , reBLUPs    :: LA.Matrix Double -- ^ BLUPs @b̂@ as a @q×r@ matrix (row j =
                                   --   group j, aligned with 'reGroups').
  , reGroups   :: V.Vector Text    -- ^ Sorted unique group labels (length q).
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Group helpers (shared by LME and GLMM)
-- ---------------------------------------------------------------------------

-- | Parse grouping vector into (sorted unique labels, per-obs index, per-group sizes).
buildGroups :: V.Vector Text -> (V.Vector Text, V.Vector Int, V.Vector Int)
buildGroups gvec =
  -- Phase 11b (2026-05-14): Set-based dedup + sort, O(n log n) instead of
  -- the O(n²) 'nub'. Important for grouping vectors with thousands of IDs.
  let labels   = V.fromList . Set.toAscList . Set.fromList . V.toList $ gvec
      q        = V.length labels
      labelMap = Map.fromList (zip (V.toList labels) ([0..] :: [Int]))
      idx      = V.map (\g -> Map.findWithDefault 0 g labelMap) gvec
      szMap    = Map.fromListWith (+) (V.toList (V.map (\j -> (j, 1 :: Int)) idx))
      sizes    = V.fromList [ Map.findWithDefault 0 j szMap | j <- [0..q-1] ]
  in (labels, idx, sizes)

-- | Group sums: (Zᵀv)_j = Σ_{i in group j} v_i
zGroupSums :: V.Vector Int -> V.Vector Double -> Int -> V.Vector Double
zGroupSums idx v q =
  let smap = Map.fromListWith (+) (V.toList (V.zipWith (,) idx v))
  in V.fromList [ Map.findWithDefault 0.0 j smap | j <- [0..q-1] ]

-- | Scatter random effects to observations: (Zu)_i = u_{g(i)}
zuScatter :: V.Vector Int -> V.Vector Double -> V.Vector Double
zuScatter idx u = V.map (u V.!) idx

-- ---------------------------------------------------------------------------
-- EM algorithm for LME (Gaussian, exact)
-- ---------------------------------------------------------------------------

maxEmIter :: Int
maxEmIter = 500

emTol :: Double
emTol = 1e-8

-- | Fit a random-intercept LME via EM (ML).
-- The E-step exploits the diagonal structure of the precision matrix for random intercepts:
--   P_jj = 1 / (1/σ²_u + n_j/σ²)
-- The M-step updates β by OLS on partial residuals; σ²_u and σ² analytically.
fitLME
  :: LA.Matrix Double  -- X (design matrix, must include intercept column)
  -> LA.Vector Double  -- y
  -> V.Vector Int      -- per-observation group index (0-based)
  -> V.Vector Text     -- sorted group labels (length q)
  -> V.Vector Int      -- per-group observation counts (length q)
  -> GLMMResult
fitLME x y idx labels sizes =
  let n = LA.rows x
      q = V.length labels

      beta0 = LA.flatten (x LA.<\> LA.asColumn y)
      yMean = LA.sumElements y / fromIntegral n
      yDev  = y - LA.konst yMean n
      ssTot = yDev `LA.dot` yDev
      varY  = ssTot / fromIntegral n
      su2_0 = varY / 2
      s2_0  = varY / 2

      emStep (beta, su2, s2) =
        let pDiag  = V.fromList [ 1.0 / (1.0/su2 + fromIntegral (sizes V.! j) / s2)
                                | j <- [0..q-1] ]
            r0     = V.fromList . LA.toList $ y - x LA.#> beta
            ztR    = zGroupSums idx r0 q
            utilde = V.zipWith (\pj sj -> pj * sj / s2) pDiag ztR
            zuU    = LA.fromList . V.toList $ zuScatter idx utilde
            betaNew = LA.flatten (x LA.<\> LA.asColumn (y - zuU))
            trP    = V.sum pDiag
            su2New = max 1e-8 $ (trP + V.sum (V.map (\u -> u*u) utilde)) / fromIntegral q
            r1     = y - x LA.#> betaNew - zuU
            trZPZt = V.sum (V.zipWith (\nj pj -> fromIntegral nj * pj) sizes pDiag)
            s2New  = max 1e-8 $ (r1 `LA.dot` r1 + trZPZt) / fromIntegral n
        in (betaNew, su2New, s2New)

      converge 0 st            = st
      converge k st@(b, su, s) =
        let st'@(b', su', s') = emStep st
        in if    LA.norm_2 (b' - b) < emTol
              && abs (su' - su)      < emTol
              && abs (s'  - s)       < emTol
           then st'
           else converge (k-1) st'

      (betaF, su2F, s2F) = converge maxEmIter (beta0, su2_0, s2_0)

      pDiagF = V.fromList [ 1.0 / (1.0/su2F + fromIntegral (sizes V.! j) / s2F)
                          | j <- [0..q-1] ]
      r0F    = V.fromList . LA.toList $ y - x LA.#> betaF
      ztRF   = zGroupSums idx r0F q
      uF     = V.zipWith (\pj sj -> pj * sj / s2F) pDiagF ztRF
      zuF    = LA.fromList . V.toList $ zuScatter idx uF
      fittedV = x LA.#> betaF + zuF
      residV  = y - fittedV
      ssResF  = residV `LA.dot` residV
      r2      = if ssTot == 0 then 1.0 else 1.0 - ssResF / ssTot
      icc     = su2F / (su2F + s2F)
      fitRes  = FitResult (LA.asColumn betaF)
                          (LA.asColumn fittedV)
                          (LA.asColumn residV)
                          (LA.fromList [r2])

  in GLMMResult fitRes su2F s2F uF labels icc

-- ---------------------------------------------------------------------------
-- General random effects (intercept + slope): vector EM for Gaussian LME
-- ---------------------------------------------------------------------------

-- | Fit a Gaussian LME with __vector__ random effects via EM (ML), Phase 48.
--
-- Per group @j@ the model is @y_j = X_j β + Z_j b_j + ε_j@ with
-- @b_j ~ N(0, G)@ (@G@ is @r×r@) and @ε ~ N(0, σ²I)@. The @Z@ argument holds
-- the raw random-effect design columns (usually a sub-block of @X@, e.g. the
-- intercept column plus the slope column for @(1+x|g)@); rows align with @X@.
--
-- EM (Laird-Ware), each step given @(β, G, σ²)@:
--
--   * E-step (per group, @r×r@): @P_j = (G⁻¹ + Z_jᵀZ_j/σ²)⁻¹@,
--     @b̂_j = P_j Z_jᵀ r_j / σ²@ with @r_j = y_j − X_j β@.
--   * M-step: @β = (XᵀX)⁻¹Xᵀ(y − Zb̂)@,
--     @G = (1/q) Σ_j (P_j + b̂_j b̂_jᵀ)@,
--     @σ² = (1/n)[Σ‖y_j − X_j β − Z_j b̂_j‖² + Σ tr(Z_jᵀZ_j P_j)]@.
--
-- With @r = 1@ and an intercept-only @Z@ this reproduces 'fitLME' exactly.
-- All linear algebra is hmatrix-native (no list-based fallbacks).
--
-- TODO (Phase 48 follow-up): this is ML; a REML variant would correct the
-- variance estimates for the fixed-effect degrees of freedom.
fitLMEGeneral
  :: LA.Matrix Double  -- ^ X (fixed-effect design, must include intercept)
  -> LA.Matrix Double  -- ^ Z (random-effect design, @n × r@; rows align with X)
  -> LA.Vector Double  -- ^ y
  -> V.Vector Int      -- ^ per-observation group index (0-based)
  -> V.Vector Text     -- ^ sorted group labels (length q)
  -> GLMMResultRE
fitLMEGeneral x z y idx labels =
  let n       = LA.rows x
      q       = V.length labels
      r       = LA.cols z
      members = precompMembers idx q n
      zRows   = V.fromList (LA.toRows z)         -- O(1) per-row access for scatter

      -- per-group X_j, Z_j, y_j (and Z_jᵀZ_j) precomputed once
      groupBlk j =
        let mem = members V.! j
            xj  = x LA.? mem
            zj  = z LA.? mem
            yj  = LA.fromList [ y `LA.atIndex` i | i <- mem ]
            ztz = LA.tr zj LA.<> zj
        in (xj, zj, yj, ztz)
      blocks = V.fromList [ groupBlk j | j <- [0..q-1] ]

      -- initial values: OLS fixed fit, residual variance split intercept/resid
      beta0 = LA.flatten (x LA.<\> LA.asColumn y)
      yMean = LA.sumElements y / fromIntegral n
      yDev  = y - LA.konst yMean n
      ssTot = yDev `LA.dot` yDev
      varY  = ssTot / fromIntegral n
      g0    = LA.scale (varY / 2) (LA.ident r)
      s20   = varY / 2

      -- scatter (Zb̂)_i = Z_i · b̂_{g(i)}
      scatterZb bhats =
        LA.fromList [ (zRows V.! i) `LA.dot` (bhats V.! (idx V.! i)) | i <- [0..n-1] ]

      emStep (beta, gMat, s2) =
        let gInv  = LA.inv gMat
            -- E-step: posterior cov P_j and mean b̂_j per group
            pbs   = V.map (\(xj, zj, yj, ztz) ->
                      let rj  = yj - xj LA.#> beta
                          pj  = LA.inv (gInv + LA.scale (1/s2) ztz)
                          bj  = LA.scale (1/s2) (pj LA.#> (LA.tr zj LA.#> rj))
                      in (pj, bj, ztz)) blocks
            bhats = V.map (\(_, bj, _) -> bj) pbs
            zb    = scatterZb bhats
            -- M-step β
            betaN = LA.flatten (x LA.<\> LA.asColumn (y - zb))
            -- M-step G = (1/q) Σ (P_j + b̂_j b̂_jᵀ)
            gAcc  = V.foldl' (\acc (pj, bj, _) -> acc + pj + LA.outer bj bj)
                             (LA.konst 0 (r, r)) pbs
            gN    = LA.scale (1 / fromIntegral q) gAcc
            -- M-step σ²: conditional residuals (using updated β) + trace term
            zbN   = scatterZb bhats
            r1    = y - x LA.#> betaN - zbN
            trc   = V.sum (V.map (\(pj, _, ztz) -> LA.sumElements (ztz * pj)) pbs)
            s2N   = max 1e-10 $ (r1 `LA.dot` r1 + trc) / fromIntegral n
        in (betaN, gN, s2N)

      converge 0 st            = st
      converge k st@(b, gM, s) =
        let st'@(b', gM', s') = emStep st
        in if    LA.norm_2 (b' - b)            < emTol
              && LA.norm_2 (LA.flatten (gM' - gM)) < emTol
              && abs (s' - s)                   < emTol
           then st'
           else converge (k-1) st'

      (betaF, gF, s2F) = converge maxEmIter (beta0, g0, s20)

      -- final BLUPs and conditional fit
      gInvF  = LA.inv gF
      bhatsF = V.map (\(xj, zj, yj, ztz) ->
                 let rj = yj - xj LA.#> betaF
                     pj = LA.inv (gInvF + LA.scale (1/s2F) ztz)
                 in LA.scale (1/s2F) (pj LA.#> (LA.tr zj LA.#> rj))) blocks
      zbF     = scatterZb bhatsF
      fittedV = x LA.#> betaF + zbF
      residV  = y - fittedV
      ssResF  = residV `LA.dot` residV
      r2      = if ssTot == 0 then 1.0 else 1.0 - ssResF / ssTot
      fitRes  = FitResult (LA.asColumn betaF)
                          (LA.asColumn fittedV)
                          (LA.asColumn residV)
                          (LA.fromList [r2])
      blupMat = LA.fromRows (V.toList bhatsF)   -- q×r

  in GLMMResultRE fitRes gF s2F blupMat labels

-- ---------------------------------------------------------------------------
-- Laplace approximation for non-Gaussian GLMM
-- ---------------------------------------------------------------------------

-- | Inverse link: μ = g⁻¹(η)
glmmInvLink :: LinkFn -> Double -> Double
glmmInvLink Identity η = η
glmmInvLink Log      η = exp (min 500 η)
glmmInvLink Logit    η = 1.0 / (1.0 + exp (-η))
glmmInvLink Sqrt     η = η * η

-- | Forward link: η = g(μ)
glmmFwdLink :: LinkFn -> Double -> Double
glmmFwdLink Identity μ = μ
glmmFwdLink Log      μ = log (max 1e-10 μ)
glmmFwdLink Logit    μ = let c = max 1e-8 (min (1-1e-8) μ) in log (c / (1 - c))
glmmFwdLink Sqrt     μ = sqrt (max 0 μ)

-- | Link derivative: g'(μ)
glmmLinkDeriv :: LinkFn -> Double -> Double
glmmLinkDeriv Identity _ = 1.0
glmmLinkDeriv Log      μ = 1.0 / max 1e-10 μ
glmmLinkDeriv Logit    μ = let c = max 1e-8 (min (1-1e-8) μ) in 1.0 / (c * (1 - c))
glmmLinkDeriv Sqrt     μ = 0.5 / sqrt (max 1e-10 μ)

-- | GLM variance function: V(μ)
glmmVarFn :: Family -> Double -> Double
glmmVarFn Gaussian _ = 1.0
glmmVarFn Binomial μ = let c = max 1e-8 (min (1-1e-8) μ) in c * (1 - c)
glmmVarFn Poisson  μ = max 1e-8 μ

-- | Clamp μ to numerically safe range.
glmmClampMu :: Family -> Double -> Double
glmmClampMu Binomial = max 1e-8 . min (1 - 1e-8)
glmmClampMu Poisson  = max 1e-8
glmmClampMu Gaussian = id

-- | IRLS weight: w_i = 1 / (g'(μ)² V(μ))
glmmWeight :: Family -> LinkFn -> Double -> Double
glmmWeight family link μ =
  let d = glmmLinkDeriv link μ
  in max 1e-10 (1.0 / (d * d * glmmVarFn family μ))

-- | Score contribution: s_i = (y_i − μ_i) / (g'(μ_i) V(μ_i))
glmmScore :: Family -> LinkFn -> Double -> Double -> Double
glmmScore family link y μ =
  (y - μ) / (glmmLinkDeriv link μ * glmmVarFn family μ)

-- | ICC approximation for non-Gaussian models (on the link scale).
-- Binomial/logit: π²/3 is the variance of the standard logistic distribution.
-- Poisson/log:    1 is the log-scale residual variance (approximation).
iccApprox :: Family -> Double -> Double
iccApprox Gaussian su2 = su2 / (su2 + 1.0)        -- placeholder; LME gives exact ICC
iccApprox Binomial su2 = su2 / (su2 + pi*pi/3.0)
iccApprox Poisson  su2 = su2 / (su2 + 1.0)

-- | Precompute group member index lists (O(n) preprocessing).
precompMembers :: V.Vector Int -> Int -> Int -> V.Vector [Int]
precompMembers idx q n =
  let mmap = Map.fromListWith (++) [ (idx V.! i, [i]) | i <- [0..n-1] ]
  in V.fromList [ Map.findWithDefault [] j mmap | j <- [0..q-1] ]

maxNRIter :: Int
maxNRIter = 50

nrTol :: Double
nrTol = 1e-10

-- | Inner Newton-Raphson: find conditional mode û_j for one group.
-- Maximises Q_j(u) = Σ log p(y_i | g⁻¹(ηᵢ + u)) − u²/(2σ²_u)
-- NR step: u ← u + grad/hess  where
--   grad = Σ s_i − u/σ²_u,   hess = Σ w_i + 1/σ²_u
nrOneGroup :: Family -> LinkFn -> Double -> [Double] -> [Double] -> Double -> Double
nrOneGroup family link su2 etaFixed ys = go maxNRIter
  where
    clamp = glmmClampMu family
    gInv  = glmmInvLink link

    go 0 u = u
    go k u =
      let mus   = map (clamp . gInv . (+ u)) etaFixed
          grad  = sum (zipWith (glmmScore family link) ys mus) - u / su2
          hess  = sum (map (glmmWeight family link) mus) + 1.0 / su2
          delta = grad / hess
          u'    = u + delta
      in if abs delta < nrTol then u' else go (k-1) u'

maxGLMMIter :: Int
maxGLMMIter = 200

glmmTol :: Double
glmmTol = 1e-7

-- | One outer GLMM iteration:
--   1. NR(û)    — find conditional modes given current β and σ²_u
--   2. IRLS(β)  — one IRLS step with random effects as offset
--   3. EM(σ²_u) — Laplace-approximated posterior variance update
glmmStep
  :: Family -> LinkFn
  -> LA.Matrix Double    -- X
  -> LA.Vector Double    -- y
  -> V.Vector Int        -- per-obs group index
  -> V.Vector [Int]      -- per-group member index lists (precomputed)
  -> (LA.Vector Double, Double, V.Vector Double)
  -> (LA.Vector Double, Double, V.Vector Double)
glmmStep family link x y idx members (beta, su2, u) =
  let q     = V.length u
      clamp = glmmClampMu family
      gInv  = glmmInvLink link
      gD    = glmmLinkDeriv link

      xBeta     = x LA.#> beta
      etaFixedV = V.fromList (LA.toList xBeta)
      yV        = V.fromList (LA.toList y)

      -- 1. Inner NR: update û_j for each group j
      uNew = V.fromList
               [ nrOneGroup family link su2
                   [ etaFixedV V.! i | i <- members V.! j ]
                   [ yV        V.! i | i <- members V.! j ]
                   (u V.! j)
               | j <- [0..q-1] ]

      -- 2. IRLS step for β (offset = Zû)
      -- z_adj_i = (y_i − μ_i) g'(μ_i) + (Xβ)_i   (WLS target without offset)
      uScatter  = LA.fromList . V.toList $ zuScatter idx uNew
      etaFull   = xBeta + uScatter
      musV      = V.map (clamp . gInv) (V.fromList (LA.toList etaFull))
      wsV       = V.map (glmmWeight family link) musV
      xBetaV    = V.fromList (LA.toList xBeta)
      zAdjV     = V.zipWith3 (\yi mui xbi -> (yi - mui) * gD mui + xbi) yV musV xBetaV
      sqrtW     = LA.diag (LA.fromList . V.toList $ V.map sqrt wsV)
      zAdj      = LA.fromList (V.toList zAdjV)
      betaNew   = LA.flatten $
                    (sqrtW LA.<> x) LA.<\> LA.asColumn (sqrtW LA.#> zAdj)

      -- 3. EM-like σ²_u update using Laplace-approximated posterior variance
      -- ṽ_j = 1 / (Σ_{i∈j} w_i + 1/σ²_u)  ≈ Var(u_j | y)
      -- σ²_u_new = Σ_j (ṽ_j + û_j²) / q
      etaNew    = x LA.#> betaNew + uScatter
      musNewV   = V.map (clamp . gInv) (V.fromList (LA.toList etaNew))
      wsNewV    = V.map (glmmWeight family link) musNewV
      wSumsV    = zGroupSums idx wsNewV q
      su2New    = max 1e-8 $
                    V.sum (V.zipWith (\ws uj -> 1.0/(ws + 1.0/su2) + uj*uj) wSumsV uNew)
                    / fromIntegral q

  in (betaNew, su2New, uNew)

-- | Fit a non-Gaussian GLMM (random intercept) via Laplace approximation.
-- For Gaussian/Identity, prefer fitLMEDataFrame which uses exact EM.
fitGLMM
  :: Family -> LinkFn
  -> LA.Matrix Double
  -> LA.Vector Double
  -> V.Vector Int      -- per-obs group index
  -> V.Vector Text     -- sorted group labels
  -> V.Vector Int      -- per-group sizes (unused; kept for API symmetry with fitLME)
  -> GLMMResult
fitGLMM family link x y idx labels _sizes =
  let n = LA.rows x
      p = LA.cols x
      q = V.length labels

      members = precompMembers idx q n

      -- Initialise: β₀ = g(ȳ_safe), rest 0; û = 0; σ²_u = half total variance
      yMean = LA.sumElements y / fromIntegral n
      ySafe = case family of
                Binomial -> max 1e-6 (min (1-1e-6) yMean)
                Poisson  -> max 1e-6 yMean
                Gaussian -> yMean
      beta0 = LA.fromList (glmmFwdLink link ySafe : replicate (p - 1) 0.0)
      u0    = V.replicate q 0.0
      yDev  = y - LA.konst yMean n
      su2_0 = max 1e-4 ((yDev `LA.dot` yDev) / fromIntegral n / 2)

      norm2V v = sqrt $ V.foldl' (\acc d -> acc + d*d) 0.0 v

      converge 0 st              = st
      converge k st@(b, su, u') =
        let st'@(b', su', u'') = glmmStep family link x y idx members st
        in if    LA.norm_2 (b' - b)             < glmmTol
              && abs (su' - su)                  < glmmTol
              && norm2V (V.zipWith (-) u'' u')   < glmmTol
           then st'
           else converge (k-1) st'

      (betaF, su2F, uF) = converge maxGLMMIter (beta0, su2_0, u0)

      -- Final conditional fitted values and statistics
      uScatterF = LA.fromList . V.toList $ zuScatter idx uF
      fittedLA  = LA.cmap (glmmClampMu family . glmmInvLink link) (x LA.#> betaF + uScatterF)
      residLA   = y - fittedLA
      ssTot     = yDev `LA.dot` yDev
      ssRes     = residLA `LA.dot` residLA
      r2        = if ssTot == 0 then 1.0 else 1.0 - ssRes / ssTot
      icc       = iccApprox family su2F
      fitRes    = FitResult (LA.asColumn betaF)
                            (LA.asColumn fittedLA)
                            (LA.asColumn residLA)
                            (LA.fromList [r2])

  in GLMMResult fitRes su2F 1.0 uF labels icc

-- ---------------------------------------------------------------------------
-- General random effects (intercept + slope): vector Laplace for GLMM
-- ---------------------------------------------------------------------------

-- | Multivariate inner Newton-Raphson: find the conditional mode @b̂_j@ of one
-- group and return @(b̂_j, P_j)@ where @P_j = (Σ_i w_i z_i z_iᵀ + G⁻¹)⁻¹@ is
-- the Laplace posterior covariance at the mode.
--
-- Maximises @Q_j(b) = Σ_i log p(y_i | g⁻¹(η_i + z_iᵀ b)) − ½ bᵀ G⁻¹ b@.
-- Newton step solves @H δ = grad@ with
-- @grad = Σ_i s_i z_i − G⁻¹ b@, @H = Σ_i w_i z_i z_iᵀ + G⁻¹@.
nrOneGroupVec
  :: Family -> LinkFn
  -> LA.Matrix Double    -- ^ G⁻¹ (r×r)
  -> [LA.Vector Double]  -- ^ z_i rows for this group (each length r)
  -> [Double]            -- ^ etaFixed_i = (X_i β)
  -> [Double]            -- ^ y_i
  -> LA.Vector Double    -- ^ initial b (length r)
  -> (LA.Vector Double, LA.Matrix Double)
nrOneGroupVec family link gInv zs etaFixed ys = go maxNRIter
  where
    clamp = glmmClampMu family
    gInvL = glmmInvLink link
    r     = LA.rows gInv

    -- negative Hessian (= posterior precision) at b: Σ_i w_i z_i z_iᵀ + G⁻¹
    hessAt b =
      let etas = zipWith (\z ef -> ef + z `LA.dot` b) zs etaFixed
          mus  = map (clamp . gInvL) etas
          ws   = map (glmmWeight family link) mus
      in foldr (\(w, z) acc -> acc + LA.scale w (LA.outer z z)) gInv (zip ws zs)

    go 0 b = (b, LA.inv (hessAt b))
    go k b =
      let etas  = zipWith (\z ef -> ef + z `LA.dot` b) zs etaFixed
          mus   = map (clamp . gInvL) etas
          ss    = zipWith (glmmScore family link) ys mus
          ws    = map (glmmWeight family link) mus
          grad  = foldr (\(s, z) acc -> acc + LA.scale s z) (LA.konst 0 r) (zip ss zs)
                    - (gInv LA.#> b)
          hess  = foldr (\(w, z) acc -> acc + LA.scale w (LA.outer z z)) gInv (zip ws zs)
          delta = LA.flatten (hess LA.<\> LA.asColumn grad)
          b'    = b + delta
      in if LA.norm_2 delta < nrTol then (b', LA.inv hess) else go (k-1) b'

-- | Fit a non-Gaussian GLMM with __vector__ random effects via Laplace
-- approximation (Phase 48). Per group @j@: @g(E[y|b]) = X_j β + Z_j b_j@,
-- @b_j ~ N(0, G)@ (@G@ is @r×r@). Outer loop: multivariate NR for the modes
-- @b̂_j@ (with Laplace posterior cov @P_j@), one IRLS step for @β@ (random
-- effects as offset), and an EM update @G = (1/q) Σ_j (P_j + b̂_j b̂_jᵀ)@.
--
-- With @r = 1@ and an intercept-only @Z@ this matches 'fitGLMM'. Supports the
-- same families/links as 'fitGLMM' (Binomial/Logit, Poisson/Log).
fitGLMMGeneral
  :: Family -> LinkFn
  -> LA.Matrix Double  -- ^ X (fixed-effect design, must include intercept)
  -> LA.Matrix Double  -- ^ Z (random-effect design, @n × r@; rows align with X)
  -> LA.Vector Double  -- ^ y
  -> V.Vector Int      -- ^ per-observation group index (0-based)
  -> V.Vector Text     -- ^ sorted group labels (length q)
  -> GLMMResultRE
fitGLMMGeneral family link x z y idx labels =
  let n       = LA.rows x
      p       = LA.cols x
      q       = V.length labels
      r       = LA.cols z
      members = precompMembers idx q n
      zRows   = V.fromList (LA.toRows z)
      yV      = V.fromList (LA.toList y)

      groupZs = V.fromList [ [ zRows V.! i | i <- members V.! j ] | j <- [0..q-1] ]
      groupYs = V.fromList [ [ yV    V.! i | i <- members V.! j ] | j <- [0..q-1] ]

      clamp = glmmClampMu family
      gInvL = glmmInvLink link
      gD    = glmmLinkDeriv link

      yMean = LA.sumElements y / fromIntegral n
      ySafe = case family of
                Binomial -> max 1e-6 (min (1-1e-6) yMean)
                Poisson  -> max 1e-6 yMean
                Gaussian -> yMean
      beta0 = LA.fromList (glmmFwdLink link ySafe : replicate (p - 1) 0.0)
      b0    = V.replicate q (LA.konst 0 r)
      yDev  = y - LA.konst yMean n
      su2_0 = max 1e-4 ((yDev `LA.dot` yDev) / fromIntegral n / 2)
      g0    = LA.scale su2_0 (LA.ident r)

      scatterZb bs =
        LA.fromList [ (zRows V.! i) `LA.dot` (bs V.! (idx V.! i)) | i <- [0..n-1] ]

      step (beta, gMat, bs) =
        let gInv      = LA.inv gMat
            xBeta     = x LA.#> beta
            etaFixedV = V.fromList (LA.toList xBeta)
            results   = V.fromList
                          [ nrOneGroupVec family link gInv (groupZs V.! j)
                              [ etaFixedV V.! i | i <- members V.! j ]
                              (groupYs V.! j)
                              (bs V.! j)
                          | j <- [0..q-1] ]
            bsNew = V.map fst results
            pjs   = V.map snd results
            -- IRLS β with random offset Zb̂ held fixed
            zb     = scatterZb bsNew
            etaF   = xBeta + zb
            musV   = V.map (clamp . gInvL) (V.fromList (LA.toList etaF))
            wsV    = V.map (glmmWeight family link) musV
            xBetaV = V.fromList (LA.toList xBeta)
            zAdjV  = V.zipWith3 (\yi mui xbi -> (yi - mui) * gD mui + xbi) yV musV xBetaV
            sqrtW  = LA.diag (LA.fromList . V.toList $ V.map sqrt wsV)
            zAdj   = LA.fromList (V.toList zAdjV)
            betaN  = LA.flatten $ (sqrtW LA.<> x) LA.<\> LA.asColumn (sqrtW LA.#> zAdj)
            -- EM update G = (1/q) Σ (P_j + b̂_j b̂_jᵀ)
            gAcc   = V.foldl' (\acc (pj, bj) -> acc + pj + LA.outer bj bj)
                              (LA.konst 0 (r, r)) (V.zip pjs bsNew)
            gN     = LA.scale (1 / fromIntegral q) gAcc
        in (betaN, gN, bsNew)

      bsDiff a b = V.sum (V.zipWith (\u v -> LA.norm_2 (u - v)) a b)
      converge 0 st                = st
      converge k st@(beta, gM, bs) =
        let st'@(beta', gM', bs') = step st
        in if    LA.norm_2 (beta' - beta)                  < glmmTol
              && LA.norm_2 (LA.flatten (gM' - gM))          < glmmTol
              && bsDiff bs' bs                              < glmmTol
           then st'
           else converge (k-1) st'

      (betaF, gF, bsF) = converge maxGLMMIter (beta0, g0, b0)

      zbF     = scatterZb bsF
      fittedV = LA.cmap (clamp . gInvL) (x LA.#> betaF + zbF)
      residV  = y - fittedV
      ssTot   = yDev `LA.dot` yDev
      ssRes   = residV `LA.dot` residV
      r2      = if ssTot == 0 then 1.0 else 1.0 - ssRes / ssTot
      fitRes  = FitResult (LA.asColumn betaF)
                          (LA.asColumn fittedV)
                          (LA.asColumn residV)
                          (LA.fromList [r2])
      blupMat = LA.fromRows (V.toList bsF)

  in GLMMResultRE fitRes gF 1.0 blupMat labels

-- ---------------------------------------------------------------------------
-- DataFrame-level API
-- ---------------------------------------------------------------------------

-- | Fit a random-intercept LME from a DataFrame (Gaussian, exact EM).
fitLMEDataFrame
  :: [(Text, Int)]   -- ^ x column specs
  -> Text            -- ^ grouping column (text/categorical)
  -> Text            -- ^ response column
  -> DXD.DataFrame
  -> Maybe GLMMResult
fitLMEDataFrame colDegs groupCol yCol df = do
  xVecs <- mapM (\(col, _) -> getDoubleVec col df) colDegs
  yVec  <- getDoubleVec yCol df
  gVec  <- getTextVec   groupCol df
  let degrees              = map snd colDegs
      dm                   = multiPolyDesignMatrix (zip xVecs degrees)
      y                    = LA.fromList (V.toList yVec)
      (labels, idx, sizes) = buildGroups gVec
  return (fitLME dm y idx labels sizes)

-- | Fit a non-Gaussian GLMM from a DataFrame (Laplace approximation).
-- Supports Binomial/Logit and Poisson/Log; for Gaussian/Identity prefer fitLMEDataFrame.
fitGLMMDataFrame
  :: Family -> LinkFn
  -> [(Text, Int)]   -- ^ x column specs
  -> Text            -- ^ grouping column (text/categorical)
  -> Text            -- ^ response column
  -> DXD.DataFrame
  -> Maybe GLMMResult
fitGLMMDataFrame family link colDegs groupCol yCol df = do
  xVecs <- mapM (\(col, _) -> getDoubleVec col df) colDegs
  yVec  <- getDoubleVec yCol df
  gVec  <- getTextVec   groupCol df
  let degrees              = map snd colDegs
      dm                   = multiPolyDesignMatrix (zip xVecs degrees)
      y                    = LA.fromList (V.toList yVec)
      (labels, idx, sizes) = buildGroups gVec
  return (fitGLMM family link dm y idx labels sizes)

-- ---------------------------------------------------------------------------
-- Multi-output GLMM (per-column EM/Laplace; grouping shared across columns)
-- ---------------------------------------------------------------------------

-- | Multi-output GLMM/LME fit result.
data GLMMResultMulti = GLMMResultMulti
  { glmmFits  :: [GLMMResult]    -- ^ Per-column fit results.
  , glmmGrpsM :: V.Vector Text   -- ^ Sorted group labels (shared across columns).
  } deriving (Show)

-- | Multi-output Gaussian LME. @Y@ has shape @n × q@; 'fitLME' is run
-- independently on each column.
fitLMEMulti :: LA.Matrix Double -> LA.Matrix Double
            -> V.Vector Int -> V.Vector Text -> V.Vector Int
            -> GLMMResultMulti
fitLMEMulti x y idx labels sizes =
  let q     = LA.cols y
      yCol j = LA.flatten (y LA.¿ [j])
      fits  = [fitLME x (yCol j) idx labels sizes | j <- [0 .. q - 1]]
  in GLMMResultMulti fits labels

-- | Multi-output non-Gaussian GLMM. @Y@ has shape @n × q@; 'fitGLMM' is
-- run independently on each column.
fitGLMMMulti :: Family -> LinkFn
             -> LA.Matrix Double -> LA.Matrix Double
             -> V.Vector Int -> V.Vector Text -> V.Vector Int
             -> GLMMResultMulti
fitGLMMMulti family link x y idx labels sizes =
  let q     = LA.cols y
      yCol j = LA.flatten (y LA.¿ [j])
      fits  = [fitGLMM family link x (yCol j) idx labels sizes
              | j <- [0 .. q - 1]]
  in GLMMResultMulti fits labels

-- ---------------------------------------------------------------------------
-- Standard errors (request/100)
-- ---------------------------------------------------------------------------

-- | Standard errors of the fixed-effect coefficients @β@.
--
-- For LME (Gaussian, Identity link) this is /exact/: it inverts
-- @Xᵀ V⁻¹ X@ where @V = σ² I + σ²_u Z Zᵀ@ is the marginal covariance
-- under the random-intercept model. The block structure of @V@ is
-- exploited so this stays @O(n p² + q p²)@ instead of forming a
-- dense @n × n@ matrix:
--
-- > Xᵀ V⁻¹ X = (1/σ²) Xᵀ X − Σ_j (α_j / σ²) s_j s_jᵀ
-- > α_j     = σ²_u / (σ² + n_j σ²_u)
-- > s_j     = Σ_{i ∈ group j} x_i           (column sums of X within group j)
--
-- For non-Gaussian families this returns a Gaussian-approximation
-- (treats @σ² = 1@) — adequate for /relative/ ordering of coefficients
-- but absolute values are off; matching lme4-style non-Gaussian SE
-- requires the converged IRLS weights which are not currently exposed
-- by 'fitGLMM'.
glmmFixedSE
  :: LA.Matrix Double      -- ^ Design matrix @X@ (n × p, intercept inclusive).
  -> V.Vector Int          -- ^ Group index per observation (length n; same as
                           --   the @idx@ produced by @buildGroups@).
  -> GLMMResult
  -> LA.Vector Double      -- ^ Length @p@; coefficient SEs in column order.
glmmFixedSE x groupIdx res =
  let n       = LA.rows x
      p       = LA.cols x
      sig2u   = glmmRandVar  res
      sig2RAW = glmmResidVar res
      sig2    = if sig2RAW > 0 then sig2RAW else 1.0   -- non-Gaussian fallback
      q       = V.length (glmmGroups res)

      -- per-group n_j
      nj :: Map.Map Int Int
      nj = V.foldl' (\acc j -> Map.insertWith (+) j 1 acc) Map.empty groupIdx

      -- per-group column sum s_j = Σ_{i ∈ group j} x_i  (length p)
      groupSum :: Map.Map Int (LA.Vector Double)
      groupSum =
        V.foldl' (\acc i ->
                    let j = groupIdx V.! i
                        xi = LA.flatten (x LA.? [i])
                    in Map.insertWith (+) j xi acc)
                 Map.empty
                 (V.enumFromN 0 n)

      xtxFull = LA.tr x LA.<> x

      correction :: LA.Matrix Double
      correction =
        Map.foldlWithKey'
          (\acc j s ->
              let nj_j = Map.findWithDefault 0 j nj
                  alpha = sig2u / (sig2 + fromIntegral nj_j * sig2u)
              in acc + LA.scale alpha (LA.outer s s))
          (LA.konst 0 (p, p))
          groupSum

      xvtinvX = LA.scale (1 / sig2) (xtxFull - correction)
      cov     = LA.inv xvtinvX
      _ = q  -- kept to make q's role explicit in the docstring
  in LA.fromList [ sqrt (max 0 (LA.atIndex cov (i, i))) | i <- [0 .. p - 1] ]

-- | Posterior standard errors of the BLUPs @û_j@ under the
-- random-intercept model:
--
-- > Var(u_j | data) = (1 / σ²_u + n_j / σ²)⁻¹
--
-- (For non-Gaussian families this uses @σ² = 1@; same caveat as
-- 'glmmFixedSE'.) Length matches 'glmmGroups'.
glmmBLUPSE :: V.Vector Int -> GLMMResult -> V.Vector Double
glmmBLUPSE groupIdx res =
  let q       = V.length (glmmGroups res)
      sig2u   = glmmRandVar  res
      sig2RAW = glmmResidVar res
      sig2    = if sig2RAW > 0 then sig2RAW else 1.0
      njMap   = V.foldl' (\acc j -> Map.insertWith (+) j 1 acc)
                         Map.empty groupIdx
      ng j    = Map.findWithDefault 0 j njMap
  in V.generate q (\j ->
       let nDouble = fromIntegral (ng j) :: Double
           varInv  = 1.0 / sig2u + nDouble / sig2
       in sqrt (1.0 / varInv))
