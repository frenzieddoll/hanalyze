module Model.GLMM
  ( GLMMResult (..)
  , fitLMEDataFrame
  , fitGLMMDataFrame
  ) where

import DataFrame.Core (DataFrame, getNumeric, getText)
import Model.Core     (FitResult (..))
import Model.GLM      (Family (..), LinkFn (..))
import Model.LM       (multiPolyDesignMatrix)

import Data.List           (nub, sort)
import qualified Data.Map.Strict as Map
import Data.Text           (Text)
import qualified Data.Vector    as V
import qualified Numeric.LinearAlgebra as LA

-- ---------------------------------------------------------------------------
-- Result type
-- ---------------------------------------------------------------------------

-- | Fit result for a random-intercept mixed model.
-- LME  (Gaussian): y = Xβ + Zu + ε,  u_j ~ N(0,σ²_u),  ε_i ~ N(0,σ²)
-- GLMM (non-Gaussian): g(E[y|u]) = Xβ + Zu,  u_j ~ N(0,σ²_u)
data GLMMResult = GLMMResult
  { glmmFixed    :: FitResult        -- fixed effects (β, conditional fitted, residuals, R²)
  , glmmRandVar  :: Double           -- random intercept variance σ²_u
  , glmmResidVar :: Double           -- residual variance σ² (1.0 for non-Gaussian)
  , glmmBLUPs    :: V.Vector Double  -- BLUP û_j, indexed parallel to glmmGroups
  , glmmGroups   :: V.Vector Text    -- sorted unique group labels
  , glmmICC      :: Double           -- ICC (exact for Gaussian; link-scale approx otherwise)
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Group helpers (shared by LME and GLMM)
-- ---------------------------------------------------------------------------

-- | Parse grouping vector into (sorted unique labels, per-obs index, per-group sizes).
buildGroups :: V.Vector Text -> (V.Vector Text, V.Vector Int, V.Vector Int)
buildGroups gvec =
  let labels   = V.fromList . sort . nub . V.toList $ gvec
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
-- DataFrame-level API
-- ---------------------------------------------------------------------------

-- | Fit a random-intercept LME from a DataFrame (Gaussian, exact EM).
fitLMEDataFrame
  :: [(Text, Int)]   -- ^ x column specs
  -> Text            -- ^ grouping column (text/categorical)
  -> Text            -- ^ response column
  -> DataFrame
  -> Maybe GLMMResult
fitLMEDataFrame colDegs groupCol yCol df = do
  xVecs <- mapM (\(col, _) -> getNumeric col df) colDegs
  yVec  <- getNumeric yCol df
  gVec  <- getText    groupCol df
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
  -> DataFrame
  -> Maybe GLMMResult
fitGLMMDataFrame family link colDegs groupCol yCol df = do
  xVecs <- mapM (\(col, _) -> getNumeric col df) colDegs
  yVec  <- getNumeric yCol df
  gVec  <- getText    groupCol df
  let degrees              = map snd colDegs
      dm                   = multiPolyDesignMatrix (zip xVecs degrees)
      y                    = LA.fromList (V.toList yVec)
      (labels, idx, sizes) = buildGroups gVec
  return (fitGLMM family link dm y idx labels sizes)
