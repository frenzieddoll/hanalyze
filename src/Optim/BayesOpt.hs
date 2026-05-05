{-# LANGUAGE OverloadedStrings #-}
-- | Bayesian Optimization loop.
--
-- Single-objective procedure:
--
--   1. Evaluate initial points (Latin hypercube or random).
--   2. Fit a Gaussian process to the observations.
--   3. Maximize an acquisition function to choose the next @x@.
--   4. Evaluate @x@ and append to the observed sequence.
--   5. Repeat steps 2-4 for @T@ iterations.
module Optim.BayesOpt
  ( BayesOptConfig (..)
  , defaultBayesOptConfig
  , bayesOpt
  , bayesOptND
  , bayesOptScalarMO
  , bayesOptMOWithNSGA
    -- * GP HP optimization helpers
  , optimizeGPMVRestart
  ) where

import Control.Exception (SomeException, try, evaluate)
import Control.Monad (forM, replicateM)
import Data.List (minimumBy, maximumBy)
import Data.Ord (comparing)
import System.IO.Unsafe (unsafePerformIO)
import System.Random.MWC (GenIO, uniform)

import Model.GP (Kernel (..), GPModel (..), GPResult (..), GPParams (..),
                 fitGP, optimizeGP, initParamsFromData,
                 GPResultMV (..), fitGPMV, optimizeGPMV,
                 logMarginalLikelihoodMV,
                 buildKernelMatrixMV, noiseKernelMV)
import qualified Stat.Cholesky as Chol
import Optim.Acquisition (ei, ucb, pi_, parEGO)
import Optim.NSGA       (NSGAConfig (..), defaultNSGAConfig,
                         Solution (..), nsga2)
import Optim.Common     (Bounds)
import qualified Optim.LineSearch as LS
import qualified Optim.LBFGS      as LBFGS
import qualified Optim.Common     as OC
import qualified Numeric.LinearAlgebra as LA
import qualified Stat.QuasiRandom      as QR
import qualified Stat.Standardize      as Std
import Statistics.Distribution        (cumulative, density)
import Statistics.Distribution.Normal (standard)

-- | Bayesian Optimization configuration.
data BayesOptConfig = BayesOptConfig
  { boIterations :: Int        -- ^ Evaluation budget (excluding initial points).
  , boInitPoints :: Int        -- ^ Number of initial sample points.
  , boKernel     :: Kernel     -- ^ GP kernel.
  , boUCBBeta    :: Double     -- ^ @β@ for UCB.
  , boGridSize   :: Int        -- ^ Inner-optimization grid density (1D).
  } deriving (Show)

-- | Default configuration: 30 iterations, 5 initial points,
-- **Matérn 5/2 kernel**, @β = 2.0@ for UCB, grid size 200 for 1D
-- inner optimization.
--
-- Matérn 5/2 is the recommended default for general-purpose BO
-- (matches scikit-optimize's defaults). RBF is too smooth for many
-- real-world objective surfaces; Matérn captures the @C²@ regularity
-- typical of engineering / black-box functions and is what the BO
-- literature converged on.
defaultBayesOptConfig :: BayesOptConfig
defaultBayesOptConfig = BayesOptConfig
  { boIterations = 30
  , boInitPoints = 5
  , boKernel     = Matern52
  , boUCBBeta    = 2.0
  , boGridSize   = 200
  }

-- | Single-objective Bayesian Optimization (1D simplified entry point).
--
-- Returns @(observations, best)@: the full @(x, y)@ history and the best
-- @(x*, y*)@.
bayesOpt :: BayesOptConfig
         -> (Double -> IO Double)   -- ^ Objective (1D, minimized).
         -> (Double, Double)        -- ^ Search bounds.
         -> GenIO
         -> IO ([(Double, Double)], (Double, Double))
bayesOpt cfg f (lo, hi) gen = do
  -- 初期点 (uniform random, 簡易)
  initX <- replicateM (boInitPoints cfg) (do
              u <- uniform gen :: IO Double
              return (lo + u * (hi - lo)))
  initY <- mapM f initX
  let history0 = zip initX initY

  -- BO ループ
  -- 内側 acquisition 最大化は **Brent 法** (1D 単峰超線形収束)。
  -- 旧 grid (boGridSize 点) は seeding として併用、Brent の bracket を作る。
  let loop t hist
        | t == 0 = return hist
        | otherwise = do
            let xs = map fst hist
                ys = map snd hist
                yBest = minimum ys
                p0 = initParamsFromData xs ys
                pOpt = optimizeGP (boKernel cfg) xs ys p0
                model = GPModel (boKernel cfg) pOpt

                -- 1 点での負 EI (Brent は最小化、引数は [Double] で受ける)
                -- Cholesky / SVD 失敗時はペナルティ +1e30 を返す。
                -- gpMean / gpUpper は遅延フィールドなので evaluate で強制してから返す。
                negEI [x] = unsafePerformIO $ do
                  let computed = do
                        let res = fitGP model xs ys [x]
                            mu  = head (gpMean res)
                            sg  = (head (gpUpper res) - mu) / 2
                        _ <- evaluate mu
                        _ <- evaluate sg
                        pure (negate (ei yBest 0.01 (mu, sg)))
                  r <- try computed :: IO (Either SomeException Double)
                  case r of
                    Left _  -> pure 1e30
                    Right v -> pure v
                negEI _   = error "negEI: 1D"

                -- 粗グリッドで bracket を作る
                gridN = max 16 (boGridSize cfg `div` 4)
                grid  = [lo + fromIntegral i * (hi - lo)
                              / fromIntegral (gridN - 1)
                        | i <- [0 .. gridN - 1]]
                gridV = [(x, negEI [x]) | x <- grid]
                bestG = minimumBy (comparing snd) gridV
                bestX = fst bestG
                idxBest = case [i | (i, (gx, _)) <- zip [0::Int ..] gridV, gx == bestX] of
                            (k:_) -> k; [] -> 0
                ax = fst (gridV !! max 0 (idxBest - 1))
                bx = fst (gridV !! min (gridN - 1) (idxBest + 1))
                -- Brent で局所最大 (= 負の最小)
                bRes = LS.brent (LS.defaultBrentConfig { LS.bcMaxIter = 80
                                                       , LS.bcTol    = 1e-7 })
                                negEI (min ax bx) (max ax bx)
                xNext = head (OC.orBest bRes)

            yNext <- f xNext
            loop (t - 1) (hist ++ [(xNext, yNext)])

  finalHist <- loop (boIterations cfg) history0
  let bestPair = head [pair | pair@(_, y) <- finalHist
                            , y == minimum (map snd finalHist)]
  return (finalHist, bestPair)

-- ---------------------------------------------------------------------------
-- GP HP optimization with multiple random restarts
-- ---------------------------------------------------------------------------

-- | Optimize a GP's hyperparameters with multiple random restarts and
-- pick the best (highest marginal likelihood). One restart corresponds
-- to a single 'optimizeGPMV' call from a perturbed initial point.
--
-- Critical for BO performance: the marginal-likelihood surface is
-- multi-modal, so a single fixed init is not robust. scikit-optimize
-- defaults to @n_restarts_optimizer = 0@ (= 1 fit) but its kernel has
-- the prior baked in; for our wider search we use 5 restarts.
optimizeGPMVRestart
  :: Int                       -- ^ Number of restarts.
  -> Kernel
  -> LA.Matrix Double          -- ^ Training X (n × p).
  -> LA.Vector Double          -- ^ Training y (length n).
  -> GenIO
  -> IO GPParams
optimizeGPMVRestart n kern x y gen = do
  let p0base = initParamsFromData (concat (LA.toLists x)) (LA.toList y)
  -- generate n random initial points: log-spaced perturbation of p0base
  -- to cover several orders of magnitude.
  let scaleVar = sqrt . max 1e-6
  inits <- forM [1 .. n] $ \_ -> do
    u1 <- uniform gen :: IO Double
    u2 <- uniform gen :: IO Double
    u3 <- uniform gen :: IO Double
    -- log-uniform multipliers in [0.1, 10]
    let m1 = exp ((u1 - 0.5) * 2 * log 10)
        m2 = exp ((u2 - 0.5) * 2 * log 10)
        m3 = exp ((u3 - 0.5) * 2 * log 10)
    pure $ p0base
      { gpLengthScale = max 1e-3 (gpLengthScale p0base * m1)
      , gpSignalVar   = max 1e-6 (scaleVar (gpSignalVar p0base) * m2)
      , gpNoiseVar    = max 1e-6 (gpNoiseVar p0base * m3)
      }
  let runOne p0 = do
        let pOpt = optimizeGPMV kern x y p0
            ll   = logMarginalLikelihoodMV x y kern pOpt
        pure (pOpt, ll)
  results <- mapM runOne inits
  let (best, _) = head [ r | r@(_, ll) <- results
                           , ll == maximum (map snd results) ]
  pure best

-- | N-dimensional single-objective Bayesian Optimization.
-- 内側 acquisition 最大化を **L-BFGS multi-start** で行う:
-- bounds 範囲内で nStarts 個の初期点を一様乱数で生成、各点から L-BFGS で
-- 負 EI を最小化、最良点を採用。
bayesOptND :: BayesOptConfig
           -> Int                         -- ^ multi-start 数 (典型 5-20)
           -> ([Double] -> IO Double)     -- ^ 目的関数 (N 次元、最小化)
           -> Bounds                      -- ^ 各次元 (lo, hi)
           -> GenIO
           -> IO ([([Double], Double)], ([Double], Double))
bayesOptND cfg nStarts f bounds gen = do
  let dim = length bounds
      kern = boKernel cfg
      -- Initial design: low-discrepancy Halton sequence (better
      -- coverage of the box than iid uniform random for the small @n@
      -- typical of BO initial designs).
      initX = QR.haltonSequenceIn (boInitPoints cfg) bounds
      sampleX = forM bounds $ \(lo, hi) -> do
        u <- uniform gen :: IO Double
        return (lo + u * (hi - lo))
  initY <- mapM f initX
  let history0 = zip initX initY

  -- BO2: per-dim X scaling — map every dim to [0, 1] using its (lo, hi)
  -- bound. After this, a single isotropic ℓ in the GP equates to per-dim
  -- length scales = ℓ × (hi - lo) in the original space, i.e. ARD with
  -- weights tied to the box width. skopt's "transform=normalize"
  -- preprocessing achieves the same effect.
  let scaleX :: [Double] -> [Double]
      scaleX xs = [ if hi > lo then (v - lo) / (hi - lo) else v
                  | ((lo, hi), v) <- zip bounds xs ]
      unitBounds = replicate dim (0, 1)
  -- Phase B (GP-Hedge, Hoffman 2011): maintain online "gains" for
  -- {EI, LCB, PI}. Each iteration each acquisition proposes its best
  -- candidate via L-BFGS multi-start; one is selected by softmax over
  -- gains, evaluated, and gains are updated using the GP's predicted
  -- μ at every proposal (lower μ = higher reward for minimisation).
  -- This protects against any single acquisition's pathological
  -- behaviour on a given problem (e.g. EI's exploitation bias on
  -- multi-modal Branin).
  let hedgeEta = 1.0 :: Double
      pickAcq gains gen0 = do
        let m   = maximum gains
            ws  = map (\g -> exp (hedgeEta * (g - m))) gains
            tot = sum ws
            ps  = map (/ tot) ws
        u <- uniform gen0 :: IO Double
        let cum = scanl1 (+) ps
        pure (length (takeWhile (< u) cum))
  let loop t hist gains
        | t == 0 = return hist
        | otherwise = do
            let xss     = map fst hist
                ys      = map snd hist
                -- BO2: scale X to [0,1]^d for the GP only (history is
                -- still kept in raw units for f).
                xssScl  = map scaleX xss
                xMat    = LA.fromLists xssScl
                yVec0   = LA.fromList ys
                -- BO1: z-score y so HP optimization is scale-free
                -- (skopt normalize_y=True equivalent). Both GP fitting
                -- and EI run in normalized space; the next-x choice is
                -- scale-equivariant.
                stdr    = Std.fitStandardizer (LA.asColumn yVec0)
                yVec    = LA.flatten
                            (Std.applyStandardizer stdr (LA.asColumn yVec0))
                yBest   = LA.minElement yVec
                -- After BO2 scaling, X lives on [0, 1]^d. The natural ℓ
                -- grows as √d (mean pairwise distance scales that way),
                -- so start L-BFGS from ℓ = 0.25 √d to keep correlations
                -- meaningful as input dimension grows.
                --
                -- Phase A (true ARD): the per-dim ℓ_d API is implemented
                -- in 'Model.GP.GPParams.gpLengthScales' but disabled in
                -- the BO loop because with only ~30 evaluations the
                -- per-dim L-BFGS over-fits noise and underperforms
                -- isotropic on both Branin and Hartmann6. Future tuning
                -- (e.g. tighter ℓ_d prior, isotropic-warm-start) can
                -- re-enable it by setting 'gpLengthScales = Just v'.
                p0Base  = initParamsFromData (concat xssScl) (LA.toList yVec)
                ell0    = 0.25 * sqrt (fromIntegral dim)
                p0      = p0Base { gpLengthScale = ell0 }
                pOpt    = optimizeGPMV kern xMat yVec p0
                params  = pOpt
                -- BO core fix: precompute Cholesky factor (R) and
                -- α = Ky⁻¹ y ONCE per BO iteration. The negEI callback
                -- reuses them via 'predictFast' below; this replaces the
                -- old fitGPMV-per-call which factorised Ky on every
                -- L-BFGS step (O(n³) wasted per evaluation).
                kyMat   = noiseKernelMV kern params xMat
                rChol   = case Chol.cholFactor kyMat of
                            Just r  -> r
                            Nothing ->
                              -- Jitter and try again.
                              let n     = LA.rows xMat
                                  kyJ   = kyMat
                                          + LA.scale 1e-4 (LA.ident n)
                              in case Chol.cholFactor kyJ of
                                   Just r  -> r
                                   Nothing -> error "BO: chol failed"
                alpha   = LA.flatten
                            (Chol.cholSolveWithFactor rChol
                              (LA.asColumn yVec))
                sf      = gpSignalVar params

                -- Predict (μ, σ, k_star, vstar) at a single x via the
                -- cached factor. vstar = Ky⁻¹ k_star is reused for both
                -- the variance and its gradient.
                predictAt xVec =
                  let xScl    = LA.fromList (scaleX xVec)
                      xRow    = LA.asRow xScl
                      kStarV  = LA.flatten
                                 (buildKernelMatrixMV kern params xRow xMat)
                      mu      = LA.dot kStarV alpha
                      vstar   = LA.flatten
                                 (Chol.cholSolveWithFactor rChol
                                   (LA.asColumn kStarV))
                      varV    = max 0 (sf - LA.dot kStarV vstar)
                  in (mu, sqrt varV, kStarV, vstar)

                predictMuSig xVec = let (m, s, _, _) = predictAt xVec in (m, s)

                -- Phase C (BO4 analytic gradient): per-input partial
                -- derivatives of μ and σ w.r.t. x. Avoids the 2(p+1)
                -- function-call overhead of central differences inside
                -- the inner L-BFGS. Periodic kernel falls back to the
                -- numeric path (gradient unsupported).
                --
                -- diffs[i, d] = scaleX(x)_d − xMat[i, d]
                -- factor_i = ∂k_i/∂(diffs_i,d) / diffs_i,d  (kernel-specific)
                -- ∂μ/∂x_scaled_d = (factor ⊙ α)ᵀ · diffs[:, d]
                -- ∂σ/∂x_scaled_d = −(1/σ) · (factor ⊙ vstar)ᵀ · diffs[:, d]
                -- Chain back to raw x_d via 1/(hi - lo) factor (BO2).
                gradMuSig xVec =
                  let xScl    = LA.fromList (scaleX xVec)
                      diffs   = LA.fromRows
                                  [ xScl - xRow | xRow <- LA.toRows xMat ]
                      sqd     = LA.fromList
                                  [ d `LA.dot` d | d <- LA.toRows diffs ]
                      l       = gpLengthScale params
                      l2      = l * l
                      kStarV  = LA.flatten
                                  (buildKernelMatrixMV kern params
                                     (LA.asRow xScl) xMat)
                      factor  = case kern of
                                  RBF      ->
                                    LA.scale (-1 / l2) kStarV
                                  Matern52 ->
                                    let r = LA.cmap (\s ->
                                              sqrt (max 0 s) * sqrt 5 / l) sqd
                                        ef = LA.cmap exp (LA.scale (-1) r)
                                        c  = LA.scale (-5 / (3 * l2))
                                                (sf `LA.scale`
                                                  (ef * (LA.cmap (1 +) r)))
                                    in c
                                  Periodic ->
                                    LA.konst 0 (LA.size kStarV)  -- numeric fallback
                      vstar   = LA.flatten
                                  (Chol.cholSolveWithFactor rChol
                                    (LA.asColumn kStarV))
                      mu      = LA.dot kStarV alpha
                      varV    = max 0 (sf - LA.dot kStarV vstar)
                      sg      = sqrt varV
                      -- ∇μ in scaled coordinates: diffsᵀ · (α ⊙ factor)
                      gradMuS  = LA.tr diffs LA.#> (alpha * factor)
                      -- ∇σ in scaled coordinates: −(1/σ) · diffsᵀ · (vstar ⊙ factor)
                      gradSgS
                        | sg < 1e-12 = LA.konst 0 (LA.cols xMat)
                        | otherwise  = LA.scale (-1 / sg)
                                         (LA.tr diffs LA.#> (vstar * factor))
                      -- Chain back through scaleX: ∂scaledX/∂x = 1/(hi-lo)
                      invSpan = LA.fromList
                                  [ if hi > lo then 1 / (hi - lo) else 1
                                  | (lo, hi) <- bounds ]
                      gradMu  = LA.toList (gradMuS * invSpan)
                      gradSg  = LA.toList (gradSgS * invSpan)
                  in (mu, sg, gradMu, gradSg)

                -- Build (negAcq, gradNegAcq) pair for each acquisition.
                -- ∂EI/∂(μ,σ) = (-Φ(z), φ(z)) so ∇EI = -Φ(z) ∇μ + φ(z) ∇σ.
                -- ∂PI/∂(μ,σ) = (-φ(z)/σ, -z·φ(z)/σ) so
                --   ∇PI = -φ(z)/σ · ∇μ - z·φ(z)/σ · ∇σ.
                -- LCB is linear: ∇LCB = ∇μ − β ∇σ.
                wrapAcqGrad
                  :: ((Double, Double) -> Double)        -- acq value
                  -> ((Double, Double) -> (Double, Double)) -- (∂/∂μ, ∂/∂σ) of acq
                  -> ([Double] -> Double, [Double] -> [Double])
                wrapAcqGrad acqFn dAcq =
                  let fn xVec = unsafePerformIO $ do
                        r <- try (evaluate
                                   (negate (acqFn (let (m, s) = predictMuSig xVec
                                                   in (m, s)))))
                              :: IO (Either SomeException Double)
                        case r of { Left _ -> pure 1e30; Right v -> pure v }
                      gn xVec = unsafePerformIO $ do
                        r <- try (evaluate
                                   (let (mu, sg, gMu, gSg) = gradMuSig xVec
                                        (dM, dS) = dAcq (mu, sg)
                                    in [ - (dM * gm + dS * gs)
                                       | (gm, gs) <- zip gMu gSg ]))
                              :: IO (Either SomeException [Double])
                        case r of
                          Left _  -> pure (replicate (length xVec) 0)
                          Right v -> pure v
                  in (fn, gn)

                eiGrad (mu, sg)
                  | sg <= 1e-12 = (0, 0)
                  | otherwise   =
                      let z   = (yBest - mu - 0.01) / sg
                          phi = density standard z
                          cdf = cumulative standard z
                      in (-cdf, phi)
                piGrad (mu, sg)
                  | sg <= 1e-12 = (0, 0)
                  | otherwise   =
                      let z   = (yBest - mu - 0.01) / sg
                          phi = density standard z
                      in (-phi / sg, -z * phi / sg)
                lcbGrad _      = (1, -2.0)  -- ∂(μ - 2σ)/∂μ = 1, ∂/∂σ = -2

                (negEI,  gNegEI)  = wrapAcqGrad (ei yBest 0.01)         eiGrad
                (negPI,  gNegPI)  = wrapAcqGrad (pi_ yBest 0.01)        piGrad
                -- For LCB we want to minimise μ - βσ. Wrap as the value
                -- itself (acq = -LCB), so negate(acq) = LCB.
                (negLCB, gNegLCB) =
                  wrapAcqGrad (negate . ucb 2.0)
                              (\ms -> let (a, b) = lcbGrad ms in (-a, -b))
                _ = unitBounds

            -- L-BFGS multi-start: 'nStarts' Halton-spaced starts +
            -- small uniform jitter. Run for each acquisition.
            haltonStarts <- pure (QR.haltonSequenceIn nStarts bounds)
            starts <- forM haltonStarts $ \xs ->
              forM (zip bounds xs) $ \((lo, hi), v) -> do
                u <- uniform gen :: IO Double
                let span_ = hi - lo
                    jit   = (u - 0.5) * 0.05 * span_
                pure (max lo (min hi (v + jit)))
            let runMSG objFn gradFn = mapM (\x0 ->
                  LBFGS.runLBFGSWith
                    (LBFGS.defaultLBFGSConfig
                       { LBFGS.lbStop = OC.defaultStopCriteria
                                          { OC.stMaxIter = 100 } })
                    objFn gradFn x0) starts
                runMS objFn = mapM (\x0 ->
                  LBFGS.runLBFGSNumeric
                    (LBFGS.defaultLBFGSConfig
                       { LBFGS.lbStop = OC.defaultStopCriteria
                                          { OC.stMaxIter = 100 } })
                    objFn x0) starts
                pickXNext rs =
                  let best     = minimumBy (comparing OC.orValue) rs
                      xRaw     = OC.orBest best
                  in zipWith (\(lo, hi) v -> max lo (min hi v)) bounds xRaw
                -- Use analytic gradients for RBF / Matern52, fall back
                -- to numeric for Periodic.
                useAnalytic = case kern of
                                Periodic -> False
                                _        -> True
            xEI  <- pickXNext <$> if useAnalytic
                                    then runMSG negEI  gNegEI
                                    else runMS  negEI
            xLCB <- pickXNext <$> if useAnalytic
                                    then runMSG negLCB gNegLCB
                                    else runMS  negLCB
            xPI  <- pickXNext <$> if useAnalytic
                                    then runMSG negPI  gNegPI
                                    else runMS  negPI
            let candidates = [xEI, xLCB, xPI]
            -- GP-Hedge selection.
            k <- pickAcq gains gen
            let kSafe   = max 0 (min 2 k)
                xNext   = candidates !! kSafe
            yNext <- f xNext
            -- Update gains: reward = -μ at each candidate (we want low μ).
            let mus     = map (fst . predictMuSig) candidates
                gains'  = zipWith (\g m -> g - m) gains mus
            loop (t - 1) (hist ++ [(xNext, yNext)]) gains'

  finalHist <- loop (boIterations cfg) history0 [0, 0, 0]
  let bestPair = minimumBy (comparing snd) finalHist
  return (finalHist, bestPair)

-- | Multi-objective BO using **scalarization** (ParEGO-style).
-- 各反復で random 重み w で Tchebycheff scalarize し、単目的 BO の 1 ステップ
-- (L-BFGS multi-start で acquisition 最大化) を実行する。
-- NSGA 版より高速、acquisition 計算コストが軽い問題に向く。
bayesOptScalarMO :: Int                                -- iter
                 -> Int                                -- nInit
                 -> Int                                -- nStarts (multi-start)
                 -> Kernel
                 -> ([Double] -> IO [Double])
                 -> Bounds
                 -> GenIO
                 -> IO [([Double], [Double])]
bayesOptScalarMO nIter nInit nStarts kern f bounds gen = do
  initX <- replicateM nInit (forM bounds $ \(lo, hi) -> do
              u <- uniform gen :: IO Double
              return (lo + u * (hi - lo)))
  initY <- mapM f initX
  let history0 = zip initX initY

      step hist = do
        let xss   = map fst hist
            ysAll = map snd hist
            qDim  = length (head ysAll)
            xsFlat = map head xss             -- 1D 入力前提の簡易版
            ysCol j = [y !! j | y <- ysAll]
        -- random scalarization weight
        wsRaw <- replicateM qDim (uniform gen :: IO Double)
        let wSum = sum wsRaw
            ws   = map (/ wSum) wsRaw
            -- 各目的の GP fit (1D 入力)
            modelFor j =
              let trainY = ysCol j
                  p0 = initParamsFromData xsFlat trainY
                  pOpt = optimizeGP kern xsFlat trainY p0
              in GPModel kern pOpt
            models = [(modelFor j, ysCol j) | j <- [0 .. qDim - 1]]
            -- Tchebycheff: max_j w_j (μ_j - z*_j) — z*_j は最良観測
            zStars = [minimum (ysCol j) | j <- [0 .. qDim - 1]]
            scalarLcb xVec = unsafePerformIO $ do
              let xkey = head xVec
                  computeOne j = do
                    let (m, ty) = models !! j
                        r = fitGP m xsFlat ty [xkey]
                        mu = head (gpMean r)
                        sg = (head (gpUpper r) - mu) / 2
                        lcb = mu - 2.0 * sg
                    _ <- evaluate mu; _ <- evaluate sg
                    pure ((ws !! j) * (lcb - (zStars !! j)))
                  safe j = do
                    res <- try (computeOne j) :: IO (Either SomeException Double)
                    case res of { Left _ -> pure 1e30; Right v -> pure v }
              perJ <- mapM safe [0 .. qDim - 1]
              pure (maximum perJ)
        -- L-BFGS multi-start で scalarLcb 最小化
        starts <- replicateM nStarts (forM bounds $ \(lo, hi) -> do
                    u <- uniform gen :: IO Double
                    return (lo + u * (hi - lo)))
        results <- mapM (\x0 ->
          LBFGS.runLBFGSNumeric
            (LBFGS.defaultLBFGSConfig
               { LBFGS.lbStop = OC.defaultStopCriteria { OC.stMaxIter = 60 } })
            scalarLcb x0) starts
        let best = minimumBy (comparing OC.orValue) results
            xNextRaw = OC.orBest best
            xNext = zipWith (\(lo, hi) v -> max lo (min hi v)) bounds xNextRaw
        yNext <- f xNext
        return (hist ++ [(xNext, yNext)])

      loop t h
        | t == 0 = return h
        | otherwise = step h >>= loop (t - 1)

  loop nIter history0

argmax :: Ord a => [a] -> Int
argmax xs = snd (maximum (zip xs [0..]))

-- ---------------------------------------------------------------------------
-- 多目的 BO with NSGA-II (Phase V4)
-- ---------------------------------------------------------------------------

-- | Multi-objective BO using NSGA-II to optimize the acquisition function.
--
-- Internally fits a 'MultiGP' to obtain per-objective @(μ, σ)@, then
-- runs NSGA-II to find the Pareto front in @(μ_1, μ_2, ...)@ space; one
-- point from that front is chosen and evaluated.
--
-- A deliberately simple implementation; an EHVI-based variant is left
-- for future extension.
bayesOptMOWithNSGA
  :: Int                                -- ^ Number of BO iterations.
  -> Int                                -- ^ Number of initial samples.
  -> Kernel
  -> ([Double] -> IO [Double])          -- ^ Multi-objective function.
  -> Bounds
  -> GenIO
  -> IO [([Double], [Double])]          -- ^ Sequence of @(x, y)@ pairs.
bayesOptMOWithNSGA nIter nInit kern f bounds gen = do
  -- 初期点
  initX <- replicateM nInit (do
              vs <- forM bounds $ \(lo, hi) -> do
                u <- uniform gen :: IO Double
                return (lo + u * (hi - lo))
              return vs)
  initY <- mapM f initX
  let history0 = zip initX initY

  let loop t hist
        | t == 0 = return hist
        | otherwise = do
            -- 各目的に GP を fit (1D 入力前提の簡易版)
            -- 多次元入力の場合は MultiGP を別途準備
            -- ここでは bounds の最初の次元のみ使う簡易動作
            let xsFlat = map head (map fst hist)  -- 1D 入力前提
                ysAll = map snd hist
                qDim  = length (head ysAll)
                ysCol j = [y !! j | y <- ysAll]

            -- 各目的 j の GP モデルを fit
            let modelFor j =
                  let trainY = ysCol j
                      p0 = initParamsFromData xsFlat trainY
                      pOpt = optimizeGP kern xsFlat trainY p0
                  in GPModel kern pOpt

                models = [modelFor j | j <- [0 .. qDim - 1]]

                -- NSGA-II で Pareto front を探索 (acquisition surface 上)
                -- 各目的: μ - β σ (LCB) を最小化
                acqObjective xVec =
                  [ unsafePerformIO $ do
                      let computed = do
                            let trainY = ysCol j
                                m = models !! j
                                gpRes = fitGP m xsFlat trainY [head xVec]
                                mu = head (gpMean gpRes)
                                sg = (head (gpUpper gpRes) - mu) / 2
                            _ <- evaluate mu; _ <- evaluate sg
                            pure (ucbToMin mu sg)
                      r <- try computed :: IO (Either SomeException Double)
                      case r of { Left _ -> pure 1e30; Right v -> pure v }
                  | j <- [0 .. qDim - 1] ]

                ucbToMin :: Double -> Double -> Double
                ucbToMin mu sigma = mu - 2.0 * sigma   -- LCB

            -- NSGA-II で Pareto front を 1 ステップ探索
            front <- nsga2 (defaultNSGAConfig { nsgaPopSize = 30
                                             , nsgaGenerations = 30 })
                          acqObjective bounds gen

            -- front から random 選択
            idx <- uniform gen :: IO Double
            let i = floor (idx * fromIntegral (length front))
                xNext = solDecision (front !! min i (length front - 1))
            yNext <- f xNext
            loop (t - 1) (hist ++ [(xNext, yNext)])

  loop nIter history0
