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
import Data.List (minimumBy)
import Data.Ord (comparing)
import System.IO.Unsafe (unsafePerformIO)
import System.Random.MWC (GenIO, uniform)

import Model.GP (Kernel (..), GPModel (..), GPResult (..), GPParams (..),
                 fitGP, optimizeGP, initParamsFromData,
                 GPResultMV (..), fitGPMV, optimizeGPMV,
                 logMarginalLikelihoodMV)
import Optim.Acquisition (ei, ucb, parEGO)
import Optim.NSGA       (NSGAConfig (..), defaultNSGAConfig,
                         Solution (..), nsga2)
import Optim.Common     (Bounds)
import qualified Optim.LineSearch as LS
import qualified Optim.LBFGS      as LBFGS
import qualified Optim.Common     as OC
import qualified Numeric.LinearAlgebra as LA
import qualified Stat.QuasiRandom      as QR
import qualified Stat.Standardize      as Std

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

  let loop t hist
        | t == 0 = return hist
        | otherwise = do
            let xss   = map fst hist
                ys    = map snd hist
                xMat  = LA.fromLists xss
                yVec0 = LA.fromList ys
                -- BO1: z-score y so HP optimization is scale-free (skopt
                -- normalize_y=True equivalent). Both GP fitting and EI
                -- run in normalized space; the next-x choice is
                -- scale-equivariant, so unnormalising is unnecessary.
                stdr  = Std.fitStandardizer (LA.asColumn yVec0)
                yVec  = LA.flatten
                          (Std.applyStandardizer stdr (LA.asColumn yVec0))
                yBest = LA.minElement yVec
                -- BO1 補強: p0 の length scale を **bounds の平均幅**
                -- に揃える (per-dim 幅の平均)。pooled (concat xss) より
                -- 適切な spatial scale を初期値に与えられる。
                avgWidth = sum [ hi - lo | (lo, hi) <- bounds ]
                           / fromIntegral (max 1 dim)
                p0Base   = initParamsFromData (concat xss) (LA.toList yVec)
                p0       = p0Base { gpLengthScale = max 1e-3 (avgWidth / 4) }
                pOpt     = optimizeGPMV kern xMat yVec p0
                model    = GPModel kern pOpt
                negEI xVec = unsafePerformIO $ do
                  let xRow = LA.asRow (LA.fromList xVec)
                      computed = do
                        let res = fitGPMV model xMat yVec xRow
                            mu  = LA.atIndex (gpmvMean res) 0
                            vr  = max 0 (LA.atIndex (gpmvVar res) 0)
                            sg  = sqrt vr
                        _ <- evaluate mu; _ <- evaluate sg
                        pure (negate (ei yBest 0.01 (mu, sg)))
                  r <- try computed :: IO (Either SomeException Double)
                  case r of { Left _ -> pure 1e30; Right v -> pure v }
            -- L-BFGS multi-start: use 'nStarts' Halton-spaced starts
            -- so we cover the box more uniformly than nStarts
            -- iid-uniform restarts. Falls back to uniform if nStarts
            -- is large enough that Halton correlations matter.
            haltonStarts <- pure (QR.haltonSequenceIn nStarts bounds)
            -- jitter each Halton start by a small uniform perturbation
            -- to avoid having every BO iteration start the inner
            -- optimization at the same anchor points.
            starts <- forM haltonStarts $ \xs ->
              forM (zip bounds xs) $ \((lo, hi), v) -> do
                u <- uniform gen :: IO Double
                let span_ = hi - lo
                    jit   = (u - 0.5) * 0.05 * span_
                pure (max lo (min hi (v + jit)))
            results <- mapM (\x0 ->
              LBFGS.runLBFGSNumeric
                (LBFGS.defaultLBFGSConfig
                   { LBFGS.lbStop = OC.defaultStopCriteria
                                      { OC.stMaxIter = 100 } })
                negEI x0) starts
            let best = minimumBy (comparing OC.orValue) results
                xNextRaw = OC.orBest best
                xNext = zipWith (\(lo, hi) v -> max lo (min hi v)) bounds xNextRaw
            yNext <- f xNext
            loop (t - 1) (hist ++ [(xNext, yNext)])

  finalHist <- loop (boIterations cfg) history0
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
