{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | Survival analysis.
--
-- Time-to-event analysis under right censoring. Implements:
--
--   * 'kaplanMeier' — non-parametric survival function estimator.
--   * 'nelsonAalen' — non-parametric cumulative hazard estimator.
--   * 'logRankTest' — compare survival between groups.
--   * 'coxPH' — Cox proportional hazards regression.
--
-- == Convention
--
-- A "survival" sample is @(time, event)@ where @time@ is duration and
-- @event ∈ {0, 1}@: @1@ = event observed (death, failure, etc.),
-- @0@ = censored (still alive at study end / dropout). All functions
-- accept the convention via @SurvSample@ records.
module Hanalyze.Model.Survival
  ( -- * Common types
    SurvSample (..)
  , Event (..)
    -- * Non-parametric estimators
  , KMResult (..)
  , kaplanMeier
  , NAResult (..)
  , nelsonAalen
    -- * Hypothesis tests
  , LogRankResult (..)
  , logRankTest
    -- * Cox proportional hazards
  , CoxFit (..)
  , coxPH
  , coxBaselineHazard
  ) where

import qualified Numeric.LinearAlgebra            as LA
import qualified Statistics.Distribution          as SD
import qualified Statistics.Distribution.ChiSquared as ChiSq
import qualified Data.Vector                      as V
import qualified Data.Vector.Unboxed              as VU
import qualified Data.Vector.Storable             as VS
import           Data.List                        (sort, sortBy, group)
import           Data.Ord                         (comparing)

-- ---------------------------------------------------------------------------
-- Common types
-- ---------------------------------------------------------------------------

-- | Event indicator.
data Event = Censored | Observed deriving (Show, Eq, Ord)

-- | A single observation: @(time, event)@.
data SurvSample = SurvSample
  { ssTime  :: !Double
  , ssEvent :: !Event
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Kaplan-Meier
-- ---------------------------------------------------------------------------

-- | Kaplan-Meier survival function estimator.
data KMResult = KMResult
  { kmrTimes      :: ![Double]   -- ^ Distinct event times.
  , kmrSurvival   :: ![Double]   -- ^ Ŝ(t) at each event time.
  , kmrAtRisk     :: ![Int]      -- ^ Number at risk just before t_i.
  , kmrEvents     :: ![Int]      -- ^ Number of events at t_i.
  , kmrCensored   :: ![Int]      -- ^ Number censored at t_i.
  } deriving (Show)

-- | Compute the Kaplan-Meier estimator.
--
-- @Ŝ(t_i) = Π_{j ≤ i} (1 − d_j / n_j)@ where @d_j@ is events at @t_j@
-- and @n_j@ is the number at risk just before @t_j@.
--
-- B9c: rewritten with a single sorted-vector pass + linear run-length
-- grouping (no @[s | s <- ss, ssTime s == t]@ filter for each time,
-- which was @O(n × distinct_times)@). On the n=2000 bench this drops
-- KM from ~33 ms to a few ms.
kaplanMeier :: [SurvSample] -> KMResult
kaplanMeier samples =
  let !sorted = sortBy (comparing ssTime) samples
      !n0     = length sorted
      groups  = runLengthGroups sorted
      go _    [] = ([], [], [], [], [])
      go !nAt ((t, dj, cj) : rest) =
        let !sFactor = if nAt > 0
                         then 1 - fromIntegral dj / fromIntegral nAt
                         else 1
            (ts, ss, ns, ds, cs) = go (nAt - dj - cj) rest
            !sNew = case ss of
                      []      -> sFactor
                      (s : _) -> s * sFactor
        in (t : ts, sNew : ss, nAt : ns, dj : ds, cj : cs)
      (ts, ss, ns, ds, cs) = go n0 groups
  in KMResult ts ss ns ds cs

-- | Walk a list pre-sorted by 'ssTime' and return per-distinct-time
-- @(time, num_events, num_censored)@ tuples.
runLengthGroups :: [SurvSample] -> [(Double, Int, Int)]
runLengthGroups []     = []
runLengthGroups (x:xs) = go (ssTime x) (countOf x) xs
  where
    countOf s = case ssEvent s of
                  Observed -> (1 :: Int, 0 :: Int)
                  Censored -> (0, 1)
    go !t (!d, !c) [] = [(t, d, c)]
    go !t (!d, !c) (s:rest)
      | ssTime s == t =
          let (di, ci) = countOf s
          in go t (d + di, c + ci) rest
      | otherwise =
          let (di, ci) = countOf s
          in (t, d, c) : go (ssTime s) (di, ci) rest

-- | Backwards-compatible export of the old @groupByTime@ API. Builds
-- on the new run-length walk for performance.
groupByTime :: [SurvSample] -> [(Double, [SurvSample], [SurvSample])]
groupByTime samples =
  let !sorted = sortBy (comparing ssTime) samples
      walk []     = []
      walk (s:rest) = collect (ssTime s) [s] rest
      collect t acc [] = [emit t acc]
      collect t acc (x:xs)
        | ssTime x == t = collect t (x:acc) xs
        | otherwise     = emit t acc : collect (ssTime x) [x] xs
      emit t bucket =
        let (evs, cns) = splitByEvent bucket
        in (t, evs, cns)
      splitByEvent = foldr step ([], [])
        where step s (es, cs) = case ssEvent s of
                Observed -> (s : es, cs)
                Censored -> (es,    s : cs)
  in walk sorted

-- ---------------------------------------------------------------------------
-- Nelson-Aalen
-- ---------------------------------------------------------------------------

-- | Nelson-Aalen cumulative hazard estimator.
data NAResult = NAResult
  { narTimes      :: ![Double]
  , narCumHazard  :: ![Double]   -- ^ Ĥ(t) = Σ_j d_j / n_j.
  , narAtRisk     :: ![Int]
  , narEvents     :: ![Int]
  } deriving (Show)

-- | Compute the Nelson-Aalen estimator.
nelsonAalen :: [SurvSample] -> NAResult
nelsonAalen samples =
  let km = kaplanMeier samples
      ts = kmrTimes km
      ns = kmrAtRisk km
      ds = kmrEvents km
      hazardIncrements = [fromIntegral d / fromIntegral n | (n, d) <- zip ns ds]
      cumH = scanl1 (+) hazardIncrements
  in NAResult ts cumH ns ds

-- ---------------------------------------------------------------------------
-- Log-rank test
-- ---------------------------------------------------------------------------

-- | Log-rank test result.
data LogRankResult = LogRankResult
  { lrChi2    :: !Double
  , lrDf      :: !Int
  , lrPValue  :: !Double
  , lrGroupSizes :: ![Int]
  } deriving (Show)

-- | Log-rank test for comparing survival across @k@ groups.
--
-- Tests @H_0: S_1(t) = S_2(t) = ⋯ = S_k(t)@ for all @t@. Asymptotic
-- chi-square approximation with @k − 1@ degrees of freedom.
logRankTest :: [[SurvSample]] -> LogRankResult
logRankTest groups =
  let k = length groups
      ns = map length groups
      -- Pool all samples with group labels.
      labelled = concat
        [ [(g, s) | s <- ss] | (g, ss) <- zip [0 :: Int ..] groups ]
      sorted = sortBy (comparing (ssTime . snd)) labelled
      times = map head (group (map (ssTime . snd) sorted))
      -- For each time t_j, compute observed events O_{ij} per group i
      -- and expected events E_{ij} = (n_{ij} / n_j) × d_j, where
      -- n_{ij} = at risk in group i, n_j = total at risk, d_j = total events.
      go _      _    [] acc = acc
      go nAtRiskBy nAtRiskTotal (t : tRest) acc =
        let -- Events / censored at this time, by group.
            atTime = [s | s <- sorted, ssTime (snd s) == t]
            eventsByGrp = [ length [() | (g, s) <- atTime,
                                          g == i, ssEvent s == Observed]
                          | i <- [0 .. k - 1] ]
            censoredByGrp = [ length [() | (g, s) <- atTime,
                                            g == i, ssEvent s == Censored]
                            | i <- [0 .. k - 1] ]
            dTotal = sum eventsByGrp
            cTotal = sum censoredByGrp
            -- Expected events per group at this time.
            expected = [ if nAtRiskTotal > 0
                           then fromIntegral nij * fromIntegral dTotal
                                / fromIntegral nAtRiskTotal
                           else 0
                       | nij <- nAtRiskBy ]
            -- Variance contribution to each group's (O - E):
            -- v_{ij} = n_{ij}(n_j - n_{ij}) d_j (n_j - d_j) / (n_j² (n_j - 1))
            varContrib =
              if nAtRiskTotal > 1 && dTotal > 0
                then [ let nij = fromIntegral nij_i :: Double
                           nj  = fromIntegral nAtRiskTotal :: Double
                           dj  = fromIntegral dTotal :: Double
                       in nij * (nj - nij) * dj * (nj - dj)
                          / (nj * nj * (nj - 1))
                     | nij_i <- nAtRiskBy ]
                else replicate k 0
            (oeAcc, varAcc) = acc
            oeNew = zipWith3 (\o e prev -> prev + (fromIntegral o - e))
                             eventsByGrp expected oeAcc
            varNew = zipWith (+) varAcc varContrib
            -- Update at-risk counts (subtract events + censored).
            nAtRiskBy' = zipWith3 (\nrij ej cj -> nrij - ej - cj)
                                  nAtRiskBy eventsByGrp censoredByGrp
        in go nAtRiskBy' (nAtRiskTotal - dTotal - cTotal) tRest (oeNew, varNew)
      (oeFinal, varFinal) = go ns (sum ns) times
                                  (replicate k 0, replicate k 0)
      -- Test statistic: (O - E)² / Var summed (approx for k=2);
      -- for general k, use first (k-1) components.
      chi2 =
        if k == 2
          then case (oeFinal, varFinal) of
                 ([o1, _], [v1, _]) | v1 > 0 -> o1 * o1 / v1
                 _ -> 0
          else
            -- General case: sum of squared standardised (O - E).
            sum [ if v > 0 then o * o / v else 0
                | (o, v) <- zip oeFinal varFinal ]
      df = k - 1
      pVal = SD.complCumulative (ChiSq.chiSquared df) chi2
  in LogRankResult
       { lrChi2    = chi2
       , lrDf      = df
       , lrPValue  = pVal
       , lrGroupSizes = ns
       }

-- ---------------------------------------------------------------------------
-- Cox proportional hazards
-- ---------------------------------------------------------------------------

-- | Cox PH model fit.
data CoxFit = CoxFit
  { coxBeta    :: !(LA.Vector Double)   -- ^ Coefficients.
  , coxSE      :: !(LA.Vector Double)   -- ^ Standard errors.
  , coxLogLik  :: !Double                -- ^ Log partial likelihood.
  , coxIters   :: !Int                   -- ^ Newton iterations.
  } deriving (Show)

-- | Fit Cox proportional hazards by maximising the partial likelihood
-- via Newton-Raphson.
--
-- Partial likelihood (ties handled by Breslow approximation):
--
-- @L(β) = Π_i exp(β·x_i) / Σ_{j ∈ R(t_i)} exp(β·x_j)@
--
-- where @R(t_i)@ is the risk set at time @t_i@.
coxPH
  :: [LA.Vector Double]   -- ^ Covariates per sample.
  -> [SurvSample]         -- ^ Times and events.
  -> CoxFit
--
-- B9c: list operations (@scanr1@, @!!@, list comprehensions over
-- 'LA.Vector') replaced with @VS@/@V@-vector reverse cumulative sums
-- and a precomputed boxed 'V.Vector' of risk-set rows. The score and
-- gradient now run in @O(n p)@ per call (no per-index list traversal).
-- Hessian remains numerical for now (algorithmic Hessian is a future
-- improvement) but each finite-difference call is now cheap.
coxPH xs samples =
  let !n = length xs
      !p = if n == 0 then 0 else LA.size (head xs)
      !indexed       = zip xs samples
      !sortedByTime  = sortBy (comparing (ssTime . snd)) indexed
      -- Event indices as an unboxed Vector for fast iteration.
      !eventIdxsV    = VU.fromList
        [ i | (i, (_, s)) <- zip [0 :: Int ..] sortedByTime
            , ssEvent s == Observed ]
      !xsArr  = LA.fromRows (map fst sortedByTime)
      !xsRows = V.fromList (LA.toRows xsArr)        -- O(1) indexing

      -- Score vector at β: X β. Storable for VS.scanr1.
      scoresV beta = LA.flatten (xsArr LA.<> LA.asColumn beta) :: VS.Vector Double

      -- Reverse cumulative sum on Storable: out[i] = Σ_{j≥i} v[j].
      revCumSum :: VS.Vector Double -> VS.Vector Double
      revCumSum = VS.fromList . scanr1 (+) . VS.toList
      -- (Acceptable: VS.toList -> scanr1 -> VS.fromList is O(n) and
      -- runs once per gradAndHess; the dominant cost is the BLAS GEMV
      -- and per-row work below.)

      -- log-partial-likelihood at β.
      logLik beta =
        let scs    = scoresV beta
            !expS  = VS.map exp scs
            !cumE  = revCumSum expS
            walk acc k
              | k >= VU.length eventIdxsV = acc
              | otherwise =
                  let !i = VU.unsafeIndex eventIdxsV k
                      !s = VS.unsafeIndex scs i
                      !c = VS.unsafeIndex cumE i
                  in walk (acc + s - log c) (k + 1)
        in walk (0 :: Double) 0

      -- Gradient of log partial likelihood w.r.t. β.
      gradAt beta =
        let scs   = scoresV beta
            !expS = VS.map exp scs
            !cumE = revCumSum expS
            -- Weighted X: rows scaled by exp(score). Then row-wise
            -- reverse cumulative sum (per column) gives Σ_{j≥i} e_j x_j.
            !weightedRows = V.zipWith
              (\x e -> LA.scale e x) xsRows
              (V.fromList (VS.toList expS))
            -- Reverse cumulative sum of vectors:
            !cumWeighted = revCumSumVecV (LA.konst 0 p) weightedRows
            walk acc k
              | k >= VU.length eventIdxsV = acc
              | otherwise =
                  let !i  = VU.unsafeIndex eventIdxsV k
                      !ri = xsRows V.! i
                      !ci = VS.unsafeIndex cumE i
                      !wi = cumWeighted V.! i
                      !contrib = ri - LA.scale (1 / ci) wi
                  in walk (acc + contrib) (k + 1)
        in walk (LA.konst 0 p) 0

      maxIter = 25 :: Int
      tol     = 1e-6
      h       = 1e-5

      -- Numerical Hessian column i (central difference of grad).
      hessCol betaList i =
        let bp = LA.fromList [if k == i then v + h else v
                             | (k, v) <- zip [0::Int ..] betaList]
            bm = LA.fromList [if k == i then v - h else v
                             | (k, v) <- zip [0::Int ..] betaList]
        in LA.scale (1 / (2 * h)) (gradAt bp - gradAt bm)

      step beta =
        let !g       = gradAt beta
            !bL      = LA.toList beta
            !hessian = LA.fromRows [hessCol bL i | i <- [0 .. p - 1]]
            !negH    = LA.scale (-1) hessian
            !delta   = negH LA.<\> g
            !betaNew = beta + delta
            !converged = LA.norm_2 delta < tol
        in (betaNew, converged)

      loop !i beta
        | i >= maxIter = (beta, i)
        | otherwise =
            let (beta', conv) = step beta
            in if conv then (beta', i + 1)
                       else loop (i + 1) beta'

      (!betaFinal, !iters) = loop 0 (LA.konst 0 p)

      -- Final Hessian for SEs.
      !bFL       = LA.toList betaFinal
      !hessFinal = LA.fromRows [hessCol bFL i | i <- [0 .. p - 1]]
      !negHFinal = LA.scale (-1) hessFinal
      !seVec     = case maybeInverse negHFinal of
                     Just inv -> LA.cmap sqrt (LA.takeDiag inv)
                     Nothing  -> LA.konst (1/0) p
  in CoxFit
       { coxBeta   = betaFinal
       , coxSE     = seVec
       , coxLogLik = logLik betaFinal
       , coxIters  = iters
       }

-- | Reverse cumulative sum over a boxed Vector of 'LA.Vector Double':
-- @out[i] = Σ_{j≥i} v[j]@. Returns a Vector of the same length.
-- Uses 'scanr' once (O(n p)) — total cost dominated by BLAS-bound
-- vector additions.
revCumSumVecV :: LA.Vector Double
              -> V.Vector (LA.Vector Double)
              -> V.Vector (LA.Vector Double)
revCumSumVecV zeroV vs =
  -- scanr produces length n+1 with a trailing zero seed; drop it.
  let !suf = scanr (+) zeroV (V.toList vs)
  in V.fromList (init suf)

-- | Baseline cumulative hazard (Breslow estimator).
coxBaselineHazard
  :: CoxFit
  -> [LA.Vector Double]
  -> [SurvSample]
  -> [(Double, Double)]         -- ^ @(t_i, Ĥ_0(t_i))@.
coxBaselineHazard fit xs samples =
  let beta    = coxBeta fit
      indexed = zip xs samples
      sortedByTime = sortBy (comparing (ssTime . snd)) indexed
      times = sort (map (ssTime . snd) sortedByTime)
      uniqueTs = map head (group times)
      atRiskAt t =
        [ x | (x, s) <- sortedByTime, ssTime s >= t ]
      eventsAt t =
        length [() | (_, s) <- sortedByTime, ssTime s == t,
                                              ssEvent s == Observed]
      hazardIncrements t =
        let denom = sum [ exp (LA.dot beta x) | x <- atRiskAt t ]
            d     = eventsAt t
        in if denom > 0 then fromIntegral d / denom else 0
      hi = map hazardIncrements uniqueTs
      cumH = scanl1 (+) hi
  in zip uniqueTs cumH

-- | Try to compute the inverse of a matrix; returns Nothing if singular.
maybeInverse :: LA.Matrix Double -> Maybe (LA.Matrix Double)
maybeInverse m =
  case LA.rank m of
    r | r == LA.rows m -> Just (LA.inv m)
      | otherwise       -> Nothing
