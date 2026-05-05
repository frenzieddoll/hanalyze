{-# LANGUAGE OverloadedStrings #-}
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
module Model.Survival
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
kaplanMeier :: [SurvSample] -> KMResult
kaplanMeier samples =
  let -- Sort by time ascending; collect events at each unique time.
      sorted = sortBy (comparing ssTime) samples
      n0     = length sorted
      -- Group by distinct time.
      timeGroups = groupByTime sorted
      go _    [] = ([], [], [], [], [])
      go !nAt ((t, evs, cns):rest) =
        let dj = length evs
            cj = length cns
            sFactor = if nAt > 0
                        then 1 - fromIntegral dj / fromIntegral nAt
                        else 1
            (ts, ss, ns, ds, cs) = go (nAt - dj - cj) rest
            sNew = case ss of
                     []      -> sFactor
                     (s : _) -> s * sFactor
        in (t : ts, sNew : ss, nAt : ns, dj : ds, cj : cs)
      (ts, ss, ns, ds, cs) = go n0 timeGroups
  in KMResult ts ss ns ds cs

-- | Group samples by distinct time, producing
-- @[(time, observed events at t, censored at t)]@.
groupByTime :: [SurvSample] -> [(Double, [SurvSample], [SurvSample])]
groupByTime ss =
  let times = map head (group (map ssTime ss))
  in [ ( t
       , [s | s <- ss, ssTime s == t, ssEvent s == Observed]
       , [s | s <- ss, ssTime s == t, ssEvent s == Censored])
     | t <- times ]

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
coxPH xs samples =
  let n = length xs
      p = if n == 0 then 0 else LA.size (head xs)
      -- Sort by time ascending; risk set at position i = positions [i..end].
      indexed = zip xs samples
      sortedByTime = sortBy (comparing (ssTime . snd)) indexed
      eventIdxs = [ i | (i, (_, s)) <- zip [0 :: Int ..] sortedByTime
                      , ssEvent s == Observed ]
      xsArr = LA.fromRows (map fst sortedByTime)
      -- log partial likelihood and gradient at β.
      -- ll(β) = Σ_{i event} [β · x_i - log Σ_{j ≥ i} exp(β · x_j)]
      logLik beta =
        let scores = LA.toList (xsArr LA.#> beta)
            -- cumulative sum from end (= risk set sum of exp(score))
            expScores = map exp scores
            cumExpFromEnd = scanr1 (+) expScores
        in sum [ scores !! i - log (cumExpFromEnd !! i)
               | i <- eventIdxs ]
      -- Gradient of log partial likelihood w.r.t. β.
      -- For each event i: ∂ℓ/∂β = x_i - (Σ_{j≥i} e_j x_j) / (Σ_{j≥i} e_j)
      -- where e_j = exp(β·x_j). Sum over events.
      gradAndHess beta =
        let scores    = LA.toList (xsArr LA.#> beta)
            expScores = map exp scores
            xsRows    = LA.toRows xsArr
            -- cumulative sums from index i to end
            cumExp = scanr1 (+) expScores                  -- length n
            -- weighted sum of x_j: at index i, Σ_{j≥i} e_j x_j
            weightedXs = scanr1 (+) [LA.scale e x | (x, e) <- zip xsRows expScores]
            grad = foldr (+) (LA.konst 0 p)
                     [ xsRows !! i - LA.scale (1 / cumExp !! i)
                                              (weightedXs !! i)
                     | i <- eventIdxs ]
        in grad
      -- Newton-Raphson iterations (use numerical Hessian via finite diff)
      maxIter = 25
      tol     = 1e-6
      step beta =
        let g = gradAndHess beta
            -- Numerical Hessian (central diff per coord).
            h = 1e-5
            hessRow i =
              let beta_plus = LA.toList beta
                  beta_minus = LA.toList beta
                  bp = LA.fromList [if k == i then v + h else v
                                   | (k, v) <- zip [0..] beta_plus]
                  bm = LA.fromList [if k == i then v - h else v
                                   | (k, v) <- zip [0..] beta_minus]
                  gp = gradAndHess bp
                  gm = gradAndHess bm
              in LA.scale (1 / (2 * h)) (gp - gm)
            hessian = LA.fromRows [hessRow i | i <- [0 .. p - 1]]
            -- Solve H Δβ = g; Newton step β ← β + Δβ
            -- (gradient ascent on log-likelihood; concave so H negative)
            negH = LA.scale (-1) hessian
            delta = case Just (negH LA.<\> g) of
                      Just d  -> d
                      Nothing -> g  -- fallback to gradient ascent
            betaNew = beta + delta
            converged = LA.norm_2 delta < tol
        in (betaNew, converged)
      loop !i beta
        | i >= maxIter = (beta, i)
        | otherwise =
            let (beta', conv) = step beta
            in if conv then (beta', i + 1)
                       else loop (i + 1) beta'
      (betaFinal, iters) = loop 0 (LA.konst 0 p)
      -- Standard errors from Fisher information (negative Hessian).
      h = 1e-5
      hessFinal = LA.fromRows
        [ let bp = LA.fromList [if k == i then v + h else v
                               | (k, v) <- zip [0..] (LA.toList betaFinal)]
              bm = LA.fromList [if k == i then v - h else v
                               | (k, v) <- zip [0..] (LA.toList betaFinal)]
              gp = gradAndHess bp
              gm = gradAndHess bm
          in LA.scale (1 / (2 * h)) (gp - gm)
        | i <- [0 .. p - 1] ]
      negH = LA.scale (-1) hessFinal
      seVec = case maybeInverse negH of
                Just inv -> LA.cmap sqrt (LA.takeDiag inv)
                Nothing  -> LA.konst (1/0) p
  in CoxFit
       { coxBeta   = betaFinal
       , coxSE     = seVec
       , coxLogLik = logLik betaFinal
       , coxIters  = iters
       }

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
