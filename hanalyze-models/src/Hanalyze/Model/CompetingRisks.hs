{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Hanalyze.Model.CompetingRisks
-- Description : 競合リスク生存解析 (累積発生関数 CIF 推定)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Competing-risks survival analysis.
--
-- Extends 'Hanalyze.Model.Survival' to settings with multiple, mutually
-- exclusive failure causes. Implements the non-parametric Cumulative
-- Incidence Function (CIF) estimator (Kalbfleisch & Prentice 1980):
--
-- @
--   F̂_k(t) = Σ_{t_i ≤ t}  Ŝ(t_i⁻) · (d_{k,i} / n_i)
-- @
--
-- where @Ŝ@ is the overall Kaplan-Meier survival treating *any* cause as
-- an event, @d_{k,i}@ is the number of failures from cause @k@ at time
-- @t_i@, and @n_i@ is the size of the risk set just before @t_i@.
--
-- The naïve approach of taking @1 - KM@ on cause-specific data ignores
-- competing events and biases the cumulative incidence upward; this
-- estimator is the canonical correction.
--
-- @
-- import Hanalyze.Model.CompetingRisks
--
-- let samples = [ CRSample 1.2 1, CRSample 2.5 2, CRSample 3.0 0, … ]
--     fit     = fitCompetingRisks samples
-- @
--
-- == Implemented
--
--   * 'fitCompetingRisks' (per-cause CIF on the distinct event grid)
module Hanalyze.Model.CompetingRisks
  ( CRSample (..)
  , CRFit (..)
  , fitCompetingRisks
  ) where

import qualified Numeric.LinearAlgebra as LA
import           Data.List             (sort, nub, sortBy)
import           Data.Ord              (comparing)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A single observation with cause-of-failure indicator.
-- @crCause = 0@ ↔ right-censored, @crCause ≥ 1@ ↔ failure from that cause.
data CRSample = CRSample
  { crTime  :: !Double
  , crCause :: !Int
  } deriving (Show, Eq)

-- | Fitted competing-risks estimator: cumulative incidence per cause,
-- evaluated on the distinct event times (causes 1, …, K combined).
data CRFit = CRFit
  { crfCauses          :: ![Int]                       -- ^ Cause labels (sorted).
  , crfTimes           :: !(LA.Vector Double)          -- ^ Distinct event times.
  , crfCIF             :: ![(Int, LA.Vector Double)]   -- ^ Per-cause CIF values
                                                       --   on @crfTimes@.
  , crfOverallSurvival :: !(LA.Vector Double)          -- ^ Overall KM survival
                                                       --   on @crfTimes@.
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Fitting
-- ---------------------------------------------------------------------------

-- | Estimate the cumulative incidence function for each observed cause.
-- Inputs need not be sorted; ties at the same time are handled jointly.
fitCompetingRisks :: [CRSample] -> CRFit
fitCompetingRisks samples =
  let sorted     = sortBy (comparing crTime) samples
      causes     = sort (nub [ c | CRSample _ c <- sorted, c > 0 ])
      eventTimes = sort (nub [ crTime s | s <- sorted, crCause s > 0 ])
      -- Number at risk just before time t: #{s | crTime s >= t}.
      atRisk t   = length [ s | s <- sorted, crTime s >= t ]
      atTime t   = [ s | s <- sorted, crTime s == t ]
      -- Per-event-time row: (S(t⁻) before update, n at risk, total d, per-cause d)
      step !sPrev t =
        let here       = atTime t
            events     = [ c | CRSample _ c <- here, c > 0 ]
            dTot       = length events
            n          = atRisk t
            sNew       = sPrev * (1 - fromIntegral dTot / fromIntegral n)
            incs       = [ ( k
                           , sPrev * fromIntegral (length [ c | c <- events, c == k ])
                                       / fromIntegral n )
                         | k <- causes ]
        in (sNew, incs)
      walk _      []       = ([], [])
      walk !sPrev (t : ts) =
        let (sNew, incs)  = step sPrev t
            (ss, incss)   = walk sNew ts
        in (sNew : ss, incs : incss)
      (survList, incList) = walk 1.0 eventTimes
      sVec     = LA.fromList survList
      -- Cumulate increments per cause across the event-time grid.
      cumulate inc = scanl1 (+) inc
      cifByCause k =
        let perTimeInc = [ snd (head [ (k', v) | (k', v) <- row, k' == k ])
                         | row <- incList ]
        in LA.fromList (cumulate perTimeInc)
      cifs = [ (k, cifByCause k) | k <- causes ]
  in CRFit
       { crfCauses          = causes
       , crfTimes           = LA.fromList eventTimes
       , crfCIF             = cifs
       , crfOverallSurvival = sVec
       }
