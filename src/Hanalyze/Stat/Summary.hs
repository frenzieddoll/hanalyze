{-# LANGUAGE OverloadedStrings #-}
-- | Posterior-distribution summary statistics.
--
-- Provides 'SummaryRow' and 'posteriorSummary', mirroring the columns of
-- ArviZ's @az.summary@ (mean, sd, HDI, ESS, R-hat). Originally lived in
-- @Hanalyze.Viz.MCMC@; moved to the statistics layer to decouple it from the
-- visualization stack.
--
-- HTML rendering and console pretty-printing remain in
-- @Hanalyze.Viz.MCMC.posteriorSummaryHtml@ / @posteriorSummaryFile@ /
-- @printPosteriorSummary@.
module Hanalyze.Stat.Summary
  ( SummaryRow (..)
  , posteriorSummary
  ) where

import Data.Text (Text)
import Hanalyze.MCMC.Core (Chain, chainVals)
import Hanalyze.Stat.MCMC (ess, hdi, rhat)

-- | One row of posterior summary statistics for a single parameter.
data SummaryRow = SummaryRow
  { srName  :: Text     -- ^ Parameter name.
  , srMean  :: Double   -- ^ Posterior mean.
  , srSD    :: Double   -- ^ Posterior standard deviation.
  , srHdiLo :: Double   -- ^ Lower bound of the 94% HDI.
  , srHdiHi :: Double   -- ^ Upper bound of the 94% HDI.
  , srEssV  :: Double   -- ^ Effective sample size.
  , srRhat  :: Maybe Double  -- ^ Split-R-hat (only for multi-chain runs).
  } deriving (Show)

-- | Compute posterior summaries for the named parameters across one or
-- more chains. With a single chain @R-hat@ is 'Nothing'; with multiple
-- chains, mean / SD / HDI / ESS are computed on the pooled samples and
-- split-R-hat is computed across chains.
posteriorSummary :: [Text] -> [Chain] -> [SummaryRow]
posteriorSummary params chains =
  let multi = length chains > 1
      mkRow p =
        let perChain = map (chainVals p) chains
            allVals  = concat perChain
            n        = length allVals
            mu       = if n == 0 then 0
                       else sum allVals / fromIntegral n
            sd_      = if n < 2 then 0
                       else sqrt (sum [(x - mu) ^ (2::Int) | x <- allVals]
                                  / fromIntegral (n - 1))
            (lo, hi) = hdi 0.94 allVals
            essV     = ess allVals
            rh       = if multi then rhat perChain else Nothing
        in SummaryRow p mu sd_ lo hi essV rh
  in map mkRow params
