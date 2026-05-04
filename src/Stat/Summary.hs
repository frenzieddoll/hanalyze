{-# LANGUAGE OverloadedStrings #-}
-- | Posterior-distribution summary statistics.
--
-- Provides 'SummaryRow' and 'posteriorSummary', mirroring the columns of
-- ArviZ's @az.summary@ (mean, sd, HDI, ESS, R-hat). Originally lived in
-- @Viz.MCMC@; moved to the statistics layer to decouple it from the
-- visualization stack.
--
-- HTML rendering and console pretty-printing remain in
-- @Viz.MCMC.posteriorSummaryHtml@ / @posteriorSummaryFile@ /
-- @printPosteriorSummary@.
module Stat.Summary
  ( SummaryRow (..)
  , posteriorSummary
  ) where

import Data.Text (Text)
import MCMC.Core (Chain, chainVals)
import Stat.MCMC (ess, hdi, rhat)

-- | パラメタ 1 行分の事後要約。
data SummaryRow = SummaryRow
  { srName  :: Text
  , srMean  :: Double
  , srSD    :: Double
  , srHdiLo :: Double  -- ^ 94% HDI 下限
  , srHdiHi :: Double  -- ^ 94% HDI 上限
  , srEssV  :: Double
  , srRhat  :: Maybe Double  -- ^ 単一チェーンなら Nothing
  } deriving (Show)

-- | 事後要約を計算する。チェーン 1 本なら R-hat は Nothing、
-- 2 本以上なら全チェーンを連結した値で mean/sd/HDI/ESS を計算し、
-- R-hat だけ split-R-hat で算出する。
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
