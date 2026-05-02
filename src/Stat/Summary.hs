{-# LANGUAGE OverloadedStrings #-}
-- | 事後分布の要約統計量 (Stat.Summary)。
--
-- @az.summary@ 相当の SummaryRow / posteriorSummary を提供する。
-- もともと Viz.MCMC で定義していたものを、可視化レイヤーから分離し、
-- 統計層 (Stat.*) に集約した (Phase H6)。
--
-- HTML 描画やコンソール出力は Viz.MCMC.posteriorSummaryHtml /
-- posteriorSummaryFile / printPosteriorSummary に残してある。
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
