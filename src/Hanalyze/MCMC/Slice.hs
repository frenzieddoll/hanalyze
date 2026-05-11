{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Slice sampler (Neal 2003) — a univariate method with no acceptance-rate
-- tuning.
--
-- Each iteration:
--
--   1. Draw @log_y = log p(θ) − Exp(1)@ from the current log-density.
--   2. Build a horizontal slice @[L, R]@ along each axis via stepping-out.
--   3. Shrink: draw @θ_i'@ uniformly from @[L, R]@ and accept when
--      @log p > log_y@.
--
-- One iteration is a Gibbs-style sweep over every coordinate. Like
-- HMC/NUTS no gradient is required, but each sweep involves many
-- log-density evaluations.
module Hanalyze.MCMC.Slice
  ( SliceConfig (..)
  , defaultSliceConfig
  , slice
  , sliceChains
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM, replicateM, when)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import System.Random.MWC (GenIO, uniform)
import System.Random.MWC.Distributions (exponential)

import Hanalyze.Model.HBM (ModelP, Params, logJoint, sampleNames)
import Hanalyze.MCMC.Core (Chain (..), spawnGen)

-- | Slice-sampler configuration.
data SliceConfig = SliceConfig
  { sliceIterations :: Int                -- ^ Total iterations (burn-in included).
  , sliceBurnIn     :: Int                -- ^ Burn-in iterations to discard.
  , sliceWidths     :: Map Text Double    -- ^ Initial stepping-out width @w@
                                          --   per coordinate (default 1.0).
  , sliceMaxSteps   :: Int                -- ^ Maximum number of stepping-out
                                          --   steps (safety bound).
  } deriving (Show)

-- | Default configuration: 1000 iterations, 200 burn-in, width 1.0 per
-- parameter, max stepping-out 50.
defaultSliceConfig :: [Text] -> SliceConfig
defaultSliceConfig names = SliceConfig
  { sliceIterations = 1000
  , sliceBurnIn     = 200
  , sliceWidths     = Map.fromList [(n, 1.0) | n <- names]
  , sliceMaxSteps   = 50
  }

-- | Run the slice sampler. One iteration updates every coordinate in
-- turn (Gibbs-style sweep).
slice :: ModelP r -> SliceConfig -> Params -> GenIO -> IO Chain
slice model cfg init_ gen = do
  let names    = sampleNames model
      total    = sliceBurnIn cfg + sliceIterations cfg
      widths   = sliceWidths cfg
      maxStep  = sliceMaxSteps cfg

      logP :: Params -> Double
      logP = logJoint model

  samplesRef  <- newIORef []
  acceptedRef <- newIORef (0 :: Int)

  -- 1 coordinate 更新 (slice sampling on one axis)
  let updateOne :: Text -> Params -> IO Params
      updateOne nm cur = do
        let w   = Map.findWithDefault 1.0 nm widths
            x0  = Map.findWithDefault 0.0 nm cur
            pAt v = logP (Map.insert nm v cur)
        -- 水平スライス: log_y = log p(θ) - Exp(1)
        e <- exponential 1.0 gen
        let logY = pAt x0 - e
        -- Stepping out
        u <- uniform gen
        let l0   = x0 - w * (u :: Double)
            r0   = l0 + w
        u2 <- uniform gen
        let kL    = floor (fromIntegral maxStep * (u2 :: Double)) :: Int
            kR    = maxStep - 1 - kL
            expandLeft k l
              | k <= 0 || pAt l <= logY = return l
              | otherwise = expandLeft (k - 1) (l - w)
            expandRight k r
              | k <= 0 || pAt r <= logY = return r
              | otherwise = expandRight (k - 1) (r + w)
        l1 <- expandLeft  kL l0
        r1 <- expandRight kR r0
        -- Shrinkage
        let shrink l r = do
              uS <- uniform gen
              let xNew = l + (uS :: Double) * (r - l)
              if pAt xNew > logY
                then return xNew
                else
                  if xNew < x0
                    then shrink xNew r
                    else shrink l xNew
        xNew <- shrink l1 r1
        modifyIORef' acceptedRef (+1)
        return (Map.insert nm xNew cur)

  let sweep current = foldr (\_ _ -> id) id [] `seq`
                       sweepGo names current
        where
          sweepGo []     c = return c
          sweepGo (n:ns) c = do c' <- updateOne n c
                                sweepGo ns c'

  let loop 0 current = return current
      loop i current = do
        next <- sweep current
        when (i <= sliceIterations cfg) $
          modifyIORef' samplesRef (next :)
        loop (i - 1) next

  _ <- loop total init_
  samples  <- fmap reverse (readIORef samplesRef)
  accepted <- readIORef acceptedRef
  return Chain
    { chainSamples     = samples
    , chainAccepted    = accepted
    , chainTotal       = total * length names
    , chainEnergy      = []
    , chainDivergences = []
    }

-- | Run 'slice' on @numChains@ parallel chains.
sliceChains :: ModelP r -> SliceConfig -> Int -> Params -> GenIO -> IO [Chain]
sliceChains model cfg numChains initP baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> slice model cfg initP g) gens
