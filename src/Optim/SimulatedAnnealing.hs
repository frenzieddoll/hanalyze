-- | Simulated Annealing.
--
-- Kirkpatrick, Gelatt, Vecchi (1983). A physical analogy (cooling solids):
-- a random walk with probabilistic acceptance approaches a global
-- optimum.
--
-- Acceptance probability (Metropolis criterion):
--
--   * Improvement (@Δf < 0@): always accept.
--   * Deterioration (@Δf ≥ 0@): accept with probability @exp(-Δf / T)@.
--
-- Temperature schedule: @T_k = T_0 · α^k@ (geometric cooling, with
-- @α ∈ [0.85, 0.99]@).
--
-- Proposal: add @Normal(0, sigma)@ independently per dimension and reflect
-- against the bounds.
module Optim.SimulatedAnnealing
  ( SAConfig (..)
  , defaultSAConfig
  , runSA
  , runSAWith
  ) where

import Control.Monad (forM)
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWCD
import Optim.Common

-- | SA 設定。
data SAConfig = SAConfig
  { saStop      :: !StopCriteria
  , saInitTemp  :: !Double          -- ^ 初期温度 T_0
  , saAlpha     :: !Double          -- ^ 冷却係数 α (0.85 〜 0.99)
  , saStepSigma :: !Double          -- ^ 提案分布の sd
  , saStepDecay :: !Double          -- ^ sd の冷却係数 (1.0 で固定)
  , saBounds    :: !Bounds                -- ^ (lo, hi) per dim、反射用
  , saDir       :: !Direction
  } deriving (Show, Eq)

defaultSAConfig :: [(Double, Double)] -> SAConfig
defaultSAConfig bs = SAConfig
  { saStop      = defaultStopCriteria { stMaxIter = 5000 }
  , saInitTemp  = 1.0
  , saAlpha     = 0.995
  , saStepSigma = 0.5
  , saStepDecay = 0.999
  , saBounds    = bs
  , saDir       = Minimize
  }

-- | 既定設定で実行。
runSA :: [(Double, Double)]
      -> ([Double] -> Double)
      -> [Double]                  -- ^ 初期点
      -> MWC.GenIO
      -> IO OptimResult
runSA bs f x0 gen = runSAWith (defaultSAConfig bs) f x0 gen

-- | 設定指定で実行。
runSAWith :: SAConfig
          -> ([Double] -> Double)
          -> [Double]
          -> MWC.GenIO
          -> IO OptimResult
runSAWith cfg fUser x0 gen = do
  let f    = flipFor (saDir cfg) fUser
      f0   = f x0
  go 0 x0 f0 x0 f0 (saInitTemp cfg) (saStepSigma cfg) [f0]
  where
    f = flipFor (saDir cfg) fUser

    go iter x fx xBest fBest temp sigma hist
      | iter >= stMaxIter (saStop cfg) =
          mkRes (saDir cfg) xBest fBest hist iter False
      | temp < 1e-12 =
          mkRes (saDir cfg) xBest fBest hist iter True
      | otherwise = do
          xRaw <- forM x $ \xi -> do
                    eps <- MWCD.normal 0 sigma gen
                    pure (xi + eps)
          let xCand = clipToBounds (saBounds cfg) xRaw
          let fNew = f xCand
          u <- MWC.uniformR (0, 1 :: Double) gen
          let dF = fNew - fx
              accept = dF < 0 || u < exp (- dF / temp)
              (xN, fxN)  = if accept then (xCand, fNew) else (x, fx)
              (xBN, fBN) = if fxN < fBest then (xN, fxN) else (xBest, fBest)
              tempN  = temp * saAlpha cfg
              sigmaN = sigma * saStepDecay cfg
              histN  = fBN : hist
          go (iter + 1) xN fxN xBN fBN tempN sigmaN histN

mkRes :: Direction -> [Double] -> Double -> [Double]
      -> Int -> Bool -> IO OptimResult
mkRes dir xb fb hist iter conv =
  let vUser = case dir of { Minimize -> fb; Maximize -> negate fb }
      hU    = case dir of
                Minimize -> reverse hist
                Maximize -> map negate (reverse hist)
  in pure $ OptimResult xb vUser hU iter conv
