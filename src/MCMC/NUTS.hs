{-# LANGUAGE OverloadedStrings #-}
-- | No-U-Turn Sampler (NUTS)。
--
-- Hoffman & Gelman (2014) Algorithm 3 を実装。
-- 制約付きパラメータは unconstrained 空間で自動変換されます（HMC と同様）。
-- 自動的に最適な軌道長を決定するため、HMC のステップ数チューニングが不要。
--
-- nutsAdaptStepSize = True に設定するとバーンイン中に
-- Nesterov の dual averaging でステップ幅を自動調整します (Stan 方式)。
module MCMC.NUTS
  ( NUTSConfig (..)
  , defaultNUTSConfig
  , nuts
  , nutsChains
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (foldM, forM, replicateM, when)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.Random.MWC (GenIO, uniform)
import System.Random.MWC.Distributions (standard)

import Model.HBM (Model, Params, sampleNames, getTransforms)
import MCMC.Core (Chain (..), spawnGen)
import MCMC.HMC
  ( kinetic, leapfrogWith, gradUU, logJointU
  , paramsToVec, toUnconstrainedParams, fromUnconstrainedParams
  )
import Stat.Distribution (Transform)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data NUTSConfig = NUTSConfig
  { nutsIterations   :: Int
  , nutsBurnIn       :: Int
  , nutsStepSize     :: Double
  , nutsMaxDepth     :: Int
  , nutsAdaptStepSize :: Bool    -- ^ バーンイン中に dual averaging でステップ幅を自動調整
  , nutsTargetAccept :: Double   -- ^ 目標受容率 (dual averaging の δ、デフォルト 0.8)
  } deriving (Show)

defaultNUTSConfig :: NUTSConfig
defaultNUTSConfig = NUTSConfig
  { nutsIterations    = 2000
  , nutsBurnIn        = 500
  , nutsStepSize      = 0.1
  , nutsMaxDepth      = 10
  , nutsAdaptStepSize = True
  , nutsTargetAccept  = 0.8
  }

-- ---------------------------------------------------------------------------
-- Dual averaging (Nesterov 2009, Hoffman & Gelman 2014 § 3.2)
-- ---------------------------------------------------------------------------

-- | Dual averaging の状態。バーンイン中だけ使用する。
data DualAvgState = DualAvgState
  { daLogEps     :: Double  -- ^ 現在の log ε
  , daLogEpsBar  :: Double  -- ^ 指数移動平均 log ε̄  (バーンイン後に採用)
  , daH          :: Double  -- ^ 累積 H_m
  , daMu         :: Double  -- ^ 収縮先 μ = log(10 ε₀)
  , daM          :: Int     -- ^ ステップカウント
  }

initDualAvg :: Double -> DualAvgState
initDualAvg eps0 = DualAvgState
  { daLogEps    = log eps0
  , daLogEpsBar = log eps0   -- ε̄₀ = ε₀ (バーンイン失敗でも ε₀ に戻る)
  , daH         = 0.0
  , daMu        = log (10 * eps0)
  , daM         = 0
  }

-- | 1 ステップ更新。alpha は今ステップの採択確率 min(1, exp(logAlpha))。
updateDualAvg :: Double -> Double -> DualAvgState -> DualAvgState
updateDualAvg delta alpha da =
  let m      = daM da + 1
      gamma  = 0.05   -- 収縮強度
      t0     = 10.0
      kappa  = 0.75   -- 指数移動平均の減衰率
      hNew   = (1 - 1 / (fromIntegral m + t0)) * daH da
             + (1 / (fromIntegral m + t0)) * (delta - alpha)
      logEps = daMu da - sqrt (fromIntegral m) / gamma * hNew
      -- ε を合理的な範囲にクリップ (爆発的な増減を防ぐ)
      logEpsClip = max (-7) (min 5 logEps)
      logEpsBar = (fromIntegral m ** (-kappa)) * logEpsClip
                + (1 - fromIntegral m ** (-kappa)) * daLogEpsBar da
  in da { daLogEps = logEpsClip, daLogEpsBar = logEpsBar, daH = hNew, daM = m }

-- ---------------------------------------------------------------------------
-- 内部: バイナリツリー
-- ---------------------------------------------------------------------------

data NUTSTree = NUTSTree
  { ntThMinus :: Params
  , ntRMinus  :: [Double]
  , ntThPlus  :: Params
  , ntRPlus   :: [Double]
  , ntThPrime :: Params
  , ntN       :: Int
  , ntS       :: Bool
  }

deltaMax :: Double
deltaMax = 1000.0

uTurn :: [Text] -> Params -> [Double] -> Params -> [Double] -> Bool
uTurn names thMinus rMinus thPlus rPlus =
  let delta     = zipWith (-) (paramsToVec names thPlus) (paramsToVec names thMinus)
      dot xs ys = sum (zipWith (*) xs ys)
  in dot delta rMinus < 0 || dot delta rPlus < 0

-- ---------------------------------------------------------------------------
-- 再帰的ツリービルダー
-- ---------------------------------------------------------------------------

buildTree
  :: Model a
  -> Map.Map Text Transform
  -> [Text]
  -> Double
  -> Params
  -> [Double]
  -> Double
  -> Int
  -> Int
  -> GenIO
  -> IO NUTSTree
buildTree model transforms names eps theta r logU dir depth gen
  | depth == 0 = do
      let gradFn  = gradUU model transforms
          (theta', r') = leapfrogWith gradFn names (fromIntegral dir * eps) 1 theta r
          h'  = -(logJointU model transforms theta') + kinetic r'
          n'  = if logU <= -h' then 1 else 0
          s'  = logU < deltaMax - h'
      return NUTSTree
        { ntThMinus = theta', ntRMinus = r'
        , ntThPlus  = theta', ntRPlus  = r'
        , ntThPrime = theta', ntN = n', ntS = s'
        }
  | otherwise = do
      t1 <- buildTree model transforms names eps theta r logU dir (depth - 1) gen
      if not (ntS t1) then return t1
      else do
        let (th0, r0) = if dir == -1
              then (ntThMinus t1, ntRMinus t1)
              else (ntThPlus  t1, ntRPlus  t1)
        t2 <- buildTree model transforms names eps th0 r0 logU dir (depth - 1) gen
        let n1 = ntN t1; n2 = ntN t2
        thPrime' <-
          if n1 == 0 then return (ntThPrime t2)
          else if n2 == 0 then return (ntThPrime t1)
          else do
            u <- uniform gen :: IO Double
            return $ if u < min 1.0 (fromIntegral n2 / fromIntegral n1)
                     then ntThPrime t2
                     else ntThPrime t1
        let (minus', rMinus', plus', rPlus') = if dir == -1
              then (ntThMinus t2, ntRMinus t2, ntThPlus t1, ntRPlus t1)
              else (ntThMinus t1, ntRMinus t1, ntThPlus t2, ntRPlus t2)
            s' = ntS t2 && not (uTurn names minus' rMinus' plus' rPlus')
        return NUTSTree
          { ntThMinus = minus', ntRMinus = rMinus'
          , ntThPlus  = plus',  ntRPlus  = rPlus'
          , ntThPrime = thPrime', ntN = n1 + n2, ntS = s'
          }

-- ---------------------------------------------------------------------------
-- NUTS サンプラー
-- ---------------------------------------------------------------------------

nuts :: Model a -> NUTSConfig -> Params -> GenIO -> IO Chain
nuts model cfg initC gen = do
  let names      = sampleNames model
      transforms = getTransforms model
      initU      = toUnconstrainedParams transforms initC
      total      = nutsBurnIn cfg + nutsIterations cfg
      doAdapt    = nutsAdaptStepSize cfg && nutsBurnIn cfg > 0

  samplesRef  <- newIORef []
  acceptedRef <- newIORef (0 :: Int)
  daRef       <- newIORef (initDualAvg (nutsStepSize cfg))

  -- 1 NUTS ステップ。dual averaging 用に単一リープフロッグの採択確率も返す。
  -- (Stan と同様: NUTS 軌道全体でなく 1-step Hamiltonian 比を alpha に使う)
  let gradFn = gradUU model transforms
      step eps currentU = do
        r0 <- forM names (\_ -> standard gen)
        u0 <- uniform gen :: IO Double
        let h0   = -(logJointU model transforms currentU) + kinetic r0
            logU = log u0 - h0
        let tree0 = NUTSTree
              { ntThMinus = currentU, ntRMinus = r0
              , ntThPlus  = currentU, ntRPlus  = r0
              , ntThPrime = currentU, ntN = 1, ntS = True
              }
        let doubleTree tree j =
              if not (ntS tree) then return tree
              else do
                u <- uniform gen :: IO Double
                let dir = if u < 0.5 then -1 else 1 :: Int
                    (th0, r0') = if dir == -1
                      then (ntThMinus tree, ntRMinus tree)
                      else (ntThPlus  tree, ntRPlus  tree)
                subtree <- buildTree model transforms names eps th0 r0' logU dir j gen
                let n1 = ntN tree; n2 = ntN subtree
                thPrime' <-
                  if not (ntS subtree) || n2 == 0
                  then return (ntThPrime tree)
                  else do
                    u2 <- uniform gen :: IO Double
                    return $ if u2 < min 1.0 (fromIntegral n2 / fromIntegral n1)
                             then ntThPrime subtree
                             else ntThPrime tree
                let (minus', rMinus', plus', rPlus') = if dir == -1
                      then (ntThMinus subtree, ntRMinus subtree,
                            ntThPlus  tree,    ntRPlus  tree)
                      else (ntThMinus tree,    ntRMinus tree,
                            ntThPlus  subtree, ntRPlus  subtree)
                    s' = ntS subtree && not (uTurn names minus' rMinus' plus' rPlus')
                return NUTSTree
                  { ntThMinus = minus', ntRMinus = rMinus'
                  , ntThPlus  = plus',  ntRPlus  = rPlus'
                  , ntThPrime = thPrime', ntN = n1 + n2, ntS = s'
                  }
        finalTree <- foldM doubleTree tree0 [0 .. nutsMaxDepth cfg - 1]
        let proposedU = ntThPrime finalTree
            -- dual averaging 用 alpha: 単一リープフロッグの Hamiltonian 比
            -- (NUTS 軌道全体の平均でなく 1-step 比を使う — Stan 方式)
            (thetaOne, rOne) = leapfrogWith gradFn names eps 1 currentU r0
            hOne   = -(logJointU model transforms thetaOne) + kinetic rOne
            alpha  = min 1.0 (exp (h0 - hOne))
        when (proposedU /= currentU) $ modifyIORef' acceptedRef (+1)
        return (proposedU, alpha)

  -- カウントダウンループ: i = total → 1
  --   i > nutsIterations cfg → バーンイン (dual averaging 期)
  --   i ≤ nutsIterations cfg → 保存期
  let loop 0 currentU _eps = return currentU
      loop i currentU eps = do
        (nextU, alpha) <- step eps currentU
        let isBurnIn = i > nutsIterations cfg
        eps' <- if doAdapt && isBurnIn
          then do
            da <- readIORef daRef
            let da' = updateDualAvg (nutsTargetAccept cfg) alpha da
            writeIORef daRef da'
            return (exp (daLogEps da'))
          else do
            -- バーンイン終了直後に ε̄ を確定
            da <- readIORef daRef
            let epsBar = if doAdapt && not isBurnIn && i == nutsIterations cfg
                         then exp (daLogEpsBar da)
                         else eps
            return epsBar
        if not isBurnIn
          then modifyIORef' samplesRef (fromUnconstrainedParams transforms nextU :)
          else return ()
        loop (i - 1) nextU eps'

  _ <- loop total initU (nutsStepSize cfg)
  samples  <- fmap reverse (readIORef samplesRef)
  accepted <- readIORef acceptedRef
  return Chain
    { chainSamples  = samples
    , chainAccepted = accepted
    , chainTotal    = total
    }

-- | NUTS を numChains 本並列実行する (+RTS -N で CPU 並列)。
nutsChains :: Model a -> NUTSConfig -> Int -> Params -> GenIO -> IO [Chain]
nutsChains model cfg numChains initC baseGen = do
  gens <- replicateM numChains (spawnGen baseGen)
  mapConcurrently (\g -> nuts model cfg initC g) gens
