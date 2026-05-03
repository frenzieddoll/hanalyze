-- | Particle Swarm Optimization (PSO)。
--
-- Kennedy & Eberhart 1995。粒子群が個人最良 (pbest) と全体最良 (gbest) に
-- 引き寄せられながら速度を更新するメタヒューリスティック。
--
-- 速度・位置の更新:
--
--   v_{t+1} = w · v_t + c_1 · r_1 · (pbest - x) + c_2 · r_2 · (gbest - x)
--   x_{t+1} = x_t + v_{t+1}
--
-- ここで w (慣性), c_1 (認知係数), c_2 (社会係数), r_1, r_2 ~ U(0,1)。
module Optim.ParticleSwarm
  ( PSOConfig (..)
  , defaultPSOConfig
  , runPSO
  , runPSOWith
  ) where

import Control.Monad (forM, replicateM)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.IORef
import qualified System.Random.MWC as MWC
import Optim.Common

-- | PSO 設定。
data PSOConfig = PSOConfig
  { psoStop     :: !StopCriteria
  , psoNum      :: !Int               -- ^ 粒子数 (典型 20-50)
  , psoInertia  :: !Double             -- ^ w (典型 0.4-0.9)
  , psoCog      :: !Double             -- ^ c_1 (typical 1.5-2.0)
  , psoSoc      :: !Double             -- ^ c_2 (typical 1.5-2.0)
  , psoBounds   :: !Bounds              -- ^ (lo, hi) per dim
  , psoVMax     :: !Double             -- ^ |v_i| の上限 (range の比率、0.5 等)
  , psoDir      :: !Direction
  } deriving (Show, Eq)

defaultPSOConfig :: [(Double, Double)] -> PSOConfig
defaultPSOConfig bs = PSOConfig
  { psoStop    = defaultStopCriteria { stMaxIter = 200 }
  , psoNum     = max 20 (5 * length bs)
  , psoInertia = 0.7
  , psoCog     = 1.5
  , psoSoc     = 1.5
  , psoBounds  = bs
  , psoVMax    = 0.5
  , psoDir     = Minimize
  }

-- | 既定設定で実行。
runPSO :: [(Double, Double)]
       -> ([Double] -> Double)
       -> MWC.GenIO
       -> IO OptimResult
runPSO bs f gen = runPSOWith (defaultPSOConfig bs) f gen

-- | 設定指定で実行。
runPSOWith :: PSOConfig
           -> ([Double] -> Double)
           -> MWC.GenIO
           -> IO OptimResult
runPSOWith cfg fUser gen = do
  let f      = flipFor (psoDir cfg) fUser
      bs     = psoBounds cfg
      n      = length bs
      np     = psoNum cfg
      vMaxes = [ psoVMax cfg * (hi - lo) | (lo, hi) <- bs ]

  -- 初期化
  xs0 <- replicateM np (sampleUniformIn bs gen)
  vs0 <- replicateM np $ forM (zip bs vMaxes) $ \((lo, hi), vM) -> do
           u <- MWC.uniformR (-1, 1) gen
           return ((u :: Double) * vM * 0.1)
  let fs0 = map f xs0

  posRef     <- newIORef xs0
  velRef     <- newIORef vs0
  pbestRef   <- newIORef (zip xs0 fs0)
  gbestRef   <- newIORef (minimumBy (comparing snd) (zip xs0 fs0))
  histRef    <- newIORef [snd (minimumBy (comparing snd) (zip xs0 fs0))]
  iterRef    <- newIORef 0

  let stop = psoStop cfg
      maxI = stMaxIter stop

  let loop = do
        i <- readIORef iterRef
        if i >= maxI then return ()
          else do
            xs <- readIORef posRef
            vs <- readIORef velRef
            pb <- readIORef pbestRef
            (gbX, gbF) <- readIORef gbestRef
            -- 更新
            updated <- forM (zip3 xs vs pb) $ \(x, v, (px, pf)) -> do
              vNew <- forM (zip4 x v px gbX) $ \(xi, vi, pxi, gxi) -> do
                r1 <- MWC.uniformR (0, 1) gen :: IO Double
                r2 <- MWC.uniformR (0, 1) gen :: IO Double
                pure $ psoInertia cfg * vi
                       + psoCog cfg * r1 * (pxi - xi)
                       + psoSoc cfg * r2 * (gxi - xi)
              -- vMax クリップ
              let vClipped = zipWith (\vi vM -> max (-vM) (min vM vi)) vNew vMaxes
              -- 位置更新 + bounds 反射
              let xNew = clipToBounds bs (zipWith (+) x vClipped)
              let fNew = f xNew
              -- pbest 更新
              let (pxN, pfN) = if fNew < pf then (xNew, fNew) else (px, pf)
              return (xNew, vClipped, (pxN, pfN), fNew)
            let xsN = [a | (a, _, _, _) <- updated]
                vsN = [b | (_, b, _, _) <- updated]
                pbN = [c | (_, _, c, _) <- updated]
                bestC = minimumBy (comparing snd) [(a, d) | (a, _, _, d) <- updated]
                (gbXN, gbFN) = if snd bestC < gbF then bestC else (gbX, gbF)
            writeIORef posRef xsN
            writeIORef velRef vsN
            writeIORef pbestRef pbN
            writeIORef gbestRef (gbXN, gbFN)
            modifyIORef histRef (gbFN :)
            writeIORef iterRef (i + 1)
            loop
  loop
  (gbX, gbF) <- readIORef gbestRef
  iters      <- readIORef iterRef
  histR      <- readIORef histRef
  let vUser = case psoDir cfg of { Minimize -> gbF; Maximize -> negate gbF }
      hU    = case psoDir cfg of
                Minimize -> reverse histR
                Maximize -> map negate (reverse histR)
  return $ OptimResult gbX vUser hU iters False
  where
    zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a, b, c, d) : zip4 as bs cs ds
    zip4 _ _ _ _ = []
