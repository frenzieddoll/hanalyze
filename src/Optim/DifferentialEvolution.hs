-- | Differential Evolution (DE/rand/1/bin) — Storn & Price 1997.
--
-- A gradient-free, global, simple-to-implement and empirically robust
-- evolutionary algorithm. Best suited to continuous non-convex problems,
-- typically effective in the 5-30 dimensional regime.
--
-- Algorithm (DE/rand/1/bin) — each generation, for every individual @i@:
--
--   1. Pick three distinct indices @a, b, c@ from the population (all
--      different from @i@).
--   2. Mutation: @v = a + F * (b - c)@ with mutation factor @F ∈ [0.5, 0.8]@
--      typical.
--   3. Binomial crossover: @u_j = v_j@ with probability @CR ∈ [0.7, 0.9]@,
--      otherwise @x_j@; at least one dimension is forced from @v@.
--   4. Selection: replace @x_i ← u@ if @f(u) ≤ f(x_i)@.
--
-- Cost: @N@ function evaluations per generation (population size). Easily
-- parallelizable, but this implementation is sequential.
module Optim.DifferentialEvolution
  ( DEConfig (..)
  , defaultDEConfig
  , runDE
  , runDEWith
  ) where

import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWCD
import Control.Monad (forM, forM_)
import Data.IORef
import Optim.Common

-- | DE 設定。
--
-- F (mutation factor) と CR (crossover rate) は典型値。
-- 集団サイズは次元 D に対して 5×D〜10×D 程度を推奨。
data DEConfig = DEConfig
  { deStop      :: !StopCriteria
  , dePopSize   :: !Int        -- ^ 集団サイズ N (典型 5*D 〜 10*D)
  , deF         :: !Double     -- ^ mutation 係数 F (典型 0.5-0.8)
  , deCR        :: !Double     -- ^ crossover 確率 CR (典型 0.7-0.9)
  , deBounds    :: !Bounds                -- ^ 各次元 (lo, hi)、初期化と境界補正に使用
  , deDir       :: !Direction
  } deriving (Show, Eq)

defaultDEConfig :: [(Double, Double)] -> DEConfig
defaultDEConfig bs = DEConfig
  { deStop    = defaultStopCriteria { stMaxIter = 200 }
  , dePopSize = max 20 (10 * length bs)
  , deF       = 0.7
  , deCR      = 0.9
  , deBounds  = bs
  , deDir     = Minimize
  }

-- | 既定設定で実行。`bounds` から `defaultDEConfig` を構築。
runDE :: [(Double, Double)]            -- ^ 各次元の探索範囲
      -> ([Double] -> Double)          -- ^ 目的関数
      -> MWC.GenIO
      -> IO OptimResult
runDE bounds f gen = runDEWith (defaultDEConfig bounds) f gen

-- | 設定を指定して実行。
runDEWith :: DEConfig
          -> ([Double] -> Double)
          -> MWC.GenIO
          -> IO OptimResult
runDEWith cfg fUser gen = do
  let f      = flipFor (deDir cfg) fUser
      d      = length (deBounds cfg)
      n      = dePopSize cfg
  -- 初期集団: 各次元 (lo, hi) 一様乱数
  pop0 <- forM [1 .. n] $ \_ -> sampleUniformIn (deBounds cfg) gen
  let fPop0 = map f pop0
  popRef  <- newIORef (zip pop0 fPop0)
  histRef <- newIORef [minimum fPop0]
  iterRef <- newIORef 0
  convRef <- newIORef False
  let stop = deStop cfg
      maxI = stMaxIter stop

  let loop = do
        i <- readIORef iterRef
        if i >= maxI
          then return ()
          else do
            pop <- readIORef popRef
            let fs    = map snd pop
                bestF = minimum fs
                worstF = maximum fs
            if abs (worstF - bestF) < stTolFun stop
              then writeIORef convRef True
              else do
                pop' <- stepDE cfg f gen pop
                writeIORef popRef pop'
                let bestF' = minimum (map snd pop')
                modifyIORef histRef (bestF' :)
                writeIORef iterRef (i + 1)
                loop
  loop
  popFinal <- readIORef popRef
  iters    <- readIORef iterRef
  conv     <- readIORef convRef
  histR    <- readIORef histRef
  let (xb, vb) = minimumBy (comparing snd) popFinal
      vUser    = case deDir cfg of { Minimize -> vb; Maximize -> negate vb }
      histUser = case deDir cfg of
                   Minimize -> reverse histR
                   Maximize -> map negate (reverse histR)
  return $ OptimResult xb vUser histUser iters conv

-- | 1 世代の更新。
stepDE :: DEConfig
       -> ([Double] -> Double)
       -> MWC.GenIO
       -> [([Double], Double)]
       -> IO [([Double], Double)]
stepDE cfg f gen pop = do
  let n   = length pop
      d   = length (deBounds cfg)
      f0  = deF cfg
      cr  = deCR cfg
      bs  = deBounds cfg
  newPop <- forM [0 .. n - 1] $ \i -> do
    -- mutation 用に i と異なる 3 個体をランダム選択
    [a, b, c] <- pickThree n i gen
    let xa = fst (pop !! a)
        xb = fst (pop !! b)
        xc = fst (pop !! c)
        v  = zipWith3 (\xai xbi xci -> xai + f0 * (xbi - xci)) xa xb xc
        v' = clipToBounds bs v
    -- crossover (binomial)
    jRand <- MWC.uniformR (0, d - 1) gen
    let (xi, fi) = pop !! i
    u <- forM (zip3 [0..] xi v') $ \(j, xj, vj) -> do
      r <- MWC.uniformR (0, 1) gen
      return $ if (r :: Double) < cr || j == jRand then vj else xj
    let fu = f u
    return $ if fu <= fi then (u, fu) else (xi, fi)
  return newPop

-- | i と異なる 3 つの相異なるインデックスを集団 [0, n) から選ぶ。
pickThree :: Int -> Int -> MWC.GenIO -> IO [Int]
pickThree n i gen = do
  let pickOne avoid = do
        k <- MWC.uniformR (0, n - 1) gen
        if k `elem` avoid then pickOne avoid else return k
  a <- pickOne [i]
  b <- pickOne [i, a]
  c <- pickOne [i, a, b]
  return [a, b, c]

-- | (`sampleUniform` and `clipBound` are now provided by `Optim.Common`
--    as `sampleUniformIn` / `clipToBounds`.)
