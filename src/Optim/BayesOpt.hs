{-# LANGUAGE OverloadedStrings #-}
-- | Bayesian Optimization loop.
--
-- Single-objective procedure:
--
--   1. Evaluate initial points (Latin hypercube or random).
--   2. Fit a Gaussian process to the observations.
--   3. Maximize an acquisition function to choose the next @x@.
--   4. Evaluate @x@ and append to the observed sequence.
--   5. Repeat steps 2-4 for @T@ iterations.
module Optim.BayesOpt
  ( BayesOptConfig (..)
  , defaultBayesOptConfig
  , bayesOpt
  , bayesOptND
  , bayesOptScalarMO
  , bayesOptMOWithNSGA
  ) where

import Control.Exception (SomeException, try, evaluate)
import Control.Monad (forM, replicateM)
import Data.List (minimumBy)
import Data.Ord (comparing)
import System.IO.Unsafe (unsafePerformIO)
import System.Random.MWC (GenIO, uniform)

import Model.GP (Kernel (..), GPModel (..), GPResult (..),
                 fitGP, optimizeGP, initParamsFromData)
import Optim.Acquisition (ei, ucb, parEGO)
import Optim.NSGA       (NSGAConfig (..), defaultNSGAConfig,
                         Solution (..), nsga2)
import Optim.Common     (Bounds)
import qualified Optim.LineSearch as LS
import qualified Optim.LBFGS      as LBFGS
import qualified Optim.Common     as OC

data BayesOptConfig = BayesOptConfig
  { boIterations :: Int        -- 評価予算 (初期点除く)
  , boInitPoints :: Int        -- 初期 LHS の点数
  , boKernel     :: Kernel
  , boUCBBeta    :: Double     -- UCB の β
  , boGridSize   :: Int        -- 内側最適化のグリッド密度 (1D 用)
  } deriving (Show)

defaultBayesOptConfig :: BayesOptConfig
defaultBayesOptConfig = BayesOptConfig
  { boIterations = 30
  , boInitPoints = 5
  , boKernel     = RBF
  , boUCBBeta    = 2.0
  , boGridSize   = 200
  }

-- | 単一目的 Bayesian Optimization (1D 限定の簡易版)。
--
-- 引数:
--   * cfg
--   * f: 目的関数 (1D, 最小化)
--   * (lo, hi): 探索範囲
--
-- 戻り値: (全観測 [(x, y)], 最良 (x*, y*))
bayesOpt :: BayesOptConfig
         -> (Double -> IO Double)
         -> (Double, Double)
         -> GenIO
         -> IO ([(Double, Double)], (Double, Double))
bayesOpt cfg f (lo, hi) gen = do
  -- 初期点 (uniform random, 簡易)
  initX <- replicateM (boInitPoints cfg) (do
              u <- uniform gen :: IO Double
              return (lo + u * (hi - lo)))
  initY <- mapM f initX
  let history0 = zip initX initY

  -- BO ループ
  -- 内側 acquisition 最大化は **Brent 法** (1D 単峰超線形収束)。
  -- 旧 grid (boGridSize 点) は seeding として併用、Brent の bracket を作る。
  let loop t hist
        | t == 0 = return hist
        | otherwise = do
            let xs = map fst hist
                ys = map snd hist
                yBest = minimum ys
                p0 = initParamsFromData xs ys
                pOpt = optimizeGP (boKernel cfg) xs ys p0
                model = GPModel (boKernel cfg) pOpt

                -- 1 点での負 EI (Brent は最小化、引数は [Double] で受ける)
                -- Cholesky / SVD 失敗時はペナルティ +1e30 を返す。
                -- gpMean / gpUpper は遅延フィールドなので evaluate で強制してから返す。
                negEI [x] = unsafePerformIO $ do
                  let computed = do
                        let res = fitGP model xs ys [x]
                            mu  = head (gpMean res)
                            sg  = (head (gpUpper res) - mu) / 2
                        _ <- evaluate mu
                        _ <- evaluate sg
                        pure (negate (ei yBest 0.01 (mu, sg)))
                  r <- try computed :: IO (Either SomeException Double)
                  case r of
                    Left _  -> pure 1e30
                    Right v -> pure v
                negEI _   = error "negEI: 1D"

                -- 粗グリッドで bracket を作る
                gridN = max 16 (boGridSize cfg `div` 4)
                grid  = [lo + fromIntegral i * (hi - lo)
                              / fromIntegral (gridN - 1)
                        | i <- [0 .. gridN - 1]]
                gridV = [(x, negEI [x]) | x <- grid]
                bestG = minimumBy (comparing snd) gridV
                bestX = fst bestG
                idxBest = case [i | (i, (gx, _)) <- zip [0::Int ..] gridV, gx == bestX] of
                            (k:_) -> k; [] -> 0
                ax = fst (gridV !! max 0 (idxBest - 1))
                bx = fst (gridV !! min (gridN - 1) (idxBest + 1))
                -- Brent で局所最大 (= 負の最小)
                bRes = LS.brent (LS.defaultBrentConfig { LS.bcMaxIter = 80
                                                       , LS.bcTol    = 1e-7 })
                                negEI (min ax bx) (max ax bx)
                xNext = head (OC.orBest bRes)

            yNext <- f xNext
            loop (t - 1) (hist ++ [(xNext, yNext)])

  finalHist <- loop (boIterations cfg) history0
  let bestPair = head [pair | pair@(_, y) <- finalHist
                            , y == minimum (map snd finalHist)]
  return (finalHist, bestPair)

-- | N 次元単目的 Bayesian Optimization。
-- 内側 acquisition 最大化を **L-BFGS multi-start** で行う:
-- bounds 範囲内で nStarts 個の初期点を一様乱数で生成、各点から L-BFGS で
-- 負 EI を最小化、最良点を採用。
bayesOptND :: BayesOptConfig
           -> Int                         -- ^ multi-start 数 (典型 5-20)
           -> ([Double] -> IO Double)     -- ^ 目的関数 (N 次元、最小化)
           -> Bounds                      -- ^ 各次元 (lo, hi)
           -> GenIO
           -> IO ([([Double], Double)], ([Double], Double))
bayesOptND cfg nStarts f bounds gen = do
  let dim = length bounds
      sampleX = forM bounds $ \(lo, hi) -> do
        u <- uniform gen :: IO Double
        return (lo + u * (hi - lo))
  initX <- replicateM (boInitPoints cfg) sampleX
  initY <- mapM f initX
  let history0 = zip initX initY
      kern = boKernel cfg

  let loop t hist
        | t == 0 = return hist
        | otherwise = do
            let xss   = map fst hist
                ys    = map snd hist
                yBest = minimum ys
                -- 1D 入力前提なら optimizeGP 直接、N-D は単純化のため第 1 軸のみ採用
                -- ※ MultiInput GP は将来課題。現状は dim==1 で完全動作、dim>1 は近似
                xsFlat = if dim == 1 then map head xss else map (sum) xss  -- fallback
                p0 = initParamsFromData xsFlat ys
                pOpt = optimizeGP kern xsFlat ys p0
                model = GPModel kern pOpt
                negEI xVec = unsafePerformIO $ do
                  let xkey = if dim == 1 then head xVec else sum xVec
                      computed = do
                        let res  = fitGP model xsFlat ys [xkey]
                            mu   = head (gpMean res)
                            sg   = (head (gpUpper res) - mu) / 2
                        _ <- evaluate mu; _ <- evaluate sg
                        pure (negate (ei yBest 0.01 (mu, sg)))
                  r <- try computed :: IO (Either SomeException Double)
                  case r of { Left _ -> pure 1e30; Right v -> pure v }
            -- L-BFGS multi-start
            starts <- replicateM nStarts sampleX
            results <- mapM (\x0 ->
              LBFGS.runLBFGSNumeric
                (LBFGS.defaultLBFGSConfig
                   { LBFGS.lbStop = OC.defaultStopCriteria { OC.stMaxIter = 100 } })
                negEI x0) starts
            let best = minimumBy (comparing OC.orValue) results
                xNextRaw = OC.orBest best
                -- bound clipping
                xNext = zipWith (\(lo, hi) v -> max lo (min hi v)) bounds xNextRaw
            yNext <- f xNext
            loop (t - 1) (hist ++ [(xNext, yNext)])

  finalHist <- loop (boIterations cfg) history0
  let bestPair = minimumBy (comparing snd) finalHist
  return (finalHist, bestPair)

-- | 多目的 BO の **scalarization 版** (ParEGO 風)。
-- 各反復で random 重み w で Tchebycheff scalarize し、単目的 BO の 1 ステップ
-- (L-BFGS multi-start で acquisition 最大化) を実行する。
-- NSGA 版より高速、acquisition 計算コストが軽い問題に向く。
bayesOptScalarMO :: Int                                -- iter
                 -> Int                                -- nInit
                 -> Int                                -- nStarts (multi-start)
                 -> Kernel
                 -> ([Double] -> IO [Double])
                 -> Bounds
                 -> GenIO
                 -> IO [([Double], [Double])]
bayesOptScalarMO nIter nInit nStarts kern f bounds gen = do
  initX <- replicateM nInit (forM bounds $ \(lo, hi) -> do
              u <- uniform gen :: IO Double
              return (lo + u * (hi - lo)))
  initY <- mapM f initX
  let history0 = zip initX initY

      step hist = do
        let xss   = map fst hist
            ysAll = map snd hist
            qDim  = length (head ysAll)
            xsFlat = map head xss             -- 1D 入力前提の簡易版
            ysCol j = [y !! j | y <- ysAll]
        -- random scalarization weight
        wsRaw <- replicateM qDim (uniform gen :: IO Double)
        let wSum = sum wsRaw
            ws   = map (/ wSum) wsRaw
            -- 各目的の GP fit (1D 入力)
            modelFor j =
              let trainY = ysCol j
                  p0 = initParamsFromData xsFlat trainY
                  pOpt = optimizeGP kern xsFlat trainY p0
              in GPModel kern pOpt
            models = [(modelFor j, ysCol j) | j <- [0 .. qDim - 1]]
            -- Tchebycheff: max_j w_j (μ_j - z*_j) — z*_j は最良観測
            zStars = [minimum (ysCol j) | j <- [0 .. qDim - 1]]
            scalarLcb xVec = unsafePerformIO $ do
              let xkey = head xVec
                  computeOne j = do
                    let (m, ty) = models !! j
                        r = fitGP m xsFlat ty [xkey]
                        mu = head (gpMean r)
                        sg = (head (gpUpper r) - mu) / 2
                        lcb = mu - 2.0 * sg
                    _ <- evaluate mu; _ <- evaluate sg
                    pure ((ws !! j) * (lcb - (zStars !! j)))
                  safe j = do
                    res <- try (computeOne j) :: IO (Either SomeException Double)
                    case res of { Left _ -> pure 1e30; Right v -> pure v }
              perJ <- mapM safe [0 .. qDim - 1]
              pure (maximum perJ)
        -- L-BFGS multi-start で scalarLcb 最小化
        starts <- replicateM nStarts (forM bounds $ \(lo, hi) -> do
                    u <- uniform gen :: IO Double
                    return (lo + u * (hi - lo)))
        results <- mapM (\x0 ->
          LBFGS.runLBFGSNumeric
            (LBFGS.defaultLBFGSConfig
               { LBFGS.lbStop = OC.defaultStopCriteria { OC.stMaxIter = 60 } })
            scalarLcb x0) starts
        let best = minimumBy (comparing OC.orValue) results
            xNextRaw = OC.orBest best
            xNext = zipWith (\(lo, hi) v -> max lo (min hi v)) bounds xNextRaw
        yNext <- f xNext
        return (hist ++ [(xNext, yNext)])

      loop t h
        | t == 0 = return h
        | otherwise = step h >>= loop (t - 1)

  loop nIter history0

argmax :: Ord a => [a] -> Int
argmax xs = snd (maximum (zip xs [0..]))

-- ---------------------------------------------------------------------------
-- 多目的 BO with NSGA-II (Phase V4)
-- ---------------------------------------------------------------------------

-- | 多目的 BO の簡易版: 各観測点を NSGA-II で acquisition 関数値を
-- 多目的最適化することで提案する。
--
-- 内部で MultiGP を fit し、各目的の (μ, σ) を予測。
-- NSGA-II で「(目的 1 の μ, 目的 2 の μ, ...) の Pareto front」を求め、
-- そこから 1 点を選んで評価。
--
-- ※ シンプルな実装。EHVI ベースのより洗練された方式は将来拡張。
bayesOptMOWithNSGA
  :: Int                                -- イテレーション数
  -> Int                                -- 初期点
  -> Kernel
  -> ([Double] -> IO [Double])          -- 多目的関数
  -> Bounds
  -> GenIO
  -> IO [([Double], [Double])]          -- (x, y) の系列
bayesOptMOWithNSGA nIter nInit kern f bounds gen = do
  -- 初期点
  initX <- replicateM nInit (do
              vs <- forM bounds $ \(lo, hi) -> do
                u <- uniform gen :: IO Double
                return (lo + u * (hi - lo))
              return vs)
  initY <- mapM f initX
  let history0 = zip initX initY

  let loop t hist
        | t == 0 = return hist
        | otherwise = do
            -- 各目的に GP を fit (1D 入力前提の簡易版)
            -- 多次元入力の場合は MultiGP を別途準備
            -- ここでは bounds の最初の次元のみ使う簡易動作
            let xsFlat = map head (map fst hist)  -- 1D 入力前提
                ysAll = map snd hist
                qDim  = length (head ysAll)
                ysCol j = [y !! j | y <- ysAll]

            -- 各目的 j の GP モデルを fit
            let modelFor j =
                  let trainY = ysCol j
                      p0 = initParamsFromData xsFlat trainY
                      pOpt = optimizeGP kern xsFlat trainY p0
                  in GPModel kern pOpt

                models = [modelFor j | j <- [0 .. qDim - 1]]

                -- NSGA-II で Pareto front を探索 (acquisition surface 上)
                -- 各目的: μ - β σ (LCB) を最小化
                acqObjective xVec =
                  [ unsafePerformIO $ do
                      let computed = do
                            let trainY = ysCol j
                                m = models !! j
                                gpRes = fitGP m xsFlat trainY [head xVec]
                                mu = head (gpMean gpRes)
                                sg = (head (gpUpper gpRes) - mu) / 2
                            _ <- evaluate mu; _ <- evaluate sg
                            pure (ucbToMin mu sg)
                      r <- try computed :: IO (Either SomeException Double)
                      case r of { Left _ -> pure 1e30; Right v -> pure v }
                  | j <- [0 .. qDim - 1] ]

                ucbToMin :: Double -> Double -> Double
                ucbToMin mu sigma = mu - 2.0 * sigma   -- LCB

            -- NSGA-II で Pareto front を 1 ステップ探索
            front <- nsga2 (defaultNSGAConfig { nsgaPopSize = 30
                                             , nsgaGenerations = 30 })
                          acqObjective bounds gen

            -- front から random 選択
            idx <- uniform gen :: IO Double
            let i = floor (idx * fromIntegral (length front))
                xNext = solDecision (front !! min i (length front - 1))
            yNext <- f xNext
            loop (t - 1) (hist ++ [(xNext, yNext)])

  loop nIter history0
