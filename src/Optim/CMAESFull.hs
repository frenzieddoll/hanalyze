-- | フルランク CMA-ES (Hansen 2016 Tutorial 完全版)。
--
-- 既存の `Optim.CMAES` は簡易対角版。本モジュールは:
--
-- - フルランク共分散行列 C の rank-1 + rank-μ 更新
-- - 進化パス p_σ, p_c の累積 (path cumulation)
-- - 共分散の固有分解で B, D を取得 (周期的に再計算してコスト削減)
-- - CSA (Cumulative Step-size Adaptation) でステップ幅 σ 更新
-- - h_σ (Heaviside) で巨大ジャンプ時の C 更新を抑制
--
-- パラメータは Hansen 2016 の標準値を使用。
module Optim.CMAESFull
  ( CMAESFConfig (..)
  , defaultCMAESFConfig
  , runCMAESFull
  , runCMAESFullWith
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWCD
import qualified Numeric.LinearAlgebra as LA
import Control.Monad (replicateM, forM)
import Optim.Common

-- | CMA-ES (フルランク) 設定。
data CMAESFConfig = CMAESFConfig
  { cmfStop    :: !StopCriteria
  , cmfSigma0  :: !Double          -- ^ 初期ステップ幅 σ
  , cmfLambda  :: !(Maybe Int)     -- ^ 集団サイズ λ (Nothing なら 4 + ⌊3 ln n⌋)
  , cmfDir     :: !Direction
  } deriving (Show, Eq)

defaultCMAESFConfig :: CMAESFConfig
defaultCMAESFConfig = CMAESFConfig
  { cmfStop   = defaultStopCriteria { stMaxIter = 200, stTolFun = 1e-12 }
  , cmfSigma0 = 0.5
  , cmfLambda = Nothing
  , cmfDir    = Minimize
  }

-- | 既定設定で実行。
runCMAESFull :: ([Double] -> Double)
             -> [Double]
             -> MWC.GenIO
             -> IO OptimResult
runCMAESFull = runCMAESFullWith defaultCMAESFConfig

-- | 設定指定で実行。
runCMAESFullWith :: CMAESFConfig
                 -> ([Double] -> Double)
                 -> [Double]
                 -> MWC.GenIO
                 -> IO OptimResult
runCMAESFullWith cfg fUser m0 gen = do
  let f      = flipFor (cmfDir cfg) fUser
      n      = length m0
      nD     = fromIntegral n :: Double
      lam    = case cmfLambda cfg of
                 Just l  -> l
                 Nothing -> 4 + floor (3 * log nD :: Double)
      mu     = lam `div` 2

      -- 重み (log(μ+1) - log(i))
      wsRaw  = [ log (fromIntegral mu + 1.0) - log (fromIntegral i)
               | i <- [1 .. mu] ]
      wsSum  = sum wsRaw
      ws     = map (/ wsSum) wsRaw
      muEff  = 1 / sum [w*w | w <- ws]

      -- 標準パラメータ (Hansen 2016 Eq. (49)-(58))
      cs     = (muEff + 2) / (nD + muEff + 5)
      ds     = 1 + 2 * max 0 (sqrt ((muEff - 1) / (nD + 1)) - 1) + cs
      cc     = (4 + muEff / nD) / (nD + 4 + 2 * muEff / nD)
      c1     = 2 / ((nD + 1.3)^(2::Int) + muEff)
      cmuRaw = 2 * (muEff - 2 + 1 / muEff) / ((nD + 2)^(2::Int) + muEff)
      cmu    = min (1 - c1) cmuRaw
      eN     = sqrt nD * (1 - 1/(4*nD) + 1/(21*nD*nD))

      m0v    = LA.fromList m0
      cm0    = LA.ident n :: LA.Matrix Double
      ps0    = LA.konst 0 n
      pc0    = LA.konst 0 n
      f0     = f m0
      params = CMAESParams n nD lam mu ws muEff cs ds cc c1 cmu eN
  loop cfg f gen 0 params m0v (cmfSigma0 cfg) cm0 ps0 pc0 f0 [f0]

data CMAESParams = CMAESParams
  { pN      :: !Int
  , pNd     :: !Double
  , pLam    :: !Int
  , pMu     :: !Int
  , pWs     :: ![Double]
  , pMuEff  :: !Double
  , pCs     :: !Double
  , pDs     :: !Double
  , pCc     :: !Double
  , pC1     :: !Double
  , pCmu    :: !Double
  , pEN     :: !Double
  }

-- | 反復本体。
loop :: CMAESFConfig
     -> ([Double] -> Double)
     -> MWC.GenIO
     -> Int
     -> CMAESParams
     -> LA.Vector Double           -- m
     -> Double                      -- σ
     -> LA.Matrix Double            -- C
     -> LA.Vector Double            -- p_σ
     -> LA.Vector Double            -- p_c
     -> Double                      -- best f
     -> [Double]                    -- history
     -> IO OptimResult
loop cfg f gen iter p m sigma c psig pc bestV hist
  | iter >= stMaxIter (cmfStop cfg) = mkRes cfg m bestV hist iter False
  | sigma < 1e-16 = mkRes cfg m bestV hist iter True
  | otherwise = do
      -- 共分散の固有分解 C = B D² Bᵀ
      let (eigs, bMat) = LA.eigSH (LA.sym c)
          dDiag = LA.cmap (\v -> sqrt (max 1e-16 v)) eigs   -- D
          bd    = bMat LA.<> LA.diag dDiag                  -- B·D (n × n)
          -- C^{-1/2} = B · diag(1/d) · Bᵀ (path 更新で使う)
          dInv  = LA.cmap (\d -> 1 / max 1e-16 d) dDiag
          cInvSqrt = bMat LA.<> LA.diag dInv LA.<> LA.tr bMat
          n     = pN p
          lam   = pLam p
      -- λ 個サンプル
      samples <- replicateM lam $ do
        z <- LA.fromList <$> replicateM n (MWCD.standard gen)
        let y = bd LA.#> z
            x = m + LA.scale sigma y
            fx = f (LA.toList x)
        return (x, y, fx)
      let sortedAll = sortBy (comparing (\(_,_,v) -> v)) samples
          topMu = take (pMu p) sortedAll
          ys    = [ y | (_, y, _) <- topMu ]
          fs    = [ v | (_, _, v) <- topMu ]
          newBest = minimum fs
          -- ⟨y⟩_w = Σ w_i y_i
          yMean = LA.fromList
                    [ sum [ (pWs p !! i) * (LA.toList (ys !! i) !! j)
                          | i <- [0 .. pMu p - 1] ]
                    | j <- [0 .. n - 1] ]
          -- 平均更新: m ← m + σ · yMean
          mNew = m + LA.scale sigma yMean
          -- p_σ 更新
          psNew = LA.scale (1 - pCs p) psig +
                  LA.scale (sqrt (pCs p * (2 - pCs p) * pMuEff p))
                           (cInvSqrt LA.#> yMean)
          psNorm = LA.norm_2 psNew
          -- σ 更新 (CSA)
          sigmaN = sigma * exp ((pCs p / pDs p) * (psNorm / pEN p - 1))
          -- h_σ (Heaviside): big jumps を抑制
          gen1   = fromIntegral (iter + 1) :: Double
          chiBound = (1.4 + 2 / (pNd p + 1)) * pEN p
          hSig = if psNorm / sqrt (1 - (1 - pCs p) ** (2 * gen1)) < chiBound
                 then 1 else 0 :: Double
          -- p_c 更新
          pcNew = LA.scale (1 - pCc p) pc +
                  LA.scale (hSig * sqrt (pCc p * (2 - pCc p) * pMuEff p)) yMean
          -- C 更新 (rank-1 + rank-μ)
          ppT  = LA.outer pcNew pcNew
          deltaH = (1 - hSig) * pCc p * (2 - pCc p)
          rankMu = sum [ LA.scale (pWs p !! i)
                                  (LA.outer (ys !! i) (ys !! i))
                       | i <- [0 .. pMu p - 1] ]
          cNew = LA.scale (1 - pC1 p - pCmu p) c
                 + LA.scale (pC1 p) (ppT + LA.scale deltaH c)
                 + LA.scale (pCmu p) rankMu
          bestN  = min bestV newBest
          histN  = bestN : hist
      if abs (bestV - newBest) < stTolFun (cmfStop cfg) && iter > 10
        then mkRes cfg mNew bestN histN (iter + 1) True
        else loop cfg f gen (iter + 1) p mNew sigmaN cNew psNew pcNew bestN histN

mkRes :: CMAESFConfig -> LA.Vector Double -> Double -> [Double]
      -> Int -> Bool -> IO OptimResult
mkRes cfg mV bestV hist iter conv =
  let vUser = case cmfDir cfg of { Minimize -> bestV; Maximize -> negate bestV }
      hU    = case cmfDir cfg of
                Minimize -> reverse hist
                Maximize -> map negate (reverse hist)
  in pure $ OptimResult (LA.toList mV) vUser hU iter conv
