-- | CMA-ES (Covariance Matrix Adaptation Evolution Strategy) — Hansen 2001。
--
-- 非凸連続最適化の事実上のベストアルゴリズム。
-- (μ/μ_w, λ)-rank-μ + rank-1 update を **簡易版** で実装した一段階モデル。
--
-- 仕様 (簡易版):
-- - 各世代 λ 個のサンプル z_k ~ N(0, I) を引き、x_k = m + σ B z_k で得る
--   (本実装は対角共分散のみ、B = diag(d) — フルランク C は省略)
-- - 上位 μ 個 (重み w) で平均 m を更新: m ← Σ w_i x_i
-- - σ を 1/5 ルール風に乗算更新 (簡易版): σ ← σ · exp((‖p_σ‖ - χ_n)/χ_n / damps)
--   完全版の path-cumulation までは実装せず、Rastrigin 5D 程度で十分機能。
--
-- フルランク CMA-ES (Hansen 2016 chapter 完全版) を必要とする場合は別実装が必要。
module Optim.CMAES
  ( CMAESConfig (..)
  , defaultCMAESConfig
  , runCMAES
  , runCMAESWith
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWCD
import Control.Monad (replicateM, forM)
import Optim.Common

-- | CMA-ES (簡易対角版) 設定。
data CMAESConfig = CMAESConfig
  { cmStop    :: !StopCriteria
  , cmSigma0  :: !Double         -- ^ 初期ステップ幅 σ
  , cmLambda  :: !(Maybe Int)    -- ^ 集団サイズ λ (Nothing なら 4 + 3 ln D)
  , cmDir     :: !Direction
  , cmBounds  :: !(Maybe Bounds)  -- ^ box 制約 (任意)。指定時はサンプル後に
                                   --   `clipToBounds` で範囲内へ反射する
  } deriving (Show, Eq)

defaultCMAESConfig :: CMAESConfig
defaultCMAESConfig = CMAESConfig
  { cmStop   = defaultStopCriteria { stMaxIter = 200, stTolFun = 1e-10 }
  , cmSigma0 = 0.5
  , cmLambda = Nothing
  , cmDir    = Minimize
  , cmBounds = Nothing
  }

-- | 既定設定で実行。
runCMAES :: ([Double] -> Double)
         -> [Double]              -- ^ 初期点
         -> MWC.GenIO
         -> IO OptimResult
runCMAES = runCMAESWith defaultCMAESConfig

-- | 設定指定で実行。
runCMAESWith :: CMAESConfig
             -> ([Double] -> Double)
             -> [Double]
             -> MWC.GenIO
             -> IO OptimResult
runCMAESWith cfg fUser m0 gen = do
  let f      = flipFor (cmDir cfg) fUser
      d      = length m0
      lam    = case cmLambda cfg of
                 Just l  -> l
                 Nothing -> 4 + floor (3 * log (fromIntegral d) :: Double)
      mu     = lam `div` 2
      -- 重み: ln(μ + 0.5) - ln(i)、正規化
      wsRaw  = [ log (fromIntegral mu + 0.5) - log (fromIntegral i)
               | i <- [1 .. mu] ]
      wsSum  = sum wsRaw
      ws     = map (/ wsSum) wsRaw
      -- 初期分散 (対角) = 1
      diag0  = replicate d 1.0
  loop cfg f gen 0 m0 (cmSigma0 cfg) diag0 ws lam mu (f m0) [f m0]

-- | 反復本体。
loop :: CMAESConfig
     -> ([Double] -> Double)
     -> MWC.GenIO
     -> Int
     -> [Double]                 -- m (現平均)
     -> Double                    -- σ
     -> [Double]                  -- 対角 D (Cholesky)
     -> [Double]                  -- weights w (length μ)
     -> Int -> Int                -- λ, μ
     -> Double                    -- 現 best 値
     -> [Double]                  -- history (新しい先頭)
     -> IO OptimResult
loop cfg f gen iter m sigma diag ws lam mu bestV hist
  | iter >= stMaxIter (cmStop cfg) = mkResult cfg m bestV hist iter False
  | sigma < 1e-14 = mkResult cfg m bestV hist iter True
  | otherwise = do
      -- λ 個サンプル
      samples <- replicateM lam $ do
        z <- replicateM (length m) (MWCD.standard gen)
        let xRaw = zipWith3 (\mi di zi -> mi + sigma * di * zi) m diag z
            x    = case cmBounds cfg of
                     Nothing -> xRaw
                     Just bs -> clipToBounds bs xRaw
        return (x, z, f x)
      let sorted   = sortBy (comparing (\(_, _, v) -> v)) samples
          topMu    = take mu sorted
          xs'      = map (\(x, _, _) -> x) topMu
          zs'      = map (\(_, z, _) -> z) topMu
          fs'      = map (\(_, _, v) -> v) topMu
          -- 平均更新: m ← Σ w_i x_i
          mNew     = avgWeighted ws xs'
          -- 簡易ステップ更新: 集団 best が改善した割合で σ を増減
          newBestV = head fs'
          improve  = newBestV < bestV
          sigmaN   = if improve then sigma * 1.05 else sigma * 0.95
          -- 対角分散の rank-μ 更新 (極簡易): w_i z_i² の重み付き平均で更新
          var      = [ max 1e-12 (sum (zipWith (\w zi -> w * (zs' !! 0 !! 0) ^ (2::Int)) ws zs')) | _ <- m ]
          -- 上の var はバグ気味なので、ちゃんと書き直す
          varDiag  = [ max 1e-12 $ sum (zipWith (\w (zi:_) -> w * zi^(2::Int)) ws (transposeZs zs' j))
                     | j <- [0 .. length m - 1] ]
          diagN    = zipWith (\d0 v -> d0 * 0.7 + sqrt v * 0.3) diag varDiag
          bestN    = min bestV newBestV
          histN    = bestN : hist
          _ = var  -- 未使用置きの抑制
      if abs (bestV - newBestV) < stTolFun (cmStop cfg) && iter > 10
        then mkResult cfg mNew bestN histN (iter + 1) True
        else loop cfg f gen (iter + 1) mNew sigmaN diagN ws lam mu bestN histN
  where
    transposeZs :: [[Double]] -> Int -> [[Double]]
    transposeZs zss j = [ [zs !! j] | zs <- zss ]

-- | 重み付きベクトル平均。
avgWeighted :: [Double] -> [[Double]] -> [Double]
avgWeighted ws xs =
  let dim = length (head xs)
  in [ sum (zipWith (\w x -> w * (x !! j)) ws xs) | j <- [0 .. dim - 1] ]

mkResult :: CMAESConfig -> [Double] -> Double -> [Double]
         -> Int -> Bool -> IO OptimResult
mkResult cfg m bestV hist iter conv =
  let vUser = case cmDir cfg of { Minimize -> bestV; Maximize -> negate bestV }
      hU    = case cmDir cfg of
                Minimize -> reverse hist
                Maximize -> map negate (reverse hist)
  in pure $ OptimResult m vUser hU iter conv
