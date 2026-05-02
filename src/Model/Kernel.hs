{-# LANGUAGE OverloadedStrings #-}
-- | カーネル回帰 (Nadaraya-Watson) と Kernel Ridge regression。
--
-- - 'Kernel': RBF / Matérn / 三角 / Epanechnikov などのカーネル関数
-- - 'nwRegression': Nadaraya-Watson (重み付き移動平均)
-- - 'kernelRidge': Kernel Ridge regression: ŷ(x*) = k(x*)ᵀ (K + λI)⁻¹ y
--
-- どちらも非パラメトリックで滑らかな非線形回帰。
-- 既存の `Model.GP` (Gaussian Process) とは異なり、不確実性は出さない。
module Model.Kernel
  ( Kernel (..)
  , kernelEval
  , nwRegression
  , KernelRidgeFit (..)
  , kernelRidge
  , predictKernelRidge
  , gridSearchBandwidth
    -- * 多出力
  , KernelRidgeFitMulti (..)
  , kernelRidgeMulti
  , predictKernelRidgeMulti
  ) where

import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA

-- ---------------------------------------------------------------------------
-- カーネル関数
-- ---------------------------------------------------------------------------

-- | サポートしているカーネル。bandwidth h は外で渡す。
data Kernel
  = Gaussian       -- ^ exp(-u²/2)        (= RBF, 無限サポート)
  | Epanechnikov   -- ^ 0.75(1-u²) on |u|≤1
  | Triangular     -- ^ (1-|u|) on |u|≤1
  | Uniform        -- ^ 0.5 on |u|≤1     (最も粗い)
  | TriCube        -- ^ (1-|u|³)³ on |u|≤1
  deriving (Show, Eq)

-- | u = (x - x_i) / h で評価。
kernelEval :: Kernel -> Double -> Double
kernelEval k u = case k of
  Gaussian     -> exp (-0.5 * u * u) / sqrt (2 * pi)
  Epanechnikov -> if abs u <= 1 then 0.75 * (1 - u * u) else 0
  Triangular   -> if abs u <= 1 then 1 - abs u else 0
  Uniform      -> if abs u <= 1 then 0.5 else 0
  TriCube      -> if abs u <= 1
                    then let t = 1 - (abs u)^(3::Int)
                         in t * t * t
                    else 0

-- ---------------------------------------------------------------------------
-- Nadaraya-Watson
-- ---------------------------------------------------------------------------

-- | Nadaraya-Watson カーネル回帰。
--
-- ŷ(x*) = Σᵢ K_h(x* - xᵢ) yᵢ / Σᵢ K_h(x* - xᵢ)
--
-- 引数:
--   * @kern@   — カーネル
--   * @h@      — bandwidth (h > 0)
--   * @xs@, @ys@ — 観測
--   * @xNew@   — 予測点
nwRegression :: Kernel -> Double
             -> V.Vector Double -> V.Vector Double
             -> V.Vector Double -> V.Vector Double
nwRegression kern h xs ys xNew = V.map predict xNew
  where
    predict xStar =
      let weights = V.map (\xi -> kernelEval kern ((xStar - xi) / h)) xs
          num     = V.sum (V.zipWith (*) weights ys)
          den     = V.sum weights
      in if den == 0 then 0 else num / den

-- ---------------------------------------------------------------------------
-- Kernel Ridge regression
-- ---------------------------------------------------------------------------

-- | Kernel Ridge regression のフィット結果。予測時に使う情報を保持。
data KernelRidgeFit = KernelRidgeFit
  { krKernel :: Kernel
  , krH      :: Double
  , krLambda :: Double
  , krXs     :: V.Vector Double         -- 訓練点
  , krAlpha  :: LA.Vector Double        -- α = (K + λI)⁻¹ y
  } deriving (Show)

-- | Gram 行列 K_{ij} = K_h(x_i - x_j) を構築。
gramMatrix :: Kernel -> Double -> V.Vector Double -> LA.Matrix Double
gramMatrix kern h xs =
  let n = V.length xs
      xv = V.toList xs
  in (n LA.>< n)
       [ kernelEval kern ((xi - xj) / h)
       | xi <- xv, xj <- xv ]

-- | Kernel Ridge regression: α = (K + λ I)⁻¹ y、予測 ŷ(x*) = k(x*)ᵀ α。
kernelRidge :: Kernel -> Double -> Double
            -> V.Vector Double -> V.Vector Double
            -> KernelRidgeFit
kernelRidge kern h lam xs ys =
  let n     = V.length xs
      kMat  = gramMatrix kern h xs
      regK  = kMat + LA.scale lam (LA.ident n)
      yV    = LA.fromList (V.toList ys)
      alpha = LA.flatten (regK LA.<\> LA.asColumn yV)
  in KernelRidgeFit kern h lam xs alpha

predictKernelRidge :: KernelRidgeFit -> V.Vector Double -> V.Vector Double
predictKernelRidge fit xNew =
  V.map predict xNew
  where
    xs    = krXs fit
    h     = krH fit
    kern  = krKernel fit
    alpha = krAlpha fit
    predict xStar =
      let kVec = LA.fromList
                   [ kernelEval kern ((xStar - xi) / h)
                   | xi <- V.toList xs ]
      in kVec LA.<.> alpha

-- ---------------------------------------------------------------------------
-- Bandwidth selection
-- ---------------------------------------------------------------------------

-- | LOO-CV (Leave-One-Out Cross Validation) で bandwidth h を選ぶ。
-- 候補 hs から RMSE 最小のものを返す (簡易グリッドサーチ)。
gridSearchBandwidth
  :: Kernel
  -> V.Vector Double      -- xs
  -> V.Vector Double      -- ys
  -> [Double]             -- 候補 h リスト
  -> (Double, Double)     -- (best h, best LOO RMSE)
gridSearchBandwidth kern xs ys hs =
  let n      = V.length xs
      looErr h =
        let yPred = V.imap
              (\i _ ->
                let xs'  = V.ifilter (\j _ -> j /= i) xs
                    ys'  = V.ifilter (\j _ -> j /= i) ys
                    xi   = xs V.! i
                    pred = nwRegression kern h xs' ys' (V.singleton xi)
                in V.head pred)
              xs
            err  = V.zipWith (\y yh -> (y - yh)^(2::Int)) ys yPred
        in sqrt (V.sum err / fromIntegral n)
      results = [(h, looErr h) | h <- hs]
      best = head [ pair | pair <- results
                         , snd pair == minimum (map snd results) ]
  in best

-- ---------------------------------------------------------------------------
-- 多出力 Kernel Ridge (Phase T2)
-- ---------------------------------------------------------------------------

-- | 多出力 Kernel Ridge: Y は n × q。各列を独立に解くが、Gram 行列 K は共有。
data KernelRidgeFitMulti = KernelRidgeFitMulti
  { krmKernel :: Kernel
  , krmH      :: Double
  , krmLambda :: Double
  , krmXs     :: V.Vector Double
  , krmAlpha  :: LA.Matrix Double   -- α (n × q)
  } deriving (Show)

-- | (K + λI)⁻¹ Y を 1 回計算で全列処理 (高速)。
kernelRidgeMulti :: Kernel -> Double -> Double
                 -> V.Vector Double -> LA.Matrix Double
                 -> KernelRidgeFitMulti
kernelRidgeMulti kern h lam xs ys =
  let n     = V.length xs
      kMat  = gramMatrix kern h xs
      regK  = kMat + LA.scale lam (LA.ident n)
      alpha = regK LA.<\> ys              -- n × q
  in KernelRidgeFitMulti kern h lam xs alpha

predictKernelRidgeMulti :: KernelRidgeFitMulti -> V.Vector Double
                        -> LA.Matrix Double
predictKernelRidgeMulti fit xNew =
  let xs    = krmXs fit
      h     = krmH fit
      kern  = krmKernel fit
      alpha = krmAlpha fit
      kMat  = LA.fromLists
                [ [ kernelEval kern ((xStar - xi) / h)
                  | xi <- V.toList xs ]
                | xStar <- V.toList xNew ]
  in kMat LA.<> alpha
