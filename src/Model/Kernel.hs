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
  , nwRegressionMulti
  , KernelRidgeFit (..)
  , kernelRidge
  , predictKernelRidge
  , gridSearchBandwidth
    -- * 多出力 (主 API)
  , KernelRidgeFitMulti (..)
  , kernelRidgeMulti
  , predictKernelRidgeMulti
  , fittedKernelRidgeMulti
  , r2Multi
  , autoTuneKernelRidgeMulti
  , defaultHGrid
  , defaultLamGrid
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
-- | 単出力 NW (多出力 'nwRegressionMulti' に Y を 1 列行列化して委譲)。
nwRegression :: Kernel -> Double
             -> V.Vector Double -> V.Vector Double
             -> V.Vector Double -> V.Vector Double
nwRegression kern h xs ys xNew =
  let yMat = LA.asColumn (LA.fromList (V.toList ys))
      mat  = nwRegressionMulti kern h xs yMat xNew
  in V.fromList (LA.toList (LA.flatten (mat LA.¿ [0])))

-- | 多出力 NW: 同じ重み行列を全 q 列で再利用。
-- W は (m × n)、Y は (n × q)、戻り値 = (W·Y) を行ごとに正規化した (m × q)。
nwRegressionMulti :: Kernel -> Double
                  -> V.Vector Double      -- xs (n)
                  -> LA.Matrix Double     -- ys (n × q)
                  -> V.Vector Double      -- xNew (m)
                  -> LA.Matrix Double     -- (m × q)
nwRegressionMulti kern h xs ys xNew =
  let n  = V.length xs
      m  = V.length xNew
      q  = LA.cols ys
      wMat = LA.fromLists
               [ [ kernelEval kern ((xStar - xi) / h)
                 | xi <- V.toList xs ]
               | xStar <- V.toList xNew ]   -- (m × n)
      num  = wMat LA.<> ys                  -- (m × q)
      dens = LA.toList (wMat LA.#> LA.konst 1 n)
      rows = [ if d == 0 then replicate q 0
                 else [ (num `LA.atIndex` (i, j)) / d | j <- [0 .. q - 1] ]
             | (i, d) <- zip [0 .. m - 1] dens ]
  in LA.fromLists rows

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

-- | Kernel Ridge regression (単出力)。多出力 'kernelRidgeMulti' に
-- Y を 1 列行列化して委譲し、α 行列の列 0 を取り出す。
kernelRidge :: Kernel -> Double -> Double
            -> V.Vector Double -> V.Vector Double
            -> KernelRidgeFit
kernelRidge kern h lam xs ys =
  let yMat = LA.asColumn (LA.fromList (V.toList ys))
      mf   = kernelRidgeMulti kern h lam xs yMat
      a    = LA.flatten (krmAlpha mf LA.¿ [0])
  in KernelRidgeFit kern h lam xs a

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

-- | 学習点での予測 (= ŷ_train)。
fittedKernelRidgeMulti :: KernelRidgeFitMulti -> LA.Matrix Double
fittedKernelRidgeMulti fit = predictKernelRidgeMulti fit (krmXs fit)

-- | 多出力 R² (q ベクトル)。Y 観測, Ŷ 予測, n×q 同形。
r2Multi :: LA.Matrix Double -> LA.Matrix Double -> V.Vector Double
r2Multi ys yhat =
  let n  = LA.rows ys
      q  = LA.cols ys
      colR2 j =
        let yc  = LA.toList (LA.flatten (ys     LA.¿ [j]))
            yhc = LA.toList (LA.flatten (yhat   LA.¿ [j]))
            mu  = sum yc / fromIntegral n
            sst = sum [(y - mu)^(2::Int) | y <- yc]
            sse = sum [(y - p)^(2::Int) | (y, p) <- zip yc yhc]
        in if sst == 0 then 0 else 1 - sse / sst
  in V.fromList [ colR2 j | j <- [0 .. q - 1] ]

-- | LOOCV 解析解で (h, λ) 同時グリッドサーチ。Hat 行列の対角を 1 回計算し
-- 全 q 出力の LOO 残差を一括評価。
--
-- 戻り値: (best fit, best h, best λ, best mean LOO MSE)
autoTuneKernelRidgeMulti
  :: Kernel
  -> V.Vector Double      -- xs (n)
  -> LA.Matrix Double     -- ys (n × q)
  -> [Double]             -- h candidates
  -> [Double]             -- λ candidates
  -> (KernelRidgeFitMulti, Double, Double, Double)
autoTuneKernelRidgeMulti kern xs ys hs lams =
  let n   = V.length xs
      q   = LA.cols ys
      tot = fromIntegral (n * q) :: Double
      score h lam =
        let kMat = gramMatrix kern h xs
            regK = kMat + LA.scale lam (LA.ident n)
            ainv = LA.inv regK
            hat  = kMat LA.<> ainv          -- (n × n)
            diagH = LA.takeDiag hat
            yhat = hat LA.<> ys             -- (n × q)
            res  = ys - yhat                -- (n × q)
            -- LOO 残差: r_i / (1 - H_ii)、列方向ブロードキャスト
            denom = LA.cmap (\h_ii -> 1 - h_ii) diagH
            invDenom = LA.cmap (\d -> if abs d < 1e-10 then 0 else 1/d) denom
            scaler = LA.fromColumns (replicate q invDenom)
            looR  = res * scaler
            sse   = LA.sumElements (looR * looR)
        in sse / tot
      grid = [ (h, lam, score h lam) | h <- hs, lam <- lams ]
      best@(bestH, bestL, bestS) = head [ p | p@(_,_,s) <- grid
                                             , s == minimum (map (\(_,_,x) -> x) grid) ]
      _ = best
      fit  = kernelRidgeMulti kern bestH bestL xs ys
  in (fit, bestH, bestL, bestS)

-- | log-spaced bandwidth 候補。`defaultHGrid xs` で xs のレンジに合わせた 30 候補。
defaultHGrid :: V.Vector Double -> [Double]
defaultHGrid xs =
  let xv  = V.toList xs
      mn  = minimum xv
      mx  = maximum xv
      rng = mx - mn
      lo  = max 1e-3 (rng / 100)
      hi  = max (lo * 10) rng
      n   = 30
      lLo = log lo
      lHi = log hi
      step = (lHi - lLo) / fromIntegral (n - 1)
  in [ exp (lLo + fromIntegral i * step) | i <- [0 .. n - 1 :: Int] ]

-- | log-spaced λ 候補 (10 値、1e-6 .. 1e0)。
defaultLamGrid :: [Double]
defaultLamGrid =
  let n = 10
      lLo = log 1e-6
      lHi = log 1e0
      step = (lHi - lLo) / fromIntegral (n - 1)
  in [ exp (lLo + fromIntegral i * step) | i <- [0 .. n - 1 :: Int] ]
