{-# LANGUAGE OverloadedStrings #-}
-- | Kernel regression — Nadaraya-Watson and kernel ridge regression.
--
--   * 'Kernel'        — RBF / Matérn / triangular / Epanechnikov kernel
--     functions.
--   * 'nwRegression'  — Nadaraya-Watson (kernel-weighted moving average).
--   * 'kernelRidge'   — kernel ridge regression
--     @ŷ(x*) = k(x*)ᵀ (K + λI)⁻¹ y@.
--
-- Both are non-parametric smooth nonlinear regressors. Unlike 'Model.GP',
-- they do not produce uncertainty estimates.
module Model.Kernel
  ( Kernel (..)
  , kernelEval
  , kernelFromSqDist
  , nwRegression
  , nwRegressionMulti
  , KernelRidgeFit (..)
  , kernelRidge
  , predictKernelRidge
  , gridSearchBandwidth
  , autoBandwidthBrent
    -- * Multi-output (1D input, multiple Y columns)
  , KernelRidgeFitMulti (..)
  , kernelRidgeMulti
  , predictKernelRidgeMulti
  , fittedKernelRidgeMulti
  , r2Multi
  , autoTuneKernelRidgeMulti
  , defaultHGrid
  , defaultLamGrid
    -- * Multi-input (primary API; X is @n × p@, Y is @n × q@)
  , gramMatrixMV
  , gramMatrixMVXY
  , KernelRidgeFitMV (..)
  , kernelRidgeMV
  , predictKernelRidgeMV
  , fittedKernelRidgeMV
  , nwRegressionMV
  ) where

import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import qualified Optim.LineSearch as LS
import qualified Optim.Common     as OC
import qualified Stat.KernelDist  as KD
import qualified Stat.Cholesky    as Chol

-- ---------------------------------------------------------------------------
-- カーネル関数
-- ---------------------------------------------------------------------------

-- | Supported kernels. The bandwidth @h@ is passed separately at the
-- call site.
data Kernel
  = Gaussian       -- ^ @exp(-u²/2)@ (= RBF, infinite support).
  | Epanechnikov   -- ^ @0.75 (1-u²)@ on @|u| ≤ 1@.
  | Triangular     -- ^ @1 - |u|@ on @|u| ≤ 1@.
  | Uniform        -- ^ @0.5@ on @|u| ≤ 1@ (coarsest).
  | TriCube        -- ^ @(1-|u|³)³@ on @|u| ≤ 1@.
  deriving (Show, Eq)

-- | Evaluate the kernel at scaled squared distance @s = ‖x − x'‖² / h²@.
-- Generalizes 'kernelEval' to multivariate inputs: every supported
-- kernel is radially symmetric, so the kernel value depends only on
-- @‖x − x'‖ / h@.
--
-- For the Gaussian kernel this avoids the redundant @sqrt@; for kernels
-- with bounded support (Epanechnikov / Triangular / Uniform / TriCube)
-- the boundary check uses @s ≤ 1@.
kernelFromSqDist :: Kernel -> Double -> Double
kernelFromSqDist k s = case k of
  Gaussian     -> exp (-0.5 * s) / sqrt (2 * pi)
  Epanechnikov -> if s <= 1 then 0.75 * (1 - s) else 0
  Triangular   -> if s <= 1 then 1 - sqrt s else 0
  Uniform      -> if s <= 1 then 0.5 else 0
  TriCube      -> if s <= 1
                    then let u = sqrt s
                             t = 1 - u * u * u
                         in t * t * t
                    else 0

-- | Evaluate the kernel at @u = (x - x_i) / h@.
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

-- | Single-output Nadaraya-Watson kernel regression.
--
-- @ŷ(x*) = Σᵢ K_h(x* - xᵢ) yᵢ / Σᵢ K_h(x* - xᵢ)@
--
-- Delegates to 'nwRegressionMulti' by promoting @y@ to a one-column
-- matrix.
nwRegression :: Kernel
             -> Double             -- ^ Bandwidth @h@ (@> 0@).
             -> V.Vector Double    -- ^ Training inputs.
             -> V.Vector Double    -- ^ Training targets.
             -> V.Vector Double    -- ^ Prediction inputs.
             -> V.Vector Double    -- ^ Predictions.
nwRegression kern h xs ys xNew =
  let yMat = LA.asColumn (LA.fromList (V.toList ys))
      mat  = nwRegressionMulti kern h xs yMat xNew
  in V.fromList (LA.toList (LA.flatten (mat LA.¿ [0])))

-- | Multi-output Nadaraya-Watson: reuse the same weight matrix across
-- every output column. With @W@ of shape @m × n@ and @Y@ of shape
-- @n × q@, the result is the row-normalized product @W · Y@ of shape
-- @m × q@.
nwRegressionMulti :: Kernel
                  -> Double               -- ^ Bandwidth @h@.
                  -> V.Vector Double      -- ^ Training inputs (length @n@).
                  -> LA.Matrix Double     -- ^ Training response @Y@ (@n × q@).
                  -> V.Vector Double      -- ^ Prediction inputs (length @m@).
                  -> LA.Matrix Double     -- ^ Predictions (@m × q@).
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

-- | Kernel ridge regression fit; carries everything needed to predict.
data KernelRidgeFit = KernelRidgeFit
  { krKernel :: Kernel
  , krH      :: Double
  , krLambda :: Double
  , krXs     :: V.Vector Double   -- ^ Training inputs.
  , krAlpha  :: LA.Vector Double  -- ^ Solution @α = (K + λI)⁻¹ y@.
  } deriving (Show)

-- | Build the Gram matrix @K_{ij} = K_h(x_i - x_j)@.
gramMatrix :: Kernel -> Double -> V.Vector Double -> LA.Matrix Double
gramMatrix kern h xs =
  let n = V.length xs
      xv = V.toList xs
  in (n LA.>< n)
       [ kernelEval kern ((xi - xj) / h)
       | xi <- xv, xj <- xv ]

-- | Single-output kernel ridge regression. Delegates to
-- 'kernelRidgeMulti' by promoting @y@ to a one-column matrix and taking
-- column 0 of the resulting @α@ matrix.
kernelRidge :: Kernel
            -> Double             -- ^ Bandwidth @h@.
            -> Double             -- ^ Ridge penalty @λ@.
            -> V.Vector Double    -- ^ Training inputs.
            -> V.Vector Double    -- ^ Training targets.
            -> KernelRidgeFit
kernelRidge kern h lam xs ys =
  let yMat = LA.asColumn (LA.fromList (V.toList ys))
      mf   = kernelRidgeMulti kern h lam xs yMat
      a    = LA.flatten (krmAlpha mf LA.¿ [0])
  in KernelRidgeFit kern h lam xs a

-- | Predict at new inputs from a 'KernelRidgeFit'.
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

-- | Pick the bandwidth @h@ by leave-one-out cross-validation. Simple
-- grid search: returns the candidate with the smallest LOO RMSE.
gridSearchBandwidth
  :: Kernel
  -> V.Vector Double      -- ^ Training inputs.
  -> V.Vector Double      -- ^ Training targets.
  -> [Double]             -- ^ Candidate bandwidths.
  -> (Double, Double)     -- ^ @(best h, best LOO RMSE)@.
gridSearchBandwidth kern xs ys hs =
  let results = [(h, looErrNW kern xs ys h) | h <- hs]
      best = head [ pair | pair <- results
                         , snd pair == minimum (map snd results) ]
  in best

-- | NW LOO-CV loss as a continuous function of @h@; shared with
-- 'autoBandwidthBrent'.
looErrNW :: Kernel -> V.Vector Double -> V.Vector Double -> Double -> Double
looErrNW kern xs ys h =
  let n = V.length xs
      yPred = V.imap
        (\i _ ->
          let xs'  = V.ifilter (\j _ -> j /= i) xs
              ys'  = V.ifilter (\j _ -> j /= i) ys
              xi   = xs V.! i
              pred = nwRegression kern h xs' ys' (V.singleton xi)
          in V.head pred)
        xs
      err = V.zipWith (\y yh -> (y - yh)^(2::Int)) ys yPred
  in sqrt (V.sum err / fromIntegral n)

-- | Continuously optimize the bandwidth @h@ with Brent's method
-- (minimizing the LOO-CV loss). Assumes the bracket @[h_lo, h_hi]@ is
-- unimodal. Avoids enumerating discrete candidates the way
-- 'gridSearchBandwidth' does.
--
-- Returns @(best h, best LOO RMSE)@.
autoBandwidthBrent
  :: Kernel
  -> V.Vector Double    -- ^ Training inputs.
  -> V.Vector Double    -- ^ Training targets.
  -> Double             -- ^ Lower bound @h_lo@.
  -> Double             -- ^ Upper bound @h_hi@.
  -> (Double, Double)
autoBandwidthBrent kern xs ys hLo hHi =
  let cfg = LS.defaultBrentConfig { LS.bcMaxIter = 80, LS.bcTol = 1e-6 }
      result = LS.brent cfg (\[h] -> looErrNW kern xs ys h) hLo hHi
      hStar  = head (OC.orBest result)
  in (hStar, OC.orValue result)

-- ---------------------------------------------------------------------------
-- 多出力 Kernel Ridge (Phase T2)
-- ---------------------------------------------------------------------------

-- | Multi-output kernel ridge regression. With @Y@ of shape @n × q@,
-- solves each column independently but shares the Gram matrix @K@.
data KernelRidgeFitMulti = KernelRidgeFitMulti
  { krmKernel :: Kernel
  , krmH      :: Double
  , krmLambda :: Double
  , krmXs     :: V.Vector Double
  , krmAlpha  :: LA.Matrix Double   -- α (n × q)
  } deriving (Show)

-- | Solve @(K + λI)⁻¹ Y@ once and reuse for every column (fast).
kernelRidgeMulti :: Kernel -> Double -> Double
                 -> V.Vector Double -> LA.Matrix Double
                 -> KernelRidgeFitMulti
kernelRidgeMulti kern h lam xs ys =
  let n     = V.length xs
      kMat  = gramMatrix kern h xs
      regK  = kMat + LA.scale lam (LA.ident n)
      -- regK is SPD (K is PSD, λI is PD). Use Cholesky-based solve;
      -- jitter retry handles ill-conditioned bandwidths.
      alpha = Chol.cholSolveJitter regK ys
  in KernelRidgeFitMulti kern h lam xs alpha

-- | Predict @Ŷ@ for new inputs from a 'KernelRidgeFitMulti'.
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

-- | Fitted values at the training inputs (= @ŷ_train@).
fittedKernelRidgeMulti :: KernelRidgeFitMulti -> LA.Matrix Double
fittedKernelRidgeMulti fit = predictKernelRidgeMulti fit (krmXs fit)

-- | Multi-output R² returned as a length-@q@ vector. @Y@ observed and
-- @Ŷ@ predicted both have shape @n × q@.
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

-- | Joint @(h, λ)@ grid search using the closed-form LOOCV. Computes the
-- hat-matrix diagonal once per
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

-- | Log-spaced bandwidth candidates. @defaultHGrid xs@ produces 30
-- candidates spanning the range of @xs@.
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

-- | Log-spaced ridge-penalty candidates (10 values from 1e-6 to 1).
defaultLamGrid :: [Double]
defaultLamGrid =
  let n = 10
      lLo = log 1e-6
      lHi = log 1e0
      step = (lHi - lLo) / fromIntegral (n - 1)
  in [ exp (lLo + fromIntegral i * step) | i <- [0 .. n - 1 :: Int] ]

-- ---------------------------------------------------------------------------
-- Multi-input (multivariate X) API
--
-- These functions take @X@ as an @n × p@ matrix (rows = samples) and use a
-- single shared bandwidth @h@ across every input dimension. Distance
-- matrices are computed via 'Stat.KernelDist' (BLAS GEMM) and the kernel
-- function is applied element-wise via 'LA.cmap'; no list traversals over
-- the @O(n²)@ pair set.
--
-- For axis-specific bandwidths, scale columns of @X@ by @1 / h_d@ before
-- calling these functions.
-- ---------------------------------------------------------------------------

-- | Multi-input Gram matrix @K[i, j] = κ(‖X[i,:] − X[j,:]‖ / h)@.
gramMatrixMV :: Kernel -> Double -> LA.Matrix Double -> LA.Matrix Double
gramMatrixMV kern h x =
  let h2 = h * h
      d2 = KD.pairwiseSqDist x
  in LA.cmap (\s -> kernelFromSqDist kern (s / h2)) d2

-- | Multi-input cross Gram matrix @K[i, j] = κ(‖X[i,:] − Y[j,:]‖ / h)@.
gramMatrixMVXY
  :: Kernel -> Double
  -> LA.Matrix Double   -- ^ Query @X_*@ (@m × p@).
  -> LA.Matrix Double   -- ^ Training @X@ (@n × p@).
  -> LA.Matrix Double   -- ^ Result (@m × n@).
gramMatrixMVXY kern h xs ts =
  let h2 = h * h
      d2 = KD.pairwiseSqDistXY xs ts
  in LA.cmap (\s -> kernelFromSqDist kern (s / h2)) d2

-- | Multi-input kernel ridge fit. Holds the training matrix and the
-- solution coefficients; @α@ has shape @n × q@.
data KernelRidgeFitMV = KernelRidgeFitMV
  { krmvKernel :: Kernel
  , krmvH      :: Double
  , krmvLambda :: Double
  , krmvXs     :: LA.Matrix Double  -- ^ Training inputs (@n × p@).
  , krmvAlpha  :: LA.Matrix Double  -- ^ @(K + λI)⁻¹ Y@ (@n × q@).
  } deriving (Show)

-- | Multi-input multi-output kernel ridge regression.
--
-- @α = (K + λI)⁻¹ Y@ with @K = gramMatrixMV kern h X@. Solving once and
-- reusing across the @q@ output columns.
kernelRidgeMV
  :: Kernel
  -> Double                 -- ^ Bandwidth @h@.
  -> Double                 -- ^ Ridge penalty @λ@.
  -> LA.Matrix Double       -- ^ Training inputs @X@ (@n × p@).
  -> LA.Matrix Double       -- ^ Training response @Y@ (@n × q@).
  -> KernelRidgeFitMV
kernelRidgeMV kern h lam x y =
  let n     = LA.rows x
      kMat  = gramMatrixMV kern h x
      regK  = kMat + LA.scale lam (LA.ident n)
      -- SPD: K + λI. Use Cholesky-based solve.
      alpha = Chol.cholSolveJitter regK y
  in KernelRidgeFitMV kern h lam x alpha

-- | Predict @Ŷ = K_* α@ for new query inputs (@m × p@). Output shape is
-- @m × q@.
predictKernelRidgeMV :: KernelRidgeFitMV -> LA.Matrix Double -> LA.Matrix Double
predictKernelRidgeMV fit xNew =
  gramMatrixMVXY (krmvKernel fit) (krmvH fit) xNew (krmvXs fit)
    LA.<> krmvAlpha fit

-- | Fitted values at the training inputs.
fittedKernelRidgeMV :: KernelRidgeFitMV -> LA.Matrix Double
fittedKernelRidgeMV fit = predictKernelRidgeMV fit (krmvXs fit)

-- | Multi-input multi-output Nadaraya-Watson regression.
--
-- @ŷ(x*) = (Σⱼ K_h(x* − xⱼ) yⱼ) / Σⱼ K_h(x* − xⱼ)@, computed for every
-- query row in one pass via @W = K(X_*, X)@ then @W Y / row-sums@.
nwRegressionMV
  :: Kernel
  -> Double                 -- ^ Bandwidth @h@.
  -> LA.Matrix Double       -- ^ Training inputs @X@ (@n × p@).
  -> LA.Matrix Double       -- ^ Training response @Y@ (@n × q@).
  -> LA.Matrix Double       -- ^ Query inputs @X_*@ (@m × p@).
  -> LA.Matrix Double       -- ^ Predictions (@m × q@).
nwRegressionMV kern h xs ys xNew =
  let wMat   = gramMatrixMVXY kern h xNew xs            -- m × n
      num    = wMat LA.<> ys                             -- m × q
      onesN  = LA.konst 1 (LA.cols wMat) :: LA.Vector Double
      denom  = wMat LA.#> onesN                          -- m
      safe   = LA.cmap (\d -> if d == 0 then 1 else 1 / d) denom
      scaler = LA.diag safe                              -- m × m
  in scaler LA.<> num
