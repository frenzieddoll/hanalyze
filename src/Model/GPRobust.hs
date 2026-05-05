{-# LANGUAGE OverloadedStrings #-}
-- | Robust GP (heavy-tailed observation likelihoods).
--
-- A closed-form Gaussian-likelihood GP is sensitive to outliers. This
-- module replaces the observation likelihood with Student-t or Cauchy and
-- iterates an IRLS-style scheme (a stable variant of variational EM /
-- Laplace) to obtain a MAP estimate.
--
-- Algorithm:
--
--   1. @f ← 0@ (GP prior mean).
--   2. Iterate until convergence:
--      a. Residual @r = y − f@.
--      b. Compute the per-observation weight:
--         * Student-t @(ν, σ)@:  @w_i = (ν + 1) / (ν + (r_i/σ)²)@.
--         * Cauchy @(γ)@:       @w_i = 2 / (1 + (r_i/γ)²)@.
--    c. 各点の有効ノイズ分散 σ²/w_i (heteroscedastic)
--    d. f ← K (K + σ² W⁻¹)⁻¹ y
-- 3. 予測点 x* で:
--    mean = k_*ᵀ (K + σ² W⁻¹)⁻¹ y
--    var  = k(x*,x*) − k_*ᵀ (K + σ² W⁻¹)⁻¹ k_*
--
-- カーネル関連 ('Kernel', 'GPParams', 'kernelFn') は 'Model.GP' を再利用。
module Model.GPRobust
  ( -- * 観測尤度
    RobustLikelihood (..)
  , -- * フィット結果と推論
    RobustGPFit (..)
  , fitGPRobust
  , predictGPRobust
    -- * Multi-output (primary API)
  , RobustGPFitMulti (..)
  , fitGPRobustMulti
  , predictGPRobustMulti
    -- * Multi-input (primary API; X is @n × p@, Y is @n × q@)
  , RobustGPFitMV (..)
  , fitGPRobustMV
  , predictGPRobustMV
  , RobustGPFitMVMulti (..)
  , fitGPRobustMVMulti
  , predictGPRobustMVMulti
  ) where

import qualified Numeric.LinearAlgebra as LA
import qualified Stat.Cholesky        as Chol
import qualified Stat.KernelDist      as KD
import Model.GP
  ( Kernel
  , GPParams (..)
  , kernelFn
  , buildKernelMatrix
  , buildKernelMatrixMV
  )

-- ---------------------------------------------------------------------------
-- 観測尤度
-- ---------------------------------------------------------------------------

-- | Heavy-tailed observation likelihood.
data RobustLikelihood
  = RGaussian Double            -- ^ Gaussian @(σ_n)@ — equivalent to a
                                --   standard GP (sanity-check baseline).
  | RStudentT Double Double     -- ^ Student-t @(df=ν, scale=σ)@; smaller
                                --   @ν@ means heavier tails.
  | RCauchy   Double            -- ^ Cauchy @(scale=γ)@, equivalent to
                                --   @StudentT(1, γ)@.
  deriving (Show, Eq)

-- | IRLS weight @w(r)@ for residual @r@. The effective noise variance is
-- @σ_eff² / w_i@ at each step.
likelihoodWeight :: RobustLikelihood -> Double -> Double
likelihoodWeight (RGaussian _)        _ = 1.0
likelihoodWeight (RStudentT nu sigma) r =
  let z = r / sigma
  in (nu + 1) / (nu + z * z)
likelihoodWeight (RCauchy gamma) r =
  let z = r / gamma
  in 2 / (1 + z * z)

-- | Reference variance @σ_eff²@ used to scale the IRLS weights.
likelihoodScale2 :: RobustLikelihood -> Double
likelihoodScale2 (RGaussian s)      = s * s
likelihoodScale2 (RStudentT _ s)    = s * s
likelihoodScale2 (RCauchy g)        = g * g

-- ---------------------------------------------------------------------------
-- フィット結果
-- ---------------------------------------------------------------------------

-- | Robust GP fit result.
data RobustGPFit = RobustGPFit
  { rgpKernel  :: Kernel
  , rgpParams  :: GPParams
  , rgpLik     :: RobustLikelihood
  , rgpTrainX  :: [Double]              -- ^ Training inputs.
  , rgpTrainY  :: [Double]              -- ^ Training targets.
  , rgpAlpha   :: LA.Vector Double      -- ^ @α = (K + σ² W⁻¹)⁻¹ y@.
  , rgpKyInv   :: LA.Matrix Double      -- ^ @(K + σ² W⁻¹)⁻¹@ at convergence.
  , rgpWeights :: LA.Vector Double      -- ^ IRLS weights at convergence.
  , rgpIters   :: Int                   -- ^ Number of IRLS iterations executed.
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- フィット
-- ---------------------------------------------------------------------------

-- | Compute the MAP of a robust GP via IRLS iteration. At most 50
-- iterations; convergence when @‖f_new − f‖∞ < 10⁻⁶@.
fitGPRobust
  :: Kernel
  -> GPParams                    -- ^ Kernel hyperparameters (held fixed —
                                 --   optimize them separately).
  -> RobustLikelihood
  -> [Double]                    -- ^ Training @X@.
  -> [Double]                    -- ^ Training @Y@.
  -> RobustGPFit
fitGPRobust ker params lik trainX trainY =
  let n         = length trainX
      kMatrix   = buildKernelMatrix ker params trainX trainX  -- K (n×n)
      yV        = LA.fromList trainY
      sigEff2   = likelihoodScale2 lik
      -- 1 反復: f, w を更新
      step (f, w, _iter) =
        let r          = LA.toList (yV - f)
            wNew'      = [ max 1e-8 (likelihoodWeight lik ri)
                         | ri <- r ]
            wNewVec    = LA.fromList wNew'
            wInvDiag   = LA.diag (LA.fromList [ sigEff2 / wi | wi <- wNew' ])
            ky         = kMatrix `LA.add` wInvDiag
            -- α = (K + σ²W⁻¹)⁻¹ y via SPD Cholesky (replaces inv + matvec).
            alpha      = LA.flatten
                          (Chol.cholSolveJitter ky (LA.asColumn yV))
            fNew       = kMatrix LA.#> alpha
            delta      = LA.maxElement (LA.cmap abs (fNew - f))
        in (fNew, wNewVec, delta)

      maxIters     = 50
      tol          = 1e-6 :: Double

      loop f w iter
        | iter >= maxIters = (f, w, iter)
        | otherwise =
            let (fNew, wNew, delta) = step (f, w, iter)
            in if delta < tol
                 then (fNew, wNew, iter + 1)
                 else loop fNew wNew (iter + 1)

      f0     = LA.fromList (replicate n 0.0)
      w0     = LA.fromList (replicate n 1.0)
      (_fOpt, wOpt, iters) = loop f0 w0 0

      -- 最終 K_y, α, K_y⁻¹ を再計算 (収束後の重みで)。
      -- kyInv は予測時の分散計算で必要なため陽に保持する。
      wInvDiag' = LA.diag (LA.cmap (\wi -> sigEff2 / max 1e-8 wi) wOpt)
      ky'       = kMatrix `LA.add` wInvDiag'
      kyInv'    = Chol.cholSolveJitter ky' (LA.ident n)
      alpha'    = LA.flatten
                  (Chol.cholSolveJitter ky' (LA.asColumn yV))
  in RobustGPFit
       { rgpKernel  = ker
       , rgpParams  = params
       , rgpLik     = lik
       , rgpTrainX  = trainX
       , rgpTrainY  = trainY
       , rgpAlpha   = alpha'
       , rgpKyInv   = kyInv'
       , rgpWeights = wOpt
       , rgpIters   = iters
       }

-- ---------------------------------------------------------------------------
-- 予測
-- ---------------------------------------------------------------------------

-- | Predictive mean and variance of @f@ at the given test points.
-- mean = k_*ᵀ α, var = k(x*,x*) − k_*ᵀ K_y⁻¹ k_*
predictGPRobust :: RobustGPFit -> [Double] -> [(Double, Double)]
predictGPRobust fit testX =
  let ker     = rgpKernel fit
      params  = rgpParams fit
      trainX  = rgpTrainX fit
      kStar   = buildKernelMatrix ker params testX trainX     -- (m, n)
      means   = LA.toList (kStar LA.#> rgpAlpha fit)
      kyInv   = rgpKyInv fit
      diagKss = [ kernelFn ker params x x | x <- testX ]
      ws      = kStar LA.<> kyInv                              -- (m, n)
      -- F1: vectorise per-row dots.
      rowDots = LA.toList (KD.rowDotsAB kStar ws)
      varList = zipWith (\d kw -> max 0 (d - kw)) diagKss rowDots
  in zip means varList

-- ---------------------------------------------------------------------------
-- 多出力 (列ごと IRLS、カーネル行列を共有)
-- ---------------------------------------------------------------------------

-- | 多出力ロバスト GP の結果。q 出力ぶんの 'RobustGPFit' を保持し、
-- カーネル / ハイパラ / 尤度は共通。
data RobustGPFitMulti = RobustGPFitMulti
  { rgmKernel :: Kernel
  , rgmParams :: GPParams
  , rgmLik    :: RobustLikelihood
  , rgmTrainX :: [Double]
  , rgmFits   :: [RobustGPFit]   -- ^ 列ごとの単出力 fit
  } deriving (Show)

-- | 多出力ロバスト GP fit。Y は n × q、各列ごとに IRLS (重みは出力依存)。
fitGPRobustMulti
  :: Kernel
  -> GPParams
  -> RobustLikelihood
  -> [Double]            -- ^ 訓練 X
  -> LA.Matrix Double    -- ^ Y (n × q)
  -> RobustGPFitMulti
fitGPRobustMulti ker params lik trainX yMat =
  let q     = LA.cols yMat
      yCols = [ LA.toList (LA.flatten (yMat LA.¿ [j])) | j <- [0 .. q - 1] ]
      fits  = [ fitGPRobust ker params lik trainX y | y <- yCols ]
  in RobustGPFitMulti ker params lik trainX fits

-- | 多出力ロバスト GP 予測。戻り値: (mean 行列 m × q, 列ごとの分散リスト)。
predictGPRobustMulti :: RobustGPFitMulti -> [Double]
                     -> (LA.Matrix Double, [[Double]])
predictGPRobustMulti mf testX =
  let preds = [ predictGPRobust f testX | f <- rgmFits mf ]
      meansCols = map (map fst) preds
      varsCols  = map (map snd) preds
      meansMat  = LA.fromColumns [ LA.fromList col | col <- meansCols ]
  in (meansMat, varsCols)

-- ---------------------------------------------------------------------------
-- Multi-input (multivariate X) API
-- ---------------------------------------------------------------------------

-- | Robust GP fit with multivariate input. Mirrors 'RobustGPFit' but
-- stores @X@ as an @n × p@ matrix and @y@ as a 'LA.Vector'.
data RobustGPFitMV = RobustGPFitMV
  { rgpmvKernel  :: Kernel
  , rgpmvParams  :: GPParams
  , rgpmvLik     :: RobustLikelihood
  , rgpmvTrainX  :: LA.Matrix Double      -- ^ @n × p@.
  , rgpmvTrainY  :: LA.Vector Double      -- ^ length @n@.
  , rgpmvAlpha   :: LA.Vector Double
  , rgpmvKyInv   :: LA.Matrix Double
  , rgpmvWeights :: LA.Vector Double
  , rgpmvIters   :: Int
  } deriving (Show)

-- | Compute the MAP of a multi-input robust GP via the same IRLS scheme
-- as 'fitGPRobust'. @X@ is @n × p@; @y@ has length @n@.
fitGPRobustMV
  :: Kernel
  -> GPParams
  -> RobustLikelihood
  -> LA.Matrix Double          -- ^ Training @X@ (@n × p@).
  -> LA.Vector Double          -- ^ Training @y@ (length @n@).
  -> RobustGPFitMV
fitGPRobustMV ker params lik trainX yV =
  let n         = LA.rows trainX
      kMatrix   = buildKernelMatrixMV ker params trainX trainX
      sigEff2   = likelihoodScale2 lik
      step (f, w, _iter) =
        let r          = LA.toList (yV - f)
            wNew'      = [ max 1e-8 (likelihoodWeight lik ri) | ri <- r ]
            wNewVec    = LA.fromList wNew'
            wInvDiag   = LA.diag (LA.fromList [ sigEff2 / wi | wi <- wNew' ])
            ky         = kMatrix `LA.add` wInvDiag
            -- α = (K + σ²W⁻¹)⁻¹ y via SPD Cholesky.
            alpha      = LA.flatten
                          (Chol.cholSolveJitter ky (LA.asColumn yV))
            fNew       = kMatrix LA.#> alpha
            delta      = LA.maxElement (LA.cmap abs (fNew - f))
        in (fNew, wNewVec, delta)

      maxIters = 50
      tol      = 1e-6 :: Double

      loop f w iter
        | iter >= maxIters = (f, w, iter)
        | otherwise =
            let (fNew, wNew, delta) = step (f, w, iter)
            in if delta < tol
                 then (fNew, wNew, iter + 1)
                 else loop fNew wNew (iter + 1)

      f0 = LA.fromList (replicate n 0.0)
      w0 = LA.fromList (replicate n 1.0)
      (_fOpt, wOpt, iters) = loop f0 w0 0

      wInvDiag' = LA.diag (LA.cmap (\wi -> sigEff2 / max 1e-8 wi) wOpt)
      ky'       = kMatrix `LA.add` wInvDiag'
      kyInv'    = Chol.cholSolveJitter ky' (LA.ident n)
      alpha'    = LA.flatten
                  (Chol.cholSolveJitter ky' (LA.asColumn yV))
  in RobustGPFitMV
       { rgpmvKernel  = ker
       , rgpmvParams  = params
       , rgpmvLik     = lik
       , rgpmvTrainX  = trainX
       , rgpmvTrainY  = yV
       , rgpmvAlpha   = alpha'
       , rgpmvKyInv   = kyInv'
       , rgpmvWeights = wOpt
       , rgpmvIters   = iters
       }

-- | Predictive mean and variance at multi-input test points (@m × p@).
predictGPRobustMV
  :: RobustGPFitMV -> LA.Matrix Double
  -> (LA.Vector Double, LA.Vector Double)
predictGPRobustMV fit testX =
  let ker     = rgpmvKernel fit
      params  = rgpmvParams fit
      trainX  = rgpmvTrainX fit
      kStar   = buildKernelMatrixMV ker params testX trainX  -- m × n
      means   = kStar LA.#> rgpmvAlpha fit
      kyInv   = rgpmvKyInv fit
      sf      = gpSignalVar params
      diagKss = LA.konst sf (LA.rows testX)
      ws      = kStar LA.<> kyInv                            -- m × n
      -- F1: vectorise per-row dots.
      vars    = LA.cmap (max 0) (diagKss - KD.rowDotsAB kStar ws)
  in (means, vars)

-- | Multi-input multi-output robust GP. Per-column IRLS (weights are
-- output-specific), but the kernel matrix @K@ is shared.
data RobustGPFitMVMulti = RobustGPFitMVMulti
  { rgmvKernel :: Kernel
  , rgmvParams :: GPParams
  , rgmvLik    :: RobustLikelihood
  , rgmvTrainX :: LA.Matrix Double
  , rgmvFits   :: [RobustGPFitMV]
  } deriving (Show)

-- | Fit a multi-input multi-output robust GP. @Y@ has shape @n × q@.
fitGPRobustMVMulti
  :: Kernel
  -> GPParams
  -> RobustLikelihood
  -> LA.Matrix Double          -- ^ Training @X@ (@n × p@).
  -> LA.Matrix Double          -- ^ Training @Y@ (@n × q@).
  -> RobustGPFitMVMulti
fitGPRobustMVMulti ker params lik trainX yMat =
  let q     = LA.cols yMat
      cols  = [ LA.flatten (yMat LA.¿ [j]) | j <- [0 .. q - 1] ]
      fits  = [ fitGPRobustMV ker params lik trainX y | y <- cols ]
  in RobustGPFitMVMulti ker params lik trainX fits

-- | Multi-input multi-output robust GP prediction. Returns the @m × q@
-- mean matrix and a per-column variance vector.
predictGPRobustMVMulti
  :: RobustGPFitMVMulti -> LA.Matrix Double
  -> (LA.Matrix Double, [LA.Vector Double])
predictGPRobustMVMulti mf testX =
  let preds   = [ predictGPRobustMV f testX | f <- rgmvFits mf ]
      meanCs  = map fst preds
      varCs   = map snd preds
      meanMat = LA.fromColumns meanCs
  in (meanMat, varCs)
