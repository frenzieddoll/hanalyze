{-# LANGUAGE OverloadedStrings #-}
-- | ロバスト GP (重い裾の観測尤度に対応)。
--
-- Gaussian 観測の閉形式 GP では外れ値に弱い。本モジュールでは観測尤度を
-- StudentT / Cauchy に置き換え、IRLS (Iterative Reweighted Least Squares)
-- 風の反復で MAP 推定を行う。
--
-- アルゴリズム (Variational EM の安定版 / Laplace 近似の実用変種):
--
-- 1. f ← 0 (GP 事前平均)
-- 2. 収束まで反復:
--    a. 残差 r = y − f
--    b. 重み w_i を観測尤度から計算:
--       - StudentT(ν, σ):  w_i = (ν + 1) / (ν + (r_i/σ)²)
--       - Cauchy(γ):       w_i = 2 / (1 + (r_i/γ)²)
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
    -- * 多出力 (主 API)
  , RobustGPFitMulti (..)
  , fitGPRobustMulti
  , predictGPRobustMulti
  ) where

import qualified Numeric.LinearAlgebra as LA
import Model.GP
  ( Kernel
  , GPParams (..)
  , kernelFn
  , buildKernelMatrix
  )

-- ---------------------------------------------------------------------------
-- 観測尤度
-- ---------------------------------------------------------------------------

-- | 重い裾の観測尤度。
data RobustLikelihood
  = RGaussian Double            -- ^ Gaussian (σ_n) — 通常 GP に相当 (検算用)
  | RStudentT Double Double     -- ^ StudentT(df=ν, scale=σ) — ν 小さいほど重い裾
  | RCauchy   Double            -- ^ Cauchy(scale=γ) ≡ StudentT(1, γ)
  deriving (Show, Eq)

-- | 残差 r に対する IRLS 重み w(r)。
-- 有効ノイズ分散は σ_eff² / w_i で 1 ステップごとに更新。
likelihoodWeight :: RobustLikelihood -> Double -> Double
likelihoodWeight (RGaussian _)        _ = 1.0
likelihoodWeight (RStudentT nu sigma) r =
  let z = r / sigma
  in (nu + 1) / (nu + z * z)
likelihoodWeight (RCauchy gamma) r =
  let z = r / gamma
  in 2 / (1 + z * z)

-- | 重みのスケーリングに使う基準分散 σ_eff²。
likelihoodScale2 :: RobustLikelihood -> Double
likelihoodScale2 (RGaussian s)      = s * s
likelihoodScale2 (RStudentT _ s)    = s * s
likelihoodScale2 (RCauchy g)        = g * g

-- ---------------------------------------------------------------------------
-- フィット結果
-- ---------------------------------------------------------------------------

data RobustGPFit = RobustGPFit
  { rgpKernel     :: Kernel
  , rgpParams     :: GPParams
  , rgpLik        :: RobustLikelihood
  , rgpTrainX     :: [Double]
  , rgpTrainY     :: [Double]
  , rgpAlpha      :: LA.Vector Double      -- α = (K + σ² W⁻¹)⁻¹ y
  , rgpKyInv      :: LA.Matrix Double      -- (K + σ² W⁻¹)⁻¹
  , rgpWeights    :: LA.Vector Double      -- 収束時の IRLS 重み
  , rgpIters      :: Int                   -- 反復回数
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- フィット
-- ---------------------------------------------------------------------------

-- | IRLS 反復でロバスト GP の MAP を計算。
-- 反復は最大 50 回、収束判定 ||f_new − f||∞ < 1e-6。
fitGPRobust
  :: Kernel
  -> GPParams                    -- ^ カーネルハイパラ (固定 — 別途最適化推奨)
  -> RobustLikelihood
  -> [Double] -> [Double]        -- ^ 訓練 X, Y
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
            kyInv      = LA.inv ky
            alpha      = kyInv LA.#> yV
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

      -- 最終 K_y, α, K_y⁻¹ を再計算 (収束後の重みで)
      wInvDiag' = LA.diag (LA.cmap (\wi -> sigEff2 / max 1e-8 wi) wOpt)
      ky'       = kMatrix `LA.add` wInvDiag'
      kyInv'    = LA.inv ky'
      alpha'    = kyInv' LA.#> yV
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

-- | テスト点での (mean, var of f) を返す。
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
      varList = zipWith3 (\d ksRow wRow -> max 0 (d - LA.dot ksRow wRow))
                  diagKss (LA.toRows kStar) (LA.toRows ws)
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
