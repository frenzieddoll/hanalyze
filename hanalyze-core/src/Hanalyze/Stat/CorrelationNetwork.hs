-- |
-- Module      : Hanalyze.Stat.CorrelationNetwork
-- Description : Graphical Lasso による sparse precision matrix 推定 (相関ネットワーク)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Correlation Network via Graphical Lasso。
--
-- 高次元データの相関構造を sparse precision matrix @Θ = Σ^{-1}@ で
-- 表現する。 「ゼロ要素 ↔ 条件付き独立」 の対応で変数間ネットワークを
-- 推定する。 scikit-learn @GraphicalLasso@、 R @glasso@ 相当。
--
-- ## 最適化
--
-- @
--   max_{Θ ≻ 0}  log det Θ - tr(SΘ) - λ ‖Θ‖_{1,off}
-- @
--
-- ここで @S@ は経験共分散行列、 @λ@ は L1 罰則。 対角は罰しない (FHT 2008
-- 慣例)。
--
-- ## アルゴリズム (Friedman-Hastie-Tibshirani 2008、 block CD)
--
-- 1. @Σ ← S + λI@ で初期化 (対角に λ shrinkage)
-- 2. 各列 @j@ について部分問題:
--    - @W_{11}@ = @Σ@ の row j / col j を除いた部分 (p-1 × p-1)
--    - @s_{12}@ = @S@ の列 j (行 j を除く)
--    - 内部 Lasso: @argmin_β (1/2) β^T W_{11} β - s_{12}^T β + λ |β|_1@
--    - @Σ_{:j} = W_{11} β@ で列を更新 (対角は @S_{jj} + λ@)
-- 3. @Σ@ が収束するまで全列 sweep を反復
-- 4. @Θ = Σ^{-1}@ を計算
--
-- Reference:
--   Friedman, Hastie, Tibshirani (2008) "Sparse inverse covariance
--   estimation with the graphical lasso". Biostatistics 9(3):432-441.
--
-- [English]: Correlation Network via Graphical Lasso.
--
-- Represents the correlation structure of high-dimensional data as a
-- sparse precision matrix @Θ = Σ^{-1}@. Estimates the inter-variable
-- network using the correspondence "zero element ↔ conditional
-- independence". Equivalent to scikit-learn's @GraphicalLasso@ and R's
-- @glasso@.
--
-- ## Optimization
--
-- @
--   max_{Θ ≻ 0}  log det Θ - tr(SΘ) - λ ‖Θ‖_{1,off}
-- @
--
-- Here @S@ is the empirical covariance matrix and @λ@ is the L1 penalty.
-- The diagonal is not penalized (FHT 2008 convention).
--
-- ## Algorithm (Friedman-Hastie-Tibshirani 2008, block CD)
--
-- 1. Initialize @Σ ← S + λI@ (λ shrinkage on the diagonal)
-- 2. For each column @j@, solve the sub-problem:
--    - @W_{11}@ = the part of @Σ@ with row j \/ col j removed (p-1 × p-1)
--    - @s_{12}@ = column j of @S@ (with row j removed)
--    - Inner Lasso: @argmin_β (1/2) β^T W_{11} β - s_{12}^T β + λ |β|_1@
--    - Update the column @Σ_{:j} = W_{11} β@ (diagonal is @S_{jj} + λ@)
-- 3. Repeat the full-column sweep until @Σ@ converges
-- 4. Compute @Θ = Σ^{-1}@
--
-- Reference:
--   Friedman, Hastie, Tibshirani (2008) "Sparse inverse covariance
--   estimation with the graphical lasso". Biostatistics 9(3):432-441.
module Hanalyze.Stat.CorrelationNetwork
  ( GLassoFit (..)
  , graphicalLasso
  , graphicalLassoFromCov
  , empiricalCov
  , nonZeroPrecision
    -- * [日本語]: Pearson 相関ネットワーク (df|-> correlationOf 用) [English]: Pearson correlation network (for df|-> correlationOf)
  , correlationMatrix
  , CorrelationGraph (..)
  ) where

import           Data.Text             (Text)
import qualified Numeric.LinearAlgebra as LA

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

data GLassoFit = GLassoFit
  { glPrecision  :: !(LA.Matrix Double)   -- ^ [日本語]: 推定された Θ (precision) [English]: The estimated Θ (precision)
  , glCovariance :: !(LA.Matrix Double)   -- ^ [日本語]: 推定された Σ = Θ⁻¹ [English]: The estimated Σ = Θ⁻¹
  , glIterations :: !Int                   -- ^ [日本語]: 外側 sweep の反復数 [English]: Number of outer sweep iterations
  , glConverged  :: !Bool                  -- ^ [日本語]: tol 内収束したか [English]: Whether it converged within tol
  , glLambda     :: !Double                -- ^ [日本語]: 使用した λ [English]: The λ used
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- API
-- ---------------------------------------------------------------------------

-- | [日本語]: 経験共分散行列 (= 中央化 + scale 1/(n-1))。
--   [English]: Empirical covariance matrix (= centering + scale 1/(n-1)).
empiricalCov :: LA.Matrix Double -> LA.Matrix Double
empiricalCov x =
  let n    = LA.rows x
      ones = LA.konst 1 n :: LA.Vector Double
      mu   = LA.scale (1 / fromIntegral n) (LA.tr x LA.#> ones)
      xc   = x - LA.asRow mu
      m    = max 1 (n - 1)
  in LA.scale (1 / fromIntegral m) (LA.tr xc LA.<> xc)

-- | [日本語]: Pearson 相関行列 (@X@ n×p → p×p)。 'empiricalCov' を対角の標準偏差で正規化する
--   (@r_ij = Σ_ij / (σ_i σ_j)@)。 分散 0 の列は 0 除算回避で 0 相関扱い。
--   [English]: Pearson correlation matrix (@X@ n×p → p×p). Normalizes
--   'empiricalCov' by the diagonal standard deviations (@r_ij = Σ_ij /
--   (σ_i σ_j)@). Columns with zero variance are treated as zero
--   correlation to avoid division by zero.
correlationMatrix :: LA.Matrix Double -> LA.Matrix Double
correlationMatrix x =
  let cov  = empiricalCov x
      p    = LA.rows cov
      sds  = [ sqrt (cov `LA.atIndex` (i, i)) | i <- [0 .. p - 1] ]
      dInv = LA.diag (LA.fromList [ if s > 1e-12 then 1 / s else 0 | s <- sds ])
  in dInv LA.<> cov LA.<> dInv

-- | [日本語]: 相関ネットワーク (Pearson 相関 + 閾値) の結果 (@df |-> correlationOf thr cols@)。
--   @Plottable@ (@Hanalyze.Plot.ML@) が @|r| > cgThreshold@ の対を辺にしたグラフを描く
--   (無向・向きは便宜上の配置。 因果でない)。 LiNGAM DAG と対比すると間接相関の過剰さが分かる。
--   [English]: Result of a correlation network (Pearson correlation +
--   threshold) (@df |-> correlationOf thr cols@). @Plottable@
--   (@Hanalyze.Plot.ML@) draws a graph with edges for pairs where
--   @|r| > cgThreshold@ (undirected; the direction is just for layout
--   convenience, not causal). Contrasting with a LiNGAM DAG reveals the
--   excess of indirect correlations.
data CorrelationGraph = CorrelationGraph
  { cgCorr      :: !(LA.Matrix Double)   -- ^ [日本語]: p × p Pearson 相関行列 [English]: p × p Pearson correlation matrix
  , cgNames     :: ![Text]               -- ^ [日本語]: 変数名 (列順) [English]: Variable names (column order)
  , cgThreshold :: !Double               -- ^ [日本語]: |r| > この値で辺を張る [English]: An edge is drawn when |r| exceeds this value
  } deriving (Show)

-- | [日本語]: データ行列 @X@ (n × p) から graphical Lasso 推定。 内部で
-- 'empiricalCov' を計算してから 'graphicalLassoFromCov' を呼ぶ。
-- [English]: Estimate graphical Lasso from a data matrix @X@ (n × p).
-- Internally computes 'empiricalCov' and then calls
-- 'graphicalLassoFromCov'.
graphicalLasso
  :: LA.Matrix Double      -- ^ X (n × p)
  -> Double                -- ^ λ
  -> Int                   -- ^ [日本語]: max outer sweeps (推奨 100) [English]: max outer sweeps (recommended 100)
  -> Double                -- ^ [日本語]: tolerance (推奨 1e-4) [English]: tolerance (recommended 1e-4)
  -> GLassoFit
graphicalLasso x lambda maxOuter tol =
  graphicalLassoFromCov (empiricalCov x) lambda maxOuter tol

-- | [日本語]: 経験共分散行列から直接推定 (= 既に共分散を持っているとき向け)。
--   [English]: Estimate directly from the empirical covariance matrix
--   (for when you already have the covariance).
graphicalLassoFromCov
  :: LA.Matrix Double      -- ^ S (p × p)
  -> Double                -- ^ λ
  -> Int -> Double
  -> GLassoFit
graphicalLassoFromCov s lambda maxOuter tol =
  let p = LA.rows s
      -- 初期化: Σ = S + λI (対角 shrinkage)
      sigma0 = s + LA.scale lambda (LA.ident p)
      -- 外側 sweep
      sweep sigma =
        foldl
          (\sigCur j -> updateColumn sigCur s lambda j)
          sigma
          [0 .. p - 1]
      loop !k !sigma
        | k >= maxOuter = (sigma, k, False)
        | otherwise     =
            let sigmaN = sweep sigma
                d      = LA.maxElement (LA.cmap abs (sigmaN - sigma))
            in if d < tol
                 then (sigmaN, k + 1, True)
                 else loop (k + 1) sigmaN
      (sigmaFinal, iters, conv) = loop 0 sigma0
      -- 対角を S + λ にリセット (FHT 慣例)
      sigmaDiag = setDiag sigmaFinal (LA.takeDiag s + LA.konst lambda p)
      theta     = LA.inv sigmaDiag
  in GLassoFit
       { glPrecision  = theta
       , glCovariance = sigmaDiag
       , glIterations = iters
       , glConverged  = conv
       , glLambda     = lambda
       }

-- | [日本語]: 1 列の更新: 内部 Lasso を解いて @Σ@ の j 列 / j 行を上書き。
--   [English]: Update a single column: solve the inner Lasso and overwrite
--   column j \/ row j of @Σ@.
updateColumn :: LA.Matrix Double -> LA.Matrix Double -> Double -> Int
             -> LA.Matrix Double
updateColumn sigma s lambda j =
  let p   = LA.rows sigma
      ids = [i | i <- [0 .. p - 1], i /= j]
      w11 = sigma LA.? ids LA.¿ ids
      s12 = LA.fromList [LA.atIndex s (i, j) | i <- ids]
      beta = innerLassoQuad w11 s12 lambda 200 1e-5
      newCol = w11 LA.#> beta
      sigma' = updateOffDiagColumn sigma j ids (LA.toList newCol)
  in sigma'

-- | [日本語]: 内部 Lasso (quadratic form):
-- @argmin_β (1/2) β^T W β - s^T β + λ |β|_1@
-- coord update: @β_k ← S(s_k - Σ_{l≠k} W_{kl} β_l, λ) / W_{kk}@。
-- [English]: Inner Lasso (quadratic form):
-- @argmin_β (1/2) β^T W β - s^T β + λ |β|_1@
-- coord update: @β_k ← S(s_k - Σ_{l≠k} W_{kl} β_l, λ) / W_{kk}@.
innerLassoQuad
  :: LA.Matrix Double -> LA.Vector Double -> Double -> Int -> Double
  -> LA.Vector Double
innerLassoQuad w sVec lambda maxIter tol =
  let m  = LA.size sVec
      diagW = LA.takeDiag w
      sweep beta =
        foldl
          (\(bAcc, mDelta) k ->
              let wkk = LA.atIndex diagW k
                  wRow = LA.flatten (w LA.? [k])
                  pred_k = wRow LA.<.> bAcc - wkk * LA.atIndex bAcc k
                  rho = LA.atIndex sVec k - pred_k
                  bk' = if wkk <= 0
                          then 0
                          else softT rho lambda / wkk
                  bk  = LA.atIndex bAcc k
                  d   = abs (bk' - bk)
                  bAcc' = updateAt bAcc k bk'
              in (bAcc', max mDelta d))
          (beta, 0)
          [0 .. m - 1]
      loop !k !beta
        | k >= maxIter = beta
        | otherwise    =
            let (betaN, d) = sweep beta
            in if d < tol
                 then betaN
                 else loop (k + 1) betaN
  in loop 0 (LA.konst 0 m)

-- ---------------------------------------------------------------------------
-- ヘルパ
-- ---------------------------------------------------------------------------

softT :: Double -> Double -> Double
softT z g
  | z > g     = z - g
  | z < -g    = z + g
  | otherwise = 0

setDiag :: LA.Matrix Double -> LA.Vector Double -> LA.Matrix Double
setDiag m d =
  let p = LA.rows m
      xs = LA.toLists m
      ds = LA.toList d
      rewrite (i, row) =
        [ if i == j then ds !! i else (xs !! i) !! j | j <- [0 .. p - 1] ]
  in LA.fromLists [rewrite (i, xs !! i) | i <- [0 .. p - 1]]

updateAt :: LA.Vector Double -> Int -> Double -> LA.Vector Double
updateAt v i nv =
  LA.fromList [ if k == i then nv else LA.atIndex v k
              | k <- [0 .. LA.size v - 1] ]

-- | [日本語]: Σ の列 j / 行 j を新値で上書き (対角は触らない、 残り対角は別 step で
-- 設定)。 @ids@ は j を除いた行 index、 @vals@ は @ids@ 順の長さ p-1。
-- [English]: Overwrite column j \/ row j of Σ with new values (the
-- diagonal is untouched; the rest of the diagonal is set in a separate
-- step). @ids@ is the row indices excluding j, @vals@ has length p-1 in
-- @ids@ order.
updateOffDiagColumn
  :: LA.Matrix Double -> Int -> [Int] -> [Double] -> LA.Matrix Double
updateOffDiagColumn sigma j ids vals =
  let p   = LA.rows sigma
      pairs = zip ids vals
      lookupV i = case lookup i pairs of
        Just v -> v
        Nothing -> 0
      rows = LA.toLists sigma
      newRow i
        | i == j    = [ if k == j then (rows !! i) !! k else lookupV k
                      | k <- [0 .. p - 1] ]
        | otherwise = [ if k == j then lookupV i
                                  else (rows !! i) !! k
                      | k <- [0 .. p - 1] ]
  in LA.fromLists [newRow i | i <- [0 .. p - 1]]

-- | [日本語]: precision matrix の非零要素数 (対角を除く上三角)。 @threshold@ で
-- 「ゼロ」 とみなす絶対値の閾値を指定。
-- [English]: Number of non-zero elements of the precision matrix (upper
-- triangle, excluding the diagonal). @threshold@ specifies the absolute
-- value below which an element is considered "zero".
nonZeroPrecision :: Double -> LA.Matrix Double -> Int
nonZeroPrecision threshold theta =
  let p = LA.rows theta
  in length [ ()
            | i <- [0 .. p - 1]
            , j <- [i + 1 .. p - 1]
            , abs (LA.atIndex theta (i, j)) > threshold ]
