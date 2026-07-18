-- |
-- Module      : Hanalyze.Model.LatentClassAnalysis
-- Description : EM アルゴリズムによる潜在クラス分析 (LCA、R poLCA 相当)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Latent Class Analysis (LCA) via EM algorithm (Phase 32-A2)。
--
-- カテゴリ変数の潜在クラスクラスタリング。 @K@ 個の潜在クラスを仮定し、
-- 各クラスでの各 categorical 特徴の条件付き分布 @P(X_j | class)@ を推定する。
-- R `poLCA` 相当。
--
-- ## モデル
--
-- @
--   P(X_i) = Σ_k π_k · Π_j ρ_{k, j, X_{i,j}}
-- @
--
-- ここで @π_k@ はクラス混合重み、 @ρ_{k,j,l}@ はクラス @k@ で特徴 @j@ が
-- 水準 @l@ を取る確率。
--
-- ## EM
--
-- - **E-step**: posterior @γ_{i,k} = π_k Π_j ρ_{k,j,X_{i,j}} / Σ_{k'} (...)@
-- - **M-step**: @π_k ← (1/n) Σ_i γ_{i,k}@、
--   @ρ_{k,j,l} ← Σ_i γ_{i,k} [X_{i,j} = l] / Σ_i γ_{i,k}@
--
-- Reference: Linzer-Lewis (2011) "poLCA: An R package for polytomous
-- variable latent class analysis". J Stat Softw 42(10).
module Hanalyze.Model.LatentClassAnalysis
  ( LCAFit (..)
  , fitLCA
  ) where

import qualified Numeric.LinearAlgebra as LA
import qualified System.Random.MWC     as MWC
import           Control.Monad         (replicateM)

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

data LCAFit = LCAFit
  { lcaPi              :: !(LA.Vector Double)       -- ^ class mixing weights (length K)
  , lcaRho             :: ![LA.Matrix Double]       -- ^ per feature: K × L (length J)
  , lcaResponsibilities :: !(LA.Matrix Double)      -- ^ posterior γ (n × K)
  , lcaIterations      :: !Int
  , lcaConverged       :: !Bool
  , lcaLogLik          :: !Double
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- fitLCA
-- ---------------------------------------------------------------------------

-- | @K@ クラス、 @L@ 水準の LCA を EM で fit。 入力 @X@ は @n@ 行 @J@ 列の
-- 0-indexed カテゴリ値 (`[[Int]]`、 全要素 ∈ @[0, L-1]@)。
--
-- 初期化はランダム (Dirichlet(1) ≈ uniform-on-simplex の近似で MWC を使う)。
-- 同じ seed で再現性あり。
fitLCA
  :: Int                  -- ^ K (classes)
  -> Int                  -- ^ L (levels per feature)
  -> [[Int]]              -- ^ X (n × J)
  -> Int                  -- ^ max EM iterations
  -> Double               -- ^ tolerance on log-likelihood diff
  -> MWC.GenIO
  -> IO LCAFit
fitLCA k l xRaw maxIter tol gen = do
  let n = length xRaw
      j = if n > 0 then length (head xRaw) else 0
  -- 初期化
  pi0  <- randomSimplex k gen
  rho0 <- replicateM j (randomRowStochastic k l gen)
  let xMat = LA.fromLists [map fromIntegral row | row <- xRaw]
      go !it !pVec !rhoList !prevLL = do
        let (gamma, ll) = eStep xMat pVec rhoList l
            (pNew, rhoNew) = mStep xMat gamma l
            converged = abs (ll - prevLL) < tol
        if it >= maxIter || converged
          then pure (pVec, rhoList, gamma, it, converged, ll)
          else go (it + 1) pNew rhoNew ll
  -- 初期 ll は -inf で 1 回目は必ず更新される
  (pFinal, rhoFinal, gamFinal, iters, conv, llFinal) <-
    go 0 pi0 rho0 (-1 / 0)
  pure LCAFit
    { lcaPi              = pFinal
    , lcaRho             = rhoFinal
    , lcaResponsibilities = gamFinal
    , lcaIterations      = iters
    , lcaConverged       = conv
    , lcaLogLik          = llFinal
    }

-- | E-step: per-row posterior @γ_{i,k}@ と log-likelihood。
-- log-space で stable: @log P(X_i | k) = Σ_j log ρ_{k, j, X_{i,j}}@
eStep
  :: LA.Matrix Double  -- ^ X (n × J)、 0/1/.../L-1 を Double で
  -> LA.Vector Double  -- ^ π
  -> [LA.Matrix Double] -- ^ ρ (J 個の K × L)
  -> Int               -- ^ L
  -> (LA.Matrix Double, Double)
eStep xMat pVec rhoList _ =
  let n = LA.rows xMat
      k = LA.size pVec
      logPi = LA.cmap (\p -> log (max 1e-300 p)) pVec
      logPx_ik i kk =
        sum [ log (max 1e-300
                     (LA.atIndex (rhoList !! jj)
                        (kk, floor (LA.atIndex xMat (i, jj)))))
            | jj <- [0 .. length rhoList - 1] ]
      logUnnormRow i = LA.fromList
        [ LA.atIndex logPi kk + logPx_ik i kk | kk <- [0 .. k - 1] ]
      rows = [logUnnormRow i | i <- [0 .. n - 1]]
      logSumExpV v =
        let mx = LA.maxElement v
        in mx + log (LA.sumElements (LA.cmap (\x -> exp (x - mx)) v))
      perRowLL = [logSumExpV r | r <- rows]
      gammaRows =
        [ LA.cmap (\x -> exp (x - lse)) r
        | (r, lse) <- zip rows perRowLL ]
      gamma = LA.fromRows gammaRows
      ll = sum perRowLL
  in (gamma, ll)

-- | M-step: γ から π / ρ を更新。
mStep
  :: LA.Matrix Double   -- ^ X (n × J)
  -> LA.Matrix Double   -- ^ γ (n × K)
  -> Int                -- ^ L
  -> (LA.Vector Double, [LA.Matrix Double])
mStep xMat gamma l =
  let n   = LA.rows xMat
      j   = LA.cols xMat
      k   = LA.cols gamma
      ones = LA.konst 1 n :: LA.Vector Double
      gSum = LA.tr gamma LA.#> ones   -- length K = Σ_i γ_{i,k}
      pNew = LA.scale (1 / fromIntegral n) gSum
      -- 各特徴 j の ρ (K × L) を再推定
      rhoFor jj =
        let countMat = LA.fromLists
              [ [ sum [ LA.atIndex gamma (i, kk)
                      | i <- [0 .. n - 1]
                      , floor (LA.atIndex xMat (i, jj)) == ll ]
                | ll <- [0 .. l - 1] ]
              | kk <- [0 .. k - 1] ]
            denom = LA.cmap (\g -> max 1e-300 g) gSum
        in LA.fromColumns
             [ LA.flatten (countMat LA.¿ [c]) / denom
             | c <- [0 .. l - 1] ]
      rhoNew = [rhoFor jj | jj <- [0 .. j - 1]]
  in (pNew, rhoNew)

-- ---------------------------------------------------------------------------
-- 初期化ヘルパ
-- ---------------------------------------------------------------------------

-- | 長さ @k@ の simplex 上の uniform ランダム vector (= Dir(1) 近似)。
-- 単純に @k@ 個の uniform を引いて正規化。
randomSimplex :: Int -> MWC.GenIO -> IO (LA.Vector Double)
randomSimplex k gen = do
  rs <- replicateM k (MWC.uniformR (1e-3, 1.0 :: Double) gen)
  let s = sum rs
  pure (LA.fromList (map (/ s) rs))

-- | K × L 行 stochastic matrix のランダム生成。 各行を randomSimplex。
randomRowStochastic :: Int -> Int -> MWC.GenIO -> IO (LA.Matrix Double)
randomRowStochastic k l gen = do
  rows <- replicateM k (randomSimplex l gen)
  pure (LA.fromRows rows)
