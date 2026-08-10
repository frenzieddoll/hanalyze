-- |
-- Module      : Hanalyze.Model.RegularizedAdvanced
-- Description : 高度な罰則項回帰 (Phase 31) — Adaptive Lasso / MCP / SCAD / Group Lasso
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 高度な罰則項回帰 (Phase 31): Adaptive Lasso / MCP / SCAD / Group Lasso。
--
-- 既存 'Hanalyze.Model.Regularized' (Lasso/Ridge/Elastic Net + CV λ 選択、
-- Phase 4 で実装) を補完する変数選択型の罰則項群。 JMP "Generalized
-- Regression" platform / R `ncvreg` / `grpreg` / `glmnet` (adaptive オプション)
-- 相当。
--
-- ## 共通の前提
--
-- - 罰則項は Lasso 同様 X の列スケールに敏感。 呼び出し側で
--   'Hanalyze.Model.Regularized.standardize' しておく
-- - 内部 CD は 'Hanalyze.Model.Regularized.cdLoop' を流用 (Adaptive Lasso は
--   列再重み付け、 MCP / SCAD は per-coord non-convex threshold)
-- - Group Lasso は block CD で別ループ (Yuan-Lin 2006 algorithm)
--
-- Reference:
--   Zou (2006), Zhang (2010), Fan-Li (2001), Yuan-Lin (2006),
--   Breheny-Huang (2011) "Coordinate descent algorithms for non-convex
--   penalized regression". Ann. Appl. Stat. 5:232-253.
module Hanalyze.Model.RegularizedAdvanced
  ( -- * Adaptive Lasso (Zou 2006)
    fitAdaptiveLasso
  , adaptiveWeightsFromOLS
    -- * MCP (Zhang 2010)
  , fitMCP
    -- * SCAD (Fan-Li 2001)
  , fitSCAD
    -- * Group Lasso (Yuan-Lin 2006)
  , fitGroupLasso
  ) where

import qualified Numeric.LinearAlgebra        as LA
import           Hanalyze.Model.Regularized
                   (RegFit (..), Penalty (..), softThreshold, cdLoop,
                    mkRegFit, fitOLS, fitLasso)

-- ---------------------------------------------------------------------------
-- 31-A1: Adaptive Lasso
-- ---------------------------------------------------------------------------

-- | Adaptive Lasso (Zou 2006): @argmin (1/2n)|y - Xβ|² + λ Σ w_j |β_j|@。
--
-- 解法: column reweighting trick — @x_j' = x_j / w_j@ で変形すると標準
-- Lasso になり、 解 @β_j' = β_j · w_j@ から @β_j = β_j' / w_j@ で復元できる。
-- 既存 'fitLasso' をそのまま流用するので追加 CD ループ不要。
--
-- @w_j@ は典型的に OLS pilot 推定値から構築する ('adaptiveWeightsFromOLS')。
--
-- 注意: @w_j = 0@ は "罰則ゼロ" ではなく実装上 "@β_j = 0@ 強制" として扱う
-- (列 j を 0 vector に潰すため)。 罰則ゼロにしたい場合は @w_j@ を非常に
-- 小さい正値にする。
fitAdaptiveLasso
  :: Double                -- ^ @λ@
  -> LA.Vector Double      -- ^ weights @w@ (length @p@、 全 @≥ 0@)
  -> LA.Matrix Double      -- ^ X (n × p)
  -> LA.Vector Double      -- ^ y
  -> Int                   -- ^ max CD iterations
  -> Double                -- ^ tolerance
  -> RegFit
fitAdaptiveLasso lambda w x y maxIter tol =
  let invW   = LA.cmap (\wj -> if wj <= 0 then 0 else 1 / wj) w
      xRew   = x LA.<> LA.diag invW
      lassoF = fitLasso lambda xRew y maxIter tol
      -- 変形空間の解 β' を元の空間の β = β' / w に戻す
      betaP  = rfBeta lassoF
      beta   = invW * betaP
      yHat   = x LA.#> beta
      r      = y - yHat
  in mkRegFit beta yHat r y (L1 lambda) (rfIters lassoF)

-- | OLS pilot 推定値から Adaptive Lasso 重み @w_j = 1 / |β̂_j^OLS|^γ@ を構築。
-- 典型値 @γ = 1@。 OLS が定義できないケース (@n < p@) では事前に Ridge pilot
-- に切り替えるなど呼び出し側で工夫する。 0 除算回避のため @|β̂| ≤ 1e-8@ の
-- 場合は floor @1e-8@ を使う。
adaptiveWeightsFromOLS
  :: Double                -- ^ @γ@ (typical 1.0)
  -> LA.Matrix Double -> LA.Vector Double -> LA.Vector Double
adaptiveWeightsFromOLS gamma x y =
  let beta0 = rfBeta (fitOLS x y)
  in LA.cmap (\b -> 1 / (max 1e-8 (abs b) ** gamma)) beta0

-- ---------------------------------------------------------------------------
-- 31-A2: MCP (Minimax Concave Penalty、 Zhang 2010)
-- ---------------------------------------------------------------------------

-- | MCP non-convex 罰則:
--
-- @
--   p_{λ,γ}(β) = λ |β| - β²/(2γ)   if |β| ≤ γλ
--              = γλ²/2              if |β| > γλ
-- @
--
-- @γ → ∞@ で Lasso に縮退、 @γ → 1@ で hard-threshold 寄りになる。 典型値
-- @γ ∈ [2, 5]@。
--
-- Coordinate descent 更新 (Breheny-Huang 2011, with column-norm @cSq@):
--
-- @
--   z = ρ_j
--   β_j = S(z, λ) / (cSq - 1/γ)   if |z| ≤ γλ·cSq
--       = z / cSq                  if |z| > γλ·cSq
-- @
--
-- 前提: @cSq > 1/γ@ (= 罰則項の凹性を局所凸性が上回る)。 標準化 @X@ (cSq ≈ 1)
-- で @γ > 1@ なら自動的に満たす。 違反時は inner CD が発散する可能性があり、
-- 呼び出し側で @standardize@ + @γ ≥ 3@ を推奨。
fitMCP
  :: Double                -- ^ @λ@
  -> Double                -- ^ @γ@ (concavity、 推奨 @≥ 3@)
  -> LA.Matrix Double      -- ^ X
  -> LA.Vector Double      -- ^ y
  -> Int                   -- ^ max CD iterations
  -> Double                -- ^ tolerance
  -> RegFit
fitMCP lambda gamma x y maxIter tol =
  let upd rho cSq =
        let z      = rho
            thresh = gamma * lambda * cSq
        in if abs z <= thresh
             then
               let denom = cSq - 1 / gamma
               in if denom <= 0
                    then z / cSq                       -- 非凸時は OLS 解で fallback
                    else softThreshold z lambda / denom
             else z / cSq
      (betaFinal, iters) = cdLoop x y maxIter tol upd
      yHat = x LA.#> betaFinal
      r    = y - yHat
  in mkRegFit betaFinal yHat r y (L1 lambda) iters

-- ---------------------------------------------------------------------------
-- 31-A3: SCAD (Smoothly Clipped Absolute Deviation、 Fan-Li 2001)
-- ---------------------------------------------------------------------------

-- | SCAD non-convex 罰則 (区分三次):
--
-- @
--   p'_{λ,a}(|β|) = λ                    if |β| ≤ λ
--                 = (aλ - |β|)/(a-1)     if λ < |β| ≤ aλ
--                 = 0                    if |β| > aλ
-- @
--
-- 典型値 @a = 3.7@ (Fan-Li 2001 推奨)。
--
-- Coordinate descent 更新 (Breheny-Huang 2011):
--
-- @
--   z = ρ_j
--   if |z| ≤ λ·(1 + cSq) :        β_j = S(z, λ) / cSq        -- Lasso 領域
--   elif |z| ≤ a·λ·cSq :          β_j = S(z, aλ/(a-1)) / (cSq - 1/(a-1))
--   else :                         β_j = z / cSq               -- OLS 領域
-- @
fitSCAD
  :: Double                -- ^ @λ@
  -> Double                -- ^ @a@ (= 3.7 推奨)
  -> LA.Matrix Double
  -> LA.Vector Double
  -> Int -> Double
  -> RegFit
fitSCAD lambda a x y maxIter tol =
  let upd rho cSq =
        let z = rho
            absZ = abs z
        in if absZ <= lambda * (1 + cSq)
             then softThreshold z lambda / cSq
             else if absZ <= a * lambda * cSq
                    then
                      let denom = cSq - 1 / (a - 1)
                          thr   = a * lambda / (a - 1)
                      in if denom <= 0
                           then z / cSq
                           else softThreshold z thr / denom
                    else z / cSq
      (betaFinal, iters) = cdLoop x y maxIter tol upd
      yHat = x LA.#> betaFinal
      r    = y - yHat
  in mkRegFit betaFinal yHat r y (L1 lambda) iters

-- ---------------------------------------------------------------------------
-- 31-A4: Group Lasso (Yuan-Lin 2006)
-- ---------------------------------------------------------------------------

-- | Group Lasso: @argmin (1/2n)|y - Xβ|² + λ Σ_g √|g| · |β_g|₂@
-- (group ごと L2 ノルムの和で penalize、 group 全体を 0 / non-0 にする)。
--
-- 解法: block coordinate descent。 各 group @g@ について部分残差
-- @r_g = r + X_g β_g@ を作り、 group 更新
--
-- @
--   z_g = X_gᵀ r_g / n
--   β_g_new = (1 - λ √|g| / |z_g|₂)_+ · z_g / cSq_g
-- @
--
-- ここで @cSq_g = |X_g|² / n@ (group 内列ノルム合計、 簡易には 1 を仮定)、
-- @(·)_+@ は max(·, 0)。 Yuan-Lin 2006 の uncorrelated-within-group 想定で
-- 動く simplified version。
--
-- @groups@ は @[[Int]]@ で、 各内側リストが列 index の集合 (重複・順不同可)。
-- 列 index が複数 group に現れた場合は最初の group のみ扱われる。
fitGroupLasso
  :: Double                -- ^ @λ@
  -> [[Int]]               -- ^ group 分割 (列 index)
  -> LA.Matrix Double      -- ^ X (n × p)
  -> LA.Vector Double      -- ^ y
  -> Int                   -- ^ max iterations
  -> Double                -- ^ tolerance
  -> RegFit
fitGroupLasso lambda groups x y maxIter tol =
  let n       = LA.rows x
      nD      = fromIntegral n :: Double
      p       = LA.cols x
      -- group ごとに前計算する design submatrix と column-norm sum
      gPrep   = [ (gValid, x LA.¿ gValid, gSize gValid)
                | g <- groups
                , let gValid = [j | j <- g, j >= 0, j < p]
                , not (null gValid) ]
      gSize g = sqrt (fromIntegral (length g))   -- √|g|
      -- 反復: β_g を block 更新
      step beta resid =
        foldl
          (\(bAcc, rAcc) (gIdx, xG, gW) ->
              let -- 部分残差 r_g = r + X_g β_g
                  bG     = LA.fromList [ LA.atIndex bAcc j | j <- gIdx ]
                  rG     = rAcc + xG LA.#> bG
                  z      = LA.tr xG LA.#> rG / LA.scalar nD
                  zNorm  = LA.norm_2 z
                  cSqG   = LA.sumElements (xG * xG) / nD
                  thr    = lambda * gW
                  bGnew  = if zNorm <= thr || cSqG <= 0
                             then LA.konst 0 (LA.size z)
                             else LA.scale ((1 - thr / zNorm) / cSqG) z
                  -- 残差を新 β_g で更新: r ← r - X_g (β_g_new - β_g)
                  rNew   = rG - xG LA.#> bGnew
                  bAcc'  = updateIndices bAcc gIdx (LA.toList bGnew)
              in (bAcc', rNew))
          (beta, resid) gPrep
      loop !k !beta !resid =
        if k >= maxIter
          then (beta, k)
          else
            let (betaNew, residNew) = step beta resid
                diff = LA.norm_2 (betaNew - beta)
            in if diff < tol
                 then (betaNew, k + 1)
                 else loop (k + 1) betaNew residNew
      beta0 = LA.konst 0 p
      (betaFinal, iters) = loop 0 beta0 y
      yHat  = x LA.#> betaFinal
      r     = y - yHat
  in mkRegFit betaFinal yHat r y (L1 lambda) iters

-- | Vector の特定 index 群を新値で置き換える (immutable 経由)。 Group Lasso
-- 専用のため module 内部 helper。
updateIndices :: LA.Vector Double -> [Int] -> [Double] -> LA.Vector Double
updateIndices v idx vals =
  let xs = LA.toList v
      m  = zip idx vals
      n  = length xs
      lookupNew j = case lookup j m of
        Just nv -> nv
        Nothing -> xs !! j
  in LA.fromList [ lookupNew j | j <- [0 .. n - 1] ]
