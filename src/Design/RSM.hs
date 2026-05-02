{-# LANGUAGE OverloadedStrings #-}
-- | 応答曲面法 (Response Surface Methodology, RSM)。
--
-- - 'centralComposite': 中心複合計画 (CCD) — 2^k 要因 + 軸点 + 中心点
-- - 'boxBehnken':       Box-Behnken 計画 — k 因子の三水準で軸無しデザイン
-- - 'quadraticDesign':  二次モデル用の計画行列 (定数 + 主効果 + 二乗項 + 交互作用)
-- - 'fitQuadratic':     二次回帰モデルを最小二乗で fit
-- - 'optimumPoint':     fit から極値 (Δ最大/最小) を解析的に求める
module Design.RSM
  ( CCDType (..)
  , centralComposite
  , centralCompositeRotatable
  , boxBehnken
  , quadraticDesign
  , quadraticTermNames
  , QuadFit (..)
  , fitQuadratic
  , optimumPoint
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Numeric.LinearAlgebra as LA
import Design.Factorial (twoLevelFactorial)

-- ---------------------------------------------------------------------------
-- 中心複合計画 (CCD)
-- ---------------------------------------------------------------------------

-- | CCD のタイプ。
data CCDType
  = CCC Double  -- ^ Circumscribed: 軸距離 α (典型は (2^k)^(1/4) = rotatable)
  | CCF         -- ^ Face-centered: α = 1 (axial が cube 面上)
  | CCI Double  -- ^ Inscribed: α=1, factorial を 1/α にスケール
  deriving (Show, Eq)

-- | 中心複合計画。
--
-- 構成:
--   * 2^k factorial part: ±1 の全組合せ (2^k 行)
--   * 2k 軸点: 各因子で (±α, 0, …, 0)
--   * nC 中心点: (0, …, 0) を nC 回
--
-- @centralComposite k ccdType nC@ で k 因子・nC 中心点。
centralComposite :: Int -> CCDType -> Int -> [[Double]]
centralComposite k ccdType nC =
  let factorial = case ccdType of
        CCI alpha ->
          [[v / alpha | v <- row] | row <- twoLevelFactorial k]
        _ -> twoLevelFactorial k
      alpha = case ccdType of
        CCC a   -> a
        CCF     -> 1.0
        CCI _   -> 1.0
      axial = concat
        [ [ replicate i 0 ++ [-alpha] ++ replicate (k - 1 - i) 0
          , replicate i 0 ++ [ alpha] ++ replicate (k - 1 - i) 0
          ]
        | i <- [0 .. k - 1] ]
      center = replicate nC (replicate k 0)
  in factorial ++ axial ++ center

-- | 回転可能 CCD: α = (2^k)^(1/4)。
centralCompositeRotatable :: Int -> Int -> [[Double]]
centralCompositeRotatable k nC =
  let alpha = (fromIntegral (2 ^ k :: Int) :: Double) ** 0.25
  in centralComposite k (CCC alpha) nC

-- ---------------------------------------------------------------------------
-- Box-Behnken 計画
-- ---------------------------------------------------------------------------

-- | Box-Behnken 計画 (k = 3, 4, 5)。
-- 中心点を nC 個追加して返す。
--
-- k=3: 12 corner points + nC center
-- k=4: 24 corner points + nC center
-- k=5: 40 corner points + nC center
boxBehnken :: Int -> Int -> [[Double]]
boxBehnken k nC
  | k == 3 = bb3 ++ centers
  | k == 4 = bb4 ++ centers
  | k == 5 = bb5 ++ centers
  | otherwise = error
      ("boxBehnken: only k = 3, 4, 5 supported (got k = "
        ++ show k ++ ")")
  where
    centers = replicate nC (replicate k 0)
    -- 因子ペア (i, j) (i < j) の二水準組合せで「他は 0」
    pairs n = [(i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1]]
    pairBlock n (i, j) =
      [ [ if x == i then s1
          else if x == j then s2
          else 0
        | x <- [0 .. n - 1] ]
      | s1 <- [-1, 1], s2 <- [-1, 1] ]
    bb3 = concatMap (pairBlock 3) (pairs 3)
    bb4 = concatMap (pairBlock 4) (pairs 4)
    bb5 = concatMap (pairBlock 5) (pairs 5)

-- ---------------------------------------------------------------------------
-- 二次モデル
-- ---------------------------------------------------------------------------

-- | 二次モデルの計画行列を構築。
--
-- 各行 [x_1, ..., x_k] に対して:
--   [1, x_1, ..., x_k,           -- 定数 + 主効果
--    x_1², ..., x_k²,            -- 二乗項
--    x_1 x_2, x_1 x_3, ..., x_{k-1} x_k]   -- 交互作用 (上三角)
--
-- 列数: 1 + k + k + k(k-1)/2 = 1 + 2k + k(k-1)/2
quadraticDesign :: [[Double]] -> LA.Matrix Double
quadraticDesign rows =
  let k = if null rows then 0 else length (head rows)
      expand row =
        let mainE = row
            sqE   = [x * x | x <- row]
            interE = [(row !! i) * (row !! j)
                     | i <- [0 .. k - 1], j <- [i + 1 .. k - 1]]
        in 1 : mainE ++ sqE ++ interE
  in LA.fromLists (map expand rows)

-- | 二次モデルの列名 (例: ["b0", "x1", "x2", "x1²", "x2²", "x1*x2"])。
quadraticTermNames :: Int -> [Text]
quadraticTermNames k =
  ["b0"]
  ++ [T.pack ("x" ++ show i) | i <- [1 .. k]]
  ++ [T.pack ("x" ++ show i ++ "^2") | i <- [1 .. k]]
  ++ [T.pack ("x" ++ show i ++ "*x" ++ show j)
     | i <- [1 .. k], j <- [i + 1 .. k]]

-- | 二次モデルのフィット結果。
data QuadFit = QuadFit
  { qfK     :: Int           -- 因子数
  , qfBeta  :: LA.Vector Double  -- 係数 [b0, β_main, β_sq, β_int]
  , qfYHat  :: LA.Vector Double
  , qfR2    :: Double
  } deriving (Show)

-- | 二次モデルを最小二乗で fit。
fitQuadratic :: [[Double]] -> [Double] -> QuadFit
fitQuadratic xs ys =
  let k = if null xs then 0 else length (head xs)
      x = quadraticDesign xs
      y = LA.fromList ys
      beta = LA.flatten (x LA.<\> LA.asColumn y)
      yHat = x LA.#> beta
      gm   = LA.sumElements y / fromIntegral (LA.size y)
      ssT  = LA.sumElements ((y - LA.scalar gm) ^ (2 :: Int))
      ssR  = LA.sumElements ((y - yHat) ^ (2 :: Int))
      r2   = if ssT == 0 then 0 else 1 - ssR / ssT
  in QuadFit k beta yHat r2

-- | 二次モデルの極値 (鞍点 / 極大 / 極小) を解析的に求める。
--
-- 二次モデルを ŷ = b₀ + bᵀx + xᵀ B x として、∂ŷ/∂x = 0 から
-- x* = −½ B⁻¹ b。固有値の符号で性質を判定。
--
-- 戻り値: (x*, predicted_y, eigenvalues)
--   eigenvalues 全部 < 0 → 極大
--   eigenvalues 全部 > 0 → 極小
--   混在 → 鞍点
optimumPoint :: QuadFit -> ([Double], Double, [Double])
optimumPoint fit =
  let k     = qfK fit
      beta  = LA.toList (qfBeta fit)
      b0    = head beta
      bMain = take k (drop 1 beta)
      bSq   = take k (drop (1 + k) beta)
      bInt  = drop (1 + 2 * k) beta
      -- B 行列: 対角は β_sq、非対角は β_int / 2 (対称化)
      bMat = LA.fromLists
        [ [ if i == j then bSq !! i
            else
              let (lo, hi) = if i < j then (i, j) else (j, i)
                  idx = pairIndex k lo hi
              in (bInt !! idx) / 2
          | j <- [0 .. k - 1] ]
        | i <- [0 .. k - 1] ]
      bVec = LA.fromList bMain
      xStar = LA.toList (LA.scale (-0.5) (LA.inv bMat LA.#> bVec))
      yStar = b0
            + sum (zipWith (*) bMain xStar)
            + sum (zipWith (\b x -> b * x * x) bSq xStar)
            + sum [ (bInt !! pairIndex k i j) * (xStar !! i) * (xStar !! j)
                  | i <- [0 .. k - 1], j <- [i + 1 .. k - 1] ]
      eigs = LA.toList (fst (LA.eigSH (LA.sym bMat)))
  in (xStar, yStar, eigs)
  where
    -- (i, j) ペア (i < j) の β_int 配列内のインデックス
    pairIndex n i j = sum [n - 1 - p | p <- [0 .. i - 1]] + (j - i - 1)
