{-# LANGUAGE OverloadedStrings #-}
-- | 設計の質を評価する指標。
--
-- - 'isOrthogonal':       設計行列の列が直交か (XᵀX が対角)
-- - 'orthogonalityScore': 直交度の数値指標 (0..1)
-- - 'conditionNumber':    XᵀX の条件数 (大きいと多重共線性)
-- - 'dEfficiency':        D-効率 (det(XᵀX/n)^(1/p))
-- - 'aEfficiency':        A-効率 (trace((XᵀX/n)⁻¹) の逆)
-- - 'vifList':            各列の VIF (Variance Inflation Factor)
module Design.Quality
  ( isOrthogonal
  , orthogonalityScore
  , conditionNumber
  , dEfficiency
  , aEfficiency
  , vifList
  ) where

import qualified Numeric.LinearAlgebra as LA

-- | 設計行列 X が直交か判定 (XᵀX が対角行列か、許容誤差 ε)。
isOrthogonal :: Double -> [[Double]] -> Bool
isOrthogonal eps xs =
  let m   = LA.fromLists xs
      xtx = LA.tr m LA.<> m
      n   = LA.rows xtx
      offDiagSum =
        sum [ abs (xtx `LA.atIndex` (i, j))
            | i <- [0 .. n - 1]
            , j <- [0 .. n - 1]
            , i /= j ]
  in offDiagSum < eps

-- | 直交度スコア [0, 1]: 0 = 完全直交ではない、1 = 完全直交。
--   off-diag の総和を diag の総和に対して比較。
orthogonalityScore :: [[Double]] -> Double
orthogonalityScore xs =
  let m   = LA.fromLists xs
      xtx = LA.tr m LA.<> m
      n   = LA.rows xtx
      diagSum =
        sum [ abs (xtx `LA.atIndex` (i, i)) | i <- [0 .. n - 1] ]
      offDiagSum =
        sum [ abs (xtx `LA.atIndex` (i, j))
            | i <- [0 .. n - 1]
            , j <- [0 .. n - 1]
            , i /= j ]
  in if diagSum == 0 then 0
       else 1 - offDiagSum / (diagSum + offDiagSum)

-- | XᵀX の条件数 (= λ_max / λ_min)。
--   大きい (> 30) と多重共線性の懸念。
conditionNumber :: [[Double]] -> Double
conditionNumber xs =
  let m   = LA.fromLists xs
      xtx = LA.tr m LA.<> m
      svs = LA.singularValues xtx
      sList = LA.toList svs
  in if null sList || minimum sList == 0
       then 1 / 0   -- ∞
       else maximum sList / minimum sList

-- | D-efficiency: det(XᵀX/n)^(1/p) を最大化したい。
--   完全な直交設計では 1 に近づく。
dEfficiency :: [[Double]] -> Double
dEfficiency xs =
  let m   = LA.fromLists xs
      n   = fromIntegral (LA.rows m) :: Double
      p   = fromIntegral (LA.cols m) :: Double
      xtx = LA.tr m LA.<> m
      detV = LA.det (LA.scale (1/n) xtx)
  in if detV <= 0 then 0
       else detV ** (1 / p)

-- | A-efficiency: trace((XᵀX/n)⁻¹) の逆。小さい trace = 推定の精度が高い。
aEfficiency :: [[Double]] -> Double
aEfficiency xs =
  let m   = LA.fromLists xs
      n   = fromIntegral (LA.rows m) :: Double
      p   = fromIntegral (LA.cols m) :: Double
      xtx = LA.tr m LA.<> m
      detV = LA.det xtx
  in if detV == 0 then 0
       else
         let inv = LA.inv (LA.scale (1/n) xtx)
             tr  = sum [inv `LA.atIndex` (i, i)
                       | i <- [0 .. round p - 1] :: [Int]]
         in p / tr

-- | 各列の VIF (Variance Inflation Factor)。
--   VIF_j = 1 / (1 - R²_j)、R²_j は列 j を他の列で回帰した決定係数。
--   VIF > 10 は深刻な多重共線性のサイン。
vifList :: [[Double]] -> [Double]
vifList xs =
  let m   = LA.fromLists xs
      p   = LA.cols m
  in [vifFor m j | j <- [0 .. p - 1]]
  where
    vifFor mat j =
      let yCol  = LA.flatten (mat LA.¿ [j])
          xCols = [k | k <- [0 .. LA.cols mat - 1], k /= j]
          xRest = mat LA.¿ xCols
          beta  = LA.flatten (xRest LA.<\> LA.asColumn yCol)
          yHat  = xRest LA.#> beta
          ssRes = LA.sumElements ((yCol - yHat) ^ (2 :: Int))
          mu    = LA.sumElements yCol / fromIntegral (LA.size yCol)
          ssTot = LA.sumElements ((yCol - LA.scalar mu) ^ (2 :: Int))
          r2    = if ssTot == 0 then 0 else 1 - ssRes / ssTot
      in if r2 >= 1 then 1/0 else 1 / (1 - r2)
