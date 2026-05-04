{-# LANGUAGE OverloadedStrings #-}
-- | Quality criteria for evaluating designs.
--
--   * 'isOrthogonal'       — are the design columns orthogonal? (i.e.
--     @XᵀX@ diagonal).
--   * 'orthogonalityScore' — numeric orthogonality score in @[0, 1]@.
--   * 'conditionNumber'    — condition number of @XᵀX@ (large values
--     indicate multicollinearity).
--   * 'dEfficiency'        — D-efficiency @det(XᵀX/n)^(1/p)@.
--   * 'aEfficiency'        — A-efficiency: reciprocal of
--     @trace((XᵀX/n)⁻¹)@.
--   * 'vifList'            — per-column Variance Inflation Factor.
module Design.Quality
  ( isOrthogonal
  , orthogonalityScore
  , conditionNumber
  , dEfficiency
  , aEfficiency
  , vifList
  ) where

import qualified Numeric.LinearAlgebra as LA

-- | True iff the design matrix @X@ is orthogonal (i.e. @XᵀX@ is
-- diagonal up to tolerance @ε@).
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

-- | Orthogonality score in @[0, 1]@: 0 = far from orthogonal,
-- 1 = exactly orthogonal. Compares the off-diagonal mass against the
-- diagonal mass.
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

-- | Condition number of @XᵀX@ (@λ_max / λ_min@). Values above 30
-- typically indicate multicollinearity.
conditionNumber :: [[Double]] -> Double
conditionNumber xs =
  let m   = LA.fromLists xs
      xtx = LA.tr m LA.<> m
      svs = LA.singularValues xtx
      sList = LA.toList svs
  in if null sList || minimum sList == 0
       then 1 / 0   -- ∞
       else maximum sList / minimum sList

-- | D-efficiency @det(XᵀX/n)^(1/p)@ — to be maximized. Approaches 1 for
-- a fully orthogonal design.
dEfficiency :: [[Double]] -> Double
dEfficiency xs =
  let m   = LA.fromLists xs
      n   = fromIntegral (LA.rows m) :: Double
      p   = fromIntegral (LA.cols m) :: Double
      xtx = LA.tr m LA.<> m
      detV = LA.det (LA.scale (1/n) xtx)
  in if detV <= 0 then 0
       else detV ** (1 / p)

-- | A-efficiency: reciprocal of @trace((XᵀX/n)⁻¹)@. A smaller trace
-- means higher per-coefficient estimation precision.
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

-- | Per-column Variance Inflation Factor.
--
-- @VIF_j = 1 / (1 - R²_j)@, where @R²_j@ is the coefficient of
-- determination from regressing column @j@ on the others.
-- @VIF > 10@ is a strong sign of multicollinearity.
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
