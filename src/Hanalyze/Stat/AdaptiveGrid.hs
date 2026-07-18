-- |
-- Module      : Hanalyze.Stat.AdaptiveGrid
-- Description : 複数 id 間で変化の急な領域に点を集中させる適応的 1D グリッド生成
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Adaptive 1D grid generation.
--
-- Builds a common grid that concentrates grid points in regions where the
-- function changes rapidly across multiple ids.
--
-- Algorithm:
--
-- 1. Interpolate each id's @(z, y)@ via 'Hanalyze.Stat.Interpolate' and evaluate on a
--    common coarse grid (e.g. 200 points).
-- 2. For each z, compute @|dy/dz|@ across all ids and take the **maximum**
--    (peak) as @density(z)@.
-- 3. Add @ε = 0.05 × max(density)@ to avoid division by zero on flat regions.
-- 4. Build the cumulative integral @F(z) = ∫ (density(z) + ε) dz@.
-- 5. Divide the range of @F@ into @N-1@ equal parts and invert to obtain
--    @N@ z-coordinates.
--
-- When @N < 'minAdaptiveN'@ (= 10), the request silently falls back to a
-- uniform grid.
module Hanalyze.Stat.AdaptiveGrid
  ( GridKind (..)
  , GridSpec (..)
  , defaultGridSpec
  , makeGrid
  , uniformGrid
  , minAdaptiveN
  ) where

import qualified Data.Vector.Unboxed as U
import           Hanalyze.Stat.Interpolate    (InterpKind (..), interp1d)

-- | Grid kind.
data GridKind
  = Uniform     -- ^ Equally spaced @N@ points on @[zmin, zmax]@.
  | Adaptive    -- ^ @N@ points concentrated where @|dy/dz|@ peaks.
  deriving (Show, Eq)

-- | Specification used to build a grid.
data GridSpec = GridSpec
  { gsKind        :: !GridKind   -- ^ Uniform or adaptive.
  , gsN           :: !Int        -- ^ Number of grid points.
  , gsInterpKind  :: !InterpKind -- ^ Per-id interpolant used to evaluate the density.
  , gsCoarseN     :: !Int        -- ^ Size of the coarse density grid (default 200).
  , gsEpsRatio    :: !Double     -- ^ Floor on density on flat regions (default 0.05).
  } deriving (Show, Eq)

-- | Recommended defaults: adaptive grid, linear interpolant, coarse grid
-- of 200 points, @ε = 0.05 × max(density)@.
defaultGridSpec :: Int -> GridSpec
defaultGridSpec n = GridSpec
  { gsKind       = Adaptive
  , gsN          = n
  , gsInterpKind = Linear
  , gsCoarseN    = 200
  , gsEpsRatio   = 0.05
  }

-- | Smallest @N@ for which adaptive grids are honored. Below this, an
-- adaptive request falls back to uniform.
minAdaptiveN :: Int
minAdaptiveN = 10

-- | Build a common grid.
--
-- Inputs: per-id observation lists @[[(z, y)]]@, the @(zmin, zmax)@
-- range, and a 'GridSpec'. The result is an ascending list of @N@ grid
-- points whose endpoints are exactly @zmin@ and @zmax@.
makeGrid :: [[(Double, Double)]] -> (Double, Double) -> GridSpec -> [Double]
makeGrid _      (zmin, zmax) spec
  | gsN spec < 2 = [zmin, zmax]
  | gsKind spec == Uniform || gsN spec < minAdaptiveN
                 = uniformGrid (gsN spec) zmin zmax
makeGrid perId  (zmin, zmax) spec =
  let n       = gsN spec
      coarseN = gsCoarseN spec
      coarse  = uniformGrid coarseN zmin zmax
      -- 各 id を補間し coarse grid 上で y を評価
      ysPerId = [ map (interp1d (gsInterpKind spec) pts) coarse
                | pts <- perId
                , length pts >= 2 ]
      -- 各 id の |dy/dz| 中央差分 → coarseN 長の Vector
      slopesPerId = map (slopeAbs coarse) ysPerId
      -- ピーク密度: 各 z 点で全 id の最大 |slope|
      peak    = U.fromList
                  [ if null slopesPerId
                      then 1.0
                      else maximum [ s U.! i | s <- slopesPerId ]
                  | i <- [0 .. coarseN - 1] ]
      mx      = U.maximum peak
      eps     = gsEpsRatio spec * (if mx > 0 then mx else 1.0)
      density = U.map (+ eps) peak
      -- 累積積分 (台形則)
      czs     = U.fromList coarse
      cumF    = trapezoidalCDF czs density
      total   = U.last cumF
      -- N-1 等分点に対応する z を逆写像
      targets = [ (fromIntegral k / fromIntegral (n - 1)) * total
                | k <- [0 .. n - 1] ]
      gridZ   = map (invMap czs cumF) targets
  in -- 端点を保証 + monotone 化 (浮動小数誤差で僅かに非単調になることがある)
     ensureMonotone zmin zmax gridZ

-- | Equally spaced @N@-point grid on @[zmin, zmax]@. With @N < 2@ the
-- result is @[zmin, zmax]@.
--
-- >>> uniformGrid 5 0 1
-- [0.0,0.25,0.5,0.75,1.0]
uniformGrid :: Int -> Double -> Double -> [Double]
uniformGrid n zmin zmax
  | n < 2     = [zmin, zmax]
  | otherwise =
      let step = (zmax - zmin) / fromIntegral (n - 1)
      in [ zmin + step * fromIntegral i | i <- [0 .. n - 1] ]

-- ---------------------------------------------------------------------------

-- | 中央差分での |dy/dz|。両端は片側差分。
slopeAbs :: [Double] -> [Double] -> U.Vector Double
slopeAbs zs ys =
  let zV = U.fromList zs
      yV = U.fromList ys
      n  = U.length zV
  in U.generate n $ \i ->
       if n < 2 then 0
       else if i == 0
              then abs ((yV U.! 1 - yV U.! 0) / (zV U.! 1 - zV U.! 0))
       else if i == n - 1
              then abs ((yV U.! (n-1) - yV U.! (n-2)) / (zV U.! (n-1) - zV U.! (n-2)))
       else
         abs ((yV U.! (i+1) - yV U.! (i-1)) / (zV U.! (i+1) - zV U.! (i-1)))

-- | 累積分布 F[i] = ∫_{z_0}^{z_i} ρ dz (台形則)。F[0] = 0。
trapezoidalCDF :: U.Vector Double -> U.Vector Double -> U.Vector Double
trapezoidalCDF zs rho =
  let n = U.length zs
  in U.scanl' (+) 0 $
       U.generate (n - 1) $ \i ->
         let dz = zs U.! (i + 1) - zs U.! i
             r  = (rho U.! i + rho U.! (i + 1)) / 2
         in dz * r

-- | 累積 F の逆写像: target に対応する z を線形内挿で求める。
invMap :: U.Vector Double -> U.Vector Double -> Double -> Double
invMap zs cum target =
  let n  = U.length cum
      -- 二分探索で cum[i] <= target <= cum[i+1] の i を見つける
      go lo hi
        | hi - lo <= 1 = lo
        | otherwise =
            let mid = (lo + hi) `div` 2
            in if cum U.! mid > target then go lo mid else go mid hi
      i  = max 0 (min (n - 2) (go 0 (n - 1)))
      c0 = cum U.! i
      c1 = cum U.! (i + 1)
      z0 = zs  U.! i
      z1 = zs  U.! (i + 1)
      t  = if c1 > c0 then (target - c0) / (c1 - c0) else 0
  in z0 + t * (z1 - z0)

-- | 端点を [zmin, zmax] にスナップ + 単調化 (重複は微小 ε ずつシフト)。
ensureMonotone :: Double -> Double -> [Double] -> [Double]
ensureMonotone zmin zmax xs0 =
  let xs = case xs0 of
             []     -> [zmin, zmax]
             [_]    -> [zmin, zmax]
             (_:rs) -> zmin : init rs ++ [zmax]
      -- 単調化 (前進方向で max を取り、僅かに ε を加算)
      go prev (x:rest) =
        let x' = max x (prev + 1e-12 * (zmax - zmin + 1))
        in x' : go x' rest
      go _    []       = []
  in case xs of
       (x0:rest) -> x0 : go x0 rest
       []        -> []
