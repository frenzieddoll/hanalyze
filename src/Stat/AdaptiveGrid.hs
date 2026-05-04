-- | Adaptive 1D grid generation.
--
-- Builds a common grid that concentrates grid points in regions where the
-- function changes rapidly across multiple ids.
--
-- Algorithm:
--
-- 1. Interpolate each id's @(z, y)@ via 'Stat.Interpolate' and evaluate on a
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
module Stat.AdaptiveGrid
  ( GridKind (..)
  , GridSpec (..)
  , defaultGridSpec
  , makeGrid
  , uniformGrid
  , minAdaptiveN
  ) where

import qualified Data.Vector.Unboxed as U
import           Stat.Interpolate    (InterpKind (..), interp1d)

-- | grid 種別。
data GridKind
  = Uniform     -- ^ [zmin, zmax] を等間隔 N 分割
  | Adaptive    -- ^ |dy/dz| のピークに集中する N 点
  deriving (Show, Eq)

-- | grid 構築の仕様。
data GridSpec = GridSpec
  { gsKind        :: !GridKind   -- ^ Uniform / Adaptive
  , gsN           :: !Int        -- ^ 出力 grid 点数
  , gsInterpKind  :: !InterpKind -- ^ density 評価用の各 id 補間方式
  , gsCoarseN     :: !Int        -- ^ density 評価用の粗 grid サイズ (default 200)
  , gsEpsRatio    :: !Double     -- ^ density 平坦部の最低密度比 (default 0.05)
  } deriving (Show, Eq)

-- | 推奨デフォルト (Adaptive / 線形補間 / 粗 grid 200 / ε=0.05*max)。
defaultGridSpec :: Int -> GridSpec
defaultGridSpec n = GridSpec
  { gsKind       = Adaptive
  , gsN          = n
  , gsInterpKind = Linear
  , gsCoarseN    = 200
  , gsEpsRatio   = 0.05
  }

-- | adaptive grid を許容する最小 N。これ未満は uniform に強制 fallback。
minAdaptiveN :: Int
minAdaptiveN = 10

-- | 共通 grid を生成。
--
-- 入力: id ごとの観測点リスト @[[(z, y)]]@、grid 範囲 (zmin, zmax)、仕様 'GridSpec'。
-- 出力: N 点の grid (昇順、zmin / zmax を必ず含む)。
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

-- | [zmin, zmax] を等間隔 N 分割。N < 2 は [zmin, zmax]。
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
