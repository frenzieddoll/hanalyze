{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-- | B13 Regrid ベンチ。
--
-- @data/io/potential_long_jagged.csv@ (21 dose × ~80 z 点、name で id) を
-- 共通 grid (N=30) に揃える。Python 側は pandas + scipy.interpolate で
-- 同等処理を合成して比較。
--
-- 出力: bench/results/haskell/regrid.csv
module Main where

import qualified DataFrame.Operations.Core     as DX
import qualified Hanalyze.DataIO.Preprocess          as Pre
import qualified Hanalyze.Stat.Interpolate           as IL
import qualified Hanalyze.Stat.AdaptiveGrid          as AG
import           Hanalyze.DataIO.CSV                 (loadAuto)

import           BenchUtil

main :: IO ()
main = do
  -- Load once (the load itself is not what we benchmark).
  edf <- loadAuto "data/io/potential_long_jagged.csv"
  case edf of
    Left err -> error ("regrid bench: failed to load: " ++ show err)
    Right df -> do
      let opts = Pre.defaultRegridOpts
                   { Pre.roInterp     = IL.PCHIP
                   , Pre.roGridKind   = AG.Adaptive
                   , Pre.roN          = 30
                   , Pre.roZBoundsMode = Pre.ZIntersection
                   }
          run :: Int -> IO Pre.RegridResult
          run _ = return (Pre.regridLong "name" "z" "y" opts df)
          probe r =
            -- Force the full regridded DataFrame by counting rows.
            fromIntegral (DX.nRows (Pre.rrDataFrame r))
      (ms, _r) <- timeitTastyIO probe run
      let row = BenchRow "haskell" "regrid"
                  "Regrid_long_jagged_PCHIP_N30" ms 0 0
                  "regridLong PCHIP+Adaptive N=30 ZIntersection on potential_long_jagged"
      writeRows "bench/results/haskell/regrid.csv" [row]
      putStrLn "wrote 1 row → bench/results/haskell/regrid.csv"
