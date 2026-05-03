{-# LANGUAGE OverloadedStrings #-}
-- | 半導体イオン注入 (II) のポテンシャル風ダミーデータ生成器。
--
-- 6 条件 (energy / dose) × 各条件で独自の z グリッド (グリッドが揃わない
-- ことを再現) を生成し、long-form CSV を 'data/io/potential_long.csv' に
-- 書き出す。
--
-- 物理モデル (簡易):
--   V(z; E, D) = D · exp(-(z - Rp(E))^2 / (2 σ(E)^2)) + ε
--   Rp(E)     = 0.5 · E   [nm]
--   σ(E)      = 0.3 · Rp  [nm]
--   ε ~ Normal(0, 0.02)
module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom, GenIO)
import qualified System.Random.MWC.Distributions as MWCD

-- | (label, energy keV, dose 相対値, z グリッド)。グリッドは条件ごとに変える。
conditions :: [(String, Double, Double, [Double])]
conditions =
  [ ("c1_E50_D1.0",  50,  1.0, [0, 5    .. 50])             -- step 5
  , ("c2_E80_D1.0",  80,  1.0, [0, 8    .. 72])             -- step 8
  , ("c3_E120_D1.0", 120, 1.0, [0, 12, 24, 36, 60, 84, 108, 120]) -- 不等間隔
  , ("c4_E80_D0.5",  80,  0.5, [0, 9    .. 72])             -- step 9
  , ("c5_E80_D2.0",  80,  2.0, [0, 6    .. 72])             -- step 6
  , ("c6_E200_D1.0", 200, 1.0, [0, 20, 40, 60, 80, 100, 120, 140, 180, 200]) -- 不等間隔
  ]

main :: IO ()
main = do
  gen <- createSystemRandom
  let header = "name,energy,dose,z,y\n"
  rows <- mapM (genRows gen) conditions
  let body = concat rows
      out  = "data/io/potential_long.csv"
  BL.writeFile out (BB.toLazyByteString (BB.stringUtf8 (header ++ body)))
  putStrLn $ "Wrote " ++ out
  putStrLn $ "Rows: " ++ show (length (concat rows) `div` 1)
  putStrLn $ "Conditions: " ++ show (length conditions)

genRows :: GenIO -> (String, Double, Double, [Double]) -> IO String
genRows gen (label, e, d, zs) = do
  let rp = 0.5 * e
      sg = 0.3 * rp
  fmap concat $ mapM (mkRow gen label e d rp sg) zs

mkRow :: GenIO -> String -> Double -> Double -> Double -> Double -> Double -> IO String
mkRow gen label e d rp sg z = do
  noise <- MWCD.normal 0 0.02 gen
  let v = d * exp (- ((z - rp)^(2::Int)) / (2 * sg * sg)) + noise
  return (printf "%s,%g,%g,%g,%.4f\n" label e d z v)
