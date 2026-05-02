{-# LANGUAGE OverloadedStrings #-}
-- | Desirability function (Derringer & Suich 1980, Phase U2)。
--
-- 多目的最適化を 1 つのスカラー目的に集約する古典的手法。
-- 各応答 y_j を [0, 1] 範囲の **desirability** d_j に変換し、
-- 全体の総合 desirability D を幾何平均で計算:
--
--   D = (Π d_j)^(1/q)
--
-- D を最大化する x が「全応答を程よく満たす」点。
module Optim.Desirability
  ( DesirabilityType (..)
  , individualDesirability
  , overallDesirability
  ) where

-- | 望ましさの 3 種類。
data DesirabilityType
  = Maximize  Double Double  -- ^ 最大化: low (= 0), high (= 1) のしきい値
  | Minimize  Double Double  -- ^ 最小化: high (= 0), low (= 1)
  | Target    Double Double Double  -- ^ 目標値 t、許容範囲 [low, high]
  deriving (Show, Eq)

-- | 個別 desirability d_j (y) を計算。
individualDesirability :: DesirabilityType -> Double -> Double
individualDesirability dt y = case dt of
  Maximize lo hi
    | y <= lo   -> 0
    | y >= hi   -> 1
    | otherwise -> (y - lo) / (hi - lo)
  Minimize hi lo
    | y >= hi   -> 0
    | y <= lo   -> 1
    | otherwise -> (hi - y) / (hi - lo)
  Target t lo hi
    | y == t                  -> 1
    | y < lo || y > hi        -> 0
    | y < t                   -> (y - lo) / (t - lo)
    | otherwise               -> (hi - y) / (hi - t)

-- | 総合 desirability D = (Π d_j)^(1/q)。
-- どれか 1 つでも 0 なら全体 0 (= 「許容外」を強く罰する)。
overallDesirability :: [DesirabilityType] -> [Double] -> Double
overallDesirability dts ys
  | length dts /= length ys = 0
  | null ys                 = 0
  | otherwise =
      let ds = zipWith individualDesirability dts ys
          q  = fromIntegral (length ds) :: Double
      in if any (<= 0) ds then 0
           else (product ds) ** (1 / q)
