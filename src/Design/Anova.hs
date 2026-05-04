{-# LANGUAGE OverloadedStrings #-}
-- | ANOVA / ANCOVA tables.
--
-- Computes one-way and two-way analysis of variance, reporting F values,
-- p values, and the @η²@ effect size.
module Design.Anova
  ( AnovaRow (..)
  , AnovaTable (..)
  , oneWayAnova
  , twoWayAnova
  , printAnovaTable
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (groupBy, sort)
import Data.Function (on)
import Text.Printf (printf)
import qualified Statistics.Distribution as SD
import qualified Statistics.Distribution.FDistribution as FD

-- | ANOVA テーブルの 1 行。
data AnovaRow = AnovaRow
  { arSource :: Text
  , arDF     :: Int      -- 自由度
  , arSS     :: Double   -- 平方和 (Sum of Squares)
  , arMS     :: Double   -- 平均平方 (Mean Square = SS/DF)
  , arF      :: Maybe Double  -- F 値 (Total / Error 行は Nothing)
  , arPVal   :: Maybe Double  -- p 値
  , arEtaSq  :: Maybe Double  -- η² = SS_factor / SS_total
  } deriving (Show)

newtype AnovaTable = AnovaTable [AnovaRow] deriving (Show)

-- | 一元配置 ANOVA。
--
-- 引数: グループラベル (各データ点の所属) と値。
oneWayAnova :: [Text] -> [Double] -> AnovaTable
oneWayAnova labels values =
  let n         = length values
      grandMean = sum values / fromIntegral n
      groups    = groupBy ((==) `on` fst)
                $ sort (zip labels values)
      ssTotal   = sum [(v - grandMean)^(2::Int) | v <- values]
      -- グループ間平方和 (Between)
      ssBetween = sum
        [ let xs   = map snd g
              gm   = sum xs / fromIntegral (length xs)
              k    = length xs
          in fromIntegral k * (gm - grandMean)^(2::Int)
        | g <- groups ]
      ssWithin  = ssTotal - ssBetween
      kGroups   = length groups
      dfBetween = kGroups - 1
      dfWithin  = n - kGroups
      msBetween = ssBetween / fromIntegral dfBetween
      msWithin  = ssWithin  / fromIntegral dfWithin
      fStat     = msBetween / msWithin
      pVal      = if dfWithin <= 0 || msWithin <= 0
                    then 1
                    else SD.complCumulative
                          (FD.fDistribution dfBetween dfWithin) fStat
      etaSq     = ssBetween / ssTotal
  in AnovaTable
      [ AnovaRow "Between" dfBetween ssBetween msBetween
                 (Just fStat) (Just pVal) (Just etaSq)
      , AnovaRow "Within"  dfWithin  ssWithin  msWithin
                 Nothing Nothing Nothing
      , AnovaRow "Total"   (n - 1)   ssTotal   (ssTotal / fromIntegral (n - 1))
                 Nothing Nothing Nothing
      ]

-- | 二元配置 ANOVA (交互作用なし、各セル 1 観測または等数の場合)。
--
-- 引数: 因子 A ラベル、因子 B ラベル、値。各セル (a, b) で 1 観測を仮定。
-- 平衡データ (= すべてのセルに同じ観測数) を仮定。
twoWayAnova :: [Text]   -- factor A
            -> [Text]   -- factor B
            -> [Double]
            -> AnovaTable
twoWayAnova as bs values =
  let n    = length values
      gm   = sum values / fromIntegral n
      ssT  = sum [(v - gm)^(2::Int) | v <- values]
      -- 因子 A の主効果
      aGroups = groupBy ((==) `on` fst) (sort (zip as values))
      ssA  = sum
        [ let vs = map snd g
              m  = sum vs / fromIntegral (length vs)
          in fromIntegral (length vs) * (m - gm)^(2::Int)
        | g <- aGroups ]
      -- 因子 B の主効果
      bGroups = groupBy ((==) `on` fst) (sort (zip bs values))
      ssB  = sum
        [ let vs = map snd g
              m  = sum vs / fromIntegral (length vs)
          in fromIntegral (length vs) * (m - gm)^(2::Int)
        | g <- bGroups ]
      ssE  = ssT - ssA - ssB
      a    = length aGroups
      b    = length bGroups
      dfA  = a - 1
      dfB  = b - 1
      dfE  = n - a - b + 1
      msA  = ssA / fromIntegral dfA
      msB  = ssB / fromIntegral dfB
      msE  = if dfE > 0 then ssE / fromIntegral dfE else 1
      fA   = msA / msE
      fB   = msB / msE
      pA   = if dfE <= 0 then 1
               else SD.complCumulative (FD.fDistribution dfA dfE) fA
      pB   = if dfE <= 0 then 1
               else SD.complCumulative (FD.fDistribution dfB dfE) fB
  in AnovaTable
      [ AnovaRow "Factor A" dfA ssA msA (Just fA) (Just pA) (Just (ssA/ssT))
      , AnovaRow "Factor B" dfB ssB msB (Just fB) (Just pB) (Just (ssB/ssT))
      , AnovaRow "Error"    dfE ssE msE Nothing Nothing Nothing
      , AnovaRow "Total"    (n - 1) ssT (ssT / fromIntegral (n - 1))
                 Nothing Nothing Nothing
      ]

-- | テーブルをコンソールに整形出力。
printAnovaTable :: AnovaTable -> IO ()
printAnovaTable (AnovaTable rows) = do
  printf "%-12s %4s %12s %12s %10s %10s %8s\n"
    ("Source" :: String) ("DF" :: String) ("SS" :: String) ("MS" :: String)
    ("F" :: String) ("p-value" :: String) ("η²" :: String)
  putStrLn (replicate 76 '-')
  mapM_ printRow rows
  where
    printRow r = do
      printf "%-12s %4d %12.4f %12.4f"
             (T.unpack (arSource r)) (arDF r) (arSS r) (arMS r)
      let fmtMaybe :: Double -> String
          fmtMaybe v = printf "%10.4f" v
      case arF r of
        Just f  -> putStr (fmtMaybe f)
        Nothing -> putStr (printf "%10s" ("--" :: String))
      case arPVal r of
        Just p  -> putStr (fmtMaybe p)
        Nothing -> putStr (printf "%10s" ("--" :: String))
      case arEtaSq r of
        Just e  -> printf "%8.4f\n" e
        Nothing -> printf "%8s\n" ("--" :: String)
