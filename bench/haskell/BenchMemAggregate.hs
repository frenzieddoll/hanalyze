{-# LANGUAGE OverloadedStrings #-}
-- | Memory audit Q2-B: Preprocess.groupBy* aggregation.
--
-- Suspected bug: 'collectInOrder' uses O(n²) lookup + 'vs ++ [v]' per
-- element. n=10⁴ rows × small group count should already be slow.
--
-- Usage:
--   ./bench-mem-aggregate <n_rows> <n_groups>  +RTS -s -M256m
module Main where

import qualified Data.Text                as T
import qualified Data.Vector              as V
import qualified DataFrame                as DX
import           Data.Time.Clock          (getCurrentTime, diffUTCTime)
import           System.Environment       (getArgs)
import           System.IO                (hSetBuffering, BufferMode (..), stdout)

import qualified Hanalyze.DataIO.Preprocess as PP

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  let (n, ng) = case args of
        [a]    -> (read a :: Int, 10 :: Int)
        [a, b] -> (read a, read b)
        _      -> (10000, 10)
  putStrLn $ "BenchMemAggregate  n=" ++ show n ++ "  groups=" ++ show ng
  let groupCol = DX.fromList
                  ([ T.pack ("g" ++ show (i `mod` ng)) | i <- [0 .. n - 1] ] :: [T.Text])
      valCol   = DX.fromList
                  ([ sin (fromIntegral i / 7) :: Double | i <- [0 .. n - 1] ])
      df       = DX.insertColumn "g" groupCol
               $ DX.insertColumn "v" valCol DX.empty
  V.length (V.fromList [(0::Int)]) `seq` return ()  -- silence vector import
  t0 <- getCurrentTime
  let !res = PP.groupByMean "g" "v" df
  case res of
    Nothing -> putStrLn "  groupByMean returned Nothing!"
    Just r  -> putStrLn $ "  result rows=" ++ show (DX.dimensions r)
  t1 <- getCurrentTime
  putStrLn $ "  elapsed=" ++ show (diffUTCTime t1 t0)
