{-# LANGUAGE OverloadedStrings #-}
-- | Phase 70.3 項目 C (C2) 目視確認用 demo。
--
-- 透過標準化ラッパ ('standardized') が、 内部では z-score 標準化空間で学習しつつ
-- **図・予測を元スケールで自動逆変換**することを確認する。 x をあえて非単位スケール
-- (数百オーダ) にして、 出力軸が z-score でなく元単位で出ることを目視できるようにする。
module Main (main) where

import           Data.Text                (Text)
import           System.Directory         (createDirectoryIfMissing)

import           Graphics.Hgg.Backend.SVG (saveSVG)
import           Hanalyze.Plot     (toPlot, standardized, knnReg, lm, (|->))

main :: IO ()
main = do
  -- gen-doc-figures.sh の SRC (= design/plot-integration) へ出力し、 確定図セットの
  -- basename (knn-standardized / lm-standardized) で docs/images へコピーされる。
  createDirectoryIfMissing True "design/plot-integration"

  -- 共通: x は 200..360 (例: 温度 K) と非単位スケール。 標準化しないと kNN 距離も
  -- LM も x 軸の桁に振り回されるが、 ラッパは透過で元スケールに戻す。
  let xs = [200, 220, 240, 260, 280, 300, 320, 340, 360] :: [Double]
      ys = [1.2, 2.0, 1.7, 3.1, 2.8, 4.2, 3.9, 5.3, 5.0]  :: [Double]
      dat = [("temp", xs), ("rate", ys)] :: [(Text, [Double])]

  -- (1) ★主対象: 距離ベース kNN 回帰。 standardized で内部標準化、 図は元スケール。
  let knnWrap = dat |-> standardized (knnReg 3 ["temp"] "rate")
  saveSVG "design/plot-integration/knn-standardized.svg" (toPlot knnWrap)
  putStrLn "wrote design/plot-integration/knn-standardized.svg"

  -- (2) 線形回帰 (整形目的)。 散布 + 回帰線 + CI 帯が元スケールで出る (帯も逆変換)。
  let lmWrap = dat |-> standardized (lm "temp" "rate")
  saveSVG "design/plot-integration/lm-standardized.svg" (toPlot lmWrap)
  putStrLn "wrote design/plot-integration/lm-standardized.svg"
