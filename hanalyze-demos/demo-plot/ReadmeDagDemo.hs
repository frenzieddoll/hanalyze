-- | README ギャラリー用の「ならでは」図 = 実行可能な階層ベイズモデルの DAG。
--   Chart / hvega (Vega-Lite) では描けない統計モデル構造図 (plate 記法) を、
--   ★実際に NUTS でサンプリングできる本物の HBM から 'dagOf' で抽出して描く。
--
--   モデル = 群レベル予測子つき 変量切片+変量傾きの階層回帰:
--     mu_a, tau_a, mu_b, tau_b, s              -- 群を貫くハイパー事前
--     plate group(J):
--       a_j ~ N(mu_a, tau_a),  b_j ~ N(mu_b, tau_b)
--       mu_j = a_j + b_j * xg_j                -- 群レベル予測子 xg
--       y_j  ~ N(mu_j, s)                      -- 群内の観測
--   観測は plate 内に置く (cross-plate index を避け DAG を完全に出す)。
--   観測値は closure 焼き込み (docP5 と同型) なので hbmModel の data 引数は []。
--
--   実行: cabal run --project-file=cabal.project.plot readme-dag-demo
--   出力: design/readme/hbm-hier-dag.svg → hgg の README へコピー。
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Monad            (forM, forM_)
import qualified Data.Text                as T
import           System.Directory         (createDirectoryIfMissing)

import           Graphics.Hgg.Backend.SVG (saveSVGBound)
import           Graphics.Hgg.Frame       ((|>>))
import           Graphics.Hgg.Spec        (ColData (..), title, width, height)
import           Hanalyze.Plot     (hbmModel, defaultHBM, HBMConfig (..), dagOf, toPlot)
import           Hanalyze.Model.HBM
                   ( Distribution (Normal, HalfNormal)
                   , ModelP, sample, deterministic, observe, plate )

nGrp :: Int
nGrp = 3

-- 群レベル予測子 (baked)。
xg :: Int -> Double
xg j = [0.0, 1.0, 2.0] !! j

-- 群 j の観測 (各群 5 点・決定的)。
groupObs :: Int -> [Double]
groupObs j =
  let base = [1.0, 4.0, 8.0] !! j + [0.5, 1.0, 0.3] !! j * xg j
  in map (base +) [0.10, -0.08, 0.05, -0.12, 0.09]

-- 群レベル予測子つき 変量切片+傾きの階層回帰。
--   ★ plate を 2 つに分ける (PyMC 同型):
--     group(J) … 群ごとの係数 a_j, b_j だけ
--     obs(N)   … 観測ごとの mu_i, y_i (群 index で a,b を参照)
--   こうしないと観測の繰り返し (obs) が group plate に潰れる。
hierModel :: ModelP ()
hierModel = do
  muA  <- sample "mu_a"  (Normal 0 10)
  tauA <- sample "tau_a" (HalfNormal 5)
  muB  <- sample "mu_b"  (Normal 0 10)
  tauB <- sample "tau_b" (HalfNormal 5)
  s    <- sample "s"     (HalfNormal 1)
  -- group plate: 係数 a_j, b_j のみ作って返す。
  coefs <- plate "group" nGrp $ forM [0 .. nGrp - 1] $ \j -> do
             aj <- sample ("a_" <> T.pack (show j)) (Normal muA tauA)
             bj <- sample ("b_" <> T.pack (show j)) (Normal muB tauB)
             pure (aj, bj)
  -- 観測 N 行 = (群 j, その群の観測点 y) を平坦化。
  let rows = [ (j, yi) | j <- [0 .. nGrp - 1], yi <- groupObs j ]
  -- obs plate: 観測ごとに mu_i を作り y_i を観測 (群 index で係数を引く)。
  _ <- plate "obs" (length rows) $ forM_ (zip [0 :: Int ..] rows) $ \(i, (j, yi)) -> do
         let (aj, bj) = coefs !! j
         mu <- deterministic ("mu_" <> T.pack (show i)) (aj + bj * realToFrac (xg j))
         observe ("y_" <> T.pack (show i)) (Normal mu s) [yi]
  pure ()

main :: IO ()
main = do
  createDirectoryIfMissing True "design/readme"
  -- DAG は構造由来なので少ない draws で十分。実行可能性の確認も兼ねて実際にサンプリングする。
  let cfg = defaultHBM { hbmChains = 2, hbmSamples = 300, hbmWarmup = 300
                       , hbmSeed = Just 20260623 }
  putStrLn "sampling intricate hierarchical model (実行可能性チェック)…"
  fit <- hbmModel cfg hierModel []
  putStrLn "  fit ok → DAG を描画"
  let noDf = [] :: [(T.Text, ColData)]
      dagPlot = noDf |>> toPlot (dagOf fit)
                <> title "階層ベイズ回帰の DAG (群レベル予測子つき 変量切片+傾き)"
                <> width 820 <> height 660
  saveSVGBound "design/readme/hbm-hier-dag.svg" dagPlot
  putStrLn "wrote design/readme/hbm-hier-dag.svg"
