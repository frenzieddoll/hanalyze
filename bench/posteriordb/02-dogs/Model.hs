{-# LANGUAGE OverloadedStrings #-}
-- | dogs-dogs (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。Solomon & Wynne (1953) の犬の
-- 回避学習実験 (30 匹 × 25 試行、Gelman & Hill 2006 Ch.24 の ARM 本例) を、
-- 累積の回避/被ショック回数を共変量とするロジスティック回帰でモデル化する。
-- Stan 原典の transformed parameters (n_avoid/n_shock) は beta 非依存の
-- 純粋な y (観測データ) の累積和なので、サンプリング前の前処理として
-- 1 度だけ計算する (Stan は反復ごとに再計算するが数学的には同一)。
--
-- 高レベル API (`df |-> hbm`) を使用: データは 'dataNamedX'/'dataNamedObs' で
-- df から束縛し、 反復は 'plateForM_' で書く (docs/api-guide/03-bayesian-hbm.md
-- の規約どおり)。 診断図は hgg の 'dashboardFullOf' を 1 枚の PNG
-- (rasterific backend) として出力する。 chain 数等の設定は PyMC 側
-- (`model.py`) と同じくコード中の定数 (= 4) で揃える (CLI 引数化しない)。
--
-- reference_posterior_name = null (posteriordb に公式 reference posterior 無し)。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-dogs
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Vector as V

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedX, dataNamedObs, plateForM_)
import Hanalyze.MCMC.Core (Chain, chainVals)
import Hanalyze.Model.HBM (gradPathLabel)
import Hanalyze.Plot (hbmModelSpec)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardFullOf, hbmChainsR)
import Graphics.Hgg.Spec (ColData (..))
import Graphics.Hgg.Frame (BoundPlot, (|>>))
import Graphics.Hgg.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary)

-- | posteriordb の @dogs_data.json@ 形状 ({"n_dogs":30,"n_trials":25,"y":[[...]]})。
data DogsData = DogsData
  { yMatrix :: [[Int]]  -- ^ 30匹 × 25試行の 0/1 (0=回避成功, 1=ショック)。
  }

instance FromJSON DogsData where
  parseJSON = withObject "DogsData" $ \v ->
    DogsData <$> v .: "y"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/02-dogs/data/dogs_data.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/02-dogs/figures"

readData :: IO [[Int]]
readData = do
  d <- either fail pure =<< eitherDecodeFileStrict dataPath
  pure (yMatrix d)

-- | 1匹分の回避/被ショック累積回数 (beta 非依存・y のみに依存する前処理)。
-- @n_avoid[0] = n_shock[0] = 0@、以降は前試行までの累積。
cumulativeCounts :: [Int] -> ([Double], [Double])
cumulativeCounts ys =
  ( scanl (\a y -> a + 1 - fromIntegral y) 0 (init ys)
  , scanl (\s y -> s + fromIntegral y) 0 (init ys)
  )

-- | 累積回避/被ショック回数を共変量とするロジスティック回帰 (Stan 原典と
-- 同一構造)。 データは df 経由で束縛 ('dataNamedX'/'dataNamedObs')・
-- 反復は 'plateForM_' (docs/api-guide 規約)。
dogsModel :: ModelP ()
dogsModel = do
  beta1 <- sample "beta1" (Normal 0 100)
  beta2 <- sample "beta2" (Normal 0 100)
  beta3 <- sample "beta3" (Normal 0 100)
  navoid <- dataNamedX   "n_avoid" []
  nshock <- dataNamedX   "n_shock" []
  ys     <- dataNamedObs "y"       []
  plateForM_ "obs" (zip3 navoid nshock ys) $ \(na, ns, yi) ->
    let logitP = beta1 + beta2 * na + beta3 * ns
        p      = 1 / (1 + exp (negate logitP))
    in observe "y" (Bernoulli p) [yi]

main :: IO ()
main = do
  rows <- readData
  let (avoidRows, shockRows) = unzip (map cumulativeCounts rows)
      yFlat     = map fromIntegral (concat rows) :: [Double]
      avoidFlat = concat avoidRows
      shockFlat = concat shockRows
      df = [ ("n_avoid", NumData (V.fromList avoidFlat))
           , ("n_shock", NumData (V.fromList shockFlat))
           , ("y",       NumData (V.fromList yFlat))
           ] :: [(T.Text, ColData)]
      -- PyMC 側 (model.py) と同じ設定を定数で揃える (draws/tune=1000×4chain)。
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      m = df |-> hbm cfg dogsModel
      pars = ["beta1", "beta2", "beta3"]

  -- Phase 96 A2: 勾配経路 = compileGradUV が実際に選ぶ経路 (束縛済
  -- hbmModelSpec で判定・Phase 91 A4 と同型)。
  putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

  printSummary $ summarize pars (hbmChainsR m)
  writeDrawsCSV "bench/posteriordb/02-dogs/hs_draws.csv" pars (hbmChainsR m)

  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardFullOf m "y" :: BoundPlot)

-- | arviz 独立検証用の生 draw ダンプ (chain,draw,<param>...) — hanalyze 自身の
-- 診断コードは使わず、外部 (arviz) で bulk/tail ESS・R-hat を再計算するため。
writeDrawsCSV :: FilePath -> [T.Text] -> [Chain] -> IO ()
writeDrawsCSV path pars chains = writeFile path (unlines (header : rows))
  where
    header = intercalate "," ("chain" : "draw" : map T.unpack pars)
    rows = [ intercalate "," (show ci : show di : [ show (chainVals p ch !! di) | p <- pars ])
           | (ci, ch) <- zip [0 :: Int ..] chains
           , di <- [0 .. length (chainVals (head pars) ch) - 1] ]
