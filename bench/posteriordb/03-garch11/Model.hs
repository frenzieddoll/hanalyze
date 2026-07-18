{-# LANGUAGE OverloadedStrings #-}
-- | garch-garch11 (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。GARCH(1,1) 時系列モデル (T=200)。
-- 分散 sigma[t] がパラメータ依存の逐次再帰 (sigma[t-1] から計算) という、
-- これまでの2モデル (GLM-Poisson・dogs) とは異なる構造。dogs の累積和は
-- observed data のみに依存する前処理だったが、本モデルの再帰は
-- alpha0/alpha1/beta1/mu という **サンプリング対象のパラメータに依存**
-- するため前処理不可 (毎回の勾配評価で再計算)。 'synthVecIR' がこの
-- パラメータ依存再帰にも対応することを事前に小規模トイモデルで実測確認済み
-- (`cabal repl` + 'synthVecIR' 直接呼び出し・`Just` を確認)。
--
-- mu/alpha0 は Stan の暗黙improper flat prior (無制約 real / 下限のみ) を
-- そのまま移植できない (hanalyze の Uniform は有限区間が必要)。実用上の
-- proper prior (mu~Normal(0,10)・alpha0~HalfNormal(5)) で代替する
-- (README「既知の課題」に記載)。alpha1/beta1 は Stan 原典の有界一様事前
-- 分布 (beta1 の上限が alpha1 に依存する動的境界) を忠実に移植する。
--
-- 高レベル API (`df |-> hbm`) を使用: データは 'dataNamedObs' で df から
-- 束縛する (説明変数無し・時系列そのもの)。 診断図は hgg の
-- 'dashboardFullOf' を 1 枚の PNG (rasterific backend) として出力する。
-- chain 数等の設定は PyMC 側 (`model.py`) と同じくコード中の定数 (= 4) で
-- 揃える (CLI 引数化しない)。
--
-- reference_posterior_name = "garch-garch11" — posteriordb に公式 reference
-- posterior あり (hanalyze vs PyMC vs 公式referenceの3者比較が可能)。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-garch11
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Vector as V

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedObs, plateForM_)
import Hanalyze.MCMC.Core (Chain, chainVals)
import Hanalyze.Model.HBM (gradPathLabel)
import Hanalyze.Plot (hbmModelSpec)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardFullOf, hbmChainsR)
import Hgg.Plot.Spec (ColData (..))
import Hgg.Plot.Frame (BoundPlot, (|>>))
import Hgg.Plot.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary)

-- | posteriordb の @garch_data.json@ 形状 ({"T":200,"y":[...],"sigma1":0.5})。
data GarchData = GarchData
  { yTS    :: [Double]
  , sigma1 :: Double  -- ^ sigma[1] の固定初期値 (データの一部)。
  }

instance FromJSON GarchData where
  parseJSON = withObject "GarchData" $ \v ->
    GarchData <$> v .: "y" <*> v .: "sigma1"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/03-garch11/data/garch_data.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/03-garch11/figures"

readData :: IO ([Double], Double)
readData = do
  d <- either fail pure =<< eitherDecodeFileStrict dataPath
  pure (yTS d, sigma1 d)

-- | GARCH(1,1): sigma[t] はパラメータ依存の逐次再帰 (Stan 原典と同一構造)。
-- @sigma[1] = sigma1@ (データの固定初期値)、@sigma[t] = sqrt(alpha0 +
-- alpha1*(y[t-1]-mu)^2 + beta1*sigma[t-1]^2)@。 データは df 経由で束縛
-- ('dataNamedObs')・観測は 'plateForM_' (docs/api-guide 規約)。
garch11Model :: Double -> ModelP ()
garch11Model s1 = do
  mu     <- sample "mu"     (Normal 0 10)
  alpha0 <- sample "alpha0" (HalfNormal 5)
  alpha1 <- sample "alpha1" (Uniform 0 1)
  beta1  <- sample "beta1"  (Uniform 0 (1 - alpha1))
  ys <- dataNamedObs "y" []
  let sigmas = scanl (\sPrev yPrev ->
                         sqrt (alpha0 + alpha1 * (realToFrac yPrev - mu) ^ (2 :: Int)
                               + beta1 * sPrev ^ (2 :: Int)))
                      (realToFrac s1) (init ys)
  plateForM_ "obs" (zip sigmas ys) $ \(s, yi) ->
    observe "y" (Normal mu s) [yi]

main :: IO ()
main = do
  (ys, s1) <- readData
  let df = [ ("y", NumData (V.fromList ys)) ] :: [(T.Text, ColData)]
      -- PyMC 側 (model.py) と同じ設定を定数で揃える (draws/tune=1000×4chain)。
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      m = df |-> hbm cfg (garch11Model s1)
      pars = ["mu", "alpha0", "alpha1", "beta1"]

  -- Phase 96 A2: 勾配経路 = compileGradUV が実際に選ぶ経路 (束縛済
  -- hbmModelSpec で判定・Phase 91 A4 と同型)。
  putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

  printSummary $ summarize pars (hbmChainsR m)
  writeDrawsCSV "bench/posteriordb/03-garch11/hs_draws.csv" pars (hbmChainsR m)

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
