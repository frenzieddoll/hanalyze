{-# LANGUAGE OverloadedStrings #-}
-- | nes1972-nes (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。ARM本 (Gelman & Hill 2006) Ch.4
-- の政党支持度回帰 (National Election Studies 1972年調査・N=1330)。
-- 9変数の線形回帰 (イデオロギー・人種・年齢層3ダミー・教育・性別・収入)。
--
-- Stan 原典 (posteriordb `models/stan/nes.stan`):
--   transformed data {
--     age30_44[n] = age_discrete[n]==2; age45_64[n] = age_discrete[n]==3;
--     age65up[n]  = age_discrete[n]==4;  // 年齢層ファクタをダミー化
--   }
--   parameters { vector[9] beta; real<lower=0> sigma; }
--   model {
--     partyid7 ~ normal(beta[1] + beta[2]*real_ideo + beta[3]*race_adj
--                      + beta[4]*age30_44 + beta[5]*age45_64 + beta[6]*age65up
--                      + beta[7]*educ1 + beta[8]*gender + beta[9]*income, sigma);
--   }
--
-- Stan 原典に明示的な prior 行は無い (暗黙の flat/improper prior)。
-- 01-glm-poisson/10-rats と同じ流儀で diffuse な代替を与える:
-- beta_i ~ Normal(0,1000) (回帰係数・UnconstrainedT なので unconstrained
-- 初期値問題なし)・sigma ~ HalfCauchy(25) (10-rats で確立した「Uniform(0,X)
-- 境界外初期値の罠」を回避する SD 事前分布・PositiveT変換)。
--
-- age30_44/age45_64/age65up は Stan の transformed data 相当として
-- readData 後に Haskell 側でダミー化する (df に個別列として渡す)。
-- N=1330×9変数の標準的な線形回帰は vecIR 高速経路が期待できる規模
-- (01-glm-poisson と同型の構造)。
--
-- reference_posterior_name = "nes1972-nes" (posteriordb に公式 reference
-- あり・hanalyze vs PyMC vs 公式reference の3者比較可能)。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-nes
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Printf (printf)

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedX, dataNamedObs, plateI_,
                                    gradPathLabel)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardFullOf, hbmChainsR, hbmModelSpec)
import Graphics.Hgg.Spec (ColData (..))
import Graphics.Hgg.Frame (BoundPlot, (|>>))
import Graphics.Hgg.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @nes1972.json@ 形状
-- ({"N":1330,"partyid7":[...],"real_ideo":[...],"race_adj":[...],
--   "educ1":[...],"gender":[...],"income":[...],"age_discrete":[...]})。
data NesRaw = NesRaw
  { partyid7Raw    :: [Double]
  , realIdeoRaw    :: [Double]
  , raceAdjRaw     :: [Double]
  , educ1Raw       :: [Double]
  , genderRaw      :: [Double]
  , incomeRaw      :: [Double]
  , ageDiscreteRaw :: [Int]
  }

instance FromJSON NesRaw where
  parseJSON = withObject "NesRaw" $ \v ->
    NesRaw <$> v .: "partyid7" <*> v .: "real_ideo" <*> v .: "race_adj"
           <*> v .: "educ1" <*> v .: "gender" <*> v .: "income"
           <*> v .: "age_discrete"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/17-nes/data/nes1972.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/17-nes/figures"

readData :: IO NesRaw
readData = either fail pure =<< eitherDecodeFileStrict dataPath

-- | 9変数線形回帰 (Stan 原典と同一構造)。 データは df 経由で束縛
-- ('dataNamedX'/'dataNamedObs')・O(1) 索引のため 'Data.Vector' に変換して
-- から 'plateI_' で反復する。
nesModel :: ModelP ()
nesModel = do
  b1 <- sample "beta1" (Normal 0 1000)
  b2 <- sample "beta2" (Normal 0 1000)
  b3 <- sample "beta3" (Normal 0 1000)
  b4 <- sample "beta4" (Normal 0 1000)
  b5 <- sample "beta5" (Normal 0 1000)
  b6 <- sample "beta6" (Normal 0 1000)
  b7 <- sample "beta7" (Normal 0 1000)
  b8 <- sample "beta8" (Normal 0 1000)
  b9 <- sample "beta9" (Normal 0 1000)
  sigma <- sample "sigma" (HalfCauchy 25)
  ideoV  <- V.fromList <$> dataNamedX "real_ideo" []
  raceV  <- V.fromList <$> dataNamedX "race_adj"  []
  a3044V <- V.fromList <$> dataNamedX "age30_44"  []
  a4564V <- V.fromList <$> dataNamedX "age45_64"  []
  a65upV <- V.fromList <$> dataNamedX "age65up"   []
  educV  <- V.fromList <$> dataNamedX "educ1"     []
  gendV  <- V.fromList <$> dataNamedX "gender"    []
  incV   <- V.fromList <$> dataNamedX "income"    []
  ysV    <- V.fromList <$> dataNamedObs "partyid7" []
  plateI_ "obs" (V.length ysV) $ \i ->
    let mu = b1 + b2 * (ideoV V.! i) + b3 * (raceV V.! i)
           + b4 * (a3044V V.! i) + b5 * (a4564V V.! i) + b6 * (a65upV V.! i)
           + b7 * (educV V.! i) + b8 * (gendV V.! i) + b9 * (incV V.! i)
    in observe "partyid7" (Normal mu sigma) [ysV V.! i]

main :: IO ()
main = do
  d <- readData
  let age3044 = [ if a == (2 :: Int) then 1 else 0 | a <- ageDiscreteRaw d ] :: [Double]
      age4564 = [ if a == (3 :: Int) then 1 else 0 | a <- ageDiscreteRaw d ] :: [Double]
      age65up = [ if a == (4 :: Int) then 1 else 0 | a <- ageDiscreteRaw d ] :: [Double]
      df = [ ("real_ideo", NumData (V.fromList (realIdeoRaw d)))
           , ("race_adj",  NumData (V.fromList (raceAdjRaw d)))
           , ("age30_44",  NumData (V.fromList age3044))
           , ("age45_64",  NumData (V.fromList age4564))
           , ("age65up",   NumData (V.fromList age65up))
           , ("educ1",     NumData (V.fromList (educ1Raw d)))
           , ("gender",    NumData (V.fromList (genderRaw d)))
           , ("income",    NumData (V.fromList (incomeRaw d)))
           , ("partyid7",  NumData (V.fromList (partyid7Raw d)))
           ] :: [(T.Text, ColData)]
      -- PyMC 側 (model.py) と同じ設定を定数で揃える。
      cfg = defaultHBM { hbmChains = 4, hbmSamples = 1000
                        , hbmWarmup = 1000, hbmSeed = Just 1 }
      m = df |-> hbm cfg nesModel

  -- 勾配経路 = compileGradUV が実際に選ぶ経路 (束縛済モデルで判定)。
  -- Phase 91 A4: 9変数線形回帰は Gaussian LM 閉形式ブロックに吸収される。
  -- ★生の nesModel を synthVecIR に渡すと data 空で Nothing と誤表示するため
  --   'hbmModelSpec m' (df 束縛済) を 'gradPathLabel' に渡す。
  putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
    (noDf |>> dashboardFullOf m "partyid7" :: BoundPlot)

  printSummary $ summarize
    ["beta1", "beta2", "beta3", "beta4", "beta5", "beta6", "beta7", "beta8", "beta9", "sigma"]
    (hbmChainsR m)
