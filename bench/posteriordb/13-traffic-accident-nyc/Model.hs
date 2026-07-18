{-# LANGUAGE OverloadedStrings #-}
-- | traffic_accident_nyc-bym2_offset_only (posteriordb) — hanalyze (ModelP) 実装。
--
-- Phase 89: posteriordb 横断ベンチマーク。BYM2 空間疫学モデル (Morris et al.
-- 2019) — NYC 交通事故データ (N=1921地域・N_edges=5461隣接ペア)。ICAR
-- (intrinsic conditional autoregressive) 事前分布を「隣接ペアごとの
-- 差分ペナルティ」(`target += -0.5*dot_self(phi[node1]-phi[node2])`) で
-- 表現する Stan の標準的な BYM2 実装 (N×N 精度行列の陽な構築/逆行列計算を
-- 回避する定石)。
--
-- Stan 原典 (posteriordb `models/stan/bym2_offset_only.stan`):
--   parameters { real beta0; real<lower=0> sigma; real<lower=0,upper=1> rho;
--                vector[N] theta; vector[N] phi; }
--   transformed parameters {
--     convolved_re = sqrt(1-rho)*theta + sqrt(rho/scaling_factor)*phi;
--   }
--   model {
--     y ~ poisson_log(log_E + beta0 + convolved_re*sigma);
--     target += -0.5 * dot_self(phi[node1] - phi[node2]);  -- ICAR pairwise
--     beta0 ~ normal(0,1); theta ~ normal(0,1); sigma ~ normal(0,1);
--     rho ~ beta(0.5,0.5);
--     sum(phi) ~ normal(0, 0.001*N);                        -- soft sum-to-zero
--   }
--
-- ★`phi` は Stan 原典で **固有の (marginal) prior を持たない** (`theta`とは
-- 対照的)。ICAR ペナルティ + ソフトゼロ和制約のみが phi の情報源であり、
-- 事実上「improper flat」を前提にしている。hanalyze には improper flat
-- distribution が無いため、他モデル (01-glm-poisson の Uniform 箱等) と
-- 同じ流儀で **`Normal 0 1000` という極めて diffuse な近似**を phi 自身の
-- 周辺成分に与える (ICAR ペナルティが実質的に支配的なので事後への影響は
-- 無視できる想定・要実測確認)。
--
-- `target += ...` の非標準尤度項は `potential` (PyMC `pm.Potential` 相当)
-- で表現する。ソフトゼロ和制約 (`sum(phi) ~ normal(...)`) は
-- `logDensity (Normal 0 sd) (sum phis)` を `potential` に渡す形で実装する。
--
-- reference_posterior_name = null (posteriordb に公式 reference 無し・2者比較のみ)。
--
-- ★N=1921 (theta+phi=3842 latent) + N_edges=5461 という大規模モデル。
-- Phase 90 A10-1 実測 (2026-07-11) により: Poisson 尤度 + theta/phi 族は
-- **vecIR に吸収済み (synthVecIR = Just)**。 ただし `potential` 2 項
-- (icar / sum_zero) は vecIR 非対応で残差 ad に落ち、 これが勾配 1 回
-- ~0.13s の 93% を占める (potential 無し対照 = 0.009s)。 詳細は
-- specification/phases/phase-90-vecir-gap-extensions.md §A10-1。
--
-- ビルド: cabal build --project-file=cabal.project.plot posteriordb-bym2
module Main (main) where

import Data.Aeson (FromJSON (..), withObject, (.:), eitherDecodeFileStrict)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as BV
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Printf (printf)

import Hanalyze.Model.HBM (ModelP, Distribution (..), sample, observe,
                                    dataNamedX, dataNamedObs, plateI, plateForM_,
                                    potential, logDensity, (.#), sampleNames)
import qualified Hanalyze.Model.HBM.Gradient as G
import qualified Hanalyze.Model.HBM.IR as IR
import Hanalyze.Model.HBM (gradPathLabel)
import Hanalyze.Plot (hbmModelSpec)
import Hanalyze.Plot (HBMConfig (..), defaultHBM, hbm, (|->),
                              dashboardOf, hbmChainsR)
import Graphics.Hgg.Spec (ColData (..))
import Graphics.Hgg.Frame (BoundPlot, (|>>))
import Graphics.Hgg.Backend.Rasterific (savePNGBound)

import Common (summarize, printSummary, timeSamplingMs)

-- | posteriordb の @traffic_accident_nyc.json@ 形状
-- ({"N":1921, "N_edges":5461, "node1":[...], "node2":[...], "y":[...],
--   "E":[...], "scaling_factor":0.7137})。
data TrafficData = TrafficData
  { nAreas   :: Int
  , nEdges   :: Int
  , node1v   :: [Int]
  , node2v   :: [Int]
  , yObs     :: [Int]
  , eOffset  :: [Double]
  , scalingF :: Double
  }

instance FromJSON TrafficData where
  parseJSON = withObject "TrafficData" $ \v ->
    TrafficData <$> v .: "N" <*> v .: "N_edges" <*> v .: "node1" <*> v .: "node2"
                <*> v .: "y" <*> v .: "E" <*> v .: "scaling_factor"

noDf :: [(T.Text, ColData)]
noDf = []

dataPath :: FilePath
dataPath = "bench/posteriordb/13-traffic-accident-nyc/data/traffic_accident_nyc.json"

figuresDir :: FilePath
figuresDir = "bench/posteriordb/13-traffic-accident-nyc/figures"

readData :: IO TrafficData
readData = either fail pure =<< eitherDecodeFileStrict dataPath

-- | BYM2 空間疫学モデル。edges (0始まりに変換済のnode1/node2ペア)・
-- scalingFactor はデータ由来の固定構造として closure で渡す。
--
-- ★Phase 90 A10-1: data list は引数で直接束縛する (`df |->` は同名同値の
-- 束縛なので挙動不変)。 旧実装は `dataNamedX "log_E" []` (空 list) だったため
-- `main` の `synthVecIR` 診断が「観測行 0 → Nothing」 を印字してしまい、
-- 実サンプラ (df 束縛済) が vecIR = Just で走っている事実を隠していた。
bym2Model :: Int -> [(Int, Int)] -> Double -> [Double] -> [Double] -> ModelP ()
bym2Model n edges scalingFactor logEsIn ysIn = do
  beta0 <- sample "beta0" (Normal 0 1)
  sigma <- sample "sigma" (HalfNormal 1)
  rho   <- sample "rho"   (Beta 0.5 0.5)
  thetas <- plateI "theta" n $ \i -> sample ("theta" .# i) (Normal 0 1)
  phis   <- plateI "phi"   n $ \i -> sample ("phi"   .# i) (Normal 0 1000)
  logEs <- dataNamedX   "log_E" logEsIn
  ys    <- dataNamedObs "y"     ysIn
  let scalingA = realToFrac scalingFactor
      convolved i = sqrt (1 - rho) * (thetas !! i) + sqrt (rho / scalingA) * (phis !! i)
  plateForM_ "obs" (zip3 [0 ..] logEs ys) $ \(i, logEi, yi) ->
    let eta = logEi + beta0 + convolved i * sigma
    in observe "y" (Poisson (exp eta)) [yi]
  -- ICAR ペア差分ペナルティ (Stan の `target += -0.5*dot_self(phi[n1]-phi[n2])`)。
  let icarPenalty = negate 0.5 * sum
        [ (phis !! a - phis !! b) * (phis !! a - phis !! b) | (a, b) <- edges ]
  potential "icar" icarPenalty
  -- ソフトゼロ和制約 (`sum(phi) ~ normal(0, 0.001*N)`)。
  let sdSumZero = realToFrac (0.001 * fromIntegral n :: Double)
  potential "sum_zero" (logDensity (Normal 0 sdSumZero) (sum phis))

-- ===========================================================================
-- Phase 99 A2: vecIR arena 命令列の静的 dump (ICAR の融合度を実測)
-- ===========================================================================

-- | @synthVecIR@ → @compileVecIR@ でコンパイルし、命令種別の内訳と長さ分布を
-- 出力する。ICAR (5461 edge の二次形式) が融合ベクトル op か scalar 展開かを
-- 目視判定するための静的解析 (BenchHBMVecIRProf.instrMix と同型)。
dumpIR :: ModelP () -> IO ()
dumpIR m = do
  let names = sampleNames m
      nP    = length names
  putStrLn $ "sampleNames nP = " ++ show nP
  case IR.synthVecIR m of
    Nothing -> putStrLn "synthVecIR = Nothing (vecIR に乗っていない)"
    Just (gs, fams, sObs) -> do
      let ixOf = Map.fromList (zip names [0 :: Int ..])
          cvi  = IR.compileVecIR ixOf gs fams
          prog = IR.cvProg cvi
          instrs = BV.toList (IR.vpInstrs prog)
          lens   = VU.toList (IR.vpLen prog)
          keyOf ins = case ins of
            IR.VIK{}       -> "VIK   (スカラ定数)"
            IR.VIKV{}      -> "VIKV  (ベクトル定数)"
            IR.VILeafS{}   -> "VILeafS (scalar leaf)"
            IR.VILeafV{}   -> "VILeafV (vector leaf)"
            IR.VIGath{}    -> "VIGath (gather)"
            IR.VIUn{}      -> "VIUn  (elementwise 単項)"
            IR.VIBin{}     -> "VIBin (elementwise 二項)"
            IR.VISum{}     -> "VISum (Σ 縮約)"
            IR.VIAxpy{}    -> "VIAxpy (a+s·v 融合)"
            IR.VIAxpyC{}   -> "VIAxpyC (a+s·const 融合)"
            IR.VISumSqD{}  -> "VISumSqD (Σ(x−m)² 融合)"
            IR.VISumSqC{}  -> "VISumSqC (Σ(c−m)² 融合)"
            IR.VIMulG{}    -> "VIMulG (s·gather 融合)"
            IR.VIAxpyG{}   -> "VIAxpyG (a+s·gather 融合)"
            IR.VIMulVC{}   -> "VIMulVC (s·v⊙c 融合)"
            IR.VISumSqC2{} -> "VISumSqC2 (Σ(c−m1−m2)² 融合)"
            IR.VISumSqDGG{} -> "VISumSqDGG (Σ(gath−gath)² 融合 = ICAR)"
          accum mp (ins, l) =
            Map.insertWith (\(c1, e1) (c2, e2) -> (c1 + c2, e1 + e2))
              (keyOf ins) (1 :: Int, max 1 l) mp
          mixed = [ (k, c, e) | (k, (c, e)) <- Map.toList (foldl accum Map.empty (zip instrs lens)) ]
                    :: [(String, Int, Int)]
          famSet = Set.fromList (concat [ ms | (ms, _, _) <- fams ])
          cps    = G.constPriorsOf m famSet
          exclNames = sObs `Set.union` famSet
                      `Set.union` Set.fromList (map fst cps)
          noResid = G.residualFreeOfDensity exclNames m
      printf "instrs=%d  vpSize(arena セル)=%d  guards=%d\n"
        (BV.length (IR.vpInstrs prog)) (IR.vpSize prog)
        (length (IR.vpGuards prog))
      printf "residual ad (mPriorGrad): %s  (constPriors=%d, excl=%d/%d)\n"
        (if noResid then "なし (noResid)" else "★あり = per-eval に ad が乗る" :: String)
        (length cps) (Set.size exclNames) nP
      putStrLn "\n--- 命令列 mix (種別 / 本数 / 総セル数) ---"
      mapM_ (\(k, c, e) -> printf "  %-28s %5d 本  %8d セル\n" k c e) mixed

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  d0 <- readData
  -- ★A9 プローブ用の一時トランケーション: 引数に N を与えると先頭 N 地域 +
  --   両端点が N 未満の edge だけに縮小する (スケーリング実測用)。
  args <- getArgs
  let d = case args of
            (nStr:_) | [(nn, "")] <- reads nStr ->
              let keep = [ (a, b) | (a, b) <- zip (node1v d0) (node2v d0)
                                  , a <= nn, b <= nn ]
              in d0 { nAreas = nn, nEdges = length keep
                    , node1v = map fst keep, node2v = map snd keep
                    , yObs = take nn (yObs d0), eOffset = take nn (eOffset d0) }
            _ -> d0
  putStrLn $ "N = " ++ show (nAreas d) ++ ", N_edges = " ++ show (nEdges d)
  let n = nAreas d
      edges = [ (a - 1, b - 1) | (a, b) <- zip (node1v d) (node2v d) ]  -- 0始まりに変換
      logEArr = map log (eOffset d)
      df = [ ("log_E", NumData (V.fromList logEArr))
           , ("y",     NumData (V.fromList (map fromIntegral (yObs d))))
           ] :: [(T.Text, ColData)]
  let ysD = map fromIntegral (yObs d)

  -- ★Phase 99 A2: `dumpir` = vecIR arena の静的命令列を dump し、ICAR ペア差分
  --   二次形式が (a) 融合 op (VIGath+VISumSqD 等) か (b) 5461 個の scalar op に
  --   展開されているかを実測する (A2a の prize サイズ判定・推測するな計測せよ)。
  case args of
    ["dumpir"] -> do
      let rawM :: ModelP ()
          rawM = bym2Model n edges (scalingF d) logEArr ysD
      dumpIR rawM
      exitSuccess
    _ -> pure ()

  -- ★N=1921・N_edges=5461 の大規模モデル。本番計測 (4chain×warmup1000+
  -- draws1000) の前に、まず縮小設定 (1chain×warmup3+draws3) でタイミング
  -- プローブを行い、フル run の所要時間を見積もってから判断すること
  -- (03-garch11/08-hudson-lynx-hare と同じ「保留判断」の慎重さで臨む)。
  -- probe モード: 引数 <N> = 縮小 cfg (1chain・3+3) / <N> <warmup> <draws>
  -- = 1chain で指定サイズ (Phase 90 A11 のプロファイリング run 用)。
  let cfg = case args of
        (_:wStr:dStr:_)
          | [(w, "")] <- reads wStr, [(dd, "")] <- reads dStr ->
              defaultHBM { hbmChains = 1, hbmSamples = dd
                         , hbmWarmup = w, hbmSeed = Just 1 }
        (_:_) -> defaultHBM { hbmChains = 1, hbmSamples = 3
                            , hbmWarmup = 3, hbmSeed = Just 1 }
        []    -> defaultHBM { hbmChains = 4, hbmSamples = 1000
                            , hbmWarmup = 1000, hbmSeed = Just 1 }
      m = df |-> hbm cfg (bym2Model n edges (scalingF d) logEArr ysD)

  -- 勾配経路 = compileGradUV が実際に選ぶ経路 (束縛済 hbmModelSpec で判定・
  -- Phase 91 A4: BYM2 は vecIR に吸収済。旧診断は生モデルを synthVecIR に渡し
  -- 観測 0 行 → Nothing と誤表示していた ので hbmModelSpec m に差替)。
  putStrLn $ "勾配経路 = " ++ gradPathLabel (hbmModelSpec m)

  (_, samplingMs) <- timeSamplingMs (hbmChainsR m)
  printf "sampling wall = %.1f ms (draws only, no dashboard/startup)\n" samplingMs

  -- N=1921×2 latent のため dashboardFullOf は非実用サイズ (05-mh と同じ
  -- 判断)。健全性2x2パネル (DAG/forest/PPC/energy) のみ。
  -- (A9 プローブのトランケーション時は figure を汚さないためスキップ)
  case args of
    (_:_) -> pure ()
    []    -> savePNGBound (figuresDir ++ "/hs_dashboard_full.png") $
               (noDf |>> dashboardOf m "y" :: BoundPlot)

  printSummary $ summarize ["beta0", "sigma", "rho"] (hbmChainsR m)
