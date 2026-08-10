{-# LANGUAGE OverloadedStrings #-}
-- | Phase 46 (hgg 統合) デモ + 静的 HTML ビューア生成。
--
-- 系統 A の 1 経路 (`df |>> (layer (scatter "x" "y") <> toPlot fit)`) で散布図に
-- 回帰線 / GP credible band を重ねた SVG を書き出し、 さらに HS backend の SVG を
-- **1 枚の `viewer.html` に埋め込む** (compare.html = PS bundle 依存とは別経路。
-- ブラウザで開くだけ・esbuild 不要)。 別パッケージ @hanalyze-plot@
-- (cabal.project.plot) の executable:
--   cabal run --project-file=cabal.project.plot plot-integration-demo
module Main (main) where

import           Control.Monad            (forM, forM_)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import qualified Data.Vector              as V
import qualified Numeric.LinearAlgebra    as LA
import           System.Directory         (createDirectoryIfMissing)
import           System.IO                (hPutStrLn, stderr)

import           Graphics.Hgg.Backend.SVG (renderBound, renderSVG, saveSVGBound)
import           Graphics.Hgg.Easy        (bars, hist, lineXY, plots, points)
import           Graphics.Hgg.Frame       ((|>>))
import           Graphics.Hgg.Spec        (ColData (..), layer, scatter, color, colorBy, fromHex, title, subplots, subplotCols, vconcat, selectPanels, width, height, pairs)
import           Graphics.Hgg.Color.Named (red, blue, green, orange)
-- Family (..) を入れると Binomial が HBM.Distribution.Binomial と衝突するため、
-- 本デモで使う Poisson / Gaussian のみ明示 import する。
import           Hanalyze.Model.GLM       (Family (Gaussian, Poisson), LinkFn (..))
import           Hanalyze.Model.GP        (GPModel (..), Kernel (..), fitGP,
                                           defaultGPParams)
import qualified Data.Map.Strict          as Map
import           Hanalyze.MCMC.Core       (Chain (..))
import           Hanalyze.Model.CompetingRisks (CRSample (..), fitCompetingRisks)
import           Hanalyze.Model.MultiLM   (fitMultiLM)
import           Hanalyze.Model.Robust    (RobustEstimator (..), defaultHuberK)
import           Hanalyze.Model.Spline    (SplineKind (..))
import           Hanalyze.Model.Survival  (Event (..), SurvSample (..),
                                           kaplanMeier)
import qualified System.Random.MWC        as MWC
import           Hanalyze.Model.PCA       (PCAStandardize (..), pca)
import           Hanalyze.Model.RandomForest (defaultRandomForest, fitRF, RandomForest, RFConfig (..))
import           Hanalyze.Plot            (forecastModel)
import           Hanalyze.Plot            (toPlot, diagnosticPlots, lmModel,
                                           glmModel, splineModel, gamModel,
                                           robustModel, quantileModel, chainModel,
                                           statModel, grid, predAt, bandMode, BandMode (..),
                                           piMethod, PIMethod (..),
                                           statColor, statLabel, statFill,
                                           (|->), lm, glm, spline, glmmF,
                                           randomForestReg, randomForestCls, decisionTree, knnCls,
                                           centralCompositeDesign, designTable, designModel, designModelGP, designModelHBM,
                                           ranIntercept, contFactor, multiOutput,
                                           profiler, profilerResidual, ResidualMode (..), contourOf,
                                           directLingam, varLingam, pairwiseLingam, multiGroupLingam,
                                           bootstrapLingam, icaLingam, bootstrapEdgeProbOf,
                                           correlationOf,
                                           mds, defaultMDS,
                                           grouped, weighted,
                                           gam, GAMConfig (..), GAMBasis (..), GAMLambda (..),
                                           gp, defaultGP, GPConfig (..), GPMethod (..),
                                           HyperStrategy (..), Kernel (..),
                                           ridge, lasso, RegModel (..),
                                           multiLMModel, statModelMulti,
                                           surfaceGrid, defaultSurfaceOpts,
                                           along, byVar, holdAt, HoldAgg (..),
                                           hbmModel, defaultHBM, HBMConfig (..),
                                           epred, forestOf, ppcOf, dagOf,
                                           dagOfModelWith,
                                           dashboardOf, dashboardFullOf, traceDensityOf,
                                           autocorrOf, rankOf,
                                           marginalsOf,
                                           TraceOpts (..), defaultTraceOpts,
                                           tracesOf, tracesOfWith, marginalsByChainOf,
                                           hbmParamNames, divergencesOf,
                                           pairOf, energyOf,
                                           clusterScatterOf, centroidsOf,
                                           clusterHullOf, clusterEllipseOf,
                                           dendrogramOf, dendrogramOf', defaultDendroOpts, DendroOpts (..),
                                           decisionBoundaryOf, confusionOf,
                                           mdsView, mdsGroupBy, nnLossOf,
                                           pdp, pdpIce,
                                           scoreView, loadingView, vipView,
                                           multiGpCurves, regPathPlot, lingamDag,
                                           testForestLabeled, describeBox,
                                           coefForest, obsVsPred)
import           Hanalyze.Model.Cluster   (kMeans, defaultKMeans)
import           Hanalyze.Model.HierarchicalCluster (fitHierarchical, Linkage (..), HClusterFit (..))
import           Hanalyze.Model.GradientBoosting (fitGBRegressor, defaultGBM)
import           Hanalyze.Model.RandomForestClassifier (fitRFClassifier, defaultRFCConfig)
import           Hanalyze.Model.DecisionTree (fitDTV, defaultDecisionTree, DTConfig (..))
import qualified Data.Vector.Unboxed      as VU
import           Hanalyze.Model.Discriminant (fitLDA)
import           Hanalyze.Model.NaiveBayes (fitGNB, NBModel (..))
import           Hanalyze.Model.NeuralNetwork (fitMLPClassifier, defaultMLP, MLPConfig (..))
import           Hanalyze.Model.SVM (fitSVM, defaultSVM
                                       , SVMConfig (..))
import           Hanalyze.Model.GP (Kernel (..), GPParams (..), defaultGPParams)
import           Hanalyze.Model.Kernel (defaultKernelParams, KernelParams (..))
import           Hanalyze.Model.PLS (fitPLS, defaultPLS, PLSConfig (..))
import           Hanalyze.Model.MultiGP (fitMultiGP)
import           Hanalyze.Model.GARCH (fitGARCH)
import           Hanalyze.Model.AFT (fitAFT, AFTDistribution (..))
import           Hanalyze.Model.FDA (smoothBasis, functionalPCA)
import qualified Hanalyze.Model.FDA as FDA
import           Hanalyze.Model.Regularized (regularizationPath, Penalty (..))
import           Hanalyze.Model.LiNGAM.Direct (defaultDirectLiNGAMConfig, DirectLiNGAMConfig (..))
import           Hanalyze.Model.LiNGAM.VAR (defaultVARLiNGAMConfig)
import           Hanalyze.Model.LiNGAM.MultiGroup (defaultMultiGroupConfig, MultiGroupConfig (..))
import           Hanalyze.Model.LiNGAM.Bootstrap (defaultBootstrapConfig, BootstrapConfig (..))
import           Hanalyze.Model.LiNGAM.ICA (defaultICALiNGAMConfig, ICALiNGAMConfig (..))
import           Hanalyze.Stat.Test (tostWelch)
import           Hanalyze.Model.HBM       (Distribution (Normal, HalfNormal,
                                             Beta, Binomial, Exponential),
                                           sample, dataNamedX, dataNamedObs, deterministic,
                                           observe, indexed, plateForM_, ModelP)
import qualified DataFrame.Internal.Column    as DX
import qualified DataFrame.Internal.DataFrame  as DX
import qualified Graphics.Hgg.ThreeD.Spec  as P3
import qualified Graphics.Hgg.ThreeD.Easy  as P3E
import           Graphics.Hgg.ThreeD.Types (cameraIso, Point3 (..), Camera3D (..), zUp)

-- | Phase 49 デモ用 HBM (ベイズ線形回帰)。 ★PyMC 同等の DAG (a,b → mu → obs, s → obs):
--   deterministic "mu" を観測ループ内で使うと Track が det 名に再ラベルされ obs の親が {mu,s} に
--   なる (extractDeps)。 observe "obs" は同名ゆえ mergeByName で 1 ノードに統合される。
--   epred も muName="mu" を x=[gx] で 1 回評価する O1 規約のまま動く。
demoHbmModel :: ModelP ()
demoHbmModel = do
  x <- dataNamedX   "x" []   -- 説明変数: モデル数値型 [a] (realToFrac 不要)
  y <- dataNamedObs "y" []   -- 目的変数: observe に渡す生 [Double]
  a <- sample "a" (Normal 0 10)
  b <- sample "b" (Normal 0 10)
  s <- sample "s" (HalfNormal 1)
  -- ★A15-3: 観測ループを plate で囲む (PyMC 流)。 plateForM_ が plate+length+forM_ を畳む。
  -- mu / obs が plate メンバになり、 DAG に "obs (N)" の囲み枠 + 個数が出る。
  plateForM_ "obs" (zip x y) $ \(xi, yi) -> do
    mu <- deterministic "mu" (a + b * xi)
    observe "obs" (Normal mu s) [yi]

-- Phase 74: 発散 (divergence) デモ用の **中心化 8-schools** (Rubin 1981 の古典データ)。
-- θ_j ~ Normal(μ, τ) を中心化 parameterization で書くと τ 小領域で funnel になり、
-- NUTS が posterior の首で発散する (= 実モデルでも普通に出る「多少の発散」 の代表例)。
-- これで 'tracesOf' の発散 rug (既定 ON) が実際に図に現れる。 per-school の既知 SE は
-- 分布パラメータゆえモデル数値型へ 'realToFrac' で持ち上げる。
eightSchoolsY, eightSchoolsSigma :: [Double]
eightSchoolsY     = [28, 8, -3, 7, -1, 1, 18, 12]
eightSchoolsSigma = [15, 10, 16, 11, 9, 11, 10, 18]

eightSchoolsCentered :: ModelP ()
eightSchoolsCentered = do
  mu  <- sample "mu"  (Normal 0 5)
  tau <- sample "tau" (HalfNormal 5)
  forM_ (zip3 [1 :: Int ..] eightSchoolsY eightSchoolsSigma) $ \(j, yj, sj) -> do
    theta <- sample (indexed "theta" j) (Normal mu tau)
    observe (indexed "y" j) (Normal theta (realToFrac sj)) [yj]

-- ---------------------------------------------------------------------------
-- docs/bayesian/02-probabilistic-model.md の各パターン用モデル (Pattern 1/2/3/5/7/8)。
-- 図は marginalsOf (事後分布) / forestOf (HDI) / dagOf (構造) で生成し docs/images へ。
-- データは closure 焼き込み (epred/ppc 不使用ゆえ hbmModel の dat=[] でよい)。
-- ---------------------------------------------------------------------------

docYs1 :: [Double]              -- Pattern 1/2 共通の観測
docYs1 = [4.8, 5.2, 4.9, 5.5, 5.0, 4.7, 5.3, 5.1]

-- Pattern 1: 単純正規 (μ→y, σ=2 既知)。
docP1 :: ModelP ()
docP1 = do
  mu <- sample "mu" (Normal 0 10)
  observe "y" (Normal mu 2) docYs1

-- Pattern 2: σ 未知 (μ,σ→y)。
docP2 :: ModelP ()
docP2 = do
  mu    <- sample "mu"    (Normal 0 10)
  sigma <- sample "sigma" (Exponential 1)
  observe "y" (Normal mu sigma) docYs1

-- Pattern 3: A/B テスト (Beta-Binomial)。
docP3 :: ModelP ()
docP3 = do
  pCtrl <- sample "p_ctrl" (Beta 1 1)
  pTrt  <- sample "p_trt"  (Beta 1 1)
  observe "y_ctrl" (Binomial 50 pCtrl) [18]
  observe "y_trt"  (Binomial 50 pTrt)  [31]

-- Pattern 5: 階層モデル (form A・μ,τ→θ_j→y_j)。
docGroups5 :: [[Double]]
docGroups5 = [ [1.1,0.8,1.3,1.0], [4.9,5.2,4.7,5.1], [9.0,8.7,9.3,8.9] ]
docP5 :: ModelP ()
docP5 = do
  mu  <- sample "mu"  (Normal 0 10)
  tau <- sample "tau" (HalfNormal 5)
  forM_ (zip [1::Int ..] docGroups5) $ \(j, ys) -> do
    theta <- sample (indexed "theta" j) (Normal mu tau)
    observe (indexed "y" j) (Normal theta 1) ys

-- Pattern 7: 3 階層 nested (district→school→students)。 2×2 の小型インスタンス。
docByDistrict7 :: [[[Double]]]
docByDistrict7 = [ [ [1.0,1.2], [2.0,2.1] ], [ [5.0,5.3], [6.1,5.9] ] ]
docP7 :: ModelP ()
docP7 = do
  mu  <- sample "mu"    (Normal 0 10)
  tD  <- sample "tau_d" (HalfNormal 5)
  tS  <- sample "tau_s" (HalfNormal 5)
  forM_ (zip [1::Int ..] docByDistrict7) $ \(d, schools) -> do
    delta <- sample (indexed "delta" d) (Normal mu tD)
    forM_ (zip [1::Int ..] schools) $ \(s, ys) -> do
      theta <- sample (T.pack (concat ["theta_", show d, "_", show s])) (Normal delta tS)
      observe (T.pack (concat ["y_", show d, "_", show s])) (Normal theta 1) ys

-- Pattern 8: 交差 (crossed) ランダム効果 (school × year)。
docObs8 :: [(Int, Int, Double)]
docObs8 = [ (0,0,3.1), (0,1,2.0), (1,0,4.2), (1,1,3.1), (2,0,5.0), (2,1,4.1) ]
docP8 :: ModelP ()
docP8 = do
  muA <- sample "mu_alpha" (Normal 0 10)
  tA  <- sample "tau_a"    (HalfNormal 5)
  tG  <- sample "tau_g"    (HalfNormal 5)
  sig <- sample "sigma"    (Exponential 1)
  alphas <- forM [0..2::Int] $ \s -> sample (indexed "alpha" s) (Normal muA tA)
  gammas <- forM [0..1::Int] $ \t -> sample (indexed "gamma" t) (Normal 0 tG)
  forM_ docObs8 $ \(s, t, y) ->
    observe (T.pack (concat ["y_", show s, "_", show t]))
            (Normal (alphas !! s + gammas !! t) sig) [y]

main :: IO ()
main = do
  createDirectoryIfMissing True "design/plot-integration"

  -- (1) 線形モデル: 散布図 + 回帰線 + CI band (A4)。
  let xsL = [1, 2, 3, 4, 5, 6, 7, 8] :: [Double]
      ysL = [2.1, 3.9, 6.2, 7.8, 10.3, 11.7, 14.1, 16.0]
      m   = lmModel (LA.fromList xsL) (LA.fromList ysL)
      df  = [ ("x", NumData (V.fromList xsL))
            , ("y", NumData (V.fromList ysL)) ] :: [(Text, ColData)]
      lmPlot = df |>> (layer (scatter "x" "y") <> toPlot m)
  saveSVGBound "design/plot-integration/lm-scatter-ci.svg" lmPlot
  putStrLn "wrote design/plot-integration/lm-scatter-ci.svg"

  -- (1b) GLM (Poisson, log link): 散布図 + μ 曲線 + 非対称 Wald CI 帯 (A8)。
  -- count データを log link で回帰。 μ スケールでは帯が非対称 (band layer で忠実)。
  let xsG = [1, 2, 3, 4, 5, 6, 7, 8] :: [Double]
      ysG = [1, 1, 3, 4, 7, 10, 15, 22]            -- 単調増加の count
      gmod' = glmModel Poisson Log (LA.fromList xsG) (LA.fromList ysG)
      gdfP  = [ ("x", NumData (V.fromList xsG))
              , ("y", NumData (V.fromList ysG)) ] :: [(Text, ColData)]
      glmPlot = gdfP |>> (layer (scatter "x" "y") <> toPlot (statModel gmod'))
  saveSVGBound "design/plot-integration/glm-poisson-ci.svg" glmPlot
  putStrLn "wrote design/plot-integration/glm-poisson-ci.svg"

  -- (1c) スプライン回帰: 散布図 + 平滑曲線 + 95% CI band (A9)。
  -- 非単調な波形を 3 次 B-spline で平滑化。 帯は基底空間の対称 Wald CI。
  let xsS = [ 0.4 * fromIntegral i | i <- [0 .. 20 :: Int] ] :: [Double]   -- 0..8
      ysS = [ sin x + 0.15 * x | x <- xsS ]                               -- 波形 + 緩い trend
      smod = splineModel (BSpline 3) [0, 2, 4, 6, 8] (LA.fromList xsS) (LA.fromList ysS)
      sdf  = [ ("x", NumData (V.fromList xsS))
             , ("y", NumData (V.fromList ysS)) ] :: [(Text, ColData)]
      splPlot = sdf |>> (layer (scatter "x" "y") <> toPlot smod)
  saveSVGBound "design/plot-integration/spline-smooth-ci.svg" splPlot
  putStrLn "wrote design/plot-integration/spline-smooth-ci.svg"

  -- (1d) Phase 70.5 項目 E: カーネル法統合 gp の 4 象限 (docs/regression/04-gp 用)。
  -- 同じ疎・ノイズ入り sin データに 3 象限を当て、 帯の有無を対比する。
  --   Gp (厳密 GP)        = 平滑曲線 + credible 帯
  --   Ridge (厳密 KRR)    = 平滑曲線のみ (点予測・帯なし・KRR ≡ GP 事後平均)
  --   GpRff (RFF 近似 GP) = 厳密 GP の低ランク近似 + 帯 (厳密 GP とほぼ一致)
  -- 疎 (13 点)・ノイズ入り (deterministic) にして credible 帯を可視化する。
  let xsGP = [ 0.5 * fromIntegral i | i <- [0 .. 12 :: Int] ] :: [Double]
      noiseGP = [ 0.18 * sin (4.7 * x + 1.3) | x <- xsGP ]   -- ±0.18 程度の決定的擾乱
      ysGP = zipWith (\x e -> sin x + e) xsGP noiseGP
      dfGPn = [ ("x", xsGP), ("y", ysGP) ] :: [(Text, [Double])]
      dfGP  = [ ("x", NumData (V.fromList xsGP))
              , ("y", NumData (V.fromList ysGP)) ] :: [(Text, ColData)]
      gpExact = dfGPn |-> gp defaultGP "x" "y"
      gpRidge = dfGPn |-> gp (GPConfig RBF Krr AutoMarginalLik) "x" "y"
      gpRff   = dfGPn |-> gp (GPConfig RBF (GpRff 400 7) AutoMarginalLik) "x" "y"
  saveSVGBound "design/plot-integration/gp-exact-ci.svg"
    (dfGP |>> (layer (scatter "x" "y") <> toPlot gpExact))
  saveSVGBound "design/plot-integration/gp-ridge-point.svg"
    (dfGP |>> (layer (scatter "x" "y") <> toPlot gpRidge))
  saveSVGBound "design/plot-integration/gp-rff-ci.svg"
    (dfGP |>> (layer (scatter "x" "y") <> toPlot gpRff))
  putStrLn "wrote design/plot-integration/gp-{exact,ridge,rff}*.svg"

  -- (1c2) Phase 16 C1: grid 評価のガタつき before/after (★本 Phase の主目的)。
  -- 疎・不均一な x (9 点) に 3 次 B-spline を当て、 訓練点評価 (toPlot smod) と
  -- grid 評価 (toPlot (statModel smod <> grid 200)) を別 SVG に。 前者は曲線/帯が
  -- 訓練点を直線で結ぶためカクつき、 後者は 200 点で滑らかになる (散布図は同一)。
  let xsW   = [0.0, 0.5, 1.2, 1.4, 3.0, 3.2, 5.5, 7.0, 8.0] :: [Double]  -- 疎・不均一
      ysW   = [ sin x + 0.15 * x | x <- xsW ]
      swmod = splineModel (BSpline 3) [0, 2, 4, 6, 8] (LA.fromList xsW) (LA.fromList ysW)
      wdf   = [ ("x", NumData (V.fromList xsW))
              , ("y", NumData (V.fromList ysW)) ] :: [(Text, ColData)]
      beforePlot = wdf |>> (layer (scatter "x" "y") <> toPlot swmod)
      -- statModel の帯は既定 ON (Phase 70.E)。 帯の滑らかさ比較が主旨。
      afterPlot  = wdf |>> (layer (scatter "x" "y") <> toPlot (statModel swmod <> grid 200))
  saveSVGBound "design/plot-integration/grid-before-training-points.svg" beforePlot
  saveSVGBound "design/plot-integration/grid-after-200.svg" afterPlot
  putStrLn "wrote design/plot-integration/grid-{before-training-points,after-200}.svg"

  -- (1c3) Phase 16 C2: predAt (予測点 + CI エラーバー)。 grid 曲線+帯に予測点 x=1,4,7 を
  -- 重ねる (μ̂ scatter + CI 区間 lineRange)。 predAt はリスト累積 (<> 連結)。
  let predPlot = wdf |>> ( layer (scatter "x" "y")
                           <> toPlot (statModel swmod <> grid 200
                                      <> predAt 1 <> predAt 4 <> predAt 7) )
  saveSVGBound "design/plot-integration/predat-points.svg" predPlot
  putStrLn "wrote design/plot-integration/predat-points.svg"

  -- (1c3') Phase 70.F: bandMode BandCIPI = CI + PI を入れ子で重ねたファンチャート。
  -- σ̂²>0 のノイズ入り LM で、 外=予測区間 (薄)・内=信頼区間 (濃)・中央=回帰線。
  let fanX = [1,2,3,4,5,6,7,8,9,10,11,12] :: [Double]
      fanN = take 12 (cycle [0.8,-1.1,0.5,-0.7,1.2,-0.4,0.9,-1.0,0.6,-0.5,1.1,-0.8])
      fanY = zipWith (\x e -> 1.5 + 1.2 * x + e) fanX fanN
      fanDF = [ ("x", NumData (V.fromList fanX))
              , ("y", NumData (V.fromList fanY)) ] :: [(Text, ColData)]
      fanFit = fanDF |-> lm "x" "y"
      fanPlot = fanDF |>> ( layer (scatter "x" "y")
                            <> toPlot (statModel fanFit <> grid 200 <> bandMode BandCIPI) )
  saveSVGBound "design/plot-integration/band-cipi-fan.svg" fanPlot
  putStrLn "wrote design/plot-integration/band-cipi-fan.svg"

  -- (1c3'') Phase 70.H: ブートストラップ PI。 非 Gaussian GLM (Poisson) は closed-form PI を
  -- 持たないが、 piMethod (PIBootstrap …) で case-resampling ブートストラップ (refit→予測を
  -- B 回) を選べば CI+PI を出せる。 PI は Family(μ) からの parametric ドロー (Poisson の離散・
  -- 非対称な裾を反映)。 bandMode で出す帯、 piMethod で算出法を選ぶ (直交)。
  let pbX = [1..12] :: [Double]
      pbY = [1,2,2,4,5,8,11,15,20,27,36,49] :: [Double]
      pbDF = [ ("x", NumData (V.fromList pbX)), ("y", NumData (V.fromList pbY)) ]
             :: [(Text, ColData)]
      pbFit = pbDF |-> glm Poisson Log "x" "y"
      pbPlot = pbDF |>> ( layer (scatter "x" "y")
                          <> toPlot (statModel pbFit <> grid 200
                                      <> bandMode BandCIPI <> piMethod (PIBootstrap 42 1000)) )
  saveSVGBound "design/plot-integration/bootstrap-poisson-pi.svg" pbPlot
  putStrLn "wrote design/plot-integration/bootstrap-poisson-pi.svg"

  -- (1c4) Phase 16 C3: 多変量 effect plot。 y ~ x1 + x2 を fit し x1 を along に動かす。
  -- x2 を byVar [1,5] で層別すると 2 曲線 (色分け)。 散布図は x1 vs y、 帯は x2 固定時の
  -- 95% CI。 主効果のみゆえ 2 曲線は平行 (x2=5 が +0.8·4 上にシフト)。
  let x1c   = concat (replicate 2 [0,1,2,3,4,5,6,7,8,9]) :: [Double]
      x2c   = replicate 10 1 ++ replicate 10 5 :: [Double]
      noise = take 20 (cycle [0.2,-0.3,0.1,-0.1,0.25,-0.2,0.15,-0.05,0.3,-0.15])
      yc    = zipWith3 (\a b e -> 2 + 1.5 * a + 0.8 * b + e) x1c x2c noise :: [Double]
      dfEff = DX.fromNamedColumns
                [ ("y",  DX.fromList yc)
                , ("x1", DX.fromList x1c)
                , ("x2", DX.fromList x2c) ]
      effMod = either error id (multiLMModel "y ~ x1 + x2" dfEff)
      effDF  = [ ("x1", NumData (V.fromList x1c))
               , ("y",  NumData (V.fromList yc)) ] :: [(Text, ColData)]
      effByVar = effDF |>> ( layer (scatter "x1" "y")
                             <> toPlot (statModelMulti effMod (along "x1")
                                        <> grid 100 <> byVar "x2" [1, 5]) )
      -- holdAt Median で x2 を中央値固定した単一 effect 曲線も別 SVG に (帯=95% CI)。
      effHold = effDF |>> ( layer (scatter "x1" "y")
                            <> toPlot (statModelMulti effMod (along "x1")
                                       <> grid 100 <> holdAt Median) )
  saveSVGBound "design/plot-integration/effect-byvar.svg" effByVar
  saveSVGBound "design/plot-integration/effect-holdat-median.svg" effHold
  putStrLn "wrote design/plot-integration/effect-{byvar,holdat-median}.svg"

  -- (1c5) Phase 72.4/72.5: 回帰診断の可視化。 effMod (y ~ x1 + x2) を再利用する。
  --   coefForest = 各係数の点推定 + 95% CI 水平バー (0 = 効果なしに参照線)。
  --   obsVsPred  = 実測 y vs 予測 ŷ の散布 + y=x 参照線 (点が線に近いほど良い当てはまり)。
  let noDfReg = [] :: [(Text, ColData)]
  saveSVGBound "design/plot-integration/coef-forest.svg"
    (noDfReg |>> (coefForest effMod <> title "係数 forest (95% CI)"))
  saveSVGBound "design/plot-integration/obs-vs-pred.svg"
    (noDfReg |>> (obsVsPred effMod <> title "実測 vs 予測"))
  putStrLn "wrote design/plot-integration/{coef-forest,obs-vs-pred}.svg"

  -- grouped: 群 g ごとに lm を fit → N 本の回帰線を色分け (geom_smooth(aes(color=g)) 相当)
  let grpX  = [1,2,3,4,5,6,7,8,9,10] ++ [1,2,3,4,5,6,7,8,9,10] :: [Double]
      grpG  = replicate 10 "A" ++ replicate 10 "B" :: [Text]
      grpN  = take 20 (cycle [0.3,-0.4,0.2,-0.2,0.1,-0.1,0.35,-0.25,0.15,-0.3])
      grpY  = zipWith3 (\x g e -> (if g == "A" then 1.0 + 1.2 * x else 4.0 + 0.5 * x) + e) grpX grpG grpN
      dfGrp = [ ("x", NumData (V.fromList grpX)), ("y", NumData (V.fromList grpY))
              , ("g", TxtData (V.fromList grpG)) ] :: [(Text, ColData)]
      gFit  = dfGrp |-> grouped "g" (lm "x" "y")
      gPlot = dfGrp |>> (layer (scatter "x" "y" <> colorBy "g") <> toPlot gFit)
  saveSVGBound "design/plot-integration/grouped-lm.svg" gPlot
  putStrLn "wrote design/plot-integration/grouped-lm.svg"

  -- weighted: 観測ごとの重み付き回帰 (WLS)。 後半に重みを偏らせる
  let wX  = [1,2,3,4,5,6,7,8,9,10,11,12] :: [Double]
      wN  = take 12 (cycle [0.5,-0.6,0.3,-0.4,0.2,-0.3])
      wY  = zipWith (\x e -> 2.0 + 1.3 * x + e) wX wN
      wW  = [0.3,0.3,0.5,0.5,0.8,0.8,1.0,1.0,1.5,1.5,2.0,2.0] :: [Double]
      dfW = [ ("x", NumData (V.fromList wX)), ("y", NumData (V.fromList wY))
            , ("w", NumData (V.fromList wW)) ] :: [(Text, ColData)]
      wFit  = dfW |-> weighted "w" (lm "x" "y")
      wPlot = dfW |>> (layer (scatter "x" "y") <> toPlot (statModel wFit))
  saveSVGBound "design/plot-integration/weighted-wls.svg" wPlot
  putStrLn "wrote design/plot-integration/weighted-wls.svg"

  -- (1d) GAM: 散布図 + 加法平滑曲線 (A9、 band 非提供 = CI helper 未実装ゆえ曲線のみ)。
  let xsM = [ 0.4 * fromIntegral i | i <- [0 .. 20 :: Int] ] :: [Double]   -- 0..8
      ysM = [ sin x + 0.15 * x + 0.1 * cos (3 * x) | x <- xsM ]            -- やや複雑な波形
      gmm = gamModel 3 6 0.5 (LA.fromList xsM) (LA.fromList ysM)
      mdf = [ ("x", NumData (V.fromList xsM))
            , ("y", NumData (V.fromList ysM)) ] :: [(Text, ColData)]
      gamPlot = mdf |>> (layer (scatter "x" "y") <> toPlot gmm)
  saveSVGBound "design/plot-integration/gam-smooth.svg" gamPlot
  putStrLn "wrote design/plot-integration/gam-smooth.svg"

  -- (1d') GAM 基底比較 (Phase 70.6): 同じデータを B-spline / 自然3次 / RBF で平滑化し
  --       色分け重畳。 λ は GCV 自動選択 (gam の高レベル df|-> API)。
  let gbBS  = mdf |-> gam (GAMConfig (BSplineB 3 8)   GCV) "x" "y"
      gbNC  = mdf |-> gam (GAMConfig (NaturalCubicB 8) GCV) "x" "y"
      gbRBF = mdf |-> gam (GAMConfig (RBFB 10 1.0)    GCV) "x" "y"
      -- 3 基底の曲線の対比が主旨ゆえ帯は出さない (CI 実装後の既定 CI を BandOff で抑制)。
      gamBasisPlot = mdf |>> (layer (scatter "x" "y")
                       <> toPlot (statModel gbBS  <> statColor blue  <> statLabel "B-spline"     <> bandMode BandOff)
                       <> toPlot (statModel gbNC  <> statColor green <> statLabel "Natural cubic" <> bandMode BandOff)
                       <> toPlot (statModel gbRBF <> statColor red   <> statLabel "RBF"           <> bandMode BandOff))
  saveSVGBound "design/plot-integration/gam-basis-compare.svg" gamBasisPlot
  putStrLn "wrote design/plot-integration/gam-basis-compare.svg"

  -- (1e) ロバスト回帰: 散布図 + ロバスト直線 (Huber) vs OLS 直線 (A9)。
  -- 末尾に外れ値を 1 点入れ、 OLS が引っ張られる一方ロバスト線が傾き ≈ 2 を保つ対比。
  let xsR = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] :: [Double]
      -- inlier は y≈2x+1 に軽いノイズ (ロバスト CI 帯が見える幅を持つように)、 末尾は外れ値。
      ysR = [3.4, 4.6, 7.5, 8.7, 11.3, 12.6, 15.4, 16.8, 19.2, 45]   -- 末尾が外れ値 (本来 ≈21)
      rmod = robustModel (Huber defaultHuberK) (LA.fromList xsR) (LA.fromList ysR)
      lmod = lmModel (LA.fromList xsR) (LA.fromList ysR)
      rdf  = [ ("x", NumData (V.fromList xsR))
             , ("y", NumData (V.fromList ysR)) ] :: [(Text, ColData)]
      -- 散布図 + OLS (青) + ロバスト (赤)、 ともに CI 帯付き (Phase 70.C でロバストにも
      -- サンドイッチ CI を実装)。 OLS 帯は外れ値で広がり、 ロバスト帯は締まる対比。
      robPlot = rdf |>> (layer (scatter "x" "y")
                         <> toPlot (statModel lmod <> statColor orange <> statFill orange <> statLabel "OLS")
                         <> toPlot (statModel rmod <> statColor red    <> statFill red    <> statLabel "Robust (Huber)"))
  saveSVGBound "design/plot-integration/robust-vs-ols.svg" robPlot
  putStrLn "wrote design/plot-integration/robust-vs-ols.svg"

  -- (1f) 多出力線形回帰: 出力間の残差相関 heatmap (A9)。
  -- 3 出力。 共有 wiggle で out1/out2 正相関、 out3 逆相関にして相関構造を可視化。
  let nM   = 30
      xcol = [ fromIntegral i | i <- [1 .. nM] ] :: [Double]
      wig  = [ sin (0.7 * fromIntegral i) | i <- [1 .. nM] ] :: [Double]
      xmat = LA.fromColumns [LA.konst 1 nM, LA.fromList xcol]       -- n×2 (intercept+x)
      yo1  = zipWith (\x w -> 2*x + w)     xcol wig
      yo2  = zipWith (\x w -> x   + 0.8*w) xcol wig                 -- +相関
      yo3  = zipWith (\x w -> -x  - w)     xcol wig                 -- 逆相関
      ymat = LA.fromColumns (map LA.fromList [yo1, yo2, yo3])        -- n×3
      mfit = fitMultiLM xmat ymat
      -- heatmap は完全 inline ゆえ df 不要 (空 df を bind)。
      mlPlot = ([] :: [(Text, ColData)]) |>> toPlot mfit
  saveSVGBound "design/plot-integration/multilm-resid-corr.svg" mlPlot
  putStrLn "wrote design/plot-integration/multilm-resid-corr.svg"

  -- (1g) 分位点回帰: 散布図 + 複数分位線 (τ=0.1/0.5/0.9) 色分け重畳 (A10)。
  -- 分散が x で増える heteroscedastic データ。 分位線が末広がりになり区間を直接表現。
  let xsQ = [ fromIntegral i | i <- [1 .. 40 :: Int] ] :: [Double]
      ysQ = [ 2 * x + (x / 6) * sin (fromIntegral i * 1.3)
            | (i, x) <- zip [1 :: Int ..] xsQ ]
      qmod = quantileModel [0.1, 0.5, 0.9] (LA.fromList xsQ) (LA.fromList ysQ)
      qdf  = [ ("x", NumData (V.fromList xsQ))
             , ("y", NumData (V.fromList ysQ)) ] :: [(Text, ColData)]
      qPlot = qdf |>> (layer (scatter "x" "y") <> toPlot qmod)
  saveSVGBound "design/plot-integration/quantile-lines.svg" qPlot
  putStrLn "wrote design/plot-integration/quantile-lines.svg"

  -- (1h) MCMC チェーン: trace plot + 周辺事後密度 (A11)。
  -- AR(1) 風の決定的な draw 列 (平均 5 まわりに揺れる) を 1 パラメータ "mu" として可視化。
  let draws = [ 5 + sin (fromIntegral i * 0.31) + 0.6 * sin (fromIntegral i * 1.7)
                  + 0.4 * cos (fromIntegral i * 2.9)
              | i <- [1 .. 200 :: Int] ]   -- 平均 5 まわりを揺れ続ける擬似 draw
      chain = Chain { chainSamples     = [ Map.singleton "mu" v | v <- draws ]
                    , chainAccepted    = 200, chainTotal = 240
                    , chainEnergy      = [], chainDivergences = [] }
      cmod  = chainModel "mu" chain
      tracePlt = ([] :: [(Text, ColData)]) |>> toPlot cmod
      densPlt  = ([] :: [(Text, ColData)]) |>> (diagnosticPlots cmod !! 1)
  saveSVGBound "design/plot-integration/mcmc-trace.svg" tracePlt
  saveSVGBound "design/plot-integration/mcmc-density.svg" densPlt
  putStrLn "wrote design/plot-integration/mcmc-{trace,density}.svg"

  -- (1i) 生存解析: Kaplan-Meier 生存曲線 + 競合リスク CIF (A12)。
  let kmSamples = [ SurvSample t e
                  | (t, e) <- [ (2, Observed), (3, Observed), (5, Censored)
                              , (6, Observed), (8, Observed), (9, Censored)
                              , (11, Observed), (12, Observed), (14, Observed)
                              , (15, Censored), (17, Observed), (19, Observed) ] ]
      kmRes = kaplanMeier kmSamples
      kmPlot = ([] :: [(Text, ColData)]) |>> toPlot kmRes
  saveSVGBound "design/plot-integration/km-survival.svg" kmPlot
  putStrLn "wrote design/plot-integration/km-survival.svg"

  let crSamples = [ CRSample t c
                  | (t, c) <- [ (1,1),(2,2),(3,1),(4,0),(5,2),(6,1),(7,2)
                              , (8,0),(9,1),(10,2),(11,1),(12,0),(13,2),(14,1) ] ]
      crRes = fitCompetingRisks crSamples
      cifPlot = ([] :: [(Text, ColData)]) |>> toPlot crRes
  saveSVGBound "design/plot-integration/cif-competing.svg" cifPlot
  putStrLn "wrote design/plot-integration/cif-competing.svg"

  -- (1j) 時系列予測: 履歴 + AR(2) 予測 + 予測区間 band (A13)。
  -- ★定常 AR(1) 過程 (φ=0.6 で平均回帰) を有界 innovation で生成。 純正弦波だと AR の根が
  --   単位円上に来て予測が発散するため、 必ず |φ|<1 の真の AR 系列にする。 12 step 予測で
  --   予測は平均 10 へ回帰し、 帯が地平とともに広がる様子を見せる。
  let tsMu = 10.0; tsPhi = 0.6
      tsNoise = [ 1.5 * sin (fromIntegral i * 1.3) + cos (fromIntegral i * 2.7)
                | i <- [1 .. 60 :: Int] ] :: [Double]
      tsSeries = drop 1 (scanl (\y e -> tsMu + tsPhi * (y - tsMu) + e) tsMu tsNoise)
      fcMod = forecastModel 2 12 (LA.fromList tsSeries)
      fcPlot = ([] :: [(Text, ColData)]) |>> toPlot fcMod
  saveSVGBound "design/plot-integration/ts-forecast.svg" fcPlot
  putStrLn "wrote design/plot-integration/ts-forecast.svg"

  -- (1k) 多変量・木: PCA scree plot + RandomForest 特徴重要度 (A14)。
  -- Center (共分散 PCA) で第 1 軸の高分散をそのまま見せる (CenterScale だと各軸を
  -- 単位分散に正規化して scree が平坦になる)。
  let pcaRows = [ [ 5 * sin (fromIntegral i * 0.3), 1.2 * cos (fromIntegral i * 0.5)
                  , 0.4 * sin (fromIntegral i), 0.2 * cos (fromIntegral i * 2.1) ]
                | i <- [1 .. 50 :: Int] ]
      pcaRes = pca Center Nothing (LA.fromLists pcaRows)
      screePlot = ([] :: [(Text, ColData)]) |>> toPlot pcaRes
  saveSVGBound "design/plot-integration/pca-scree.svg" screePlot
  putStrLn "wrote design/plot-integration/pca-scree.svg"

  -- RF 特徴重要度 (75.24: R varImpPlot 流の 2 パネル = 左 impurity / 右 permutation)。
  -- df|-> で実列名つき。 area/age は近直交な信号 (異周波の三角関数) で price を等しく
  -- 決め、 noise は price に無関係。 → 両パネルとも area/age が高く noise が低い正直な例。
  -- seed 固定で決定的 (drift ゲート用)。
  let n0     = 120 :: Int
      areaC  = [ sin (fromIntegral i * 0.7) * 10 | i <- [1 .. n0] ] :: [Double]
      ageC   = [ cos (fromIntegral i * 0.4) * 10 | i <- [1 .. n0] ] :: [Double]
      noiseC = [ sin (fromIntegral i * 2.3) * 10 | i <- [1 .. n0] ] :: [Double]
      priceC = [ 2 * areaC !! (i - 1) + 2 * ageC !! (i - 1) | i <- [1 .. n0] ] :: [Double]
      rfDF   = [ ("area",  NumData (V.fromList areaC))
               , ("age",   NumData (V.fromList ageC))
               , ("noise", NumData (V.fromList noiseC))
               , ("price", NumData (V.fromList priceC)) ] :: [(Text, ColData)]
      rf     = rfDF |-> randomForestReg (defaultRandomForest { rfTrees = 100 }) 314
                       ["area", "age", "noise"] "price" :: RandomForest
      rfPlot = ([] :: [(Text, ColData)]) |>> toPlot rf
  saveSVGBound "design/plot-integration/rf-importance.svg" rfPlot
  putStrLn "wrote design/plot-integration/rf-importance.svg"

  -- RFClassifier 特徴重要度 (doc 05-ml RF 節の分類併記図)。 回帰版と同じ df|-> 高レベルで、
  -- x1/x2 が species (3 クラス) を決め x3 は無関係 → 重要度は x1/x2 高・x3 低の正直な例。
  -- clsCol は数値ラベル (reqLabelI が round してクラス整数化)。 seed 固定で決定的 (drift ゲート用)。
  let nC      = 150 :: Int
      x1C     = [ sin (fromIntegral i * 0.5) * 3 | i <- [1 .. nC] ] :: [Double]
      x2C     = [ cos (fromIntegral i * 0.9) * 3 | i <- [1 .. nC] ] :: [Double]
      x3C     = [ sin (fromIntegral i * 2.7) * 3 | i <- [1 .. nC] ] :: [Double]
      speciesC = [ let s = x1C !! (i - 1) + x2C !! (i - 1)
                   in fromIntegral (max 0 (min 2 (round ((s + 6) / 4) :: Int)))
                 | i <- [1 .. nC] ] :: [Double]
      rfcDF   = [ ("x1",      NumData (V.fromList x1C))
                , ("x2",      NumData (V.fromList x2C))
                , ("x3",      NumData (V.fromList x3C))
                , ("species", NumData (V.fromList speciesC)) ] :: [(Text, ColData)]
      rfcM    = rfcDF |-> randomForestCls defaultRFCConfig 314 ["x1", "x2", "x3"] "species"
      rfcPlot2 = ([] :: [(Text, ColData)]) |>> toPlot rfcM
  saveSVGBound "design/plot-integration/rfc-importance.svg" rfcPlot2
  putStrLn "wrote design/plot-integration/rfc-importance.svg"

  -- 部分従属図 PDP / ICE (Phase 75.27): 上で学習した rf の price ~ area 部分従属。
  -- area を観測範囲の grid で振り、 他特徴 (age/noise) は観測分布のまま各行 predict → 平均
  -- (PDP・青太線)。 個体条件付き期待 (ICE) は各観測行の曲線 (薄灰)。 rf は price=2*area+2*age
  -- 由来ゆえ area 増で PDP は右肩上がり・ICE はほぼ平行 (area と他特徴の交互作用が弱い)。
  -- Phase 76.D: HBM 抽出子 (forestOf 等) と同型の Plottable 中間型 + toPlot・<> で装飾合成。
  let pdpPlt = noDfReg |>> toPlot (pdp rf rfDF ["area","age","noise"] "area")
                          <> title "Partial dependence: price ~ area" <> width 640 <> height 420
      icePlt = noDfReg |>> toPlot (pdpIce rf rfDF ["area","age","noise"] "area")
                          <> title "ICE + PDP: price ~ area" <> width 640 <> height 420
  saveSVGBound "design/plot-integration/pdp-area.svg" pdpPlt
  saveSVGBound "design/plot-integration/pdp-ice-area.svg" icePlt
  putStrLn "wrote design/plot-integration/{pdp-area,pdp-ice-area}.svg"

  -- (1m) 混合効果 (Phase 52 D3): GLMMResultRE の random effect caterpillar plot。
  -- 8 群 (G1..G8) の random intercept が異なる i.i.d. データを y ~ x + (1|group) で
  -- 学習し、 各群の BLUP を値で昇順ソートした caterpillar (= forest mark の点 + 0 参照線)。
  -- CI 帯は現状なし (点のみ): per-group conditional variance / n_j を非格納のため。
  let reLevels  = [3, 5, 4, 7, 2, 6, 4.5, 5.5] :: [Double]   -- 群ごとの切片 (真値)
      reGroups8 = concat [ replicate 4 (T.pack ('G' : show g)) | g <- [1 .. 8 :: Int] ]
      reX8      = concat (replicate 8 [1, 2, 3, 4]) :: [Double]
      reY8      = concat [ [ lvl + 0.5 * x + e
                           | (x, e) <- zip [1, 2, 3, 4] [0.1, -0.05, 0.08, -0.1] ]
                         | lvl <- reLevels ] :: [Double]
      dfRE8 = DX.fromNamedColumns
        [ ("x",     DX.fromList reX8)
        , ("y",     DX.fromList reY8)
        , ("group", DX.fromList reGroups8) ]
      (reFit8, _) = dfRE8 |-> glmmF "y ~ x + (1|group)"
      catPlot = ([] :: [(Text, ColData)]) |>> toPlot reFit8
  saveSVGBound "design/plot-integration/glmm-caterpillar.svg" catPlot
  putStrLn "wrote design/plot-integration/glmm-caterpillar.svg"

  -- (2) ガウス過程: 散布図 + 事後平均 (曲線) + credible band (A6)。
  -- 訓練点を疎 (7 点) + 非最適化 (default params) にして、 点間・端で band が広がる
  -- 様子を見せる (密+最適化だと GP がほぼ補間し band が極細で見えない)。
  let gx   = [ 1.0 * fromIntegral i | i <- [0 .. 6 :: Int] ]       -- 訓練 x: 0,1,..,6
      gy   = map sin gx                                            -- 訓練 y = sin x
      gmod = GPModel RBF defaultGPParams
      grid = [ 0.1 * fromIntegral i | i <- [0 .. 60 :: Int] ]      -- 予測 grid: 0..6
      gres = fitGP gmod gx gy grid
      gdf  = [ ("x", NumData (V.fromList gx))
             , ("y", NumData (V.fromList gy)) ] :: [(Text, ColData)]
      gpPlot = gdf |>> (layer (scatter "x" "y") <> toPlot gres)
  saveSVGBound "design/plot-integration/gp-mean-ci.svg" gpPlot
  putStrLn "wrote design/plot-integration/gp-mean-ci.svg"

  -- (2a') モデル比較 (Phase 52 A10: 重畳凡例合成に対応)。
  --   学習は df |-> spec、 描画は toPlot。 各当てはめを statModel でくるみ statColor で線色・
  --   statLabel で凡例ラベルを指定すると、 LM/GLM/spline が色分けされ凡例も 3 件並ぶ。
  --   A10 前は色辞書が最後勝ち (vsColorManual=Last) + 凡例が先頭 enc のみで全線同色・凡例1件に
  --   潰れていたが、 plot-core の色辞書マージ + 凡例の全カテゴリ union で解消した。
  let xsC  = [1 .. 12] :: [Double]
      ysC  = [2.1, 2.0, 3.4, 4.8, 5.0, 7.2, 7.0, 9.5, 11.0, 10.8, 13.9, 15.2]  -- 正値・緩く非線形
      cdf  = [ ("x", NumData (V.fromList xsC))
             , ("y", NumData (V.fromList ysC)) ] :: [(Text, ColData)]
      -- 3 線の中心傾向の対比が主旨ゆえ帯は出さない (帯既定 CI を bandMode BandOff で抑制)。
      cmpPlot = cdf |>>
        ( layer (scatter "x" "y")
        <> toPlot (statModel (cdf |-> lm "x" "y")              <> statColor (fromHex "#1f77b4") <> statLabel "LM" <> bandMode BandOff)      -- 青
        <> toPlot (statModel (cdf |-> glm Poisson Log "x" "y") <> statColor (fromHex "#ff7f0e") <> statLabel "GLM" <> bandMode BandOff)     -- 橙
        <> toPlot (statModel (cdf |-> spline (BSpline 3) [4, 8] "x" "y") <> statColor (fromHex "#2ca02c") <> statLabel "spline" <> bandMode BandOff)  -- 緑
        <> title "モデル比較 (LM / GLM / spline)" )
  saveSVGBound "design/plot-integration/model-comparison.svg" cmpPlot
  putStrLn "wrote design/plot-integration/model-comparison.svg"

  -- (2b) HBM (ベイズ確率プログラム): epred / trace / forest / ppc / dag (Phase 49)。
  -- y = 1 + 2x + 微小ゆらぎ を線形 HBM で学習し、 5 つの抽出子をそれぞれ SVG に。
  let hbmX  = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] :: [Double]
      hbmNz = [0.12,-0.08,0.05,-0.15,0.10,0.03,-0.07,0.14,-0.04,0.06]
      hbmY  = zipWith (\xx e -> 1 + 2 * xx + e) hbmX hbmNz
      -- A12: hbmAdaptMass は defaultHBM で既定 True。 mass 適応は warmup window が要るため
      -- 1000/1000 にして s (scale param) まで収束させる (4 chain で R-hat 確認可)。
      hbmCfg = defaultHBM { hbmChains = 4, hbmSamples = 1000, hbmWarmup = 1000
                          , hbmSeed = Just 49490605 }
      hbmDf = [ ("x", NumData (V.fromList hbmX))
              , ("y", NumData (V.fromList hbmY)) ] :: [(Text, ColData)]
      noDf  = [] :: [(Text, ColData)]
  hbmFit <- hbmModel hbmCfg demoHbmModel [("x", hbmX), ("y", hbmY)]
  -- epred の既定 grid は 100 点 (ローカル GP `grid` と名前衝突するため明示しない)。
  let epredPlot = hbmDf |>> (layer (scatter "x" "y")
                              <> toPlot (epred hbmFit "x" "mu"))
      -- Phase 74: param ごと独立パネルで縦並び (旧 foldMap toPlot = 全 param 重畳の誤りを排除)。
      --   ★autocorr/rank と同様、 縦1列は総高を param 数比例にしないと各パネルが潰れる
      --     (固定キャンバス 288pt を param 数で割るため)。 param 1 つあたり 150pt + 上下マージン。
      tracePanels = tracesOf hbmFit
      tracePltH = noDf |>> ( subplots tracePanels <> subplotCols 1
                             <> height (60 + 150 * fromIntegral (length tracePanels)) )
      forestPlt = noDf |>> toPlot (forestOf hbmFit)
      dagPlt    = noDf |>> toPlot (dagOf hbmFit)
      ppcSpec   = ppcOf hbmFit "obs"          -- 純粋・正本 (IO 版は ppcOfIO)
      ppcPlt    = noDf |>> toPlot ppcSpec
  saveSVGBound "design/plot-integration/hbm-epred.svg"  epredPlot
  saveSVGBound "design/plot-integration/hbm-trace.svg"  tracePltH
  saveSVGBound "design/plot-integration/hbm-forest.svg" forestPlt
  saveSVGBound "design/plot-integration/hbm-ppc.svg"    ppcPlt
  saveSVGBound "design/plot-integration/hbm-dag.svg"    dagPlt
  putStrLn "wrote design/plot-integration/hbm-{epred,trace,forest,ppc,dag}.svg"

  -- Phase 74.5: epred の予測区間 (PI) 帯。 同じ fit を bandMode で CI (μ HDI) / PI
  --   (観測ノイズ込み) / CIPI (入れ子ファンチャート) に切替え 3 パネル比較する
  --   (頻度論 statModel と同綴り)。 散布は points (inline) でパネルを純 VisualSpec 化。
  --   ★CI/PI の差を見せるため観測ノイズを大きめにした専用 fit (上の hbmFit は微小ノイズ
  --     ゆえ帯がほぼ重なる)。 y = 1 + 2x + N(0, ~2)。
  let piNz = [ 2.1,-1.6, 2.4,-2.8, 1.3, 0.7,-2.2, 2.9,-1.1, 1.8 ]
      piY  = zipWith (\xx e -> 1 + 2 * xx + e) hbmX piNz
  piFit <- hbmModel hbmCfg demoHbmModel [("x", hbmX), ("y", piY)]
  let epredBand bm ttl = toPlot (epred piFit "x" "mu" <> bandMode bm)
                           <> layer (points hbmX piY) <> title ttl
      epredPiPlt = noDf |>> ( subplots [ epredBand BandCI   "CI (μ HDI)"
                                       , epredBand BandPI   "PI (観測ノイズ込み)"
                                       , epredBand BandCIPI "CIPI (入れ子)" ]
                              <> subplotCols 3 <> width 960 )
  saveSVGBound "design/plot-integration/hbm-epred-pi.svg" epredPiPlt
  putStrLn "wrote design/plot-integration/hbm-epred-pi.svg"

  -- (2b1) Phase 74: 発散 (divergence) デモ。 中心化 8-schools は funnel ゆえ NUTS が
  --   τ 小領域で発散する。 tracesOf (発散 rug 既定 ON) を chain 別重畳で出し、 τ を含む
  --   代表 param だけ selectPanels で抜く。 ★推測せず実測: 発散数を stderr に出す。
  divFit <- hbmModel (defaultHBM { hbmChains = 4, hbmSamples = 1000, hbmWarmup = 1000
                                 , hbmSeed = Just 8112 })
                     eightSchoolsCentered []
  hPutStrLn stderr $ "[divergence demo] params = " <> show (hbmParamNames divFit)
  hPutStrLn stderr $ "[divergence demo] #divergences = "
                       <> show (length (divergencesOf divFit))
  let divPanels = tracesOfWith defaultTraceOpts { toByChain = True } divFit
      -- τ (funnel の首) + 2 schools を抜き、 発散 rug を見せる。
      divPick   = ["mu", "tau", "theta_1", "theta_2"]
      divTrace  = noDf |>> ( subplots divPanels <> selectPanels divPick <> subplotCols 1
                             <> height (60 + 150 * fromIntegral (length divPick))
                             <> title "中心化 8-schools の発散 (tracesOf・発散 rug 既定 ON)" )
  saveSVGBound "design/plot-integration/hbm-trace-divergent.svg" divTrace
  putStrLn "wrote design/plot-integration/hbm-trace-divergent.svg"

  -- (2b1b) Phase 74: pairOf / energyOf の図 (API↔図 1:1 を満たす)。 同じ funnel fit を
  --   使うと診断が一貫する: pairOf は τ–θ の漏斗に発散が首へ集中する古典図
  --   (ArviZ plot_pair(divergences=True) 同型)、 energyOf は marginal vs ΔE (funnel ゆえ
  --   BFMI が低く ΔE 分布が狭い)。
  let pairPlt   = noDf |>> head (pairOf divFit [("tau", "theta_1")])
      energyPlt = noDf |>> energyOf divFit
  saveSVGBound "design/plot-integration/hbm-pair.svg"   pairPlt
  saveSVGBound "design/plot-integration/hbm-energy.svg" energyPlt
  putStrLn "wrote design/plot-integration/hbm-{pair,energy}.svg"

  -- (2b1c) Phase 74: marginalsOf の図 (周辺事後密度・param ごと)。 クリーンな線形 fit で。
  let marPanels = marginalsOf hbmFit
      marPlt    = noDf |>> ( subplots marPanels <> subplotCols 1
                             <> height (60 + 150 * fromIntegral (length marPanels))
                             <> title "周辺事後密度 (marginalsOf)" )
  saveSVGBound "design/plot-integration/hbm-marginals.svg" marPlt
  putStrLn "wrote design/plot-integration/hbm-marginals.svg"

  -- (2b2) Phase 73.1/73.2: 収束診断 (自己相関 / rank plot)。 param ごと 1 図ゆえ
  --   subplots で縦に束ねる。 autocorrOf = ACF 棒 (速く 0 に減衰=良 mixing)、
  --   rankOf = chain 別 rank ヒスト重畳 (各 chain 一様=収束)。
  --   ★縦1列だと固定キャンバス (既定 288pt) を param 数で割って各パネルが潰れるため、
  --     総高を param 数に比例させ各パネル高を一定に保つ (= ArviZ が figsize を rows から
  --     算出するのと同方針・backend/matplotlib/core.py の height ∝ (rows+1)^1.1)。
  let acPanels = autocorrOf hbmFit
      rkPanels = rankOf hbmFit
      stackH n = height (60 + 150 * fromIntegral n)   -- param 1 つあたり 150pt + 上下マージン
  saveSVGBound "design/plot-integration/hbm-autocorr.svg"
    (noDf |>> vconcat acPanels <> stackH (length acPanels) <> title "autocorrOf (自己相関)")
  saveSVGBound "design/plot-integration/hbm-rank.svg"
    (noDf |>> vconcat rkPanels <> stackH (length rkPanels) <> title "rankOf (rank plot・chain 一様性)")
  putStrLn "wrote design/plot-integration/hbm-{autocorr,rank}.svg"

  -- (2c) HBM 診断ダッシュボード (Phase 74.8 = 専用関数 dashboardOf / dashboardFullOf)。
  --   compact = 推定値 (forest) / 当てはまり (PPC) / サンプラ健全性 (energy) の 3 枚。
  --   full    = 構造 (DAG) / trace / 事後分布 / HDI・PPC / energy の徹底点検版。
  --   どちらも observe 名 ("obs") を取る。 ★旧 demo の手組み 5 列を関数化した。
  saveSVGBound "design/plot-integration/hbm-dashboard.svg"
    (noDf |>> dashboardOf hbmFit "obs")
  saveSVGBound "design/plot-integration/hbm-dashboard-full.svg"
    (noDf |>> dashboardFullOf hbmFit "obs")
  putStrLn "wrote design/plot-integration/hbm-dashboard{,-full}.svg"

  -- Phase 74.8: trace + 事後分布だけの dashboard (= ArviZ plot_trace 相当)。
  saveSVGBound "design/plot-integration/hbm-trace-density.svg"
    (noDf |>> traceDensityOf hbmFit)
  -- Phase 74.9: 学習前 DAG (ModelP 直接・サンプリングなし)。 demoHbmModel は data 駆動
  --   plate ゆえ dagOfModelWith でデータを束ねてから描く (NUTS は走らない)。
  saveSVGBound "design/plot-integration/hbm-dag-model.svg"
    (noDf |>> toPlot (dagOfModelWith [("x", hbmX), ("y", hbmY)] demoHbmModel))
  putStrLn "wrote design/plot-integration/hbm-{trace-density,dag-model}.svg"

  -- (2d) docs/bayesian/02-probabilistic-model.md の各パターン代表図 (Pattern 1/2/3/5/7/8)。
  --   marginalsOf=事後分布 / forestOf=HDI / dagOf=構造。 epred/ppc 不使用ゆえ dat=[]。
  --   DAG だけの P7/P8 は構造のみ要るので軽い config (dagOf は spec のみ参照)。
  createDirectoryIfMissing True "docs/images"
  let docCfg    = defaultHBM { hbmChains = 4, hbmSamples = 500, hbmWarmup = 500
                             , hbmSeed = Just 20260607 }
      docDagCfg = defaultHBM { hbmChains = 1, hbmSamples = 120, hbmWarmup = 120
                             , hbmSeed = Just 7 }
  fitP1 <- hbmModel docCfg    docP1 []
  fitP2 <- hbmModel docCfg    docP2 []
  fitP3 <- hbmModel docCfg    docP3 []
  fitP5 <- hbmModel docCfg    docP5 []
  fitP7 <- hbmModel docDagCfg docP7 []
  fitP8 <- hbmModel docDagCfg docP8 []
  let p1Plot = noDf |>> (subplots (marginalsOf fitP1) <> subplotCols 1
                         <> title "Pattern 1: μ の事後分布")
      p2Plot = noDf |>> (subplots (marginalsOf fitP2) <> subplotCols 2
                         <> title "Pattern 2: μ, σ の事後分布")
      p3Plot = noDf |>> (toPlot (forestOf fitP3) <> title "Pattern 3: A/B 事後 (94% HDI)")
      p5Dag  = noDf |>> (toPlot (dagOf    fitP5) <> title "Pattern 5: 階層モデルの DAG")
      p5For  = noDf |>> (toPlot (forestOf fitP5) <> title "Pattern 5: 群平均の事後 (94% HDI)")
      p7Dag  = noDf |>> (toPlot (dagOf    fitP7) <> title "Pattern 7: 3 階層 nested の DAG")
      p8Dag  = noDf |>> (toPlot (dagOf    fitP8) <> title "Pattern 8: 交差ランダム効果の DAG")
  saveSVGBound "docs/images/hbm-p1-posterior.svg" p1Plot
  saveSVGBound "docs/images/hbm-p2-posterior.svg" p2Plot
  saveSVGBound "docs/images/hbm-p3-forest.svg"    p3Plot
  saveSVGBound "docs/images/hbm-p5-dag.svg"       p5Dag
  saveSVGBound "docs/images/hbm-p5-forest.svg"    p5For
  saveSVGBound "docs/images/hbm-p7-dag.svg"       p7Dag
  saveSVGBound "docs/images/hbm-p8-dag.svg"       p8Dag
  putStrLn "wrote docs/images/hbm-p{1,2,3,5,7,8}-*.svg"

  -- (2e) クラスタリング (KMeans) — Phase 68 A1。 合成 3 クラスタ (2D blob) を
  -- k-means で学習し、 「クラスタ別散布 (色=ラベル) + centroid ✚ 重畳」 の定番図を
  -- 出す。 二層イディオム = clusterScatterOf (data 層) <> centroidsOf (model 層)。
  let blob (cx, cy) =
        [ (cx + dx, cy + dy)
        | (dx, dy) <- [ (-0.4, 0.3), (0.2, -0.5), (0.5, 0.4), (-0.3, -0.2), (0.1, 0.6) ] ]
      pts2d  = concatMap blob [ (1.0, 1.0), (5.0, 5.0), (1.0, 5.0) ]
      xsK    = map fst pts2d
      ysK    = map snd pts2d
      xMat   = LA.fromLists [ [x, y] | (x, y) <- pts2d ]   -- n×2 特徴量行列
  kGen <- MWC.initialize (V.fromList [42])
  kres <- kMeans (defaultKMeans 3) xMat kGen
  let kdf   = [ ("x", NumData (V.fromList xsK))
              , ("y", NumData (V.fromList ysK)) ] :: [(Text, ColData)]
      kPlot = kdf |>> ( clusterScatterOf kdf kres "x" "y"
                        <> centroidsOf kres 0 1
                        <> title "KMeans clusters (k=3)" )
  saveSVGBound "design/plot-integration/kmeans-clusters.svg" kPlot
  putStrLn "wrote design/plot-integration/kmeans-clusters.svg"
  -- Phase 76.B: クラスタを囲む。 ① 凸包の輪郭 (geom_encircle 相当)、 ② 95% 共分散楕円
  -- (stat_ellipse 相当)。 群色は clusterScatterOf と一致・輪郭線のみ (annotation 制約)。
  let kHull = kdf |>> ( clusterScatterOf kdf kres "x" "y"
                        <> clusterHullOf kdf kres "x" "y"
                        <> centroidsOf kres 0 1
                        <> title "KMeans clusters + convex hull" )
      kEll  = kdf |>> ( clusterScatterOf kdf kres "x" "y"
                        <> clusterEllipseOf kdf kres "x" "y"
                        <> centroidsOf kres 0 1
                        <> title "KMeans clusters + 95% ellipse" )
  saveSVGBound "design/plot-integration/kmeans-hull.svg" kHull
  saveSVGBound "design/plot-integration/kmeans-ellipse.svg" kEll
  putStrLn "wrote design/plot-integration/{kmeans-hull,kmeans-ellipse}.svg"
  -- Phase 76.C: 階層クラスタリング dendrogram (Ward)。 同じ 3 blob を Ward linkage で結合し、
  -- U 字リンクで樹形図を描く。 色閾値 = 上位 2 マージの中間 (= 3 クラスタに色分け)。
  let hfit  = fitHierarchical Ward xMat
      hs    = hcHeights hfit                     -- 昇順 (agglomerative)
      nh    = length hs
      thr   = (hs !! (nh - 3) + hs !! (nh - 2)) / 2
      dPlot = noDf |>> ( dendrogramOf' defaultDendroOpts { doColorThreshold = Just thr } hfit
                         <> title "Hierarchical clustering (Ward) dendrogram" )
  saveSVGBound "design/plot-integration/dendrogram.svg" dPlot
  putStrLn "wrote design/plot-integration/dendrogram.svg"

  -- (2f) 木/アンサンブル — Phase 68 A2。 合成データ (3 特徴・y は f0,f1 に依存・f2 ノイズ) で
  -- ① GradientBoosting 回帰の特徴重要度 bar、 ② RandomForestClassifier の重要度 bar、
  -- ③ DecisionTree の樹形図 (MDAG 再利用) を出す。 ①② は既存 bar、 ③ は既存 MDAG。
  let n2     = 40 :: Int
      f0s    = [ fromIntegral (i `mod` 8)             | i <- [0 .. n2 - 1] ] :: [Double]
      f1s    = [ fromIntegral ((i * 3) `mod` 8)       | i <- [0 .. n2 - 1] ] :: [Double]
      f2s    = [ fromIntegral ((i * 5 + 1) `mod` 8)   | i <- [0 .. n2 - 1] ] :: [Double]   -- ノイズ特徴
      xMat2  = LA.fromLists (zipWith3 (\a b c -> [a, b, c]) f0s f1s f2s)                  -- 40×3
      yReg2  = zipWith (\a b -> 2 * a + 3 * b) f0s f1s                                    -- f0,f1 に線形依存
      yCls2  = [ if a + b >= 7 then 1 else 0 | (a, b) <- zip f0s f1s ] :: [Int]
  -- ① GBM 回帰 重要度
  let gbr    = fitGBRegressor defaultGBM xMat2 (VU.fromList yReg2)
      gbPlot = noDf |>> (toPlot gbr <> title "GradientBoosting feature importance")
  saveSVGBound "design/plot-integration/gbm-importance.svg" gbPlot
  -- ② RandomForestClassifier 重要度 (permutation・要 RNG)
  rfcGen <- MWC.initialize (V.fromList [7])
  rfc <- fitRFClassifier defaultRFCConfig xMat2 (VU.fromList yCls2) rfcGen
  let rfcPlot = noDf |>> (toPlot rfc <> title "RandomForestClassifier importance")
  saveSVGBound "design/plot-integration/rfclassifier-importance.svg" rfcPlot
  -- ③ DecisionTree 樹形図 (浅い木で可読性確保)
  let dt     = fitDTV defaultDecisionTree { dtMaxDepth = Just 3 } xMat2 (VU.fromList yCls2)
      dtPlot = noDf |>> (toPlot dt <> title "DecisionTree")
  saveSVGBound "design/plot-integration/decisiontree.svg" dtPlot
  -- ④ DecisionTree 樹形図 rpart.plot 流 (Phase 75.26)。 R の iris 例と直接比較できるよう
  --   **3 クラス** (setosa/versicolor/virginica)・2 特徴 (Petal.Length/Width) の
  --   iris 相当データを使う (各クラス 8 点を平均周りに決定的に散らす)。
  -- 高レベル df|-> で当てて toPlot (DTFit が特徴量名・クラス名 levels を保持)。
  -- species は text 列 (TxtData)・reqLabelWithLevels が辞書順 = setosa/versicolor/virginica
  -- で 0/1/2 に符号化 (低レベル irisY と同順)。
  let irisOff  = [ (-0.30,-0.10),(0.30,0.10),(0.00,-0.16),(0.00,0.16)
                 , (-0.20,0.05),(0.20,-0.05),(0.12,0.10),(-0.12,-0.10) ] :: [(Double, Double)]
      spread (mL, mW) = [ (mL + dl, mW + dw) | (dl, dw) <- irisOff ]
      irisRows = spread (1.4, 0.24) ++ spread (4.3, 1.32) ++ spread (5.5, 2.03)
      irisDF = [ ("Petal.Length", NumData (V.fromList (map fst irisRows)))
               , ("Petal.Width",  NumData (V.fromList (map snd irisRows)))
               , ("species",      TxtData (V.fromList
                   (concatMap (replicate 8) ["setosa", "versicolor", "virginica"]))) ] :: [(Text, ColData)]
      dtIris = irisDF |-> decisionTree defaultDecisionTree { dtMaxDepth = Just 3 }
                            ["Petal.Length", "Petal.Width"] "species"
      dtRpartPlot = noDf |>>
        (toPlot dtIris
           <> title "DecisionTree (rpart.plot style)"
           <> width 720 <> height 520)
  saveSVGBound "design/plot-integration/decisiontree-rpart.svg" dtRpartPlot
  putStrLn "wrote design/plot-integration/{gbm-importance,rfclassifier-importance,decisiontree,decisiontree-rpart}.svg"

  -- (2g) 分類 — Phase 68 A3。 2 特徴・3 クラスの blob で ① KNN 決定境界 + 訓練点、
  -- ② LDA 決定境界 + クラス平均、 ③ confusion 行列 (KNN)。 ①② は MScatter 領域、
  -- ③ は MHeatmap。 いずれも新規 mark 不要。
  let cblob (cx, cy) =
        [ (cx + dx, cy + dy)
        | (dx, dy) <- [ (-0.5, 0.4), (0.3, -0.4), (0.5, 0.5), (-0.4, -0.3), (0.0, 0.2), (0.2, 0.5) ] ]
      cls0 = cblob (1.2, 1.2); cls1 = cblob (5.0, 1.4); cls2 = cblob (3.0, 5.0)
      cpts = cls0 ++ cls1 ++ cls2
      clab = concat [ replicate (length cls0) 0, replicate (length cls1) 1
                    , replicate (length cls2) 2 ] :: [Int]
      cMat = LA.fromLists [ [x, y] | (x, y) <- cpts ]
      -- 高レベル df|-> で当てて knnC にクラス名 (levels) を保持させる。 species は text 列
      -- ("setosa"/"versicolor"/"virginica"・辞書順 = clab 0/1/2)。 confusion 軸・凡例に名前が出る。
      cdf  = [ ("f1",      NumData (V.fromList (map fst cpts)))
             , ("f2",      NumData (V.fromList (map snd cpts)))
             , ("species", TxtData (V.fromList
                 (concat [ replicate (length cls0) "setosa"
                         , replicate (length cls1) "versicolor"
                         , replicate (length cls2) "virginica" ]))) ] :: [(Text, ColData)]
      knnC = cdf |-> knnCls 5 ["f1", "f2"] "species"
      ldaC = either (error . T.unpack) id (fitLDA cMat (V.fromList clab))
      xr   = (0, 6.5); yr = (0, 6.5)
      knnDB = noDf |>> ( decisionBoundaryOf knnC xr yr 60 <> toPlot knnC
                         <> title "KNN decision boundary (k=5)" )
      ldaDB = noDf |>> ( decisionBoundaryOf ldaC xr yr 60 <> toPlot ldaC
                         <> title "LDA decision boundary + class means" )
      confP = noDf |>> ( confusionOf knnC cMat clab <> title "KNN confusion (train)" )
  saveSVGBound "design/plot-integration/knn-decision-boundary.svg" knnDB
  saveSVGBound "design/plot-integration/lda-decision-boundary.svg" ldaDB
  saveSVGBound "design/plot-integration/knn-confusion.svg" confP
  putStrLn "wrote design/plot-integration/{knn-decision-boundary,lda-decision-boundary,knn-confusion}.svg"

  -- (2g1) Phase 75.7: Naive Bayes 決定境界 (Gaussian NB)。 toPlot=クラス平均散布を重ねる。
  let nbM = NBGaussian (fitGNB cMat (VU.fromList clab))
      nbDB = noDf |>> ( decisionBoundaryOf nbM xr yr 60 <> toPlot nbM
                        <> title "Naive Bayes decision boundary (Gaussian)" )
  saveSVGBound "design/plot-integration/nb-decision-boundary.svg" nbDB
  putStrLn "wrote design/plot-integration/nb-decision-boundary.svg"

  -- (2g2) Phase 75: MDS 埋め込み散布 / NN 学習損失曲線用のデータ (NN fit は IO)。
  let svmMat = LA.fromLists [ [x, y] | (x, y) <- cls0 ++ cls1 ]
      svmLab = VU.fromList (replicate (length cls0) 0 ++ replicate (length cls1) 1)
  -- (2g3) Phase 75.11/75.12/75.15: カーネル SVM (RBF・真の SV)。 内側=密な円盤 (class 0)・
  --   外側=密な輪 (class 1) の同心 (線形非分離) を、 非線形決定境界 + クラス色散布 +
  --   サポートベクタ (✚赤) で描く。 点はクラス色 (scatter+colorBy) で塗り分ける。
  let innerPts = (0, 0)
               : [ (cos t * r, sin t * r)
                 | r <- [0.6, 1.1 :: Double], k <- [0 .. 7 :: Int]
                 , let t = fromIntegral k * pi / 4 ]                 -- 円盤 (17 点)
      outerPts = [ (cos t * r, sin t * r)
                 | r <- [2.6, 3.2 :: Double], k <- [0 .. 11 :: Int]
                 , let t = fromIntegral k * pi / 6 ]                 -- 輪 (24 点)
      ringPts = innerPts ++ outerPts
      ringLab = replicate (length innerPts) 0 ++ replicate (length outerPts) 1 :: [Int]
      ringMat = LA.fromLists [ [a, b] | (a, b) <- ringPts ]
      -- RBF・γ=0.3 ⇔ ℓ=√(1/0.6) (γ=1/(2ℓ²)・Phase 75.15 共有 Kernel)。
      svm = fitSVM defaultSVM
               { svmKernel = RBF
               , svmParams = defaultKernelParams { kpLengthScale = sqrt (1 / 0.6) }
               , svmC = 10 }
               ringMat (VU.fromList ringLab)
      rr = (-4, 4)
      -- ★cls は **カテゴリ列 (Text)** で渡す。 数値列だと colorBy が連続スケール
      --   (viridis: 紫→金) になりクラス色に見えない。 Text なら categorical = ggplot hue_pal。
      ringDf = [ ("x1",  NumData (V.fromList (map fst ringPts)))
               , ("x2",  NumData (V.fromList (map snd ringPts)))
               , ("cls", TxtData (V.fromList (map (T.pack . show) ringLab))) ] :: [(Text, ColData)]
      -- Phase 76.A: 決定領域を annotRect grid で実塗り。 非線形 (RBF) 境界がクラス色で
      -- 塗り分けられ、 訓練点 (colorBy) を薄塗りの上に重ねる。
      svmDB = ringDf |>> ( decisionBoundaryOf svm rr rr 80
                          <> layer (scatter "x1" "x2" <> colorBy "cls")
                          <> title "Kernel SVM (RBF) decision regions" )
  saveSVGBound "design/plot-integration/svm-rbf-boundary.svg" svmDB
  putStrLn "wrote design/plot-integration/svm-rbf-boundary.svg"
  -- MDS: 3 クラス blob を df |-> mds (モデル型 MDSResult)・cls 列で群色散布。
  let mdsDf = [ ("x1",  NumData (V.fromList (map fst cpts)))
              , ("x2",  NumData (V.fromList (map snd cpts)))
              , ("cls", TxtData (V.fromList (map (T.pack . show) clab))) ] :: [(Text, ColData)]
      mdsM  = mdsDf |-> mds defaultMDS ["x1","x2"]
      mdsP  = noDf |>> ( toPlot (mdsView mdsM <> mdsGroupBy "cls")
                         <> title "MDS embedding (classical)" )
  saveSVGBound "design/plot-integration/mds-scatter.svg" mdsP
  -- NN 決定境界 (主役): 同心 2 クラス (線形非分離) を MLP が曲げて分離する。
  nnRingGen <- MWC.initialize (V.fromList [76])
  nnRingFit <- fitMLPClassifier defaultMLP { mlpHidden = [16, 8], mlpEpochs = 600 }
                 ringMat (VU.fromList ringLab) nnRingGen
  let nnDB = ringDf |>> ( decisionBoundaryOf nnRingFit (-4, 4) (-4, 4) 80
                          <> layer (scatter "x1" "x2" <> colorBy "cls")
                          <> title "MLP decision regions" )
  saveSVGBound "design/plot-integration/nn-decision-boundary.svg" nnDB
  -- NN 損失曲線 (副次・収束確認)。
  nnGen <- MWC.initialize (V.fromList [75])
  nnFit <- fitMLPClassifier defaultMLP svmMat svmLab nnGen
  let nnP = noDf |>> ( nnLossOf nnFit <> title "MLP training loss" )
  saveSVGBound "design/plot-integration/nn-loss.svg" nnP
  putStrLn "wrote design/plot-integration/{mds-scatter,nn-decision-boundary,nn-loss}.svg"

  -- (2h) 次元圧縮 — Phase 68 A4。 ① PLS score / loading / VIP、 ② MultiGP 多出力曲線。
  -- ① は自己完結 (PLSFit は scores/loadings/VIP 保持)、 ② は出力ごとの mean+band 重畳。
  let n4   = 24 :: Int
      f0   = [ sin (0.4 * fromIntegral i)              | i <- [0 .. n4 - 1] ] :: [Double]
      f1   = [ cos (0.3 * fromIntegral i)              | i <- [0 .. n4 - 1] ]
      f2   = [ 0.5 * fromIntegral (i `mod` 5)          | i <- [0 .. n4 - 1] ]
      f3   = [ fromIntegral ((i * 7) `mod` 4) - 1.5    | i <- [0 .. n4 - 1] ]   -- 弱い特徴
      xPLS = LA.fromLists [ [a, b, c, d] | (((a, b), c), d) <- zip (zip (zip f0 f1) f2) f3 ]  -- 24×4
      yPLS = LA.fromLists [ [2 * a + b, a - c] | (a, b, c) <- zip3 f0 f1 f2 ]    -- 24×2 (2 出力)
      plsFit = either (error . T.unpack) id (fitPLS defaultPLS { plsN_Components = 2 } xPLS yPLS)
  saveSVGBound "design/plot-integration/pls-score.svg"
    (noDf |>> (toPlot (scoreView plsFit) <> title "PLS score plot"))
  saveSVGBound "design/plot-integration/pls-loading.svg"
    (noDf |>> (toPlot (loadingView plsFit) <> title "PLS loading plot"))
  saveSVGBound "design/plot-integration/pls-vip.svg"
    (noDf |>> (toPlot (vipView plsFit) <> title "PLS VIP"))
  -- ② MultiGP: 1D 入力で 2 出力 (sin, cos) を予測 grid で曲線+band
  let gx    = [ 0.5 * fromIntegral i | i <- [0 .. 20 :: Int] ] :: [Double]       -- train x (0..10)
      gy0   = map sin gx
      gy1   = map cos gx
      tx    = [ 0.25 * fromIntegral i | i <- [0 .. 40 :: Int] ]                  -- test grid
      mgp   = fitMultiGP gx [gy0, gy1] tx
      mgpPlot = noDf |>> (multiGpCurves mgp <> title "MultiGP multi-output curves")
  saveSVGBound "design/plot-integration/multigp-curves.svg" mgpPlot
  putStrLn "wrote design/plot-integration/{pls-score,pls-loading,pls-vip,multigp-curves}.svg"

  -- (2i) 時系列・生存・FDA — Phase 68 A5。 ① GARCH volatility 帯付き線、
  -- ② AFT 生存曲線 (基準共変量)、 ③ FDA 平均関数 + 固有関数。 新規 mark 不要。
  -- ① GARCH: volatility clustering する合成リターン系列
  let gN    = 120 :: Int
      regime i = if (i `div` 20) `mod` 2 == (0 :: Int) then 0.5 else 2.0 :: Double
      garchY = LA.fromList [ regime i * cos (fromIntegral i * 1.3) | i <- [0 .. gN - 1] ]
      gfit   = fitGARCH garchY
  saveSVGBound "design/plot-integration/garch-volatility.svg"
    (noDf |>> (toPlot gfit <> title "GARCH conditional volatility band"))
  -- ② AFT: Weibull 生存データ (intercept + 1 共変量)
  let aN    = 40 :: Int
      aCov  = [ fromIntegral (i `mod` 4) - 1.5 | i <- [0 .. aN - 1] ] :: [Double]
      aX    = LA.fromLists [ [1, c] | c <- aCov ]                       -- intercept + covariate
      aT    = LA.fromList [ 2 + 0.5 * fromIntegral (i `mod` 9) + 0.3 * (aCov !! i) + 1
                          | i <- [0 .. aN - 1] ]                        -- 正の生存時刻
      aDel  = V.fromList [ even i | i <- [0 .. aN - 1] ]                -- 約半数 censored
  aftRes <- fitAFT AFTWeibull aX aT aDel
  case aftRes of
    Left err  -> putStrLn ("AFT fit failed: " ++ T.unpack err)
    Right afit ->
      saveSVGBound "design/plot-integration/aft-survival.svg"
        (noDf |>> (toPlot afit <> title "AFT survival curve (baseline)"))
  -- ③ FDA: 10 本の合成曲線 (位相ずれ正弦) を smooth → functionalPCA
  let fGrid = LA.fromList [ fromIntegral i / 4 | i <- [0 .. 28 :: Int] ]   -- 0..7
      curve ph = [ sin (t + ph) + 0.3 * t | t <- LA.toList fGrid ]
      fY    = LA.fromLists [ curve (0.3 * fromIntegral s) | s <- [0 .. 9 :: Int] ]  -- 10×29
      fSamp = smoothBasis (FDA.BSpline 3 [1, 2, 3, 4, 5, 6]) 0.1 fGrid fY
      fpca  = functionalPCA 3 fSamp
  saveSVGBound "design/plot-integration/fda-fpca.svg"
    (noDf |>> (toPlot fpca <> title "FDA: mean + eigenfunctions"))
  putStrLn "wrote design/plot-integration/{garch-volatility,aft-survival,fda-fpca}.svg"

  -- (2j) 罰則回帰・因果 — Phase 68 A6。 ① LASSO 係数パス、 ② 因果 DAG (LiNGAM)。
  -- ★パスと係数 bar は **同一 5 特徴データ**で統一。 真の信号 x1,x2・x3=x1 相関の冗長・
  -- x4,x5=純ノイズ。 y = 3 x1 + 1.5 x2 + 小ノイズ。 lasso は x3-x5 を 0 へ・ridge は分散して残す。
  let rN    = 50 :: Int
      rnoise k i = sin (fromIntegral (i * 7 + k * 13 :: Int))            -- 決定的・有界
      rf1   = [ rnoise 1 i | i <- [0 .. rN - 1] ] :: [Double]
      rf2   = [ rnoise 2 i | i <- [0 .. rN - 1] ]
      rgX3  = zipWith (\a i -> 0.9 * a + 0.1 * rnoise 4 i) rf1 [0 :: Int ..]  -- x1 と強相関 (冗長)
      rgX4  = [ rnoise 5 i | i <- [0 .. rN - 1] ]                        -- 純ノイズ
      rgX5  = [ rnoise 6 i | i <- [0 .. rN - 1] ]                        -- 純ノイズ
      ry    = [ 3 * a + 1.5 * b + 0.1 * rnoise 9 i
              | (i, (a, b)) <- zip [0 ..] (zip rf1 rf2) ] :: [Double]
      dfReg = [ ("x1", rf1), ("x2", rf2), ("x3", rgX3), ("x4", rgX4), ("x5", rgX5)
              , ("y", ry) ] :: [(Text, [Double])]
      cols5 = ["x1", "x2", "x3", "x4", "x5"]
      -- 正則化パス用の設計行列 [1, x1..x5] (係数 bar と同一の 5 特徴)。
      rX5   = LA.fromLists [ [1, rf1 !! i, rf2 !! i, rgX3 !! i, rgX4 !! i, rgX5 !! i]
                           | i <- [0 .. rN - 1] ]
      lams  = [ 0.001 * 1.6 ** fromIntegral k | k <- [0 .. 18 :: Int] ]  -- 昇順 λ (log₁₀≈ -3..1.1)
      lpath = regularizationPath (\l -> L1 l) lams rX5 (LA.fromList ry)
      mLas  = dfReg |-> lasso cols5 "y"
      mRid  = dfReg |-> ridge cols5 "y"
      -- lasso (左・スパース選択) ↔ ridge (右・分散縮約) を 1 枚で対比 (subplot 横並び)。
      coefCompare = noDf |>> ( subplots
                        [ toPlot mLas <> title "Lasso (sparse selection)"
                        , toPlot mRid <> title "Ridge (variance shrinkage)" ]
                      <> subplotCols 2 <> width 1000 <> height 380 )
  saveSVGBound "design/plot-integration/lasso-path.svg"
    $ noDf |>> regPathPlot lpath <> title "LASSO coefficient path"
  saveSVGBound "design/plot-integration/regularized-coefs.svg" coefCompare
  putStrLn "wrote design/plot-integration/{lasso-path,regularized-coefs}.svg"
  -- ② LiNGAM: 6 変数の健康モデル (分岐・合流・複数親を含む非自明な DAG)。
  --   genetics/diet = 外生。 exercise←genetics。 bmi←diet,exercise。 bp←bmi,genetics。
  --   chd←bp,bmi。 LiNGAM は誤差が **独立な非ガウス** 前提ゆえノイズは MWC 一様分布。
  let lN = 400 :: Int
  lingGen <- MWC.initialize (V.fromList [99])
  let mkNoise = mapM (const (MWC.uniformR (-1.0, 1.0) lingGen)) [1 .. lN]
  eg  <- mkNoise
  ed  <- mkNoise
  eex <- mkNoise
  ebm <- mkNoise
  ebp <- mkNoise
  ech <- mkNoise
  let genetics = eg :: [Double]
      diet     = ed
      exercise = zipWith  (\g e -> -0.6 * g + e) genetics eex
      bmi      = zipWith3 (\d x e -> 0.8 * d - 0.7 * x + e) diet exercise ebm
      bp       = zipWith3 (\b g e -> 0.9 * b + 0.4 * g + e) bmi genetics ebp
      chd      = zipWith3 (\b m e -> 1.1 * b + 0.5 * m + e) bp bmi ech
      lCfg = defaultDirectLiNGAMConfig { dlcPruneThr = 0.3 }   -- 真係数 0.4-1.1 ゆえ弱い偽陽性を除く
      lNames = ["genetics","diet","exercise","bmi","bp","chd"]
      lDF  = [ ("genetics", NumData (V.fromList genetics))
             , ("diet",     NumData (V.fromList diet))
             , ("exercise", NumData (V.fromList exercise))
             , ("bmi",      NumData (V.fromList bmi))
             , ("bp",       NumData (V.fromList bp))
             , ("chd",      NumData (V.fromList chd)) ] :: [(Text, ColData)]
      lfit = lDF |-> directLingam lCfg lNames
  saveSVGBound "design/plot-integration/lingam-dag.svg"
    (noDf |>> toPlot lfit <> title "LiNGAM causal DAG (df |-> directLingam)")
  -- 検証用: 元データのペアプロット (周辺相関)。 DAG (直接因果) と対比すると、 marginal に
  -- 相関する対 (子孫同士・交絡経由) でも直接辺が無いものは DAG に出ないことが分かる。
  saveSVGBound "design/plot-integration/lingam-pairs.svg"
    (lDF |>> pairs ["genetics","diet","exercise","bmi","bp","chd"]
          <> title "Pair plot (周辺相関) vs 因果 DAG (直接因果)" <> width 720 <> height 720)
  putStrLn "wrote design/plot-integration/{lasso-path,lingam-dag,lingam-pairs}.svg"
  -- Phase 77.B: Pairwise (向き) / MultiGroup (共通 DAG) / VAR (時間ラグ DAG)。
  -- Pairwise は 2 変数直結の向き判定ゆえ、 信号が強い単方向ペアで示す
  -- (cause = 非ガウス外生、 effect = 1.8·cause + 弱ノイズ)。
  let pwCause  = genetics
      pwEffect = zipWith (\c e -> 1.8 * c + 0.3 * e) pwCause eex
      pwDF = [ ("cause",  NumData (V.fromList pwCause))
             , ("effect", NumData (V.fromList pwEffect)) ] :: [(Text, ColData)]
      pwFit = pwDF |-> pairwiseLingam 0.0 "cause" "effect"
  saveSVGBound "design/plot-integration/lingam-pairwise.svg"
    (noDf |>> toPlot pwFit <> title "Pairwise LiNGAM (cause -> effect)")
  -- MultiGroup: 同じ SEM を 2 群に分け共通 DAG (grp 0/1)。
  let mgDF = lDF ++ [ ("grp", NumData (V.fromList (replicate (lN `div` 2) 0 ++ replicate (lN - lN `div` 2) 1))) ]
      mgCfg = defaultMultiGroupConfig { mgcDirectCfg = lCfg }   -- 内部 DirectLiNGAM も閾値 0.3
      mgFit = mgDF |-> multiGroupLingam mgCfg lNames "grp"
  saveSVGBound "design/plot-integration/lingam-multigroup.svg"
    (noDf |>> toPlot mgFit <> title "MultiGroup LiNGAM common DAG")
  -- VAR: 2 変数時系列 a_t = 0.5 a_{t-1} + e; b_t = 0.6 a_t + 0.3 b_{t-1} + e。
  varGen <- MWC.initialize (V.fromList [7])
  vea <- mapM (const (MWC.uniformR (-1.0, 1.0) varGen)) [1 .. lN]
  veb <- mapM (const (MWC.uniformR (-1.0, 1.0) varGen)) [1 .. lN]
  let vabPairs = tail (scanl vstep (0, 0) (zip vea veb))
      vstep (aPrev, bPrev) (ea, eb) =
        let a = 0.5 * aPrev + ea
            b = 0.6 * a + 0.3 * bPrev + 0.3 * eb
        in (a, b)
      varDF  = [ ("a", NumData (V.fromList (map fst vabPairs)))
               , ("b", NumData (V.fromList (map snd vabPairs))) ] :: [(Text, ColData)]
      varFit = varDF |-> varLingam defaultVARLiNGAMConfig ["a","b"]
  saveSVGBound "design/plot-integration/lingam-var-lag.svg"
    (noDf |>> toPlot varFit <> title "VAR-LiNGAM time-lag DAG")
  -- Phase 77.C: Bootstrap (エッジ確信度) / ICA。 6 変数健康 SEM に適用。
  let bsCfg  = defaultBootstrapConfig { bcNumBootstraps = 100, bcDirectCfg = lCfg }
      bsFit  = lDF |-> bootstrapLingam bsCfg lNames
  saveSVGBound "design/plot-integration/lingam-bootstrap-dag.svg"
    (noDf |>> toPlot bsFit <> title "Bootstrap LiNGAM: 確信度 DAG (edge prob >= 0.5)")
  saveSVGBound "design/plot-integration/lingam-bootstrap-prob.svg"
    (noDf |>> bootstrapEdgeProbOf bsFit <> title "Bootstrap LiNGAM: edge 出現確率")
  let icaFit = lDF |-> icaLingam defaultICALiNGAMConfig { ilcPruneThr = 0.3 } lNames
  saveSVGBound "design/plot-integration/lingam-ica-dag.svg"
    (noDf |>> toPlot icaFit <> title "ICA-LiNGAM causal DAG")
  putStrLn "wrote design/plot-integration/{lingam-bootstrap-dag,lingam-bootstrap-prob,lingam-ica-dag}.svg"
  -- Phase 78.C/D/F: DOE prediction profiler。 2 因子 RSM を組み **複数応答** (strength/yield) を
  -- multiOutput で一括当てはめ、 行=応答 × 列=因子のグリッドに並べる (JMP Prediction Profiler 相当)。
  let doePlan = centralCompositeDesign [contFactor "temp" (150, 180), contFactor "time" (10, 20)]
      doeRs   = designTable doePlan
      dTemp   = maybe [] id (lookup "temp" doeRs)
      dTime   = maybe [] id (lookup "time" doeRs)
      -- sim 応答 1 (strength): temp に 2 次 (中心 165 で最大)・time は線形・弱ノイズ。
      strength = zipWith3 (\t d e -> 50 + 0.6 * t - 0.02 * (t - 165) * (t - 165) + 1.5 * d + e)
                  dTemp dTime (cycle [0.6, -0.5, 0.4, -0.3, 0.2, -0.1])
      -- sim 応答 2 (yield): temp は線形・time に 2 次 (中心 15 で最大)・弱ノイズ。
      yield    = zipWith3 (\t d e -> 20 + 0.3 * t + 0.8 * d - 0.05 * (d - 15) * (d - 15) + e)
                  dTemp dTime (cycle [0.3, -0.2, 0.25, -0.15, 0.1, -0.05])
      doeDF    = [ (n, NumData (V.fromList v))
                 | (n, v) <- ("strength", strength) : ("yield", yield) : doeRs ] :: [(Text, ColData)]
      -- 複数応答を 1 動詞で: [(応答名, MultiLMModel)]。同じ plan を各応答に使い回す。
      doeModels = doeDF |-> multiOutput ["strength","yield"] (designModel doePlan)
  saveSVGBound "design/plot-integration/doe-profiler.svg"
    (noDf |>> toPlot (profiler doeModels ["temp","time"])
          <> title "DOE prediction profiler (行=応答 × 列=因子・予測線+CI+実測)"
          <> width 760 <> height 620)
  putStrLn "wrote design/plot-integration/doe-profiler.svg"
  -- Phase 78.D: 偏残差版 (<> profilerResidual Partial)。 生点は他因子が動くぶん予測線から
  -- 縦に散るが、 偏残差 (fⱼ(xⱼ)+全モデル残差) を打点すると点が予測線に乗り当てはまりが見やすい。
  saveSVGBound "design/plot-integration/doe-profiler-partial.svg"
    (noDf |>> toPlot (profiler doeModels ["temp","time"] <> profilerResidual Partial)
          <> title "DOE profiler (偏残差版・fⱼ(xⱼ)+残差で点が予測線に乗る)"
          <> width 760 <> height 620)
  putStrLn "wrote design/plot-integration/doe-profiler-partial.svg"
  -- Phase 78.E: RSM 等高線 / 応答曲面 (strength モデル)。 2 因子を grid で動かし μ̂ を塗り+等高線で俯瞰。
  saveSVGBound "design/plot-integration/doe-contour.svg"
    (noDf |>> contourOf (snd (head doeModels)) "temp" "time"
          <> title "DOE response surface (RSM 等高線・strength の temp×time 応答面)"
          <> width 560 <> height 460)
  putStrLn "wrote design/plot-integration/doe-contour.svg"
  -- Phase 78.G-e: designModel の非 LM 版 designModelGP。同じ plan・同じデータを GP/RFF で
  -- 当て、profiler の帯が LM の正規 CI から **GP 事後予測帯** に置き換わる (小 n DOE の
  -- 非パラメトリック不確実性)。multiOutput が designModelGP でも同型に使える (カレー化)。
  let doeModelsGP = doeDF |-> multiOutput ["strength","yield"] (designModelGP defaultGP doePlan)
  saveSVGBound "design/plot-integration/doe-profiler-gp.svg"
    (noDf |>> toPlot (profiler doeModelsGP ["temp","time"])
          <> title "DOE profiler (GP 版・designModelGP・帯 = GP 事後予測帯)"
          <> width 760 <> height 620)
  putStrLn "wrote design/plot-integration/doe-profiler-gp.svg"
  -- Phase 78.G-f: designModel の階層ベイズ版 designModelHBM。 単一因子 (temp) の RSM に
  -- lot (A/B/C) のランダム切片を加えた合成データを fit し、 profiler の帯が **HBM 事後予測帯**
  -- (固定効果 β の draw 分散 + 観測 noise σ²・群平均で marginalize) になることを示す。
  -- lot 間に明確な切片差 (-6/0/+8) を仕込み、 階層モデルが効くデータにしている。
  let doeHbmPlan     = centralCompositeDesign [contFactor "temp" (150, 180)]
      doeHbmRuns     = designTable doeHbmPlan
      doeHbmTempPts  = maybe [] id (lookup "temp" doeHbmRuns)          -- CCD (k=1): 5 点
      doeHbmLots     = ["A", "B", "C"] :: [Text]
      doeHbmLotShift = [("A", 0), ("B", 8), ("C", -6)] :: [(Text, Double)]
      -- 各 lot × 各 temp 点を 2 反復 = 3*5*2 = 30 行 (固定シード無しの決定的ノイズ列)。
      doeHbmRows     = [ (lot, t) | lot <- doeHbmLots, _rep <- [1, 2 :: Int], t <- doeHbmTempPts ]
      doeHbmTemp     = map snd doeHbmRows
      doeHbmLot      = map fst doeHbmRows
      doeHbmNoise    = cycle [0.5, -0.4, 0.3, -0.2, 0.1, -0.1, 0.2, -0.3, 0.4, -0.5]
      doeHbmY        = zipWith3 (\lot t e -> 60 + 0.6 * t - 0.02 * (t - 165) * (t - 165)
                                            + maybe 0 id (lookup lot doeHbmLotShift) + e)
                      doeHbmLot doeHbmTemp doeHbmNoise
      doeHbmDF       = [ ("temp", NumData (V.fromList doeHbmTemp))
                    , ("lot",  TxtData (V.fromList doeHbmLot))
                    , ("y",    NumData (V.fromList doeHbmY)) ] :: [(Text, ColData)]
      doeHbmModelFit = doeHbmDF |-> designModelHBM defaultHBM doeHbmPlan [ranIntercept "lot"] "y"
  saveSVGBound "design/plot-integration/doe-profiler-hbm.svg"
    (noDf |>> toPlot (profiler [("y", doeHbmModelFit)] ["temp"])
          <> title "DOE profiler (HBM 版・designModelHBM・帯 = 階層ベイズ事後予測帯・lot 変量切片)"
          <> width 760 <> height 460)
  putStrLn "wrote design/plot-integration/doe-profiler-hbm.svg"
  -- 相関グラフ vs LiNGAM DAG の対比 (相関は間接・交絡も辺に = 過剰に密・LiNGAM は直接因果に削減)。
  saveSVGBound "design/plot-integration/corr-graph.svg"
    (noDf |>> toPlot (lDF |-> correlationOf 0.3 lNames)
          <> title "Correlation graph (|r|>0.3): 相関で辺は出るが過剰に密"
          <> width 640 <> height 560)
  putStrLn "wrote corr-graph.svg"
  putStrLn "wrote design/plot-integration/{lingam-pairwise,lingam-multigroup,lingam-var-lag}.svg"

  -- (2k) 記述統計・検定 — Phase 68 A7。 ① 群間平均差の CI forest、 ② describe の box。
  -- forest の 0 基準線は平均差 (null=0) 向け。 tostWelch の trCI は Welch 平均差の CI
  -- (大半の検定は trCI=Nothing で forest 非対象・要 CI 保持検定)。
  let grpA  = LA.fromList [ 5.1, 4.9, 5.3, 5.0, 5.4, 4.8, 5.2, 5.1 ]
      grpB  = LA.fromList [ 6.0, 5.8, 6.2, 5.9, 6.3, 5.7, 6.1, 6.0 ]
      grpC  = LA.fromList [ 4.6, 4.4, 4.8, 4.5, 4.7, 4.3, 4.9, 4.5 ]
      tAB   = tostWelch grpA grpB 1.0
      tAC   = tostWelch grpA grpC 1.0
      tBC   = tostWelch grpB grpC 1.0
      fPlot = noDf |>> ( testForestLabeled [("A vs B", tAB), ("A vs C", tAC), ("B vs C", tBC)]
                         <> title "Welch mean difference 90% CI" )
  saveSVGBound "design/plot-integration/test-forest.svg" fPlot
  saveSVGBound "design/plot-integration/describe-box.svg"
    (noDf |>> (describeBox (LA.toList grpA) <> title "describe: distribution box"))
  putStrLn "wrote design/plot-integration/{test-forest,describe-box}.svg"

  -- (3) 静的 HTML ビューア = HS SVG を 1 枚に埋め込み (compare.html 非依存)。
  -- 統合図 (LM/GP) + プレーン例 (scatter/line/bar/hist) を並べる。
  let bx   = [1, 2, 3, 4, 5] :: [Double]
      bv   = [3, 7, 4, 8, 5] :: [Double]
      hx   = [ sin (0.3 * fromIntegral i) + 0.2 * fromIntegral (i `mod` 5)
             | i <- [0 .. 120 :: Int] ] :: [Double]
      cells =
        [ ("LM — 散布図 + 回帰線 + 95% CI band (toPlot LMModel)", renderBound lmPlot)
        , ("GLM — Poisson μ 曲線 + 非対称 Wald CI 帯 (toPlot GLMModel)", renderBound glmPlot)
        , ("Spline — 平滑曲線 + 95% CI band (toPlot SplineModel)", renderBound splPlot)
        , ("GAM — 加法平滑曲線 (toPlot GAMModel、 band 非提供)", renderBound gamPlot)
        , ("Robust — ロバスト直線 (Huber) vs OLS+band (toPlot RobustModel)", renderBound robPlot)
        , ("MultiLM — 出力間の残差相関 heatmap (toPlot MultiFit)", renderBound mlPlot)
        , ("Quantile — 複数分位線 τ=0.1/0.5/0.9 (toPlot QuantileModel)", renderBound qPlot)
        , ("MCMC trace — draw 列 (toPlot ChainModel)", renderBound tracePlt)
        , ("MCMC density — 周辺事後 (diagnosticPlots ChainModel)", renderBound densPlt)
        , ("KM — Kaplan-Meier 生存曲線 (toPlot KMResult)", renderBound kmPlot)
        , ("CIF — 競合リスク累積発生 (toPlot CRFit)", renderBound cifPlot)
        , ("Forecast — AR(2) 予測 + 予測区間 band (toPlot ForecastModel)", renderBound fcPlot)
        , ("PCA — scree plot 寄与率 (toPlot PCAResult)", renderBound screePlot)
        , ("RandomForest — 特徴重要度 (toPlot RandomForest)", renderBound rfPlot)
        , ("GP — 事後平均 + credible band (toPlot GPResult)",     renderBound gpPlot)
        , ("HBM epred — 事後予測平均 + 94% HDI band (epred)", renderBound epredPlot)
        , ("HBM trace — a/b/s を param ごと縦並び (tracesOf)", renderBound tracePltH)
        , ("HBM forest — 事後平均 + 94% HDI (forestOf)", renderBound forestPlt)
        , ("HBM ppc — 観測 vs 事後予測 y_rep (ppcOf)", renderBound ppcPlt)
        , ("HBM dag — モデル構造 DAG (dagOf)", renderBound dagPlt)
        , ("散布図 (Easy.points)",   renderSVG (plots [points xsL ysL]))
        , ("折れ線 (Easy.lineXY)",   renderSVG (plots [lineXY xsL ysL]))
        , ("棒グラフ (Easy.bars)",   renderSVG (plots [bars bx bv]))
        , ("ヒストグラム (Easy.hist)", renderSVG (plots [hist hx]))
        ]
  -- (RSM 3D) Phase 24 A9: 応答曲面法 (RSM) の cross-repo 実務フロー。
  -- 中心複合計画 (CCD) で 2 因子 (温度 x1・時間 x2) の収率 y を取り、 2 次
  -- モデルを fit → analyze surfaceGrid で μ̂ grid 評価 → plot-3d で応答曲面
  -- (colormap + 床面 contour) + 実測点 overlay。 A2/A5/A8 の機能を一通り使う。
  let aAx = sqrt 2          -- CCD axial 距離 α
      ccd = [ (-1,-1), (1,-1), (-1,1), (1,1)            -- factorial corners
            , (-aAx,0), (aAx,0), (0,-aAx), (0,aAx)       -- axial points
            , (0,0) ] :: [(Double, Double)]             -- center
      -- 真の応答 = 凹 2 次のピーク (収率最大付近)。
      yOf (x1,x2) = 82 + 4*x1 + 3*x2 - 7*x1*x1 - 5*x2*x2 - 3*x1*x2
      rsmDf = DX.fromNamedColumns
        [ ("x1", DX.fromList [ x1   | (x1,_) <- ccd ])
        , ("x2", DX.fromList [ x2   | (_,x2) <- ccd ])
        , ("y",  DX.fromList [ yOf p | p <- ccd ]) ]
      mRSM = either error id
               (multiLMModel "y ~ x1 + x2 + I(x1^2) + I(x2^2) + I(x1*x2)" rsmDf)
      (gxs, gys, grid3) = surfaceGrid mRSM "x1" "x2" defaultSurfaceOpts
      -- 実測点は CCD 生データから直接 (大きめ size で視認性↑)。
      obsPts = [ Point3 x1 x2 (yOf (x1, x2)) | (x1, x2) <- ccd ]
      -- 曲面 + contour + 実測点 + 軸名 (camera 抜き = 回転 montage で差し替え)。
      rsmBody =
            P3.layer3D ( P3.surface3DGrid grid3
                      <> P3.xRange3D (head gxs, last gxs)
                      <> P3.yRange3D (head gys, last gys)
                      <> P3.colormap3D <> P3.contourZ 8 )
         <> P3.layer3D (P3.scatter3DPoints obsPts <> P3.color3D (fromHex "#d62728") <> P3.size3D 7)
         <> P3.axisTitles3D "温度 x1" "時間 x2" "収率 y"
      rsmSpec = rsmBody
         <> P3.camera (cameraIso 5)
         <> P3.width3DV 760 <> P3.height3DV 720
         <> P3.title3D "RSM: 2 次応答曲面 + 床面 contour + CCD 実測点"
  P3E.saveSVG3D "design/plot-integration/rsm-surface-3d.svg" rsmSpec
  P3E.savePNG3D "design/plot-integration/rsm-surface-3d.png" rsmSpec
  putStrLn "wrote design/plot-integration/rsm-surface-3d.{svg,png}"

  -- z 軸まわりに方位角を振った 4 視点を facet で並置 (= 「z 軸回転」 の montage)。
  -- iso 既定 eye (5,-5,3) を z 軸まわりに θ 回転。 半径 √50・高さ 3 固定。
  let rotCam thetaDeg =
        let r0 = sqrt 32; h = 2.4               -- 既定より近い eye (cube を大きく)
            base = atan2 (-4) 4                 -- 方位角 -45°
            a    = base + thetaDeg * pi / 180
        in Camera3D (Point3 (r0 * cos a) (r0 * sin a) h) (Point3 0 0 0) zUp
      rotPanel thetaDeg =
        ( T.pack ("方位 " ++ show (round thetaDeg :: Int) ++ "°")
        , rsmBody <> P3.camera (rotCam thetaDeg)
                  <> P3.width3DV 600 <> P3.height3DV 540 )
  P3E.saveSVG3DFacet "design/plot-integration/rsm-rotation.svg"
    [ rotPanel t | t <- [0, 45, 90, 135] ]
  putStrLn "wrote design/plot-integration/rsm-rotation.svg"

  TIO.writeFile "design/plot-integration/viewer.html" (viewerHtml cells)
  putStrLn "wrote design/plot-integration/viewer.html (open in a browser)"

-- | (タイトル, SVG text) の並びを 1 枚の自己完結 HTML に。 SVG は inline 埋め込み
-- (xml 宣言なしの @\<svg\>@ 直開始ゆえそのまま body に置ける)。 CSS grid でタイル表示。
viewerHtml :: [(Text, Text)] -> Text
viewerHtml cells = T.concat
  [ "<!DOCTYPE html>\n<html lang=\"ja\"><head><meta charset=\"utf-8\">"
  , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "<title>hgg — HS SVG viewer</title><style>"
  , "body{font-family:system-ui,sans-serif;margin:24px;background:#fafafa;color:#222}"
  , "h1{font-size:20px;margin:0 0 4px}p.note{color:#666;margin:0 0 20px;font-size:13px}"
  , ".grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(360px,1fr));gap:20px}"
  , "figure{margin:0;background:#fff;border:1px solid #e0e0e0;border-radius:8px;"
  , "padding:12px;box-shadow:0 1px 3px rgba(0,0,0,.06)}"
  , "figcaption{font-weight:600;margin-bottom:8px;font-size:14px}"
  , "svg{width:100%;height:auto;display:block}"
  , "</style></head><body>"
  , "<h1>hgg — HS SVG viewer</h1>"
  , "<p class=\"note\">HS backend が出力した SVG をそのまま埋め込み。 "
  , "ブラウザで開くだけ (PS bundle / esbuild 不要、 compare.html とは別経路)。</p>"
  , "<div class=\"grid\">"
  , T.concat (map renderCell cells)
  , "</div></body></html>"
  ]
  where
    renderCell (caption, svg) =
      T.concat ["<figure><figcaption>", caption, "</figcaption>", svg, "</figure>"]
