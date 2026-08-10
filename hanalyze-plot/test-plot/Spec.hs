{-# LANGUAGE OverloadedStrings #-}
-- | Phase 46 (hgg 統合) の数値・構造テスト。
--
-- 別パッケージ hanalyze-plot の一部として build/run される (= Hanalyze.Plot が必要)。
--   cabal test --project-file=cabal.project.plot hanalyze-plot-test
module Main (main) where

import           Data.Monoid           (First (..), Last (..))
import           Data.Text             (Text)
import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed    as VU
import qualified Numeric.LinearAlgebra as LA
import           Test.Hspec

import           Graphics.Hgg.Spec     (ColorEnc (..), ColRef (..), Layer (..),
                                        MarkKind (..), MarkShape (..), VisualSpec (..),
                                        Annotation (..), CustomMark (..),
                                        ColData (..), LineType (..), fromHex,
                                        DAGSpec (..), DAGNode (..), DAGEdge (..),
                                        DAGNodeKind (..), DAGPlate (..))
import           Graphics.Hgg.Custom.Dendrogram (DendroPayload (..), DendroSeg (..))
import           Data.Aeson            (fromJSON, Result (..))
import qualified Data.Map.Strict       as Map
import qualified System.Random.MWC     as MWC
import           Hanalyze.MCMC.Core    (Chain (..))
import           Hanalyze.Model.PCA    (PCAStandardize (..))
import qualified Hanalyze.Model.PCA    as PCALow
import           Hanalyze.Model.RandomForest (defaultRandomForest, fitRF
                                       , fitRFVPure, predictRF, RandomForest (..))
import           Hanalyze.Model.CompetingRisks (CRSample (..), fitCompetingRisks)
import           Hanalyze.Model.Quantile (QRFit (..))
import           Hanalyze.Model.Survival (Event (..), SurvSample (..),
                                          kaplanMeier)
import           Hanalyze.Model.MultiLM (fitMultiLM)
import           Hanalyze.Model.NeuralNetwork (fitMLPClassifier, fitMLPClassifierPure, defaultMLP, MLPFit (..))
import           Hanalyze.Model.GP (Kernel (..), GPParams (..), defaultGPParams)
import           Hanalyze.Model.Kernel (defaultKernelParams)
import           Hanalyze.Model.Robust (RobustEstimator (..), RobustFit (..),
                                        defaultHuberK, fitRobustLM, robustCovBeta)
import           Hanalyze.Model.Core   (coefficientsV, fittedV, predictAt)
import           Hanalyze.Model.GLM    (Family (..), LinkFn (..))
import           Hanalyze.Model.GP     (GPResult (..))
import           Hanalyze.Model.GAM    (GAMFit (..), fitGAM)
import           Hanalyze.Model.Spline (SplineFit (..), SplineKind (..))
import           Hanalyze.Plot         (GAMModel (..), GLMModel (..),
                                        GAMBasis (..), GAMLambda (..),
                                        GAMConfig (..), defaultGAMConfig, gam, gamMulti,
                                        GAMModelN (..), fitGAMWith,
                                        GPConfig (..), defaultGP, gp, GPMethod (..),
                                        HyperStrategy (..), GPRegModel (..),
                                        gpMulti, GPRegModelN (..),
                                        RegMethod (..), LambdaStrat (..), RegConfig (..),
                                        defaultRidge, defaultLasso, regularized,
                                        ridge, lasso, elasticNet, RegModel (..), regPredict,
                                        fitEither,
                                        Kernel (..), GPParams (..), defaultGPParams,
                                        LMModel (..), QuantileModel (..),
                                        RobustModel (..), SplineModel (..),
                                        chainModel, diagnosticPlots,
                                        forecastModel, gamModel, glmModel,
                                        lmModel, quantileModel, robustModel,
                                        splineModel, toPlot,
                                        Coef (..), modelCoefficients,
                                        predictPoint, describeModel,
                                        statModel, grid, gridRange,
                                        BandMode (..), bandMode,
                                        statColor, statFill, statLinetype,
                                        statLinewidth, statAlpha, statLabel,
                                        statEquation, statR2,
                                        SingleVarModel (..),
                                        predAt,
                                        MultiLMModel (..), multiLMModel,
                                        MultiGLMModel (..), multiGLMModel,
                                        along, statModelMulti,
                                        HoldAgg (..), holdAt, byVar,
                                        plsModel, selectOutput,
                                        SurfaceOpts (..), defaultSurfaceOpts,
                                        surfaceGrid, surfaceOf, surfaceOfWith,
                                        dataScatter3DOf,
                                        epredSurfaceOf, epredSurfaceOfWith,
                                        HBMConfig (..), defaultHBM,
                                        HBMModel (..), hbmModel, hbmModelPure,
                                        hbmModelIO,
                                        ppcOfWithIO,
                                        hbmParamNames, marginalsOf,
                                        TraceOpts (..), defaultTraceOpts,
                                        tracesOf, tracesOfWith,
                                        ForestSpec (..), forestOf,
                                        statLevel, epred, epredAt,
                                        PPCConfig (..), defaultPPC,
                                        PPCSpec (..), ppcOf, ppcOfWith,
                                        DagSpec (..), dagOf, dagOfRaw,
                                        dagOfModel, dagOfModelWith,
                                        dashboardOf, dashboardFullOf, traceDensityOf,
                                        divergencesOf,
                                        pairOf, energyOf, autocorrOf, autocorrOfLag,
                                        rankOf, rankOfBins,
                                        Fit (..), (|->), (|->!),
                                        lm, glm, rq, rlm,
                                        piMethod, PIMethod (..),
                                        lmF, glmF, glmmF,
                                        hbm, dataScatterOf,
                                        grouped, groupModels, groupLabels,
                                        groupedFullrange,
                                        weighted, WeightedLMModel (..),
                                        CoefStats (..), lmDiag, groupedLmDiag,
                                        clusterScatterOf, centroidsOf,
                                        clusterHullOf, clusterEllipseOf,
                                        dendrogramOf, dendrogramOf', defaultDendroOpts, DendroOpts (..),
                                        GBRegressor (..), GBClassifier (..),
                                        RFClassifierFit (..), DTree (..),
                                        treeImportances,
                                        ClassPredict (..), decisionBoundaryOf,
                                        confusionOf, mdsView, mdsGroupBy, mds, defaultMDS, nnLossOf,
                                        mlpCls, mlpReg,
                                        svmCls, defaultSVM,
                                        SVMConfig (..), SVMMulti (..),
                                        numSupportVectors, svmSupportVectorsOf, decisionLineOf,
                                        RegPredict (..), pdp, pdpIce, pdpOf, pdpIceOf, pdpPlot, pdpIcePlot,
                                        partialDependencePlot, partialDependenceIcePlot,
                                        KNNClassifier (..),
                                        PLSFit (..), scoreView, vipView,
                                        MultiGPResult (..), multiGpCurves,
                                        GARCHFit (..), garchVolatility,
                                        AFTFit (..), aftSurvivalAt,
                                        FunctionalPCA (..), FLMResult (..),
                                        RegFit (..), regPathPlot,
                                        DirectLiNGAMFit (..), lingamDag,
                                        directLingam, parceLingam, multiGroupLingam,
                                        varLingam, pairwiseLingam,
                                        bootstrapLingam, icaLingam, bootstrapEdgeProbOf, LiNGAMFitted (..),
                                        correlationOf, CorrelationGraph (..),
                                        factorialDesign, centralCompositeDesign, designTable, designModel, multiOutput,
                                        contFactor, catFactor,
                                        profiler, profilerResidual, ProfilerSpec (..), ResidualMode (..), contourOf,
                                        TestResult (..), testForest,
                                        testForestLabeled, describeBox,
                                        pca, pls, lda, ccaOf, CCAFit (..),
                                        gbmReg, gbmCls, defaultGBM,
                                        decisionTree, defaultDecisionTree, knnCls, knnReg, naiveBayes,
                                        kmeans, randomForestReg, randomForestCls,
                                        lmMulti, glmMulti, rlmMulti,
                                        CoefRow (..), coefSummary,
                                        obsVsPred, obsPredPairs, coefForest,
                                        MultiRobustModel (..),
                                        standardized, standardizedY,
                                        StandardizedModel (..),
                                        predictorCols, responseCol,
                                        rqMulti, MultiQuantileModel (..))
import           Hanalyze.Model.Multivariate (cca)
import           Control.Exception (evaluate)
import           Hanalyze.Stat.Test (Alternative (..))
import           Hanalyze.Model.AFT (AFTDistribution (..))
import           Hanalyze.Model.Regularized (Penalty (NoPen))
import           Hanalyze.Model.Cluster (KMeansResult (..)
                                       , KMeansConfig (..), defaultKMeans, kMeansPure, kMeans)
import           Hanalyze.Model.HierarchicalCluster (fitHierarchical, Linkage (..), HClusterFit (..))
import           Hanalyze.Model.LiNGAM.Direct (fitDirectLiNGAM, defaultDirectLiNGAMConfig)
import           Hanalyze.Model.LiNGAM.Parce (defaultParceConfig)
import           Hanalyze.Model.LiNGAM.MultiGroup (defaultMultiGroupConfig)
import           Hanalyze.Model.LiNGAM.VAR (defaultVARLiNGAMConfig)
import           Hanalyze.Model.LiNGAM.Bootstrap (defaultBootstrapConfig, BootstrapConfig (..)
                                       , BootstrapResult (..), fitBootstrapLiNGAM, fitBootstrapLiNGAMPure)
import           Hanalyze.Model.LiNGAM.ICA (defaultICALiNGAMConfig, ICALiNGAMFit (..)
                                       , fitICALiNGAM, fitICALiNGAMPure)
import qualified Hanalyze.Model.RandomForest as RF
import           Hanalyze.Model.RandomForestClassifier (defaultRFCConfig, fitRFClassifierPure)
import           Hanalyze.Model.PLS (fitPLS, defaultPLS, PLSConfig (..))
import           Hanalyze.Model.KNN (predictKNNR, fitKNNR)
import           Hanalyze.Stat.Standardize (Standardizer (..), fitStandardizer, applyStandardizer)
import qualified Graphics.Hgg.ThreeD.Spec     as P3
import           Graphics.Hgg.ThreeD.Types    (Point3 (..))
import           Hanalyze.Model.HBM    ( Distribution (Normal, HalfNormal)
                                       , sample, dataNamedX, dataNamedObs, dataNamedIx, (!!!)
                                       , deterministic
                                       , observeColumns, observe, withData
                                       , plate, ModelP )
import           Hanalyze.Data.ColumnSource (ColumnSource (lookupCol))
import           Control.Monad         (forM_)
import qualified Data.Text             as T
import           Hanalyze.MCMC.Core    (chainVals)
import qualified DataFrame.Internal.Column    as DX
import qualified DataFrame.Internal.DataFrame  as DX
-- Phase 106.4: WorkflowSpec (umbrella test) から移行した plot 連携診断テスト用。
import           Data.Maybe            (isJust)
import           Hanalyze.Fit   (designModelHBM, DesignHBMFit (..), ranIntercept)
import           Hanalyze.Model.Formula.Frame (ModelFrame (..), VarRole (..))
import           Hanalyze.Plot.Core (MultiVarModel (..))
import           Hanalyze.Plot.ML ()  -- instance MultiVarModel DesignHBMFit

-- y = 2x + 1 (完全線形) を入れて係数を検証する。
xs, ys :: LA.Vector Double
xs = LA.fromList [1, 2, 3, 4, 5]
ys = LA.fromList [3, 5, 7, 9, 11]

m :: LMModel
m = lmModel xs ys

allClose :: [Double] -> [Double] -> Bool
allClose a b = length a == length b && and (zipWith (\x y -> abs (x - y) < 1e-9) a b)

-- Phase 49 A1: 線形 HBM (y ~ Normal(a + b·x, s))。 data は placeholder ([]) で書き、
-- hbmModel が列名で withData 自動 bind する (PyMC set_data 同型)。
hbmLinModel :: ModelP ()
hbmLinModel = do
  x <- dataNamedX "x" []
  y <- dataNamedObs "y" []
  a <- sample "a" (Normal 0 10)
  b <- sample "b" (Normal 0 10)
  s <- sample "s" (HalfNormal 1)
  observeColumns "obs"
    [ (Normal (a + b * xi) s, [yi]) | (xi, yi) <- zip x y ]

posteriorMeanOf :: Text -> [Chain] -> Double
posteriorMeanOf name chains =
  let vals = concatMap (chainVals name) chains
  in sum vals / fromIntegral (length vals)

-- Phase 49 A3: epred 用 O1 規約モデル。 学習 likelihood は per-point inline mu のまま、
-- 平均 μ を deterministic "mu" として 1 点スカラで併存公開する。 epred は grid を
-- withData "x" [xi] で 1 点に差し替えて mu を読む (head x = xi)。 訓練時は x が full data
-- ゆえ head x は安全 (deterministic 値は thunk で構築時に head [] を踏まない)。
hbmEpredModel :: ModelP ()
hbmEpredModel = do
  x <- dataNamedX "x" []
  y <- dataNamedObs "y" []
  a <- sample "a" (Normal 0 10)
  b <- sample "b" (Normal 0 10)
  s <- sample "s" (HalfNormal 1)
  _ <- deterministic "mu" (a + b * head x)
  observeColumns "obs"
    [ (Normal (a + b * xi) s, [yi]) | (xi, yi) <- zip x y ]

-- plot Phase 24 A3: epred 応答曲面用の 2 予測子 O1 規約モデル。
hbmEpred2Model :: ModelP ()
hbmEpred2Model = do
  x1 <- dataNamedX "x1" []
  x2 <- dataNamedX "x2" []
  y  <- dataNamedObs "y" []
  a  <- sample "a" (Normal 0 10)
  b  <- sample "b" (Normal 0 10)
  c  <- sample "c" (Normal 0 10)
  s  <- sample "s" (HalfNormal 1)
  _  <- deterministic "mu" (a + b * head x1 + c * head x2)
  observeColumns "obs"
    [ (Normal (a + b * xi + c * zi) s, [yi])
    | (xi, (zi, yi)) <- zip x1 (zip x2 y) ]

-- Phase 60.3: dataNamedIx で群 index を受ける 2 群モデル (factor 自動コード化の検証)。
-- mus !! g に round が消えるのが新 DSL の眼目。 sort 順 levels なら code 0 = "A"。
hbmIxModel :: ModelP ()
hbmIxModel = do
  gs <- dataNamedIx  "g" []
  y  <- dataNamedObs "y" []
  mu0 <- sample "mu0" (Normal 0 5)
  mu1 <- sample "mu1" (Normal 0 5)
  s   <- sample "s" (HalfNormal 1)
  let mus = [mu0, mu1]
  observeColumns "obs" [ (Normal (mus !!! g) s, [yi]) | (g, yi) <- zip gs y ]

-- Phase 49 A5: plate を使う階層モデル (dagOf の DAGPlate 変換検証用)。 group g の
-- 各メンバ eta_j を plate "g" 4 で囲い、 mu/tau は plate 外に置く (8-schools 風)。
hbmPlateModel :: ModelP ()
hbmPlateModel = do
  mu  <- sample "mu"  (Normal 0 5)
  tau <- sample "tau" (HalfNormal 5)
  _ <- plate "g" 4 $ forM_ [0 .. 3 :: Int] $ \j -> do
    eta <- sample ("eta_" <> T.pack (show j)) (Normal 0 1)
    observe ("y_" <> T.pack (show j)) (Normal (mu + tau * eta) 1) [realToFrac j]
  pure ()

main :: IO ()
main = hspec $ do
  describe "Phase 46 A4: LMModel + toPlot" $ do

    it "係数 ≈ [1, 2] (intercept, slope) = fitLM 直計算と一致" $ do
      let cs = LA.toList (coefficientsV (lmResult m))
      cs `shouldSatisfy` allClose [1, 2]

    it "predictAt (lmResult) X == fitted (PredictiveModel 整合)" $ do
      let yhat = LA.toList (LA.flatten (predictAt (lmResult m) (lmDesign m)))
      yhat `shouldSatisfy` allClose (LA.toList (fittedV (lmResult m)))

    it "toPlot は band + line の 2 layer を inline encY 付きで返す" $ do
      let ls = vsLayers (toPlot m)
      length ls `shouldBe` 2
      getFirst (lyKind (ls !! 0)) `shouldBe` Just MBand
      getFirst (lyKind (ls !! 1)) `shouldBe` Just MLine
      case getLast (lyEncY (ls !! 1)) of
        Just (ColNum v) -> V.length v `shouldBe` 5
        _               -> expectationFailure "line encY が inline ColNum でない"

    it "line layer の inline encY ≈ 予測 ŷ = Xβ (回帰線が fit と一致)" $ do
      let l = vsLayers (toPlot m) !! 1                          -- line layer
          yhatFit = LA.toList (fittedV (lmResult m))           -- x 昇順入力ゆえ既に整列
      case getLast (lyEncY l) of
        Just (ColNum v) -> V.toList v `shouldSatisfy` allClose yhatFit
        _               -> expectationFailure "line encY が inline ColNum でない"

    it "CI band 半幅 (encY2 − encY) は全点 ≥ 0" $ do
      let b = head (vsLayers (toPlot m))                        -- band layer
      case (getLast (lyEncY b), getLast (lyEncY2 b)) of
        (Just (ColNum vlo), Just (ColNum vhi)) ->
          zipWith (-) (V.toList vhi) (V.toList vlo) `shouldSatisfy` all (>= 0)
        _ -> expectationFailure "band encY/encY2 が inline ColNum でない"

  describe "Phase 70.A: df |-> pca / pls (行列入力モデルの高レベル化)" $ do
    -- 3 列の df と、 同じ並びの行列。 列名 spec が低レベル行列 fit と一致するか。
    let c1 = [ 5   * sin (fromIntegral i * 0.3) | i <- [1 .. 30 :: Int] ]
        c2 = [ 1.2 * cos (fromIntegral i * 0.5) | i <- [1 .. 30 :: Int] ]
        c3 = [ 0.3 * sin (fromIntegral i)       | i <- [1 .. 30 :: Int] ]
        df = [ ("x1", NumData (V.fromList c1))
             , ("x2", NumData (V.fromList c2))
             , ("x3", NumData (V.fromList c3)) ] :: [(Text, ColData)]
        xmat = LA.fromColumns (map LA.fromList [c1, c2, c3])
        encY r = case getLast (lyEncY (head (vsLayers (toPlot r)))) of
                   Just (ColNum v) -> V.toList v
                   _               -> []

    it "df |-> pca == 低レベル pca (toPlot 寄与率が一致)" $ do
      let resHi = df |-> pca CenterScale Nothing ["x1", "x2", "x3"]
          resLo = PCALow.pca CenterScale Nothing xmat
      encY resHi `shouldSatisfy` allClose (encY resLo)

    it "df |-> pls == 低レベル fitPLS (回帰係数 plsBeta が一致)" $ do
      let y    = [ 2 * a + 0.5 * b | (a, b) <- zip c1 c2 ]
          df'  = df ++ [ ("y", NumData (V.fromList y)) ] :: [(Text, ColData)]
          ymat = LA.fromColumns [LA.fromList y]
          mHi  = df' |-> pls defaultPLS ["x1", "x2"] ["y"]
      case fitPLS defaultPLS (LA.fromColumns (map LA.fromList [c1, c2])) ymat of
        Right mLo -> concat (LA.toLists (plsCoef mHi))
                       `shouldSatisfy` allClose (concat (LA.toLists (plsCoef mLo)))
        Left e    -> expectationFailure (T.unpack e)

    it "df |-> ccaOf == 低レベル cca (正準相関 ccaCorr が一致)" $ do
      let y1 = [ a + 0.1 * b | (a, b) <- zip c1 c2 ]
          y2 = [ b - 0.2 * a | (a, b) <- zip c1 c2 ]
          df' = df ++ [ ("y1", NumData (V.fromList y1))
                      , ("y2", NumData (V.fromList y2)) ] :: [(Text, ColData)]
          mHi = df' |-> ccaOf ["x1", "x2"] ["y1", "y2"]
          mLo = cca (LA.fromColumns (map LA.fromList [c1, c2]))
                    (LA.fromColumns (map LA.fromList [y1, y2]))
      LA.toList (ccaCorr mHi) `shouldSatisfy` allClose (LA.toList (ccaCorr mLo))

    it "df |-> lda は DiscriminantFit を当て描画可能 (クラス列の整数化が効く)" $ do
      let cls = [ if a > 0 then 1 else 0 | a <- c1 ] :: [Int]   -- 2 クラス
          df' = df ++ [ ("cls", NumData (V.fromList (map fromIntegral cls))) ]
                  :: [(Text, ColData)]
          m   = df' |-> lda ["x1", "x2"] "cls"
      length (vsLayers (toPlot m)) `shouldSatisfy` (> 0)

    -- 教師あり ML 分類器/回帰器 (純粋 fit)。 ラベル/応答列を足した df で fit→描画可。
    let yreg = [ 2 * a + 0.5 * b | (a, b) <- zip c1 c2 ]
        cls2 = [ fromIntegral (if a > 0 then 1 else 0 :: Int) | a <- c1 ] :: [Double]
        dfML = df ++ [ ("y", NumData (V.fromList yreg))
                     , ("cls", NumData (V.fromList cls2)) ] :: [(Text, ColData)]
    it "df |-> gbmReg / decisionTree / knnCls / naiveBayes が当てて描画可 (toPlot レイヤ > 0)" $ do
      let mGB = dfML |-> gbmReg defaultGBM ["x1", "x2"] "y"
          mDT = dfML |-> decisionTree defaultDecisionTree ["x1", "x2"] "cls"
          mKN = dfML |-> knnCls 3 ["x1", "x2"] "cls"
          mNB = dfML |-> naiveBayes ["x1", "x2"] "cls"
      length (vsLayers (toPlot mGB)) `shouldSatisfy` (> 0)
      length (vsLayers (toPlot mDT)) `shouldSatisfy` (> 0)
      length (vsLayers (toPlot mKN)) `shouldSatisfy` (> 0)
      length (vsLayers (toPlot mNB)) `shouldSatisfy` (> 0)
    it "df |-> gbmCls / knnReg は error なく当たる (fitEither が Right)" $ do
      let mGBC = dfML |-> gbmCls defaultGBM ["x1", "x2"] "cls"
          mKNR = dfML |-> knnReg 3 ["x1", "x2"] "y"
      _ <- evaluate (length (vsLayers (toPlot mGBC)))   -- GBClassifier は Plottable
      _ <- evaluate mKNR                                 -- KNNRegressor は WHNF へ強制
      pure ()

  describe "Phase 76: mark 拡充 (決定領域塗り / クラスタ囲み / dendrogram) の primitive" $ do
    let isAnnLine a = case a of AnnLine{} -> True; _ -> False
        -- Phase 48: dendrogram の U 字リンクは custom mark の焼き込み payload に載る。
        -- 先頭 layer の lyCustom (Last CustomMark) → cmOptions (JSON) を DendroPayload へ
        -- decode し、 その線分列 (dpSegments) を取り出す。
        dendroSegs vs = case vsLayers vs of
          (ly:_) -> case getLast (lyCustom ly) of
            Just cm -> case fromJSON (cmOptions cm) of
              Success p -> dpSegments p
              _         -> []
            Nothing -> []
          _ -> []
        -- 2 群 (各 3 点・非共線の三角形) の決定的データ + KMeansResult。
        hdf  = [ ("x", NumData (V.fromList [0, 1, 0.5, 5, 6, 5.5]))
               , ("y", NumData (V.fromList [0, 0, 1.0, 5, 5, 6.0])) ] :: [(Text, ColData)]
        kres = KMeansResult
                 { kmrCentroids = LA.fromLists [[0.5, 0.33], [5.5, 5.33]]
                 , kmrLabels    = [0, 0, 0, 1, 1, 1]
                 , kmrInertia   = 0, kmrIters = 1, kmrConverged = True }

    it "clusterHullOf: 各群の凸包 = 三角形 (3 点) → 群ごと 3 辺・全て AnnLine・layer 無し" $ do
      let vs = clusterHullOf hdf kres "x" "y"
      vsLayers vs `shouldBe` []
      length (vsAnnotations vs) `shouldBe` 6            -- 2 群 × 3 辺
      all isAnnLine (vsAnnotations vs) `shouldBe` True

    it "clusterHullOf: 列が無ければ空" $
      clusterHullOf hdf kres "nope" "y" `shouldBe` mempty

    it "clusterEllipseOf: 群ごと 64 辺の楕円折れ線 + 不可視 anchor layer 1" $ do
      let vs = clusterEllipseOf hdf kres "x" "y"
      length (vsAnnotations vs) `shouldBe` 128          -- 2 群 × 64 辺
      all isAnnLine (vsAnnotations vs) `shouldBe` True
      length (vsLayers vs) `shouldBe` 1                 -- 軸 auto-fit 用の alpha=0 散布

    it "dendrogramOf: U 字リンク 3*(n-1) 本 (custom mark 焼き込み) + y 軸線 1 本 (AnnLine)" $ do
      let xm = LA.fromLists [[0,0],[0.2,0.1],[5,5],[5.1,4.9]]   -- n=4・2 群
          hc = fitHierarchical Ward xm
          vs = dendrogramOf hc
          n  = 4 :: Int
      -- Phase 48: U 字リンクは 1 layer (MCustom) に焼き込み・segments = 3*(n-1) 本。
      length (vsLayers vs) `shouldBe` 1
      getFirst (lyKind (head (vsLayers vs))) `shouldBe` Just MCustom
      length (dendroSegs vs) `shouldBe` 3 * (n - 1)
      -- annotation は y 軸線 1 本のみ。
      length (vsAnnotations vs) `shouldBe` 1
      all isAnnLine (vsAnnotations vs) `shouldBe` True

    it "dendrogramOf': 色閾値で葉クラスタが色分け (閾値超は既定線色・複数色出る)" $ do
      let xm = LA.fromLists [[0,0],[0.2,0.1],[5,5],[5.1,4.9]]
          hc = fitHierarchical Ward xm
          hs = hcHeights hc
          thr = (hs !! (length hs - 2) + hs !! (length hs - 1)) / 2
          vs = dendrogramOf' defaultDendroOpts { doColorThreshold = Just thr } hc
          cols = map segColor (dendroSegs vs)             -- 焼き込み線分の色
      any (/= head cols) cols `shouldBe` True           -- 群色 + 閾値超色 = 2 色以上

    -- Phase 76.D: PDP を HBM 同型の Plottable 中間型 (toPlot) に。 pdpOf と同一 primitive。
    it "toPlot (pdp …) == pdpOf (同一 layer 数)" $ do
      let pdf = [ ("a", NumData (V.fromList [0,1,2,3,4,5,6,7]))
                , ("b", NumData (V.fromList [1,0,1,0,1,0,1,0])) ] :: [(Text, ColData)]
          xm  = LA.fromColumns [ LA.fromList [0,1,2,3,4,5,6,7], LA.fromList [1,0,1,0,1,0,1,0] ]
          yv  = VU.fromList [1,2,3,4,5,6,7,8 :: Double]
          rf  = fitRFVPure defaultRandomForest xm yv 7
          viaView   = toPlot (pdp rf pdf ["a","b"] "a")
          viaDirect = pdpOf rf pdf ["a","b"] "a"
      length (vsLayers viaView) `shouldBe` length (vsLayers viaDirect)
      length (vsLayers viaView) `shouldSatisfy` (> 0)

    it "toPlot (pdpIce …) は ICE 曲線 + 平均で pdp より layer が多い" $ do
      let pdf = [ ("a", NumData (V.fromList [0,1,2,3,4,5,6,7]))
                , ("b", NumData (V.fromList [1,0,1,0,1,0,1,0])) ] :: [(Text, ColData)]
          xm  = LA.fromColumns [ LA.fromList [0,1,2,3,4,5,6,7], LA.fromList [1,0,1,0,1,0,1,0] ]
          yv  = VU.fromList [1,2,3,4,5,6,7,8 :: Double]
          rf  = fitRFVPure defaultRandomForest xm yv 7
          nP  = length (vsLayers (toPlot (pdp    rf pdf ["a","b"] "a")))
          nI  = length (vsLayers (toPlot (pdpIce rf pdf ["a","b"] "a")))
      nI `shouldSatisfy` (> nP)

  describe "Phase 70.A: KMeans / RandomForest の seed 純粋化 (df |-> + 決定性)" $ do
    -- 2 クラスタ (中心 (1,1) と (5,5)・各 20 点) の決定的データ。
    let pts  = [ (1 + 0.1 * sin (fromIntegral i), 1 + 0.1 * cos (fromIntegral i))
               | i <- [1 .. 20 :: Int] ]
            ++ [ (5 + 0.1 * sin (fromIntegral i), 5 + 0.1 * cos (fromIntegral i))
               | i <- [1 .. 20 :: Int] ]
        as    = map fst pts
        bs    = map snd pts
        xmatK = LA.fromColumns [LA.fromList as, LA.fromList bs]
        dfK   = [ ("a", NumData (V.fromList as))
                , ("b", NumData (V.fromList bs)) ] :: [(Text, ColData)]
        cfgK  = defaultKMeans 2

    it "kMeansPure は決定的 (同 seed → inertia/centroids/labels がビット一致)" $ do
      let r1 = kMeansPure cfgK xmatK 42
          r2 = kMeansPure cfgK xmatK 42
      kmrInertia r1 `shouldBe` kmrInertia r2
      LA.toList (LA.flatten (kmrCentroids r1))
        `shouldBe` LA.toList (LA.flatten (kmrCentroids r2))
      kmrLabels r1 `shouldBe` kmrLabels r2

    it "kMeansPure seed==42 は IO kMeans(initialize 42) とビット一致 (ST/IO 同コード)" $ do
      gen <- MWC.initialize (V.singleton 42)
      rIO <- kMeans cfgK xmatK gen
      let rST = kMeansPure cfgK xmatK 42
      kmrInertia rIO `shouldBe` kmrInertia rST
      kmrLabels  rIO `shouldBe` kmrLabels  rST
      LA.toList (LA.flatten (kmrCentroids rIO))
        `shouldBe` LA.toList (LA.flatten (kmrCentroids rST))

    it "df |-> kmeans == kMeansPure (列名経路と行列経路が一致・描画可)" $ do
      let r = dfK |-> kmeans cfgK 42 ["a", "b"]
      kmrInertia r `shouldBe` kmrInertia (kMeansPure cfgK xmatK 42)
      length (vsLayers (toPlot r)) `shouldSatisfy` (> 0)

    -- RandomForest 回帰: y = 2a + 3b。
    let yR    = [ 2 * a + 3 * b | (a, b) <- pts ]
        yvR   = VU.fromList yR
        dfR   = dfK ++ [ ("y", NumData (V.fromList yR)) ] :: [(Text, ColData)]
        cfgR  = defaultRandomForest

    it "fitRFVPure は決定的 (同 seed → importance/予測がビット一致)" $ do
      let f1 = fitRFVPure cfgR xmatK yvR 7
          f2 = fitRFVPure cfgR xmatK yvR 7
      V.toList (rfImportance f1) `shouldBe` V.toList (rfImportance f2)
      predictRF f1 [1, 1] `shouldBe` predictRF f2 [1, 1]

    it "df |-> randomForestReg == fitRFVPure (同 seed・特徴重要度バー描画可)" $ do
      let r = dfR |-> randomForestReg cfgR 7 ["a", "b"] "y"
      V.toList (rfImportance r)
        `shouldBe` V.toList (rfImportance (fitRFVPure cfgR xmatK yvR 7))
      -- toPlot は 2 パネル (impurity + permutation) の subplots (75.24)。
      length (vsSubplots (toPlot r)) `shouldBe` 2

    -- RandomForest 分類 (75.24d): 2 クラスタ → クラス 0/1。df|-> で実列名・決定的。
    let clsC = replicate 20 0 ++ replicate 20 1 :: [Int]
        dfC  = dfK ++ [ ("cls", NumData (V.fromList (map fromIntegral clsC))) ] :: [(Text, ColData)]

    it "df |-> randomForestCls == fitRFClassifierPure (同 seed・実列名・2 パネル)" $ do
      let r   = dfC |-> randomForestCls defaultRFCConfig 7 ["a", "b"] "cls"
          ref = fitRFClassifierPure defaultRFCConfig xmatK (VU.fromList clsC) 7
      -- 決定性: gini/permutation がビット一致 (df|-> = 行列経路)
      LA.toList (rfcGiniImportance r) `shouldBe` LA.toList (rfcGiniImportance ref)
      LA.toList (rfcImportance r)     `shouldBe` LA.toList (rfcImportance ref)
      -- df|-> は実列名を載せる (行列経路の f1.. でなく)
      rfcFeatureNames r `shouldBe` ["a", "b"]
      length (vsSubplots (toPlot r)) `shouldBe` 2

  describe "Phase 70.D: 重回帰 統一 API (lmMulti/glmMulti/robustMulti + coefSummary)" $ do
    -- 固定データ (12 行・x1,x2,x3)。 末尾 y=40 が外れ値 (ロバストの効きを見る)。
    -- 期待値は statsmodels 0.14.6 で生成 (experiments/phase-70d-coefsummary/ref_statsmodels.py)。
    let x1 = [1,2,3,4,5,6,7,8,9,10,11,12] :: [Double]
        x2 = [2,1,4,3,6,5,8,7,10,9,12,11] :: [Double]
        x3 = [0.5,1.5,1,2.5,2,3.5,3,4.5,4,5.5,5,6.5] :: [Double]
        yv = [3.1,4,7.2,8.1,11,12.3,15.1,16,19.2,20.1,23,40] :: [Double]
        dfM = [ ("x1", NumData (V.fromList x1)), ("x2", NumData (V.fromList x2))
              , ("x3", NumData (V.fromList x3)), ("y",  NumData (V.fromList yv)) ]
              :: [(Text, ColData)]
        near a b = abs (a - b) < 1e-6
        nearV xs ys = and (zipWith near xs ys)
        -- GLM は IRLS 収束点が statsmodels とごく僅か異なり SE が ~1e-5 ずれる
        -- (β は 1e-9 一致。 独立実装間として 5 桁一致は良好)。 GLM のみ緩い許容。
        nearV4 xs ys = and (zipWith (\a b -> abs (a - b) < 1e-4) xs ys)
        rowsOf m = ( map crEstimate m, map crStdErr m, map crStat m
                   , map crPValue m, concatMap (\r -> [fst (crCI95 r), snd (crCI95 r)]) m )

    it "lmMulti coefSummary (t) == statsmodels OLS .summary()" $ do
      let m  = dfM |-> lmMulti ["x1", "x2", "x3"] "y" :: MultiLMModel
          cs = coefSummary m
          (est, se, tv, pv, ci) = rowsOf cs
      -- 名前 = (Intercept) + 列名
      map crTerm cs `shouldBe` ["(Intercept)", "x1", "x2", "x3"]
      nearV est [-3.121363636363598,-5.105000000000107,3.5004545454546294,8.650909090909133]
        `shouldBe` True
      nearV se  [3.2114157547937565,10.36170010985987,5.21966108203359,10.83050421206717]
        `shouldBe` True
      nearV tv  [-0.97195874800834,-0.49267976740055897,0.6706287037492568,0.798754048890026]
        `shouldBe` True
      nearV pv  [0.35953762982419396,0.6354752397806847,0.5213433571725514,0.4474952370377395]
        `shouldBe` True
      nearV ci  [-10.526901646229312,4.284174373502117,-28.999123299312696,18.789123299312482
                ,-8.536105493187584,15.537014584096845,-16.3242784066141,33.62609658843236]
        `shouldBe` True

    it "robustMulti coefSummary (z, Huber 1.345) == statsmodels RLM (cov=H1)" $ do
      let m  = dfM |-> rlmMulti (Huber 1.345) ["x1", "x2", "x3"] "y" :: MultiRobustModel
          cs = coefSummary m
          (est, se, zv, pv, ci) = rowsOf cs
      map crTerm cs `shouldBe` ["(Intercept)", "x1", "x2", "x3"]
      nearV est [0.5497573872869811,1.4082792355091573,0.5398730747685647,0.12443445533346953]
        `shouldBe` True
      nearV se  [0.1120721724104495,0.3616032086297613,0.18215603377936052,0.3779635612533277]
        `shouldBe` True
      nearV zv  [4.905387086399712,3.8945429739011743,2.9637946301712677,0.3292234175189923]
        `shouldBe` True
      nearV pv  [9.324326853300844e-7,9.838404940536295e-5,0.0030387101129149083,0.7419868240281127]
        `shouldBe` True
      nearV ci  [0.33009996569333655,0.7694148088806256,0.699549969900702,2.1170085011176125
                ,0.1828538089943566,0.8968923405427729,-0.6163605121915514,0.8652294228584905]
        `shouldBe` True

    -- GLM は scale=1 の族 (Poisson/Binomial) が本来用途。 z 経路 (正規) で statsmodels
    -- GLM と一致する (Gaussian は分散スケール推定が入り別経路 → 連続応答は lmMulti を使う)。
    it "glmMulti Poisson/log coefSummary (z) == statsmodels GLM Poisson" $ do
      let yc  = [1,2,2,4,5,7,10,14,19,26,35,48] :: [Double]
          dfP = [ ("x1", NumData (V.fromList x1)), ("x2", NumData (V.fromList x2))
                , ("x3", NumData (V.fromList x3)), ("yc", NumData (V.fromList yc)) ]
                :: [(Text, ColData)]
          cs = coefSummary (dfP |-> glmMulti Poisson Log ["x1", "x2", "x3"] "yc")
          (est, se, zv, pv, ci) = rowsOf cs
      nearV  est [0.03218678346706216,1.0213360876134985,-0.35313387540543173,-0.6945102404086394]
        `shouldBe` True
      nearV4 se  [0.31691453822410903,2.071111308008677,1.0400997867517308,2.081045612659335]
        `shouldBe` True
      nearV4 zv  [0.1015629754552345,0.4931343301850291,-0.3395192268121519,-0.33373138780996536]
        `shouldBe` True
      nearV4 pv  [0.9191035687400753,0.6219176751774244,0.7342186154504002,0.7385822621359937]
        `shouldBe` True
      nearV4 ci  [-0.5889542976293338,0.6533278645634581,-3.037967484057151,5.080639659284148
                ,-2.3916919977666145,1.6854242469557508,-4.773284691406028,3.3842642105887486]
        `shouldBe` True

    it "lmMulti は effect plot (statModelMulti + along) が即使える" $ do
      let m = dfM |-> lmMulti ["x1", "x2", "x3"] "y" :: MultiLMModel
          spec = statModelMulti m (along "x1") <> holdAt Mean
      length (vsLayers (toPlot spec)) `shouldSatisfy` (> 0)

    it "robustMulti も effect plot (band + line) が即使える" $ do
      let m = dfM |-> rlmMulti (Huber 1.345) ["x1", "x2", "x3"] "y" :: MultiRobustModel
          spec = statModelMulti m (along "x1") <> holdAt Mean
      length (vsLayers (toPlot spec)) `shouldSatisfy` (>= 2)

  describe "Phase 46 A8: GLMModel + toPlot (非対称 μ-CI 帯)" $ do
    -- Poisson 回帰 (log link)。 count が単調増加するデータ。
    let gxs = LA.fromList [1, 2, 3, 4, 5, 6]
        gys = LA.fromList [1, 2, 4, 7, 12, 20]
        gm  = glmModel Poisson Log gxs gys
        layersOf = vsLayers (toPlot gm)
        bandL = head [ l | l <- layersOf, getFirst (lyKind l) == Just MBand ]
        lineL = head [ l | l <- layersOf, getFirst (lyKind l) == Just MLine ]
        numOf f l = case getLast (f l) of
          Just (ColNum v) -> V.toList v
          _               -> error "encoding が inline ColNum でない"

    it "toPlot は band (MBand) + μ 線 (MLine) の 2 layer を返す" $ do
      length layersOf `shouldBe` 2
      map (getFirst . lyKind) layersOf `shouldMatchList`
        [Just MBand, Just MLine]

    it "band の x は昇順、 点数は n=6" $ do
      let xb = numOf lyEncX bandL
      length xb `shouldBe` 6
      xb `shouldSatisfy` \v -> and (zipWith (<=) v (drop 1 v))

    it "band 上境界 (encY2) ≥ 下境界 (encY) = 帯が潰れない" $ do
      let lo = numOf lyEncY  bandL
          hi = numOf lyEncY2 bandL
      and (zipWith (<=) lo hi) `shouldBe` True

    it "μ 線 (MLine encY) は band の [lo, hi] 内 = 中心が帯内" $ do
      let lo = numOf lyEncY  bandL
          hi = numOf lyEncY2 bandL
          mu = numOf lyEncY  lineL
      and (zipWith3 (\l u c -> l - 1e-9 <= c && c <= u + 1e-9) lo hi mu)
        `shouldBe` True

    it "μ 線は fitted μ̂ と一致 (= predictGlmMuWithCI が fit と整合)" $ do
      let mu = numOf lyEncY lineL
      mu `shouldSatisfy` allClose (LA.toList (fittedV (glmResult gm)))

  describe "Phase 46 A6: GPResult + toPlot (protocol 汎用性)" $ do
    -- 予測 grid をわざと降順にして、 toPlot がソートして折れ線を作ることを検証。
    -- 値は手組み (GP 数値計算に非依存・決定的): mean = grid, band 半幅 = 0.5。
    let gres = GPResult
          { gpTestX = [3, 1, 2]
          , gpMean  = [30, 10, 20]
          , gpVar   = [0.25, 0.25, 0.25]
          , gpLower = [29.5, 9.5, 19.5]
          , gpUpper = [30.5, 10.5, 20.5]
          }

    it "toPlot は band + line の 2 layer を返す (FitResult 系と別型でも成立)" $ do
      let ls = vsLayers (toPlot gres)
      length ls `shouldBe` 2
      getFirst (lyKind (ls !! 0)) `shouldBe` Just MBand
      getFirst (lyKind (ls !! 1)) `shouldBe` Just MLine

    it "line の encX/encY は x 昇順にソートされる (= 折れ線が交差しない)" $ do
      let l = vsLayers (toPlot gres) !! 1                        -- line layer
      case (getLast (lyEncX l), getLast (lyEncY l)) of
        (Just (ColNum vx), Just (ColNum vy)) -> do
          V.toList vx `shouldSatisfy` allClose [1, 2, 3]
          V.toList vy `shouldSatisfy` allClose [10, 20, 30]
        _ -> expectationFailure "encX/encY が inline ColNum でない"

    it "band は gpMean ± (gpUpper − gpMean) = [low, high] を encY/encY2 に持つ" $ do
      let b = head (vsLayers (toPlot gres))                      -- band layer
      case (getLast (lyEncY b), getLast (lyEncY2 b)) of
        (Just (ColNum vlo), Just (ColNum vhi)) -> do
          V.toList vlo `shouldSatisfy` allClose [9.5, 19.5, 29.5]
          V.toList vhi `shouldSatisfy` allClose [10.5, 20.5, 30.5]
        _ -> expectationFailure "band encY/encY2 が inline ColNum でない"

  describe "Phase 46 A9: SplineModel + toPlot (平滑曲線 + 対称 CI band)" $ do
    -- なめらかな非線形データ。 x は意図的に降順で渡し、 toPlot が昇順整列することも確認。
    let sxs = LA.fromList [6, 5, 4, 3, 2, 1, 0]
        sys = LA.fromList [36, 25, 16, 9, 4, 1, 0]  -- y = x²
        sm  = splineModel (BSpline 3) [0, 2, 4, 6] sxs sys
        sBand = head (vsLayers (toPlot sm))           -- band layer (CI)
        sLine = vsLayers (toPlot sm) !! 1             -- line layer (ŷ 曲線)
        numBand f = case getLast (f sBand) of
          Just (ColNum v) -> V.toList v
          _               -> error "band encoding が inline ColNum でない"
        numLine f = case getLast (f sLine) of
          Just (ColNum v) -> V.toList v
          _               -> error "line encoding が inline ColNum でない"

    it "toPlot は band + line の 2 layer を返す (曲線 + band)" $ do
      let ls = vsLayers (toPlot sm)
      length ls `shouldBe` 2
      getFirst (lyKind sBand) `shouldBe` Just MBand
      getFirst (lyKind sLine) `shouldBe` Just MLine

    it "encX は x 昇順にソートされる (= 平滑曲線が交差しない)" $ do
      let xb = numLine lyEncX
      and (zipWith (<=) xb (drop 1 xb)) `shouldBe` True

    it "line encY (ŷ) は fitted と一致 (= 基底空間 fit と整合)" $ do
      -- fittedV は入力順 (降順)、 encY は昇順整列ゆえ reverse して突合。
      let yhatFit = reverse (LA.toList (fittedV (sfResult (splFit sm))))
      numLine lyEncY `shouldSatisfy` allClose yhatFit

    it "CI band 半幅 (encY2 − encY) は全点 ≥ 0 (基底空間 Wald CI)" $ do
      zipWith (-) (numBand lyEncY2) (numBand lyEncY) `shouldSatisfy` all (>= 0)

    it "Phase 70.G: svGridPI (PI) ⊃ svGrid (CI) — 基底空間 closed-form PI" $ do
      let gx = [1.0, 3.0, 5.0]
      case (svGrid sm 0.95 gx, svGridPI sm 0.95 gx) of
        ((_, Just (clo, chi)), Just (plo, phi)) -> do
          and (zipWith (<=) plo clo) `shouldBe` True   -- PI 下限 ≤ CI 下限
          and (zipWith (>=) phi chi) `shouldBe` True   -- PI 上限 ≥ CI 上限
        _ -> expectationFailure "spline の CI/PI が出ない"

    it "Phase 70.G: bandMode BandCIPI で CI+PI 入れ子 (MBand 2 + MLine 1)" $ do
      let ls = vsLayers (toPlot (statModel sm <> grid 10 <> bandMode BandCIPI))
      map (getFirst . lyKind) ls `shouldMatchList` [Just MBand, Just MBand, Just MLine]

  describe "Phase 70.6 G: GAMModel CI 帯 (mgcv 流 Bayesian)" $ do
    -- 非線形データ。 GAM は CI 実装後、 grid 経路で band + line を出す。
    let gmxs = LA.fromList [0, 1, 2, 3, 4, 5, 6, 7, 8]
        gmys = LA.fromList [ sin x + 0.1 * x | x <- LA.toList gmxs ]
        gmm  = gamModel 3 5 0.0 gmxs gmys

    it "toPlot は band(MBand) + line(MLine) の 2 layer を返す" $ do
      let ls = vsLayers (toPlot gmm)
      length ls `shouldBe` 2
      getFirst (lyKind (ls !! 0)) `shouldBe` Just MBand
      getFirst (lyKind (ls !! 1)) `shouldBe` Just MLine

    it "svGrid は Just の CI 帯を返し lo ≤ μ ≤ hi (帯幅 > 0)" $ do
      let (mu, mb) = svGrid gmm 0.95 [1.0, 4.0, 7.0]
      length mu `shouldBe` 3
      case mb of
        Just (lo, hi) -> do
          and (zipWith (<=) lo mu) `shouldBe` True
          and (zipWith (<=) mu hi) `shouldBe` True
          and (zipWith (\l h -> h - l > 0) lo hi) `shouldBe` True
        Nothing -> expectationFailure "CI 帯 (Just) を期待"

    it "★厳密検証: GAM(PolyB 1, λ=0) の CI 帯は LM と一致 (基底が {1,x} と同一スパン)" $ do
      -- y = 2x + 1 + ノイズ。 PolyB 1・λ=0 の列空間は {1, x} ＝ OLS と同一ゆえ、
      -- 予測も予測分散も (したがって CI 帯も) LMModel と厳密一致するはず。
      let xsC = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] :: [Double]
          ysC = [3.1, 4.8, 7.2, 8.9, 11.3, 12.7, 15.1, 16.8, 19.2, 20.9]
          dfC = [ ("x", xsC), ("y", ysC) ] :: [(Text, [Double])]
          gmP = dfC |-> gam (GAMConfig (PolyB 1) (FixedL 0)) "x" "y"   -- GAMModelN
          lmM = lmModel (LA.fromList xsC) (LA.fromList ysC)            -- LMModel (OLS)
          gridC = [2.0, 4.5, 6.0, 8.5]
          (gmu, Just (glo, ghi)) = svGrid gmP 0.95 gridC
          (lmu, Just (llo, lhi)) = svGrid lmM 0.95 gridC
      gmu `shouldSatisfy` allClose lmu
      glo `shouldSatisfy` allClose llo
      ghi `shouldSatisfy` allClose lhi

  describe "Phase 70.6: GAM 基底一般化 + GCV + gam/gamMulti (df|->)" $ do
    -- 非線形 y = sin x + 0.1 x を x∈[0,8] で 21 点。
    let gx70 = [ 0.4 * fromIntegral i | i <- [0 .. 20 :: Int] ] :: [Double]
        gy70 = [ sin x + 0.1 * x | x <- gx70 ]
        xv70 = V.fromList gx70
        yv70 = V.fromList gy70
        df70 = [ ("x", gx70), ("y", gy70) ] :: [(Text, [Double])]

    it "F1: fitGAMWith [BSplineB] が fitGAM とビット一致 (後方互換)" $ do
      let a = fitGAMWith [BSplineB 3 5] 0.0 [xv70] yv70
          b = fitGAM 3 5 0.0 [xv70] yv70
      LA.toList (gamYHat a) `shouldSatisfy` allClose (LA.toList (gamYHat b))

    it "F1: 自然3次基底で非線形を高 R² でフィット" $ do
      gamR2 (fitGAMWith [NaturalCubicB 6] 0.0 [xv70] yv70) `shouldSatisfy` (> 0.9)

    it "F1: Fourier 基底で非線形をフィット (周期=レンジゆえ sin の周期と不一致で 0.8 台)" $ do
      gamR2 (fitGAMWith [FourierB 4] 0.0 [xv70] yv70) `shouldSatisfy` (> 0.8)

    it "F1: RBF 基底で非線形を高 R² でフィット" $ do
      gamR2 (fitGAMWith [RBFB 8 1.0] 0.0 [xv70] yv70) `shouldSatisfy` (> 0.9)

    it "F1: 多項基底でも非線形をある程度フィット (R² > 0.7)" $ do
      gamR2 (fitGAMWith [PolyB 5] 0.0 [xv70] yv70) `shouldSatisfy` (> 0.7)

    it "F2: GCV が有限な λ を選び高 R² (gam 経由)" $ do
      let m = df70 |-> gam (GAMConfig (BSplineB 3 8) GCV) "x" "y"
          f = gamNFit m
      gamLambda f `shouldSatisfy` (\l -> not (isNaN l) && not (isInfinite l) && l >= 0)
      gamEdf f    `shouldSatisfy` (< 21)   -- edf < n
      gamR2 f     `shouldSatisfy` (> 0.95)

    it "F3: gam の toPlot は band(MBand) + line(MLine) の 2 layer を返す (CI 実装後)" $ do
      let m  = df70 |-> gam defaultGAMConfig "x" "y"
          ls = vsLayers (toPlot m)
      length ls `shouldBe` 2
      getFirst (lyKind (ls !! 0)) `shouldBe` Just MBand
      getFirst (lyKind (ls !! 1)) `shouldBe` Just MLine

    it "F3: gam の svGrid が grid 点数分の有限値 + CI 帯 (Just) を返す" $ do
      let m       = df70 |-> gam defaultGAMConfig "x" "y"
          (mu, b) = svGrid m 0.95 [1.0, 4.0, 7.0]
      length mu `shouldBe` 3
      all (\v -> not (isNaN v)) mu `shouldBe` True
      case b of
        Just (lo, hi) -> and (zipWith3 (\l u h -> l <= u && u <= h) lo mu hi) `shouldBe` True
        Nothing       -> expectationFailure "CI 帯 (Just) を期待"

    it "F3: 多予測子は第1予測子を軸に・他を訓練平均で固定して評価できる" $ do
      let dfMV = [ ("x1", gx70)
                 , ("x2", reverse gx70)
                 , ("y",  zipWith (\a c -> sin a + 0.05 * c) gx70 (reverse gx70)) ]
                 :: [(Text, [Double])]
          m       = dfMV |-> gamMulti (GAMConfig (BSplineB 3 5) (FixedL 0.1)) ["x1", "x2"] "y"
          (lo, hi) = svRange m
          (mu, _) = svGrid m 0.95 [lo, (lo + hi) / 2, hi]
      gamNNames m `shouldBe` ["x1", "x2"]
      length mu `shouldBe` 3
      all (\v -> not (isNaN v)) mu `shouldBe` True

  describe "Phase 70.5 項目 E (E1): GP/KRR/RFF 統合 (gp + GPConfig・df|->)" $ do
    -- 非線形 y = sin x を x∈[0,6] で 21 点 (帯/予測の数値検証用)。
    let ex = [ 0.3 * fromIntegral i | i <- [0 .. 20 :: Int] ] :: [Double]
        ey = map sin ex
        dfE = [ ("x", ex), ("y", ey) ] :: [(Text, [Double])]
        q   = [1.0, 3.0, 5.0]
        truth = map sin q
        -- 予測 RMSE (μ̂ vs 真値)。
        rmse cfg =
          let (mu, _) = gprPredict (dfE |-> gp cfg "x" "y") q
          in sqrt (sum (zipWith (\a b -> (a - b) ^ (2 :: Int)) mu truth) / 3)

    it "E1: defaultGP (厳密 GP・周辺尤度) が sin を高精度予測 (RMSE < 0.1)" $ do
      rmse defaultGP `shouldSatisfy` (< 0.1)

    it "E1: Gp 象限は事後分散 (Just) を返す・Ridge 象限は帯なし (Nothing)" $ do
      let (_, vGp)    = gprPredict (dfE |-> gp (GPConfig RBF Gp    AutoMarginalLik) "x" "y") q
          (_, vRidge) = gprPredict (dfE |-> gp (GPConfig RBF Krr AutoMarginalLik) "x" "y") q
      vRidge `shouldBe` Nothing
      case vGp of
        Just vs -> do length vs `shouldBe` 3
                      all (\v -> not (isNaN v) && v >= 0) vs `shouldBe` True
        Nothing -> expectationFailure "Gp 象限は Just 分散を返すべき"

    it "E1: KRR (Ridge) の μ̂ ≡ GP 事後平均 (同ハイパラでビット一致)" $ do
      let p   = defaultGPParams { gpLengthScale = 1.0, gpSignalVar = 1.0, gpNoiseVar = 0.05 }
          cfgG = GPConfig RBF Gp    (FixedHyper p)
          cfgR = GPConfig RBF Krr (FixedHyper p)
          (muG, _) = gprPredict (dfE |-> gp cfgG "x" "y") q
          (muR, _) = gprPredict (dfE |-> gp cfgR "x" "y") q
      muR `shouldBe` muG          -- KRR は GP の mean のみ (帯を捨てるだけ)

    it "E1: RFF 近似 (GpRff) が厳密 GP に概ね一致 (RMSE < 0.15)" $ do
      rmse (GPConfig RBF (GpRff 500 12345) AutoMarginalLik) `shouldSatisfy` (< 0.15)

    it "E1: RFF seed 純粋化 = 同 seed で完全再現・別 seed で別結果" $ do
      let pull s = fst (gprPredict (dfE |-> gp (GPConfig RBF (GpRff 300 s) AutoMarginalLik) "x" "y") q)
      pull 7 `shouldBe` pull 7
      pull 7 `shouldNotBe` pull 99

    it "E1: AutoCV (Gram-LOOCV) が有限な予測を返し高精度 (RMSE < 0.12)" $ do
      let (mu, _) = gprPredict (dfE |-> gp (GPConfig RBF Gp AutoCV) "x" "y") q
      all (\v -> not (isNaN v) && not (isInfinite v)) mu `shouldBe` True
      rmse (GPConfig RBF Gp AutoCV) `shouldSatisfy` (< 0.12)

    it "E1: Periodic + RFF 近似指定 → 厳密象限へフォールバック (gprMethod = Gp)" $ do
      let m = dfE |-> gp (GPConfig Periodic (GpRff 200 7) AutoMarginalLik) "x" "y"
      gprMethod m `shouldBe` Gp
      let (mu, v) = gprPredict m q
      all (\val -> not (isNaN val)) mu `shouldBe` True
      v `shouldSatisfy` (/= Nothing)   -- Gp へ落ちたので分布あり

    it "E1: Matern52 厳密 GP も sin を高精度予測" $ do
      rmse (GPConfig Matern52 Gp AutoMarginalLik) `shouldSatisfy` (< 0.1)

    -- E2: SingleVarModel + Plottable
    it "E2: Gp 象限の svGrid は credible 帯 (Just) を返す" $ do
      let m       = dfE |-> gp defaultGP "x" "y"
          (mu, b) = svGrid m 0.95 [1.0, 3.0, 5.0]
      length mu `shouldBe` 3
      case b of
        Just (los, his) -> do
          length los `shouldBe` 3
          and (zipWith3 (\l u h -> l <= u && u <= h) los mu his) `shouldBe` True
        Nothing -> expectationFailure "Gp 象限は帯を返すべき"

    it "E2: Ridge 象限の svGrid は帯なし (Nothing)" $ do
      let m      = dfE |-> gp (GPConfig RBF Krr AutoMarginalLik) "x" "y"
          (_, b) = svGrid m 0.95 [1.0, 3.0, 5.0]
      b `shouldBe` Nothing

    it "E2: Gp の toPlot は band + line の 2 layer・Ridge は line のみ 1 layer" $ do
      let mGp = dfE |-> gp defaultGP "x" "y"
          mRi = dfE |-> gp (GPConfig RBF Krr AutoMarginalLik) "x" "y"
      length (vsLayers (toPlot mGp)) `shouldBe` 2
      length (vsLayers (toPlot mRi)) `shouldBe` 1
      getFirst (lyKind (last (vsLayers (toPlot mGp)))) `shouldBe` Just MLine

    it "E2: svGridPI (事後予測分散) は CI より広い帯 (Gp 象限)" $ do
      let m         = dfE |-> gp defaultGP "x" "y"
          (_, mbCI) = svGrid m 0.95 [1.0, 3.0, 5.0]
          mbPI      = svGridPI m 0.95 [1.0, 3.0, 5.0]
      case (mbCI, mbPI) of
        (Just (lci, hci), Just (lpi, hpi)) ->
          and (zipWith (\(lc, hc) (lp, hp) -> (hp - lp) >= (hc - lc))
                       (zip lci hci) (zip lpi hpi))
            `shouldBe` True
        _ -> expectationFailure "Gp 象限は CI/PI 両帯を返すべき"

  describe "Phase 70.5 項目 E (E3): gpMulti 多変量 (df|->)" $ do
    -- y = sin x1 + 0.5 x2 を 2 予測子で。 第1予測子を軸に偏依存曲線を評価。
    let ex1 = [ 0.4 * fromIntegral i | i <- [0 .. 20 :: Int] ] :: [Double]
        ex2 = [ 0.2 * fromIntegral i | i <- [0 .. 20 :: Int] ] :: [Double]
        ey3 = zipWith (\a c -> sin a + 0.5 * c) ex1 ex2
        dfM = [ ("x1", ex1), ("x2", ex2), ("y", ey3) ] :: [(Text, [Double])]

    it "E3: gpMulti Gp は予測子名を保持し svGrid が credible 帯を返す" $ do
      let m       = dfM |-> gpMulti defaultGP ["x1", "x2"] "y"
          (lo, hi) = svRange m
          (mu, b) = svGrid m 0.95 [lo, (lo + hi) / 2, hi]
      gprnNames m `shouldBe` ["x1", "x2"]
      length mu `shouldBe` 3
      all (\v -> not (isNaN v)) mu `shouldBe` True
      b `shouldSatisfy` (/= Nothing)

    it "E3: gpMulti Ridge は帯なし (Nothing)" $ do
      let m      = dfM |-> gpMulti (GPConfig RBF Krr AutoMarginalLik) ["x1", "x2"] "y"
          (_, b) = svGrid m 0.95 [1.0, 3.0, 5.0]
      b `shouldBe` Nothing

    it "E3: gpMulti GpRff (MV RFF GP) も帯を返し有限・同 seed 再現" $ do
      let mk s = dfM |-> gpMulti (GPConfig RBF (GpRff 300 s) AutoMarginalLik) ["x1", "x2"] "y"
          (mu1, b1) = svGrid (mk 7) 0.95 [1.0, 3.0, 5.0]
          (mu2, _)  = svGrid (mk 7) 0.95 [1.0, 3.0, 5.0]
      all (\v -> not (isNaN v)) mu1 `shouldBe` True
      b1 `shouldSatisfy` (/= Nothing)
      mu1 `shouldBe` mu2

    it "E3: gpMulti の偏依存曲線が sin(x1) の山谷を捉える (x2 平均固定)" $ do
      let m       = dfM |-> gpMulti defaultGP ["x1", "x2"] "y"
          (mu, _) = svGrid m 0.95 [1.5, 4.5]   -- sin の山 (≈1) と谷 (≈−1)
      -- x2 を平均で固定すれば PD 曲線 ≈ sin(x1) + const ゆえ μ(1.5) > μ(4.5)。
      (mu !! 0 > mu !! 1) `shouldBe` True

    it "E3: gpMulti AutoCV も有限な多変量予測を返す" $ do
      let m       = dfM |-> gpMulti (GPConfig RBF Gp AutoCV) ["x1", "x2"] "y"
          (mu, _) = svGrid m 0.95 [1.0, 4.0, 7.0]
      all (\v -> not (isNaN v) && not (isInfinite v)) mu `shouldBe` True

  describe "Phase 70.7 項目 G (G1): 罰則付き回帰の高レベル df|-> 化" $ do
    -- y = 2 x1 + 0 x2 + 3 x3 + 小ノイズ。 x2 は無関係 (Lasso が 0 にすべき)。
    let n      = 60 :: Int
        x1d    = [ sin (0.3 * fromIntegral i) | i <- [0 .. n - 1] ] :: [Double]
        x2d    = [ cos (0.21 * fromIntegral i) | i <- [0 .. n - 1] ]   -- 無関係
        x3d    = [ sin (0.13 * fromIntegral i + 1.0) | i <- [0 .. n - 1] ]
        noised = [ 0.02 * sin (3.1 * fromIntegral i) | i <- [0 .. n - 1] ]
        yd     = zipWith3 (\a c e -> 2 * a + 3 * c + e) x1d x3d noised
        dfR    = [ ("x1", x1d), ("x2", x2d), ("x3", x3d), ("y", yd) ] :: [(Text, [Double])]
        rows   = [ [a, b, c] | (a, b, c) <- zip3 x1d x2d x3d ]
        rmseOf m = sqrt (sum (zipWith (\p t -> (p - t) ^ (2 :: Int)) (regPredict m rows) yd)
                          / fromIntegral n)

    it "G1: ridge (df|->) が名前を保持し元スケールで高精度予測 (RMSE 小)" $ do
      let m = dfR |-> ridge ["x1", "x2", "x3"] "y"
      rmgNames m `shouldBe` ["x1", "x2", "x3"]
      length (rmgCoefs m) `shouldBe` 3
      rmseOf m `shouldSatisfy` (< 0.3)

    it "G1: lasso が無関係な x2 を縮約 (|β₂| < |β₁|,|β₃|)" $ do
      let m = dfR |-> lasso ["x1", "x2", "x3"] "y"
          [b1, b2, b3] = map abs (rmgCoefs m)
      b2 `shouldSatisfy` (< b1)
      b2 `shouldSatisfy` (< b3)

    it "G1: FixedLambda 経路 — Ridge/Lasso/EN/MCP/SCAD/Adaptive すべて有限係数" $ do
      let mk meth = dfR |-> regularized (RegConfig meth (FixedLambda 0.1)) ["x1", "x2", "x3"] "y"
          finite m = all (\v -> not (isNaN v) && not (isInfinite v)) (rmgCoefs m)
      all finite [ mk Ridge, mk Lasso, mk (ElasticNet 0.5)
                 , mk (MCP 3.0), mk (SCAD 3.7), mk (AdaptiveLasso 1.0) ] `shouldBe` True

    it "G1: LambdaLOOCV は Ridge で成功・Lasso では Left (線形平滑器専用)" $ do
      let okRidge = fitEither (regularized (RegConfig Ridge LambdaLOOCV) ["x1","x2","x3"] "y") dfR
          noLasso = fitEither (regularized (RegConfig Lasso LambdaLOOCV) ["x1","x2","x3"] "y") dfR
      (case okRidge of Right _ -> True; Left _ -> False) `shouldBe` True
      (case (noLasso :: Either String RegModel) of Left _ -> True; Right _ -> False) `shouldBe` True

    it "G1: LambdaCV seed 再現性 (同 seed → 同 λ・別 seed で変わりうる)" $ do
      let mk s = dfR |-> regularized (RegConfig Lasso (LambdaCV 5 s)) ["x1","x2","x3"] "y"
      rmgLambda (mk 7) `shouldBe` rmgLambda (mk 7)

    it "G1: LambdaCV1SE の λ は LambdaCV(best) 以上 (より保守的=スパース)" $ do
      let best = rmgLambda (dfR |-> regularized (RegConfig Lasso (LambdaCV    5 7)) ["x1","x2","x3"] "y")
          one  = rmgLambda (dfR |-> regularized (RegConfig Lasso (LambdaCV1SE 5 7)) ["x1","x2","x3"] "y")
      one `shouldSatisfy` (>= best)

    it "G1: Group Lasso の群 ID 長さ不一致は Left" $ do
      let bad = fitEither (regularized (RegConfig (GroupLasso [0,0]) (FixedLambda 0.1)) ["x1","x2","x3"] "y") dfR
      (case (bad :: Either String RegModel) of Left _ -> True; Right _ -> False) `shouldBe` True

    it "G1: Group Lasso (群指定) も有限な係数で当てはまる" $ do
      let m = dfR |-> regularized (RegConfig (GroupLasso [0,1,0]) (FixedLambda 0.05)) ["x1","x2","x3"] "y"
      all (\v -> not (isNaN v)) (rmgCoefs m) `shouldBe` True

  describe "Phase 46 A9 + 70.C: RobustModel + toPlot (ロバスト直線 + CI 帯)" $ do
    -- y = 2x + 1 にして、 1 点だけ大きな外れ値を入れる。 ロバスト傾きが外れ値に
    -- 引っ張られず ≈ 2 に留まることを確認。
    let rxs = LA.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        rys = LA.fromList [3, 5, 7, 9, 11, 13, 15, 17, 19, 100]  -- 末尾が外れ値 (本来 21)
        rm  = robustModel (Huber defaultHuberK) rxs rys
        rLine = head [ l | l <- vsLayers (toPlot rm), getFirst (lyKind l) == Just MLine ]

    it "toPlot は MBand + MLine の 2 layer (CI 帯付き・Phase 70.C で揃えた)" $ do
      let ls = vsLayers (toPlot rm)
      map (getFirst . lyKind) ls `shouldBe` [Just MBand, Just MLine]

    it "ロバスト傾き ≈ 2 (外れ値に引っ張られない)" $ do
      -- 直線 2 点から傾きを復元 (encX/encY は昇順)。
      case (getLast (lyEncX rLine), getLast (lyEncY rLine)) of
        (Just (ColNum vx), Just (ColNum vy)) -> do
          let xs = V.toList vx; ys = V.toList vy
              slope = (last ys - head ys) / (last xs - head xs)
          abs (slope - 2) `shouldSatisfy` (< 0.2)
        _ -> expectationFailure "encX/encY が inline ColNum でない"

    it "外れ値 (末尾) の IRLS 重みは健全点より小さい" $ do
      let ws = LA.toList (rfWeights (rmFit rm))
      last ws `shouldSatisfy` (< minimum (init ws) + 1e-9)

    -- 検証: Huber の k を巨大にすると全点 inlier (ψ'=1, ψ(u)=u) ゆえ、 サンドイッチ
    -- 共分散は σ が打ち消えて OLS 共分散 (Σr²/(n-p)·(XᵀX)⁻¹) に厳密一致する。
    -- = statsmodels RLM cov="H1" の OLS 極限。 これを数値で実測する。
    it "robustCovBeta(Huber 巨大k) の SE は OLS SE に厳密一致 (サンドイッチ→OLS極限)" $ do
      let n      = LA.rows xX
          xX     = LA.fromColumns [ LA.konst 1 (LA.size rxs), rxs ]   -- [1, x]
          xtxInv = LA.inv (LA.tr xX LA.<> xX)
          betaO  = xtxInv LA.#> (LA.tr xX LA.#> rys)
          residO = rys - (xX LA.#> betaO)
          sigma2 = (residO LA.<.> residO) / fromIntegral (n - 2)
          olsSE  = map sqrt (LA.toList (LA.takeDiag (LA.scale sigma2 xtxInv)))
          rfit   = fitRobustLM (Huber 1e6) xX rys 50 1e-9
          rcov   = robustCovBeta (Huber 1e6) (rfScale rfit) (rfResiduals rfit) xX
          rSE    = map sqrt (LA.toList (LA.takeDiag rcov))
      rSE `shouldSatisfy` allClose olsSE

    -- ★statsmodels RLM (HuberT t=1.345, scale=mad, cov="H1") との実測突合。
    -- 参照値 (statsmodels 0.14.6):
    --   params = [0.83418235, 2.06340481]、 scale = 0.64004295、 bse = [0.41150687, 0.06632034]
    it "robust SE/係数/scale が statsmodels RLM (HuberT, cov=H1) に一致" $ do
      let vx   = LA.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
          vy   = LA.fromList [3.4, 4.6, 7.5, 8.7, 11.3, 12.6, 15.4, 16.8, 19.2, 45]
          xX   = LA.fromColumns [ LA.konst 1 (LA.size vx), vx ]
          rfit = fitRobustLM (Huber 1.345) xX vy 100 1e-10
          se   = map sqrt (LA.toList (LA.takeDiag
                   (robustCovBeta (Huber 1.345) (rfScale rfit) (rfResiduals rfit) xX)))
          near tol a b = abs (a - b) <= tol * (1 + abs b)
          closeTo tol ref xs = length xs == length ref && and (zipWith (near tol) xs ref)
      LA.toList (rfCoef rfit) `shouldSatisfy` closeTo 1e-3 [0.83418235, 2.06340481]
      rfScale rfit            `shouldSatisfy` near    1e-3 0.64004295
      se                      `shouldSatisfy` closeTo 1e-3 [0.41150687, 0.06632034]

    it "diagnosticPlots は 3 枚 (直線 + 残差 + 重み encode 散布図)" $ do
      let ds = diagnosticPlots rm
      length ds `shouldBe` 3
      -- 3 枚目に size encoding (lySizeBy) が乗っている。
      let wLayer = head (vsLayers (ds !! 2))
      getLast (lySizeBy wLayer) `shouldSatisfy` \x -> case x of
        Just (ColNum _) -> True
        _               -> False

  describe "Phase 46 A9: MultiFit + toPlot (残差相関 heatmap)" $ do
    -- 3 出力。 各出力に残差を残すため決定的な wiggle を加える (完全 fit だと残差分散 0
    -- → 相関が NaN になる)。 共有 wiggle で out1/out2 は正相関、 out3 は逆相関にする。
    let nObs = 20
        xcol = [ fromIntegral i | i <- [1 .. nObs] ] :: [Double]
        wig  = [ sin (fromIntegral i) | i <- [1 .. nObs] ] :: [Double]
        xmat = LA.fromColumns [LA.konst 1 nObs, LA.fromList xcol]   -- n×2 (intercept+x)
        y1   = zipWith (\x w -> 2*x + w)      xcol wig
        y2   = zipWith (\x w -> x   + 0.8*w)  xcol wig             -- wig 共有 → +相関
        y3   = zipWith (\x w -> -x  - w)      xcol wig             -- -wig → 逆相関
        ymat = LA.fromColumns (map LA.fromList [y1, y2, y3])        -- n×3
        mf   = fitMultiLM xmat ymat
        hLayer = head (vsLayers (toPlot mf))
        -- heatmap は categorical 軸: encX/encY は出力名ラベル (ColTxt inline)。
        txtH f = case getLast (f hLayer) of
          Just (ColTxt v) -> V.toList v
          _               -> error "encoding が inline ColTxt でない"
        -- value は lyColor = ColorByContinuous (inline) に入る。
        valsH = case getLast (lyColor hLayer) of
          Just (ColorByContinuous (ColNum v)) -> V.toList v
          _ -> error "color が ColorByContinuous inline でない"

    it "toPlot は MHeatmap layer を 1 つ返す" $ do
      length (vsLayers (toPlot mf)) `shouldBe` 1
      getFirst (lyKind hLayer) `shouldBe` Just MHeatmap

    it "heatmap は q×q = 9 セル・軸は出力名ラベル (q=3 出力)" $ do
      length (txtH lyEncX) `shouldBe` 9
      txtH lyEncX `shouldSatisfy` all (`elem` ["y1", "y2", "y3"])

    it "対角は相関 = 1 (自己相関)" $ do
      -- セルは (x=yⱼ, y=yᵢ)。 対角 = x==y のラベル一致セル。
      let xs = txtH lyEncX; ys = txtH lyEncY; vs = valsH
          diag = [ v | (x, y, v) <- zip3 xs ys vs, x == y ]
      diag `shouldSatisfy` allClose [1, 1, 1]

    it "out1-out2 は正相関、 out1-out3 は逆相関 (符号検証)" $ do
      let xs = txtH lyEncX; ys = txtH lyEncY; vs = valsH
          at i j = head [ v | (x, y, v) <- zip3 xs ys vs, x == j, y == i ]
      at "y1" "y2" `shouldSatisfy` (> 0)   -- out1 vs out2
      at "y1" "y3" `shouldSatisfy` (< 0)   -- out1 vs out3

  describe "Phase 46 A10: QuantileModel + toPlot (複数分位線・色分け重畳)" $ do
    -- heteroscedastic データ: 分散が x で増える → 分位線が末広がりになる。
    let qxs = LA.fromList [ fromIntegral i | i <- [1 .. 40 :: Int] ]
        qys = LA.fromList [ 2 * x + (x / 8) * sin (fromIntegral i)
                          | (i, x) <- zip [1 :: Int ..] (LA.toList qxs) ]
        qm  = quantileModel [0.1, 0.5, 0.9] qxs qys
        qLayers = vsLayers (toPlot qm)

    it "toPlot は分位数ぶんの MLine layer を返す (3 本)" $ do
      length qLayers `shouldBe` 3
      map (getFirst . lyKind) qLayers `shouldSatisfy` all (== Just MLine)

    it "各 line layer に固定色 (ColorStatic) が付く" $ do
      qLayers `shouldSatisfy` all (\l -> case getLast (lyColor l) of
        Just (ColorStatic _) -> True
        _                    -> False)

    it "分位は単調: 全 x で τ=0.1 の ŷ ≤ τ=0.9 の ŷ" $ do
      let yhatOf t = head [ V.toList (V.fromList (LA.toList (qfYHat f)))
                          | (tt, f) <- qmFits qm, tt == t ]
          lo = yhatOf 0.1; hi = yhatOf 0.9
      and (zipWith (<=) lo hi) `shouldBe` True

    it "中央値 (τ=0.5) の傾き ≈ 2" $ do
      let med = head [ f | (tt, f) <- qmFits qm, tt == 0.5 ]
          b   = LA.toList (qfBeta med)
      abs (b !! 1 - 2) `shouldSatisfy` (< 0.3)

  describe "Phase 46 A11: ChainModel + toPlot (trace + 周辺事後密度)" $ do
    -- AR(1) 風の決定的な draw 列 (平均 5 まわりに揺れる)。 1 パラメータ "mu"。
    let draws = take 100 (iterate (\v -> 5 + 0.7 * (v - 5) + 0.5 * sin v) 4.0)
        ch    = Chain { chainSamples     = [ Map.singleton "mu" v | v <- draws ]
                      , chainAccepted    = 100
                      , chainTotal       = 120
                      , chainEnergy      = []
                      , chainDivergences = []
                      , chainTreeDepths  = [] }
        cm     = chainModel "mu" ch
        tLayer = head (vsLayers (toPlot cm))

    it "toPlot は trace (MTrace) layer を 1 つ返す" $ do
      length (vsLayers (toPlot cm)) `shouldBe` 1
      getFirst (lyKind tLayer) `shouldBe` Just MTrace

    it "trace の encX は draw index 1..N、 encY は draw 値" $ do
      case (getLast (lyEncX tLayer), getLast (lyEncY tLayer)) of
        (Just (ColNum vx), Just (ColNum vy)) -> do
          V.toList vx `shouldSatisfy` allClose [ fromIntegral i | i <- [1 .. length draws] ]
          V.toList vy `shouldSatisfy` allClose draws
        _ -> expectationFailure "encX/encY が inline ColNum でない"

    it "diagnosticPlots は 2 枚 (trace + density)" $ do
      let ds = diagnosticPlots cm
      length ds `shouldBe` 2
      getFirst (lyKind (head (vsLayers (ds !! 0)))) `shouldBe` Just MTrace
      getFirst (lyKind (head (vsLayers (ds !! 1)))) `shouldBe` Just MDensity

  describe "Phase 46 A12: KMResult + toPlot (KM 生存曲線・階段)" $ do
    let samples = [ SurvSample t e
                  | (t, e) <- [ (1, Observed), (2, Observed), (3, Censored)
                              , (4, Observed), (5, Observed), (6, Censored)
                              , (7, Observed), (8, Observed) ] ]
        km  = kaplanMeier samples
        kLayer = head (vsLayers (toPlot km))
        kY  = case getLast (lyEncY kLayer) of
          Just (ColNum v) -> V.toList v
          _               -> error "encY が ColNum でない"

    it "toPlot は MLine layer を 1 つ返す (階段)" $ do
      length (vsLayers (toPlot km)) `shouldBe` 1
      getFirst (lyKind kLayer) `shouldBe` Just MLine

    it "生存曲線は S=1 から始まり単調非増加 (∈ [0,1])" $ do
      head kY `shouldBe` 1.0
      and (zipWith (>=) kY (drop 1 kY)) `shouldBe` True
      kY `shouldSatisfy` all (\s -> s >= 0 && s <= 1)

  describe "Phase 46 A12: CRFit + toPlot (競合リスク CIF・色分け階段)" $ do
    -- 2 cause + 打ち切り (cause 0)。
    let crs = [ CRSample t c
              | (t, c) <- [ (1, 1), (2, 2), (3, 1), (4, 0), (5, 2)
                          , (6, 1), (7, 0), (8, 2), (9, 1), (10, 2) ] ]
        cr  = fitCompetingRisks crs
        crLayers = vsLayers (toPlot cr)
        firstY = case getLast (lyEncY (head crLayers)) of
          Just (ColNum v) -> V.toList v
          _               -> error "encY が ColNum でない"

    it "toPlot は cause 数ぶんの MLine layer を返す (2 本)" $ do
      length crLayers `shouldBe` 2
      map (getFirst . lyKind) crLayers `shouldSatisfy` all (== Just MLine)

    it "各 cause CIF は 0 から始まり単調非減少" $ do
      head firstY `shouldBe` 0.0
      and (zipWith (<=) firstY (drop 1 firstY)) `shouldBe` True

    it "各 line layer に固定色 (ColorStatic) が付く" $ do
      crLayers `shouldSatisfy` all (\l -> case getLast (lyColor l) of
        Just (ColorStatic _) -> True
        _                    -> False)

  describe "Phase 46 A13: ForecastModel + toPlot (AR 予測 + 予測区間 band)" $ do
    -- 定常 AR(1) 風の系列。 h=8 step 予測。
    let series = LA.fromList [ 10 + 3 * sin (fromIntegral i * 0.4) + 0.5 * cos (fromIntegral i)
                             | i <- [1 .. 60 :: Int] ]
        fm    = forecastModel 2 8 series
        layers = vsLayers (toPlot fm)
        bandL = head [ l | l <- layers, getFirst (lyKind l) == Just MBand ]
        bandLo f = case getLast (f bandL) of
          Just (ColNum v) -> V.toList v
          _               -> error "band encoding が ColNum でない"

    it "toPlot は band (MBand) + line 2 本 (履歴 + 予測) を返す" $ do
      length layers `shouldBe` 3
      length [ l | l <- layers, getFirst (lyKind l) == Just MBand ] `shouldBe` 1
      length [ l | l <- layers, getFirst (lyKind l) == Just MLine ] `shouldBe` 2

    it "band は h=8 点、 上境界 ≥ 下境界" $ do
      let lo = bandLo lyEncY; hi = bandLo lyEncY2
      length lo `shouldBe` 8
      and (zipWith (<=) lo hi) `shouldBe` True

    it "予測区間幅は地平とともに単調増加 (se が h で広がる)" $ do
      let lo = bandLo lyEncY; hi = bandLo lyEncY2
          widths = zipWith (-) hi lo
      and (zipWith (<=) widths (drop 1 widths)) `shouldBe` True

  describe "Phase 46 A14: PCAResult + toPlot (scree plot)" $ do
    -- 第 1 軸に強い分散・第 2 軸に弱い分散を持たせた 3 次元データ。
    let rows = [ [ 5 * sin (fromIntegral i * 0.3)
                 , 1.2 * cos (fromIntegral i * 0.5)
                 , 0.3 * sin (fromIntegral i) ]
               | i <- [1 .. 40 :: Int] ]
        xmat = LA.fromLists rows
        res  = PCALow.pca CenterScale Nothing xmat
        pLayer = head (vsLayers (toPlot res))

    it "toPlot は bar (MBar) layer を 1 つ返す" $ do
      length (vsLayers (toPlot res)) `shouldBe` 1
      getFirst (lyKind pLayer) `shouldBe` Just MBar

    it "棒は PC ラベル軸・寄与率は降順 (PCA は分散順)" $ do
      case getLast (lyEncY pLayer) of
        Just (ColNum v) -> do
          let ratios = V.toList v
          and (zipWith (>=) ratios (drop 1 ratios)) `shouldBe` True
          abs (sum ratios - 1) `shouldSatisfy` (< 1e-6)   -- 全成分で和=1
        _ -> expectationFailure "encY が ColNum でない"

  describe "Phase 46 A14 / 75.24: RandomForest + toPlot (2 パネル importance)" $ do
    it "toPlot は 2 パネル (impurity/permutation)・impurity は特徴数ぶんの bar・非負" $ do
      -- y は主に x0 で決まる → x0 の重要度が高いはず (構造のみ検証)。
      let xss = [ [ fromIntegral i, sin (fromIntegral i) ] | i <- [1 .. 50 :: Int] ]
          ys  = [ 2 * fromIntegral i | i <- [1 .. 50 :: Int] ]
      gen <- MWC.createSystemRandom
      rf  <- fitRF defaultRandomForest xss ys gen
      let panels = vsSubplots (toPlot rf)
      length panels `shouldBe` 2                          -- R varImpPlot 流 2 パネル
      let impLayer = head (vsLayers (head panels))        -- 左 = impurity
      getFirst (lyKind impLayer) `shouldBe` Just MBar
      case getLast (lyEncY impLayer) of
        Just (ColNum v) -> do
          V.length v `shouldBe` 2                         -- 2 特徴
          V.toList v `shouldSatisfy` all (>= 0)           -- impurity 重要度 ≥ 0
        _ -> expectationFailure "encY が ColNum でない"

  describe "Phase 16 §3-D: モデル API 層 (predict / describe / coefficients)" $ do
    -- m は冒頭の LMModel (y = 2x + 1)。
    it "modelCoefficients == coefficientsV (LM)" $ do
      modelCoefficients m `shouldSatisfy` allClose (LA.toList (coefficientsV (lmResult m)))

    it "predictPoint LM (訓練点) == fitted (線形予測の整合)" $ do
      let preds = map (predictPoint m) (LA.toList xs)
      preds `shouldSatisfy` allClose (LA.toList (fittedV (lmResult m)))

    it "describeModel: 係数値=modelCoefficients・SE≥0・CI が値を含む" $ do
      let cs = describeModel m
      map coefValue cs `shouldSatisfy` allClose (modelCoefficients m)
      cs `shouldSatisfy` all (\c -> coefSE c >= 0)
      cs `shouldSatisfy` all (\c -> let (lo, hi) = coefCI c
                                    in lo <= coefValue c && coefValue c <= hi)

    it "predictPoint GLM (訓練点) == 逆リンク後の μ̂ (Poisson/Log)" $ do
      let gxs = LA.fromList [1, 2, 3, 4, 5, 6]
          gys = LA.fromList [1, 2, 4, 7, 12, 20]
          gm  = glmModel Poisson Log gxs gys
          preds = map (predictPoint gm) (LA.toList gxs)
      preds `shouldSatisfy` allClose (LA.toList (fittedV (glmResult gm)))

  describe "Phase 16 §3 C1: grid 評価 (滑らかな曲線・ModelSpec)" $ do
    -- m = 冒頭の LMModel (y = 2x + 1, x ∈ [1..5])。 grid 評価は訓練点数と独立。
    let lineLayerOf spec = head [ l | l <- vsLayers (toPlot spec)
                                    , getFirst (lyKind l) == Just MLine ]
        numX l = case getLast (lyEncX l) of
                   Just (ColNum v) -> V.toList v
                   _               -> error "encX が ColNum でない"
        numY l = case getLast (lyEncY l) of
                   Just (ColNum v) -> V.toList v
                   _               -> error "encY が ColNum でない"

    it "grid n で曲線の頂点数 = n (訓練点数 5 と独立)" $ do
      length (numX (lineLayerOf (statModel m <> grid 25))) `shouldBe` 25

    it "既定 grid = 100 点" $ do
      length (numX (lineLayerOf (statModel m))) `shouldBe` 100

    it "grid 範囲の既定は説明変数 min/max (= [1, 5])" $ do
      let xg = numX (lineLayerOf (statModel m <> grid 50))
      head xg `shouldSatisfy` (\v -> abs (v - 1) < 1e-9)
      last xg `shouldSatisfy` (\v -> abs (v - 5) < 1e-9)

    it "gridRange lo hi が評価範囲を上書きする" $ do
      let xg = numX (lineLayerOf (statModel m <> grid 11 <> gridRange 0 10))
      head xg `shouldSatisfy` (\v -> abs v < 1e-9)
      last xg `shouldSatisfy` (\v -> abs (v - 10) < 1e-9)

    it "grid 上の μ̂ = predictPoint (= fit と整合・補間でない)" $ do
      let l  = lineLayerOf (statModel m <> grid 7)
      numY l `shouldSatisfy` allClose (map (predictPoint m) (numX l))

    it "LM grid: 曲線は 2x+1 (完全線形データを正しく外挿)" $ do
      let l = lineLayerOf (statModel m <> grid 9)
      numY l `shouldSatisfy` allClose (map (\x -> 2 * x + 1) (numX l))

    it "既定 (帯 ON, Phase 70.E) は MBand + MLine の 2 layer" $ do
      let ls = vsLayers (toPlot (statModel m <> grid 20))
      map (getFirst . lyKind) ls `shouldMatchList` [Just MBand, Just MLine]

    it "bandMode BandOff で帯が消え MLine 1 layer のみ" $ do
      let ls = vsLayers (toPlot (statModel m <> grid 20 <> bandMode BandOff))
      map (getFirst . lyKind) ls `shouldBe` [Just MLine]

    it "bandMode BandCIPI で CI+PI 入れ子 (MBand 2 + MLine 1)・PI ⊃ CI" $ do
      let ls    = vsLayers (toPlot (statModel m <> grid 20 <> bandMode BandCIPI))
          bands = [ l | l <- ls, getFirst (lyKind l) == Just MBand ]
          loY l = case getLast (lyEncY l) of
            Just (ColNum v) -> V.toList v
            _               -> error "encY が ColNum でない"
      map (getFirst . lyKind) ls `shouldMatchList` [Just MBand, Just MBand, Just MLine]
      -- 1 本目が PI (下に描く=広い)、 2 本目が CI。 PI 下限 < CI 下限。
      case bands of
        [pib, cib] -> and (zipWith (<=) (loY pib) (loY cib)) `shouldBe` True
        _          -> expectationFailure "MBand が 2 本でない"

    it "オプションのみ (モデル無し) は空図" $ do
      vsLayers (toPlot (grid 50 <> bandMode BandOff)) `shouldBe` []

    it "GAM grid は CI 帯 + 線 (Phase 70.6 G・grid 点数反映)" $ do
      let gm = gamModel 3 5 0.0
                 (LA.fromList [0, 1, 2, 3, 4, 5, 6, 7, 8])
                 (LA.fromList [0, 1, 4, 9, 16, 25, 36, 49, 64])
          ls = vsLayers (toPlot (statModel gm <> grid 30))
      map (getFirst . lyKind) ls `shouldMatchList` [Just MBand, Just MLine]
      length (numX (head ls)) `shouldBe` 30   -- band も line も grid 30 点

    -- Phase 52 A2: 線/帯 aes setter (color/fill/linetype/linewidth/alpha)
    let bandLayerOf spec = head [ l | l <- vsLayers (toPlot spec)
                                    , getFirst (lyKind l) == Just MBand ]

    it "A2 statColor: 線レイヤに固定色 (ColorStatic)" $ do
      getLast (lyColor (lineLayerOf (statModel m <> statColor (fromHex "#ff0000"))))
        `shouldBe` Just (ColorStatic "#ff0000")

    it "A2 statLinetype: 線レイヤに固定線種" $ do
      getLast (lyLinetype (lineLayerOf (statModel m <> statLinetype LtDashed)))
        `shouldBe` Just LtDashed

    it "A2 statLinewidth: 線レイヤに stroke 幅" $ do
      getLast (lyStroke (lineLayerOf (statModel m <> statLinewidth 2.5)))
        `shouldBe` Just 2.5

    it "A2 statFill + statAlpha: 帯レイヤに塗り色 + 透明度" $ do
      let spec = statModel m <> statFill (fromHex "#4682b4") <> statAlpha 0.2
          b    = bandLayerOf spec
      getLast (lyColor b) `shouldBe` Just (ColorStatic "#4682b4")
      getLast (lyAlpha b) `shouldBe` Just 0.2

    it "A2 statColor は帯 fill に漏れない (線のみ・帯は statFill)" $ do
      let spec = statModel m <> statColor (fromHex "#ff0000")
      getLast (lyColor (lineLayerOf spec)) `shouldBe` Just (ColorStatic "#ff0000")
      getLast (lyColor (bandLayerOf spec)) `shouldBe` Nothing

    -- Phase 52 A3: statLabel (単線命名・凡例)
    it "A3 statLabel: 線は ColorByCol (固定色でなく凡例が出る encoding)" $ do
      let l = lineLayerOf (statModel m <> statLabel "OLS")
      case getLast (lyColor l) of
        Just (ColorByCol _) -> pure ()
        other               -> expectationFailure ("ColorByCol を期待: " ++ show other)

    it "A3 statLabel: scaleColorManual で色固定 (既定パレット先頭) + legend 有効" $ do
      let vs = toPlot (statModel m <> statLabel "OLS")
      getLast (vsColorManual vs) `shouldBe` Just [("OLS", "#1f77b4")]
      getLast (vsLegend vs) `shouldSatisfy` \x -> case x of
        Just _  -> True
        Nothing -> False

    it "A3 statLabel + statColor: scaleColorManual の色は statColor" $ do
      let vs = toPlot (statModel m <> statLabel "OLS" <> statColor (fromHex "#aa0000"))
      getLast (vsColorManual vs) `shouldBe` Just [("OLS", "#aa0000")]

    it "A3 statLabel 無し: scaleColorManual も legend も付かない" $ do
      let vs = toPlot (statModel m)
      getLast (vsColorManual vs) `shouldBe` Nothing
      getLast (vsLegend vs) `shouldBe` Nothing

    -- Phase 52 A8: statEquation / statR2 (回帰式/R² 凡例注釈)。
    -- m = y = 2x + 1 (intercept 1・slope 2・R²=1)。 A3 と同じ ColorByCol+scaleColorManual 経路。
    it "A8 statEquation: 凡例ラベルが回帰式 'y = 1.000 + 2.000x'" $ do
      let vs = toPlot (statModel m <> statEquation)
      getLast (vsColorManual vs) `shouldBe` Just [("y = 1.000 + 2.000x", "#1f77b4")]
      getLast (vsLegend vs) `shouldSatisfy` \x -> case x of Just _ -> True; Nothing -> False

    it "A8 statR2: 凡例ラベルが 'R² = 1.000'" $ do
      let vs = toPlot (statModel m <> statR2)
      getLast (vsColorManual vs) `shouldBe` Just [("R² = 1.000", "#1f77b4")]

    it "A8 statEquation + statR2: 1 ラベルに連結 'y = … , R² = …'" $ do
      let vs = toPlot (statModel m <> statEquation <> statR2)
      getLast (vsColorManual vs)
        `shouldBe` Just [("y = 1.000 + 2.000x, R² = 1.000", "#1f77b4")]

    it "A8 statLabel 明示は autoLabel より優先 (式に上書きされない)" $ do
      let vs = toPlot (statModel m <> statEquation <> statLabel "OLS")
      getLast (vsColorManual vs) `shouldBe` Just [("OLS", "#1f77b4")]

    it "A8 注釈無し: scaleColorManual は付かない (オプトイン)" $ do
      getLast (vsColorManual (toPlot (statModel m))) `shouldBe` Nothing

    it "A8 svCoefR2 を持たないモデル (GAM) は式注釈が出ない" $ do
      -- GAM は svCoefR2 = Nothing ゆえ statEquation を付けても凡例ラベルなし。
      let gamM = gamModel 3 5 0.0 (LA.fromList [1, 2, 3, 4, 5])
                                  (LA.fromList [1, 4, 9, 16, 25])
          vs   = toPlot (statModel gamM <> statEquation)
      getLast (vsColorManual vs) `shouldBe` Nothing

  describe "Phase 16 §3 C2: predAt (予測点・点 + CI エラーバー)" $ do
    -- m = 冒頭の LMModel (y = 2x + 1)。 band ありモデルは lineRange + scatter。
    let kindsOf spec = map (getFirst . lyKind) (vsLayers (toPlot spec))
        layerOfKind k spec = head [ l | l <- vsLayers (toPlot spec)
                                      , getFirst (lyKind l) == Just k ]
        numOf f l = case getLast (f l) of
          Just (ColNum v) -> V.toList v
          _               -> error "encoding が ColNum でない"

    it "predAt 1 点: band + line に lineRange + scatter が加わる (LM)" $ do
      kindsOf (statModel m <> grid 20 <> predAt 3)
        `shouldMatchList` [Just MBand, Just MLine, Just MLineRange, Just MScatter]

    it "predAt はリスト累積 (<> で複数点): scatter に 3 点" $ do
      let sc = layerOfKind MScatter (statModel m <> predAt 1 <> predAt 3 <> predAt 5)
      numOf lyEncX sc `shouldSatisfy` allClose [1, 3, 5]

    it "予測点 μ̂ = predictPoint (= D の点予測と一致・LM 2x+1)" $ do
      let sc = layerOfKind MScatter (statModel m <> predAt 2 <> predAt 4)
      numOf lyEncY sc `shouldSatisfy` allClose [5, 9]  -- 2·2+1, 2·4+1

    it "CI エラーバー (lineRange) の中心は区間中点・半幅 ≥ 0 (LM=対称)" $ do
      let lr = layerOfKind MLineRange (statModel m <> predAt 3)
      numOf lyErrorY lr `shouldSatisfy` all (>= 0)
      -- LM は対称ゆえ中点 = μ̂ = 7
      numOf lyEncY  lr `shouldSatisfy` allClose [7]

    it "GLM predAt: μ̂ は逆リンク後 μ̂、 lineRange 区間は μ̂ を内包 (非対称可)" $ do
      let gxs = LA.fromList [1, 2, 3, 4, 5, 6]
          gys = LA.fromList [1, 2, 4, 7, 12, 20]
          gm  = glmModel Poisson Log gxs gys
          spec = statModel gm <> predAt 4
          sc  = layerOfKind MScatter   spec
          lr  = layerOfKind MLineRange spec
          mu  = head (numOf lyEncY sc)
          mid = head (numOf lyEncY lr)
          haf = head (numOf lyErrorY lr)
      -- μ̂ は区間 [mid-haf, mid+haf] = [lo, hi] 内
      (mid - haf - 1e-9 <= mu && mu <= mid + haf + 1e-9) `shouldBe` True

    it "GAM predAt: CI 帯ありゆえ band + line + lineRange + scatter (Phase 70.6 G)" $ do
      let gm = gamModel 3 5 0.0
                 (LA.fromList [0, 1, 2, 3, 4, 5, 6, 7, 8])
                 (LA.fromList [0, 1, 4, 9, 16, 25, 36, 49, 64])
      kindsOf (statModel gm <> predAt 4)   -- GAM に CI 実装後は LM と同形
        `shouldMatchList` [Just MBand, Just MLine, Just MLineRange, Just MScatter]

  describe "Phase 16 §3 C3: 多変量 effect plot (statModelMulti + holdAt + byVar)" $ do
    -- y = 1 + 2·x1 + 3·x2 + 4·x3 (厳密線形・無誤差)。 12 行 = x1∈{1,2,3} × x2∈{0,1} × x3∈{0,1}。
    -- 設計フルランクゆえ OLS は β=[1,2,3,4] を厳密復元 → 評価点 μ̂ も厳密。
    let dfMV = DX.fromNamedColumns
          [ ("y",  DX.fromList ([3,7,6,10, 5,9,8,12, 7,11,10,14] :: [Double]))
          , ("x1", DX.fromList ([1,1,1,1, 2,2,2,2, 3,3,3,3] :: [Double]))
          , ("x2", DX.fromList ([0,0,1,1, 0,0,1,1, 0,0,1,1] :: [Double]))
          , ("x3", DX.fromList ([0,1,0,1, 0,1,0,1, 0,1,0,1] :: [Double]))
          ]
        mlm = either error id (multiLMModel "y ~ x1 + x2 + x3" dfMV)
        lineLayersOf spec = [ l | l <- vsLayers (toPlot spec)
                                , getFirst (lyKind l) == Just MLine ]
        bandLayersOf spec = [ l | l <- vsLayers (toPlot spec)
                                , getFirst (lyKind l) == Just MBand ]
        firstLine spec = head (lineLayersOf spec)
        numXof l = case getLast (lyEncX l) of
                     Just (ColNum v) -> V.toList v
                     _               -> error "encX が ColNum でない"
        numYof l = case getLast (lyEncY l) of
                     Just (ColNum v) -> V.toList v
                     _               -> error "encY が ColNum でない"
        kindsMV spec = map (getFirst . lyKind) (vsLayers (toPlot spec))

    it "along x1 (既定 帯 ON, 既定 holdAt Mean): MBand + MLine の 2 layer・頂点 100" $ do
      let spec = statModelMulti mlm (along "x1")
      kindsMV spec `shouldMatchList` [Just MBand, Just MLine]
      length (numXof (firstLine spec)) `shouldBe` 100

    it "Phase 70.G: 重回帰 effect plot も BandCIPI で CI+PI 入れ子 (PI ⊃ CI)" $ do
      -- σ̂²>0 のノイズ入り重回帰 (exact-linear だと PI=CI になるため別データ)。
      let nz   = cycle [0.4,-0.5,0.3,-0.2,0.6,-0.4,0.2,-0.3]
          ys'  = zipWith3 (\a b e -> 1 + 2*a + 1.5*b + e)
                          ([1..12] :: [Double]) (cycle [0,1,2]) (take 12 nz)
          dfN  = DX.fromNamedColumns
                   [ ("y",  DX.fromList ys')
                   , ("x1", DX.fromList ([1..12] :: [Double]))
                   , ("x2", DX.fromList (take 12 (cycle [0,1,2]) :: [Double])) ]
          mN   = either error id (multiLMModel "y ~ x1 + x2" dfN)
          spec = statModelMulti mN (along "x1") <> grid 5 <> bandMode BandCIPI
          bands = bandLayersOf spec
          halfOf l = case (getLast (lyEncY l), getLast (lyEncY2 l)) of
            (Just (ColNum lo), Just (ColNum hi)) -> zipWith (-) (V.toList hi) (V.toList lo)
            _ -> error "band encoding が ColNum でない"
      length bands `shouldBe` 2          -- PI (外) + CI (内)
      -- bands[0]=PI が bands[1]=CI より各点で広い (PI ⊃ CI)。
      case bands of
        [pib, cib] -> and (zipWith (>=) (halfOf pib) (halfOf cib)) `shouldBe` True
        _          -> expectationFailure "MBand が 2 本でない"

    it "along grid 範囲は x1 の観測 min/max = [1, 3]" $ do
      let xg = numXof (firstLine (statModelMulti mlm (along "x1") <> grid 5))
      head xg `shouldSatisfy` (\v -> abs (v - 1) < 1e-9)
      last xg `shouldSatisfy` (\v -> abs (v - 3) < 1e-9)

    it "holdAt Mean: 曲線 = 4.5 + 2·x1 (x2,x3 を平均 0.5 で固定)" $ do
      let l = firstLine (statModelMulti mlm (along "x1") <> grid 9)
      numYof l `shouldSatisfy` allClose (map (\x -> 4.5 + 2 * x) (numXof l))

    it "holdAt (Fixed x2=1, x3=1): 曲線 = 8 + 2·x1" $ do
      let l = firstLine (statModelMulti mlm (along "x1") <> grid 7
                          <> holdAt (Fixed [("x2", 1), ("x3", 1)]))
      numYof l `shouldSatisfy` allClose (map (\x -> 8 + 2 * x) (numXof l))

    it "holdAt (Fixed x2=0) 部分指定: x3 は Mean 0.5 のまま → 曲線 = 3 + 2·x1" $ do
      let l = firstLine (statModelMulti mlm (along "x1") <> grid 7
                          <> holdAt (Fixed [("x2", 0)]))
      numYof l `shouldSatisfy` allClose (map (\x -> 3 + 2 * x) (numXof l))

    it "byVar x2 [0,1] (既定 帯 ON): 2 曲線 (MLine 2 本・MBand 2 本)" $ do
      let spec = statModelMulti mlm (along "x1") <> byVar "x2" [0, 1]
      length (lineLayersOf spec) `shouldBe` 2
      length (bandLayersOf spec) `shouldBe` 2

    it "byVar x2 [0,1]: 曲線は x2=0→3+2x1, x2=1→6+2x1 (x3 は Mean・順序保持)" $ do
      let spec    = statModelMulti mlm (along "x1") <> grid 5 <> byVar "x2" [0, 1]
          ls      = lineLayersOf spec
          [l0, l1] = ls
      length ls `shouldBe` 2
      numYof l0 `shouldSatisfy` allClose (map (\x -> 3 + 2 * x) (numXof l0))
      numYof l1 `shouldSatisfy` allClose (map (\x -> 6 + 2 * x) (numXof l1))

    it "holdAt Marginalize: band 無し (MLine のみ)・PDP = 4.5+2x1 (線形ゆえ Mean と一致)" $ do
      let spec = statModelMulti mlm (along "x1") <> grid 6 <> holdAt Marginalize
      kindsMV spec `shouldBe` [Just MLine]
      numYof (firstLine spec)
        `shouldSatisfy` allClose (map (\x -> 4.5 + 2 * x) (numXof (firstLine spec)))

    it "bandMode BandOff: MLine のみ" $ do
      kindsMV (statModelMulti mlm (along "x1") <> bandMode BandOff) `shouldBe` [Just MLine]

    it "多変量 GLM effect (Poisson/Log, 既定 帯 ON): μ 曲線 + 非対称帯 (MBand + MLine)・μ̂ > 0 単調増" $ do
      let glm  = either error id (multiGLMModel Poisson Log "y ~ x1 + x2" dfMV)
          spec = statModelMulti glm (along "x1") <> grid 8
          l    = firstLine spec
          ys'  = numYof l
      kindsMV spec `shouldMatchList` [Just MBand, Just MLine]
      all (> 0) ys' `shouldBe` True
      -- Log リンク + 正係数 → x1 に対し μ̂ 単調増
      and (zipWith (<=) ys' (tail ys')) `shouldBe` True

  describe "Phase 49 A1: hbmModel (HBM 学習 = 列名 bind + 並列 multi-chain)" $ do
    -- y = 1 + 2x + 小さな決定論的ゆらぎ を生成し、 線形 HBM で a≈1, b≈2 を復元する。
    -- (完全無ノイズだと σ→0 で尤度が発散し NUTS が荒れるため微小ゆらぎを足す)。
    let xdat  = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] :: [Double]
        noise = [0.12, -0.08, 0.05, -0.15, 0.10, 0.03, -0.07, 0.14, -0.04, 0.06]
        ydat  = zipWith (\x e -> 1 + 2 * x + e) xdat noise
        cfg   = defaultHBM { hbmChains = 2, hbmSamples = 500, hbmWarmup = 500
                           , hbmSeed = Just 20260605 }

    it "列名で withData 自動 bind し multi-chain で学習・posterior が真値を復元" $ do
      studied <- hbmModel cfg hbmLinModel [("x", xdat), ("y", ydat)]
      -- chain 数 = config 通り。
      length (hbmChainsR studied) `shouldBe` 2
      -- bind 済みデータが保持されている。
      hbmData studied `shouldBe` [("x", xdat), ("y", ydat)]
      -- 事後平均が真値 a=1, b=2 を概ね復元 (緩い許容)。
      let aHat = posteriorMeanOf "a" (hbmChainsR studied)
          bHat = posteriorMeanOf "b" (hbmChainsR studied)
      abs (aHat - 1) `shouldSatisfy` (< 0.5)
      abs (bHat - 2) `shouldSatisfy` (< 0.2)

    it "各 chain の draw 数 = hbmSamples (post-warmup)" $ do
      studied <- hbmModel cfg hbmLinModel [("x", xdat), ("y", ydat)]
      map (length . chainSamples) (hbmChainsR studied)
        `shouldBe` [500, 500]

  describe "Phase 49 A2 / 74: tracesOf + forestOf (HBM 出力抽出子)" $ do
    let xdat = [1, 2, 3, 4, 5, 6, 7, 8] :: [Double]
        ydat = zipWith (\x e -> 1 + 2 * x + e) xdat
                 [0.1, -0.05, 0.08, -0.1, 0.05, -0.03, 0.07, -0.06]
        -- A2 は構造検証ゆえ軽い設定で十分。
        cfgS = defaultHBM { hbmChains = 2, hbmSamples = 60, hbmWarmup = 60
                          , hbmSeed = Just 49 }

    it "tracesOf: latent パラメータ数 (a,b,s = 3) 個の VisualSpec" $ do
      studied <- hbmModel cfgS hbmLinModel [("x", xdat), ("y", ydat)]
      hbmParamNames studied `shouldMatchList` ["a", "b", "s"]
      length (tracesOf studied) `shouldBe` 3

    it "tracesOf: merged trace の頂点数 = 全 chain 連結の draw 総数 (2×60=120)" $ do
      studied <- hbmModel cfgS hbmLinModel [("x", xdat), ("y", ydat)]
      -- 既定は merged + rug。trace 層は先頭 (rug は分岐後)。divergence 無しでも先頭は trace。
      let vs      = head (tracesOf studied)
          tLayer  = head (vsLayers vs)
      case getLast (lyEncX tLayer) of
        Just (ColNum vx) -> V.length vx `shouldBe` 120
        _                -> expectationFailure "trace encX が inline ColNum でない"
      getFirst (lyKind tLayer) `shouldBe` Just MTrace

    it "forestOf: MForest layer・点 3 個 (= パラメータ数)・誤差半幅 ≥ 0" $ do
      studied <- hbmModel cfgS hbmLinModel [("x", xdat), ("y", ydat)]
      let fLayer = head (vsLayers (toPlot (forestOf studied)))
      getFirst (lyKind fLayer) `shouldBe` Just MForest
      case (getLast (lyEncX fLayer), getLast (lyErrorX fLayer)) of
        (Just (ColNum ests), Just (ColNum errs)) -> do
          V.length ests `shouldBe` 3
          V.length errs `shouldBe` 3
          V.toList errs `shouldSatisfy` all (>= 0)
        _ -> expectationFailure "forest encX/errorX が inline ColNum でない"

    -- Phase 52.B2: marginalsOf = 周辺事後密度を per-param で list 返し。
    it "marginalsOf: latent 数 (3) 個・各図 density layer 1 枚 + title=param 名" $ do
      studied <- hbmModel cfgS hbmLinModel [("x", xdat), ("y", ydat)]
      let ms = marginalsOf studied
      length ms `shouldBe` 3
      -- 各図のタイトル = パラメータ名 (a,b,s)
      [ t | s <- ms, Just t <- [getLast (vsTitle s)] ]
        `shouldMatchList` ["a", "b", "s"]
      -- 各図は density (MDensity) layer のみ
      [ getFirst (lyKind l) | s <- ms, l <- vsLayers s ]
        `shouldSatisfy` all (== Just MDensity)

  describe "Phase 49 A3: epred (事後予測平均 + HDI band・O1 規約)" $ do
    -- y = 1 + 2x + 微小ゆらぎ。 epred の事後平均線が真値 1+2x を復元するか検証。
    let xdat = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] :: [Double]
        ydat = zipWith (\x e -> 1 + 2 * x + e) xdat
                 [0.12, -0.08, 0.05, -0.15, 0.10, 0.03, -0.07, 0.14, -0.04, 0.06]
        cfg  = defaultHBM { hbmChains = 2, hbmSamples = 400, hbmWarmup = 400
                          , hbmSeed = Just 4903 }
        lineLayerOf spec = head [ l | l <- vsLayers (toPlot spec)
                                    , getFirst (lyKind l) == Just MLine ]
        numX l = case getLast (lyEncX l) of
                   Just (ColNum v) -> V.toList v
                   _               -> error "encX が ColNum でない"
        numY l = case getLast (lyEncY l) of
                   Just (ColNum v) -> V.toList v
                   _               -> error "encY が ColNum でない"

    it "既定は band (MBand) + 事後平均線 (MLine) の 2 layer・grid 100 点" $ do
      studied <- hbmModel cfg hbmEpredModel [("x", xdat), ("y", ydat)]
      let ls = vsLayers (toPlot (epred studied "x" "mu"))
      map (getFirst . lyKind) ls `shouldMatchList` [Just MBand, Just MLine]
      length (numX (lineLayerOf (epred studied "x" "mu"))) `shouldBe` 100

    it "grid n / gridRange が grid を制御 (既定範囲 = 予測子 min/max = [1,10])" $ do
      studied <- hbmModel cfg hbmEpredModel [("x", xdat), ("y", ydat)]
      let xg = numX (lineLayerOf (epred studied "x" "mu" <> grid 21))
      length xg `shouldBe` 21
      head xg `shouldSatisfy` (\v -> abs (v - 1) < 1e-9)
      last xg `shouldSatisfy` (\v -> abs (v - 10) < 1e-9)

    -- epred は HDI 帯を本体として焼き込む (帯既定 ON・bandOff でも消えない)。

    it "事後平均線が真値 1+2x を概ね復元 (grid 上で平均絶対誤差 < 0.4)" $ do
      studied <- hbmModel cfg hbmEpredModel [("x", xdat), ("y", ydat)]
      let l    = lineLayerOf (epred studied "x" "mu" <> grid 10)
          errs = zipWith (\x mu -> abs (mu - (1 + 2 * x))) (numX l) (numY l)
          mae  = sum errs / fromIntegral (length errs)
      mae `shouldSatisfy` (< 0.4)

    it "epredAt: HDI 幅は level とともに単調増 (0.5 < 0.94 < 0.99)" $ do
      studied <- hbmModel cfg hbmEpredModel [("x", xdat), ("y", ydat)]
      let widthAt lvl =
            let (_, (lo, hi)) = epredAt studied "x" "mu" lvl 5.0 in hi - lo
          w50 = widthAt 0.50
          w94 = widthAt 0.94
          w99 = widthAt 0.99
      w50 `shouldSatisfy` (< w94)
      w94 `shouldSatisfy` (< w99)

    it "epredAt: 事後平均は線層の対応点と一致 (= 同じ評価核)" $ do
      studied <- hbmModel cfg hbmEpredModel [("x", xdat), ("y", ydat)]
      let (mean5, _) = epredAt studied "x" "mu" 0.94 5.0
      abs (mean5 - (1 + 2 * 5)) `shouldSatisfy` (< 0.4)

  -- Phase 74: epred の多予測子 hold (holdAt / byVar)。 mu = a + b*x1 + c*x2 で
  -- 非軸 x2 を固定値/水準別に動かし、 既存 holdAt/byVar (頻度論と同綴り) が効くか検証。
  describe "Phase 74: epred holdAt / byVar (多予測子 hold)" $ do
    let x1d = [1, 2, 3, 4, 5, 6, 7, 8] :: [Double]
        x2d = [2, 1, 3, 2, 4, 3, 5, 4] :: [Double]
        y2d = zipWith3 (\a b e -> 1 + 2 * a + 3 * b + e) x1d x2d
                [0.05, -0.04, 0.03, -0.06, 0.02, 0.01, -0.03, 0.04]
        cfg2 = defaultHBM { hbmChains = 2, hbmSamples = 400, hbmWarmup = 400
                          , hbmSeed = Just 7402 }
        lineLayerOf spec = head [ l | l <- vsLayers (toPlot spec)
                                    , getFirst (lyKind l) == Just MLine ]
        mlines spec = [ l | l <- vsLayers (toPlot spec), getFirst (lyKind l) == Just MLine ]
        numY l = case getLast (lyEncY l) of
                   Just (ColNum v) -> V.toList v
                   _               -> error "encY が ColNum でない"

    it "holdAt (Fixed): x2 を Δ=4 上げると曲線が c*Δ ≈ 12 だけ一様に上シフト" $ do
      m <- hbmModel cfg2 hbmEpred2Model [("x1", x1d), ("x2", x2d), ("y", y2d)]
      let muAt v = numY (lineLayerOf (epred m "x1" "mu" <> holdAt (Fixed [("x2", v)]) <> grid 6))
          diffs  = zipWith (-) (muAt 4) (muAt 0)
      -- c ≈ 3・Δx2 = 4 ゆえ全 grid 点で ≈ 12 の一様シフト (傾きは x1 のまま不変)。
      diffs `shouldSatisfy` all (\d -> abs (d - 12) < 1.5)

    it "holdAt 既定 (Mean) = Fixed mean(x2) (非軸の既定は head でなく中央化された Mean)" $ do
      m <- hbmModel cfg2 hbmEpred2Model [("x1", x1d), ("x2", x2d), ("y", y2d)]
      let meanX2 = sum x2d / fromIntegral (length x2d)
          dflt = numY (lineLayerOf (epred m "x1" "mu" <> grid 6))
          fixd = numY (lineLayerOf (epred m "x1" "mu" <> holdAt (Fixed [("x2", meanX2)]) <> grid 6))
      zip dflt fixd `shouldSatisfy` all (\(a, b) -> abs (a - b) < 1e-9)

    it "byVar: x2 の水準数だけ曲線 (MLine) が出て、 水準が高いほど mu 大" $ do
      m <- hbmModel cfg2 hbmEpred2Model [("x1", x1d), ("x2", x2d), ("y", y2d)]
      let spec = epred m "x1" "mu" <> byVar "x2" [0, 4] <> grid 6
          ls   = mlines spec
          midOf l = numY l !! 3
      length ls `shouldBe` 2
      midOf (ls !! 1) `shouldSatisfy` (> midOf (head ls))

  -- Phase 74.5: epred の予測区間 (PI) 帯。 bandMode で CI (μ HDI) / PI (観測ノイズ込み) /
  -- CIPI (入れ子) を切替 (頻度論 statModel と同綴り)。 PI は観測分布サンプルゆえ固定 seed で
  -- 決定的。 noise を大きめにして PI が CI を有意に上回る (s が効く) ことを検証可能にする。
  describe "Phase 74.5: epred bandMode (CI / PI / CIPI)" $ do
    let xdat = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] :: [Double]
        ydat = zipWith (\x e -> 1 + 2 * x + e) xdat
                 [0.4, -0.3, 0.5, -0.5, 0.3, 0.2, -0.4, 0.5, -0.2, 0.3]
        cfg  = defaultHBM { hbmChains = 2, hbmSamples = 400, hbmWarmup = 400
                          , hbmSeed = Just 4903 }
        bandsOf spec = [ l | l <- vsLayers (toPlot spec), getFirst (lyKind l) == Just MBand ]
        kindsOf spec = map (getFirst . lyKind) (vsLayers (toPlot spec))
        loY l = case getLast (lyEncY l)  of { Just (ColNum v) -> V.toList v; _ -> error "encY" }
        hiY l = case getLast (lyEncY2 l) of { Just (ColNum v) -> V.toList v; _ -> error "encY2" }
        widthsOf l = zipWith (-) (hiY l) (loY l)

    it "既定 (bandMode 無し) = bandMode BandCI とバイト一致 (後方互換)" $ do
      m <- hbmModel cfg hbmEpredModel [("x", xdat), ("y", ydat)]
      let dflt = bandsOf (epred m "x" "mu" <> grid 8)
          ci   = bandsOf (epred m "x" "mu" <> grid 8 <> bandMode BandCI)
      map loY dflt `shouldBe` map loY ci
      map hiY dflt `shouldBe` map hiY ci

    it "BandPI: PI 帯は CI 帯を全 grid 点で包含し、 幅が広い (観測 σ ぶん)" $ do
      m <- hbmModel cfg hbmEpredModel [("x", xdat), ("y", ydat)]
      let [ciB] = bandsOf (epred m "x" "mu" <> grid 8 <> bandMode BandCI)
          [piB] = bandsOf (epred m "x" "mu" <> grid 8 <> bandMode BandPI)
      and (zipWith (<=) (loY piB) (loY ciB)) `shouldBe` True
      and (zipWith (>=) (hiY piB) (hiY ciB)) `shouldBe` True
      and (zipWith (>)  (widthsOf piB) (widthsOf ciB)) `shouldBe` True

    it "BandCIPI: MBand 2 (外 PI + 内 CI) + MLine 1、 PI が外側 (包含)" $ do
      m <- hbmModel cfg hbmEpredModel [("x", xdat), ("y", ydat)]
      let spec  = epred m "x" "mu" <> grid 8 <> bandMode BandCIPI
          bands = bandsOf spec
      kindsOf spec `shouldMatchList` [Just MBand, Just MBand, Just MLine]
      case bands of
        [pib, cib] -> do
          and (zipWith (<=) (loY pib) (loY cib)) `shouldBe` True
          and (zipWith (>=) (hiY pib) (hiY cib)) `shouldBe` True
        _ -> expectationFailure "MBand が 2 本でない"

    it "BandOff: 帯が消え MLine 1 layer のみ" $ do
      m <- hbmModel cfg hbmEpredModel [("x", xdat), ("y", ydat)]
      kindsOf (epred m "x" "mu" <> grid 8 <> bandMode BandOff) `shouldBe` [Just MLine]

    it "BandPI: PI 帯は妥当な区間 (全 grid 点で lo < hi・grid 点数一致)" $ do
      m <- hbmModel cfg hbmEpredModel [("x", xdat), ("y", ydat)]
      let [piB] = bandsOf (epred m "x" "mu" <> grid 6 <> bandMode BandPI)
      length (widthsOf piB) `shouldBe` 6
      widthsOf piB `shouldSatisfy` all (> 0)

  -- Phase 74.8: 診断ダッシュボード (抽出子を subplots で束ねる便宜関数)。
  describe "Phase 74.8: dashboardOf / dashboardFullOf" $ do
    let xdat = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] :: [Double]
        ydat = zipWith (\x e -> 1 + 2 * x + e) xdat
                 [0.4, -0.3, 0.5, -0.5, 0.3, 0.2, -0.4, 0.5, -0.2, 0.3]
        cfg  = defaultHBM { hbmChains = 2, hbmSamples = 300, hbmWarmup = 300
                          , hbmSeed = Just 4903 }

    it "dashboardOf: 2×2 = 4 パネル (構造 / 推定値 / 当てはまり / サンプラ健全性)" $ do
      m <- hbmModel cfg hbmEpredModel [("x", xdat), ("y", ydat)]
      length (vsSubplots (dashboardOf m "obs")) `shouldBe` 4

    it "dashboardFullOf: 健全性 4 + param ごと [事後分布,trace] 2×3 = 10 パネル" $ do
      m <- hbmModel cfg hbmEpredModel [("x", xdat), ("y", ydat)]
      -- a,b,s の 3 param ゆえ 4 + 2*3 = 10。 係数が増えると下に行 (2 パネル) ずつ増える。
      length (vsSubplots (dashboardFullOf m "obs")) `shouldBe` 10

    it "traceDensityOf: param ごと [事後分布,trace] = 2*3 = 6 パネル" $ do
      m <- hbmModel cfg hbmEpredModel [("x", xdat), ("y", ydat)]
      length (vsSubplots (traceDensityOf m)) `shouldBe` 6

  describe "Phase 49 A4: ppcOf (事後予測チェック = ArviZ kde overlay)" $ do
    -- y = 1 + 2x + 微小ゆらぎ。 ppc は観測 (黒) + 各 draw の y_rep 群 (青) を重ねる
    -- (Phase 74.10: プール赤線は KDE バンド幅が n 依存で誤解を招くため削除)。
    let xdat = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] :: [Double]
        ydat = zipWith (\x e -> 1 + 2 * x + e) xdat
                 [0.12, -0.08, 0.05, -0.15, 0.10, 0.03, -0.07, 0.14, -0.04, 0.06]
        cfg  = defaultHBM { hbmChains = 2, hbmSamples = 300, hbmWarmup = 300
                          , hbmSeed = Just 7711 }
        ppcCfg = defaultPPC { ppcReps = 10, ppcSeed = Just 2024 }
        numX l = case getLast (lyEncX l) of
                   Just (ColNum v) -> V.toList v
                   _               -> error "encX が ColNum でない"
        meanOf zs = sum zs / fromIntegral (length zs)
        varOf zs  = let m = meanOf zs
                    in sum [ (z - m) ^ (2 :: Int) | z <- zs ]
                         / fromIntegral (length zs)

    it "層 = y_rep 10 本 + 観測 = 11 層・全て MDensity (プール線なし)" $ do
      studied <- hbmModel cfg hbmLinModel [("x", xdat), ("y", ydat)]
      sp <- ppcOfWithIO ppcCfg studied "obs"
      let ls = vsLayers (toPlot sp)
      length ls `shouldBe` 11
      map (getFirst . lyKind) ls `shouldSatisfy` all (== Just MDensity)

    it "ppcCumulative で全層 MEcdf に切り替わる" $ do
      studied <- hbmModel cfg hbmLinModel [("x", xdat), ("y", ydat)]
      sp <- ppcOfWithIO ppcCfg { ppcCumulative = True } studied "obs"
      let ls = vsLayers (toPlot sp)
      map (getFirst . lyKind) ls `shouldSatisfy` all (== Just MEcdf)

    it "観測層 (最後) の encX = 実 y データ (10 点)" $ do
      studied <- hbmModel cfg hbmLinModel [("x", xdat), ("y", ydat)]
      sp <- ppcOfWithIO ppcCfg studied "obs"
      let ls  = vsLayers (toPlot sp)
          obs = numX (last ls)
      obs `shouldSatisfy` (\v -> length v == 10
                                   && all (\(a, b) -> abs (a - b) < 1e-9) (zip v ydat))

    it "各 draw の y_rep 層 (先頭 = 1 draw) の平均/分散が観測と整合" $ do
      studied <- hbmModel cfg hbmLinModel [("x", xdat), ("y", ydat)]
      sp <- ppcOfWithIO ppcCfg studied "obs"
      let ls   = vsLayers (toPlot sp)
          yrep = numX (head ls)   -- 先頭 = 1 つの draw の y_rep 層 (n=n_obs・観測と同条件)
          obs  = numX (last ls)
      -- 事後予測が観測の中心・広がりを再現 (緩い許容)。
      abs (meanOf yrep - meanOf obs) `shouldSatisfy` (< 2.0)
      (varOf yrep / varOf obs) `shouldSatisfy` (\r -> r > 0.5 && r < 2.0)

    it "ppcReps が draw 総数を超えても全 draw 数で頭打ち (層 = draw+1)" $ do
      let smallCfg = defaultHBM { hbmChains = 1, hbmSamples = 5, hbmWarmup = 50
                                , hbmSeed = Just 5 }
      studied <- hbmModel smallCfg hbmLinModel [("x", xdat), ("y", ydat)]
      sp <- ppcOfWithIO defaultPPC { ppcReps = 1000, ppcSeed = Just 1 } studied "obs"
      length (vsLayers (toPlot sp)) `shouldBe` 5 + 1

  describe "Phase 49 A5: dagOf (モデル構造 DAG = buildModelGraph 橋渡し)" $ do
    -- dagOf は構造のみ (学習不要)。 data を withData で bind した spec を手で組み、
    -- chains 空の HBMModel を作って DAG を取り出す (NUTS を回さず高速)。
    let xdat = [1, 2, 3, 4] :: [Double]
        ydat = [3, 5, 7, 9] :: [Double]
        boundLin :: ModelP ()
        boundLin = withData "x" xdat (withData "y" ydat hbmLinModel)
        linDagM  = HBMModel { hbmModelSpec = boundLin
                            , hbmChainsR = [], hbmData = []
                            , hbmFactorLevels = [] }
        plateDagM = HBMModel { hbmModelSpec = hbmPlateModel
                             , hbmChainsR = [], hbmData = []
                             , hbmFactorLevels = [] }
        -- Phase 59.3: dagOf は collapse 済が既定、 旧挙動 (indexed 個別) は dagOfRaw
        dsOf hm = case getLast (lyDAG (head (vsLayers (toPlot (dagOf hm))))) of
                    Just ds -> ds
                    Nothing -> error "dagOf に lyDAG が無い"
        dsOfRaw hm = case getLast (lyDAG (head (vsLayers (toPlot (dagOfRaw hm))))) of
                       Just ds -> ds
                       Nothing -> error "dagOfRaw に lyDAG が無い"

    it "MDAG layer 1 枚 (mark = MDAG)" $ do
      let ls = vsLayers (toPlot (dagOf linDagM))
      length ls `shouldBe` 1
      getFirst (lyKind (head ls)) `shouldBe` Just MDAG

    -- Phase 60.4: dataNamedX/dataNamedObs slot (x, y) が NodeData として DAG に
    -- 出るようになった (pm.Data parity・既定 ON)。 x は mu 経由で obs への
    -- エッジを持つが、 y (dataNamedObs = 生 [Double] view) はエッジなし。
    it "線形モデル (collapsed 既定): a,b,s,obs + data x,y = 6 node" $ do
      let ds    = dsOf linDagM
          names = map dnId (dsNodes ds)
      length (dsNodes ds) `shouldBe` 6
      names `shouldSatisfy` (\ns -> all (`elem` ns) ["a", "b", "s", "obs", "x", "y"])
      length (filter (\n -> dnKind n == NodeLatent) (dsNodes ds)) `shouldBe` 3
      length (filter (\n -> dnKind n == NodeObserved) (dsNodes ds)) `shouldBe` 1
      length (filter (\n -> dnKind n == NodeData) (dsNodes ds)) `shouldBe` 2

    it "線形モデル (collapsed 既定): edge は (a,b,s,x) → obs の 4 本" $ do
      let ds  = dsOf linDagM
          es  = [ (deFrom e, deTo e) | e <- dsEdges ds ]
      length es `shouldBe` 4
      es `shouldSatisfy` (("a", "obs") `elem`)
      es `shouldSatisfy` (("s", "obs") `elem`)
      es `shouldSatisfy` (("x", "obs") `elem`)

    it "線形モデル (dagOfRaw): latent 3 + obs_0..3 + data 2 = 9 node" $ do
      let ds    = dsOfRaw linDagM
          names = map dnId (dsNodes ds)
      length (dsNodes ds) `shouldBe` 9
      names `shouldSatisfy` (\ns -> all (`elem` ns) ["a", "b", "s", "x", "y"])
      length (filter (\n -> dnKind n == NodeLatent) (dsNodes ds)) `shouldBe` 3
      length (filter (\n -> dnKind n == NodeObserved) (dsNodes ds)) `shouldBe` 4
      length (filter (\n -> dnKind n == NodeData) (dsNodes ds)) `shouldBe` 2

    it "線形モデル (dagOfRaw): edge は (a,b,s,x) → 各 obs (4×4 = 16 本)" $ do
      let ds  = dsOfRaw linDagM
          es  = [ (deFrom e, deTo e) | e <- dsEdges ds ]
      length es `shouldBe` 16
      es `shouldSatisfy` (("a", "obs_0") `elem`)
      es `shouldSatisfy` (("s", "obs_3") `elem`)
      es `shouldSatisfy` (("x", "obs_0") `elem`)

    it "線形モデル (plate 無し): dsPlates は空 (collapsed/raw とも)" $ do
      dsPlates (dsOf linDagM) `shouldBe` []
      dsPlates (dsOfRaw linDagM) `shouldBe` []

    it "plate モデル (collapsed 既定): eta_0..3/y_0..3 が eta/y に畳まれる" $ do
      let ds    = dsOf plateDagM
          names = map dnId (dsNodes ds)
      length (dsNodes ds) `shouldBe` 4   -- mu, tau, eta, y
      names `shouldSatisfy` (\ns -> all (`elem` ns) ["mu", "tau", "eta", "y"])
      length (dsPlates ds) `shouldBe` 1
      let p = head (dsPlates ds)
      dpLabel p `shouldBe` "g (4)"
      dpNodeIds p `shouldSatisfy` ("eta" `elem`)

    it "plate モデル (dagOfRaw): dsPlates に \"g (4)\" が 1 個・eta_0 を含む" $ do
      let ds = dsOfRaw plateDagM
      length (dsPlates ds) `shouldBe` 1
      let p = head (dsPlates ds)
      dpLabel p `shouldBe` "g (4)"
      dpNodeIds p `shouldSatisfy` ("eta_0" `elem`)

    it "plate モデル: 分布名が dnDist に入る (mu ~ Normal)" $ do
      let ds = dsOf plateDagM
      case filter (\n -> dnId n == "mu") (dsNodes ds) of
        [n] -> dnDist n `shouldBe` Just "Normal"
        _   -> expectationFailure "mu node not found"

  -- Phase 74.9: 学習前 DAG (ModelP 直接・サンプリングなし)。
  describe "Phase 74.9: dagOfModel / dagOfModelWith" $ do
    let xdat = [1, 2, 3, 4] :: [Double]
        ydat = [3, 5, 7, 9] :: [Double]
        dsD d = case getLast (lyDAG (head (vsLayers (toPlot d)))) of
                  Just ds -> ds
                  Nothing -> error "DAG に lyDAG が無い"

    it "dagOfModelWith: データ束ねで data 駆動 plate の全ノードが出る (6 node・NUTS なし)" $ do
      let ds    = dsD (dagOfModelWith [("x", xdat), ("y", ydat)] hbmLinModel)
          names = map dnId (dsNodes ds)
      length (dsNodes ds) `shouldBe` 6
      names `shouldSatisfy` (\ns -> all (`elem` ns) ["a", "b", "s", "obs", "x", "y"])

    it "dagOfModel: 既に withData 束ね済みモデルなら dagOf と同一構造 (6 node)" $ do
      let bound :: ModelP ()
          bound = withData "x" xdat (withData "y" ydat hbmLinModel)
          ds    = dsD (dagOfModel bound)
      length (dsNodes ds) `shouldBe` 6

    it "dagOfModel: 未束縛 (slot []) は data 駆動 plate 本体が出ない (caveat: obs 無し)" $ do
      let ds    = dsD (dagOfModel hbmLinModel)
          names = map dnId (dsNodes ds)
      -- a,b,s + data x,y は出るが、 ループ本体 (obs) は 0 反復ゆえ出ない。
      names `shouldSatisfy` (\ns -> notElem "obs" ns)
      names `shouldSatisfy` (\ns -> all (`elem` ns) ["a", "b", "s"])

  describe "Phase 59.4 / 74: divergencesOf / tracesOfWith byChain+divergence (divergence 診断)" $ do
    -- fake chain で offset 規約を決定的に検証 (chainDivergences = chain 内 0-based
    -- post-burn-in index、 root: request/255 §4。 NUTS を回さない)
    let muModel :: ModelP ()
        muModel = do
          _ <- sample "mu" (Normal 0 1)
          pure ()
        mkCh vals divs = Chain { chainSamples     = [ Map.singleton "mu" v | v <- vals ]
                               , chainAccepted    = 0
                               , chainTotal       = length vals
                               , chainEnergy      = map (* 2) vals
                               , chainDivergences = divs
                               , chainTreeDepths  = [] }
        ch1 = mkCh [1, 2, 3] [0, 2]   -- 3 draws・div = 0,2
        ch2 = mkCh [4, 5] [1]         -- 2 draws・div = 1
        divM   = HBMModel { hbmModelSpec = muModel
                          , hbmChainsR = [ch1, ch2], hbmData = []
                          , hbmFactorLevels = [] }
        cleanM = HBMModel { hbmModelSpec = muModel
                          , hbmChainsR = [mkCh [1, 2, 3] [], mkCh [4, 5] []]
                          , hbmData = [], hbmFactorLevels = [] }

    it "divergencesOf: chain offset 加算の通し index ([0,2] ++ map (+3) [1] = [0,2,4])" $ do
      divergencesOf divM `shouldBe` [0, 2, 4]

    it "divergencesOf: divergence 無しなら空" $ do
      divergencesOf cleanM `shouldBe` []

    -- Phase 74: 旧 tracesWithDivergencesOf = tracesOfWith (byChain + divergence ON)。
    let byChainDiv = tracesOfWith defaultTraceOpts { toByChain = True }

    it "tracesOfWith byChain+div: trace 2 層 (chain 別) + rug 1 層 (MLineRange 縦棒)" $ do
      let [vs] = byChainDiv divM
          ls   = vsLayers vs
      length ls `shouldBe` 3
      -- Phase 60.5: rug は scatter の点から lineRange の縦棒へ (ArviZ tick 同型)
      map (getFirst . lyKind) ls `shouldBe` [Just MTrace, Just MTrace, Just MLineRange]

    it "tracesOfWith byChain+div: rug の x = chain 内 1-based iteration・縦棒 = 下端から値域 2%" $ do
      let [vs] = byChainDiv divM
          rug  = last (vsLayers vs)
      case (getLast (lyEncX rug), getLast (lyEncY rug), getLast (lyErrorY rug)) of
        (Just (ColNum vx), Just (ColNum vy), Just (ColNum ve)) -> do
          V.toList vx `shouldBe` [1, 3, 2]   -- ch1: 0,2 → 1,3 / ch2: 1 → 2
          -- lineRange は (x, 中心 y, ±err)。 divM の mu は 1..5 → 値域 4・
          -- tick = 4*0.02 = 0.08。 中心 = 1 + 0.04、 err = 0.04
          V.toList vy `shouldSatisfy` all (\v -> abs (v - 1.04) < 1e-9)
          V.toList ve `shouldSatisfy` all (\v -> abs (v - 0.04) < 1e-9)
        _ -> expectationFailure "rug の encX/encY/errorY が inline ColNum でない"

    it "tracesOfWith byChain+div: divergence 無しなら rug 層なし" $ do
      let [vs] = byChainDiv cleanM
      map (getFirst . lyKind) (vsLayers vs) `shouldBe` [Just MTrace, Just MTrace]

    it "tracesOf 既定 (merged + div): merged trace 1 層 + rug 1 層" $ do
      let [vs] = tracesOf divM
          ls   = vsLayers vs
      -- merged は単線 trace 1 層 + rug 1 層
      map (getFirst . lyKind) ls `shouldBe` [Just MTrace, Just MLineRange]

    it "tracesOfWith divergence OFF: rug 層なし (merged)" $ do
      let [vs] = tracesOfWith defaultTraceOpts { toShowDivergences = False } divM
      map (getFirst . lyKind) (vsLayers vs) `shouldBe` [Just MTrace]

  describe "Phase 59.5: pairOf (joint 散布 + 発散強調)" $ do
    let abModel :: ModelP ()
        abModel = do
          _ <- sample "a" (Normal 0 1)
          _ <- sample "b" (Normal 0 1)
          pure ()
        mkCh2 avs bvs divs = Chain
          { chainSamples     = [ Map.fromList [("a", av), ("b", bv)]
                               | (av, bv) <- zip avs bvs ]
          , chainAccepted    = 0
          , chainTotal       = length avs
          , chainEnergy      = []
          , chainDivergences = divs }
        ch1 = mkCh2 [10, 11, 12] [20, 21, 22] [2]   -- div: chain 内 2 → 通し 2
        ch2 = mkCh2 [13, 14] [23, 24] [0]           -- div: chain 内 0 → 通し 3
        m2  = HBMModel { hbmModelSpec = abModel
                       , hbmChainsR = [ch1, ch2], hbmData = []
                       , hbmFactorLevels = [] }

    it "pairOf: 図数 = ペア数・layer = base + 強調の MScatter 2 層" $ do
      let vss = pairOf m2 [("a", "b")]
      length vss `shouldBe` 1
      map (getFirst . lyKind) (vsLayers (head vss))
        `shouldBe` [Just MScatter, Just MScatter]

    it "pairOf: 強調点 = 通し index の draw (chain 跨ぎ x=[12,13] y=[22,23])" $ do
      let [vs] = pairOf m2 [("a", "b")]
          ov   = last (vsLayers vs)
      case (getLast (lyEncX ov), getLast (lyEncY ov)) of
        (Just (ColNum vx), Just (ColNum vy)) -> do
          V.toList vx `shouldBe` [12, 13]
          V.toList vy `shouldBe` [22, 23]
        _ -> expectationFailure "強調層の encX/encY が inline ColNum でない"

    it "pairOf: divergence 無しなら base 層のみ" $ do
      let m0   = HBMModel { hbmModelSpec = abModel
                          , hbmChainsR = [mkCh2 [1] [2] []], hbmData = []
                          , hbmFactorLevels = [] }
          [vs] = pairOf m0 [("a", "b")]
      map (getFirst . lyKind) (vsLayers vs) `shouldBe` [Just MScatter]

  describe "Phase 59.6: energyOf (marginal vs ΔE energy 密度)" $ do
    let muModel :: ModelP ()
        muModel = do
          _ <- sample "mu" (Normal 0 1)
          pure ()
        mkEnCh es = Chain { chainSamples     = [ Map.singleton "mu" e | e <- es ]
                          , chainAccepted    = 0
                          , chainTotal       = length es
                          , chainEnergy      = es
                          , chainDivergences = []
                          , chainTreeDepths  = [] }
        es1 = [10, 13, 11, 15, 12, 14, 16, 11, 13, 12]
        enM = HBMModel { hbmModelSpec = muModel
                       , hbmChainsR = [mkEnCh es1, mkEnCh (map (+ 1) es1)]
                       , hbmData = [], hbmFactorLevels = [] }
        noEnM = HBMModel { hbmModelSpec = muModel
                         , hbmChainsR =
                             [ Chain [Map.singleton "mu" 1] 0 1 [] [] [] ]
                         , hbmData = [], hbmFactorLevels = [] }

    it "energyOf: marginal + ΔE の MLine 2 層 (KDE 200 点)" $ do
      let vs = energyOf enM
          ls = vsLayers vs
      map (getFirst . lyKind) ls `shouldBe` [Just MLine, Just MLine]
      case getLast (lyEncX (head ls)) of
        Just (ColNum vx) -> V.length vx `shouldBe` 200
        _ -> expectationFailure "encX が inline ColNum でない"

    it "energyOf: energy 記録なし (MH 等) なら layer 0 (空図)" $ do
      length (vsLayers (energyOf noEnM)) `shouldBe` 0

  describe "Phase 73.1: autocorrOf (自己相関・ArviZ plot_autocorr)" $ do
    let muModel :: ModelP ()
        muModel = sample "mu" (Normal 0 1) >> pure ()
        mkAcCh xs = Chain { chainSamples     = [ Map.singleton "mu" x | x <- xs ]
                          , chainAccepted    = 0
                          , chainTotal       = length xs
                          , chainEnergy      = []
                          , chainDivergences = []
                          , chainTreeDepths  = [] }
        acXs = [ sin (0.3 * fromIntegral i) | i <- [0 .. 49 :: Int] ]   -- 自己相関のある系列
        acM  = HBMModel { hbmModelSpec = muModel
                        , hbmChainsR = [mkAcCh acXs, mkAcCh (map (* 0.9) acXs)]
                        , hbmData = [], hbmFactorLevels = [] }

    it "autocorrOf: param ごと 1 図・MBar 層" $ do
      let specs = autocorrOf acM
      length specs `shouldBe` 1                                   -- "mu" の 1 つ
      map (getFirst . lyKind) (vsLayers (head specs)) `shouldBe` [Just MBar]

    it "autocorrOf: lag 0 の ACF == 1.0 (自己相関の定義・chain 平均)" $ do
      let l = head (vsLayers (head (autocorrOf acM)))
      case getLast (lyEncY l) of
        Just (ColNum v) -> V.head v `shouldSatisfy` (\a -> abs (a - 1.0) < 1e-9)
        _               -> expectationFailure "encY が inline ColNum でない"

    it "autocorrOfLag: 最大ラグ k なら bar は k+1 本 (lag 0..k)" $ do
      let l = head (vsLayers (head (autocorrOfLag 8 acM)))
      case getLast (lyEncX l) of
        Just (ColNum v) -> V.length v `shouldBe` 9
        _               -> expectationFailure "encX が inline ColNum でない"

  describe "Phase 73.2: rankOf (rank plot・ArviZ plot_rank)" $ do
    let muModel :: ModelP ()
        muModel = sample "mu" (Normal 0 1) >> pure ()
        mkRkCh xs = Chain { chainSamples     = [ Map.singleton "mu" x | x <- xs ]
                          , chainAccepted    = 0
                          , chainTotal       = length xs
                          , chainEnergy      = []
                          , chainDivergences = []
                          , chainTreeDepths  = [] }
        rkXs1 = [ fromIntegral i * 0.5 | i <- [0 .. 39 :: Int] ]
        rkXs2 = map (+ 0.25) rkXs1
        rk2M  = HBMModel { hbmModelSpec = muModel
                         , hbmChainsR = [mkRkCh rkXs1, mkRkCh rkXs2]
                         , hbmData = [], hbmFactorLevels = [] }
        rk1M  = HBMModel { hbmModelSpec = muModel
                         , hbmChainsR = [mkRkCh rkXs1]   -- chain 1 本
                         , hbmData = [], hbmFactorLevels = [] }

    it "rankOf: 2 chain を横並び (dodge) した MBar 1 層" $ do
      let ls = vsLayers (head (rankOf rk2M))
      length ls `shouldBe` 1
      getFirst (lyKind (head ls)) `shouldBe` Just MBar

    it "rankOf: count 総和 = 全 chain の総標本数 (2 chain × 40)" $ do
      let l0 = head (vsLayers (head (rankOf rk2M)))
      case getLast (lyEncY l0) of
        Just (ColNum v) -> round (V.sum v) `shouldBe` (80 :: Int)
        _               -> expectationFailure "encY が inline ColNum でない"

    it "rankOf: chain 1 本なら空図 (rank が自明に一様)" $ do
      length (vsLayers (head (rankOf rk1M))) `shouldBe` 0

    it "rankOfBins: ビン数 k・chain c なら long-form bar は k×c 本" $ do
      let l0 = head (vsLayers (head (rankOfBins 10 rk2M)))
      case getLast (lyEncY l0) of
        Just (ColNum v) -> V.length v `shouldBe` 20   -- 10 bins × 2 chains
        _               -> expectationFailure "encY が inline ColNum でない"

  describe "Phase 50.4: hbmModelPure / ppcOf (HBM 純粋版・正本)" $ do
    let xdat = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] :: [Double]
        ydat = zipWith (\x e -> 1 + 2 * x + e) xdat
                 [0.12,-0.08,0.05,-0.15,0.10,0.03,-0.07,0.14,-0.04,0.06]
        cfg  = defaultHBM { hbmChains = 2, hbmSamples = 300, hbmWarmup = 300
                          , hbmSeed = Just 314 }
        dat  = [("x", xdat), ("y", ydat)]
        chainsData m = map chainSamples (hbmChainsR m)

    it "hbmModelPure: IO 無しで学習でき posterior が真値 (a≈1,b≈2) を復元" $ do
      let m = hbmModelPure cfg hbmLinModel dat
      length (hbmChainsR m) `shouldBe` 2
      abs (posteriorMeanOf "a" (hbmChainsR m) - 1) `shouldSatisfy` (< 0.5)
      abs (posteriorMeanOf "b" (hbmChainsR m) - 2) `shouldSatisfy` (< 0.2)

    it "hbmModelPure: 同 config なら chainSamples がビット同一 (再現性)" $ do
      let m1 = hbmModelPure cfg hbmLinModel dat
          m2 = hbmModelPure cfg hbmLinModel dat
      chainsData m1 `shouldBe` chainsData m2

    -- Phase 61.3: IO + 進捗表示版は bind + seed 規約 (chainSeeds 共有) が
    -- 同一ゆえ純粋版とビット一致するのが設計の柱 (進捗は stderr に出る)。
    it "hbmModelIO: hbmModelPure と chainSamples ビット同一 (Phase 61.3)" $ do
      mIO <- hbmModelIO cfg hbmLinModel dat
      let mP = hbmModelPure cfg hbmLinModel dat
      chainsData mIO `shouldBe` chainsData mP

    -- Phase 61.4: IO 動詞 (|->!) = fitIO。 HBM は進捗つき学習 (ビット一致)、
    -- 純粋 spec は既定実装 (pure . fitWith) で挙動不変。
    it "df |->! hbm == df |-> hbm (chainSamples ビット同一・Phase 61.4)" $ do
      mIO <- dat |->! hbm cfg hbmLinModel
      let mP = dat |-> hbm cfg hbmLinModel
      chainsData mIO `shouldBe` chainsData mP

    it "(|->!): 純粋 spec (lm) は既定 fitIO = pure . fitWith (Phase 61.4)" $ do
      let dat61 = [("x", [1, 2, 3, 4, 5]), ("y", [3, 5, 7, 9, 11])]
                    :: [(Text, [Double])]
      m1 <- dat61 |->! lm "x" "y"
      let m2 = dat61 |-> lm "x" "y"
      LA.toList (coefficientsV (lmResult m1))
        `shouldBe` LA.toList (coefficientsV (lmResult m2))

    it "ppcOf (純粋): 層 = y_rep + 観測・全て MDensity (純粋・プール線なし)" $ do
      let m  = hbmModelPure cfg hbmLinModel dat
          sp = ppcOfWith defaultPPC { ppcReps = 8, ppcSeed = Just 1 } m "obs"
          ls = vsLayers (toPlot sp)
      length ls `shouldBe` 8 + 1
      map (getFirst . lyKind) ls `shouldSatisfy` all (== Just MDensity)

    it "ppcOf (純粋): 同 seed なら y_rep がビット同一 (再現性)" $ do
      let m   = hbmModelPure cfg hbmLinModel dat
          c   = defaultPPC { ppcReps = 8, ppcSeed = Just 7 }
          dataOf sp = [ case getLast (lyEncX l) of
                          Just (ColNum v) -> V.toList v
                          _               -> []
                      | l <- vsLayers (toPlot sp) ] :: [[Double]]
      dataOf (ppcOfWith c m "obs") `shouldBe` dataOf (ppcOfWith c m "obs")

  describe "Phase 51.2: df |-> spec (二変量近道・ColumnSource)" $ do
    -- y = 2x + 1 を assoc データ源 ([(Text,[Double])] = core instance) で渡す。
    let dat51 = [ ("x", [1, 2, 3, 4, 5])
                , ("y", [3, 5, 7, 9, 11]) ] :: [(Text, [Double])]

    it "df |-> lm \"x\" \"y\" == lmModel xs ys (係数一致)" $ do
      let m1 = dat51 |-> lm "x" "y"
          m2 = lmModel xs ys
      LA.toList (coefficientsV (lmResult m1))
        `shouldSatisfy` allClose (LA.toList (coefficientsV (lmResult m2)))

    it "df |-> lm の係数 ≈ [1, 2] (intercept, slope)" $ do
      let m1 = dat51 |-> lm "x" "y"
      LA.toList (coefficientsV (lmResult m1)) `shouldSatisfy` allClose [1, 2]

    it "fitWith == (|->) (演算子は fitWith のラッパ)" $ do
      let m1 = dat51 |-> lm "x" "y"
          m2 = fitWith (lm "x" "y") dat51
      LA.toList (coefficientsV (lmResult m1))
        `shouldSatisfy` allClose (LA.toList (coefficientsV (lmResult m2)))

    it "df |-> glm Gauss Identity == glmModel (係数一致)" $ do
      let m1 = dat51 |-> glm Gaussian Identity "x" "y"
          m2 = glmModel Gaussian Identity xs ys
      LA.toList (coefficientsV (glmResult m1))
        `shouldSatisfy` allClose (LA.toList (coefficientsV (glmResult m2)))

    it "fitEither: 列が存在すれば Right" $ do
      case fitEither (lm "x" "y") dat51 of
        Right m1 -> LA.toList (coefficientsV (lmResult m1))
                      `shouldSatisfy` allClose [1, 2]
        Left e   -> expectationFailure ("Right を期待したが Left: " <> e)

    it "fitEither: 欠落列は Left (total・error を投げない)" $ do
      case fitEither (lm "nope" "y") dat51 of
        Left _  -> pure ()
        Right _ -> expectationFailure "欠落列で Left を期待"

    it "df |-> rq [0.5] == 中央値回帰 (傾き ≈ 2)" $ do
      let m1 = dat51 |-> rq [0.5] "x" "y"
      case qmFits m1 of
        [(t, qf)] -> do
          t `shouldBe` 0.5
          (LA.toList (qfBeta qf) !! 1) `shouldSatisfy` (\b -> abs (b - 2) < 1e-6)
        _         -> expectationFailure "分位 fit が 1 本でない"

  describe "Phase 70.3 項目 C: 透過標準化ラッパ (standardized / standardizedY)" $ do
    -- y = 2x + 1 (μx=3, σx=√2.5≈1.58114)。 X のみ標準化した内側 LM の係数は
    -- β1 = a1·σx = 2·1.58114 = 3.16228, β0 = a0 + a1·μx = 1 + 6 = 7 になる。
    let datC = [ ("x", [1, 2, 3, 4, 5])
               , ("y", [3, 5, 7, 9, 11]) ] :: [(Text, [Double])]
        -- スケール差の大きい 2 特徴 (x1 ~ O(1), x2 ~ O(1000))。 距離は x2 が支配。
        datKNN = [ ("x1", [0.1, 0.2, 0.3, 0.4, 0.5, 0.6])
                 , ("x2", [1000, 2000, 3000, 4000, 5000, 6000])
                 , ("y",  [1, 2, 3, 4, 5, 6]) ] :: [(Text, [Double])]

    it "predictorCols / responseCol が spec から正しく出る (列名を二重に書かない)" $ do
      predictorCols (knnReg 3 ["x1", "x2"] "y") `shouldBe` ["x1", "x2"]
      responseCol   (knnReg 3 ["x1", "x2"] "y") `shouldBe` Just "y"
      predictorCols (knnCls 3 ["x1", "x2"] "c") `shouldBe` ["x1", "x2"]
      responseCol   (knnCls 3 ["x1", "x2"] "c") `shouldBe` Nothing  -- 分類
      responseCol   (glm Poisson Log "x" "y")     `shouldBe` Nothing  -- family/link 拘束

    it "standardized (lm): 内側 LM は標準化空間の係数 [7, 3.16228]" $ do
      let StandardizedModel { smInner = inner, smXStd = sx, smYStd = sy } =
            datC |-> standardized (lm "x" "y")
      LA.toList (coefficientsV (lmResult inner)) `shouldSatisfy` allClose [7, sqrt 2.5 * 2]
      stMu sx `shouldSatisfy` allClose [3]
      stSd sx `shouldSatisfy` allClose [sqrt 2.5]
      sy `shouldBe` Nothing                                  -- X のみ → y 標準化なし

    it "standardizedY (lm): X+y 標準化で内側係数 ≈ [0, 1] (完全相関)" $ do
      let StandardizedModel { smInner = inner, smYStd = sy } =
            datC |-> standardizedY (lm "x" "y")
      LA.toList (coefficientsV (lmResult inner))
        `shouldSatisfy` (\cs -> allClose [0, 1] (map (\v -> if abs v < 1e-9 then 0 else v) cs))
      case sy of
        Just (muY, sdY) -> do
          muY `shouldSatisfy` (\v -> abs (v - 7) < 1e-9)        -- ȳ = 7
          sdY `shouldSatisfy` (\v -> abs (v - sqrt 10) < 1e-9)  -- σy = √10
        Nothing -> expectationFailure "standardizedY は smYStd = Just を期待"

    it "smTrain: 単変量 (予測子1列) では元スケール (x,y) を保持" $ do
      let StandardizedModel { smTrain = tr } = datC |-> standardized (lm "x" "y")
      tr `shouldBe` Just ([1, 2, 3, 4, 5], [3, 5, 7, 9, 11])

    it "standardized (knnReg): 内側は手動標準化 df の fit とビット一致" $ do
      -- 手動: 特徴行列を fitStandardizer/applyStandardizer で標準化 → 同じ列名 df を組む。
      let feats   = ["x1", "x2"]
          col n   = LA.fromList (maybe (error "col") id (lookup n datKNN))
          xm      = LA.fromColumns (map col feats)        -- n × p
          sx      = fitStandardizer xm
          xmZ     = applyStandardizer sx xm
          datZ    = zip feats (map LA.toList (LA.toColumns xmZ))
                      ++ [("y", [1, 2, 3, 4, 5, 6])] :: [(Text, [Double])]
          mWrap   = datKNN |-> standardized (knnReg 3 feats "y")
          mManual = datZ   |-> knnReg 3 feats "y"
          -- 標準化空間の query (訓練点) で予測を突合。
          q       = xmZ
      VU.toList (predictKNNR (smInner mWrap) q)
        `shouldBe` VU.toList (predictKNNR mManual q)

    it "ガード: 内部標準化済 spec (regularized) に standardized → Left" $ do
      case fitEither (standardized (ridge ["x1", "x2"] "y")) datKNN of
        Left _  -> pure ()
        Right _ -> expectationFailure "predictorCols=[] の spec は Left を期待"

    it "ガード: スケール不変 spec (randomForestReg) に standardized → Left" $ do
      case fitEither (standardized (randomForestReg defaultRandomForest 42 ["x1", "x2"] "y")) datKNN of
        Left _  -> pure ()
        Right _ -> expectationFailure "木系は predictorCols=[] ゆえ Left を期待"

    it "ガード: standardizedY を分類 spec (knnCls) に付けると Left" $ do
      case fitEither (standardizedY (knnCls 3 ["x1", "x2"] "y")) datKNN of
        Left _  -> pure ()
        Right _ -> expectationFailure "responseCol=Nothing への standardizedY は Left を期待"

    it "ガード: standardizedY を GLM (family/link 拘束) に付けると Left" $ do
      case fitEither (standardizedY (glm Gaussian Identity "x" "y")) datC of
        Left _  -> pure ()
        Right _ -> expectationFailure "GLM の responseCol=Nothing ゆえ standardizedY は Left を期待"

    -- --- C2: 元スケール逆変換 (SingleVarModel / Plottable) ---
    let plainLM = lmModel (LA.fromList [1, 2, 3, 4, 5]) (LA.fromList [3, 5, 7, 9, 11])
        gC      = [1.5, 2.5, 3.5, 4.5]

    it "C2 svRange: standardized ラッパは元スケールの x 範囲 (1,5) を返す" $ do
      let wrap = datC |-> standardized (lm "x" "y")
      svRange wrap `shouldSatisfy` (\(lo, hi) -> abs (lo - 1) < 1e-9 && abs (hi - 5) < 1e-9)

    it "C2 svGrid: standardized (lm) の元スケール予測が plain lm と一致 (X 標準化は ŷ 不変)" $ do
      let wrap     = datC |-> standardized (lm "x" "y")
          (muW, _) = svGrid wrap 0.95 gC
          (muP, _) = svGrid plainLM 0.95 gC
      muW `shouldSatisfy` allClose muP

    it "C2 svGrid: standardizedY (lm) も round-trip で plain lm 予測に一致" $ do
      let wrap     = datC |-> standardizedY (lm "x" "y")
          (muW, _) = svGrid wrap 0.95 gC
          (muP, _) = svGrid plainLM 0.95 gC
      muW `shouldSatisfy` allClose muP

    it "C2 svGrid CI band: standardized (lm) の帯も plain lm と一致" $ do
      let wrap = datC |-> standardized (lm "x" "y")
      case (svGrid wrap 0.95 gC, svGrid plainLM 0.95 gC) of
        ((_, Just (loW, hiW)), (_, Just (loP, hiP))) -> do
          loW `shouldSatisfy` allClose loP
          hiW `shouldSatisfy` allClose hiP
        _ -> expectationFailure "両者とも CI band (Just) を期待"

    it "C2 svCoefR2: 元スケール係数が plain lm [1,2] に一致 (standardized / standardizedY)" $ do
      let plainCs = LA.toList (coefficientsV (lmResult plainLM))
      case (svCoefR2 (datC |-> standardized  (lm "x" "y")),
            svCoefR2 (datC |-> standardizedY (lm "x" "y"))) of
        (Just (csX, _), Just (csXY, _)) -> do
          csX  `shouldSatisfy` allClose plainCs
          csXY `shouldSatisfy` allClose plainCs
          csX  `shouldSatisfy` allClose [1, 2]
        _ -> expectationFailure "両ラッパとも線形ゆえ svCoefR2 = Just を期待"

    it "C2 svGrid: standardized (knnReg・1 特徴) は plain kNN と一致 (単調変換で近傍不変)" $ do
      let dat1   = [ ("x", [1, 2, 3, 4, 5, 6, 7, 8])
                   , ("y", [1, 3, 2, 5, 4, 7, 6, 9]) ] :: [(Text, [Double])]
          wrap   = dat1 |-> standardized (knnReg 2 ["x"] "y")
          plainK = fitKNNR 2 (LA.fromColumns [LA.fromList [1,2,3,4,5,6,7,8]])
                             (VU.fromList [1,3,2,5,4,7,6,9])
          g      = [1.5, 3.0, 5.5, 7.0]
          (muW, mbBand) = svGrid wrap 0.95 g
          muP    = VU.toList (predictKNNR plainK (LA.fromColumns [LA.fromList g]))
      muW `shouldSatisfy` allClose muP
      mbBand `shouldBe` Nothing                       -- kNN は band なし

    it "C2 toPlot: standardized ラッパは散布 + 曲線の 2 レイヤ以上を出す" $ do
      let wrap = datC |-> standardized (lm "x" "y")
      length (vsLayers (toPlot wrap)) `shouldSatisfy` (>= 2)

  describe "Phase 70.4?: quantileMulti (多変量分位点回帰)" $ do
    -- 厳密線形 (無誤差) y = 1 + 2·x1 + 3·x2。 無誤差なら全 τ が同じ β=[1,2,3] を復元。
    let dfQM = [ ("x1", [1, 2, 3, 1, 2, 3, 1, 2, 3])
               , ("x2", [0, 0, 0, 1, 1, 1, 2, 2, 2])
               , ("y",  [ 1 + 2*a + 3*b
                        | (a, b) <- zip [1,2,3,1,2,3,1,2,3] [0,0,0,1,1,1,2,2,2] ]) ]
              :: [(Text, [Double])]

    it "predictorCols / responseCol が正しい" $ do
      predictorCols (rqMulti [0.5] ["x1", "x2"] "y") `shouldBe` ["x1", "x2"]
      responseCol   (rqMulti [0.5] ["x1", "x2"] "y") `shouldBe` Just "y"

    it "★無誤差線形: 全 τ が β=[1,2,3] を復元 (intercept, x1, x2)" $ do
      let m = dfQM |-> rqMulti [0.25, 0.5, 0.75] ["x1", "x2"] "y"
      length (mqmFits m) `shouldBe` 3
      mqmNames m `shouldBe` ["x1", "x2"]
      -- QR は MM-IRLS (eps=1e-6) ゆえ厳密一致でなく 1e-5 許容で突合。
      mapM_ (\(_, qf) -> LA.toList (qfBeta qf)
                `shouldSatisfy` (\bs -> and (zipWith (\a b -> abs (a - b) < 1e-5) bs [1, 2, 3])))
            (mqmFits m)

    it "toPlot は τ ごとに 1 本ずつ線を出す (3 τ → 3 MLine)" $ do
      let m  = dfQM |-> rqMulti [0.1, 0.5, 0.9] ["x1", "x2"] "y"
          ls = vsLayers (toPlot m)
      length ls `shouldBe` 3
      all (\l -> getFirst (lyKind l) == Just MLine) ls `shouldBe` True

  describe "Phase 51.3: df |-> formula spec (R 流多変量)" $ do
    -- y = 1 + 2 x1 + 3 x2 (完全線形・OLS が係数を厳密復元)。
    let dfMV = DX.fromNamedColumns
                 [ ("x1", DX.fromList ([1, 2, 3, 4, 5] :: [Double]))
                 , ("x2", DX.fromList ([2, 1, 4, 3, 6] :: [Double]))
                 , ("y",  DX.fromList ([9, 8, 19, 18, 29] :: [Double])) ]
        assocMV = [ ("x1", [1, 2, 3, 4, 5])
                  , ("x2", [2, 1, 4, 3, 6])
                  , ("y",  [9, 8, 19, 18, 29]) ] :: [(Text, [Double])]
        -- glmmF 用 (text factor 列 = toFrame=id で温存する canonical 経路)。
        dfRE = DX.fromNamedColumns
                 [ ("x",     DX.fromList ([1,2,3,4, 1,2,3,4, 1,2,3,4] :: [Double]))
                 , ("y",     DX.fromList ([7.1,6.9,7.0,7.0, 5.0,4.9,5.1,5.0, 3.0,2.9,3.1,3.0] :: [Double]))
                 , ("group", DX.fromList (["A","A","A","A","B","B","B","B","C","C","C","C"] :: [T.Text])) ]

    it "df |-> lmF == multiLMModel (係数一致)" $ do
      let m1 = dfMV |-> lmF "y ~ x1 + x2"
      case multiLMModel "y ~ x1 + x2" dfMV of
        Right m2 -> LA.toList (coefficientsV (mlmResult m1))
                      `shouldSatisfy` allClose (LA.toList (coefficientsV (mlmResult m2)))
        Left e   -> expectationFailure e

    it "df |-> lmF の係数 ≈ [1, 2, 3] (完全線形を復元)" $ do
      let m1 = dfMV |-> lmF "y ~ x1 + x2"
      LA.toList (coefficientsV (mlmResult m1)) `shouldSatisfy` allClose [1, 2, 3]

    it "assoc 源 (toFrame 数値再構築) でも同じ係数" $ do
      let m1 = assocMV |-> lmF "y ~ x1 + x2"
      LA.toList (coefficientsV (mlmResult m1)) `shouldSatisfy` allClose [1, 2, 3]

    it "df |-> glmF Gaussian Identity == multiGLMModel (係数一致)" $ do
      let m1 = dfMV |-> glmF Gaussian Identity "y ~ x1 + x2"
      case multiGLMModel Gaussian Identity "y ~ x1 + x2" dfMV of
        Right m2 -> LA.toList (coefficientsV (mglmResult m1))
                      `shouldSatisfy` allClose (LA.toList (coefficientsV (mglmResult m2)))
        Left e   -> expectationFailure e

    it "fitEither: formula parse 失敗は Left (total)" $ do
      case fitEither (lmF "garbage") dfMV of
        Left _  -> pure ()
        Right _ -> expectationFailure "parse 失敗で Left を期待"

    it "df |-> glmmF (1|group): Right で固定効果 2 個 (toFrame=id で factor 温存)" $ do
      case fitEither (glmmF "y ~ x + (1|group)") dfRE of
        Right (_, labels) -> length labels `shouldBe` 2
        Left e            -> expectationFailure e

  describe "Phase 51.4: df |-> hbm + dataScatterOf (ColumnSource→HBM)" $ do
    let xdat4 = [1,2,3,4,5,6,7,8,9,10] :: [Double]
        ydat4 = zipWith (\x e -> 1 + 2 * x + e) xdat4
                  [0.12,-0.08,0.05,-0.15,0.10,0.03,-0.07,0.14,-0.04,0.06]
        cfg4  = defaultHBM { hbmChains = 2, hbmSamples = 200, hbmWarmup = 200
                           , hbmSeed = Just 314 }
        assoc4 = [("x", xdat4), ("y", ydat4)] :: [(Text, [Double])]
        df4    = DX.fromNamedColumns [ ("x", DX.fromList xdat4)
                                     , ("y", DX.fromList ydat4) ]
        coldata4 = [ ("x", NumData (V.fromList xdat4))
                   , ("y", NumData (V.fromList ydat4)) ] :: [(Text, ColData)]
        sams m = map chainSamples (hbmChainsR m)

    it "assoc |-> hbm == hbmModelPure (chainSamples ビット同一)" $ do
      let m1 = assoc4 |-> hbm cfg4 hbmLinModel
          m2 = hbmModelPure cfg4 hbmLinModel assoc4
      sams m1 `shouldBe` sams m2

    it "DataFrame 源でも posterior が真値 (a≈1, b≈2)" $ do
      let m = df4 |-> hbm cfg4 hbmLinModel
      abs (posteriorMeanOf "a" (hbmChainsR m) - 1) `shouldSatisfy` (< 0.5)
      abs (posteriorMeanOf "b" (hbmChainsR m) - 2) `shouldSatisfy` (< 0.2)

    it "ColData 源 (NumData) でも assoc と同じ結果 (ビット同一)" $ do
      let m1 = coldata4 |-> hbm cfg4 hbmLinModel
          m2 = assoc4   |-> hbm cfg4 hbmLinModel
      sams m1 `shouldBe` sams m2

    it "dataScatterOf: hbmData から scatter 層 1 枚 (10 点・MScatter)" $ do
      let m  = assoc4 |-> hbm cfg4 hbmLinModel
          ls = vsLayers (dataScatterOf m "x" "y")
      length ls `shouldBe` 1
      getFirst (lyKind (head ls)) `shouldBe` Just MScatter
      case getLast (lyEncX (head ls)) of
        Just (ColNum v) -> V.length v `shouldBe` 10
        _               -> expectationFailure "encX が inline ColNum でない"

    it "dataScatterOf: 欠落列なら空 (mempty)" $ do
      let m = assoc4 |-> hbm cfg4 hbmLinModel
      vsLayers (dataScatterOf m "nope" "y") `shouldBe` []

  describe "Phase 60.3: DataIx 束縛 + Integer 許容 + 突合 loud error" $ do
    let cfg60 = defaultHBM { hbmChains = 1, hbmSamples = 100, hbmWarmup = 100
                           , hbmSeed = Just 60 }
        -- 群 A ≈ 1、 群 B ≈ 5。 行順は B,A,B,A,... (sort 順コード化の検証:
        -- 出現順なら B=0 になるが、 sort 順なら A=0)。
        gTxt  = ["B","A","B","A","B","A","B","A"] :: [T.Text]
        yMix  = [5.1, 0.9, 4.9, 1.1, 5.0, 1.0, 5.2, 0.8] :: [Double]
        dfFac = DX.fromNamedColumns [ ("g", DX.fromList gTxt)
                                    , ("y", DX.fromList yMix) ]

    it "Text factor 列 → sort 順コード化 (levels=[A,B]・mu0=A群≈1, mu1=B群≈5)" $ do
      let m = dfFac |-> hbm cfg60 hbmIxModel
      hbmFactorLevels m `shouldBe` [("g", ["A", "B"])]
      abs (posteriorMeanOf "mu0" (hbmChainsR m) - 1) `shouldSatisfy` (< 0.5)
      abs (posteriorMeanOf "mu1" (hbmChainsR m) - 5) `shouldSatisfy` (< 0.5)

    it "Int 数値列でも DataIx slot に直結 (levels は空)" $ do
      let dfNum = DX.fromNamedColumns
                    [ ("g", DX.fromList ([1,0,1,0,1,0,1,0] :: [Int]))
                    , ("y", DX.fromList yMix) ]
          m = dfNum |-> hbm cfg60 hbmIxModel
      hbmFactorLevels m `shouldBe` []
      abs (posteriorMeanOf "mu0" (hbmChainsR m) - 1) `shouldSatisfy` (< 0.5)
      abs (posteriorMeanOf "mu1" (hbmChainsR m) - 5) `shouldSatisfy` (< 0.5)

    it "Integer 列が dataNamedX (連続) で黙殺されず通る (60.3a 根治の確認)" $ do
      let dfInt = DX.fromNamedColumns
                    [ ("x", DX.fromList ([1..10] :: [Integer]))
                    , ("y", DX.fromList (zipWith (\x e -> 1 + 2 * x + e)
                                          ([1..10] :: [Double])
                                          [0.12,-0.08,0.05,-0.15,0.10
                                          ,0.03,-0.07,0.14,-0.04,0.06])) ]
      lookupCol "x" dfInt `shouldBe` Just (map fromIntegral [1..10 :: Int])
      let m = dfInt |-> hbm cfg60 hbmLinModel
      abs (posteriorMeanOf "b" (hbmChainsR m) - 2) `shouldSatisfy` (< 0.3)

    it "空 placeholder dataNamed の列欠落は fitEither Left (loud)" $ do
      let dfNoX = DX.fromNamedColumns [ ("y", DX.fromList yMix) ]
      case fitEither (hbm cfg60 hbmLinModel) dfNoX of
        Left e  -> e `shouldSatisfy` (("dataNamed" `T.isInfixOf`) . T.pack)
        Right _ -> expectationFailure "列欠落 (x) で Left を期待"

    it "空 placeholder dataNamedIx の列欠落も Left (loud)" $ do
      let dfNoG = DX.fromNamedColumns [ ("y", DX.fromList yMix) ]
      case fitEither (hbm cfg60 hbmIxModel) dfNoG of
        Left e  -> e `shouldSatisfy` (("dataNamedIx" `T.isInfixOf`) . T.pack)
        Right _ -> expectationFailure "列欠落 (g) で Left を期待"

    it "非整数の数値列を DataIx slot に bind すると Left" $ do
      let dfBad = DX.fromNamedColumns
                    [ ("g", DX.fromList ([0.5, 1.5, 0.5, 1.5, 0.5, 1.5, 0.5, 1.5] :: [Double]))
                    , ("y", DX.fromList yMix) ]
      case fitEither (hbm cfg60 hbmIxModel) dfBad of
        Left e  -> e `shouldSatisfy` (("非整数" `T.isInfixOf`) . T.pack)
        Right _ -> expectationFailure "非整数列で Left を期待"

  describe "Phase 52.A4: grouped (群別フィット・HBM 整合)" $ do
    -- 群 0 = y=2x (傾き 2)、 群 1 = y=5x (傾き 5)。 数値群列 "g"。
    let datG = [ ("x", [1, 2, 3, 4,  1, 2, 3, 4])
               , ("y", [2, 4, 6, 8,  5, 10, 15, 20])
               , ("g", [0, 0, 0, 0,  1, 1, 1, 1]) ] :: [(Text, [Double])]
        gfG  = datG |-> grouped "g" (lm "x" "y")
        slopeOf m = LA.toList (coefficientsV (lmResult m)) !! 1
        lineLayers vs = [ l | l <- vsLayers vs, getFirst (lyKind l) == Just MLine ]

    it "groupLabels: 群が 2 つ (出現順 [\"0\", \"1\"])" $ do
      groupLabels gfG `shouldBe` ["0", "1"]

    it "groupModels: 各群を別々に fit (群 0 傾き≈2・群 1 傾き≈5)" $ do
      case map snd (groupModels gfG) of
        [m0, m1] -> do
          slopeOf m0 `shouldSatisfy` (\b -> abs (b - 2) < 1e-9)
          slopeOf m1 `shouldSatisfy` (\b -> abs (b - 5) < 1e-9)
        _        -> expectationFailure "群モデルが 2 本でない"

    it "toPlot: 群数ぶんの MLine layer (2 本)・各線は ColorByCol" $ do
      let ls = lineLayers (toPlot gfG)
      length ls `shouldBe` 2
      let colorEncs = map (getLast . lyColor) ls
      colorEncs `shouldSatisfy` all (\x -> case x of
        Just (ColorByCol _) -> True
        _                   -> False)

    it "toPlot: scaleColorManual で群色固定 (effectPalette) + legend" $ do
      let vs = toPlot gfG
      getLast (vsColorManual vs)
        `shouldBe` Just [("0", "#1f77b4"), ("1", "#ff7f0e")]
      getLast (vsLegend vs) `shouldSatisfy` \x -> case x of
        Just _  -> True
        Nothing -> False

    it "factor (文字) 群列でも分割できる (DataFrame 源・getTextVec 経路)" $ do
      let dfG = DX.fromNamedColumns
                  [ ("x", DX.fromList ([1, 2, 3, 4, 1, 2, 3, 4] :: [Double]))
                  , ("y", DX.fromList ([2, 4, 6, 8, 5, 10, 15, 20] :: [Double]))
                  , ("g", DX.fromList (["a", "a", "a", "a", "b", "b", "b", "b"] :: [Text])) ]
          gf = dfG |-> grouped "g" (lm "x" "y")
      groupLabels gf `shouldBe` ["a", "b"]
      case map snd (groupModels gf) of
        [m0, m1] -> do
          slopeOf m0 `shouldSatisfy` (\b -> abs (b - 2) < 1e-9)
          slopeOf m1 `shouldSatisfy` (\b -> abs (b - 5) < 1e-9)
        _        -> expectationFailure "群モデルが 2 本でない"

    it "fitEither: 群列が無ければ Left (total)" $ do
      case fitEither (grouped "nope" (lm "x" "y")) datG of
        Left _  -> pure ()
        Right _ -> expectationFailure "欠落群列で Left を期待"

  describe "Phase 52.A7: groupedFullrange (回帰線をデータ全幅へ延長)" $ do
    -- 群 0 = x∈[1..4]・y=2x、 群 1 = x∈[10..13]・y=5x。 x 範囲が群間で重ならない。
    let datF = [ ("x", [1, 2, 3, 4,   10, 11, 12, 13])
               , ("y", [2, 4, 6, 8,   50, 55, 60, 65])
               , ("g", [0, 0, 0, 0,    1,  1,  1,  1]) ] :: [(Text, [Double])]
        gfF  = datF |-> grouped "g" (lm "x" "y")
        lineLayers' vs = [ l | l <- vsLayers vs, getFirst (lyKind l) == Just MLine ]
        xRangeOf l = case getLast (lyEncX l) of
          Just (ColNum vx) -> let xs = V.toList vx in (minimum xs, maximum xs)
          _                -> error "encX が inline ColNum でない"

    it "既定 toPlot: 各群線は自群の x 範囲のみ ([1,4] と [10,13])" $ do
      case lineLayers' (toPlot gfF) of
        [l0, l1] -> do
          xRangeOf l0 `shouldSatisfy` \(lo, hi) -> abs (lo - 1) < 1e-9 && abs (hi - 4) < 1e-9
          xRangeOf l1 `shouldSatisfy` \(lo, hi) -> abs (lo - 10) < 1e-9 && abs (hi - 13) < 1e-9
        _        -> expectationFailure "群線が 2 本でない"

    it "groupedFullrange: 各群線が全群 union 範囲 [1,13] へ延長される" $ do
      case lineLayers' (groupedFullrange gfF) of
        [l0, l1] -> do
          xRangeOf l0 `shouldSatisfy` \(lo, hi) -> abs (lo - 1) < 1e-9 && abs (hi - 13) < 1e-9
          xRangeOf l1 `shouldSatisfy` \(lo, hi) -> abs (lo - 1) < 1e-9 && abs (hi - 13) < 1e-9
        _        -> expectationFailure "群線が 2 本でない"

    it "groupedFullrange: 傾き・凡例は toPlot と不変 (range のみ拡張)" $ do
      let vsF = groupedFullrange gfF
      -- 凡例 (scaleColorManual) は既定と同じ群色固定。
      getLast (vsColorManual vsF) `shouldBe` Just [("0", "#1f77b4"), ("1", "#ff7f0e")]
      -- 延長端でも各線は当該群の傾きを保つ (群 1 は y=5x ゆえ x=1 で μ≈5)。
      case lineLayers' vsF of
        [_, l1] -> case (getLast (lyEncX l1), getLast (lyEncY l1)) of
          (Just (ColNum vx), Just (ColNum vy)) -> do
            -- linspace は昇順 (lo=1 が先頭) ゆえ先頭が x=1 での μ̂。
            V.head vx `shouldSatisfy` (\x -> abs (x - 1) < 1e-9)
            V.head vy `shouldSatisfy` (\y -> abs (y - 5) < 1e-6)
          _ -> expectationFailure "encX/encY が inline ColNum でない"
        _       -> expectationFailure "群線が 2 本でない"

  describe "Phase 52.A9: lmDiag (係数診断の薄アクセサ)" $ do
    -- statsmodels OLS と突合: x=[1..5], y=[2.1,3.9,6.2,7.8,10.1]。
    -- intercept: SE=0.19807406 t=0.25243083 p=0.81701518
    -- slope    : SE=0.05972158 t=33.32129066 p=5.94153911e-5
    let m9 = lmModel (LA.fromList [1, 2, 3, 4, 5])
                     (LA.fromList [2.1, 3.9, 6.2, 7.8, 10.1])

    it "lmDiag: 係数 2 つ ([(Intercept), slope]) の CoefStats" $ do
      length (lmDiag m9) `shouldBe` 2

    it "lmDiag: SE/t/p が statsmodels と一致 (intercept)" $ do
      case lmDiag m9 of
        (c0 : _) -> do
          csSE c0     `shouldSatisfy` \v -> abs (v - 0.19807406) < 1e-6
          csTValue c0 `shouldSatisfy` \v -> abs (v - 0.25243083) < 1e-6
          csPValue c0 `shouldSatisfy` \v -> abs (v - 0.81701518) < 1e-6
        _ -> expectationFailure "係数が空"

    it "lmDiag: SE/t/p が statsmodels と一致 (slope)" $ do
      case lmDiag m9 of
        (_ : c1 : _) -> do
          csSE c1     `shouldSatisfy` \v -> abs (v - 0.05972158) < 1e-6
          csTValue c1 `shouldSatisfy` \v -> abs (v - 33.32129066) < 1e-5
          csPValue c1 `shouldSatisfy` \v -> abs (v - 5.94153911e-5) < 1e-9
        _ -> expectationFailure "slope が無い"

    it "groupedLmDiag: 各群の係数診断を群ラベル付きで取り出す" $ do
      -- 群 0 = y=2x, 群 1 = y=5x (傾きは厳密ゆえ SE≈0・t は大)。
      let datG = [ ("x", [1, 2, 3, 4,  1, 2, 3, 4])
                 , ("y", [2, 4, 6, 8,  5, 10, 15, 20])
                 , ("g", [0, 0, 0, 0,  1, 1, 1, 1]) ] :: [(Text, [Double])]
          gf   = datG |-> grouped "g" (lm "x" "y")
          diags = groupedLmDiag gf
      map fst diags `shouldBe` ["0", "1"]
      map (length . snd) diags `shouldBe` [2, 2]   -- 各群 intercept+slope

  describe "Phase 52.A6: weighted (WLS 露出)" $ do
    -- statsmodels WLS と突合: x=[1..5], y=[2.1,3.9,6.2,7.8,10.1], w=[1..5]。
    -- beta=[0.00285714, 2.00285714], rsquared=0.99619888,
    -- mean_ci @x=3,5: lower=[5.68995295,9.60211961] upper=[6.33290419,10.4321661]
    let datW6 = [ ("x", [1, 2, 3, 4, 5])
                , ("y", [2.1, 3.9, 6.2, 7.8, 10.1])
                , ("w", [1, 2, 3, 4, 5])      -- 重み列 (列名で参照)
                , ("w1", [1, 1, 1, 1, 1])     -- 全重み 1 (OLS 一致確認用)
                , ("wbad", [1, 2, 3]) ] :: [(Text, [Double])]  -- 長さ不一致 (Left 確認用)
        wm6   = datW6 |-> weighted "w" (lm "x" "y")
        nearW a b = abs (a - b) < 1e-6
        allNearW xs ys = and (zipWith nearW xs ys)

    it "weighted β̂ が statsmodels WLS と一致" $ do
      LA.toList (coefficientsV (lmResult (wlmInner wm6)))
        `shouldSatisfy` allNearW [0.00285714, 2.00285714]

    it "weighted svGrid (CI) が statsmodels WLS mean_ci と一致 (x=3,5)" $ do
      case svGrid wm6 0.95 [3.0, 5.0] of
        (_, Just (los, his)) -> do
          los `shouldSatisfy` allNearW [5.68995295, 9.60211961]
          his `shouldSatisfy` allNearW [6.33290419, 10.4321661]
        _ -> expectationFailure "WLS CI 帯が出ない"

    it "weighted svCoefR2 の R² が statsmodels WLS rsquared と一致 (weighted R²)" $ do
      case svCoefR2 wm6 of
        Just (_, r2) -> r2 `shouldSatisfy` nearW 0.9961988825728395
        Nothing      -> expectationFailure "svCoefR2 が Nothing"

    it "全重み 1 の WLS は OLS と一致" $ do
      let wm1 = datW6 |-> weighted "w1" (lm "x" "y")
          ols = datW6 |-> lm "x" "y"
      LA.toList (coefficientsV (lmResult (wlmInner wm1)))
        `shouldSatisfy` allNearW (LA.toList (coefficientsV (lmResult ols)))

    it "重み列長が観測数と不一致なら Left" $ do
      case fitEither (weighted "wbad" (lm "x" "y")) datW6 of
        Left _  -> pure ()
        Right _ -> expectationFailure "長さ不一致が Left にならない"

    it "toPlot は grid 経路 (line layer) を返す (訓練点経路を使わない)" $ do
      length (vsLayers (toPlot wm6)) `shouldSatisfy` (>= 1)

  describe "Phase 52.A5 / 70.F: bandMode CI/PI (予測区間)" $ do
    -- statsmodels OLS の mean_ci / obs_ci と突合する固定データ (σ̂²>0)。
    let xsA5 = LA.fromList [1, 2, 3, 4, 5]
        ysA5 = LA.fromList [2.1, 3.9, 6.2, 7.8, 10.1]
        mA5  = lmModel xsA5 ysA5
        gxA5 = [3.0, 5.0]
        near a b = abs (a - b) < 1e-6
        allNear xs ys = and (zipWith near xs ys)

    it "svGrid (CI) が statsmodels mean_ci と一致 (x=3,5)" $ do
      case svGrid mA5 0.95 gxA5 of
        (_, Just (los, his)) -> do
          los `shouldSatisfy` allNear [5.75121357, 9.53444824]
          his `shouldSatisfy` allNear [6.28878643, 10.46555176]
        _ -> expectationFailure "CI 帯が出ない"

    it "svGridPI (PI) が statsmodels obs_ci と一致 (x=3,5)" $ do
      case svGridPI mA5 0.95 gxA5 of
        Just (los, his) -> do
          los `shouldSatisfy` allNear [5.36161039, 9.23975716]
          his `shouldSatisfy` allNear [6.67838961, 10.76024284]
        Nothing -> expectationFailure "PI 帯が出ない"

    it "PI ⊃ CI (各点で PI の方が広い)" $ do
      case (svGrid mA5 0.95 gxA5, svGridPI mA5 0.95 gxA5) of
        ((_, Just (clo, chi)), Just (plo, phi)) -> do
          and (zipWith (<) plo clo) `shouldBe` True   -- PI 下限 < CI 下限
          and (zipWith (>) phi chi) `shouldBe` True   -- PI 上限 > CI 上限
        _ -> expectationFailure "帯が揃わない"

    it "GLM Gaussian/Identity の PI は LM と一致 (closed form 帰着)" $ do
      let gm = glmModel Gaussian Identity xsA5 ysA5
      case (svGridPI gm 0.95 gxA5, svGridPI mA5 0.95 gxA5) of
        (Just (glo, ghi), Just (llo, lhi)) -> do
          allNear glo llo `shouldBe` True
          allNear ghi lhi `shouldBe` True
        _ -> expectationFailure "GLM/LM PI が揃わない"

    it "非 Gaussian GLM は PI=Nothing (over-claim しない)" $ do
      let gp = glmModel Poisson Log xsA5 (LA.fromList [1, 2, 3, 5, 8])
      svGridPI gp 0.95 gxA5 `shouldBe` Nothing

    it "GAM は PI=Nothing (既定実装)" $ do
      let gm = gamModel 3 5 0.0 xsA5 ysA5
      svGridPI gm 0.95 gxA5 `shouldBe` Nothing

    it "renderGrid: bandMode BandPI の帯は CI より広い" $ do
      let bandLayerOf spec = head [ l | l <- vsLayers (toPlot spec)
                                      , getFirst (lyKind l) == Just MBand ]
          loY l = case getLast (lyEncY l) of
            Just (ColNum v) -> V.toList v
            _               -> error "encY が ColNum でない"
          spec k = statModel mA5 <> gridRange 3 5 <> grid 2 <> bandMode k
          ciLo = loY (bandLayerOf (spec BandCI))
          piLo = loY (bandLayerOf (spec BandPI))
      and (zipWith (<) piLo ciLo) `shouldBe` True   -- PI 帯下限 < CI 帯下限
      head piLo `shouldSatisfy` near 5.36161039     -- PI 下限 = obs_ci 下限

    it "renderGrid: BandPI は GAM (PI 非提供・CI 提供) で CI へフォールバックし帯が出る (Phase 70.6 G)" $ do
      let gm   = gamModel 3 5 0.0 xsA5 ysA5
          spec = statModel gm <> gridRange 1 5 <> grid 5 <> bandMode BandPI
          bands = [ l | l <- vsLayers (toPlot spec), getFirst (lyKind l) == Just MBand ]
      -- GAM は CI (svGrid Just) を持つが PI (svGridPI) は Nothing。BandPI は CI へフォールバック。
      length bands `shouldBe` 1

  describe "Phase 70.H: ブートストラップ CI/PI (piMethod PIBootstrap)" $ do
    let bxs = [1,2,3,4,5,6,7,8,9,10,11,12] :: [Double]
        bys = [2.4,3.6,6.5,7.4,11.1,11.9,15.2,15.8,19.0,20.4,22.7,24.1] :: [Double]
        bdf = [ ("x", NumData (V.fromList bxs)), ("y", NumData (V.fromList bys)) ]
              :: [(Text, ColData)]
        ycount = [1,2,2,4,5,8,11,15,20,27,36,49] :: [Double]
        bdfP = [ ("x", NumData (V.fromList bxs)), ("y", NumData (V.fromList ycount)) ]
               :: [(Text, ColData)]
        boot = PIBootstrap 7 400      -- seed 7・400 draws
        -- statModel model <> bandMode <> piMethod (PIBootstrap …) の第 1 MBand 層の (lo, hi)。
        bandBoundsP model mode pm =
          let ls = vsLayers (toPlot (statModel model <> grid 100 <> bandMode mode <> piMethod pm))
              b  = head [ l | l <- ls, getFirst (lyKind l) == Just MBand ]
              col f = case getLast (f b) of
                Just (ColNum v) -> V.toList v
                _               -> error "band encoding が ColNum でない"
          in (col lyEncY, col lyEncY2)
        lmM   = bdf  |-> lm "x" "y"                          :: LMModel
        glmM  = bdfP |-> glm Poisson Log "x" "y"             :: GLMModel
        robM  = bdf  |-> rlm (Huber defaultHuberK) "x" "y" :: RobustModel

    it "決定的: 同 seed → ビット同一の帯 (PI)" $ do
      bandBoundsP lmM BandPI boot `shouldBe` bandBoundsP lmM BandPI boot

    it "別 seed → 異なる帯 (実際に確率的)" $ do
      (bandBoundsP lmM BandPI boot == bandBoundsP lmM BandPI (PIBootstrap 99 400))
        `shouldBe` False

    it "LM: bootstrap PI ⊃ CI かつ grid 100 点" $ do
      let (clo, chi) = bandBoundsP lmM BandCI boot
          (plo, phi) = bandBoundsP lmM BandPI boot
      length clo `shouldBe` 100
      and (zipWith (<=) plo clo) `shouldBe` True
      and (zipWith (>=) phi chi) `shouldBe` True

    it "非 Gaussian GLM (Poisson/Log) でも bootstrap PI が出る (closed-form 非対応)" $ do
      let (clo, chi) = bandBoundsP glmM BandCI boot
          (plo, phi) = bandBoundsP glmM BandPI boot
      length plo `shouldBe` 100
      and (zipWith (<=) plo clo) `shouldBe` True   -- PI 下限 ≤ CI 下限
      and (zipWith (>=) phi chi) `shouldBe` True

    it "ロバスト (Huber) でも bootstrap PI が出る" $ do
      let (clo, chi) = bandBoundsP robM BandCI boot
          (plo, phi) = bandBoundsP robM BandPI boot
      and (zipWith (<=) plo clo) `shouldBe` True
      and (zipWith (>=) phi chi) `shouldBe` True

    it "piMethod 既定 (PIClosedForm) は closed-form 帯と一致" $ do
      let viaDefault = vsLayers (toPlot (statModel lmM <> grid 20 <> bandMode BandCI))
          viaClosed  = vsLayers (toPlot (statModel lmM <> grid 20 <> bandMode BandCI
                                          <> piMethod PIClosedForm))
      map (getFirst . lyKind) viaDefault `shouldBe` map (getFirst . lyKind) viaClosed

  describe "Phase 52.D3: GLMMResultRE + toPlot (混合効果 caterpillar)" $ do
    -- 3 群 (A,B,C) の random intercept。 群ごとに水準が違う (A≈7, B≈5, C≈3)。
    let dfRE3 = DX.fromNamedColumns
                  [ ("x",     DX.fromList ([1,2,3,4, 1,2,3,4, 1,2,3,4] :: [Double]))
                  , ("y",     DX.fromList ([7.1,6.9,7.0,7.0, 5.0,4.9,5.1,5.0, 3.0,2.9,3.1,3.0] :: [Double]))
                  , ("group", DX.fromList (["A","A","A","A","B","B","B","B","C","C","C","C"] :: [T.Text])) ]
        reOf = case fitEither (glmmF "y ~ x + (1|group)") dfRE3 of
                 Right (re, _) -> re
                 Left e        -> error e

    it "toPlot: MForest layer・点 3 個 (= 群数)・誤差半幅 0 (CI 帯なし)" $ do
      let fLayer = head (vsLayers (toPlot reOf))
      getFirst (lyKind fLayer) `shouldBe` Just MForest
      case (getLast (lyEncX fLayer), getLast (lyErrorX fLayer)) of
        (Just (ColNum ests), Just (ColNum errs)) -> do
          V.length ests `shouldBe` 3
          V.toList errs `shouldSatisfy` all (== 0)   -- conditional variance 未格納ゆえ点のみ
        _ -> expectationFailure "forest encX/errorX が inline ColNum でない"

    it "toPlot: BLUP が値で昇順ソート (caterpillar の並び順)" $ do
      let fLayer = head (vsLayers (toPlot reOf))
      case getLast (lyEncX fLayer) of
        Just (ColNum ests) ->
          let es = V.toList ests
          in and (zipWith (<=) es (drop 1 es)) `shouldBe` True
        _ -> expectationFailure "encX が inline ColNum でない"

    it "diagnosticPlots: random intercept のみ (r=1) ゆえ 1 枚" $ do
      length (diagnosticPlots reOf) `shouldBe` 1

  describe "plot Phase 24 A3: 応答曲面 3D 直結 (surfaceGrid / surfaceOf / epredSurfaceOf)" $ do
    -- y = 1 + 2·x1 + 3·x2 + 4·x3 (厳密線形・無誤差、 C3 と同じ design)。
    let dfRS = DX.fromNamedColumns
          [ ("y",  DX.fromList ([3,7,6,10, 5,9,8,12, 7,11,10,14] :: [Double]))
          , ("x1", DX.fromList ([1,1,1,1, 2,2,2,2, 3,3,3,3] :: [Double]))
          , ("x2", DX.fromList ([0,0,1,1, 0,0,1,1, 0,0,1,1] :: [Double]))
          , ("x3", DX.fromList ([0,1,0,1, 0,1,0,1, 0,1,0,1] :: [Double]))
          ]
        mrs   = either error id (multiLMModel "y ~ x1 + x2 + x3" dfRS)
        opts5 = defaultSurfaceOpts { soN = 5 }

    it "surfaceGrid: 範囲 = 観測 min/max・寸法 n×n" $ do
      let (gxs, gys, grd) = surfaceGrid mrs "x1" "x2" opts5
      (head gxs, last gxs) `shouldBe` (1, 3)
      (head gys, last gys) `shouldBe` (0, 1)
      (length grd, length (head grd)) `shouldBe` (5, 5)

    it "surfaceGrid: grid[j][i] = 3 + 2·gxs[i] + 3·gys[j] (x3 hold Mean 0.5) を厳密復元" $ do
      let (gxs, gys, grd) = surfaceGrid mrs "x1" "x2" opts5
          expectAt j i = 1 + 2 * (gxs !! i) + 3 * (gys !! j) + 4 * 0.5
      sequence_ [ (grd !! j !! i) `shouldSatisfy`
                    (\v -> abs (v - expectAt j i) < 1e-8)
                | j <- [0 .. 4], i <- [0 .. 4] ]

    it "surfaceGrid: holdAt (Fixed x3=1) で面全体が +2 (vs Mean 0.5)" $ do
      let (_, _, g0) = surfaceGrid mrs "x1" "x2" opts5
          (_, _, g1) = surfaceGrid mrs "x1" "x2" opts5 { soHoldAt = Fixed [("x3", 1)] }
      (g1 !! 2 !! 2 - g0 !! 2 !! 2) `shouldSatisfy` (\d -> abs (d - 2) < 1e-8)

    it "surfaceOf: M3Surface 1 layer・colormap ON・x/y range 焼き込み" $ do
      let ls = P3.vs3Layers (surfaceOf mrs "x1" "x2")
      length ls `shouldBe` 1
      let l = head ls
      getFirst (P3.lyr3Kind l) `shouldBe` Just P3.M3Surface
      getLast (P3.lyr3Colormap l) `shouldBe` Just P3.viridisStops3D
      getLast (P3.lyr3XRange l) `shouldBe` Just (1, 3)
      getLast (P3.lyr3YRange l) `shouldBe` Just (0, 1)

    it "dataScatter3DOf: 訓練 12 点の M3Scatter (z = 実測 y)" $ do
      let ls = P3.vs3Layers (dataScatter3DOf mrs "x1" "x2")
          l  = head ls
      getFirst (P3.lyr3Kind l) `shouldBe` Just P3.M3Scatter
      case getLast (P3.lyr3Points l) of
        Just pts -> do
          length pts `shouldBe` 12
          case head pts of Point3 px py pz -> (px, py, pz) `shouldBe` (1, 0, 3)
        Nothing -> expectationFailure "scatter3D の points が無い"

    it "epredSurfaceOf: 事後平均面が 1 + 2·x1 + 1.5·x2 に近い (無誤差データ)" $ do
      let xs1  = [0, 0.5, 1, 1.5, 2, 0, 0.5, 1, 1.5, 2]
          xs2  = [0, 1, 0, 1, 0, 1, 0, 1, 0, 1]
          ysE  = [ 1 + 2 * a + 1.5 * b | (a, b) <- zip xs1 xs2 ]
          cfg  = defaultHBM { hbmChains = 2, hbmSamples = 400, hbmWarmup = 400 }
      studied <- hbmModel cfg hbmEpred2Model
                   [("x1", xs1), ("x2", xs2), ("y", ysE)]
      let spec = epredSurfaceOfWith studied "x1" "x2" "mu" defaultSurfaceOpts { soN = 3 }
          l    = head (P3.vs3Layers spec)
      getFirst (P3.lyr3Kind l) `shouldBe` Just P3.M3Surface
      case getLast (P3.lyr3Grid l) of
        Just grd -> do
          (length grd, length (head grd)) `shouldBe` (3, 3)
          -- 角 (x1=2, x2=1) の真値 = 6.5、 原点 = 1
          (grd !! 0 !! 0) `shouldSatisfy` (\v -> abs (v - 1) < 0.5)
          (grd !! 2 !! 2) `shouldSatisfy` (\v -> abs (v - 6.5) < 0.5)
        Nothing -> expectationFailure "surface3D の grid が無い"

  -- =====================================================================
  -- Phase 68 A1: KMeans クラスタリングの図 (Plottable + ヘルパ)
  -- 二層イディオム: clusterScatterOf (data 層・色=ラベル) <> centroidsOf
  -- (model 層・✚ centroid)。 KMeansResult を直接構築して構造を検証する。
  -- =====================================================================
  describe "Phase 68 A1: KMeans + Plottable / clusterScatterOf / centroidsOf" $ do
    let kres = KMeansResult
                 { kmrCentroids = LA.fromLists [[1, 1], [5, 5], [1, 5]]   -- 3×2
                 , kmrLabels    = [0, 0, 1, 1, 2]
                 , kmrInertia   = 0
                 , kmrIters     = 1
                 , kmrConverged = True
                 }
        kdf  = [ ("x", [0.9, 1.1, 5.2, 4.8, 1.0])
               , ("y", [1.2, 0.8, 5.1, 4.9, 5.3]) ] :: [(Text, [Double])]

    it "centroidsOf: 第 0/1 次元 centroid を MScatter + ✚ shape + クラスタ色で 1 layer" $ do
      let ls = vsLayers (centroidsOf kres 0 1)
      length ls `shouldBe` 1
      let l = head ls
      getFirst (lyKind l)  `shouldBe` Just MScatter
      getLast  (lyShape l) `shouldBe` Just MShCross
      -- x = centroid 第0次元 [1,5,1]、 y = 第1次元 [1,5,5]
      case (getLast (lyEncX l), getLast (lyEncY l)) of
        (Just (ColNum vx), Just (ColNum vy)) -> do
          V.toList vx `shouldBe` [1, 5, 1]
          V.toList vy `shouldBe` [1, 5, 5]
        _ -> expectationFailure "centroid encX/encY が inline ColNum でない"
      -- 色 = クラスタ id "0","1","2" の categorical
      case getLast (lyColor l) of
        Just (ColorByCol (ColTxt v)) -> V.toList v `shouldBe` ["0", "1", "2"]
        _ -> expectationFailure "centroid の colorBy が ColTxt クラスタ id でない"

    it "centroidsOf: 範囲外 index は mempty (layer ゼロ)" $ do
      vsLayers (centroidsOf kres 0 2) `shouldBe` []   -- d=2 ゆえ index 2 は範囲外
      vsLayers (centroidsOf kres (-1) 0) `shouldBe` []

    it "toPlot (KMeansResult) == centroidsOf res 0 1 (代表図 = centroid 散布)" $ do
      let a = vsLayers (toPlot kres)
          b = vsLayers (centroidsOf kres 0 1)
      length a `shouldBe` 1
      map (getFirst . lyKind)  a `shouldBe` map (getFirst . lyKind)  b
      map (getLast  . lyShape) a `shouldBe` map (getLast  . lyShape) b

    it "clusterScatterOf: データ点を MScatter + ラベル色で 1 layer (点と同順)" $ do
      let ls = vsLayers (clusterScatterOf kdf kres "x" "y")
      length ls `shouldBe` 1
      let l = head ls
      getFirst (lyKind l) `shouldBe` Just MScatter
      case (getLast (lyEncX l), getLast (lyEncY l)) of
        (Just (ColNum vx), Just (ColNum vy)) -> do
          V.toList vx `shouldBe` [0.9, 1.1, 5.2, 4.8, 1.0]
          V.length vy `shouldBe` 5
        _ -> expectationFailure "data 散布の encX/encY が inline ColNum でない"
      -- 色 = kmrLabels を文字列化した categorical (5 点ぶん)
      case getLast (lyColor l) of
        Just (ColorByCol (ColTxt v)) -> V.toList v `shouldBe` ["0", "0", "1", "1", "2"]
        _ -> expectationFailure "data 散布の colorBy が ColTxt ラベルでない"

    it "clusterScatterOf: 存在しない列名は mempty" $ do
      vsLayers (clusterScatterOf kdf kres "nope" "y") `shouldBe` []

  -- =====================================================================
  -- Phase 68 A2: 木/アンサンブル (重要度 bar / 決定木 樹形図)
  -- 結果型を直接構築して構造を検証 (IO/学習不要)。
  -- =====================================================================
  describe "Phase 68 A2: GBM/RFClassifier 重要度 bar + DecisionTree 樹形図" $ do
    -- feature 0 を 2 回・feature 1 を 1 回 split に使う木
    let tree1 = RF.Node 0 0.5 (RF.Leaf 1) (RF.Node 1 0.5 (RF.Leaf 0) (RF.Leaf 1))
        tree2 = RF.Node 0 0.5 (RF.Leaf 0) (RF.Leaf 1)

    it "treeImportances: split 使用回数を正規化 (feat0 2回 / feat1 1回 → [2/3, 1/3])" $ do
      treeImportances [tree1, tree2] `shouldSatisfy`
        (\xs -> length xs == 2
                && abs (xs !! 0 - 2/3) < 1e-9
                && abs (xs !! 1 - 1/3) < 1e-9)

    it "treeImportances: 空入力は []" $ do
      treeImportances [] `shouldBe` []

    it "GBRegressor toPlot: 1 bar layer・重要度は合計 1 に正規化" $ do
      let gb = GBRegressor { gbrInit = 0, gbrTrees = [tree1, tree2], gbrLR = 0.1 }
          ls = vsLayers (toPlot gb)
      length ls `shouldBe` 1
      getFirst (lyKind (head ls)) `shouldBe` Just MBar
      case getLast (lyEncY (head ls)) of
        Just (ColNum v) -> do
          V.length v `shouldBe` 2
          abs (V.head v - 2/3) `shouldSatisfy` (< 1e-9)
          sum (V.toList v) `shouldSatisfy` (\s -> abs (s - 1) < 1e-9)
        _ -> expectationFailure "GBM importance bar の encY が inline ColNum でない"

    it "GBClassifier toPlot: 1 bar layer (MBar)" $ do
      let gb = GBClassifier { gbcInit = 0, gbcTrees = [tree2], gbcLR = 0.1 }
          ls = vsLayers (toPlot gb)
      length ls `shouldBe` 1
      getFirst (lyKind (head ls)) `shouldBe` Just MBar

    it "RFClassifierFit toPlot: 2 パネル (permutation/gini)・実列名 (75.24b)" $ do
      let fit = RFClassifierFit
                  { rfcTrees          = []
                  , rfcOOBSamples     = []
                  , rfcClasses        = [0, 1]
                  , rfcOOBError       = 0
                  , rfcImportance     = LA.fromList [1, 3]
                  , rfcGiniImportance = LA.fromList [0.25, 0.75]
                  , rfcFeatureNames   = ["a", "b"]
                  , rfcConfig         = defaultRFCConfig
                  }
          panels = vsSubplots (toPlot fit)
      length panels `shouldBe` 2                          -- permutation + gini
      let permLayer = head (vsLayers (head panels))       -- 左 = permutation
      getFirst (lyKind permLayer) `shouldBe` Just MBar
      case getLast (lyEncY permLayer) of
        Just (ColNum v) -> V.toList v `shouldBe` [1, 3]   -- permutation raw (データ順・sort は limits 側)
        _ -> expectationFailure "RFClassifier importance bar の encY が inline ColNum でない"

    it "DTree toPlot: MDAG 樹形図を 1 layer (split=矩形/葉)" $ do
      let dt = DNode 1 3.5 (DLeaf (Map.fromList [(0,1)]) 0 5 0)
                           (DLeaf (Map.fromList [(1,1)]) 1 4 0)
                           9 0.49 (Map.fromList [(0,0.55),(1,0.45)]) 0
          ls = vsLayers (toPlot dt)
      length ls `shouldBe` 1
      getFirst (lyKind (head ls)) `shouldBe` Just MDAG

  -- =====================================================================
  -- Phase 75.27: 部分従属図 (PDP / ICE)
  -- 純粋エンジンは PartialDependenceSpec で検証済。 ここは VisualSpec への
  -- 落とし込み (line layer 数・grid・encY 値) を検証。
  -- =====================================================================
  describe "Phase 75.27: PDP / ICE (pdpPlot / partialDependencePlot)" $ do
    -- 3 行 × 2 列。 列 0 = {0,1,2}, 列 1 = {10,20,30}。
    let trainX  = LA.fromLists [[0, 10], [1, 20], [2, 30]]
        -- 加法 predict f = 2*x0 + x1 → 特徴 0 の PDP = 2*grid + mean(x1) = 2*grid + 20。
        predict m = [ 2 * (row LA.! 0) + (row LA.! 1) | row <- LA.toRows m ]

    it "partialDependencePlot (閉包): 1 line layer・grid 40 点・PDP = 2*grid+20" $ do
      let ls = vsLayers (partialDependencePlot trainX predict 0 "x0")
      length ls `shouldBe` 1
      getFirst (lyKind (head ls)) `shouldBe` Just MLine
      case getLast (lyEncY (head ls)) of
        Just (ColNum v) -> do
          V.length v `shouldBe` 40
          abs (V.head v - 20) `shouldSatisfy` (< 1e-9)   -- g=0 → 20
          abs (V.last v - 24) `shouldSatisfy` (< 1e-9)   -- g=2 → 24
        _ -> expectationFailure "PDP line の encY が inline ColNum でない"

    it "partialDependenceIcePlot (閉包): ICE n 本 + PDP 1 本 = n+1 line layer" $ do
      let ls = vsLayers (partialDependenceIcePlot trainX predict 0 "x0")
      length ls `shouldBe` 4                              -- 3 ICE + 1 PDP
      all ((== Just MLine) . getFirst . lyKind) ls `shouldBe` True

    it "pdpPlot (RegPredict GBRegressor instance): line layer を出す" $ do
      let t  = RF.Node 0 0.5 (RF.Leaf 0) (RF.Leaf 1)
          gb = GBRegressor { gbrInit = 0, gbrTrees = [t], gbrLR = 1 }
          ls = vsLayers (pdpPlot gb trainX 0 "x0")
      length ls `shouldBe` 1
      getFirst (lyKind (head ls)) `shouldBe` Just MLine

    it "pdpOf (高レベル・df + 列名): 行列版 pdpPlot と一致" $ do
      let t   = RF.Node 0 0.5 (RF.Leaf 0) (RF.Leaf 1)
          gb  = GBRegressor { gbrInit = 0, gbrTrees = [t], gbrLR = 1 }
          -- trainX = [[0,10],[1,20],[2,30]] を df 化。
          df  = [ ("x0", NumData (V.fromList [0, 1, 2]))
                , ("x1", NumData (V.fromList [10, 20, 30])) ] :: [(Text, ColData)]
          encOf spec = getLast (lyEncY (head (vsLayers spec)))
      encOf (pdpOf gb df ["x0","x1"] "x0") `shouldBe` encOf (pdpPlot gb trainX 0 "x0")

    it "pdpOf: target が featCols に無ければ空 spec" $ do
      let t  = RF.Node 0 0.5 (RF.Leaf 0) (RF.Leaf 1)
          gb = GBRegressor { gbrInit = 0, gbrTrees = [t], gbrLR = 1 }
          df = [ ("x0", NumData (V.fromList [0, 1, 2])) ] :: [(Text, ColData)]
      vsLayers (pdpOf gb df ["x0"] "nope") `shouldBe` []

    it "列外 index は空 spec" $
      vsLayers (partialDependencePlot trainX predict 9 "x9") `shouldBe` []

  -- =====================================================================
  -- Phase 68 A3: 分類 (決定境界 + confusion + 代表散布)
  -- KNNClassifier を直接構築 (1-NN・2 点) して構造を検証。
  -- =====================================================================
  describe "Phase 68 A3: ClassPredict / decisionBoundaryOf / confusionOf" $ do
    let knn = KNNClassifier
                { knnCK          = 1
                , knnCX          = LA.fromLists [[0, 0], [4, 4]]
                , knnCY          = VU.fromList [0, 1]
                , knnCClasses    = [0, 1]
                , knnCClassNames = []
                }

    it "predictClasses (1-NN): 近い訓練点のクラスを返す" $ do
      predictClasses knn (LA.fromLists [[0, 0], [4, 4], [0.5, 0.5], [3.6, 3.6]])
        `shouldBe` [0, 1, 0, 1]

    it "decisionBoundaryOf: res×res の annotRect grid で領域塗り (Phase 76.A)" $ do
      let vs = decisionBoundaryOf knn (0, 4) (0, 4) 3
      -- layer は持たず (塗りは全て annotation)、 res² = 9 個の塗り矩形を敷き詰める。
      vsLayers vs `shouldBe` []
      length (vsAnnotations vs) `shouldBe` 9
      -- 全 annotation が AnnRect (塗り矩形) であること。
      let isAnnRect a = case a of AnnRect{} -> True; _ -> False
      all isAnnRect (vsAnnotations vs) `shouldBe` True
      -- 軸ドメインをグリッド範囲へ固定 (expand=FALSE・はみ出し防止)。
      getLast (vsCoordXLim vs) `shouldBe` Just (0, 4)
      getLast (vsCoordYLim vs) `shouldBe` Just (0, 4)

    it "confusionOf: MHeatmap + MLabel (件数注釈)・セル数 = クラス数² (完全予測で対角)" $ do
      let spec = confusionOf knn (LA.fromLists [[0, 0], [4, 4]]) [0, 1]
          ls   = vsLayers spec
      map (getFirst . lyKind) ls `shouldBe` [Just MHeatmap, Just MLabel]   -- Phase 75.2
      case getLast (lyEncY (head ls)) of
        Just (ColTxt v) -> V.length v `shouldBe` 4      -- 2 クラス × 2
        _               -> expectationFailure "confusion の encY が inlineCat でない"
      -- 件数注釈 (MLabel) は cells 数 (=4) の数値ラベルを持ち、 値は counts と一致
      -- (完全予測ゆえ対角 = [1,0,0,1] = (t,p)∈{(0,0),(0,1),(1,0),(1,1)} の件数)。
      case getLast (lyLabel (ls !! 1)) of
        Just (ColTxt v) -> V.toList v `shouldBe` ["1", "0", "0", "1"]
        _               -> expectationFailure "confusion 件数注釈の lyLabel が ColTxt でない"

    -- Phase 75.21: MDS モデル型 (df |-> mds) + 単色/群色 toPlot。
    it "df |-> mds: 埋め込みの散布点数 = n (toPlot m・単色)" $ do
      let df = [ ("x1", NumData (V.fromList [0,1,5,6]))
               , ("x2", NumData (V.fromList [0,1,5,6]))
               , ("x3", NumData (V.fromList [0,1,5,6]))
               , ("cls", NumData (V.fromList [0,0,1,1])) ] :: [(Text, ColData)]
          m  = df |-> mds defaultMDS ["x1","x2","x3"]
          sl = head [ l | l <- vsLayers (toPlot m), getFirst (lyKind l) == Just MScatter ]
      case getLast (lyEncX sl) of
        Just (ColNum v) -> V.length v `shouldBe` 4
        _               -> expectationFailure "mds toPlot encX が ColNum でない"

    it "toPlot (mdsView m <> mdsGroupBy cls): 群色列が colorBy に乗る" $ do
      let df = [ ("x1", NumData (V.fromList [0,1,5,6]))
               , ("x2", NumData (V.fromList [0,1,5,6]))
               , ("x3", NumData (V.fromList [0,1,5,6]))
               , ("cls", TxtData (V.fromList ["a","a","b","b"])) ] :: [(Text, ColData)]
          m  = df |-> mds defaultMDS ["x1","x2","x3"]
          sl = head [ l | l <- vsLayers (toPlot (mdsView m <> mdsGroupBy "cls"))
                        , getFirst (lyKind l) == Just MScatter ]
      -- 群色列があれば lyColor (categorical) が入る。
      case getLast (lyColor sl) of
        Just _  -> pure ()
        Nothing -> expectationFailure "mdsGroupBy で lyColor が入らない"

    it "nnLossOf: 損失曲線の点数 = epoch 数 (mlpLossHist)・MLPFit decisionBoundaryOf" $ do
      let xMat = LA.fromLists [[0, 0], [0, 1], [4, 4], [4, 5]]
          yLab = VU.fromList [0, 0, 1, 1]
      gen <- MWC.initialize (V.fromList [7])
      fit <- fitMLPClassifier defaultMLP xMat yLab gen
      let ln = head [ l | l <- vsLayers (nnLossOf fit), getFirst (lyKind l) == Just MLine ]
      length (mlpLossHist fit) `shouldSatisfy` (> 0)
      case getLast (lyEncX ln) of
        Just (ColNum v) -> V.length v `shouldBe` length (mlpLossHist fit)
        _               -> expectationFailure "nnLoss encX が ColNum でない"
      length (vsAnnotations (decisionBoundaryOf fit (0, 4) (0, 5) 8)) `shouldBe` 64

    -- Phase 75.8: 乱数純粋化 (NN は seed でビット一致)。
    it "fitMLPClassifierPure ≡ fitMLPClassifier (同 seed)・損失ビット一致" $ do
      let xMat = LA.fromLists [[0, 0], [0, 1], [4, 4], [4, 5]]
          yLab = VU.fromList [0, 0, 1, 1]
      gen <- MWC.initialize (V.fromList [99])
      ioFit <- fitMLPClassifier defaultMLP xMat yLab gen
      let pFit = fitMLPClassifierPure defaultMLP xMat yLab 99
      mlpLossHist pFit `shouldBe` mlpLossHist ioFit

    -- Phase 75.9: 高レベル df |-> (mlpCls)。
    it "df |-> mlpCls: MLPFit を返し損失曲線/決定境界が出る" $ do
      let df2 = [ ("x1", NumData (V.fromList [0,0,4,4]))
                , ("x2", NumData (V.fromList [0,1,4,5]))
                , ("cls", NumData (V.fromList [0,0,1,1])) ] :: [(Text, ColData)]
          m  = df2 |-> mlpCls defaultMLP 7 ["x1","x2"] "cls"
      length (mlpLossHist m) `shouldSatisfy` (> 0)
      length (vsAnnotations (decisionBoundaryOf m (0,4) (0,5) 8)) `shouldBe` 64

    -- Phase 75.11/75.12: カーネル SVM の高レベル + SV 可視化。
    it "df |-> svmCls: SVMMulti・非線形 decisionBoundaryOf・SV 強調" $ do
      let df = [ ("x1", NumData (V.fromList [0,0.2,-0.2, 3,-3,0,0,2.1,-2.1]))
               , ("x2", NumData (V.fromList [0,0.1,0.1, 0,0,3,-3,2.1,-2.1]))
               , ("cls", NumData (V.fromList [0,0,0, 1,1,1,1,1,1])) ] :: [(Text, ColData)]
          -- RBF・γ=0.5 ⇔ ℓ=1 (γ=1/(2ℓ²)・Phase 75.15 共有 Kernel)。
          m  = df |-> svmCls defaultSVM
                        { svmKernel = RBF, svmParams = defaultKernelParams, svmC = 10 }
                        ["x1","x2"] "cls"
      length (svmmClasses m) `shouldBe` 2
      length (vsAnnotations (decisionBoundaryOf m (-4,4) (-4,4) 8)) `shouldBe` 64
      let bin = head (svmmBinaries m)
          sv  = vsLayers (svmSupportVectorsOf bin)
      numSupportVectors bin `shouldSatisfy` (> 0)
      length sv `shouldSatisfy` (> 0)
      -- 決定境界を線 (スコア=0 等高線) で: 内外を分ける曲線ゆえ segment が出る。
      length (vsLayers (decisionLineOf bin (-4,4) (-4,4) 40)) `shouldSatisfy` (> 0)

    it "KNNClassifier toPlot: 訓練点をラベル色で散布 (MScatter)" $ do
      let ls = vsLayers (toPlot knn)
      length ls `shouldBe` 1
      getFirst (lyKind (head ls)) `shouldBe` Just MScatter
      case getLast (lyColor (head ls)) of
        Just (ColorByCol (ColTxt v)) -> V.toList v `shouldBe` ["0", "1"]
        _ -> expectationFailure "KNN 訓練散布の colorBy が ColTxt ラベルでない"

  -- =====================================================================
  -- Phase 68 A4: 次元圧縮 (PLS score/loading/VIP, MultiGP 多出力曲線)
  -- =====================================================================
  describe "Phase 68 A4: PLS + MultiGP" $ do
    let xP   = LA.fromLists [[1,0],[0,1],[2,1],[1,2],[3,2]]   -- 5×2
        yP   = LA.fromLists [[1],[2],[3],[4],[5]]             -- 5×1
        plsFit = either (error . T.unpack) id
                   (fitPLS defaultPLS { plsN_Components = 2 } xP yP)

    it "scoreView: 1 MScatter layer・点数 = 標本数 n" $ do
      let ls = vsLayers (toPlot (scoreView plsFit))
      length ls `shouldBe` 1
      getFirst (lyKind (head ls)) `shouldBe` Just MScatter
      case getLast (lyEncX (head ls)) of
        Just (ColNum v) -> V.length v `shouldBe` 5
        _               -> expectationFailure "score plot の encX が inline ColNum でない"

    it "vipView: 1 MBar layer・本数 = 特徴数 p" $ do
      let ls = vsLayers (toPlot (vipView plsFit))
      length ls `shouldBe` 1
      getFirst (lyKind (head ls)) `shouldBe` Just MBar
      case getLast (lyEncY (head ls)) of
        Just (ColNum v) -> V.length v `shouldBe` 2
        _               -> expectationFailure "VIP bar の encY が inline ColNum でない"

    it "multiGpCurves: 出力ごとに band+line の 2 layer (2 出力 → 4 layer)" $ do
      let res = MultiGPResult
                  { mgpMean  = [[1, 2, 3], [3, 2, 1]]
                  , mgpLower = [[0, 1, 2], [2, 1, 0]]
                  , mgpUpper = [[2, 3, 4], [4, 3, 2]]
                  , mgpModels = []
                  }
          ls  = vsLayers (multiGpCurves res)
      length ls `shouldBe` 4
      map (getFirst . lyKind) ls
        `shouldBe` [Just MBand, Just MLine, Just MBand, Just MLine]

    it "MultiGPResult toPlot == multiGpCurves" $ do
      let res = MultiGPResult
                  { mgpMean = [[1, 2]], mgpLower = [[0, 1]]
                  , mgpUpper = [[2, 3]], mgpModels = [] }
      length (vsLayers (toPlot res)) `shouldBe` length (vsLayers (multiGpCurves res))

  -- =====================================================================
  -- Phase 70.B2/B3: PLS effect plot (plsModel + statModelMulti + selectOutput)
  -- =====================================================================
  describe "Phase 70.B2/B3: PLS effect plot + 出力セレクタ" $ do
    -- 2 入力・2 出力。 y1 = 2*x1、 y2 = -3*x2 (出力で傾きの符号が違う = セレクタ確認用)。
    let n    = 30
        x1   = [ fromIntegral i * 0.2       | i <- [1 .. n :: Int] ]
        x2   = [ 1 + sin (fromIntegral i)   | i <- [1 .. n :: Int] ]
        y1   = [ 2 * a            | a <- x1 ]
        y2   = [ negate (3 * b)   | b <- x2 ]
        dfP  = [ ("x1", NumData (V.fromList x1)), ("x2", NumData (V.fromList x2))
               , ("y1", NumData (V.fromList y1)), ("y2", NumData (V.fromList y2)) ]
               :: [(Text, ColData)]
        cfgP = defaultPLS { plsN_Components = 2 }
        m    = either error id (plsModel cfgP ["x1", "x2"] ["y1", "y2"] dfP)
        -- effect plot の MLine layer の encY (μ 曲線) を取り出す
        -- (ModelSpec は Plottable・データはモデル frame 内ゆえ df |>> 不要)。
        effLine mdl =
          let ls = vsLayers (toPlot (statModelMulti mdl (along "x1")))
              ln = head [ l | l <- ls, getFirst (lyKind l) == Just MLine ]
          in case getLast (lyEncY ln) of
               Just (ColNum v) -> V.toList v
               _               -> []

    it "plsModel + statModelMulti: along 曲線 (MLine) を 1 本・band 非提供" $ do
      let ls = vsLayers (toPlot (statModelMulti m (along "x1")))
      map (getFirst . lyKind) ls `shouldBe` [Just MLine]   -- band 無し (PLS CI 非提供)

    it "selectOutput: 第0出力 (y1) は x1 増で増加・既定と一致" $ do
      let muDefault = effLine m
          muY1      = effLine (selectOutput "y1" m)
      muDefault `shouldSatisfy` allClose muY1                  -- 既定 = 第0出力
      head muY1 `shouldSatisfy` (< last muY1)                  -- y1 ∝ +x1 → 単調増

    it "selectOutput: 第1出力 (y2) は第0出力と異なる曲線 (出力選択が効く)" $ do
      let muY1 = effLine (selectOutput "y1" m)
          muY2 = effLine (selectOutput "y2" m)
      (muY1 == muY2) `shouldBe` False                         -- 出力で曲線が変わる

    it "selectOutput: 未知の出力名は無変更 (既定 = 第0出力のまま)" $ do
      effLine (selectOutput "nope" m) `shouldSatisfy` allClose (effLine m)

  -- =====================================================================
  -- Phase 68 A5: 時系列・生存・FDA (GARCH / AFT / FunctionalPCA / FLM)
  -- =====================================================================
  describe "Phase 68 A5: GARCH / AFT / FDA" $ do
    it "GARCHFit toPlot: band + line の 2 layer・hi ≥ lo" $ do
      let gf = GARCHFit { gOmega = 0.1, gAlpha = 0.1, gBeta = 0.8, gMu = 0
                        , gSigma2 = LA.fromList [1, 4, 1]
                        , gResiduals = LA.fromList [0.5, -1, 0.2], gLogLik = 0 }
          ls = vsLayers (garchVolatility gf)
      length ls `shouldBe` 2
      map (getFirst . lyKind) ls `shouldBe` [Just MBand, Just MLine]
      case (getLast (lyEncY (head ls)), getLast (lyEncY2 (head ls))) of
        (Just (ColNum lo), Just (ColNum hi)) ->
          zipWith (-) (V.toList hi) (V.toList lo) `shouldSatisfy` all (>= 0)
        _ -> expectationFailure "GARCH band の encY/encY2 が ColNum でない"

    it "AFTFit toPlot: 生存曲線 S(t) は [0,1]・単調非増加・始点 ≈ 1" $ do
      let af = AFTFit { aftBeta = LA.fromList [1, 0.5], aftScale = 1
                      , aftLogLik = 0, aftDistribution = AFTWeibull, aftIters = 1 }
          ls = vsLayers (toPlot af)
      length ls `shouldBe` 1
      getFirst (lyKind (head ls)) `shouldBe` Just MLine
      case getLast (lyEncY (head ls)) of
        Just (ColNum v) -> do
          let ss = V.toList v
          all (\s -> s >= -1e-9 && s <= 1 + 1e-9) ss `shouldBe` True
          and (zipWith (>=) ss (drop 1 ss)) `shouldBe` True   -- 単調非増加
          head ss `shouldSatisfy` (> 0.9)
        _ -> expectationFailure "AFT survival の encY が ColNum でない"

    it "aftSurvivalAt: 共変量を変えると線形予測子で生存が変わる" $ do
      let af = AFTFit { aftBeta = LA.fromList [1, 0.5], aftScale = 1
                      , aftLogLik = 0, aftDistribution = AFTWeibull, aftIters = 1 }
      length (vsLayers (aftSurvivalAt af [1, 2])) `shouldBe` 1

    it "FunctionalPCA toPlot: 平均 + 上位固有関数 (≤3) を line 重畳 (4 layer)" $ do
      let fpca = FunctionalPCA
                   { fpcaScores      = LA.fromLists [[1, 0, 0]]
                   , fpcaEigenfn     = LA.fromLists [[0,1,2],[2,1,0],[1,1,1],[0,0,1]]  -- 4 PC
                   , fpcaEigenvalues = LA.fromList [4, 2, 1, 0.5]
                   , fpcaMeanFn      = LA.fromList [1, 2, 3] }
          ls = vsLayers (toPlot fpca)
      length ls `shouldBe` 4    -- mean + 上位 3 PC
      all (\l -> getFirst (lyKind l) == Just MLine) ls `shouldBe` True

    it "FLMResult toPlot: β(t) を 1 MLine" $ do
      let flm = FLMResult { flmAlpha = 0, flmBetaFn = LA.fromList [0.1, 0.3, 0.2]
                          , flmFitted = LA.fromList [1, 2], flmR2 = 0.8 }
          ls = vsLayers (toPlot flm)
      length ls `shouldBe` 1
      getFirst (lyKind (head ls)) `shouldBe` Just MLine

  -- =====================================================================
  -- Phase 68 A6: 罰則回帰・因果 (RegFit bar / 係数パス / LiNGAM DAG)
  -- =====================================================================
  describe "Phase 68 A6: Regularized + LiNGAM" $ do
    it "RegFit toPlot: 係数 bar (本数 = β の長さ)" $ do
      let rf = RegFit { rfBeta = LA.fromList [0.5, 1.2, 0]
                      , rfYHat = LA.fromList [1], rfResid = LA.fromList [0]
                      , rfR2 = 0.9, rfPenalty = NoPen, rfNonZero = 2, rfIters = 0 }
          ls = vsLayers (toPlot rf)
      length ls `shouldBe` 1
      getFirst (lyKind (head ls)) `shouldBe` Just MBar
      case getLast (lyEncY (head ls)) of
        Just (ColNum v) -> V.length v `shouldBe` 3
        _               -> expectationFailure "RegFit bar の encY が ColNum でない"

    it "regPathPlot: 係数ごとに 1 line (2 係数 → 2 layer)" $ do
      let path = [ (0.1, [3.0, 1.0]), (0.5, [2.0, 0.5]), (1.0, [0.0, 0.0]) ]
          ls   = vsLayers (regPathPlot path)
      length ls `shouldBe` 2
      all (\l -> getFirst (lyKind l) == Just MLine) ls `shouldBe` True

    it "regPathPlot: 空パスは mempty" $ do
      vsLayers (regPathPlot []) `shouldBe` []

    it "DirectLiNGAMFit toPlot: 因果 DAG を MDAG 1 layer (x0->x1->x2)" $ do
      let adj = LA.fromLists [[0,0,0],[1,0,0],[0,1,0]]   -- edge 0->1, 1->2
          fit = DirectLiNGAMFit { dlOrder = [0,1,2], dlB = adj, dlAdjacency = adj
                                , dlResiduals = LA.fromLists [[0,0,0]] }
          ls  = vsLayers (toPlot fit)
      length ls `shouldBe` 1
      getFirst (lyKind (head ls)) `shouldBe` Just MDAG

    -- Phase 77.A: 高レベル df |-> directLingam (変数名保持・名前付き DAG)。
    it "df |-> directLingam: 変数名保持・低レベル fit と一致・toPlot は名前付き MDAG" $ do
      let xs0 = [0.5, -0.3, 0.8, -0.6, 0.2, -0.9, 0.7, -0.1]
          xs1 = [1.1, -0.5, 1.7, -1.3, 0.3, -1.9, 1.5, -0.4]   -- ≈ 2·xs0 + 揺らぎ
          xs2 = [-1.5, 0.8, -2.4, 2.0, -0.6, 2.7, -2.1, 0.5]   -- ≈ -1.5·xs1 + 揺らぎ
          df  = [ ("smoking", NumData (V.fromList xs0))
                , ("tar",     NumData (V.fromList xs1))
                , ("cancer",  NumData (V.fromList xs2)) ] :: [(Text, ColData)]
          mat = LA.fromColumns [LA.fromList xs0, LA.fromList xs1, LA.fromList xs2]
          fitted = df |-> directLingam defaultDirectLiNGAMConfig ["smoking","tar","cancer"]
      lfNames fitted `shouldBe` ["smoking","tar","cancer"]
      -- df|-> は低レベル fitDirectLiNGAM と同一 (列名経路と行列経路が一致)
      LA.toLists (dlAdjacency (lfFit fitted))
        `shouldBe` LA.toLists (dlAdjacency (fitDirectLiNGAM defaultDirectLiNGAMConfig mat))
      -- toPlot は名前付き DAG (1 MDAG layer)
      let ls2 = vsLayers (toPlot fitted)
      length ls2 `shouldBe` 1
      getFirst (lyKind (head ls2)) `shouldBe` Just MDAG

    -- Phase 77.B: Parce / MultiGroup も高レベル df|-> + 名前付き DAG。
    it "df |-> parceLingam: 変数名保持・toPlot は名前付き MDAG" $ do
      let xs0 = [0.5,-0.3,0.8,-0.6,0.2,-0.9,0.7,-0.1,0.4,-0.5]
          xs1 = [1.1,-0.5,1.7,-1.3,0.3,-1.9,1.5,-0.4,0.9,-1.0]
          xs2 = [-1.5,0.8,-2.4,2.0,-0.6,2.7,-2.1,0.5,-1.2,1.4]
          df  = [ ("a", NumData (V.fromList xs0)), ("b", NumData (V.fromList xs1))
                , ("c", NumData (V.fromList xs2)) ] :: [(Text, ColData)]
          fitted = df |-> parceLingam defaultParceConfig ["a","b","c"]
      lfNames fitted `shouldBe` ["a","b","c"]
      getFirst (lyKind (head (vsLayers (toPlot fitted)))) `shouldBe` Just MDAG

    it "df |-> multiGroupLingam: group 列で分割・共通 DAG (名前付き MDAG)" $ do
      let g0x0 = [0.5,-0.3,0.8,-0.6,0.2,-0.9]; g0x1 = [1.1,-0.5,1.7,-1.3,0.3,-1.9]
          g1x0 = [0.4,-0.5,0.7,-0.2,0.6,-0.8]; g1x1 = [0.9,-1.0,1.5,-0.4,1.2,-1.6]
          df  = [ ("a",   NumData (V.fromList (g0x0 ++ g1x0)))
                , ("b",   NumData (V.fromList (g0x1 ++ g1x1)))
                , ("grp", NumData (V.fromList (replicate 6 0 ++ replicate 6 1))) ] :: [(Text, ColData)]
          fitted = df |-> multiGroupLingam defaultMultiGroupConfig ["a","b"] "grp"
      lfNames fitted `shouldBe` ["a","b"]
      getFirst (lyKind (head (vsLayers (toPlot fitted)))) `shouldBe` Just MDAG

    it "df |-> varLingam: 時系列因果・toPlot は時間ラグ MDAG" $ do
      let n = 40 :: Int
          ea = [ sin (fromIntegral i * 0.7) | i <- [1..n] ]
          eb = [ cos (fromIntegral i * 0.9) | i <- [1..n] ]
          go (aP,bP) (x,y) = let a = 0.5*aP + x; b = 0.6*a + 0.3*bP + 0.3*y in (a,b)
          ps = tail (scanl go (0,0) (zip ea eb))
          df = [ ("a", NumData (V.fromList (map fst ps)))
               , ("b", NumData (V.fromList (map snd ps))) ] :: [(Text, ColData)]
          fitted = df |-> varLingam defaultVARLiNGAMConfig ["a","b"]
      lfNames fitted `shouldBe` ["a","b"]
      getFirst (lyKind (head (vsLayers (toPlot fitted)))) `shouldBe` Just MDAG

    it "df |-> pairwiseLingam: 2 変数の向き・toPlot は 2 ノード MDAG" $ do
      let xs = [0.5,-0.3,0.8,-0.6,0.2,-0.9,0.7,-0.1,0.4,-0.5]
          ys = map (\v -> 2.0*v) xs   -- x → y (関数従属で向き明確)
          df = [ ("x", NumData (V.fromList xs)), ("y", NumData (V.fromList ys)) ] :: [(Text, ColData)]
          fitted = df |-> pairwiseLingam 0.0 "x" "y"
      lfNames fitted `shouldBe` ["x","y"]
      getFirst (lyKind (head (vsLayers (toPlot fitted)))) `shouldBe` Just MDAG

    -- Phase 77.C: Bootstrap / ICA の seed 純粋版 (IO とビット一致) + 高レベル + plot。
    let semABC = let xs0 = [0.5,-0.3,0.8,-0.6,0.2,-0.9,0.7,-0.1,0.4,-0.5,0.3,-0.7]
                     xs1 = zipWith (\a i -> 2.0*a + 0.2*sin (fromIntegral i)) xs0 [0..]
                     xs2 = zipWith (\b i -> -1.5*b + 0.2*cos (fromIntegral i)) xs1 [0..]
                 in (xs0, xs1, xs2)
        (sa, sb, sc) = semABC
        semMat = LA.fromColumns [LA.fromList sa, LA.fromList sb, LA.fromList sc]
        semDF  = [ ("a", NumData (V.fromList sa)), ("b", NumData (V.fromList sb))
                 , ("c", NumData (V.fromList sc)) ] :: [(Text, ColData)]
        bcfg = defaultBootstrapConfig { bcNumBootstraps = 20 }

    it "fitBootstrapLiNGAMPure ≡ fitBootstrapLiNGAM (同 seed・edge 確率ビット一致)" $ do
      ioRes <- fitBootstrapLiNGAM bcfg semMat
      let pRes = fitBootstrapLiNGAMPure bcfg semMat
      LA.toLists (brEdgeProbability pRes) `shouldBe` LA.toLists (brEdgeProbability ioRes)

    it "df |-> bootstrapLingam: 確信度 DAG (MDAG) + edge 確率ヒートマップ (MHeatmap)" $ do
      let fitted = semDF |-> bootstrapLingam bcfg ["a","b","c"]
      lfNames fitted `shouldBe` ["a","b","c"]
      getFirst (lyKind (head (vsLayers (toPlot fitted)))) `shouldBe` Just MDAG
      getFirst (lyKind (head (vsLayers (bootstrapEdgeProbOf fitted)))) `shouldBe` Just MHeatmap

    it "fitICALiNGAMPure ≡ fitICALiNGAM (同 seed・adjacency ビット一致)" $ do
      ioFit <- fitICALiNGAM defaultICALiNGAMConfig semMat
      let pFit = fitICALiNGAMPure defaultICALiNGAMConfig semMat
      LA.toLists (ilAdjacency pFit) `shouldBe` LA.toLists (ilAdjacency ioFit)

    it "df |-> icaLingam: 名前付き DAG (MDAG)" $ do
      let fitted = semDF |-> icaLingam defaultICALiNGAMConfig ["a","b","c"]
      lfNames fitted `shouldBe` ["a","b","c"]
      getFirst (lyKind (head (vsLayers (toPlot fitted)))) `shouldBe` Just MDAG

    -- Phase 77: 相関ネットワーク (高レベル df|-> correlationOf → toPlot)。
    it "df |-> correlationOf: 相関行列 (対角=1) + toPlot は相関グラフ (MDAG)" $ do
      let cg = semDF |-> correlationOf 0.3 ["a","b","c"]
      cgNames cg `shouldBe` ["a","b","c"]
      cgThreshold cg `shouldBe` 0.3
      -- 相関行列の対角は 1 (自己相関)
      [ cgCorr cg `LA.atIndex` (i,i) | i <- [0..2] ] `shouldSatisfy` all ((< 1e-9) . abs . subtract 1)
      getFirst (lyKind (head (vsLayers (toPlot cg)))) `shouldBe` Just MDAG

    -- Phase 78.C/D/F: DOE prediction profiler (行=応答 × 列=因子・toPlot + <> オプション)。
    it "multiOutput + profiler: 応答×因子ぶんの subplots (2 応答 × 2 因子 = 4)" $ do
      let plan   = factorialDesign [contFactor "a" (0,1), contFactor "b" (0,1)]
          rs     = designTable plan
          av     = maybe [] id (lookup "a" rs); bv = maybe [] id (lookup "b" rs)
          y1     = zipWith (\x y -> 1 + 2*x + 3*y) av bv
          y2     = zipWith (\x y -> 5 + x - y) av bv
          df     = [ (n, NumData (V.fromList v))
                   | (n,v) <- ("y1",y1):("y2",y2):rs ] :: [(Text,ColData)]
          models = df |-> multiOutput ["y1","y2"] (designModel plan)
          vs     = toPlot (profiler models ["a","b"])
      length models `shouldBe` 2                          -- multiOutput = [(応答名, モデル)]
      map fst models `shouldBe` ["y1","y2"]
      length (vsSubplots vs) `shouldBe` 4                 -- 2 応答 × 2 因子
      all (not . null . vsLayers) (vsSubplots vs) `shouldBe` True

    it "profiler: 空 models / 空 factors は mempty (subplots なし)" $ do
      let plan   = factorialDesign [contFactor "a" (0,1)]
          rs     = designTable plan
          av     = maybe [] id (lookup "a" rs)
          df     = [ (n, NumData (V.fromList v)) | (n,v) <- ("y",av):rs ] :: [(Text,ColData)]
          models = df |-> multiOutput ["y"] (designModel plan)
      vsSubplots (toPlot (profiler models []))              `shouldBe` []
      vsSubplots (toPlot (profiler [] ["a"] :: ProfilerSpec MultiLMModel)) `shouldBe` []

    it "profilerResidual: <> で打点モードが後勝ち合成 (Semigroup)" $ do
      psResidual (profiler [] [] <> profilerResidual Partial :: ProfilerSpec MultiLMModel)
        `shouldBe` Just Partial
      psResidual (profilerResidual Raw <> profilerResidual Partial :: ProfilerSpec MultiLMModel)
        `shouldBe` Just Partial      -- 右 (後) 勝ち
      psResidual (profiler [] [] :: ProfilerSpec MultiLMModel)
        `shouldBe` Nothing           -- 既定 (= Raw)

    -- Phase 78.G-e: FRR/HBM 化 (GP/RFF)。MultiVarModel GPRegModelN で profiler/contour に
    -- GP 事後予測帯を出す。分布あり象限 (Gp/GpRff) は帯、mean のみ象限 (Krr) は帯なし。
    it "profiler (GP): 応答×因子の subplots + Gp は予測帯・Krr は帯なし" $ do
      let plan = factorialDesign [contFactor "a" (0,1), contFactor "b" (0,1)]
          rs   = designTable plan
          av   = maybe [] id (lookup "a" rs); bv = maybe [] id (lookup "b" rs)
          y    = zipWith (\x z -> 1 + 2*x + 3*z) av bv
          df   = [ (n, NumData (V.fromList v)) | (n,v) <- ("y",y):rs ] :: [(Text,ColData)]
          mGp  = df |-> gpMulti (GPConfig RBF Gp  AutoMarginalLik) ["a","b"] "y"
          mKrr = df |-> gpMulti (GPConfig RBF Krr AutoMarginalLik) ["a","b"] "y"
          panelLayers mm =
            length (vsLayers (head (vsSubplots (toPlot (profiler [("y", mm)] ["a"])))))
      length (vsSubplots (toPlot (profiler [("y", mGp)] ["a","b"]))) `shouldBe` 2
      panelLayers mGp `shouldSatisfy` (> panelLayers mKrr)   -- Gp = 帯レイヤぶん多い

    -- Phase 78.E: RSM 等高線 / 応答曲面 (contourFilled + contour の 2 層)。
    it "contourOf: 塗り等値帯 (MContourFilled) + 等高線 (MContour) の 2 層" $ do
      let plan = centralCompositeDesign [contFactor "a" (0,1), contFactor "b" (0,1)]
          rs   = designTable plan
          av   = maybe [] id (lookup "a" rs); bv = maybe [] id (lookup "b" rs)
          ys   = zipWith (\x y -> 1 + 2*x + 3*y + x*y) av bv
          df   = [ (n, NumData (V.fromList v)) | (n,v) <- ("y",ys):rs ] :: [(Text,ColData)]
          m    = df |-> designModel plan "y"
          ls   = vsLayers (contourOf m "a" "b")
      length ls `shouldBe` 2
      map (getFirst . lyKind) ls `shouldBe` [Just MContourFilled, Just MContour]

  -- =====================================================================
  -- Phase 68 A7: 記述統計・検定 (test forest / describe box)
  -- =====================================================================
  describe "Phase 68 A7: TestResult forest + describeBox" $ do
    let mkTest nm ci = TestResult
                         { trMethod = nm, trStatistic = 2.0
                         , trDf = Just (10, Nothing), trPValue = 0.04
                         , trEffect = Just ("d", 0.8), trCI = ci
                         , trAlternative = TwoSided, trNote = Nothing }

    it "TestResult toPlot: CI 付き検定を 1 行 forest (MForest)" $ do
      let ls = vsLayers (toPlot (mkTest "t-test" (Just (0.2, 1.4))))
      length ls `shouldBe` 1
      getFirst (lyKind (head ls)) `shouldBe` Just MForest

    it "testForest: CI を持たない検定は除外 (全滅なら mempty)" $ do
      vsLayers (testForest [mkTest "a" Nothing, mkTest "b" Nothing]) `shouldBe` []

    it "testForestLabeled: CI 中心=点推定・半幅=誤差 (1 行 forest)" $ do
      let ls = vsLayers (testForestLabeled [("A vs B", mkTest "t" (Just (-1.0, 0.2)))])
      length ls `shouldBe` 1
      getFirst (lyKind (head ls)) `shouldBe` Just MForest

    it "describeBox: 生データ列を box plot (MBox)" $ do
      let ls = vsLayers (describeBox [1, 2, 3, 4, 5, 6, 7])
      length ls `shouldBe` 1
      getFirst (lyKind (head ls)) `shouldBe` Just MBox

  -- =====================================================================
  -- Phase 72.4/72.5: 回帰診断の可視化 (実測vs予測 / 係数 forest)
  -- =====================================================================
  describe "Phase 72.4/72.5: obsVsPred + coefForest (回帰診断の可視化)" $ do

    it "obsVsPred: y=x 参照線 (MLine) + 散布 (MScatter) の 2 layer" $ do
      let ls = vsLayers (obsVsPred m)
      length ls `shouldBe` 2
      getFirst (lyKind (ls !! 0)) `shouldBe` Just MLine     -- 参照線が下
      getFirst (lyKind (ls !! 1)) `shouldBe` Just MScatter  -- 散布が上

    it "obsVsPred: 散布の (x=実測, y=予測) は obsPredPairs と一致" $ do
      let (obs, prd) = obsPredPairs m
          sl         = vsLayers (obsVsPred m) !! 1
      case (getLast (lyEncX sl), getLast (lyEncY sl)) of
        (Just (ColNum vx), Just (ColNum vy)) -> do
          V.toList vx `shouldSatisfy` allClose obs
          V.toList vy `shouldSatisfy` allClose prd
        _ -> expectationFailure "scatter encX/encY が inline ColNum でない"

    it "obsPredPairs: 完全線形 fit は 実測 == 予測" $ do
      let (obs, prd) = obsPredPairs m
      prd `shouldSatisfy` allClose obs

    it "obsVsPred: y=x 参照線は (lo,hi)→(lo,hi) の 2 点 (傾き 1)" $ do
      let l = head (vsLayers (obsVsPred m))
      case (getLast (lyEncX l), getLast (lyEncY l)) of
        (Just (ColNum vx), Just (ColNum vy)) -> do
          V.length vx `shouldBe` 2
          V.toList vx `shouldBe` V.toList vy   -- y = x
        _ -> expectationFailure "参照線 encX/encY が inline ColNum でない"

    it "coefForest: 係数表を 1 行 forest (MForest)" $ do
      let ls = vsLayers (coefForest m)
      length ls `shouldBe` 1
      getFirst (lyKind (head ls)) `shouldBe` Just MForest

    it "coefForest: 点推定 (encX) は coefSummary の crEstimate と一致 ([1,2])" $ do
      let l = head (vsLayers (coefForest m))
      case getLast (lyEncX l) of
        Just (ColNum v) -> V.toList v `shouldSatisfy` allClose (map crEstimate (coefSummary m))
        _               -> expectationFailure "forest encX が inline ColNum でない"

    it "coefForest: 誤差 (errorX) は CI 半幅 = (hi-lo)/2 と一致" $ do
      let l       = head (vsLayers (coefForest m))
          halfCIs = [ (hi - lo) / 2 | r <- coefSummary m, let (lo, hi) = crCI95 r ]
      case getLast (lyErrorX l) of
        Just (ColNum v) -> V.toList v `shouldSatisfy` allClose halfCIs
        _               -> expectationFailure "forest errorX が inline ColNum でない"

  -- ==========================================================================
  -- Phase 106.4: umbrella の WorkflowSpec から移行した plot 連携診断テスト
  -- (旧 #ifdef PLOT_INTEGRATION 節。 umbrella component は hanalyze-plot に
  --  依存できない = package 循環のため、 本 suite が正式な置き場)。
  describe "Design.Workflow x plot 連携 (Phase 78.J / 78.G-f Task 4)" $ do

    -- Phase 78.J: designModelHBM の学習済 HBM を dhfModel で露出し、 診断抽出子
    -- (tracesOf / dagOf 等) に渡せる (DesignHBMFit から診断が出せる)。
    it "designModelHBM: dhfModel で診断 (tracesOf) が出せる" $ do
      let temps  = [-1, 1, -1, 1, -1, 1, -1, 1] :: [Double]
          lots   = ["A","A","A","A","B","B","B","B"] :: [T.Text]
          yv     = [ 2 + 3 * t + (if l == "A" then -1 else 1) | (t, l) <- zip temps lots ]
          df     = DX.insertColumn "lot"  (DX.fromList lots)
                 $ DX.insertColumn "temp" (DX.fromList temps)
                 $ DX.insertColumn "y"    (DX.fromList yv)
                 $ DX.empty
          plan   = factorialDesign [contFactor "temp" (-1, 1)]
          mWk      = df |-> designModelHBM defaultHBM plan [ranIntercept "lot"] "y"
      length (tracesOf (dhfModel mWk)) `shouldSatisfy` (> 0)   -- param ごとの trace が出る

    -- Phase 78.G-f Task 4: profiler/contour が designModelHBM に載る事後予測帯。
    -- designMatrixF (dhfFormula mWk) ef を直接叩き、 fit 時と同じ列順の設計行列を作る
    -- ( evalFrameAt 相当の helper は無いので eval ModelFrame を手組みする)。
    -- ★mfParams (合成パラメータ名 "_p0"/"_p1"…) は formula 内部表現なので手で推測せず、
    --   訓練済 'dhfFrame' (= mvFrame mWk) を土台に mfRoles/mfNRows だけ差し替える
    --   (本番の Core.hs evalFrame と同じ据え置き方)。
    it "MultiVarModel DesignHBMFit は事後予測帯を返す" $ do
      let temps  = [-1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1] :: [Double]
          lots   = ["A","A","A","A","A","A","B","B","B","B","B","B"] :: [T.Text]
          noise  = [0.05, -0.03, 0.02, -0.04, 0.01, -0.02, 0.03, -0.01, 0.04, -0.05, 0.02, -0.03]
          lotShift l = if l == "A" then (-1.0) else 1.0
          yv     = [ 2 + 3 * t + lotShift l + e
                   | (t, l, e) <- zip3 temps lots noise ]
          mkFrameHBM = DX.insertColumn "lot"  (DX.fromList lots)
                     $ DX.insertColumn "temp" (DX.fromList temps)
                     $ DX.insertColumn "y"    (DX.fromList yv)
                     $ DX.empty
          plan = factorialDesign [contFactor "temp" (-1, 1)]
          mWk    = mkFrameHBM |-> designModelHBM defaultHBM plan [ranIntercept "lot"] "y"
          ef   = (mvFrame mWk)
                   { mfRoles  = [ ("y",    RoleResponse (V.fromList [0, 0, 0]))
                                , ("temp", RoleContinuous (V.fromList [-1, 0, 1]))
                                ]
                   , mfNRows  = 3
                   }
          (mu, band) = mvEvalFrame mWk 0.95 ef
      length mu `shouldBe` 3
      band `shouldSatisfy` isJust
      -- 中心 μ は temp とともに増加 (真の傾き ≈ 3)。
      (last mu - head mu) `shouldSatisfy` (> 3)
