-- |
-- Module      : Hanalyze.Plot.ML
-- Description : hgg 連携層 — ML / 統計モデル連携族の図化 instance + 抽出子
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- hgg 連携層 — **ML / 統計モデル連携族** の図化 instance + 抽出子 (Phase 71.6)。
--
-- ⚠ 親 'Hanalyze.Plot' と同じ cabal flag @plot-integration@ (既定 off) を
-- on にしたときのみ build される。 共通基盤 (class / ModelSpec / grid 評価核) は
-- 'Hanalyze.Plot.Core' を import して取り込む (orphan instance を許容:
-- クラス=Core・instance=ここ・型=Wrappers/各 Model module)。
--
-- 担当する型・ヘルパ (= Phase 68 A1-A7 群):
--   クラスタリング (KMeans) / 木・アンサンブル (PCA/RF/GB/DT) / 分類
--   (Discriminant/NaiveBayes/KNN) / 次元圧縮 (PLS) / 時系列・生存・FDA
--   (Forecast/GARCH/AFT/FunctionalPCA/FLM) / 罰則回帰・因果探索 (Reg/LiNGAM) /
--   記述統計・検定 (TestResult)。 新規 plot mark は不要 (既存 mark の組合せ)。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Hanalyze.Plot.ML
  ( -- * クラスタリング (Phase 68 A1)
    clusterScatterOf
  , centroidsOf
    -- * クラスタを囲む (凸包輪郭 / 95% 共分散楕円) — Phase 76.B
  , clusterHullOf
  , clusterEllipseOf
    -- * DOE prediction profiler — Phase 78.C / 78.D / 78.E / 78.F
  , ResidualMode (..)
  , ProfilerSpec (..)
  , profiler
  , profilerResidual
  , contourOf
    -- * 階層クラスタリング dendrogram — Phase 76.C
  , DendroOpts (..)
  , defaultDendroOpts
  , dendrogramOf
  , dendrogramOf'
    -- * 木/アンサンブル — Phase 68 A2
  , treeImportances
    -- * 決定木 樹形図 (rpart.plot 流・annotation ベース) — Phase 75.26
  , treePlot
  , treePlotRaw
    -- * 分類 — Phase 68 A3
  , decisionBoundaryOf
  , confusionOf
    -- * MDS 埋め込み (モデル型 + 群色オプション) — Phase 75.21
  , MDSView
  , mdsView
  , mdsGroupBy
    -- * NN 可視化 — Phase 75.5
  , nnLossOf
    -- * カーネル SVM サポートベクタ可視化 — Phase 75.12
  , svmSupportVectorsOf
    -- * 決定境界を線で描く (等高線) — Phase 75.13b
  , ScorePredict (..)
  , decisionLineOf
    -- * 部分従属図 (PDP / ICE) — Phase 75.27
  , RegPredict (..)
    -- ** Plottable 中間型 (Phase 76.D・HBM 抽出子と同型・toPlot で描画)
  , PDPView
  , pdp
  , pdpIce
  , pdpOf
  , pdpIceOf
  , pdpPlot
  , pdpIcePlot
  , partialDependencePlot
  , partialDependenceIcePlot
    -- * 次元圧縮 (PLS 診断ビュー) — Phase 68 A4 / 70.B
  , PLSView (..)
  , PLSViewKind (..)
  , scoreView
  , loadingView
  , vipView
    -- * 時系列・生存・FDA — Phase 68 A5
  , garchVolatility
  , aftSurvivalAt
    -- * 罰則回帰・因果探索 — Phase 68 A6
  , regPathPlot
  , lingamDag
  , lingamDagNamed
  , varLagDagNamed
  , bootstrapEdgeProbOf
    -- * 記述統計・検定 — Phase 68 A7
  , testForest
  , testForestLabeled
  , describeBox
  ) where

import           Control.Applicative   ((<|>))
import           Data.Maybe            (fromMaybe)
import           Data.List             (nub, sort, elemIndex, sortBy, foldl')
import           Data.Ord              (comparing)
import qualified Data.Map.Strict       as Map
import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed    as VU
import qualified Numeric.LinearAlgebra as LA

import           Data.Text             (Text)
import qualified Data.Text             as T

import           Hgg.Plot.Spec     ( VisualSpec, layer, inline, inlineCat
                                       , fromHex
                                       , scatter, line, band
                                       , shape, MarkShape (..)
                                       , heatmap, contour, contourFilled, contourLevels
                                       , label, color, colorBy, bar, boxplot, forest, forestNull
                                       , legendOff
                                       , title, coordFlip, coordCartesian, subplots, subplotCols
                                       , scaleXDiscreteLimits
                                       , xLabel, yLabel
                                       , annotTextP, annotRectP
                                       , annotate, Annotation (..)
                                       , theme, ThemeName (..), themeGrid, themeAxisLine, panelBorder
                                       , tickColor
                                       , xAxis, yAxis, hideTicks
                                       , axisBreaksLabeled, axisRotate
                                       , scaleColorManual
                                       , themeLegendFont, fontSize
                                       , alpha
                                       , dagFromListsWithPlates
                                       , DAGNode (..), DAGEdge (..)
                                       , DAGNodeKind (..), DAGLayoutAlgorithm (..) )
import           Hgg.Plot.Unit     (Pos (..))
import           Hgg.Plot.Palette  (ggplotHue)
import           Hgg.Plot.Custom.Dendrogram (DendroSeg (..), DendroPayload (..), dendrogramMark)  -- Phase 48
import           Hgg.Plot.DAG      (layoutHierarchicalFullWithPlates)
import           Hgg.Plot.Render.Special (bakeDAGRoutesInSpec)

import           Numeric               (showFFloat)

import           Hanalyze.Data.ColumnSource     (ColumnSource (..))
import           Hanalyze.Model.Formula.Frame   (ModelFrame (..), VarRole (..))
import           Hanalyze.Model.Formula.Design  (designMatrixF)
import           Hanalyze.Fit                   (DesignHBMFit (..))
import           Hanalyze.Model.Wrappers
import           Hanalyze.Plot.Core
import           Hanalyze.Model.LM     (linspace)
import           Hanalyze.Model.GP     (gpNoiseVar)
import           Hanalyze.Model.Weibull (quantileNormal)
import           Hanalyze.Model.PLS    (predictPLS)
import           Hanalyze.Model.Cluster (KMeansResult (..))
import           Hanalyze.Model.HierarchicalCluster
                   (HClusterFit (..), cutTree)
import           Hanalyze.Model.RandomForest (RandomForest (..), featureImportance, rfPermutationImportance, defaultFeatureNames, Tree)
import qualified Hanalyze.Model.RandomForest as RF
import           Hanalyze.Model.GradientBoosting (GBRegressor (..), GBClassifier (..), predictGBR)
import           Hanalyze.Model.PartialDependence
                   (PDPResult, partialDependence, pdpGrid, pdpMean)   -- pdpIce 欄は PD. で参照 (関数名と衝突回避)
import qualified Hanalyze.Model.PartialDependence as PD
import           Hanalyze.Model.RandomForestClassifier (RFClassifierFit (..))
import           Hanalyze.Model.DecisionTree (DTree (..), DTFit (..))
import           Hanalyze.Model.Discriminant (DiscriminantFit (..), predictDiscriminant)
import           Hanalyze.Model.NaiveBayes (NBModel (..), GaussianNB (..)
                                       , MultinomialNB (..), predictNB)
import           Hanalyze.Model.KNN (KNNClassifier (..), predictKNNC)
import           Hanalyze.Model.NeuralNetwork (MLPFit (..), predictMLPClass)
import           Hanalyze.Model.SVM (SVM (..), SVMMulti (..)
                                       , predictSVM, predictSVMMulti, predictSVMScore)
import           Hanalyze.Model.MDS (MDSResult (..))
import           Hanalyze.DataIO.Convert (getTextVec, getDoubleVec)
import qualified DataFrame.Internal.DataFrame  as DXD
import           Hanalyze.Model.PLS (PLSFit (..))
import           Hanalyze.Model.GARCH (GARCHFit (..))
import           Hanalyze.Model.AFT (AFTFit (..), logS, predictAFT)
import           Hanalyze.Model.FDA (FunctionalPCA (..), FLMResult (..))
import           Hanalyze.Model.Regularized (RegFit (..))
import           Hanalyze.Model.LiNGAM.Direct (DirectLiNGAMFit (..))
import           Hanalyze.Model.LiNGAM.Parce (ParceFit (..))
import           Hanalyze.Model.LiNGAM.MultiGroup (MultiGroupFit (..))
import           Hanalyze.Model.LiNGAM.VAR (VARLiNGAMFit (..))
import           Hanalyze.Model.LiNGAM.Pairwise (PairwiseResult (..), PairwiseDirection (..))
import           Hanalyze.Model.LiNGAM.Bootstrap (BootstrapResult (..))
import           Hanalyze.Model.LiNGAM.ICA (ICALiNGAMFit (..))
import           Hanalyze.Stat.CorrelationNetwork (CorrelationGraph (..))
import           Hanalyze.Stat.Test (TestResult (..))
import           Hanalyze.Model.PCA     (PCAResult (..))
import           Hanalyze.Model.Survival (KMResult (..))
import           Hanalyze.Model.CompetingRisks (CRFit (..))
import           Hanalyze.Model.TimeSeries (ARFit (..), forecastAR)

-- ===========================================================================
-- クラスタリング (KMeans) の図 — Phase 68 A1
--
-- KMeans の分野定番の図は「クラスタ別散布 (色=ラベル)」。 ただし
-- 'KMeansResult' は centroids + labels + inertia のみ保持し **生データ座標を
-- 持たない**。 そこで 'surfaceOf' <> 'dataScatter3DOf' と同じ **model 層 / data
-- 層の二層イディオム**に分ける:
--
--   * 'Plottable' 'KMeansResult' の 'toPlot' = centroid 散布のみ (データ不要・
--     クラス契約 @m -> VisualSpec@ を満たす)。 既定は centroid 行列の第 0/1 次元。
--   * 'clusterScatterOf' = データ点をラベル色で散布 (要データ源・列名指定)。
--   * 'centroidsOf' = centroid を任意 2 次元で重畳 (✚ マーカー・次元 index 明示)。
--
-- 定番図 = @df |>> (clusterScatterOf df res \"x\" \"y\" <> centroidsOf res 0 1)@。
-- ⚠ centroid 行列は **学習時の特徴量列順**のみで列名を持たない。 重畳時は
-- データ列 (@xn@, @yn@) と centroid 次元 (@i@, @j@) の対応をユーザが揃える。
-- ===========================================================================

-- | KMeans クラスタの代表図 = centroid 散布 (第 0/1 次元・クラスタ色・✚ マーカー)。
--   生データ点は 'clusterScatterOf' で別 layer に重ねる
--   (cf. 'surfaceOf' (model) <> 'dataScatter3DOf' (data) の二層イディオム)。
instance Plottable KMeansResult where
  toPlot res = centroidsOf res 0 1

-- | データ点をラベル色で散布する (= KMeans の定番「クラスタ別散布」)。
--   @d@ は 'ColumnSource' (DataFrame / assoc / Map 等)、 @xn@\/@yn@ は描く列名。
--   色はクラスタラベル ('kmrLabels') の categorical (= 点と同順)。
--   列が無ければ空 ('mempty')。
clusterScatterOf :: ColumnSource d => d -> KMeansResult -> Text -> Text -> VisualSpec
clusterScatterOf d res xn yn =
  case (lookupCol xn d, lookupCol yn d) of
    (Just xs, Just ys) ->
      layer ( scatter (inline xs) (inline ys)
            <> colorBy (inlineCat (map (T.pack . show) (kmrLabels res))) )
    _ -> mempty

-- | centroid を任意 2 次元 (@i@, @j@) で散布 (クラスタ色・✚ マーカーで点と区別)。
--   index が centroid 次元数を超える / 負なら空 ('mempty')。
centroidsOf :: KMeansResult -> Int -> Int -> VisualSpec
centroidsOf res i j
  | i < 0 || j < 0 || i >= d || j >= d = mempty
  | otherwise =
      layer ( scatter (inline xs) (inline ys)
            <> colorBy (inlineCat cids)
            <> shape MShCross )
  where
    cs   = kmrCentroids res
    d    = LA.cols cs
    k    = LA.rows cs
    cols = LA.toColumns cs
    xs   = LA.toList (cols !! i)
    ys   = LA.toList (cols !! j)
    cids = map (T.pack . show) [0 .. k - 1 :: Int]

-- | render の categorical 群色 (colorBy → @sort.nub@ 順 → 'ggplotHue') を analyze 側で
--   再現し、 カテゴリ名 → 色(hex) の辞書を返す。 annotation の色は spec 時に確定するため
--   ('clusterScatterOf'/'toPlot' の凡例色と一致させる用)。
hueColorMap :: [Text] -> Map.Map Text Text
hueColorMap labels =
  let cats = sort (nub labels)
  in Map.fromList (zip cats (ggplotHue (length cats) ++ repeat "#cccccc"))

-- | 色・太さ指定の線分注釈 ('annotLineP' は色固定なので 'AnnLine' を直接構築)。
annotLineC :: Text -> Double -> (Double, Double) -> (Double, Double) -> VisualSpec
annotLineC col w (x1, y1) (x2, y2) = annotate AnnLine
  { anX1 = PNative x1, anY1 = PNative y1, anX2 = PNative x2, anY2 = PNative y2
  , anColor = col, anWidth = w }

-- | 頂点列を閉じた折れ線 (最後→最初も結ぶ) として色付き線分で描く。
closedPolyline :: Text -> Double -> [(Double, Double)] -> VisualSpec
closedPolyline _   _ []  = mempty
closedPolyline _   _ [_] = mempty
closedPolyline col w vs  =
  mconcat [ annotLineC col w p q | (p, q) <- zip vs (tail vs ++ [head vs]) ]

-- | 2D 凸包 (Andrew monotone chain)・反時計回り頂点列。 3 点未満は入力そのまま。
convexHull :: [(Double, Double)] -> [(Double, Double)]
convexHull ps0 =
  let ps = sort (nub ps0)                 -- lexicographic (x, y)
  in if length ps <= 2 then ps
     else let lower = half ps
              upper = half (reverse ps)
          in init lower ++ init upper      -- 端点重複を除いて連結
  where
    -- 単調鎖: 直近 2 点と p が右回り (cross<=0) の間は pop。 stack は head=最新。
    half = reverse . foldl step []
    step acc p = p : popRight acc p
    popRight (b : a : rest) p
      | cross a b p <= 0 = popRight (a : rest) p
    popRight acc _ = acc
    cross (ox, oy) (ax, ay) (bx, by) = (ax - ox) * (by - oy) - (ay - oy) * (bx - ox)

-- | クラスタ点群をラベルごとにグルーピング (色は 'clusterScatterOf' と一致)。
--   d が xn/yn 列を持たなければ空。
clusterGroups
  :: ColumnSource d => d -> KMeansResult -> Text -> Text
  -> [(Text, [(Double, Double)])]         -- (群色 hex, 点列)
clusterGroups d res xn yn =
  case (lookupCol xn d, lookupCol yn d) of
    (Just xs, Just ys) ->
      let labs = kmrLabels res
          cmap = hueColorMap (map (T.pack . show) labs)
          gmap = Map.fromListWith (flip (++))
                   [ (l, [(x, y)]) | (l, x, y) <- zip3 labs xs ys ]
      in [ (Map.findWithDefault "#cccccc" (T.pack (show l)) cmap, ps)
         | (l, ps) <- Map.toList gmap ]
    _ -> []

-- | 各クラスタを **凸包の輪郭線**で囲む (ggplot @geom_encircle@ 相当・塗りなし)。
--   群色は 'clusterScatterOf' と一致。 定番 = @cdf |>> (clusterScatterOf … \<\> clusterHullOf …)@。
--   ⚠ annotation は軸平行矩形しか塗れないため**輪郭線のみ** (半透明塗りは将来 'MPolygon' 移譲)。
clusterHullOf :: ColumnSource d => d -> KMeansResult -> Text -> Text -> VisualSpec
clusterHullOf d res xn yn =
  mconcat [ closedPolyline col 1.5 (convexHull ps)
          | (col, ps) <- clusterGroups d res xn yn ]

-- | 各クラスタを **95% 共分散楕円** (χ²(0.95, 2)=5.991) の輪郭で囲む (ggplot @stat_ellipse@
--   相当・正規分布仮定)。 群平均 μ・共分散 Σ を固有分解 ('LA.eigSH') し、 固有軸方向へ
--   半径 √5.991·√λ の楕円点列を折れ線で近似。 群色は 'clusterScatterOf' と一致。
--   点数 3 未満の群は描かない (共分散が定義できないため)。
clusterEllipseOf :: ColumnSource d => d -> KMeansResult -> Text -> Text -> VisualSpec
clusterEllipseOf d res xn yn =
  let ellipses = [ (col, ellipse95 ps) | (col, ps) <- clusterGroups d res xn yn ]
      outlines = mconcat [ closedPolyline col 1.5 pts | (col, pts) <- ellipses ]
      allPts   = concatMap snd ellipses
      -- annotation は軸ドメインを駆動しないため、 95% 楕円 (データ点より外へ広がる) が
      -- フレームをはみ出す。 楕円点を alpha=0 の不可視散布で載せ軸を広げる (colorBy 無し=
      -- 凡例に出ない)。 決定境界の coordCartesian と違い、 ここは重畳データ点も含めて
      -- auto-fit させたいので固定でなく anchor 方式。
      anchor
        | null allPts = mempty
        | otherwise   = layer ( scatter (inline (map fst allPts)) (inline (map snd allPts))
                                <> alpha 0 )
  in outlines <> anchor
  where
    seg   = 64 :: Int
    scl   = sqrt 5.991                       -- χ²(0.95, 2)
    ellipse95 ps
      | n < 3     = []
      | otherwise =
          [ ( mux + a * (v1 !! 0) + b * (v2 !! 0)
            , muy + a * (v1 !! 1) + b * (v2 !! 1) )
          | t <- [ 2 * pi * fromIntegral k / fromIntegral seg | k <- [0 .. seg - 1] ]
          , let a = scl * sqrt (max 0 l1) * cos t
                b = scl * sqrt (max 0 l2) * sin t ]
      where
        n   = length ps
        xs  = map fst ps; ys = map snd ps
        mux = sum xs / fromIntegral n
        muy = sum ys / fromIntegral n
        sxx = sum [ (x - mux) ^ (2 :: Int) | x <- xs ] / fromIntegral (n - 1)
        syy = sum [ (y - muy) ^ (2 :: Int) | y <- ys ] / fromIntegral (n - 1)
        sxy = sum [ (x - mux) * (y - muy) | (x, y) <- ps ] / fromIntegral (n - 1)
        sigma = LA.fromLists [[sxx, sxy], [sxy, syy]]
        (vals, vecs) = LA.eigSH (LA.trustSym sigma)   -- λ 降順・列=固有ベクトル
        l1 = vals `LA.atIndex` 0
        l2 = vals `LA.atIndex` 1
        cols = LA.toColumns vecs
        v1 = LA.toList (cols !! 0)
        v2 = LA.toList (cols !! 1)

-- | dendrogram の描画オプション。
data DendroOpts = DendroOpts
  { doLineColor      :: !Text            -- ^ 閾値超 (または閾値未指定) の線色。
  , doWidth          :: !Double          -- ^ 線幅。
  , doColorThreshold :: !(Maybe Double)  -- ^ @Just t@ で高さ @t@ 未満のサブツリーをクラスタ色分け
                                         --   (scipy @color_threshold@ 流)。 @Nothing@ で単色。
  } deriving (Show)

-- | 既定 = 単色 (grey20 相当・閾値なし)。
defaultDendroOpts :: DendroOpts
defaultDendroOpts = DendroOpts "#4C4C4C" 1.2 Nothing

instance Plottable HClusterFit where
  toPlot = dendrogramOf

-- | 階層クラスタリング結果を **dendrogram** で描く (scipy @dendrogram@ / ggdendro 流)。
--   マージ列 ('hcMerges') と高さ ('hcHeights') から U 字リンク (縦 2 + 横 1) を 'AnnLine' で
--   描画。 葉は x 軸に等間隔・各マージノードの x = 子の中点・y = マージ高。 リーフに元サンプル
--   ID ラベル。 plot core は触らず annotation で描く (将来 plot 正式 mark 移譲予定)。
dendrogramOf :: HClusterFit -> VisualSpec
dendrogramOf = dendrogramOf' defaultDendroOpts

-- | 色閾値・線色等を指定できる版。
dendrogramOf' :: DendroOpts -> HClusterFit -> VisualSpec
dendrogramOf' opts fit
  | n <= 1 || null merges = mempty
  | otherwise =
      -- R base / scipy 同様 grid・軸線・枠なし (theme_minimal + grid off)。
      theme ThemeMinimal <> themeGrid False <> themeAxisLine False <> panelBorder False
        <> tickColor "transparent"      -- 目盛マーク (短線) を消す。 数字ラベルは残る。
        -- 葉ラベルは x 軸目盛 (slot 位置・縦書き) で。 軸ラベルは margin を予約するので
        -- リンク根と被らない (annotText と違い R と同挙動)。
        -- ★ axisRotate は CCW 正 (R/matplotlib/ggplot 準拠・hgg Phase 50 A1)。
        --   90 = CCW 90 = 下→上読みで R base / scipy dendrogram の既定向きと一致。
        <> xAxis (axisBreaksLabeled leafTicks <> axisRotate 90)
        <> layer (dendrogramMark payload)  -- ★ Phase 48: U字リンクを custom mark で描く (焼き込み)。
                                           --   encX/encY で軸 range を束ねる (旧 anchor 不要)。
        <> yAxisLine                    -- 軸線は 2 辺一括制御しか無いので y 軸線だけ自前描画。
        <> yLabel "height"              -- y = マージ高 (結合時の非類似度・Ward 増分)。
  where
    n       = hcNumOriginals fit
    merges  = hcMerges fit
    heights = hcHeights fit
    root    = 2 * n - 2                                 -- 最終マージ = 根ノード
    childrenOf node = merges !! (node - n)
    leavesOf node
      | node < n  = [node]
      | otherwise = let (a, b) = childrenOf node in leavesOf a ++ leavesOf b
    order   = leavesOf root                             -- 葉 ID を左→右の並びで
    slotOf  = Map.fromList (zip order [0 :: Int ..])
    -- ノードの x (子の中点)・高さ・代表葉を fold で確定 (子は id が小さく先に入る)。
    (nodeX, nodeH, leafRep) = foldl' step (x0, h0, r0) (zip [0 :: Int ..] merges)
      where
        x0 = Map.fromList [ (l, fromIntegral (slotOf Map.! l)) | l <- [0 .. n - 1] ]
        h0 = Map.fromList [ (l, 0 :: Double) | l <- [0 .. n - 1] ]
        r0 = Map.fromList [ (l, l) | l <- [0 .. n - 1] ]
        step (mx, mh, mr) (i, (a, b)) =
          let node = n + i
          in ( Map.insert node ((mx Map.! a + mx Map.! b) / 2) mx
             , Map.insert node (heights !! i) mh
             , Map.insert node (mr Map.! a) mr )
    maxH    = maximum heights
    -- 葉ラベル = x 軸目盛 (slot 位置に元サンプル ID)。 縦書きは axisRotate 90。
    leafTicks = [ (fromIntegral slot, T.pack (show leaf))
                | (leaf, slot) <- zip order [0 :: Int ..] ]
    -- 色閾値: t 未満マージ数だけ切って各葉のクラスタ ID を得る (hcMerges は高さ昇順)。
    thrInf     = maybe (1 / 0) id (doColorThreshold opts)
    kCut       = n - length (filter (< thrInf) heights)
    clusterIds = cutTree fit kCut
    distinctCs = foldr (\c acc -> if c `elem` acc then acc else acc ++ [c])
                       [] (V.toList clusterIds)         -- 出現順
    cmap       = Map.fromList (zip distinctCs
                   (ggplotHue (length distinctCs) ++ repeat "#999999"))
    linkColor i = case doColorThreshold opts of
      Just t | heights !! i < t ->
        Map.findWithDefault (doLineColor opts)
                            (clusterIds V.! (leafRep Map.! (n + i))) cmap
      _ -> doLineColor opts
    -- U 字リンク (子の高さ→マージ高の縦線 2 本 + マージ高の横線 1 本) を焼き込み線分に。
    -- 座標系は従来の annotLine 版と同一 (x=葉 slot/node 中点、 y=height)。
    payload = DendroPayload
      { dpSegments = concat
          [ [ DendroSeg xa ha  xa hgt col w
            , DendroSeg xa hgt xb hgt col w
            , DendroSeg xb hgt xb hb  col w ]
          | (i, (a, b)) <- zip [0 :: Int ..] merges
          , let xa  = nodeX Map.! a; xb = nodeX Map.! b
                ha  = nodeH Map.! a; hb = nodeH Map.! b
                hgt = heights !! i
                col = linkColor i
                w   = doWidth opts ]
      , dpXRange = (-0.6, fromIntegral n - 0.4)   -- 旧 anchor と同じ range
      , dpYRange = (0, maxH * 1.05)
      }
    -- 左辺 (panel npc x=0) に y 軸線を 1 本 (下辺 x 軸線は出さない = R 流)。
    yAxisLine = annotate AnnLine
      { anX1 = PNpc 0, anY1 = PNpc 0, anX2 = PNpc 0, anY2 = PNpc 1
      , anColor = "#333333", anWidth = 1 }

-- ===========================================================================
-- 時系列予測 (描画可能)
--
-- AR(p) の点予測 'forecastAR' は将来値の中心のみを返す。 予測の不確実性帯は **h-step
-- 予測分散** から得る: AR の MA(∞) 表現の ψ-weights (ψ₀=1, ψⱼ=Σφᵢψⱼ₋ᵢ) を用いて
-- @Var(ŷ_{n+k}) = σ² Σ_{j=0}^{k-1} ψⱼ²@ (σ² = 革新分散 'arResidVar')。 これは Gaussian
-- 革新の下での正統な予測区間 (地平 k とともに単調に広がる)。 対称ゆえ band は
-- @中心 ± z·se@。 'toPlot' は履歴折れ線 + 予測折れ線 + 予測区間 band を 1 枚に重ねる。
-- ===========================================================================

-- | AR(p) の MA(∞) 表現の ψ-weights ψ₀..ψ_{h-1} (ψ₀=1, ψⱼ=Σ_{i=1}^{min j p} φᵢ ψⱼ₋ᵢ)。
arPsiWeights :: [Double] -> Int -> [Double]
arPsiWeights phi h = go [1.0]
  where
    p = length phi
    go ps
      | length ps >= h = take h ps
      | otherwise =
          let j  = length ps
              pj = sum [ (phi !! (i - 1)) * (ps !! (j - i)) | i <- [1 .. min j p] ]
          in go (ps ++ [pj])

-- | k-step (k=1..h) 予測標準誤差 se_k = sqrt(σ² Σ_{j<k} ψⱼ²)。
arForecastSE :: ARFit -> Int -> [Double]
arForecastSE fit h =
  let phi  = LA.toList (arPhi fit)
      s2   = arResidVar fit
      psis = arPsiWeights phi h
  in [ sqrt (s2 * sum (map (^ (2 :: Int)) (take k psis))) | k <- [1 .. h] ]

instance Plottable ForecastModel where
  -- 履歴折れ線 + 予測折れ線 + 予測区間 band (中心 ± 1.96·se)。 x = 時刻 index
  -- (履歴 1..n、 予測 n+1..n+h)。 予測線は履歴末尾点から繋げる。 帯を先・線を後に重ねる。
  toPlot m =
    let fit  = fmFit m
        hist = LA.toList (fmHistory m)
        n    = length hist
        h    = fmHorizon m
        fc   = LA.toList (forecastAR fit (fmHistory m) h)
        se   = arForecastSE fit h
        fx   = [ fromIntegral (n + k) | k <- [1 .. h] ] :: [Double]
        lo   = zipWith (\f s -> f - 1.96 * s) fc se
        hi   = zipWith (\f s -> f + 1.96 * s) fc se
        histX = [ fromIntegral i | i <- [1 .. n] ] :: [Double]
        -- 予測線は履歴末尾 (n, hist[n-1]) から始めて連続させる。
        lineX = fromIntegral n : fx
        lineY = last hist : fc
    in layer (band (inline fx) (inline lo) (inline hi))
         <> layer (line (inline histX) (inline hist))
         <> layer (line (inline lineX) (inline lineY))

-- ===========================================================================
-- 生存解析 (描画可能)
--
-- KM 生存曲線・CIF (競合リスク) はいずれも階段関数。 'stepVerts' (Core) で階段頂点を
-- 明示展開して line で結ぶ。 KM は s0=1 で下降、 CIF は s0=0 で上昇。
-- ===========================================================================

instance Plottable KMResult where
  -- KM 生存曲線 (階段、 S=1 から下降)。
  toPlot km =
    let pts   = zip (kmrTimes km) (kmrSurvival km)
        verts = stepVerts 1.0 pts
    in layer (line (inline (map fst verts)) (inline (map snd verts)))

instance Plottable CRFit where
  -- 競合リスク CIF (cause ごとに 0 から上昇する階段、 色分け重畳)。
  toPlot cr =
    let ts = LA.toList (crfTimes cr)
        mkCause (i, (_cause, cifV)) =
          let pts   = zip ts (LA.toList cifV)
              verts = stepVerts 0.0 pts
              col   = quantilePalette !! (i `mod` length quantilePalette)
          in layer (line (inline (map fst verts)) (inline (map snd verts))
                      <> color (fromHex col))
    in foldMap mkCause (zip [0 ..] (crfCIF cr))

-- ===========================================================================
-- 多変量・木 (描画可能)
--
-- PCA の代表図は **scree plot** (各主成分の寄与率 'pcaExplainedRatio' を棒で)、 木 (RF) の
-- 代表図は **特徴重要度バー** ('featureImportance')。 いずれも自己完結ゆえそのまま
-- 'Plottable'。 棒の x 軸はラベル ("PC1".. / "f1"..) なので 'inlineCat' (categorical) で渡す
-- (heatmap A9 と同じく 'bar' も categorical 軸が必要)。 優先低 (§3.5 A14) ゆえ scree/重要度
-- の 1 枚ずつに絞る (biplot や木構造図は将来拡張)。
-- ===========================================================================

instance Plottable PCAResult where
  -- scree plot: 各主成分 (PC1, PC2, …) の寄与率を棒で。
  toPlot res =
    let ratios = LA.toList (pcaExplainedRatio res)
        labels = [ "PC" <> T.pack (show k) | k <- [1 .. length ratios] ]
    in layer (bar (inlineCat labels) (inline ratios))

instance Plottable RandomForest where
  -- R 'varImpPlot' 流の 2 パネル: 左 = impurity (IncNodePurity)、 右 = permutation
  -- (%IncMSE)。 各パネルは降順ソート + 実列名 + 横棒 ('coordFlip')。
  toPlot rf =
    let n     = V.length (featureImportance rf)
        names = case rfFeatureNames rf of
                  [] -> defaultFeatureNames n
                  ns -> ns
        imp   = V.toList (featureImportance rf)
        perm  = V.toList (rfPermutationImportance rf)
    in subplots
         [ importanceBarNamed "IncNodePurity (impurity)" names imp
         , importanceBarNamed "%IncMSE (permutation)"    names perm ]
       <> subplotCols 2

-- | 名前つき importance を横棒 ('coordFlip') で描く (R 'varImpPlot' 流)。 重要度で
--   ソートするため 'scaleXDiscreteLimits' でカテゴリ順を明示する (bar 軸は既定
--   アルファベット順ゆえデータ並びでは効かない)。 coordFlip 後は limits 順が下→上
--   なので、 昇順 limits を渡して最重要を上端に置く。 タイトル付き。
importanceBarNamed :: T.Text -> [T.Text] -> [Double] -> VisualSpec
importanceBarNamed ttl names vals =
  let ascByVal = map fst (sortBy (comparing snd) (zip names vals))  -- 昇順 → 最大が末尾 = 上端
  in layer (bar (inlineCat names) (inline vals))
       <> scaleXDiscreteLimits ascByVal
       <> coordFlip <> title ttl

-- ===========================================================================
-- 木/アンサンブル — Phase 68 A2
--
-- 各モデルの分野定番図を **既存 mark のみ**で描く (新規 plot mark 不要):
--
--   * GradientBoosting (回帰/分類)・RandomForestClassifier = **特徴重要度 bar**。
--     GBM は重要度フィールドを持たないので弱学習器 ('Tree') の split 使用回数から
--     純粋計算する ('treeImportances'・RF.'featureImportance' と同方式・正規化)。
--   * DecisionTree = **樹形図**。 決定木は DAG の特殊形 (二分木) ゆえ、 HBM の
--     ModelGraph と同じ MDAG (Sugiyama 階層 layout) を **再利用**して node-link で描く
--     (split ノード = "f{j} ≤ {thr}"、 葉 = "y={class}")。
--
-- ⚠ DecisionTree の edge True/False ラベル・gini・サンプル数表示 (sklearn plot_tree
-- 相当) は DAGNode/DAGEdge が持たないため v1 では描かない。 必要なら専用 mark を
-- plot 側 Phase として起こす (= dendrogram Phase 48 と同型の判断)。
-- ===========================================================================

-- | 弱学習器 ('Tree') 列の split 使用回数による特徴重要度 (RF と同方式・合計 1 に正規化)。
--   特徴数は出現した最大 index + 1 (= 木で一度も使われない末尾特徴は現れない)。
treeImportances :: [Tree] -> [Double]
treeImportances trees =
  let counts = foldr walk Map.empty trees
      walk (RF.Leaf _)       m = m
      walk (RF.Node j _ l r) m = walk l (walk r (Map.insertWith (+) j (1 :: Double) m))
      d   = if Map.null counts then 0 else maximum (Map.keys counts) + 1
      raw = [ Map.findWithDefault 0 j counts | j <- [0 .. d - 1] ]
      tot = sum raw
  in if tot <= 0 then raw else map (/ tot) raw

instance Plottable GBRegressor where
  -- 弱学習器の split 使用回数による特徴重要度 bar。
  toPlot gb = importanceBar (treeImportances (gbrTrees gb))

instance Plottable GBClassifier where
  toPlot gb = importanceBar (treeImportances (gbcTrees gb))

instance Plottable RFClassifierFit where
  -- R 'varImpPlot' 流の 2 パネル: 左 = permutation (MeanDecreaseAccuracy)、
  -- 右 = gini 減少 (MeanDecreaseGini・MDI)。 各パネル降順・実列名・横棒。
  toPlot fit =
    let perm  = LA.toList (rfcImportance fit)
        gini  = LA.toList (rfcGiniImportance fit)
        names = case rfcFeatureNames fit of
                  [] -> defaultFeatureNames (length perm)
                  ns -> ns
    in subplots
         [ importanceBarNamed "MeanDecreaseAccuracy" names perm
         , importanceBarNamed "MeanDecreaseGini"     names gini ]
       <> subplotCols 2

instance Plottable DTree where
  -- 決定木 → node-link 樹形図 (MDAG 再利用・Sugiyama 階層 layout)。
  toPlot t =
    let (dnodes, dedges)     = dtreeToDag t
        (positioned, routed) = layoutHierarchicalFullWithPlates dnodes dedges []
    in bakeDAGRoutesInSpec $
         layer (dagFromListsWithPlates positioned routed LayoutHierarchical [])

-- | 学習済み 'DTFit' → **rpart.plot 流**の樹形図 ('treePlot' と同じ)。 @df |-> decisionTree@
--   の返り値をそのまま @toPlot@ に渡せる。 素の node-link 図は 'DTree' の 'Plottable'。
instance Plottable DTFit where
  toPlot = treePlot

-- | 'DTree' を MDAG の node/edge 列へ変換する。 ノード id は根から L/R を辿る経路
--   ("n" / "nL" / "nLR" …) で一意。 split ノードは @NodeOther@、 葉は @NodeObserved@
--   (色で区別)。 左 child = 条件成立 (≤)・右 = 不成立 (>) の慣例で並べる。
dtreeToDag :: DTree -> ([DAGNode], [DAGEdge])
dtreeToDag = go "n"
  where
    mkNode nid lbl kind = DAGNode
      { dnId = nid, dnLabel = lbl, dnKind = kind, dnDist = Nothing, dnX = 0, dnY = 0 }
    go nid DLeaf{dlMajority = maj} =
      ( [ mkNode nid ("y=" <> T.pack (show maj)) NodeObserved ], [] )
    go nid DNode{dnFeature = f, dnThr = thr, dnLeft = l, dnRight = r} =
      let self     = mkNode nid ("f" <> T.pack (show f) <> " ≤ " <> fmt2 thr) NodeOther
          lid      = nid <> "L"
          rid      = nid <> "R"
          (ln, le) = go lid l
          (rn, re) = go rid r
          edges    = [ DAGEdge nid lid Nothing Nothing
                     , DAGEdge nid rid Nothing Nothing ]
      in (self : ln ++ rn, edges ++ le ++ re)
    fmt2 x = T.pack (showFFloat (Just 2) x "")

-- ---------------------------------------------------------------------------
-- Phase 75.26: 決定木 樹形図 (rpart.plot 流・annotation ベース)
-- ---------------------------------------------------------------------------

-- | 位置付け済みの決定木ノード (annotation 描画用の中間表現)。 @tpU@ は葉単位の
--   水平座標 (葉 = 0,1,2,…・内部 = 子の中点)、 @tpDepth@ は根からの深さ。
data TPNode = TPNode
  { tpU     :: !Double                -- ^ 葉単位の水平座標。
  , tpDepth :: !Int                   -- ^ 根からの深さ (根 = 0)。
  , tpMaj   :: !Int                   -- ^ 多数決 (予測) クラス。
  , tpN     :: !Int                   -- ^ ノードのサンプル数。
  , tpProbs :: !(Map.Map Int Double)  -- ^ クラス割合。
  , tpSplit :: !(Maybe (Int, Double)) -- ^ 分岐なら (特徴 index, 閾値)。 葉は Nothing。
  , tpKids  :: [TPNode]               -- ^ [] = 葉、 [左, 右] = 分岐。
  }

-- | Phase 75.26: 決定木を **rpart.plot 流**の樹形図で描く (analyze 側 annotation ベース)。
--
-- 各ノードを矩形で表し、 内部に **予測クラス / 全クラス確率 / サンプル割合** を 3 行で
-- 書く (rpart.plot @type=2@ 既定に相当)。 配線は R と同じく **親→バスの縦線を引かず**、
-- 分割条件 @feat < thr@ を親の少し下の水平バス上に置き、 枝はその両端から出て子の真上で
-- 折れる。 条件の両脇 (**根の分岐のみ**) に枠付き白箱で @yes@ (左=成立)・@no@ (右) を添える。
--
-- 塗り色は rpart.plot @box.palette="auto"@ 準拠で、 クラスごとに ColorBrewer 連番
-- パレット (Reds/Greys/Greens/…) を割当て、 **濃淡で予測クラスの確率 (確信度)** を表す
-- (淡=低・濃=高)。 暗い塗りには白文字を自動選択。 右上にクラス色の凡例を出す。
--
-- 第 1 = 特徴量名、 第 2 = クラス名 ('printRpart' と同型・長さ不足は @f{i}@/整数へ
-- フォールバック)。 木レイアウトは葉を左→右へ等間隔・深さ→縦位置で配置し、 座標は
-- panel 正規化 (PNpc) で算術する。 plot core の型は触らず annotation だけで描く
-- (図が固まれば plot 正式 mark へ移譲予定・PS parity は移譲時に対応)。
--
-- ⚠ 文字幅は annotation では実測できないため npc で概算する ('wpc')。 既定は図幅
-- 〜680px 前提に調律してあり、 極端なサイズでは箱幅/マスク幅が僅かにズレる。
--
-- 高レベル 'treePlot' は 'DTFit' 一つを取り (@df |-> decisionTree@ の返り値をそのまま
-- 渡せる)、 内部に載った特徴量名・クラス名を使う。 名前を手渡ししたい行列 fit 用は
-- 'treePlotRaw'。 'DTFit' は 'Plottable' なので @toPlot@ でも同じ図が出る。
treePlot :: DTFit -> VisualSpec
treePlot (DTFit tree feats classes) = treePlotRaw feats classes tree

-- | 行列 fit 用の低レベル版 — 特徴量名・クラス名を明示的に渡す (名無しは @f{i}@/整数へ
--   フォールバック)。
treePlotRaw :: [Text] -> [Text] -> DTree -> VisualSpec
treePlotRaw featNames classNames tree =
  theme ThemeVoid
    <> xAxis hideTicks <> yAxis hideTicks       -- 目盛線・目盛ラベルを消す (樹形図は座標軸不要)。
    <> legendLayer                              -- クラス色の凡例 (標準機構・他マークと同じ)。
    <> themeLegendFont (fontSize 11)            -- 凡例文字をノード (class 11pt) に揃える。
    <> mconcat (concatMap edgesOf allNodes)     -- 枝を先に (ノード矩形の下敷き)。
    <> mconcat (concatMap nodeAnns allNodes)
  where
    (nLeaves, root) = assign 0 0 tree
    allNodes        = flatten root
    total           = tpN root
    maxD            = maximum (map tpDepth allNodes)
    classes         = Map.keys (Map.fromList
                        [ (c, ()) | t <- allNodes
                        , c <- tpMaj t : Map.keys (tpProbs t) ])
    nClasses        = length classes
    colorIx         = Map.fromList (zip classes [0 :: Int ..])

    -- ---- 配色: rpart.plot box.palette="auto" 準拠 --------------------------
    --   クラスごとに ColorBrewer 連番パレット (Reds/Greys/Greens/…) を割当て、
    --   塗りの **濃淡で予測クラスの確率 (確信度)** を表す。 R iris 実測と一致:
    --   setosa=Reds・versicolor=Greys・virginica=Greens、 淡=低確率・濃=高確率。
    nodeFill t =
      let pi_  = maybe 0 id (Map.lookup (tpMaj t) colorIx)
          pal9 = ix greysP brewerPals (pi_ `mod` length brewerPals)
          p    = Map.findWithDefault 0 (tpMaj t) (tpProbs t)
      in ix "#cccccc" pal9 (shadeIx p)
    -- 予測確率 p∈[1/K,1] を 9 段 palette の index (概ね 1..5) へ (R 実測に fit)。
    shadeIx p =
      let k = fromIntegral (max 2 nClasses) :: Double
      in max 0 (min 8 (round (1 + (p - 1 / k) / (1 - 1 / k) * 4) :: Int))
    -- 塗りが暗いときは白文字 (簡易輝度判定)。
    textColorFor hex = if luminance hex < 0.5 then "#ffffff" else "#111111"

    -- ---- npc 座標変換 -----------------------------------------------------
    leftM = 0.04; rightM = 0.04; topM = 0.85; botM = 0.16
    spanX = 1 - leftM - rightM
    xNpc u = leftM + (u + 0.5) / fromIntegral nLeaves * spanX
    yNpc d | maxD <= 0 = topM
           | otherwise = topM - fromIntegral d / fromIntegral maxD * (topM - botM)
    colW = spanX / fromIntegral nLeaves
    -- 箱は中身 (最長のクラス名 / 確率行) に合わせて締める (スカスカ回避)。 フォントは
    -- **凡例 (themeLegendFont 11pt) と揃える** (class 11 / 数値 10)。 font を膨らませず
    -- 箱側を締めて詰めて見せる (凡例とノードのサイズを統一)。
    contentW = maximum (0.06 : [ wpc 10 (plineOf t) | t <- allNodes ]
                            ++ [ wpc 11 (classLabel (tpMaj t)) | t <- allNodes ])
    hw   = min (colW * 0.47) (contentW / 2 + 0.016)  -- 矩形半幅。
    hh   = 0.054                      -- 矩形半高。
    dy   = 0.030                      -- 3 行ラベルの行間 (npc)。
    bc   = -0.011                     -- ベースライン補正 (npc・下げて上下中央に見せる)。
    plineOf t = T.intercalate "  "
                  [ fmtP (Map.findWithDefault 0 c (tpProbs t)) | c <- classes ]

    -- ---- ノード矩形 + 3 行ラベル (rpart.plot type=2 相当・上下中央) ---------
    --   1 行目 = 予測クラス、 2 行目 = 全クラス確率 (.34 .30 .35 形式)、
    --   3 行目 = 全体に占めるサンプル割合 (%)。
    nodeAnns t =
      let x    = xNpc (tpU t); y = yNpc (tpDepth t)
          fill = nodeFill t
          tc   = textColorFor fill
          pct  = 100 * fromIntegral (tpN t) / fromIntegral total :: Double
          box  = rectA fill "#404040" 0.7 (x - hw) (y - hh) (x + hw) (y + hh)
          l1   = textC tc x (y + dy + bc) 11 (classLabel (tpMaj t))
          l2   = textC tc x (y      + bc) 10 (plineOf t)
          l3   = textC tc x (y - dy + bc) 10 (fmt0 pct <> "%")
      in [box, l1, l2, l3]

    -- ---- 凡例 (標準機構) --------------------------------------------------
    --   手描き annotation は中央アンカーで文字が揃わないため、 **他マークと同じ
    --   凡例機構**に載せる: 不可視 (alpha 0) の colorBy 散布レイヤを 1 枚足し、
    --   'scaleColorManual' で各クラス名→代表色 (ColorBrewer index 4) を固定する。
    --   凡例スウォッチは layer alpha 非適用ゆえ満色で出る (グリフだけ不可視)。
    reprColor i = ix "#888888" (ix greysP brewerPals (i `mod` length brewerPals)) 4
    legendLayer =
      let cats = [ classLabel c | c <- classes ] :: [Text]
          xs   = [ fromIntegral i | i <- [0 .. nClasses - 1] ] :: [Double]
          dict = [ (classLabel c, reprColor i) | (i, c) <- zip [0 :: Int ..] classes ]
      in layer (scatter (inline xs) (inline xs) <> colorBy (inlineCat cats) <> alpha 0)
           <> scaleColorManual dict

    -- ---- 枝 = rpart.plot type=2 の配線 -----------------------------------
    --   ★親→バスの縦線は引かない (R 準拠)。 分割ラベルを親の少し下に置き、 枝は
    --   ラベル両端から水平に出て子の真上で下へ折れる。 中央 (ラベル/yes-no) 部分は
    --   線を描かないことで枝線をマスクする。 yes/no は **根の分岐のみ**・枠付き白箱。
    edgesOf t = case (tpKids t, tpSplit t) of
      ([l, r], Just (f, thr)) ->
        let px   = xNpc (tpU t); pBot = yNpc (tpDepth t) - hh
            lx   = xNpc (tpU l); rx   = xNpc (tpU r)
            cTop = yNpc (tpDepth l) + hh          -- 子上端 (左右子は同じ深さ)。
            busY = pBot - 0.03                    -- バスは親の少し下 (縦線なし)。
            condTxt = featName f <> " < " <> fmt2 thr
            lw    = wpc 11 condTxt
            isRoot = tpDepth t == 0
            -- 中央の非描画幅 (ラベル + 根なら yes/no 箱ぶん)。
            clr   = lw / 2 + (if isRoot then 0.075 else 0.008)
            branch = [ lineA lx busY (px - clr) busY  -- 左枝 (水平)。
                     , lineA (px + clr) busY rx busY  -- 右枝 (水平)。
                     , lineA lx busY lx cTop          -- 左子へ縦。
                     , lineA rx busY rx cTop ]        -- 右子へ縦。
            cond = textA px (busY - 0.004) 11 condTxt
            yn   = if isRoot
                     then labelBox (px - lw / 2 - 0.03) busY "yes"
                       ++ labelBox (px + lw / 2 + 0.026) busY "no"
                     else []
        in branch ++ cond : yn
      _ -> []

    -- yes/no の枠付き白箱 (中央にテキスト)。
    labelBox cx cy txt =
      let w = wpc 10 txt + 0.014; h = 0.03
      in [ rectA "#ffffff" "#555555" 0.7 (cx - w / 2) (cy - h / 2) (cx + w / 2) (cy + h / 2)
         , textA cx (cy - 0.004) 10 txt ]

    -- ---- annotation プリミティブ (PNpc 固定) ----------------------------
    rectA fill stroke sw x1 y1 x2 y2 = annotate $
      AnnRect (PNpc x1) (PNpc y1) (PNpc x2) (PNpc y2) fill stroke sw 1.0
    textA = textC "#111111"
    textC col x y sz t = annotate $
      AnnText (PNpc x) (PNpc y) t col sz
    lineA x1 y1 x2 y2 = annotate $
      AnnLine (PNpc x1) (PNpc y1) (PNpc x2) (PNpc y2) "#606060" 0.8

    -- 文字列の描画幅を npc で概算 (font px と文字数から線形近似・図幅 ~680px 前提)。
    -- annotation は実測不可ゆえの heuristic。 doc/demo は size を指定して調律に合わせる。
    wpc fs t = 0.00095 * fs * fromIntegral (T.length t)

    -- ---- 名前解決 ('printRpart' と同じ規則) ------------------------------
    featName i   = pick i featNames  ("f" <> tShowI i)
    classLabel i = pick i classNames (tShowI i)
    pick i xs d  = case drop i xs of
      (nm : _) | not (T.null nm) -> nm
      _                          -> d

    tShowI = T.pack . show :: Int -> Text
    fmt2 x = T.pack (showFFloat (Just 2) x "")
    fmt0 x = T.pack (showFFloat (Just 0) x "")
    -- rpart.plot 流の確率表記 (先頭 0 を落として ".34"、 1.00 は据置き)。
    fmtP x = let s = T.pack (showFFloat (Just 2) x "")
             in maybe s id (T.stripPrefix "0" s)
    ix d xs i = if i >= 0 && i < length xs then xs !! i else d

-- | 'DTree' を葉単位で位置付けした 'TPNode' へ変換する。 葉に左→右で連番 (slot) を
--   振り、 内部ノードは左右子の中点を水平座標にする。 戻りは (葉総数, 根ノード)。
assign :: Int -> Int -> DTree -> (Int, TPNode)
assign depth k node = case node of
  DLeaf p m n _ ->
    (k + 1, TPNode (fromIntegral k) depth m n p Nothing [])
  DNode f thr l r n _ p m ->
    let (k1, lp) = assign (depth + 1) k  l
        (k2, rp) = assign (depth + 1) k1 r
        u        = (tpU lp + tpU rp) / 2
    in (k2, TPNode u depth m n p (Just (f, thr)) [lp, rp])

-- | 'TPNode' 木を前順で平坦化する。
flatten :: TPNode -> [TPNode]
flatten t = t : concatMap flatten (tpKids t)

-- | ColorBrewer 9 段連番パレット (rpart.plot box.palette="auto" の per-class 割当)。
--   クラス index 0,1,2,… に Reds, Greys, Greens, Blues, Purples, Oranges を循環割当。
brewerPals :: [[Text]]
brewerPals = [redsP, greysP, greensP, bluesP, purplesP, orangesP]

redsP, greysP, greensP, bluesP, purplesP, orangesP :: [Text]
redsP    = ["#fff5f0","#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d"]
greysP   = ["#ffffff","#f0f0f0","#d9d9d9","#bdbdbd","#969696","#737373","#525252","#252525","#000000"]
greensP  = ["#f7fcf5","#e5f5e0","#c7e9c0","#a1d99b","#74c476","#41ab5d","#238b45","#006d2c","#00441b"]
bluesP   = ["#f7fbff","#deebf7","#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b"]
purplesP = ["#fcfbfd","#efedf5","#dadaeb","#bcbddc","#9e9ac8","#807dba","#6a51a3","#54278f","#3f007d"]
orangesP = ["#fff5eb","#fee6ce","#fdd0a2","#fdae6b","#fd8d3c","#f16913","#d94801","#a63603","#7f2704"]

-- | @#rrggbb@ の相対輝度 (0..1・Rec.601 加重和)。 塗りの明暗で文字色を切替える用。
luminance :: Text -> Double
luminance hex =
  let s = T.dropWhile (== '#') hex
      hx a b = fromIntegral (16 * hv a + hv b) :: Double
      hv c | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
           | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
           | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
           | otherwise            = 0
  in case T.unpack s of
       (r1:r2:g1:g2:b1:b2:_) ->
         (0.299 * hx r1 r2 + 0.587 * hx g1 g2 + 0.114 * hx b1 b2) / 255
       _ -> 1

-- ===========================================================================
-- 分類 (Discriminant / NaiveBayes / KNN) — Phase 68 A3
--
-- 代表図は **決定境界** と **confusion 行列**。 いずれも「学習済モデルを評価点で
-- 走らせる」 図ゆえ、 KMeans (A1) と同じく **データ/範囲を取るヘルパ**で提供する
-- (新規 plot mark 不要):
--
--   * 'decisionBoundaryOf' = 2D grid を予測しクラス色で塗る (= 連続軸の散布を
--     四角マーカー・低 alpha で「領域」表現。 ★renderHeatmap はカテゴリ軸なので
--     連続 grid には不適 → 'MScatter' + 'colorBy' (離散色) を採用)。 2 特徴前提。
--   * 'confusionOf' = テストデータの真値×予測の件数を 'MHeatmap' で (カテゴリ軸が適合)。
--
-- 'Plottable' の 'toPlot' (データ非保持で描ける代表 1 枚):
--   * KNN は訓練データ ('knnCX'/'knnCY') を保持 → **ラベル色の訓練点散布**。
--   * Discriminant / NaiveBayes(Gaussian) は **クラス平均散布** (✚)、
--     NaiveBayes(Multinomial) は **クラス事前確率 bar**。
-- ===========================================================================


instance ClassPredict DiscriminantFit where
  predictClasses fit m = V.toList (fst (predictDiscriminant fit m))

instance ClassPredict NBModel where
  predictClasses nb m = VU.toList (predictNB nb m)
  classNamesOf (NBGaussian g)    = gnbClassNames g
  classNamesOf (NBMultinomial g) = mnbClassNames g

instance ClassPredict KNNClassifier where
  predictClasses knn m = VU.toList (predictKNNC knn m)
  classNamesOf = knnCClassNames

-- Phase 75.5: 分類 NN も同様に decisionBoundaryOf / confusionOf 対応。
instance ClassPredict MLPFit where
  predictClasses fit m = V.toList (predictMLPClass fit m)
  classNamesOf = mlpClassNames

-- Phase 75.12: カーネル SVM (真の SV) も decisionBoundaryOf (非線形境界) / confusionOf 対応。
instance ClassPredict SVM where
  predictClasses m x = VU.toList (predictSVM m x)

instance ClassPredict SVMMulti where
  predictClasses m x = VU.toList (predictSVMMulti m x)
  classNamesOf = svmmClassNames

-- | 決定境界 (2 特徴) の **領域塗り** (Phase 76.A・annotation ベース)。
--
-- @res×res@ の格子セルを中心で予測し、 各セルを予測クラス色の塗り矩形 ('annotRectP')
-- で敷き詰める (sklearn @DecisionBoundaryDisplay@ の pcolormesh 相当)。 点散布でなく
-- **実矩形**をセル境界ぴったりに敷くので、 旧実装 (半透明の四角散布) の**縞模様**が出ない。
--
-- クラス色は 'toPlot' の凡例 (@colorBy@ → ggplot @hue_pal()@) と一致させる。 render が
-- categorical を @sort.nub@ 順に並べ 'ggplotHue' を割り当てるのと同順で再現する。
-- 訓練点・クラス平均は呼び出し側で上に重ねる (@decisionBoundaryOf c xr yr res \<\> toPlot c@)。
--
-- ⚠ **annotation の制約**: 塗りは 'annotRectP' 固定の @fill-opacity=0.2@ (薄塗り)。
-- また annotation は layer の**後**に描かれるため、 塗りは重ねた訓練点の**上**に来る
-- (0.2 の薄塗りなので点は透けて見える)。 「点が上・塗りが下」 の厳密な重ね順や
-- 半透明でない濃淡は将来 plot 正式 mark ('MTile'/'MRaster') 移譲時に対応する。
-- クラス色は既定 hue パレット前提 (theme series palette を差し替えた場合、 塗り色は
-- 追従しない — annotation 色は spec 時に確定するため)。
decisionBoundaryOf
  :: ClassPredict c => c -> (Double, Double) -> (Double, Double) -> Int -> VisualSpec
decisionBoundaryOf c (x0, x1) (y0, y1) res
  | res <= 0 || x1 <= x0 || y1 <= y0 = mempty
  | otherwise =
      -- 軸ドメインをグリッド範囲へ正確に固定 (expand=FALSE)。 annotation は軸を駆動しない
      -- ため、 これが無いと軸がデータ点範囲に縮み塗りがフレーム外へはみ出す (sklearn は
      -- 軸 = グリッド範囲)。 範囲外の重畳点は panel に clip される。
      coordCartesian x0 x1 y0 y1 <> mconcat
      [ annotRectP (PNative cx0) (PNative cy0) (PNative cx1) (PNative cy1) (colorFor k)
      | (idx, k) <- zip [0 :: Int ..] preds
      , let (i, j) = idx `divMod` res
            cx0 = x0 + fromIntegral i * dx
            cx1 = cx0 + dx
            cy0 = y0 + fromIntegral j * dy
            cy1 = cy0 + dy ]
  where
    dx = (x1 - x0) / fromIntegral res
    dy = (y1 - y0) / fromIntegral res
    -- セル中心 (行 = i*res + j・列 = [x, y]) をまとめて 1 回でバッチ予測する。
    centers = [ [ x0 + (fromIntegral i + 0.5) * dx, y0 + (fromIntegral j + 0.5) * dy ]
              | i <- [0 .. res - 1], j <- [0 .. res - 1] ]
    preds = predictClasses c (LA.fromLists centers)
    -- クラス色の対応: render は colorBy の categorical を sort.nub 順に並べ ggplotHue を
    -- 割り当てる。 同じ手順を再現し、 予測クラス k → クラス名 → cats 内 index → 色。
    names     = classNamesOf c
    labelOf k = classNameByIx names k
    classK    = if null names then sort (nub preds) else [0 .. length names - 1]
    cmap      = hueColorMap (map labelOf classK)
    colorFor k = Map.findWithDefault "#cccccc" (labelOf k) cmap

-- | confusion 行列のヒートマップ: テストデータ @X@ を予測し、 真値 @yTrue@ との件数を
--   x=予測 / y=真値 のセルに集計する ('MHeatmap'・色 = 件数)。
-- | クラス番号 k → 名前 (levels があれば @names !! k@・範囲外/空なら整数 show)。
--   分類 toPlot / confusion がクラス名を出す共通ヘルパ。
classNameByIx :: [Text] -> Int -> Text
classNameByIx names k
  | k >= 0 && k < length names = names !! k
  | otherwise                  = T.pack (show k)

confusionOf :: ClassPredict c => c -> LA.Matrix Double -> [Int] -> VisualSpec
confusionOf c x yTrue =
  let yPred   = predictClasses c x
      classes = sort (nub (yTrue ++ yPred))
      -- クラス番号 → クラス名 (levels があれば名前・無ければ整数)。 対角は t==p→同名→
      -- 同 index ゆえ、 名前順が整数順とずれても混同行列は正しい (対角=正解が保たれる)。
      nameOf  = classNameByIx (classNamesOf c)
      counts  = Map.fromListWith (+) [ ((t, p), 1 :: Int) | (t, p) <- zip yTrue yPred ]
      cells   = [ (t, p, Map.findWithDefault 0 (t, p) counts) | t <- classes, p <- classes ]
      xs = [ nameOf p | (_, p, _) <- cells ]
      ys = [ nameOf t | (t, _, _) <- cells ]
      vs = [ fromIntegral nC | (_, _, nC) <- cells ] :: [Double]
      -- セル件数の数値注釈 (sklearn ConfusionMatrixDisplay 同型)。 heatmap の categorical
      -- 軸は label を 'orderedCats' (= sort.nub) の index 位置に置くので、 text は同じ
      -- index 位置 (数値座標) に重ねる (任意クラス数で整合)。 背景 box 付き ('label') ゆえ
      -- viridis のどのセル色 (暗紫〜黄) でも読める。
      axisLabels = sort (nub xs)                       -- x/y 同 classes ゆえ共通・軸順と一致
      idxOf lbl  = maybe 0 fromIntegral (elemIndex lbl axisLabels) :: Double
      txIdx  = [ idxOf (nameOf p) | (_, p, _) <- cells ]
      tyIdx  = [ idxOf (nameOf t) | (t, _, _) <- cells ]
      cntTxt = [ T.pack (show nC) | (_, _, nC) <- cells ]
  in layer (heatmap (inlineCat xs) (inlineCat ys) (inline vs))
       <> layer (label (inline txIdx) (inline tyIdx) (inlineCat cntTxt))
       <> xLabel "predicted" <> yLabel "true"

-- ===========================================================================
-- MDS 埋め込み (モデル型 'MDSResult' + 群色オプション) — Phase 75.21
--
-- 'MDSResult' は @df |-> mds cfg cols@ の結果 (PCAResult 同格のモデル型)。
-- 既定は単色散布 ('Plottable' 'MDSResult' の @toPlot m@)、 群色は元データの列名を
-- 指定する 'mdsGroupBy' を @<>@ で合成する (regression の @statModel <> statColor@ と
-- 同形。 ただし 'statColor' は 'Color' 専用ゆえ「列名で群色」は別オプション)。
--
-- > m = df |-> mds defaultMDS ["x1","x2","x3"]
-- > noDf |>> toPlot m                              -- 単色
-- > noDf |>> toPlot (mdsView m <> mdsGroupBy "species")  -- species で群色
--
-- MDS は反転・回転自由度があるので軸の向きは本質でない (相対配置を見る)。
-- ===========================================================================

-- | MDS 埋め込みの描画オプション束 (Monoid)。 'mdsView' で結果を載せ、
-- 'mdsGroupBy' で群色列を足して @<>@ で合成する。
data MDSView = MDSView
  { mvResult   :: !(Maybe MDSResult)  -- ^ 描く埋め込み (後勝ち)。
  , mvGroupCol :: !(Maybe Text)       -- ^ 群色に使う元データの列名 (後勝ち)。
  }

instance Semigroup MDSView where
  a <> b = MDSView (orElse (mvResult b) (mvResult a))
                   (orElse (mvGroupCol b) (mvGroupCol a))
    where orElse (Just x) _ = Just x
          orElse Nothing  y = y

instance Monoid MDSView where
  mempty = MDSView Nothing Nothing

-- | MDS 結果を描画オプションに載せる (@<>@ の起点)。
mdsView :: MDSResult -> MDSView
mdsView m = mempty { mvResult = Just m }

-- | 元データの列名で群色を付ける (factor/数値どちらでも categorical 色に)。
-- @toPlot (mdsView m <> mdsGroupBy "species")@。
mdsGroupBy :: Text -> MDSView
mdsGroupBy c = mempty { mvGroupCol = Just c }

instance Plottable MDSResult where
  -- 単色の埋め込み散布。
  toPlot m = toPlot (mdsView m)

instance Plottable MDSView where
  toPlot v = case mvResult v of
    Nothing -> mempty
    Just m  ->
      let cols = LA.toColumns (mdsEmbedding m)
          xs   = if not (null cols)  then LA.toList (head cols) else []
          ys   = if length cols >= 2 then LA.toList (cols !! 1) else replicate (length xs) 0
          base = scatter (inline xs) (inline ys)
          withColor = case mvGroupCol v >>= \gc -> groupLabels gc (mdsSourceFrame m) of
            Just labs -> base <> colorBy (inlineCat labs)
            Nothing   -> base
      in layer withColor <> xLabel "MDS1" <> yLabel "MDS2"

-- | 元データの列を categorical な群ラベル ('[Text]') に変換する。 text 列
-- ('getTextVec') を優先し、 無ければ数値列 ('getDoubleVec') を整数寄せで文字列化。
groupLabels :: Text -> DXD.DataFrame -> Maybe [Text]
groupLabels gc frame =
  case getTextVec gc frame of
    Just tv -> Just (V.toList tv)
    Nothing -> case getDoubleVec gc frame of
      Just dv -> Just (map numLabel (V.toList dv))
      Nothing -> Nothing
  where
    -- 整数値は小数点を出さない (0.0 → "0")。
    numLabel x = let r = round x :: Int
                 in if fromIntegral r == x then T.pack (show r) else T.pack (show x)

-- | NN 学習損失曲線 (Phase 75.5)。 'mlpLossHist' (エポックごとの損失) を epoch (x) 対
-- loss (y) の line で描く。 損失が単調減少して平坦化すれば収束 (keras @history@ 同型)。
nnLossOf :: MLPFit -> VisualSpec
nnLossOf fit =
  let losses = mlpLossHist fit
      epochs = [ fromIntegral i | i <- [1 .. length losses] ] :: [Double]
  in layer (line (inline epochs) (inline losses))
       <> xLabel "epoch" <> yLabel "loss"

-- | カーネル SVM のサポートベクタ (α>0 の点) を強調散布する (Phase 75.12)。 第 0/1 特徴を
-- **そのクラスの色のまま ✚ (cross) マーカー**で打つ (通常点 ○ と形で区別・色はクラスで一致)。
-- 決定境界に重ねて「SV が境界を定義する」 様子を見る。 凡例は通常点散布側に任せる
-- ('legendOff')。 SV が無い/1 次元なら空。
svmSupportVectorsOf :: SVM -> VisualSpec
svmSupportVectorsOf m =
  let cols = LA.toColumns (svmSVx m)
      xs   = if not (null cols)  then LA.toList (head cols) else []
      ys   = if length cols >= 2 then LA.toList (cols !! 1) else []
      -- svmSVy は ±1 (+1 = 正クラス=1・-1 = クラス 0)。 散布の colorBy "cls" と同綴りに
      -- "0"/"1" の categorical 色で合わせる (= 同グループ同色)。
      labs = [ if y > 0 then "1" else "0" | y <- VU.toList (svmSVy m) ] :: [Text]
  in if null xs || null ys then mempty
     else layer ( scatter (inline xs) (inline ys)
                  <> colorBy (inlineCat labs) <> shape MShCross )

-- | 連続な決定スコアを持つ分類器 (decisionLineOf 用)。 score ≥ 0 が片クラス、 < 0 が他。
class ScorePredict c where
  decisionScore :: c -> LA.Matrix Double -> [Double]

instance ScorePredict SVM where
  decisionScore m x = VU.toList (predictSVMScore m x)

-- | 決定境界を **線 (等高線)** で描く (Phase 75.13b)。 'decisionBoundaryOf' が領域を色で
-- 塗り分けるのに対し、 こちらは決定スコア = 0 の等値線を marching squares で引く
-- (sklearn の @contour(…, levels=[0])@ 相当)。 スコアベースなので滑らかな曲線になる。
-- @res@ = grid 解像度 (大きいほど滑らか)。 2 特徴前提。
decisionLineOf :: ScorePredict c
               => c -> (Double, Double) -> (Double, Double) -> Int -> VisualSpec
decisionLineOf c (xlo, xhi) (ylo, yhi) res0 =
  let res = max 2 res0
      ax i = xlo + (xhi - xlo) * fromIntegral i / fromIntegral (res - 1)
      ay j = ylo + (yhi - ylo) * fromIntegral j / fromIntegral (res - 1)
      xsV  = V.generate res ax
      ysV  = V.generate res ay
      grid = LA.fromLists [ [xsV V.! i, ysV V.! j] | j <- [0 .. res - 1], i <- [0 .. res - 1] ]
      zV   = V.fromList (decisionScore c grid)     -- row-major: index = j*res + i
      z i j = zV V.! (j * res + i)
      lvl = 0 :: Double
      straddle a b = (a < lvl) /= (b < lvl)
      interp (px, py) (qx, qy) va vb =
        let t = (lvl - va) / (vb - va) in (px + t * (qx - px), py + t * (qy - py))
      cellSegs i j =
        let p00 = (xsV V.! i, ysV V.! j);       v00 = z i j
            p10 = (xsV V.! (i+1), ysV V.! j);   v10 = z (i+1) j
            p01 = (xsV V.! i, ysV V.! (j+1));   v01 = z i (j+1)
            p11 = (xsV V.! (i+1), ysV V.! (j+1)); v11 = z (i+1) (j+1)
            cross = concat
              [ [ interp p00 p10 v00 v10 | straddle v00 v10 ]
              , [ interp p10 p11 v10 v11 | straddle v10 v11 ]
              , [ interp p01 p11 v01 v11 | straddle v01 v11 ]
              , [ interp p00 p01 v00 v01 | straddle v00 v01 ] ]
        in case cross of
             [a, b]       -> [(a, b)]
             [a, b, d, e] -> [(a, b), (d, e)]   -- saddle (近似ペアリング)
             _            -> []
      segs = concat [ cellSegs i j | i <- [0 .. res - 2], j <- [0 .. res - 2] ]
  in mconcat
       [ layer ( line (inline [x1, x2]) (inline [y1, y2])
                 <> color (fromHex "#333333") )
       | ((x1, y1), (x2, y2)) <- segs ]



-- ===========================================================================
-- 部分従属図 (PDP / ICE) — Phase 75.27
--
-- 純粋エンジン 'partialDependence' ('Model.PartialDependence') を VisualSpec に落とす
-- 玄関。 回帰モデルは 'RegPredict' instance で短く (@pdpPlot rf trainX 0 "age"@)、 未対応の
-- モデルや分類確率は predict 閉包を直接渡す escape hatch (@partialDependencePlot@) で描く。
-- R @pdp::partial@ / sklearn @PartialDependenceDisplay@ 相当。
-- ===========================================================================

-- | 学習済モデルを評価点行列で走らせ、 各行の **連続予測値** を返す共通インターフェース
--   (回帰モデルの PDP を種に依らず組むための薄い抽象)。 分類確率など instance の無い
--   ものは 'partialDependencePlot' に predict 閉包を直接渡す。
class RegPredict m where
  predictReg :: m -> LA.Matrix Double -> [Double]

instance RegPredict RandomForest where
  predictReg rf x = map (RF.predictRF rf) (LA.toLists x)

instance RegPredict GBRegressor where
  predictReg gb x = VU.toList (predictGBR gb x)

-- | 高レベル PDP: 訓練 df ('ColumnSource') と**列名**で部分従属図を描く。
--   @featCols@ = fit に使った特徴列 (順序込み)、 @target@ = 部分従属を見る列。 注目特徴を
--   観測範囲の grid で振り、 他特徴は訓練分布のまま各行予測して平均した曲線を描く
--   (R pdp / sklearn @kind='average'@ 相当)。 列が引けない / target が featCols に無いときは空図。
pdpOf :: (RegPredict m, ColumnSource d) => m -> d -> [Text] -> Text -> VisualSpec
pdpOf model d featCols target =
  case (reqColsM featCols d, elemIndex target featCols) of
    (Right x, Just j) -> partialDependencePlot x (predictReg model) j target
    _                 -> mempty

-- | 高レベル PDP + ICE 重畳 (sklearn @kind='both'@)。 個体条件付き期待 (ICE) を薄灰で観測数
--   ぶん重ね、 平均 (PDP) を上描きする。 'pdpOf' の ICE 版。
pdpIceOf :: (RegPredict m, ColumnSource d) => m -> d -> [Text] -> Text -> VisualSpec
pdpIceOf model d featCols target =
  case (reqColsM featCols d, elemIndex target featCols) of
    (Right x, Just j) -> partialDependenceIcePlot x (predictReg model) j target
    _                 -> mempty

-- | 低レベル PDP: 訓練特徴**行列**と列 index を直接取る ('pdpOf' の実体)。
pdpPlot :: RegPredict m => m -> LA.Matrix Double -> Int -> Text -> VisualSpec
pdpPlot m x j name = partialDependencePlot x (predictReg m) j name

-- | 低レベル PDP + ICE (行列・列 index 版)。
pdpIcePlot :: RegPredict m => m -> LA.Matrix Double -> Int -> Text -> VisualSpec
pdpIcePlot m x j name = partialDependenceIcePlot x (predictReg m) j name

-- | 任意モデル用 PDP。 predict 閉包 (行列 → 予測値) を直接受ける escape hatch。
--   分類の部分従属 (あるクラスの予測確率) 等、 'RegPredict' instance の無いモデルに使う。
partialDependencePlot
  :: LA.Matrix Double -> (LA.Matrix Double -> [Double]) -> Int -> Text -> VisualSpec
partialDependencePlot x predict j name =
  let r = partialDependence x predict j 40
  in if null (pdpGrid r)
       then mempty
       else layer ( line (inline (pdpGrid r)) (inline (pdpMean r))
                    <> color (fromHex "#1f77b4") )
            <> xLabel name <> yLabel "partial dependence"

-- | 任意モデル用 PDP+ICE。 'partialDependencePlot' の ICE 重畳版 (predict 閉包版)。
partialDependenceIcePlot
  :: LA.Matrix Double -> (LA.Matrix Double -> [Double]) -> Int -> Text -> VisualSpec
partialDependenceIcePlot x predict j name =
  let r  = partialDependence x predict j 40
      g  = pdpGrid r
  in if null g
       then mempty
       else mconcat
              [ layer ( line (inline g) (inline curve)
                        <> color (fromHex "#bbbbbb") <> alpha 0.35 )
              | curve <- PD.pdpIce r ]
            <> layer ( line (inline g) (inline (pdpMean r))
                       <> color (fromHex "#1f77b4") )
            <> xLabel name <> yLabel "partial dependence"

-- ---------------------------------------------------------------------------
-- Phase 76.D: PDP を HBM 抽出子と同型に (Plottable 中間型 + toPlot・<> で合成)
--
-- @pdpOf model d featCols target@ は 'VisualSpec' を直に返すが、 demo は @[] |>> (…)@ の
-- ダミー束ねが要り不格好だった。 HBM の @forestOf@/@epred@ と同じく **Plottable 中間型**
-- ('PDPView') にし、 @toPlot@ で描画・@<>@ で装飾を合成する:
--
-- > noDf |>> (toPlot (pdp rf trainDf featCols target) <> title \"…\")
--
-- ★HBM 抽出子は fit が事後分布を内包し自己完結だが、 RF/GBM は訓練データを保持しないため
--   PDP は訓練 df ('ColumnSource') を受け取る (周辺化に訓練分布が要る)。 予測は 'RegPredict'。
-- ---------------------------------------------------------------------------

data PDPKind = PDPAverage | PDPBoth

-- | PDP の Plottable 中間型 (Phase 76.D)。 特徴行列・予測子・注目列 index を捕捉し、
--   'toPlot' で PDP (平均) / PDP+ICE 曲線に描く。 'pdp' / 'pdpIce' で作る。
data PDPView = PDPView
  { pvX       :: !(LA.Matrix Double)              -- 訓練特徴行列 (周辺化の分布)
  , pvPredict :: LA.Matrix Double -> [Double]     -- モデルの連続予測 (RegPredict 由来)
  , pvJ       :: !Int                             -- 注目特徴の列 index
  , pvName    :: !Text                            -- 注目特徴名 (x 軸)
  , pvKind    :: !PDPKind
  }

-- | 訓練 df + 特徴列から (特徴行列, 注目列 index) を解く。 引けない / target が featCols に
--   無いときは 0×0 行列 (toPlot が 'mempty' にする)。
pdpXJ :: ColumnSource d => d -> [Text] -> Text -> (LA.Matrix Double, Int)
pdpXJ d feats target =
  case (reqColsM feats d, elemIndex target feats) of
    (Right x, Just j) -> (x, j)
    _                 -> (LA.fromLists [], 0)

-- | 平均部分従属 (PDP)。 @noDf |>> (toPlot (pdp model trainDf featCols target) <> …)@。
pdp :: (RegPredict m, ColumnSource d) => m -> d -> [Text] -> Text -> PDPView
pdp model d feats target =
  let (x, j) = pdpXJ d feats target in PDPView x (predictReg model) j target PDPAverage

-- | PDP + ICE 重畳 (sklearn @kind='both'@)。 個体曲線 (薄灰) + 平均 (青)。
pdpIce :: (RegPredict m, ColumnSource d) => m -> d -> [Text] -> Text -> PDPView
pdpIce model d feats target =
  let (x, j) = pdpXJ d feats target in PDPView x (predictReg model) j target PDPBoth

instance Plottable PDPView where
  toPlot (PDPView x predict j name k)
    | LA.rows x == 0 = mempty
    | otherwise = case k of
        PDPAverage -> partialDependencePlot    x predict j name
        PDPBoth    -> partialDependenceIcePlot x predict j name

instance Plottable KNNClassifier where
  -- 訓練データをラベル色で散布 (第 0/1 特徴)。 KNN は X/Y を保持するので data-rich。
  -- 凡例は df|-> が載せた knnCClassNames があればクラス名・無ければ整数 ('classNameByIx')。
  toPlot knn =
    let cols = LA.toColumns (knnCX knn)
        xs   = if not (null cols)      then LA.toList (head cols)   else []
        ys   = if length cols >= 2     then LA.toList (cols !! 1)   else []
        labs = map (classNameByIx (knnCClassNames knn)) (VU.toList (knnCY knn))
    in layer (scatter (inline xs) (inline ys) <> colorBy (inlineCat labs))

instance Plottable DiscriminantFit where
  toPlot fit =
    classMeansScatter (LA.toLists (dfMeans fit))
                      (map round (LA.toList (dfClasses fit)))

instance Plottable NBModel where
  toPlot (NBGaussian m)    =
    classMeansScatterNamed (map LA.toList (gnbMeans m)) (gnbClasses m) (gnbClassNames m)
  toPlot (NBMultinomial m) =
    let labels = [ classNameByIx (mnbClassNames m) cl | cl <- mnbClasses m ]
    in layer (bar (inlineCat labels) (inline (map exp (mnbLogPrior m))))

-- ===========================================================================
-- 次元圧縮 (PLS / MultiGP) — Phase 68 A4
--
-- どちらも結果が自己完結 ('PCAResult' 同様) なので外部データ不要で 'Plottable':
--
--   * 'PLSFit' = 潜在空間の **score plot** (標本 T) を代表図に、 'loading plot' (変数 P)
--     と **VIP bar** を診断図束に。 いずれも既存 'MScatter'/'bar'。
--   * 'MultiGPResult' = **多出力の予測曲線 + 95% band** (出力ごとに色分け・x=index)。
--     'MLine' + 'MBand' を出力数ぶん重畳。
--
-- ※ 'Hanalyze.Model.MultiOutput' は変換+メトリクスの **ユーティリティ**で
-- fit 結果型を持たないため 'Plottable' 対象外 (多出力の「相関」図は既存
-- 'MultiFit' = 残差相関 heatmap が担当)。 新規 plot mark は不要。
-- ===========================================================================


-- | PLS 診断ビューの種別 (score / loading / VIP)。
data PLSViewKind = ScoreView | LoadingView | VipView
  deriving (Show, Eq)

-- | PLS の中間 Plottable Spec (HBM 式統一 — Phase 70.B)。 終端 'VisualSpec' を
-- 直返ししていた旧 @plsScorePlot@ 系を、 forest/trace 等と同じく **'Plottable' な
-- 中間 Spec** に揃える ('toPlot' 境界でオプション合成可・診断束を型で表現)。
data PLSView = PLSView !PLSFit !PLSViewKind

-- | score ビュー: 標本を潜在空間の第 1/2 成分 (T[:,0] vs T[:,1]) で散布。
scoreView :: PLSFit -> PLSView
scoreView fit = PLSView fit ScoreView

-- | loading ビュー: 変数を潜在空間の第 1/2 成分 (P[:,0] vs P[:,1]) で散布。
loadingView :: PLSFit -> PLSView
loadingView fit = PLSView fit LoadingView

-- | VIP ビュー: 変数重要度 (Variable Importance in Projection) bar。
vipView :: PLSFit -> PLSView
vipView fit = PLSView fit VipView

instance Plottable PLSView where
  toPlot (PLSView fit ScoreView) =
    let (xs, ys) = matCols2 (plsScoresT fit) 0 1
    in layer (scatter (inline xs) (inline ys))
         <> xLabel "comp 1" <> yLabel "comp 2"
  toPlot (PLSView fit LoadingView) =
    let (xs, ys) = matCols2 (plsLoadingsP fit) 0 1
    in layer (scatter (inline xs) (inline ys))
         <> xLabel "loading 1" <> yLabel "loading 2"
  toPlot (PLSView fit VipView) =
    let vips   = LA.toList (plsVIP fit)
        labels = [ "f" <> T.pack (show k) | k <- [1 .. length vips] ]
    in layer (bar (inlineCat labels) (inline vips))

instance Plottable PLSFit where
  -- 代表図 = score ビュー (標本の潜在空間布置)。
  toPlot = toPlot . scoreView
  -- 診断束 = score / loading / VIP の 3 枚。
  diagnosticPlots fit = map toPlot [ scoreView fit, loadingView fit, vipView fit ]

-- ===========================================================================
-- 時系列・生存・FDA (GARCH / AFT / FDA) — Phase 68 A5
--
-- 新規 plot mark は不要 (既存 line/band の重畳):
--
--   * 'GARCHFit'      = 系列 (μ + ε_t) + 条件付き volatility 帯 (μ ± 2σ_t) の帯付き線。
--   * 'AFTFit'        = パラメトリック生存曲線 S(t|x)。 fit は観測時刻を持たないので
--                       代表図 ('toPlot') は **基準共変量** (intercept のみ) の曲線、
--                       任意共変量は 'aftSurvivalAt' ヘルパ。 t 範囲は予測平均寿命から導出。
--   * 'FunctionalPCA' = 平均関数 + 上位固有関数を grid 上に重畳 (x = grid index)。
--   * 'FLMResult'     = 関数回帰係数 β(t) の曲線。
-- ===========================================================================

-- | GARCH の条件付き volatility 帯付き線: 系列 @y_t = μ + ε_t@ の line に、
--   @μ ± 2σ_t@ (σ_t = √σ²_t) の帯を重ねる。 x = 時刻 index。
garchVolatility :: GARCHFit -> VisualSpec
garchVolatility fit =
  let eps   = LA.toList (gResiduals fit)
      s2    = LA.toList (gSigma2 fit)
      mu    = gMu fit
      n     = min (length eps) (length s2)
      xs    = [ fromIntegral i | i <- [1 .. n] ] :: [Double]
      ys    = [ mu + e | e <- take n eps ]
      sig   = [ sqrt (max 0 v) | v <- take n s2 ]
      lo    = zipWith (\_ s -> mu - 2 * s) xs sig
      hi    = zipWith (\_ s -> mu + 2 * s) xs sig
  in layer (band (inline xs) (inline lo) (inline hi) <> alpha 0.25)
       <> layer (line (inline xs) (inline ys))
       <> xLabel "t" <> yLabel "y"

instance Plottable GARCHFit where
  toPlot = garchVolatility

-- | AFT 生存曲線 S(t|x): 共変量 @x@ の線形予測子 @lp = x·β@ から
--   @z(t) = (log t − lp)/σ@・@S = exp(logS dist z)@ を t-grid 上で評価する。
--   t 範囲は予測平均寿命の @(0.01, 3×mean)@、 grid 120 点。
aftSurvivalAt :: AFTFit -> [Double] -> VisualSpec
aftSurvivalAt fit x =
  let beta   = LA.toList (aftBeta fit)
      lp     = sum (zipWith (*) x beta)
      sigma  = aftScale fit
      dist   = aftDistribution fit
      meanL  = let v = predictAFT fit (LA.fromLists [x]) in head (LA.toList v)
      tMax   = if meanL > 0 && not (isInfinite meanL) then 3 * meanL else 10
      tMin   = max 1e-3 (tMax / 200)
      ts     = linspace tMin tMax 120
      surv t = exp (logS dist ((log t - lp) / sigma))
      ss     = map surv ts
  in layer (line (inline ts) (inline ss))
       <> xLabel "t" <> yLabel "S(t)"

instance Plottable AFTFit where
  -- 代表図 = 基準共変量 (intercept 列のみ = [1,0,…,0]) の生存曲線。
  toPlot fit =
    let p = LA.size (aftBeta fit)
        xRef = if p <= 0 then [] else 1 : replicate (p - 1) 0
    in aftSurvivalAt fit xRef


instance Plottable FunctionalPCA where
  -- 平均関数 + 上位 (最大 3) 固有関数を grid 上に重畳。
  toPlot fpca =
    let meanFn = LA.toList (fpcaMeanFn fpca)
        eigs   = LA.toRows (fpcaEigenfn fpca)
        eigNs  = [ ("PC" <> T.pack (show k), LA.toList e)
                 | (k, e) <- zip [1 :: Int ..] (take 3 eigs) ]
    in gridCurves (("mean", meanFn) : eigNs)

instance Plottable FLMResult where
  -- 関数回帰係数 β(t) の曲線 (x = grid index)。
  toPlot flm =
    let betaFn = LA.toList (flmBetaFn flm)
        xs     = [ fromIntegral i | i <- [1 .. length betaFn] ] :: [Double]
    in layer (line (inline xs) (inline betaFn))
         <> xLabel "t" <> yLabel "beta(t)"

-- ===========================================================================
-- 罰則回帰・因果探索 (Regularized / LiNGAM) — Phase 68 A6
--
-- 新規 plot mark は不要:
--
--   * 'RegFit'          = 単一 λ の係数 ('rfBeta') を bar (代表図)。
--   * 'regPathPlot'     = 正則化パス @[(λ, [β_j])]@ ('regularizationPath' 出力) を、
--                         係数ごとに 1 本の line で λ-横軸に重畳 (= LASSO 係数パス図)。
--   * 'DirectLiNGAMFit' = 推定した因果構造を **MDAG** で描く (B 行列 → node/edge、
--                         決定木と同じ MDAG 再利用)。 edge j→i は @|adjacency[i,j]|>0@。
-- ===========================================================================

instance Plottable RegFit where
  -- 係数 bar (b1, b2, … = rfBeta)。 intercept 含む並びをそのまま描く。
  toPlot fit =
    let bs     = LA.toList (rfBeta fit)
        labels = [ "b" <> T.pack (show k) | k <- [0 .. length bs - 1] ]
    in layer (bar (inlineCat labels) (inline bs))

-- | 正則化パス図: @[(λ, [β_j])]@ を係数ごとに 1 本の line で重畳。 横軸は **log₁₀λ**
--   (glmnet の係数パス図と同じ慣例・小 λ=full model が左、 大 λ=sparse が右)。 色=係数 index。
--   λ は正を仮定する (パスの λ グリッドは常に @> 0@)。
regPathPlot :: [(Double, [Double])] -> VisualSpec
regPathPlot path
  | null path = mempty
  | otherwise =
      let logLams = map (logBase 10 . fst) path   -- x = log₁₀λ
          rows = map snd path           -- λ ごとの [β_j]
          p    = minimum (map length rows)
          mkCoef j =
            let ys  = [ r !! j | r <- rows ]
                lbl = "b" <> T.pack (show j)
            in layer ( line (inline logLams) (inline ys)
                     <> colorBy (inlineCat (replicate (length logLams) lbl)) )
      in mconcat [ mkCoef j | j <- [0 .. p - 1] ]
           <> xLabel "log10(lambda)" <> yLabel "coef"

-- | 隣接行列 + 変数名から因果 DAG (MDAG) を描く低レベル (Phase 77.A で切り出し)。
--   edge @j→i@ は @|adj[i,j]| > 0@ (= x_i が x_j に依存)。 @names@ が列数と一致しなければ
--   @x0..@ フォールバック。 全 LiNGAM variant の Plottable が共有する。
lingamDagNamed :: [Text] -> LA.Matrix Double -> VisualSpec
lingamDagNamed rawNames adj =
  let p     = LA.rows adj
      names = if length rawNames == p && p > 0
                then rawNames
                else [ "x" <> T.pack (show j) | j <- [0 .. p - 1] ]
      dnodes = [ DAGNode { dnId = nm, dnLabel = nm, dnKind = NodeObserved
                         , dnDist = Nothing, dnX = 0, dnY = 0 } | nm <- names ]
      dedges = [ DAGEdge (names !! j) (names !! i) Nothing Nothing
               | i <- [0 .. p - 1], j <- [0 .. p - 1]
               , abs (adj `LA.atIndex` (i, j)) > 0 ]
      (positioned, routed) = layoutHierarchicalFullWithPlates dnodes dedges []
  in bakeDAGRoutesInSpec $
       layer (dagFromListsWithPlates positioned routed LayoutHierarchical [])

-- | 推定因果構造 (DirectLiNGAM) を MDAG で描く。 ノード = @x0..x_{p-1}@ (変数名は
--   高レベル @df |-> directLingam@ 経由で付く・'LiNGAMFitted' の Plottable 参照)。
lingamDag :: DirectLiNGAMFit -> VisualSpec
lingamDag fit = lingamDagNamed [] (dlAdjacency fit)

instance Plottable DirectLiNGAMFit where
  toPlot = lingamDag

-- | 高レベル @df |-> directLingam cols@ の結果 = **実変数名**の因果 DAG (Phase 77.A)。
instance Plottable (LiNGAMFitted DirectLiNGAMFit) where
  toPlot (LiNGAMFitted fit names) = lingamDagNamed names (dlAdjacency fit)

-- | ParceLiNGAM の名前付き DAG (Phase 77.B・pcAdjacency)。
instance Plottable (LiNGAMFitted ParceFit) where
  toPlot (LiNGAMFitted fit names) = lingamDagNamed names (pcAdjacency fit)

-- | MultiGroupLiNGAM の**共通** DAG (Phase 77.B・多数決 mgCommonAdj・名前付き)。
instance Plottable (LiNGAMFitted MultiGroupFit) where
  toPlot (LiNGAMFitted fit names) = lingamDagNamed names (mgCommonAdj fit)

-- | VARLiNGAM の**時間ラグ DAG** (Phase 77.B)。 ノード = 各変数の @name[t]@ / @name[t-l]@、
--   辺 = 同時刻 (@B0@: x_j[t]→x_i[t]) + ラグ (@structuralLags[l]@: x_j[t-l]→x_i[t])。
--   @thr@ 未満の係数は辺を出さない。 孤立したラグノード (辺に現れない) は省く。
varLagDagNamed :: [Text] -> LA.Matrix Double -> [LA.Matrix Double] -> Double -> VisualSpec
varLagDagNamed rawNames b0 lags thr =
  let k    = LA.rows b0
      base = if length rawNames == k && k > 0
               then rawNames else [ "x" <> T.pack (show j) | j <- [0 .. k - 1] ]
      p    = length lags
      nm i 0 = base !! i <> "[t]"
      nm i l = base !! i <> "[t-" <> T.pack (show l) <> "]"
      contempEdges = [ DAGEdge (nm j 0) (nm i 0) Nothing Nothing
                     | i <- [0 .. k - 1], j <- [0 .. k - 1]
                     , abs (b0 `LA.atIndex` (i, j)) > thr ]
      lagEdges = [ DAGEdge (nm j l) (nm i 0) Nothing Nothing
                 | l <- [1 .. p], i <- [0 .. k - 1], j <- [0 .. k - 1]
                 , abs ((lags !! (l - 1)) `LA.atIndex` (i, j)) > thr ]
      dedges = contempEdges ++ lagEdges
      refIds = concatMap (\(DAGEdge a b _ _) -> [a, b]) dedges
      allNodes = [ (i, l) | l <- [0 .. p], i <- [0 .. k - 1] ]
      keep (i, l) = l == 0 || nm i l `elem` refIds        -- 現時刻は常に・ラグは辺があるものだけ
      dnodes = [ DAGNode { dnId = nm i l, dnLabel = nm i l, dnKind = NodeObserved
                         , dnDist = Nothing, dnX = 0, dnY = 0 }
               | (i, l) <- allNodes, keep (i, l) ]
      (positioned, routed) = layoutHierarchicalFullWithPlates dnodes dedges []
  in bakeDAGRoutesInSpec $
       layer (dagFromListsWithPlates positioned routed LayoutHierarchical [])

-- | VARLiNGAM の高レベル結果 = 時間ラグ DAG (辺閾値 0.1・同時刻 + ラグ)。
instance Plottable (LiNGAMFitted VARLiNGAMFit) where
  toPlot (LiNGAMFitted fit names) =
    varLagDagNamed names (vlB0 fit) (vlStructuralLags fit) 0.1

-- | PairwiseLiNGAM の 2 変数向き図 (Phase 77.B)。 検出向きの矢印 1 本 (Inconclusive は無向)。
--   2×2 隣接に落として 'lingamDagNamed' を再利用する。
instance Plottable (LiNGAMFitted PairwiseResult) where
  toPlot (LiNGAMFitted r names) =
    let adj = case prDirection r of
          XtoY         -> LA.fromLists [[0, 0], [1, 0]]   -- x(0) → y(1): adj[1,0]=1
          YtoX         -> LA.fromLists [[0, 1], [0, 0]]   -- y(1) → x(0)
          Inconclusive -> LA.fromLists [[0, 0], [0, 0]]   -- 無向 (2 ノードのみ)
    in lingamDagNamed names adj

-- | ICA-LiNGAM の名前付き DAG (Phase 77.C・ilAdjacency)。
instance Plottable (LiNGAMFitted ICALiNGAMFit) where
  toPlot (LiNGAMFitted fit names) = lingamDagNamed names (ilAdjacency fit)

-- | 相関ネットワークのグラフ (Phase 77)。 @|r| > cgThreshold@ の対を辺にする (無向・向きは
--   index 順の便宜配置で**因果でない**)。 LiNGAM DAG と対比すると間接相関の過剰さが分かる。
--   下三角のみ辺にして重複/自己ループを避ける (相関は対称ゆえ)。
instance Plottable CorrelationGraph where
  toPlot (CorrelationGraph corr names thr) =
    let p   = LA.rows corr
        adj = LA.build (p, p)
                (\i j -> let (ii, jj) = (round i, round j)
                         in if ii > jj && abs (corr `LA.atIndex` (ii, jj)) > thr
                              then 1 else 0 :: Double)
    in lingamDagNamed names adj

-- | BootstrapLiNGAM の**確信度 DAG** (Phase 77.C)。 出現確率 ≥ 0.5 のエッジだけ描く
--   (= 過半数の bootstrap で現れた信頼できる因果構造)。 全確率は 'bootstrapEdgeProbOf' で。
instance Plottable (LiNGAMFitted BootstrapResult) where
  toPlot (LiNGAMFitted res names) =
    let prob = brEdgeProbability res
        p    = LA.rows prob
        adj  = LA.build (p, p)
                 (\i j -> if prob `LA.atIndex` (round i, round j) >= 0.5 then 1 else 0)
    in lingamDagNamed names adj

-- | BootstrapLiNGAM の**エッジ出現確率ヒートマップ** (Phase 77.C)。 行=結果 i・列=原因 j、
--   セル = P(j→i) (0..1)。 確信度の全体像を DAG と別に見せる (python lingam の確率行列相当)。
bootstrapEdgeProbOf :: LiNGAMFitted BootstrapResult -> VisualSpec
bootstrapEdgeProbOf (LiNGAMFitted res rawNames) =
  let prob  = brEdgeProbability res
      p     = LA.rows prob
      names = if length rawNames == p && p > 0
                then rawNames else [ "x" <> T.pack (show j) | j <- [0 .. p - 1] ]
      cells = [ (names !! j, names !! i, prob `LA.atIndex` (i, j))
              | i <- [0 .. p - 1], j <- [0 .. p - 1] ]
      xs = [ c | (c, _, _) <- cells ]      -- 原因 j (x 軸)
      ys = [ r | (_, r, _) <- cells ]      -- 結果 i (y 軸)
      vs = [ v | (_, _, v) <- cells ]
  in layer (heatmap (inlineCat xs) (inlineCat ys) (inline vs))
       <> xLabel "cause (j)" <> yLabel "effect (i)"

-- ===========================================================================
-- DOE prediction profiler — Phase 78.C/D/F
--
-- JMP の Prediction Profiler 相当 = **応答 × 各因子**のパネルをグリッドに並べる
-- (行=応答・列=因子)。 各パネル = 予測線 + 95% CI 帯 (他因子は中央値固定) + 打点。
-- 打点は 'Raw' (実測 y) か 'Partial' (偏残差 = 部分効果 + 全モデル残差) を @<>@ で選ぶ。
-- 既存 effect plot ('statModelMulti' + 'along' + 'holdAt') を再利用する。
--
-- 中間 Plottable 型 ('ProfilerSpec') にして @toPlot@ で描画・@<>@ でオプション合成
-- (HBM @epred@ / 'PDPView' と同じ流儀)。 打点はモデル ('mvFrame') の観測値から算出
-- するので @noDf@ で束ねられる。 複数応答は @df |-> 'multiOutput' ys (designModel plan)@
-- が返す @[(応答名, モデル)]@ をそのまま渡す。
--
-- > let model = df |-> multiOutput ["strength","yield"] (designModel plan)
-- > noDf |>> toPlot (profiler model ["temp","time"] <> profilerResidual Partial)
-- ===========================================================================

-- | 打点の種別。 'Raw' = 実測 y (他因子が動くぶん予測線から縦に散る = 多変量の正しい挙動)。
--   'Partial' = **偏残差** @fⱼ(xⱼ) + (全モデル残差)@ で他因子の寄与を除き点を予測線に乗せる
--   (R @termplot(partial.resid=TRUE)@ / @car::crPlots@ 相当)。
data ResidualMode = Raw | Partial
  deriving (Eq, Show)

-- | prediction profiler の中間 Plottable Spec (Phase 78.F)。 @(応答名, モデル)@ のリスト
--   (複数応答)・因子名・打点モード ('ResidualMode') を捕捉し、 'toPlot' で「行=応答 ×
--   列=因子」 のグリッドに描く。 'profiler' で作り、 @<> 'profilerResidual' Partial@ で
--   モードを合成する。
data ProfilerSpec m = ProfilerSpec
  { psModels   :: [(Text, m)]        -- ^ (応答ラベル, 学習済モデル)。 行になる。
  , psFactors  :: [Text]             -- ^ 説明因子名。 列になる。
  , psResidual :: Maybe ResidualMode -- ^ 打点モード (合成後 'Nothing' は 'Raw' 既定)。
  }

-- | 右バイアス合成 (option-only 片は models\/factors が空)。 mode は後勝ち。
instance Semigroup (ProfilerSpec m) where
  a <> b = ProfilerSpec
    { psModels   = psModels a  <> psModels b
    , psFactors  = if null (psFactors b) then psFactors a else psFactors b
    , psResidual = psResidual b <|> psResidual a }

instance Monoid (ProfilerSpec m) where
  mempty = ProfilerSpec [] [] Nothing

-- | @profiler models factors@ — 応答×因子の profiler。 @models@ は
--   @df |-> 'multiOutput' ys (designModel plan)@ が返す @[(応答名, モデル)]@。 既定は 'Raw'。
profiler :: [(Text, m)] -> [Text] -> ProfilerSpec m
profiler models factors = ProfilerSpec models factors Nothing

-- | 打点モードを差す option (@<>@ で合成)。 @profiler … <> profilerResidual Partial@。
profilerResidual :: ResidualMode -> ProfilerSpec m
profilerResidual mode = mempty { psResidual = Just mode }

instance MultiVarModel m => Plottable (ProfilerSpec m) where
  toPlot (ProfilerSpec models factors mMode)
    | null models || null factors = mempty
    | otherwise =
        subplots [ panel lbl m f | (lbl, m) <- models, f <- factors ]
          <> subplotCols (length factors)
    where
      mode = fromMaybe Raw mMode
      -- 1 パネル = 予測線 + CI + 打点 (Raw: 実測 y / Partial: 偏残差)。他因子は中央値固定。
      panel lbl m f =
        let mf    = mvFrame m
            contOf nm = case lookup nm (mfRoles mf) of
              Just (RoleContinuous xs) -> V.toList xs
              _                        -> []
            xsf   = contOf f
            (pts, ylab) = case mode of
              Raw ->
                let ysObs = case [ v | (_, RoleResponse v) <- mfRoles mf ] of
                              (v : _) -> V.toList v
                              []      -> []
                in (ysObs, lbl)
              Partial ->
                let ysObs = case [ v | (_, RoleResponse v) <- mfRoles mf ] of
                              (v : _) -> V.toList v
                              []      -> []
                    (muFull, _) = mvEvalFrame m 0.95 mf
                    resid       = zipWith (-) ysObs muFull
                    -- 部分効果 fⱼ(xⱼ): f=観測値・他因子=中央値固定 (予測線と同じ hold)。
                    ef          = evalFrame mf f Median [] xsf
                    (muPart, _) = mvEvalFrame m 0.95 ef
                in (zipWith (+) muPart resid, "partial: " <> lbl)
        in layer (scatter (inline xsf) (inline pts))
             <> toPlot (statModelMulti m (along f) <> holdAt Median <> grid 60)
             <> xLabel f <> yLabel ylab

-- | RSM **等高線 / 応答曲面** (Phase 78.E)。 2 因子 (v1, v2) を grid で動かし他因子を
--   中央値固定して応答 μ̂ を評価し、 **塗り等値帯 ('contourFilled') + 等高線 ('contour')** で
--   描く (R @rsm::contour@ / matplotlib @contourf+contour@ 相当・応答面を平面で俯瞰)。
--   3D の応答曲面は 'surfaceOf' (別途 @saveSVG3D@)。 評価はモデル観測範囲なので
--   @noDf |>> contourOf model "temp" "time"@ で描ける。
contourOf :: MultiVarModel m => m -> Text -> Text -> VisualSpec
contourOf m v1 v2 =
  let (gxs, gys, grid') = surfaceGrid m v1 v2 (defaultSurfaceOpts { soHoldAt = Median })
      -- grid' !! j !! i = μ̂(gxs!!i, gys!!j)。 (x, y, z) へ平坦化。
      pts = concat (zipWith (\gy row -> zipWith (\gx z -> (gx, gy, z)) gxs row) gys grid')
      xs  = [ x | (x, _, _) <- pts ]
      ys  = [ y | (_, y, _) <- pts ]
      zs  = [ z | (_, _, z) <- pts ]
  in layer (contourFilled (inline xs) (inline ys) (inline zs))
       <> layer (contour (inline xs) (inline ys) (inline zs) <> contourLevels 10)
       <> xLabel v1 <> yLabel v2

-- ===========================================================================
-- 記述統計・検定 (Stat.*) — Phase 68 A7
--
-- 新規 plot mark は不要:
--
--   * 'TestResult'  = 効果量 + 95% CI の **forest** (検定パラメータの区間 + 0 基準線)。
--                     代表図 ('toPlot') は 1 行 forest、 複数検定は 'testForest'。
--   * 'describeBox' = 生データ列の **box plot** (= describe の分布図・5 数要約を可視化)。
-- ===========================================================================

-- | 検定結果の forest plot: 各検定の 95% CI ('trCI') を区間、 中心を点推定として
--   1 行に並べ、 0 の基準線を引く。 CI を持たない検定は除外する。 行ラベルは
--   'trMethod'。 同種検定を群間で並べるなど **ラベルを区別したい場合は
--   'testForestLabeled'** を使う。
--
-- ⚠ 0 基準線は **平均差・効果量** (null = 0) 向け。 生の平均など null ≠ 0 の量を
-- 混在させると軸ドメインが歪むので、 同一スケールの量だけを 1 枚に並べること。
testForest :: [TestResult] -> VisualSpec
testForest = testForestLabeled . map (\r -> (trMethod r, r))

-- | ラベル指定版 'testForest' (= 行ラベルを呼び出し側で与える)。 同じ検定種を
--   群ごとに並べる (= 同名衝突を避ける) 用途に使う。
testForestLabeled :: [(Text, TestResult)] -> VisualSpec
testForestLabeled labeled =
  let rows  = [ (nm, lo, hi) | (nm, r) <- labeled, Just (lo, hi) <- [trCI r] ]
      names = [ nm            | (nm, _,  _ ) <- rows ]
      ests  = [ (lo + hi) / 2 | (_,  lo, hi) <- rows ]
      errs  = [ (hi - lo) / 2 | (_,  lo, hi) <- rows ]
  in if null rows
       then mempty
       else layer (forest (inlineCat names) (inline ests) (inline errs) <> forestNull 0)

instance Plottable TestResult where
  -- 代表図 = 単一検定の 1 行 forest (effect/CI)。
  toPlot r = testForest [r]

-- | describe の分布図: 生データ列の box plot (5 数要約を可視化)。
describeBox :: [Double] -> VisualSpec
describeBox xs = layer (boxplot (inline xs))

-- ===========================================================================
-- 次元圧縮 (PLS effect plot) — Phase 70.B2/B3
-- ===========================================================================

instance MultiVarModel PLSModel where
  mvFrame = plsmFrame
  -- PLS は閉形式 CI を持たない → band 非提供 (曲線のみ・GAM と同じ honest 方針)。
  mvEvalFrame m _level ef =
    let n      = mfNRows ef
        colOf nm = case lookup nm (mfRoles ef) of
          Just (RoleContinuous v) -> LA.fromList (V.toList v)
          _                       -> LA.fromList (replicate n 0)
        xMat  = LA.fromColumns (map colOf (plsmXNames m))   -- n × p (xNames 順)
        yPred = predictPLS (plsmFit m) xMat                 -- n × q
        ycols = LA.toColumns yPred
        idx   = plsmOutIdx m
        mu    = if idx < length ycols then LA.toList (ycols !! idx)
                                      else replicate n 0
    in (mu, Nothing)

-- Phase 78.G-e: 多変量カーネル回帰 (GP/RFF) を effect plot / profiler / contour で使う
-- (DOE の非 LM 化)。 mvEvalFrame は ef から予測子を 'gprnNames' 順に取り 'gprnPredict'
-- に渡す ('PLSModel' と同型)。 帯 = **事後予測帯** (潜在分散 + 観測 noise σ_n²) で、
-- 分布あり象限 (Gp/GpRff) のみ Just、 mean のみ象限 (Krr/KrrRff) は帯なし。 'gpmvVar' は
-- σ_n² を含まない ('GP.hs' の diagKss=σ_f²) ので noise を足して予測帯にする。
instance MultiVarModel GPRegModelN where
  mvFrame m =
    let n     = LA.size (gprnYraw m)
        roles = ("__gp_resp", RoleResponse (V.fromList (LA.toList (gprnYraw m))))
              : [ (nm, RoleContinuous (V.fromList (LA.toList xv)))
                | (nm, xv) <- zip (gprnNames m) (gprnXraws m) ]
    in ModelFrame { mfRoles = roles, mfNRows = n }
  mvEvalFrame m level ef =
    let n        = mfNRows ef
        colOf nm = case lookup nm (mfRoles ef) of
          Just (RoleContinuous v) -> V.toList v
          _                       -> replicate n 0
        xMat        = LA.fromColumns (map (LA.fromList . colOf) (gprnNames m))  -- n × p
        (mu, mbVar) = gprnPredict m xMat
        z           = quantileNormal (1 - (1 - level) / 2)
        sn2         = max 0 (gpNoiseVar (gprnParams m))
    in case mbVar of
         Just vs -> let sds = map (\v -> sqrt (max 0 v + sn2)) vs
                    in ( mu, Just ( zipWith (\u s -> u - z * s) mu sds
                                  , zipWith (\u s -> u + z * s) mu sds ) )
         Nothing -> (mu, Nothing)

-- | DOE 階層ベイズ fit の effect plot 開通 (Phase 78.G-f)。固定効果 β の事後 draw で
--   評価点の μ を計算し、事後予測帯 (μ の分散 + 観測 noise σ²) を CI slot に載せる。
--   ランダム効果は集団平均で marginalize (profiler = 代表条件の予測)。
instance MultiVarModel DesignHBMFit where
  mvFrame = dhfFrame
  mvEvalFrame m level ef =
    case designMatrixF (dhfFormula m) ef of
      Left _          -> ([], Nothing)
      Right (xMat, _) ->
        let rows  = map LA.toList (LA.toRows xMat)   -- 評価点 × p
            draws = dhfBetaDraws m                   -- draws × p
            muAt row = [ sum (zipWith (*) row bd) | bd <- draws ]
            perPoint = map muAt rows                 -- 評価点ごとの draw 列
            z     = quantileNormal (1 - (1 - level) / 2)
            s2bar = let ss = dhfSigmaDraws m
                    in if null ss then 0 else sum (map (^ (2::Int)) ss) / fromIntegral (length ss)
            center = map mean0L perPoint
            sds    = map (\ds -> sqrt (varL ds + s2bar)) perPoint
        in ( center
           , Just ( zipWith (\c s -> c - z * s) center sds
                  , zipWith (\c s -> c + z * s) center sds ) )
    where
      mean0L xs = if null xs then 0 else sum xs / fromIntegral (length xs)
      varL   xs = let mu = mean0L xs
                  in if null xs then 0 else sum (map (\x -> (x - mu) ^ (2::Int)) xs) / fromIntegral (length xs)

