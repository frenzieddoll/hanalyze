{-# LANGUAGE OverloadedStrings #-}
-- | LM / GLM / GLMM の分析結果を1つの HTML レポートにまとめるモジュール。
--
-- セクション構成:
--   1. データの特性 (N, 列統計, ヒストグラム)
--   2. モデル概要 (種別, 式, ファミリー/リンク)
--   3. 回帰結果 (係数表, R², 散布図, 残差プロット)
--   4. 対話的予測 (リアルタイム散布図 + CI/PI 表示)
--   5. 付録 (モデルの原理説明)
module Viz.AnalysisReport
  ( -- * 設定
    AnalysisReportConfig (..)
  , defaultAnalysisConfig
    -- * スムーズフィットデータ
  , SmoothData (..)
    -- * フィット要約
  , FitSummary (..)
  , mkFitSummary
  , GLMMSummary (..)
  , mkGLMMSummary
    -- * GP フィット要約
  , GPKernelFit (..)
  , GPFitSummary (..)
    -- * HBM (ベイズ回帰) フィット要約
  , HBMRegSummary (..)
    -- * モデルフィット (統一型)
  , ModelFit (..)
    -- * 名前付きプロット
  , NamedPlot (..)
    -- * レポート生成
  , writeAnalysisReport
  , writeAnalysisReportPlots
    -- * 複数モデル比較レポート
  , CompareEntry (..)
  , writeComparisonReport
  ) where

import Data.Aeson (encode)
import Data.ByteString.Lazy (toStrict)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (decodeUtf8)
import Graphics.Vega.VegaLite (VegaLite, fromVL)
import Numeric (showFFloat)
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA

import DataFrame.Core
import MCMC.Core    (Chain, chainSamples, chainAccepted, chainTotal)
import Model.Core   (FitResult (..), coeffList, fittedList,
                     residualsV, rSquared1)
import Model.GLM    (Family (..), LinkFn (..))
import Stat.ModelSelect (WAICResult (..), LOOResult (..))
import Model.GLMM   (GLMMResult (..))
import Model.GP     (Kernel (..), GPParams (..), GPResult (..), GPPredData (..))
import Model.HBM    (ModelGraph)
import Viz.Assets   (vegaJS, vegaLiteJS, vegaEmbedJS)
import Viz.Core     (PlotConfig (..), OutputFormat (..), writeSpec)
import Viz.GP       (gpPlot)
import Viz.ModelGraph (buildMermaid)

-- ---------------------------------------------------------------------------
-- Public types
-- ---------------------------------------------------------------------------

data AnalysisReportConfig = AnalysisReportConfig
  { arcTitle :: Text
  } deriving (Show)

defaultAnalysisConfig :: Text -> AnalysisReportConfig
defaultAnalysisConfig = AnalysisReportConfig

-- | スムーズフィット曲線データ (対話的予測チャート用)。
data SmoothData = SmoothData
  { sdXs      :: [Double]  -- ^ グリッド x 値
  , sdYs      :: [Double]  -- ^ 予測 y 値
  , sdLower   :: [Double]  -- ^ CI/PI 下限
  , sdUpper   :: [Double]  -- ^ CI/PI 上限
  , sdHasBand :: Bool      -- ^ バンドを持つか
  } deriving (Show)

-- | LM / GLM の回帰サマリー。
data FitSummary = FitSummary
  { fsModelType    :: Text                       -- ^ "LM", "GLM (Poisson/Log)" etc.
  , fsFormula      :: Text                       -- ^ "y ~ x + x²"
  , fsCoeffs       :: [(Text, Double)]           -- ^ (ラベル, 値)
  , fsR2           :: Double                     -- ^ R² or McFadden R²
  , fsR2Label      :: Text                       -- ^ "R²" or "McFadden R²"
  , fsFitted       :: [Double]                   -- ^ fitted values
  , fsResiduals    :: [Double]                   -- ^ residuals
  , fsLinkName     :: Text                       -- ^ "identity"|"log"|"logit"|"sqrt"
  , fsXColDegs     :: [(Text, Int)]              -- ^ x列と次数 (JS予測用)
  , fsSmoothData   :: Maybe (Text, SmoothData)   -- ^ (x列名, スムーズデータ) 単回帰のみ
  , fsModelSelect  :: Maybe (WAICResult, LOOResult) -- ^ WAIC/LOO-CV (--waic 時のみ)
  } deriving (Show)

mkFitSummary
  :: Family
  -> LinkFn
  -> [(Text, Int)]
  -> Maybe (Text, SmoothData)
  -> FitResult
  -> FitSummary
mkFitSummary fam lnk colDegs mSmooth res = FitSummary
  { fsModelType    = modelTypeLabel fam lnk
  , fsFormula      = formulaText colDegs
  , fsCoeffs       = zip (coeffLabels colDegs) (coeffList res)
  , fsR2           = rSquared1 res
  , fsR2Label      = r2Label fam
  , fsFitted       = fittedList res
  , fsResiduals    = LA.toList (residualsV res)
  , fsLinkName     = linkName lnk
  , fsXColDegs     = colDegs
  , fsSmoothData   = mSmooth
  , fsModelSelect  = Nothing
  }

-- | GLMM / LME のサマリー。
data GLMMSummary = GLMMSummary
  { gsModelType    :: Text
  , gsFormula      :: Text
  , gsFixed        :: [(Text, Double)]
  , gsR2           :: Double
  , gsR2Label      :: Text
  , gsGroupCol     :: Text
  , gsRandVar      :: Double
  , gsResidVar     :: Double
  , gsICC          :: Double
  , gsBLUPs        :: [(Text, Double)]
  , gsFitted       :: [Double]
  , gsResiduals    :: [Double]
  , gsLinkName     :: Text
  , gsXColDegs     :: [(Text, Int)]
  , gsSmoothData   :: Maybe (Text, SmoothData)
  , gsModelSelect  :: Maybe (WAICResult, LOOResult)  -- ^ 条件付き WAIC/LOO (--waic 時)
  } deriving (Show)

mkGLMMSummary
  :: Family
  -> LinkFn
  -> [(Text, Int)]
  -> Text
  -> Maybe (Text, SmoothData)
  -> GLMMResult
  -> GLMMSummary
mkGLMMSummary fam lnk colDegs grpCol mSmooth gr = GLMMSummary
  { gsModelType    = glmmTypeLabel fam lnk
  , gsFormula      = formulaText colDegs <> " | " <> grpCol
  , gsFixed        = zip (coeffLabels colDegs) (coeffList (glmmFixed gr))
  , gsR2           = rSquared1 (glmmFixed gr)
  , gsR2Label      = r2Label fam
  , gsGroupCol     = grpCol
  , gsRandVar      = glmmRandVar gr
  , gsResidVar     = glmmResidVar gr
  , gsICC          = glmmICC gr
  , gsBLUPs        = zip (V.toList (glmmGroups gr)) (V.toList (glmmBLUPs gr))
  , gsFitted       = fittedList (glmmFixed gr)
  , gsResiduals    = LA.toList (residualsV (glmmFixed gr))
  , gsLinkName     = linkName lnk
  , gsXColDegs     = colDegs
  , gsSmoothData   = mSmooth
  , gsModelSelect  = Nothing
  }

-- | GP の1カーネルのフィット結果。
data GPKernelFit = GPKernelFit
  { gkLabel    :: Text
  , gkKernel   :: Kernel
  , gkParams   :: GPParams
  , gkResult   :: GPResult
  , gkLML      :: Double
  , gkPredData :: GPPredData
  } deriving (Show)

-- | GP 回帰サマリー (複数カーネル比較)。
data GPFitSummary = GPFitSummary
  { gfKernelFits :: [GPKernelFit]   -- ^ LML 降順でソート済み
  , gfXCol       :: Text
  , gfYCol       :: Text
  , gfTrainXs    :: [Double]
  , gfTrainYs    :: [Double]
  } deriving (Show)

-- | HBM (ベイズ回帰) のサマリー。
-- 内部に LM 互換の 'FitSummary' を持ち、加えて DAG と MCMC チェーンを保持する。
data HBMRegSummary = HBMRegSummary
  { hbmsFit           :: FitSummary    -- ^ 回帰スタイルの基本サマリー
                                       -- (係数 = 事後平均、smoothData = 信用区間付き予測曲線)
  , hbmsModelGraph    :: ModelGraph    -- ^ Mermaid DAG (モデル概要に表示)
  , hbmsChain         :: Chain         -- ^ MCMC チェーン (回帰結果に診断プロット表示)
  , hbmsParams        :: [Text]        -- ^ 全潜在変数名 (alpha/beta/sigma 等)
  , hbmsPosteriorRows :: [(Text, Double, Double, Double, Double)]
                                       -- ^ (name, mean, sd, q025, q975)
  } deriving (Show)

-- | モデルフィットの統一型。
data ModelFit
  = RegFit   FitSummary
  | MixFit   GLMMSummary
  | GPFit    GPFitSummary
  | HBMFit   HBMRegSummary
  | NoRegFit

-- | 名前付き Vega-Lite プロット。
data NamedPlot = NamedPlot
  { npName :: Text
  , npTitle :: Text
  , npSpec  :: VegaLite
  }

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

writeAnalysisReport
  :: FilePath
  -> AnalysisReportConfig
  -> DataFrame
  -> [Text]
  -> Text
  -> ModelFit
  -> [NamedPlot]
  -> IO ()
writeAnalysisReport path cfg df xCols yCol fit plots =
  TIO.writeFile path (buildHtml cfg df xCols yCol fit plots)

-- | レポートに含まれる Vega-Lite プロットを個別ファイルとして書き出す。
--
-- 各 'NamedPlot' を @<prefix>-<idx>-<name>.<ext>@ に出力する。
-- HTML 専用要素 (DAG, 事後分布表, 対話的予測 UI, ヒストグラム JS) は
-- vl-convert で変換できないためスキップする。
--
-- 戻り値: 書き出したファイルパスのリスト。
writeAnalysisReportPlots
  :: FilePath        -- ^ ファイル名プレフィックス (拡張子なし)
  -> OutputFormat    -- ^ PNG / SVG (HTML は 'writeAnalysisReport' を使うこと)
  -> [NamedPlot]
  -> IO [FilePath]
writeAnalysisReportPlots prefix fmt plots = do
  let ext = case fmt of
        PNG  -> ".png"
        SVG  -> ".svg"
        HTML -> ".html"
      paths = [ prefix <> "-" <> show (i :: Int) <> "-"
                <> sanitize (T.unpack (npName p)) <> ext
              | (i, p) <- zip [1..] plots ]
  mapM_ (\(path, p) -> writeSpec fmt path (npSpec p))
        (zip paths plots)
  return paths
  where
    sanitize = map (\c -> if c `elem` ("/\\: " :: String) then '_' else c)

-- ---------------------------------------------------------------------------
-- HTML builder
-- ---------------------------------------------------------------------------

buildHtml :: AnalysisReportConfig -> DataFrame -> [Text] -> Text -> ModelFit -> [NamedPlot] -> Text
buildHtml cfg df xCols yCol fit plots = T.unlines $
  [ "<!DOCTYPE html>"
  , "<html lang=\"ja\">"
  , "<head>"
  , "  <meta charset=\"utf-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "  <title>" <> arcTitle cfg <> "</title>"
  , "  <script>" <> vegaJS      <> "</script>"
  , "  <script>" <> vegaLiteJS  <> "</script>"
  , "  <script>" <> vegaEmbedJS <> "</script>"
  , if isHBMFit fit
      then "  <script src=\"https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js\"></script>"
      else ""
  , "  <style>" , reportCss , "  </style>"
  , "</head>"
  , "<body>"
  , navBar cfg fit
  , "<main>"
  , dataSummarySection df xCols yCol
  , modelSection fit
  , resultsSection fit plots
  ] ++
  predictionSection df xCols yCol fit ++
  [ appendixSection fit
  , "</main>"
  , "<script>"
  , if isHBMFit fit
      then "mermaid.initialize({ startOnLoad: true, theme: 'default' });"
      else ""
  , embedScript plots
  , gpVegaEmbedJS fit
  , columnDataJS df xCols yCol
  , predChartSpecJS fit xCols yCol df
  , gpModelsDataJS fit
  , predJS fit
  , histogramInitJS (xCols ++ [yCol])
  , gpTabSwitchJS fit
  , smoothScrollScript
  , "</script>"
  , "</body>"
  , "</html>"
  ]

-- ---------------------------------------------------------------------------
-- Nav bar
-- ---------------------------------------------------------------------------

navBar :: AnalysisReportConfig -> ModelFit -> Text
navBar cfg fit = T.unlines
  [ "<nav>"
  , "  <h1>&#128202; " <> arcTitle cfg <> "</h1>"
  , "  <a class=\"nav-link\" href=\"#sec-data\">データ</a>"
  , "  <a class=\"nav-link\" href=\"#sec-model\">" <> modelNavLabel fit <> "</a>"
  , "  <a class=\"nav-link\" href=\"#sec-results\">結果</a>"
  , if hasPrediction fit
      then "  <a class=\"nav-link\" href=\"#sec-predict\">予測</a>"
      else ""
  , "  <a class=\"nav-link\" href=\"#sec-appendix\">付録</a>"
  , "</nav>"
  ]

modelNavLabel :: ModelFit -> Text
modelNavLabel (GPFit _)  = "モデル比較"
modelNavLabel (HBMFit _) = "モデル"
modelNavLabel _          = "モデル"

hasPrediction :: ModelFit -> Bool
hasPrediction NoRegFit = False
hasPrediction _        = True

isGPFit :: ModelFit -> Bool
isGPFit (GPFit _) = True
isGPFit _         = False

isHBMFit :: ModelFit -> Bool
isHBMFit (HBMFit _) = True
isHBMFit _          = False

-- ---------------------------------------------------------------------------
-- Section 1: Data summary with histograms
-- ---------------------------------------------------------------------------

dataSummarySection :: DataFrame -> [Text] -> Text -> Text
dataSummarySection df xCols yCol = T.unlines $
  [ "<section id=\"sec-data\">"
  , "  <h2><span class=\"sec-icon\">&#128202;</span> 1. データの特性</h2>"
  , "  <div class=\"stat-grid\" style=\"margin-bottom:20px\">"
  , statBox "N (サンプル数)" (T.pack (show (numRows df))) False
  , "  </div>"
  , "  <div class=\"col-cards\">"
  ] ++
  concatMap (colCard df "説明変数") xCols ++
  colCard df "目的変数" yCol ++
  [ "  </div>"
  , "</section>"
  ]

colCard :: DataFrame -> Text -> Text -> [Text]
colCard df role col =
  case getNumeric col df of
    Nothing -> []
    Just v  ->
      let sorted  = sort (V.toList v)
          n       = length sorted
          nD      = fromIntegral n :: Double
          mn      = head sorted
          mx      = last sorted
          mu      = sum sorted / nD
          sd      = sqrt (sum (map (\x -> (x - mu)^(2::Int)) sorted) / nD)
          med     = if odd n
                      then sorted !! (n `div` 2)
                      else (sorted !! (n `div` 2 - 1) + sorted !! (n `div` 2)) / 2
          skew    = if sd < 1e-12 then 0
                    else sum (map (\x -> ((x - mu)/sd)^(3::Int)) sorted) / nD
          histId  = "hist-" <> col
      in [ "    <div class=\"col-card\">"
         , "      <div class=\"col-card-title\">"
         , "        <span class=\"col-role\">" <> role <> "</span>"
         , "        <span class=\"col-name\">" <> col <> "</span>"
         , "      </div>"
         , "      <div class=\"col-card-body\">"
         , "        <div class=\"col-hist\"><div id=\"" <> histId <> "\"></div></div>"
         , "        <div class=\"col-stats-mini\">"
         , colStatRow "N" (T.pack (show n))
         , colStatRow "最小値" (fmt4 mn)
         , colStatRow "最大値" (fmt4 mx)
         , colStatRow "平均" (fmt4 mu)
         , colStatRow "中央値" (fmt4 med)
         , colStatRow "標準偏差" (fmt4 sd)
         , colStatRow "歪度" (fmt4 skew)
         , "        </div>"
         , "      </div>"
         , "    </div>"
         ]

colStatRow :: Text -> Text -> Text
colStatRow k v =
  "          <div class=\"col-stat-row\"><span class=\"sk\">" <> k
  <> "</span><span class=\"sv\">" <> v <> "</span></div>"

-- ---------------------------------------------------------------------------
-- Section 2: Model overview
-- ---------------------------------------------------------------------------

modelSection :: ModelFit -> Text
modelSection NoRegFit = T.unlines
  [ "<section id=\"sec-model\">"
  , "  <h2><span class=\"sec-icon\">&#9878;</span> 2. モデル概要</h2>"
  , "  <p>回帰モデルなし (散布図のみ)</p>"
  , "</section>"
  ]
modelSection (RegFit fs) = T.unlines $
  [ "<section id=\"sec-model\">"
  , "  <h2><span class=\"sec-icon\">&#9878;</span> 2. モデル概要</h2>"
  , "  <div class=\"info-grid\">"
  , infoBox "モデル種別" (fsModelType fs)
  , infoBox "回帰式" (fsFormula fs)
  , infoBox "リンク関数" (fsLinkName fs)
  , "  </div>"
  ] ++ waicLooSection (fsModelSelect fs) ++
  [ "</section>"
  ]
modelSection (HBMFit hs) =
  let fs = hbmsFit hs
  in T.unlines $
    [ "<section id=\"sec-model\">"
    , "  <h2><span class=\"sec-icon\">&#9878;</span> 2. モデル概要</h2>"
    , "  <div class=\"info-grid\">"
    , infoBox "モデル種別" (fsModelType fs)
    , infoBox "回帰式" (fsFormula fs)
    , infoBox "尤度" (fsLinkName fs)
    , "  </div>"
    , "  <h3 style=\"margin-top:20px\">モデル DAG</h3>"
    , "  <p class=\"sec-desc\" style=\"font-size:.85em;color:#555\">"
    , "    依存グラフは <code>extractDeps</code> (Track 型による多相 DSL の解釈) で自動抽出。"
    , "  </p>"
    , "  <div class=\"mermaid-wrap\">"
    , "    <pre class=\"mermaid\">"
    , buildMermaid (hbmsModelGraph hs)
    , "    </pre>"
    , "  </div>"
    , "  <div class=\"legend\" style=\"margin-top:8px;font-size:.82em;color:#666\">"
    , "    <span style=\"display:inline-block;width:11px;height:11px;background:#4C72B0;border-radius:2px;margin-right:4px;vertical-align:middle\"></span>latent &nbsp;&nbsp;"
    , "    <span style=\"display:inline-block;width:11px;height:11px;background:#DD8844;border-radius:2px;margin-right:4px;vertical-align:middle\"></span>observed"
    , "  </div>"
    , "</section>"
    ]
modelSection (MixFit gs) = T.unlines $
  [ "<section id=\"sec-model\">"
  , "  <h2><span class=\"sec-icon\">&#9878;</span> 2. モデル概要</h2>"
  , "  <div class=\"info-grid\">"
  , infoBox "モデル種別" (gsModelType gs)
  , infoBox "固定効果式" (gsFormula gs)
  , infoBox "グループ変数" (gsGroupCol gs)
  , infoBox "リンク関数" (gsLinkName gs)
  , "  </div>"
  , "  <h3>分散成分</h3>"
  , "  <div class=\"stat-grid\">"
  , statBox ("σ²_u (" <> gsGroupCol gs <> ")") (fmt4 (gsRandVar gs)) False
  , statBox "σ² (残差)" (fmt4 (gsResidVar gs)) False
  , statBox "ICC" (fmt4 (gsICC gs)) False
  , "  </div>"
  ] ++ waicLooSection (gsModelSelect gs) ++
  [ "  <h3>BLUP (グループ別ランダム切片)</h3>"
  , blupTable (gsBLUPs gs)
  , "</section>"
  ]
modelSection (GPFit gf) = T.unlines $
  [ "<section id=\"sec-model\">"
  , "  <h2><span class=\"sec-icon\">&#9878;</span> 2. モデル比較</h2>"
  , "  <div class=\"info-grid\">"
  , infoBox "モデル種別" "GP Regression"
  , infoBox "説明変数" (gfXCol gf)
  , infoBox "目的変数" (gfYCol gf)
  , infoBox "比較カーネル数" (T.pack (show (length (gfKernelFits gf))))
  , "  </div>"
  , "  <p style=\"font-size:.88em;color:#666;margin-bottom:14px\">"
  , "    対数周辺尤度 (LML) が高いほどデータへの適合が良い。ハイパーパラメータは自動最適化済み。"
  , "  </p>"
  , "  <table>"
  , "    <thead><tr>"
  , "      <th>カーネル</th><th style=\"text-align:right\">ℓ</th>"
  , "      <th style=\"text-align:right\">σ_f</th><th style=\"text-align:right\">σ_n</th>"
  , "      <th style=\"text-align:right\">p</th><th style=\"text-align:right\">LML ↑</th>"
  , "      <th style=\"text-align:right\">順位</th>"
  , "    </tr></thead>"
  , "    <tbody>"
  ] ++
  zipWith (gpModelRow (maximum (map gkLML (gfKernelFits gf)))) [1..] (gfKernelFits gf) ++
  [ "    </tbody>"
  , "  </table>"
  , "  <p style=\"margin-top:12px;font-size:.82em;color:#888\">"
  , "    LML = log p(y | X, θ)。データ適合とモデル複雑度ペナルティのバランス。"
  , "  </p>"
  , "</section>"
  ]

gpModelRow :: Double -> Int -> GPKernelFit -> Text
gpModelRow bestLML rank fit =
  let isBest = gkLML fit == bestLML
      style  = if isBest then " style=\"background:#f0faf0;font-weight:600\"" else ""
      hasPer = gkKernel fit == Periodic
      badge  = if isBest then " <span style=\"background:#e8f4e8;color:#2e7d32;padding:1px 7px;border-radius:10px;font-size:.78em\">Best</span>" else ""
  in T.unlines
       [ "      <tr" <> style <> ">"
       , "        <td>" <> gkLabel fit <> badge <> "</td>"
       , "        <td style=\"text-align:right\">" <> fmt4 (gpLengthScale (gkParams fit)) <> "</td>"
       , "        <td style=\"text-align:right\">" <> fmt4 (sqrt (gpSignalVar (gkParams fit))) <> "</td>"
       , "        <td style=\"text-align:right\">" <> fmt4 (sqrt (gpNoiseVar (gkParams fit))) <> "</td>"
       , "        <td style=\"text-align:right\">" <> (if hasPer then fmt4 (gpPeriod (gkParams fit)) else "—") <> "</td>"
       , "        <td style=\"text-align:right\">" <> fmt4 (gkLML fit) <> "</td>"
       , "        <td style=\"text-align:right\">#" <> T.pack (show (rank :: Int)) <> "</td>"
       , "      </tr>"
       ]

-- | WAIC/LOO-CV の結果をスタットボックスで表示する HTML フラグメント。
waicLooSection :: Maybe (WAICResult, LOOResult) -> [Text]
waicLooSection Nothing = []
waicLooSection (Just (w, l)) =
  let kBad = looKHatBad l
      kAlert = kBad > 0
  in [ "  <h3 style=\"margin-top:20px\">モデル比較指標 (WAIC / LOO-CV)</h3>"
     , "  <div class=\"stat-grid\">"
     , statBox "WAIC ↓" (fmt4 (waicValue w)) False
     , statBox "LOO ↓"  (fmt4 (looValue l))  False
     , statBox "p_WAIC" (fmt4 (waicPwaic w)) False
     , statBox "LOO SE" (fmt4 (looSE l))     False
     , statBox ("k̂>0.7") (T.pack (show kBad) <> "件") kAlert
     , "  </div>"
     , "  <p style=\"font-size:.84em;color:#666;margin-top:8px\">"
     , "    WAIC/LOO は小さいほど良い。p_WAIC = 実効パラメータ数。"
     , if kAlert
         then "k̂&gt;0.7 の観測値が多い場合は LOO 推定の信頼性が低下する。"
         else "k̂&gt;0.7 の観測値はなく LOO は安定。"
     , "  </p>"
     ]

blupTable :: [(Text, Double)] -> Text
blupTable blups = T.unlines $
  [ "  <table style=\"max-width:400px\">"
  , "    <thead><tr><th>グループ</th><th>BLUP (û_j)</th></tr></thead>"
  , "    <tbody>"
  ] ++ map row blups ++
  [ "    </tbody>"
  , "  </table>"
  ]
  where
    row (g, v) = "      <tr><td>" <> g <> "</td><td>" <> fmtSigned v <> "</td></tr>"

-- ---------------------------------------------------------------------------
-- Section 3: Regression results
-- ---------------------------------------------------------------------------

resultsSection :: ModelFit -> [NamedPlot] -> Text
resultsSection (GPFit gf) _ = T.unlines $
  [ "<section id=\"sec-results\">"
  , "  <h2><span class=\"sec-icon\">&#128200;</span> 3. 回帰結果</h2>"
  , "  <p style=\"font-size:.88em;color:#666;margin-bottom:14px\">青い帯 = 平均 ± 2σ (≈95% 信用区間)。黒点 = 訓練データ。</p>"
  , "  <div class=\"tab-bar\">"
  ] ++
  zipWith (gpTabBtn (gfKernelFits gf)) [0..] (gfKernelFits gf) ++
  [ "  </div>" ] ++
  concatMap (gpTabContent gf) (zip [0..] (gfKernelFits gf)) ++
  [ "</section>" ]
resultsSection fit plots = T.unlines $
  [ "<section id=\"sec-results\">"
  , "  <h2><span class=\"sec-icon\">&#128200;</span> 3. 回帰結果</h2>"
  ] ++
  fitTable fit ++
  [ residualSummary fit ] ++
  concatMap plotDiv (zip [0::Int ..] plots) ++
  [ "</section>" ]

gpTabBtn :: [GPKernelFit] -> Int -> GPKernelFit -> Text
gpTabBtn fits i fit =
  let bestLML = maximum (map gkLML fits)
      star    = if gkLML fit == bestLML then " &#11088;" else ""
      active  = if i == 0 then " active" else ""
  in "  <button class=\"tab-btn" <> active <> "\" onclick=\"showGPTab(" <> T.pack (show i) <> ")\">"
     <> gkLabel fit <> star <> "</button>"

gpTabContent :: GPFitSummary -> (Int, GPKernelFit) -> [Text]
gpTabContent gf (i, fit) =
  let active = if i == 0 then " active" else ""
      pCfg   = PlotConfig
                 { plotTitle  = gkLabel fit <> " — GP Regression"
                 , plotWidth  = 700
                 , plotHeight = 320
                 }
      spec   = gpPlot pCfg (gfXCol gf) (gfYCol gf)
                 (zip (gfTrainXs gf) (gfTrainYs gf)) (gkResult fit)
      json   = specJson spec
      hasPer = gkKernel fit == Periodic
  in [ "  <div id=\"gp-tab-" <> T.pack (show i) <> "\" class=\"tab-content" <> active <> "\">"
     , "    <div class=\"vl-wrap\"><div id=\"vl-gp-" <> T.pack (show i) <> "\"></div></div>"
     , "    <script>window.__vlGP" <> T.pack (show i) <> " = " <> json <> ";</script>"
     , "    <div style=\"margin-top:12px;background:#f7f9fc;border-radius:8px;padding:10px 16px;"
     , "         display:flex;gap:20px;flex-wrap:wrap;font-size:.85em;\">"
     , "      <span><b>カーネル:</b> " <> gkLabel fit <> "</span>"
     , "      <span><b>ℓ =</b> " <> fmt4 (gpLengthScale (gkParams fit)) <> "</span>"
     , "      <span><b>σ_f =</b> " <> fmt4 (sqrt (gpSignalVar (gkParams fit))) <> "</span>"
     , "      <span><b>σ_n =</b> " <> fmt4 (sqrt (gpNoiseVar (gkParams fit))) <> "</span>"
     , if hasPer then "      <span><b>p =</b> " <> fmt4 (gpPeriod (gkParams fit)) <> "</span>"
                 else ""
     , "      <span style=\"margin-left:auto;color:#888\"><b>LML =</b> " <> fmt4 (gkLML fit) <> "</span>"
     , "    </div>"
     , "  </div>"
     ]

fitTable :: ModelFit -> [Text]
fitTable NoRegFit = []
fitTable (RegFit fs) =
  [ "  <h3>係数</h3>"
  , "  <table style=\"max-width:600px\">"
  , "    <thead><tr><th>パラメータ</th><th>推定値</th></tr></thead>"
  , "    <tbody>"
  ] ++
  map (\(l,v) -> "      <tr><td>" <> l <> "</td><td>" <> fmtSigned v <> "</td></tr>")
      (fsCoeffs fs) ++
  [ "    </tbody>"
  , "  </table>"
  , "  <div class=\"stat-grid\" style=\"margin-top:14px\">"
  , statBox (fsR2Label fs) (fmt4 (fsR2 fs)) True
  , "  </div>"
  ]
fitTable (HBMFit hs) =
  let fs = hbmsFit hs
      ch = hbmsChain hs
      total    = chainTotalOf ch
      accepted = chainAcceptedOf ch
      acceptR  = if total == 0 then 0
                 else fromIntegral accepted / fromIntegral total :: Double
      nSamp    = chainNSamples ch
  in [ "  <h3>事後分布サマリー</h3>"
     , "  <p class=\"sec-desc\" style=\"font-size:.85em;color:#555\">"
     , "    各潜在変数の事後平均・標準偏差・95% 信用区間 (2.5% / 97.5% 分位点)。"
     , "  </p>"
     , "  <table style=\"max-width:760px\">"
     , "    <thead><tr><th>パラメータ</th><th>事後平均</th>"
       <> "<th>事後 SD</th><th>2.5%</th><th>97.5%</th></tr></thead>"
     , "    <tbody>"
     ] ++
     map posteriorRowHtml (hbmsPosteriorRows hs) ++
     [ "    </tbody>"
     , "  </table>"
     , "  <div class=\"stat-grid\" style=\"margin-top:14px\">"
     , statBox (fsR2Label fs) (fmt4 (fsR2 fs)) True
     , statBox "サンプル数" (T.pack (show nSamp)) False
     , statBox "受容率" (fmt1 (acceptR * 100) <> "%") False
     , "  </div>"
     ]
  where
    posteriorRowHtml (n, m, sd_, lo, hi) =
      "      <tr><td>" <> n <> "</td>"
      <> "<td>" <> fmtSigned m <> "</td>"
      <> "<td>" <> fmt4 sd_ <> "</td>"
      <> "<td>" <> fmtSigned lo <> "</td>"
      <> "<td>" <> fmtSigned hi <> "</td></tr>"
fitTable (MixFit gs) =
  [ "  <h3>固定効果係数</h3>"
  , "  <table style=\"max-width:600px\">"
  , "    <thead><tr><th>パラメータ</th><th>推定値</th></tr></thead>"
  , "    <tbody>"
  ] ++
  map (\(l,v) -> "      <tr><td>" <> l <> "</td><td>" <> fmtSigned v <> "</td></tr>")
      (gsFixed gs) ++
  [ "    </tbody>"
  , "  </table>"
  , "  <div class=\"stat-grid\" style=\"margin-top:14px\">"
  , statBox (gsR2Label gs) (fmt4 (gsR2 gs)) True
  , statBox "ICC" (fmt4 (gsICC gs)) False
  , "  </div>"
  ]

chainNSamples :: Chain -> Int
chainNSamples = length . chainSamples

chainTotalOf :: Chain -> Int
chainTotalOf = chainTotal

chainAcceptedOf :: Chain -> Int
chainAcceptedOf = chainAccepted

residualSummary :: ModelFit -> Text
residualSummary fit =
  let resids = case fit of
                 RegFit  fs -> fsResiduals fs
                 MixFit  gs -> gsResiduals gs
                 HBMFit  hs -> fsResiduals (hbmsFit hs)
                 NoRegFit   -> []
      n      = fromIntegral (length resids) :: Double
      rmse   = if n == 0 then 0 else sqrt (sum (map (^(2::Int)) resids) / n)
      mx     = if null resids then 0 else maximum (map abs resids)
  in if null resids then ""
     else T.unlines
       [ "  <h3>残差サマリー</h3>"
       , "  <div class=\"stat-grid\">"
       , statBox "RMSE"       (fmt4 rmse) False
       , statBox "最大絶対残差" (fmt4 mx)   False
       , "  </div>"
       ]

plotDiv :: (Int, NamedPlot) -> [Text]
plotDiv (i, np) =
  let divId = npName np <> "-" <> T.pack (show i)
  in [ "  <h3>" <> npTitle np <> "</h3>"
     , "  <div class=\"vl-wrap\"><div id=\"" <> divId <> "\"></div></div>"
     , "  <script>window.__vl_" <> T.pack (show i) <> " = " <> specJson (npSpec np) <> ";</script>"
     ]

-- ---------------------------------------------------------------------------
-- Section 4: Interactive prediction
-- ---------------------------------------------------------------------------

predictionSection :: DataFrame -> [Text] -> Text -> ModelFit -> [Text]
predictionSection _ _ _ NoRegFit = []
predictionSection _ _ _ (GPFit gf) = gpPredictionSection gf
predictionSection df xCols yCol fit =
  let -- データ範囲を ±50% 拡張してスライダーに使う
      xRanges = [ (col, mn, mx, smin, smax)
                | col <- xCols
                , Just v <- [getNumeric col df]
                , let mn   = V.minimum v
                , let mx   = V.maximum v
                , let ext  = max 1e-8 (mx - mn) * 0.5
                , let smin = mn - ext
                , let smax = mx + ext
                ]
      groups = case fit of
                 MixFit gs -> map fst (gsBLUPs gs)
                 _         -> []
      hasSingle = length xCols == 1 && case smoothDataFor fit of { Just _ -> True; Nothing -> False }
  in [ "<section id=\"sec-predict\">"
     , "  <h2><span class=\"sec-icon\">&#127919;</span> 4. 対話的予測</h2>"
     , "  <p class=\"sec-desc\">"
     , "    スライダーまたは入力欄で説明変数の値を変えると、"
     , "    回帰曲線上の予測点がリアルタイムで移動します。"
     , "    スライダーはデータ範囲の ±50% まで外挿できます。"
     , "  </p>"
     , "  <div class=\"predict-layout\">"
     , "    <div class=\"predict-left\">"
     , "      <div class=\"predict-controls\">"
     ] ++
     concatMap xSlider xRanges ++
     (if null groups then []
      else [ "        <div class=\"slider-row\">"
           , "          <label>グループ (" <> grpCol fit <> "):</label>"
           , "          <select id=\"pred-group\" onchange=\"updatePrediction()\">"
           , T.concat [ "            <option value=\"" <> g <> "\">" <> g <> "</option>\n"
                      | g <- groups ]
           , "          </select>"
           , "        </div>"
           ]) ++
     [ "      </div>"
     , "      <div class=\"predict-output\">"
     , "        <div class=\"pred-box mean-box\">"
     , "          <div class=\"plbl\">予測値 (" <> yCol <> ")"
     , "            <span id=\"extrap-warn\" class=\"extrap-badge\" style=\"display:none\">外挿</span>"
     , "          </div>"
     , "          <div class=\"pval\" id=\"pred-y\">—</div>"
     , "          <div class=\"psub\">g⁻¹(η)</div>"
     , "        </div>"
     , "        <div class=\"pred-box\">"
     , "          <div class=\"plbl\">線形予測子 (η)</div>"
     , "          <div class=\"pval\" id=\"pred-eta\">—</div>"
     , "          <div class=\"psub\">Xβ</div>"
     , "        </div>"
     , if hasSingle
         then "        <div class=\"pred-box ci-box\">"
              <> "<div class=\"plbl\" id=\"ci-lbl\">95% CI</div>"
              <> "<div class=\"pval\" id=\"pred-ci-lo\">—</div>"
              <> "<div class=\"psub\" id=\"pred-ci-hi\">—</div>"
              <> "</div>"
         else ""
     , "      </div>"
     , "    </div>"
     , if hasSingle
         then "    <div class=\"predict-chart\"><div id=\"pred-chart\"></div></div>"
         else ""
     , "  </div>"
     , "</section>"
     ]
  where
    grpCol (MixFit gs) = gsGroupCol gs
    grpCol _           = ""

gpPredictionSection :: GPFitSummary -> [Text]
gpPredictionSection gf =
  let xs    = gfTrainXs gf
      xMin  = minimum xs
      xMax  = maximum xs
      ext   = max 1e-8 (xMax - xMin) * 0.5
      smin  = xMin - ext
      smax  = xMax + ext
      step  = (smax - smin) / 500
      mid   = (smin + smax) / 2
      xCol  = gfXCol gf
      yCol  = gfYCol gf
  in [ "<section id=\"sec-predict\">"
     , "  <h2><span class=\"sec-icon\">&#127919;</span> 4. 対話的予測</h2>"
     , "  <p class=\"sec-desc\">スライダーまたは入力欄で x 値を変えると、選択したカーネルの GP 事後平均と信用区間をリアルタイムで計算します。曲線はベストカーネルを表示。</p>"
     , "  <div class=\"predict-layout\">"
     , "    <div class=\"predict-left\">"
     , "      <div class=\"predict-controls\">"
     , "        <div class=\"slider-row\">"
     , "          <label>カーネル:</label>"
     , "          <select id=\"pred-kernel\" onchange=\"updateGPPrediction()\">"
     , T.concat [ "            <option value=\"" <> T.pack (show i) <> "\">"
                  <> gkLabel fit
                  <> " (LML=" <> fmt4 (gkLML fit) <> ")"
                  <> "</option>\n"
                | (i, fit) <- zip [0 :: Int ..] (gfKernelFits gf) ]
     , "          </select>"
     , "        </div>"
     , "        <div class=\"slider-row\">"
     , "          <label>" <> xCol <> ":</label>"
     , "          <input type=\"range\" id=\"x-gp\""
     , "                 min=\"" <> fmtJS smin <> "\" max=\"" <> fmtJS smax <> "\""
     , "                 step=\"" <> fmtJS step <> "\" value=\"" <> fmtJS mid <> "\""
     , "                 oninput=\"syncGPSlider()\">"
     , "          <input type=\"number\" id=\"x-gp-num\""
     , "                 step=\"" <> fmtJS step <> "\" value=\"" <> fmtJS mid <> "\""
     , "                 onchange=\"syncGPNum()\">"
     , "        </div>"
     , "      </div>"
     , "      <div class=\"predict-output\">"
     , "        <div class=\"pred-box mean-box\">"
     , "          <div class=\"plbl\">事後平均 (" <> yCol <> ")"
     , "            <span id=\"gp-extrap-warn\" class=\"extrap-badge\" style=\"display:none\">外挿</span>"
     , "          </div>"
     , "          <div class=\"pval\" id=\"gp-pred-mean\">—</div>"
     , "          <div class=\"psub\">μ(x*)</div>"
     , "        </div>"
     , "        <div class=\"pred-box\">"
     , "          <div class=\"plbl\">標準偏差</div>"
     , "          <div class=\"pval\" id=\"gp-pred-std\">—</div>"
     , "          <div class=\"psub\">σ(x*)</div>"
     , "        </div>"
     , "        <div class=\"pred-box ci-box\">"
     , "          <div class=\"plbl\">95% 信用区間</div>"
     , "          <div class=\"pval\" id=\"gp-pred-lo\">—</div>"
     , "          <div class=\"psub\" id=\"gp-pred-hi\">—</div>"
     , "        </div>"
     , "      </div>"
     , "    </div>"
     , "    <div class=\"predict-chart\"><div id=\"pred-chart\"></div></div>"
     , "  </div>"
     , "</section>"
     ]

smoothDataFor :: ModelFit -> Maybe (Text, SmoothData)
smoothDataFor (RegFit fs) = fsSmoothData fs
smoothDataFor (MixFit gs) = gsSmoothData gs
smoothDataFor (HBMFit hs) = fsSmoothData (hbmsFit hs)
smoothDataFor NoRegFit    = Nothing

-- (col, data_min, data_max, slider_min, slider_max)
xSlider :: (Text, Double, Double, Double, Double) -> [Text]
xSlider (col, _mn, _mx, smin, smax) =
  let step = (smax - smin) / 500
      mid  = (smin + smax) / 2
      sid  = "x-" <> col
  in [ "        <div class=\"slider-row\">"
     , "          <label>" <> col <> ":</label>"
     , "          <input type=\"range\" id=\"" <> sid <> "\""
     , "                 min=\"" <> fmtJS smin <> "\" max=\"" <> fmtJS smax <> "\""
     , "                 step=\"" <> fmtJS step <> "\" value=\"" <> fmtJS mid <> "\""
     , "                 oninput=\"syncSlider('" <> col <> "')\">"
     , "          <input type=\"number\" id=\"x-num-" <> col <> "\""
     , "                 step=\"" <> fmtJS step <> "\" value=\"" <> fmtJS mid <> "\""
     , "                 onchange=\"syncNum('" <> col <> "')\">"
     , "        </div>"
     ]

-- ---------------------------------------------------------------------------
-- Section 5: Appendix
-- ---------------------------------------------------------------------------

appendixSection :: ModelFit -> Text
appendixSection fit = T.unlines
  [ "<section id=\"sec-appendix\">"
  , "  <h2><span class=\"sec-icon\">&#128218;</span> 5. 付録: モデルの原理</h2>"
  , appendixContent fit
  , "</section>"
  ]

appendixContent :: ModelFit -> Text
appendixContent NoRegFit = "  <p>回帰モデルなし。</p>"
appendixContent (RegFit fs) = T.unlines $
  [ "  <div class=\"appendix-block\">"
  , "    <h4>" <> fsModelType fs <> " モデル</h4>"
  , "    <p>一般化線形モデル (GLM) は線形予測子 η = Xβ をリンク関数 g で連結します:</p>"
  , "    <div class=\"formula\">g(E[y]) = β₀ + β₁x₁ + β₂x₁² + ...</div>"
  , "    <p>リンク関数 <b>" <> fsLinkName fs <> "</b> を使用しています。</p>"
  , "  </div>"
  , lmAppendix (fsLinkName fs)
  ] ++ waicLooAppendix (fsModelSelect fs)
appendixContent (MixFit gs) = T.unlines
  [ "  <div class=\"appendix-block\">"
  , "    <h4>" <> gsModelType gs <> " モデル</h4>"
  , "    <p>混合効果モデルはグループ固有のランダム切片 û_j を固定効果に加えます:</p>"
  , "    <div class=\"formula\">g(E[y_ij]) = β₀ + β₁x + ... + û_j,  û_j ~ N(0, σ²_u)</div>"
  , "    <p><b>ICC</b> = σ²_u / (σ²_u + σ²) = " <> fmt4 (gsICC gs) <> "</p>"
  , "  </div>"
  , lmAppendix (gsLinkName gs)
  ]
appendixContent (HBMFit hs) = T.unlines
  [ "  <div class=\"appendix-block\">"
  , "    <h4>" <> fsModelType (hbmsFit hs) <> "</h4>"
  , "    <p>ベイズ線形回帰では係数を点推定ではなく <b>事後分布</b> として推定します:</p>"
  , "    <div class=\"formula\">"
  , "      α ~ Normal(0, σ_α),&nbsp; β ~ Normal(0, σ_β),&nbsp; σ ~ Exponential(1)<br>"
  , "      y_i ~ Normal(α + β·x_i, σ)"
  , "    </div>"
  , "    <p>推論は NUTS (No-U-Turn Sampler, AD 勾配) で実行。"
  , "    各パラメータの 95% 信用区間 = 事後分布の 2.5%/97.5% 分位点。</p>"
  , "    <p>予測曲線の <b>信用区間バンド</b> は、グリッド点 x* に対して"
  , "    全事後サンプル (α^(s), β^(s)) で μ^(s) = α^(s) + β^(s)·x* を計算し、"
  , "    その分布の 2.5%/97.5% 分位点を取ったものです。</p>"
  , "  </div>"
  ]
appendixContent (GPFit gf) = T.unlines
  [ "  <div class=\"appendix-block\">"
  , "    <h4>ガウス過程 (Gaussian Process) とは</h4>"
  , "    <p>ガウス過程は関数に対する確率分布です。平均関数 m(x) とカーネル k(x,x') によって定義されます:</p>"
  , "    <div class=\"formula\">f(x) ~ GP( m(x), k(x, x') )</div>"
  , "    <p>訓練データ (X, y) を条件付けた事後分布:</p>"
  , "    <div class=\"formula\">"
  , "    μ(x*) = K(x*, X) · [K(X,X) + σ²_n I]⁻¹ · y<br>"
  , "    σ²(x*) = k(x*, x*) − K(x*, X) · [K(X,X) + σ²_n I]⁻¹ · K(X, x*)"
  , "    </div>"
  , "  </div>"
  , gpKernelAppendix gf
  , "  <div class=\"appendix-block\">"
  , "    <h4>対数周辺尤度 (LML) によるモデル選択</h4>"
  , "    <div class=\"formula\">log p(y|X,θ) = −½ yᵀ K⁻¹ y − ½ log|K| − n/2 · log(2π)</div>"
  , "    <p>LML はデータ適合度とモデル複雑度ペナルティのバランスを取ります。</p>"
  , "  </div>"
  ]

gpKernelAppendix :: GPFitSummary -> Text
gpKernelAppendix gf = T.unlines $
  [ "  <div class=\"appendix-block\">"
  , "    <h4>使用したカーネル関数</h4>"
  ] ++
  concatMap kernelDesc (map gkKernel (gfKernelFits gf)) ++
  [ "  </div>" ]
  where
    kernelDesc RBF =
      [ "    <p><b>RBF (二乗指数カーネル)</b></p>"
      , "    <div class=\"formula\">k(x, x') = σ²_f · exp( −(x−x')² / (2ℓ²) )</div>"
      ]
    kernelDesc Matern52 =
      [ "    <p><b>Matérn 5/2 カーネル</b></p>"
      , "    <div class=\"formula\">k(x, x') = σ²_f · (1 + √5·r/ℓ + 5r²/(3ℓ²)) · exp(−√5·r/ℓ)</div>"
      ]
    kernelDesc Periodic =
      [ "    <p><b>Periodic カーネル</b></p>"
      , "    <div class=\"formula\">k(x, x') = σ²_f · exp( −2 sin²(π|x−x'|/p) / ℓ² )</div>"
      ]

lmAppendix :: Text -> Text
lmAppendix link = T.unlines
  [ "  <div class=\"appendix-block\">"
  , "    <h4>リンク関数とその逆関数</h4>"
  , "    <table style=\"max-width:500px\">"
  , "      <thead><tr><th>リンク</th><th>g(μ)</th><th>g⁻¹(η) (予測値変換)</th></tr></thead>"
  , "      <tbody>"
  , "        <tr" <> markActive "identity" link <> "><td>identity</td><td>μ</td><td>η</td></tr>"
  , "        <tr" <> markActive "log"      link <> "><td>log</td><td>log(μ)</td><td>exp(η)</td></tr>"
  , "        <tr" <> markActive "logit"    link <> "><td>logit</td><td>log(μ/(1-μ))</td><td>1/(1+exp(-η))</td></tr>"
  , "        <tr" <> markActive "sqrt"     link <> "><td>sqrt</td><td>√μ</td><td>η²</td></tr>"
  , "      </tbody>"
  , "    </table>"
  , "  </div>"
  ]
  where
    markActive l cur = if l == cur then " style=\"background:#f0faf0;font-weight:600\"" else ""

waicLooAppendix :: Maybe (WAICResult, LOOResult) -> [Text]
waicLooAppendix Nothing = []
waicLooAppendix (Just _) =
  [ "  <div class=\"appendix-block\">"
  , "    <h4>WAIC と LOO-CV</h4>"
  , "    <p><b>WAIC</b> (Widely Applicable Information Criterion) は"
  , "    事後予測分布に基づくモデル比較指標です:</p>"
  , "    <div class=\"formula\">"
  , "    WAIC = −2 × (lppd − p_WAIC)<br>"
  , "    lppd = Σᵢ log E_θ[p(yᵢ|θ)]  (対数点予測密度)<br>"
  , "    p_WAIC = Σᵢ Var_θ[log p(yᵢ|θ)]  (実効パラメータ数)"
  , "    </div>"
  , "    <p><b>LOO-CV</b> (PSIS-LOO) は各観測を1つ除いた予測精度の推定値です。"
  , "    Pareto k̂ 診断: k̂ &lt; 0.5 = 良好、0.5–0.7 = 許容、&gt; 0.7 = 要注意。</p>"
  , "    <p>いずれも <b>値が小さいほど良い</b>。WAIC ≈ LOO であれば両者は一致。</p>"
  , "    <p>LM では flat prior の解析的事後分布からサンプリング。"
  , "    GLM では Laplace 近似 β ~ MVN(β̂, Fisher⁻¹) を使用。</p>"
  , "  </div>"
  ]

-- ---------------------------------------------------------------------------
-- JavaScript: embed main plots
-- ---------------------------------------------------------------------------

embedScript :: [NamedPlot] -> Text
embedScript plots = T.unlines
  [ "vegaEmbed('#" <> npName np <> "-" <> T.pack (show i)
    <> "', window.__vl_" <> T.pack (show i)
    <> ", {renderer:'canvas',actions:false}).catch(console.error);"
  | (i, np) <- zip [0::Int ..] plots
  ]

-- ---------------------------------------------------------------------------
-- JavaScript: column raw data (for histogram rendering)
-- ---------------------------------------------------------------------------

columnDataJS :: DataFrame -> [Text] -> Text -> Text
columnDataJS df xCols yCol = T.unlines
  [ "const columnData = {" <> entries <> "};"
  , "const xColNames  = " <> jsStrArray xCols <> ";"
  , "const yColName   = " <> jsStr yCol <> ";"
  ]
  where
    allCols = xCols ++ [yCol]
    entry c = case getNumeric c df of
      Nothing -> ""
      Just v  -> jsStr c <> ": " <> jsDoubleArray (V.toList v)
    entries = T.intercalate "," (filter (not . T.null) (map entry allCols))

-- ヒストグラムを動的に描画する JS。
--
-- ビン数は **Freedman-Diaconis 公式** で自動選択する:
--   bin width = 2 · IQR / n^(1/3)
--   k         = ceil((max − min) / bin width)
-- ロバスト (外れ値に強く) かつ N に応じて適切な粒度になる。
-- IQR=0 の場合は Sturges 公式 (k = ceil(log₂ n + 1)) にフォールバック。
-- いずれにせよ最終的に [5, 25] にクランプして極端を避ける。
histogramInitJS :: [Text] -> Text
histogramInitJS cols = T.unlines $
  [ "(function() {"
  , "  function chooseBins(vals) {"
  , "    const n = vals.length;"
  , "    if (n < 4) return 5;"
  , "    const sorted = [...vals].sort((a,b) => a-b);"
  , "    const q1   = sorted[Math.floor(n * 0.25)];"
  , "    const q3   = sorted[Math.floor(n * 0.75)];"
  , "    const iqr  = q3 - q1;"
  , "    const range = sorted[n-1] - sorted[0];"
  , "    let k;"
  , "    if (iqr > 0 && range > 0) {"
  , "      // Freedman-Diaconis"
  , "      const w = 2 * iqr / Math.pow(n, 1/3);"
  , "      k = Math.ceil(range / w);"
  , "    } else {"
  , "      // Sturges (フォールバック)"
  , "      k = Math.ceil(Math.log2(Math.max(2, n)) + 1);"
  , "    }"
  , "    return Math.max(5, Math.min(25, k));"
  , "  }"
  , "  function makeHistSpec(col, vals) {"
  , "    const k = chooseBins(vals);"
  , "    return {"
  , "      '$schema': 'https://vega.github.io/schema/vega-lite/v5.json',"
  , "      width: 240, height: 130,"
  , "      background: 'transparent',"
  , "      data: {values: vals.map(v => ({v}))},"
  , "      mark: {type:'bar',color:'#4472c4',cornerRadiusEnd:2,tooltip:true},"
  , "      encoding: {"
  , "        x: {field:'v', bin:{maxbins:k, nice:true}, type:'quantitative', axis:{title:col, labelFontSize:10}},"
  , "        y: {aggregate:'count', type:'quantitative', axis:{title:'度数', labelFontSize:10}}"
  , "      }"
  , "    };"
  , "  }"
  ] ++
  [ "  vegaEmbed('#hist-" <> col <> "', makeHistSpec(" <> jsStr col <> ", columnData[" <> jsStr col <> "] || []), {actions:false}).catch(console.error);"
  | col <- cols
  ] ++
  [ "})();" ]

-- ---------------------------------------------------------------------------
-- JavaScript: interactive prediction chart spec
-- ---------------------------------------------------------------------------

predChartSpecJS :: ModelFit -> [Text] -> Text -> DataFrame -> Text
predChartSpecJS (GPFit gf) _ _ _ =
  -- ベストカーネルの曲線でチャートを構築
  case gfKernelFits gf of
    [] -> "window.__pred_chart = null;"
    (best:_) ->
      let res        = gkResult best
          scatterData = jsonArr
            [ "{\"x\":" <> fmtJS x <> ",\"y\":" <> fmtJS y <> "}"
            | (x, y) <- zip (gfTrainXs gf) (gfTrainYs gf) ]
          curveData = jsonArr
            [ "{\"x\":" <> fmtJS x <> ",\"y\":" <> fmtJS y <> "}"
            | (x, y) <- zip (gpTestX res) (gpMean res) ]
          bandData = jsonArr
            [ "{\"x\":" <> fmtJS x <> ",\"lo\":" <> fmtJS lo <> ",\"hi\":" <> fmtJS hi <> "}"
            | (x, lo, hi) <- zip3 (gpTestX res) (gpLower res) (gpUpper res) ]
          xMin = minimum (gfTrainXs gf)
          xMax = maximum (gfTrainXs gf)
          boundsData = "[{\"x\":" <> fmtJS xMin <> "},{\"x\":" <> fmtJS xMax <> "}]"
          spec = buildPredChartJson (gfXCol gf) (gfYCol gf) scatterData curveData bandData boundsData True
      in T.unlines
           [ "window.__pred_chart = " <> spec <> ";"
           , "const gpDataXMin = " <> fmtJS xMin <> ";"
           , "const gpDataXMax = " <> fmtJS xMax <> ";"
           ]
  where jsonArr xs = "[" <> T.intercalate "," xs <> "]"
predChartSpecJS fit xCols yCol df =
  case (smoothDataFor fit, xCols) of
    (Just (xCol, sd), [_]) ->
      case (getNumeric xCol df, getNumeric yCol df) of
        (Just xVec, Just yVec) ->
          let scatterData = jsonArr
                [ "{\"x\":" <> fmtJS x <> ",\"y\":" <> fmtJS y <> "}"
                | (x, y) <- zip (V.toList xVec) (V.toList yVec) ]
              curveData = jsonArr
                [ "{\"x\":" <> fmtJS x <> ",\"y\":" <> fmtJS y <> "}"
                | (x, y) <- zip (sdXs sd) (sdYs sd) ]
              bandData = if sdHasBand sd
                then jsonArr
                  [ "{\"x\":" <> fmtJS x <> ",\"lo\":" <> fmtJS lo <> ",\"hi\":" <> fmtJS hi <> "}"
                  | (x, lo, hi) <- zip3 (sdXs sd) (sdLower sd) (sdUpper sd) ]
                else "[]"
              hasBandJS  = if sdHasBand sd then "true" else "false"
              smoothLoJS = jsDoubleArray (sdLower sd)
              smoothHiJS = jsDoubleArray (sdUpper sd)
              smoothXsJS = jsDoubleArray (sdXs sd)
              dataXMin   = V.minimum xVec
              dataXMax   = V.maximum xVec
              boundsData = "[{\"x\":" <> fmtJS dataXMin <> "},{\"x\":" <> fmtJS dataXMax <> "}]"
              spec       = buildPredChartJson xCol yCol scatterData curveData bandData boundsData (sdHasBand sd)
          in T.unlines
               [ "window.__pred_chart = " <> spec <> ";"
               , "const predXCol      = " <> jsStr xCol <> ";"
               , "const smoothHasBand = " <> hasBandJS <> ";"
               , "const smoothXs = " <> smoothXsJS <> ";"
               , "const smoothLo = " <> smoothLoJS <> ";"
               , "const smoothHi = " <> smoothHiJS <> ";"
               , "const dataXMin = " <> fmtJS dataXMin <> ";"
               , "const dataXMax = " <> fmtJS dataXMax <> ";"
               ]
        _ -> "window.__pred_chart = null;"
    _ -> "window.__pred_chart = null;"
  where
    jsonArr xs = "[" <> T.intercalate "," xs <> "]"

buildPredChartJson :: Text -> Text -> Text -> Text -> Text -> Text -> Bool -> Text
buildPredChartJson xCol yCol scatterData curveData bandData boundsData hasBand = T.unlines
  [ "{"
  , "  \"$schema\": \"https://vega.github.io/schema/vega-lite/v5.json\","
  , "  \"width\": 480, \"height\": 300,"
  , "  \"datasets\": {"
  , "    \"scatter\": " <> scatterData <> ","
  , "    \"curve\":   " <> curveData <> ","
  , "    \"band\":    " <> bandData <> ","
  , "    \"data_bounds\": " <> boundsData <> ","
  , "    \"pred_point\": [],"
  , "    \"pred_ci\":    []"
  , "  },"
  , "  \"layer\": ["
  , if hasBand then bandLayer else ""
  , "    {"
  , "      \"data\": {\"name\": \"curve\"},"
  , "      \"mark\": {\"type\": \"line\", \"color\": \"#2a7dbc\", \"strokeWidth\": 2.5},"
  , "      \"encoding\": {"
  , "        \"x\": {\"field\": \"x\", \"type\": \"quantitative\", \"axis\": {\"title\": " <> jsStr xCol <> "}},"
  , "        \"y\": {\"field\": \"y\", \"type\": \"quantitative\", \"axis\": {\"title\": " <> jsStr yCol <> "}}"
  , "      }"
  , "    },"
  , "    {"
  , "      \"data\": {\"name\": \"scatter\"},"
  , "      \"mark\": {\"type\": \"point\", \"opacity\": 0.55, \"color\": \"#555\", \"size\": 50},"
  , "      \"encoding\": {"
  , "        \"x\": {\"field\": \"x\", \"type\": \"quantitative\"},"
  , "        \"y\": {\"field\": \"y\", \"type\": \"quantitative\"}"
  , "      }"
  , "    },"
  -- データ範囲境界の縦線 (外挿域の視覚的インジケーター)
  , "    {"
  , "      \"data\": {\"name\": \"data_bounds\"},"
  , "      \"mark\": {\"type\": \"rule\", \"color\": \"#bbb\", \"strokeDash\": [5,4], \"strokeWidth\": 1},"
  , "      \"encoding\": {\"x\": {\"field\": \"x\", \"type\": \"quantitative\"}}"
  , "    },"
  , if hasBand then predCILayer else ""
  , "    {"
  , "      \"data\": {\"name\": \"pred_point\"},"
  , "      \"mark\": {\"type\": \"point\", \"color\": \"#e74c3c\", \"size\": 180,"
  , "                \"filled\": true, \"stroke\": \"white\", \"strokeWidth\": 1.5},"
  , "      \"encoding\": {"
  , "        \"x\": {\"field\": \"x\", \"type\": \"quantitative\"},"
  , "        \"y\": {\"field\": \"y\", \"type\": \"quantitative\"},"
  , "        \"tooltip\": [{\"field\": \"x\", \"type\": \"quantitative\", \"title\": " <> jsStr xCol <> "},"
  , "                      {\"field\": \"y\", \"type\": \"quantitative\", \"title\": " <> jsStr yCol <> "}]"
  , "      }"
  , "    }"
  , "  ]"
  , "}"
  ]
  where
    bandLayer = T.unlines
      [ "    {"
      , "      \"data\": {\"name\": \"band\"},"
      , "      \"mark\": {\"type\": \"area\", \"opacity\": 0.18, \"color\": \"#2a7dbc\"},"
      , "      \"encoding\": {"
      , "        \"x\": {\"field\": \"x\", \"type\": \"quantitative\"},"
      , "        \"y\": {\"field\": \"lo\", \"type\": \"quantitative\"},"
      , "        \"y2\": {\"field\": \"hi\"}"
      , "      }"
      , "    },"
      ]
    predCILayer = T.unlines
      [ "    {"
      , "      \"data\": {\"name\": \"pred_ci\"},"
      , "      \"mark\": {\"type\": \"rule\", \"color\": \"#e74c3c\", \"strokeWidth\": 2, \"strokeDash\": [4,3]},"
      , "      \"encoding\": {"
      , "        \"x\": {\"field\": \"x\", \"type\": \"quantitative\"},"
      , "        \"y\": {\"field\": \"lo\", \"type\": \"quantitative\"},"
      , "        \"y2\": {\"field\": \"hi\"}"
      , "      }"
      , "    },"
      ]

-- ---------------------------------------------------------------------------
-- JavaScript: prediction logic
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- JavaScript: GP-specific helpers
-- ---------------------------------------------------------------------------

gpVegaEmbedJS :: ModelFit -> Text
gpVegaEmbedJS (GPFit gf) = T.unlines $
  [ "vegaEmbed('#vl-gp-" <> T.pack (show i)
    <> "', window.__vlGP" <> T.pack (show i)
    <> ", {renderer:'canvas',actions:false}).catch(console.error);"
  | i <- [0 .. length (gfKernelFits gf) - 1]
  ]
gpVegaEmbedJS _ = ""

gpModelsDataJS :: ModelFit -> Text
gpModelsDataJS (GPFit gf) = T.unlines
  [ "const gpModels = " <> jsGPModels (gfKernelFits gf) <> ";"
  ]
gpModelsDataJS _ = ""

jsGPModels :: [GPKernelFit] -> Text
jsGPModels fits = "[" <> T.intercalate "," (map jsGPModel fits) <> "]"

jsGPModel :: GPKernelFit -> Text
jsGPModel fit = T.unlines
  [ "{"
  , "  kernel: '" <> jsKernelId (gkKernel fit) <> "',"
  , "  params: " <> jsGPParams (gkKernel fit) (gkParams fit) <> ","
  , "  trainX: " <> jsDoubleArray (pdTrainX (gkPredData fit)) <> ","
  , "  alpha:  " <> jsDoubleArray (pdAlpha  (gkPredData fit)) <> ","
  , "  kyInv:  " <> jsMatrix      (pdKyInv  (gkPredData fit))
  , "}"
  ]

jsKernelId :: Kernel -> Text
jsKernelId RBF      = "rbf"
jsKernelId Matern52 = "matern52"
jsKernelId Periodic = "periodic"

jsGPParams :: Kernel -> GPParams -> Text
jsGPParams ker p =
  "{ell:" <> fmtJS (gpLengthScale p)
  <> ",sf2:" <> fmtJS (gpSignalVar p)
  <> ",sn2:" <> fmtJS (gpNoiseVar p)
  <> (if ker == Periodic then ",period:" <> fmtJS (gpPeriod p) else "")
  <> "}"

jsMatrix :: [[Double]] -> Text
jsMatrix rows = "[" <> T.intercalate "," (map jsDoubleArray rows) <> "]"

gpTabSwitchJS :: ModelFit -> Text
gpTabSwitchJS (GPFit _) = T.unlines
  [ "function showGPTab(idx) {"
  , "  document.querySelectorAll('.tab-content').forEach((el,i) => {"
  , "    el.classList.toggle('active', i === idx);"
  , "  });"
  , "  document.querySelectorAll('.tab-btn').forEach((el,i) => {"
  , "    el.classList.toggle('active', i === idx);"
  , "  });"
  , "}"
  ]
gpTabSwitchJS _ = ""

-- ---------------------------------------------------------------------------
-- CSS addition for tabs (appended in reportCss)
-- ---------------------------------------------------------------------------

predJS :: ModelFit -> Text
predJS NoRegFit = ""
predJS (GPFit _) = T.unlines
  [ "// ----- GP 予測 JS -----"
  , "function kernelEval(ker, p, x1, x2) {"
  , "  if (ker === 'rbf') {"
  , "    const d = x1 - x2, l = p.ell;"
  , "    return p.sf2 * Math.exp(-(d*d) / (2*l*l));"
  , "  } else if (ker === 'matern52') {"
  , "    const d = Math.abs(x1 - x2), l = p.ell;"
  , "    const s = Math.sqrt(5) * d / l;"
  , "    return p.sf2 * (1 + s + s*s/3) * Math.exp(-s);"
  , "  } else {"
  , "    const d = Math.abs(x1 - x2);"
  , "    const s = Math.sin(Math.PI * d / p.period);"
  , "    return p.sf2 * Math.exp(-2 * s*s / (p.ell * p.ell));"
  , "  }"
  , "}"
  , ""
  , "function gpPredict(midx, xStar) {"
  , "  const m = gpModels[midx];"
  , "  const kStar = m.trainX.map(xi => kernelEval(m.kernel, m.params, xi, xStar));"
  , "  const mean  = kStar.reduce((s, k, i) => s + k * m.alpha[i], 0);"
  , "  const v     = m.kyInv.map(row => row.reduce((s, v, j) => s + v * kStar[j], 0));"
  , "  const kss   = kernelEval(m.kernel, m.params, xStar, xStar);"
  , "  const variance = Math.max(0, kss - kStar.reduce((s, k, i) => s + k * v[i], 0));"
  , "  return { mean, std: Math.sqrt(variance) };"
  , "}"
  , ""
  , "window.__predView = null;"
  , ""
  , "function updateGPPrediction() {"
  , "  const xStar = parseFloat(document.getElementById('x-gp').value);"
  , "  const midx  = parseInt(document.getElementById('pred-kernel').value);"
  , "  const { mean, std } = gpPredict(midx, xStar);"
  , "  const lo = mean - 2 * std, hi = mean + 2 * std;"
  , "  const el = id => document.getElementById(id);"
  , "  if (el('gp-pred-mean')) el('gp-pred-mean').textContent = mean.toFixed(5);"
  , "  if (el('gp-pred-std'))  el('gp-pred-std').textContent  = std.toFixed(5);"
  , "  if (el('gp-pred-lo'))   el('gp-pred-lo').textContent   = lo.toFixed(5);"
  , "  if (el('gp-pred-hi'))   el('gp-pred-hi').textContent   = hi.toFixed(5);"
  , "  if (typeof gpDataXMin !== 'undefined') {"
  , "    const extrap = xStar < gpDataXMin || xStar > gpDataXMax;"
  , "    const warn = el('gp-extrap-warn');"
  , "    if (warn) warn.style.display = extrap ? 'inline-block' : 'none';"
  , "  }"
  , "  if (window.__predView) {"
  , "    const { mean: m0 } = gpPredict(0, xStar);"
  , "    window.__predView.change('pred_point',"
  , "      vega.changeset().remove(() => true).insert([{x: xStar, y: m0}])).run();"
  , "    window.__predView.change('pred_ci',"
  , "      vega.changeset().remove(() => true)"
  , "        .insert([{x: xStar, lo: m0 - 2*gpPredict(0,xStar).std,"
  , "                  hi: m0 + 2*gpPredict(0,xStar).std}])).run();"
  , "  }"
  , "}"
  , ""
  , "function syncGPSlider() {"
  , "  const v = document.getElementById('x-gp').value;"
  , "  const n = document.getElementById('x-gp-num');"
  , "  if (n) n.value = parseFloat(v).toFixed(5);"
  , "  updateGPPrediction();"
  , "}"
  , ""
  , "function syncGPNum() {"
  , "  const v = parseFloat(document.getElementById('x-gp-num').value);"
  , "  const s = document.getElementById('x-gp');"
  , "  if (s) s.value = v;"
  , "  updateGPPrediction();"
  , "}"
  , ""
  , "if (window.__pred_chart) {"
  , "  vegaEmbed('#pred-chart', window.__pred_chart, {renderer:'canvas',actions:false})"
  , "    .then(({view}) => { window.__predView = view; updateGPPrediction(); })"
  , "    .catch(console.error);"
  , "} else {"
  , "  updateGPPrediction();"
  , "}"
  ]
predJS fit = T.unlines $
  [ "// ----- 予測 JS -----"
  , "const linkName   = '" <> lnk <> "';"
  , "const xColDegs   = " <> jsXColDegs colDegs <> ";"
  , "const coeffs     = " <> jsDoubleArray (map snd cs) <> ";"
  ] ++
  (case fit of
     MixFit gs -> ["const blups = " <> jsBLUPs (gsBLUPs gs) <> ";"]
     _         -> []) ++
  [ ""
  , "// Vega view (初期化後にセット)"
  , "window.__predView = null;"
  , ""
  , "function invLink(link, eta) {"
  , "  switch(link) {"
  , "    case 'log':   return Math.exp(eta);"
  , "    case 'logit': return 1 / (1 + Math.exp(-eta));"
  , "    case 'sqrt':  return eta * eta;"
  , "    default:      return eta;"
  , "  }"
  , "}"
  , ""
  , "function computeEta(xVals, groupName) {"
  , "  let eta = coeffs[0];"
  , "  let i = 1;"
  , "  for (const [col, deg] of xColDegs) {"
  , "    const x = parseFloat(xVals[col] || 0);"
  , "    for (let k = 1; k <= deg; k++) {"
  , "      eta += coeffs[i++] * Math.pow(x, k);"
  , "    }"
  , "  }"
  ] ++
  (case fit of
     MixFit _ ->
       [ "  if (groupName) {"
       , "    const b = blups.find(([g]) => g === groupName);"
       , "    if (b) eta += b[1];"
       , "  }"
       ]
     _ -> []) ++
  [ "  return eta;"
  , "}"
  , ""
  , "function getXVals() {"
  , "  const vals = {};"
  , "  for (const [col] of xColDegs) {"
  , "    vals[col] = document.getElementById('x-' + col)?.value || '0';"
  , "  }"
  , "  return vals;"
  , "}"
  , ""
  -- CI補間 (smoothXs は predChartSpecJS でセット)
  , "function interpAt(x, arr) {"
  , "  const n = smoothXs.length;"
  , "  if (!n) return 0;"
  , "  if (x <= smoothXs[0])   return arr[0];"
  , "  if (x >= smoothXs[n-1]) return arr[n-1];"
  , "  let lo = 0, hi = n - 1;"
  , "  while (lo < hi - 1) {"
  , "    const mid = (lo + hi) >> 1;"
  , "    if (smoothXs[mid] <= x) lo = mid; else hi = mid;"
  , "  }"
  , "  const t = (x - smoothXs[lo]) / (smoothXs[hi] - smoothXs[lo]);"
  , "  return arr[lo] + t * (arr[hi] - arr[lo]);"
  , "}"
  , ""
  , "function updatePrediction() {"
  , "  const xVals = getXVals();"
  , groupSelectJS fit
  , "  const eta = computeEta(xVals, grp);"
  , "  const y   = invLink(linkName, eta);"
  , "  const etaEl = document.getElementById('pred-eta');"
  , "  const yEl   = document.getElementById('pred-y');"
  , "  if (etaEl) etaEl.textContent = eta.toFixed(4);"
  , "  if (yEl)   yEl.textContent   = y.toFixed(4);"
  , ""
  , "  // 外挿域チェック"
  , "  if (typeof predXCol !== 'undefined' && typeof dataXMin !== 'undefined') {"
  , "    const xv = parseFloat(xVals[predXCol] || 0);"
  , "    const isExtrap = xv < dataXMin || xv > dataXMax;"
  , "    const warnEl = document.getElementById('extrap-warn');"
  , "    if (warnEl) warnEl.style.display = isExtrap ? 'inline-block' : 'none';"
  , "  }"
  , ""
  , "  // Vega チャート更新"
  , "  if (window.__predView && typeof predXCol !== 'undefined') {"
  , "    const xv = parseFloat(xVals[predXCol] || 0);"
  , "    window.__predView.change('pred_point',"
  , "      vega.changeset().remove(() => true).insert([{x: xv, y}])).run();"
  , "    if (smoothHasBand) {"
  , "      const lo = interpAt(xv, smoothLo);"
  , "      const hi = interpAt(xv, smoothHi);"
  , "      window.__predView.change('pred_ci',"
  , "        vega.changeset().remove(() => true).insert([{x: xv, lo, hi}])).run();"
  , "      const loEl = document.getElementById('pred-ci-lo');"
  , "      const hiEl = document.getElementById('pred-ci-hi');"
  , "      if (loEl) loEl.textContent = lo.toFixed(4);"
  , "      if (hiEl) hiEl.textContent = hi.toFixed(4);"
  , "    }"
  , "  }"
  , "}"
  , ""
  , "function syncSlider(col) {"
  , "  const v = document.getElementById('x-' + col).value;"
  , "  const num = document.getElementById('x-num-' + col);"
  , "  if (num) num.value = parseFloat(v).toFixed(5);"
  , "  updatePrediction();"
  , "}"
  , ""
  , "function syncNum(col) {"
  , "  const v = parseFloat(document.getElementById('x-num-' + col).value);"
  , "  const sld = document.getElementById('x-' + col);"
  , "  if (sld) sld.value = v;"
  , "  updatePrediction();"
  , "}"
  , ""
  , "// 予測チャートの初期化"
  , "if (window.__pred_chart) {"
  , "  vegaEmbed('#pred-chart', window.__pred_chart, {renderer:'canvas',actions:false})"
  , "    .then(({view}) => {"
  , "      window.__predView = view;"
  , "      updatePrediction();"
  , "    }).catch(console.error);"
  , "} else {"
  , "  updatePrediction();"
  , "}"
  ]
  where
    (cs, colDegs, lnk) = fitDataFor fit

fitDataFor :: ModelFit -> ([(Text, Double)], [(Text, Int)], Text)
fitDataFor (RegFit fs) = (fsCoeffs fs, fsXColDegs fs, fsLinkName fs)
fitDataFor (MixFit gs) = (gsFixed gs,  gsXColDegs gs,  gsLinkName gs)
fitDataFor (HBMFit hs) = let fs = hbmsFit hs
                         in (fsCoeffs fs, fsXColDegs fs, fsLinkName fs)
fitDataFor (GPFit _)   = ([], [], "identity")
fitDataFor NoRegFit    = ([], [], "identity")

groupSelectJS :: ModelFit -> Text
groupSelectJS (MixFit _) =
  "  const sel = document.getElementById('pred-group');\n" <>
  "  const grp = sel ? sel.value : null;"
groupSelectJS _ = "  const grp = null;"

smoothScrollScript :: Text
smoothScrollScript = T.unlines
  [ "document.querySelectorAll('.nav-link').forEach(a => {"
  , "  a.addEventListener('click', e => {"
  , "    e.preventDefault();"
  , "    const t = document.querySelector(a.getAttribute('href'));"
  , "    if (t) t.scrollIntoView({ behavior: 'smooth' });"
  , "  });"
  , "});"
  ]

-- ---------------------------------------------------------------------------
-- JS helpers
-- ---------------------------------------------------------------------------

jsStr :: Text -> Text
jsStr t = "\"" <> t <> "\""

jsStrArray :: [Text] -> Text
jsStrArray xs = "[" <> T.intercalate "," (map jsStr xs) <> "]"

jsXColDegs :: [(Text, Int)] -> Text
jsXColDegs xs = "[" <> T.intercalate "," (map kv xs) <> "]"
  where kv (c, d) = "[\"" <> c <> "\"," <> T.pack (show d) <> "]"

jsDoubleArray :: [Double] -> Text
jsDoubleArray xs = "[" <> T.intercalate "," (map fmtJS xs) <> "]"

jsBLUPs :: [(Text, Double)] -> Text
jsBLUPs bs = "[" <> T.intercalate "," (map kv bs) <> "]"
  where kv (g, v) = "[\"" <> g <> "\"," <> fmtJS v <> "]"

specJson :: VegaLite -> Text
specJson = decodeUtf8 . toStrict . encode . fromVL

-- ---------------------------------------------------------------------------
-- Formatting helpers
-- ---------------------------------------------------------------------------

fmtJS :: Double -> Text
fmtJS v
  | isNaN v      = "0"
  | isInfinite v = if v > 0 then "1e308" else "-1e308"
  | otherwise    = T.pack (showFFloat (Just 10) v "")

fmt4 :: Double -> Text
fmt4 v = T.pack (showFFloat (Just 4) v "")

fmt1 :: Double -> Text
fmt1 v = T.pack (showFFloat (Just 1) v "")

fmtSigned :: Double -> Text
fmtSigned v
  | v >= 0    = " " <> fmt4 v
  | otherwise = fmt4 v

-- ---------------------------------------------------------------------------
-- Model text helpers
-- ---------------------------------------------------------------------------

linkName :: LinkFn -> Text
linkName Identity = "identity"
linkName Log      = "log"
linkName Logit    = "logit"
linkName Sqrt     = "sqrt"

modelTypeLabel :: Family -> LinkFn -> Text
modelTypeLabel Gaussian Identity = "LM (Gaussian / Identity)"
modelTypeLabel fam lnk =
  "GLM (" <> T.pack (show fam) <> " / " <> linkName lnk <> ")"

glmmTypeLabel :: Family -> LinkFn -> Text
glmmTypeLabel Gaussian Identity = "LME (Gaussian, exact EM)"
glmmTypeLabel fam lnk =
  "GLMM (" <> T.pack (show fam) <> " / " <> linkName lnk <> ", Laplace)"

r2Label :: Family -> Text
r2Label Gaussian = "R²"
r2Label _        = "McFadden R²"

formulaText :: [(Text, Int)] -> Text
formulaText colDegs =
  "y ~ " <> T.intercalate " + "
  [ col <> if k == 1 then "" else "^" <> T.pack (show k)
  | (col, deg) <- colDegs
  , k <- [1..deg]
  ]

coeffLabels :: [(Text, Int)] -> [Text]
coeffLabels colDegs =
  "β₀ (intercept)" : zipWith lbl [1..] terms
  where
    terms = [(col, k) | (col, deg) <- colDegs, k <- [1..deg]]
    lbl i (col, k) =
      "β" <> T.pack (show (i::Int)) <> " ("
      <> col
      <> (if k == 1 then "" else "^" <> T.pack (show k))
      <> ")"

-- ---------------------------------------------------------------------------
-- HTML component builders
-- ---------------------------------------------------------------------------

statBox :: Text -> Text -> Bool -> Text
statBox lbl val hi = T.unlines
  [ "    <div class=\"stat-box" <> (if hi then " highlight" else "") <> "\">"
  , "      <div class=\"lbl\">" <> lbl <> "</div>"
  , "      <div class=\"val\">" <> val <> "</div>"
  , "    </div>"
  ]

infoBox :: Text -> Text -> Text
infoBox lbl val = T.unlines
  [ "    <div class=\"info-box\">"
  , "      <div class=\"lbl\">" <> lbl <> "</div>"
  , "      <div class=\"ival\">" <> val <> "</div>"
  , "    </div>"
  ]

-- ---------------------------------------------------------------------------
-- CSS
-- ---------------------------------------------------------------------------

reportCss :: Text
reportCss = T.unlines
  [ "* { box-sizing: border-box; margin: 0; padding: 0; }"
  , "body { font-family: 'Segoe UI', system-ui, sans-serif; background: #f0f2f5; color: #333; line-height: 1.6; }"
  , "nav { position: sticky; top: 0; z-index: 100; background: #1e3a5c;"
  , "      padding: 10px 28px; display: flex; gap: 20px; align-items: center;"
  , "      box-shadow: 0 2px 6px rgba(0,0,0,.25); }"
  , "nav h1 { color: #ecf0f1; font-size: 1em; font-weight: 600; flex: 1; }"
  , ".nav-link { color: #9ab; text-decoration: none; font-size: .82em; white-space: nowrap; }"
  , ".nav-link:hover { color: #fff; }"
  , "main { max-width: 1160px; margin: 0 auto; padding: 32px 20px; }"
  , "section { background: white; border-radius: 12px; padding: 26px 28px;"
  , "          margin-bottom: 28px; box-shadow: 0 2px 10px rgba(0,0,0,.07); }"
  , "h2 { font-size: 1.05em; font-weight: 700; color: #1e3a5c; margin-bottom: 18px;"
  , "     border-bottom: 2px solid #e4e9f0; padding-bottom: 8px; display: flex; align-items: center; gap: 8px; }"
  , "h3 { font-size: .92em; font-weight: 600; color: #2a5298; margin: 18px 0 10px; }"
  , ".sec-icon { font-size: 1.1em; }"
  , ".sec-desc { font-size: .88em; color: #666; margin-bottom: 16px; }"
  -- stat boxes
  , ".stat-grid { display: flex; gap: 12px; flex-wrap: wrap; margin-bottom: 16px; }"
  , ".stat-box { background: #f7f9fc; border: 1px solid #e4e9f0; border-radius: 10px;"
  , "            padding: 12px 16px; min-width: 120px; text-align: center; }"
  , ".stat-box .lbl { font-size: .7em; color: #888; text-transform: uppercase; letter-spacing: .05em; margin-bottom: 4px; }"
  , ".stat-box .val { font-size: 1.25em; font-weight: 700; color: #1e3a5c; }"
  , ".stat-box.highlight { background: #e8f4e8; border-color: #4caf50; }"
  , ".stat-box.highlight .val { color: #2e7d32; }"
  -- info boxes
  , ".mermaid-wrap { background:#f7fafc; border-radius:8px; padding:24px; margin:12px 0; text-align:center; overflow-x:auto; }"
  , ".mermaid-wrap .mermaid { display:inline-block; min-width:320px; min-height:200px;"
  , "   font-family:'Segoe UI',sans-serif; line-height:1.4; }"
  , ".mermaid-wrap .mermaid svg { max-width:100%; height:auto; min-height:240px; }"
  , ".info-grid { display: flex; gap: 12px; flex-wrap: wrap; margin-bottom: 16px; }"
  , ".info-box { background: #f7f9fc; border: 1px solid #e4e9f0; border-radius: 10px;"
  , "            padding: 12px 18px; min-width: 180px; }"
  , ".info-box .lbl { font-size: .72em; color: #888; text-transform: uppercase; letter-spacing: .04em; margin-bottom: 4px; }"
  , ".info-box .ival { font-size: .95em; font-weight: 600; color: #1e3a5c; }"
  -- column cards (data section)
  , ".col-cards { display: flex; flex-wrap: wrap; gap: 16px; }"
  , ".col-card { background: #f7f9fc; border: 1px solid #e4e9f0; border-radius: 10px;"
  , "            padding: 16px 18px; flex: 1; min-width: 320px; }"
  , ".col-card-title { display: flex; align-items: center; gap: 8px; margin-bottom: 12px; }"
  , ".col-role { font-size: .7em; background: #1e3a5c; color: white; border-radius: 4px;"
  , "            padding: 2px 7px; text-transform: uppercase; letter-spacing: .04em; }"
  , ".col-name { font-size: .95em; font-weight: 700; color: #1e3a5c; }"
  , ".col-card-body { display: flex; gap: 14px; align-items: flex-start; }"
  , ".col-hist { flex: 1; min-width: 0; }"
  , ".col-stats-mini { min-width: 140px; font-size: .82em; }"
  , ".col-stat-row { display: flex; justify-content: space-between; padding: 3px 0;"
  , "                border-bottom: 1px solid #eef; gap: 8px; }"
  , ".col-stat-row .sk { color: #777; }"
  , ".col-stat-row .sv { font-family: monospace; font-weight: 600; color: #1e3a5c; text-align: right; }"
  -- tables
  , "table { width: 100%; border-collapse: collapse; font-size: .88em; margin-bottom: 8px; }"
  , "thead tr { background: #f0f4f8; }"
  , "th { padding: 8px 14px; text-align: left; font-weight: 600; color: #444; }"
  , "td { padding: 7px 14px; border-bottom: 1px solid #f0f2f5; font-family: monospace; }"
  , "td:first-child { font-family: inherit; font-weight: 500; }"
  , "tr:last-child td { border-bottom: none; }"
  , ".vl-wrap { overflow-x: auto; margin-bottom: 8px; }"
  -- prediction section layout
  , ".predict-layout { display: flex; gap: 20px; flex-wrap: wrap; }"
  , ".predict-left { flex: 0 0 340px; min-width: 280px; }"
  , ".predict-chart { flex: 1; min-width: 320px; }"
  , ".predict-controls { background: #f7f9fc; border-radius: 10px; padding: 16px 18px; margin-bottom: 14px; }"
  , ".slider-row { display: flex; align-items: center; gap: 10px; margin-bottom: 10px; flex-wrap: wrap; }"
  , ".slider-row label { font-size: .88em; color: #555; min-width: 80px; font-weight: 500; }"
  , "input[type=range] { flex: 1; min-width: 120px; accent-color: #1e3a5c; }"
  , "input[type=number] { width: 105px; padding: 5px 8px; border: 1.5px solid #c0ccd8; border-radius: 6px; font-size: .88em; }"
  , "select { padding: 6px 10px; border: 1.5px solid #c0ccd8; border-radius: 6px; font-size: .86em; background: white; }"
  , ".predict-output { display: flex; gap: 10px; flex-wrap: wrap; }"
  , ".pred-box { flex: 1; min-width: 100px; background: white; border: 1.5px solid #e4e9f0;"
  , "            border-radius: 10px; padding: 12px 14px; text-align: center; }"
  , ".pred-box .plbl { font-size: .72em; color: #888; text-transform: uppercase; letter-spacing: .04em; }"
  , ".pred-box .pval { font-size: 1.3em; font-weight: 700; color: #1e3a5c; margin: 4px 0; }"
  , ".pred-box .psub { font-size: .76em; color: #999; }"
  , ".pred-box.mean-box { border-color: #1e3a5c; }"
  , ".pred-box.ci-box   { border-color: #e74c3c; }"
  , ".pred-box.ci-box .pval { font-size: 1.0em; color: #c0392b; }"
  , ".tab-bar { display: flex; gap: 6px; margin-bottom: 18px; flex-wrap: wrap; }"
  , ".tab-btn { padding: 7px 18px; border: 1.5px solid #c0ccd8; border-radius: 20px;"
  , "           background: white; color: #555; cursor: pointer; font-size: .88em;"
  , "           transition: all .15s; }"
  , ".tab-btn:hover { border-color: #1e3a5c; color: #1e3a5c; }"
  , ".tab-btn.active { background: #1e3a5c; color: white; border-color: #1e3a5c; }"
  , ".tab-content { display: none; }"
  , ".tab-content.active { display: block; }"
  , ".extrap-badge { background: #ff9800; color: white; border-radius: 4px;"
  , "                padding: 1px 7px; font-size: .7em; font-weight: 700;"
  , "                margin-left: 6px; vertical-align: middle; letter-spacing: .03em; }"
  -- appendix
  , ".appendix-block { background: #f7f9fc; border-left: 4px solid #1e3a5c;"
  , "                  padding: 14px 18px; margin: 12px 0; border-radius: 0 8px 8px 0; }"
  , ".appendix-block h4 { font-size: .9em; font-weight: 700; color: #1e3a5c; margin-bottom: 6px; }"
  , ".appendix-block p, .appendix-block li { font-size: .88em; color: #444; margin-bottom: 4px; }"
  , ".formula { background: #f7f9fc; border: 1px solid #e4e9f0; border-radius: 8px;"
  , "           padding: 10px 14px; margin: 8px 0; font-family: monospace; font-size: .88em; color: #333; }"
  , ".cmp-table { width: 100%; border-collapse: collapse; font-size: .88em; margin: 12px 0; }"
  , ".cmp-table th { background: #f0f4f9; padding: 9px 12px; text-align: left;"
  , "                font-weight: 600; color: #2c3e50; border-bottom: 2px solid #d0d7e3; }"
  , ".cmp-table td { padding: 7px 12px; border-bottom: 1px solid #eef2f6; }"
  , ".cmp-table td.num { text-align: right; font-variant-numeric: tabular-nums; }"
  , ".cmp-color { display: inline-block; width: 14px; height: 14px; border-radius: 3px;"
  , "             margin-right: 6px; vertical-align: middle; border: 1px solid rgba(0,0,0,.15); }"
  , ".cmp-ci    { color: #555; font-size: .82em; }"
  ]

-- ===========================================================================
-- 複数モデル比較レポート
-- ===========================================================================

-- | 比較レポートに含めるモデルエントリ。
data CompareEntry = CompareEntry
  { ceLabel :: Text       -- ^ モデル表示名 (例: "LM (Pooled)")
  , ceColor :: Text       -- ^ プロットの色 (CSS カラーコード, 例: "#e41a1c")
  , ceFit   :: ModelFit
  }

-- | 複数モデルを 1 つの HTML レポートに並べた比較レポートを生成する。
--
-- セクション構成:
--   1. データの特性 (1 度だけ)
--   2. モデル概要 (各モデルの種別・式・係数を 1 行ずつ並べた表)
--   3. 予測曲線オーバーレイ (全モデルの曲線 + 信用区間を 1 つの散布図に)
--   4. 係数比較 (forest plot 形式の表)
--   5. WAIC/LOO 比較 (利用可能なモデルのみ)
writeComparisonReport
  :: FilePath
  -> AnalysisReportConfig
  -> DataFrame
  -> [Text]              -- ^ x 列名 (典型的には 1 つ)
  -> Text                -- ^ y 列名
  -> [CompareEntry]
  -> IO ()
writeComparisonReport path cfg df xCols yCol entries =
  TIO.writeFile path (buildCompareHtml cfg df xCols yCol entries)

buildCompareHtml
  :: AnalysisReportConfig -> DataFrame -> [Text] -> Text
  -> [CompareEntry] -> Text
buildCompareHtml cfg df xCols yCol entries = T.unlines $
  [ "<!DOCTYPE html>"
  , "<html lang=\"ja\">"
  , "<head>"
  , "  <meta charset=\"utf-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "  <title>" <> arcTitle cfg <> "</title>"
  , "  <script>" <> vegaJS      <> "</script>"
  , "  <script>" <> vegaLiteJS  <> "</script>"
  , "  <script>" <> vegaEmbedJS <> "</script>"
  , "  <style>" , reportCss , "  </style>"
  , "</head>"
  , "<body>"
  , compareNavBar cfg
  , "<main>"
  , dataSummarySection df xCols yCol
  , compareModelsSection entries
  , compareOverlaySection xCols yCol
  , compareCoefSection entries
  , compareWaicSection entries
  , "</main>"
  , "<script>"
  , compareOverlayJS df xCols yCol entries
  , columnDataJS df xCols yCol
  , histogramInitJS (xCols ++ [yCol])
  , smoothScrollScript
  , "</script>"
  , "</body>"
  , "</html>"
  ]

compareNavBar :: AnalysisReportConfig -> Text
compareNavBar cfg = T.unlines
  [ "<nav>"
  , "  <h1>&#128202; " <> arcTitle cfg <> "</h1>"
  , "  <a class=\"nav-link\" href=\"#sec-data\">データ</a>"
  , "  <a class=\"nav-link\" href=\"#sec-cmp-models\">モデル一覧</a>"
  , "  <a class=\"nav-link\" href=\"#sec-cmp-overlay\">予測比較</a>"
  , "  <a class=\"nav-link\" href=\"#sec-cmp-coef\">係数比較</a>"
  , "  <a class=\"nav-link\" href=\"#sec-cmp-waic\">WAIC/LOO</a>"
  , "</nav>"
  ]

-- | モデル一覧表
compareModelsSection :: [CompareEntry] -> Text
compareModelsSection entries = T.unlines $
  [ "<section id=\"sec-cmp-models\">"
  , "  <h2><span class=\"sec-icon\">&#9878;</span> 2. モデル一覧</h2>"
  , "  <p class=\"sec-desc\">本レポートで比較する " <> T.pack (show (length entries))
    <> " モデルの概要。色はオーバーレイ図と凡例で共通。</p>"
  , "  <table class=\"cmp-table\">"
  , "    <thead><tr>"
  , "      <th></th><th>モデル</th><th>種別</th><th>回帰式</th>"
  , "      <th class=\"num\">R²</th>"
  , "    </tr></thead>"
  , "    <tbody>"
  ] ++
  map modelRowHtml entries ++
  [ "    </tbody>"
  , "  </table>"
  , "</section>"
  ]
  where
    modelRowHtml e = T.unlines
      [ "      <tr>"
      , "        <td><span class=\"cmp-color\" style=\"background:" <> ceColor e <> "\"></span></td>"
      , "        <td><b>" <> ceLabel e <> "</b></td>"
      , "        <td>" <> modelTypeOf (ceFit e) <> "</td>"
      , "        <td>" <> modelFormulaOf (ceFit e) <> "</td>"
      , "        <td class=\"num\">" <> fmt4 (modelR2Of (ceFit e)) <> "</td>"
      , "      </tr>"
      ]

modelTypeOf :: ModelFit -> Text
modelTypeOf (RegFit fs)  = fsModelType fs
modelTypeOf (MixFit gs)  = gsModelType gs
modelTypeOf (HBMFit hs)  = fsModelType (hbmsFit hs)
modelTypeOf (GPFit _)    = "Gaussian Process"
modelTypeOf NoRegFit     = "—"

modelFormulaOf :: ModelFit -> Text
modelFormulaOf (RegFit fs)  = fsFormula fs
modelFormulaOf (MixFit gs)  = gsFormula gs
modelFormulaOf (HBMFit hs)  = fsFormula (hbmsFit hs)
modelFormulaOf (GPFit _)    = "y ~ GP(m, k)"
modelFormulaOf NoRegFit     = "—"

modelR2Of :: ModelFit -> Double
modelR2Of (RegFit fs)  = fsR2 fs
modelR2Of (MixFit gs)  = gsR2 gs
modelR2Of (HBMFit hs)  = fsR2 (hbmsFit hs)
modelR2Of _            = 0

-- | 予測曲線オーバーレイ (描画は JS で実装)
compareOverlaySection :: [Text] -> Text -> Text
compareOverlaySection xCols yCol = T.unlines
  [ "<section id=\"sec-cmp-overlay\">"
  , "  <h2><span class=\"sec-icon\">&#128200;</span> 3. 予測曲線比較</h2>"
  , "  <p class=\"sec-desc\">同一データに対する各モデルの予測曲線を重ね描き。"
  , "    HBM など信用区間を持つモデルはバンドも表示。</p>"
  , "  <div class=\"vl-wrap\"><div id=\"cmp-overlay\"></div></div>"
  , if length xCols /= 1
      then "  <p style=\"font-size:.85em;color:#888\">x 列が単一でないため曲線比較は省略しました。</p>"
      else "  <p style=\"font-size:.82em;color:#666;margin-top:12px\">x = "
           <> head xCols <> ", y = " <> yCol <> "</p>"
  , "</section>"
  ]

-- | 係数比較表 (HBM は CI 付き)
compareCoefSection :: [CompareEntry] -> Text
compareCoefSection entries = T.unlines $
  [ "<section id=\"sec-cmp-coef\">"
  , "  <h2><span class=\"sec-icon\">&#128300;</span> 4. 係数比較</h2>"
  , "  <p class=\"sec-desc\">各モデルが推定したパラメータの一覧。"
  , "    HBM は事後平均と 95% 信用区間 [2.5%, 97.5%] を表示。</p>"
  , "  <table class=\"cmp-table\">"
  , "    <thead><tr>"
  , "      <th></th><th>モデル</th><th>パラメータ</th>"
  , "      <th class=\"num\">推定値</th><th class=\"num\">95% CI</th>"
  , "    </tr></thead>"
  , "    <tbody>"
  ] ++
  concatMap coefRowsForEntry entries ++
  [ "    </tbody>"
  , "  </table>"
  , "</section>"
  ]
  where
    coefRowsForEntry e =
      let coefs = extractCoefRows (ceFit e)
          n = length coefs
      in zipWith (mkCoefRow e n) [0 :: Int ..] coefs

    mkCoefRow e n i (cname, val, mci) =
      let firstCol = if i == 0
                      then "<td rowspan=\"" <> T.pack (show n) <> "\">"
                           <> "<span class=\"cmp-color\" style=\"background:" <> ceColor e <> "\"></span>"
                           <> "</td><td rowspan=\"" <> T.pack (show n) <> "\"><b>"
                           <> ceLabel e <> "</b></td>"
                      else ""
          ciCell = case mci of
            Just (lo, hi) -> "<span class=\"cmp-ci\">[" <> fmtSigned lo
                          <> ", " <> fmtSigned hi <> "]</span>"
            Nothing       -> "<span class=\"cmp-ci\">—</span>"
      in T.unlines
           [ "      <tr>"
           , "        " <> firstCol
           , "        <td>" <> cname <> "</td>"
           , "        <td class=\"num\">" <> fmtSigned val <> "</td>"
           , "        <td class=\"num\">" <> ciCell <> "</td>"
           , "      </tr>"
           ]

extractCoefRows :: ModelFit -> [(Text, Double, Maybe (Double, Double))]
extractCoefRows (RegFit fs)  = [(n, v, Nothing) | (n, v) <- fsCoeffs fs]
extractCoefRows (MixFit gs)  = [(n, v, Nothing) | (n, v) <- gsFixed gs]
extractCoefRows (HBMFit hs)  =
  [ (n, m, Just (lo, hi))
  | (n, m, _, lo, hi) <- hbmsPosteriorRows hs ]
extractCoefRows _            = []

-- | WAIC / LOO 比較 (どれか 1 つでも持っていれば表示)
compareWaicSection :: [CompareEntry] -> Text
compareWaicSection entries =
  let rows = [ (e, w, l) | e <- entries
             , let mws = waicLooOf (ceFit e)
             , Just (w, l) <- [mws] ]
  in if null rows
     then ""
     else
       let bestWaic = minimum [waicValue w | (_, w, _) <- rows]
           bestLoo  = minimum [looValue  l | (_, _, l) <- rows]
       in T.unlines $
       [ "<section id=\"sec-cmp-waic\">"
       , "  <h2><span class=\"sec-icon\">&#128202;</span> 5. WAIC / LOO 比較</h2>"
       , "  <p class=\"sec-desc\">情報量規準が小さいほど良い。"
         <> "ΔWAIC ≈ 0 のモデル群は実質的に同等。最良モデルを <b>★</b> で示す。</p>"
       , "  <table class=\"cmp-table\">"
       , "    <thead><tr>"
       , "      <th></th><th>モデル</th><th class=\"num\">WAIC</th>"
       , "      <th class=\"num\">ΔWAIC</th><th class=\"num\">LOO</th>"
       , "      <th class=\"num\">ΔLOO</th>"
       , "    </tr></thead>"
       , "    <tbody>"
       ] ++
       map (waicRowHtml bestWaic bestLoo) rows ++
       [ "    </tbody>"
       , "  </table>"
       , "</section>"
       ]
  where
    waicRowHtml bw bl (e, w, l) =
      let dw = waicValue w - bw
          dl = looValue  l - bl
          star x = if x == 0 then " ★" else ""
      in T.unlines
           [ "      <tr>"
           , "        <td><span class=\"cmp-color\" style=\"background:"
             <> ceColor e <> "\"></span></td>"
           , "        <td><b>" <> ceLabel e <> "</b></td>"
           , "        <td class=\"num\">" <> fmt4 (waicValue w) <> "</td>"
           , "        <td class=\"num\">" <> fmt4 dw <> star dw <> "</td>"
           , "        <td class=\"num\">" <> fmt4 (looValue l) <> "</td>"
           , "        <td class=\"num\">" <> fmt4 dl <> star dl <> "</td>"
           , "      </tr>"
           ]

waicLooOf :: ModelFit -> Maybe (WAICResult, LOOResult)
waicLooOf (RegFit fs) = fsModelSelect fs
waicLooOf (HBMFit hs) = fsModelSelect (hbmsFit hs)
waicLooOf (MixFit gs) = gsModelSelect gs
waicLooOf _           = Nothing

-- | オーバーレイ用の Vega-Lite spec を組み立てる JS (data URL 経由)
compareOverlayJS :: DataFrame -> [Text] -> Text -> [CompareEntry] -> Text
compareOverlayJS df xCols yCol entries
  | length xCols /= 1 = ""
  | otherwise =
      let xCol = head xCols
          (xs, ys) = case (getNumeric xCol df, getNumeric yCol df) of
            (Just xv, Just yv) -> (V.toList xv, V.toList yv)
            _                  -> ([], [])
          gs = case getText "group" df of
                 Just gv -> map Just (V.toList gv)
                 Nothing -> map (const Nothing) xs
          dataPoints = T.intercalate "," $
            zipWith3 (\x y mg ->
              let g = maybe "" (\t -> ",\"g\":\"" <> t <> "\"") mg
              in "{\"x\":" <> fmtJS x <> ",\"y\":" <> fmtJS y <> g <> "}")
              xs ys gs
          hasGroups = any ( /= Nothing) gs
          -- 各 modelLayer は ",{band},{line}" 形式で返す (リーディングカンマあり)
          modelLayers = T.concat (map (modelLayer xCol yCol) entries)
          legendItems = T.intercalate "," (map legendItem entries)
      in T.unlines
           [ "(function() {"
           , "  const spec = {"
           , "    '$schema':'https://vega.github.io/schema/vega-lite/v5.json',"
           , "    width: 720, height: 400, background:'transparent',"
           , "    layer: ["
           , "      {"
           , "        data:{values:[" <> dataPoints <> "]},"
           , "        mark:{type:'circle',size:60,opacity:0.7},"
           , "        encoding:{"
           , "          x:{field:'x',type:'quantitative',axis:{title:'" <> xCol <> "'}},"
           , "          y:{field:'y',type:'quantitative',axis:{title:'" <> yCol <> "'}}"
           , if hasGroups
               then "          ,color:{field:'g',type:'nominal',scale:{scheme:'tableau10'},legend:{title:'group'}}"
               else "          ,color:{value:'#888'}"
           , "        }"
           , "      }"
           , modelLayers   -- 各要素はすでに先頭カンマ付き
           , "    ]"
           , "  };"
           , "  vegaEmbed('#cmp-overlay', spec, {actions:false}).catch(console.error);"
           , "  // モデル凡例 (色対応をテキストで表示)"
           , "  const lg = [" <> legendItems <> "];"
           , "  console.log('Model legend:', lg);"
           , "})();"
           ]
  where
    legendItem e = "{\"label\":\"" <> ceLabel e <> "\",\"color\":\"" <> ceColor e <> "\"}"

-- | 1 モデルの予測曲線レイヤー (smoothData があれば線 + バンド)
modelLayer :: Text -> Text -> CompareEntry -> Text
modelLayer xCol yCol e =
  case smoothDataFor (ceFit e) of
    Nothing -> ""
    Just (_, sd) ->
      let pts = T.intercalate "," $
            zipWith4 (\x y lo hi ->
              "{\"x\":" <> fmtJS x <> ",\"y\":" <> fmtJS y
              <> ",\"lo\":" <> fmtJS lo <> ",\"hi\":" <> fmtJS hi <> "}")
            (sdXs sd) (sdYs sd) (sdLower sd) (sdUpper sd)
          color = ceColor e
          band  = if sdHasBand sd
                  then T.unlines
                    [ "      ,{"
                    , "        data:{values:[" <> pts <> "]},"
                    , "        mark:{type:'area',color:'" <> color <> "',opacity:0.18},"
                    , "        encoding:{"
                    , "          x:{field:'x',type:'quantitative'},"
                    , "          y:{field:'lo',type:'quantitative'},"
                    , "          y2:{field:'hi'}"
                    , "        }"
                    , "      }"
                    ]
                  else ""
          line = T.unlines
            [ "      ,{"
            , "        data:{values:[" <> pts <> "]},"
            , "        mark:{type:'line',color:'" <> color
              <> "',strokeWidth:2.5,tooltip:{content:'data'}},"
            , "        encoding:{"
            , "          x:{field:'x',type:'quantitative'},"
            , "          y:{field:'y',type:'quantitative'}"
            , "        }"
            , "      }"
            ]
      in band <> line
  where
    _ = (xCol, yCol)  -- 軸タイトルは点レイヤーで設定済み

-- 4 引数 zipWith
zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 f (a:as) (b:bs) (c:cs) (d:ds) = f a b c d : zipWith4 f as bs cs ds
zipWith4 _ _ _ _ _ = []
