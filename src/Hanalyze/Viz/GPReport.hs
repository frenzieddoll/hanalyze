{-# LANGUAGE OverloadedStrings #-}
-- | Comprehensive HTML report for GP regression.
--
-- Bundles data characteristics, model comparison, regression results,
-- interactive prediction and an appendix into a single file. Sliders for
-- the predictor variables let JavaScript update predictions and credible
-- intervals in real time.
--
-- @
-- let fits = [ makeGPFit "RBF"       RBF      optRBF  trainX trainY testX
--            , makeGPFit "Matérn5/2" Matern52 optM52  trainX trainY testX
--            ]
-- writeGPReport "report.html" (defaultGPReportConfig "My GP") trainData fits
-- @
module Hanalyze.Viz.GPReport
  ( GPReportConfig (..)
  , defaultGPReportConfig
  , GPModelFit (..)
  , makeGPFit
  , writeGPReport
  ) where

import Data.Aeson (encode)
import Data.ByteString.Lazy (toStrict)
import Data.List (sortBy)
import Data.Ord (comparing, Down (..))
import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (decodeUtf8)
import Graphics.Vega.VegaLite (fromVL)
import Numeric (showFFloat)

import Hanalyze.Model.GP
import Hanalyze.Viz.Assets (vegaJS, vegaLiteJS, vegaEmbedJS)
import Hanalyze.Viz.Core  (PlotConfig (..))
import Hanalyze.Viz.GP    (gpPlot)

-- ---------------------------------------------------------------------------
-- Public types
-- ---------------------------------------------------------------------------

data GPReportConfig = GPReportConfig
  { gpReportTitle :: Text   -- ^ レポートタイトル
  , gpXLabel      :: Text   -- ^ X 軸ラベル
  , gpYLabel      :: Text   -- ^ Y 軸ラベル
  } deriving (Show)

defaultGPReportConfig :: Text -> GPReportConfig
defaultGPReportConfig t = GPReportConfig t "x" "y"

-- | 1つのカーネルに対するフィット結果。
data GPModelFit = GPModelFit
  { fLabel    :: Text        -- ^ 表示ラベル (例: "RBF")
  , fKernel   :: Kernel
  , fParams   :: GPParams
  , fResult   :: GPResult
  , fLML      :: Double      -- ^ 対数周辺尤度
  , fPredData :: GPPredData  -- ^ JS 対話予測用データ
  } deriving (Show)

-- | フィット結果を計算してまとめる。
makeGPFit
  :: Text          -- ^ ラベル
  -> Kernel
  -> GPParams      -- ^ 最適化済みハイパーパラメータ
  -> [Double]      -- ^ 訓練 X
  -> [Double]      -- ^ 訓練 Y
  -> [Double]      -- ^ テスト X (予測グリッド)
  -> GPModelFit
makeGPFit lbl ker params trainX trainY testX =
  let model    = GPModel ker params
      res      = fitGP model trainX trainY testX
      lml      = logMarginalLikelihood trainX trainY ker params
      predData = gpPredData model trainX trainY
  in GPModelFit lbl ker params res lml predData

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

writeGPReport
  :: FilePath
  -> GPReportConfig
  -> [(Double, Double)]  -- ^ 訓練データ (x, y)
  -> [GPModelFit]
  -> IO ()
writeGPReport path cfg trainData fits =
  TIO.writeFile path (buildHtml cfg trainData sortedFits)
  where
    sortedFits = sortBy (comparing (Down . fLML)) fits

-- ---------------------------------------------------------------------------
-- HTML builder
-- ---------------------------------------------------------------------------

buildHtml :: GPReportConfig -> [(Double, Double)] -> [GPModelFit] -> Text
buildHtml cfg trainData fits = T.unlines $
  [ "<!DOCTYPE html>"
  , "<html lang=\"ja\">"
  , "<head>"
  , "  <meta charset=\"utf-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "  <title>" <> gpReportTitle cfg <> "</title>"
  , "  <script>" <> vegaJS      <> "</script>"
  , "  <script>" <> vegaLiteJS  <> "</script>"
  , "  <script>" <> vegaEmbedJS <> "</script>"
  , "  <style>"
  , css
  , "  </style>"
  , "</head>"
  , "<body>"
  , navBar cfg fits
  , "<main>"
  , dataSummarySection cfg trainData
  , modelComparisonSection fits
  , regressionSection cfg trainData fits
  , predictionSection cfg trainData fits
  , appendixSection fits
  , "</main>"
  , "<script>"
  , vegaEmbedScript fits
  , tabScript
  , predictionScript cfg trainData fits
  , smoothScrollScript
  , "</script>"
  , "</body>"
  , "</html>"
  ]

-- ---------------------------------------------------------------------------
-- CSS
-- ---------------------------------------------------------------------------

css :: Text
css = T.unlines
  [ "* { box-sizing: border-box; margin: 0; padding: 0; }"
  , "body { font-family: 'Segoe UI', system-ui, sans-serif; background: #f0f2f5; color: #333; line-height: 1.6; }"
  , "nav { position: sticky; top: 0; z-index: 100; background: #1a3a5c;"
  , "      padding: 10px 28px; display: flex; gap: 20px; align-items: center;"
  , "      box-shadow: 0 2px 6px rgba(0,0,0,.25); }"
  , "nav h1 { color: #ecf0f1; font-size: 1em; font-weight: 600; flex: 1; }"
  , ".nav-link { color: #9ab; text-decoration: none; font-size: .82em; white-space: nowrap; }"
  , ".nav-link:hover { color: #fff; }"
  , "main { max-width: 1100px; margin: 0 auto; padding: 32px 20px; }"
  , "section { background: white; border-radius: 12px; padding: 26px 28px;"
  , "          margin-bottom: 28px; box-shadow: 0 2px 10px rgba(0,0,0,.07); }"
  , "h2 { font-size: 1.05em; font-weight: 700; color: #1a3a5c; margin-bottom: 18px;"
  , "     border-bottom: 2px solid #e4e9f0; padding-bottom: 8px;"
  , "     display: flex; align-items: center; gap: 8px; }"
  , "h3 { font-size: .95em; font-weight: 600; color: #2c5; margin: 18px 0 10px; }"
  , ".sec-icon { font-size: 1.1em; }"
  , ".stat-grid { display: flex; gap: 14px; flex-wrap: wrap; margin-bottom: 20px; }"
  , ".stat-box { background: #f7f9fc; border: 1px solid #e4e9f0; border-radius: 10px;"
  , "            padding: 14px 20px; min-width: 120px; text-align: center; }"
  , ".stat-box .lbl { font-size: .72em; color: #888; text-transform: uppercase;"
  , "                 letter-spacing: .05em; margin-bottom: 4px; }"
  , ".stat-box .val { font-size: 1.35em; font-weight: 700; color: #1a3a5c; }"
  , ".stat-box.highlight { background: #e8f4e8; border-color: #4caf50; }"
  , ".stat-box.highlight .val { color: #2e7d32; }"
  , "table { width: 100%; border-collapse: collapse; font-size: .88em; }"
  , "thead tr { background: #f0f4f8; }"
  , "th { padding: 9px 14px; text-align: right; font-weight: 600; color: #444; }"
  , "th:first-child { text-align: left; }"
  , "td { padding: 8px 14px; border-bottom: 1px solid #f0f2f5; text-align: right; font-family: monospace; }"
  , "td:first-child { text-align: left; font-family: inherit; font-weight: 500; }"
  , "tr:last-child td { border-bottom: none; }"
  , "tr.best-row td { background: #f0faf0; font-weight: 600; }"
  , ".vl-wrap { overflow-x: auto; }"
  , ".tab-bar { display: flex; gap: 6px; margin-bottom: 18px; flex-wrap: wrap; }"
  , ".tab-btn { padding: 7px 18px; border: 1.5px solid #c0ccd8; border-radius: 20px;"
  , "           background: white; color: #555; cursor: pointer; font-size: .88em;"
  , "           transition: all .15s; }"
  , ".tab-btn:hover { border-color: #1a3a5c; color: #1a3a5c; }"
  , ".tab-btn.active { background: #1a3a5c; color: white; border-color: #1a3a5c; }"
  , ".tab-content { display: none; }"
  , ".tab-content.active { display: block; }"
  , ".predict-controls { background: #f7f9fc; border-radius: 10px; padding: 20px 24px; margin-bottom: 20px; }"
  , ".slider-row { display: flex; align-items: center; gap: 16px; margin-bottom: 14px; flex-wrap: wrap; }"
  , ".slider-row label { font-size: .9em; color: #555; min-width: 80px; }"
  , "input[type=range] { flex: 1; min-width: 200px; accent-color: #1a3a5c; }"
  , "input[type=number] { width: 110px; padding: 6px 10px; border: 1.5px solid #c0ccd8;"
  , "                     border-radius: 6px; font-size: .9em; }"
  , "select { padding: 7px 12px; border: 1.5px solid #c0ccd8; border-radius: 6px;"
  , "         font-size: .88em; background: white; }"
  , ".predict-output { display: flex; gap: 14px; flex-wrap: wrap; margin-top: 6px; }"
  , ".pred-box { flex: 1; min-width: 160px; background: white; border: 1.5px solid #e4e9f0;"
  , "            border-radius: 10px; padding: 14px 18px; text-align: center; }"
  , ".pred-box .plbl { font-size: .75em; color: #888; text-transform: uppercase; letter-spacing: .05em; }"
  , ".pred-box .pval { font-size: 1.4em; font-weight: 700; color: #1a3a5c; margin: 4px 0; }"
  , ".pred-box .psub { font-size: .78em; color: #888; }"
  , ".pred-box.mean-box { border-color: #1a3a5c; }"
  , ".pred-box.mean-box .pval { color: #1a3a5c; }"
  , ".appendix-block { background: #f7f9fc; border-left: 4px solid #1a3a5c;"
  , "                  padding: 14px 18px; margin: 14px 0; border-radius: 0 8px 8px 0; }"
  , ".appendix-block h4 { font-size: .9em; font-weight: 700; color: #1a3a5c; margin-bottom: 6px; }"
  , ".appendix-block p, .appendix-block li { font-size: .88em; color: #444; margin-bottom: 4px; }"
  , "code { background: #f0f2f5; padding: 2px 6px; border-radius: 4px; font-size: .9em; }"
  , ".formula { background: #f7f9fc; border: 1px solid #e4e9f0; border-radius: 8px;"
  , "           padding: 12px 16px; margin: 10px 0; font-family: monospace; font-size: .88em; color: #333; }"
  , ".kernel-badge { display: inline-block; padding: 2px 10px; border-radius: 12px;"
  , "                font-size: .78em; font-weight: 600; background: #e8f0fe; color: #1a3a5c; }"
  , ".best-badge { background: #e8f4e8; color: #2e7d32; margin-left: 6px; }"
  ]

-- ---------------------------------------------------------------------------
-- Nav bar
-- ---------------------------------------------------------------------------

navBar :: GPReportConfig -> [GPModelFit] -> Text
navBar cfg _ = T.unlines
  [ "<nav>"
  , "  <h1>&#128202; " <> gpReportTitle cfg <> "</h1>"
  , "  <a class=\"nav-link\" href=\"#sec-data\">データ</a>"
  , "  <a class=\"nav-link\" href=\"#sec-models\">モデル比較</a>"
  , "  <a class=\"nav-link\" href=\"#sec-results\">回帰結果</a>"
  , "  <a class=\"nav-link\" href=\"#sec-predict\">予測</a>"
  , "  <a class=\"nav-link\" href=\"#sec-appendix\">付録</a>"
  , "</nav>"
  ]

-- ---------------------------------------------------------------------------
-- Section 1: Data Summary
-- ---------------------------------------------------------------------------

dataSummarySection :: GPReportConfig -> [(Double, Double)] -> Text
dataSummarySection cfg trainData = T.unlines $
  [ "<section id=\"sec-data\">"
  , "  <h2><span class=\"sec-icon\">&#128202;</span> 1. データの特性</h2>"
  , "  <div class=\"stat-grid\">"
  , statBox "N (観測数)" (T.pack (show n)) False
  , statBox "X 最小値"  (fmt4 xMin) False
  , statBox "X 最大値"  (fmt4 xMax) False
  , statBox "X 平均"    (fmt4 xMean) False
  , statBox "X 標準偏差" (fmt4 xStd) False
  , statBox "Y 最小値"  (fmt4 yMin) False
  , statBox "Y 最大値"  (fmt4 yMax) False
  , statBox "Y 平均"    (fmt4 yMean) False
  , statBox "Y 標準偏差" (fmt4 yStd) False
  , "  </div>"
  , "  <div class=\"vl-wrap\"><div id=\"vl-data\"></div></div>"
  , "  <script>window.__vlData = " <> scatterSpecJson cfg trainData <> ";</script>"
  , "</section>"
  ]
  where
    (xs, ys) = unzip trainData
    n     = length xs
    xMin  = minimum xs;  xMax  = maximum xs
    yMin  = minimum ys;  yMax  = maximum ys
    xMean = sum xs / fromIntegral n
    yMean = sum ys / fromIntegral n
    xStd  = sqrt (sum (map (\x -> (x - xMean)^(2::Int)) xs) / fromIntegral n)
    yStd  = sqrt (sum (map (\y -> (y - yMean)^(2::Int)) ys) / fromIntegral n)

-- 訓練データだけの散布図 Vega-Lite JSON
scatterSpecJson :: GPReportConfig -> [(Double, Double)] -> Text
scatterSpecJson cfg trainData =
  let (xs, ys) = unzip trainData
      xl = gpXLabel cfg
      yl = gpYLabel cfg
      spec = toVegaLitePure
               [ ("$schema", "\"https://vega.github.io/schema/vega-lite/v5.json\"")
               , ("title",   "\"Training Data\"")
               , ("width",   "600")
               , ("height",  "240")
               , ("data",    mkDataJson xl yl xs ys)
               , ("mark",    "{\"type\":\"point\",\"tooltip\":true,\"size\":50,\"color\":\"#1a3a5c\"}")
               , ("encoding", mkEncJson xl yl)
               ]
  in spec

-- 簡易 Vega-Lite JSON ビルダー（hvega を使わない生JSONアプローチ）
toVegaLitePure :: [(Text, Text)] -> Text
toVegaLitePure pairs = "{" <> T.intercalate "," (map kv pairs) <> "}"
  where kv (k, v) = "\"" <> k <> "\":" <> v

mkDataJson :: Text -> Text -> [Double] -> [Double] -> Text
mkDataJson xl yl xs ys =
  let rows = zipWith mkRow xs ys
      mkRow x y = "{\"" <> xl <> "\":" <> fmtJS x <> ",\"" <> yl <> "\":" <> fmtJS y <> "}"
  in "{\"values\":[" <> T.intercalate "," rows <> "]}"

mkEncJson :: Text -> Text -> Text
mkEncJson xl yl = T.unlines
  [ "{"
  , "  \"x\": {\"field\": \"" <> xl <> "\", \"type\": \"quantitative\","
  , "          \"axis\": {\"title\": \"" <> xl <> "\"}},"
  , "  \"y\": {\"field\": \"" <> yl <> "\", \"type\": \"quantitative\","
  , "          \"axis\": {\"title\": \"" <> yl <> "\"}}"
  , "}"
  ]

-- ---------------------------------------------------------------------------
-- Section 2: Model Comparison
-- ---------------------------------------------------------------------------

modelComparisonSection :: [GPModelFit] -> Text
modelComparisonSection fits = T.unlines
  [ "<section id=\"sec-models\">"
  , "  <h2><span class=\"sec-icon\">&#9878;</span> 2. モデル比較</h2>"
  , "  <p style=\"font-size:.88em;color:#666;margin-bottom:14px\">"
  , "    対数周辺尤度 (LML) が高いほどデータへの適合が良い。ハイパーパラメータは自動最適化済み。"
  , "  </p>"
  , "  <table>"
  , "    <thead><tr>"
  , "      <th>カーネル</th>"
  , "      <th>ℓ (長さスケール)</th>"
  , "      <th>σ_f (シグナル)</th>"
  , "      <th>σ_n (ノイズ)</th>"
  , "      <th>p (周期)</th>"
  , "      <th>LML ↑</th>"
  , "      <th>順位</th>"
  , "    </tr></thead>"
  , "    <tbody>"
  , T.concat (zipWith (modelRow bestLML) [1..] fits)
  , "    </tbody>"
  , "  </table>"
  , "  <p style=\"margin-top:12px;font-size:.82em;color:#888\">"
  , "    LML = 対数周辺尤度 log p(y | X, θ)。モデル複雑度へのペナルティを含む。"
  , "  </p>"
  , "</section>"
  ]
  where
    bestLML = maximum (map fLML fits)

    modelRow best rank fit =
      let isBest = fLML fit == best
          rowCls = if isBest then " class=\"best-row\"" else ""
          hasPeriod = fKernel fit == Periodic
          pCell = if hasPeriod
                  then td (fmt4 (gpPeriod (fParams fit)))
                  else td "—"
          badge = if isBest
                  then " <span class=\"kernel-badge best-badge\">&#11088; Best</span>"
                  else ""
      in T.unlines
           [ "      <tr" <> rowCls <> ">"
           , "        <td>" <> fLabel fit <> badge <> "</td>"
           , td (fmt4 (gpLengthScale (fParams fit)))
           , td (fmt4 (sqrt (gpSignalVar (fParams fit))))
           , td (fmt6 (sqrt (gpNoiseVar (fParams fit))))
           , pCell
           , td (fmt2 (fLML fit))
           , td ("#" <> T.pack (show (rank :: Int)))
           , "      </tr>"
           ]

td :: Text -> Text
td v = "        <td>" <> v <> "</td>"

-- ---------------------------------------------------------------------------
-- Section 3: Regression Results
-- ---------------------------------------------------------------------------

regressionSection :: GPReportConfig -> [(Double, Double)] -> [GPModelFit] -> Text
regressionSection cfg trainData fits = T.unlines $
  [ "<section id=\"sec-results\">"
  , "  <h2><span class=\"sec-icon\">&#128200;</span> 3. 回帰結果</h2>"
  , "  <p style=\"font-size:.88em;color:#666;margin-bottom:14px\">"
  , "    青い帯 = 平均 ± 2σ (≈95% 信用区間)。黒点 = 訓練データ。"
  , "  </p>"
  , "  <div class=\"tab-bar\">"
  ] ++
  zipWith (tabBtn fits) [0..] fits ++
  [ "  </div>" ] ++
  concatMap (tabContent cfg trainData) (zip [0..] fits) ++
  [ "</section>" ]

tabBtn :: [GPModelFit] -> Int -> GPModelFit -> Text
tabBtn fits i fit =
  let bestLML = maximum (map fLML fits)
      star    = if fLML fit == bestLML then " &#11088;" else ""
      active  = if i == 0 then " active" else ""
  in "  <button class=\"tab-btn" <> active <> "\" onclick=\"showTab(" <> T.pack (show i) <> ")\">"
     <> fLabel fit <> star <> "</button>"

tabContent :: GPReportConfig -> [(Double, Double)] -> (Int, GPModelFit) -> [Text]
tabContent cfg trainData (i, fit) =
  let active = if i == 0 then " active" else ""
      xl  = gpXLabel cfg
      yl  = gpYLabel cfg
      pCfg = PlotConfig
               { plotTitle  = fLabel fit <> " — GP Regression"
               , plotWidth  = 700
               , plotHeight = 320
               }
      spec = gpPlot pCfg xl yl trainData (fResult fit)
      json = decodeUtf8 . toStrict . encode . fromVL $ spec
      divId = "vl-fit-" <> T.pack (show i)
  in [ "  <div id=\"tab-" <> T.pack (show i) <> "\" class=\"tab-content" <> active <> "\">"
     , "    <div class=\"vl-wrap\"><div id=\"" <> divId <> "\"></div></div>"
     , "    <script>window.__vlFit" <> T.pack (show i) <> " = " <> json <> ";</script>"
     , "    " <> fitParamSummary fit
     , "  </div>"
     ]

fitParamSummary :: GPModelFit -> Text
fitParamSummary fit = T.unlines
  [ "    <div style=\"margin-top:16px;background:#f7f9fc;border-radius:8px;padding:12px 16px;"
  , "         display:flex;gap:20px;flex-wrap:wrap;font-size:.85em;\">"
  , "      <span><b>カーネル:</b> " <> fLabel fit <> "</span>"
  , "      <span><b>ℓ =</b> " <> fmt4 (gpLengthScale (fParams fit)) <> "</span>"
  , "      <span><b>σ_f =</b> " <> fmt4 (sqrt (gpSignalVar (fParams fit))) <> "</span>"
  , "      <span><b>σ_n =</b> " <> fmt6 (sqrt (gpNoiseVar (fParams fit))) <> "</span>"
  , if fKernel fit == Periodic
      then "      <span><b>p =</b> " <> fmt4 (gpPeriod (fParams fit)) <> "</span>"
      else ""
  , "      <span style=\"margin-left:auto;color:#888\"><b>LML =</b> " <> fmt2 (fLML fit) <> "</span>"
  , "    </div>"
  ]

-- ---------------------------------------------------------------------------
-- Section 4: Interactive Prediction
-- ---------------------------------------------------------------------------

predictionSection :: GPReportConfig -> [(Double, Double)] -> [GPModelFit] -> Text
predictionSection cfg trainData fits =
  let (xs, _) = unzip trainData
      xMin = minimum xs
      xMax = maximum xs
      xMid = (xMin + xMax) / 2
  in T.unlines
  [ "<section id=\"sec-predict\">"
  , "  <h2><span class=\"sec-icon\">&#127919;</span> 4. 対話的予測</h2>"
  , "  <p style=\"font-size:.88em;color:#666;margin-bottom:18px\">"
  , "    スライダーまたは入力欄で説明変数 x の値を変えると、選択モデルの予測値をリアルタイムで計算します。"
  , "  </p>"
  , "  <div class=\"predict-controls\">"
  , "    <div class=\"slider-row\">"
  , "      <label>モデル:</label>"
  , "      <select id=\"pred-kernel\" onchange=\"updatePrediction()\">"
  , T.concat (zipWith modelOption [0..] fits)
  , "      </select>"
  , "    </div>"
  , "    <div class=\"slider-row\">"
  , "      <label>" <> gpXLabel cfg <> " 値:</label>"
  , "      <input type=\"range\" id=\"x-slider\""
  , "             min=\"" <> fmtJS xMin <> "\" max=\"" <> fmtJS xMax <> "\""
  , "             step=\"" <> fmtJS ((xMax - xMin) / 500) <> "\""
  , "             value=\"" <> fmtJS xMid <> "\""
  , "             oninput=\"syncXFromSlider()\">"
  , "      <input type=\"number\" id=\"x-num\""
  , "             min=\"" <> fmtJS xMin <> "\" max=\"" <> fmtJS xMax <> "\""
  , "             step=\"" <> fmtJS ((xMax - xMin) / 500) <> "\""
  , "             value=\"" <> fmtJS xMid <> "\""
  , "             onchange=\"syncXFromInput()\">"
  , "    </div>"
  , "    <div class=\"slider-row\">"
  , "      <label>現在の " <> gpXLabel cfg <> ":</label>"
  , "      <span id=\"x-current\" style=\"font-size:1.1em;font-weight:700;color:#1a3a5c\">"
  , "        " <> fmtJS xMid
  , "      </span>"
  , "    </div>"
  , "  </div>"
  , "  <div class=\"predict-output\">"
  , "    <div class=\"pred-box mean-box\">"
  , "      <div class=\"plbl\">予測値 (事後平均)</div>"
  , "      <div class=\"pval\" id=\"pred-mean\">—</div>"
  , "      <div class=\"psub\">" <> gpYLabel cfg <> "</div>"
  , "    </div>"
  , "    <div class=\"pred-box\">"
  , "      <div class=\"plbl\">標準偏差 (σ)</div>"
  , "      <div class=\"pval\" id=\"pred-std\">—</div>"
  , "      <div class=\"psub\">事後不確実性</div>"
  , "    </div>"
  , "    <div class=\"pred-box\">"
  , "      <div class=\"plbl\">95% 信用区間 下限</div>"
  , "      <div class=\"pval\" id=\"pred-lo\">—</div>"
  , "      <div class=\"psub\">平均 − 2σ</div>"
  , "    </div>"
  , "    <div class=\"pred-box\">"
  , "      <div class=\"plbl\">95% 信用区間 上限</div>"
  , "      <div class=\"pval\" id=\"pred-hi\">—</div>"
  , "      <div class=\"psub\">平均 + 2σ</div>"
  , "    </div>"
  , "  </div>"
  , "</section>"
  ]

modelOption :: Int -> GPModelFit -> Text
modelOption i fit =
  "      <option value=\"" <> T.pack (show i) <> "\">"
  <> fLabel fit <> " (LML=" <> fmt2 (fLML fit) <> ")"
  <> "</option>\n"

-- ---------------------------------------------------------------------------
-- Section 5: Appendix
-- ---------------------------------------------------------------------------

appendixSection :: [GPModelFit] -> Text
appendixSection fits = T.unlines
  [ "<section id=\"sec-appendix\">"
  , "  <h2><span class=\"sec-icon\">&#128218;</span> 付録: GP 回帰の原理</h2>"
  , appendixGP
  , appendixKernels fits
  , appendixHyperparams
  , appendixLML
  , "</section>"
  ]

appendixGP :: Text
appendixGP = T.unlines
  [ "  <div class=\"appendix-block\">"
  , "    <h4>ガウス過程 (Gaussian Process) とは</h4>"
  , "    <p>ガウス過程は関数に対する確率分布です。有限個の点での関数値が常に多変量正規分布に従うとき、"
  , "    その関数の分布をガウス過程と呼びます。</p>"
  , "    <p>平均関数 m(x) と共分散関数 (カーネル) k(x, x') によって定義されます:</p>"
  , "    <div class=\"formula\">f(x) ~ GP( m(x), k(x, x') )</div>"
  , "    <p>訓練データ (X, y) を条件付けることで事後分布が計算できます:</p>"
  , "    <div class=\"formula\">"
  , "    事後平均:   μ(x*) = K(x*, X) · [K(X,X) + σ²_n I]⁻¹ · y<br>"
  , "    事後分散:   σ²(x*) = k(x*, x*) − K(x*, X) · [K(X,X) + σ²_n I]⁻¹ · K(X, x*)"
  , "    </div>"
  , "    <p>この実装では hmatrix (LAPACK/BLAS) でコレスキー分解を行い数値的安定性を確保しています。</p>"
  , "  </div>"
  ]

appendixKernels :: [GPModelFit] -> Text
appendixKernels fits = T.unlines $
  [ "  <div class=\"appendix-block\">"
  , "    <h4>使用したカーネル関数</h4>"
  ] ++
  concatMap kernelDesc usedKernels ++
  [ "  </div>" ]
  where
    usedKernels = map fKernel fits

    kernelDesc RBF =
      [ "    <p><b>RBF (Squared Exponential / 二乗指数カーネル)</b></p>"
      , "    <div class=\"formula\">k(x, x') = σ²_f · exp( −(x−x')² / (2ℓ²) )</div>"
      , "    <p>無限回微分可能な滑らかな関数をモデル化します。最も広く使われるカーネル。</p>"
      ]
    kernelDesc Matern52 =
      [ "    <p><b>Matérn 5/2 カーネル</b></p>"
      , "    <div class=\"formula\">k(x, x') = σ²_f · (1 + √5·r/ℓ + 5r²/(3ℓ²)) · exp(−√5·r/ℓ) &nbsp; (r = |x−x'|)</div>"
      , "    <p>RBF より少し荒れた関数に対応。物理・気象・機械学習でよく使われます。</p>"
      ]
    kernelDesc Periodic =
      [ "    <p><b>Periodic カーネル</b></p>"
      , "    <div class=\"formula\">k(x, x') = σ²_f · exp( −2 sin²(π|x−x'|/p) / ℓ² )</div>"
      , "    <p>周期 p の周期的パターンを持つ関数をモデル化します。</p>"
      ]

appendixHyperparams :: Text
appendixHyperparams = T.unlines
  [ "  <div class=\"appendix-block\">"
  , "    <h4>ハイパーパラメータの意味</h4>"
  , "    <table>"
  , "      <thead><tr><th>パラメータ</th><th style=\"text-align:left\">意味</th><th>影響</th></tr></thead>"
  , "      <tbody>"
  , "        <tr><td>ℓ (長さスケール)</td><td style=\"text-align:left\">関数の「滑らかさの範囲」</td><td style=\"text-align:left\">大きい → 広範囲で相関、小さい → 局所的</td></tr>"
  , "        <tr><td>σ_f (シグナル標準偏差)</td><td style=\"text-align:left\">関数値の変動幅</td><td style=\"text-align:left\">大きい → 振れ幅が大きい関数</td></tr>"
  , "        <tr><td>σ_n (ノイズ標準偏差)</td><td style=\"text-align:left\">観測ノイズの大きさ</td><td style=\"text-align:left\">小さい → 補間、大きい → 平滑化</td></tr>"
  , "        <tr><td>p (周期、Periodicのみ)</td><td style=\"text-align:left\">パターンの繰り返し周期</td><td style=\"text-align:left\">データの周期に合わせて設定</td></tr>"
  , "      </tbody>"
  , "    </table>"
  , "  </div>"
  ]

appendixLML :: Text
appendixLML = T.unlines
  [ "  <div class=\"appendix-block\">"
  , "    <h4>対数周辺尤度 (Log Marginal Likelihood, LML) によるモデル選択</h4>"
  , "    <div class=\"formula\">"
  , "    log p(y | X, θ) = −½ yᵀ K⁻¹_y y − ½ log|K_y| − n/2 · log(2π)"
  , "    </div>"
  , "    <p>LML はデータへの当てはまり (第1項) とモデル複雑度ペナルティ (第2項) のバランスを自動的に取ります。</p>"
  , "    <p>この実装では log-space で数値勾配上昇法 (400ステップ) によりハイパーパラメータを最適化しています。</p>"
  , "  </div>"
  ]

-- ---------------------------------------------------------------------------
-- JavaScript
-- ---------------------------------------------------------------------------

-- Vega-Lite の embed 呼び出し
vegaEmbedScript :: [GPModelFit] -> Text
vegaEmbedScript fits = T.unlines $
  [ "vegaEmbed('#vl-data', window.__vlData, {renderer:'canvas',actions:false}).catch(console.error);" ] ++
  [ "vegaEmbed('#vl-fit-" <> T.pack (show i) <> "', window.__vlFit" <> T.pack (show i)
    <> ", {renderer:'canvas',actions:false}).catch(console.error);"
  | i <- [0 .. length fits - 1]
  ]

-- タブ切り替え
tabScript :: Text
tabScript = T.unlines
  [ "function showTab(idx) {"
  , "  document.querySelectorAll('.tab-content').forEach((el,i) => {"
  , "    el.classList.toggle('active', i === idx);"
  , "  });"
  , "  document.querySelectorAll('.tab-btn').forEach((el,i) => {"
  , "    el.classList.toggle('active', i === idx);"
  , "  });"
  , "}"
  ]

-- 対話予測 JS
predictionScript :: GPReportConfig -> [(Double, Double)] -> [GPModelFit] -> Text
predictionScript _cfg _trainData fits = T.unlines $
  [ "// ---- GP prediction data ----"
  , "const gpModels = " <> jsModelsArray fits <> ";"
  , ""
  , "// カーネル評価関数"
  , "function kernelEval(ker, p, x1, x2) {"
  , "  if (ker === 'rbf') {"
  , "    const d = x1 - x2, l = p.ell;"
  , "    return p.sf2 * Math.exp(-(d*d) / (2*l*l));"
  , "  } else if (ker === 'matern52') {"
  , "    const d = Math.abs(x1 - x2), l = p.ell;"
  , "    const s = Math.sqrt(5) * d / l;"
  , "    return p.sf2 * (1 + s + s*s/3) * Math.exp(-s);"
  , "  } else { // periodic"
  , "    const d = Math.abs(x1 - x2);"
  , "    const s = Math.sin(Math.PI * d / p.period);"
  , "    return p.sf2 * Math.exp(-2 * s*s / (p.ell * p.ell));"
  , "  }"
  , "}"
  , ""
  , "// GP 事後予測"
  , "function gpPredict(modelIdx, xStar) {"
  , "  const m = gpModels[modelIdx];"
  , "  const kStar = m.trainX.map(xi => kernelEval(m.kernel, m.params, xi, xStar));"
  , "  const mean  = kStar.reduce((s, k, i) => s + k * m.alpha[i], 0);"
  , "  const v     = m.kyInv.map(row => row.reduce((s, v, j) => s + v * kStar[j], 0));"
  , "  const kss   = kernelEval(m.kernel, m.params, xStar, xStar);"
  , "  const variance = Math.max(0, kss - kStar.reduce((s, k, i) => s + k * v[i], 0));"
  , "  return { mean, std: Math.sqrt(variance) };"
  , "}"
  , ""
  , "function updatePrediction() {"
  , "  const xStar = parseFloat(document.getElementById('x-slider').value);"
  , "  const midx  = parseInt(document.getElementById('pred-kernel').value);"
  , "  const { mean, std } = gpPredict(midx, xStar);"
  , "  document.getElementById('x-current').textContent = xStar.toFixed(5);"
  , "  document.getElementById('pred-mean').textContent = mean.toFixed(5);"
  , "  document.getElementById('pred-std').textContent  = std.toFixed(5);"
  , "  document.getElementById('pred-lo').textContent   = (mean - 2*std).toFixed(5);"
  , "  document.getElementById('pred-hi').textContent   = (mean + 2*std).toFixed(5);"
  , "}"
  , ""
  , "function syncXFromSlider() {"
  , "  const v = document.getElementById('x-slider').value;"
  , "  document.getElementById('x-num').value = parseFloat(v).toFixed(6);"
  , "  updatePrediction();"
  , "}"
  , ""
  , "function syncXFromInput() {"
  , "  const v = parseFloat(document.getElementById('x-num').value);"
  , "  document.getElementById('x-slider').value = v;"
  , "  updatePrediction();"
  , "}"
  , ""
  , "updatePrediction();"
  ]

smoothScrollScript :: Text
smoothScrollScript = T.unlines
  [ "document.querySelectorAll('.nav-link').forEach(a => {"
  , "  a.addEventListener('click', e => {"
  , "    e.preventDefault();"
  , "    const target = document.querySelector(a.getAttribute('href'));"
  , "    if (target) target.scrollIntoView({ behavior: 'smooth' });"
  , "  });"
  , "});"
  ]

-- ---------------------------------------------------------------------------
-- JS data serialisation
-- ---------------------------------------------------------------------------

jsModelsArray :: [GPModelFit] -> Text
jsModelsArray fits = "[" <> T.intercalate "," (map jsModel fits) <> "]"

jsModel :: GPModelFit -> Text
jsModel fit = T.unlines
  [ "{"
  , "  kernel: '" <> jsKernelId (fKernel fit) <> "',"
  , "  params: " <> jsParams (fKernel fit) (fParams fit) <> ","
  , "  trainX: " <> jsDoubleArray (pdTrainX (fPredData fit)) <> ","
  , "  alpha:  " <> jsDoubleArray (pdAlpha  (fPredData fit)) <> ","
  , "  kyInv:  " <> jsMatrix      (pdKyInv  (fPredData fit))
  , "}"
  ]

jsKernelId :: Kernel -> Text
jsKernelId RBF      = "rbf"
jsKernelId Matern52 = "matern52"
jsKernelId Periodic = "periodic"

jsParams :: Kernel -> GPParams -> Text
jsParams ker p = "{ell:" <> fmtJS (gpLengthScale p)
              <> ",sf2:"  <> fmtJS (gpSignalVar   p)
              <> ",sn2:"  <> fmtJS (gpNoiseVar    p)
              <> if ker == Periodic then ",period:" <> fmtJS (gpPeriod p) else ""
              <> "}"

jsDoubleArray :: [Double] -> Text
jsDoubleArray xs = "[" <> T.intercalate "," (map fmtJS xs) <> "]"

jsMatrix :: [[Double]] -> Text
jsMatrix rows = "[" <> T.intercalate "," (map jsDoubleArray rows) <> "]"

-- ---------------------------------------------------------------------------
-- Formatting helpers
-- ---------------------------------------------------------------------------

-- | Double を JavaScript 数値リテラルに変換 (10桁精度)。
fmtJS :: Double -> Text
fmtJS v
  | isNaN v      = "0"
  | isInfinite v = if v > 0 then "1e308" else "-1e308"
  | otherwise    = T.pack (showFFloat (Just 10) v "")

fmt2 :: Double -> Text
fmt2 v = T.pack (showFFloat (Just 2) v "")

fmt4 :: Double -> Text
fmt4 v = T.pack (showFFloat (Just 4) v "")

fmt6 :: Double -> Text
fmt6 v = T.pack (showFFloat (Just 6) v "")

statBox :: Text -> Text -> Bool -> Text
statBox lbl val highlight = T.unlines
  [ "    <div class=\"stat-box" <> (if highlight then " highlight" else "") <> "\">"
  , "      <div class=\"lbl\">" <> lbl <> "</div>"
  , "      <div class=\"val\">" <> val <> "</div>"
  , "    </div>"
  ]
