{-# LANGUAGE OverloadedStrings #-}
-- | コンポジション型 HTML レポートビルダ。
--
-- 既存の 'Viz.AnalysisReport' は LM/GLM/GLMM/GP/HBM 専用で密結合だったため、
-- ridge / kernel / spline / RobustGP / Taguchi など多様なモデル/分析にも
-- 統一 API を提供する。
--
-- 設計原則:
-- - 'ReportSection' は HTML 1 セクションを表す sum type
-- - ユーザー (CLI / library 利用者) は @[ReportSection]@ を構築するだけ
-- - 'renderReport' が並べて 1 つの自己完結 HTML を生成 (Vega-Lite アセット込)
-- - 'Reportable' typeclass で各 fit 型から既定セクション群を生成可能
--
-- 利用例:
--
-- @
-- import Viz.ReportBuilder
-- renderReport "out.html" (defaultReportConfig "My Analysis")
--   [ secDataOverview df ["x"] "y"
--   , secModelOverview "Ridge regression" "y = β₀ + β₁x" Nothing
--   , secCoefficients [("β₀", 1.2), ("β₁", 2.4)] (Just ("R²", 0.96))
--   , secFitScatter "x" "y" xs ys (Just smooth)
--   , secResiduals fitted resids
--   ]
-- @
module Viz.ReportBuilder
  ( -- * 設定
    ReportConfig (..)
  , defaultReportConfig
    -- * セクション
  , ReportSection (..)
  , SmoothCurve (..)
    -- * セクションビルダ (smart constructors)
  , secDataOverview
  , secModelOverview
  , secKeyValue
  , secCoefficients
  , secFitScatter
  , secResiduals
  , secBarChart
  , secVega
  , secMermaid
  , secTable
  , secMarkdown
  , secHtml
    -- * MCMC / 事後分布関連 (Phase F)
  , secMCMCDiagnostics
  , secMCMCDiagnosticsMulti
  , secMCMCAutocorr
  , secMCMCPair
  , secPosteriorSummary
    -- * 対話的予測 (LM/GLM 用)
  , secInteractiveLM
    -- * レンダリング
  , renderReport
    -- * Reportable typeclass
  , Reportable (..)
    -- * 専用 Vega-Lite ヘルパ (regularization path 等)
  , regPathSpec
  ) where

import Data.Aeson (encode)
import Data.ByteString.Lazy (toStrict)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (decodeUtf8)
import Graphics.Vega.VegaLite hiding (filter, name)
import Numeric (showFFloat)
import qualified Data.Vector as V
import Text.Printf (printf)

import DataFrame.Core
import MCMC.Core (Chain)
import Viz.Assets (vegaJS, vegaLiteJS, vegaEmbedJS)
import Viz.Core (PlotConfig (..), defaultConfig)
import qualified Viz.MCMC as VM

-- ---------------------------------------------------------------------------
-- 設定
-- ---------------------------------------------------------------------------

data ReportConfig = ReportConfig
  { rcTitle    :: Text   -- ^ レポート見出し (上部 + <title>)
  , rcSubtitle :: Text   -- ^ サブタイトル (空文字なら非表示)
  } deriving (Show)

defaultReportConfig :: Text -> ReportConfig
defaultReportConfig t = ReportConfig t ""

-- ---------------------------------------------------------------------------
-- セクション型
-- ---------------------------------------------------------------------------

-- | 滑らか曲線データ (信頼帯付き)。
data SmoothCurve = SmoothCurve
  { scXs    :: [Double]
  , scYs    :: [Double]
  , scLower :: [Double]   -- ^ 空リストならバンドなし
  , scUpper :: [Double]
  } deriving (Show, Eq)

-- | レポート 1 セクション。
data ReportSection
  = -- | データ概要: 列ごとの型/N/min/max/mean/SD + ヒストグラム
    SecDataOverview DataFrame [Text] Text
    -- | モデル概要: タイトル / 説明 / オプションの Mermaid DAG
  | SecModelOverview Text Text (Maybe Text)
    -- | 係数表: ラベル/値 + オプションの (R² ラベル, 値)
  | SecCoefficients [(Text, Double)] (Maybe (Text, Double))
    -- | 散布図 + 滑らか曲線 (信頼帯あれば描画)
  | SecFitScatter Text Text [Double] [Double] (Maybe SmoothCurve)
    -- | 残差プロット (fitted vs residuals + Predicted vs Actual)
  | SecResiduals [Double] [Double]
    -- | 棒グラフ (要因効果や lambda パスなど)
  | SecBarChart Text [(Text, Double)]
    -- | 任意の Vega-Lite チャート
  | SecVega Text VegaLite
    -- | Mermaid.js DAG
  | SecMermaid Text
    -- | 任意テーブル: ヘッダ / 行
  | SecTable Text [Text] [[Text]]
    -- | "key: value" 形式の小テーブル
  | SecKeyValue Text [(Text, Text)]
    -- | Markdown 風テキスト (実体は <p> 内 plain HTML)
  | SecMarkdown Text Text
    -- | raw HTML 本体 (escape hatch)
  | SecHtml Text Text
    -- | 単変数 LM/GLM の対話的予測 (スライダー + リアルタイム scatter)。
    --   フィールド: title / xCol / yCol / xs / ys / smooth / (xSliderMin, xSliderMax)
  | SecInteractiveLM Text Text Text [Double] [Double] SmoothCurve (Double, Double)

-- ---------------------------------------------------------------------------
-- ビルダ
-- ---------------------------------------------------------------------------

secDataOverview :: DataFrame -> [Text] -> Text -> ReportSection
secDataOverview = SecDataOverview

secModelOverview :: Text -> Text -> Maybe Text -> ReportSection
secModelOverview = SecModelOverview

secKeyValue :: Text -> [(Text, Text)] -> ReportSection
secKeyValue = SecKeyValue

secCoefficients :: [(Text, Double)] -> Maybe (Text, Double) -> ReportSection
secCoefficients = SecCoefficients

secFitScatter :: Text -> Text -> [Double] -> [Double]
              -> Maybe SmoothCurve -> ReportSection
secFitScatter = SecFitScatter

secResiduals :: [Double] -> [Double] -> ReportSection
secResiduals = SecResiduals

secBarChart :: Text -> [(Text, Double)] -> ReportSection
secBarChart = SecBarChart

secVega :: Text -> VegaLite -> ReportSection
secVega = SecVega

secMermaid :: Text -> ReportSection
secMermaid = SecMermaid

secTable :: Text -> [Text] -> [[Text]] -> ReportSection
secTable = SecTable

secMarkdown :: Text -> Text -> ReportSection
secMarkdown = SecMarkdown

secHtml :: Text -> Text -> ReportSection
secHtml = SecHtml

-- ---------------------------------------------------------------------------
-- MCMC セクションビルダ (Viz.MCMC のラッパ)
-- ---------------------------------------------------------------------------

-- | 単一チェーンの MCMC 診断 (KDE + トレース)。
secMCMCDiagnostics :: Text       -- ^ セクションタイトル
                   -> [Text]     -- ^ パラメータ名
                   -> Chain
                   -> ReportSection
secMCMCDiagnostics title params chain =
  SecVega title (VM.mcmcDiagnostics (defaultConfig title) params chain)

-- | 多チェーン MCMC 診断 (KDE 合算 + 色分けトレース)。
secMCMCDiagnosticsMulti :: Text -> [Text] -> [Chain] -> ReportSection
secMCMCDiagnosticsMulti title params chains =
  SecVega title (VM.mcmcDiagnosticsMulti (defaultConfig title) params chains)

-- | 自己相関プロット。
secMCMCAutocorr :: Text -> Int -> [Text] -> Chain -> ReportSection
secMCMCAutocorr title maxLag params chain =
  SecVega title (VM.autocorrPlot (defaultConfig title) maxLag params chain)

-- | ペアスキャッタープロット。
secMCMCPair :: Text -> Text -> Text -> Chain -> ReportSection
secMCMCPair title pa pb chain =
  SecVega title (VM.pairScatter (defaultConfig title) pa pb chain)

-- | 事後要約テーブル (mean / SD / 2.5% / 97.5% / ESS / R-hat)。
-- 入力: パラメータごとに (name, mean, sd, q025, q975, ess, rhat)。
secPosteriorSummary
  :: Text                                                    -- title
  -> [(Text, Double, Double, Double, Double, Double, Maybe Double)]
  -> ReportSection
secPosteriorSummary title rows =
  let headers = ["Parameter", "Mean", "SD", "2.5%", "97.5%", "ESS", "R-hat"]
      body    = [ [ p
                  , T.pack (printf "%.4f" m)
                  , T.pack (printf "%.4f" sd)
                  , T.pack (printf "%.4f" lo)
                  , T.pack (printf "%.4f" hi)
                  , T.pack (printf "%.0f" ess)
                  , maybe "—" (T.pack . printf "%.3f") rhat
                  ]
                | (p, m, sd, lo, hi, ess, rhat) <- rows ]
  in SecTable title headers body

-- ---------------------------------------------------------------------------
-- 対話的予測 (LM/GLM 単変数)
-- ---------------------------------------------------------------------------

-- | 単変数 LM/GLM の対話的予測セクション。
-- 与えられた x グリッド + 予測 y + バンドから埋め込み JS を生成し、
-- スライダーで予測点をリアルタイム移動できる scatter+chart を表示。
--
-- 引数:
--   * title         — セクション見出し
--   * xCol / yCol   — 軸ラベル
--   * xs / ys       — 観測データ
--   * sc            — グリッド + 予測曲線 (信頼帯付きなら band も描画)
--   * (xMin, xMax)  — スライダー範囲 (データ範囲 ±50% 推奨)
secInteractiveLM
  :: Text             -- title
  -> Text             -- x 列名
  -> Text             -- y 列名
  -> [Double]         -- xs
  -> [Double]         -- ys
  -> SmoothCurve      -- 予測曲線 (信頼帯あれば band)
  -> (Double, Double) -- スライダー範囲 (xMin, xMax)
  -> ReportSection
secInteractiveLM = SecInteractiveLM

-- ---------------------------------------------------------------------------
-- Reportable typeclass
-- ---------------------------------------------------------------------------

-- | フィット結果から既定セクション群を生成する型クラス。
-- ライブラリ利用者が `renderReport file cfg (toReport cfg df xCols yCol fit)` の
-- 形で簡潔に書ける。各モデル型 (RegFit / SplineFit / RobustGPFit 等) は
-- このクラスのインスタンスで既定セクションを定義する。
class Reportable a where
  toReport :: ReportConfig -> DataFrame -> [Text] -> Text -> a -> [ReportSection]

-- ---------------------------------------------------------------------------
-- レンダリング
-- ---------------------------------------------------------------------------

-- | 単一の自己完結 HTML ファイルとして書き出す。
renderReport :: FilePath -> ReportConfig -> [ReportSection] -> IO ()
renderReport path cfg sections =
  TIO.writeFile path (buildHtml cfg sections)

buildHtml :: ReportConfig -> [ReportSection] -> Text
buildHtml cfg sections =
  let body = T.intercalate "\n" (zipWith (renderSection . sectionId) [0..] sections)
      scripts = T.intercalate "\n" (zipWith (sectionScript . sectionId) [0..] sections)
  in T.unlines
       [ "<!DOCTYPE html>"
       , "<html lang=\"ja\">"
       , "<head>"
       , "<meta charset=\"utf-8\">"
       , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
       , "<title>" <> rcTitle cfg <> "</title>"
       , "<script>" <> vegaJS      <> "</script>"
       , "<script>" <> vegaLiteJS  <> "</script>"
       , "<script>" <> vegaEmbedJS <> "</script>"
       , "<script src=\"https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js\"></script>"
       , "<style>" <> css <> "</style>"
       , "</head>"
       , "<body>"
       , "<header><h1>" <> rcTitle cfg <> "</h1>"
       , if T.null (rcSubtitle cfg) then ""
         else "<div class=\"subtitle\">" <> rcSubtitle cfg <> "</div>"
       , "</header>"
       , "<main>"
       , body
       , "</main>"
       , "<script>"
       , "mermaid.initialize({ startOnLoad: true, theme: 'default' });"
       , scripts
       , "</script>"
       , "</body>"
       , "</html>"
       ]

sectionId :: Int -> Text
sectionId i = "sec_" <> T.pack (show i)

-- ---------------------------------------------------------------------------
-- セクション → HTML
-- ---------------------------------------------------------------------------

renderSection :: Text -> ReportSection -> Text
renderSection sid sec = case sec of
  SecDataOverview df xs y     -> renderDataOverview sid df xs y
  SecModelOverview ty fm mer  -> renderModelOverview sid ty fm mer
  SecCoefficients cs mr2      -> renderCoefficients sid cs mr2
  SecFitScatter xc yc xs ys s -> renderFitScatter sid xc yc xs ys s
  SecResiduals fit res        -> renderResiduals sid fit res
  SecBarChart t vs            -> renderBarChart sid t vs
  SecVega t _                 -> renderVegaPlaceholder sid t
  SecMermaid m                -> renderMermaid sid m
  SecTable t hs rs            -> renderTable sid t hs rs
  SecKeyValue t kvs           -> renderKeyValue sid t kvs
  SecMarkdown t txt           -> renderMarkdown sid t txt
  SecHtml _ html              -> wrapSection sid "" html
  SecInteractiveLM t xc yc xs ys sc rng -> renderInteractiveLM sid t xc yc xs ys sc rng

wrapSection :: Text -> Text -> Text -> Text
wrapSection sid title inner = T.unlines
  [ "<section id=\"" <> sid <> "\">"
  , if T.null title then "" else "  <h2>" <> title <> "</h2>"
  , inner
  , "</section>"
  ]

-- データ概要 -----------------------------------------------------------------

renderDataOverview :: Text -> DataFrame -> [Text] -> Text -> Text
renderDataOverview sid df xCols yCol =
  let allCols  = xCols ++ [yCol]
      relevant = [ (c, getColumn c df) | c <- allCols ]
      n        = numRows df
      header   = "<tr><th>Column</th><th>Type</th><th>N</th><th>Min</th>"
                 <> "<th>Max</th><th>Mean</th><th>Median</th><th>SD</th></tr>"
      rows = T.intercalate "\n" (map renderColRow relevant)
      summary = "Rows: <strong>" <> T.pack (show n)
                <> "</strong>, columns analyzed: <strong>"
                <> T.pack (show (length allCols)) <> "</strong>"
  in wrapSection sid "Data overview" $ T.unlines
       [ "<p>" <> summary <> "</p>"
       , "<table>"
       , "<thead>" <> header <> "</thead>"
       , "<tbody>" <> rows <> "</tbody>"
       , "</table>"
       ]
  where
    renderColRow (c, Just (NumericCol v)) =
      let xs = V.toList v
          m  = length xs
          mean = sum xs / fromIntegral m
          ss   = sort xs
          mn = if m == 0 then 0 else minimum xs
          mx = if m == 0 then 0 else maximum xs
          med = if m == 0 then 0 else ss !! (m `div` 2)
          var = if m <= 1 then 0
                else sum [(x - mean) ^ (2 :: Int) | x <- xs] / fromIntegral (m - 1)
          sdv = sqrt var
      in "<tr>" <> T.intercalate ""
           [ td c
           , td "numeric"
           , td (T.pack (show m))
           , td (showD4 mn)
           , td (showD4 mx)
           , td (showD4 mean)
           , td (showD4 med)
           , td (showD4 sdv)
           ] <> "</tr>"
    renderColRow (c, Just (TextCol v)) =
      let xs = V.toList v
          m  = length xs
          uniq = length (unique xs)
      in "<tr>" <> T.intercalate ""
           [ td c
           , td "text"
           , td (T.pack (show m))
           , td "—" , td "—" , td "—" , td "—"
           , td ("unique=" <> T.pack (show uniq))
           ] <> "</tr>"
    renderColRow (c, Nothing) =
      "<tr><td>" <> c <> "</td><td colspan=7>(missing)</td></tr>"
    td x = "<td>" <> x <> "</td>"
    unique = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- モデル概要 -----------------------------------------------------------------

renderModelOverview :: Text -> Text -> Text -> Maybe Text -> Text
renderModelOverview sid ty formula mer =
  let merBlock = case mer of
        Nothing -> ""
        Just m  -> "<div class=\"mermaid\">" <> m <> "</div>"
  in wrapSection sid "Model overview" $ T.unlines
       [ "<div class=\"kv\">"
       , "<div><span class=\"k\">Type</span><span class=\"v\">" <> ty <> "</span></div>"
       , "<div><span class=\"k\">Formula</span><span class=\"v\">"
         <> formula <> "</span></div>"
       , "</div>"
       , merBlock
       ]

-- 係数表 -------------------------------------------------------------------

renderCoefficients :: Text -> [(Text, Double)] -> Maybe (Text, Double) -> Text
renderCoefficients sid coeffs mR2 =
  let rows = T.intercalate "\n"
        [ "<tr><td>" <> lbl <> "</td><td class=\"num\">"
          <> showD4 v <> "</td></tr>"
        | (lbl, v) <- coeffs ]
      r2Row = case mR2 of
        Just (lbl, v) ->
          "<tfoot><tr><td><strong>" <> lbl <> "</strong></td><td class=\"num\"><strong>"
          <> showD4 v <> "</strong></td></tr></tfoot>"
        Nothing -> ""
  in wrapSection sid "Coefficients" $ T.unlines
       [ "<table class=\"narrow\">"
       , "<thead><tr><th>Parameter</th><th>Value</th></tr></thead>"
       , "<tbody>" <> rows <> "</tbody>"
       , r2Row
       , "</table>"
       ]

-- 散布図 + 滑らか曲線 -------------------------------------------------------

renderFitScatter :: Text -> Text -> Text -> [Double] -> [Double]
                 -> Maybe SmoothCurve -> Text
renderFitScatter sid _xc _yc _xs _ys _msc =
  wrapSection sid "Scatter and fit" $
    "<div class=\"vl-wrap\"><div id=\"vl-" <> sid <> "\"></div></div>"

-- 残差 -----------------------------------------------------------------------

renderResiduals :: Text -> [Double] -> [Double] -> Text
renderResiduals sid _fitted _resids =
  wrapSection sid "Residuals" $
    "<div class=\"vl-wrap\"><div id=\"vl-" <> sid <> "\"></div></div>"

-- 棒グラフ -------------------------------------------------------------------

renderBarChart :: Text -> Text -> [(Text, Double)] -> Text
renderBarChart sid title _vs =
  wrapSection sid title $
    "<div class=\"vl-wrap\"><div id=\"vl-" <> sid <> "\"></div></div>"

-- 任意 Vega -----------------------------------------------------------------

renderVegaPlaceholder :: Text -> Text -> Text
renderVegaPlaceholder sid title =
  wrapSection sid title $
    "<div class=\"vl-wrap\"><div id=\"vl-" <> sid <> "\"></div></div>"

-- Mermaid -------------------------------------------------------------------

renderMermaid :: Text -> Text -> Text
renderMermaid sid m =
  wrapSection sid "Model graph" $
    "<div class=\"mermaid\">" <> m <> "</div>"

-- テーブル -------------------------------------------------------------------

renderTable :: Text -> Text -> [Text] -> [[Text]] -> Text
renderTable sid title hs rows =
  let head' = T.intercalate "" ["<th>" <> h <> "</th>" | h <- hs]
      body  = T.intercalate "\n"
                [ "<tr>" <> T.intercalate "" ["<td>" <> c <> "</td>" | c <- r]
                  <> "</tr>"
                | r <- rows ]
  in wrapSection sid title $ T.unlines
       [ "<table>"
       , "<thead><tr>" <> head' <> "</tr></thead>"
       , "<tbody>" <> body <> "</tbody>"
       , "</table>"
       ]

-- KV ------------------------------------------------------------------------

renderKeyValue :: Text -> Text -> [(Text, Text)] -> Text
renderKeyValue sid title kvs =
  let block = T.intercalate "\n"
        [ "<div><span class=\"k\">" <> k <> "</span><span class=\"v\">"
          <> v <> "</span></div>"
        | (k, v) <- kvs ]
  in wrapSection sid title $
       "<div class=\"kv\">" <> block <> "</div>"

-- Markdown ------------------------------------------------------------------

renderMarkdown :: Text -> Text -> Text -> Text
renderMarkdown sid title txt =
  wrapSection sid title $ "<p>" <> txt <> "</p>"

-- Interactive LM ------------------------------------------------------------

renderInteractiveLM :: Text -> Text -> Text -> Text
                    -> [Double] -> [Double]
                    -> SmoothCurve -> (Double, Double) -> Text
renderInteractiveLM sid title xc yc _xs _ys _sc (xMin, xMax) =
  let mid  = (xMin + xMax) / 2
      step = (xMax - xMin) / 200
  in wrapSection sid (if T.null title then "Interactive prediction" else title) $
       T.unlines
         [ "<div class=\"interactive-controls\">"
         , "  <label>" <> xc <> ": "
         , "    <input type=\"range\" id=\"i-" <> sid <> "-slider\""
         , "      min=\"" <> showD4 xMin <> "\""
         , "      max=\"" <> showD4 xMax <> "\""
         , "      step=\"" <> showD4 step <> "\""
         , "      value=\"" <> showD4 mid <> "\""
         , "      oninput=\"window.__upd_" <> sid <> "(this.value)\">"
         , "    <span id=\"i-" <> sid <> "-x\">" <> showD4 mid <> "</span>"
         , "  </label>"
         , "  <span class=\"pred-readout\"><strong>Predicted " <> yc
           <> ":</strong> <span id=\"i-" <> sid <> "-y\">—</span>"
         , "    <span id=\"i-" <> sid <> "-band\" class=\"band-readout\"></span></span>"
         , "</div>"
         , "<div class=\"vl-wrap\"><div id=\"vl-" <> sid <> "\"></div></div>"
         ]

-- ---------------------------------------------------------------------------
-- Vega-Lite spec 埋め込みスクリプト
-- ---------------------------------------------------------------------------

sectionScript :: Text -> ReportSection -> Text
sectionScript sid sec = case sec of
  SecFitScatter xc yc xs ys msc ->
    embed sid (fitScatterSpec xc yc xs ys msc)
  SecResiduals fitted resids ->
    embed sid (residualsSpec fitted resids)
  SecBarChart t vs ->
    embed sid (barChartSpec t vs)
  SecVega _ spec ->
    embed sid spec
  SecInteractiveLM _ xc yc xs ys sc _ ->
    interactiveLMScript sid xc yc xs ys sc
  _ -> ""
  where
    embed s spec =
      let json = decodeUtf8 . toStrict . encode . fromVL $ spec
      in "vegaEmbed('#vl-" <> s <> "', " <> json <> ", {actions:false});"

-- | Interactive LM の JS: scatter+曲線を描画し、スライダーで予測点を更新。
-- グリッド (sc) で線形補間して予測値を計算する (モデル係数を JS に渡さなくて済む)。
interactiveLMScript :: Text -> Text -> Text -> [Double] -> [Double]
                    -> SmoothCurve -> Text
interactiveLMScript sid xc yc xs ys sc =
  let baseSpec = fitScatterSpec xc yc xs ys (Just sc)
      -- 予測点用のオーバーレイ追加: クライアント側で 1 点 (cx, cy) を mark Point
      -- ここは簡易: vegaEmbed したあと、別 div に readout を表示するのみ
      json = decodeUtf8 . toStrict . encode . fromVL $ baseSpec
      gridX = scXs sc
      gridY = scYs sc
      gridLo = scLower sc
      gridHi = scUpper sc
      hasBand = not (null gridLo) && length gridLo == length gridX
      -- JS arrays as text
      arr xs0 = "[" <> T.intercalate "," (map showD4 xs0) <> "]"
  in T.unlines
       [ "vegaEmbed('#vl-" <> sid <> "', " <> json <> ", {actions:false});"
       , "(() => {"
       , "  const gx = " <> arr gridX <> ";"
       , "  const gy = " <> arr gridY <> ";"
       , "  const gl = " <> arr (if hasBand then gridLo else []) <> ";"
       , "  const gh = " <> arr (if hasBand then gridHi else []) <> ";"
       , "  const interp = (x, xs, ys) => {"
       , "    if (xs.length === 0) return null;"
       , "    if (x <= xs[0]) return ys[0];"
       , "    if (x >= xs[xs.length-1]) return ys[ys.length-1];"
       , "    for (let i = 1; i < xs.length; i++) {"
       , "      if (x <= xs[i]) {"
       , "        const t = (x - xs[i-1]) / (xs[i] - xs[i-1]);"
       , "        return ys[i-1] + t * (ys[i] - ys[i-1]);"
       , "      }"
       , "    }"
       , "    return ys[ys.length-1];"
       , "  };"
       , "  window.__upd_" <> sid <> " = function(v) {"
       , "    const x = parseFloat(v);"
       , "    document.getElementById('i-" <> sid <> "-x').textContent = x.toFixed(3);"
       , "    const y = interp(x, gx, gy);"
       , "    document.getElementById('i-" <> sid <> "-y').textContent = y === null ? '—' : y.toFixed(4);"
       , "    if (gl.length > 0) {"
       , "      const lo = interp(x, gx, gl);"
       , "      const hi = interp(x, gx, gh);"
       , "      document.getElementById('i-" <> sid <> "-band').textContent ="
       , "        ' [' + lo.toFixed(3) + ', ' + hi.toFixed(3) + ']';"
       , "    }"
       , "  };"
       , "  // 初期表示"
       , "  const initX = (gx[0] + gx[gx.length-1]) / 2;"
       , "  window.__upd_" <> sid <> "(initX);"
       , "})();"
       ]

fitScatterSpec :: Text -> Text -> [Double] -> [Double]
               -> Maybe SmoothCurve -> VegaLite
fitScatterSpec xc yc xs ys msc =
  let scatterLayer = asSpec
        [ dataFromColumns []
            . dataColumn xc (Numbers xs)
            . dataColumn yc (Numbers ys)
            $ []
        , mark Point [MOpacity 0.7, MSize 50, MColor "#4C72B0"]
        , encoding
            . position X [PName xc, PmType Quantitative,
                          PAxis [AxTitle xc]]
            . position Y [PName yc, PmType Quantitative,
                          PAxis [AxTitle yc]]
            $ []
        ]
      smoothLayers = case msc of
        Nothing -> []
        Just sc ->
          let lineLayer = asSpec
                [ dataFromColumns []
                    . dataColumn "x_grid" (Numbers (scXs sc))
                    . dataColumn "y_fit"  (Numbers (scYs sc))
                    $ []
                , mark Line [MColor "#DD5566", MStrokeWidth 2.5]
                , encoding
                    . position X [PName "x_grid", PmType Quantitative]
                    . position Y [PName "y_fit",  PmType Quantitative]
                    $ []
                ]
              hasBand = not (null (scLower sc)) && not (null (scUpper sc))
                          && length (scLower sc) == length (scXs sc)
              bandLayer
                | hasBand = [asSpec
                  [ dataFromColumns []
                      . dataColumn "x_grid" (Numbers (scXs sc))
                      . dataColumn "lo" (Numbers (scLower sc))
                      . dataColumn "hi" (Numbers (scUpper sc))
                      $ []
                  , mark Area [MOpacity 0.2, MColor "#DD5566"]
                  , encoding
                      . position X  [PName "x_grid", PmType Quantitative]
                      . position Y  [PName "lo", PmType Quantitative]
                      . position Y2 [PName "hi"]
                      $ []
                  ]]
                | otherwise = []
          in bandLayer ++ [lineLayer]
  in toVegaLite
       [ layer (scatterLayer : smoothLayers)
       , width 600
       , height 320
       ]

residualsSpec :: [Double] -> [Double] -> VegaLite
residualsSpec fitted resids =
  toVegaLite
    [ dataFromColumns []
        . dataColumn "fitted"  (Numbers fitted)
        . dataColumn "residual" (Numbers resids)
        $ []
    , mark Point [MOpacity 0.7, MSize 50, MColor "#4C72B0"]
    , encoding
        . position X [PName "fitted",  PmType Quantitative,
                      PAxis [AxTitle "Fitted"]]
        . position Y [PName "residual", PmType Quantitative,
                      PAxis [AxTitle "Residual"]]
        $ []
    , width 600
    , height 280
    ]

-- | Regularization path (lambda 対 各係数) をログスケール x 軸の多線グラフで描く。
-- 入力: 係数ラベル + (λ, 係数ベクトル) のリスト。intercept は除外推奨。
regPathSpec
  :: [Text]                    -- ^ 係数ラベル (length = 係数数)
  -> [(Double, [Double])]      -- ^ (λ, [coef])
  -> VegaLite
regPathSpec labels path =
  let -- long format: 各 (λ, label, value) を平坦化
      rows = [ (lam, lbl, val)
             | (lam, coefs) <- path
             , (lbl, val)   <- zip labels coefs ]
      lams   = [ lam | (lam, _, _) <- rows ]
      lbls   = [ lbl | (_, lbl, _) <- rows ]
      vals   = [ val | (_, _, val) <- rows ]
  in toVegaLite
       [ dataFromColumns []
           . dataColumn "lambda"      (Numbers lams)
           . dataColumn "coefficient" (Strings lbls)
           . dataColumn "value"       (Numbers vals)
           $ []
       , mark Line [MStrokeWidth 2.2, MOpacity 0.9]
       , encoding
           . position X [PName "lambda", PmType Quantitative,
                         PScale [SType ScLog],
                         PAxis [AxTitle "λ (log scale)"]]
           . position Y [PName "value", PmType Quantitative,
                         PAxis [AxTitle "Coefficient"]]
           . color [MName "coefficient", MmType Nominal,
                    MScale [SScheme "tableau10" []],
                    MLegend [LTitle "feature"]]
           $ []
       , width 640
       , height 320
       ]

barChartSpec :: Text -> [(Text, Double)] -> VegaLite
barChartSpec _title vs =
  let labels = map fst vs
      values = map snd vs
  in toVegaLite
       [ dataFromColumns []
           . dataColumn "label" (Strings labels)
           . dataColumn "value" (Numbers values)
           $ []
       , mark Bar [MColor "#4C72B0", MOpacity 0.85]
       , encoding
           . position X [PName "label", PmType Nominal,
                         PAxis [AxTitle "", AxLabelAngle (-30)],
                         PSort []]
           . position Y [PName "value", PmType Quantitative,
                         PAxis [AxTitle ""]]
           $ []
       , widthStep 40
       , height 220
       ]

-- ---------------------------------------------------------------------------
-- 数値フォーマット
-- ---------------------------------------------------------------------------

showD4 :: Double -> Text
showD4 d = T.pack (showFFloat (Just 4) d "")

-- ---------------------------------------------------------------------------
-- CSS
-- ---------------------------------------------------------------------------

css :: Text
css = T.unlines
  [ "* { box-sizing: border-box; margin: 0; padding: 0; }"
  , "body { font-family: 'Segoe UI', -apple-system, sans-serif; background: #f0f2f5; color: #333; }"
  , "header { background: #2c3e50; color: #ecf0f1; padding: 18px 30px; }"
  , "header h1 { font-size: 1.2em; font-weight: 600; }"
  , ".subtitle { color: #bdc3c7; font-size: .85em; margin-top: 4px; }"
  , "main { max-width: 1100px; margin: 0 auto; padding: 30px 20px; }"
  , "section { background: white; border-radius: 10px; padding: 24px;"
  , "          margin-bottom: 22px; box-shadow: 0 2px 8px rgba(0,0,0,.08); }"
  , "h2 { font-size: 1.05em; color: #2c3e50; margin-bottom: 14px;"
  , "     border-bottom: 2px solid #e8ecf0; padding-bottom: 6px; }"
  , "table { width: 100%; border-collapse: collapse; font-size: .9em; }"
  , "table.narrow { max-width: 480px; }"
  , "th { background: #f0f2f5; text-align: right; padding: 8px 14px;"
  , "     font-weight: 600; color: #555; }"
  , "th:first-child { text-align: left; }"
  , "td { padding: 7px 14px; border-bottom: 1px solid #f0f2f5; text-align: right; }"
  , "td:first-child { text-align: left; font-family: monospace; }"
  , ".num { font-family: monospace; }"
  , "tr:last-child td { border-bottom: none; }"
  , "tfoot td { border-top: 2px solid #ddd; }"
  , ".vl-wrap { overflow-x: auto; }"
  , ".kv { display: flex; flex-wrap: wrap; gap: 12px 20px; }"
  , ".kv > div { display: flex; flex-direction: column; min-width: 140px; }"
  , ".kv .k { font-size: .75em; color: #888; text-transform: uppercase; }"
  , ".kv .v { font-size: 1.1em; font-weight: 600; color: #2c3e50; margin-top: 2px; }"
  , ".mermaid { text-align: center; margin: 14px 0; }"
  , "p { line-height: 1.6; color: #444; font-size: .92em; }"
  , ".interactive-controls { margin-bottom: 16px; padding: 14px; background: #f8f9fa; border-radius: 8px; }"
  , ".interactive-controls input[type='range'] { width: 360px; vertical-align: middle; margin: 0 10px; }"
  , ".interactive-controls label { display: block; margin-bottom: 8px; }"
  , ".pred-readout { font-size: 1em; }"
  , ".pred-readout strong { color: #2c3e50; }"
  , ".band-readout { color: #888; font-size: .9em; }"
  ]
