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
  , secModelOverviewLink
  , secModelOverviewExtras
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
  , secCollapsible
  , secCard
  , secStatRow
    -- * Markdown ファイル読込 (appendix 用)
  , secAppendixFromMd
  , renderSimpleMarkdown
    -- * MCMC / 事後分布関連 (Phase F)
  , secMCMCDiagnostics
  , secMCMCDiagnosticsMulti
  , secMCMCAutocorr
  , secMCMCPair
  , secPosteriorSummary
    -- * モデル比較・診断セクション (Cycle 1)
  , secComparisonTable
  , secForestPlot
  , secFeatureImportance
  , secPPC
    -- * 追加可視化セクション (Cycle 9)
  , secCalibration
  , sec3DScatter
  , secHeatmap
    -- * 補間 / regrid レポート (Phase G4)
  , InterpReport (..)
  , defaultInterpReport
  , secInterpolation
    -- * 対話的予測 (LM/GLM 用)
  , secInteractiveLM
  , secInteractiveMulti
  , InteractiveModel (..)
    -- * 対話的予測 (多変量 RFF Ridge)
  , secInteractiveRFFMV
  , InteractiveRFFMV (..)
    -- * 対話的予測 (多出力: 1 入力 → q 出力カーブ)
  , secInteractiveMultiOut
  , InteractiveMultiOut (..)
  , InteractivePredictor (..)
  , mkInteractiveMOLinear
  , mkInteractiveMOKernelRBF
    -- * レンダリング
  , renderReport
    -- * Reportable typeclass
  , Reportable (..)
    -- * 専用 Vega-Lite ヘルパ
  , regPathSpec
  , forestPlotSpec
  , ppcSpec
  , calibrationSpec
  , scatter3DSpec
  , heatmapSpec
  , interpolationOverlaySpec
  , densityProfileSpec
  , idAlignmentSpec
  ) where

import Data.Aeson (encode)
import Data.ByteString.Lazy (toStrict)
import Data.List (sort, sortBy)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (decodeUtf8)
import Graphics.Vega.VegaLite hiding (filter, name)
import qualified Graphics.Vega.VegaLite as VL
import Numeric (showFFloat)
import qualified Data.Vector as V
import Text.Printf (printf)

import qualified DataFrame                    as DX
import qualified DataFrame.Internal.DataFrame as DXD
import DataIO.Convert (getDoubleVec, getTextVec)
import MCMC.Core (Chain)
import qualified Stat.MCMC as SM
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

-- | 多変量対話的予測のモデル情報。
--
-- LM/GLM の線形予測子を JS で評価するための情報を保持:
--   y = invLink(β₀ + β₁ x_1 + β₂ x_2 + ... + β_p x_p)
--
-- 信頼帯は副軸を slider 値に固定したときの平均応答 CI (現状は 95% 等幅近似)。
-- | 多変量 RFF Ridge の対話的予測モデル (Phase B-RFF)。
--
-- ブラウザ JS 側で以下を計算して曲線を更新する:
--
-- * @x_full[k] = (k == mainAxisIdx) ? z : sliderValues[k]@ for each z in mainGrid
-- * @arg_j = b_j + Σ_k ω_jk · x_full[k]@
-- * @ŷ(z) = Σ_j w_j · σ_f √(2/D) · cos(arg_j)@
data InteractiveRFFMV = InteractiveRFFMV
  { irfXCols       :: [Text]              -- ^ 全説明変数名 (length = p)
  , irfYCol        :: Text
  , irfXObs        :: [[Double]]          -- ^ p × n の観測 x (列ごと)
  , irfYObs        :: [Double]
  , irfGroups      :: [Text]              -- ^ n 個の group ラベル (色分け用)
  , irfMainAxis    :: Text                -- ^ 横軸として変化させる列名 (例 "z")
  , irfMainGrid    :: [Double]            -- ^ 横軸グリッド (例 z 線形空間 100 点)
  , irfSliders     :: [(Text, Double, Double, Double)]
                                          -- ^ 副軸スライダ [(name, min, mid, max)]
                                          --   横軸列以外の全列に対応
  , irfOmegasRowMaj :: [Double]           -- ^ p × D 行列を row-major に
  , irfBs          :: [Double]            -- ^ D
  , irfSigmaF      :: Double
  , irfDim         :: Int                 -- ^ D
  , irfP           :: Int                 -- ^ p
  , irfWeights     :: [Double]            -- ^ D
  , irfStdMu       :: Maybe [Double]      -- ^ 標準化 ON 時の μ (length p)。
                                          --   JS 側で raw → 標準化変換に使用
  , irfStdSd       :: Maybe [Double]      -- ^ 同 σ
  } deriving (Show)

-- | 多出力対話的予測のモデル情報 (1 入力 x → q 出力 y(z_1..z_q))。
--
-- 散布図の横軸は z (出力グリッド)、縦軸は y。スライダ 1 本で入力 x を
-- 動かすと、q 個の出力すべてを再計算して曲線として描画する。
data InteractiveMultiOut = InteractiveMultiOut
  { imoXCol     :: Text                          -- ^ 入力変数名 (例 "dose")
  , imoYCol     :: Text                          -- ^ 出力名 (例 "potential V")
  , imoOutAxis  :: Text                          -- ^ 出力軸ラベル (例 "z [nm]")
  , imoOutGrid  :: [Double]                      -- ^ 出力グリッド (長さ q)
  , imoXObs     :: [Double]                      -- ^ 観測 x (長さ n)
  , imoYObs     :: [[Double]]                    -- ^ 観測 Y (n × q、行 = sample)
  , imoXSlider  :: (Double, Double, Double)      -- ^ (min, mid, max) スライダ範囲
  , imoPred     :: InteractivePredictor          -- ^ predictor 種別
  } deriving (Show)

-- | 多出力 predictor 種別。将来 RFF / GP 等を追加する余地あり。
data InteractivePredictor
  = -- | 線形: ŷ_j(x) = β0_j + β1_j · x
    PredLinearMO
      { plmoIntercepts :: [Double]   -- ^ 長さ q
      , plmoSlopes     :: [Double]   -- ^ 長さ q
      }
    -- | 1D RBF Kernel Ridge: ŷ_j(x) = Σ_i exp(-(x-x_i)²/(2h²)) · α_{ij}
  | PredKernelRBF1
      { pkrXTrain :: [Double]        -- ^ 長さ n
      , pkrAlpha  :: [[Double]]      -- ^ n × q (行 = sample)
      , pkrH      :: Double          -- ^ bandwidth
      }
  deriving (Show)

data InteractiveModel = InteractiveModel
  { imXCols     :: [Text]                  -- ^ 説明変数名 (length = p)
  , imYCol      :: Text                    -- ^ 応答名
  , imXValues   :: [[Double]]              -- ^ 各列の観測値 (n samples × p)
  , imYValues   :: [Double]                -- ^ 観測 y
  , imIntercept :: Double                  -- ^ β₀
  , imBetas     :: [Double]                -- ^ [β₁, ..., β_p]
  , imLink      :: Text                    -- ^ "identity" | "log" | "logit" | "sqrt"
  , imSlider    :: [(Double, Double, Double)]
                                           -- ^ 各列の (min, mid, max) スライダー範囲
  , imCISigma   :: Maybe Double            -- ^ 残差 σ_hat (CI 計算用; Nothing なら CI なし)
  } deriving (Show)

-- | レポート 1 セクション。
data ReportSection
  = -- | データ概要: 列ごとの型/N/min/max/mean/SD + ヒストグラム
    SecDataOverview DXD.DataFrame [Text] Text
    -- | モデル概要: タイトル / 数式 / 任意の追加 info-box [(label,value)] / Mermaid DAG
  | SecModelOverview Text Text [(Text, Text)] (Maybe Text)
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
    -- | 多変量対話的予測。主軸選択 dropdown + 各副軸 slider + 散布図。
  | SecInteractiveMulti Text InteractiveModel
    -- | 多変量 RFF Ridge の対話的予測。横軸固定 + 副軸スライダ + 散布図。
  | SecInteractiveRFFMV Text InteractiveRFFMV
    -- | 多出力対話的予測 (1 入力 → q 出力)。
  | SecInteractiveMultiOut Text InteractiveMultiOut
    -- | 折りたたみ可能なグループ。子セクションを 1 つの details で囲む。
    --   フィールド: title / openByDefault / 子セクション
  | SecCollapsible Text Bool [ReportSection]
    -- | 淡い背景色の囲みカード。SecCollapsible の内部などで使い、
    --   関連する図表をひとまとめにする (常に開いた状態)。
  | SecCard Text [ReportSection]
    -- | フラットな統計行 (section 包装なし)。
    --   info-box が横並びになる stat-row。
  | SecStatRow [(Text, Text)]

-- ---------------------------------------------------------------------------
-- ビルダ
-- ---------------------------------------------------------------------------

secDataOverview :: DXD.DataFrame -> [Text] -> Text -> ReportSection
secDataOverview = SecDataOverview

-- | モデル概要 (追加 box なし)。LM 等。
secModelOverview :: Text -> Text -> Maybe Text -> ReportSection
secModelOverview ty fm mer = SecModelOverview ty fm [] mer

-- | モデル概要 + リンク関数。GLM / GLMM 等で使用。
secModelOverviewLink :: Text       -- ^ モデル種別
                     -> Text       -- ^ 数式 (HTML 可)
                     -> Text       -- ^ リンク関数 (例: "log" / "logit" / "identity")
                     -> Maybe Text -- ^ Mermaid DAG
                     -> ReportSection
secModelOverviewLink ty fm link mer =
  SecModelOverview ty fm [("リンク関数", link)] mer

-- | モデル概要 + 任意の追加 info-box (label, value)。
--   HBM のサンプラー種類や GP のカーネル種類を表示する場合などに使用。
secModelOverviewExtras :: Text             -- ^ モデル種別
                       -> Text             -- ^ 数式 (HTML 可)
                       -> [(Text, Text)]   -- ^ 追加 info-box (label, value) の列
                       -> Maybe Text       -- ^ Mermaid DAG
                       -> ReportSection
secModelOverviewExtras = SecModelOverview

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

-- | 折りたたみ可能グループ。
secCollapsible :: Text -> Bool -> [ReportSection] -> ReportSection
secCollapsible = SecCollapsible

-- | 淡い背景の囲みカード。回帰結果セクション内で関連図表をグループ化するのに使う。
secCard :: Text -> [ReportSection] -> ReportSection
secCard = SecCard

-- | フラットな統計行 (section box なし)。Card と Card の間で軽く統計を並べる用途。
secStatRow :: [(Text, Text)] -> ReportSection
secStatRow = SecStatRow

-- ---------------------------------------------------------------------------
-- Markdown appendix
-- ---------------------------------------------------------------------------

-- | 指定の md ファイルを読み込み、簡易 markdown パーサで HTML 化して
-- appendix セクションとして返す。
--
-- サポートする markdown 機能:
-- - 見出し: @# H1@, @## H2@, @### H3@
-- - 段落: 空行で区切られた連続行
-- - 箇条書き: @- item@
-- - インライン: @**bold**@, @*italic*@, @\`code\`@
-- - リンク: @[text](url)@
-- - インラインコード周辺は等幅フォント
secAppendixFromMd :: Text -> FilePath -> IO ReportSection
secAppendixFromMd title path = do
  contents <- TIO.readFile path
  let html  = renderSimpleMarkdown contents
      icon  = "<span class=\"sec-icon\">&#128218;</span>"
      tFull = icon <> " " <> title
  -- 折りたたみ可能 section として返す (default open)
  return (SecHtml title $ T.unlines
    [ "<section class=\"collapsible-wrap appendix-md\">"
    , "  <details open>"
    , "    <summary><h2>" <> tFull <> "</h2></summary>"
    , "    <div class=\"collapsible-body md-body\">"
    , html
    , "    </div>"
    , "  </details>"
    , "</section>"
    ])

-- | 簡易 markdown → HTML 変換。フル機能ではない。
renderSimpleMarkdown :: Text -> Text
renderSimpleMarkdown txt =
  let lns      = T.lines txt
      blocks   = groupBlocks lns
      htmlBlks = map renderBlock blocks
  in T.intercalate "\n" htmlBlks

-- | 行群を「ブロック」に分割。空行で区切る。
groupBlocks :: [Text] -> [[Text]]
groupBlocks = filter (not . all T.null) . splitOn T.null
  where
    splitOn _ [] = []
    splitOn p xs =
      let (chunk, rest) = break p xs
          rest' = dropWhile p rest
      in chunk : splitOn p rest'

-- | ブロック (連続行のリスト) を HTML 化。
renderBlock :: [Text] -> Text
renderBlock []       = ""
renderBlock ls@(l:_)
  | "# "  `T.isPrefixOf` l =
      "<h3>"  <> renderInline (T.drop 2 l) <> "</h3>"
  | "## " `T.isPrefixOf` l =
      "<h4>"  <> renderInline (T.drop 3 l) <> "</h4>"
  | "### " `T.isPrefixOf` l =
      "<h5>" <> renderInline (T.drop 4 l) <> "</h5>"
  | all isListLine ls =
      "<ul>" <> T.intercalate "\n"
                 [ "<li>" <> renderInline (T.drop 2 li) <> "</li>"
                 | li <- ls
                 , let li' = T.stripStart li
                 , let _ = li' ]  -- ダミー (li 自体を使う)
             <> "</ul>"
  | otherwise =
      "<p>" <> renderInline (T.intercalate " " ls) <> "</p>"
  where
    isListLine x = "- " `T.isPrefixOf` T.stripStart x

-- | インラインフォーマット: bold/italic/code/link を順に置換。
-- 数式 ($...$, $$...$$) は MathJax が処理するため、ここでは触らずに保持。
-- ただし $...$ 内の '*' を italic と誤認しないよう、まず数式部分を退避してから処理する。
renderInline :: Text -> Text
renderInline txt =
  let (chunks, maths) = extractMath txt
      processed = applyLinks . applyCode . applyItalic . applyBold $ chunks
  in restoreMath processed maths
  where
    applyBold t   = pairReplace "**" "<strong>" "</strong>" t
    applyItalic t = pairReplace "*"  "<em>"     "</em>"     t
    applyCode t   = pairReplace "`"  "<code>"   "</code>"   t
    -- [text](url) → <a href="url">text</a>
    applyLinks t = case T.breakOn "[" t of
      (pre, "")   -> pre
      (pre, rest) ->
        case T.breakOn "](" (T.drop 1 rest) of
          (lbl, "") -> pre <> rest
          (lbl, rest1) ->
            case T.breakOn ")" (T.drop 2 rest1) of
              (url, "") -> pre <> rest
              (url, rest2) ->
                pre <> "<a href=\"" <> url <> "\">" <> lbl <> "</a>"
                    <> applyLinks (T.drop 1 rest2)

-- | 開始/終了マーカーが交互に対になるとして置換。簡易版。
pairReplace :: Text -> Text -> Text -> Text -> Text
pairReplace marker startTag endTag txt = go txt True
  where
    go t inOpen =
      case T.breakOn marker t of
        (pre, "")   -> pre
        (pre, rest) ->
          let tag  = if inOpen then startTag else endTag
              rest' = T.drop (T.length marker) rest
          in pre <> tag <> go rest' (not inOpen)

-- | $...$ や $$...$$ の数式範囲を抽出してプレースホルダ "@@MATHn@@" に置換、
-- 元の数式テキストをリストで返す。
extractMath :: Text -> (Text, [Text])
extractMath = go 0 ""
  where
    go n acc t
      | "$$" `T.isPrefixOf` t =
          case T.breakOn "$$" (T.drop 2 t) of
            (math, rest) | not (T.null rest) ->
              let placeholder = "@@MATH" <> T.pack (show n) <> "@@"
                  full = "$$" <> math <> "$$"
                  (txt', maths) = go (n+1) (acc <> placeholder) (T.drop 2 rest)
              in (txt', full : maths)
            _ -> (acc <> t, [])
      | "$" `T.isPrefixOf` t =
          case T.breakOn "$" (T.drop 1 t) of
            (math, rest) | not (T.null rest) ->
              let placeholder = "@@MATH" <> T.pack (show n) <> "@@"
                  full = "$" <> math <> "$"
                  (txt', maths) = go (n+1) (acc <> placeholder) (T.drop 1 rest)
              in (txt', full : maths)
            _ -> (acc <> t, [])
      | T.null t = (acc, [])
      | otherwise =
          let (chunk, rest) = T.break (== '$') t
          in go n (acc <> chunk) rest

restoreMath :: Text -> [Text] -> Text
restoreMath txt maths = foldr replaceOne txt (zip [0::Int ..] maths)
  where
    replaceOne (i, m) acc =
      T.replace ("@@MATH" <> T.pack (show i) <> "@@") m acc

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
  let headers = ["パラメータ", "事後平均", "SD", "2.5%", "97.5%", "ESS", "R-hat"]
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
-- モデル比較・診断セクション (Cycle 1)
-- ---------------------------------------------------------------------------

-- | モデル比較テーブル。'secTable' のラッパだが、
-- 'mBest' で 0-based 行 index を渡すと、その行をハイライト表示する。
-- WAIC / LOO / RMSE などを横並びにし最良モデルを強調するのに使う。
secComparisonTable
  :: Text         -- ^ タイトル
  -> [Text]       -- ^ ヘッダ
  -> [[Text]]     -- ^ 行
  -> Maybe Int    -- ^ 最良行 index (0-based、'Nothing' でハイライトなし)
  -> ReportSection
secComparisonTable title headers rows mBest = case mBest of
  Nothing  -> SecTable title headers rows
  Just idx -> SecHtml title (renderComparisonHtml headers rows idx)

renderComparisonHtml :: [Text] -> [[Text]] -> Int -> Text
renderComparisonHtml headers rows bestIdx =
  let hdr = "<tr>" <> T.concat ["<th>" <> h <> "</th>" | h <- headers] <> "</tr>"
      mkRow i r =
        let style | i == bestIdx =
                      " style=\"background:#fff7d6;font-weight:600\""
                  | otherwise = ""
        in "<tr" <> style <> ">"
           <> T.concat ["<td>" <> c <> "</td>" | c <- r]
           <> "</tr>"
      body = T.concat (zipWith mkRow [0 :: Int ..] rows)
      legend = "<p style=\"margin-top:6px;font-size:.85em;color:#666\">"
            <> "★ ハイライト行 = 最良 (黄色背景)</p>"
  in "<table class=\"datatable\">" <> hdr <> body <> "</table>" <> legend

-- | Forest plot — 各パラメータの中央値 + 信用 (HDI/CI) 区間を横並び。
-- ベイズモデルの coefficient 比較や階層モデルの BLUP 表示に使う。
secForestPlot
  :: Text                                          -- ^ タイトル
  -> [(Text, Double, Double, Double)]              -- ^ (label, lower, mean, upper)
  -> ReportSection
secForestPlot title rows = SecVega title (forestPlotSpec rows)

-- | 特徴量重要度バー — 値降順にソートして 'secBarChart' に渡す。
-- Random Forest / GBM の feature importance 表示用。
secFeatureImportance :: Text -> [(Text, Double)] -> ReportSection
secFeatureImportance title items =
  SecBarChart title (sortBy (comparing (Down . snd)) items)

-- | Posterior Predictive Check — 観測データ密度 + 事後予測サンプルの密度を重ね描き。
-- 各 replicate の KDE を薄い線で、観測の KDE を太線で描画。
secPPC
  :: Text         -- ^ タイトル
  -> [Double]     -- ^ 観測値 y_obs
  -> [[Double]]   -- ^ 事後予測サンプル (replicate ごと、各長さ ~ length y_obs)
  -> ReportSection
secPPC title observed reps = SecVega title (ppcSpec observed reps)

-- | Calibration plot — 二値分類器の予測確率と観測頻度の対応図。
-- 入力データを 10 個のビン (`[0,0.1)..[0.9,1.0]`) に分割し、各ビンで
-- 予測確率の平均と観測 1 の頻度を計算し、対角線 (y = x) と重ねて描画。
-- 観測値は 0/1 (Bool 相当)。
secCalibration
  :: Text         -- ^ タイトル
  -> [Double]     -- ^ 予測確率 p ∈ [0, 1]
  -> [Double]     -- ^ 観測値 y ∈ {0, 1}
  -> ReportSection
secCalibration title pPred yObs =
  SecVega title (calibrationSpec pPred yObs)

-- | 3D scatter (Vega-Lite は 3D 非対応のため、x/y 軸 + 色エンコード z で代用)。
-- z が連続なら viridis 系のグラデーション、離散ならカテゴリ色。
sec3DScatter
  :: Text         -- ^ セクションタイトル
  -> Text         -- ^ x ラベル
  -> Text         -- ^ y ラベル
  -> Text         -- ^ z ラベル (色エンコード)
  -> [Double] -> [Double] -> [Double]
  -> ReportSection
sec3DScatter title xL yL zL xs ys zs =
  SecVega title (scatter3DSpec xL yL zL xs ys zs)

-- | 2D heatmap (rect mark + 値の色エンコード)。
-- 行ラベル × 列ラベルのグリッドに値を配置し、色強度で表現。
-- 例: 相関行列、混同行列、要因 × 水準の効果。
secHeatmap
  :: Text         -- ^ タイトル
  -> [Text]       -- ^ 列ラベル
  -> [Text]       -- ^ 行ラベル
  -> [[Double]]   -- ^ 値 (rows × cols)
  -> ReportSection
secHeatmap title colLabels rowLabels values =
  SecVega title (heatmapSpec colLabels rowLabels values)

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

-- | 多変量対話的予測 (主軸 dropdown + 副軸 slider + 散布図)。
secInteractiveMulti :: Text -> InteractiveModel -> ReportSection
secInteractiveMulti = SecInteractiveMulti

-- | 多変量 RFF Ridge の対話的予測セクション。
secInteractiveRFFMV :: Text -> InteractiveRFFMV -> ReportSection
secInteractiveRFFMV = SecInteractiveRFFMV

-- | 多出力対話的予測セクション (1 入力 → q 出力カーブ)。
secInteractiveMultiOut :: Text -> InteractiveMultiOut -> ReportSection
secInteractiveMultiOut = SecInteractiveMultiOut

-- | 線形多出力 fit から 'InteractiveMultiOut' を作る。
-- 入力: 列名・観測 x・観測 Y (n×q)・出力グリッド・intercepts (q)・slopes (q)・スライダ範囲
mkInteractiveMOLinear
  :: Text          -- xCol
  -> Text          -- yCol
  -> Text          -- outAxis label
  -> [Double]      -- output grid (length q)
  -> [Double]      -- observed x (length n)
  -> [[Double]]    -- observed Y (n × q)
  -> [Double]      -- intercepts (length q)
  -> [Double]      -- slopes (length q)
  -> (Double, Double, Double)   -- slider (min, mid, max)
  -> InteractiveMultiOut
mkInteractiveMOLinear xc yc oa grid xs ys ints slps slider =
  InteractiveMultiOut xc yc oa grid xs ys slider (PredLinearMO ints slps)

-- | RBF Kernel Ridge 多出力 fit から 'InteractiveMultiOut' を作る。
-- alpha 行列は (n × q)、行 = sample。
mkInteractiveMOKernelRBF
  :: Text          -- xCol
  -> Text          -- yCol
  -> Text          -- outAxis label
  -> [Double]      -- output grid (length q)
  -> [Double]      -- observed x (length n)
  -> [[Double]]    -- observed Y (n × q)
  -> [Double]      -- training x (length n) — 通常は xObs と同じ
  -> [[Double]]    -- alpha (n × q)
  -> Double        -- bandwidth h
  -> (Double, Double, Double)
  -> InteractiveMultiOut
mkInteractiveMOKernelRBF xc yc oa grid xs ys xtr alpha h slider =
  InteractiveMultiOut xc yc oa grid xs ys slider (PredKernelRBF1 xtr alpha h)

-- ---------------------------------------------------------------------------
-- Reportable typeclass
-- ---------------------------------------------------------------------------

-- | フィット結果から既定セクション群を生成する型クラス。
-- ライブラリ利用者が `renderReport file cfg (toReport cfg df xCols yCol fit)` の
-- 形で簡潔に書ける。各モデル型 (RegFit / SplineFit / RobustGPFit 等) は
-- このクラスのインスタンスで既定セクションを定義する。
class Reportable a where
  toReport :: ReportConfig -> DXD.DataFrame -> [Text] -> Text -> a -> [ReportSection]

-- ---------------------------------------------------------------------------
-- レンダリング
-- ---------------------------------------------------------------------------

-- | 単一の自己完結 HTML ファイルとして書き出す。
renderReport :: FilePath -> ReportConfig -> [ReportSection] -> IO ()
renderReport path cfg sections =
  TIO.writeFile path (buildHtml cfg sections)

buildHtml :: ReportConfig -> [ReportSection] -> Text
buildHtml cfg sections =
  let pairs   = zip (map sectionId [0..]) sections
      body    = T.intercalate "\n" [ renderSection sid s | (sid, s) <- pairs ]
      scripts = T.intercalate "\n" [ sectionScript sid s | (sid, s) <- pairs ]
      navBar  = mkNavBar cfg pairs
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
       , "<script>window.MathJax = { tex: {"
       , "  inlineMath: [['$','$'], ['\\\\(','\\\\)']],"
       , "  displayMath: [['$$','$$'], ['\\\\[','\\\\]']]"
       , "}, svg: { fontCache: 'global' } };</script>"
       , "<script src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js\""
       , "        async></script>"
       , "<style>" <> css <> "</style>"
       , "</head>"
       , "<body>"
       , navBar
       , "<main>"
       , body
       , "</main>"
       , "<script>"
       , "mermaid.initialize({ startOnLoad: true, theme: 'default' });"
       , scripts
       , "document.querySelectorAll('.nav-link').forEach(a => {"
       , "  a.addEventListener('click', e => {"
       , "    const target = document.querySelector(a.getAttribute('href'));"
       , "    if (target) { e.preventDefault();"
       , "      target.scrollIntoView({ behavior: 'smooth' }); }"
       , "  });"
       , "});"
       , "</script>"
       , "</body>"
       , "</html>"
       ]

-- | ナビバーを構築。各 section のタイトルから生成。
mkNavBar :: ReportConfig -> [(Text, ReportSection)] -> Text
mkNavBar cfg pairs =
  let links = [ "  <a class=\"nav-link\" href=\"#" <> sid <> "\">"
                <> shortTitle s <> "</a>"
              | (sid, s) <- pairs
              , not (isInvisible s) ]
  in T.unlines $
       [ "<nav>"
       , "  <h1>&#128202; " <> rcTitle cfg <> "</h1>"
       ] ++ links ++ [ "</nav>" ]
  where
    isInvisible (SecHtml _ _) = False
    isInvisible _             = False
    shortTitle s = case s of
      SecDataOverview {}     -> "データ"
      SecModelOverview {}    -> "モデル"
      SecCoefficients {}     -> "係数"
      SecFitScatter {}       -> "散布図"
      SecResiduals {}        -> "残差"
      SecBarChart t _        -> if T.null t then "図表" else t
      SecVega t _            -> if T.null t then "図表" else t
      SecMermaid _           -> "DAG"
      SecTable t _ _         -> if T.null t then "表" else t
      SecKeyValue t _        -> if T.null t then "情報" else t
      SecMarkdown t _        -> if T.null t then "備考" else t
      SecHtml t _            -> if T.null t then "付録" else t
      SecInteractiveLM {}    -> "対話的予測"
      SecInteractiveMulti {} -> "対話的予測"
      SecInteractiveRFFMV {} -> "対話的予測"
      SecInteractiveMultiOut {} -> "対話的予測"
      SecCollapsible t _ _   -> if T.null t then "詳細" else t
      SecCard t _            -> if T.null t then "" else t
      SecStatRow _           -> ""

sectionId :: Int -> Text
sectionId i = "sec_" <> T.pack (show i)

-- ---------------------------------------------------------------------------
-- セクション → HTML
-- ---------------------------------------------------------------------------

renderSection :: Text -> ReportSection -> Text
renderSection sid sec = case sec of
  SecDataOverview df xs y     -> renderDataOverview sid df xs y
  SecModelOverview ty fm extras mer -> renderModelOverview sid ty fm extras mer
  SecCoefficients cs mr2      -> renderCoefficients sid cs mr2
  SecFitScatter xc yc xs ys s -> renderFitScatter sid xc yc xs ys s
  SecResiduals fit res        -> renderResiduals sid fit res
  SecBarChart t vs            -> renderBarChart sid t vs
  SecVega t _                 -> renderVegaPlaceholder sid t
  SecMermaid m                -> renderMermaid sid m
  SecTable t hs rs            -> renderTable sid t hs rs
  SecKeyValue t kvs           -> renderKeyValue sid t kvs
  SecMarkdown t txt           -> renderMarkdown sid t txt
  -- SecHtml は <section> ラッパを付けず、生 HTML を <div id> で囲むのみ。
  -- 利用側で <section> を含む完全な HTML を渡すことを想定 (secAppendixFromMd 等)。
  SecHtml _ html              ->
    "<div id=\"" <> sid <> "\" class=\"raw-section\">" <> html <> "</div>"
  SecInteractiveLM t xc yc xs ys sc rng -> renderInteractiveLM sid t xc yc xs ys sc rng
  SecInteractiveMulti t im   -> renderInteractiveMulti sid t im
  SecInteractiveRFFMV t r    -> renderInteractiveRFFMV sid t r
  SecInteractiveMultiOut t imo -> renderInteractiveMultiOut sid t imo
  SecCollapsible t open children ->
    renderCollapsible sid t open children
  SecCard t children     -> renderCard sid t children
  SecStatRow kvs         -> renderStatRow sid kvs

wrapSection :: Text -> Text -> Text -> Text
wrapSection sid title inner = T.unlines
  [ "<section id=\"" <> sid <> "\">"
  , if T.null title then "" else "  <h2>" <> title <> "</h2>"
  , inner
  , "</section>"
  ]

-- | 折りたたみ可能な section 箱 (white bg、h2 をクリックで折りたたみ)。
-- データの特性 / モデル概要などで使う。
collapsibleSection :: Text -> Text -> Bool -> Text -> Text
collapsibleSection sid title open inner =
  let attr = if open then " open" else ""
  in T.unlines
       [ "<section id=\"" <> sid <> "\" class=\"collapsible-wrap\">"
       , "  <details" <> attr <> ">"
       , "    <summary><h2>" <> title <> "</h2></summary>"
       , "    <div class=\"collapsible-body\">"
       , inner
       , "    </div>"
       , "  </details>"
       , "</section>"
       ]

-- データ概要 -----------------------------------------------------------------

-- | 列ごとの簡易分類: 数値列なら @NumCol [Double]@、Text 列なら @TxtCol [Text]@、
-- 取得不能なら 'NoCol'。ReportBuilder 内部のみで使用。
data ColView = NumCol [Double] | TxtCol [Text] | NoCol

classifyCol :: Text -> DXD.DataFrame -> ColView
classifyCol c df = case getDoubleVec c df of
  Just v  -> NumCol (V.toList v)
  Nothing -> case getTextVec c df of
    Just v  -> TxtCol (V.toList v)
    Nothing -> NoCol

renderDataOverview :: Text -> DXD.DataFrame -> [Text] -> Text -> Text
renderDataOverview sid df xCols yCol =
  let allCols  = xCols ++ [yCol]
      relevant = [ (i, c, classifyCol c df) | (i, c) <- zip [0::Int ..] allCols ]
      (n, _)   = DX.dimensions df
      header   =
        T.concat
          [ "<tr>"
          , "<th>列</th><th>型</th><th>N</th>"
          , "<th>欠損</th>"
          , "<th>最小</th><th>Q1</th><th>中央</th><th>Q3</th><th>最大</th>"
          , "<th>平均</th><th>SD</th>"
          , "<th>歪度</th><th>尖度</th>"
          , "</tr>"
          ]
      rows = T.intercalate "\n" (map renderColRow relevant)
      summary = "行数: <strong>" <> T.pack (show n)
                <> "</strong>, 解析対象列: <strong>"
                <> T.pack (show (length allCols)) <> "</strong>"
      -- ヒストグラム (グループ全体で 1 つのトグル、各列は独立カード)
      histBlocks = T.intercalate "\n"
        [ "  <div class=\"hist-card\"><div class=\"hist-title\">" <> c
          <> "</div><div class=\"vl-wrap\"><div id=\"hist_" <> sid
          <> "_" <> T.pack (show i) <> "\"></div></div></div>"
        | (i, c, NumCol _) <- relevant ]
      title = "<span class=\"sec-icon\">&#128202;</span> データの特性"
      body  = T.unlines
        [ "<p class=\"sec-desc\">" <> summary <> "</p>"
        , "<div class=\"table-scroll\"><table class=\"stats-table\">"
        , "<thead>" <> header <> "</thead>"
        , "<tbody>" <> rows <> "</tbody>"
        , "</table></div>"
        , "<details class=\"hist-toggle\"><summary>ヒストグラム (列ごと)</summary>"
        , "<div class=\"hist-grid\">"
        , histBlocks
        , "</div>"
        , "</details>"
        ]
  in collapsibleSection sid title True body
  where
    renderColRow (_, c, NumCol xs) =
      let m  = length xs
          ss = sort xs
          mn = if m == 0 then 0 else minimum xs
          mx = if m == 0 then 0 else maximum xs
          mean = if m == 0 then 0 else sum xs / fromIntegral m
          q1   = if m == 0 then 0 else ss !! (m `div` 4)
          med  = if m == 0 then 0 else ss !! (m `div` 2)
          q3   = if m == 0 then 0 else ss !! (3 * m `div` 4)
          var  = if m <= 1 then 0
                 else sum [(x - mean) ^ (2 :: Int) | x <- xs]
                       / fromIntegral (m - 1)
          sdv  = sqrt var
          skew = if sdv <= 1e-12 then 0
                 else sum [((x - mean) / sdv) ^ (3 :: Int) | x <- xs]
                      / fromIntegral m
          kurt = if sdv <= 1e-12 then 0
                 else sum [((x - mean) / sdv) ^ (4 :: Int) | x <- xs]
                      / fromIntegral m - 3
      in "<tr>" <> T.intercalate ""
           [ td c, td "numeric", td (T.pack (show m)), td "0"
           , td (showD4 mn), td (showD4 q1), td (showD4 med)
           , td (showD4 q3), td (showD4 mx)
           , td (showD4 mean), td (showD4 sdv)
           , td (showD4 skew), td (showD4 kurt)
           ] <> "</tr>"
    renderColRow (_, c, TxtCol xs) =
      let m  = length xs
          uniq = length (unique xs)
      in "<tr>" <> T.intercalate ""
           [ td c, td "text", td (T.pack (show m))
           , td "0"
           , td "—", td "—", td "—", td "—", td "—", td "—"
           , td ("unique=" <> T.pack (show uniq))
           , td "—"
           ] <> "</tr>"
    renderColRow (_, c, NoCol) =
      "<tr><td>" <> c <> "</td><td colspan=12>(missing)</td></tr>"
    td x = "<td>" <> x <> "</td>"
    unique = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- | データ概要セクションのスクリプト: 各 numeric 列のヒストグラムを embed。
dataOverviewScript :: Text -> DXD.DataFrame -> [Text] -> Text -> Text
dataOverviewScript sid df xCols yCol =
  let allCols = xCols ++ [yCol]
      pairs = [ (i, c, getDoubleVec c df)
              | (i, c) <- zip [0::Int ..] allCols ]
      embed i v =
        let json = decodeUtf8 . toStrict . encode . fromVL $
                    histogramSpec (allCols !! i) (V.toList v)
        in "vegaEmbed('#hist_" <> sid <> "_" <> T.pack (show i)
           <> "', " <> json <> ", {actions:false});"
  in T.intercalate "\n"
       [ embed i v | (i, _, Just v) <- pairs ]

-- | 単純なヒストグラム Vega-Lite spec。
histogramSpec :: Text -> [Double] -> VegaLite
histogramSpec col vals =
  toVegaLite
    [ dataFromColumns []
        . dataColumn col (Numbers vals)
        $ []
    , mark Bar [MOpacity 0.85, MColor "#4C72B0"]
    , encoding
        . position X [PName col, PmType Quantitative,
                      PBin [], PAxis [AxTitle col]]
        . position Y [PAggregate Count, PmType Quantitative,
                      PAxis [AxTitle "Count"]]
        $ []
    , width 320
    , height 160
    ]

-- モデル概要 -----------------------------------------------------------------

renderModelOverview :: Text -> Text -> Text -> [(Text, Text)] -> Maybe Text -> Text
renderModelOverview sid ty formula extras mer =
  let merBlock = case mer of
        Nothing -> ""
        Just m  ->
          T.unlines
            [ "<h3>モデル構造 (DAG)</h3>"
            , "<div class=\"mermaid-wrap\"><div class=\"mermaid\">"
            , m
            , "</div></div>"
            ]
      extraBox (lbl, val) = T.unlines
        [ "  <div class=\"info-box\">"
        , "    <div class=\"lbl\">" <> lbl <> "</div>"
        , "    <div class=\"ival\">" <> val <> "</div>"
        , "  </div>"
        ]
      extraBoxes = T.concat (map extraBox extras)
  in collapsibleSection sid "<span class=\"sec-icon\">&#9878;</span> モデル概要" True $
       T.unlines
         [ "<div class=\"info-grid\">"
         , "  <div class=\"info-box\">"
         , "    <div class=\"lbl\">モデル種別</div>"
         , "    <div class=\"ival\">" <> ty <> "</div>"
         , "  </div>"
         , extraBoxes
         , "  <div class=\"info-box\" style=\"flex: 2\">"
         , "    <div class=\"lbl\">数式</div>"
         , "    <div class=\"ival\">" <> formula <> "</div>"
         , "  </div>"
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
  in wrapSection sid "係数" $ T.unlines
       [ "<table class=\"narrow\">"
       , "<thead><tr><th>パラメータ</th><th>値</th></tr></thead>"
       , "<tbody>" <> rows <> "</tbody>"
       , r2Row
       , "</table>"
       ]

-- 散布図 + 滑らか曲線 -------------------------------------------------------

renderFitScatter :: Text -> Text -> Text -> [Double] -> [Double]
                 -> Maybe SmoothCurve -> Text
renderFitScatter sid _xc _yc _xs _ys _msc =
  wrapSection sid "散布図 + 適合曲線" $
    "<div class=\"vl-wrap\"><div id=\"vl-" <> sid <> "\"></div></div>"

-- 残差 -----------------------------------------------------------------------

renderResiduals :: Text -> [Double] -> [Double] -> Text
renderResiduals sid _fitted _resids =
  wrapSection sid "残差" $
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

-- Interactive Multi (multivariate) -----------------------------------------

renderInteractiveMulti :: Text -> Text -> InteractiveModel -> Text
renderInteractiveMulti sid title im =
  let xCols  = imXCols im
      sliders = imSlider im
      xCount = length xCols
      sliderHtml = T.intercalate "\n"
        [ T.unlines
            [ "<div class=\"slider-row\">"
            , "  <label>" <> col <> ":"
            , "    <input type=\"range\" id=\"i-" <> sid <> "-s" <> T.pack (show i) <> "\""
            , "      min=\"" <> showD4 mn <> "\""
            , "      max=\"" <> showD4 mx <> "\""
            , "      step=\"" <> showD4 ((mx - mn) / 200) <> "\""
            , "      value=\"" <> showD4 mid <> "\""
            , "      oninput=\"window.__updMulti_" <> sid <> "()\">"
            , "    <span id=\"i-" <> sid <> "-s" <> T.pack (show i)
              <> "-val\">" <> showD4 mid <> "</span>"
            , "  </label>"
            , "</div>"
            ]
        | (i, col, (mn, mid, mx)) <- zip3 [0::Int ..] xCols sliders ]
      primaryDropdown = T.unlines
        [ "<div class=\"slider-row\">"
        , "  <label>Primary axis (chart x):"
        , "    <select id=\"i-" <> sid <> "-primary\""
        , "            onchange=\"window.__updMulti_" <> sid <> "()\">"
        , T.intercalate "\n"
            [ "      <option value=\"" <> T.pack (show i)
              <> "\">" <> col <> "</option>"
            | (i, col) <- zip [0::Int ..] xCols ]
        , "    </select>"
        , "  </label>"
        , "</div>"
        ]
      _ = xCount
      tFull = "<span class=\"sec-icon\">&#127919;</span> "
              <> (if T.null title then "対話的予測" else title)
  in collapsibleSection sid tFull True $
       T.unlines
         [ "<div class=\"interactive-multi\">"
         , "  <div class=\"i-controls\">"
         , primaryDropdown
         , sliderHtml
         , "    <div class=\"pred-output\">"
         , "      <div><strong>Predicted " <> imYCol im <> ":</strong>"
         , "        <span id=\"i-" <> sid <> "-yhat\">—</span></div>"
         , "      <div class=\"band-readout\">95% CI:"
         , "        <span id=\"i-" <> sid <> "-ci\">—</span></div>"
         , "    </div>"
         , "  </div>"
         , "  <div class=\"i-chart\">"
         , "    <div class=\"vl-wrap\"><div id=\"vl-" <> sid <> "\"></div></div>"
         , "  </div>"
         , "</div>"
         ]

interactiveMultiScript :: Text -> InteractiveModel -> Text
interactiveMultiScript sid im =
  let xCols   = imXCols im
      yCol    = imYCol im
      betas   = imBetas im
      icpt    = imIntercept im
      link    = imLink im
      xVals   = imXValues im
      yVals   = imYValues im
      ciSigma = maybe 0 id (imCISigma im)
      hasCI   = case imCISigma im of { Just s -> s > 0; _ -> False }
      arrD xs = "[" <> T.intercalate "," (map showD4 xs) <> "]"
      arrS xs = "[" <> T.intercalate "," (map (\s -> "\"" <> s <> "\"") xs) <> "]"
      xMatJson =
        "[" <> T.intercalate ","
                  [ arrD row | row <- xVals ] <> "]"
      yArrJson = arrD yVals
      betasArr = arrD betas
  in T.unlines
       [ "(() => {"
       , "  const xCols = " <> arrS xCols <> ";"
       , "  const yCol  = \"" <> yCol <> "\";"
       , "  const xMat  = " <> xMatJson <> ";"
       , "  const yArr  = " <> yArrJson <> ";"
       , "  const beta0 = " <> showD4 icpt <> ";"
       , "  const betas = " <> betasArr <> ";"
       , "  const link  = \"" <> link <> "\";"
       , "  const sigma = " <> showD4 ciSigma <> ";"
       , "  const hasCI = " <> (if hasCI then "true" else "false") <> ";"
       , "  const invLink = (eta) => {"
       , "    if (link === 'log')   return Math.exp(eta);"
       , "    if (link === 'logit') return 1.0/(1.0+Math.exp(-eta));"
       , "    if (link === 'sqrt')  return eta * eta;"
       , "    return eta;"
       , "  };"
       , "  const predEta = (xs) => {"
       , "    let e = beta0;"
       , "    for (let i = 0; i < betas.length; i++) e += betas[i] * xs[i];"
       , "    return e;"
       , "  };"
       , "  const sliderVals = () => xCols.map((_, i) =>"
       , "    parseFloat(document.getElementById('i-" <> sid <> "-s' + i).value));"
       , "  const primaryIdx = () =>"
       , "    parseInt(document.getElementById('i-" <> sid <> "-primary').value);"
       , "  let chartView = null;"
       , "  const baseSpec = (pIdx, sliderXs) => {"
       , "    const pCol = xCols[pIdx];"
       , "    // primary 軸の min/max"
       , "    let pMin = Infinity, pMax = -Infinity;"
       , "    for (const row of xMat) {"
       , "      if (row[pIdx] < pMin) pMin = row[pIdx];"
       , "      if (row[pIdx] > pMax) pMax = row[pIdx];"
       , "    }"
       , "    const ext = (pMax - pMin) * 0.5;"
       , "    pMin -= ext; pMax += ext;"
       , "    // slider 範囲も外挿に含める"
       , "    const sMin = parseFloat(document.getElementById('i-" <> sid <> "-s' + pIdx).min);"
       , "    const sMax = parseFloat(document.getElementById('i-" <> sid <> "-s' + pIdx).max);"
       , "    pMin = Math.min(pMin, sMin);"
       , "    pMax = Math.max(pMax, sMax);"
       , "    const N = 120;"
       , "    const grid = [];"
       , "    for (let i = 0; i < N; i++)"
       , "      grid.push(pMin + i * (pMax - pMin) / (N - 1));"
       , "    // 予測曲線: 副軸を slider 値で固定、primary を grid で動かす"
       , "    const curve = grid.map(p => {"
       , "      const xs = sliderXs.slice();"
       , "      xs[pIdx] = p;"
       , "      const eta = predEta(xs);"
       , "      const y = invLink(eta);"
       , "      return { gx: p, gy: y, lo: y - 1.96 * sigma, hi: y + 1.96 * sigma };"
       , "    });"
       , "    const obs = xMat.map((row, i) => ({ x: row[pIdx], y: yArr[i] }));"
       , "    // 予測マーカー (slider 位置の現在予測値)"
       , "    const curEta = predEta(sliderXs);"
       , "    const curY   = invLink(curEta);"
       , "    const predPoint = [{ px: sliderXs[pIdx], py: curY }];"
       , "    const layers = ["
       , "      { data: { values: obs },"
       , "        mark: { type: 'point', opacity: 0.55, size: 50, color: '#5b8bbf' },"
       , "        encoding: {"
       , "          x: { field: 'x', type: 'quantitative', axis: { title: pCol } },"
       , "          y: { field: 'y', type: 'quantitative', axis: { title: yCol } } } }"
       , "    ];"
       , "    if (hasCI) {"
       , "      layers.push({"
       , "        data: { values: curve },"
       , "        mark: { type: 'area', opacity: 0.18, color: '#e74c3c' },"
       , "        encoding: {"
       , "          x: { field: 'gx', type: 'quantitative' },"
       , "          y: { field: 'lo', type: 'quantitative' },"
       , "          y2:{ field: 'hi' } } });"
       , "    }"
       , "    layers.push({"
       , "      data: { values: curve },"
       , "      mark: { type: 'line', color: '#e74c3c', strokeWidth: 2.5 },"
       , "      encoding: {"
       , "        x: { field: 'gx', type: 'quantitative' },"
       , "        y: { field: 'gy', type: 'quantitative' } } });"
       , "    // 予測マーカー (大きい赤丸)"
       , "    layers.push({"
       , "      data: { values: predPoint },"
       , "      mark: { type: 'point', filled: true, size: 250, color: '#c0392b',"
       , "              stroke: 'white', strokeWidth: 2 },"
       , "      encoding: {"
       , "        x: { field: 'px', type: 'quantitative' },"
       , "        y: { field: 'py', type: 'quantitative' } } });"
       , "    return { '$schema': 'https://vega.github.io/schema/vega-lite/v4.json',"
       , "             layer: layers, width: 600, height: 320 };"
       , "  };"
       , "  window.__updMulti_" <> sid <> " = function() {"
       , "    const xs = sliderVals();"
       , "    xCols.forEach((_, i) => {"
       , "      document.getElementById('i-" <> sid <> "-s' + i + '-val')"
       , "        .textContent = xs[i].toFixed(3);"
       , "    });"
       , "    const eta = predEta(xs);"
       , "    const yhat = invLink(eta);"
       , "    document.getElementById('i-" <> sid <> "-yhat').textContent = yhat.toFixed(4);"
       , "    if (hasCI) {"
       , "      const lo = yhat - 1.96 * sigma;"
       , "      const hi = yhat + 1.96 * sigma;"
       , "      document.getElementById('i-" <> sid <> "-ci').textContent ="
       , "        '[' + lo.toFixed(3) + ', ' + hi.toFixed(3) + ']';"
       , "    } else {"
       , "      document.getElementById('i-" <> sid <> "-ci').textContent = '—';"
       , "    }"
       , "    const pIdx = primaryIdx();"
       , "    vegaEmbed('#vl-" <> sid <> "', baseSpec(pIdx, xs),"
       , "              {actions:false}).then(r => { chartView = r.view; });"
       , "  };"
       , "  // 初期描画"
       , "  window.__updMulti_" <> sid <> "();"
       , "})();"
       ]

-- Collapsible group ---------------------------------------------------------

childId :: Text -> Int -> Text
childId sid i = sid <> "_c" <> T.pack (show i)

renderCollapsible :: Text -> Text -> Bool -> [ReportSection] -> Text
renderCollapsible sid title open children =
  let childHtml = T.intercalate "\n"
        [ renderSection (childId sid i) c
        | (i, c) <- zip [0::Int ..] children ]
      attr = if open then " open" else ""
  in T.unlines
       [ "<section id=\"" <> sid <> "\" class=\"collapsible-wrap\">"
       , "  <details" <> attr <> ">"
       , "    <summary><h2>" <> title <> "</h2></summary>"
       , "    <div class=\"collapsible-body\">"
       , childHtml
       , "    </div>"
       , "  </details>"
       , "</section>"
       ]

-- | 淡い背景色のカード。子セクションの section ラッパは CSS で flat 化される。
renderCard :: Text -> Text -> [ReportSection] -> Text
renderCard sid title children =
  let childHtml = T.intercalate "\n"
        [ renderSection (childId sid i) c
        | (i, c) <- zip [0::Int ..] children ]
      titleHtml = if T.null title then ""
                  else "  <h3 class=\"card-title\">" <> title <> "</h3>"
  in T.unlines
       [ "<div class=\"result-card\" id=\"" <> sid <> "\">"
       , titleHtml
       , childHtml
       , "</div>"
       ]

-- | フラットな統計行 (section box なし)。
renderStatRow :: Text -> [(Text, Text)] -> Text
renderStatRow sid kvs =
  let boxes = T.intercalate "\n"
        [ "  <div class=\"stat-box\">"
          <> "<div class=\"lbl\">" <> k
          <> "</div><div class=\"val\">" <> v <> "</div></div>"
        | (k, v) <- kvs ]
  in T.unlines
       [ "<div class=\"stat-row\" id=\"" <> sid <> "\">"
       , boxes
       , "</div>"
       ]

-- Interactive LM ------------------------------------------------------------

renderInteractiveLM :: Text -> Text -> Text -> Text
                    -> [Double] -> [Double]
                    -> SmoothCurve -> (Double, Double) -> Text
renderInteractiveLM sid title xc yc _xs _ys _sc (xMin, xMax) =
  let mid  = (xMin + xMax) / 2
      step = (xMax - xMin) / 200
      tFull = "<span class=\"sec-icon\">&#127919;</span> "
              <> (if T.null title then "対話的予測" else title)
  in collapsibleSection sid tFull True $
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
  SecInteractiveMulti _ im ->
    interactiveMultiScript sid im
  SecInteractiveRFFMV _ r ->
    interactiveRFFMVScript sid r
  SecInteractiveMultiOut _ imo ->
    interactiveMultiOutScript sid imo
  SecCollapsible _ _ children ->
    T.intercalate "\n"
      [ sectionScript (childId sid i) child
      | (i, child) <- zip [0::Int ..] children ]
  SecCard _ children ->
    T.intercalate "\n"
      [ sectionScript (childId sid i) child
      | (i, child) <- zip [0::Int ..] children ]
  SecDataOverview df xCols yCol ->
    dataOverviewScript sid df xCols yCol
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
  let gridX = scXs sc
      gridY = scYs sc
      gridLo = scLower sc
      gridHi = scUpper sc
      hasBand = not (null gridLo) && length gridLo == length gridX
      arr xs0 = "[" <> T.intercalate "," (map showD4 xs0) <> "]"
      arrObs xs0 ys0 = "[" <> T.intercalate ","
        [ "{\"x\":" <> showD4 x <> ",\"y\":" <> showD4 y <> "}"
        | (x, y) <- zip xs0 ys0 ] <> "]"
  in T.unlines
       [ "(() => {"
       , "  const gx = " <> arr gridX <> ";"
       , "  const gy = " <> arr gridY <> ";"
       , "  const gl = " <> arr (if hasBand then gridLo else []) <> ";"
       , "  const gh = " <> arr (if hasBand then gridHi else []) <> ";"
       , "  const obs = " <> arrObs xs ys <> ";"
       , "  const xc = \"" <> xc <> "\";"
       , "  const yc = \"" <> yc <> "\";"
       , "  const hasBand = gl.length > 0;"
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
       , "  const buildSpec = (curX) => {"
       , "    const curY = interp(curX, gx, gy);"
       , "    const layers = ["
       , "      { data: { values: obs },"
       , "        mark: { type: 'point', opacity: 0.55, size: 50, color: '#5b8bbf' },"
       , "        encoding: {"
       , "          x: { field: 'x', type: 'quantitative', axis: { title: xc } },"
       , "          y: { field: 'y', type: 'quantitative', axis: { title: yc } } } }"
       , "    ];"
       , "    if (hasBand) {"
       , "      const bandData = gx.map((x, i) => ({ gx: x, lo: gl[i], hi: gh[i] }));"
       , "      layers.push({"
       , "        data: { values: bandData },"
       , "        mark: { type: 'area', opacity: 0.18, color: '#e74c3c' },"
       , "        encoding: {"
       , "          x: { field: 'gx', type: 'quantitative' },"
       , "          y: { field: 'lo', type: 'quantitative' },"
       , "          y2:{ field: 'hi' } } });"
       , "    }"
       , "    const lineData = gx.map((x, i) => ({ gx: x, gy: gy[i] }));"
       , "    layers.push({"
       , "      data: { values: lineData },"
       , "      mark: { type: 'line', color: '#e74c3c', strokeWidth: 2.5 },"
       , "      encoding: {"
       , "        x: { field: 'gx', type: 'quantitative' },"
       , "        y: { field: 'gy', type: 'quantitative' } } });"
       , "    layers.push({"
       , "      data: { values: [{ px: curX, py: curY }] },"
       , "      mark: { type: 'point', filled: true, size: 250, color: '#c0392b',"
       , "              stroke: 'white', strokeWidth: 2 },"
       , "      encoding: {"
       , "        x: { field: 'px', type: 'quantitative' },"
       , "        y: { field: 'py', type: 'quantitative' } } });"
       , "    return { '$schema': 'https://vega.github.io/schema/vega-lite/v4.json',"
       , "             layer: layers, width: 600, height: 320 };"
       , "  };"
       , "  window.__upd_" <> sid <> " = function(v) {"
       , "    const x = parseFloat(v);"
       , "    document.getElementById('i-" <> sid <> "-x').textContent = x.toFixed(3);"
       , "    const y = interp(x, gx, gy);"
       , "    document.getElementById('i-" <> sid <> "-y').textContent ="
       , "      y === null ? '—' : y.toFixed(4);"
       , "    if (hasBand) {"
       , "      const lo = interp(x, gx, gl);"
       , "      const hi = interp(x, gx, gh);"
       , "      document.getElementById('i-" <> sid <> "-band').textContent ="
       , "        ' [' + lo.toFixed(3) + ', ' + hi.toFixed(3) + ']';"
       , "    }"
       , "    vegaEmbed('#vl-" <> sid <> "', buildSpec(x), {actions:false});"
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

-- | Forest plot — 各パラメータの中央値 (点) と HDI/CI (横棒)。
forestPlotSpec :: [(Text, Double, Double, Double)] -> VegaLite
forestPlotSpec rows =
  let names = [n | (n, _, _, _) <- rows]
      means = [m | (_, _, m, _) <- rows]
      los   = [l | (_, l, _, _) <- rows]
      his   = [h | (_, _, _, h) <- rows]
  in toVegaLite
       [ dataFromColumns []
           . dataColumn "param" (Strings names)
           . dataColumn "mean"  (Numbers means)
           . dataColumn "lo"    (Numbers los)
           . dataColumn "hi"    (Numbers his)
           $ []
       , layer
           [ asSpec
               [ mark Rule [MStrokeWidth 2.4, MColor "#4C72B0"]
               , encoding
                   . position Y  [PName "param", PmType Nominal,
                                  PAxis [AxTitle ""]]
                   . position X  [PName "lo", PmType Quantitative,
                                  PAxis [AxTitle "推定値"]]
                   . position X2 [PName "hi"]
                   $ []
               ]
           , asSpec
               [ mark Circle [MSize 110, MColor "#1e3a5c", MOpacity 0.95]
               , encoding
                   . position Y [PName "param", PmType Nominal]
                   . position X [PName "mean", PmType Quantitative]
                   $ []
               ]
           ]
       , width 540
       , heightStep 28
       ]

-- | Posterior Predictive Check — 観測 KDE + 各 replicate KDE 重ね描き。
ppcSpec :: [Double] -> [[Double]] -> VegaLite
ppcSpec observed reps =
  let nGrid = 200
      obsKde   = SM.kde nGrid observed
      repKdes  = [ SM.kde nGrid r | r <- reps, not (null r) ]
      obsRows  = [ (x, y, "観測 (y_obs)" :: Text, 0 :: Int) | (x, y) <- obsKde ]
      repRows  = [ (x, y, "事後予測", j)
                 | (j, kd) <- zip [1 :: Int ..] repKdes
                 , (x, y)  <- kd ]
      rows     = obsRows ++ repRows
      xs    = [ x  | (x, _, _, _) <- rows ]
      ys    = [ y  | (_, y, _, _) <- rows ]
      grps  = [ g  | (_, _, g, _) <- rows ]
      idx   = [ T.pack ("rep_" <> show k) | (_, _, _, k) <- rows ]
  in toVegaLite
       [ dataFromColumns []
           . dataColumn "x"     (Numbers xs)
           . dataColumn "y"     (Numbers ys)
           . dataColumn "group" (Strings grps)
           . dataColumn "rep"   (Strings idx)
           $ []
       , layer
           [ asSpec
               [ transform . VL.filter (FExpr "datum.group === '事後予測'") $ []
               , mark Line [MStrokeWidth 0.7, MOpacity 0.25, MColor "#888"]
               , encoding
                   . position X [PName "x", PmType Quantitative,
                                 PAxis [AxTitle "y"]]
                   . position Y [PName "y", PmType Quantitative,
                                 PAxis [AxTitle "密度"]]
                   . detail [DName "rep", DmType Nominal]
                   $ []
               ]
           , asSpec
               [ transform . VL.filter (FExpr "datum.group === '観測 (y_obs)'") $ []
               , mark Line [MStrokeWidth 2.4, MColor "#1e3a5c"]
               , encoding
                   . position X [PName "x", PmType Quantitative]
                   . position Y [PName "y", PmType Quantitative]
                   $ []
               ]
           ]
       , width 640
       , height 280
       ]

-- | Calibration spec: 10 ビンに分割し (mean p, observed freq) を点 + 対角線で描画。
calibrationSpec :: [Double] -> [Double] -> VegaLite
calibrationSpec pPred yObs =
  let pairs = zip pPred yObs
      bin p
        | p >= 1.0  = 9
        | p <= 0.0  = 0
        | otherwise = max 0 (min 9 (floor (p * 10) :: Int))
      bins = [0 .. 9 :: Int]
      perBin =
        [ let inB = [ (p, y) | (p, y) <- pairs, bin p == k ]
              n   = length inB
              mP  = if n == 0 then fromIntegral k / 10 + 0.05
                    else sum (map fst inB) / fromIntegral n
              mY  = if n == 0 then 0
                    else sum (map snd inB) / fromIntegral n
          in (k, n, mP, mY)
        | k <- bins ]
      nonEmpty = [ (mP, mY, n) | (_, n, mP, mY) <- perBin, n > 0 ]
      meanPs = [ p | (p, _, _) <- nonEmpty ]
      meanYs = [ y | (_, y, _) <- nonEmpty ]
      counts = [ fromIntegral n :: Double | (_, _, n) <- nonEmpty ]
      diagXs = [0, 1] :: [Double]
      diagYs = [0, 1] :: [Double]
  in toVegaLite
       [ layer
           [ asSpec
               [ dataFromColumns []
                   . dataColumn "x" (Numbers diagXs)
                   . dataColumn "y" (Numbers diagYs)
                   $ []
               , mark Line [MStrokeWidth 1.2, MColor "#888", MStrokeDash [4, 4]]
               , encoding
                   . position X [PName "x", PmType Quantitative,
                                 PScale [SDomain (DNumbers [0, 1])],
                                 PAxis [AxTitle "予測確率 (mean)"]]
                   . position Y [PName "y", PmType Quantitative,
                                 PScale [SDomain (DNumbers [0, 1])],
                                 PAxis [AxTitle "観測頻度"]]
                   $ []
               ]
           , asSpec
               [ dataFromColumns []
                   . dataColumn "p"     (Numbers meanPs)
                   . dataColumn "y"     (Numbers meanYs)
                   . dataColumn "count" (Numbers counts)
                   $ []
               , mark Circle [MOpacity 0.85, MColor "#1e3a5c"]
               , encoding
                   . position X [PName "p", PmType Quantitative]
                   . position Y [PName "y", PmType Quantitative]
                   . size [MName "count", MmType Quantitative,
                           MLegend [LTitle "n"]]
                   $ []
               ]
           ]
       , width 480
       , height 380
       ]

-- | 3D scatter (z は色エンコード)。
scatter3DSpec :: Text -> Text -> Text -> [Double] -> [Double] -> [Double]
              -> VegaLite
scatter3DSpec xL yL zL xs ys zs =
  toVegaLite
    [ dataFromColumns []
        . dataColumn xL (Numbers xs)
        . dataColumn yL (Numbers ys)
        . dataColumn zL (Numbers zs)
        $ []
    , mark Circle [MSize 80, MOpacity 0.85]
    , encoding
        . position X [PName xL, PmType Quantitative,
                      PAxis [AxTitle xL]]
        . position Y [PName yL, PmType Quantitative,
                      PAxis [AxTitle yL]]
        . color [MName zL, MmType Quantitative,
                 MScale [SScheme "viridis" []],
                 MLegend [LTitle zL]]
        $ []
    , width 560
    , height 380
    ]

-- | 2D heatmap (rect + 色エンコード)。
heatmapSpec :: [Text] -> [Text] -> [[Double]] -> VegaLite
heatmapSpec colLabels rowLabels values =
  let rows = [ (rLbl, cLbl, v)
             | (rLbl, rowVals) <- zip rowLabels values
             , (cLbl, v)       <- zip colLabels rowVals ]
      rs   = [ r | (r, _, _) <- rows ]
      cs   = [ c | (_, c, _) <- rows ]
      vs   = [ v | (_, _, v) <- rows ]
  in toVegaLite
       [ dataFromColumns []
           . dataColumn "row" (Strings rs)
           . dataColumn "col" (Strings cs)
           . dataColumn "val" (Numbers vs)
           $ []
       , mark Rect [MStroke "#fff", MStrokeWidth 0.5]
       , encoding
           . position X [PName "col", PmType Nominal,
                         PAxis [AxTitle "", AxLabelAngle (-30)]]
           . position Y [PName "row", PmType Nominal,
                         PAxis [AxTitle ""]]
           . color [MName "val", MmType Quantitative,
                    MScale [SScheme "viridis" []],
                    MLegend [LTitle "値"]]
           $ []
       , width 520
       , height 380
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
  , "body { font-family: 'Segoe UI', system-ui, sans-serif; background: #f0f2f5;"
  , "       color: #333; line-height: 1.6; }"
  , "nav { position: sticky; top: 0; z-index: 100; background: #1e3a5c;"
  , "      padding: 10px 28px; display: flex; gap: 18px; align-items: center;"
  , "      box-shadow: 0 2px 6px rgba(0,0,0,.25); flex-wrap: wrap; }"
  , "nav h1 { color: #ecf0f1; font-size: 1em; font-weight: 600; flex: 1; min-width: 250px; }"
  , ".nav-link { color: #9ab; text-decoration: none; font-size: .82em; white-space: nowrap; }"
  , ".nav-link:hover { color: #fff; }"
  , "main { max-width: 1160px; margin: 0 auto; padding: 32px 20px; }"
  , "section { background: white; border-radius: 12px; padding: 26px 28px;"
  , "          margin-bottom: 28px; box-shadow: 0 2px 10px rgba(0,0,0,.07); }"
  , "h2 { font-size: 1.05em; font-weight: 700; color: #1e3a5c; margin-bottom: 18px;"
  , "     border-bottom: 2px solid #e4e9f0; padding-bottom: 8px; }"
  , "h3 { font-size: .92em; font-weight: 600; color: #2a5298; margin: 18px 0 10px; }"
  , "table { width: 100%; border-collapse: collapse; font-size: .88em; margin-bottom: 8px; }"
  , "table.narrow { max-width: 480px; }"
  , "thead tr { background: #f0f4f8; }"
  , "th { padding: 8px 14px; text-align: left; font-weight: 600; color: #444; }"
  , "td { padding: 7px 14px; border-bottom: 1px solid #f0f2f5; font-family: monospace; }"
  , "td:first-child { font-family: inherit; font-weight: 500; }"
  , "tr:last-child td { border-bottom: none; }"
  , "tfoot td { border-top: 2px solid #ddd; }"
  , ".num { font-family: monospace; }"
  , ".vl-wrap { overflow-x: auto; margin-bottom: 8px; }"
  , ".kv { display: flex; flex-wrap: wrap; gap: 12px; margin-bottom: 16px; }"
  , ".kv > div { background: #f7f9fc; border: 1px solid #e4e9f0; border-radius: 10px;"
  , "            padding: 12px 16px; min-width: 140px; text-align: center;"
  , "            display: flex; flex-direction: column; }"
  , ".kv .k { font-size: .7em; color: #888; text-transform: uppercase; letter-spacing: .05em; margin-bottom: 4px; }"
  , ".kv .v { font-size: 1.2em; font-weight: 700; color: #1e3a5c; }"
  , ".sec-icon { font-size: 1.1em; margin-right: 6px; }"
  , ".sec-desc { font-size: .88em; color: #666; margin-bottom: 16px; }"
  , ".info-grid { display: flex; gap: 12px; flex-wrap: wrap; margin-bottom: 16px; }"
  , ".info-box { background: #f7f9fc; border: 1px solid #e4e9f0; border-radius: 10px;"
  , "            padding: 12px 18px; min-width: 180px; flex: 1; }"
  , ".info-box .lbl { font-size: .72em; color: #888; text-transform: uppercase; letter-spacing: .04em; margin-bottom: 4px; }"
  , ".info-box .ival { font-size: .95em; font-weight: 600; color: #1e3a5c; }"
  , ".mermaid-wrap { background:#f7fafc; border-radius:8px; padding:24px;"
  , "                margin:12px 0; text-align:center; overflow-x:auto; }"
  , ".mermaid-wrap .mermaid { display:inline-block; min-width:320px; min-height:200px;"
  , "                         font-family:'Segoe UI',sans-serif; line-height:1.4; }"
  , ".mermaid-wrap .mermaid svg { max-width:100%; height:auto; min-height:240px; }"
  , ".hist-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));"
  , "             gap: 14px; margin-top: 12px; }"
  , ".hist-card { background: #f7f9fc; border: 1px solid #e4e9f0; border-radius: 8px;"
  , "             padding: 10px; }"
  , ".hist-title { font-weight: 600; color: #1e3a5c; margin-bottom: 6px; font-size: .9em; }"
  , "p { line-height: 1.6; color: #444; font-size: .92em; }"
  , ".interactive-controls { margin-bottom: 16px; padding: 16px 18px;"
  , "                         background: #f7f9fc; border: 1px solid #e4e9f0;"
  , "                         border-radius: 10px; }"
  , ".interactive-controls input[type='range'] { width: 360px; vertical-align: middle;"
  , "                                            margin: 0 10px; accent-color: #1e3a5c; }"
  , ".interactive-controls label { display: block; margin-bottom: 8px; font-size: .9em; }"
  , ".pred-readout { font-size: 1em; }"
  , ".pred-readout strong { color: #1e3a5c; }"
  , ".band-readout { color: #888; font-size: .9em; }"
  , "details { margin: 8px 0; }"
  , "details summary { cursor: pointer; padding: 10px 14px;"
  , "                  background: #f0f4f8; border-radius: 8px;"
  , "                  font-weight: 600; color: #1e3a5c; user-select: none; }"
  , "details summary h2 { display: inline; font-size: 1.05em; border: none;"
  , "                     padding: 0; margin: 0; color: inherit; }"
  , "details[open] summary { background: #dce6f0; }"
  , "details summary::-webkit-details-marker { color: #888; }"
  -- Collapsible は通常の section と同じ白背景・影付きの箱として表示。
  -- summary の h2 は通常の h2 と同じスタイルにし、右に折りたたみ三角を付ける。
  , ".collapsible-wrap > details > summary { list-style: none; cursor: pointer;"
  , "                                        padding: 0; background: transparent;"
  , "                                        margin: 0; }"
  , ".collapsible-wrap > details > summary::-webkit-details-marker { display: none; }"
  , ".collapsible-wrap > details > summary > h2 { display: block;"
  , "  font-size: 1.05em; font-weight: 700; color: #1e3a5c;"
  , "  margin: 0; border-bottom: 2px solid #e4e9f0; padding-bottom: 8px; }"
  , ".collapsible-wrap > details[open] > summary > h2 { margin-bottom: 18px; }"
  , ".collapsible-wrap > details > summary > h2::after { content: '\\25BC';"
  , "  font-size: .7em; margin-left: 10px; color: #888;"
  , "  transition: transform .2s; display: inline-block; }"
  , ".collapsible-wrap > details:not([open]) > summary > h2::after {"
  , "  transform: rotate(-90deg); }"
  , ".collapsible-body { padding: 0; }"
  , ".collapsible-body > section { background: transparent; border: none;"
  , "                              box-shadow: none; padding: 6px 0; margin: 0; }"
  , ".collapsible-body > section > h2 { display: none; }"
  , ".collapsible-body > .table-scroll { margin: 0; }"
  , ".collapsible-body > .info-grid { margin-top: 0; }"
  -- Card (淡い背景の囲み)
  , ".result-card { background: #f7f9fc; border: 1px solid #e4e9f0;"
  , "               border-radius: 10px; padding: 14px 16px; margin: 12px 0; }"
  , ".result-card .card-title { font-weight: 600; color: #1e3a5c;"
  , "                           margin-bottom: 10px; font-size: .98em;"
  , "                           border-bottom: 1px solid #dde6ee; padding-bottom: 6px; }"
  , ".result-card section { background: transparent; border: none;"
  , "                       box-shadow: none; padding: 0; margin: 0; }"
  , ".result-card section > h2 { display: none; }"
  -- Stat row (Card 間のフラットな統計バー)
  , ".stat-row { display: flex; gap: 12px; flex-wrap: wrap;"
  , "            margin: 14px 0; }"
  , ".stat-row .stat-box { background: white; border: 1px solid #d6dde6;"
  , "                      border-radius: 8px; padding: 10px 14px;"
  , "                      min-width: 110px; flex: 1; text-align: center; }"
  , ".stat-row .lbl { font-size: .7em; color: #888; text-transform: uppercase;"
  , "                 letter-spacing: .04em; margin-bottom: 4px; }"
  , ".stat-row .val { font-size: 1.1em; font-weight: 700; color: #1e3a5c;"
  , "                 font-family: monospace; }"
  , ".stats-card, .hist-card-group { margin: 10px 0; }"
  , ".stats-card[open] summary, .hist-card-group[open] summary { background: #d6e4f0; }"
  , ".hist-card { border: 1px solid #e0e6ee; border-radius: 6px;"
  , "             padding: 4px 8px; margin: 6px 0; }"
  , ".hist-card summary { background: transparent; padding: 4px 0; }"
  , ".hist-card summary strong { color: #2c3e50; }"
  , ".table-scroll { overflow-x: auto; }"
  , ".stats-table { font-size: .85em; }"
  , ".stats-table th, .stats-table td { padding: 5px 10px; }"
  , ".interactive-multi { display: grid; grid-template-columns: 280px 1fr;"
  , "                     gap: 20px; align-items: start; }"
  , ".interactive-multi .i-controls { background: #f8f9fa;"
  , "                                  border-radius: 8px; padding: 14px; }"
  , ".interactive-multi .slider-row { margin-bottom: 10px; }"
  , ".interactive-multi .slider-row label { display: block; font-size: .9em; }"
  , ".interactive-multi input[type='range'] { width: 100%; vertical-align: middle; }"
  , ".interactive-multi select { width: 100%; padding: 4px; }"
  , ".interactive-multi .pred-output { margin-top: 14px; padding-top: 12px;"
  , "                                  border-top: 1px solid #ddd; font-size: .95em; }"
  , ".interactive-multi .pred-output strong { color: #2c3e50; }"
  , "@media (max-width: 700px) {"
  , "  .interactive-multi { grid-template-columns: 1fr; }"
  , "}"
  , ".raw-section { margin-bottom: 28px; }"
  , ".appendix-md.collapsible-wrap { background: white; }"
  , ".md-body h3 { font-size: 1em; color: #2c3e50; margin: 12px 0 6px; }"
  , ".md-body h4 { font-size: .95em; color: #34495e; margin: 10px 0 4px; }"
  , ".md-body h5 { font-size: .9em; color: #555; margin: 8px 0 4px; }"
  , ".md-body p { margin: 8px 0; }"
  , ".md-body ul { margin: 6px 0 6px 20px; }"
  , ".md-body code { background: #eef2f7; padding: 1px 5px; border-radius: 3px;"
  , "                font-family: monospace; font-size: .92em; }"
  , ".md-body strong { color: #2c3e50; }"
  , ".md-body a { color: #2980b9; text-decoration: none; }"
  , ".md-body a:hover { text-decoration: underline; }"
  ]

-- ---------------------------------------------------------------------------
-- Interactive RFF MV (多変量 RFF Ridge の対話的予測) -----------------------
-- ---------------------------------------------------------------------------

renderInteractiveRFFMV :: Text -> Text -> InteractiveRFFMV -> Text
renderInteractiveRFFMV sid title r =
  let sliderHtml = T.intercalate "\n"
        [ T.unlines
            [ "<div class=\"slider-row\">"
            , "  <label>" <> col <> ":"
            , "    <input type=\"range\" id=\"i-" <> sid <> "-s" <> T.pack (show i) <> "\""
            , "      min=\"" <> showD4 mn <> "\""
            , "      max=\"" <> showD4 mx <> "\""
            , "      step=\"" <> showD4 ((mx - mn) / 200) <> "\""
            , "      value=\"" <> showD4 mid <> "\""
            , "      oninput=\"window.__updRFFMV_" <> sid <> "()\">"
            , "    <span id=\"i-" <> sid <> "-s" <> T.pack (show i)
              <> "-val\">" <> showD4 mid <> "</span>"
            , "  </label>"
            , "</div>"
            ]
        | (i, (col, mn, mid, mx)) <- zip [0::Int ..] (irfSliders r) ]
      tFull = "<span class=\"sec-icon\">&#127919;</span> "
              <> (if T.null title then "対話的予測" else title)
  in collapsibleSection sid tFull True $
       T.unlines
         [ "<div class=\"interactive-multi\">"
         , "  <div class=\"i-controls\">"
         , "    <div class=\"slider-row\"><em>主軸: " <> irfMainAxis r
            <> " (横軸固定。副軸を以下のスライダで動かすと予測曲線が更新されます)</em></div>"
         , sliderHtml
         , "  </div>"
         , "  <div class=\"i-chart\">"
         , "    <div class=\"vl-wrap\"><div id=\"vl-" <> sid <> "\"></div></div>"
         , "  </div>"
         , "</div>"
         ]

interactiveRFFMVScript :: Text -> InteractiveRFFMV -> Text
interactiveRFFMVScript sid r =
  let mainAxis  = irfMainAxis r
      yCol      = irfYCol r
      xColsAll  = irfXCols r
      mainIdx   = case [ i | (i, c) <- zip [0::Int ..] xColsAll, c == mainAxis ] of
                    (i:_) -> i
                    []    -> 0
      sliderCols = [ c | c <- xColsAll, c /= mainAxis ]
      sliderIdx = [ i | (i, c) <- zip [0::Int ..] xColsAll, c /= mainAxis ]
      arrD xs = "[" <> T.intercalate "," (map showD4 xs) <> "]"
      arrS xs = "[" <> T.intercalate "," (map (\s -> "\"" <> s <> "\"") xs) <> "]"
      omegasArr = arrD (irfOmegasRowMaj r)
      bsArr     = arrD (irfBs r)
      wArr      = arrD (irfWeights r)
      muArr  = case irfStdMu r of { Just xs -> arrD xs; Nothing -> "null" }
      sdArr  = case irfStdSd r of { Just xs -> arrD xs; Nothing -> "null" }
      xObsJson  =
        "[" <> T.intercalate ","
                  [ arrD col | col <- irfXObs r ] <> "]"
      yObsJson  = arrD (irfYObs r)
      groupsJson = arrS (irfGroups r)
      mainGridJson = arrD (irfMainGrid r)
      sliderColsJson = arrS sliderCols
      sliderIdxJson  = "[" <> T.intercalate "," (map (T.pack . show) sliderIdx) <> "]"
  in T.unlines
       [ "(() => {"
       , "  const sid       = \"" <> sid <> "\";"
       , "  const xCols     = " <> arrS xColsAll <> ";"
       , "  const yCol      = \"" <> yCol <> "\";"
       , "  const mainAxis  = \"" <> mainAxis <> "\";"
       , "  const mainIdx   = " <> T.pack (show mainIdx) <> ";"
       , "  const sliderCols = " <> sliderColsJson <> ";"
       , "  const sliderIdx  = " <> sliderIdxJson <> ";"
       , "  const omegas   = " <> omegasArr <> ";"  -- length p*D, row-major
       , "  const bs       = " <> bsArr <> ";"
       , "  const sigmaF   = " <> showD4 (irfSigmaF r) <> ";"
       , "  const Ddim     = " <> T.pack (show (irfDim r)) <> ";"
       , "  const pDim     = " <> T.pack (show (irfP r)) <> ";"
       , "  const weights  = " <> wArr <> ";"
       , "  const xObs     = " <> xObsJson <> ";"  -- p arrays (each n)
       , "  const yObs     = " <> yObsJson <> ";"
       , "  const groups   = " <> groupsJson <> ";"
       , "  const mainGrid = " <> mainGridJson <> ";"
       , "  const coef     = sigmaF * Math.sqrt(2 / Ddim);"
       , "  const stdMu    = " <> muArr <> ";"
       , "  const stdSd    = " <> sdArr <> ";"
       , "  function standardize(xVec) {"
       , "    if (stdMu === null) return xVec;"
       , "    return xVec.map((v, k) => (v - stdMu[k]) / stdSd[k]);"
       , "  }"
       , "  function predictY(xVecRaw) {"
       , "    const xVec = standardize(xVecRaw);"
       , "    let y = 0;"
       , "    for (let j = 0; j < Ddim; j++) {"
       , "      let arg = bs[j];"
       , "      for (let k = 0; k < pDim; k++) {"
       , "        arg += omegas[k * Ddim + j] * xVec[k];"
       , "      }"
       , "      y += weights[j] * coef * Math.cos(arg);"
       , "    }"
       , "    return y;"
       , "  }"
       , "  function readSliders() {"
       , "    const vals = new Array(pDim).fill(0);"
       , "    for (let s = 0; s < sliderCols.length; s++) {"
       , "      const el = document.getElementById('i-' + sid + '-s' + s);"
       , "      const v  = parseFloat(el.value);"
       , "      vals[sliderIdx[s]] = v;"
       , "      const lbl = document.getElementById('i-' + sid + '-s' + s + '-val');"
       , "      if (lbl) lbl.textContent = (Math.round(v*1000)/1000).toString();"
       , "    }"
       , "    return vals;"
       , "  }"
       , "  function buildSpec() {"
       , "    const sliders = readSliders();"
       , "    // 観測点 (固定)"
       , "    const obs = [];"
       , "    const n = yObs.length;"
       , "    for (let i = 0; i < n; i++) {"
       , "      obs.push({ z: xObs[mainIdx][i], y: yObs[i], group: groups[i] });"
       , "    }"
       , "    // 予測曲線 (現在のスライダ値で)"
       , "    const pred = [];"
       , "    for (const z of mainGrid) {"
       , "      const xVec = sliders.slice();"
       , "      xVec[mainIdx] = z;"
       , "      pred.push({ z: z, yhat: predictY(xVec) });"
       , "    }"
       , "    return {"
       , "      $schema: 'https://vega.github.io/schema/vega-lite/v5.json',"
       , "      width: 720, height: 480,"
       , "      layer: ["
       , "        { data: { values: obs },"
       , "          mark: { type: 'point', filled: true, opacity: 0.6 },"
       , "          encoding: {"
       , "            x: { field: 'z', type: 'quantitative', title: mainAxis },"
       , "            y: { field: 'y', type: 'quantitative', title: yCol },"
       , "            color: { field: 'group', type: 'nominal' },"
       , "            tooltip: ["
       , "              { field: 'group' }, { field: 'z' }, { field: 'y' }"
       , "            ]"
       , "          } },"
       , "        { data: { values: pred },"
       , "          mark: { type: 'line', strokeWidth: 3, color: '#333' },"
       , "          encoding: {"
       , "            x: { field: 'z', type: 'quantitative' },"
       , "            y: { field: 'yhat', type: 'quantitative' }"
       , "          } }"
       , "      ]"
       , "    };"
       , "  }"
       , "  function update() {"
       , "    const spec = buildSpec();"
       , "    if (window.vegaEmbed) {"
       , "      window.vegaEmbed('#vl-' + sid, spec, { actions: false });"
       , "    }"
       , "  }"
       , "  window['__updRFFMV_' + sid] = update;"
       , "  setTimeout(update, 0);"
       , "})();"
       ]

-- ---------------------------------------------------------------------------
-- 多出力対話的予測 (1 入力 → q 出力)
-- ---------------------------------------------------------------------------

renderInteractiveMultiOut :: Text -> Text -> InteractiveMultiOut -> Text
renderInteractiveMultiOut sid title imo =
  let (mn, mid, mx) = imoXSlider imo
      tFull = "<span class=\"sec-icon\">&#127919;</span> "
              <> (if T.null title then "対話的予測" else title)
  in collapsibleSection sid tFull True $
       T.unlines
         [ "<div class=\"interactive-multi\">"
         , "  <div class=\"i-controls\">"
         , "    <div class=\"slider-row\"><em>入力 " <> imoXCol imo
            <> " を動かすと " <> imoYCol imo <> "(" <> imoOutAxis imo
            <> ") の予測曲線が更新されます</em></div>"
         , "    <div class=\"slider-row\">"
         , "      <label><b>" <> imoXCol imo <> "</b>:"
         , "        <input type=\"range\" id=\"i-" <> sid <> "-x\""
         , "          min=\"" <> showD4 mn <> "\""
         , "          max=\"" <> showD4 mx <> "\""
         , "          step=\"" <> showD4 ((mx - mn) / 200) <> "\""
         , "          value=\"" <> showD4 mid <> "\""
         , "          oninput=\"window.__updMO_" <> sid <> "()\">"
         , "        <span id=\"i-" <> sid <> "-x-val\">" <> showD4 mid <> "</span>"
         , "      </label>"
         , "    </div>"
         , "  </div>"
         , "  <div class=\"i-chart\">"
         , "    <div class=\"vl-wrap\"><div id=\"vl-" <> sid <> "\"></div></div>"
         , "  </div>"
         , "</div>"
         ]

interactiveMultiOutScript :: Text -> InteractiveMultiOut -> Text
interactiveMultiOutScript sid imo =
  let arrD xs = "[" <> T.intercalate "," (map showD4 xs) <> "]"
      arr2D xss = "[" <> T.intercalate "," (map arrD xss) <> "]"
      gridArr = arrD (imoOutGrid imo)
      xObsArr = arrD (imoXObs imo)
      yObsArr = arr2D (imoYObs imo)
      predBlock = case imoPred imo of
        PredLinearMO ints slps -> T.unlines
          [ "  const model = 'linear-mo';"
          , "  const intercepts = " <> arrD ints <> ";"
          , "  const slopes     = " <> arrD slps <> ";"
          , "  function predict(x) {"
          , "    const out = new Array(intercepts.length);"
          , "    for (let j = 0; j < intercepts.length; j++)"
          , "      out[j] = intercepts[j] + slopes[j] * x;"
          , "    return out;"
          , "  }"
          ]
        PredKernelRBF1 xtr alpha h -> T.unlines
          [ "  const model = 'kernel-rbf-1d';"
          , "  const xTrain = " <> arrD xtr <> ";"
          , "  const alpha  = " <> arr2D alpha <> ";"  -- n × q
          , "  const hBand  = " <> showD4 h <> ";"
          , "  function predict(x) {"
          , "    const n = xTrain.length;"
          , "    const q = alpha[0].length;"
          , "    const out = new Array(q).fill(0);"
          , "    for (let i = 0; i < n; i++) {"
          , "      const u = (x - xTrain[i]) / hBand;"
          , "      const k = Math.exp(-0.5 * u * u) / Math.sqrt(2 * Math.PI);"
          , "      const row = alpha[i];"
          , "      for (let j = 0; j < q; j++) out[j] += k * row[j];"
          , "    }"
          , "    return out;"
          , "  }"
          ]
  in T.unlines
       [ "(() => {"
       , "  const sid = \"" <> sid <> "\";"
       , "  const xCol = \"" <> imoXCol imo <> "\";"
       , "  const yCol = \"" <> imoYCol imo <> "\";"
       , "  const outAxis = \"" <> imoOutAxis imo <> "\";"
       , "  const outGrid = " <> gridArr <> ";"
       , "  const xObs    = " <> xObsArr <> ";"
       , "  const yObs    = " <> yObsArr <> ";"
       , predBlock
       , "  function buildSpec() {"
       , "    const slider = document.getElementById('i-' + sid + '-x');"
       , "    const x = parseFloat(slider.value);"
       , "    const lbl = document.getElementById('i-' + sid + '-x-val');"
       , "    if (lbl) lbl.textContent = (Math.round(x*1000)/1000).toString();"
       , "    const yPred = predict(x);"
       , "    const predData = outGrid.map((z, j) => ({ z: z, y: yPred[j] }));"
       , "    const obsData = [];"
       , "    for (let i = 0; i < xObs.length; i++) {"
       , "      const lab = xCol + '=' + xObs[i].toFixed(2);"
       , "      for (let j = 0; j < outGrid.length; j++) {"
       , "        obsData.push({ z: outGrid[j], y: yObs[i][j], src: lab });"
       , "      }"
       , "    }"
       , "    return {"
       , "      $schema: 'https://vega.github.io/schema/vega-lite/v5.json',"
       , "      width: 760, height: 420,"
       , "      layer: ["
       , "        { data: { values: obsData },"
       , "          mark: { type: 'circle', size: 18, opacity: 0.35 },"
       , "          encoding: {"
       , "            x: { field: 'z', type: 'quantitative', title: outAxis },"
       , "            y: { field: 'y', type: 'quantitative', title: yCol },"
       , "            color: { field: 'src', type: 'nominal', title: 'observed', legend: null }"
       , "          } },"
       , "        { data: { values: predData },"
       , "          mark: { type: 'line', strokeWidth: 3, color: '#d62728' },"
       , "          encoding: {"
       , "            x: { field: 'z', type: 'quantitative' },"
       , "            y: { field: 'y', type: 'quantitative' }"
       , "          } }"
       , "      ]"
       , "    };"
       , "  }"
       , "  function update() {"
       , "    const spec = buildSpec();"
       , "    if (window.vegaEmbed) {"
       , "      window.vegaEmbed('#vl-' + sid, spec, { actions: false });"
       , "    }"
       , "  }"
       , "  window['__updMO_' + sid] = update;"
       , "  setTimeout(update, 0);"
       , "})();"
       ]

-- ---------------------------------------------------------------------------
-- 補間 / regrid レポート (Phase G4)
-- ---------------------------------------------------------------------------

-- | regrid 結果を可視化するためのデータ。
--
-- R1-R7 は必須情報、R8-R10 はオプション (空リスト/Nothing で非表示)。
-- 'DataIO.Preprocess.RegridResult' から構築する想定だが、
-- セクション側ではプリミティブ型のみで受けて柔軟性を保つ。
data InterpReport = InterpReport
  { irTitle         :: !Text
  , irInterpKind    :: !Text                       -- ^ "Linear" | "NaturalSpline" | "PCHIP"
  , irGridKind      :: !Text                       -- ^ "Uniform" | "Adaptive"
  , irN             :: !Int                        -- ^ 出力 grid 点数
  , irZBoundsMode   :: !Text                       -- ^ "intersect" | "union"
  , irZMin          :: !Double
  , irZMax          :: !Double
  , irPerIdObserved :: ![(Text, [(Double, Double)])]
                          -- ^ id ごとの元観測点 [(z, y)]
  , irPerIdInterpY  :: ![(Text, [(Double, Double)])]
                          -- ^ id ごとの (z_grid, y_interp) (R2 ライン用)
  , irGrid          :: ![Double]                   -- ^ 共通 grid (R3 spacing 用)
  , irDensity       :: ![(Double, Double)]         -- ^ (z, peak |dy/dz|) — adaptive 時のみ
  , irPerIdSummary  :: ![(Text, Int, Double, Double, Double, Double, Double)]
                          -- ^ (id, n_obs, zmin, zmax, extrap_below, extrap_above, residual_max)
                          -- R4 用
    -- R8-R10 オプション
  , irExtraEnabled  :: !Bool                       -- ^ True で R8-R10 を出力
  , irPerIdYRange   :: ![(Text, Double, Double, Double, Double)]
                          -- ^ (id, ymin_orig, ymax_orig, ymin_grid, ymax_grid) — R10 用
  } deriving (Show)

-- | 最低限のフィールドだけ埋めた InterpReport (テスト/ダミー用)。
defaultInterpReport :: Text -> InterpReport
defaultInterpReport t = InterpReport
  { irTitle         = t
  , irInterpKind    = "Linear"
  , irGridKind      = "Uniform"
  , irN             = 0
  , irZBoundsMode   = "intersect"
  , irZMin          = 0
  , irZMax          = 1
  , irPerIdObserved = []
  , irPerIdInterpY  = []
  , irGrid          = []
  , irDensity       = []
  , irPerIdSummary  = []
  , irExtraEnabled  = False
  , irPerIdYRange   = []
  }

-- | 補間 / regrid のレポートセクションを構築。
--
-- 出力構造:
--
-- * Card "Regrid summary"
--   - R1: パラメタテーブル (KeyValue)
--   - R4: id ごとの観測点数 / z レンジ / 外挿距離 / 残差表 (Table)
--   - R6: 外挿警告テーブル (該当 id のみ; 0 件なら省略)
--   - R7: id 間 z アラインメント dot plot (Vega)
--   - R2: 補間オーバーレイ small multiples (Vega)
--   - R3: adaptive 時のみ density(z) + grid spacing (Vega)
--   - R5: 補間残差サマリ (R4 と統合済)
--   - (オプション) R8: id ごとの観測点数 bar chart
--   - (オプション) R9: 単調性チェック (PCHIP 以外、簡易判定)
--   - (オプション) R10: y レンジ比較表
secInterpolation :: InterpReport -> ReportSection
secInterpolation ir =
  let -- R1 params
      r1 = secKeyValue "Parameters"
             [ ("Interpolation",  irInterpKind ir)
             , ("Grid",           irGridKind ir)
             , ("Grid points (N)", T.pack (show (irN ir)))
             , ("Z bounds mode",  irZBoundsMode ir)
             , ("Effective zmin", T.pack (showFFloat (Just 4) (irZMin ir) ""))
             , ("Effective zmax", T.pack (showFFloat (Just 4) (irZMax ir) ""))
             , ("Number of ids",  T.pack (show (length (irPerIdSummary ir))))
             ]
      -- R4 per-id summary table
      fmt n x = T.pack (showFFloat (Just n) x "")
      r4Rows = [ [ i, T.pack (show n), fmt 4 zmn, fmt 4 zmx
                 , fmt 4 eb, fmt 4 ea, fmt 4 res ]
               | (i, n, zmn, zmx, eb, ea, res) <- irPerIdSummary ir ]
      r4 = secTable "Per-id summary"
             ["id", "n_observed", "z_min", "z_max"
             , "extrap_below", "extrap_above", "interp_residual_max"]
             r4Rows
      -- R6 extrapolation warning (only ids with extrap > 0)
      r6Rows = [ [ i, fmt 4 eb, fmt 4 ea ]
               | (i, _, _, _, eb, ea, _) <- irPerIdSummary ir
               , eb > 1e-12 || ea > 1e-12 ]
      r6 = if null r6Rows
             then Nothing
             else Just (secTable "Extrapolation warnings"
                          ["id", "extrap_below", "extrap_above"]
                          r6Rows)
      -- R7 id-z alignment dot plot
      r7 = secVega "Z alignment across ids" (idAlignmentSpec ir)
      -- R2 interpolation overlay (small multiples)
      r2 = secVega "Interpolation overlay (per id)" (interpolationOverlaySpec ir)
      -- R3 density profile (adaptive only)
      r3 = if null (irDensity ir)
             then Nothing
             else Just (secVega "Adaptive density profile" (densityProfileSpec ir))
      -- R8 obs count bar (extra)
      r8 = if irExtraEnabled ir
             then Just (secBarChart "Observation count per id"
                         [ (i, fromIntegral n)
                         | (i, n, _, _, _, _, _) <- irPerIdSummary ir ])
             else Nothing
      -- R10 y-range comparison (extra)
      r10Rows = [ [ i, fmt 4 yo0, fmt 4 yo1, fmt 4 yg0, fmt 4 yg1
                  , fmt 4 (yg0 - yo0), fmt 4 (yg1 - yo1) ]
                | (i, yo0, yo1, yg0, yg1) <- irPerIdYRange ir ]
      r10 = if irExtraEnabled ir && not (null r10Rows)
              then Just (secTable
                          "Y range: original vs interpolated"
                          ["id", "y_min_orig", "y_max_orig"
                          , "y_min_grid", "y_max_grid"
                          , "Δ_min", "Δ_max"]
                          r10Rows)
              else Nothing
      -- R9 monotonicity check (extra; skip for PCHIP since guaranteed)
      r9 = if irExtraEnabled ir && irInterpKind ir /= "PCHIP"
             then
               let nonMono =
                     [ i
                     | (i, ys) <- irPerIdInterpY ir
                     , let vs = map snd ys
                     , let asc = and (zipWith (<=) vs (tail vs))
                     , let desc = and (zipWith (>=) vs (tail vs))
                     , not asc && not desc
                       -- かつ 元データが単調なら警告
                     , let obs = Prelude.lookup i (irPerIdObserved ir)
                     , case obs of
                         Just ps ->
                           let os = map snd ps
                           in and (zipWith (<=) os (tail os))
                              || and (zipWith (>=) os (tail os))
                         Nothing -> False
                     ]
               in if null nonMono
                    then Nothing
                    else Just (secMarkdown "Monotonicity warning"
                                ("Non-monotone interpolation curves "
                                 <> "(observed data was monotone): "
                                 <> T.intercalate ", " nonMono))
             else Nothing
      sections = [r1, r4]
              ++ maybe [] (:[]) r6
              ++ [r7, r2]
              ++ maybe [] (:[]) r3
              ++ maybe [] (:[]) r8
              ++ maybe [] (:[]) r9
              ++ maybe [] (:[]) r10
  in secCard (irTitle ir) sections

-- | R2: 補間オーバーレイ — id ごとに facet 化 (small multiples)。
-- 元観測点を dot、補間曲線を line で重ね描き (kind 列で区別)。
interpolationOverlaySpec :: InterpReport -> VegaLite
interpolationOverlaySpec ir =
  let mkObsRows = concat
        [ [ dataRow [ ("id", Str i), ("z", Number z), ("y", Number y)
                    , ("kind", Str "obs") ] []
          | (z, y) <- pts ]
        | (i, pts) <- irPerIdObserved ir ]
      mkLineRows = concat
        [ [ dataRow [ ("id", Str i), ("z", Number z), ("y", Number y)
                    , ("kind", Str "interp") ] []
          | (z, y) <- ys ]
        | (i, ys) <- irPerIdInterpY ir ]
      datValues = dataFromRows [] (concat (mkObsRows ++ mkLineRows))
      enc = encoding
            . position X [PName "z", PmType Quantitative]
            . position Y [PName "y", PmType Quantitative]
            . color [MName "kind", MmType Nominal
                   , MScale [SDomain (DStrings ["obs", "interp"])
                           , SRange (RStrings ["#d62728", "#1f77b4"])]]
            . VL.shape [MName "kind", MmType Nominal]
      facetCfg = facetFlow [FName "id", FmType Nominal, FHeader [HTitle ""]]
      spec = asSpec
        [ mark Point [MOpacity 0.7]
        , (enc [])
        ]
  in toVegaLite
       [ datValues
       , columns 3
       , facetCfg
       , specification spec
       , VL.width 200, VL.height 150
       ]

-- | R3: adaptive density(z) を line で表示し、その下に grid 点を rule (vertical) で重ねる。
densityProfileSpec :: InterpReport -> VegaLite
densityProfileSpec ir =
  let densRows  = [ dataRow [("z", Number z), ("density", Number d)] []
                  | (z, d) <- irDensity ir ]
      gridRows  = [ dataRow [("z", Number z)] [] | z <- irGrid ir ]
      densSpec  = asSpec
        [ dataFromRows [] (concat densRows)
        , mark Line [MStrokeWidth 2, MColor "#2ca02c"]
        , (encoding . position X [PName "z", PmType Quantitative]
                   . position Y [PName "density", PmType Quantitative
                               , PAxis [AxTitle "peak |dy/dz|"]]) []
        ]
      gridSpec  = asSpec
        [ dataFromRows [] (concat gridRows)
        , mark Rule [MStrokeWidth 1, MColor "#ff7f0e", MOpacity 0.4]
        , (encoding . position X [PName "z", PmType Quantitative]) []
        ]
  in toVegaLite
       [ layer [densSpec, gridSpec]
       , VL.width 600, VL.height 200
       ]

-- | R7: id ごとの z 観測点を縦並びの dot plot で表示 (z レンジ揃え目視確認)。
idAlignmentSpec :: InterpReport -> VegaLite
idAlignmentSpec ir =
  let rows = concat
        [ [ dataRow [("id", Str i), ("z", Number z)] [] | (z, _) <- pts ]
        | (i, pts) <- irPerIdObserved ir ]
      enc  = encoding
             . position X [PName "z", PmType Quantitative]
             . position Y [PName "id", PmType Nominal]
  in toVegaLite
       [ dataFromRows [] (concat rows)
       , mark Tick [MOpacity 0.7, MColor "#4c78a8"]
       , (enc [])
       , VL.width 600
       , VL.height 200
       ]
