{-# LANGUAGE OverloadedStrings #-}
-- | HTML report for Taguchi-method analysis.
--
-- Bundles the results of 'Hanalyze.Design.Taguchi.analyzeSN' / @optimalLevels@ /
-- @predictSN@ into a single self-contained HTML file:
--
--   * Summary: array name, SN type, run count, predicted SN.
--   * Per-run SN bar chart.
--   * Per-factor main-effects bars (one bar per level).
--   * Best-level table.
module Hanalyze.Viz.Taguchi
  ( TaguchiReport (..)
  , renderTaguchiReport
  ) where

import Data.Aeson (encode)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (decodeUtf8)
import Graphics.Vega.VegaLite
import Text.Printf (printf)

import qualified Hanalyze.Design.Orthogonal as OA
import qualified Hanalyze.Design.Taguchi    as TG
import           Hanalyze.Viz.Assets        (vegaJS, vegaLiteJS, vegaEmbedJS)

-- ---------------------------------------------------------------------------
-- Report data type
-- ---------------------------------------------------------------------------

-- | Inputs needed to render a Taguchi-method HTML report.
data TaguchiReport = TaguchiReport
  { trTitle     :: Text                                  -- ^ Report heading.
  , trArrayName :: Text                                  -- ^ Orthogonal-array
                                                         --   name (e.g. @\"L9(3^4)\"@).
  , trSNType    :: TG.SNType                             -- ^ SN-ratio type.
  , trPerRunSN  :: [Double]                              -- ^ Per-run SN ratios.
  , trEffects   :: [TG.FactorEffect]                     -- ^ Per-factor effects.
  , trOptimal   :: [(Text, OA.LevelValue, Double)]       -- ^ Best level per factor.
  , trPredicted :: Double                                -- ^ Predicted SN ratio.
  }

-- ---------------------------------------------------------------------------
-- Top-level renderer
-- ---------------------------------------------------------------------------

-- | Write the rendered HTML report to the given path.
renderTaguchiReport :: FilePath -> TaguchiReport -> IO ()
renderTaguchiReport path tr = TIO.writeFile path (buildHtml tr)

-- ---------------------------------------------------------------------------
-- HTML
-- ---------------------------------------------------------------------------

buildHtml :: TaguchiReport -> Text
buildHtml tr = T.unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"ja\">"
  , "<head>"
  , "  <meta charset=\"utf-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "  <title>" <> trTitle tr <> "</title>"
  , "  <script>" <> vegaJS      <> "</script>"
  , "  <script>" <> vegaLiteJS  <> "</script>"
  , "  <script>" <> vegaEmbedJS <> "</script>"
  , "  <style>" <> css <> "</style>"
  , "</head>"
  , "<body>"
  , "<header><h1>" <> trTitle tr <> "</h1></header>"
  , "<main>"
  , summarySection tr
  , perRunSection tr
  , factorEffectsSection tr
  , optimumSection tr
  , "</main>"
  , "<script>" <> embedScript tr <> "</script>"
  , "</body>"
  , "</html>"
  ]

-- ---------------------------------------------------------------------------
-- Sections
-- ---------------------------------------------------------------------------

summarySection :: TaguchiReport -> Text
summarySection tr = T.unlines
  [ "<section>"
  , "  <h2>Summary</h2>"
  , "  <div class=\"stat-grid\">"
  , statBox "Array"        (trArrayName tr)
  , statBox "SN type"      (TG.snTypeName (trSNType tr))
  , statBox "Inner runs"   (T.pack (show (length (trPerRunSN tr))))
  , statBox "Predicted SN" (T.pack (printf "%.3f dB" (trPredicted tr)))
  , "  </div>"
  , "</section>"
  ]
  where
    statBox lbl val = T.unlines
      [ "  <div class=\"stat-box\">"
      , "    <div class=\"label\">" <> lbl <> "</div>"
      , "    <div class=\"value\">" <> val <> "</div>"
      , "  </div>"
      ]

perRunSection :: TaguchiReport -> Text
perRunSection _ = T.unlines
  [ "<section>"
  , "  <h2>SN ratio per run</h2>"
  , "  <div class=\"vl-wrap\"><div id=\"vl-perrun\"></div></div>"
  , "</section>"
  ]

factorEffectsSection :: TaguchiReport -> Text
factorEffectsSection tr = T.unlines
  [ "<section>"
  , "  <h2>Factor effects (mean SN per level)</h2>"
  , "  <div class=\"effects-grid\">"
  , T.intercalate "\n"
      [ "    <div class=\"effect-card\">"
        <> "<h3>" <> TG.feFactor fe <> "</h3>"
        <> "<div id=\"vl-factor-" <> T.pack (show i) <> "\"></div>"
        <> "</div>"
      | (i, fe) <- zip [0::Int ..] (trEffects tr) ]
  , "  </div>"
  , "</section>"
  ]

optimumSection :: TaguchiReport -> Text
optimumSection tr = T.unlines
  [ "<section>"
  , "  <h2>Optimal levels (max mean SN)</h2>"
  , "  <table>"
  , "    <thead><tr><th>Factor</th><th>Best level</th><th>Mean SN (dB)</th></tr></thead>"
  , "    <tbody>"
  , T.intercalate "\n"
      [ "      <tr><td>" <> f
        <> "</td><td>" <> levelToText lvl
        <> "</td><td>" <> T.pack (printf "%.3f" eta)
        <> "</td></tr>"
      | (f, lvl, eta) <- trOptimal tr ]
  , "    </tbody>"
  , "  </table>"
  , "  <p class=\"note\">Predicted SN at this combination "
    <> "(additive main-effects model): "
    <> "<strong>" <> T.pack (printf "%.3f dB" (trPredicted tr))
    <> "</strong></p>"
  , "</section>"
  ]
  where
    levelToText (OA.LText t) = t
    levelToText (OA.LNumeric d)
      | d == fromIntegral (round d :: Integer) = T.pack (show (round d :: Integer))
      | otherwise                              = T.pack (printf "%g" d)

-- ---------------------------------------------------------------------------
-- Vega-Lite specs (embedded as JS)
-- ---------------------------------------------------------------------------

embedScript :: TaguchiReport -> Text
embedScript tr =
  let perRunJSON = vlJson (perRunSpec tr)
      effectsJS  = T.intercalate "\n"
        [ "vegaEmbed('#vl-factor-" <> T.pack (show i) <> "', "
          <> vlJson (factorSpec fe) <> ", {actions:false});"
        | (i, fe) <- zip [0::Int ..] (trEffects tr) ]
  in T.unlines
       [ "vegaEmbed('#vl-perrun', " <> perRunJSON <> ", {actions:false});"
       , effectsJS
       ]

vlJson :: VegaLite -> Text
vlJson = decodeUtf8 . toStrict . encode . fromVL

-- | Per-run SN-ratio bar chart.
perRunSpec :: TaguchiReport -> VegaLite
perRunSpec tr =
  let n = length (trPerRunSN tr)
      runs = [ T.pack (show (i :: Int)) | i <- [1 .. n] ]
  in toVegaLite
       [ dataFromColumns []
           . dataColumn "Run" (Strings runs)
           . dataColumn "SN"  (Numbers (trPerRunSN tr))
           $ []
       , mark Bar [MColor "#4C72B0", MOpacity 0.85]
       , encoding
           . position X [PName "Run", PmType Ordinal,
                         PAxis [AxTitle "Inner Run"], PSort []]
           . position Y [PName "SN",  PmType Quantitative,
                         PAxis [AxTitle "SN ratio (dB)"]]
           $ []
       , width 600
       , height 220
       ]

-- | Bar chart of per-level SN ratio for a single factor.
factorSpec :: TG.FactorEffect -> VegaLite
factorSpec fe =
  let lvls = map levelToShort (TG.feLevels fe)
      sns  = TG.feSNByLevel fe
  in toVegaLite
       [ dataFromColumns []
           . dataColumn "level" (Strings lvls)
           . dataColumn "SN"    (Numbers sns)
           $ []
       , mark Bar [MColor "#DD7755", MOpacity 0.85]
       , encoding
           . position X [PName "level", PmType Nominal,
                         PAxis [AxTitle "Level", AxLabelAngle 0],
                         PSort []]
           . position Y [PName "SN", PmType Quantitative,
                         PAxis [AxTitle "Mean SN (dB)"]]
           $ []
       , width 240
       , height 180
       ]
  where
    levelToShort (OA.LText t) = t
    levelToShort (OA.LNumeric d)
      | d == fromIntegral (round d :: Integer) = T.pack (show (round d :: Integer))
      | otherwise                              = T.pack (printf "%g" d)

-- ---------------------------------------------------------------------------
-- CSS
-- ---------------------------------------------------------------------------

css :: Text
css = T.unlines
  [ "* { box-sizing: border-box; margin: 0; padding: 0; }"
  , "body { font-family: 'Segoe UI', sans-serif; background: #f0f2f5; color: #333; }"
  , "header { background: #2c3e50; color: #ecf0f1; padding: 18px 30px; }"
  , "header h1 { font-size: 1.2em; font-weight: 600; }"
  , "main { max-width: 1100px; margin: 0 auto; padding: 30px 20px; }"
  , "section { background: white; border-radius: 10px; padding: 24px;"
  , "          margin-bottom: 28px; box-shadow: 0 2px 8px rgba(0,0,0,.08); }"
  , "h2 { font-size: 1.1em; color: #2c3e50; margin-bottom: 16px;"
  , "     border-bottom: 2px solid #e8ecf0; padding-bottom: 8px; }"
  , "h3 { font-size: .95em; color: #555; margin-bottom: 8px; }"
  , ".stat-grid { display: flex; gap: 16px; flex-wrap: wrap; }"
  , ".stat-box { background: #f8f9fa; border-radius: 8px; padding: 14px 20px;"
  , "            min-width: 160px; text-align: center; }"
  , ".stat-box .label { font-size: .75em; color: #888; text-transform: uppercase; }"
  , ".stat-box .value { font-size: 1.25em; font-weight: 600; color: #2c3e50; margin-top: 4px; }"
  , ".effects-grid { display: flex; flex-wrap: wrap; gap: 16px; }"
  , ".effect-card { flex: 1 1 260px; min-width: 260px; }"
  , "table { width: 100%; border-collapse: collapse; font-size: .9em; }"
  , "th { background: #f0f2f5; text-align: right; padding: 8px 14px;"
  , "     font-weight: 600; color: #555; }"
  , "th:first-child { text-align: left; }"
  , "td { padding: 7px 14px; border-bottom: 1px solid #f0f2f5; text-align: right; }"
  , "td:first-child { text-align: left; font-family: monospace; }"
  , ".vl-wrap { overflow-x: auto; }"
  , ".note { margin-top: 14px; font-size: .9em; color: #666; }"
  ]
