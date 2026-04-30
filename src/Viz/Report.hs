{-# LANGUAGE OverloadedStrings #-}
module Viz.Report
  ( MCMCReport (..)
  , defaultReport
  , renderReport
  ) where

import Data.Aeson (encode)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (decodeUtf8)
import Graphics.Vega.VegaLite (fromVL)

import Model.HBM        (ModelGraph)
import Model.MCMC       (Chain (..), posteriorMean, posteriorSD, posteriorQuantile)
import Stat.MCMC        (ess)
import Viz.MCMC         (mcmcDiagnostics, autocorrPlot, pairScatter)
import Viz.ModelGraph   (buildMermaid)
import Viz.Core         (defaultConfig)

import qualified Data.Map.Strict as Map

-- ---------------------------------------------------------------------------
-- Report data type
-- ---------------------------------------------------------------------------

data MCMCReport = MCMCReport
  { reportTitle    :: Text
  , reportGraph    :: Maybe ModelGraph
  , reportChain    :: Chain
  , reportParams   :: [Text]
  , reportPairs    :: [(Text, Text)]
  , reportMaxLag   :: Int
  }

defaultReport :: Text -> Chain -> [Text] -> MCMCReport
defaultReport title_ chain params = MCMCReport
  { reportTitle  = title_
  , reportGraph  = Nothing
  , reportChain  = chain
  , reportParams = params
  , reportPairs  = []
  , reportMaxLag = 40
  }

-- ---------------------------------------------------------------------------
-- Top-level renderer
-- ---------------------------------------------------------------------------

renderReport :: FilePath -> MCMCReport -> IO ()
renderReport path rpt =
  TIO.writeFile path (buildHtml rpt)

-- ---------------------------------------------------------------------------
-- HTML builder
-- ---------------------------------------------------------------------------

buildHtml :: MCMCReport -> Text
buildHtml rpt = T.unlines $
  [ "<!DOCTYPE html>"
  , "<html lang=\"ja\">"
  , "<head>"
  , "  <meta charset=\"utf-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "  <title>" <> reportTitle rpt <> "</title>"
  , "  <script src=\"https://cdn.jsdelivr.net/npm/vega@5\"></script>"
  , "  <script src=\"https://cdn.jsdelivr.net/npm/vega-lite@5\"></script>"
  , "  <script src=\"https://cdn.jsdelivr.net/npm/vega-embed@6\"></script>"
  , "  <script src=\"https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js\"></script>"
  , "  <style>"
  , css
  , "  </style>"
  , "</head>"
  , "<body>"
  , nav rpt
  , "<main>"
  ] ++
  maybe [] modelGraphSection (reportGraph rpt) ++
  [ summarySection rpt
  , diagnosticsSection rpt
  , autocorrSection rpt
  ] ++
  pairSection rpt ++
  [ "</main>"
  , "<script>"
  , "mermaid.initialize({ startOnLoad: true, theme: 'default' });"
  , vegaEmbedJs rpt
  , "document.querySelectorAll('.nav-link').forEach(a => {"
  , "  a.addEventListener('click', e => {"
  , "    e.preventDefault();"
  , "    document.querySelector(a.getAttribute('href')).scrollIntoView({ behavior: 'smooth' });"
  , "  });"
  , "});"
  , "</script>"
  , "</body>"
  , "</html>"
  ]

-- ---------------------------------------------------------------------------
-- CSS
-- ---------------------------------------------------------------------------

css :: Text
css = T.unlines
  [ "    * { box-sizing: border-box; margin: 0; padding: 0; }"
  , "    body { font-family: 'Segoe UI', sans-serif; background: #f0f2f5; color: #333; }"
  , "    nav { position: sticky; top: 0; z-index: 100; background: #2c3e50;"
  , "          padding: 10px 24px; display: flex; gap: 20px; align-items: center; }"
  , "    nav h1 { color: #ecf0f1; font-size: 1em; flex: 1; }"
  , "    .nav-link { color: #bdc3c7; text-decoration: none; font-size: .85em; }"
  , "    .nav-link:hover { color: #fff; }"
  , "    main { max-width: 1100px; margin: 0 auto; padding: 30px 20px; }"
  , "    section { background: white; border-radius: 10px; padding: 24px;"
  , "              margin-bottom: 28px; box-shadow: 0 2px 8px rgba(0,0,0,.08); }"
  , "    h2 { font-size: 1.1em; color: #2c3e50; margin-bottom: 16px;"
  , "         border-bottom: 2px solid #e8ecf0; padding-bottom: 8px; }"
  , "    .stat-grid { display: flex; gap: 16px; flex-wrap: wrap; margin-bottom: 20px; }"
  , "    .stat-box { background: #f8f9fa; border-radius: 8px; padding: 14px 20px;"
  , "                min-width: 140px; text-align: center; }"
  , "    .stat-box .label { font-size: .75em; color: #888; text-transform: uppercase; }"
  , "    .stat-box .value { font-size: 1.4em; font-weight: 600; color: #2c3e50; }"
  , "    table { width: 100%; border-collapse: collapse; font-size: .9em; }"
  , "    th { background: #f0f2f5; text-align: right; padding: 8px 14px;"
  , "         font-weight: 600; color: #555; }"
  , "    th:first-child { text-align: left; }"
  , "    td { padding: 7px 14px; border-bottom: 1px solid #f0f2f5; text-align: right; }"
  , "    td:first-child { text-align: left; font-family: monospace; font-weight: 500; }"
  , "    tr:last-child td { border-bottom: none; }"
  , "    .vl-wrap { overflow-x: auto; }"
  , "    .pair-grid { display: flex; flex-wrap: wrap; gap: 16px; }"
  , "    .mermaid { text-align: center; }"
  , "    .legend { margin-top: 12px; font-size: .82em; color: #666; }"
  , "    .legend span { display: inline-block; width: 11px; height: 11px;"
  , "                   border-radius: 2px; margin-right: 4px; vertical-align: middle; }"
  ]

-- ---------------------------------------------------------------------------
-- Nav bar
-- ---------------------------------------------------------------------------

nav :: MCMCReport -> Text
nav rpt = T.unlines $
  [ "<nav>"
  , "  <h1>" <> reportTitle rpt <> "</h1>"
  ] ++
  maybe [] (const ["  <a class=\"nav-link\" href=\"#sec-graph\">Model Graph</a>"]) (reportGraph rpt) ++
  [ "  <a class=\"nav-link\" href=\"#sec-summary\">Summary</a>"
  , "  <a class=\"nav-link\" href=\"#sec-diagnostics\">Diagnostics</a>"
  , "  <a class=\"nav-link\" href=\"#sec-autocorr\">Autocorrelation</a>"
  ] ++
  (if null (reportPairs rpt) then []
   else ["  <a class=\"nav-link\" href=\"#sec-pairs\">Pair Plots</a>"]) ++
  [ "</nav>" ]

-- ---------------------------------------------------------------------------
-- Model graph section
-- ---------------------------------------------------------------------------

modelGraphSection :: ModelGraph -> [Text]
modelGraphSection mg =
  [ "<section id=\"sec-graph\">"
  , "  <h2>Model Graph</h2>"
  , "  <div class=\"mermaid\">"
  , buildMermaid mg
  , "  </div>"
  , "  <div class=\"legend\">"
  , "    <span style=\"background:#4C72B0\"></span>latent &nbsp;&nbsp;"
  , "    <span style=\"background:#DD8844\"></span>observed"
  , "  </div>"
  , "</section>"
  ]

-- ---------------------------------------------------------------------------
-- Summary section
-- ---------------------------------------------------------------------------

summarySection :: MCMCReport -> Text
summarySection rpt =
  let chain  = reportChain rpt
      params = reportParams rpt
      total  = chainTotal chain
      acc    = chainAccepted chain
      rate   = if total == 0 then 0 else fromIntegral acc / fromIntegral total :: Double
      nSamp  = length (chainSamples chain)

      fmtD :: Int -> Double -> Text
      fmtD dec v = T.pack (showF dec v)

      showF :: Int -> Double -> String
      showF 1 v = let s = show (round (v * 10) :: Int)
                      (i, f) = splitAt (length s - 1) s
                  in (if null i then "0" else i) ++ "." ++ f
      showF _ v = let s = show (round v :: Int) in s

      statBox lbl val = T.unlines
        [ "    <div class=\"stat-box\">"
        , "      <div class=\"label\">" <> lbl <> "</div>"
        , "      <div class=\"value\">" <> val <> "</div>"
        , "    </div>"
        ]

      get f p = maybe 0.0 id (f p chain)

      tableRow p =
        let mean_ = get posteriorMean p
            sd_   = get posteriorSD   p
            lo    = get (posteriorQuantile 0.025) p
            hi    = get (posteriorQuantile 0.975) p
            ess_  = ess [ v | ps <- chainSamples chain
                             , Just v <- [Map.lookup p ps] ]
        in T.unlines
          [ "      <tr>"
          , "        <td>" <> p <> "</td>"
          , "        <td>" <> fmt4 mean_ <> "</td>"
          , "        <td>" <> fmt4 sd_   <> "</td>"
          , "        <td>" <> fmt4 lo    <> "</td>"
          , "        <td>" <> fmt4 hi    <> "</td>"
          , "        <td>" <> T.pack (show (round ess_ :: Int)) <> "</td>"
          , "      </tr>"
          ]

  in T.unlines
    [ "<section id=\"sec-summary\">"
    , "  <h2>Posterior Summary</h2>"
    , "  <div class=\"stat-grid\">"
    , statBox "Samples"         (T.pack (show nSamp))
    , statBox "Acceptance"      (fmtD 1 (rate * 100) <> "%")
    , statBox "Accepted"        (T.pack (show acc))
    , statBox "Total Proposals" (T.pack (show total))
    , "  </div>"
    , "  <table>"
    , "    <thead><tr>"
    , "      <th>Parameter</th><th>Mean</th><th>SD</th>"
    , "      <th>2.5%</th><th>97.5%</th><th>ESS</th>"
    , "    </tr></thead>"
    , "    <tbody>"
    , T.concat (map tableRow params)
    , "    </tbody>"
    , "  </table>"
    , "</section>"
    ]

fmt4 :: Double -> Text
fmt4 v = T.pack (showFFloat4 v)

showFFloat4 :: Double -> String
showFFloat4 v
  | isNaN v || isInfinite v = show v
  | otherwise =
      let scaled = round (v * 10000) :: Integer
          (whole, frac) = divMod (abs scaled) 10000
          sign = if v < 0 then "-" else ""
      in sign ++ show whole ++ "." ++ pad4 (fromIntegral frac)
  where
    pad4 :: Int -> String
    pad4 n = let s = show n in replicate (4 - length s) '0' ++ s

-- ---------------------------------------------------------------------------
-- Diagnostics section (trace + posterior hist)
-- ---------------------------------------------------------------------------

diagnosticsSection :: MCMCReport -> Text
diagnosticsSection rpt =
  let cfg   = defaultConfig (reportTitle rpt <> " — Diagnostics")
      spec  = mcmcDiagnostics cfg (reportParams rpt) (reportChain rpt)
      json  = decodeUtf8 . toStrict . encode . fromVL $ spec
  in T.unlines
    [ "<section id=\"sec-diagnostics\">"
    , "  <h2>MCMC Diagnostics (Posterior &amp; Trace)</h2>"
    , "  <div class=\"vl-wrap\">"
    , "    <div id=\"vl-diagnostics\"></div>"
    , "  </div>"
    , "  <script>window.__vlDiag = " <> json <> ";</script>"
    , "</section>"
    ]

-- ---------------------------------------------------------------------------
-- Autocorrelation section
-- ---------------------------------------------------------------------------

autocorrSection :: MCMCReport -> Text
autocorrSection rpt =
  let cfg   = defaultConfig (reportTitle rpt <> " — Autocorrelation")
      spec  = autocorrPlot cfg (reportMaxLag rpt) (reportParams rpt) (reportChain rpt)
      json  = decodeUtf8 . toStrict . encode . fromVL $ spec
  in T.unlines
    [ "<section id=\"sec-autocorr\">"
    , "  <h2>Autocorrelation</h2>"
    , "  <div class=\"vl-wrap\">"
    , "    <div id=\"vl-autocorr\"></div>"
    , "  </div>"
    , "  <script>window.__vlAcf = " <> json <> ";</script>"
    , "</section>"
    ]

-- ---------------------------------------------------------------------------
-- Pair scatter section
-- ---------------------------------------------------------------------------

pairSection :: MCMCReport -> [Text]
pairSection rpt
  | null (reportPairs rpt) = []
  | otherwise =
      [ "<section id=\"sec-pairs\">"
      , "  <h2>Pair Scatter Plots</h2>"
      , "  <div class=\"pair-grid\">"
      ] ++
      zipWith mkPairDiv [0 :: Int ..] (reportPairs rpt) ++
      [ "  </div>"
      , "</section>"
      ]
  where
    mkPairDiv idx (xn, yn) =
      let cfg  = defaultConfig (xn <> " vs " <> yn)
          spec = pairScatter cfg xn yn (reportChain rpt)
          json = decodeUtf8 . toStrict . encode . fromVL $ spec
          divId = "vl-pair-" <> T.pack (show idx)
      in T.unlines
          [ "    <div id=\"" <> divId <> "\"></div>"
          , "    <script>window.__vlPair" <> T.pack (show idx) <> " = " <> json <> ";</script>"
          ]

-- ---------------------------------------------------------------------------
-- vegaEmbed JS (all plots in one script block)
-- ---------------------------------------------------------------------------

vegaEmbedJs :: MCMCReport -> Text
vegaEmbedJs rpt = T.unlines $
  [ "vegaEmbed('#vl-diagnostics', window.__vlDiag, {renderer:'canvas',actions:false}).catch(console.error);"
  , "vegaEmbed('#vl-autocorr',    window.__vlAcf,  {renderer:'canvas',actions:false}).catch(console.error);"
  ] ++
  zipWith mkEmbedCall [0 :: Int ..] (reportPairs rpt)
  where
    mkEmbedCall idx _ =
      let divId = "#vl-pair-" <> T.pack (show idx)
          varNm = "window.__vlPair" <> T.pack (show idx)
      in "vegaEmbed('" <> divId <> "', " <> varNm <> ", {renderer:'canvas',actions:false}).catch(console.error);"
