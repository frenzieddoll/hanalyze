{-# LANGUAGE OverloadedStrings #-}
-- | MCMC diagnostic plots (built on Vega-Lite).
--
-- Provides single-chain and multi-chain variants. Posterior densities
-- are drawn with kernel density estimation (KDE).
module Hanalyze.Viz.MCMC
  ( -- * 単一チェーン
    tracePlot,       tracePlotFile
  , tracePlotHDI,    tracePlotHDIFile
  , posteriorPlot,   posteriorPlotFile
  , autocorrPlot,    autocorrPlotFile
  , pairScatter,     pairScatterFile
  , mcmcDiagnostics, mcmcDiagnosticsFile
    -- * Multi-chain panels (PyMC style)
  , multiTracePlot,        multiTracePlotFile
  , mcmcDiagnosticsMulti,  mcmcDiagnosticsMultiFile
    -- * Forest plot (cross-parameter posterior comparison)
  , forestPlot, forestPlotFile
    -- * Energy plot (NUTS BFMI diagnostic)
  , energyPlot, energyPlotFile
    -- * Rank plot (multi-chain convergence diagnostic)
  , rankPlot, rankPlotFile
    -- * Posterior predictive check (PyMC @pp_check@ analogue)
  , ppcPlot, ppcPlotFile
    -- * Divergence overlay (visualize NUTS divergent transitions)
  , pairScatterDiv, pairScatterDivFile
    -- * Posterior summary table (@az.summary@ analogue)
  , SummaryRow (..)
  , posteriorSummary
  , posteriorSummaryHtml
  , posteriorSummaryFile
  , printPosteriorSummary
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Vega.VegaLite

import Hanalyze.MCMC.Core  (Chain (..), chainVals)
import Hanalyze.Stat.MCMC    (autocorr, hdi, kde, bfmi)
import Hanalyze.Stat.Summary (SummaryRow (..), posteriorSummary)
import Data.List   (sortBy)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import qualified Data.Text.IO as TIO
import Hanalyze.Viz.Core   (PlotConfig (..), OutputFormat, writeSpec)

-- ---------------------------------------------------------------------------
-- Trace plot (単一チェーン)
-- ---------------------------------------------------------------------------

-- | Trace plot for one or more parameters of a single chain. Each
-- parameter gets its own vertical panel.
tracePlot :: PlotConfig -> [Text] -> Chain -> VegaLite
tracePlot cfg names chain = toVegaLite
  [ title (plotTitle cfg) []
  , vConcat (map tracePanel names)
  ]
  where
    n = length (chainSamples chain)
    tracePanel pname =
      let vals = chainVals pname chain
      in asSpec
          [ dataFromColumns []
              . dataColumn "iter"  (Numbers (map fromIntegral [1 .. n]))
              . dataColumn "value" (Numbers vals)
              $ []
          , mark Line [MColor "#4C72B0", MStrokeWidth 1.0, MOpacity 0.7]
          , encoding
              . position X [ PName "iter",  PmType Quantitative
                           , PAxis [AxTitle "Iteration"] ]
              . position Y [ PName "value", PmType Quantitative
                           , PAxis [AxTitle pname] ]
              $ []
          , width  (plotWidth cfg)
          , height 90
          ]

tracePlotFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> Chain -> IO ()
tracePlotFile fmt path cfg names chain =
  writeSpec fmt path (tracePlot cfg names chain)

-- | Trace plot with the HDI band overlaid (e.g. @level = 0.94@).
-- 上下の HDI 境界を赤い水平ルールで描画し、内側を半透明赤で塗りつぶす。
-- バーンイン後サンプルから HDI を計算し、視覚的に「事後分布の質量がどこに
-- 集中しているか」をトレースと一緒に確認できる。
tracePlotHDI :: PlotConfig -> Double -> [Text] -> Chain -> VegaLite
tracePlotHDI cfg level names chain = toVegaLite
  [ title (plotTitle cfg) []
  , vConcat (map tracePanel names)
  ]
  where
    n = length (chainSamples chain)
    tracePanel pname =
      let vals     = chainVals pname chain
          (lo, hi) = hdi level vals
      in asSpec
          [ layer
              [ -- HDI 帯 (rect)
                asSpec
                  [ dataFromColumns []
                      . dataColumn "lo" (Numbers [lo])
                      . dataColumn "hi" (Numbers [hi])
                      $ []
                  , mark Rect [MColor "#DD4444", MOpacity 0.12]
                  , encoding
                      . position Y  [PName "lo", PmType Quantitative]
                      . position Y2 [PName "hi"]
                      $ []
                  ]
              , -- HDI 上限 / 下限ライン
                asSpec
                  [ dataFromColumns []
                      . dataColumn "y" (Numbers [lo, hi])
                      $ []
                  , mark Rule [MColor "#DD4444", MStrokeWidth 1.5,
                               MStrokeDash [3, 3]]
                  , encoding
                      . position Y [PName "y", PmType Quantitative]
                      $ []
                  ]
              , -- トレース本体
                asSpec
                  [ dataFromColumns []
                      . dataColumn "iter"  (Numbers (map fromIntegral [1 .. n]))
                      . dataColumn "value" (Numbers vals)
                      $ []
                  , mark Line [MColor "#4C72B0", MStrokeWidth 1.0, MOpacity 0.7]
                  , encoding
                      . position X [ PName "iter",  PmType Quantitative
                                   , PAxis [AxTitle "Iteration"] ]
                      . position Y [ PName "value", PmType Quantitative
                                   , PAxis [AxTitle pname] ]
                      $ []
                  ]
              ]
          , width  (plotWidth cfg)
          , height 90
          ]

tracePlotHDIFile :: OutputFormat -> FilePath -> PlotConfig
                 -> Double -> [Text] -> Chain -> IO ()
tracePlotHDIFile fmt path cfg level names chain =
  writeSpec fmt path (tracePlotHDI cfg level names chain)

-- ---------------------------------------------------------------------------
-- Multi-chain trace plot
-- ---------------------------------------------------------------------------

-- | Multi-chain trace plot. Each chain is overlaid with its own color.
multiTracePlot :: PlotConfig -> [Text] -> [Chain] -> VegaLite
multiTracePlot cfg names chains = toVegaLite
  [ title (plotTitle cfg) []
  , vConcat (map (mkMultiTracePanel' (plotWidth cfg) 90) names)
  ]
  where
    mkMultiTracePanel' w h pname = mkMultiTracePanel pname w h chains

multiTracePlotFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> [Chain] -> IO ()
multiTracePlotFile fmt path cfg names chains =
  writeSpec fmt path (multiTracePlot cfg names chains)

-- ---------------------------------------------------------------------------
-- Posterior KDE plot (単一チェーン)
-- ---------------------------------------------------------------------------

-- | Posterior density plot per parameter (KDE-based).
posteriorPlot :: PlotConfig -> [Text] -> Chain -> VegaLite
posteriorPlot cfg names chain = toVegaLite
  [ title (plotTitle cfg) []
  , vConcat (map (\n -> mkKdePanel n (plotWidth cfg) 110 chain) names)
  ]

posteriorPlotFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> Chain -> IO ()
posteriorPlotFile fmt path cfg names chain =
  writeSpec fmt path (posteriorPlot cfg names chain)

-- ---------------------------------------------------------------------------
-- Autocorrelation plot
-- ---------------------------------------------------------------------------

-- | Per-parameter autocorrelation plot up to a given maximum lag.
autocorrPlot :: PlotConfig -> Int -> [Text] -> Chain -> VegaLite
autocorrPlot cfg maxLag names chain = toVegaLite
  [ title (plotTitle cfg) []
  , vConcat (map acfPanel names)
  ]
  where
    acfPanel pname =
      let acData         = autocorr maxLag (chainVals pname chain)
          (lags, acVals) = unzip acData
      in asSpec
          [ dataFromColumns []
              . dataColumn "lag" (Numbers (map fromIntegral lags))
              . dataColumn "acf" (Numbers acVals)
              $ []
          , mark Bar [MColor "#4C72B0", MOpacity 0.8]
          , encoding
              . position X [ PName "lag", PmType Quantitative
                           , PAxis [AxTitle "Lag"] ]
              . position Y [ PName "acf", PmType Quantitative
                           , PScale [SDomain (DNumbers [-1, 1])]
                           , PAxis [AxTitle pname] ]
              $ []
          , width  (plotWidth cfg)
          , height 80
          ]

autocorrPlotFile :: OutputFormat -> FilePath -> PlotConfig -> Int -> [Text] -> Chain -> IO ()
autocorrPlotFile fmt path cfg maxLag names chain =
  writeSpec fmt path (autocorrPlot cfg maxLag names chain)

-- ---------------------------------------------------------------------------
-- Pair scatter
-- ---------------------------------------------------------------------------

-- | Bivariate posterior scatter for two parameters of a chain.
pairScatter :: PlotConfig -> Text -> Text -> Chain -> VegaLite
pairScatter cfg xName yName chain = toVegaLite
  [ title (plotTitle cfg) []
  , dataFromColumns []
      . dataColumn xName (Numbers (chainVals xName chain))
      . dataColumn yName (Numbers (chainVals yName chain))
      $ []
  , mark Point [MOpacity 0.25, MSize 15, MColor "#4C72B0"]
  , encoding
      . position X [PName xName, PmType Quantitative]
      . position Y [PName yName, PmType Quantitative]
      $ []
  , width  (plotWidth  cfg)
  , height (plotHeight cfg)
  ]

pairScatterFile :: OutputFormat -> FilePath -> PlotConfig -> Text -> Text -> Chain -> IO ()
pairScatterFile fmt path cfg xName yName chain =
  writeSpec fmt path (pairScatter cfg xName yName chain)

-- ---------------------------------------------------------------------------
-- Combined PyMC-style: [KDE | trace]  (単一チェーン)
-- ---------------------------------------------------------------------------

-- | PyMC-style combined diagnostics (KDE + trace) for one chain.
mcmcDiagnostics :: PlotConfig -> [Text] -> Chain -> VegaLite
mcmcDiagnostics cfg names chain = toVegaLite
  [ title (plotTitle cfg) []
  , vConcat (map rowFor names)
  ]
  where
    n = length (chainSamples chain)
    rowFor pname = asSpec
      [ hConcat [ mkKdePanel   pname 220 80 chain
                , mkTracePanel pname 420 80 n chain ] ]

mcmcDiagnosticsFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> Chain -> IO ()
mcmcDiagnosticsFile fmt path cfg names chain =
  writeSpec fmt path (mcmcDiagnostics cfg names chain)

-- ---------------------------------------------------------------------------
-- Combined PyMC-style: [KDE | multi-trace]  (多チェーン)
-- ---------------------------------------------------------------------------

-- | PyMC-style combined diagnostics for multiple chains.
-- 左: 全チェーン合算の KDE。右: チェーン別色分けトレース。
mcmcDiagnosticsMulti :: PlotConfig -> [Text] -> [Chain] -> VegaLite
mcmcDiagnosticsMulti cfg names chains = toVegaLite
  [ title (plotTitle cfg) []
  , vConcat (map rowFor names)
  ]
  where
    combined pname = concatMap (chainVals pname) chains
    rowFor pname = asSpec
      [ hConcat
          [ mkKdePanelFrom pname 220 80 (combined pname)
          , mkMultiTracePanel pname 420 80 chains
          ]
      ]

mcmcDiagnosticsMultiFile :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> [Chain] -> IO ()
mcmcDiagnosticsMultiFile fmt path cfg names chains =
  writeSpec fmt path (mcmcDiagnosticsMulti cfg names chains)

-- ---------------------------------------------------------------------------
-- 内部: KDE パネル
-- ---------------------------------------------------------------------------

-- | KDE density plot with a 94 % HDI rule overlay.
mkKdePanel :: Text -> Double -> Double -> Chain -> VLSpec
mkKdePanel pname w h chain =
  mkKdePanelFrom pname w h (chainVals pname chain)

mkKdePanelFrom :: Text -> Double -> Double -> [Double] -> VLSpec
mkKdePanelFrom pname w h vals =
  let kdeData      = kde 200 vals
      (xs, ys)     = unzip kdeData
      (lo, hi)     = hdi 0.94 vals
  in asSpec
      [ layer
          [ asSpec  -- KDE filled area
              [ dataFromColumns []
                  . dataColumn "x" (Numbers xs)
                  . dataColumn "y" (Numbers ys)
                  $ []
              , mark Area [MColor "#4C72B0", MOpacity 0.3]
              , encoding
                  . position X [ PName "x", PmType Quantitative
                               , PAxis [AxTitle pname] ]
                  . position Y [ PName "y", PmType Quantitative
                               , PAxis [AxTitle "Density", AxGrid False] ]
                  $ []
              ]
          , asSpec  -- KDE line
              [ dataFromColumns []
                  . dataColumn "x" (Numbers xs)
                  . dataColumn "y" (Numbers ys)
                  $ []
              , mark Line [MColor "#4C72B0", MStrokeWidth 2.0]
              , encoding
                  . position X [PName "x", PmType Quantitative]
                  . position Y [PName "y", PmType Quantitative]
                  $ []
              ]
          , asSpec  -- 94% HDI span (rule at bottom)
              [ dataFromColumns []
                  . dataColumn "lo" (Numbers [lo])
                  . dataColumn "hi" (Numbers [hi])
                  $ []
              , mark Rule [MColor "#DD4444", MStrokeWidth 3.5]
              , encoding
                  . position X  [PName "lo", PmType Quantitative]
                  . position X2 [PName "hi"]
                  $ []
              ]
          ]
      , width w, height h
      ]

-- ---------------------------------------------------------------------------
-- 内部: トレースパネル (単一チェーン)
-- ---------------------------------------------------------------------------

mkTracePanel :: Text -> Double -> Double -> Int -> Chain -> VLSpec
mkTracePanel pname w h n chain =
  let vals = chainVals pname chain
  in asSpec
      [ dataFromColumns []
          . dataColumn "iter"  (Numbers (map fromIntegral [1 .. n]))
          . dataColumn "value" (Numbers vals)
          $ []
      , mark Line [MColor "#4C72B0", MStrokeWidth 1.0, MOpacity 0.7]
      , encoding
          . position X [ PName "iter",  PmType Quantitative
                       , PAxis [AxTitle "Iteration"] ]
          . position Y [ PName "value", PmType Quantitative
                       , PAxis [AxTitle ""] ]
          $ []
      , width w, height h
      ]

-- ---------------------------------------------------------------------------
-- 内部: 多チェーントレースパネル
-- ---------------------------------------------------------------------------

mkMultiTracePanel :: Text -> Double -> Double -> [Chain] -> VLSpec
mkMultiTracePanel pname w h chains =
  let (iters, values, chainIds) = unzip3
        [ (fromIntegral i :: Double, v, T.pack (show c))
        | (c, ch) <- zip [1 :: Int ..] chains
        , (i, v)  <- zip [1 :: Int ..] (chainVals pname ch)
        ]
  in asSpec
      [ dataFromColumns []
          . dataColumn "iter"  (Numbers  iters)
          . dataColumn "value" (Numbers  values)
          . dataColumn "chain" (Strings  chainIds)
          $ []
      , mark Line [MStrokeWidth 1.0, MOpacity 0.7]
      , encoding
          . position X [ PName "iter",  PmType Quantitative
                       , PAxis [AxTitle "Iteration"] ]
          . position Y [ PName "value", PmType Quantitative
                       , PAxis [AxTitle ""] ]
          . color [ MName "chain", MmType Nominal
                  , MScale [SScheme "tableau10" []]
                  , MLegend [LTitle "Chain"] ]
          $ []
      , width w, height h
      ]

-- ---------------------------------------------------------------------------
-- Forest plot (パラメータ事後を 1 つの図に並べて比較)
-- ---------------------------------------------------------------------------

-- | Forest plot: per-parameter posterior mean with a 95 % credible
-- interval, stacked horizontally.
--
-- ArviZ の @plot_forest@ 相当。複数モデル/複数チェーンの比較や、
-- 階層モデルでグループ別パラメータを並べて見るのに便利。
--
-- 単一チェーンの場合は @[chain]@ に 1 要素入れて呼ぶ。
forestPlot
  :: PlotConfig
  -> [Text]      -- ^ 表示するパラメータ名 (上から下に並ぶ)
  -> [Chain]     -- ^ 1 つ以上のチェーン (複数あれば色分け)
  -> VegaLite
forestPlot cfg params chains = toVegaLite
  [ title (plotTitle cfg) []
  , dataFromColumns []
      . dataColumn "param" (Strings params')
      . dataColumn "chain" (Strings chainIds)
      . dataColumn "mean"  (Numbers means)
      . dataColumn "lo"    (Numbers loQs)
      . dataColumn "hi"    (Numbers hiQs)
      $ []
  , layer
      [ -- 信用区間の横線
        asSpec
          [ mark Rule [MStrokeWidth 2, MOpacity 0.7]
          , encoding
              . position Y [ PName "param", PmType Nominal
                           , PAxis [AxTitle "Parameter", AxLabelFontSize 11] ]
              . position X  [ PName "lo", PmType Quantitative
                            , PAxis [AxTitle "Posterior 95% CI"] ]
              . position X2 [ PName "hi" ]
              . color [ MName "chain", MmType Nominal
                      , MScale [SScheme "tableau10" []]
                      , MLegend [LTitle "Chain"] ]
              $ []
          ]
        -- 事後平均ドット
      , asSpec
          [ mark Circle [MSize 80, MOpacity 0.95]
          , encoding
              . position Y [ PName "param", PmType Nominal ]
              . position X [ PName "mean", PmType Quantitative ]
              . color [ MName "chain", MmType Nominal
                      , MScale [SScheme "tableau10" []] ]
              $ []
          ]
      ]
  , width (plotWidth cfg)
  , height (max 200 (fromIntegral (length params * 30) :: Double))
  ]
  where
    cs = zip [1 :: Int ..] chains
    -- 各 (param, chain) の組について 1 行
    rows =
      [ (p, T.pack (show ci), m, l, h)
      | (ci, ch) <- cs
      , p        <- params
      , let xs = chainVals p ch
      , not (null xs)
      , let n   = length xs
            sxs = sortAsc xs
            mu  = sum xs / fromIntegral n
            qAt q = sxs !! min (n - 1) (max 0 (floor (q * fromIntegral n) :: Int))
            (l, h) = (qAt 0.025, qAt 0.975)
            m  = mu
      ]
    params'   = [p | (p,_,_,_,_) <- rows]
    chainIds  = [c | (_,c,_,_,_) <- rows]
    means     = [m | (_,_,m,_,_) <- rows]
    loQs      = [l | (_,_,_,l,_) <- rows]
    hiQs      = [h | (_,_,_,_,h) <- rows]

    sortAsc :: [Double] -> [Double]
    sortAsc = qs
      where
        qs []     = []
        qs (p:xs) = qs [x | x <- xs, x <= p] ++ [p] ++ qs [x | x <- xs, x > p]

forestPlotFile
  :: OutputFormat -> FilePath -> PlotConfig -> [Text] -> [Chain] -> IO ()
forestPlotFile fmt path cfg params chains =
  writeSpec fmt path (forestPlot cfg params chains)

-- ---------------------------------------------------------------------------
-- Energy plot (NUTS の BFMI 診断)
-- ---------------------------------------------------------------------------

-- | PyMC-style energy plot for HMC / NUTS chains.
--
-- 2 本の KDE を重ね描き:
--
--   * Marginal energy E_n         — 事後分布から見た energy の分布
--   * Energy transition |E_n − E_{n−1}| を中心化した分布 (= π_E)
--
-- 両者がよく重なるなら良好。乖離が大きい (= BFMI が低い) と
-- 運動量再サンプリングがエネルギー方向の探索を取りこぼしている可能性。
--
-- 'chainEnergy' が空のチェーン (MH/Gibbs 由来) では空の図になる。
energyPlot :: PlotConfig -> Chain -> VegaLite
energyPlot cfg chain =
  let es     = chainEnergy chain
      mu     = if null es then 0 else sum es / fromIntegral (length es)
      eMar   = map (\e -> e - mu) es                    -- 中心化エネルギー
      eTrans = zipWith (-) (drop 1 es) es               -- ΔE_n
      bfmiV  = fromMaybe (0/0) (bfmi es)
      sub    = T.pack (printf "BFMI = %.3f" bfmiV)
      kdeMar = kde 200 eMar
      kdeTr  = kde 200 eTrans
      (xM, yM) = unzip kdeMar
      (xT, yT) = unzip kdeTr
  in toVegaLite
      [ title (plotTitle cfg <> " — " <> sub) []
      , layer
          [ asSpec
              [ dataFromColumns []
                  . dataColumn "x" (Numbers xM)
                  . dataColumn "y" (Numbers yM)
                  . dataColumn "kind" (Strings (replicate (length xM) "marginal E (centered)"))
                  $ []
              , mark Area [MOpacity 0.35]
              , encoding
                  . position X [PName "x", PmType Quantitative,
                                PAxis [AxTitle "Energy"]]
                  . position Y [PName "y", PmType Quantitative,
                                PAxis [AxTitle "Density"]]
                  . color [ MName "kind", MmType Nominal
                          , MScale [SScheme "tableau10" []]
                          , MLegend [LTitle ""] ]
                  $ []
              ]
          , asSpec
              [ dataFromColumns []
                  . dataColumn "x" (Numbers xT)
                  . dataColumn "y" (Numbers yT)
                  . dataColumn "kind" (Strings (replicate (length xT) "transition ΔE"))
                  $ []
              , mark Area [MOpacity 0.35]
              , encoding
                  . position X [PName "x", PmType Quantitative]
                  . position Y [PName "y", PmType Quantitative]
                  . color [ MName "kind", MmType Nominal
                          , MScale [SScheme "tableau10" []]
                          , MLegend [LTitle ""] ]
                  $ []
              ]
          ]
      , width  (plotWidth cfg)
      , height (plotHeight cfg)
      ]

energyPlotFile :: OutputFormat -> FilePath -> PlotConfig -> Chain -> IO ()
energyPlotFile fmt path cfg chain =
  writeSpec fmt path (energyPlot cfg chain)

-- ---------------------------------------------------------------------------
-- Posterior summary table  (az.summary 相当)
-- ---------------------------------------------------------------------------

-- SummaryRow / posteriorSummary は Hanalyze.Stat.Summary に移管 (Phase H6)。
-- 後方互換のため Hanalyze.Viz.MCMC からも export 経由で参照可能。

-- | Standalone HTML table summarizing the posterior.
posteriorSummaryHtml :: Text -> [SummaryRow] -> Text
posteriorSummaryHtml title rows =
  let multi      = any (\r -> case srRhat r of Just _ -> True; _ -> False) rows
      rhatHeader = if multi then "<th>R-hat</th>" else ""
      cell t     = "<td>" <> t <> "</td>"
      fmt v      = T.pack (printf "%.4f" v)
      essCell e  = cell (T.pack (show (round e :: Int)))
      rhatCell r = case r of
        Nothing -> if multi then "<td>—</td>" else ""
        Just v  -> "<td style=\"color:" <>
                   (if v < 1.01 then "#2a9d2a" else "#cc2222") <>
                   "\">" <> fmt v <> "</td>"
      row r = T.unlines
        [ "    <tr>"
        , "      " <> cell (srName r)
        , "      " <> cell (fmt (srMean r))
        , "      " <> cell (fmt (srSD r))
        , "      " <> cell (fmt (srHdiLo r))
        , "      " <> cell (fmt (srHdiHi r))
        , "      " <> essCell (srEssV r)
        , "      " <> rhatCell (srRhat r)
        , "    </tr>"
        ]
      header = T.unlines
        [ "    <tr>"
        , "      <th>Parameter</th><th>Mean</th><th>SD</th>"
        , "      <th>HDI 3%</th><th>HDI 97%</th><th>ESS</th>" <> rhatHeader
        , "    </tr>"
        ]
  in T.unlines
       [ "<!DOCTYPE html>"
       , "<html><head><meta charset=\"utf-8\"><title>" <> title <> "</title>"
       , "<style>"
       , "body{font-family:sans-serif;max-width:900px;margin:2em auto;padding:0 1em;}"
       , "table{border-collapse:collapse;width:100%;}"
       , "th,td{padding:.4em .8em;border-bottom:1px solid #ddd;text-align:right;}"
       , "th:first-child,td:first-child{text-align:left;}"
       , "th{background:#f3f3f3;}"
       , "tr:hover{background:#fafafa;}"
       , "h2{border-bottom:2px solid #333;padding-bottom:.3em;}"
       , "</style></head><body>"
       , "<h2>" <> title <> "</h2>"
       , "<table>"
       , "  <thead>" <> header <> "  </thead>"
       , "  <tbody>"
       , T.concat (map row rows)
       , "  </tbody>"
       , "</table>"
       , "</body></html>"
       ]

-- | Write the posterior summary to a standalone HTML file.
posteriorSummaryFile :: FilePath -> Text -> [Text] -> [Chain] -> IO ()
posteriorSummaryFile path title params chains =
  TIO.writeFile path
    (posteriorSummaryHtml title (posteriorSummary params chains))

-- | Print the posterior summary to the console as a table.
printPosteriorSummary :: [Text] -> [Chain] -> IO ()
printPosteriorSummary params chains = do
  let rows  = posteriorSummary params chains
      multi = any (\r -> case srRhat r of Just _ -> True; _ -> False) rows
      hdr | multi     =
              printf "%-12s  %10s  %10s  %10s  %10s  %6s  %6s\n"
                     ("Parameter" :: String) ("mean" :: String) ("sd" :: String)
                     ("hdi_3%" :: String) ("hdi_97%" :: String)
                     ("ess" :: String) ("r_hat" :: String)
          | otherwise =
              printf "%-12s  %10s  %10s  %10s  %10s  %6s\n"
                     ("Parameter" :: String) ("mean" :: String) ("sd" :: String)
                     ("hdi_3%" :: String) ("hdi_97%" :: String)
                     ("ess" :: String)
      pr r
        | multi =
            let rh = case srRhat r of Just v -> printf "%.3f" v; Nothing -> "—" :: String
            in printf "%-12s  %10.4f  %10.4f  %10.4f  %10.4f  %6d  %6s\n"
                  (T.unpack (srName r)) (srMean r) (srSD r)
                  (srHdiLo r) (srHdiHi r) (round (srEssV r) :: Int) rh
        | otherwise =
            printf "%-12s  %10.4f  %10.4f  %10.4f  %10.4f  %6d\n"
                  (T.unpack (srName r)) (srMean r) (srSD r)
                  (srHdiLo r) (srHdiHi r) (round (srEssV r) :: Int)
  hdr
  putStrLn (replicate (if multi then 79 else 72) '-')
  mapM_ pr rows

-- ---------------------------------------------------------------------------
-- Rank plot (多チェーン収束診断)
-- ---------------------------------------------------------------------------

-- | Rank plot (analogous to PyMC's @plot_rank@). Proposed by Vehtari et al. (2021)
-- 多チェーンの収束診断: 全チェーンを混ぜた順位を各チェーン内で
-- ヒストグラムにすると、収束時はチェーンごとに一様分布に近づく。
--
-- 引数 nBins は順位ヒストグラムのビン数 (典型値: 20)。
rankPlot :: PlotConfig -> Int -> [Text] -> [Chain] -> VegaLite
rankPlot cfg nBins names chains = toVegaLite
  [ title (plotTitle cfg) []
  , vConcat (map panel names)
  ]
  where
    nChains = length chains
    panel pname =
      let perChain  = map (chainVals pname) chains
          flat      = [ (cid, v)
                      | (cid, vs) <- zip [(1 :: Int) ..] perChain
                      , v <- vs ]
          n         = length flat
          -- 順位 (1..n) を value 昇順に割り当てる
          indexed   = zip [(0 :: Int) ..] flat
          sorted    = sortBy (\(_, (_, a)) (_, (_, b)) -> compare a b) indexed
          ranked    = zipWith (\rk (origIdx, _) -> (origIdx, rk))
                              [(1 :: Int) ..] sorted
          rankBy    = sortBy (\(a,_) (b,_) -> compare a b) ranked
          ranks     = map snd rankBy   -- 元 (chain, value) 順序の rank 列
          chainSeq  = map fst flat
          binSize   = max 1 (n `div` nBins)
          binOf r   = min (nBins - 1) ((r - 1) `div` binSize)
          counts    = [ ((cid, b), 1 :: Int)
                      | (cid, r) <- zip chainSeq ranks
                      , let b = binOf r ]
          -- (chain, bin) ごとに集計
          tally     = groupAndCount counts
          xs        = [ fromIntegral b :: Double
                      | (_, b) <- map fst tally ]
          ys        = [ fromIntegral c :: Double
                      | (_, c) <- tally ]
          chainIds  = [ T.pack (show cid)
                      | ((cid, _), _) <- tally ]
      in asSpec
          [ dataFromColumns []
              . dataColumn "bin"   (Numbers xs)
              . dataColumn "count" (Numbers ys)
              . dataColumn "chain" (Strings chainIds)
              $ []
          , mark Bar [MOpacity 0.7]
          , encoding
              . position X [ PName "bin",   PmType Ordinal
                           , PAxis [AxTitle "Rank bin"] ]
              . position Y [ PName "count", PmType Quantitative
                           , PAxis [AxTitle pname] ]
              . color [ MName "chain", MmType Nominal
                      , MScale [SScheme "tableau10" []]
                      , MLegend [LTitle "Chain"] ]
              . column [ FName "chain", FmType Nominal,
                         FHeader [HTitle ("chain (" <> pname <> ")")] ]
              $ []
          , width (max 60 (plotWidth cfg / fromIntegral nChains))
          , height 100
          ]

    groupAndCount :: [((Int, Int), Int)] -> [((Int, Int), Int)]
    groupAndCount xs =
      let mp = foldr (\(k, v) acc -> insertWith (+) k v acc) [] xs
      in mp
      where
        insertWith f k v ((k', v'):rest)
          | k == k'   = (k', f v v') : rest
          | otherwise = (k', v')     : insertWith f k v rest
        insertWith _ k v [] = [(k, v)]

rankPlotFile :: OutputFormat -> FilePath -> PlotConfig
             -> Int -> [Text] -> [Chain] -> IO ()
rankPlotFile fmt path cfg nBins names chains =
  writeSpec fmt path (rankPlot cfg nBins names chains)

-- ---------------------------------------------------------------------------
-- Posterior predictive check (pp_check 相当)
-- ---------------------------------------------------------------------------

-- | Posterior-predictive check: overlay the KDE of the observations on
-- the KDEs returned by @posteriorPredictive@
-- K 件の予測サンプル KDE をスパゲッティ式に重ね描き、平均予測 KDE を太線で
-- 重ねる。観測 (青) と予測の中心線 (オレンジ) が一致しなければモデル誤指定。
--
-- 引数:
--   * @observed@ — 元観測 y のリスト
--   * @predDraws@ — @posteriorPredictive@ 出力の各サンプル (各要素は元データと
--                   同じ長さの y_rep)
--   * @nOverlay@ — 描画する個別予測ドローの本数 (典型 50)
ppcPlot :: PlotConfig -> [Double] -> [[Double]] -> Int -> VegaLite
ppcPlot cfg observed predDraws nOverlay =
  let nDraws        = length predDraws
      step          = max 1 (nDraws `div` max 1 nOverlay)
      thinned       = [ predDraws !! i
                      | i <- [0, step .. nDraws - 1]
                      , i < nDraws ]
      -- 個別ドローごとの KDE (id, x, y)
      drawSpecs     = concat
        [ [ (i :: Int, x, y) | (x, y) <- kde 100 d ]
        | (i, d) <- zip [0 ..] thinned
        , length d >= 2 ]
      drawIds       = [ T.pack (show i)            | (i, _, _) <- drawSpecs ]
      drawXs        = [ x                          | (_, x, _) <- drawSpecs ]
      drawYs        = [ y                          | (_, _, y) <- drawSpecs ]
      -- 全予測平均の KDE
      flatPred      = concat predDraws
      meanKde       = kde 200 flatPred
      (mxs, mys)    = unzip meanKde
      -- 観測の KDE
      obsKde        = kde 200 observed
      (oxs, oys)    = unzip obsKde
  in toVegaLite
      [ title (plotTitle cfg) []
      , layer
          [ -- スパゲッティ (個別予測ドロー)
            asSpec
              [ dataFromColumns []
                  . dataColumn "x"  (Numbers drawXs)
                  . dataColumn "y"  (Numbers drawYs)
                  . dataColumn "id" (Strings drawIds)
                  $ []
              , mark Line [MColor "#FF8C42", MOpacity 0.15, MStrokeWidth 0.8]
              , encoding
                  . position X [PName "x", PmType Quantitative,
                                PAxis [AxTitle "y"]]
                  . position Y [PName "y", PmType Quantitative,
                                PAxis [AxTitle "Density"]]
                  . detail [DName "id", DmType Nominal]
                  $ []
              ]
          , -- 予測平均
            asSpec
              [ dataFromColumns []
                  . dataColumn "x" (Numbers mxs)
                  . dataColumn "y" (Numbers mys)
                  $ []
              , mark Line [MColor "#FF8C42", MStrokeWidth 2.5]
              , encoding
                  . position X [PName "x", PmType Quantitative]
                  . position Y [PName "y", PmType Quantitative]
                  $ []
              ]
          , -- 観測 KDE
            asSpec
              [ dataFromColumns []
                  . dataColumn "x" (Numbers oxs)
                  . dataColumn "y" (Numbers oys)
                  $ []
              , mark Line [MColor "#1F77B4", MStrokeWidth 3.0]
              , encoding
                  . position X [PName "x", PmType Quantitative]
                  . position Y [PName "y", PmType Quantitative]
                  $ []
              ]
          ]
      , width  (plotWidth cfg)
      , height (plotHeight cfg)
      ]

ppcPlotFile :: OutputFormat -> FilePath -> PlotConfig
            -> [Double] -> [[Double]] -> Int -> IO ()
ppcPlotFile fmt path cfg observed predDraws nOverlay =
  writeSpec fmt path (ppcPlot cfg observed predDraws nOverlay)

-- ---------------------------------------------------------------------------
-- Divergence overlay (NUTS divergent transitions の可視化)
-- ---------------------------------------------------------------------------

-- | Pair scatter overlaid with divergent iterations as red X markers.
--
-- 引数:
--   * @xName@, @yName@: ペア散布の軸となる latent パラメタ名
--   * @divIdx@        : divergent 反復の 0-origin index 列 (バーンイン後)。
--                       将来 NUTS が `chainDivergences` を返したらそれを渡す。
--                       Phase F5 では空リストや手動指定で動作確認できる。
--
-- パラメタ空間で divergent が局所化していれば、その付近の事後分布が
-- 病的 (高曲率) であることを示し、reparameterization の検討材料になる。
pairScatterDiv :: PlotConfig -> Text -> Text -> Chain -> [Int] -> VegaLite
pairScatterDiv cfg xName yName chain divIdx =
  let xs       = chainVals xName chain
      ys       = chainVals yName chain
      n        = min (length xs) (length ys)
      validIdx = [ i | i <- divIdx, i >= 0, i < n ]
      divXs    = [ xs !! i | i <- validIdx ]
      divYs    = [ ys !! i | i <- validIdx ]
  in toVegaLite
      [ title (plotTitle cfg) []
      , layer
          [ asSpec  -- 通常の散布
              [ dataFromColumns []
                  . dataColumn xName (Numbers xs)
                  . dataColumn yName (Numbers ys)
                  $ []
              , mark Point [MOpacity 0.25, MSize 15, MColor "#4C72B0"]
              , encoding
                  . position X [PName xName, PmType Quantitative]
                  . position Y [PName yName, PmType Quantitative]
                  $ []
              ]
          , asSpec  -- divergent な点を赤 X で重ねる
              [ dataFromColumns []
                  . dataColumn xName (Numbers divXs)
                  . dataColumn yName (Numbers divYs)
                  $ []
              , mark Point [ MShape SymCross, MSize 80
                           , MColor "#DD2222", MStrokeWidth 2.0
                           , MOpacity 0.9 ]
              , encoding
                  . position X [PName xName, PmType Quantitative]
                  . position Y [PName yName, PmType Quantitative]
                  $ []
              ]
          ]
      , width  (plotWidth  cfg)
      , height (plotHeight cfg)
      ]

pairScatterDivFile :: OutputFormat -> FilePath -> PlotConfig
                   -> Text -> Text -> Chain -> [Int] -> IO ()
pairScatterDivFile fmt path cfg xName yName chain divIdx =
  writeSpec fmt path (pairScatterDiv cfg xName yName chain divIdx)
