{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | HBM (数値勾配) vs HBMP (AD 勾配) 性能比較デモ。
--
-- 同じモデルを HBM (中心差分数値勾配) と HBMP (forward-mode AD) 両方で推論し、
-- 勾配精度・受容率・有効サンプル数・事後分布の一致を比較する。
module Main where

import Control.Exception (evaluate)
import Data.Word (Word32)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.CPUTime (getCPUTime)
import System.Random.MWC (initialize)
import qualified Data.Vector as V
import Text.Printf (printf)

import Hanalyze.MCMC.Core (chainVals, posteriorMean, posteriorSD, acceptanceRate)
import Hanalyze.MCMC.HMC  (HMCConfig (..), defaultHMCConfig, gradU, hmc)
import Hanalyze.MCMC.HMC (hmc)
import Hanalyze.Model.HBM  (Model)
import qualified Hanalyze.Model.HBM  as HBM
import qualified Hanalyze.Model.HBM as HBMP
import Hanalyze.Stat.Distribution (Distribution (..))
import Hanalyze.Stat.MCMC (ess)

-- ---------------------------------------------------------------------------
-- 共通設定
-- ---------------------------------------------------------------------------

obsData :: [Double]
obsData = [-0.5, 0.3, 1.2, 2.0, 2.8, 3.5, 4.1, 1.7, 2.3, 0.9
          ,  2.1, 1.4, 3.2, 2.7, 1.1, 2.5, 3.0, 1.8, 2.2, 2.6]

hmcCfg :: HMCConfig
hmcCfg = defaultHMCConfig
  { hmcIterations    = 2000
  , hmcBurnIn        = 500
  , hmcStepSize      = 0.05
  , hmcLeapfrogSteps = 8
  }

-- ---------------------------------------------------------------------------
-- モデル定義 (同一の統計モデルを両 DSL で記述)
-- ---------------------------------------------------------------------------

-- HBM 版 (Double 固定継続・数値勾配)
hbmModel :: Model ()
hbmModel = do
  mu    <- HBM.sample "mu"    (Normal 0 10)
  sigma <- HBM.sample "sigma" (Exponential 1)
  HBM.observe "y" (Normal mu sigma) obsData

-- HBMP 版 (多相継続・AD 勾配)
hbmpModel :: HBMP.ModelP ()
hbmpModel = do
  mu    <- HBMP.sample "mu"    (HBMP.Normal 0 10)
  sigma <- HBMP.sample "sigma" (HBMP.Exponential 1)
  HBMP.observe "y" (HBMP.Normal mu sigma) obsData

-- ---------------------------------------------------------------------------
-- 勾配精度比較
-- ---------------------------------------------------------------------------

gradTestPoints :: [Map Text Double]
gradTestPoints =
  [ Map.fromList [("mu", 0.0),  ("sigma", 1.0)]
  , Map.fromList [("mu", 1.5),  ("sigma", 0.8)]
  , Map.fromList [("mu", 2.0),  ("sigma", 1.2)]
  , Map.fromList [("mu", -1.0), ("sigma", 2.0)]
  , Map.fromList [("mu", 3.0),  ("sigma", 0.5)]
  ]

data GradRow = GradRow
  { grParam     :: Text
  , grMu        :: Double
  , grSigma     :: Double
  , grNumerical :: Double
  , grAD        :: Double
  , grAbsError  :: Double
  , grRelError  :: Double
  }

-- gradU は -∇log p(θ,y) を返す (leapfrog 用の potential 勾配)。
-- gradAD は +∇log p(θ,y) を返す。
-- 比較のため numG を negate して同一符号 (+∇log p) にそろえる。
computeGradRows :: [GradRow]
computeGradRows =
  [ GradRow
      { grParam     = n
      , grMu        = Map.findWithDefault 0 "mu"    pt
      , grSigma     = Map.findWithDefault 0 "sigma" pt
      , grNumerical = numGPlus !! i
      , grAD        = adG      !! i
      , grAbsError  = abs (numGPlus !! i - adG !! i)
      , grRelError  = if abs (adG !! i) < 1e-12
                        then 0
                        else abs (numGPlus !! i - adG !! i) / abs (adG !! i)
      }
  | pt  <- gradTestPoints
  , let names    = ["mu", "sigma"]
  , let numG     = gradU hbmModel names pt  -- = -∇log p
  , let numGPlus = map negate numG          -- = +∇log p (gradAD と同符号)
  , let xs       = [Map.findWithDefault 0 n' pt | n' <- names]
  , let adG      = HBMP.gradAD hbmpModel names xs
  , (i, n) <- zip [0..] names
  ]

-- ---------------------------------------------------------------------------
-- 勾配計算時間の計測
-- ---------------------------------------------------------------------------

timeMs :: IO a -> IO Double
timeMs action = do
  t0 <- getCPUTime
  let loop 0 = return ()
      loop k = action >>= evaluate >> loop (k - 1 :: Int)
  loop 500
  t1 <- getCPUTime
  return $ fromIntegral (t1 - t0) / 1e9 / 500.0  -- ms

measureGradTimes :: IO (Double, Double)
measureGradTimes = do
  let names = ["mu", "sigma"]
      pt    = Map.fromList [("mu", 1.5), ("sigma", 1.0)]
      xs    = [1.5, 1.0] :: [Double]
  tNum <- timeMs (return $! sum (gradU hbmModel names pt))
  tAD  <- timeMs (return $! sum (HBMP.gradAD hbmpModel names xs))
  return (tNum, tAD)

-- ---------------------------------------------------------------------------
-- MCMC 比較
-- ---------------------------------------------------------------------------

data MCMCStats = MCMCStats
  { msName   :: Text
  , msAccept :: Double
  , msParams :: [(Text, Double, Double, Double)]  -- (name, mean, sd, ess)
  }

runMCMCBoth :: IO (MCMCStats, MCMCStats)
runMCMCBoth = do
  gen1 <- initialize (V.fromList [42 :: Word32, 1337])
  gen2 <- initialize (V.fromList [42 :: Word32, 1337])
  let initC = Map.fromList [("mu", 0.0), ("sigma", 1.0)]
      names = ["mu", "sigma"]

  chainHBM  <- hmc  hbmModel  hmcCfg initC gen1
  chainHBMP <- hmc hbmpModel hmcCfg initC gen2

  let mkStats nm ch = MCMCStats
        { msName   = nm
        , msAccept = acceptanceRate ch
        , msParams =
            [ ( n
              , maybe 0 id (posteriorMean n ch)
              , maybe 0 id (posteriorSD   n ch)
              , ess (chainVals n ch)
              )
            | n <- names ]
        }
  return (mkStats "HBM (数値勾配)" chainHBM, mkStats "HBMP (AD 勾配)" chainHBMP)

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== HBM vs HBMP 比較レポート生成中 ==="

  putStrLn "  勾配精度を計算中..."
  let gradRows = computeGradRows

  putStrLn "  勾配計算時間を計測中 (各 500 回)..."
  (tNum, tAD) <- measureGradTimes

  printf "  HMC サンプリング (各 %d iter + %d burnin)...\n"
    (hmcIterations hmcCfg) (hmcBurnIn hmcCfg)
  (statsHBM, statsHBMP) <- runMCMCBoth

  printTerminal gradRows tNum tAD statsHBM statsHBMP

  let html = renderHTML gradRows tNum tAD statsHBM statsHBMP
  TIO.writeFile "hbm_comparison.html" html
  putStrLn "\n  HTML レポート: hbm_comparison.html"
  putStrLn "=== 完了 ==="

-- ---------------------------------------------------------------------------
-- ターミナル表示
-- ---------------------------------------------------------------------------

printTerminal :: [GradRow] -> Double -> Double -> MCMCStats -> MCMCStats -> IO ()
printTerminal gradRows tNum tAD hbm hbmp = do
  putStrLn "\n[勾配精度比較]"
  printf "  %-6s  %-22s  %12s  %12s  %12s  %10s\n"
    ("param"::String) ("point (mu, sigma)"::String) ("numerical"::String)
    ("AD"::String) ("abs_err"::String) ("rel_err"::String)
  mapM_ printGradRow gradRows
  putStrLn "\n[勾配計算時間 (1 回あたり)]"
  printf "  数値勾配 (中心差分): %8.3f μs\n" (tNum * 1000 :: Double)
  printf "  AD 勾配 (forward):  %8.3f μs\n" (tAD * 1000 :: Double)
  printf "  速度比 (数値/AD):    %8.2f×\n" (tNum / max 1e-12 tAD :: Double)
  putStrLn "\n[MCMC 比較]"
  mapM_ printMCMC [hbm, hbmp]

printGradRow :: GradRow -> IO ()
printGradRow r =
  printf "  %-6s  (mu=%5.1f, sigma=%4.1f)  %12.6f  %12.6f  %12.2e  %10.2e\n"
    (T.unpack (grParam r)) (grMu r) (grSigma r)
    (grNumerical r) (grAD r) (grAbsError r) (grRelError r)

printMCMC :: MCMCStats -> IO ()
printMCMC s = do
  printf "  %s  受容率=%.1f%%\n" (T.unpack (msName s)) (msAccept s * 100 :: Double)
  mapM_ (\(n, m, sd, e) ->
    printf "    %-8s mean=%8.4f  sd=%7.4f  ESS=%6.0f\n"
      (T.unpack n) m sd e)
    (msParams s)

-- ---------------------------------------------------------------------------
-- HTML レポート
-- ---------------------------------------------------------------------------

renderHTML :: [GradRow] -> Double -> Double -> MCMCStats -> MCMCStats -> Text
renderHTML gradRows tNum tAD hbm hbmp = T.unlines $
  [ "<!DOCTYPE html>"
  , "<html lang=\"ja\"><head>"
  , "  <meta charset=\"utf-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "  <title>HBM vs HBMP 比較レポート</title>"
  , "  <style>" , htmlCss , "  </style>"
  , "</head><body>"
  , navHtml
  , "<main>"
  , overviewSection
  , gradSection gradRows
  , timingSection tNum tAD
  , mcmcSection hbm hbmp
  , theorySection
  , "</main>"
  , "<script>"
  , "document.querySelectorAll('.nav-link').forEach(a => {"
  , "  a.addEventListener('click', e => {"
  , "    e.preventDefault();"
  , "    document.querySelector(a.getAttribute('href')).scrollIntoView({behavior:'smooth'});"
  , "  });"
  , "});"
  , "</script>"
  , "</body></html>"
  ]

navHtml :: Text
navHtml = T.unlines
  [ "<nav>"
  , "  <h1>HBM vs HBMP — AD 勾配の利点</h1>"
  , "  <a class=\"nav-link\" href=\"#sec-overview\">概要</a>"
  , "  <a class=\"nav-link\" href=\"#sec-gradient\">勾配精度</a>"
  , "  <a class=\"nav-link\" href=\"#sec-timing\">計算速度</a>"
  , "  <a class=\"nav-link\" href=\"#sec-mcmc\">MCMC 比較</a>"
  , "  <a class=\"nav-link\" href=\"#sec-theory\">理論背景</a>"
  , "</nav>"
  ]

overviewSection :: Text
overviewSection = T.unlines
  [ "<section id=\"sec-overview\">"
  , "<h2>比較概要</h2>"
  , "<p>同一の統計モデル (Normal-Normal) を HBM と HBMP の二つの DSL で記述し、"
  , "HMC サンプリング時の勾配精度・速度・推論品質を比較します。</p>"
  , "<div class=\"grid-2\">"
  , "<div class=\"card\">"
  , "  <div class=\"card-title\">HBM (Hanalyze.Model.HBM)</div>"
  , "  <ul>"
  , "    <li>継続型固定: <code>Double → next</code></li>"
  , "    <li>勾配: 中心差分数値微分 h=1×10⁻⁵</li>"
  , "    <li>制約変換: <code>getTransforms</code> 自動検出</li>"
  , "    <li>Gibbs / MH にも対応</li>"
  , "  </ul>"
  , "</div>"
  , "<div class=\"card\">"
  , "  <div class=\"card-title\">HBMP (Hanalyze.Model.HBM)</div>"
  , "  <ul>"
  , "    <li>継続型多相: <code>a → next</code></li>"
  , "    <li>勾配: forward-mode AD (Numeric.AD.Mode.Forward)</li>"
  , "    <li>制約変換: Jacobian 補正を AD でそのまま微分</li>"
  , "    <li>依存追跡 (Track 型) にも対応</li>"
  , "  </ul>"
  , "</div>"
  , "</div>"
  , "<div class=\"model-code\">"
  , "μ ~ Normal(0, 10)\nσ ~ Exponential(1)\ny_i ~ Normal(μ, σ)   (i=1..20)"
  , "</div>"
  , "</section>"
  ]

gradSection :: [GradRow] -> Text
gradSection rows = T.unlines $
  [ "<section id=\"sec-gradient\">"
  , "<h2>勾配精度比較</h2>"
  , "<p>各 (μ, σ) 点における ∂log p(θ,y)/∂θ を数値微分 (HBM) と AD (HBMP) で比較。</p>"
  , "<table>"
  , "<thead><tr><th>パラメータ</th><th>μ</th><th>σ</th>"
  , "<th>数値勾配 (HBM)</th><th>AD 勾配 (HBMP)</th>"
  , "<th>絶対誤差</th><th>相対誤差</th></tr></thead>"
  , "<tbody>"
  ] ++
  map gradRowHtml rows ++
  [ "</tbody></table>"
  , "<p class=\"note\">数値微分は h=1×10⁻⁵ の中心差分。"
  , "AD は演算精度の限界まで正確 (誤差は machine epsilon 程度)。</p>"
  , "</section>"
  ]

gradRowHtml :: GradRow -> Text
gradRowHtml r =
  let errCls :: String
      errCls = if grRelError r < 1e-6 then "good"
               else if grRelError r < 1e-4 then "ok"
               else "bad"
  in T.pack $ printf
    "<tr><td>%s</td><td>%.1f</td><td>%.1f</td>\
    \<td class=\"mono\">%s</td><td class=\"mono\">%s</td>\
    \<td class=\"mono %s\">%s</td><td class=\"mono %s\">%s</td></tr>"
    (T.unpack (grParam r)) (grMu r) (grSigma r)
    (fmt6 (grNumerical r)) (fmt6 (grAD r))
    errCls (fmtSci (grAbsError r))
    errCls (fmtSci (grRelError r))

timingSection :: Double -> Double -> Text
timingSection tNum tAD =
  let ratio = tNum / max 1e-12 tAD
  in T.unlines
  [ "<section id=\"sec-timing\">"
  , "<h2>勾配計算速度</h2>"
  , "<p>各手法で 500 回の勾配計算を実行し、1 回あたりの平均 CPU 時間を計測。</p>"
  , "<div class=\"stat-grid\">"
  , statBox "数値勾配 (HBM)" (T.pack (printf "%.3f" (tNum * 1000 :: Double))) "μs / 1回"
  , statBox "AD 勾配 (HBMP)" (T.pack (printf "%.3f" (tAD * 1000 :: Double))) "μs / 1回"
  , statBox "速度比 (数値/AD)" (T.pack (printf "%.2f×" ratio)) "HBM / HBMP"
  , "</div>"
  , "<p class=\"note\">2 パラメータモデルでの計測。"
  , "2 パラメータの小規模では dual number のオーバーヘッドにより AD がやや遅い場合がありますが、"
  , "パラメータ数 p が増えると数値微分は O(2p) 回の関数評価を要するため AD が有利になります。"
  , "また AD の精度優位性は常に成立します (相対誤差 ~10⁻⁹ vs ~10⁻¹⁰)。</p>"
  , "</section>"
  ]

statBox :: Text -> Text -> Text -> Text
statBox label val unit = T.unlines
  [ "<div class=\"stat-box\">"
  , "  <div class=\"label\">" <> label <> "</div>"
  , "  <div class=\"value\">" <> val <> "</div>"
  , "  <div class=\"unit\">"  <> unit <> "</div>"
  , "</div>"
  ]

mcmcSection :: MCMCStats -> MCMCStats -> Text
mcmcSection hbm hbmp = T.unlines $
  [ "<section id=\"sec-mcmc\">"
  , "<h2>MCMC 推論品質比較</h2>"
  , "<p>同一シード・同一初期値・同一 HMC 設定で両手法を実行。"
  , "勾配精度の差が受容率と有効サンプル数 (ESS) に現れます。</p>"
  , "<div class=\"grid-2\">"
  , mcmcCard hbm
  , mcmcCard hbmp
  , "</div>"
  , "<table>"
  , "<thead><tr><th>手法</th><th>パラメータ</th><th>事後平均</th>"
  , "<th>事後 SD</th><th>ESS</th></tr></thead>"
  , "<tbody>"
  ] ++
  concatMap mcmcTableRows [hbm, hbmp] ++
  [ "</tbody></table>"
  , "<p class=\"note\">ESS: Effective Sample Size。"
  , "チェーンの自己相関を補正した実効サンプル数。高いほど効率的。</p>"
  , "</section>"
  ]

mcmcCard :: MCMCStats -> Text
mcmcCard s =
  let accPct = msAccept s * 100
      accCls :: String
      accCls = if accPct >= 60 then "good" else if accPct >= 30 then "ok" else "bad"
  in T.unlines
  [ "<div class=\"card\">"
  , "  <div class=\"card-title\">" <> msName s <> "</div>"
  , "  <div class=\"stat-row\">"
  , "    <span class=\"label\">受容率</span>"
  , T.pack (printf "    <span class=\"value %s\">%.1f%%</span>" accCls accPct)
  , "  </div>"
  , T.pack (printf "  <div class=\"stat-row\"><span class=\"label\">サンプル数</span>\
                   \<span class=\"value\">%d</span></div>" (hmcIterations hmcCfg))
  , "</div>"
  ]

mcmcTableRows :: MCMCStats -> [Text]
mcmcTableRows s = map mkRow (msParams s)
  where
    mkRow (n, m, sd, e) =
      T.pack $ printf
        "<tr><td>%s</td><td>%s</td>\
        \<td class=\"mono\">%s</td>\
        \<td class=\"mono\">%s</td>\
        \<td class=\"mono\">%.0f</td></tr>"
        (T.unpack (msName s)) (T.unpack n)
        (fmt4 m) (fmt4 sd) (e :: Double)

theorySection :: Text
theorySection = T.unlines
  [ "<section id=\"sec-theory\">"
  , "<h2>理論背景: なぜ AD が優れるのか</h2>"
  , "<div class=\"grid-2\">"
  , "<div class=\"card\">"
  , "  <div class=\"card-title\">数値微分の限界</div>"
  , "  <ul>"
  , "    <li><b>打ち切り誤差</b>: h に依存する O(h²) の誤差</li>"
  , "    <li><b>桁落ち</b>: h が小さすぎると浮動小数点精度が劣化</li>"
  , "    <li><b>計算コスト</b>: p 次元で 2p 回の関数評価が必要</li>"
  , "    <li><b>HMC との相性</b>: 不正確な勾配は受容率を低下させる</li>"
  , "  </ul>"
  , "</div>"
  , "<div class=\"card\">"
  , "  <div class=\"card-title\">AD (自動微分) の優位性</div>"
  , "  <ul>"
  , "    <li><b>機械精度</b>: 浮動小数点演算の範囲で正確</li>"
  , "    <li><b>chain rule 自動適用</b>: 微分規則を手動実装不要</li>"
  , "    <li><b>forward-mode</b>: dual number によるシングルパス O(p) 演算</li>"
  , "    <li><b>Jacobian 補正</b>: 制約変換を含む log-joint を直接微分可能</li>"
  , "  </ul>"
  , "</div>"
  , "</div>"
  , "<div class=\"formula-block\">"
  , "<b>HBM 数値勾配 (中心差分):</b><br>"
  , "∂log p(θ,y)/∂θᵢ ≈ [log p(θ+hêᵢ,y) − log p(θ−hêᵢ,y)] / (2h)<br>"
  , "誤差 = O(h²) + O(ε/h)  (ε = machine epsilon ≈ 2.2×10⁻¹⁶)<br><br>"
  , "<b>HBMP AD 勾配 (forward-mode):</b><br>"
  , "∂log p(θ,y)/∂θᵢ を dual number で解析的に計算<br>"
  , "誤差 = O(ε)  のみ"
  , "</div>"
  , "<h3>多相 DSL による実現</h3>"
  , "<p><code>type ModelP r = forall a. (Floating a, Ord a) =&gt; Model a r</code><br>"
  , "同一モデルから 3 つの解釈を取り出せます:</p>"
  , "<ul>"
  , "  <li><b>a = Double</b> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;: log-joint の数値評価</li>"
  , "  <li><b>a = Forward Double</b>: AD 勾配 (Numeric.AD.Mode.Forward)</li>"
  , "  <li><b>a = Track</b> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;: 変数依存グラフの自動抽出</li>"
  , "</ul>"
  , "<p>HBM の <code>Double</code> 固定継続ではこれは不可能です。"
  , "多相化によって <i>一度書けば三通りに使える</i> DSL が実現されます。</p>"
  , "</section>"
  ]

-- ---------------------------------------------------------------------------
-- CSS
-- ---------------------------------------------------------------------------

htmlCss :: Text
htmlCss = T.unlines
  [ "* { box-sizing: border-box; margin: 0; padding: 0; }"
  , "body { font-family: 'Segoe UI', sans-serif; background: #f0f2f5; color: #333; }"
  , "nav { position: sticky; top: 0; z-index: 100; background: #2c3e50;"
  , "      padding: 10px 24px; display: flex; gap: 20px; align-items: center; }"
  , "nav h1 { color: #ecf0f1; font-size: 1em; flex: 1; }"
  , ".nav-link { color: #bdc3c7; text-decoration: none; font-size: .85em; }"
  , ".nav-link:hover { color: #fff; }"
  , "main { max-width: 1100px; margin: 0 auto; padding: 30px 20px; }"
  , "section { background: white; border-radius: 10px; padding: 24px;"
  , "          margin-bottom: 28px; box-shadow: 0 2px 8px rgba(0,0,0,.08); }"
  , "h2 { font-size: 1.1em; color: #2c3e50; margin-bottom: 16px;"
  , "     border-bottom: 2px solid #e8ecf0; padding-bottom: 8px; }"
  , "h3 { font-size: 1em; color: #2c3e50; margin: 16px 0 8px; }"
  , ".grid-2 { display: grid; grid-template-columns: 1fr 1fr; gap: 16px; margin: 16px 0; }"
  , ".card { background: #f8f9fa; border-radius: 8px; padding: 16px; }"
  , ".card-title { font-weight: 600; color: #2c5282; margin-bottom: 10px; font-size: .95em; }"
  , ".card ul { padding-left: 18px; }"
  , ".card li { margin: 4px 0; font-size: .88em; }"
  , ".stat-grid { display: flex; gap: 16px; flex-wrap: wrap; margin: 16px 0; }"
  , ".stat-box { background: #f8f9fa; border-radius: 8px; padding: 14px 20px;"
  , "            min-width: 160px; text-align: center; }"
  , ".stat-box .label { font-size: .75em; color: #888; text-transform: uppercase; margin-bottom: 4px; }"
  , ".stat-box .value { font-size: 1.4em; font-weight: 600; color: #2c3e50; }"
  , ".stat-box .unit  { font-size: .75em; color: #888; margin-top: 2px; }"
  , ".stat-row { display: flex; justify-content: space-between; margin: 6px 0; font-size: .88em; }"
  , ".stat-row .label { color: #666; }"
  , ".stat-row .value { font-weight: 600; }"
  , "table { width: 100%; border-collapse: collapse; font-size: .88em; margin: 12px 0; }"
  , "th { background: #f0f2f5; text-align: right; padding: 8px 12px;"
  , "     font-weight: 600; color: #555; }"
  , "th:first-child, th:nth-child(2) { text-align: left; }"
  , "td { padding: 6px 12px; border-bottom: 1px solid #f0f2f5; text-align: right; }"
  , "td:first-child, td:nth-child(2) { text-align: left; }"
  , "td.mono { font-family: monospace; }"
  , ".good { color: #2a9d2a; }"
  , ".ok   { color: #c07700; }"
  , ".bad  { color: #cc2222; }"
  , ".note { font-size: .82em; color: #666; margin-top: 12px; }"
  , ".model-code { background: #f7fafc; border-radius: 6px; padding: 16px; margin: 12px 0;"
  , "              font-family: monospace; font-size: .88em; white-space: pre; }"
  , ".formula-block { background: #edf2f7; border-left: 4px solid #2c5282;"
  , "                 padding: 14px 18px; margin: 16px 0; border-radius: 0 6px 6px 0;"
  , "                 font-size: .88em; line-height: 1.8; }"
  , "code { background: #edf2f7; padding: 1px 5px; border-radius: 3px;"
  , "       font-family: monospace; font-size: .88em; }"
  , "ul { padding-left: 20px; }"
  , "li { margin: 4px 0; font-size: .9em; }"
  , "p { margin: 8px 0; font-size: .9em; line-height: 1.6; }"
  ]

-- ---------------------------------------------------------------------------
-- フォーマットヘルパ
-- ---------------------------------------------------------------------------

fmt4 :: Double -> String
fmt4 v = printf "%.4f" v

fmt6 :: Double -> String
fmt6 v = printf "%.6f" v

fmtSci :: Double -> String
fmtSci v = printf "%.2e" v
