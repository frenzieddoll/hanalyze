{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | 多相 HBM DSL + AD 勾配 HMC の統合デモ。
--
-- 4 つのモデルそれぞれについて:
--   - 自動依存抽出 (extractDeps + Track)
--   - Mermaid DAG 自動生成
--   - AD 勾配 HMC (hmc) で事後推論
--   - 結果のターミナル表示と HTML レポート出力
module Main where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Text.Printf (printf)
import System.Random.MWC (createSystemRandom, GenIO)

import MCMC.Core   (Chain (..), posteriorMean, posteriorSD, acceptanceRate)
import MCMC.HMC    (HMCConfig (..), defaultHMCConfig)
import MCMC.HMC   (hmc)
import Model.HBM

-- ---------------------------------------------------------------------------
-- モデル群: 多相 HBM DSL で書かれた 4 種のモデル
-- ---------------------------------------------------------------------------

-- 1. Normal-Normal
normalModel :: [Double] -> ModelP ()
normalModel ys = do
  mu    <- sample "mu"    (Normal 0 10)
  sigma <- sample "sigma" (Exponential 1)
  observe "y" (Normal mu sigma) ys

-- 2. Beta-Binomial
betaBinomModel :: Int -> [Double] -> ModelP ()
betaBinomModel n ys = do
  p <- sample "p" (Beta 2 2)
  observe "y" (Binomial n p) ys

-- 3. 階層モデル
hierModel :: [Double] -> ModelP ()
hierModel ys = do
  tau   <- sample "tau"   (Exponential 1)
  mu    <- sample "mu"    (Normal 0 tau)
  sigma <- sample "sigma" (Exponential 1)
  observe "y" (Normal mu sigma) ys

-- 4. Gamma-Poisson
gammaPoissonModel :: [Double] -> ModelP ()
gammaPoissonModel ys = do
  lam <- sample "lambda" (Gamma 2 1)
  observe "y" (Poisson lam) ys

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  gen <- createSystemRandom

  let normalObs = [-0.5, 0.3, 1.2, 2.0, 2.8, 3.5, 4.1, 1.7, 2.3, 0.9
                  ,  2.1, 1.4, 3.2, 2.7, 1.1, 2.5, 3.0, 1.8, 2.2, 2.6]
      binomObs  = [7, 8, 6, 7, 9, 7, 8, 7, 6, 7]
      poisObs   = [3, 5, 4, 6, 4, 3, 5, 4, 7, 4, 3, 5, 4, 6, 5]
      cfg       = defaultHMCConfig
                    { hmcIterations = 1500
                    , hmcBurnIn     = 500
                    , hmcStepSize   = 0.05
                    , hmcLeapfrogSteps = 8
                    }

  putStrLn "============================================================"
  putStrLn "  多相 HBM DSL × AD HMC: 4 モデル統合レポート"
  putStrLn "============================================================"

  r1 <- runOne gen cfg "Model 1: Normal-Normal"
          "y ~ Normal(μ, σ),  μ ~ Normal(0,10),  σ ~ Exp(1)"
          (extractDeps (normalModel normalObs))
          (hmc (normalModel normalObs) cfg
                (Map.fromList [("mu",0.0),("sigma",1.0)]))

  r2 <- runOne gen cfg "Model 2: Beta-Binomial"
          "y ~ Binomial(10, p),  p ~ Beta(2, 2)"
          (extractDeps (betaBinomModel 10 binomObs))
          (hmc (betaBinomModel 10 binomObs) cfg
                (Map.singleton "p" 0.5))

  r3 <- runOne gen cfg "Model 3: 階層モデル (tau → mu, σ → y)"
          "y ~ Normal(μ, σ),  μ ~ Normal(0, τ),  τ,σ ~ Exp(1)"
          (extractDeps (hierModel normalObs))
          (hmc (hierModel normalObs) cfg
                (Map.fromList [("tau",1.0),("mu",0.0),("sigma",1.0)]))

  r4 <- runOne gen cfg "Model 4: Gamma-Poisson"
          "y ~ Poisson(λ),  λ ~ Gamma(2, 1)"
          (extractDeps (gammaPoissonModel poisObs))
          (hmc (gammaPoissonModel poisObs) cfg
                (Map.singleton "lambda" 4.0))

  let html = renderHTMLReport [r1, r2, r3, r4]
  TIO.writeFile "hbmp_report.html" html
  putStrLn "\n============================================================"
  putStrLn "  HTML レポート: hbmp_report.html"
  putStrLn "  (Mermaid CDN 経由で DAG 描画。ブラウザで開いて確認)"
  putStrLn "============================================================"

-- ---------------------------------------------------------------------------
-- 実行ヘルパ
-- ---------------------------------------------------------------------------

runOne :: GenIO -> HMCConfig
       -> Text -> Text -> [Node] -> (GenIO -> IO Chain)
       -> IO (Text, Text, [Node], Chain)
runOne gen _cfg title descr nodes runHmc = do
  TIO.putStrLn ("\n## " <> title)
  TIO.putStrLn descr

  putStrLn "\n[依存抽出 (Track 多相インタープリタ)]"
  mapM_ printNode nodes

  putStrLn "\n[Mermaid DAG]"
  TIO.putStrLn (mermaidDAG nodes)

  putStrLn "[HMC 推論 (AD 勾配 + 自動制約変換)]"
  ch <- runHmc gen
  let acc = acceptanceRate ch
  printf "  受容率: %.1f%%  サンプル数: %d\n"
    (acc * 100) (length (chainSamples ch))
  let names = [ nodeName n | n <- nodes, nodeKind n == LatentN ]
  mapM_ (printPosterior ch) names
  return (title, descr, nodes, ch)

printNode :: Node -> IO ()
printNode n = do
  let kindStr = case nodeKind n of
        LatentN     -> "[latent]"
        ObservedN k -> "[obs n=" ++ show k ++ "]"
      depsStr = if Set.null (nodeDeps n)
                  then ""
                  else " ← {" ++ T.unpack (T.intercalate "," (Set.toList (nodeDeps n))) ++ "}"
  printf "  %-12s %-7s ~ %s%s\n"
         kindStr (T.unpack (nodeName n)) (T.unpack (nodeDist n)) depsStr

printPosterior :: Chain -> Text -> IO ()
printPosterior ch nm =
  printf "    %-8s: mean=%8.4f  sd=%7.4f\n"
         (T.unpack nm)
         (maybe 0 id (posteriorMean nm ch))
         (maybe 0 id (posteriorSD   nm ch))

-- ---------------------------------------------------------------------------
-- Mermaid DAG 自動生成
-- ---------------------------------------------------------------------------

mermaidDAG :: [Node] -> Text
mermaidDAG nodes = T.unlines $
  [ "graph TD" ] ++
  [ "    " <> nodeName n <> shape n
  | n <- nodes ] ++
  [ "    " <> p <> " --> " <> nodeName n
  | n <- nodes, p <- Set.toList (nodeDeps n) ]
  where
    shape n = case nodeKind n of
      LatentN     -> "((" <> nodeName n <> "))"
      ObservedN _ -> "[" <> nodeName n <> "]"

-- ---------------------------------------------------------------------------
-- HTML レポート (Mermaid 描画 + 結果テーブル)
-- ---------------------------------------------------------------------------

renderHTMLReport :: [(Text, Text, [Node], Chain)] -> Text
renderHTMLReport results = T.unlines $
  [ "<!DOCTYPE html>"
  , "<html><head><meta charset=\"utf-8\">"
  , "<title>HBMP × AD HMC レポート</title>"
  , "<script src=\"https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js\"></script>"
  , "<script>mermaid.initialize({startOnLoad:true,theme:'default'});</script>"
  , "<style>"
  , "body{font-family:system-ui,sans-serif;max-width:980px;margin:24px auto;padding:0 16px}"
  , "h1{border-bottom:3px solid #2c5282;padding-bottom:8px;color:#2c5282}"
  , "h2{color:#2b6cb0;margin-top:36px;border-left:4px solid #2c5282;padding-left:10px}"
  , "h3{color:#444;margin-top:18px}"
  , "code,pre{background:#f7fafc;border-radius:4px;padding:2px 6px;font-family:monospace}"
  , "pre{padding:12px;overflow-x:auto}"
  , ".dag{display:flex;justify-content:center;background:#f7fafc;padding:16px;border-radius:6px;margin:8px 0}"
  , "table{border-collapse:collapse;margin:8px 0}"
  , "th,td{padding:6px 14px;border:1px solid #cbd5e0;text-align:right}"
  , "th{background:#edf2f7}"
  , "td.name{text-align:left;font-family:monospace;font-weight:600}"
  , ".info-grid{display:grid;grid-template-columns:repeat(2,1fr);gap:8px;margin:8px 0;max-width:520px}"
  , ".info-box{background:#edf2f7;padding:8px 12px;border-radius:4px}"
  , ".info-box b{color:#2c5282}"
  , "</style></head><body>"
  , "<h1>HBMP × AD HMC: 多相 DSL からの自動推論レポート</h1>"
  , "<p>各モデルは <code>forall a. Floating a =&gt; Model a r</code> 形式の多相 DSL で記述。"
  , "依存グラフは <code>Track</code> 型による自動微分風の依存伝播で抽出。"
  , "HMC は <code>Numeric.AD</code> による正確な勾配と事前分布から自動検出した制約変換を使用。</p>"
  ] ++ concatMap renderRun results ++
  [ "</body></html>" ]
  where
    renderRun (title, descr, nodes, ch) =
      [ "<h2>" <> title <> "</h2>"
      , "<p>" <> descr <> "</p>"
      , "<h3>依存グラフ (自動抽出)</h3>"
      , "<div class=\"dag\"><pre class=\"mermaid\">"
      , mermaidDAG nodes
      , "</pre></div>"
      , "<h3>事後分布 (AD HMC)</h3>"
      , "<div class=\"info-grid\">"
      , "<div class=\"info-box\"><b>受容率:</b> " <> tShow (round (acceptanceRate ch * 100) :: Int) <> "%</div>"
      , "<div class=\"info-box\"><b>サンプル数:</b> " <> tShow (length (chainSamples ch)) <> "</div>"
      , "</div>"
      , "<table><thead><tr><th>パラメータ</th><th>事後平均</th><th>事後 SD</th></tr></thead><tbody>"
      ] ++
      [ "<tr><td class=\"name\">" <> nm <> "</td><td>" <> fmt4 mn <> "</td><td>" <> fmt4 sd <> "</td></tr>"
      | n <- nodes, nodeKind n == LatentN
      , let nm = nodeName n
      , let mn = maybe 0 id (posteriorMean nm ch)
      , let sd = maybe 0 id (posteriorSD   nm ch)
      ] ++
      [ "</tbody></table>" ]

    tShow :: Show a => a -> Text
    tShow = T.pack . show
    fmt4 :: Double -> Text
    fmt4 v = T.pack (printf "%.4f" v)
