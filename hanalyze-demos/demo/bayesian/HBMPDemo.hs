{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | 多相 HBM DSL (Hanalyze.Model.HBM) のデモ。
--
-- 同じ多相モデル一つから 4 つの解釈を取り出す:
--   1. 構造検査  (collectNodes)         — Double 特殊化
--   2. log joint (logJoint)             — Double 特殊化
--   3. AD 勾配   (gradAD)               — Numeric.AD で多相化
--   4. 依存追跡  (extractDeps)          — Track 型で多相化
--
-- これらすべてが「@forall a. Floating a => Model a r@」という
-- 多相型の異なる特殊化として実現される。
module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

import Hanalyze.Model.HBM

-- ---------------------------------------------------------------------------
-- 多相モデル定義 (一度書けば 4 つの解釈に使える)
-- ---------------------------------------------------------------------------

-- 階層モデル:
--   tau   ~ Exp(1)              (グループ間分散)
--   mu_g  ~ Normal(0, tau)      (各グループの平均) — 簡易表現
--   sigma ~ Exp(1)              (観測ノイズ)
--   y_i   ~ Normal(mu_g, sigma) (観測)
--
-- この例では mu と sigma の単純なモデルとする (階層は後で拡張可能)。
normalModel :: [Double] -> ModelP ()
normalModel ys = do
  mu    <- sample "mu"    (Normal 0 10)
  sigma <- sample "sigma" (Exponential 1)
  observe "y" (Normal mu sigma) ys

-- もう少し依存関係が複雑なモデル
hierModel :: [Double] -> ModelP ()
hierModel ys = do
  tau   <- sample "tau"   (Exponential 1)
  mu    <- sample "mu"    (Normal 0 tau)         -- mu は tau に依存
  sigma <- sample "sigma" (Exponential 1)
  observe "y" (Normal mu sigma) ys                -- y は mu, sigma に依存

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  let obs = [-0.5, 0.3, 1.2, 2.0, 2.8, 3.5, 4.1, 1.7, 2.3, 0.9]

  putStrLn "=== 多相 HBM DSL の威力: 一つのモデル定義から複数の解釈 ==="
  putStrLn ""

  -- 解釈1: 構造検査 (Double 特殊化)
  putStrLn "■ 解釈1: 構造検査 (collectNodes :: ... → [Node])"
  mapM_ (printNode False) (collectNodes (normalModel obs))

  -- 解釈2: log joint (Double 評価)
  putStrLn "\n■ 解釈2: log joint 数値評価 (a = Double)"
  let params = Map.fromList [("mu", 1.5), ("sigma", 1.2)] :: Map.Map Text Double
  printf "  logJoint (mu=1.5, sigma=1.2) = %.4f\n"
         (logJoint (normalModel obs) params)

  -- 解釈3: AD 勾配 (Reverse/Forward Double で多相化)
  putStrLn "\n■ 解釈3: AD 勾配 (a = Numeric.AD の Forward Double)"
  let g = gradAD (normalModel obs) ["mu", "sigma"] [1.5, 1.2]
  putStrLn "  ∇log p(θ, y) at θ=[1.5, 1.2]:"
  mapM_ (\(n, v) -> printf "    ∂/∂%-6s = %12.6f\n" (T.unpack n) v)
        (zip ["mu","sigma"] g)

  -- 解釈4: 依存追跡 (Track 特殊化)
  putStrLn "\n■ 解釈4: 自動依存追跡 (a = Track)"
  putStrLn "  各ノードが直接依存する変数を Track 型で自動抽出:"
  mapM_ (printNode True) (extractDeps (normalModel obs))

  -- ---------------------------------------------------------------------------
  putStrLn "\n=== より複雑なモデル: 階層構造 (tau → mu, sigma → y) ==="

  putStrLn "\n■ 階層モデルの依存抽出:"
  mapM_ (printNode True) (extractDeps (hierModel obs))

  -- DAG 形式で出力
  putStrLn "\n■ Mermaid DAG (自動生成、コピーして mermaid.live で可視化可):"
  putStrLn (T.unpack (mermaidDAG (extractDeps (hierModel obs))))

  putStrLn "\n✓ 完了"

-- ---------------------------------------------------------------------------
-- ヘルパー
-- ---------------------------------------------------------------------------

printNode :: Bool -> Node -> IO ()
printNode showDeps n = do
  let kindStr = case nodeKind n of
        LatentN     -> "[latent]   "
        ObservedN k -> "[observed] " ++ "(n=" ++ show k ++ ")"
      depsStr = if showDeps && not (Set.null (nodeDeps n))
                  then " ← {" ++ T.unpack (T.intercalate ", " (Set.toList (nodeDeps n))) ++ "}"
                  else ""
  printf "  %s %-8s ~ %s%s\n"
    kindStr (T.unpack (nodeName n)) (T.unpack (nodeDist n)) depsStr

-- 依存ノードリストから Mermaid DAG を生成
mermaidDAG :: [Node] -> Text
mermaidDAG nodes = T.unlines $
  [ "  graph TD"
  ] ++
  [ "    " <> nodeName n <> shape n
  | n <- nodes
  ] ++
  [ "    " <> p <> " --> " <> nodeName n
  | n <- nodes, p <- Set.toList (nodeDeps n)
  ]
  where
    shape n = case nodeKind n of
      LatentN     -> "((" <> nodeName n <> "))"      -- 円
      ObservedN _ -> "[" <> nodeName n <> "]"         -- 長方形
