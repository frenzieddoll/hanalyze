{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Hanalyze.Model.HBM.Interp
-- Description : HBM dialog DSL の評価系 (interpreter) と NUTS 設定・結果整形
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- HBM dialog DSL の評価系 (interpreter) + NUTS 設定 reader + 結果整形。
--
-- Phase 27.5 (2026-05-31) step 1: canvas-backend @フロントエンド app.Analysis.HBM@
-- から eval/interp/curve コアを移設。 frontend が backend 統一 parser から得た
-- @program_ast@ を streaming sidecar が直接 interpret して実モデルを構築できる
-- よう、 DSL の評価系をライブラリ層 (hanalyze) に置く。
--
-- 本 module は **canvas wire 型 (AnalysisRequest 等) にも text parser
-- (DSL frontend) にも依存しない**。 依存は 'Hanalyze.Model.HBM.Ast' (AST) +
-- @Hanalyze.Model.HBM@ (ModelP/Distribution) + @Hanalyze.Stat.*@ /
-- @Hanalyze.MCMC.*@ + aeson / hmatrix のみ。
--
-- text → AST 変換 (@parseHbmText@ 経路) と canvas 専用の @runHbm@ /
-- @buildDataMap@ / @ProgramInfo@ 解決は canvas-backend 側に残す。
module Hanalyze.Model.HBM.Interp
  ( -- * core types
    DataMap
  , Column (..)
  , colDoubles
  , colLength
  , colLevels
  , lookupDoubles
  , EnvA
  , Value (..)
  , PlateCtx (..)
  , topCtx
  , TopBind (..)
  , ParamSummary (..)
  , HbmMeanCurve (..)
    -- * evaluation
  , hasColRef
  , liftD
  , builtinTable
  , asNum
  , asBool
  , asList
  , asMatrix
  , evalScalar
  , evalValue
  , evalDist
  , buildTopEnv
    -- * plate / groups (GLMM forEachGroup)
  , matchForEachGroup
  , lamBodyToStmts
  , retToStmts
  , groupValsIn
  , rowsForGroup
  , groupSuffix
  , groupSuffixFor
    -- * validation / interpretation
  , inferTransforms
  , validateAst
  , preprocessAliases
  , validateStmts
  , interpStmts
  , observeNodeMap
    -- * NUTS config readers
  , readChainCount
  , readNutsConfig
    -- * result shaping
  , paramSummaryMulti
  , fmtSummary
  , round4
  , takeEvery
  , summaryToJson
  , hbmMeanCurveToJson
  , extractObserveMeans
  , collectCols
  , percentileOf
  , computeMeanCurves
    -- * WAIC / LOO / posterior predictive
  , ObsDistSet (..)
  , computeObsDists
  , pointwiseLogLik
  , finitePointwiseLogLik
    -- * Phase 44: multi-column observe (observeMV) WAIC / PPC
  , MvObsDistSet (..)
  , computeMvObsDists
  , pointwiseLogLikMv
  , reconstructMatrixComb
  , MatrixCombSpec (..)
    -- * model graph plate aggregation (GLMM forEachGroup)
  , GraphPlate (..)
  , plateRenameMap
  , collectGraphPlates
  , collapsePlateGraph
  ) where

import Control.Monad (forM_, when)
import Data.Char (isAlpha, isAlphaNum)
import Data.List (sort, nub, transpose)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Numeric.LinearAlgebra as LA

import qualified Hanalyze.MCMC.Core as MC
import qualified Hanalyze.MCMC.NUTS as NUTS
import qualified Hanalyze.Model.HBM as HBM
import qualified Hanalyze.Stat.Distribution as HD
import qualified Hanalyze.Stat.MCMC as SMC

import Hanalyze.Model.HBM.Ast
  ( Expr (..)
  , Lit (..)
  , Bind (..)
  , DoStmt (..)
  , collectApp
  , Err
  )

-- ===========================================================================
-- Interpreter コア型
-- ===========================================================================

-- | 列参照を含む式かどうか(observe の dist 引数に列が混じったら per-row 展開する)。
hasColRef :: Expr -> Bool
hasColRef = go
  where
    go (ECol _)       = True
    go (EApp f x)     = go f || go x
    go (ENeg e)       = go e
    go (EOp _ a b)    = go a || go b
    go (EList xs)     = any go xs
    go (ELet bs e)    = any (go . bindValue) bs || go e
    go (EIf c a b)    = go c || go a || go b
    go (ELam _ b)     = go b
    go (EDo _ _)      = False  -- nested do 不可
    go (ELit _)       = False
    go (EVar _)       = False

-- 'Err' (= Either Text) は Hanalyze.Model.HBM.Ast から import。

-- | スカラ式評価。col 参照は許可されるなら row idx を指定。Nothing なら無効でエラー。
liftD :: Floating a => Double -> a
liftD d = fromRational (toRational d)

-- | 環境: sample / let / top-level で束縛された値 (数値・真偽・関数)。
-- Phase 27 §F-3a で scalar から 'Value' に拡張 (ユーザ定義関数を持てる)。
type EnvA a = Map.Map Text (Value a)

-- | データ列の値 (Phase 41)。 数値列 (連続/整数) と categorical 列
-- (factor = level 辞書 + 整数 code) を区別する sum 型。 'Numeric' は従来の
-- @[Double]@ 相当で後方互換、 'Factor' は R の factor / PyMC coords 相当
-- (level 出現順に 0,1,2,... の code を振る)。
data Column
  = Numeric ![Double]                                     -- ^ 連続 / 整数列
  | Factor  { facLevels :: ![Text], facCodes :: ![Int] }  -- ^ categorical
  deriving (Eq, Show)

-- | データ列の Map。 列名 → 'Column'。
type DataMap = Map.Map Text Column

-- | 列を @[Double]@ として見る (群比較 / 数値 observe / mean-curve 用)。
-- 'Numeric' はそのまま、 'Factor' は code を Double 化 (0,1,2,...)。 既存の
-- 数値ロジックはこの accessor 経由で Factor も透過に扱える。
colDoubles :: Column -> [Double]
colDoubles (Numeric xs)  = xs
colDoubles (Factor _ cs) = map fromIntegral cs

-- | 列長 (行数)。
colLength :: Column -> Int
colLength (Numeric xs)  = length xs
colLength (Factor _ cs) = length cs

-- | 'Factor' なら level 辞書 (出現順)、 'Numeric' なら Nothing。
colLevels :: Column -> Maybe [Text]
colLevels (Factor ls _) = Just ls
colLevels (Numeric _)   = Nothing

-- | 列を @[Double]@ として引く (無ければ空)。 旧 @Map.findWithDefault [] k dm@ の
-- 'Column' 対応版。
lookupDoubles :: Text -> DataMap -> [Double]
lookupDoubles k = maybe [] colDoubles . Map.lookup k

-- | Phase 27 §F-3a: モデル本体評価中の値。 Double 閉の数値 + 真偽 +
-- 一階クロージャ (ユーザ定義関数 / lambda) + 組込関数マーカ + 遅延エラー
-- (top-level 値束縛の評価失敗を lookup まで遅延運搬する)。
data Value a
  = VNum a
  | VBool Bool
  | VList [Value a]                -- Phase 42: list リテラル ([e₁, …, eₙ])。 多値分布
                                   --   (Categorical [probs] / OrderedLogistic eta [cuts])
                                   --   の list 引数評価の土台。 要素はスカラ閉前提。
  | VClosure (EnvA a) [Text] Expr  -- 捕捉環境, 残り仮引数, 本体 (一階・カリー化)
  | VBuiltin Text Int              -- 組込関数 (名前, arity)。 適用時に builtinTable で解決
  | VErr Text                      -- 遅延エラー (top-level 値 thunk の評価失敗)

-- | Phase 27 §F-3a: 組込数学関数ホワイトリスト (name → (arity, impl))。
-- すべて Double 上で閉じる純関数 (IO/import なし)。 GLM リンク
-- (invLogit/logistic) もここに含む。 必要に応じ追加。
builtinTable :: forall a. (Floating a, Ord a) => Map.Map Text (Int, [a] -> a)
builtinTable = Map.fromList
  [ ("exp",      (1, \xs -> exp (head xs)))
  , ("log",      (1, \xs -> log (head xs)))
  , ("log1p",    (1, \xs -> log (1 + head xs)))
  , ("sqrt",     (1, \xs -> sqrt (head xs)))
  , ("abs",      (1, \xs -> abs (head xs)))
  , ("signum",   (1, \xs -> signum (head xs)))
  , ("recip",    (1, \xs -> recip (head xs)))
  , ("negate",   (1, \xs -> negate (head xs)))
  , ("tanh",     (1, \xs -> tanh (head xs)))
  , ("sin",      (1, \xs -> sin (head xs)))
  , ("cos",      (1, \xs -> cos (head xs)))
  , ("logistic", (1, \xs -> 1 / (1 + exp (negate (head xs)))))
  , ("invLogit", (1, \xs -> 1 / (1 + exp (negate (head xs)))))
  , ("min",      (2, \xs -> min (xs !! 0) (xs !! 1)))
  , ("max",      (2, \xs -> max (xs !! 0) (xs !! 1)))
  ]

-- | Phase 43: list builtin (VList→VList) の名前集合。 スカラ 'builtinTable'
-- (= @[a] -> a@) には乗らないので別管理。 softmax 多項ロジット
-- (@Categorical (softmax [η₀, η₁, …])@) で多クラス線形予測子を確率に変換する。
listBuiltins :: Set.Set Text
listBuiltins = Set.fromList ["softmax"]

-- | 安定 softmax (= @exp(xₖ − max x) / Σ@)。 識別性のため基準クラスは η=0 を
-- 明示的に並べる前提 (例 @softmax [0, b₁·x, b₂·x]@)。 空リストはエラー。
softmaxList :: forall a. (Floating a, Ord a) => [a] -> Err [a]
softmaxList [] = Left "softmax: 空リストには適用できません (クラス数 ≥ 1 の [..] が必要です)"
softmaxList xs =
  let m  = maximum xs
      es = map (\x -> exp (x - m)) xs
      s  = sum es
  in Right (map (/ s) es)

-- | Value を数値に落とす (= 親切な日本語エラー)。
asNum :: Value a -> Err a
asNum (VNum x)     = Right x
asNum (VBool _)    = Left "真偽値が数値の位置に現れました (比較式を算術に混ぜていませんか)"
asNum (VClosure{}) = Left "関数が数値の位置に現れました (引数不足か、 適用し忘れ)"
asNum (VBuiltin n _) = Left ("組込関数 " <> n <> " が数値の位置に現れました (引数を渡してください)")
asNum (VList _)    = Left "リストが数値の位置に現れました (list 引数はスカラとして使えません)"
asNum (VErr msg)   = Left msg

-- | Value を真偽に落とす。
asBool :: Value a -> Err Bool
asBool (VBool b)    = Right b
asBool (VNum _)     = Left "数値が真偽の位置に現れました (if の条件は比較式である必要があります)"
asBool (VClosure{}) = Left "関数が真偽の位置に現れました"
asBool (VBuiltin n _) = Left ("組込関数 " <> n <> " が真偽の位置に現れました")
asBool (VList _)    = Left "リストが真偽の位置に現れました"
asBool (VErr msg)   = Left msg

-- | Value を list に落とす (Phase 42: 多値分布の list 引数評価)。 各要素は
-- 'asNum' でスカラに落とせる前提。
asList :: Value a -> Err [Value a]
asList (VList xs)    = Right xs
asList (VNum _)      = Left "数値がリストの位置に現れました (list 引数には [..] を渡してください)"
asList (VBool _)     = Left "真偽値がリストの位置に現れました"
asList (VClosure{})  = Left "関数がリストの位置に現れました"
asList (VBuiltin n _) = Left ("組込関数 " <> n <> " がリストの位置に現れました")
asList (VErr msg)    = Left msg

-- | Value を行列 ([[a]]) に落とす (Phase 44: MvNormal の cov / lkjCorrCholesky の
-- 行列値を評価する用途)。 VList-of-VList を期待し、 各内側 VList の長さ一致と
-- 数値性を検査する。 list 操作で書くのは DSL スカラ値 'Value' 上の小さい構造
-- 変換のためで、 hmatrix Matrix 経路ではない (density 計算側の choleskyL /
-- forwardSub は既存実装を流用)。
asMatrix :: Value a -> Err [[a]]
asMatrix v = do
  rows <- asList v
  mat  <- mapM (\r -> asList r >>= mapM asNum) rows
  case mat of
    []        -> Left "行列が空です (cov / 行列引数には [[..],..] が必要です)"
    (r0 : rs)
      | all ((== length r0) . length) rs -> Right mat
      | otherwise -> Left "行列の各行の長さが揃っていません (cov は正方行列である必要があります)"

-- | スカラ式評価 (= 数値を返す)。 'evalValue' の薄いラッパ。 col 参照は
-- mi=Just i なら行 i、 Nothing なら不可。
evalScalar
  :: forall a. (Floating a, Ord a)
  => EnvA a -> DataMap -> Maybe Int -> Expr -> Err a
evalScalar env dataMap mi e = evalValue env dataMap mi e >>= asNum

-- | Phase 27 §F-3a: 式を 'Value' に評価する interpreter 中核。
-- 適用 (EApp) / if (EIf) / 比較・論理 (EOp) / lambda (ELam) / let を解釈。
-- ユーザ定義関数 (一階・カリー化) と組込数学関数を呼べる。
-- 再帰なし (total) ・ IO/ADT/型クラスなし。
evalValue
  :: forall a. (Floating a, Ord a)
  => EnvA a -> DataMap -> Maybe Int -> Expr -> Err (Value a)
evalValue env dataMap mi = go
  where
    go :: Expr -> Err (Value a)
    go (ELit (LNumber d)) = Right (VNum (liftD d))
    go (ELit (LBool b))   = Right (VBool b)
    go (ELit (LText t))   = Left ("文字列リテラル \"" <> t <> "\" は数式の中では使えません")
    go (EVar n) = case Map.lookup n env of
      Just v  -> Right v
      Nothing -> case Map.lookup n (builtinTable :: Map.Map Text (Int, [a] -> a)) of
        Just (ar, _) -> Right (VBuiltin n ar)
        Nothing
          -- Phase 43: list builtin (softmax 等、 VList→VList) はスカラ
          -- builtinTable に乗らないので別途 VBuiltin として解決する。
          | n `Set.member` listBuiltins -> Right (VBuiltin n 1)
          | otherwise -> Left ("未定義の変数です: " <> n)
    go (ECol c) = case mi of
      Nothing -> Left ("列参照 '" <> c <> "' は行ごとの評価文脈 (observe の per-row) でのみ使えます")
      Just i  -> case Map.lookup c dataMap of
        Just col -> case drop i (colDoubles col) of
          (x : _) -> Right (VNum (liftD x))
          []      -> Left ("列のインデックスが範囲外です: " <> c <> "[" <> T.pack (show i) <> "]")
        Nothing -> Left ("未知の列です: " <> c)
    go (ENeg e) = do v <- go e; x <- asNum v; Right (VNum (negate x))
    go (EOp op a b) = evalOp op a b
    go (EIf c a b) = do
      cv <- go c
      cond <- asBool cv
      if cond then go a else go b
    go (ELet bs body) = do
      env' <- foldEnv env bs
      evalValue env' dataMap mi body
    go (ELam x body) = Right (VClosure env [x] body)
    go appE@(EApp _ _) =
      let (h, args) = spine appE []
      in do
        hv <- go h
        argVs <- mapM go args
        applyValue hv argVs
    -- Phase 42: list リテラルを VList に評価。 多値分布 (Categorical /
    -- OrderedLogistic) の list 引数で使う。 要素は順次スカラ評価される。
    go (EList xs) = VList <$> mapM go xs
    go (EDo _ _) = Left "入れ子の do ブロックは未対応です"

    -- 適用の脊柱を平坦化: f x y → (f, [x, y])。
    spine :: Expr -> [Expr] -> (Expr, [Expr])
    spine (EApp f x) acc = spine f (x : acc)
    spine e acc = (e, acc)

    -- 値を引数列に適用 (一階・カリー化)。
    applyValue :: Value a -> [Value a] -> Err (Value a)
    applyValue v [] = Right v
    -- Phase 43: list builtin (softmax)。 VList を取り VList を返す (多項ロジット:
    -- Categorical (softmax [η₀, η₁, …]))。 スカラ builtinTable とは別経路。
    applyValue (VBuiltin "softmax" _) args = case args of
      [VList xs] -> do ns <- mapM asNum xs; VList . map VNum <$> softmaxList ns
      [_]        -> Left "softmax はリスト引数 ([..]) が必要です"
      _          -> Left ("softmax はリスト引数 1 個が必要ですが " <> T.pack (show (length args)) <> " 個でした")
    applyValue (VBuiltin name ar) args =
      case Map.lookup name (builtinTable :: Map.Map Text (Int, [a] -> a)) of
        Nothing -> Left ("内部エラー: 未知の組込関数 " <> name)
        Just (_, impl)
          | length args == ar -> do ns <- mapM asNum args; Right (VNum (impl ns))
          | length args <  ar -> Left ("組込関数 " <> name <> " は引数 "
              <> T.pack (show ar) <> " 個が必要ですが " <> T.pack (show (length args))
              <> " 個でした (部分適用は未対応)")
          | otherwise -> Left ("組込関数 " <> name <> " に引数が多すぎます (必要 "
              <> T.pack (show ar) <> " 個)")
    applyValue (VClosure cenv params body) args = applyClosure cenv params body args
    applyValue (VNum _) _  = Left "数値を関数として適用しています (関数ではない値に引数を渡しています)"
    applyValue (VBool _) _ = Left "真偽値を関数として適用しています"
    applyValue (VList _) _ = Left "リストを関数として適用しています"
    applyValue (VErr m) _  = Left m

    -- クロージャ適用: 引数を仮引数に順次束縛。 仮引数が尽きたら (= 完全
    -- 適用) 本体を評価して残り引数をさらに適用、 引数が尽きたが仮引数が
    -- 残るなら部分適用 (VClosure を返す)。 節順が重要: 完全適用
    -- (ps=[], args=[]) は本体評価を先に判定する。
    applyClosure :: EnvA a -> [Text] -> Expr -> [Value a] -> Err (Value a)
    applyClosure cenv [] body args = do
      r <- evalValue cenv dataMap mi body
      applyValue r args
    applyClosure cenv ps body [] = Right (VClosure cenv ps body)
    applyClosure cenv (p : ps) body (a : as) =
      applyClosure (Map.insert p a cenv) ps body as

    evalOp :: Text -> Expr -> Expr -> Err (Value a)
    evalOp op a b = case op of
      "+"  -> num2 (+)
      "-"  -> num2 (-)
      "*"  -> num2 (*)
      "/"  -> num2 (/)
      "**" -> num2 (**)
      "^"  -> num2 (**)   -- DSL では Double 冪 (整数冪に限定しない)
      "==" -> cmp (==)
      "/=" -> cmp (/=)
      "<"  -> cmp (<)
      "<=" -> cmp (<=)
      ">"  -> cmp (>)
      ">=" -> cmp (>=)
      "&&" -> bool2 (&&)
      "||" -> bool2 (||)
      _    -> Left ("モデル中で未対応の演算子です: " <> op)
      where
        num2 f  = do av <- asNum =<< go a; bv <- asNum =<< go b; Right (VNum (f av bv))
        cmp f   = do av <- asNum =<< go a; bv <- asNum =<< go b; Right (VBool (f av bv))
        bool2 f = do av <- asBool =<< go a; bv <- asBool =<< go b; Right (VBool (f av bv))

    foldEnv :: EnvA a -> [Bind] -> Err (EnvA a)
    foldEnv e [] = Right e
    foldEnv e (Bind n v : rest) = do
      vv <- evalValue e dataMap mi v
      foldEnv (Map.insert n vv e) rest

-- ===========================================================================
-- Phase 27 §F-3a: top-level 束縛環境
-- ===========================================================================

-- | 1 つの top-level 値/関数束縛 (= ユーザが model と並べて書く
-- `tmpvar = 1` / `linkfunc x = log x`)。 model (= do-block 束縛) は含まない。
data TopBind = TopBind
  { tbName   :: Text
  , tbParams :: [Text]
  , tbBody   :: Expr
  } deriving (Show)

-- | top-level 束縛から評価環境を組む。 値束縛 (引数なし) は env の中で
-- 遅延評価して 'Value' に (相互参照は laziness で解決、 再帰は無い前提)。
-- 関数束縛 (引数あり) は env を捕捉した 'VClosure' に。 評価失敗は 'VErr'
-- として運び、 実際に参照された時にエラーを出す。
buildTopEnv :: forall a. (Floating a, Ord a) => DataMap -> [TopBind] -> EnvA a
buildTopEnv dataMap binds = env
  where
    env :: EnvA a
    env = Map.fromList [ (tbName b, toVal b) | b <- binds ]
    toVal b
      | null (tbParams b) = case evalValue env dataMap Nothing (tbBody b) of
          Right v -> v
          Left e  -> VErr e
      | otherwise = VClosure env (tbParams b) (tbBody b)

-- ===========================================================================
-- Phase 27 §F-3c: GLMM plate (forEachGroup)
-- ===========================================================================
--
-- `forEachGroup "gcol" $ \g -> do { … }` は群列 gcol の distinct 値ごとに
-- 内部 do-block を展開する専用構文。 native ModelP の
-- `forM_ groups $ \j -> do { theta <- sample ("theta_"++show j) …; observe … }`
-- (= Phase37 demo randomSlope/multiLevel) を AST 経由で再現する。
--   * sample / observe 名は群値で suffix を付け、 群ごとに別 latent にする。
--   * observe は当該群の行のみを対象にする (= 行サブセット)。
--   * lambda 引数 g は群値 (Double) に束縛 (式で使える)。
--   * nest 可能 (forEachGroup の中に forEachGroup): suffix 連結 + 行積集合。

-- | plate 評価コンテキスト。 top-level は 'topCtx' = ("", 全行) で従来挙動を保つ。
data PlateCtx = PlateCtx
  { pcSuffix :: Text          -- sample/observe 名に付ける suffix (例 "_1_2")
  , pcRows   :: Maybe [Int]    -- observe 対象行 (Nothing = 全行)
  }

topCtx :: PlateCtx
topCtx = PlateCtx "" Nothing

-- | `forEachGroup "gcol" (\g -> do { … })` を検出し (群列, 引数名, 内部 stmts)。
matchForEachGroup :: Expr -> Maybe (Text, Text, [DoStmt])
matchForEachGroup e = case collectApp e of
  Right ("forEachGroup", [ELit (LText gcol), ELam param body]) ->
    Just (gcol, param, lamBodyToStmts body)
  _ -> Nothing

-- | lambda 本体を DoStmt 列に。 do なら stmts + 末尾式、 それ以外は単一 DoExpr。
lamBodyToStmts :: Expr -> [DoStmt]
lamBodyToStmts (EDo stmts ret) = stmts ++ retToStmts ret
lamBodyToStmts other           = [DoExpr other]

-- | do-block 末尾式: pure/return は捨て、 それ以外 (observe 等) は DoExpr に。
retToStmts :: Expr -> [DoStmt]
retToStmts r = case r of
  EApp (EVar "pure") _   -> []
  EApp (EVar "return") _ -> []
  ELit (LBool _)         -> []
  _                      -> [DoExpr r]

-- | ctx の対象行に限定した群列の distinct 値 (昇順)。
groupValsIn :: DataMap -> Text -> Maybe [Int] -> [Double]
groupValsIn dm gcol mrows =
  let col  = lookupDoubles gcol dm
      idxs = fromMaybe [0 .. length col - 1] mrows
  in sort (nub [ col !! i | i <- idxs, i >= 0, i < length col ])

-- | ctx の対象行のうち 群列 == gval の行 index。
rowsForGroup :: DataMap -> Text -> Double -> Maybe [Int] -> [Int]
rowsForGroup dm gcol gval mrows =
  let col  = lookupDoubles gcol dm
      idxs = fromMaybe [0 .. length col - 1] mrows
  in [ i | i <- idxs, i >= 0, i < length col, col !! i == gval ]

-- | 群値を name suffix に (整数なら "_3"、 非整数なら小数表記)。
-- 群列は整数コード前提なので通常は整数 suffix。
groupSuffix :: Double -> Text
groupSuffix g
  | g == fromIntegral (round g :: Integer) = "_" <> T.pack (show (round g :: Integer))
  | otherwise                              = "_" <> T.pack (show g)

-- | 群 suffix (Phase 41.4)。 群列が 'Factor' で code が level を指し、 その
-- level が安全な識別子 (先頭英字/下線 + 英数字/下線のみ) なら可読 suffix
-- "_<level>" (例 "_setosa")、 それ以外は 'groupSuffix' (数値 code suffix) に
-- フォールバック。 charset / 衝突安全のため不安全な level は code に落とす。
-- interpStmts / collectObsInstances / plateRenameMap の 3 経路で同一規律を使う
-- 必要がある (node 名が一致しないと観測値/グラフが噛み合わない)。
groupSuffixFor :: Maybe Column -> Double -> Text
groupSuffixFor (Just (Factor levels _)) g
  | i >= 0, i < length levels, isSafeIdent (levels !! i) = "_" <> (levels !! i)
  where i = round g :: Int
groupSuffixFor _ g = groupSuffix g

-- | node 名 suffix に使える安全な識別子か (先頭英字/下線、 以降英数字/下線)。
isSafeIdent :: Text -> Bool
isSafeIdent t = case T.uncons t of
  Nothing          -> False
  Just (c0, rest)  -> (isAlpha c0 || c0 == '_')
                        && T.all (\c -> isAlphaNum c || c == '_') rest

-- | Distribution AST を Hanalyze.Model.HBM.Distribution に変換。
-- mi が Nothing なら列参照不可。 mi=Just i なら行 i で評価。
evalDist
  :: forall a. (Floating a, Ord a)
  => EnvA a -> DataMap -> Maybe Int -> Expr -> Err (HBM.Distribution a)
evalDist env dataMap mi expr = do
  (name, args) <- collectApp expr
  case (name, args) of
    ("Normal",     [m, s])    -> mk2 HBM.Normal m s
    ("HalfNormal", [s])       -> mk1 HBM.HalfNormal s
    ("Beta",       [a, b])    -> mk2 HBM.Beta a b
    ("Gamma",      [s, r])    -> mk2 HBM.Gamma s r
    ("Exponential", [r])      -> mk1 HBM.Exponential r
    ("Poisson",    [l])       -> mk1 HBM.Poisson l
    ("Bernoulli",  [p])       -> mk1 HBM.Bernoulli p
    ("Uniform",    [l, h])    -> mk2 HBM.Uniform l h
    ("StudentT",   [df, m, s]) -> mk3 HBM.StudentT df m s
    ("Cauchy",     [l, s])    -> mk2 HBM.Cauchy l s
    ("HalfCauchy", [s])       -> mk1 HBM.HalfCauchy s
    ("LogNormal",  [m, s])    -> mk2 HBM.LogNormal m s
    -- Phase 42: 多値 categorical 応答。 list 引数は VList 経由でスカラ列に
    -- 評価する (observe は factor code 0..K-1)。 2 値応答は Bernoulli +
    -- factor code 0/1 で Phase 41.5 対応済。
    ("Categorical", [probs])  -> HBM.Categorical <$> evalList probs
    ("OrderedLogistic", [eta, cuts]) -> HBM.OrderedLogistic <$> eval eta <*> evalList cuts
    -- Phase 44: 多変量正規 (観測専用)。 mu = 平均ベクトル ([a])、 cov = full Σ
    -- (VList-of-VList → [[a]])。 observeMV 経由で k-vector を観測する。 cov の
    -- 正定値性は density 評価時 (choleskyL→-∞) 任せ、 ここでは正方性のみ検査。
    ("MvNormal", [mu, cov]) -> do
      muV  <- evalList mu
      covV <- evalMat cov
      let k = length muV
      when (length covV /= k)
        (Left ("MvNormal: 平均ベクトル長 " <> T.pack (show k)
               <> " と共分散の行数 " <> T.pack (show (length covV)) <> " が一致しません"))
      when (any ((/= k) . length) covV)
        (Left ("MvNormal: 共分散は " <> T.pack (show k) <> "×" <> T.pack (show k)
               <> " 正方行列である必要があります"))
      Right (HBM.MvNormal muV covV)
    -- Phase 44: scale vector σ + 相関 Cholesky L パラメタ化 (観測専用)。 L は
    -- lkjCorrCholesky bind 由来の VList-of-VList。 covariance = (diag σ·L)(diag σ·L)ᵀ。
    ("MvNormalChol", [mu, sigma, lExpr]) -> do
      muV    <- evalList mu
      sigmaV <- evalList sigma
      lV     <- evalMat lExpr
      let k = length muV
      when (length sigmaV /= k)
        (Left ("MvNormalChol: 平均ベクトル長 " <> T.pack (show k)
               <> " と scale ベクトル長 " <> T.pack (show (length sigmaV)) <> " が一致しません"))
      when (length lV /= k || any ((/= k) . length) lV)
        (Left ("MvNormalChol: 相関 Cholesky L は " <> T.pack (show k) <> "×"
               <> T.pack (show k) <> " 行列である必要があります"))
      Right (HBM.MvNormalChol muV sigmaV lV)
    -- Phase 45: 混合分布 (スカラ単一列、 観測は scalar observe)。 第 1 引数 =
    -- 重みベクトル ([a]、 literal `[0.3,0.7]` or dirichlet 由来 VList ref、 既存
    -- evalList で評価)、 第 2 引数 = 成分分布リスト (EList の各要素を **再帰
    -- evalDist** = `[Distribution a]`)。 component 数 K は EList 要素数で静的決定。
    -- MvNormal (Phase 44) と異なり multi-column ではない (logDensity はスカラ x)。
    ("Mixture", [weights, EList distExprs]) -> do
      ws    <- evalList weights
      comps <- mapM (evalDist env dataMap mi) distExprs
      when (null comps)
        (Left "Mixture: 成分分布が空です (第 2 引数に少なくとも 1 つの分布が必要)")
      when (length ws /= length comps)
        (Left ("Mixture: 重み数 " <> T.pack (show (length ws))
               <> " と成分分布数 " <> T.pack (show (length comps)) <> " が一致しません"))
      Right (HBM.Mixture ws comps)
    _ -> Left ("Unsupported distribution: " <> name <> " with " <> T.pack (show (length args)) <> " args")
  where
    eval = evalScalar env dataMap mi
    -- list 引数 ([a]): EList を VList に評価 → 各要素をスカラに。
    evalList e = evalValue env dataMap mi e >>= asList >>= mapM asNum
    -- 行列引数 ([[a]]): VList-of-VList に評価 → 'asMatrix' で正方性検査。
    evalMat e = evalValue env dataMap mi e >>= asMatrix
    mk1 f a       = f <$> eval a
    mk2 f a b     = f <$> eval a <*> eval b
    mk3 f a b c   = f <$> eval a <*> eval b <*> eval c

-- | k-vector 観測を取る多変量分布か。 @observeMV@ (Phase 44) はこれらのみ
-- 受理し、 scalar 分布が渡されたら親切エラーにする。 obsLogSum
-- (HBM.hs:987) が chunk 処理する分布と対応する。
isMultivariateDist :: HBM.Distribution a -> Bool
isMultivariateDist d = HBM.distName d `elem`
  [ "MvNormal", "MvNormalChol", "MvStudentT"
  , "Multinomial", "DirichletMultinomial", "Wishart" ]

-- 'collectApp' は Hanalyze.Model.HBM.Ast から import。

-- ===========================================================================
-- Phase 43: list 値 Model combinator (latent vector を返す DoBind)
-- ===========================================================================
--
-- 現 'DoBind' は scalar @sample@ 専用 (= @x <- Dist …@ で 1 値)。 だが
-- @cuts <- orderedCuts "cut" 2 (-2) 1@ や @probs <- dirichlet "pi" [1,1,1]@ の
-- ように **latent vector を返す** Model combinator (HBM.orderedCuts /
-- HBM.dirichlet、 いずれも @Model a [a]@) は scalar に乗らない。 これらは
-- 'evalDist' (= Distribution を返す) とは別経路で、 DoBind の RHS を
-- 'matchListComb' で検出し、 Model モナドで実行して 'VList' に束縛する。
--
-- 消費側 (@OrderedLogistic eta cuts@ / @Categorical probs@) は env 内で cuts /
-- probs が VList に束縛されるので Phase 42 の evalList (evalValue >>= asList >>=
-- mapM asNum) がそのまま解決する (本機構の追加は **bind 側のみ**)。

-- | 検出した list 値 combinator 呼び出し (引数は評価済 = base 名 + 構築情報)。
-- 結果ベクトルの長さは引数から静的に決まる ('listCombLen')。
data ListComb a
  = OrderedCutsComb Text Int a a   -- ^ name, nCuts (= K-1 ≥ 1), cMin, HalfNormal scale
  | DirichletComb   Text [a]       -- ^ name, α 集中度ベクトル (長さ K ≥ 2)

-- | combinator が返すベクトルの長さ (validateStmts の placeholder VList 長 /
-- interpStmts は実値から決まるので参照不要)。
listCombLen :: ListComb a -> Int
listCombLen (OrderedCutsComb _ n _ _) = n
listCombLen (DirichletComb _ as)      = length as

-- | base 名に plate suffix を付ける (forEachGroup 内で群ごとに別 latent にする)。
listCombSuffix :: Text -> ListComb a -> ListComb a
listCombSuffix suf (OrderedCutsComb nm n cm sc) = OrderedCutsComb (nm <> suf) n cm sc
listCombSuffix suf (DirichletComb nm as)        = DirichletComb (nm <> suf) as

-- | DoBind の RHS が list 値 combinator (orderedCuts / dirichlet) なら
-- 引数を評価して 'ListComb' に。 combinator でなければ 'Nothing' (= scalar
-- sample 経路へ)。 名前は文字列リテラル必須、 nCuts は数値リテラル必須
-- (静的に長さを決めるため。 Phase 43 当面の制約、 doc 想定リスク参照)。
matchListComb
  :: forall a. (Floating a, Ord a)
  => EnvA a -> DataMap -> Expr -> Maybe (Err (ListComb a))
matchListComb env dataMap expr = case collectApp expr of
  Right ("orderedCuts", [nameE, nCutsE, cMinE, scaleE]) -> Just $ do
    nm <- textLit "orderedCuts" nameE
    n  <- intLit  "orderedCuts" nCutsE
    when (n < 1) (Left "orderedCuts: カット数 (第 2 引数) は 1 以上である必要があります")
    cm <- evalScalar env dataMap Nothing cMinE
    sc <- evalScalar env dataMap Nothing scaleE
    Right (OrderedCutsComb nm n cm sc)
  Right ("dirichlet", [nameE, alphasE]) -> Just $ do
    nm <- textLit "dirichlet" nameE
    as <- evalValue env dataMap Nothing alphasE >>= asList >>= mapM asNum
    when (length as < 2) (Left "dirichlet: α ベクトル (第 2 引数) は長さ 2 以上の [..] である必要があります")
    Right (DirichletComb nm as)
  _ -> Nothing
  where
    textLit _ (ELit (LText t)) = Right t
    textLit fn _ = Left (fn <> " の名前引数 (第 1 引数) は文字列リテラルである必要があります")
    intLit :: Text -> Expr -> Err Int
    intLit _ (ELit (LNumber d)) = Right (round d)
    intLit fn _ = Left (fn <> " のカット数引数 (第 2 引数) は数値リテラルである必要があります (変数経由は未対応)")

-- | 'ListComb' を実際の Model アクション (latent vector を sample) に。
runListComb :: forall a. (Floating a, Ord a) => ListComb a -> HBM.Model a [a]
runListComb (OrderedCutsComb nm n cm sc) = HBM.orderedCuts nm n cm sc
runListComb (DirichletComb nm as)        = HBM.dirichlet nm as

-- ===========================================================================
-- Phase 44: 行列値 Model combinator (latent 相関行列を返す DoBind)
-- ===========================================================================
--
-- 'ListComb' (Phase 43、 @Model a [a]@) の行列版。 @lkjCorrCholesky@ は
-- @Model a [[a]]@ で k×k 下三角の相関 Cholesky 因子 L を返す latent
-- combinator。 @L <- lkjCorrCholesky "L" 2 2.0@ を 'VList'-of-'VList' に束縛し、
-- 消費側 ('MvNormalChol' の第 3 引数) は env 内の VList-of-VList を 'asMatrix'
-- で解決する。 内部 latent (@L_pc*@ / @L_L*@ 等) は Model が自動登録する
-- (DSL は latent を再実装しない)。

-- | 検出した行列値 combinator 呼び出し (引数評価済)。
data MatrixComb a
  = LkjCholComb Text Int a   -- ^ name, dim k (≥ 2), eta (LKJ 集中度)

-- | combinator が返す行列の次元 k (validateStmts の placeholder 用)。
matrixCombDim :: MatrixComb a -> Int
matrixCombDim (LkjCholComb _ k _) = k

-- | base 名に plate suffix を付ける (群ごとに別 latent にする)。
matrixCombSuffix :: Text -> MatrixComb a -> MatrixComb a
matrixCombSuffix suf (LkjCholComb nm k eta) = LkjCholComb (nm <> suf) k eta

-- | DoBind の RHS が行列値 combinator (lkjCorrCholesky) なら引数を評価して
-- 'MatrixComb' に。 combinator でなければ 'Nothing'。 名前は文字列リテラル、
-- 次元 k は数値リテラル必須 (静的に行列サイズを決めるため)。
matchMatrixComb
  :: forall a. (Floating a, Ord a)
  => EnvA a -> DataMap -> Expr -> Maybe (Err (MatrixComb a))
matchMatrixComb env dataMap expr = case collectApp expr of
  Right ("lkjCorrCholesky", [nameE, kE, etaE]) -> Just $ do
    nm  <- textLit nameE
    k   <- intLit  kE
    when (k < 2) (Left "lkjCorrCholesky: 次元 (第 2 引数) は 2 以上である必要があります")
    eta <- evalScalar env dataMap Nothing etaE
    Right (LkjCholComb nm k eta)
  _ -> Nothing
  where
    textLit (ELit (LText t)) = Right t
    textLit _ = Left "lkjCorrCholesky の名前引数 (第 1 引数) は文字列リテラルである必要があります"
    intLit :: Expr -> Err Int
    intLit (ELit (LNumber d)) = Right (round d)
    intLit _ = Left "lkjCorrCholesky の次元引数 (第 2 引数) は数値リテラルである必要があります (変数経由は未対応)"

-- | 'MatrixComb' を実際の Model アクション (latent 相関行列を sample) に。
runMatrixComb :: forall a. (Floating a, Ord a) => MatrixComb a -> HBM.Model a [[a]]
runMatrixComb (LkjCholComb nm k eta) = HBM.lkjCorrCholesky nm k eta

-- | Phase 9.1d-5: stmts を walk して各 latent 変数(DoBind の左辺)に対する
-- Transform を返す。 constrained 空間で 0 初期化は PositiveT で log 0 = -∞
-- 発散するため、 streaming endpoint で transform 別に初期値を選ぶのに使う。
inferTransforms :: [DoStmt] -> Map.Map Text HD.Transform
inferTransforms = Map.fromList . concatMap extract
  where
    extract (DoBind name distExpr) = case collectApp distExpr of
      Right (dname, _) -> [(name, distNameToTransform dname)]
      _ -> [(name, HD.UnconstrainedT)]
    extract _ = []

    distNameToTransform "Normal"       = HD.UnconstrainedT
    distNameToTransform "StudentT"     = HD.UnconstrainedT
    distNameToTransform "Cauchy"       = HD.UnconstrainedT
    distNameToTransform "Uniform"      = HD.UnconstrainedT
    distNameToTransform "HalfNormal"   = HD.PositiveT
    distNameToTransform "HalfCauchy"   = HD.PositiveT
    distNameToTransform "Gamma"        = HD.PositiveT
    distNameToTransform "Exponential"  = HD.PositiveT
    distNameToTransform "LogNormal"    = HD.PositiveT
    distNameToTransform "InverseGamma" = HD.PositiveT
    distNameToTransform "Weibull"      = HD.PositiveT
    distNameToTransform "Beta"         = HD.UnitIntervalT
    distNameToTransform "Bernoulli"    = HD.UnitIntervalT
    distNameToTransform _              = HD.UnconstrainedT

-- ===========================================================================
-- AST → Model モナド構築
-- ===========================================================================

-- | EDo 内の各 stmt を Model モナドに翻訳。実装は 'forall a' のもとに
-- 動作する必要があるが、Err は Haskell の純粋値なので外側で先に検査して
-- Model 構築は失敗しない前提にする。エラーは事前検証で全部捕まえる方針。
--
-- ここでは「事前検証ありで、検証通過後に Model を直接組み上げる」設計。
-- ModelP は forall を含む rank-1 polymorphic 型なので Either に直接乗らない
-- (ImpredicativeTypes を避けるため)。validate と build を分離する。
validateAst :: [TopBind] -> Expr -> DataMap -> Either Text [DoStmt]
validateAst topBinds body0 dataMap = do
  rawStmts <- case body0 of
    -- Phase 13 §9.3c-2: frontend parser は最終 DoExpr を ret として分離する
    -- (hanalyze 慣行で「observe が最後 + pure 省略」 が許される)。 ret が
    -- pure / return のときは捨て、 それ以外(例: observe)は body 末尾に
    -- DoExpr として戻して扱う。
    EDo s ret ->
      let isDiscard = case ret of
            EApp (EVar "pure") _ -> True
            EApp (EVar "return") _ -> True
            ELit (LBool _) -> True   -- frontend の implicit ret fallback
            _ -> False
          full = if isDiscard then s else s ++ [DoExpr ret]
      in Right full
    _ -> Left "Model body must be a do-block (`do { ... }`)"
  -- Phase 26.1 §A-2 (2026-05-27): `x <- Data "label" expr` を pre-process で
  -- substitute (= 列 alias 経路)。 詳細は streaming bridge/.../HbmAst.hs の同名
  -- 関数 doc 参照。 hanalyze の pm.Data 厳密対応 (withData 経由) は将来 phase。
  let stmts = preprocessAliases rawStmts
  validateStmts topBinds dataMap stmts
  pure stmts

-- | Phase 26.1 §A-2 alias 経路 (2026-05-27 王道方針に切替): Haskell の `let`
-- を syntactic alias として扱う pre-processing。 spec §4.2 と整合
-- (= ∀LIC∃Code は Haskell サブセット、 `let x = col "..."` で Expression
-- Language alias)。 詳細は streaming bridge/.../HbmAst.hs の同名関数 doc 参照。
preprocessAliases :: [DoStmt] -> [DoStmt]
preprocessAliases = go Map.empty
  where
    go _ [] = []
    go aliases (s : rest) = case extractLetAliases aliases s of
      Just (newAliases, mStmt) -> case mStmt of
        Nothing   -> go newAliases rest
        Just stmt -> stmt : go newAliases rest
      Nothing ->
        substituteStmt aliases s : go aliases rest

    extractLetAliases
      :: Map.Map Text Expr -> DoStmt
      -> Maybe (Map.Map Text Expr, Maybe DoStmt)
    extractLetAliases aliases (DoLet bs) =
      let (newAliases, kept) = foldl step (aliases, []) bs
          step (al, acc) (Bind n v) =
            let substV = substitute al v
            in if hasColRef substV
                 then (Map.insert n substV al, acc)
                 else (al, acc ++ [Bind n substV])
      in Just (newAliases, if null kept then Nothing else Just (DoLet kept))
    extractLetAliases _ _ = Nothing

    substituteStmt :: Map.Map Text Expr -> DoStmt -> DoStmt
    substituteStmt aliases (DoBind n v) = DoBind n (substitute aliases v)
    substituteStmt aliases (DoLet bs)   = DoLet (map (substBind aliases) bs)
    substituteStmt aliases (DoExpr e)   = DoExpr (substitute aliases e)

    substBind :: Map.Map Text Expr -> Bind -> Bind
    substBind aliases (Bind n v) = Bind n (substitute aliases v)

    substitute :: Map.Map Text Expr -> Expr -> Expr
    substitute env = go'
      where
        go' (EVar n) = case Map.lookup n env of
          Just e  -> e
          Nothing -> EVar n
        go' (EOp op a b) = EOp op (go' a) (go' b)
        go' (EApp f x)   = EApp (go' f) (go' x)
        go' (ENeg e)     = ENeg (go' e)
        go' (ELet bs body) = ELet (map (substBind env) bs) (go' body)
        go' (EList xs)   = EList (map go' xs)
        go' (EIf c a b)  = EIf (go' c) (go' a) (go' b)
        go' (ELam x b)   = ELam x (go' b)
        go' (EDo s r)    = EDo s r   -- 入れ子 do は触らない
        go' x            = x

-- | 静的検証(変数名スコープ / 列存在)。型は (Double, Double) 環境で一度評価して
-- 実行時エラーが起きないかを確認する。
validateStmts :: [TopBind] -> DataMap -> [DoStmt] -> Err ()
validateStmts topBinds dataMap stmts0 = go (buildTopEnv dataMap topBinds) stmts0
  where
    go :: EnvA Double -> [DoStmt] -> Err ()
    go _env [] = Right ()
    -- Phase 43: RHS が list 値 combinator (orderedCuts / dirichlet) なら、
    -- 構造検証 + 結果長 K の placeholder VList を束縛する (Model モナドが無い
    -- 検証経路では実行できないため。 後続 Categorical/OrderedLogistic の検証が
    -- 通るように長さだけ合わせる)。
    go env (DoBind name distExpr : rest)
      | Just ecomb <- matchListComb env dataMap distExpr :: Maybe (Err (ListComb Double)) = do
          comb <- ecomb
          let k = listCombLen comb
          go (Map.insert name (VList (replicate k (VNum 0.0))) env) rest
    -- Phase 44: 行列値 combinator は k×k placeholder VList-of-VList を束縛する
    -- (Model モナドが無い検証経路では実行できないため、 次元だけ合わせる)。
    go env (DoBind name distExpr : rest)
      | Just ecomb <- matchMatrixComb env dataMap distExpr :: Maybe (Err (MatrixComb Double)) = do
          comb <- ecomb
          let k = matrixCombDim comb
          go (Map.insert name (VList (replicate k (VList (replicate k (VNum 0.0))))) env) rest
    go env (DoBind name distExpr : rest) = do
      _ <- if hasColRef distExpr
             then Left ("sample distribution cannot reference data column: " <> name)
             else evalDist env dataMap Nothing distExpr :: Err (HBM.Distribution Double)
      go (Map.insert name (VNum 0.0) env) rest
    go env (DoLet bs : rest) = do
      env' <- foldEnv env bs
      go env' rest
    go env (DoExpr e : rest)
      -- Phase 27 §F-3c: forEachGroup は群列の存在を確認し、 内部 stmts を
      -- 代表コンテキスト (param = 0) で検証する (行サブセットは検証不要)。
      | Just (gcol, param, inner) <- matchForEachGroup e = do
          when (Map.notMember gcol dataMap)
            (Left ("forEachGroup の群列が見つかりません: " <> gcol))
          go (Map.insert param (VNum 0) env) inner
          go env rest
      | otherwise = do
          -- observe の構文形を検査
          validateObserve env e
          go env rest

    foldEnv e [] = Right e
    foldEnv e (Bind n v : rest) = do
      vv <- evalValue e dataMap Nothing v
      foldEnv (Map.insert n vv e) rest

    validateObserve env e = do
      (fname, args) <- collectApp e
      case (fname, args) of
        ("observe", [ELit (LText _obsName), distExpr, dataRef]) -> do
          colName <- requireColRef dataRef
          when (Map.notMember colName dataMap) (Left ("Unknown column in observe: " <> colName))
          if hasColRef distExpr
            then do
              -- 列参照あり: 各行で評価できることを確認(row 0 でテスト)
              _ <- evalDist env dataMap (Just 0) distExpr :: Err (HBM.Distribution Double)
              pure ()
            else do
              _ <- evalDist env dataMap Nothing distExpr :: Err (HBM.Distribution Double)
              pure ()
        -- Phase 44: multi-column observe。 第 3 引数は観測列リスト ['y1','y2',..]。
        -- 全列の存在 + 列数 ≥ 2 + 列長一致 + dist が多変量かを検査する。 dist の
        -- μ/cov は行不変前提なので row 参照不可 (Nothing) で評価する。
        ("observeMV", [ELit (LText _obsName), distExpr, EList colRefs]) -> do
          cols <- mapM requireColRef colRefs
          when (length cols < 2)
            (Left "observeMV: 観測列は 2 列以上必要です (1 列なら scalar observe を使ってください)")
          forM_ cols $ \c ->
            when (Map.notMember c dataMap) (Left ("Unknown column in observeMV: " <> c))
          let lens = map (length . (`lookupDoubles` dataMap)) cols
          when (any (/= head lens) (tail lens))
            (Left "observeMV: 観測列の長さが揃っていません (k-vector 組成には全列同長が必要です)")
          d <- evalDist env dataMap Nothing distExpr :: Err (HBM.Distribution Double)
          when (not (isMultivariateDist d))
            (Left ("observeMV: 第 2 引数は多変量分布 (MvNormal 等) が必要ですが、 scalar 分布 "
                   <> HBM.distName d <> " が渡されました"))
        ("observeMV", _) ->
          Left "observeMV: 第 3 引数は観測列リスト ['y1','y2',..] が必要です"
        ("pure", _) -> Right ()   -- pure x は最終行用
        ("return", _) -> Right ()
        _ -> Left ("Unsupported statement: " <> fname)

    requireColRef (ECol n) = Right n
    requireColRef _ = Left "observe's third argument must be a column reference 'colname'"

-- | 検証通過後の Model 構築(polymorphic in a)。エラーは想定外なので
-- error で落とす(validation で漏れたバグは fail-fast)。
interpStmts :: [TopBind] -> DataMap -> [DoStmt] -> HBM.ModelP ()
interpStmts topBinds dataMap stmts = goM topCtx (buildTopEnv dataMap topBinds) stmts
  where
    goM :: forall a. (Floating a, Ord a) => PlateCtx -> EnvA a -> [DoStmt] -> HBM.Model a ()
    goM _ _env [] = pure ()
    -- Phase 43: list 値 combinator (orderedCuts / dirichlet) は scalar sample
    -- ではなく Model アクションを実行して latent vector を VList 束縛する。
    -- base 名に plate suffix を付け、 forEachGroup 内で群ごとに別 latent にする。
    goM ctx env (DoBind name distExpr : rest)
      | Just ecomb <- matchListComb env dataMap distExpr = do
          let comb = case ecomb of
                Right c -> listCombSuffix (pcSuffix ctx) c
                Left e  -> error (T.unpack e)
          xs <- runListComb comb
          goM ctx (Map.insert name (VList (map VNum xs)) env) rest
    -- Phase 44: 行列値 combinator (lkjCorrCholesky) は latent 相関 Cholesky 因子を
    -- Model で実行し、 VList-of-VList に束縛する (MvNormalChol の L 引数で消費)。
    goM ctx env (DoBind name distExpr : rest)
      | Just ecomb <- matchMatrixComb env dataMap distExpr = do
          let comb = case ecomb of
                Right c -> matrixCombSuffix (pcSuffix ctx) c
                Left e  -> error (T.unpack e)
          m <- runMatrixComb comb
          goM ctx (Map.insert name (VList (map (VList . map VNum) m)) env) rest
    goM ctx env (DoBind name distExpr : rest) = do
      let dist = case evalDist env dataMap Nothing distExpr of
            Right d -> d
            Left e -> error (T.unpack e)
      x <- HBM.sample (name <> pcSuffix ctx) dist
      goM ctx (Map.insert name (VNum x) env) rest
    goM ctx env (DoLet bs : rest) = do
      let env' = foldlBinds env bs
      goM ctx env' rest
    goM ctx env (DoExpr e : rest)
      -- Phase 27 §F-3c: forEachGroup は群ごとに内部 do-block を展開する。
      | Just (gcol, param, inner) <- matchForEachGroup e = do
          let gvals = groupValsIn dataMap gcol (pcRows ctx)
          forM_ gvals $ \gval -> do
            let ctx' = ctx
                  { pcSuffix = pcSuffix ctx <> groupSuffixFor (Map.lookup gcol dataMap) gval
                  , pcRows   = Just (rowsForGroup dataMap gcol gval (pcRows ctx))
                  }
                env' = Map.insert param (VNum (liftD gval)) env
            goM ctx' env' inner
          goM ctx env rest
      | otherwise = do
          execObserve ctx env e
          goM ctx env rest

    foldlBinds :: forall a. (Floating a, Ord a) => EnvA a -> [Bind] -> EnvA a
    foldlBinds e [] = e
    foldlBinds e (Bind n v : rs) =
      let vv = case evalValue e dataMap Nothing v of
            Right x  -> x
            Left err -> VErr err  -- 検証通過しているはずなので通常来ない
      in foldlBinds (Map.insert n vv e) rs

    execObserve :: forall a. (Floating a, Ord a) => PlateCtx -> EnvA a -> Expr -> HBM.Model a ()
    execObserve ctx env e = case collectApp e of
      Right ("observe", [ELit (LText obsName), distExpr, ECol colName]) ->
        let ys      = lookupDoubles colName dataMap
            allRows = [0 .. length ys - 1]
            rows    = fromMaybe allRows (pcRows ctx)  -- 群コンテキストなら当該群の行
            nm      = obsName <> pcSuffix ctx
        in if hasColRef distExpr
             then do
               -- per-row distribution。 対象行のみ observeColumns でまとめる。
               let pairs = [ (case evalDist env dataMap (Just i) distExpr of
                                Right d -> d
                                Left _ -> error "validation should have caught this"
                             , [ys !! i])
                           | i <- rows, i >= 0, i < length ys
                           ]
               HBM.observeColumns nm pairs
             else do
               let dist = case evalDist env dataMap Nothing distExpr of
                     Right d -> d
                     Left _ -> error "validation should have caught this"
               HBM.observe nm dist [ ys !! i | i <- rows, i >= 0, i < length ys ]
      -- Phase 44: multi-column observe。 dist (μ/cov) は行不変なので 1 回だけ
      -- 評価し、 観測列だけを行ごとに k-vector に組んで HBM.observeMV に流す。
      Right ("observeMV", [ELit (LText obsName), distExpr, EList colRefs]) ->
        let cols    = [ c | ECol c <- colRefs ]
            colVecs = map (`lookupDoubles` dataMap) cols
            n       = if null colVecs then 0 else minimum (map length colVecs)
            allRows = [0 .. n - 1]
            rows    = fromMaybe allRows (pcRows ctx)
            nm      = obsName <> pcSuffix ctx
            dist    = case evalDist env dataMap Nothing distExpr of
                        Right d -> d
                        Left _  -> error "validation should have caught this"
            obss    = [ [ cv !! i | cv <- colVecs ] | i <- rows, i >= 0, i < n ]
        in HBM.observeMV nm dist obss
      Right ("pure", _) -> pure ()
      Right ("return", _) -> pure ()
      _ -> pure ()  -- validateObserve で弾く想定

-- ===========================================================================
-- NUTS 設定 reader
-- ===========================================================================

-- | extra から NUTS 設定を読む(欠落時はデフォルト)。
readChainCount :: A.Object -> Int
readChainCount o = case KM.lookup (Key.fromText "hbmChains") o of
  Just (A.Number n) -> let v = floor (realToFrac n :: Double) in max 1 (min 16 v)
  _ -> 4

readNutsConfig :: A.Object -> NUTS.NUTSConfig
readNutsConfig o =
  let
    def = NUTS.defaultNUTSConfig
    getInt :: Text -> Int
    getInt k = case KM.lookup (Key.fromText k) o of
      Just (A.Number n) -> floor (realToFrac n :: Double)
      _ -> 0
    getDbl :: Text -> Double
    getDbl k = case KM.lookup (Key.fromText k) o of
      Just (A.Number n) -> realToFrac n
      _ -> 0
    getBool' k = case KM.lookup (Key.fromText k) o of
      Just (A.Bool b) -> b
      _ -> False
  in def
    { NUTS.nutsIterations    = if getInt "hbmIterations" > 0 then getInt "hbmIterations" else NUTS.nutsIterations def
    , NUTS.nutsBurnIn        = max 0 (getInt "hbmBurnIn")
    , NUTS.nutsStepSize      = if getDbl "hbmStepSize" > 0 then getDbl "hbmStepSize" else NUTS.nutsStepSize def
    , NUTS.nutsMaxDepth      = if getInt "hbmMaxDepth" > 0 then getInt "hbmMaxDepth" else NUTS.nutsMaxDepth def
    , NUTS.nutsAdaptStepSize = getBool' "hbmAdaptStepSize"
    , NUTS.nutsTargetAccept  = let v = getDbl "hbmTargetAccept" in if v > 0 && v < 1 then v else NUTS.nutsTargetAccept def
    , NUTS.nutsAdaptMass     = getBool' "hbmAdaptMass"
    }

-- | Phase 13 §9.3c-2: observe ノード名 → 観測列名 のマッピングを取り出す。
-- DSL 構文 @observe "NAME" DIST 'COL'@ から (NAME, COL) を抽出。
-- frontend で「observe ノード "y" の観測値はどの列か」 を解決する用途。
observeNodeMap :: [DoStmt] -> [(Text, Text)]
observeNodeMap = concatMap step
  where
    step (DoExpr e) = case collectApp e of
      Right ("observe", [ELit (LText obsName), _distExpr, ECol colName]) ->
        [(obsName, colName)]
      _ -> []
    step _ = []

-- ===========================================================================
-- 結果整形 (param summary / posterior mean curves)
-- ===========================================================================

data ParamSummary = ParamSummary
  { psName :: !Text
  , psMean :: !Double
  , psSd   :: !Double
  , psLow  :: !Double
  , psHigh :: !Double
  , psRhat :: !(Maybe Double)
  , psEss  :: !Double
  } deriving (Show)

-- | SC29: 複数 chain から事後統計を計算。R̂ は split-R̂ (hanalyze)、
-- ESS は Geyer initial monotone(全チェーン pool)。
paramSummaryMulti :: [MC.Chain] -> Text -> ParamSummary
paramSummaryMulti chains name =
  let perChain = map (MC.chainVals name) chains
      pooled = concat perChain
      n = length pooled
      m = if n == 0 then 0 else sum pooled / fromIntegral n
      sd2 = if n < 2 then 0
            else sum (map (\v -> (v - m) ** 2) pooled) / fromIntegral (n - 1)
      sd = sqrt sd2
      sorted = LA.toList (LA.sortVector (LA.fromList pooled))
      pct :: Double -> Double
      pct q = if n == 0 then 0
              else let idx = max 0 (min (n - 1) (floor (q * fromIntegral (n - 1)) :: Int))
                   in case drop idx sorted of (x:_) -> x; [] -> 0
      rh = SMC.rhat perChain
      e  = SMC.ess pooled
  in ParamSummary name m sd (pct 0.025) (pct 0.975) rh e

fmtSummary :: ParamSummary -> Text
fmtSummary p = psName p <> "="
  <> T.pack (show (round4 (psMean p)))
  <> "±" <> T.pack (show (round4 (psSd p)))

round4 :: Double -> Double
round4 x = fromIntegral (round (x * 10000) :: Int) / 10000

-- | thinning: stride 飛ばしに要素を取る。
takeEvery :: Int -> [a] -> [a]
takeEvery _ []     = []
takeEvery n (x:xs) = x : takeEvery n (drop (max 0 (n - 1)) xs)

summaryToJson :: ParamSummary -> A.Value
summaryToJson p = A.object
  [ Key.fromText "name" A..= psName p
  , Key.fromText "mean" A..= psMean p
  , Key.fromText "sd"   A..= psSd p
  , Key.fromText "ci2_5" A..= psLow p
  , Key.fromText "ci97_5" A..= psHigh p
  , Key.fromText "rhat"  A..= psRhat p
  , Key.fromText "ess"   A..= psEss p
  ]

-- ---------------------------------------------------------------------------
-- Phase NN §A (2026-05-27): HBM posterior predictive mean curves
-- ---------------------------------------------------------------------------
-- DSL の observe 文の mean expression (= 例: `alpha + beta * 'x'`) を
-- 直接評価して posterior predictive curve を計算する。 frontend の
-- buildHbmOverlay 内 heuristic (= "alpha" / "beta_<x>" 等の名前推定) を
-- 撤去するための backend AST driven 経路。

data HbmMeanCurve = HbmMeanCurve
  { hmcObsName   :: !Text          -- observe 文の名前 (top="y"、 per-group="y_1")
  , hmcPredictor :: !Text          -- 説明変数列名 (= 例: "x")
  , hmcX         :: ![Double]      -- 64 grid points
  , hmcMedian    :: ![Double]      -- posterior median of mu(x)
  , hmcLower     :: ![Double]      -- 2.5%
  , hmcUpper     :: ![Double]      -- 97.5%
  -- Phase 27 GLMM overlay: per-group curve の群ラベル (top-level は Nothing)。
  -- frontend が群ごと色分け + 凡例に使う。
  , hmcGroupCol  :: !(Maybe Text)   -- 群列名 (= forEachGroup "gcol")
  , hmcGroupVal  :: !(Maybe Double) -- 群値
  } deriving (Show)

hbmMeanCurveToJson :: HbmMeanCurve -> A.Value
hbmMeanCurveToJson c = A.object
  [ Key.fromText "obsName"   A..= hmcObsName c
  , Key.fromText "predictor" A..= hmcPredictor c
  , Key.fromText "x"         A..= hmcX c
  , Key.fromText "median"    A..= hmcMedian c
  , Key.fromText "lower"     A..= hmcLower c
  , Key.fromText "upper"     A..= hmcUpper c
  , Key.fromText "groupCol"  A..= hmcGroupCol c
  , Key.fromText "groupVal"  A..= hmcGroupVal c
  ]

-- | stmts から各 observe の mean 式 (= Distribution の第 1 引数) を抽出。
extractObserveMeans :: [DoStmt] -> [(Text, Expr)]
extractObserveMeans = concatMap go
  where
    go (DoExpr e) = case collectApp e of
      Right ("observe", [ELit (LText nm), distExpr, _]) ->
        case collectApp distExpr of
          Right (_, m : _) -> [(nm, m)]
          _ -> []
      _ -> []
    go _ = []

-- | 式中の全 ECol 列名を集める (重複排除)。
collectCols :: Expr -> [Text]
collectCols = nub . go
  where
    go (ECol c)    = [c]
    go (EOp _ a b) = go a <> go b
    go (EApp f x)  = go f <> go x
    go (ENeg e)    = go e
    go (ELet bs body) = concatMap (\(Bind _ v) -> go v) bs <> go body
    go _           = []

-- | percentile (= 0..1) を sorted リストから線形補間で取る。
percentileOf :: Double -> [Double] -> Double
percentileOf q xs0 =
  let xs = sort xs0
      n = length xs
  in if n == 0 then 0
     else
       let idxF = q * fromIntegral (n - 1)
           idx  = max 0 (min (n - 1) (floor idxF))
       in case drop idx xs of
            (v:_) -> v
            []    -> 0

-- | observe 1 個分の mean-curve 計算文脈 (top-level または per-group)。
-- Phase 27 GLMM overlay: `forEachGroup` 内の observe も拾えるよう、 plate
-- 展開を replay しながら集める ('collectObsInstances')。
-- | Phase 43: WAIC/PPC 再評価用、 list 値 combinator 由来の latent vector を
-- posterior サンプルから再構築する仕様。 stream worker は @sampleNames@
-- (= sample された latent のみ) を samples に載せ、 combinator の deterministic
-- (cut_c_*/pi_*) は載せないため、 再評価 env で sampled latent (cut_d_*/pi_b*) から
-- VList を組み直す ('reconstructComb')。
data ListCombSpec
  = OCutsSpec Text Int Expr   -- ^ suffix 付き base 名, nCuts, cMin 式 (定数想定)
  | DirSpec   Text Int        -- ^ suffix 付き base 名, K (= α ベクトル長)

data ObsInstance = ObsInstance
  { oiObsName  :: Text                  -- 表示名 (top="y"、 group="y_1")
  , oiMeanExpr :: Expr                  -- distribution の第 1 引数 (mean 式)
  , oiDistExpr :: Expr                  -- distribution 式全体 (例: `Normal mu sigma`)
  , oiObsCol   :: Text                  -- 観測列名 (observe の第 3 引数 `col "y"`)
  , oiNameKey  :: Map.Map Text Text     -- local latent 名 → samples のキー (theta→"theta_1")
  , oiParamEnv :: Map.Map Text Double   -- 群変数 (forEachGroup の \g) → 群値
  , oiRows     :: Maybe [Int]           -- 対象行 (Nothing=全行)
  , oiGroup    :: Maybe (Text, Double)  -- 最内 forEachGroup の (群列, 群値)
  , oiListCombs :: [(Text, ListCombSpec)]  -- Phase 43: list latent bind (cuts/probs) の再構築仕様
  }

-- | list 値 combinator の latent vector を 1 posterior サンプルから再構築する。
-- @base@ は cMin 定数評価用の env (sample 非依存)、 @sm@ は sampled latent の Map。
-- sampled latent (cut_d_*/pi_b*) が欠けていれば 'Nothing'。
reconstructComb
  :: EnvA Double -> DataMap -> Map.Map Text Double -> ListCombSpec -> Maybe [Double]
reconstructComb base dataMap sm spec = case spec of
  -- orderedCuts: c_1 = cMin、 c_i = c_{i-1} + d_i (d_i = name_d_i, i=2..n)。
  OCutsSpec nm n cMinE -> do
    cMin <- either (const Nothing) Just (evalScalar base dataMap Nothing cMinE)
    ds   <- mapM (\j -> Map.lookup (nm <> "_d_" <> T.pack (show j)) sm) [2 .. n]
    pure (scanl (+) cMin ds)            -- 長さ n、 単調増加
  -- dirichlet: stick-breaking。 betas = name_b0..name_b{K-2}、 π を復元。
  DirSpec nm k -> do
    betas <- mapM (\j -> Map.lookup (nm <> "_b" <> T.pack (show j)) sm) [0 .. k - 2]
    let prods = scanl (\acc b -> acc * (1 - b)) 1 betas
    pure [ if j < length betas then (betas !! j) * (prods !! j) else prods !! j
         | j <- [0 .. k - 1] ]

-- | DoBind の RHS が list 値 combinator なら 'ListCombSpec' を返す (suffix 付き
-- base 名で。 'matchListComb' / 'listCombSuffix' と同じ命名規律)。
matchListCombSpec :: Text -> Expr -> Maybe ListCombSpec
matchListCombSpec suf rhs = case collectApp rhs of
  Right ("orderedCuts", [ELit (LText bn), ELit (LNumber d), cMinE, _scaleE]) ->
    Just (OCutsSpec (bn <> suf) (round d) cMinE)
  Right ("dirichlet", [ELit (LText bn), EList alphas]) ->
    Just (DirSpec (bn <> suf) (length alphas))
  _ -> Nothing

-- | observe 式から (名前, mean 式, distribution 式全体, 観測列名) を抽出。
-- mean 式 = Distribution の第 1 引数、 dist 式全体は WAIC/PPC で logDensity /
-- sampleDist を回すために必要。 観測列名は第 3 引数 `col "y"` の列。
observeFull :: Expr -> Maybe (Text, Expr, Expr, Text)
observeFull e = case collectApp e of
  Right ("observe", [ELit (LText nm), distExpr, ECol colName]) ->
    case collectApp distExpr of
      Right (_, m : _) -> Just (nm, m, distExpr, colName)
      _                -> Nothing
  _ -> Nothing

-- | stmts を plate 展開しながら observe instance を集める。 'interpStmts' と
-- 同じ suffix (groupSuffix) / 行 (rowsForGroup) / 群変数束縛の規律を replay し、
-- top-level observe は suffix=""・全行、 forEachGroup 内 observe は群ごとに
-- suffix 付き・群行で展開する。 latent 名は sample された scope の suffix で
-- samples のキーに対応づける (top latent="mu"、 群 latent="theta_1" 等)。
collectObsInstances :: DataMap -> [DoStmt] -> [ObsInstance]
collectObsInstances dm = go "" Nothing Map.empty Map.empty Nothing []
  where
    go :: Text -> Maybe [Int] -> Map.Map Text Text -> Map.Map Text Double
       -> Maybe (Text, Double) -> [(Text, ListCombSpec)] -> [DoStmt] -> [ObsInstance]
    go _ _ _ _ _ _ [] = []
    go suf rows nameKey penv grp combs (st : rest) = case st of
      -- latent: 現 suffix 付きで samples に載る (= キー対応を記録)。 Phase 43:
      -- list 値 combinator なら再構築仕様も記録 (= WAIC/PPC 再評価で VList 復元)。
      DoBind name rhs ->
        let combs' = case matchListCombSpec suf rhs of
                       Just spec -> (name, spec) : combs
                       Nothing   -> combs
        in go suf rows (Map.insert name (name <> suf) nameKey) penv grp combs' rest
      DoLet _ -> go suf rows nameKey penv grp combs rest
      DoExpr e
        | Just (gcol, param, inner) <- matchForEachGroup e ->
            let gvals = groupValsIn dm gcol rows
                here = concatMap
                  (\gv ->
                     go (suf <> groupSuffixFor (Map.lookup gcol dm) gv)
                        (Just (rowsForGroup dm gcol gv rows))
                        nameKey
                        (Map.insert param gv penv)
                        (Just (gcol, gv))
                        combs
                        inner)
                  gvals
            in here ++ go suf rows nameKey penv grp combs rest
        | Just (obsName, meanExpr, distExpr, obsCol) <- observeFull e ->
            ObsInstance (obsName <> suf) meanExpr distExpr obsCol nameKey penv rows grp combs
              : go suf rows nameKey penv grp combs rest
        | otherwise -> go suf rows nameKey penv grp combs rest

-- ===========================================================================
-- model graph plate aggregation (Phase 27.5 後続 TODO3、 2026-06-02)
-- ===========================================================================
--
-- forEachGroup は群ごとに内部 do-block を展開するため、 'buildModelGraph' が
-- 見る realized ModelP には alpha_1 / alpha_2 / alpha_3 … と群数ぶんの latent /
-- observe ノードが並ぶ (3 群 × 数 latent で 40 ノード級に肥大)。 PyMC 流の
-- plate 表記では「群コピーを 1 つの代表ノードに畳み、 箱のラベルに群数」 を
-- 出すので、 ここでは AST (= forEachGroup 構造が残る層) から
--
--   * realized 名 → base 名 の rename map ('plateRenameMap')
--   * forEachGroup ごとの plate (ラベル "<群列> (<群数>)" + 直下 base 名)
--     ('collectGraphPlates')
--
-- を導き、 realized ModelGraph を base 名へ collapse する
-- ('collapsePlateGraph')。 rename は interpStmts と同じ groupSuffix 規律を
-- replay して作る total な対応なので、 文字列推測ではない。

-- | model graph 上の plate (= forEachGroup 1 サイト)。 frontend
--   hgg DAGPlate (label + member id 群) に対応。
data GraphPlate = GraphPlate
  { gpLabel   :: Text     -- ^ "<群列> (<群数>)"
  , gpMembers :: [Text]   -- ^ この plate 直下の base 名 (latent + observe)
  } deriving (Show, Eq)

-- | realized node 名 (alpha_1 等) → base 名 (alpha) の rename map。
--   'collectObsInstances' / 'interpStmts' と同じ suffix (groupSuffix) /
--   行 (rowsForGroup) 規律を replay し、 各 DoBind latent / observe を
--   その出現 suffix 付き名 → base 名 で登録する。 top-level は suffix="" なので
--   恒等 (alpha → alpha)。
plateRenameMap :: DataMap -> [DoStmt] -> Map.Map Text Text
plateRenameMap dm = go "" Nothing
  where
    go :: Text -> Maybe [Int] -> [DoStmt] -> Map.Map Text Text
    go _ _ [] = Map.empty
    go suf rows (st : rest) = case st of
      DoBind name _ ->
        Map.insert (name <> suf) name (go suf rows rest)
      DoLet _ -> go suf rows rest
      DoExpr e
        | Just (gcol, _param, inner) <- matchForEachGroup e ->
            let gvals  = groupValsIn dm gcol rows
                inners = Map.unions
                  [ go (suf <> groupSuffixFor (Map.lookup gcol dm) gv)
                       (Just (rowsForGroup dm gcol gv rows)) inner
                  | gv <- gvals ]
            in inners `Map.union` go suf rows rest
        | Just (obsName, _meanE, distExpr, obsCol) <- observeFull e ->
            -- observe は col 参照を含むと 'observeColumns' で per-row 展開され、
            -- 実ノードは "<obsName><suf>_<j>" (j = 当該 plate 対象行の 0 始まり
            -- 連番、 = execObserve の規律) になる。 col 参照無しなら単一
            -- "<obsName><suf>"。 どちらも base 名 obsName に畳む。
            let colLen     = length (lookupDoubles obsCol dm)
                baseRows   = fromMaybe [0 .. colLen - 1] rows
                validCount = length [ i | i <- baseRows, i >= 0, i < colLen ]
                names | hasColRef distExpr =
                          [ obsName <> suf <> "_" <> T.pack (show j)
                          | j <- [0 .. validCount - 1] ]
                      | otherwise = [ obsName <> suf ]
            in Map.union (Map.fromList [ (nm, obsName) | nm <- names ])
                         (go suf rows rest)
        | otherwise -> go suf rows rest

-- | forEachGroup ごとに 1 plate を集める (群値ぶんは展開しない)。 ラベルは
--   "<群列> (<群数>)"、 member は当該 forEachGroup 直下の base 名 (latent +
--   observe、 ネストした forEachGroup の中身は含めない = ネストは別 plate)。
--   ネスト plate は代表 1 群の行で再帰的に拾う。
collectGraphPlates :: DataMap -> [DoStmt] -> [GraphPlate]
collectGraphPlates dm = go Nothing
  where
    go :: Maybe [Int] -> [DoStmt] -> [GraphPlate]
    go _ [] = []
    go rows (st : rest) = case st of
      DoExpr e
        | Just (gcol, _param, inner) <- matchForEachGroup e ->
            let gvals   = groupValsIn dm gcol rows
                count   = length gvals
                label   = gcol <> " (" <> T.pack (show count) <> ")"
                members = directMembers inner
                nested  = case gvals of
                  (gv : _) -> go (Just (rowsForGroup dm gcol gv rows)) inner
                  []       -> []
            in GraphPlate label members : nested ++ go rows rest
        | otherwise -> go rows rest
      _ -> go rows rest
    -- 直下の DoBind latent + observe 名 (ネスト forEachGroup は DoExpr なので除外)。
    directMembers :: [DoStmt] -> [Text]
    directMembers stmts =
      [ name | DoBind name _ <- stmts ]
      ++ [ obsName | DoExpr e <- stmts, Just (obsName, _, _, _) <- [observeFull e] ]

-- | realized 'HBM.ModelGraph' を plate 単位に collapse。 群展開ノードを base 名に
--   畳み (重複ノードは初出を残す)、 辺は両端を rename して自己ループ除去 + 重複
--   除去。 併せて plate 一覧を返す。
collapsePlateGraph
  :: DataMap -> [DoStmt] -> HBM.ModelGraph -> (HBM.ModelGraph, [GraphPlate])
collapsePlateGraph dm stmts mg =
  let rn      = plateRenameMap dm stmts
      ren x   = Map.findWithDefault x x rn
      nodes'  = dedupNodes [ renameNode ren n | n <- HBM.mgNodes mg ]
      edges'  = nub [ (ren a, ren b)
                    | (a, b) <- HBM.mgEdges mg, ren a /= ren b ]
     -- Phase 40 merge: ModelGraph に mgPlates (plate→size) フィールドが追加された。
     -- DSL forEachGroup の collapse は独自の GraphPlate 列 (collectGraphPlates) を
     -- 別途返すので、 ここでは入力 graph の mgPlates をそのまま引き継ぐ。
  in (HBM.ModelGraph nodes' edges' (HBM.mgPlates mg), collectGraphPlates dm stmts)
  where
    renameNode ren n = n
      { HBM.nodeName = ren (HBM.nodeName n)
      , HBM.nodeDeps = Set.map ren (HBM.nodeDeps n)
      }
    dedupNodes = goD Set.empty
      where
        goD _ [] = []
        goD seen (n : ns)
          | HBM.nodeName n `Set.member` seen = goD seen ns
          | otherwise = n : goD (Set.insert (HBM.nodeName n) seen) ns

-- | observe ごと × 列ごとに 64 点 curve を計算。 Phase 27 GLMM overlay:
-- top-level observe に加え forEachGroup 内の per-group observe も対象
-- ('collectObsInstances' が plate 展開)。
-- |   * 主 predictor = 当該列、 grid は (per-group なら群の) data min..max
-- |   * 他 predictor は (per-group なら群の) data median で固定
-- |   * 群 latent は suffix 付きキー (theta_1 等)、 群変数は定数として env に注入
-- |   * 各 sample × 各 grid 点で mean 式を Double 評価
-- |   * 各 grid 点で全 sample から median + 2.5% / 97.5% percentile
computeMeanCurves
  :: [TopBind]                            -- top-level 値/関数束縛 (ユーザ定義リンク等)
  -> [DoStmt]
  -> DataMap                              -- data: col → values
  -> [Map.Map Text Double]                -- posterior samples
  -> [HbmMeanCurve]
computeMeanCurves topBinds stmts dataMap samples =
  [ HbmMeanCurve
      { hmcObsName   = oiObsName inst
      , hmcPredictor = col
      , hmcX = xGrid
      , hmcMedian = map (percentileOf 0.5)   valuesPerX
      , hmcLower  = map (percentileOf 0.025) valuesPerX
      , hmcUpper  = map (percentileOf 0.975) valuesPerX
      , hmcGroupCol = fst <$> oiGroup inst
      , hmcGroupVal = snd <$> oiGroup inst
      }
  | inst <- collectObsInstances dataMap stmts
  , let meanExpr = oiMeanExpr inst
        mrows    = oiRows inst
        -- per-group なら群の行に限定して列値を取り出す。
        colValsFor c =
          let ca = lookupDoubles c dataMap
          in case mrows of
               Nothing -> ca
               Just rs -> [ ca !! i | i <- rs, i >= 0, i < length ca ]
  , col <- collectCols meanExpr
  , let xs = colValsFor col
  , not (null xs)
  , let nGrid = 64 :: Int
        xLo = minimum xs
        xHi = maximum xs
        step = if xHi == xLo then 1.0 else (xHi - xLo) / fromIntegral (nGrid - 1)
        xGrid = [ xLo + step * fromIntegral i | i <- [0 .. nGrid - 1] ]
        otherCols = filter (/= col) (collectCols meanExpr)
        medianOf vs = case sort vs of
          [] -> 0
          ss -> ss !! (length ss `div` 2)
        fixedOther =
          Map.fromList [ (c, Numeric [medianOf (colValsFor c)]) | c <- otherCols ]
        -- 群変数 (forEachGroup の \g) を定数として env に注入。
        paramVNums = Map.map VNum (oiParamEnv inst)
        -- sample を nameKey で remap: local latent 名 → samples の suffix 付きキー。
        -- (Map.union は left-biased なので renamed が元キーより優先)
        remapSample sample =
          let renamed = Map.fromList
                [ (localNm, v)
                | (localNm, key) <- Map.toList (oiNameKey inst)
                , Just v <- [Map.lookup key sample] ]
          in Map.union renamed sample
        evalAt sample x =
          let synthetic = Map.insert col (Numeric [x]) fixedOther
              sm = remapSample sample
              -- posterior サンプル (alpha/beta 等) を env に、 top-level
              -- 値/関数 (ユーザ定義リンク等) + 群変数も併せて見えるようにする。
              senv = Map.union paramVNums
                       (Map.union (Map.map VNum sm) (buildTopEnv dataMap topBinds))
          in case evalScalar @Double senv synthetic (Just 0) meanExpr of
               Right v -> v
               Left _  -> 0 / 0  -- NaN
        valuesPerX = [ [ evalAt sample x | sample <- samples ] | x <- xGrid ]
  ]

-- ---------------------------------------------------------------------------
-- Phase 27.5 (2026-06-02): WAIC / LOO / posterior predictive 用の observe
-- distribution 評価。 computeMeanCurves と同じ plate 展開 (collectObsInstances)
-- を使い、 mean 式ではなく distribution 式全体を各 sample × 各対象行で評価する。
-- これにより pointwise log-likelihood (WAIC/LOO) と posterior predictive draw
-- (PPC、 worker 側で sampleDist) の共通基盤を 1 度で作る。
-- ---------------------------------------------------------------------------

-- | observe instance 1 個分の、 全 posterior sample × 対象行で評価した結果。
data ObsDistSet = ObsDistSet
  { odsName     :: !Text                          -- observe ノード名 (suffix 付き)
  , odsObserved :: ![Double]                      -- 対象行の観測値 (= col の当該行)
  , odsDists    :: ![[HBM.Distribution Double]]   -- [sample][row] の Distribution
  }

-- | 各 observe instance について、 各 posterior sample × 各対象行で
-- distribution を評価する。 mean に列参照がある GLM 形 (例: Normal (a+b*'x') s)
-- も evalDist の per-row 評価 ((Just i)) で正しく行ごとに展開される。
computeObsDists
  :: [TopBind]
  -> [DoStmt]
  -> DataMap
  -> [Map.Map Text Double]
  -> [ObsDistSet]
computeObsDists topBinds stmts dataMap samples =
  [ ObsDistSet
      { odsName     = oiObsName inst
      , odsObserved = ys
      , odsDists    = [ [ distAt sample i | i <- rows ] | sample <- samples ]
      }
  | inst <- collectObsInstances dataMap stmts
  , let distExpr = oiDistExpr inst
        yCol     = lookupDoubles (oiObsCol inst) dataMap
        allRows  = [0 .. length yCol - 1]
        rows     = filter (\i -> i >= 0 && i < length yCol)
                     (fromMaybe allRows (oiRows inst))
        ys       = [ yCol !! i | i <- rows ]
        paramVNums = Map.map VNum (oiParamEnv inst)
        remapSample sample =
          let renamed = Map.fromList
                [ (localNm, v)
                | (localNm, key) <- Map.toList (oiNameKey inst)
                , Just v <- [Map.lookup key sample] ]
          in Map.union renamed sample
        distAt sample i =
          let sm   = remapSample sample
              base = Map.union paramVNums
                       (Map.union (Map.map VNum sm) (buildTopEnv dataMap topBinds))
              -- Phase 43: combinator 由来 latent vector (cuts/probs) を sampled
              -- latent から再構築して VList 束縛 (= 消費分布の list 引数を解決)。
              combBinds = [ (bindNm, VList (map VNum vals))
                          | (bindNm, spec) <- oiListCombs inst
                          , Just vals <- [reconstructComb base dataMap sample spec] ]
              senv = Map.union (Map.fromList combBinds) base
          in case evalDist senv dataMap (Just i) distExpr :: Err (HBM.Distribution Double) of
               Right d -> d
               Left _  -> HBM.Normal (0 / 0) 1   -- 評価不能は NaN 化 (logDensity→NaN)
  , not (null rows)
  ]

-- | 'ObsDistSet' 群から WAIC/LOO 用の log-likelihood 行列 (S × N) を作る。
-- 行 = posterior sample、 列 = 全 observe instance の全対象行を連結。
-- @Hanalyze.Stat.ModelSelect.waic@ / @loo@ がこの shape を期待する。
pointwiseLogLik :: [ObsDistSet] -> [[Double]]
pointwiseLogLik sets =
  [ concatMap (\set -> zipWith HBM.logDensity (sampleRow set s) (odsObserved set)) sets
  | s <- [0 .. nSamples - 1] ]
  where
    nSamples = case sets of
      (set : _) -> length (odsDists set)
      []        -> 0
    sampleRow set s = case drop s (odsDists set) of
      (row : _) -> row
      []        -> []

-- | log-lik 行列 (S×N) から非有限 (NaN / ±Inf) を含む観測列を除外する
-- (Phase 27.5 後続 TODO4、 2026-06-02)。 'distAt' の eval 失敗 (= Normal NaN) や
-- 退化パラメータで 'logDensity' が NaN / Inf になった観測点は、 そのまま waic/loo に
-- 渡すと per-observation 集計 (lppd / pwaic) を汚染して全体が NaN → JSON null に
-- なる。 該当する観測列 (= 全 sample で同一観測点) を丸ごと落として残りで waic/loo を
-- 計算できるようにする。 返り値は (除外後行列, 落とした列数)。 全列が非有限なら
-- ([], N) を返し、 呼び出し側は waic/loo を Nothing にできる。
finitePointwiseLogLik :: [[Double]] -> ([[Double]], Int)
finitePointwiseLogLik mat =
  let cols     = transpose mat              -- [obs点][sample]
      keptCols = filter (all isFiniteD) cols
      dropped  = length cols - length keptCols
  in (transpose keptCols, dropped)
  where
    isFiniteD x = not (isNaN x || isInfinite x)

-- ===========================================================================
-- Phase 44: multi-column observe (observeMV) の WAIC / PPC 経路
-- ===========================================================================
--
-- scalar observe (1 列) の WAIC/PPC ('collectObsInstances' / 'computeObsDists' /
-- 'pointwiseLogLik') は per-row スカラ logDensity 前提で、 multi-column の
-- k-vector joint density (= 'HBM.obsLogSum') を扱えない。 そこで Phase 44 の
-- 設計方針 (observeMV は scalar observe と別 builtin の並行経路) を WAIC/PPC まで
-- 貫き、 **MV 専用の並行経路** を足す (scalar 経路は無傷)。
--
-- latent Σ (lkjCorrCholesky 由来の相関 Cholesky L) は posterior sample に
-- 内部 latent (@L_u<i>_<j>@ = partial-correlation の Beta latent) として載るので、
-- bind 名 @L@ を 'reconstructMatrixComb' で再構築して MvNormalChol に渡す
-- (Phase 43 'reconstructComb' の行列版)。

-- | 行列値 combinator の再構築仕様 (suffix 付き base 名 + 次元 k)。
data MatrixCombSpec
  = LkjCholSpec Text Int   -- ^ suffix 付き base 名, k (= 行列次元)
  deriving (Show)

-- | DoBind の RHS が行列値 combinator (lkjCorrCholesky) なら 'MatrixCombSpec' を
-- 返す ('matchMatrixComb' / 'matrixCombSuffix' と同じ命名規律)。
matchMatrixCombSpec :: Text -> Expr -> Maybe MatrixCombSpec
matchMatrixCombSpec suf rhs = case collectApp rhs of
  Right ("lkjCorrCholesky", [ELit (LText bn), ELit (LNumber d), _etaE]) ->
    Just (LkjCholSpec (bn <> suf) (round d))
  _ -> Nothing

-- | lkjCorrCholesky の相関 Cholesky L を 1 posterior サンプルから再構築する。
-- sampled latent は @<nm>_u<i>_<j>@ (Beta in (0,1))、 partial correlation は
-- @z_ij = 2u - 1@。 L は 'HBM.lkjCorrCholesky' の deterministic 構築を replay:
--   L_00 = 1、 対角 L_ii = √(1 - Σ_{k<i} z_{i,k}²)、
--   対角下 L_ij = z_ij · √(Π_{k<j}(1 - z_{i,k}²))  (j < i)。
-- sampled latent が欠ければ 'Nothing'。
reconstructMatrixComb :: Map.Map Text Double -> MatrixCombSpec -> Maybe [[Double]]
reconstructMatrixComb sm (LkjCholSpec nm k) = do
  let uKey i j = nm <> "_u" <> T.pack (show i) <> "_" <> T.pack (show j)
  pcPairs <- mapM
    (\(i, j) -> do u <- Map.lookup (uKey i j) sm; pure ((i, j), 2 * u - 1))
    [(i, j) | i <- [1 .. k - 1], j <- [0 .. i - 1]]
  let pcMap = Map.fromList pcPairs
      pc i j = Map.findWithDefault 0 (i, j) pcMap
      sq z = z * z
      lRow i =
        [ if j > i then 0
          else if i == 0 && j == 0 then 1
          else if j == i
               then sqrt (max 0 (1 - sum [ sq (pc i kk) | kk <- [0 .. i - 1] ]))
          else pc i j * sqrt (max 0 (product [ 1 - sq (pc i kk) | kk <- [0 .. j - 1] ]))
        | j <- [0 .. k - 1] ]
  pure [ lRow i | i <- [0 .. k - 1] ]

-- | observeMV 式から (名前, distribution 式全体, 観測列名リスト) を抽出。
observeMVFull :: Expr -> Maybe (Text, Expr, [Text])
observeMVFull e = case collectApp e of
  Right ("observeMV", [ELit (LText nm), distExpr, EList colRefs]) ->
    let cols = [ c | ECol c <- colRefs ]
    in if length cols == length colRefs && length cols >= 2
         then Just (nm, distExpr, cols) else Nothing
  _ -> Nothing

-- | observeMV instance (plate 展開済)。 'collectObsInstances' の MV 版で、
-- 単一 'oiObsCol' でなく **列リスト** を持ち、 list/matrix combinator の
-- 再構築仕様も保持する。
data MvObsInstance = MvObsInstance
  { mviObsName     :: Text
  , mviDistExpr    :: Expr
  , mviObsCols     :: [Text]
  , mviNameKey     :: Map.Map Text Text
  , mviParamEnv    :: Map.Map Text Double
  , mviRows        :: Maybe [Int]
  , mviListCombs   :: [(Text, ListCombSpec)]
  , mviMatrixCombs :: [(Text, MatrixCombSpec)]
  }

-- | stmts を plate 展開しながら observeMV instance を集める
-- ('collectObsInstances' と同じ suffix / 行 / 群変数 / latent 名規律を replay)。
collectMvObsInstances :: DataMap -> [DoStmt] -> [MvObsInstance]
collectMvObsInstances dm = go "" Nothing Map.empty Map.empty [] []
  where
    go :: Text -> Maybe [Int] -> Map.Map Text Text -> Map.Map Text Double
       -> [(Text, ListCombSpec)] -> [(Text, MatrixCombSpec)] -> [DoStmt]
       -> [MvObsInstance]
    go _ _ _ _ _ _ [] = []
    go suf rows nameKey penv lcombs mcombs (st : rest) = case st of
      DoBind name rhs ->
        let lcombs' = case matchListCombSpec suf rhs of
                        Just spec -> (name, spec) : lcombs
                        Nothing   -> lcombs
            mcombs' = case matchMatrixCombSpec suf rhs of
                        Just spec -> (name, spec) : mcombs
                        Nothing   -> mcombs
        in go suf rows (Map.insert name (name <> suf) nameKey) penv lcombs' mcombs' rest
      DoLet _ -> go suf rows nameKey penv lcombs mcombs rest
      DoExpr e
        | Just (gcol, param, inner) <- matchForEachGroup e ->
            let gvals = groupValsIn dm gcol rows
                here = concatMap
                  (\gv ->
                     go (suf <> groupSuffixFor (Map.lookup gcol dm) gv)
                        (Just (rowsForGroup dm gcol gv rows))
                        nameKey (Map.insert param gv penv) lcombs mcombs inner)
                  gvals
            in here ++ go suf rows nameKey penv lcombs mcombs rest
        | Just (obsName, distExpr, cols) <- observeMVFull e ->
            MvObsInstance (obsName <> suf) distExpr cols nameKey penv rows lcombs mcombs
              : go suf rows nameKey penv lcombs mcombs rest
        | otherwise -> go suf rows nameKey penv lcombs mcombs rest

-- | observeMV 1 個分の WAIC/PPC 評価結果。 'ObsDistSet' の MV 版。
data MvObsDistSet = MvObsDistSet
  { mvodsName     :: !Text                          -- ^ 表示名 (top="y"、 group="y_1")
  , mvodsCols     :: ![Text]                        -- ^ 観測列名リスト (長さ k)
  , mvodsObserved :: ![[Double]]                    -- ^ [row][component] = 各行の k-vector
  , mvodsDists    :: ![[HBM.Distribution Double]]   -- ^ [sample][row] の Distribution
  }

-- | observeMV instance × sample × 行で多変量 Distribution を評価する
-- ('computeObsDists' の MV 版)。 dist は行不変 (μ/Σ は latent) なので各行で
-- 同一だが、 既存経路に合わせ row 評価する。 latent Σ は 'reconstructMatrixComb'
-- で L を、 list 引数は 'reconstructComb' で復元して env に束縛する。
computeMvObsDists
  :: [TopBind]
  -> [DoStmt]
  -> DataMap
  -> [Map.Map Text Double]
  -> [MvObsDistSet]
computeMvObsDists topBinds stmts dataMap samples =
  [ MvObsDistSet
      { mvodsName     = mviObsName inst
      , mvodsCols     = cols
      , mvodsObserved = [ [ lookupDoubles c dataMap !! i | c <- cols ] | i <- rows ]
      , mvodsDists    = [ [ distAt sample i | i <- rows ] | sample <- samples ]
      }
  | inst <- collectMvObsInstances dataMap stmts
  , let distExpr = mviDistExpr inst
        cols     = mviObsCols inst
        colLens  = map (length . (`lookupDoubles` dataMap)) cols
        colLen   = if null colLens then 0 else minimum colLens
        allRows  = [0 .. colLen - 1]
        rows     = filter (\i -> i >= 0 && i < colLen)
                     (fromMaybe allRows (mviRows inst))
        paramVNums = Map.map VNum (mviParamEnv inst)
        remapSample sample =
          let renamed = Map.fromList
                [ (localNm, v)
                | (localNm, key) <- Map.toList (mviNameKey inst)
                , Just v <- [Map.lookup key sample] ]
          in Map.union renamed sample
        distAt sample i =
          let sm   = remapSample sample
              base = Map.union paramVNums
                       (Map.union (Map.map VNum sm) (buildTopEnv dataMap topBinds))
              listBinds = [ (bn, VList (map VNum vals))
                          | (bn, spec) <- mviListCombs inst
                          , Just vals <- [reconstructComb base dataMap sample spec] ]
              matBinds  = [ (bn, VList (map (VList . map VNum) m))
                          | (bn, spec) <- mviMatrixCombs inst
                          , Just m <- [reconstructMatrixComb sample spec] ]
              senv = Map.union (Map.fromList (listBinds ++ matBinds)) base
          in case evalDist senv dataMap (Just i) distExpr :: Err (HBM.Distribution Double) of
               Right d -> d
               Left _  -> HBM.MvNormal [0 / 0] [[1]]   -- eval 不能は NaN 化
  , not (null rows)
  ]

-- | MV observe の pointwise log-lik 行列 (S×N)。 各行 (= 1 観測点) の寄与は
-- k-vector joint density 'HBM.obsLogSum'。 scalar 経路の 'pointwiseLogLik' と
-- 列方向に連結して使う (worker 側)。
pointwiseLogLikMv :: [MvObsDistSet] -> [[Double]]
pointwiseLogLikMv sets =
  [ concatMap (\set -> [ HBM.obsLogSum (distRow set s !! r) (mvodsObserved set !! r)
                       | r <- [0 .. nRows set - 1] ]) sets
  | s <- [0 .. nSamples - 1] ]
  where
    nSamples = case sets of
      (set : _) -> length (mvodsDists set)
      []        -> 0
    nRows set = length (mvodsObserved set)
    distRow set s = case drop s (mvodsDists set) of
      (row : _) -> row
      []        -> []
