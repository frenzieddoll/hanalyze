{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Hanalyze.Model.HBM.Ast
-- Description : HBM dialog DSL の AST 型と JSON decoder
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- HBM dialog DSL の AST 型と JSON decoder。
--
-- Phase 27.5 (2026-05-31): canvas-backend @フロントエンド app.Analysis.HBM@ から
-- 移設。 frontend が backend 統一 parser (@/api/v1/dsl/parse@) から得た
-- @program_ast@ (JSON) を、 streaming sidecar が直接 decode して実モデルを
-- 構築できるよう、 AST 型 + 'parseAst' をライブラリ層 (hanalyze) に置く。
--
-- 本 module は **canvas wire 型にも text parser (DSL frontend) にも依存しない**
-- (= aeson のみ)。 text → AST 変換 ('parseHbmTextToExpr' 等) は HT (DSL frontend)
-- に依存するため canvas-backend 側に残す。
module Hanalyze.Model.HBM.Ast
  ( -- * AST
    Expr (..)
  , Lit (..)
  , Bind (..)
  , DoStmt (..)
    -- * JSON decode (= frontend program_ast → Expr)
  , parseAst
  , parseLit
  , parseBind
  , parseDoStmt
    -- * JSON encode (= 'parseAst' の正確な逆。 backend が sidecar に
    --   program_ast / top_binds を送る際に使う、 Phase 27.5 step 3)
  , exprToJSON
  , litToJSON
  , bindToJSON
  , doStmtToJSON
    -- * helpers
  , Err
  , collectApp
  , getField
  , getStr
  , getNum
  , getBool
  , getArray
  ) where

import Data.Text (Text)
import qualified Data.Aeson as A
import Data.Aeson.Types (Pair)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V

-- ---------------------------------------------------------------------------
-- AST (= frontend App.Hbm.Ast / DSL frontend hanalyze.HBM.Text.HbmExpr と同形、
--   11 ctor: ELit / ECol / EVar / EApp / ELam / EIf / ELet / ENeg / EOp /
--   EList / EDo)
-- ---------------------------------------------------------------------------

data Expr
  = ELit Lit
  | ECol Text
  | EVar Text
  | EApp Expr Expr
  | ELam Text Expr
  | EIf Expr Expr Expr
  | ELet [Bind] Expr
  | ENeg Expr
  | EOp Text Expr Expr
  | EList [Expr]
  | EDo [DoStmt] Expr
  deriving (Show)

data Lit = LNumber Double | LText Text | LBool Bool deriving (Show)

data Bind = Bind { bindName :: Text, bindValue :: Expr } deriving (Show)

data DoStmt
  = DoBind Text Expr
  | DoLet [Bind]
  | DoExpr Expr
  deriving (Show)

-- | 評価系で多用する Either alias。
type Err a = Either Text a

-- ---------------------------------------------------------------------------
-- JSON parser (= frontend が送る program_ast を Expr に decode)
-- ---------------------------------------------------------------------------

parseAst :: A.Value -> Either Text Expr
parseAst v = case v of
  A.Object o -> do
    tag <- getStr o "tag"
    case tag of
      "ELit" -> ELit <$> (parseLit =<< getField o "lit")
      "ECol" -> ECol <$> getStr o "name"
      "EVar" -> EVar <$> getStr o "name"
      "EApp" -> EApp <$> (parseAst =<< getField o "f") <*> (parseAst =<< getField o "x")
      "ELam" -> ELam <$> getStr o "arg" <*> (parseAst =<< getField o "body")
      "EIf"  -> EIf <$> (parseAst =<< getField o "c")
                    <*> (parseAst =<< getField o "a")
                    <*> (parseAst =<< getField o "b")
      "ELet" -> do
        bs <- getArray o "binds" >>= mapM parseBind
        body <- parseAst =<< getField o "body"
        Right (ELet bs body)
      "ENeg" -> ENeg <$> (parseAst =<< getField o "e")
      "EOp"  -> EOp <$> getStr o "op"
                    <*> (parseAst =<< getField o "a")
                    <*> (parseAst =<< getField o "b")
      "EList" -> EList <$> (getArray o "items" >>= mapM parseAst)
      "EDo"  -> do
        stmts <- getArray o "stmts" >>= mapM parseDoStmt
        ret <- parseAst =<< getField o "ret"
        Right (EDo stmts ret)
      _ -> Left ("Unknown AST tag: " <> tag)
  _ -> Left "AST root must be a JSON object"

parseLit :: A.Value -> Either Text Lit
parseLit v = case v of
  A.Object o -> do
    tag <- getStr o "tag"
    case tag of
      "LNumber" -> do
        n <- getNum o "value"
        Right (LNumber n)
      "LText"   -> LText <$> getStr o "value"
      "LBool"   -> LBool <$> getBool o "value"
      _ -> Left ("Unknown literal tag: " <> tag)
  _ -> Left "Literal must be an object"

parseBind :: A.Value -> Either Text Bind
parseBind v = case v of
  A.Object o -> do
    n <- getStr o "name"
    e <- parseAst =<< getField o "value"
    Right (Bind n e)
  _ -> Left "Bind must be an object"

parseDoStmt :: A.Value -> Either Text DoStmt
parseDoStmt v = case v of
  A.Object o -> do
    tag <- getStr o "tag"
    case tag of
      "DoBind" -> do
        name <- getStr o "name"
        rawValue <- parseAst =<< getField o "value"
        -- Phase 9.1d-4 fix: frontend が `x <- sample "obsName" dist` を
        -- DoBind の value に raw expression として渡してくる。 validateStmts
        -- 以降は value が「純粋な distribution」 であることを期待するので、
        -- ここで sample wrapper を剥がす。 sample 形でなければそのまま通す
        -- (互換: 直接 dist を入れた古い経路があった場合のため)。
        let distOnly = case collectApp rawValue of
              Right ("sample", [ELit (LText _samplerName), d]) -> d
              _ -> rawValue
        pure (DoBind name distOnly)
      "DoLet"  -> DoLet <$> (getArray o "binds" >>= mapM parseBind)
      "DoExpr" -> DoExpr <$> (parseAst =<< getField o "value")
      _ -> Left ("Unknown DoStmt tag: " <> tag)
  _ -> Left "DoStmt must be an object"

-- ---------------------------------------------------------------------------
-- JSON encoder (= parseAst の正確な逆。 round-trip: parseAst . exprToJSON ≡ Right)
--
-- Phase 27.5 step 3 (2026-06-01): topology B で backend が stream sidecar に
-- start.params を組む際、 resolveHbmModel が返す Expr / TopBind を worker
-- (= parseAst で decode) が読める JSON 文字列に直す必要がある。 decoder と
-- 同じ module に逆変換を置き、 tag / field 名のズレを構造的に防ぐ。
--
-- 注: DoBind の value は sample wrapper を剥がした dist-only を前提とする
-- (parseDoStmt は sample wrapper を剥がすが、 既に剥がれた式には作用しない =
-- idempotent。 resolveHbmModel 経由の Expr は剥がし済)。
-- ---------------------------------------------------------------------------

exprToJSON :: Expr -> A.Value
exprToJSON e = case e of
  ELit l       -> obj "ELit"  ["lit"   A..= litToJSON l]
  ECol n       -> obj "ECol"  ["name"  A..= n]
  EVar n       -> obj "EVar"  ["name"  A..= n]
  EApp f x     -> obj "EApp"  ["f"     A..= exprToJSON f, "x" A..= exprToJSON x]
  ELam a b     -> obj "ELam"  ["arg"   A..= a, "body" A..= exprToJSON b]
  EIf c a b    -> obj "EIf"   ["c"     A..= exprToJSON c, "a" A..= exprToJSON a, "b" A..= exprToJSON b]
  ELet bs body -> obj "ELet"  ["binds" A..= map bindToJSON bs, "body" A..= exprToJSON body]
  ENeg x       -> obj "ENeg"  ["e"     A..= exprToJSON x]
  EOp op a b   -> obj "EOp"   ["op"    A..= op, "a" A..= exprToJSON a, "b" A..= exprToJSON b]
  EList xs     -> obj "EList" ["items" A..= map exprToJSON xs]
  EDo stmts r  -> obj "EDo"   ["stmts" A..= map doStmtToJSON stmts, "ret" A..= exprToJSON r]
  where
    obj :: Text -> [Pair] -> A.Value
    obj tag fields = A.object (("tag" A..= tag) : fields)

litToJSON :: Lit -> A.Value
litToJSON l = case l of
  LNumber n -> A.object ["tag" A..= ("LNumber" :: Text), "value" A..= n]
  LText t   -> A.object ["tag" A..= ("LText" :: Text),   "value" A..= t]
  LBool b   -> A.object ["tag" A..= ("LBool" :: Text),   "value" A..= b]

bindToJSON :: Bind -> A.Value
bindToJSON (Bind n v) = A.object ["name" A..= n, "value" A..= exprToJSON v]

doStmtToJSON :: DoStmt -> A.Value
doStmtToJSON s = case s of
  DoBind n v -> A.object ["tag" A..= ("DoBind" :: Text), "name" A..= n, "value" A..= exprToJSON v]
  DoLet bs   -> A.object ["tag" A..= ("DoLet"  :: Text), "binds" A..= map bindToJSON bs]
  DoExpr v   -> A.object ["tag" A..= ("DoExpr" :: Text), "value" A..= exprToJSON v]

-- | @EApp (EApp (EVar f) a) b@ → @(f, [a, b])@。 distribution / 関数適用の
-- head + 引数列を取り出す。 head が変数でなければ Left。
collectApp :: Expr -> Err (Text, [Expr])
collectApp e0 = go e0 []
  where
    go (EVar n) acc = Right (n, acc)
    go (EApp f x) acc = go f (x : acc)
    go _ _ = Left "Distribution must be a function applied to scalar args"

-- ---------------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------------

getField :: A.Object -> Text -> Either Text A.Value
getField o k = case KM.lookup (Key.fromText k) o of
  Just v -> Right v
  Nothing -> Left ("Missing field: " <> k)

getStr :: A.Object -> Text -> Either Text Text
getStr o k = case KM.lookup (Key.fromText k) o of
  Just (A.String s) -> Right s
  _ -> Left ("Field not string: " <> k)

getNum :: A.Object -> Text -> Either Text Double
getNum o k = case KM.lookup (Key.fromText k) o of
  Just (A.Number n) -> Right (realToFrac n)
  _ -> Left ("Field not number: " <> k)

getBool :: A.Object -> Text -> Either Text Bool
getBool o k = case KM.lookup (Key.fromText k) o of
  Just (A.Bool b) -> Right b
  _ -> Left ("Field not bool: " <> k)

getArray :: A.Object -> Text -> Either Text [A.Value]
getArray o k = case KM.lookup (Key.fromText k) o of
  Just (A.Array xs) -> Right (V.toList xs)
  _ -> Left ("Field not array: " <> k)
