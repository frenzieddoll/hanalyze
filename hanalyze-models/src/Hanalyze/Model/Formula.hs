{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Hanalyze.Model.Formula
-- Description : Formula DSL 正本 front-end (独自・明示係数構文) の parser と AST
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Formula DSL — 正本 front-end (独自・明示係数構文) の parser と AST。
--
--   このモジュールの責務は「文字列 → 構文木 (Formula AST)」 のみ。
--   AST が真の正本で、 R/patsy front-end も同じ AST に落とす。
--   意味論的分類 (Ref がデータ変数かパラメータか・factor 添字・基底展開) は
--   data と突合する後段 (ModelFrame / designMatrixF) に委ねる。
--   ゆえに本モジュールは plot 非依存・portable (upstream hanalyze cherry-pick 候補)。
--
--   構文 (例): @"y x group = b0 + b1*x + b2*log x + bg ! group"@
--     - 左辺 @y x group@ で 応答=y / データ変数=x,group を宣言。
--     - 右辺の自由名 (左辺に無い名前) = 推定パラメータ。
--     - @+@ @-@ @*@ @/@ @^@ は常に本物の算術 (R formula の「項追加」 ではない)。
--     - 添字 @bg ! group@ = 係数ベクトル × factor 水準 (@!@ は Haskell 正規の添字演算子)。
--     - 交互作用は型で分解: 連続×連続 @b*x*z@ / factor×連続 @bg ! group * x@ /
--       factor×factor @b ! x ! z@ (@!@ 連鎖 = 2 次元添字)。
--     - 適用 @log x@ / @exp(-b*x)@ / @bspline(x,k)@ (空白並置・括弧引数どちらも App)。
--
-- [English]: Formula DSL — the parser and AST for the canonical
-- front-end (its own, explicit-coefficient syntax).
--
--   This module's sole responsibility is "string → syntax tree (Formula
--   AST)". The AST is the true source of truth, and the R/patsy
--   front-end also lowers into the same AST. Semantic classification
--   (whether a Ref is a data variable or a parameter, factor
--   subscripting, basis expansion) is deferred to the later stage that
--   cross-references it against the data (ModelFrame \/ designMatrixF).
--   Hence this module is plot-independent and portable (a candidate for
--   cherry-picking into the upstream hanalyze).
--
--   Syntax (example): @"y x group = b0 + b1*x + b2*log x + bg ! group"@
--     - The left-hand side @y x group@ declares response=y \/ data
--       variables=x,group.
--     - Free names on the right-hand side (names not on the left) are
--       estimated parameters.
--     - @+@ @-@ @*@ @/@ @^@ are always real arithmetic (not R formula's
--       "term addition").
--     - Subscript @bg ! group@ = coefficient vector × factor level (@!@
--       is Haskell's ordinary indexing operator).
--     - Interactions are decomposed by type: continuous×continuous
--       @b*x*z@ \/ factor×continuous @bg ! group * x@ \/
--       factor×factor @b ! x ! z@ (chained @!@ = 2-dimensional
--       subscripting).
--     - Application @log x@ \/ @exp(-b*x)@ \/ @bspline(x,k)@ (both
--       whitespace juxtaposition and parenthesized arguments are App).
module Hanalyze.Model.Formula
  ( -- * AST (真の正本)
    Formula (..)
  , Term (..)
  , BinOp (..)
    -- * Parse (正本 front-end = 独自構文)
  , parseFormula
    -- * Pretty (round-trip 検証用・正規形)
  , prettyFormula
  , prettyTerm
  ) where

import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Void                      (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char           (alphaNumChar, char, letterChar,
                                                 space1)
import qualified Text.Megaparsec.Char.Lexer     as L

-- ============================================================================
-- AST — parse 結果の構文木 (意味論的分類は後段)
-- ============================================================================

-- | [日本語]: 二項算術演算子 (すべて本物の算術)。
--   [English]: Binary arithmetic operators (all real arithmetic).
data BinOp = Add | Sub | Mul | Div | Pow
  deriving (Eq, Show)

-- | [日本語]: 右辺の式木。 Ref がデータ変数かパラメータかは 'Formula' の LHS
--   宣言で決まる。
--   [English]: The expression tree for the right-hand side. Whether a
--   Ref is a data variable or a parameter is decided by the LHS
--   declaration in 'Formula'.
data Term
  = Lit Double          -- ^ [日本語]: 数値リテラル (非負。 負号は 'Neg' が担う)。 [English]: A numeric literal (non-negative; the sign is handled by 'Neg').
  | Ref Text            -- ^ [日本語]: 識別子参照 (x / b1 / group)。 [English]: An identifier reference (x / b1 / group).
  | App Text [Term]     -- ^ [日本語]: 関数適用 log x / exp(-b*x) / bspline(x,k)。 [English]: Function application: log x / exp(-b*x) / bspline(x,k).
  | Index Term Term     -- ^ [日本語]: 添字 bg ! group (連鎖 b!x!z = Index (Index (Ref b) (Ref x)) (Ref z))。 [English]: Subscripting bg ! group (chained b!x!z = Index (Index (Ref b) (Ref x)) (Ref z)).
  | Neg Term            -- ^ [日本語]: 単項マイナス -x。 [English]: Unary minus -x.
  | Bin BinOp Term Term -- ^ [日本語]: 二項算術。 [English]: Binary arithmetic.
  deriving (Eq, Show)

-- | [日本語]: formula 全体。 左辺で応答 + データ変数を宣言、 右辺が式。
--   [English]: The whole formula. The left-hand side declares the
--   response plus data variables, and the right-hand side is the
--   expression.
data Formula = Formula
  { formResponse :: Text   -- ^ [日本語]: 応答変数 y。 [English]: The response variable y.
  , formDataVars :: [Text] -- ^ [日本語]: データ変数宣言 (x, group, …)。 右辺の自由名でこれに無い名前 = パラメータ。 [English]: Data-variable declarations (x, group, …). Free names on the right-hand side not in this list are parameters.
  , formRHS      :: Term   -- ^ [日本語]: 右辺式。 [English]: The right-hand-side expression.
  }
  deriving (Eq, Show)

-- ============================================================================
-- Parser (megaparsec) — 字句 / 優先順位 / formula 全体
-- ============================================================================

type Parser = Parsec Void Text

-- | [日本語]: 空白消費 (コメントは持たない)。
--   [English]: Consumes whitespace (has no comment support).
sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | [日本語]: 識別子: 英字/_ 始まり、 英数/_ 継続。
--   [English]: An identifier: starts with a letter or @_@, continues
--   with letters/digits/@_@.
identifier :: Parser Text
identifier = lexeme $ do
  c  <- letterChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_')
  pure (T.pack (c : cs))

-- | [日本語]: 数値リテラル (非負)。 float 優先 (0.5)、 無ければ整数 (2)。
--   [English]: A numeric literal (non-negative). Tries float first
--   (0.5), falling back to integer (2).
number :: Parser Double
number = lexeme (try L.float <|> (fromIntegral <$> (L.decimal :: Parser Integer)))

-- | [日本語]: 括弧でくくった部分式 (grouping)。
--   [English]: A subexpression wrapped in parentheses (grouping).
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | [日本語]: atom = 数値 | 括弧グループ | 識別子参照。
--   [English]: atom = number | parenthesized group | identifier
--   reference.
pAtom :: Parser Term
pAtom =
      (Lit <$> number)
  <|> parens pExpr
  <|> (Ref <$> identifier)

-- | [日本語]: 適用項。 識別子の直後に
--     - 括弧引数 @f(a, b, …)@ が来れば多引数 App、
--     - 空白並置 atom @log x@ が来れば単/多引数 App、
--   どちらも無ければただの atom。
--   [English]: An application term. Immediately after an identifier:
--     - if parenthesized arguments @f(a, b, …)@ follow, a multi-arg App;
--     - if a whitespace-juxtaposed atom @log x@ follows, a single-\/
--       multi-arg App;
--   if neither, just the atom.
pApp :: Parser Term
pApp = do
  h <- pAtom
  case h of
    Ref f -> do
      mcall <- optional (parens (pExpr `sepBy1` symbol ","))
      case mcall of
        Just args -> pure (App f args)          -- f(a, b)
        Nothing   -> do
          xs <- many pAtom                       -- log x (空白並置)
          pure (if null xs then h else App f xs)
    _ -> pure h

-- | [日本語]: 式 (優先順位付き)。 高→低: @!@ 添字 > @^@ > 単項@-@ > @* /@ > @+ -@。
--   [English]: An expression (with precedence). Highest to lowest:
--   @!@ subscript > @^@ > unary @-@ > @* /@ > @+ -@.
pExpr :: Parser Term
pExpr = makeExprParser pApp opTable

opTable :: [[Operator Parser Term]]
opTable =
  [ [ InfixL (Index       <$ symbol "!") ]                  -- 添字 (左結合・最高位)
  , [ InfixR (Bin Pow     <$ symbol "^") ]                  -- べき (右結合)
  , [ Prefix (Neg         <$ symbol "-") ]                  -- 単項マイナス (^ より下)
  , [ InfixL (Bin Mul     <$ symbol "*")
    , InfixL (Bin Div     <$ symbol "/") ]
  , [ InfixL (Bin Add     <$ symbol "+")
    , InfixL (Bin Sub     <$ symbol "-") ]
  ]

-- | [日本語]: formula 全体: @LHS変数列 = RHS式@。
--   [English]: The whole formula: @LHS variable list = RHS expression@.
pFormula :: Parser Formula
pFormula = do
  sc
  vars <- some identifier
  _    <- symbol "="
  rhs  <- pExpr
  eof
  case vars of
    (y : ds) -> pure (Formula y ds rhs)
    []       -> fail "左辺に応答変数がありません"

-- | [日本語]: 文字列 → 'Formula'。 失敗時は人間可読なエラーメッセージ。
--   [English]: Parses a string into a 'Formula'. On failure, produces a
--   human-readable error message.
parseFormula :: Text -> Either String Formula
parseFormula t =
  case parse pFormula "<formula>" t of
    Left err -> Left (errorBundlePretty err)
    Right f  -> Right f

-- ============================================================================
-- Pretty — round-trip の正規形 (App は常に括弧形式で曖昧性ゼロ)
-- ============================================================================

-- | [日本語]: 'Formula' を正規形文字列に。 @parseFormula (prettyFormula f) ==
--   Right f@ を満たす。
--   [English]: Renders a 'Formula' to its canonical-form string.
--   Satisfies @parseFormula (prettyFormula f) == Right f@.
prettyFormula :: Formula -> Text
prettyFormula (Formula y ds rhs) =
  T.unwords (y : ds) <> " = " <> prettyTerm rhs

-- | [日本語]: 右辺式を正規形に (優先順位に応じ最小限の括弧)。
--   [English]: Renders the right-hand-side expression to canonical form
--   (minimal parentheses according to precedence).
prettyTerm :: Term -> Text
prettyTerm = go 0
  where
    -- prec: 親文脈の結合度。 子の演算子優先度が親より緩ければ括弧。
    go :: Int -> Term -> Text
    go _ (Lit d)     = prettyNum d
    go _ (Ref x)     = x
    go _ (App f as)  = f <> "(" <> T.intercalate ", " (map (go 0) as) <> ")"
    go p (Index a b) = paren (p > 6) (go 6 a <> " ! " <> go 7 b)
    -- operand は prec 5 で描く: 連続前置 (Neg (Neg …) = "-(-…)") も括弧化され parse 可能に。
    go p (Neg a)     = paren (p > 4) ("-" <> go 5 a)
    go p (Bin op a b) =
      let pr = binPrec op
          (lp, rp) = case op of
            Pow -> (pr + 1, pr)        -- 右結合
            _   -> (pr, pr + 1)        -- 左結合
      in paren (p > pr) (go lp a <> " " <> binSym op <> " " <> go rp b)

    paren True  s = "(" <> s <> ")"
    paren False s = s

binPrec :: BinOp -> Int
binPrec Add = 1
binPrec Sub = 1
binPrec Mul = 2
binPrec Div = 2
binPrec Pow = 5

binSym :: BinOp -> Text
binSym Add = "+"
binSym Sub = "-"
binSym Mul = "*"
binSym Div = "/"
binSym Pow = "^"

-- | [日本語]: 整数値は小数点無しで (round-trip 安定)。
--   [English]: Integer-valued numbers are rendered without a decimal
--   point (for round-trip stability).
prettyNum :: Double -> Text
prettyNum d
  | d == fromIntegral n = T.pack (show n)
  | otherwise           = T.pack (show d)
  where n = round d :: Integer
