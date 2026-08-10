{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Hanalyze.Model.Formula.RFormula
-- Description : Formula DSL の R/patsy 互換 front-end (@y ~ x + C(g)@ 構文)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Formula DSL — R/patsy front-end (A18)。 @y ~ x + C(g)@ 形式を
--   __同じ 'Formula' AST__ に落とす (サブ front-end)。 正本は独自構文 (A15)、
--   本モジュールは互換・オラクル用途。
--
--   ★dispatch: 文字列に @~@ が含まれれば R、 無ければ独自 ('parseModel')。 @~@ と @=@ は
--   字句的に分離ゆえ曖昧性ゼロ。
--
--   ★R formula 意味論 → 我々の AST:
--     - @~@ で 応答 / 予測子 を分離。 予測子は @+@ 区切り (これは「項追加」、 算術でない)。
--     - 暗黙の切片あり。 @-1@ / @0@ で切片除去。
--     - 連続変数 @x@ → @b*x@ (本物の積)。 ★categorical は __@C(g)@__ で明示
--       (patsy 同様。 data 無しで parse するため列型推論はしない)。
--     - @a:b@ = 交互作用のみ、 @a*b@ = @a + b + a:b@ (crossing)。
--     - @I(expr)@ = 算術 (@x**2@/@x^2@ 等)、 @log(x)@ = 関数変換、 @poly(x,n)@/@bs(x,n)@ = 基底。
--   ★パラメータ名は合成 (@_p0,_p1,…@)。 線形 OLS では係数名は fit に無関係ゆえ問題なし。
--   ★data 変数は RHS に現れた変数名 (合成パラメータ以外) を収集。
--
--   plot 非依存・portable (AST のみ依存)。
--
-- [English]: Formula DSL — R\/patsy front-end (A18). Compiles the
--   @y ~ x + C(g)@ form down to the __same 'Formula' AST__ (a sub
--   front-end). The canonical syntax is the original one (A15); this module
--   is for compatibility\/oracle use.
--
--   ★Dispatch: if the string contains @~@, use R; otherwise use the
--   original ('parseModel'). @~@ and @=@ are lexically distinct, so there is
--   zero ambiguity.
--
--   ★R formula semantics → our AST:
--     - @~@ separates the response \/ predictors. Predictors are @+@
--       separated (this is "term addition," not arithmetic).
--     - There is an implicit intercept. @-1@ \/ @0@ removes the intercept.
--     - A continuous variable @x@ → @b*x@ (a genuine product). ★categorical
--       is made explicit with __@C(g)@__ (as in patsy; since parsing happens
--       without data, there is no column-type inference).
--     - @a:b@ = interaction only, @a*b@ = @a + b + a:b@ (crossing).
--     - @I(expr)@ = arithmetic (@x**2@\/@x^2@ etc.), @log(x)@ = function
--       transform, @poly(x,n)@\/@bs(x,n)@ = basis.
--   ★Parameter names are synthesized (@_p0,_p1,…@). For linear OLS the
--   coefficient names are irrelevant to the fit, so this is not a problem.
--   ★Data variables are collected from the variable names appearing on the
--   RHS (excluding synthesized parameters).
--
--   Plot-independent, portable (depends only on the AST).
module Hanalyze.Model.Formula.RFormula
  ( parseRFormula
  , parseModel
  ) where

import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Data.List                      (isPrefixOf, nub, subsequences)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Void                      (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char           (alphaNumChar, char, letterChar,
                                                 space1)
import qualified Text.Megaparsec.Char.Lexer     as L

import           Hanalyze.Model.Formula          (BinOp (..), Formula (..),
                                                  Term (..), parseFormula)

-- ============================================================================
-- dispatch
-- ============================================================================

-- | [日本語]: front-end 自動判別: @~@ を含めば R、 さもなくば独自構文。
--   [English]: Automatic front-end detection: R if it contains @~@,
--   otherwise the original syntax.
parseModel :: Text -> Either String Formula
parseModel t
  | T.any (== '~') t = parseRFormula t
  | otherwise        = parseFormula t

-- ============================================================================
-- 字句
-- ============================================================================

type P = Parsec Void Text

sc :: P ()
sc = L.space space1 empty empty

lexeme :: P a -> P a
lexeme = L.lexeme sc

symbol :: Text -> P Text
symbol = L.symbol sc

ident :: P Text
ident = lexeme $ do
  c  <- letterChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_' <|> char '.')
  pure (T.pack (c : cs))

intLit :: P Int
intLit = lexeme (L.signed (pure ()) L.decimal)

numLit :: P Double
numLit = lexeme (try (L.signed (pure ()) L.float)
                 <|> (fromIntegral <$> L.signed (pure ()) (L.decimal :: P Integer)))

parens :: P a -> P a
parens = between (symbol "(") (symbol ")")

-- ============================================================================
-- 中間表現 (R 項)
-- ============================================================================

-- | [日本語]: R 項の因子。
--   [English]: A factor within an R term.
data RFactor
  = RVar  Text             -- ^ [日本語]: 連続変数 x。 [English]: Continuous variable x.
  | RCat  Text (Maybe Text) -- ^ [日本語]: C(g) / C(g, Sum) categorical (+ contrast 名)。 [English]: C(g) \/ C(g, Sum) categorical (with an optional contrast name).
  | RFun  Text Term        -- ^ [日本語]: log(x) 等の関数変換 (1 引数)。 [English]: A function transform such as log(x) (1 argument).
  | RI    Term        -- ^ [日本語]: I(expr) 算術。 [English]: I(expr) arithmetic.
  | RPoly Text Int    -- ^ [日本語]: poly(x, n)   生べき (x¹..xⁿ)。 [English]: poly(x, n), raw powers (x¹..xⁿ).
  | ROPoly Text Int   -- ^ [日本語]: opoly(x, n)  実測値の直交多項式 (R poly 既定と同じ)。 [English]: opoly(x, n), orthogonal polynomials on the observed values (same as R's poly default).
  | RBs   Text Int    -- ^ [日本語]: bs(x, n)。 [English]: bs(x, n).

-- | [日本語]: R 項: 数値 (0/1) か、 因子の積 (hasStar=True なら crossing 展開)。
--   [English]: An R term: either a number (0\/1) or a product of factors
--   (crossing expansion when hasStar=True).
data RComp = RNum Int | RProd Bool [RFactor]

-- ============================================================================
-- パーサ
-- ============================================================================

-- | [日本語]: @lhs ~ rhs@。
--   [English]: @lhs ~ rhs@.
pRFormula :: P Formula
pRFormula = do
  sc
  lhs   <- ident
  _     <- symbol "~"
  comps <- pRHS
  eof
  buildFormula lhs comps

-- | [日本語]: RHS = 符号付き項の並び。 戻り値 = (符号, 項)。
--   [English]: RHS = a sequence of signed terms. Return value = (sign, term).
pRHS :: P [(Int, RComp)]
pRHS = do
  s0 <- option 1 sign
  c0 <- pComp
  rest <- many ((,) <$> sign <*> pComp)
  pure ((s0, c0) : rest)
  where sign = (1 <$ symbol "+") <|> ((-1) <$ symbol "-")

-- | [日本語]: 1 項 (数値 or 因子の積)。
--   [English]: A single term (a number or a product of factors).
pComp :: P RComp
pComp =
      try (RNum <$> lexeme L.decimal)
  <|> pProduct

-- | [日本語]: 因子を @*@ / @:@ で結んだ積。 @*@ が 1 つでもあれば crossing。
--   [English]: A product of factors joined by @*@ \/ @:@. Crossing if there
--   is at least one @*@.
pProduct :: P RComp
pProduct = do
  f0 <- pFactor
  rest <- many ((,) <$> ((True <$ symbol "*") <|> (False <$ symbol ":")) <*> pFactor)
  let hasStar = any fst rest
      facs    = f0 : map snd rest
  pure (RProd hasStar facs)

pFactor :: P RFactor
pFactor =
      try (symbol "C" *> parens pCatArgs)
  <|> try (RI    <$> (symbol "I"  *> parens pArith))
  <|> try (ROPoly <$> (symbol "opoly" *> symbol "(" *> ident) <*> (symbol "," *> intLit <* symbol ")"))
  <|> try (RPoly <$> (symbol "poly" *> symbol "(" *> ident) <*> (symbol "," *> intLit <* symbol ")"))
  <|> try (RBs   <$> (symbol "bs"   *> symbol "(" *> ident) <*> (symbol "," *> intLit <* symbol ")"))
  <|> try pFunOrVar

-- | [日本語]: @C(g)@ / @C(g, Sum)@ の中身: factor 名 + 省略可能な contrast 名。
--   [English]: The contents of @C(g)@ \/ @C(g, Sum)@: a factor name plus an
--   optional contrast name.
pCatArgs :: P RFactor
pCatArgs = do
  g     <- ident
  mcode <- optional (symbol "," *> ident)
  pure (RCat g mcode)

-- | [日本語]: @log(x)@ のような関数変換、 または裸の変数。
--   [English]: A function transform like @log(x)@, or a bare variable.
pFunOrVar :: P RFactor
pFunOrVar = do
  nm <- ident
  margs <- optional (parens pArith)
  pure $ case margs of
    Just a  -> RFun nm a
    Nothing -> RVar nm

-- | [日本語]: I(...) 内の算術式 (@+ - * / ^ **@・関数適用・括弧)。
--   [English]: The arithmetic expression inside I(...) (@+ - * / ^ **@,
--   function application, parentheses).
pArith :: P Term
pArith = makeExprParser pArithApp
  [ [ InfixR (Bin Pow <$ (symbol "**" <|> symbol "^")) ]
  , [ Prefix (Neg     <$ symbol "-") ]
  , [ InfixL (Bin Mul <$ symbol "*"), InfixL (Bin Div <$ symbol "/") ]
  , [ InfixL (Bin Add <$ symbol "+"), InfixL (Bin Sub <$ symbol "-") ]
  ]

pArithApp :: P Term
pArithApp = do
  h <- pArithAtom
  case h of
    Ref f -> do
      margs <- optional (parens (pArith `sepBy1` symbol ","))
      pure $ maybe h (App f) margs
    _ -> pure h

pArithAtom :: P Term
pArithAtom =
      (Lit <$> numLit)
  <|> parens pArith
  <|> (Ref <$> ident)

-- ============================================================================
-- 構築 (中間表現 → Formula AST)
-- ============================================================================

buildFormula :: Text -> [(Int, RComp)] -> P Formula
buildFormula lhs comps = do
  let removeInt = any (\(s, c) -> case c of
                         RNum 0 -> s == 1            -- + 0
                         RNum 1 -> s == (-1)         -- - 1
                         _      -> False) comps
      prods = [ p | (_, RProd star fs) <- comps, p <- expand star fs ]
      terms = (if removeInt then [] else [const1]) ++ map prodToTerm prods
  if null terms
    then fail "R formula: 項がありません"
    else do
      let named   = zipWith (\i mk -> mk (synth i)) [0 :: Int ..] terms
          rhs     = foldr1 (Bin Add) named
          dvars   = nub (filter (not . isSynth) (refNamesT rhs))
      pure (Formula lhs dvars rhs)
  where
    synth i  = T.pack ("_p" ++ show i)
    const1 p = Ref p                                  -- 切片 (定数項)

-- | [日本語]: crossing 展開: @*@ なら全非空部分集合 (R の a*b = a + b + a:b)、 @:@ なら
--   単一交互作用。 列の順序は fit (ŷ) に無関係ゆえ 'subsequences' の順序で可。
--   [English]: Crossing expansion: for @*@, all non-empty subsets (R's
--   a*b = a + b + a:b); for @:@, a single interaction. Since column order is
--   irrelevant to the fit (ŷ), the order from 'subsequences' is fine as-is.
expand :: Bool -> [RFactor] -> [[RFactor]]
expand False fs = [fs]
expand True  fs = filter (not . null) (subsequences fs)

-- | [日本語]: 1 つの積 (因子リスト) → パラメータ名を取って Term を作る関数。
--   [English]: A single product (a list of factors) → a function that takes
--   a parameter name and produces a Term.
prodToTerm :: [RFactor] -> (Text -> Term)
prodToTerm facs p =
  let cats   = [ (nm, mc) | RCat nm mc <- facs ]
      polys  = [ (nm, n) | RPoly nm n <- facs ]
      opolys = [ (nm, n) | ROPoly nm n <- facs ]
      bss    = [ (nm, n) | RBs   nm n <- facs ]
      datums = concatMap factorData facs
  in case (polys, opolys, bss) of
       ((nm, n) : _, _, _) -> Index (Ref p) (App "poly"    [Ref nm, Lit (fromIntegral n)])
       (_, (nm, n) : _, _) -> Index (Ref p) (App "opoly"   [Ref nm, Lit (fromIntegral n)])
       (_, _, (nm, n) : _) -> Index (Ref p) (App "bspline" [Ref nm, Lit (fromIntegral n)])
       _ ->
         let base = foldl (\acc (nm, mc) -> Index acc (catTerm nm mc)) (Ref p) cats
         in case datums of
              []     -> base                          -- 切片 or 純 factor
              (d:ds) -> Bin Mul base (foldl (Bin Mul) d ds)

-- | [日本語]: categorical 添字項を AST に: @C(g)@ → @Ref g@ (無注釈 treatment)、
--   @C(g, Sum)@ → @App "C" [Ref g, Ref Sum]@ (contrast 注釈・正本 AST と同形)。
--   [English]: Turns a categorical index term into the AST: @C(g)@ →
--   @Ref g@ (unannotated treatment), @C(g, Sum)@ →
--   @App "C" [Ref g, Ref Sum]@ (contrast annotation, same shape as the
--   canonical AST).
catTerm :: Text -> Maybe Text -> Term
catTerm nm Nothing  = Ref nm
catTerm nm (Just c) = App "C" [Ref nm, Ref c]

-- | [日本語]: 因子のデータ式部分 (連続/関数/I)。 factor/basis はここに出さない。
--   [English]: The data-expression part of a factor (continuous \/
--   function \/ I). Factor\/basis are not emitted here.
factorData :: RFactor -> [Term]
factorData (RVar x)   = [Ref x]
factorData (RFun f a) = [App f [a]]
factorData (RI t)     = [t]
factorData _          = []

-- | [日本語]: 合成パラメータ名か。
--   [English]: Whether this is a synthesized parameter name.
isSynth :: Text -> Bool
isSynth n = "_p" `isPrefixOf` T.unpack n

-- | [日本語]: Term 中の Ref 名 (data 変数収集用)。
--   [English]: Ref names within a Term (for collecting data variables).
refNamesT :: Term -> [Text]
refNamesT t = case t of
  Ref x               -> [x]
  Lit _               -> []
  App "C" (Ref x : _) -> [x]                     -- contrast 注釈: factor 名のみ (coding 名は除外)
  App _ as            -> concatMap refNamesT as
  Index a b           -> refNamesT a ++ refNamesT b
  Neg a               -> refNamesT a
  Bin _ a b           -> refNamesT a ++ refNamesT b

-- | [日本語]: 文字列 → 'Formula' (R front-end)。
--   [English]: String → 'Formula' (R front-end).
parseRFormula :: Text -> Either String Formula
parseRFormula txt = case parse pRFormula "<r-formula>" txt of
  Left e  -> Left (errorBundlePretty e)
  Right f -> Right f
