-- |
-- Module      : Hanalyze.Model.Formula.Mixed
-- Description : Formula DSL の混合効果モデル (random effect) 接続層
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Formula DSL — 混合効果モデル (random effect) の接続層。
--
--   lme4 流の @(1|g)@ / @(x|g)@ / @(1+x|g)@ を Formula DSL に追加し、
--   'Hanalyze.Model.GLMM' の一般ランダム効果フィット ('fitLMEGeneral' /
--   'fitGLMMGeneral') へ route する。
--
--   ★設計判断: random 項を AST の @Term@ 構成子として持たせず、
--   __字句プリパスで @(…|g)@ ブロックを抽出__ する方式を採る。 理由は
--   @Term@ に構成子を足すと 'Hanalyze.Model.Formula' 系 5 モジュールの網羅
--   pattern match が全て破壊されるため (計画のリスク注記)。 本方式なら
--   @Term@/'Formula' は不変で、 固定効果は既存の 'parseModel'/'designMatrixF'
--   経路をそのまま使え、 random 項の解釈は本モジュールに閉じる。
--
--   frequentist GLMM ゆえ random 効果に prior 宣言は不要 (分散 G は推定対象)。
--
-- [English]: Formula DSL — connection layer for mixed-effects models
-- (random effects).
--
--   Adds lme4-style @(1|g)@ \/ @(x|g)@ \/ @(1+x|g)@ to the Formula DSL and
--   routes to 'Hanalyze.Model.GLMM''s general random-effects fitters
--   ('fitLMEGeneral' \/ 'fitGLMMGeneral').
--
--   ★__Design decision__: rather than giving random terms their own
--   @Term@ constructor, this module __extracts @(…|g)@ blocks in a lexical pre-pass__.
--   The reason: adding a constructor to @Term@ would break the
--   exhaustive pattern matches across the 5 modules of the
--   'Hanalyze.Model.Formula' family (noted as a risk in the plan). This
--   approach leaves @Term@\/'Formula' unchanged — fixed effects still go
--   through the existing 'parseModel'\/'designMatrixF' path, and random-term
--   interpretation stays contained in this module.
--
--   Since this is frequentist GLMM, random effects need no prior declaration
--   (the variance G is estimated).
module Hanalyze.Model.Formula.Mixed
  ( RandomSpec (..)
  , extractRandom
  , fitMixedF
  , fitMixedLME
  , fitMixedGLMM
  ) where

import           Control.Monad           (unless, when)
import           Data.Char               (isSpace)
import           Data.List               (intercalate)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Vector             as V
import qualified Numeric.LinearAlgebra   as LA

import qualified DataFrame.Internal.DataFrame as DXD
import           Hanalyze.DataIO.Convert      (getDoubleVec, getTextVec)
import           Hanalyze.DataIO.Preprocess   (dropMissingRows)
import           Hanalyze.Model.Formula       (Formula (..))
import           Hanalyze.Model.Formula.Design (designMatrixF, responseVec)
import           Hanalyze.Model.Formula.Frame  (modelFrame)
import           Hanalyze.Model.Formula.RFormula (parseModel)
import           Hanalyze.Model.GLM           (Family (..), LinkFn (..))
import           Hanalyze.Model.GLMM          (GLMMResultRE, buildGroups,
                                               fitGLMMGeneral, fitLMEGeneral)

-- ============================================================================
-- random 項の表現
-- ============================================================================

-- | [日本語]: 1 つの @(…|g)@ ブロックの解釈結果。
--   例: @(1+x|g)@ → @RandomSpec True ["x"] "g"@ / @(0+x|g)@ → @RandomSpec False ["x"] "g"@.
--   [English]: The parsed result of a single @(…|g)@ block.
--   Example: @(1+x|g)@ → @RandomSpec True ["x"] "g"@ \/ @(0+x|g)@ → @RandomSpec False ["x"] "g"@.
data RandomSpec = RandomSpec
  { rsIntercept :: Bool    -- ^ [日本語]: random intercept を含むか (@1@ あり or 既定 True、 @0@/@-1@ で抑制)。 [English]: Whether a random intercept is included (present via @1@ or default True; suppressed by @0@\/@-1@).
  , rsSlopes    :: [Text]  -- ^ [日本語]: random slope の変数名 (左辺の @1@/@0@/@-1@ 以外)。 [English]: Random-slope variable names (excluding @1@\/@0@\/@-1@ on the left-hand side).
  , rsGroup     :: Text    -- ^ [日本語]: grouping 変数名 (@|@ の右)。 [English]: Grouping variable name (right of @|@).
  } deriving (Eq, Show)

-- ============================================================================
-- 字句プリパス: (…|g) ブロックの抽出
-- ============================================================================

-- | [日本語]: formula 文字列から random 項 @(…|g)@ を抽出し、 (固定効果 formula, [RandomSpec])
--   を返す。 LHS (@~@ or @=@) は保持し、 RHS から random ブロックを取り除く。
--
--   - R 構文: @"y ~ x + (1+x|g)"@ → (@"y ~ x"@, [RandomSpec True ["x"] "g"])
--   - 独自構文: @"y x = b0 + b1*x + (1|g)"@ → (@"y x = b0 + b1*x"@, [RandomSpec True [] "g"])
--
--   固定効果側に項が残らない場合 (例 @"y ~ (1|g)"@) は intercept @"1"@ を補う。
--   [English]: Extracts random terms @(…|g)@ from a formula string and
--   returns (fixed-effects formula, [RandomSpec]). The LHS (@~@ or @=@) is
--   kept as-is; random blocks are stripped from the RHS.
--
--   - R syntax: @"y ~ x + (1+x|g)"@ → (@"y ~ x"@, [RandomSpec True ["x"] "g"])
--   - Custom syntax: @"y x = b0 + b1*x + (1|g)"@ → (@"y x = b0 + b1*x"@, [RandomSpec True [] "g"])
--
--   If no terms remain on the fixed-effects side (e.g. @"y ~ (1|g)"@), an
--   intercept @"1"@ is added.
extractRandom :: Text -> Either String (Text, [RandomSpec])
extractRandom t =
  let s = T.unpack t
      (lhs, sep, rhs) = splitLHS s
  in do
       tokens <- pure (splitTopPlus rhs)
       (fixedToks, specStrs) <- partitionTokens tokens
       specs <- mapM parseBlock specStrs
       let fixedRHS = case map trimStr (filter (not . all isSpace) fixedToks) of
                        [] -> "1"
                        ts -> intercalate " + " ts
           fixedFormula = case sep of
                            "" -> fixedRHS                       -- LHS 無し (RHS のみ)
                            _  -> trimStr lhs ++ " " ++ sep ++ " " ++ fixedRHS
       Right (T.pack fixedFormula, specs)

-- | [日本語]: LHS と RHS を @~@ (R) または @=@ (独自) で分割。 区切りが無ければ ("", "", whole)。
--   [English]: Splits into LHS and RHS on @~@ (R) or @=@ (custom). If no
--   separator is found, returns ("", "", whole).
splitLHS :: String -> (String, String, String)
splitLHS s
  | Just (l, r) <- breakTop '~' s = (l, "~", r)
  | Just (l, r) <- breakTop '=' s = (l, "=", r)
  | otherwise                     = ("", "", s)

-- | [日本語]: top-level (括弧外) の最初の区切り文字で 1 回分割。
--   [English]: Splits once at the first top-level (outside parentheses)
--   separator character.
breakTop :: Char -> String -> Maybe (String, String)
breakTop target = go (0 :: Int) []
  where
    go _ _   [] = Nothing
    go d acc (c:cs)
      | c == '('            = go (d+1) (c:acc) cs
      | c == ')'            = go (d-1) (c:acc) cs
      | c == target && d == 0 = Just (reverse acc, cs)
      | otherwise           = go d (c:acc) cs

-- | [日本語]: top-level の @+@ で分割 (括弧内の @+@ は分割しない)。
--   [English]: Splits on top-level @+@ (does not split on @+@ inside
--   parentheses).
splitTopPlus :: String -> [String]
splitTopPlus = go (0 :: Int) [] []
  where
    go _ cur acc [] = reverse (reverse cur : acc)
    go d cur acc (c:cs)
      | c == '('            = go (d+1) (c:cur) acc cs
      | c == ')'            = go (d-1) (c:cur) acc cs
      | c == '+' && d == 0  = go d [] (reverse cur : acc) cs
      | otherwise           = go d (c:cur) acc cs

-- | [日本語]: 各トークンを固定効果トークンか random ブロック (中身) に振り分ける。
--   random ブロック = trim 後 @(…)@ で囲まれ、 内部 top-level に @|@ を持つもの。
--   [English]: Sorts each token into either a fixed-effects token or a
--   random block (its contents). A random block is one that, after
--   trimming, is enclosed in @(…)@ and contains a top-level @|@ inside.
partitionTokens :: [String] -> Either String ([String], [String])
partitionTokens = go [] []
  where
    go fixed rand [] = Right (reverse fixed, reverse rand)
    go fixed rand (tok:rest) =
      case asRandomBlock (trimStr tok) of
        Just inner -> go fixed (inner : rand) rest
        Nothing    -> go (tok : fixed) rand rest

-- | [日本語]: トークンが @(…|…)@ なら内部文字列を返す。
--   [English]: If the token is @(…|…)@, returns the inner string.
asRandomBlock :: String -> Maybe String
asRandomBlock tok =
  case tok of
    ('(':rest) | not (null rest), last rest == ')' ->
      let inner = init rest
      in if hasTopPipe inner then Just inner else Nothing
    _ -> Nothing

-- | [日本語]: top-level に @|@ を含むか。
--   [English]: Whether it contains a top-level @|@.
hasTopPipe :: String -> Bool
hasTopPipe = go (0 :: Int)
  where
    go _ [] = False
    go d (c:cs)
      | c == '('          = go (d+1) cs
      | c == ')'          = go (d-1) cs
      | c == '|' && d == 0 = True
      | otherwise         = go d cs

-- | [日本語]: @"1 + x | g"@ → 'RandomSpec'。
--   [English]: @"1 + x | g"@ → 'RandomSpec'.
parseBlock :: String -> Either String RandomSpec
parseBlock inner =
  case breakTop '|' inner of
    Nothing       -> Left "random ブロックに '|' がありません"
    Just (lhs, rhs) ->
      let grp   = trimStr rhs
          terms = map trimStr (splitTopPlus lhs)
          isSup t = t == "0" || t == "-1"
          isOne t = t == "1"
          hasSup  = any isSup terms
          slopes  = [ T.pack t | t <- terms, not (isSup t), not (isOne t), not (null t) ]
      in if null grp
           then Left "random ブロックの grouping 変数 (| の右) が空です"
           else Right RandomSpec
                  { rsIntercept = not hasSup           -- 0/-1 が無ければ intercept あり
                  , rsSlopes    = slopes
                  , rsGroup     = T.pack grp
                  }

trimStr :: String -> String
trimStr = f . f where f = reverse . dropWhile isSpace

-- ============================================================================
-- route 入口: 固定/random を分離し GLMM 一般フィットへ
-- ============================================================================

-- | [日本語]: 混合効果モデルを DataFrame からフィットする。 @Nothing@ = Gaussian LME
--   ('fitLMEGeneral')、 @Just (family, link)@ = 非 Gaussian GLMM ('fitGLMMGeneral')。
--   戻り値は (結果, 固定効果係数名)。
--
--   ★現状は __単一 grouping factor__ のみ対応 ((1|g) / (x|g) / (1+x|g))。 複数の
--   @(…|g1) + (…|g2)@ は block-diagonal Z が要るため未対応 (明示エラー)。
--
--   TODO (follow-up):
--     - 複数 grouping factor @(…|g1) + (…|g2)@ — 群ごと Z ブロックを block-diagonal に
--       積み、 fitLMEGeneral/fitGLMMGeneral を multi-grouping 一般化する。
--     - GLMM offset (Poisson log-exposure 等) — 現状は線形 offset のみ (@fitWLSF@)。
--     - REML 推定 — 現状の EM/Laplace は ML。 REML は固定効果 df 補正付き。
--   [English]: Fits a mixed-effects model from a DataFrame. @Nothing@ =
--   Gaussian LME ('fitLMEGeneral'), @Just (family, link)@ = non-Gaussian GLMM
--   ('fitGLMMGeneral'). Returns (result, fixed-effect coefficient names).
--
--   ★Currently supports only a __single grouping factor__ ((1|g) \/ (x|g) \/
--   (1+x|g)). Multiple @(…|g1) + (…|g2)@ is not supported yet, since it
--   would require a block-diagonal Z (raises an explicit error).
--
--   TODO (follow-up):
--     - Multiple grouping factors @(…|g1) + (…|g2)@ — stack per-group Z
--       blocks block-diagonally and generalize fitLMEGeneral\/fitGLMMGeneral
--       to multi-grouping.
--     - GLMM offset (e.g. Poisson log-exposure) — currently only linear
--       offset is supported (@fitWLSF@).
--     - REML estimation — the current EM\/Laplace is ML. REML would add a
--       fixed-effect df correction.
fitMixedF
  :: Maybe (Family, LinkFn)
  -> Text -> DXD.DataFrame
  -> Either String (GLMMResultRE, [Text])
fitMixedF mfam formulaText df0 = do
  (fixedText, specs) <- extractRandom formulaText
  spec <- case specs of
            [s] -> Right s
            []  -> Left "random effect 項 (…|g) がありません (固定効果のみなら fitLMF を使用)"
            _   -> Left "複数の grouping factor は未対応 (単一の (…|g) のみ)"
  f@(Formula resp dvars _) <- parseModel fixedText
  let slopeVars = rsSlopes spec
      grp       = rsGroup spec
      -- 行整列: fixedWLF と同じく formula 関与列 ∪ slope ∪ group を一括 drop
      df        = dropMissingRows (resp : dvars ++ slopeVars ++ [grp]) df0
  mf          <- modelFrame f df
  (x, labels) <- designMatrixF f mf
  yv          <- responseVec mf
  let n = V.length yv
  slopeCols <- mapM (\v ->
                  maybe (Left $ "random slope 列 '" <> T.unpack v <> "' が数値列として見つかりません")
                        Right (getDoubleVec v df)) slopeVars
  let interceptCol = [ V.replicate n 1.0 | rsIntercept spec ]
      zCols        = interceptCol ++ slopeCols
  when (null zCols) $ Left "random effect の設計列が空です ((0|g) のみは不可)"
  unless (all ((== n) . V.length) zCols) $
    Left "random slope 列の長さが応答と一致しません"
  gv <- maybe (Left $ "grouping 列 '" <> T.unpack grp <> "' が見つかりません")
              Right (getTextVec grp df)
  let z = LA.fromColumns (map (LA.fromList . V.toList) zCols)
      y = LA.fromList (V.toList yv)
      (glabels, idx, _sizes) = buildGroups gv
      res = case mfam of
              Nothing          -> fitLMEGeneral x z y idx glabels
              Just (fam, link) -> fitGLMMGeneral fam link x z y idx glabels
  Right (res, labels)

-- | [日本語]: Gaussian 線形混合効果モデル (LME)。 @fitMixedLME "y ~ x + (1+x|g)" df@。
--   [English]: Gaussian linear mixed-effects model (LME).
--   @fitMixedLME "y ~ x + (1+x|g)" df@.
fitMixedLME :: Text -> DXD.DataFrame -> Either String (GLMMResultRE, [Text])
fitMixedLME = fitMixedF Nothing

-- | [日本語]: 非 Gaussian GLMM。 @fitMixedGLMM Binomial Logit "y ~ x + (1|g)" df@。
--   [English]: Non-Gaussian GLMM.
--   @fitMixedGLMM Binomial Logit "y ~ x + (1|g)" df@.
fitMixedGLMM :: Family -> LinkFn -> Text -> DXD.DataFrame
             -> Either String (GLMMResultRE, [Text])
fitMixedGLMM fam link = fitMixedF (Just (fam, link))
