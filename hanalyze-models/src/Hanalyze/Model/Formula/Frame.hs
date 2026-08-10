{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Hanalyze.Model.Formula.Frame
-- Description : Formula DSL の ModelFrame (変数役割割り当て + パラメータ分離)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Formula DSL — ModelFrame (A16)。 'Formula' AST + 'DataFrame' を突合し、
--   各名前に役割 (応答 / 連続データ変数 / factor) を割り当て、 推定パラメータを分離する。
--
--   ★設計の要点 (実測で確定): 「factor かどうか」 は **列の型ではなく formula 内の
--   使われ方** で決まる。 すなわち @bg ! group@ のように Index の右オペランドに現れた
--   データ変数を factor とみなす (numeric コードの factor も拾える)。 算術中にのみ現れる
--   データ変数は連続。 左辺で宣言されていない右辺の自由名 = 推定パラメータ。
--
--   基底展開 (@bs ! bspline(x,k)@) の設計行列化や係数ベクトル長の確定は A17
--   ('designMatrixF') に委ねる。 本モジュールは「役割の割り当てとパラメータ抽出」 まで。
--   DataFrame 依存ゆえ Formula.hs (純 AST) とは分離 (portable 区分は維持)。
module Hanalyze.Model.Formula.Frame
  ( VarRole (..)
  , ModelFrame (..)
  , MissingPolicy (..)
  , ImputeKind (..)
  , modelFrame
  , modelFrameWith
    -- * 内部 (テスト用に公開)
  , refNames
  , indexedVars
  ) where

import           Control.Applicative    ((<|>))
import           Data.List              (foldl', nub, sort)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Vector            as V
import qualified DataFrame.Internal.DataFrame  as DX

import           Hanalyze.DataIO.Convert    (getDoubleVec, getTextVec)
import           Hanalyze.DataIO.Preprocess (Value (..), countMissing, deriveText,
                                             dropMissingRows, imputeMean,
                                             imputeMedian, isNAString)
import           Hanalyze.Model.Formula  (Formula (..), Term (..))

-- ============================================================================
-- 役割付き列と ModelFrame
-- ============================================================================

-- | データ変数 (応答含む) の役割。
data VarRole
  = RoleResponse   (V.Vector Double)        -- ^ 応答 y (数値)
  | RoleContinuous (V.Vector Double)        -- ^ 連続説明変数 (数値)
  | RoleFactor     [Text] (V.Vector Int)    -- ^ factor: 水準ラベル (昇順) + 行ごとの水準 index
  deriving (Eq, Show)

-- | AST + data を突合した結果。
data ModelFrame = ModelFrame
  { mfRoles  :: [(Text, VarRole)]  -- ^ 応答 + データ変数 → 役割 (応答が先頭、 以降は宣言順)
  , mfParams :: [Text]             -- ^ 推定パラメータ (右辺自由名 − データ変数、 出現順)
  , mfNRows  :: Int                -- ^ 行数 (応答列の長さ)
  }
  deriving (Eq, Show)

-- | 欠損値の扱い方。 NA 検出・除去・補完は ModelFrame の **単一責務点** (spec §2.2)。
--   policy で整形した DataFrame を 'buildFrame' に通すことで、 各 fit 関数に
--   NA 検出を散らさず一元化する。
data MissingPolicy
  = DropRows           -- ^ NA を含む行を全関与列から除外 (listwise deletion、 既定・後方互換)。
  | Pairwise           -- ^ 線形 OLS では設計行列が成立しないので DropRows に縮退する
                       --   (相関等の別用途のために policy 値としては保持。 'fitLMF' 等は警告)。
  | Impute ImputeKind  -- ^ 連続説明変数を平均/中央値で補完。 応答・factor の NA は
                       --   別 policy 併用が要る (Impute では埋めない)。
  | TreatAsCategory    -- ^ factor 列の NA を独立水準 @"<NA>"@ として扱う。
  | ErrorOnMissing     -- ^ 関与列に NA があれば 'Left' (列名 + 件数つき)。
  deriving (Eq, Show)

-- | 'Impute' の補完方式。
data ImputeKind = ImputeMean | ImputeMedian
  deriving (Eq, Show)

-- ============================================================================
-- 解析ヘルパ (AST 走査)
-- ============================================================================

-- | 右辺に現れる全 Ref 名 (出現順、 重複あり)。
--   contrast 注釈 @C(g, Sum)@ は **factor 名 g のみ** を拾う (coding 名 "Sum" は
--   推定パラメータでもデータ変数でもないので除外)。
refNames :: Term -> [Text]
refNames t = case t of
  Ref x               -> [x]
  Lit _               -> []
  App "C" (Ref x : _) -> [x]                   -- contrast 注釈: factor 名のみ
  App _ as            -> concatMap refNames as -- 関数名 (App の Text) はパラメータでない
  Index a b           -> refNames a ++ refNames b
  Neg a               -> refNames a
  Bin _ a b           -> refNames a ++ refNames b

-- | Index の右オペランドに factor として現れた名前 (= factor 候補)。
--   右が @Ref g@ (無注釈 = treatment) または @C(g, coding)@ (contrast 注釈) なら g を拾う。
--   右が基底展開 App (bspline / poly 等) の場合は factor でない (A17 が扱う) ので拾わない。
indexedVars :: Term -> [Text]
indexedVars = nub . go
  where
    go t = case t of
      Index a b -> rightRef b ++ go a ++ go b
      App _ as  -> concatMap go as
      Neg a     -> go a
      Bin _ a b -> go a ++ go b
      _         -> []
    rightRef (Ref x)               = [x]
    rightRef (App "C" (Ref x : _)) = [x]       -- C(g, coding) → factor g
    rightRef _                     = []

-- ============================================================================
-- modelFrame
-- ============================================================================

-- | 既定 policy ('DropRows') で 'ModelFrame' を構築する (後方互換: NA 無しデータでは不変)。
modelFrame :: Formula -> DX.DataFrame -> Either String ModelFrame
modelFrame = modelFrameWith DropRows

-- | 欠損 'MissingPolicy' を指定して 'ModelFrame' を構築する。
--   policy で整形した DataFrame を 'buildFrame' に通す (NA 検出・除去・補完を一元化)。
modelFrameWith :: MissingPolicy -> Formula -> DX.DataFrame -> Either String ModelFrame
modelFrameWith policy fml@(Formula resp dvars rhs) df = do
  let involved = resp : dvars
      factors  = filter (`elem` dvars) (indexedVars rhs)
      conts    = filter (`notElem` factors) dvars       -- 連続説明変数 (factor 以外)
      naOf d c = maybe 0 id (lookup c (countMissing d))  -- 列 c の NA 件数
  df' <- case policy of
    DropRows -> Right (dropMissingRows involved df)
    Pairwise -> Right (dropMissingRows involved df)  -- 単一 frame では DropRows と同義
    ErrorOnMissing ->
      let bad = [ (T.unpack c, naOf df c) | c <- involved, naOf df c > 0 ]
      in if null bad then Right df
         else Left $ "ErrorOnMissing: 欠損のある関与列 " <> show bad
    Impute kind -> do
      df1 <- imputeCols kind conts df
      let stillBad = [ T.unpack c | c <- resp : factors, naOf df1 c > 0 ]
      if null stillBad then Right df1
        else Left $ "Impute は連続説明変数のみ補完します。 応答/factor の欠損 "
                    <> show stillBad <> " は DropRows か TreatAsCategory を併用してください"
    TreatAsCategory ->
      let df1      = foldl' (flip naToCategory) df factors
          stillBad = [ T.unpack c | c <- resp : conts, naOf df1 c > 0 ]
      in if null stillBad then Right df1
         else Left $ "TreatAsCategory は factor 列のみ扱います。 応答/連続の欠損 "
                     <> show stillBad <> " は DropRows か Impute を併用してください"
  buildFrame fml df'

-- | 連続列群を平均/中央値で補完。 数値列でなければ 'Left'。
imputeCols :: ImputeKind -> [Text] -> DX.DataFrame -> Either String DX.DataFrame
imputeCols kind = go
  where
    impute1 c = case kind of { ImputeMean -> imputeMean c; ImputeMedian -> imputeMedian c }
    go []     d = Right d
    go (c:cs) d = case impute1 c d of
      Just d' -> go cs d'
      Nothing -> Left $ "連続変数 '" <> T.unpack c <> "' を数値列として補完できません"

-- | factor 列の NA を独立水準 @"<NA>"@ に置換した Text 列で上書きする。
--   非 NA 値は 'showNum' で文字列化 ('columnAsText' の数値→文字列と同形)。
naToCategory :: Text -> DX.DataFrame -> DX.DataFrame
naToCategory c = deriveText c toLbl
  where
    toLbl row = case Map.lookup c row of
      Just (VText t) | not (isNAString t) -> t
      Just (VNum d)                       -> T.pack (showNum d)
      _                                   -> "<NA>"

-- | 'Formula' と (policy 適用済) 'DataFrame' を突合して 'ModelFrame' を構築する。
buildFrame :: Formula -> DX.DataFrame -> Either String ModelFrame
buildFrame (Formula resp dvars rhs) df = do
  -- 応答列 (数値必須)
  yv <- maybe (Left $ "応答変数 '" <> T.unpack resp <> "' が数値列として見つかりません")
              Right (getDoubleVec resp df)
  let n        = V.length yv
      indexed  = filter (`elem` dvars) (indexedVars rhs)
      -- R 意味論 (A17b): @!@ 添字が無くても **非数値 (Text) 列は factor** として扱う
      --   (character→factor 自動判定)。 数値列は連続のまま (numeric-coded factor は従来どおり
      --   @!@ 添字必須) なので、 従来 error だった「Text 列を裸で置いた」場合だけが factor 化する。
      autoFac  = [ v | v <- dvars, v `notElem` indexed, nonNumericText v ]
      factors  = indexed ++ autoFac
      params   = refNames rhs `minus` (resp : dvars)
      nonNumericText v = case getDoubleVec v df of
                           Just _  -> False
                           Nothing -> case getTextVec v df of
                                        Just _  -> True
                                        Nothing -> False
  -- 各データ変数の役割を解決
  varRoles <- mapM (resolveVar factors df) dvars
  pure ModelFrame
    { mfRoles  = (resp, RoleResponse yv) : zip dvars varRoles
    , mfParams = params
    , mfNRows  = n
    }

-- | データ変数 1 つを役割に解決する。 factors に含まれれば factor、 さもなくば連続。
resolveVar :: [Text] -> DX.DataFrame -> Text -> Either String VarRole
resolveVar factors df name
  | name `elem` factors = factorRole name df
  | otherwise           =
      maybe (Left $ "連続変数 '" <> T.unpack name <> "' が数値列として見つかりません")
            (Right . RoleContinuous) (getDoubleVec name df)

-- | factor 列を水準ラベル (昇順) + 行ごとの水準 index に。
--   text 列を優先、 無ければ数値列を文字列化 (numeric コードの factor)。
factorRole :: Text -> DX.DataFrame -> Either String VarRole
factorRole name df =
  case columnAsText name df of
    Nothing  -> Left $ "factor 変数 '" <> T.unpack name <> "' が列として見つかりません"
    Just col ->
      let levels = sort (nub (V.toList col))           -- 昇順 = treatment contrast の参照=第1水準
          idxOf v = length (takeWhile (/= v) levels)    -- levels 内の位置
          idx    = V.map idxOf col
      in Right (RoleFactor levels idx)

-- | 列を [Text] 表現で取得 (factor 水準列挙用)。 text 列優先、 無ければ数値を文字列化。
columnAsText :: Text -> DX.DataFrame -> Maybe (V.Vector Text)
columnAsText name df =
      getTextVec name df
  <|> (V.map (T.pack . showNum) <$> getDoubleVec name df)

-- | 数値を factor 水準ラベル用に文字列化 (整数は小数点なし)。
showNum :: Double -> String
showNum d
  | d == fromIntegral i = show i
  | otherwise           = show d
  where i = round d :: Integer

-- | リスト差 (左の出現順を保ち、 右に含まれる要素を除く)。
minus :: Eq a => [a] -> [a] -> [a]
minus xs ys = foldl' (\acc x -> if x `elem` ys || x `elem` acc then acc else acc ++ [x]) [] xs
