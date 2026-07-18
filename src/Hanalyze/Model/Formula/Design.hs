{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Hanalyze.Model.Formula.Design
-- Description : Formula DSL の設計行列組み立て (designMatrixF) + 線形性/識別性検出
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Formula DSL — designMatrixF + 線形性検出 + 識別性 (A17)。
--   'ModelFrame' から OLS 用の設計行列を組み立て、 線形モデルなら 'fitLMF' で fit する。
--
--   ★中核の考え方:
--     - 右辺を加法項に分解し、 各項を乗法葉 (param / factor 添字 / data 式) に分類。
--     - **線形 OLS では parameter 名自体は fit に効かない** (各設計列に 1 係数が付くだけ)。
--       param 名が効くのは ① 報告 ② 非線形検出。 → param が data 式の内側に現れたら
--       「非線形 (OLS 不可)」 として Left を返す = 線形性検出を兼ねる。
--     - factor は **使われ方 (! 添字)** で展開 ('ModelFrame' が既に判定済)。 識別性は
--       treatment contrast: 切片があれば参照水準 (=第1水準, 昇順先頭) を drop して満ランク化。
--     - 交互作用は専用演算子を持たず、 連続×連続=積・factor×連続=水準別列・factor×factor=
--       添字連鎖の grid 展開、 として加法項ごとに独立に列生成。
--
--   ★検証原理 (parameterization 不変): ŷ と R² は contrast の取り方に依らない。
--     飽和 factor×factor の ŷ = セル平均、 という Python 非依存オラクルで正しさを確認できる。
--
--   spline/poly 基底展開 (@bs ! bspline(x,k)@) は本 sub では未対応 (明示エラー)。 後続で配線。
module Hanalyze.Model.Formula.Design
  ( designMatrixF
  , fitLMF
  , responseVec
  , linearityCheck
    -- * Contrast coding (A2)
  , ContrastCoding (..)
  , contrastMatrix
  , parseContrast
    -- * weights / offset = WLS (A3)
  , WLSConfig (..)
  , defaultWLS
  , fitWLSF
  ) where

import           Data.Maybe              (catMaybes, isNothing)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Vector             as V
import qualified Numeric.LinearAlgebra   as LA

import           Hanalyze.DataIO.Convert    (getDoubleVec)
import           Hanalyze.DataIO.Preprocess (dropMissingRows)
import           Hanalyze.Model.Core     (FitResult)
import           Hanalyze.Model.LM       (fitLM)
import           Hanalyze.Model.Spline   (bsplineBasis, quantileKnots)
import           Hanalyze.Model.Formula  (BinOp (..), Formula (..), Term (..),
                                          prettyTerm)
import           Hanalyze.Model.Formula.Frame
import qualified DataFrame.Internal.DataFrame  as DX

-- ============================================================================
-- 加法 / 乗法への分解
-- ============================================================================

-- | 加法項に分解。 符号 (Sub/Neg) は係数に吸収され ŷ に効かないので Add 扱い。
flattenAdd :: Term -> [Term]
flattenAdd (Bin Add a b) = flattenAdd a ++ flattenAdd b
flattenAdd (Bin Sub a b) = flattenAdd a ++ flattenAdd b
flattenAdd (Neg a)       = flattenAdd a
flattenAdd t             = [t]

-- | 乗法葉に分解。
mulLeaves :: Term -> [Term]
mulLeaves (Bin Mul a b) = mulLeaves a ++ mulLeaves b
mulLeaves (Neg a)       = mulLeaves a
mulLeaves t             = [t]

-- | Index spine: 入れ子添字を (base項, [添字項]) に。 base が Ref でなければ Nothing。
indexSpine :: Term -> Maybe (Term, [Term])
indexSpine (Index a b) = do (base, ixs) <- indexSpine a; pure (base, ixs ++ [b])
indexSpine t           = Just (t, [])

-- ============================================================================
-- 乗法葉の分類
-- ============================================================================

data Leaf
  = LParam Text                       -- ^ パラメータ単独 (係数。 OLS 列は持たない)
  | LFactor [(Text, ContrastCoding)]  -- ^ factor 添字 + contrast (1 個=主効果 / 複数=交互作用)
  | LBasis Text [Term]                -- ^ 基底展開 (bs ! bspline(x,n) / bp ! poly(x,n))
  | LData Term                        -- ^ データ式 (連続変数・Lit・単項関数・算術)

-- | 基底関数名 (! の右に App として現れたら factor でなく基底展開)。
basisFns :: [Text]
basisFns = ["poly", "opoly", "bspline"]

classify :: ModelFrame -> Term -> Either String Leaf
classify mf leaf =
  case indexSpine leaf of
    Just (Ref _, [App f args]) | f `elem` basisFns -> Right (LBasis f args)
    Just (Ref _, ixs@(_:_))                        -> LFactor <$> mapM ixName ixs
    _ -> case leaf of
      Ref x
        | x `elem` mfParams mf -> Right (LParam x)
        -- 裸の factor = 主効果 (R 意味論 A17b: @y ~ … + g@ の @g@ が factor 列なら treatment
        --   contrast の主効果列。 @!@ 添字版 @bg!g@ と同一の LFactor に落とす)。
        | isFactor x           -> Right (LFactor [(x, Treatment)])
      _ -> Right (LData leaf)
  where
    -- 添字 → (factor 名, contrast)。 @Ref g@ = 無注釈 treatment、
    -- @C(g, coding)@ = contrast 注釈、 @C(g)@ = treatment。
    ixName (Ref x)
      | isFactor x = Right (x, Treatment)
      | otherwise  = Left $ "添字 '" <> T.unpack x <> "' は factor でなければなりません"
    ixName (App "C" (Ref x : rest))
      | isFactor x = (\c -> (x, c)) <$> codingOf rest
      | otherwise  = Left $ "C(...) の '" <> T.unpack x <> "' は factor でなければなりません"
    ixName (App f _) = Left $ "基底 '" <> T.unpack f
                              <> "' は factor 添字と混在できません (基底項は単独で)"
    ixName _         = Left "添字は変数名でなければなりません"
    codingOf []            = Right Treatment
    codingOf (Ref c : _)   = parseContrast c
    codingOf _             = Left "C(g, coding) の coding は名前でなければなりません"
    isFactor x = case lookup x (mfRoles mf) of
                   Just (RoleFactor _ _) -> True
                   _                     -> False

-- ============================================================================
-- データ式の評価 (パラメータが内側に出たら非線形)
-- ============================================================================

evalData :: ModelFrame -> Term -> Either String (V.Vector Double)
evalData mf t = case t of
  Lit d -> Right (V.replicate n d)
  Ref x -> case lookup x (mfRoles mf) of
    Just (RoleContinuous v) -> Right v
    Just (RoleResponse _)   -> Left $ "応答 '" <> T.unpack x <> "' をデータ式に使えません"
    Just (RoleFactor _ _)   -> Left $ "factor '" <> T.unpack x
                                       <> "' は ! で添字してください"
    Nothing
      | x `elem` mfParams mf -> Left $ "非線形: パラメータ '" <> T.unpack x
                                        <> "' がデータ式の内側に現れます (線形モデルでありません)"
      | otherwise            -> Left $ "未知の変数 '" <> T.unpack x <> "'"
  Neg a -> V.map negate <$> evalData mf a
  App f [a]
    | Just fn <- lookup f unaryFns -> V.map fn <$> evalData mf a
  App f _ -> Left $ "未対応の関数 '" <> T.unpack f
                     <> "' (A17 は log/exp/sqrt/sin/cos/tan/abs の単項のみ)"
  Bin op a b -> V.zipWith (binFn op) <$> evalData mf a <*> evalData mf b
  Index _ _  -> Left "添字項はデータ式に直接置けません (係数として扱われます)"
  where n = mfNRows mf

unaryFns :: [(Text, Double -> Double)]
unaryFns =
  [ ("log", log), ("exp", exp), ("sqrt", sqrt)
  , ("sin", sin), ("cos", cos), ("tan", tan), ("abs", abs) ]

binFn :: BinOp -> (Double -> Double -> Double)
binFn Add = (+)
binFn Sub = (-)
binFn Mul = (*)
binFn Div = (/)
binFn Pow = (**)

-- ============================================================================
-- 加法項 → 設計列
-- ============================================================================

-- | 切片項か (data も factor も無く param のみ → 1 の列)。
isInterceptTerm :: ModelFrame -> Term -> Bool
isInterceptTerm mf term =
  case mapM (classify mf) (mulLeaves term) of
    Right leaves -> not (null leaves)
                 && all isParam leaves
    _ -> False
  where isParam (LParam _) = True
        isParam _          = False

-- | 加法項 1 つの設計列群 (列ラベル, 列ベクトル)。
termColumns :: Bool -> ModelFrame -> Term -> Either String [(Text, V.Vector Double)]
termColumns hasInt mf term = do
  leaves <- mapM (classify mf) (mulLeaves term)
  let factorNames = concat [ fs | LFactor fs <- leaves ]
      dataLeaves  = [ d | LData d <- leaves ]
      basisLeaves = [ (f, a) | LBasis f a <- leaves ]
  case basisLeaves of
    [(f, a)]
      | null factorNames && null dataLeaves -> basisColumns hasInt mf f a
      | otherwise -> Left "基底項は単独で記述してください (factor/データ式との積は未対応)"
    (_ : _ : _) -> Left "1 項に複数の基底は未対応"
    [] -> do
      dataVec <- case dataLeaves of
                   [] -> Right (V.replicate (mfNRows mf) 1)
                   ts -> foldr1 (V.zipWith (*)) <$> mapM (evalData mf) ts
      let dataLabel | null dataLeaves = Nothing
                    | otherwise       = Just (T.intercalate "*" (map prettyTerm dataLeaves))
      case factorNames of
        [] -> Right [ (prettyTerm term, dataVec) ]
        fs -> factorColumns hasInt mf fs dataVec dataLabel

-- | 基底展開列。
--   - @poly(x,n)@ = x¹..xⁿ (n 列・定数なし。 切片は b0 が担う → polyDesignMatrix と同 span)。
--   - @bspline(x,n)@ = degree-3 clamped B-spline、 knots = quantileKnots n x
--     (= fitSpline (BSpline 3) (quantileKnots n x) と同一基底)。 既定 degree=3、
--     @bspline(x,n,k)@ で degree 指定可。 B-spline 基底は partition of unity ゆえ切片と
--     共線 → 切片併用時 (hasInt) は先頭基底列を drop して満ランク化 (R splines::bs 既定と同様)。
basisColumns :: Bool -> ModelFrame -> Text -> [Term]
             -> Either String [(Text, V.Vector Double)]
basisColumns hasInt mf fname args = case (fname, args) of
  ("poly", [xe, Lit nd]) -> do
    xv <- evalData mf xe
    let deg = round nd :: Int
    pure [ (lbl xe ("^" <> tshow j), V.map (^ j) xv) | j <- [1 .. deg] ]
  -- opoly(x,n) = 実測値の直交多項式 (R poly 既定・raw=FALSE と同 span)。
  --   Vandermonde [1, x, …, xⁿ] を QR 直交化し、 定数列を落とした 1..n 列を返す。
  --   raw poly と違い列が相互直交 (不等間隔でも linear ⊥ quadratic) ゆえ効果検定が独立。
  --   ŷ は raw poly と同一 (span 不変・parameterization のみ差)。
  ("opoly", [xe, Lit nd]) -> do
    xv <- evalData mf xe
    let deg    = round nd :: Int
        xs     = V.toList xv
        vand   = LA.fromLists [ [ x ^ p | p <- [0 .. deg] ] | x <- xs ]
        (q, _) = LA.qr vand
        qcols  = take deg (drop 1 (LA.toColumns q))  -- 定数列を除いた orthogonal 基底 (1..deg)
    pure [ (lbl xe ("^" <> tshow j), V.fromList (LA.toList c))
         | (j, c) <- zip [1 :: Int ..] qcols ]
  ("bspline", [xe, Lit nk])          -> bspl xe (round nk) 3
  ("bspline", [xe, Lit nk, Lit kk])  -> bspl xe (round nk) (round kk)
  _ -> Left $ "基底 '" <> T.unpack fname
              <> "' の引数形が不正 (poly(x,n) / bspline(x,n) / bspline(x,n,k))"
  where
    bspl xe nKnots deg = do
      xv <- evalData mf xe
      let mat     = bsplineBasis deg (quantileKnots nKnots xv) xv
          colsAll = map (V.fromList . LA.toList) (LA.toColumns mat)
          cols    = if hasInt then drop 1 colsAll else colsAll
      pure [ (lbl xe ("_" <> tshow j), c) | (j, c) <- zip [(1 :: Int) ..] cols ]
    lbl xe suf = fname <> "(" <> prettyTerm xe <> ")" <> suf
    tshow      = T.pack . show

-- | factor (1 個=主効果 / 複数=交互作用) を contrast 符号化で展開 (A2 一般化)。
--   ★各 factor の **contrast 行列 C** (k×m) で行を符号化する。 交互作用列は factor ごとの
--   contrast 列の **Kronecker 積** (各行で contrast 値の積) を取り、 data ベクトルを掛ける。
--   ★符号化の縮約は **指示列のとき (dataLabel == Nothing) のみ**: 指示列は合計が切片 (1s)
--   と共線ゆえ contrast 行列 (k×(k-1)) で 1 列落として満ランク化する。 一方 factor×連続
--   (dataLabel == Just、 masked データ列) は切片と共線でない → **full coding (k×k 単位行列)**
--   = 全水準保持で per-level の傾きを持つ (Phase 46 の masked 列罠を踏襲。 落とすと参照群の
--   傾きが 0 固定で自由度を失う = statsmodels の C(g):x と不一致)。 full coding では単位行列
--   ゆえ contrast の選択は ŷ に影響しない (= parameterization 不変)。
factorColumns :: Bool -> ModelFrame -> [(Text, ContrastCoding)] -> V.Vector Double -> Maybe Text
              -> Either String [(Text, V.Vector Double)]
factorColumns hasInt mf fcs dataVec dataLabel = do
  facs <- mapM getFac fcs
  let reduced = hasInt && isNothing dataLabel
      facCols = [ factorContrastCols reduced f | f <- facs ]  -- factor ごとの [(列ラベル, 行ベクトル)]
      combos  = cartesian facCols                              -- 交互作用 = 列の直積
  pure [ mkCol picks | picks <- combos ]
  where
    getFac (name, coding) = case lookup name (mfRoles mf) of
      Just (RoleFactor lev idx) -> Right (name, lev, idx, coding)
      _ -> Left $ "factor '" <> T.unpack name <> "' が ModelFrame にありません"
    mkCol picks =
      let prodVec = foldr1 (V.zipWith (*)) (map snd picks)   -- 各 factor の contrast 値の積
          col     = V.zipWith (*) prodVec dataVec
          lbl     = T.intercalate ":" (map fst picks ++ maybe [] (: []) dataLabel)
      in (lbl, col)

-- | 1 factor の contrast 列群。 reduced=True で contrast 行列 (k×(k-1))、 False で
--   full coding (k×k 単位行列 = 指示変数)。 各列は行ごとの contrast 値ベクトル。
factorContrastCols :: Bool -> (Text, [Text], V.Vector Int, ContrastCoding)
                   -> [(Text, V.Vector Double)]
factorContrastCols reduced (nm, lev, idx, coding) =
  let k      = length lev
      cmat   = if reduced then contrastMatrix coding k else LA.ident k
      m      = LA.cols cmat
      colVec j = V.map (\l -> cmat `LA.atIndex` (l, j)) idx
      lbl j
        | not reduced         = nm <> "=" <> (lev !! j)              -- full = 水準名 (指示)
        | coding == Treatment = nm <> "=" <> (lev !! (j + 1))        -- 参照 (水準0) を除く
        | otherwise           = nm <> "[" <> codingTag coding <> "." <> tshow j <> "]"
  in [ (lbl j, colVec j) | j <- [0 .. m - 1] ]
  where tshow = T.pack . show

cartesian :: [[a]] -> [[a]]
cartesian []       = [[]]
cartesian (xs:rest) = [ x : r | x <- xs, r <- cartesian rest ]

-- ============================================================================
-- Contrast coding (A2)
-- ============================================================================

-- | factor 符号化方式。 切片併用時に満ランク化する contrast。
data ContrastCoding
  = Treatment                    -- ^ 参照水準 (昇順先頭) を 0 に、 他を指示 (既定・R 既定 contr.treatment)
  | Sum                          -- ^ sum-to-zero (最終水準 = −Σ others、 R contr.sum)
  | Helmert                      -- ^ 各水準 vs それ以前の平均 (R contr.helmert)
  | Polynomial                   -- ^ ordered factor 用の直交多項式 (R contr.poly)
  | CustomContrast (LA.Matrix Double)  -- ^ ユーザ指定の k×(k-1) contrast 行列
  deriving (Eq, Show)

-- | contrast 名 (C(g, name) の name) を解釈。
parseContrast :: Text -> Either String ContrastCoding
parseContrast t = case T.toLower t of
  "treatment" -> Right Treatment
  "sum"        -> Right Sum
  "helmert"    -> Right Helmert
  "poly"       -> Right Polynomial
  "polynomial" -> Right Polynomial
  _ -> Left $ "未知の contrast '" <> T.unpack t
              <> "' (Treatment/Sum/Helmert/Polynomial)"

-- | 列ラベル用の短いタグ。
codingTag :: ContrastCoding -> Text
codingTag Treatment          = "T"
codingTag Sum                = "S"
codingTag Helmert            = "H"
codingTag Polynomial         = "P"
codingTag (CustomContrast _) = "C"

-- | k 水準の contrast 行列 (k×(k-1))。 切片併用時の満ランク符号化。
--   行 = 水準 (昇順 index)、 列 = contrast。 行 l の値が水準 l の設計行寄与。
contrastMatrix :: ContrastCoding -> Int -> LA.Matrix Double
contrastMatrix coding k = case coding of
  Treatment ->
    LA.fromLists [ [ if l == j + 1 then 1 else 0 | j <- [0 .. k - 2] ] | l <- [0 .. k - 1] ]
  Sum ->
    LA.fromLists [ sumRow l | l <- [0 .. k - 1] ]
  Helmert ->
    LA.fromLists [ [ helmert l j | j <- [0 .. k - 2] ] | l <- [0 .. k - 1] ]
  Polynomial       -> polyContrast k
  CustomContrast m -> m
  where
    sumRow l | l == k - 1 = replicate (k - 1) (-1)
             | otherwise  = [ if l == j then 1 else 0 | j <- [0 .. k - 2] ]
    helmert l j | l <= j      = -1
                | l == j + 1  = fromIntegral (j + 1)
                | otherwise   = 0

-- | 直交多項式 contrast (k×(k-1))。 中心化水準スコアの Vandermonde を QR 分解し
--   定数列を落とした直交基底 (R contr.poly と同 span。 符号差は ŷ 不変ゆえ無害)。
polyContrast :: Int -> LA.Matrix Double
polyContrast k =
  let xs    = map fromIntegral [1 .. k] :: [Double]
      xbar  = sum xs / fromIntegral k
      vand  = LA.fromLists [ [ (x - xbar) ^ p | p <- [0 .. k - 1] ] | x <- xs ]
      (q, _) = LA.qr vand
  in LA.fromColumns (drop 1 (LA.toColumns q))

-- ============================================================================
-- designMatrixF / fitLMF / linearityCheck
-- ============================================================================

-- | 'Formula' + 'ModelFrame' → 設計行列 (n×p) と列ラベル。 非線形なら Left。
designMatrixF :: Formula -> ModelFrame -> Either String (LA.Matrix Double, [Text])
designMatrixF (Formula _ _ rhs) mf = do
  let terms  = flattenAdd rhs
      hasInt = any (isInterceptTerm mf) terms
  colss <- mapM (termColumns hasInt mf) terms
  let cols   = concat colss
      labels = map fst cols
  if null cols
    then Left "空のモデル (設計列がありません)"
    else Right ( LA.fromColumns (map (LA.fromList . V.toList . snd) cols)
               , labels )

-- | 線形モデルを OLS で fit。 設計列ラベルも返す。 非線形なら Left。
fitLMF :: Formula -> DX.DataFrame -> Either String (FitResult, [Text])
fitLMF f df = do
  mf            <- modelFrame f df
  (x, labels)   <- designMatrixF f mf
  yv            <- responseVec mf
  let y = LA.asColumn (LA.fromList (V.toList yv))
  Right (fitLM x y, labels)

-- | 応答ベクトル取り出し。
responseVec :: ModelFrame -> Either String (V.Vector Double)
responseVec mf = case mfRoles mf of
  ((_, RoleResponse v) : _) -> Right v
  _                         -> Left "ModelFrame に応答列がありません"

-- ============================================================================
-- weights / offset = WLS (A3)
-- ============================================================================

-- | 重み付き最小二乗 + offset の設定。 statsmodels @smf.wls(formula, data, weights=…)@
--   に倣い、 weights/offset は **列名で渡す** (R でも weights は formula 外)。
data WLSConfig = WLSConfig
  { wcWeights :: Maybe Text  -- ^ 重み列名 (WLS。 'Nothing' = 等重み OLS)
  , wcOffset  :: Maybe Text  -- ^ offset 列名 (η への固定加算。 線形では @y* = y − offset@ を fit)
  }
  deriving (Eq, Show)

-- | 既定 (重みなし・offset なし = OLS、 'fitLMF' と等価)。
defaultWLS :: WLSConfig
defaultWLS = WLSConfig Nothing Nothing

-- | weights / offset 付きで線形モデルを fit。
--
--   ★行整列: 'modelFrame' は欠損 policy で行を落とし得るので、 weights/offset 列が frame と
--   ずれないよう **formula 関与列 ∪ weights ∪ offset をまとめて 'dropMissingRows'** してから
--   frame を組み、 weights/offset も同じ DataFrame から取り出す。
--   ★WLS = @√w@ で X/y を行スケール (@X' = diag(√w) X@, @y' = √w ⊙ y@) し OLS に帰着。
--   ★offset = η への固定加算ゆえ線形では @y − offset@ を解けばよい (GLM offset は別経路・未対応)。
fitWLSF :: WLSConfig -> Formula -> DX.DataFrame -> Either String (FitResult, [Text])
fitWLSF cfg f@(Formula resp dvars _) df0 = do
  let extra = catMaybes [wcWeights cfg, wcOffset cfg]
      df    = dropMissingRows (resp : dvars ++ extra) df0  -- 整列のため一括 drop
  mf          <- modelFrame f df
  (x, labels) <- designMatrixF f mf
  yv0         <- responseVec mf
  yv <- case wcOffset cfg of
          Nothing -> Right yv0
          Just oc -> do ov <- col df oc; Right (V.zipWith (-) yv0 ov)
  case wcWeights cfg of
    Nothing -> Right (fitLM x (asCol yv), labels)
    Just wc -> do
      wv <- col df wc
      let swv = LA.fromList (map sqrt (V.toList wv))            -- √w
          xw  = LA.fromColumns [ swv * c | c <- LA.toColumns x ] -- diag(√w) X
          yw  = swv * LA.fromList (V.toList yv)                  -- √w ⊙ y
      Right (fitLM xw (LA.asColumn yw), labels)
  where
    col d name = maybe (Left $ "WLS 列 '" <> T.unpack name <> "' が数値列として見つかりません")
                       Right (getDoubleVec name d)
    asCol v = LA.asColumn (LA.fromList (V.toList v))

-- | 線形性チェック (designMatrixF が通れば線形)。 メッセージ付き Either。
linearityCheck :: Formula -> DX.DataFrame -> Either String ()
linearityCheck f df = do
  mf <- modelFrame f df
  _  <- designMatrixF f mf
  Right ()
