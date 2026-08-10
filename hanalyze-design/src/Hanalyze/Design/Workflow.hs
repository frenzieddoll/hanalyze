{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Design.Workflow
-- Description : DOE ワークフロー層 — 低レベル設計関数を設計オブジェクト Design に束ねる R 流の対話的入口
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- DOE ワークフロー層 (Phase 78) — 散在する低レベル設計関数 (`Design.Factorial`/`RSM` 等・
--   生 @[[Double]]@ 返し) を **設計オブジェクト `Design`** に束ね、 R 流の対話的ワークフローに
--   載せる玄関。
--
--   * `factorialDesign` / `centralCompositeDesign` — 因子 (名前 + 実値の下限/上限) から `Design` を作る
--     pure コンストラクタ。 `Design` は **coded 設計行列 + モデル formula の含意**を運ぶ。
--   * `designTable` — 実行用の **runsheet** (uncoded 実値・因子名列 + run 番号) を出す。
--     戻り値 @[(Text,[Double])]@ は 'ColumnSource' ゆえそのまま @df |->@ にも載る。
--   * `designFormula` — 設計種別からモデル formula を生成 (要因計画 = 全交互作用
--     @y ~ x1 * x2 * …@、 RSM = 2 次 @y ~ x1 + x2 + x1:x2 + I(x1^2) + I(x2^2)@)。
--
--   解析 (`designModel`) は `Hanalyze.Fit` 側 (formula → 既存 LM 当てはめ)。
--   ★coded/uncoded の要点: fit を coded でやるか uncoded でやるかは**予測に影響しない**
--   (同一項の LM は再パラメータ化・予測/R²/profiler は同値)。 だから fit は uncoded (自然単位)
--   のまま — 係数がそのまま実単位で読める。 coding が実質的に効くのは**スケール依存な最適化幾何**
--   (停留点方向・canonical 軸・steepest ascent 方向) だけ。 そこは `rsmAnalysis` /
--   `steepestAscentNatural` が内部で coded の計量を使い、 結果を**自然単位で報告**する
--   (Phase 78.G-d)。 runsheet は一貫して実験者向けの uncoded 実値。
module Hanalyze.Design.Workflow
  ( -- * 設計オブジェクト
    DesignFactor (..)
  , FactorKind (..)
  , FactorScale (..)
  , DesignKind (..)
  , Design (..)
    -- * 因子の smart constructor (連続 / 数値順序 / カテゴリ)
  , contFactor
  , contFactorLog
  , numFactor
  , catFactor
    -- * コンストラクタ (pure)
  , factorialDesign
  , centralCompositeDesign
  , boxBehnkenDesign
    -- * 一部実施要因 (run 削減) — Phase 78.G
  , Resolution (..)
  , resNum
  , fractionalDesign
  , fractionalDesignGen
  , fractionalDesignInter
  , fractionalDesignGenInter
  , fractionalCatalog
  , fracResolution
  , aliasStructure
    -- * Taguchi 直交表 (2 水準スクリーニング) — Phase 78.G-a
  , OATable (..)
  , taguchiDesign
  , taguchiDesignOA
    -- * 最適計画 (D/A/I/E/G-最適・カスタム formula) — Phase 78.G-b1
  , OptCriterion (..)
  , optimalDesign
  , optimalDesignWith
  , optimalDesignLevels
    -- ** モデル指定 効果 DSL (Formula の糖衣)
  , mainEffects
  , twoWay
  , quadratic
    -- ** 効果 DSL / Formula → Custom.Model 変換 (Phase 78.M M3)
  , formulaToCustomModel
    -- * 完全カスタムデザインエンジン (pure・座標交換 / 階層構造 / 制約) — Phase 79
  , CustomSpec (..)
  , customSpec
  , customDesign
  , Structure (..)
  , splitPlot
  , stripPlot
  , blocked
  , Constraint (..)
  , ConstraintRel (..)
  , ConstraintGuard (..)
  , FactorValue (..)
    -- ** 自然単位の制約 (推奨・Phase 82)
  , NatConstraint (..)
  , natLeq
  , natGeq
  , natEq
  , natForbid
    -- * 取り出し
  , designFactorNames
  , designTable
  , designFrame
  , designFrameRound
  , designFormula
    -- * 応答曲面 解析 (自然単位で報告) — Phase 78.G-d
  , RSMNature (..)
  , RSMReport (..)
  , rsmAnalysis
  , steepestAscentNatural
    -- * 設計の保存 / DataFrame からの復元 — Phase 78.K
  , saveDesign
  , planFromFrame
  ) where

import           Data.List (sort, subsequences, (\\), foldl1', nub, minimumBy, elemIndex, transpose, find)
import           Data.Ord (comparing)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified DataFrame.Internal.Column    as DX
import qualified DataFrame.Internal.DataFrame  as DX
import qualified DataFrame.IO.CSV as DXIO
import qualified Numeric.LinearAlgebra as LA

import           Hanalyze.DataIO.Convert (getDoubleVec, getTextVec)

import           Hanalyze.Design.Custom.Constraint
                   (Constraint (..), ConstraintRel (..), ConstraintGuard (..), FactorValue (..))
import qualified Hanalyze.Design.Custom.Factor as CF
import qualified Hanalyze.Design.Custom.Model  as CMd
import qualified Hanalyze.Design.Custom.Coordinate as CX
import qualified Hanalyze.Design.Custom.Structured as ST
import qualified Data.Vector.Storable as VS
import           Hanalyze.Design.Factorial (fullFactorial, fractionalFactorial)
import           Hanalyze.Design.RSM
                   ( centralCompositeRotatable, boxBehnken
                   , QuadFit (..), fitQuadratic, optimumPoint, canonicalAnalysis )
import           Hanalyze.Design.Sequential
                   (SteepestAscentResult (..), steepestAscentFromQuad)
import           Hanalyze.Design.Orthogonal
                   (OA (..), l4, l8, l9, l12, l16, l18, l27)
import           Hanalyze.Design.Optimal   (OptCriterion (..))
import qualified Hanalyze.Design.Optimal as OPT
import           Hanalyze.Model.Formula
                   (Formula (..), Term (..), BinOp (..), prettyFormula)
import           Hanalyze.Model.Formula.RFormula (parseRFormula)
import           Hanalyze.Model.Formula.Frame    (modelFrame)
import           Hanalyze.Model.Formula.Design   (designMatrixF)

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | DOE 因子 (Phase 78.G-b2)。 **識別子** ('dfName') と **性質** ('dfKind') を分離し、
--   accessor は全て total (partial field を作らない)。 構築は smart constructor
--   'contFactor' / 'numFactor' / 'catFactor' で行う。
--
--   Phase 79: 因子は純粋に因子であり、 どの因子がどの階層 (whole-plot / block) に属するかは
--   因子ではなく 'CustomSpec' の 'Structure' が **名前で** 持つ (役割 'dfRole' は撤去)。
data DesignFactor = DesignFactor
  { dfName :: !Text          -- ^ 因子名 (runsheet の列名・formula の項)
  , dfKind :: !FactorKind    -- ^ 連続 ('Cont') / 数値順序 ('Num') / カテゴリ ('Cat')
  } deriving (Show, Eq)

-- | 連続因子の**スケール** (Phase 82.3)。 coded @[-1,1]@ 軸を自然単位へどう写すか。
--
--   * 'SLinear' — 線形。 @nat = center + coded·half@ (既定)。
--   * 'SLog'    — 対数 (幾何)。 @nat = 10^(logCenter + coded·logHalf)@。 桁が大きく違う
--     因子 (触媒濃度 0.01〜10 等) の水準・中心点を幾何的に等間隔にする。 @lo, hi > 0@ 必須。
data FactorScale = SLinear | SLog
  deriving (Show, Eq)

-- | 因子の性質。 因子1つは連続・数値順序・カテゴリの**いずれか一つ**で、 混在不正状態は表現不能。
--
--   * 'Cont' — 2 端点連続 + スケール ('FactorScale')。 coded @-1@ = 下限、 @+1@ = 上限。
--     線形なら uncoded 実値 = @center + coded·halfRange@、 対数なら幾何 (下記 'FactorScale')。
--     Taguchi では 2 水準列に載る。
--   * 'Num'  — **数値順序水準リスト** (Phase 78.G-a2)。 3 水準以上の連続量 (温度 150/165/180 等) を
--     順序付き実値で持つ。 coded は水準リストの位置 index (@0,1,2,…@)、 runsheet/designFrame では
--     **実水準値** (Double) に戻る。 formula は **直交多項式** @opoly(name, 水準数−1)@
--     (linear+quadratic…) で載り、 実測間隔で直交分解する (等間隔前提を置かない)。
--   * 'Cat'  — カテゴリ (順序なし) 水準名リスト。 coded は位置 index、 runsheet では水準名 (Text)。
--     formula は主効果名 (engine が contrast 展開)。
data FactorKind
  = Cont !Double !Double !FactorScale  -- ^ 連続因子 (下限, 上限, スケール)
  | Num  ![Double]        -- ^ 数値順序因子 (順序付き水準値リスト) → opoly
  | Cat  ![Text]          -- ^ カテゴリ因子 (水準名リスト) → contrast
  deriving (Show, Eq)

-- | 連続因子の smart constructor (線形スケール)。 @contFactor "temp" (150, 180)@。
contFactor :: Text -> (Double, Double) -> DesignFactor
contFactor n (lo, hi) = DesignFactor n (Cont lo hi SLinear)

-- | **対数スケール**連続因子の smart constructor (Phase 82.3)。 @contFactorLog "conc" (0.01, 10)@。
--   coded 軸は従来通り @[-1,1]@ だが、 自然単位へは幾何的 (@10^…@) に写す。 水準・中心点が
--   幾何等間隔になり、 桁の異なる因子を扱える。 @lo, hi > 0@ 必須 (負/零は log 不能)。
contFactorLog :: Text -> (Double, Double) -> DesignFactor
contFactorLog n (lo, hi) = DesignFactor n (Cont lo hi SLog)

-- | 数値順序因子の smart constructor (Phase 78.G-a2)。 @numFactor "temp" [150, 165, 180]@。
--   3 水準以上の連続量を Taguchi 3 水準表 (L9/L18/L27) に載せ、 実測間隔の直交多項式
--   (@opoly@) で linear+quadratic 分解する。 実水準値をそのまま渡す (等間隔でなくてよい)。
numFactor :: Text -> [Double] -> DesignFactor
numFactor n levels = DesignFactor n (Num levels)

-- | カテゴリ因子の smart constructor。 @catFactor "catalyst" ["A", "B", "C"]@。
catFactor :: Text -> [Text] -> DesignFactor
catFactor n levels = DesignFactor n (Cat levels)

-- | 設計種別 (モデル formula の含意を決める)。
data DesignKind
  = KFactorial   -- ^ 要因計画 → 全交互作用モデル
  | KRSM         -- ^ 応答曲面 → 2 次モデル
  | KFractional  -- ^ 一部実施要因 → **主効果のみ** (交互作用は交絡ゆえ主効果限定)
  | KFracInter ![[Int]]
      -- ^ 一部実施要因 (**交互作用込み**・'fractionalDesignInter')。 generator を保持し、
      --   'designFormula' が主効果 + **主効果と交絡しない 2 因子交互作用の代表** (交絡群ごと 1 個) を
      --   生成する。 交絡構造は 'aliasStructure' で確認できる。
  | KCustom !Formula
      -- ^ 最適計画 ('optimalDesign') → モデル formula を**焼き込む**。 応答は placeholder
      --   ('formResponse') を持ち、 'designModel'/'designFormula' で実応答名に差し替わる。
  | KStructured ![(Text, [Int])] !Formula
      -- ^ 完全カスタムデザイン ('customDesign'・Phase 79)。 **群列** (@[(群列名, 各 run の群 ID)]@ の
      --   リスト) + 焼き込み formula を保持する。 CRD = @[]@、 SplitPlot = @[("wholePlot", ids)]@、
      --   StripPlot = @[("wholePlot", wpIds), ("strip", stripIds)]@、 Blocked = @[("block", ids)]@。
      --   'designFrame' は各群列を **Text ラベル** (@wp0…@ / @strip0…@ / @blk0…@) で追加し、
      --   'designModelHBM' @[ranIntercept 群列名, …]@ が階層効果として当てられる (round-trip)。
      --   固定効果 formula は 'KCustom' 同様 'designFormula' で応答名に差し替わる。
  deriving (Show, Eq)

-- | 設計オブジェクト = 因子 + coded 設計行列 (各行 = 1 run・列 = 因子) + 種別。
data Design = Design
  { dsFactors :: ![DesignFactor]
  , dsCoded   :: ![[Double]]      -- ^ coded 座標 (±1 / ±α / 0)
  , dsKind    :: !DesignKind
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Phase 79: 完全カスタムデザインエンジン (Structure / CustomSpec)
-- ---------------------------------------------------------------------------

-- | 実験のランダム化 / 階層構造 = 共分散 @M@ を決める。 総称 (v1 はエンジンが 4 種を実装)。
--   どの因子がどの層に属するかは因子 ('DesignFactor') ではなく **この構造が名前で持つ**。
--   'CustomSpec' の 'csStructure' に載る (既定 'CRD')。
--
--   * 'CRD' — 完全ランダム化 (@M = I@)。 既定。 座標交換 D-最適 (per-cell ムーブ)。
--   * 'SplitPlot' — whole-plot 因子が群内で一定 (@M = I + η·Z Zᵀ@)。 群単位ムーブ。
--   * 'StripPlot' — whole-plot × strip の直交 2 階層 (@M = I + ηW·Z_W Z_Wᵀ + ηS·Z_S Z_Sᵀ@)。
--   * 'Blocked' — ランダムブロック (@M = I + η·Z_B Z_Bᵀ@)。 全因子がブロック内で自由。
--
--   v1 未実装の構造 (多段ネスト・複数交差 RE) は 'customDesign' が
--   @unsupported structure@ で error になる (総称ゆえコンストラクタ追加のみで拡張できる)。
data Structure
  = CRD
      -- ^ 完全ランダム化 (@M = I@)。 既定。
  | SplitPlot
      { spWhole   :: ![Text]    -- ^ whole-plot 因子名 (群内で一定)
      , spNWhole  :: !Int       -- ^ whole-plot 数
      , spEta     :: !Double    -- ^ η = σ²_WP / σ² (既定 1.0)
      , spColName :: !Text      -- ^ designFrame に出す群列名 (既定 "wholePlot")
      }
  | StripPlot
      { stWhole    :: ![Text], stNWhole :: !Int, stEtaW :: !Double, stWholeCol :: !Text
      , stStrip    :: ![Text], stNStrip :: !Int, stEtaS :: !Double, stStripCol :: !Text
      }
      -- ^ whole-plot × strip の直交 2 階層。
  | Blocked
      { blkNBlocks :: !Int, blkEta :: !Double, blkColName :: !Text }
      -- ^ ランダムブロック。 全因子がブロック内で自由 (block は run 割付のみ)。
  deriving (Eq, Show)

-- | 'SplitPlot' の smart constructor。 η = 1.0・群列名 = @"wholePlot"@ を既定にする。
--   @splitPlot ["temp"] 4@ = temp を whole-plot 因子、 whole-plot 数 4。
splitPlot :: [Text] -> Int -> Structure
splitPlot whole nWhole = SplitPlot whole nWhole 1.0 "wholePlot"

-- | 'StripPlot' の smart constructor。 ηW = ηS = 1.0・群列名 = @"wholePlot"@ / @"strip"@ を既定に。
--   @stripPlot ["A"] 3 ["B"] 4@ = A が whole-plot (3 群) × B が strip (4 群)。
stripPlot :: [Text] -> Int -> [Text] -> Int -> Structure
stripPlot whole nWhole strip nStrip =
  StripPlot whole nWhole 1.0 "wholePlot" strip nStrip 1.0 "strip"

-- | 'Blocked' の smart constructor。 η = 1.0・群列名 = @"block"@ を既定にする。
--   @blocked 3@ = 3 ランダムブロック。
blocked :: Int -> Structure
blocked nBlocks = Blocked nBlocks 1.0 "block"

-- | **完全カスタムデザインの仕様** (Phase 79)。 因子 × 固定効果モデル × run 数 × seed に、
--   最適化基準 ('csCriterion')・制約 ('csConstraints')・階層構造 ('csStructure') を載せた
--   1 本のスペックレコード。 'customSpec' で既定 (DOpt・制約なし・CRD) を作り、 レコード更新で
--   criterion / constraints / structure を足す。 唯一の生成入口 'customDesign' に渡す。
data CustomSpec = CustomSpec
  { csFactors     :: ![DesignFactor]  -- ^ 因子 ('contFactor' / 'catFactor' / 'numFactor')
  , csFormula     :: !Formula         -- ^ 固定効果モデル (効果 DSL / 'parseRFormula')
  , csNRuns       :: !Int             -- ^ run 数 n
  , csSeed        :: !Int             -- ^ seed (決定的 pure)
  , csCriterion   :: !OptCriterion    -- ^ 最適化基準 (既定 'DOpt')
  , csConstraints :: ![Constraint]    -- ^ 低レベル制約 (既定 []・**coded 単位**・エスケープハッチ)
  , csNatConstraints :: ![NatConstraint]
      -- ^ **自然単位の制約** (既定 []・Phase 82・推奨 API)。 実単位で書き ('natLeq' 等)、
      --   'customDesign' 入口で coded の 'csConstraints' へ正規化・合流する。
  , csStructure   :: !Structure       -- ^ 階層構造 (既定 'CRD')
  } deriving (Show)

-- | 'CustomSpec' の smart constructor。 既定 = DOpt・制約なし・CRD。 レコード更新で
--   @{ csCriterion = … }@ / @{ csNatConstraints = … }@ / @{ csStructure = … }@ を足す。
--   @customSpec factors formula nRuns seed@。
customSpec :: [DesignFactor] -> Formula -> Int -> Int -> CustomSpec
customSpec fs fml n seed = CustomSpec
  { csFactors = fs, csFormula = fml, csNRuns = n, csSeed = seed
  , csCriterion = DOpt, csConstraints = [], csNatConstraints = []
  , csStructure = CRD }

-- ---------------------------------------------------------------------------
-- Phase 82: 自然単位の制約 (公開 API) → coded 内部制約への正規化
-- ---------------------------------------------------------------------------

-- | **自然単位の制約** (Phase 82・公開 API)。 因子を**実単位**で参照する
--   (@temp <= 160@ 等)。 'customDesign' 入口で因子の coded↔natural 情報を使って
--   内部 'Constraint' (coded) へ正規化される。 これにより ユーザは coded @[-1,1]@ や
--   水準 index を意識せず、 実験の言葉 (実温度・実流量) で制約を書ける。
--
--   * 'natLeq' / 'natGeq' / 'natEq' — 連続因子の線形不等式/等式 (実単位係数)。
--     @Σ aᵢ·x_natᵢ  rel  b@。 **離散数値 ('Num')** も**単一項** (@a·temp <= 160@ 等)
--     なら参照可で、 閾値を満たさない水準を除外する糖衣に展開する (Phase 82.2)。
--     カテゴリ ('Cat') は順序を持たないため拒否 ('Left'、 'natForbid' を使う)。
--   * 'natForbid' — 禁止組合せ。 カテゴリは水準名 ('FVText')、 離散数値/連続は
--     実値 ('FVDouble') で指定 (内部で index / coded へ変換)。
data NatConstraint
  = NatLinear ![(Text, Double)] !ConstraintRel !Double
    -- ^ @Σ aᵢ·x_natᵢ `rel` rhs@ (連続因子のみ)
  | NatForbid ![(Text, FactorValue)]
    -- ^ 全項が一致する row を禁止 (実単位/水準名で指定)
  deriving (Eq, Show)

-- | @Σ aᵢ·x_natᵢ ≤ b@ (連続因子・実単位)。
natLeq :: [(Text, Double)] -> Double -> NatConstraint
natLeq coefs b = NatLinear coefs CLeq b

-- | @Σ aᵢ·x_natᵢ ≥ b@ (連続因子・実単位)。
natGeq :: [(Text, Double)] -> Double -> NatConstraint
natGeq coefs b = NatLinear coefs CGeq b

-- | @Σ aᵢ·x_natᵢ = b@ (連続因子・実単位)。 grid 解像度に注意。
natEq :: [(Text, Double)] -> Double -> NatConstraint
natEq coefs b = NatLinear coefs CEq b

-- | 禁止組合せ (実単位/水準名)。 @natForbid [("catalyst", FVText \"A\"), ("temp", FVDouble 180)]@。
natForbid :: [(Text, FactorValue)] -> NatConstraint
natForbid = NatForbid

-- | 因子名で 'DesignFactor' を引く (制約正規化用)。
lookupDF :: [DesignFactor] -> Text -> Either Text DesignFactor
lookupDF fs nm =
  maybe (Left ("未知の因子 '" <> nm <> "' が制約に現れました")) Right
        (find ((== nm) . dfName) fs)

-- | 自然単位の 'NatConstraint' を因子情報を使って coded 内部 'Constraint' へ正規化。
--
--   * 線形スケール連続因子 (coded −1=lo/+1=hi/中心=平均、 @nat = center + coded·half@):
--     @Σ aᵢ·natᵢ ≤ b ⟺ Σ (aᵢ·halfᵢ)·codedᵢ ≤ b − Σ aᵢ·centerᵢ@。 half>0 ゆえ関係子不変。
--   * **対数スケール**連続因子: @a·nat = a·10^(…)@ は coded について非線形なので、 線形結合に
--     混ぜられない。 **単一因子の境界** (@temp <= X@ 等) のみ許可し、 閾値を @codeCont@ で
--     coded 境界へ写す (係数が負なら関係子を反転)。 混在は 'Left'。
--   * 禁止組合せ: カテゴリは水準名そのまま (buildRowFV が index→名前に戻すため)、
--     離散数値は実値→水準 index、 連続は実値→coded ('codeCont'・線形/対数を分岐)。
normalizeNat :: [DesignFactor] -> NatConstraint -> Either Text [Constraint]
normalizeNat fs (NatLinear coefs rel rhs) = do
    terms <- traverse resolve coefs                 -- (name, coef, factor)
    let numTerms = [ (nm, a, lvs) | (nm, a, f) <- terms, Num lvs <- [dfKind f] ]
        catNames = [ nm | (nm, _, f) <- terms, Cat _ <- [dfKind f] ]
    case (catNames, numTerms) of
      (nm : _, _) ->
        Left ("自然単位の線形制約はカテゴリ因子 '" <> nm
              <> "' を参照できません (順序を持たないため)。 natForbid を使ってください")
      (_, (nm, a, lvs) : rest)
        | not (null rest) || length terms /= 1 ->
            Left ("離散数値因子 '" <> nm <> "' を含む自然単位制約は単一項 (a·" <> nm
                  <> " rel b) でのみ書けます (許容水準の除外へ展開するため)。"
                  <> " 他因子との線形結合は不可")
        | otherwise -> numFilter nm a lvs
      (_, []) ->                                    -- 全項が連続 (線形/対数)
        let logTerms = filter (\(_, _, f) -> isLog f) terms
        in case logTerms of
             []                               -> Right [linearCombo terms]
             [(nm, a, f)] | length terms == 1 -> (: []) <$> singleLogBound nm a f
             _ -> Left ("自然単位の線形結合に対数スケール因子は混ぜられません (非線形)。"
                        <> " 対数因子は単一因子の境界 (temp<=X 等) でのみ書けます")
  where
    resolve (nm, a) = (\f -> (nm, a, f)) <$> lookupDF fs nm
    isLog f = case dfKind f of Cont _ _ SLog -> True; _ -> False
    -- 単一 Num 因子の実値閾値 → 満たさない水準を Forbidden で除外 (Phase 82.2)。
    --   @a·level rel rhs@ を各水準で判定し、 満たさない水準の index を禁止する。
    --   半空間ではなく水準除外になる (順序 Num は index 尺度・doc 明記)。
    numFilter nm a lvs =
      let bad = [ i | (i, lv) <- zip [0 :: Int ..] lvs, not (relHolds rel (a * lv) rhs) ]
      in if length bad == length lvs
           then Left ("離散数値因子 '" <> nm <> "' の制約 " <> tshowD a <> "·" <> nm
                      <> " " <> relSym rel <> " " <> tshowD rhs
                      <> " を満たす水準がありません (水準 " <> T.pack (show lvs) <> ")")
           else Right [ Forbidden [(nm, FVDouble (fromIntegral i))] | i <- bad ]
    relHolds CLeq x r = x <= r + 1e-9
    relHolds CEq  x r = abs (x - r) <= 1e-9
    relHolds CGeq x r = x >= r - 1e-9
    -- 全項が線形スケール連続: Σaᵢ·natᵢ rel rhs → coded の LinearIneq へ
    linearCombo terms' =
      let part (nm, a, f) = case dfKind f of
            Cont lo hi _ -> let half = (hi - lo) / 2; center = (lo + hi) / 2
                            in ((nm, a * half), a * center)
            _            -> ((nm, 0), 0)      -- 連続のみ到達
          ps    = map part terms'
          shift = sum (map snd ps)
      in LinearIneq (map fst ps) rel (rhs - shift)
    -- 単一対数因子の境界: a·nat rel rhs → nat rel' (rhs/a) → codeCont で coded 境界
    singleLogBound nm a f
      | a == 0    = Left ("対数因子 '" <> nm <> "' の係数が 0 です")
      | thr <= 0  = Left ("対数因子 '" <> nm <> "' の自然単位境界 " <> T.pack (show thr)
                          <> " が非正です (log 不能)。 正の閾値で指定してください")
      | otherwise = Right (LinearIneq [(nm, 1)] rel' (codeCont f thr))
      where
        thr  = rhs / a
        rel' = if a > 0 then rel else flipRel rel
    flipRel CLeq = CGeq
    flipRel CGeq = CLeq
    flipRel CEq  = CEq
normalizeNat fs (NatForbid vs) = (\c -> [Forbidden c]) <$> traverse conv vs
  where
    conv (nm, v) = do
      f <- lookupDF fs nm
      case (dfKind f, v) of
        (Cat _, FVText _)   -> Right (nm, v)   -- 水準名はそのまま
        (Cat _, FVDouble _) ->
          Left ("カテゴリ因子 '" <> nm <> "' の禁止値は水準名 (FVText) で指定してください")
        (Num levels, FVDouble x) ->
          case elemIndex x levels of
            Just i  -> Right (nm, FVDouble (fromIntegral i))
            Nothing -> Left ("離散数値因子 '" <> nm <> "' に水準 "
                             <> T.pack (show x) <> " はありません")
        (Cont _ _ _, FVDouble x) -> Right (nm, FVDouble (codeCont f x))
        _ -> Left ("因子 '" <> nm <> "' の禁止値の型が不正です")

-- | 'CustomSpec' の有効な coded 制約 = 低レベル 'csConstraints' + 正規化した
--   'csNatConstraints'。 正規化失敗は 'error' (customDesign の既存パターンに合わせる)。
effectiveConstraints :: CustomSpec -> [Constraint]
effectiveConstraints cs =
  case traverse (normalizeNat (csFactors cs)) (csNatConstraints cs) of
    Left e   -> error ("customDesign: " <> T.unpack e)
    Right ns -> csConstraints cs ++ concat ns

-- | 座標交換が **実行不能** (feasible な初期解が得られない等) で 'Left' を返したとき、
--   エラーに「有効な制約 (実単位)」と「因子の範囲」を添えて原因追跡を助ける (Phase 82.2)。
--   実行不能系でないメッセージ (引数不正等) はそのまま。 制約が空なら添えない。
enrichInfeasError :: CustomSpec -> Text -> Text
enrichInfeasError cs e
  | isFeasErr && hasCons =
      base <> "\n  有効な制約 (実単位):" <> T.concat (map ("\n    - " <>) items)
           <> "\n  因子の範囲:" <> T.concat (map ("\n    - " <>) ranges)
  | otherwise = base
  where
    base      = "customDesign: " <> e
    isFeasErr = any (`T.isInfixOf` e) ["feasible", "infeasible", "too tight", "初期解"]
    natC      = csNatConstraints cs
    lowC      = csConstraints cs
    hasCons   = not (null natC) || not (null lowC)
    items     = map renderNatC natC ++ map ((<> "  (coded 単位)") . renderLowC) lowC
    ranges    = [ dfName f <> " ∈ " <> renderRange f | f <- csFactors cs ]

-- | 自然単位 'NatConstraint' を人間可読な文字列に (エラー添付用)。
renderNatC :: NatConstraint -> Text
renderNatC (NatLinear coefs rel rhs) =
  T.intercalate " + " (map (\(nm, a) -> tshowD a <> "·" <> nm) coefs)
    <> " " <> relSym rel <> " " <> tshowD rhs
renderNatC (NatForbid vs) =
  "禁止: " <> T.intercalate ", " (map (\(nm, v) -> nm <> "=" <> renderFV v) vs)

-- | 低レベル coded 'Constraint' の要約 (エラー添付用・完全網羅でなく主要 2 種)。
renderLowC :: Constraint -> Text
renderLowC (LinearIneq coefs rel rhs) =
  T.intercalate " + " (map (\(nm, a) -> tshowD a <> "·" <> nm) coefs)
    <> " " <> relSym rel <> " " <> tshowD rhs
renderLowC (RangeBound nm lo hi) = nm <> " ∈ [" <> tshowD lo <> ", " <> tshowD hi <> "]"
renderLowC (Forbidden vs) =
  "禁止: " <> T.intercalate ", " (map (\(nm, v) -> nm <> "=" <> renderFV v) vs)
renderLowC (Conditional _ _) = "条件付制約"

renderFV :: FactorValue -> Text
renderFV (FVDouble x) = tshowD x
renderFV (FVText t)   = t

relSym :: ConstraintRel -> Text
relSym CLeq = "≤"
relSym CGeq = "≥"
relSym CEq  = "="

-- | 因子の値域を実単位で (連続 = 範囲・対数注記、 数値順序 = 水準列、 カテゴリ = 水準名)。
renderRange :: DesignFactor -> Text
renderRange f = case dfKind f of
  Cont lo hi sc -> "[" <> tshowD lo <> ", " <> tshowD hi <> "]"
                     <> (case sc of SLog -> " (log)"; SLinear -> "")
  Num levels    -> T.pack (show levels)
  Cat levels    -> "{" <> T.intercalate ", " levels <> "}"

tshowD :: Double -> Text
tshowD = T.pack . show

-- ---------------------------------------------------------------------------
-- コンストラクタ
-- ---------------------------------------------------------------------------

-- | 因子の coded 水準集合。 連続 = @{-1,+1}@ (2 水準)、 カテゴリ = 水準 index @{0,1,…,m-1}@。
--   完全要因の列挙 ('fullFactorial') と 最適計画の候補格子に使う。
factorLevelsCoded :: DesignFactor -> [Double]
factorLevelsCoded f = case dfKind f of
  Cont _ _ _ -> [-1, 1]
  Num levels -> map fromIntegral [0 .. length levels - 1]
  Cat levels -> map fromIntegral [0 .. length levels - 1]

-- | 全因子が連続であることを要求する (rsm/boxBehnken/taguchi 等・連続専用設計)。
--   カテゴリが混じれば呼び名付きで error。
requireContinuous :: String -> [DesignFactor] -> [DesignFactor]
requireContinuous who fs =
  case [ dfName f | f <- fs, isCat (dfKind f) ] of
    []   -> fs
    cats -> error
      (who ++ ": カテゴリ因子 " ++ show (map T.unpack cats)
        ++ " は扱えません (この設計は連続因子のみ・カテゴリは factorialDesign/optimalDesign へ)")
  where isCat (Cat _) = True
        isCat _       = False

-- | 全カテゴリ因子が 2 水準 (binary) であることを要求する (fractional・v1 は binary のみ)。
--   3 水準以上の 'Cat' は呼び名付きで error。 連続因子は素通り。
requireBinaryCats :: String -> [DesignFactor] -> [DesignFactor]
requireBinaryCats who fs =
  case [ dfName f | f <- fs, overTwo (dfKind f) ] of
    []  -> fs
    bad -> error
      (who ++ ": カテゴリ因子 " ++ show (map T.unpack bad)
        ++ " は 2 水準 (binary) のみ対応です (3 水準以上は factorialDesign/optimalDesign か G-a2 の L9/L18 へ)")
  where overTwo (Cat ls) = length ls /= 2
        overTwo _         = False

-- | ±1 coded 設計 (fractional/taguchi) のカテゴリ列を水準 index に写す。 binary 前提
--   (@-1@ → 水準0、 @+1@ → 水準1)。 連続列は ±1 のまま (直交/平衡構造を保つ)。
codeCatColumns :: [DesignFactor] -> [[Double]] -> [[Double]]
codeCatColumns fs = map (zipWith recode fs)
  where recode f v = case dfKind f of
          Cat _ -> if v < 0 then 0 else 1
          _     -> v

-- | 2 水準 完全要因計画。 @factorialDesign [contFactor "temp" (150,180), contFactor "time" (10,20)]@
--   → 2^k run (全交互作用モデル)。 カテゴリ因子を混ぜると各因子の水準を総当り
--   (連続=2 水準・カテゴリ=m 水準) した完全要因になる (例: 連続1×カテゴリ3水準 = 2×3=6 run)。
factorialDesign :: [DesignFactor] -> Design
factorialDesign fs =
  Design fs (fullFactorial (map factorLevelsCoded fs)) KFactorial

-- | 応答曲面計画 (回転可能 中心複合計画 CCD)。
--   @centralCompositeDesign [contFactor "temp" (150,180), contFactor "time" (10,20)]@ → 2^k factorial + 2k 軸点
--   + 中心点 (2 次モデル)。 中心点数は k (最低 1)。 ★連続因子のみ (±α 軸点ゆえカテゴリ不可)。
centralCompositeDesign :: [DesignFactor] -> Design
centralCompositeDesign fs0 =
  let fs = requireContinuous "centralCompositeDesign" fs0
      k  = length fs
  in Design fs (centralCompositeRotatable k (max 1 k)) KRSM

-- | Box-Behnken 応答曲面計画 (**k = 3, 4, 5 のみ**)。 CCD より run が少なく、 **極端な軸点
--   (±α) を持たない** (各点は立方体の辺の中点・因子は @-1,0,+1@ の 3 水準に収まる) 3 水準 RSM。
--   2 次モデル ('KRSM') を含意。 因子数が 3〜5 でないと下層が error (数学的制約)。 ★連続因子のみ。
--   @boxBehnkenDesign [contFactor "t" (150,180), contFactor "p" (1,3), contFactor "c" (5,15)]@。
boxBehnkenDesign :: [DesignFactor] -> Design
boxBehnkenDesign fs0 =
  let fs = requireContinuous "boxBehnkenDesign" fs0
      k  = length fs
  in Design fs (boxBehnken k (max 1 k)) KRSM

-- ---------------------------------------------------------------------------
-- 一部実施要因 (fractional factorial) — Phase 78.G
--
-- 完全要因 2^k は run 数が指数増するので、 交互作用の一部を主効果と交絡させて
-- **2^(k-p) に減らす** (直交表と等価)。 交絡の重さは **解像度 (resolution)** で測る:
--   * Res III … 主効果と 2 因子交互作用が交絡 (最も攻めた削減)。
--   * Res IV  … 主効果は 2 因子交互作用と交絡しないが、 2 因子交互作用同士が交絡。
--   * Res V+  … 主効果・2 因子交互作用が (ほぼ) 独立。
-- generator (追加因子の定義関係) は **最小交絡 (minimum aberration)** の標準表
-- (Montgomery Table 8-14 / NIST) を 'fractionalCatalog' に持つ。 k = 3〜11, 15 (16/32-run)。
-- ---------------------------------------------------------------------------

-- | 設計の解像度 (defining word の最短長)。 数字は @'resNum'@。
data Resolution = ResIII | ResIV | ResV | ResVI | ResVII
  deriving (Show, Eq, Ord, Enum, Bounded)

resNum :: Resolution -> Int
resNum r = fromEnum r + 3

-- | 最小交絡 2 水準一部実施の標準カタログ (k → @[(runs, resolution, generators)]@)。
--   generator は追加因子を基底因子の**積**で定義する 1-based index リスト
--   (例: @[[1,2]]@ = 「追加因子 = 基底1×基底2」 = C=AB)。 出典 = Montgomery Table 8-14 /
--   NIST e-Handbook (§5.3.3.4.7)。 各行の resolution は 'fracResolution' で test 検証済 (誤り混入ガード)。
--   k=8〜15 は NIST 収録の 16/32-run 設計 (Phase 78.G-c 拡張)。 NIST が非収録の帯 (16-run k=12〜14・
--   32-run k=12〜16) は本カタログも持たない (該当 k はより多 run の設計 or k=15/31 飽和を使う)。
fractionalCatalog :: Int -> [(Int, Resolution, [[Int]])]
fractionalCatalog k = case k of
  3  -> [ (4,  ResIII, [[1,2]]) ]                                   -- C=AB
  4  -> [ (8,  ResIV,  [[1,2,3]]) ]                                 -- D=ABC
  5  -> [ (16, ResV,   [[1,2,3,4]])                                 -- E=ABCD
        , (8,  ResIII, [[1,2],[1,3]]) ]                             -- D=AB, E=AC
  6  -> [ (32, ResVI,  [[1,2,3,4,5]])                               -- F=ABCDE
        , (16, ResIV,  [[1,2,3],[2,3,4]])                           -- E=ABC, F=BCD
        , (8,  ResIII, [[1,2],[1,3],[2,3]]) ]                       -- D=AB, E=AC, F=BC
  7  -> [ (64, ResVII, [[1,2,3,4,5,6]])                             -- G=ABCDEF
        , (16, ResIV,  [[1,2,3],[2,3,4],[1,3,4]])                   -- E=ABC, F=BCD, G=ACD
        , (8,  ResIII, [[1,2],[1,3],[2,3],[1,2,3]]) ]               -- D=AB, E=AC, F=BC, G=ABC
  -- ↓ NIST 標準表 (16/32-run)。 基底は 16-run=ABCD(4)・32-run=ABCDE(5)。
  8  -> [ (16, ResIV,  [[2,3,4],[1,3,4],[1,2,3],[1,2,4]]) ]         -- 2^(8-4): E=BCD,F=ACD,G=ABC,H=ABD
  9  -> [ (32, ResIV,  [[2,3,4,5],[1,3,4,5],[1,2,4,5],[1,2,3,5]])   -- 2^(9-4): F=BCDE,G=ACDE,H=ABDE,J=ABCE
        , (16, ResIII, [[1,2,3],[2,3,4],[1,3,4],[1,2,4],[1,2,3,4]]) ]  -- 2^(9-5): E=ABC,F=BCD,G=ACD,H=ABD,J=ABCD
  10 -> [ (32, ResIV,  [[1,2,3,4],[1,2,3,5],[1,2,4,5],[1,3,4,5],[2,3,4,5]])  -- 2^(10-5)
        , (16, ResIII, [[1,2,3],[2,3,4],[1,3,4],[1,2,4],[1,2,3,4],[1,2]]) ]  -- 2^(10-6): …,J=ABCD,K=AB
  11 -> [ (32, ResIV,  [[1,2,3],[2,3,4],[3,4,5],[1,3,4],[1,4,5],[2,4,5]])    -- 2^(11-6)
        , (16, ResIII, [[1,2,3],[2,3,4],[1,3,4],[1,2,4],[1,2,3,4],[1,2],[1,3]]) ]  -- 2^(11-7)
  15 -> [ (16, ResIII, [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]         -- 2^(15-11) 飽和: 全 15 列
                       ,[1,2,3],[1,2,4],[1,3,4],[2,3,4],[1,2,3,4]]) ]
  _  -> []

-- | generator 集合から**解像度** (数字) を計算する。 各 generator が定める defining word
--   (追加因子 ∪ 基底集合) の生成する部分群 (全 XOR 組合せ) の**最短語長**。 = 設計の resolution。
--   'fractionalCatalog' の resolution ラベルを test で照合するのに使う (= 表の自己検証)。
fracResolution :: Int -> [[Int]] -> Int
fracResolution k gens =
  case definingSubgroup k gens of
    []     -> k
    combos -> minimum (map length combos)

-- | defining word の対称差 (mod-2 積 = 語の XOR)。
symDiffW :: [Int] -> [Int] -> [Int]
symDiffW a b = sort ((a \\ b) ++ (b \\ a))

-- | 各 generator が定める defining word (追加因子 ∪ 基底集合)。 generator i (1-based) は
--   因子 (kBase+i) を追加する (kBase = k − generator 数)。 defining word = 基底集合 ∪ {kBase+i}。
definingWords :: Int -> [[Int]] -> [[Int]]
definingWords k gens =
  let kBase = k - length gens
  in [ sort (gen ++ [kBase + i]) | (i, gen) <- zip [1 ..] gens ]

-- | defining 部分群の**非恒等元** (全 defining word の非空部分集合の XOR)。 defining relation
--   @I = …@ の右辺集合。 交絡 (alias) と解像度 ('fracResolution') はこの群で決まる。
definingSubgroup :: Int -> [[Int]] -> [[Int]]
definingSubgroup k gens =
  [ foldl1' symDiffW ws | ws <- tail (subsequences (definingWords k gens)) ]

-- | 効果 (因子 index 集合) の **alias 剰余類** = @{ effect XOR w | w ∈ 部分群 ∪ {恒等} }@。
--   効果自身を含む。 同じ剰余類に入る効果同士は設計上区別できない (交絡)。
aliasCoset :: Int -> [[Int]] -> [Int] -> [[Int]]
aliasCoset k gens effect =
  nub (sort [ symDiffW effect w | w <- [] : definingSubgroup k gens ])

-- | 主効果と交絡しない 2 因子交互作用の**代表** (交絡群ごと 1 個)。 各 2FI の alias 剰余類が
--   主効果語 (長さ1) を含めば群ごと除外、 含まなければ未代表の群から 1 個を採る。 結果は満ランクで
--   主効果を不バイアスに保つ (Res V=全 2FI・Res IV=群ごと 1 個・Res III=主効果非交絡の 2FI のみ)。
clearTwoFactorInteractions :: Int -> [[Int]] -> [[Int]]
clearTwoFactorInteractions k gens = go [] twoFIs
  where
    twoFIs = [ [i, j] | i <- [1 .. k], j <- [i + 1 .. k] ]
    go _    []       = []
    go seen (t : ts)
      | any (`elem` seen) coset     = go seen ts               -- 代表済みの交絡群
      | any ((== 1) . length) coset = go (coset ++ seen) ts     -- 主効果と交絡 → 群ごと除外
      | otherwise                   = t : go (coset ++ seen) ts
      where coset = aliasCoset k gens t

-- | 効果 (因子 index 集合) を @":"@ 連結ラベル (@"a"@ / @"a:b"@) に。 因子名は 'dsFactors' 順。
effectLabel :: [Text] -> [Int] -> Text
effectLabel names is = T.intercalate ":" [ names !! (i - 1) | i <- is ]

-- | 一部実施 (交互作用版・'KFracInter') の **alias 構造**。 主効果と 2 因子交互作用について、
--   各効果と交絡する他効果 (同じ剰余類の残り) をラベルで返す。 他の設計種別では空。
--   @lookup "a:b" (aliasStructure plan)@ で「@a:b@ は何と交絡するか」を引ける。
aliasStructure :: Design -> [(Text, [Text])]
aliasStructure (Design fs _ kind) = case kind of
  KFracInter gens ->
    let k     = length fs
        names = map dfName fs
        effs  = [ [i] | i <- [1 .. k] ]
                ++ [ [i, j] | i <- [1 .. k], j <- [i + 1 .. k] ]
    in [ ( effectLabel names e
         , [ effectLabel names a
           | a <- aliasCoset k gens e, a /= e, not (null a) ] )
       | e <- effs ]
  _ -> []

-- | 一部実施要因計画 (**解像度で自動選択**・k = 3〜7)。 指定解像度**以上**を満たす中で
--   **最小 run 数**の最小交絡設計を選ぶ (削減を最大化しつつ要求解像度を確保)。 該当なしは error。
--   @fractionalDesign [("a",(0,1)),…,("g",(0,1))] ResIII@。 formula は**主効果のみ** (交互作用は交絡)。
fractionalDesign :: [DesignFactor] -> Resolution -> Design
fractionalDesign specs res =
  let fs   = requireBinaryCats "fractionalDesign" specs
      k    = length fs
      cands = [ e | e@(_, r, _) <- fractionalCatalog k, r >= res ]
  in case cands of
       [] -> error
         ("fractionalDesign: k=" ++ show k ++ " で resolution >= " ++ show res
           ++ " の標準設計がありません (k=3〜11,15・利用可能: "
           ++ show [ (n, r) | (n, r, _) <- fractionalCatalog k ] ++ ")")
       _  -> let (_, _, gens) = minimumBy' (\(n1,_,_) (n2,_,_) -> compare n1 n2) cands
             in Design fs (codeCatColumns fs (fractionalFactorial k gens)) KFractional

-- | 一部実施要因計画 (**generator 明示**・玄人向け escape hatch)。 generator は追加因子を
--   基底因子の積で定義する 1-based index リスト (例: @[[1,2,3]]@ = D=ABC)。 追加因子数 = @length gens@、
--   run 数 = @2^(k - length gens)@。 formula は**主効果のみ**。
fractionalDesignGen :: [DesignFactor] -> [[Int]] -> Design
fractionalDesignGen specs gens =
  let fs = requireBinaryCats "fractionalDesignGen" specs
      k  = length fs
  in Design fs (codeCatColumns fs (fractionalFactorial k gens)) KFractional

-- | 一部実施要因計画 (**交互作用込み**・解像度自動)。 設計点は 'fractionalDesign' と同一だが、
--   'designFormula' が主効果に加え**主効果と交絡しない 2 因子交互作用の代表** (交絡群ごと 1 個) を
--   含める ('KFracInter')。 交絡構造は 'aliasStructure' で確認できる。 該当解像度なしは error。
--   @fractionalDesignInter [contFactor "a" (0,1), …] ResV@ (Res V なら全 2FI が独立に載る)。
fractionalDesignInter :: [DesignFactor] -> Resolution -> Design
fractionalDesignInter specs res =
  let fs    = requireBinaryCats "fractionalDesignInter" specs
      k     = length fs
      cands = [ e | e@(_, r, _) <- fractionalCatalog k, r >= res ]
  in case cands of
       [] -> error
         ("fractionalDesignInter: k=" ++ show k ++ " で resolution >= " ++ show res
           ++ " の標準設計がありません (k=3〜11,15・利用可能: "
           ++ show [ (n, r) | (n, r, _) <- fractionalCatalog k ] ++ ")")
       _  -> let (_, _, gens) = minimumBy' (\(n1,_,_) (n2,_,_) -> compare n1 n2) cands
             in Design fs (codeCatColumns fs (fractionalFactorial k gens)) (KFracInter gens)

-- | 一部実施要因計画 (**交互作用込み**・generator 明示)。 'fractionalDesignGen' の交互作用版。
--   @fractionalDesignGenInter [contFactor "a" (0,1), …] [[1,2,3]]@ (D=ABC の Res IV)。
fractionalDesignGenInter :: [DesignFactor] -> [[Int]] -> Design
fractionalDesignGenInter specs gens =
  let fs = requireBinaryCats "fractionalDesignGenInter" specs
      k  = length fs
  in Design fs (codeCatColumns fs (fractionalFactorial k gens)) (KFracInter gens)

-- | 小さな minimumBy (Data.List.minimumBy 相当・依存を増やさない)。
minimumBy' :: (a -> a -> Ordering) -> [a] -> a
minimumBy' cmp = foldl1' (\x y -> if cmp x y == GT then y else x)

-- ---------------------------------------------------------------------------
-- Taguchi 直交表 (orthogonal array) — Phase 78.G-a
--
-- 一部実施要因 ('fractionalDesign') と同じ「run を減らして主効果を推定する」目的だが、
-- **Taguchi の Lₙ 直交表**を土台にする枠組み。 v1 は **2 水準表のみ** (L4/L8/L12/L16)。
--   * L4(2³) 4 run … 〜3 因子   * L8(2⁷) 8 run … 〜7 因子
--   * L12(2¹¹) 12 run … 〜11 因子 (★Plackett-Burman・主効果スクリーニングの定番)
--   * L16(2¹⁵) 16 run … 〜15 因子
-- L8/L16 は fractional と数学的に等価だが、 「因子を直交表の列に割り当てる」 Taguchi の
-- framing で透過的に扱えるようにする。 ★目玉は fractional に無い **L12 (11 因子/12 run)**。
--
-- ★Phase 78.G-a2: **3 水準/混合水準表 (L9/L18/L27)** と **数値順序 ('Num') / カテゴリ ('Cat')**
-- 因子に拡張。 各因子の要求水準数 (Cont=2・Num/Cat=水準数) を表の列水準 ('oaLevels') に
-- **貪欲に突合**して割り当てる (混合表 L18=2¹×3⁷ では 2 水準因子を 2 水準列へ、 3 水準因子を
-- 3 水準列へ)。 割当先列の code を各因子の coded へ写す:
--   * Cont … code 1→@-1@ / 2→@+1@ (2 水準)。 uncode で実値へ。
--   * Num/Cat … code c → 水準 index @c-1@。 designFrame で実値/水準名へ、 Num は formula で
--     直交多項式 (opoly) に、 Cat は engine の contrast に展開。
-- formula は 'fractionalDesign' と同じ**主効果のみ** ('KFractional')。 designModel は共通経路。
-- ---------------------------------------------------------------------------

-- | 自動選択が run 数の昇順に舐める直交表 (2 水準 + 3 水準/混合)。
taguchiOAs :: [OA]
taguchiOAs = [l4, l8, l9, l12, l16, l18, l27]   -- runs: 4,8,9,12,16,18,27

-- | 因子が要求する水準数 (Cont=2・Num/Cat=水準リスト長)。 直交表の列水準への突合に使う。
factorLevelCount :: DesignFactor -> Int
factorLevelCount f = case dfKind f of
  Cont _ _ _ -> 2
  Num levels -> length levels
  Cat levels -> length levels

-- | 因子を直交表の列に貪欲割当 (各因子の要求水準数に一致する未使用列を順に取る)。
--   成功なら各因子の割当先**列 index**、 一致列が尽きたら 'Nothing' (混合水準の突合失敗)。
assignOAColumns :: OA -> [DesignFactor] -> Maybe [Int]
assignOAColumns oa = go (zip [0 ..] (oaLevels oa))
  where
    go _     []       = Just []
    go avail (f : fs) =
      case break (\(_, lvl) -> lvl == factorLevelCount f) avail of
        (_,   [])              -> Nothing
        (pre, (col, _) : post) -> (col :) <$> go (pre ++ post) fs

-- | 割当先列の 1-based level code を各因子の coded へ。 Cont は code 1→@-1@/2→@+1@、
--   Num/Cat は水準 index @c-1@。
oaToCodedCols :: OA -> [Int] -> [DesignFactor] -> [[Double]]
oaToCodedCols oa cols fs =
  [ [ codeFor f (row !! col) | (col, f) <- zip cols fs ] | row <- oaTable oa ]
  where
    codeFor f code = case dfKind f of
      Cont _ _ _ -> if code == 1 then -1 else 1
      _        -> fromIntegral (code - 1)   -- Num/Cat: 水準 index

-- | Taguchi 直交表計画 (**最小 OA 自動選択**・2/3 水準・混合)。 各因子の要求水準数 (連続=2・
--   'numFactor'/'catFactor' は水準数) に一致する列を持つ最小 run 表 (L4/L8/L9/L12/L16/L18/L27
--   の run 昇順) を選び、 因子を列へ割り当てる。 該当表なしは error。 formula は
--   'fractionalDesign' と同じ**主効果のみ** ('KFractional'・Num は 'opoly')。
--   @taguchiDesign [contFactor "a" (0,1), …]@ (11 連続) → L12、
--   @taguchiDesign [catFactor "x" ["A","B","C"], …]@ (3 水準) → L9、
--   @taguchiDesign [contFactor "p" (0,1), catFactor "q" ["a","b","c"], …]@ (混合) → L18。
taguchiDesign :: [DesignFactor] -> Design
taguchiDesign specs =
  case [ (oa, cols) | oa <- taguchiOAs, Just cols <- [assignOAColumns oa specs] ] of
    []              -> error
      ("taguchiDesign: 因子の水準構成 " ++ show (map factorLevelCount specs)
        ++ " を収容できる標準直交表がありません "
        ++ "(利用可能: L4/L8/L12/L16 = 2 水準、 L9/L18(混合)/L27 = 3 水準)")
    ((oa, cols) : _) -> Design specs (oaToCodedCols oa cols specs) KFractional

-- | 標準直交表の識別子 ('taguchiDesignOA' で表を明示するための**列挙型**)。
--   文字列でなく型で表を指定するので、 打ち間違い (@\"L10\"@ 等) は**コンパイル時に弾かれる**。
--   2 水準表 = 'L4'/'L8'/'L12'/'L16'、 3 水準表 = 'L9'/'L27'、 混合水準表 = 'L18' (2^1×3^7)。
data OATable = L4 | L8 | L9 | L12 | L16 | L18 | L27
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | 'OATable' → 低レベル 'OA' 定義。
oaTableToOA :: OATable -> OA
oaTableToOA t = case t of
  L4 -> l4; L8 -> l8; L9 -> l9; L12 -> l12; L16 -> l16; L18 -> l18; L27 -> l27

-- | Taguchi 直交表計画 (**直交表を型で明示**・fractional の generator 版と対称の escape hatch)。
--   表は 'OATable' の列挙値 ('L4'/'L8'/'L9'/'L12'/'L16'/'L18'/'L27') で渡すので、 未知の表名は
--   型検査で弾かれる (文字列指定の実行時 error が無い)。 因子の水準数が表の列に割り当てられない
--   場合のみ error。
--   @taguchiDesignOA L12 specs@ で run 数を意図的に選ぶ、 @taguchiDesignOA L9 cats@ で 3 水準表を明示。
taguchiDesignOA :: OATable -> [DesignFactor] -> Design
taguchiDesignOA table specs =
  let oa = oaTableToOA table
  in case assignOAColumns oa specs of
    Nothing   -> error
      ("taguchiDesignOA: " ++ show table ++ " (列水準 " ++ show (oaLevels oa)
        ++ ") に因子の水準構成 " ++ show (map factorLevelCount specs) ++ " を割り当てられません")
    Just cols -> Design specs (oaToCodedCols oa cols specs) KFractional

-- ---------------------------------------------------------------------------
-- 最適計画 (optimal design) — Phase 78.G-b1
--
-- 標準の格子計画 (factorial/RSM) と違い、 **モデル formula** と **run 数 n** を先に決め、
-- 候補点集合から情報行列 XᵀX の基準 (D/A/I/E/G) を最適化する n 点を選ぶ (Fedorov 交換)。
-- run 数が制約されている・非標準のモデル項を当てたい・候補領域が不規則、 等で使う。
--
-- 実装は既存部品の**接着**で、 新規の数値アルゴは無い:
--   1. 因子 specs から coded 候補グリッド 'candidateGrid' (各因子 [-1,1] の等間隔 levels 水準)。
--   2. グリッド各点を DataFrame 化し 'modelFrame'+'designMatrixF' で **formula の設計行列 X 行**へ展開。
--   3. 低レベル 'OPT.optimalDesign' (Fedorov) で n 行を選択 → 選択 index。
--   4. 選ばれた候補の**因子座標**を 'dsCoded' に、 formula を 'KCustom' に焼き込む。
-- 以降は 'designTable'/'designFrame'/'designModel' が factorial 等と同じ共通経路で処理する。
--
-- モデルは 'Formula' に一本化 (二重管理回避)。 入口は 2 つ:
--   * 効果 DSL 'mainEffects'/'twoWay'/'quadratic' (対話向け・型付き)。
--   * 文字列 RHS を 'parseRFormula' で 'Formula' に (アプリ/外部から組み立て)。
-- ★連続因子のみ (v1)。 カテゴリ因子は 'DesignFactor' の水準リスト化 (型手術) を伴う G-b2。
-- ---------------------------------------------------------------------------

-- | 主効果のみモデル @y ~ x1 + x2 + …@ を作る効果 DSL。 応答は placeholder (@_y@)。
--   'optimalDesign' のモデル引数に渡す。
mainEffects :: [Text] -> Formula
mainEffects names = effectFormula (T.intercalate " + " names)

-- | 主効果 + 全 2 因子交互作用モデル @y ~ x1 + x2 + x1:x2 + …@ を作る効果 DSL。
twoWay :: [Text] -> Formula
twoWay names = effectFormula (T.intercalate " + " (names ++ twoWayTerms names))

-- | 主効果 + 2 因子交互作用 + 2 次項モデル @y ~ … + I(x1^2) + …@ (RSM 相当) を作る効果 DSL。
--   2 次項を含むので 'optimalDesign' の既定候補水準は 3 になる。
quadratic :: [Text] -> Formula
quadratic names =
  effectFormula (T.intercalate " + " (names ++ twoWayTerms names ++ squareTerms names))

-- | 全 2 因子交互作用の項 (@a:b@) を名前順に。
twoWayTerms :: [Text] -> [Text]
twoWayTerms names =
  [ a <> ":" <> b | (i, a) <- zip [0 :: Int ..] names, b <- drop (i + 1) names ]

-- | 各因子の 2 次項 (@I(x^2)@)。
squareTerms :: [Text] -> [Text]
squareTerms names = [ "I(" <> n <> "^2)" | n <- names ]

-- ---------------------------------------------------------------------------
-- Phase 78.M M3: Formula (効果 DSL) → Custom.Model ([ModelTerm]) 変換
-- ---------------------------------------------------------------------------

-- | 効果 DSL / 'Formula' を Custom Design 層の 'CMd.Model' ('mainEffects'/'twoWay'/
--   'quadratic' 相当の @[ModelTerm]@) に変換する。 高レベル生成 API ('customDesign'・M4)
--   が座標交換 ('coordinateExchangePure') に渡すモデルを組み立てる橋渡し。
--
--   効果 DSL の formula は 'parseRFormula' が各項に係数パラメータ @_pN@ を挿入した
--   積和 (例 @_p0 + _p1·a + _p3·(a·b) + _p4·a^2@) になる。 加法分解した各項の
--   **因子名を参照する葉だけ**を残して分類する (与えた @facNames@ に含まれる 'Ref' が因子・
--   それ以外の 'Ref' は係数パラメータとして無視):
--
--     * 因子葉が 0 個 (係数のみ)          → 'CMd.TIntercept'
--     * 単一 @Ref x@                        → 'CMd.TMain' x
--     * 単一 @Bin Pow (Ref x) (Lit k)@      → 'CMd.TPower' x (round k)
--     * 複数の @Ref@ の積                    → 'CMd.TInter' [x, y, …]
--
--   正規化は 'CMd.NCoded' (optimalDesign と同じ coded 規約)。 基底関数 (@opoly@/@bspline@)
--   や未対応構造は @Left@ を返す (M3 スコープ = 主効果 / 2 因子交互作用 / 冪)。
formulaToCustomModel :: [Text] -> Formula -> Either Text CMd.Model
formulaToCustomModel facNames (Formula _ _ rhs) = do
  terms <- mapM termToModelTerm (flattenAddW rhs)
  Right (CMd.Model terms CMd.NCoded)
  where
    isFacRef (Ref x) = x `elem` facNames
    isFacRef _       = False
    -- 係数パラメータ葉 = facNames に無い裸の 'Ref' (parseRFormula が挿入する @_pN@)。
    isParamLeaf (Ref x) = x `notElem` facNames
    isParamLeaf _       = False
    -- 積を葉分解し、 係数パラメータを除いた「中核」葉で項を分類する。
    -- 中核が空 = 係数のみ = 切片。 それ以外は因子参照 (主効果/交互作用/冪)。
    -- 中核に因子を参照しない葉 (基底関数 'App' 等) が残れば未対応 → Left。
    termToModelTerm t =
      case filter (not . isParamLeaf) (mulLeavesW t) of
        []        -> Right CMd.TIntercept
        [Ref x] | x `elem` facNames -> Right (CMd.TMain x)
        [Bin Pow (Ref x) (Lit k)]
          | x `elem` facNames && k >= 2 -> Right (CMd.TPower x (round k))
        ls | not (null ls) && all isFacRef ls ->
               Right (CMd.TInter [ x | Ref x <- ls ])
        _ -> Left (T.pack
               ("formulaToCustomModel: 未対応の項 (基底関数や非線形項は Custom.Model に"
                <> " 変換できません・主効果/交互作用/冪のみ対応): " <> show t))

-- | 加法項へ分解 (符号は係数に吸収されるので Add/Sub/Neg 同一視)。
--   'Formula.Design.flattenAdd' と同義だが import 循環回避のため局所定義。
flattenAddW :: Term -> [Term]
flattenAddW (Bin Add a b) = flattenAddW a ++ flattenAddW b
flattenAddW (Bin Sub a b) = flattenAddW a ++ flattenAddW b
flattenAddW (Neg a)       = flattenAddW a
flattenAddW t             = [t]

-- | 乗法葉へ分解。 'Formula.Design.mulLeaves' と同義 (局所定義)。
mulLeavesW :: Term -> [Term]
mulLeavesW (Bin Mul a b) = mulLeavesW a ++ mulLeavesW b
mulLeavesW (Neg a)       = mulLeavesW a
mulLeavesW t             = [t]

-- ---------------------------------------------------------------------------
-- Phase 79: 高レベル生成 API (pure・座標交換 / 階層構造 / 制約)
-- ---------------------------------------------------------------------------

-- | 高レベル 'DesignFactor' を Custom Design 層の 'CF.Factor' に変換する。
--   連続 = coded [-1,1] 前提の 'CF.Continuous'、 カテゴリ = 'CF.Categorical' (水準 index)。
--   数値順序 ('Num') は **水準 index を grid とする** 'CF.DiscreteNum' @[0..k-1]@ に写す
--   (Phase 78.M M4-b)。 これで座標交換の出力 (cdMatrix) が水準 index になり、 'dsCoded' の
--   Num 規約 ('numLevelAt' = index → 実水準値) と一致する。 ★交換は index 尺度 (等間隔) で
--   D-最適化する (実測不等間隔の直交多項式 opoly は Custom.Model 非対応ゆえ、 モデル項は
--   ユーザ formula の I(x^2) 等がそのまま使われる)。
--   ★役割 ('CF.fRole') は Phase 79 で高レベルから撤去したので一律 'CF.Controllable'
--   (階層は因子ではなく 'Structure' が持つ。 fRole の去就は 79.2 の M-construction 移植時に判断)。
toCustomFactor :: DesignFactor -> CF.Factor
toCustomFactor f = CF.Factor (dfName f) (kindC (dfKind f)) CF.Controllable
  where
    kindC (Cont lo hi _) = CF.Continuous lo hi
    kindC (Cat ls)     = CF.Categorical ls
    kindC (Num levels) = CF.DiscreteNum (map fromIntegral [0 .. length levels - 1])

-- | 因子 + formula から Custom Design の 'CX.CustomDesignSpec' を組み立てる共通処理 (M4)。
--   モデルは 'formulaToCustomModel' で [ModelTerm] 化 (失敗は error)。 最適化基準 @crit@ と
--   制約 @cons@ を受け取り、 budget は既定。 座標交換 / split-plot 生成の両方が使う。
toCustomSpec :: OptCriterion -> [Constraint]
             -> [DesignFactor] -> Formula -> Int -> Int -> CX.CustomDesignSpec
toCustomSpec crit cons fs fml n seed =
  case formulaToCustomModel (map dfName fs) fml of
    Left e      -> error ("customDesign: モデル変換に失敗: " <> T.unpack e)
    Right model -> CX.CustomDesignSpec
      { CX.cdsFactors      = map toCustomFactor fs
      , CX.cdsModel        = model
      , CX.cdsConstraints  = cons
      , CX.cdsNRuns        = n
      , CX.cdsCriterion    = crit
      , CX.cdsBudget       = CX.defaultBudget
      , CX.cdsSeed         = Just seed
      , CX.cdsInitial      = Nothing
      , CX.cdsDJConvention = False
      }

-- | **完全カスタムデザインの生成** (pure・Phase 79)。 唯一の生成入口。 'CustomSpec' 1 本
--   (因子 × 固定効果モデル × run 数 × seed × 基準 × 制約 × 'Structure') を受け取り、
--   構造に応じた座標交換 (M / 制約 / 群単位ムーブ) を解いて 'Design' ('KStructured') に包む。
--   **seed 決定的**な純粋関数 (同 seed → 同結果)。
--
--   > -- CRD (最小)
--   > let plan = customDesign (customSpec fs (quadratic ["x1","x2"]) 12 42)
--   >
--   > -- 制約つき CRD
--   > customDesign (customSpec fs fml 8 42)
--   >   { csConstraints = [LinearIneq [("x1",1),("x2",1)] CLeq 0.5] }
--   >
--   > -- split-plot (temp が whole-plot) + 制約
--   > customDesign (customSpec fs (twoWay ["temp","rate"]) 8 50)
--   >   { csStructure = splitPlot ["temp"] 4, csConstraints = [RangeBound "rate" (-0.5) 1] }
--
--   ★制約の因子参照は **coded 単位** ([-1,1]) で書く (座標交換が coded 空間で解くため)。
--   v1 未実装の構造は @unsupported structure@ で error。
customDesign :: CustomSpec -> Design
customDesign cs = case csStructure cs of
  CRD ->
    -- CRD は M=I・per-cell の高速路 ('coordinateExchangePure') へそのまま委譲 (ビット一致)。
    let fs   = csFactors cs
        spec = toCustomSpec (csCriterion cs) (effectiveConstraints cs) fs
                            (csFormula cs) (csNRuns cs) (csSeed cs)
    in case CX.coordinateExchangePure spec of
         Left e   -> error (T.unpack (enrichInfeasError cs e))
         Right cd -> Design fs (LA.toLists (CX.cdMatrix cd)) (KStructured [] (csFormula cs))
  structure ->
    -- 非自明な構造 (SplitPlot/StripPlot/Blocked) は Structure を GroupingPlan に
    -- コンパイルし、 構造駆動エンジン ('structuredExchangePure') で群単位ムーブ + GLS 基準を解く。
    let fs = csFactors cs
    in case buildGrouping fs (csNRuns cs) structure of
         Left e -> error ("customDesign: " <> T.unpack e)
         Right (gplan, groups) ->
           let spec = toCustomSpec (csCriterion cs) (effectiveConstraints cs) fs
                                   (csFormula cs) (csNRuns cs) (csSeed cs)
           in case ST.structuredExchangePure spec gplan of
                Left e       -> error (T.unpack (enrichInfeasError cs e))
                Right (m, _) -> Design fs (LA.toLists m) (KStructured groups (csFormula cs))

-- | 'Structure' を構造駆動エンジンの 'GroupingPlan' (列ごと cells + M⁻¹) と
--   出力用群列 @[(群列名, 各 run の群 ID)]@ にコンパイルする (Phase 79)。 CRD は
--   別処理 ('customDesign' が高速路へ委譲) ゆえここでは扱わない。 v1 未実装構造は
--   @unsupported structure@ で 'Left'。
buildGrouping :: [DesignFactor] -> Int -> Structure
              -> Either Text (ST.GroupingPlan, [(Text, [Int])])
buildGrouping fs n structure = case structure of
  CRD -> Left "buildGrouping: CRD は構造エンジンを使わない (内部エラー)"
  SplitPlot whole nWhole eta col
    | nWhole < 1 -> Left "whole-plot 数 (spNWhole) は 1 以上が必要"
    | n < nWhole -> Left "run 数 n は whole-plot 数 (spNWhole) 以上が必要"
    | null whole -> Left "split-plot に whole-plot 因子名 (spWhole) が空"
    | not (all (`elem` names) whole) ->
        Left ("whole-plot 因子名 "
               <> T.pack (show (map T.unpack (filter (`notElem` names) whole)))
               <> " が因子リストに無い")
    | otherwise ->
        let ids    = balancedGroupIds n nWhole
            idsV   = VS.fromList ids
            wpCols = [ j | (j, f) <- zip [0 :: Int ..] fs, dfName f `elem` whole ]
            cells  = [ if j `elem` wpCols then groupCells ids nWhole else perRowCells n
                     | j <- [0 .. length fs - 1] ]
            mInv   = ST.buildMInvFromGroups n [(eta, idsV)]
        in Right (ST.GroupingPlan cells mInv, [(col, ids)])
  StripPlot whole nWhole etaW wCol strip nStrip etaS sCol
    | nWhole < 1 || nStrip < 1 -> Left "strip-plot の whole-plot 数 / strip 数は 1 以上が必要"
    | n /= nWhole * nStrip ->
        Left ("strip-plot は n = stNWhole × stNStrip が必要 (n=" <> tshowW n
               <> ", " <> tshowW nWhole <> "×" <> tshowW nStrip <> "=" <> tshowW (nWhole * nStrip) <> ")")
    | null whole || null strip -> Left "strip-plot に whole-plot / strip 因子名が空"
    | not (all (`elem` names) (whole ++ strip)) ->
        Left ("strip-plot 因子名 "
               <> T.pack (show (map T.unpack (filter (`notElem` names) (whole ++ strip))))
               <> " が因子リストに無い")
    | not (null (filter (`elem` strip) whole)) ->
        Left "同一因子を whole-plot と strip の両方に指定できません"
    | otherwise ->
        let wpIds    = balancedGroupIds n nWhole          -- 行 i → i `div` nStrip
            stripIds = [ i `mod` nStrip | i <- [0 .. n - 1] ]
            wpCols   = [ j | (j, f) <- zip [0 :: Int ..] fs, dfName f `elem` whole ]
            stCols   = [ j | (j, f) <- zip [0 :: Int ..] fs, dfName f `elem` strip ]
            cells    = [ if j `elem` wpCols then groupCells wpIds nWhole
                         else if j `elem` stCols then groupCells stripIds nStrip
                         else perRowCells n
                       | j <- [0 .. length fs - 1] ]
            mInv     = ST.buildMInvFromGroups n
                         [(etaW, VS.fromList wpIds), (etaS, VS.fromList stripIds)]
        in Right (ST.GroupingPlan cells mInv, [(wCol, wpIds), (sCol, stripIds)])
  Blocked nBlocks eta col
    | nBlocks < 1 -> Left "ブロック数 (blkNBlocks) は 1 以上が必要"
    | n < nBlocks -> Left "run 数 n はブロック数 (blkNBlocks) 以上が必要"
    | otherwise ->
        -- ランダムブロック: 全因子はブロック内で自由 (per-row cell)。 block は run 割付のみで
        -- M = I + η·Z_B Z_Bᵀ に効く。 grouped 列は無い。
        let ids   = balancedGroupIds n nBlocks
            cells = replicate (length fs) (perRowCells n)
            mInv  = ST.buildMInvFromGroups n [(eta, VS.fromList ids)]
        in Right (ST.GroupingPlan cells mInv, [(col, ids)])
  where names  = map dfName fs
        tshowW = T.pack . show

-- | n 行を k 群に均等割り当て (余りは先頭群に +1)。 各行 → 群 ID (0..k-1)。
balancedGroupIds :: Int -> Int -> [Int]
balancedGroupIds n k =
  let base  = n `div` k
      extra = n `mod` k
      sizes = [ if i < extra then base + 1 else base | i <- [0 .. k - 1] ]
  in concat [ replicate s i | (i, s) <- zip [0 ..] sizes ]

-- | 群 ID リストから「同群の行集合」 の cells を作る (grouped 列用)。
groupCells :: [Int] -> Int -> [[Int]]
groupCells ids k = [ [ i | (i, g) <- zip [0 ..] ids, g == w ] | w <- [0 .. k - 1] ]

-- | per-row cells (各行が独立の cell・CRD 因子 / sub-plot 因子用)。
perRowCells :: Int -> [[Int]]
perRowCells n = [ [i] | i <- [0 .. n - 1] ]

-- | RHS 文字列 (R 構文) を placeholder 応答 @_y@ 付きで 'parseRFormula' に通す糖衣。
--   効果 DSL は別モデル表現を作らず 'Formula' を組み立てるだけ (二重管理回避)。
effectFormula :: Text -> Formula
effectFormula rhs =
  either (\e -> error ("effect DSL: formula parse error: " ++ e)) id
         (parseRFormula ("_y ~ " <> rhs))

-- | formula の RHS に 2 次以上の冪 (@Bin Pow@) が含まれるか。 候補水準の既定値
--   (2 次項があれば 3 水準・無ければ 2 水準) を決めるのに使う。
formulaHasPower :: Formula -> Bool
formulaHasPower (Formula _ _ rhs) = go rhs
  where
    go (Bin Pow _ _) = True
    go (Bin _ a b)   = go a || go b
    go (Neg a)       = go a
    go (Index a b)   = go a || go b
    go (App _ as)    = any go as
    go _             = False

-- | D-最適計画 (既定基準 = 'DOpt'・seed = 42・候補水準は自動)。 @n@ = run 数 (必須)。
--   @optimalDesign [contFactor "t" (150,180), contFactor "p" (1,5)] (quadratic ["t","p"]) 10@。
--   候補水準は formula が 2 次項を含めば 3、 他は 2 (明示は 'optimalDesignLevels')。 カテゴリ因子
--   ('catFactor') は候補格子で全水準を展開し、 設計行列で contrast 展開される。
--   @n@ が formula のパラメータ数 @p@ 未満だと error (情報行列が特異)。
optimalDesign :: [DesignFactor]              -- ^ 因子 ('contFactor' / 'catFactor')
              -> Formula                     -- ^ モデル formula (効果 DSL / 'parseRFormula')
              -> Int                         -- ^ run 数 n
              -> Design
optimalDesign = optimalDesignWith DOpt Nothing 42

-- | 候補水準を明示する D-最適計画 (基準 = 'DOpt'・seed = 42)。 各連続因子 [-1,1] を @levels@
--   水準に離散化した格子を候補集合にする (カテゴリ因子は水準数固定なので @levels@ の影響を受けない)。
optimalDesignLevels :: Int                          -- ^ 候補格子の水準数 (各連続因子)
                    -> [DesignFactor]
                    -> Formula
                    -> Int
                    -> Design
optimalDesignLevels levels = optimalDesignWith DOpt (Just levels) 42

-- | 最適計画 (フル制御)。 基準 'OptCriterion' (D/A/I/E/G/Compound/BayesianD)、 候補水準
--   (@Nothing@ = 自動)、 seed を明示。 @n@ = run 数 (必須・@n >= p@ 検査あり)。
optimalDesignWith :: OptCriterion              -- ^ 最適化基準
                  -> Maybe Int                 -- ^ 候補格子の水準数 (Nothing = 自動)
                  -> Int                       -- ^ seed (初期選択)
                  -> [DesignFactor]
                  -> Formula
                  -> Int                       -- ^ run 数 n
                  -> Design
optimalDesignWith crit mLevels seed fs fml n =
  let lv   = maybe (if formulaHasPower fml then 3 else 2) id mLevels
      grid = fullFactorial (map (factorCandidateLevels lv) fs)
  in case candidateXRows fml fs grid of
       Left err -> error ("optimalDesign: 設計行列の構築に失敗: " ++ err)
       Right xRows ->
         let p = if null xRows then 0 else length (head xRows)
         in if n < p
              then error
                ("optimalDesign: run 数 n=" ++ show n
                  ++ " がモデルのパラメータ数 p=" ++ show p
                  ++ " 未満です (n >= p が必要・情報行列が特異になる)")
              else let (idx, _) = OPT.optimalDesign crit xRows n seed
                   in Design fs (map (grid !!) idx) (KCustom fml)

-- | 因子の候補水準集合 (最適計画のグリッド)。 連続 = @[-1,1]@ を @lv@ 等間隔 (candidateGrid 相当)、
--   カテゴリ = 水準 index @{0,…,m-1}@ (水準数固定・@lv@ 無関係)。 全連続なら 'candidateGrid' と一致。
factorCandidateLevels :: Int -> DesignFactor -> [Double]
factorCandidateLevels lv f = case dfKind f of
  Cont _ _ _ -> evenSpaced lv
  Num levels -> map fromIntegral [0 .. length levels - 1]
  Cat levels -> map fromIntegral [0 .. length levels - 1]
  where
    evenSpaced n
      | n <= 1    = [0]
      | otherwise = [ -1 + 2 * fromIntegral i / fromIntegral (n - 1)
                    | i <- [0 .. n - 1] :: [Int] ]

-- | 候補グリッド (coded 因子座標) を formula の設計行列 X 行 (@[[Double]]@) に展開する。
--   連続因子は coded 値のまま数値項に、 カテゴリ因子は水準名 (Text) 列にして contrast 展開させる。
--   応答は placeholder 列を 0 埋め (設計行列は RHS のみに依存し応答値は無関係)。
candidateXRows :: Formula -> [DesignFactor] -> [[Double]] -> Either String [[Double]]
candidateXRows fml fs grid = do
  mf     <- modelFrame fml (candidateFrame fml fs grid)
  (x, _) <- designMatrixF fml mf
  pure (LA.toLists x)

-- | 候補グリッド (coded 座標行) を DataFrame 化 (応答 placeholder 列 + 各因子列)。
--   連続 = coded Double 列、 カテゴリ = 水準名 Text 列 ('modelFrame' で factor 扱い)。
candidateFrame :: Formula -> [DesignFactor] -> [[Double]] -> DX.DataFrame
candidateFrame fml fs grid =
  DX.fromNamedColumns $
    (formResponse fml, DX.fromList (replicate (length grid) (0 :: Double)))
      : [ factorFrameColumn Nothing False f [ row !! j | row <- grid ]
        | (j, f) <- zip [0 :: Int ..] fs ]

-- ---------------------------------------------------------------------------
-- 取り出し
-- ---------------------------------------------------------------------------

designFactorNames :: Design -> [Text]
designFactorNames = map dfName . dsFactors

-- | 実行用 runsheet (uncoded 実値・**連続因子のみ**)。 先頭に @run@ 番号列、 続いて各因子の
--   実値列。 戻り値は 'ColumnSource' なので @designTable plan |-> …@ / 表示に使える。
--   ★カテゴリ因子を含む設計では数値 runsheet に水準名を出せないので **error** となる
--   ('designFrame' を使うこと・Text 列を持つ整形表を返す)。
designTable :: Design -> [(Text, [Double])]
designTable (Design fs coded _) =
  case [ dfName f | f <- fs, isCat (dfKind f) ] of
    (c : _) -> error
      ("designTable: カテゴリ因子 " ++ T.unpack c
        ++ " は数値 runsheet に出せません。 designFrame を使ってください "
        ++ "(Text 列を持つ整形表 DataFrame を返します)")
    []      ->
      ("run", map fromIntegral [1 .. length coded])
        : [ (dfName f, [ tableCell f (row !! j) | row <- coded ])
          | (j, f) <- zip [0 ..] fs ]
  where
    isCat (Cat _) = True
    isCat _       = False
    -- Cont は uncoded 実値、 Num は水準 index → 実水準値。 Cat は上でガード済。
    tableCell f v = case dfKind f of
      Num levels -> numLevelAt levels v
      _          -> uncodeCont f v

-- | runsheet を **整形表** (Hackage @DataFrame@) にする。 連続因子は uncoded 実値の Double 列、
--   **カテゴリ因子は水準名の Text 列** として直接構築する (数値 'designTable' 経由でなく列ごと)。
--   DataFrame は 'ColumnSource' ゆえ @designFrame plan |-> …@ もそのまま通り、 fit 側は
--   Text 列を contrast 展開する。 @print (designFrame plan)@ で型付き ASCII テーブルを確認できる。
designFrame :: Design -> DX.DataFrame
designFrame (Design fs coded kind) =
  DX.fromNamedColumns $
    ("run", DX.fromList (map fromIntegral [1 .. length coded] :: [Double]))
      : [ factorFrameColumn Nothing True f [ row !! j | row <- coded ]
        | (j, f) <- zip [0 :: Int ..] fs ]
      ++ splitGroupColumns kind

-- | 実験者に渡す runsheet を **小数第 @nd@ 位に丸めて**返す ('designFrame' の桁数調整版)。
--   応答曲面 (CCD) の軸点 (±α) など無理数由来の長い小数 (@143.78679656440357@) を
--   @designFrameRound 2 plan@ で @143.79@ 等に整える。 連続 / 数値順序因子の実値列だけを丸め、
--   run 番号 / カテゴリ (Text) / 群列はそのまま。 ★丸めた値がそのまま runsheet の値になる
--   (実験は丸めた水準で行う想定)。 fit にそのまま載せてよい ('designFrame' 同様 'ColumnSource')。
--
--   > print (designFrameRound 2 (centralCompositeDesign [contFactor "temp" (150,180), …]))
designFrameRound :: Int -> Design -> DX.DataFrame
designFrameRound nd (Design fs coded kind) =
  DX.fromNamedColumns $
    ("run", DX.fromList (map fromIntegral [1 .. length coded] :: [Double]))
      : [ factorFrameColumn (Just nd) True f [ row !! j | row <- coded ]
        | (j, f) <- zip [0 :: Int ..] fs ]
      ++ splitGroupColumns kind

-- | 完全カスタムデザインの群列を Text ラベルで作る。 各群列は @<列名>0, <列名>1, …@
--   のラベル (例: @wholePlot0@ / @strip0@ / @block0@)。 CRD (群列なし) や他種別は空。
--   designModelHBM の grouping 列は getTextVec で読まれる (Text 必須) ため Text 化する。
splitGroupColumns :: DesignKind -> [(Text, DX.Column)]
splitGroupColumns (KStructured groups _) =
  [ (col, DX.fromList (map (\i -> col <> T.pack (show i)) ids :: [Text]))
  | (col, ids) <- groups ]
splitGroupColumns _ = []

-- | 因子 1 列を coded 座標列から DataFrame 列に。 連続因子は Double 列 (@uncodeC@=True なら
--   uncoded 実値へ・'designFrame' 用、 False なら coded のまま・'candidateFrame' 用)、
--   カテゴリ因子は水準 index を 'Cat' リストで引いた**水準名 Text 列** ('modelFrame' で factor 扱い)。
--   @mRound = Just nd@ なら Double 列 (連続 / 数値順序) を小数第 @nd@ 位に丸める
--   ('designFrameRound' 用)。 カテゴリ Text 列は丸めない。
factorFrameColumn :: Maybe Int -> Bool -> DesignFactor -> [Double] -> (Text, DX.Column)
factorFrameColumn mRound uncodeC f coded = case dfKind f of
  Cont _ _ _ -> (dfName f, DX.fromList (map (rnd . contVal) coded))
  Num levels -> (dfName f, DX.fromList (map (rnd . numLevelAt levels) coded))  -- 水準 index → 実値 Double
  Cat levels -> (dfName f, DX.fromList (map (levelAt levels) coded :: [Text]))
  where
    contVal = if uncodeC then uncodeCont f else id
    rnd     = maybe id roundTo mRound
    levelAt levels v =
      let i = round v
      in if i >= 0 && i < length levels then levels !! i else "?"

-- | 小数第 @n@ 位への丸め (負値・整数もそのまま)。 @roundTo 2 143.78679 = 143.79@。
roundTo :: Int -> Double -> Double
roundTo n x = fromIntegral (round (x * m) :: Integer) / m
  where m = 10 ^^ n

-- | 数値順序因子の coded (水準 index) を実水準値へ。 範囲外は NaN (呼び元は index を保証)。
numLevelAt :: [Double] -> Double -> Double
numLevelAt levels v =
  let i = round v
  in if i >= 0 && i < length levels then levels !! i else 0/0

-- | 連続因子の coded 値 @c@ を uncoded 実値へ。 線形は @center + c·half@、 対数は
--   @10^(logCenter + c·logHalf)@ (幾何)。 カテゴリ因子で呼ぶと error (呼び元が連続に
--   限定して使う・'designTable' はガード済)。
uncodeCont :: DesignFactor -> Double -> Double
uncodeCont f c = case dfKind f of
  Cont lo hi SLinear -> (lo + hi) / 2 + c * (hi - lo) / 2
  Cont lo hi SLog    ->
    let llo = logBase 10 lo; lhi = logBase 10 hi
    in 10 ** ((llo + lhi) / 2 + c * (lhi - llo) / 2)
  Num _      -> error ("uncodeCont: " ++ T.unpack (dfName f) ++ " は数値順序因子です (numLevelAt を使う)")
  Cat _      -> error ("uncodeCont: " ++ T.unpack (dfName f) ++ " はカテゴリ因子です")

-- | 連続因子の uncoded 実値 @x@ を coded 値へ ('uncodeCont' の逆)。 線形は @(x−center)/half@、
--   対数は @(log10 x − logCenter)/logHalf@。 対数因子で @x <= 0@ は NaN (呼び元が正値を保証)。
--   カテゴリ/数値順序因子で呼ぶと error。
codeCont :: DesignFactor -> Double -> Double
codeCont f x = case dfKind f of
  Cont lo hi SLinear ->
    let half = (hi - lo) / 2
    in if half == 0 then 0 else (x - (lo + hi) / 2) / half
  Cont lo hi SLog    ->
    let llo = logBase 10 lo; lhi = logBase 10 hi
        lhalf = (lhi - llo) / 2
    in if lhalf == 0 then 0 else (logBase 10 x - (llo + lhi) / 2) / lhalf
  Num _ -> error ("codeCont: " ++ T.unpack (dfName f) ++ " は数値順序因子です")
  Cat _ -> error ("codeCont: " ++ T.unpack (dfName f) ++ " はカテゴリ因子です")

-- ---------------------------------------------------------------------------
-- 設計の保存 / DataFrame からの復元 (Phase 78.K)
-- ---------------------------------------------------------------------------

-- | 設計の **runsheet** ('designFrame') を CSV に書き出す。 実験者に渡す runsheet
--   (uncoded 実値・run 番号列・カテゴリは水準名) がそのまま保存される。
--
--   > saveDesign "runsheet.csv" plan
saveDesign :: FilePath -> Design -> IO ()
saveDesign path = DXIO.writeCsv path . designFrame

-- | **DataFrame から設計 ('Design') を復元**する。 因子 (名前 + 種類 + 範囲/水準) と
--   モデル formula (効果 DSL 'mainEffects' / 'quadratic' 等 or 'parseRFormula') を明示し、
--   @df@ の各因子列を coded 化して 'KCustom' 設計に包む。 CSV から読んだ runsheet を
--   解析ワークフロー (@df |-> designModel plan "y"@ / 'rsmAnalysis') に載せ直すのに使う。
--
--   > let plan = planFromFrame [contFactor "temp" (150,180), contFactor "time" (10,20)]
--   >                          (quadratic ["temp","time"]) loadedDf
--   > filledDf |-> designModel plan "y"
--
--   ★fit は formula + df だけで動く ('designModel') が、 'rsmAnalysis' /
--   'steepestAscentNatural' は coded 幾何を使うので、 因子の範囲/水準を正しく渡すこと。
--   因子列が @df@ に無い / 型が合わない場合は error。
planFromFrame :: [DesignFactor] -> Formula -> DX.DataFrame -> Design
planFromFrame fs fml df =
  let cols  = map (`factorCodedColumn` df) fs   -- 各因子の coded 列 (行順)
      coded = transpose cols                    -- 行 = run、 列 = 因子
  in Design fs coded (KCustom fml)

-- | 因子 1 列を @df@ から取り出し coded 座標列にする。 連続 = @(x−center)/half@、
--   数値順序 = 最近傍水準の index、 カテゴリ = 水準名の index (designFrame の逆変換)。
factorCodedColumn :: DesignFactor -> DX.DataFrame -> [Double]
factorCodedColumn f df = case dfKind f of
  Cont _ _ _ -> [ codeCont f x | x <- doubles ]   -- 線形/対数は codeCont が分岐
  Num levels -> [ fromIntegral (nearestIndex levels x) | x <- doubles ]
  Cat levels -> [ fromIntegral (catIndex levels t)     | t <- texts ]
  where
    nm = dfName f
    doubles = case getDoubleVec nm df of
      Just v  -> V.toList v
      Nothing -> error ("planFromFrame: 数値因子列 '" <> T.unpack nm <> "' が df に無い / 数値でない")
    texts = case getTextVec nm df of
      Just v  -> V.toList v
      Nothing -> error ("planFromFrame: カテゴリ因子列 '" <> T.unpack nm <> "' が df に無い / Text でない")
    nearestIndex levels x =
      fst (minimumBy (comparing (\(_, l) -> abs (l - x))) (zip [0 :: Int ..] levels))
    catIndex levels t = case elemIndex t levels of
      Just i  -> i
      Nothing -> error
        ("planFromFrame: カテゴリ因子 '" <> T.unpack nm <> "' に水準 '" <> T.unpack t
          <> "' が定義されていません (catFactor の水準: " <> show (map T.unpack levels) <> ")")

-- | 設計種別からモデル formula 文字列を生成 (@y@ = 応答列名)。
--   要因計画 = 全交互作用 (@y ~ x1 * x2 * …@)、 RSM = 2 次
--   (@y ~ x1 + x2 + x1:x2 + I(x1^2) + I(x2^2)@)、 一部実施 = **主効果のみ**
--   (@y ~ x1 + x2 + …@・交互作用は交絡ゆえ v1 は含めない)。
--   最適計画 ('KCustom') は焼き込んだ formula の応答を @y@ に差し替えて返す
--   (native 正規形。 'multiLMModel' は @~@ の有無で R/独自 front-end を自動判別する)。
designFormula :: Design -> Text -> Text
designFormula (Design fs _ kind) y =
  let names = map dfName fs
      k     = length names
      inter = [ (names !! i) <> ":" <> (names !! j)
              | i <- [0 .. k - 1], j <- [i + 1 .. k - 1] ]
      sq    = [ "I(" <> n <> "^2)" | n <- names ]
      -- 主効果項: 数値順序因子 ('Num') は直交多項式 opoly(name, 水準数−1)
      -- (linear+quadratic…・実測間隔で直交)、 連続/カテゴリは主効果名 (Cat は engine が contrast 展開)。
      mainTerm f = case dfKind f of
        Num levels -> "opoly(" <> dfName f <> "," <> tshow (length levels - 1) <> ")"
        _          -> dfName f
  in case kind of
    KCustom fml       -> prettyFormula (fml { formResponse = y })
    KStructured _ fml -> prettyFormula (fml { formResponse = y })  -- 固定効果は KCustom 同様
    KFactorial  -> y <> " ~ " <> T.intercalate " * " names
    KRSM        -> y <> " ~ " <> T.intercalate " + " (names ++ inter ++ sq)
    KFractional -> y <> " ~ " <> T.intercalate " + " (map mainTerm fs)  -- 主効果のみ
    -- 主効果 + 主効果と交絡しない 2FI の代表 (交絡群ごと 1 個)。
    KFracInter gens ->
      let reps = [ (names !! (i - 1)) <> ":" <> (names !! (j - 1))
                 | [i, j] <- clearTwoFactorInteractions k gens ]
      in y <> " ~ " <> T.intercalate " + " (map mainTerm fs ++ reps)
  where tshow = T.pack . show

-- ---------------------------------------------------------------------------
-- 応答曲面 解析 (自然単位で報告) — Phase 78.G-d
-- ---------------------------------------------------------------------------
--
-- R rsm の要点は「fit を coded 空間でやる」ことではない (coded/uncoded fit は予測が
-- 同一・単なる便宜)。 本質は **runsheet を自然単位で発行し、 スケール依存な最適化幾何
-- (停留点 / canonical / steepest ascent) だけ内部で coded の計量を使い、 結果を自然単位で
-- 報告する** ワークフロー。 ここではその報告レイヤを与える。
--
--   * fit そのものは 'designModel' が自然単位で行う (係数がそのまま実単位で読める)。
--   * 一方、 停留点の方向・canonical 軸・steepest ascent 方向は計量依存なので、
--     設計が保持する coded 行列 ('Design' の coded ±1/±α) で二次モデルを当て
--     ('fitQuadratic')、 幾何を coded で解いてから 'uncodeCont' で自然単位へ decode する。

-- | 応答曲面の停留点の性質 (canonical 固有値の符号で判定)。
data RSMNature = RMaximum | RMinimum | RSaddle
  deriving (Show, Eq)

-- | 応答曲面 解析レポート。 停留点・予測は**自然単位**、 canonical 方向は coded 座標
--   (設計の実験範囲を単位に取った軸) で報告する。
data RSMReport = RSMReport
  { rsmStationary :: ![(Text, Double)]
    -- ^ 停留点 (因子名 → **自然単位**の値)。
  , rsmPredicted  :: !Double
    -- ^ 停留点での予測応答。
  , rsmNature     :: !RSMNature
    -- ^ 極大 / 極小 / 鞍点。
  , rsmInRegion   :: !Bool
    -- ^ 停留点が実験領域 (全因子 coded @|x| <= 1@) の内側か。 外なら外挿。
  , rsmCanonical  :: ![(Double, [Double])]
    -- ^ canonical: (固有値, coded 方向ベクトル) を固有値昇順で。 固有値の大きさ =
    --   その方向の曲率 (負=下に凸で応答が落ちる方向 / 正=上に凸)。
  , rsmR2         :: !Double
    -- ^ 当てた二次モデルの R²。
  } deriving (Show)

-- | 設計 + 応答 @ys@ から応答曲面を解析し、 停留点・性質・canonical・R² を返す。
--   二次モデルを設計の coded 行列で当て ('fitQuadratic')、 停留点を自然単位へ decode。
--   **連続因子 (RSM 系設計) 専用** — カテゴリを含めば呼び名付きで error。
--   @ys@ は runsheet ('designTable' / 'designFrame') と同じ run 順の応答値。
rsmAnalysis :: Design -> [Double] -> RSMReport
rsmAnalysis (Design fs0 coded _) ys =
  let fs        = requireContinuous "rsmAnalysis" fs0  -- 連続専用ガード (error を強制)
      qf        = fitQuadratic coded ys
      (xC, yC, _) = optimumPoint qf
      canon     = canonicalAnalysis qf
      eigs      = map fst canon
      nature
        | all (< 0) eigs = RMaximum
        | all (> 0) eigs = RMinimum
        | otherwise      = RSaddle
      inRegion  = all (\x -> abs x <= 1 + 1e-9) xC
      stationary = [ (dfName f, uncodeCont f x) | (f, x) <- zip fs xC ]
  in RSMReport
       { rsmStationary = stationary
       , rsmPredicted  = yC
       , rsmNature     = nature
       , rsmInRegion   = inRegion
       , rsmCanonical  = canon
       , rsmR2         = qfR2 qf
       }

-- | 自然単位の steepest ascent / descent 経路。 一次係数の勾配方向は計量依存なので、
--   設計の coded 行列で当てた coefs で coded 空間の方向を取り (= 各因子の実験範囲を
--   単位にした scale 不変な方向)、 生成した各点を 'uncodeCont' で自然単位へ decode する。
--   実験者はこの自然単位の系列をそのまま次の試行に使える。
--   @step@ は **coded スケール**の 1 歩幅 (例 0.5)、 @nSteps@ は歩数 (path 長 = nSteps+1)。
--   **連続因子専用**。
steepestAscentNatural
  :: Bool                 -- ^ True = ascent (最大化) / False = descent
  -> Design
  -> [Double]             -- ^ 応答 @ys@ (run 順)
  -> Double               -- ^ step (coded スケール)
  -> Int                  -- ^ nSteps
  -> [[(Text, Double)]]   -- ^ 経路。 各点 = [(因子名, 自然単位値)]、 先頭 = 設計中心
steepestAscentNatural maximize (Design fs0 coded _) ys step nSteps =
  let fs   = requireContinuous "steepestAscentNatural" fs0
      qf   = fitQuadratic coded ys
      k    = length fs
      sar  = steepestAscentFromQuad maximize (replicate k 0) qf step nSteps
  in [ [ (dfName f, uncodeCont f x) | (f, x) <- zip fs row ]
     | row <- sarStepPoints sar ]
