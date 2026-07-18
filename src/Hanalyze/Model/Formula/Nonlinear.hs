{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Hanalyze.Model.Formula.Nonlinear
-- Description : Formula DSL の非線形最小二乗 (NLS) fit
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Formula DSL — 非線形最小二乗 (NLS、 A4)。
--   現状 @a*exp(-b*x)@ のように **パラメータがデータ式の内側に現れる式** ('designMatrixF'
--   は線形でないとして 'Left') を、 parse 済 AST を評価関数化して既存の最適化器
--   ('Hanalyze.Optim.NelderMead') で SSR を最小化し fit する。
--
--   ★考え方: 線形 OLS と違い param 名が ŷ に効く。 @evalNL@ が「params 表 + ModelFrame」 から
--   右辺式を **行ごとの ŷ ベクトル** に評価する (param は定数、 連続データ変数は列ベクトル)。
--   目的関数 @SSR(θ) = Σ(y − ŷ(θ))²@ を Nelder-Mead で最小化。
--   ★初期値はユーザ必須 (NLS は初期値依存)。 factor 添字は非対応 (線形側で扱う)。
--   ★最適化器は IO を返すが決定論的ゆえ 'unsafePerformIO' で pure 化 (Convert.hs 同方針)。
--
--   plot 非依存・portable。
module Hanalyze.Model.Formula.Nonlinear
  ( NLSResult (..)
  , fitNLS
  , evalNL
  ) where

import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Vector             as V
import           System.IO.Unsafe        (unsafePerformIO)

import           Hanalyze.Model.Formula  (BinOp (..), Formula (..), Term (..))
import           Hanalyze.Model.Formula.Frame
import           Hanalyze.Optim.Common    (OptimResult (..))
import           Hanalyze.Optim.NelderMead (runNelderMead)
import qualified DataFrame.Internal.DataFrame  as DX

-- | 非線形 fit の結果。
data NLSResult = NLSResult
  { nlsParams    :: [(Text, Double)]   -- ^ 推定パラメータ (名前つき)
  , nlsFitted    :: V.Vector Double    -- ^ ŷ
  , nlsResidual  :: V.Vector Double    -- ^ y − ŷ
  , nlsSSR       :: Double             -- ^ 残差平方和
  , nlsConverged :: Bool               -- ^ 最適化器が許容誤差で停止したか
  }
  deriving (Eq, Show)

-- | 右辺式を **行ごとの値ベクトル** に評価する。 params は表から定数、 連続データ変数は
--   ModelFrame の列、 factor / 応答は 'Left'。 (線形の 'evalData' と違い param を許す。)
evalNL :: [(Text, Double)] -> ModelFrame -> Term -> Either String (V.Vector Double)
evalNL pm mf = go
  where
    n = mfNRows mf
    go t = case t of
      Lit d -> Right (V.replicate n d)
      Ref x -> case lookup x (mfRoles mf) of
        Just (RoleContinuous v) -> Right v
        Just (RoleResponse _)   -> Left $ "応答 '" <> T.unpack x <> "' をデータ式に使えません"
        Just (RoleFactor _ _)   -> Left $ "非線形フィットは factor '" <> T.unpack x
                                           <> "' を扱えません"
        Nothing -> case lookup x pm of
          Just d  -> Right (V.replicate n d)
          Nothing -> Left $ "未知の変数 '" <> T.unpack x <> "'"
      Neg a -> V.map negate <$> go a
      App f [a] | Just fn <- lookup f unaryFns -> V.map fn <$> go a
      App f _   -> Left $ "未対応の関数 '" <> T.unpack f
                           <> "' (log/exp/sqrt/sin/cos/tan/abs の単項のみ)"
      Bin op a b -> V.zipWith (binFn op) <$> go a <*> go b
      Index _ _  -> Left "非線形フィットは factor 添字を扱えません"

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

-- | 非線形最小二乗。 @inits@ = 各パラメータの初期値 (mfParams を網羅する必要がある)。
--   SSR を Nelder-Mead で最小化する。 不正値 (NaN) を出すパラメータ域は +∞ で罰する。
fitNLS :: Formula -> DX.DataFrame -> [(Text, Double)] -> Either String NLSResult
fitNLS f@(Formula _ _ rhs) df inits = do
  mf <- modelFrame f df
  yv <- case mfRoles mf of
          ((_, RoleResponse v) : _) -> Right v
          _                         -> Left "ModelFrame に応答列がありません"
  let pnames  = map fst inits
      missing = filter (`notElem` pnames) (mfParams mf)
  if not (null missing)
    then Left $ "初期値が無いパラメータ: " <> show (map T.unpack missing)
    else do
      _ <- evalNL inits mf rhs                       -- 評価可能性を先に検証
      let sse yhat = V.sum (V.map (\e -> e * e) (V.zipWith (-) yv yhat))
          ssrAt vals = case evalNL (zip pnames vals) mf rhs of
                         Right yhat -> let s = sse yhat in if isNaN s then 1 / 0 else s
                         Left _     -> 1 / 0
          res  = unsafePerformIO (runNelderMead ssrAt (map snd inits))
          pm   = zip pnames (orBest res)
      yhat <- evalNL pm mf rhs
      let resid = V.zipWith (-) yv yhat
      Right NLSResult
        { nlsParams    = pm
        , nlsFitted    = yhat
        , nlsResidual  = resid
        , nlsSSR       = sse yhat
        , nlsConverged = orConverged res
        }
