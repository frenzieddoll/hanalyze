{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hanalyze.Model.FitYByX
-- Description : JMP "Fit Y by X" platform 相当の自動 dispatch wrapper
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- JMP \"Fit Y by X\" platform 相当の wrapper。
--
-- X / Y それぞれが連続 (Continuous) か カテゴリ (Categorical) かで
-- 適切な解析を自動 dispatch する:
--
-- @
--   X \\ Y  | Continuous           | Categorical
--   --------+----------------------+---------------------
--   Cont    | 単回帰 (LM)          | logistic GLM
--   Cat     | one-way ANOVA        | chi-square independence
-- @
--
-- canvas frontend で 「変数 2 つドラッグ → 自動分析」 を支える backend wrapper。
module Hanalyze.Model.FitYByX
  ( VarType (..)
  , FitYByXResult (..)
  , fitYByX
  ) where

import qualified Data.Vector             as V
import qualified Numeric.LinearAlgebra   as LA
import           Data.List               (nub, sort)
import           Data.Text               (Text)

import qualified Hanalyze.Model.Core     as Core
import qualified Hanalyze.Model.LM       as LM
import qualified Hanalyze.Model.GLM      as GLM
import qualified Hanalyze.Stat.Test      as ST

-- ===========================================================================
-- 型
-- ===========================================================================

data VarType
  = Continuous
  | Categorical
  deriving (Show, Eq)

data FitYByXResult
  = FitContCont !Core.FitResult
    -- ^ 単回帰: y = β₀ + β₁ x
  | FitCatCont  !ST.TestResult ![Double]
    -- ^ one-way ANOVA + group means (group order = sort.nub of x)
  | FitContCat  !Core.FitResult
    -- ^ logistic GLM: P(Y=1) = sigmoid(β₀ + β₁ x)
  | FitCatCat   !ST.TestResult
    -- ^ chi-square independence
  deriving (Show)

-- ===========================================================================
-- 公開 API
-- ===========================================================================

-- | X / Y の型に応じて適切な解析を dispatch する。
--   入力は両方とも Double Vector。 Categorical の場合は整数値を Double 化
--   して渡す前提 (例: 0, 1, 2, ...)。
fitYByX
  :: VarType -> VarType
  -> LA.Vector Double      -- ^ X
  -> LA.Vector Double      -- ^ Y
  -> Either Text FitYByXResult
fitYByX xt yt x y
  | LA.size x /= LA.size y =
      Left "fitYByX: X and Y must have the same length"
  | LA.size x < 2 =
      Left "fitYByX: need at least 2 observations"
  | otherwise = case (xt, yt) of
      (Continuous, Continuous) ->
        let xMat = LA.fromColumns [LA.fromList (replicate (LA.size x) 1), x]
        in Right (FitContCont (LM.fitLMVec xMat y))

      (Categorical, Continuous) ->
        let levels = sort (nub (LA.toList x))
            groups = [ LA.fromList
                        [ LA.atIndex y i
                        | i <- [0 .. LA.size x - 1]
                        , LA.atIndex x i == lvl ]
                     | lvl <- levels ]
            tr     = ST.anovaOneWay groups
            means  = [ LA.sumElements g / fromIntegral (LA.size g)
                     | g <- groups ]
        in if any ((< 1) . LA.size) groups
             then Left "fitYByX (cat × cont): some groups are empty"
             else Right (FitCatCont tr means)

      (Continuous, Categorical) ->
        -- Y must be binary 0/1 for logistic
        let ys = LA.toList y
        in if not (all (\v -> v == 0 || v == 1) ys)
             then Left "fitYByX (cont × cat): Y must be binary 0/1 for logistic GLM"
             else
               let xMat = LA.fromColumns
                            [LA.fromList (replicate (LA.size x) 1), x]
               in Right (FitContCat (GLM.fitGLM GLM.Binomial xMat y))

      (Categorical, Categorical) ->
        let xLevels = sort (nub (LA.toList x))
            yLevels = sort (nub (LA.toList y))
            cell xl yl = fromIntegral $ length
              [ () | i <- [0 .. LA.size x - 1]
                   , LA.atIndex x i == xl
                   , LA.atIndex y i == yl ]
            tbl = LA.fromLists
              [ [ cell xl yl | yl <- yLevels ] | xl <- xLevels ]
        in if length xLevels < 2 || length yLevels < 2
             then Left "fitYByX (cat × cat): need at least 2 levels per axis"
             else Right (FitCatCat (ST.chiSquareIndep tbl))
  where
    _ = V.length :: V.Vector Int -> Int  -- silence warn
