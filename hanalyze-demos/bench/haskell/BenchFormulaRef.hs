{-# LANGUAGE OverloadedStrings #-}

-- | Phase 47 A5: Formula DSL の Haskell 参照値生成器。
--   bench/python/bench_formula.py が statsmodels / scipy と突合するための ŷ/R²/係数を
--   実際の Hanalyze 実装で計算し formula_haskell_ref.json に書き出す (再現可能化)。
--
--   実行: cabal run formula-ref-gen   (cwd は repo root を想定、 bench/python/ に書く)
module Main (main) where

import           Data.List              (intercalate)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified DataFrame.Internal.Column    as DX
import qualified DataFrame.Internal.DataFrame  as DX
import qualified Numeric.LinearAlgebra  as LA

import qualified Hanalyze.Model.Core              as Core
import           Hanalyze.Model.Formula.RFormula  (parseModel)
import           Hanalyze.Model.Formula.Design    (fitLMF, fitWLSF, defaultWLS,
                                                   WLSConfig (..))
import           Hanalyze.Model.Formula.Nonlinear (fitNLS, nlsParams)
import           Hanalyze.Model.Formula.Mixed     (fitMixedLME)
import           Hanalyze.Model.GLMM              (GLMMResultRE (..))

-- ============================================================================
-- データ (Python 側と完全一致させる)
-- ============================================================================

dfOLS :: DX.DataFrame
dfOLS = DX.fromNamedColumns
  [ ("y", DX.fromList ([10,20,30,40,50,60,12,22,34,44,52,62] :: [Double]))
  , ("g", DX.fromList (["A","A","B","B","C","C","A","A","B","B","C","C"] :: [Text]))
  , ("t", DX.fromList (["P","Q","P","Q","P","Q","P","Q","P","Q","P","Q"] :: [Text]))
  , ("x", DX.fromList ([1,2,3,4,5,6,1,2,3,4,5,6] :: [Double]))
  ]

olsFormulas :: [Text]
olsFormulas =
  [ "y ~ x"
  , "y ~ C(g) * C(t)"
  , "y ~ C(g) + C(g):x"
  , "y ~ x + I(x**2)"
  , "y ~ C(g, Sum)"                  -- A2 contrast (ŷ は treatment と不変 → statsmodels 一致)
  , "y ~ C(g, Sum) + C(g, Sum):x"
  ]

dfWLS :: DX.DataFrame
dfWLS = DX.fromNamedColumns
  [ ("y", DX.fromList ([2.1,3.9,6.2,7.8,10.1,12.2,13.8,16.1] :: [Double]))
  , ("x", DX.fromList ([1,2,3,4,5,6,7,8] :: [Double]))
  , ("w", DX.fromList ([1,1,2,2,3,3,4,4] :: [Double]))
  ]

-- Phase 48: mixed-effects (random intercept + slope) 突合用データ。
-- 4 群 × 5 obs、 群ごとに切片・傾きが異なる線形 (固定平均 β≈[2,3])。
dfMixed :: DX.DataFrame
dfMixed = DX.fromNamedColumns
  [ ("y", DX.fromList ([ 3.01,6.49,10.01,13.49,17.00
                       , 1.01,3.49, 6.01, 8.49,11.00
                       , 2.51,5.19, 7.91,10.59,13.30
                       , 1.51,4.79, 8.11,11.39,14.70 ] :: [Double]))
  , ("x", DX.fromList (concat (replicate 4 [0,1,2,3,4]) :: [Double]))
  , ("g", DX.fromList (concatMap (replicate 5) (["A","B","C","D"] :: [Text])))
  ]

nlsXs :: [Double]
nlsXs = [0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5]

dfNLS :: DX.DataFrame
dfNLS = DX.fromNamedColumns
  [ ("y", DX.fromList (map (\x -> 3.0 * exp (negate 0.5 * x)) nlsXs))   -- a=3 b=0.5
  , ("x", DX.fromList nlsXs)
  ]

-- ============================================================================
-- JSON (手書き・依存最小)
-- ============================================================================

jstr :: String -> String
jstr s = "\"" ++ s ++ "\""

jarr :: [Double] -> String
jarr xs = "[" ++ intercalate ", " (map show xs) ++ "]"

main :: IO ()
main = do
  olsEntries <- mapM olsEntry olsFormulas
  wlsJson    <- pure wlsEntry
  nlsJson    <- pure nlsEntry
  mixedJson  <- pure mixedEntry
  let body = intercalate ",\n  " (olsEntries ++ [wlsJson, nlsJson, mixedJson])
      json = "{\n  " ++ body ++ "\n}\n"
  writeFile "bench/python/formula_haskell_ref.json" json
  putStrLn ("formula_haskell_ref.json 生成: OLS " ++ show (length olsEntries)
            ++ " + __wls__ + __nls__ + __mixed__")

olsEntry :: Text -> IO String
olsEntry f =
  case parseModel f >>= \fm -> fitLMF fm dfOLS of
    Left e        -> error ("OLS " ++ T.unpack f ++ ": " ++ e)
    Right (fr, _) ->
      pure $ jstr (T.unpack f) ++ ": {\"r2\": " ++ show (Core.rSquared1 fr)
             ++ ", \"yhat\": " ++ jarr (Core.fittedList fr) ++ "}"

-- WLS 係数 (parameterization 同一ゆえ係数も突合可)。
wlsEntry :: String
wlsEntry =
  case parseModel "y ~ x" >>= \fm -> fitWLSF defaultWLS { wcWeights = Just "w" } fm dfWLS of
    Left e        -> error ("WLS: " ++ e)
    Right (fr, _) ->
      jstr "__wls__" ++ ": {\"coef\": " ++ jarr (LA.toList (Core.coefficientsV fr)) ++ "}"

-- NLS パラメータ。
nlsEntry :: String
nlsEntry =
  case parseModel "y x = a * exp(-b * x)" of
    Left e   -> error ("NLS parse: " ++ e)
    Right fm -> case fitNLS fm dfNLS [("a",1),("b",1)] of
      Left e  -> error ("NLS: " ++ e)
      Right r -> let pm = nlsParams r
                     val k = maybe (0/0) id (lookup k pm)
                 in jstr "__nls__" ++ ": {\"a\": " ++ show (val "a")
                    ++ ", \"b\": " ++ show (val "b") ++ "}"

-- 混合効果 (random intercept + slope) の β / G (2×2) / σ²。
-- 突合先 = statsmodels smf.mixedlm(..., re_formula="~x").fit(reml=False) (ML)。
mixedEntry :: String
mixedEntry =
  case fitMixedLME "y ~ x + (1+x|g)" dfMixed of
    Left e         -> error ("mixed: " ++ e)
    Right (res, _) ->
      let beta = LA.toList (Core.coefficientsV (reFixed res))
          g    = LA.toLists (reRandCov res)        -- [[g00,g01],[g10,g11]]
          flatG = concat g
      in jstr "__mixed__" ++ ": {\"beta\": " ++ jarr beta
         ++ ", \"cov_re\": " ++ jarr flatG
         ++ ", \"sigma2\": " ++ show (reResidVar res) ++ "}"
