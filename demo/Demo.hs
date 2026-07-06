{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified DataFrame.Internal.Column    as DX
import qualified DataFrame.Internal.DataFrame  as DX
import Hanalyze.Model.GLMM
import Hanalyze.Model.Core (coeffList, rSquared1, fittedList)
import Hanalyze.Model.LM   (multiPolyDesignMatrix, fitLMVec)

import qualified Data.Vector           as V
import qualified Data.Text             as T
import qualified Numeric.LinearAlgebra as LA
import Data.List   (zip4)
import Text.Printf (printf)

-- ---------------------------------------------------------------------------
-- テストデータ: 3クラスの試験結果
--
-- 真のモデル: score = 64 + u_school + 2×hours + ε
--   u_A ≈ +20,  u_B ≈ 0,  u_C ≈ -20
--
-- クラスA(優秀): 1〜5時間、成績80台  ← 少ない時間で高得点
-- クラスB(平均): 3〜7時間、成績60台
-- クラスC(苦手): 6〜10時間、成績40台 ← 多くの時間で低得点
--
-- OLSで見ると: 「時間 ↑ → 成績 ↓」(Simpson's paradox)
-- GLMMで見ると: 「時間 +1h → +2点」(真の効果)
-- ---------------------------------------------------------------------------

hoursVec :: V.Vector Double
hoursVec = V.fromList [1,2,3,4,5, 3,4,5,6,7, 6,7,8,9,10]

scoresVec :: V.Vector Double
scoresVec = V.fromList
  [ 80.2, 82.0, 84.1, 86.0, 88.2   -- class A
  , 59.9, 62.1, 64.0, 66.2, 68.1   -- class B
  , 40.1, 42.2, 44.0, 45.9, 47.8 ] -- class C

schoolVec :: V.Vector String   -- annotated for readability; converted below
schoolVec = V.fromList
  ["A","A","A","A","A", "B","B","B","B","B", "C","C","C","C","C"]

main :: IO ()
main = do
  let df = DX.insertColumn "hours"  (DX.fromList (V.toList hoursVec :: [Double]))
         $ DX.insertColumn "score"  (DX.fromList (V.toList scoresVec :: [Double]))
         $ DX.insertColumn "school" (DX.fromList
             (["A","A","A","A","A","B","B","B","B","B","C","C","C","C","C"] :: [T.Text]))
         $ DX.empty

  -- ── OLS (school を無視した単純回帰) ──────────────────────────────────
  let dm     = multiPolyDesignMatrix [(hoursVec, 1)]
      y      = LA.fromList (V.toList scoresVec)
      olsRes = fitLMVec dm y
      (b0, b1) = case coeffList olsRes of { (a:b:_) -> (a,b); _ -> (0,0) }

  putStrLn "╔══════════════════════════════════════════════════════════╗"
  putStrLn "║  OLS  (school を無視した単純回帰)                       ║"
  putStrLn "╚══════════════════════════════════════════════════════════╝"
  printf "  β₀ (切片)   : %8.3f\n" b0
  printf "  β₁ (hours)  : %8.3f   ← 負! 時間が増えると成績が下がる？\n" b1
  printf "  R²           : %8.3f\n" (rSquared1 olsRes)
  putStrLn "  ↑ Simpson's paradox: schoolベースライン差がhours効果を逆転させている"

  -- ── GLMM (school ランダム切片) ────────────────────────────────────────
  putStrLn ""
  putStrLn "╔══════════════════════════════════════════════════════════╗"
  putStrLn "║  GLMM (school ランダム切片モデル)                       ║"
  putStrLn "╚══════════════════════════════════════════════════════════╝"
  case fitLMEDataFrame [("hours", 1)] "school" "score" df of
    Nothing -> putStrLn "Error: GLMM推定に失敗"
    Just gr -> do
      let (g0, g1) = case coeffList (glmmFixed gr) of { (a:b:_) -> (a,b); _ -> (0,0) }

      putStrLn "  固定効果:"
      printf "    β₀ (切片)   : %8.3f\n" g0
      printf "    β₁ (hours)  : %8.3f   ← 正! 真の効果を回収\n" g1
      putStrLn "  分散成分:"
      printf "    σ²_u (school間) : %8.3f\n" (glmmRandVar gr)
      printf "    σ²   (残差)     : %8.3f\n" (glmmResidVar gr)
      printf "    ICC              : %8.3f  (分散の%.0f%%がschool間)\n"
             (glmmICC gr) (glmmICC gr * 100)
      putStrLn "  BLUPs (schoolごとのランダム切片 û_j):"
      mapM_ (\(s, u) -> printf "    %s : %+8.3f\n" s u)
            (zip (V.toList (glmmGroups gr)) (V.toList (glmmBLUPs gr)))
      putStrLn ""
      putStrLn "  観測値 vs 条件付きフィット値:"
      putStrLn "  school  hours  actual  fitted  resid"
      let fitted  = fittedList (glmmFixed gr)
          sLabels = ["A","A","A","A","A","B","B","B","B","B","C","C","C","C","C"] :: [String]
      mapM_ (\(s, h, ya, yf) ->
               printf "    %-4s   %5.0f  %6.1f  %6.1f  %+5.2f\n"
                      s h ya yf (ya - yf))
            (zip4 sLabels (V.toList hoursVec) (V.toList scoresVec) fitted)
