{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.Design.GaugeRRSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Hanalyze.Model.Formula
import Hanalyze.Model.Formula.Frame
import Hanalyze.Model.Formula.Design
import Hanalyze.Model.Formula.RFormula
import Hanalyze.Model.Formula.Nonlinear
import Hanalyze.Model.Formula.Mixed
import Hanalyze.Model.GLMM
import Hanalyze.Model.GLM (Family (..), LinkFn (..))
import Hanalyze.Stat.Distribution (Transform)
import Data.List (sort, nub)
import Control.Monad (forM, forM_)
import System.IO.Temp (withSystemTempFile)
import System.IO     (hPutStr, hClose)
import           Hanalyze.Model.HBM.Ast (Expr (..), Lit (..), DoStmt (..), Err)
import           Data.IORef         (newIORef, readIORef, modifyIORef')
import qualified Data.Vector as V
import qualified Data.Text   as T
import qualified Hanalyze.Design.GaugeRR    as GRR
import SpecHelper

spec :: Spec
spec = do
  describe "Hanalyze.Design.GaugeRR (Phase 10)" $ do
    -- 3 parts × 2 operators × 2 reps = 12 obs
    -- 部品 0/1/2 で本来 ばらつき、 操作者間は小さい
    let crossedData =
          let parts = V.fromList (concat (replicate 4 [0, 1, 2]))  -- 12 obs total
              ops   = V.fromList (concat
                        [ [0, 0, 0]  -- part rep 1 ops
                        , [0, 0, 0]  -- part rep 2 ops
                        , [1, 1, 1]
                        , [1, 1, 1]
                        ])
              -- Each part has 4 obs (2 ops × 2 reps), part effect strong
              ys = V.fromList
                [10.0, 20.0, 30.0,    -- part 0/1/2 by op 0 rep 1
                 10.1, 20.2, 29.9,    -- ... rep 2
                 10.2, 20.1, 30.1,    -- op 1 rep 1
                 9.9,  19.9, 30.0]    -- op 1 rep 2
          in (ops, parts, ys)

    it "gaugeRRCrossed: 部品支配ばらつき → grrPctPart 高い" $ do
      let (ops, parts, ys) = crossedData
      case GRR.gaugeRRCrossed ops parts ys of
        Left e -> expectationFailure (T.unpack e)
        Right r -> do
          -- 部品 0/1/2 の平均がそれぞれ 10/20/30 で大きく異なる → 部品分散支配
          GRR.grrPctPart r `shouldSatisfy` (> 90)
          GRR.grrTotalVar r `shouldSatisfy` (> 0)

    it "gaugeRRCrossed: 入力長 mismatch は Left" $
      case GRR.gaugeRRCrossed
             (V.fromList [0, 1])
             (V.fromList [0, 1, 2])
             (V.fromList [1, 2, 3]) of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Left"
